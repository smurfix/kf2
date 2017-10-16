#include "fraktal_sft.h"
#include <float.h>
#include "complex.h"

#include "../formula/formula.h"

extern double g_real;
extern double g_imag;
extern double g_FactorAR;
extern double g_FactorAI;

DWORD WINAPI ThMC2(MC2 *pMC);
DWORD WINAPI ThMC(MC *pMC);
extern double g_SeedR;
extern double g_SeedI;

#include "../common/barrier.h"

struct mcthread_common
{
	barrier *barrier;
	mpf_t xr, xi, xrn, xin, xrn1, xin1, xrxid, xrxid1, sr, si, cr, ci;
	long double *m_ldxr, *m_ldxi;
	double *m_db_z, *terminate, *glitch_threshold;
	int *m_nMaxIter, *m_nGlitchIter, *nMaxIter, *m_nRDone;
	int *antal;
	double *test1;
	double *test2;
	volatile BOOL *stop;
};

struct mcthread
{
	int nType;
	HANDLE hDone;
	mcthread_common *common;
};

static DWORD WINAPI mcthreadfunc(mcthread *p0)
{
	int antal = 0;
	double test1 = 0;
	double test2 = 0;
	bool stored = false;
	double old_absval = 0;
	double abs_val = 0;

	mcthread_common *p = p0->common;
	const double glitch_threshold = *p->glitch_threshold;
	int i = 0;
	switch (p0->nType)
	{
		case 0:
		{
			for (i = 0; i < *p->nMaxIter; i++)
			{
				mpf_sub(p->xrn1, p->sr, p->si);
				mpf_add(p->xrn, p->cr, p->xrn1);
				if (p->barrier->wait(p->stop)) break;
				mpf_set(p->xr, p->xrn);
				mpf_mul(p->sr, p->xrn, p->xrn);
				p->m_ldxr[i] = mpf_get_ld(p->xrn);
				if (p->barrier->wait(p->stop)) break;
			}
		}
		break;
		case 1:
		{
			for (i = 0; i < *p->nMaxIter; i++)
			{
				mpf_add(p->xin, p->sr, p->si);
				mpf_sub(p->xin1, p->ci, p->xin);
				mpf_add(p->xin, p->xin1, p->xrxid);
				if (p->barrier->wait(p->stop)) break;
				mpf_set(p->xi, p->xin);
				mpf_mul(p->si, p->xin, p->xin);
				p->m_ldxi[i] = mpf_get_ld(p->xin);
				if (p->barrier->wait(p->stop)) break;
			}
		}
		break;
		case 2:
		{
			for (i = 0; i < *p->nMaxIter; i++)
			{
				if (i > 0)
				{
					const long double lr = p->m_ldxr[i-1];
					const long double li = p->m_ldxi[i-1];
					old_absval = abs_val;
					abs_val = g_real * lr * lr + g_imag * li * li;
					p->m_db_z[i-1] = abs_val * glitch_threshold;
					if (abs_val >= 4)
					{
						if (*p->terminate == 4 && !stored)
						{
							stored = true;
							antal = i;
							test1 = abs_val;
							test2 = old_absval;
						}
					}
					if (abs_val >= *p->terminate){
						if (*p->terminate > 4 && !stored)
						{
							stored = true;
							antal = i;
							test1 = abs_val;
							test2 = old_absval;
						}
						if (*p->nMaxIter == *p->m_nMaxIter)
						{
							*p->nMaxIter = i-1 + 3;
							if (*p->nMaxIter > *p->m_nMaxIter)
								*p->nMaxIter = *p->m_nMaxIter;
							*p->m_nGlitchIter = *p->nMaxIter;
						}
					}
					(*p->m_nRDone)++;
				}
				if (p->barrier->wait(p->stop)) break;
				mpf_add(p->xrxid1, p->xrn, p->xin);
				mpf_mul(p->xrxid, p->xrxid1, p->xrxid1);
				if (p->barrier->wait(p->stop)) break;
			}
		}
		break;
	}
	if (p->barrier->wait(p->stop))
	{
		SetEvent(p0->hDone);
		return 0;
	}
	if (p0->nType == 2)
	{
		if (i > 0)
		{
			const long double lr = p->m_ldxr[i-1];
			const long double li = p->m_ldxi[i-1];
			old_absval = abs_val;
			abs_val = g_real * lr * lr + g_imag * li * li;
			p->m_db_z[i-1] = abs_val * glitch_threshold;
			if (abs_val >= 4)
			{
				if (*p->terminate == 4 && !stored)
				{
					stored = true;
					antal = i;
					test1 = abs_val;
					test2 = old_absval;
				}
			}
			if (abs_val >= *p->terminate){
				if (*p->terminate > 4 && !stored)
				{
					stored = true;
					antal = i;
					test1 = abs_val;
					test2 = old_absval;
				}
				if (*p->nMaxIter == *p->m_nMaxIter)
				{
					*p->nMaxIter = i-1 + 3;
					if (*p->nMaxIter > *p->m_nMaxIter)
						*p->nMaxIter = *p->m_nMaxIter;
					*p->m_nGlitchIter = *p->nMaxIter;
				}
			}
			(*p->m_nRDone)++;
		}
		const long double xr = mpf_get_ld(p->xr);
		const long double xi = mpf_get_ld(p->xi);
		for (; i < *p->nMaxIter && !*p->stop; i++)
		{
			p->m_ldxr[i] = xr;
			p->m_ldxi[i] = xi;
		}
		*p->antal = antal;
		*p->test1 = test1;
		*p->test2 = test2;
	}
	SetEvent(p0->hDone);
	return 0;
}

void CFraktalSFT::CalculateReferenceLDBL()
{
	int i;
	if (m_ldxr)
		delete[] m_ldxr;
	m_ldxr = new long double[m_nMaxIter];
	if (m_ldxi)
		delete[] m_ldxi;
	m_ldxi = new long double[m_nMaxIter];
	if (m_db_z)
		delete[] m_db_z;
	m_db_z = new double [m_nMaxIter];

	int antal = 0;
	double test1 = 0;
	double test2 = 0;

	double terminate = SMOOTH_BAILOUT*SMOOTH_BAILOUT;
	m_nGlitchIter = m_nMaxIter + 1;
	int nMaxIter = m_nMaxIter;

	if (m_nFractalType == 0 && m_nPower == 2){
		double glitch_threshold = 0.0000001;
		if (GetGlitchLowTolerance()) {
			glitch_threshold = sqrt(glitch_threshold);
		}

		mcthread mc[3];
		barrier barrier(3);
		HANDLE hDone[3];

		mcthread_common co;
	  co.barrier = &barrier;
		mp_bitcnt_t bits = mpf_get_prec(m_rref.m_f.backend().data());
		mpf_init2(co.xr, bits);
		mpf_init2(co.xi, bits);
		mpf_init2(co.xrn, bits);
		mpf_init2(co.xin, bits);
		mpf_init2(co.xrn1, bits);
		mpf_init2(co.xin1, bits);
		mpf_init2(co.xrxid, bits);
		mpf_init2(co.xrxid1, bits);
		mpf_init2(co.sr, bits);
		mpf_init2(co.si, bits);
		mpf_init2(co.cr, bits);
		mpf_init2(co.ci, bits);
		mpf_set(co.cr, m_rref.m_f.backend().data());
		mpf_set(co.ci, m_iref.m_f.backend().data());
		mpf_set_d(co.xr, g_SeedR);
		mpf_set_d(co.xi, g_SeedI);
		mpf_mul(co.sr, co.xr, co.xr);
		mpf_mul(co.si, co.xi, co.xi);
		mpf_set_d(co.xrxid, 0);
		co.m_ldxr = m_ldxr;
		co.m_ldxi = m_ldxi;
		co.m_db_z = m_db_z;
		co.terminate = &terminate;
		co.glitch_threshold = &glitch_threshold;
		co.m_nMaxIter = &m_nMaxIter;
		co.m_nGlitchIter = &m_nGlitchIter;
		co.nMaxIter = &nMaxIter;
		co.m_nRDone = &m_nRDone;
		co.stop = &m_bStop;
		co.antal = &antal;
		co.test1 = &test1;
		co.test2 = &test2;
		// spawn threads
		for (i = 0; i < 3; i++)
		{
			mc[i].nType = i;
			hDone[i] = mc[i].hDone = CreateEvent(NULL, 0, 0, NULL);
			mc[i].common = &co;
			HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE) mcthreadfunc, (LPVOID)&mc[i], 0, NULL);
			CloseHandle(hThread);
		}
		// wait for completion
		WaitForMultipleObjects(3, hDone, TRUE, INFINITE);
		for (i = 0; i < 3; i++){
			CloseHandle(hDone[i]);
		}

		mpf_clear(co.xr);
		mpf_clear(co.xi);
		mpf_clear(co.xrn);
		mpf_clear(co.xin);
		mpf_clear(co.xrn1);
		mpf_clear(co.xin1);
		mpf_clear(co.xrxid);
		mpf_clear(co.xrxid1);
		mpf_clear(co.sr);
		mpf_clear(co.si);
		mpf_clear(co.cr);
		mpf_clear(co.ci);

	}
	else if (m_nFractalType == 0 && m_nPower > 10)
	{
		bool stored = false;
		double old_absval = 0;
		double abs_val = 0;
		CFixedFloat xr = g_SeedR, xi = g_SeedI;
		double threashold = 0.0001;
		for (i = 7; i <= m_nPower; i += 2)
			threashold *= 10;
    if (GetGlitchLowTolerance()) {
			threashold = sqrt(threashold);
		}
		if (threashold>.5)
			threashold = .5;
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			complex<CFixedFloat> X(xr, xi), r(m_rref, m_iref);
			complex<CFixedFloat> Xn = (X^m_nPower) + r;
			xr = Xn.m_r;
			xi = Xn.m_i;
			m_ldxr[i] = mpf_get_ld(xr.m_f.backend().data());
			m_ldxi[i] = mpf_get_ld(xi.m_f.backend().data());
			old_absval = abs_val;
			abs_val = g_real * m_ldxr[i] * m_ldxr[i] + g_imag * m_ldxi[i] * m_ldxi[i];
			m_db_z[i] = abs_val*threashold;
			if (abs_val >= 4)
			{
				if (terminate == 4 && !stored)
				{
					stored = true;
					antal = i;
					test1 = abs_val;
					test2 = old_absval;
				}
			}
			if (abs_val >= terminate){
				if (terminate > 4 && !stored)
				{
					stored = true;
					antal = i;
					test1 = abs_val;
					test2 = old_absval;
				}
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
			m_nRDone++;
		}

	}
	else
	{

		bool ok = reference_long_double(m_nFractalType, m_nPower, m_ldxr, m_ldxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, m_rref, m_iref, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, GetGlitchLowTolerance(), antal, test1, test2);
    assert(ok && "reference_long_double");

	}

	if (0 <= g_nAddRefX && g_nAddRefX < m_nX && 0 <= g_nAddRefY && g_nAddRefY < m_nY)
		OutputIterationData(g_nAddRefX, g_nAddRefY, false, antal, test1, test2);
}
