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

extern void *(AllocateArray)(int nSize);
extern void(ReleaseArray)(void *p);
extern void(AssignInt)(void *p, int nValue);
extern void(DLLConvertFromFixedFloat)(void *p, const mpf_t value);
#define ConvertFromFixedFloat(p,x) DLLConvertFromFixedFloat((p),(x).m_f.backend().data())
extern double(SquareAdd)(void *a, void *b);

#ifdef KF_THREADED_REFERENCE_BARRIER

#include "../common/barrier.h"

long double mpf_get_ld(const mpf_t value)
{
	using std::ldexp;
	signed long int e = 0;
	long double l = mpf_get_d_2exp(&e, value);
	l = ldexp(l, e);
	if ((mpf_sgn(value) >= 0) != (l >= 0)) l = -l; // workaround GMP bug
	return l;
}

struct mcthread_common
{
	barrier *barrier;
	mpf_t xr, xi, xrn, xin, xrn1, xin1, xrxid, xrxid1, sr, si, cr, ci;
	long double *m_ldxr, *m_ldxi;
	double *m_db_z, *terminate;
	int *m_nMaxIter, *m_nGlitchIter, *nMaxIter, *m_nRDone;
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
	mcthread_common *p = p0->common;
	int i;
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
					const double abs_val = g_real * lr * lr + g_imag * li * li;
					p->m_db_z[i-1] = abs_val * 0.0000001;
					if (abs_val >= *p->terminate){
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
	if (p0->nType == 0)
	{
		if (i > 0)
		{
			const long double lr = p->m_ldxr[i-1];
			const long double li = p->m_ldxi[i-1];
			const double abs_val = g_real * lr * lr + g_imag * li * li;
			p->m_db_z[i-1] = abs_val * 0.0000001;
			if (abs_val >= *p->terminate){
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
	}
	SetEvent(p0->hDone);
	return 0;
}

#endif

void CFraktalSFT::CalculateReferenceLDBL()
{
	int i;
	if (m_ldxr)
		ReleaseArray(m_ldxr);
	m_ldxr = (ldbl*)AllocateArray(m_nMaxIter);
	if (m_ldxi)
		ReleaseArray(m_ldxi);
	m_ldxi = (ldbl*)AllocateArray(m_nMaxIter);
	if (m_db_z)
		delete[] m_db_z;
	m_db_z = new double [m_nMaxIter];

	ldbl noll;
	AssignInt(&noll,0);

	double abs_val;
	double terminate = SMOOTH_BAILOUT*SMOOTH_BAILOUT;
	m_nGlitchIter = m_nMaxIter + 1;
	int nMaxIter = m_nMaxIter;

	if (m_nFractalType == 0 && m_nPower == 2){

#ifdef KF_THREADED_REFERENCE_OPENMP
		CFixedFloat xr = g_SeedR, xi = g_SeedI, xin, xrn, sr = xr.Square(), si = xi.Square(), xrxid = 0;
		// avoid deadlock on race condition if different threads get different values
		// for m_bStop at loop entry time
		int dummy;
		int should_stop = m_bStop;
		#pragma omp parallel num_threads(3)
		{
			for (int j = 0; j < nMaxIter && !should_stop; ++j)
			{
				#pragma omp for
				for (int thread = 0; thread < 2; ++thread)
				{
					if (thread == 0) { xrn = sr - si + m_rref; }
					if (thread == 1) { xin = xrxid - sr - si + m_iref; }
				}
				#pragma omp for
				for (int thread = 0; thread < 3; ++thread)
			  {
					if (thread == 0) { xr = xrn; sr = xr.Square(); ConvertFromFixedFloat(&m_ldxr[j], xr); }
					if (thread == 1) { xi = xin; si = xi.Square(); ConvertFromFixedFloat(&m_ldxi[j], xi); }
					if (thread == 2) { xrxid = (xrn + xin).Square(); }
				}
				#pragma omp single
				{
					abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[j], g_imag==0?&noll:&m_ldxi[j]);
					m_db_z[j] = abs_val*0.0000001;
					if (abs_val >= terminate){
						if (nMaxIter == m_nMaxIter){
							nMaxIter = i + 3;
							if (nMaxIter>m_nMaxIter)
								nMaxIter = m_nMaxIter;
							m_nGlitchIter = nMaxIter;
						}
					}
					m_nRDone++;
					i = j + 1;
					should_stop = m_bStop;
				}
			}
		}
		for (; i < nMaxIter && !m_bStop; i++)
		{
			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
		}
#else

#ifdef KF_THREADED_REFERENCE_BARRIER
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
		co.m_nMaxIter = &m_nMaxIter;
		co.m_nGlitchIter = &m_nGlitchIter;
		co.nMaxIter = &nMaxIter;
		co.m_nRDone = &m_nRDone;
		co.stop = &m_bStop;
		// spawn threads
		for (i = 0; i < 3; i++)
		{
			mc[i].nType = i;
			hDone[i] = mc[i].hDone = CreateEvent(NULL, 0, 0, NULL);
			mc[i].common = &co;
			HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE) mcthreadfunc, (LPVOID)&mc[i], 0, NULL);
			SetThreadAffinityMask(hThread, 3);
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

#else

#ifdef KF_THREADED_REFERENCE_CUSTOM
		CFixedFloat xr = g_SeedR, xi = g_SeedI, xin, xrn, sr = xr.Square(), si = xi.Square(), xrxid = 0;
		MC mc[3];
		HANDLE hDone[3];
		HANDLE hWait[3];
		HANDLE hExit[3];
		for (i = 0; i<3; i++){
			mc[i].xr = &xr;
			mc[i].xi = &xi;
			mc[i].sr = &sr;
			mc[i].si = &si;
			mc[i].xrxid = &xrxid;
			hDone[i] = mc[i].hDone = CreateEvent(NULL, 0, 0, NULL);
			hWait[i] = mc[i].hWait = CreateEvent(NULL, 0, 0, NULL);
			hExit[i] = mc[i].hExit = CreateEvent(NULL, 0, 0, NULL);
			mc[i].nType = i;
		}
		HANDLE hThread;
		DWORD dw;
		for (i = 0; i<3; i++){
			hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)ThMC, (LPVOID)&mc[i], 0, &dw);
			CloseHandle(hThread);
		}
		MC2 mc2[2];
		HANDLE hDone2[2];
		HANDLE hWait2[2];
		HANDLE hExit2[2];
		for (i = 0; i<2; i++){
			mc2[i].xrn = &xrn;
			mc2[i].xin = &xin;
			mc2[i].xrxid = &xrxid;
			mc2[i].sr = &sr;
			mc2[i].si = &si;
			mc2[i].m_iref = &m_iref;
			mc2[i].m_rref = &m_rref;
			hDone2[i] = mc2[i].hDone = CreateEvent(NULL, 0, 0, NULL);
			hWait2[i] = mc2[i].hWait = CreateEvent(NULL, 0, 0, NULL);
			hExit2[i] = mc2[i].hExit = CreateEvent(NULL, 0, 0, NULL);
			mc2[i].nType = i;
		}
		for (i = 0; i<2; i++){
			hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)ThMC2, (LPVOID)&mc2[i], 0, &dw);
			CloseHandle(hThread);
		}
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			//xin = xrxid-sr-si + m_iref;
			//xrn = sr - si + m_rref; 
			SetEvent(hWait2[0]);
			SetEvent(hWait2[1]);
			WaitForMultipleObjects(2, hDone2, TRUE, INFINITE);
			xr = xrn;
			xi = xin;
			//sr = xr.Square();
			//si = xi.Square(); 
			//xrxid = (xr+xi).Square(); 
			SetEvent(hWait[0]);
			SetEvent(hWait[1]);
			SetEvent(hWait[2]);
			WaitForMultipleObjects(3, hDone, TRUE, INFINITE);
			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
			m_db_z[i] = abs_val*0.0000001;
			//m_db_z[i] = abs_val*0.000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
			m_nRDone++;
		}
		SetEvent(hExit[0]);
		SetEvent(hExit[1]);
		SetEvent(hExit[2]);
		SetEvent(hExit2[0]);
		SetEvent(hExit2[1]);
		WaitForMultipleObjects(3, hDone, TRUE, INFINITE);
		WaitForMultipleObjects(2, hDone2, TRUE, INFINITE);
		for (; i<nMaxIter && !m_bStop; i++){
			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
		}
		for (i = 0; i<3; i++){
			CloseHandle(hDone[i]);
			CloseHandle(hWait[i]);
			CloseHandle(hExit[i]);
		}
		for (i = 0; i<2; i++){
			CloseHandle(hDone2[i]);
			CloseHandle(hWait2[i]);
			CloseHandle(hExit2[i]);
		}
#else

    bool ok = reference_long_double(m_nFractalType, m_nPower, (long double *)m_ldxr, (long double *)m_ldxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, m_rref, m_iref, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag);
    assert(ok && "reference_long_double");

#endif
#endif
#endif

	}
	else if (m_nFractalType == 0 && m_nPower > 10)
	{

		CFixedFloat xr = g_SeedR, xi = g_SeedI;
		double threashold = 0.0001;
		for (i = 7; i <= m_nPower; i += 2)
			threashold *= 10;
		if (threashold>.5)
			threashold = .5;
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			complex<CFixedFloat> X(xr, xi), r(m_rref, m_iref);
			complex<CFixedFloat> Xn = (X^m_nPower) + r;
			xr = Xn.m_r;
			xi = Xn.m_i;
			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
			m_db_z[i] = abs_val*threashold;
			if (abs_val >= terminate){
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

		bool ok = reference_long_double(m_nFractalType, m_nPower, (long double *)m_ldxr, (long double *)m_ldxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, m_rref, m_iref, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag);
    assert(ok && "reference_long_double");

	}
}
