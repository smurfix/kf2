#include "fraktal_sft.h"
#include <float.h>
#include "complex.h"

#include "../formula/formula.h"

extern double g_real;
extern double g_imag;
DWORD WINAPI ThMC2(MC2 *pMC);
DWORD WINAPI ThMC(MC *pMC);
BOOL ISFLOATOK(double a);
extern double g_SeedR;
extern double g_SeedI;
extern double g_FactorAR;
extern double g_FactorAI;

#ifdef KF_THREADED_REFERENCE_BARRIER
#include "../common/barrier.h"
struct mcthread
{
	barrier *barrier;
	int nType;
	HANDLE hDone;
	CFixedFloat *xr, *xi, *xrn, *xin, *sr, *si, *xrxid, *m_rref, *m_iref;
	floatexp *m_dxr, *m_dxi;
	double *m_db_z, *terminate;
	int *m_nMaxIter, *m_nGlitchIter, *nMaxIter, *m_nRDone;
	volatile BOOL *stop;
};

static DWORD WINAPI mcthreadfunc(mcthread *p)
{
	floatexp real(g_real);
	floatexp imag(g_imag);

	int i;
	for (i = 0; i < *p->nMaxIter && !*p->stop; i++)
	{
		if (p->barrier->wait(p->stop)) break;
		switch (p->nType)
		{
			case 0: *p->xrn = *p->sr - *p->si + *p->m_rref; break;
			case 1: *p->xin = *p->xrxid - *p->sr - *p->si + *p->m_iref; break;
			case 2:
				if (i > 0)
				{
					double abs_val = (real * p->m_dxr[i-1] * p->m_dxr[i-1] + imag * p->m_dxi[i-1] * p->m_dxi[i-1]).todouble();
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
				break;
		}
		if (p->barrier->wait(p->stop)) break;
		switch (p->nType)
		{
			case 0: *p->xr = *p->xrn; *p->sr = p->xrn->Square(); p->m_dxr[i] = *p->xr; break;
			case 1: *p->xi = *p->xin; *p->si = p->xin->Square(); p->m_dxi[i] = *p->xi; break;
			case 2: *p->xrxid = (*p->xrn + *p->xin).Square(); break;
		}
	}
	if (p->barrier->wait(p->stop))
	{
		SetEvent(p->hDone);
		return 0;
	}
	if (p->nType == 0)
	{
		double abs_val = (real * p->m_dxr[i-1] * p->m_dxr[i-1] + imag * p->m_dxi[i-1] * p->m_dxi[i-1]).todouble();
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
		floatexp xr = p->m_dxr[i] = *p->xr;
		floatexp xi = p->m_dxi[i] = *p->xi;
		for (; i < *p->nMaxIter && !*p->stop; i++)
		{
			p->m_dxr[i] = xr;
			p->m_dxi[i] = xi;
		}
	}
	SetEvent(p->hDone);
	return 0;
}
#endif


void CFraktalSFT::CalculateReferenceEXP()
{
	int i;
	if (m_dxr)
		delete[] m_dxr;
	m_dxr = new floatexp[m_nMaxIter];
	if (m_dxi)
		delete[] m_dxi;
	m_dxi = new floatexp[m_nMaxIter];
	if (m_db_z)
		delete[] m_db_z;
	m_db_z = new double [m_nMaxIter];

	CFixedFloat xr = g_SeedR, xi = g_SeedI, xin, xrn, sr = xr.Square(), si = xi.Square(), xrxid = 0;

	floatexp real(g_real);
	floatexp imag(g_imag);

	double abs_val;
	double terminate = SMOOTH_BAILOUT*SMOOTH_BAILOUT;
	m_nGlitchIter = m_nMaxIter + 1;
	int nMaxIter = m_nMaxIter;
	if (m_nFractalType == 3 && m_nPower == 2){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr - si).Abs() + m_rref;
			xin = xr * xi * 2.0 + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
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
		}
	}
	else if (m_nFractalType == 3 && m_nPower == 3){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = ((sr - (si * 3)) * xr).Abs() + m_rref;
			xin = ((sr * 3) - si) * xi + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
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
		}
	}
	else if (m_nFractalType == 3 && m_nPower == 4){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr * sr + si * si - 6 * sr * si).Abs() +m_rref;
			xin = 4 * xr * xi * (sr - si) + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.00001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 3 && m_nPower == 5){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (xr * (sr*sr - 10 * sr*si + 5 * si*si)).Abs() +m_rref;
			xin = xi * (5 * sr*sr - 10 * sr*si + si*si) +m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 4 && m_nPower == 2){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = sr - si + m_rref;
			xin = m_iref - xr*xi*2.0;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
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
		}
	}
	else if (m_nFractalType == 4 && m_nPower == 3){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = -(sr - si * 3) * xr + m_rref;
			xin = (sr * 3 - si) * xi + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
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
		}
	}
	else if (m_nFractalType == 4 && m_nPower == 4){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xin = -4 * xr * xi * (sr - si) + m_iref;
			xrn = sr * sr + si * si - 6 * sr * si +m_rref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.00001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 4 && m_nPower == 5){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = xr * (sr*sr - 10 * sr*si + 5 * si*si) +m_rref;
			xin = -xi * (5 * sr*sr - 10 * sr*si + si*si) +m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 5){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr - si).Abs() + m_rref;
			xin = m_iref - xr*xi*2.0;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
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
		}
	}
	else if (m_nFractalType == 6){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = sr - si + m_rref;
			xin = m_iref - xr.Abs()*xi*2.0;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
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
		}
	}
	else if (m_nFractalType == 7){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = sr - si + m_rref;
			xin = m_iref - xr*xi.Abs()*2.0;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
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
		}
	}
	else if (m_nFractalType == 8){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr - si).Abs() + m_rref;
			xin = m_iref - xr.Abs()*xi*2.0;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
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
		}
	}
	else if (m_nFractalType == 9){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr - si).Abs() + m_rref;
			xin = m_iref - xr*xi.Abs()*2.0;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
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
		}
	}
	else if (m_nFractalType == 10){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr - (si * 3)) * xr.Abs() + m_rref;
			xin = -(((sr * 3.0) - si) * xi).Abs() + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
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
		}
	}
	else if (m_nFractalType == 11){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr - (si * 3)) * xr.Abs() + m_rref;
			xin = (sr * 3 - si) * xi + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
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
		}
	}
	else if (m_nFractalType == 12){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr - si * 3) * xr + m_rref;
			xin = ((sr * 3) - si) * xi.Abs() + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
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
		}
	}
	else if (m_nFractalType == 13){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr - si * 3) * xr + m_rref;
			xin = (((sr * 3.0) - si) * xi).Abs() + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
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
		}
	}
	else if (m_nFractalType == 14){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr - si * 3) * xr.Abs() + m_rref;
			xin = m_iref - (sr * 3 - si).Abs() * xi;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 0 && m_nPower == 2){

#ifdef KF_THREADED_REFERENCE_BARRIER
		mcthread mc[3];
		barrier barrier(3);
		HANDLE hDone[3];
		// spawn threads
		for (i = 0; i < 3; i++)
		{
			mc[i].barrier = &barrier;
			mc[i].nType = i;
			hDone[i] = mc[i].hDone = CreateEvent(NULL, 0, 0, NULL);
			mc[i].xr = &xr;
			mc[i].xi = &xi;
			mc[i].xrn = &xrn;
			mc[i].xin = &xin;
			mc[i].sr = &sr;
			mc[i].si = &si;
			mc[i].xrxid = &xrxid;
			mc[i].m_iref = &m_iref;
			mc[i].m_rref = &m_rref;
			mc[i].m_dxr = m_dxr;
			mc[i].m_dxi = m_dxi;
			mc[i].m_db_z = m_db_z;
			mc[i].terminate = &terminate;
			mc[i].m_nMaxIter = &m_nMaxIter;
			mc[i].m_nGlitchIter = &m_nGlitchIter;
			mc[i].nMaxIter = &nMaxIter;
			mc[i].m_nRDone = &m_nRDone;
			mc[i].stop = &m_bStop;
			HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE) mcthreadfunc, (LPVOID)&mc[i], 0, NULL);
			SetThreadAffinityMask(hThread, 1<<i);
			CloseHandle(hThread);
		}
		// wait for completion
		WaitForMultipleObjects(3, hDone, TRUE, INFINITE);
		for (i = 0; i < 3; i++){
			CloseHandle(hDone[i]);
		}
#else

#ifdef KF_THREADED_REFERENCE_CUSTOM
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
			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
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
			m_dxr[i] = xr;
			m_dxi[i] = xi;
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

    bool ok = reference_floatexp(m_nFractalType, m_nPower, m_dxr, m_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, m_rref, m_iref, g_SeedR, g_SeedI, terminate, real, imag);
    assert(ok && "reference_floatexp");

#endif
#endif
	}
	else
	{
    bool ok = reference_floatexp(m_nFractalType, m_nPower, m_dxr, m_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, m_rref, m_iref, g_SeedR, g_SeedI, terminate, real, imag);
    assert(ok && "reference_floatexp");
	}
#if 0
	else{
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
			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
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
#endif
}
void CFraktalSFT::CalculateReferenceEXP1()
{
	int i;
	if (m_dxr)
		delete[] m_dxr;
	m_dxr = new floatexp[m_nMaxIter];
	if (m_dxi)
		delete[] m_dxi;
	m_dxi = new floatexp[m_nMaxIter];
	if (m_db_z)
		delete[] m_db_z;
	m_db_z = new double [m_nMaxIter];

	CFixedFloat xr = g_SeedR, xi = g_SeedI, xin, xrn, sr = xr.Square(), si = xi.Square(), xrxid = 0;

	floatexp real(g_real);
	floatexp imag(g_imag);

	double abs_val;
	double terminate = SMOOTH_BAILOUT*SMOOTH_BAILOUT;
	m_nGlitchIter = m_nMaxIter + 1;
	int nMaxIter = m_nMaxIter;
	if (m_nFractalType == 15){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = sr * sr + si * si - 6 * sr * si +m_rref;
			xin = 4 * xr * xi.Abs() * (sr - si) + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 16){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = sr * sr + si * si - 6 * sr * si +m_rref;
			xin = 4 * xr.Abs() * xi * (sr - si) + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 17){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = sr * sr + si * si - 6 * sr * si +m_rref;
			xin = -4 * xr.Abs() * xi * (sr - si) + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 18){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr * sr + si * si - 6 * sr * si).Abs() +m_rref;
			xin = 4 * xr * xi.Abs() * (sr - si) + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 19){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr * sr + si * si - 6 * sr * si).Abs() +m_rref;	//func5
			xin = 4 * xr.Abs() * xi * (sr - si) + m_iref;		//func3
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 20){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr * sr + si * si - 6 * sr * si).Abs() +m_rref;	//func5
			xin = -4 * xr.Abs() * xi * (sr - si) + m_iref;		//func4
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 21){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = sr * sr + si * si - 6 * sr * si +m_rref;		//func2
			xin = (4 * xr * xi * (sr - si)).Abs() + m_iref;		//func6
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 22){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr * sr + si * si - 6 * sr * si).Abs() +m_rref;	//func5
			xin = -4 * xr * xi * (sr - si) + m_iref;		//func8
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 23){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = sr * sr + si * si - 6 * sr * si +m_rref;		//func2
			xin = -4 * xr * xi * (sr - si).Abs() + m_iref;		//func9
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 24){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = sr * sr + si * si - 6 * sr * si +m_rref;		//func2
			xin = 4 * xr * xi * (sr - si).Abs() + m_iref;		//func10
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 25){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr * sr + si * si - 6 * sr * si).Abs() +m_rref;	//func5
			xin = -4 * xr * xi * (sr - si).Abs() + m_iref;		//func9
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 26){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr * sr + si * si - 6 * sr * si).Abs() +m_rref;	//func5
			xin = 4 * xr * xi * (sr - si).Abs() + m_iref;		//func10
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}

	else if (m_nFractalType == 27){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = xr.Abs() * (sr*sr - 10 * sr*si + 5 * si*si) +m_rref;	//func14
			xin = xi * (5 * sr*sr - 10 * sr*si + si*si) +m_iref;		//func15
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 28){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = xr.Abs() * (sr*sr - 10 * sr*si + 5 * si*si) +m_rref;	//func14
			xin = -xi * (5 * sr*sr - 10 * sr*si + si*si) +m_iref;		//func16
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 29){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (xr * (sr*sr - 10 * sr*si + 5 * si*si)).Abs() +m_rref;	//func17
			xin = -xi * (5 * sr*sr - 10 * sr*si + si*si) +m_iref;		//func16
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
}

void CFraktalSFT::CalculateReferenceEXP2()
{
	int i;
	if (m_dxr)
		delete[] m_dxr;
	m_dxr = new floatexp[m_nMaxIter];
	if (m_dxi)
		delete[] m_dxi;
	m_dxi = new floatexp[m_nMaxIter];
	if (m_db_z)
		delete[] m_db_z;
	m_db_z = new double [m_nMaxIter];

	CFixedFloat xr = g_SeedR, xi = g_SeedI, xin, xrn, sr = xr.Square(), si = xi.Square(), xrxid = 0;

	floatexp real(g_real);
	floatexp imag(g_imag);

	double abs_val;
	double terminate = SMOOTH_BAILOUT*SMOOTH_BAILOUT;
	m_nGlitchIter = m_nMaxIter + 1;
	int nMaxIter = m_nMaxIter;
	if (m_nFractalType == 30){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = xr.Abs() * (sr*sr - 10 * sr*si + 5 * si*si) +m_rref;	//func14
			xin = -(xi * (5 * sr*sr - 10 * sr*si + si*si)).Abs() +m_iref;	//func18
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 31){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = xr.Abs() * (sr*sr - 10 * sr*si + 5 * si*si) +m_rref;	//func14
			xin = -xi * (5 * sr*sr - 10 * sr*si + si*si).Abs() +m_iref;		//func19
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 32){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = xr.Abs() * (sr*sr - 10 * sr*si + 5 * si*si) +m_rref;	//func14
			xin = xi * (5 * sr*sr - 10 * sr*si + si*si).Abs() +m_iref;	//func20
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 33){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = si*si.Abs()-4*xi*(xr*xi).Abs()*xr-sr*si.Abs()-si*sr.Abs()+sr*sr.Abs() + m_rref;
			xin = - 2*xr*xi*si.Abs()-2*si*(xr*xi).Abs()+2*sr*(xr*xi).Abs()+2*xr*xi*sr.Abs() + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 34){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = sr * sr + si * si - 6 * sr * si +m_rref;
			xin = 4 * xr * (xi*sr - xi*si).Abs() + m_iref;	//func21
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 35){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = sr * sr + si * si - 6 * sr * si +m_rref;
			xin = -4 * xi * (xr*sr - xr*si).Abs() + m_iref;	//func22
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 36){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = sr * sr + si * si - 6 * sr * si +m_rref;
			xin = 4 * xi * (xr*sr - xr*si).Abs() + m_iref;	//-func22
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 37){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr * sr + si * si - 6 * sr * si).Abs() +m_rref;
			xin = 4 * xr * (xi*sr - xi*si).Abs() + m_iref; //func21
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 38){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr * sr + si * si - 6 * sr * si).Abs() +m_rref;
			xin = -4 * xi * (xr*sr - xr*si).Abs() + m_iref;	//func22
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 39){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr * sr + si * si - 6 * sr * si).Abs() +m_rref;
			xin = 4 * xi * (xr*sr - xr*si).Abs() + m_iref;	//func22
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 40){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = 3*(sr*xi).Abs()*si*xi-(si*xi).Abs()*si*xi+9*(xr*si).Abs()*xr*si-3*(sr*xr).Abs()*xr*si+3*(si*xi).Abs()*sr*xi-9*(sr*xi).Abs()*sr*xi-3*(xr*si).Abs()*sr*xr+(sr*xr).Abs()*sr*xr +m_rref;
			xin = 3*si*xi*(xr*si).Abs()-(sr*xr).Abs()*si*xi-9*(sr*xi).Abs()*si*xr-9*(xr*si).Abs()*xi*sr+3*(sr*xr).Abs()*xi*sr+3*(si*xi).Abs()*xr*si-(si*xi).Abs()*sr*xr+3*(xi*sr).Abs()*sr*xr + m_iref;	//func22
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 41){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr - si) - xr.Abs() +m_rref;
			xin = (xr * xi).Abs() * 2.0 - xi.Abs() + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
}
void CFraktalSFT::CalculateReferenceEXP3()
{
	int i;
	if (m_dxr)
		delete[] m_dxr;
	m_dxr = new floatexp[m_nMaxIter];
	if (m_dxi)
		delete[] m_dxi;
	m_dxi = new floatexp[m_nMaxIter];
	if (m_db_z)
		delete[] m_db_z;
	m_db_z = new double [m_nMaxIter];

	CFixedFloat xr = g_SeedR, xi = g_SeedI, xin, xrn, sr = xr.Square(), si = xi.Square(), xrxid = 0;

	floatexp real(g_real);
	floatexp imag(g_imag);

	double abs_val;
	double terminate = SMOOTH_BAILOUT*SMOOTH_BAILOUT;
	m_nGlitchIter = m_nMaxIter + 1;
	int nMaxIter = m_nMaxIter;
	if (m_nFractalType == 42){
		complex<CFixedFloat> a(g_FactorAR,g_FactorAI);
		complex<CFixedFloat> z(xr,xi);
		complex<CFixedFloat> c(m_rref,m_iref);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2)+(z^3)+c;
			m_nRDone++;

			m_dxr[i] = z.m_r;
			m_dxi[i] = z.m_i;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 43){
		complex<CFixedFloat> z(xr,xi);
		complex<CFixedFloat> c(m_rref,m_iref);
		complex<CFixedFloat> a(g_FactorAR,g_FactorAI);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2) - (z^3)+c;
			m_nRDone++;

			m_dxr[i] = z.m_r;
			m_dxi[i] = z.m_i;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 44){
		complex<CFixedFloat> a(2,0);
		complex<CFixedFloat> z(xr,xi);
		complex<CFixedFloat> c(m_rref,m_iref);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2) - (z^3)+c;
			m_nRDone++;

			m_dxr[i] = z.m_r;
			m_dxi[i] = z.m_i;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 45){
		complex<CFixedFloat> a(g_FactorAR,g_FactorAI);
		complex<CFixedFloat> z(xr,xi);
		complex<CFixedFloat> c(m_rref,m_iref);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2) + (z^4) + c;
			m_nRDone++;

			m_dxr[i] = z.m_r;
			m_dxi[i] = z.m_i;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 46){
		complex<CFixedFloat> a(g_FactorAR,g_FactorAI);
		complex<CFixedFloat> z(xr,xi);
		complex<CFixedFloat> c(m_rref,m_iref);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2) - (z^4) + c;
			m_nRDone++;

			m_dxr[i] = z.m_r;
			m_dxi[i] = z.m_i;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 47){
		complex<CFixedFloat> a(g_FactorAR,g_FactorAI);
		complex<CFixedFloat> z(xr,xi);
		complex<CFixedFloat> c(m_rref,m_iref);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2) + (z^5) + c;
			m_nRDone++;

			m_dxr[i] = z.m_r;
			m_dxi[i] = z.m_i;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 48){
		complex<CFixedFloat> a(g_FactorAR,g_FactorAI);
		complex<CFixedFloat> z(xr,xi);
		complex<CFixedFloat> c(m_rref,m_iref);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2) - (z^5) + c;
			m_nRDone++;

			m_dxr[i] = z.m_r;
			m_dxi[i] = z.m_i;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 49){
		complex<CFixedFloat> a(g_FactorAR,g_FactorAI);
		complex<CFixedFloat> z(xr,xi);
		complex<CFixedFloat> c(m_rref,m_iref);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2) + (z^6) + c;
			m_nRDone++;

			m_dxr[i] = z.m_r;
			m_dxi[i] = z.m_i;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 50){
		complex<CFixedFloat> z(xr,xi);
		complex<CFixedFloat> c(m_rref,m_iref);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = (z^2) - (z^6) + c;
			m_nRDone++;

			m_dxr[i] = z.m_r;
			m_dxi[i] = z.m_i;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
	else if (m_nFractalType == 51){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr-si)*(sr-si).Abs() - 2*xr*xi*(2*xr*xi).Abs() + m_rref;
			xin = (sr-si)*(2*xr*xi).Abs() + 2*xr*xi*(sr-si).Abs() + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			m_dxr[i] = xr;
			m_dxi[i] = xi;
			abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
	}
}