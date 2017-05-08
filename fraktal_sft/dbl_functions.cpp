#include "fraktal_sft.h"
#include <float.h>
#include "complex.h"

extern double g_real;
extern double g_imag;
extern double g_FactorAR;
extern double g_FactorAI;

DWORD WINAPI ThMC2(MC2 *pMC);
DWORD WINAPI ThMC(MC *pMC);
extern double g_SeedR;
extern double g_SeedI;

#ifdef KF_LONG_DOUBLE_DLL
extern void *(*AllocateArray)(int nSize);
extern void(*ReleaseArray)(void *p);
extern void(*AssignInt)(void *p, int nValue);
#ifdef KF_FLOAT_BACKEND_CUSTOM
extern void(*DLLConvertFromFixedFloat)(void *p, int nValues, FIXEDFLOAT_TYPE *pValues, BOOL bSign);
#define ConvertFromFixedFloat(p,x) DLLConvertFromFixedFloat((p),(x).m_nValues,(x).m_pValues,(x).m_bSign)
#else
#ifdef KF_FLOAT_BACKEND_MPFR
extern void(*DLLConvertFromFixedFloat)(void *p, const mpfr_t value);
#define ConvertFromFixedFloat(p,x) DLLConvertFromFixedFloat((p),(x).m_f.backend().data())
#else
extern void(*DLLConvertFromFixedFloat)(void *p, const mpf_t value);
#define ConvertFromFixedFloat(p,x) DLLConvertFromFixedFloat((p),(x).m_f.backend().data())
#endif
#endif
extern double(*SquareAdd)(void *a, void *b);
#else
extern void *(AllocateArray)(int nSize);
extern void(ReleaseArray)(void *p);
extern void(AssignInt)(void *p, int nValue);
#ifdef KF_FLOAT_BACKEND_CUSTOM
extern void(DLLConvertFromFixedFloat)(void *p, int nValues, FIXEDFLOAT_TYPE *pValues, BOOL bSign);
#define ConvertFromFixedFloat(p,x) DLLConvertFromFixedFloat((p),(x).m_nValues,(x).m_pValues,(x).m_bSign)
#else
#ifdef KF_FLOAT_BACKEND_MPFR
extern void(DLLConvertFromFixedFloat)(void *p, const mpfr_t value);
#define ConvertFromFixedFloat(p,x) DLLConvertFromFixedFloat((p),(x).m_f.backend().data())
#else
extern void(DLLConvertFromFixedFloat)(void *p, const mpf_t value);
#define ConvertFromFixedFloat(p,x) DLLConvertFromFixedFloat((p),(x).m_f.backend().data())
#endif
#endif
extern double(SquareAdd)(void *a, void *b);
#endif

#ifdef KF_THREADED_REFERENCE_BARRIER
#include "../common/barrier.h"
struct mcthread
{
	barrier *barrier;
	int nType;
	HANDLE hDone;
	CFixedFloat *xr, *xi, *xrn, *xin, *sr, *si, *xrxid, *m_rref, *m_iref;
	ldbl *m_ldxr, *m_ldxi;
	double *m_db_z, *terminate;
	int *m_nMaxIter, *m_nGlitchIter, *nMaxIter, *m_nRDone;
	volatile BOOL *stop;
};

static DWORD WINAPI mcthreadfunc(mcthread *p)
{
	ldbl noll;
	AssignInt(&noll, 0);

	int i;
	for (i = 0; i < *p->nMaxIter && !*p->stop; i++)
	{
		if (p->barrier->wait(p->stop)) break;
		switch (p->nType)
		{
			case 0: *p->xrn = *p->sr - *p->si + *p->m_rref; break;
			case 1: *p->xin = *p->xrxid - *p->sr - *p->si + *p->m_iref; break;
		}
		if (p->barrier->wait(p->stop)) break;
		switch (p->nType)
		{
			case 0: *p->xr = *p->xrn; *p->sr = p->xrn->Square(); ConvertFromFixedFloat(&p->m_ldxr[i], *p->xr); break;
			case 1: *p->xi = *p->xin; *p->si = p->xin->Square(); ConvertFromFixedFloat(&p->m_ldxi[i], *p->xi); break;
			case 2: *p->xrxid = (*p->xrn + *p->xin).Square(); break;
		}
		if (p->barrier->wait(p->stop)) break;
		if (p->nType == 0)
		{
			double abs_val = SquareAdd(g_real==0?&noll:&p->m_ldxr[i], g_imag==0?&noll:&p->m_ldxi[i]);
			p->m_db_z[i] = abs_val * 0.0000001;
			if (abs_val >= *p->terminate){
				if (*p->nMaxIter == *p->m_nMaxIter)
				{
					*p->nMaxIter = i + 3;
					if (*p->nMaxIter > *p->m_nMaxIter)
						*p->nMaxIter = *p->m_nMaxIter;
					*p->m_nGlitchIter = *p->nMaxIter;
				}
			}
			(*p->m_nRDone)++;
		}
	}
	if (p->nType == 0)
	{
		for (; i < *p->nMaxIter && !*p->stop; i++)
		{
			ConvertFromFixedFloat(&p->m_ldxr[i], *p->xr);
			ConvertFromFixedFloat(&p->m_ldxi[i], *p->xi);
		}
	}
	SetEvent(p->hDone);
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
	CFixedFloat xr = g_SeedR, xi = g_SeedI, xin, xrn, sr = xr.Square(), si = xi.Square(), xrxid = 0;
	if (m_nFractalType == 1 && m_nPower == 2){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = sr - si + m_rref;
			xrxid = 2 * xr*xi;
			xin = xrxid.Abs() + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

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
		}
	}
	else if (m_nFractalType == 1 && m_nPower == 3){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr - (si * 3)) * xr.Abs() + m_rref;
			xin = ((sr * 3) - si) * xi.Abs() + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

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
		}
	}
	else if (m_nFractalType == 1 && m_nPower == 4){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = sr*sr + si*si - 6*sr*si + m_rref;
			xin = 4 * (xr*xi).Abs()*(sr-si) + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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
	else if (m_nFractalType == 1 && m_nPower == 5){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = xr.Abs() * (sr*sr - 10 * sr*si + 5 * si*si) + m_rref;
			xin = xi.Abs() * (5 * sr*sr - 10 * sr*si + si*si) + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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
	else if (m_nFractalType == 2 && m_nPower == 2){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr - si).Abs() + m_rref;
			xin = -2.0 * (xr * xi).Abs() + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;
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
		}
	}
	else if (m_nFractalType == 2 && m_nPower == 3){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = ((sr - (si * 3.0)) * xr).Abs() + m_rref;
			xin = (((sr * 3.0) - si) * xi).Abs() + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;
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
		}
	}
	else if (m_nFractalType == 2 && m_nPower == 4){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr*sr + si*si - 6*sr*si).Abs() + m_rref;
			xin = (4*xr*xi*(sr-si)).Abs() + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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
	else if (m_nFractalType == 2 && m_nPower == 5){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (xr * (sr*sr - 10 * sr*si + 5 * si*si)).Abs() + m_rref;
			xin = (xi * (5 * sr*sr - 10 * sr*si + si*si)).Abs() + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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
	else if (m_nFractalType == 3 && m_nPower == 2){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr - si).Abs() + m_rref;
			xin = xr * xi * 2.0 + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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
		}
	}
	else if (m_nFractalType == 14){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr - (si * 3)) * xr.Abs() + m_rref;
			xin = -(((sr * 3.0) - si)).Abs() * xi + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

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
		}
	}
	else if (m_nPower == 2){

#ifdef KF_THREADED_REFERENCE_OPENMP
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
			mc[i].m_ldxr = m_ldxr;
			mc[i].m_ldxi = m_ldxi;
			mc[i].m_db_z = m_db_z;
			mc[i].terminate = &terminate;
			mc[i].m_nMaxIter = &m_nMaxIter;
			mc[i].m_nGlitchIter = &m_nGlitchIter;
			mc[i].nMaxIter = &nMaxIter;
			mc[i].m_nRDone = &m_nRDone;
			mc[i].stop = &m_bStop;
			HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE) mcthreadfunc, (LPVOID)&mc[i], 0, NULL);
			SetThreadAffinityMask(hThread, 3);
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

		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = xr.Square() - xi.Square() + m_rref;
			xin = (xr*xi).Double() + m_iref;
			xr = xrn;
			xi = xin;
			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
			m_db_z[i] = abs_val*0.0000001;
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

#endif
#endif
#endif

	}
	else if (m_nPower == 3){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = xr*(xr.Square() - 3 * xi.Square()) + m_rref;
			xin = (3 * xr.Square() - xi.Square())*xi + m_iref;
			xr = xrn;
			xi = xin;
			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
			m_db_z[i] = abs_val*0.000001;
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
	else if (m_nPower == 4){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			sr = xr.Square();
			si = xi.Square();
			xrxid = xr*xi;
			xrn = sr.Square() - 6 * sr*si + si.Square() + m_rref;
			xin = 4 * xrxid*(sr - si) + m_iref;
			xr = xrn;
			xi = xin;
			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
			m_db_z[i] = abs_val*0.00001;
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
}
void CFraktalSFT::CalculateReferenceLDBL1()
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
	CFixedFloat xr = g_SeedR, xi = g_SeedI, xin, xrn, sr = xr.Square(), si = xi.Square(), xrxid = 0;

	if (m_nFractalType == 15){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = sr * sr + si * si - 6 * sr * si +m_rref;
			xin = 4 * xr * xi.Abs() * (sr - si) + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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
void CFraktalSFT::CalculateReferenceLDBL2()
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
	CFixedFloat xr = g_SeedR, xi = g_SeedI, xin, xrn, sr = xr.Square(), si = xi.Square(), xrxid = 0;

	if (m_nFractalType == 30){
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = xr.Abs() * (sr*sr - 10 * sr*si + 5 * si*si) +m_rref;	//func14
			xin = -(xi * (5 * sr*sr - 10 * sr*si + si*si)).Abs() +m_iref;	//func18
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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
void CFraktalSFT::CalculateReferenceLDBL3()
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
	CFixedFloat xr = g_SeedR, xi = g_SeedI, xin, xrn, sr = xr.Square(), si = xi.Square(), xrxid = 0;

	if (m_nFractalType == 42){
		complex<CFixedFloat> z(xr,xi);
		complex<CFixedFloat> a(g_FactorAR,g_FactorAI);
		complex<CFixedFloat> c(m_rref,m_iref);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2)+(z^3)+c;
			m_nRDone++;

			ConvertFromFixedFloat(&m_ldxr[i], z.m_r);
			ConvertFromFixedFloat(&m_ldxi[i], z.m_i);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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
		complex<CFixedFloat> a(g_FactorAR,g_FactorAI);
		complex<CFixedFloat> c(m_rref,m_iref);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2) - (z^3)+c;
			m_nRDone++;

			ConvertFromFixedFloat(&m_ldxr[i], z.m_r);
			ConvertFromFixedFloat(&m_ldxi[i], z.m_i);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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
		complex<CFixedFloat> z(xr,xi);
		complex<CFixedFloat> c(m_rref,m_iref);
		complex<CFixedFloat> a(2,0);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2) - (z^3)+c;
			m_nRDone++;

			ConvertFromFixedFloat(&m_ldxr[i], z.m_r);
			ConvertFromFixedFloat(&m_ldxi[i], z.m_i);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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
		complex<CFixedFloat> z(xr,xi);
		complex<CFixedFloat> c(m_rref,m_iref);
		complex<CFixedFloat> a(g_FactorAR,g_FactorAI);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2) + (z^4) + c;
			m_nRDone++;

			ConvertFromFixedFloat(&m_ldxr[i], z.m_r);
			ConvertFromFixedFloat(&m_ldxi[i], z.m_i);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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
		complex<CFixedFloat> z(xr,xi);
		complex<CFixedFloat> c(m_rref,m_iref);
		complex<CFixedFloat> a(g_FactorAR,g_FactorAI);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2) - (z^4) + c;
			m_nRDone++;

			ConvertFromFixedFloat(&m_ldxr[i], z.m_r);
			ConvertFromFixedFloat(&m_ldxi[i], z.m_i);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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
		complex<CFixedFloat> z(xr,xi);
		complex<CFixedFloat> c(m_rref,m_iref);
		complex<CFixedFloat> a(g_FactorAR,g_FactorAI);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2) + (z^5) + c;
			m_nRDone++;

			ConvertFromFixedFloat(&m_ldxr[i], z.m_r);
			ConvertFromFixedFloat(&m_ldxi[i], z.m_i);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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
		complex<CFixedFloat> z(xr,xi);
		complex<CFixedFloat> c(m_rref,m_iref);
		complex<CFixedFloat> a(g_FactorAR,g_FactorAI);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2) - (z^5) + c;
			m_nRDone++;

			ConvertFromFixedFloat(&m_ldxr[i], z.m_r);
			ConvertFromFixedFloat(&m_ldxi[i], z.m_i);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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
		complex<CFixedFloat> z(xr,xi);
		complex<CFixedFloat> c(m_rref,m_iref);
		complex<CFixedFloat> a(g_FactorAR,g_FactorAI);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2) + (z^6) + c;
			m_nRDone++;

			ConvertFromFixedFloat(&m_ldxr[i], z.m_r);
			ConvertFromFixedFloat(&m_ldxi[i], z.m_i);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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
		complex<CFixedFloat> a(g_FactorAR,g_FactorAI);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			z = a*(z^2) - (z^6) + c;
			m_nRDone++;

			ConvertFromFixedFloat(&m_ldxr[i], z.m_r);
			ConvertFromFixedFloat(&m_ldxi[i], z.m_i);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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
	else if (m_nFractalType == 51){//MessageBox(GetActiveWindow(),"Sorry this formula is not supported","Kalles Fraktaler",MB_OK|MB_ICONWARNING);return;
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			xrn = (sr-si)*(sr-si).Abs() - 2*xr*xi*(2*xr*xi).Abs() + m_rref;
			xin = (sr-si)*(2*xr*xi).Abs() + 2*xr*xi*(sr-si).Abs() + m_iref;
			xr = xrn;
			xi = xin;
			sr = xr.Square();
			si = xi.Square();
			m_nRDone++;

			ConvertFromFixedFloat(&m_ldxr[i], xr);
			ConvertFromFixedFloat(&m_ldxi[i], xi);
			abs_val = SquareAdd(g_real==0?&noll:&m_ldxr[i], g_imag==0?&noll:&m_ldxi[i]);
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