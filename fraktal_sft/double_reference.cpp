#include "fraktal_sft.h"
#include "complex.h"
#include "../formula/formula.h"

bool reference_double_0_2_ld(const int m_nFractalType, const int m_nPower, double *m_db_dxr, double *m_db_dxi, double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr0, const CFixedFloat &Ci0, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &antal, double &test1, double &test2, long double &dr0, long double &di0)
{
  if (m_nFractalType == 0 && m_nPower == 2)
  {
    bool stored = false;
    double old_absval = 0;
    double abs_val = 0;
    m_nGlitchIter = m_nMaxIter + 1;
    int nMaxIter = m_nMaxIter;
    int i;
    double glitch = 0.0000001;
    if (m_bGlitchLowTolerance) {
      glitch = sqrt(glitch);
    }
    mp_bitcnt_t bits = mpfr_get_prec(Cr0.m_f.backend().data());
    mpfr_t Cr; mpfr_init2(Cr, bits); mpfr_set(Cr, Cr0.m_f.backend().data(), MPFR_RNDN);
    mpfr_t Ci; mpfr_init2(Ci, bits); mpfr_set(Ci, Ci0.m_f.backend().data(), MPFR_RNDN);
    mpfr_t Xr; mpfr_init2(Xr, bits); mpfr_set_d(Xr, g_SeedR, MPFR_RNDN);
    mpfr_t Xi; mpfr_init2(Xi, bits); mpfr_set_d(Xi, g_SeedI, MPFR_RNDN);
    double Xrd = mpfr_get_d(Xr, MPFR_RNDN);
    double Xid = mpfr_get_d(Xi, MPFR_RNDN);
    mpfr_t Xr2; mpfr_init2(Xr2, bits); mpfr_sqr(Xr2, Xr, MPFR_RNDN);
    mpfr_t Xi2; mpfr_init2(Xi2, bits); mpfr_sqr(Xi2, Xi, MPFR_RNDN);
    mpfr_t Xrn; mpfr_init2(Xrn, bits);
    mpfr_t Xin; mpfr_init2(Xin, bits);
    mpfr_t Ar; mpfr_init2(Ar, bits); mpfr_set_d(Ar, g_FactorAR, MPFR_RNDN);
    mpfr_t Ai; mpfr_init2(Ai, bits); mpfr_set_d(Ai, g_FactorAI, MPFR_RNDN);
    long double dr = dr0, di = di0;
    long double drn = 0, din = 0;


#define LOOP \
      dr = drn; di = din; \
      mpfr_set(Xr, Xrn, MPFR_RNDN); \
      mpfr_set(Xi, Xin, MPFR_RNDN); \
      mpfr_sqr(Xr2, Xr, MPFR_RNDN); \
      mpfr_sqr(Xi2, Xi, MPFR_RNDN); \
      m_nRDone++; \
      Xrd = mpfr_get_d(Xr, MPFR_RNDN); \
      Xid = mpfr_get_d(Xi, MPFR_RNDN); \
      old_absval = abs_val; \
      abs_val = g_real * Xrd * Xrd + g_imag * Xid * Xid; \
      const double Xz = abs_val * glitch; \
      m_db_dxr[i] = Xrd; \
      m_db_dxi[i] = Xid; \
      m_db_z[i] = Xz; \
      if (abs_val >= 4) \
      { \
        if (terminate == 4 && !stored) \
        { \
          stored = true; \
          antal = i; \
          test1 = abs_val; \
          test2 = old_absval; \
        } \
      } \
      if (abs_val >= terminate){ \
        if (terminate > 4 && !stored) \
        { \
          stored = true; \
          antal = i; \
          test1 = abs_val; \
          test2 = old_absval; \
        } \
        if (nMaxIter == m_nMaxIter){ \
          nMaxIter = i + 3; \
          if (nMaxIter > m_nMaxIter) \
            nMaxIter = m_nMaxIter; \
          m_nGlitchIter = nMaxIter; \
        } \
      }


#define DLOOP \
 \
        const double Xxr = Xrd; (void) Xxr; \
        const double Xxi = Xid; (void) Xxi; \
         \
        drn = 2 * (Xxr * dr - Xxi * di) + 1; \
        din = 2 * (Xxr * di + Xxi * dr); \
       \
       \


mpfr_t t0; mpfr_init2(t0, bits);
mpfr_t t1; mpfr_init2(t1, bits);
for (i = 0; i < nMaxIter && !m_bStop; i++) { DLOOP
mpfr_sub(t0,Xr2,Xi2,MPFR_RNDN);
mpfr_add(t1,t0,Cr,MPFR_RNDN);
mpfr_set(Xrn,t1,MPFR_RNDN);
mpfr_add(t1,Xr,Xi,MPFR_RNDN);
mpfr_mul(t0,t1,t1,MPFR_RNDN);
mpfr_sub(t1,t0,Xr2,MPFR_RNDN);
mpfr_sub(t0,t1,Xi2,MPFR_RNDN);
mpfr_add(t1,t0,Ci,MPFR_RNDN);
mpfr_set(Xin,t1,MPFR_RNDN);
LOOP }
mpfr_clear(t0);
mpfr_clear(t1);
#undef DLOOP



#undef LOOP
    mpfr_clear(Cr);
    mpfr_clear(Ci);
    mpfr_clear(Xr);
    mpfr_clear(Xi);
    mpfr_clear(Xr2);
    mpfr_clear(Xi2);
    mpfr_clear(Xrn);
    mpfr_clear(Xin);
    mpfr_clear(Ar);
    mpfr_clear(Ai);
    dr0 = dr; di0 = di;
    return true;
  }
  return false;
}

bool reference_double_0_3_ld(const int m_nFractalType, const int m_nPower, double *m_db_dxr, double *m_db_dxi, double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr0, const CFixedFloat &Ci0, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &antal, double &test1, double &test2, long double &dr0, long double &di0)
{
  if (m_nFractalType == 0 && m_nPower == 3)
  {
    bool stored = false;
    double old_absval = 0;
    double abs_val = 0;
    m_nGlitchIter = m_nMaxIter + 1;
    int nMaxIter = m_nMaxIter;
    int i;
    double glitch = 0.000001;
    if (m_bGlitchLowTolerance) {
      glitch = sqrt(glitch);
    }
    mp_bitcnt_t bits = mpfr_get_prec(Cr0.m_f.backend().data());
    mpfr_t Cr; mpfr_init2(Cr, bits); mpfr_set(Cr, Cr0.m_f.backend().data(), MPFR_RNDN);
    mpfr_t Ci; mpfr_init2(Ci, bits); mpfr_set(Ci, Ci0.m_f.backend().data(), MPFR_RNDN);
    mpfr_t Xr; mpfr_init2(Xr, bits); mpfr_set_d(Xr, g_SeedR, MPFR_RNDN);
    mpfr_t Xi; mpfr_init2(Xi, bits); mpfr_set_d(Xi, g_SeedI, MPFR_RNDN);
    double Xrd = mpfr_get_d(Xr, MPFR_RNDN);
    double Xid = mpfr_get_d(Xi, MPFR_RNDN);
    mpfr_t Xr2; mpfr_init2(Xr2, bits); mpfr_sqr(Xr2, Xr, MPFR_RNDN);
    mpfr_t Xi2; mpfr_init2(Xi2, bits); mpfr_sqr(Xi2, Xi, MPFR_RNDN);
    mpfr_t Xrn; mpfr_init2(Xrn, bits);
    mpfr_t Xin; mpfr_init2(Xin, bits);
    mpfr_t Ar; mpfr_init2(Ar, bits); mpfr_set_d(Ar, g_FactorAR, MPFR_RNDN);
    mpfr_t Ai; mpfr_init2(Ai, bits); mpfr_set_d(Ai, g_FactorAI, MPFR_RNDN);
    long double dr = dr0, di = di0;
    long double drn = 0, din = 0;


#define LOOP \
      dr = drn; di = din; \
      mpfr_set(Xr, Xrn, MPFR_RNDN); \
      mpfr_set(Xi, Xin, MPFR_RNDN); \
      mpfr_sqr(Xr2, Xr, MPFR_RNDN); \
      mpfr_sqr(Xi2, Xi, MPFR_RNDN); \
      m_nRDone++; \
      Xrd = mpfr_get_d(Xr, MPFR_RNDN); \
      Xid = mpfr_get_d(Xi, MPFR_RNDN); \
      old_absval = abs_val; \
      abs_val = g_real * Xrd * Xrd + g_imag * Xid * Xid; \
      const double Xz = abs_val * glitch; \
      m_db_dxr[i] = Xrd; \
      m_db_dxi[i] = Xid; \
      m_db_z[i] = Xz; \
      if (abs_val >= 4) \
      { \
        if (terminate == 4 && !stored) \
        { \
          stored = true; \
          antal = i; \
          test1 = abs_val; \
          test2 = old_absval; \
        } \
      } \
      if (abs_val >= terminate){ \
        if (terminate > 4 && !stored) \
        { \
          stored = true; \
          antal = i; \
          test1 = abs_val; \
          test2 = old_absval; \
        } \
        if (nMaxIter == m_nMaxIter){ \
          nMaxIter = i + 3; \
          if (nMaxIter > m_nMaxIter) \
            nMaxIter = m_nMaxIter; \
          m_nGlitchIter = nMaxIter; \
        } \
      }


#define DLOOP \
 \
        const double Xxr = Xrd; (void) Xxr; \
        const double Xxi = Xid; (void) Xxi; \
         \
        drn = 3 * (dr * (Xxr * Xxr - Xxi * Xxi) - di * (2 * Xxr * Xxi)) + 1; \
        din = 3 * (di * (Xxr * Xxr - Xxi * Xxi) + dr * (2 * Xxr * Xxi)); \
       \
       \


mpfr_t t0; mpfr_init2(t0, bits);
mpfr_t t1; mpfr_init2(t1, bits);
for (i = 0; i < nMaxIter && !m_bStop; i++) { DLOOP
mpfr_mul_ui(t0,Xi2,3,MPFR_RNDN);
mpfr_sub(t1,Xr2,t0,MPFR_RNDN);
mpfr_mul(t0,Xr,t1,MPFR_RNDN);
mpfr_add(t1,t0,Cr,MPFR_RNDN);
mpfr_set(Xrn,t1,MPFR_RNDN);
mpfr_mul_ui(t1,Xr2,3,MPFR_RNDN);
mpfr_sub(t0,t1,Xi2,MPFR_RNDN);
mpfr_mul(t1,t0,Xi,MPFR_RNDN);
mpfr_add(t0,t1,Ci,MPFR_RNDN);
mpfr_set(Xin,t0,MPFR_RNDN);
LOOP }
mpfr_clear(t0);
mpfr_clear(t1);
#undef DLOOP



#undef LOOP
    mpfr_clear(Cr);
    mpfr_clear(Ci);
    mpfr_clear(Xr);
    mpfr_clear(Xi);
    mpfr_clear(Xr2);
    mpfr_clear(Xi2);
    mpfr_clear(Xrn);
    mpfr_clear(Xin);
    mpfr_clear(Ar);
    mpfr_clear(Ai);
    dr0 = dr; di0 = di;
    return true;
  }
  return false;
}


void CFraktalSFT::CalculateReference()
{
	Precision prec(m_rref.m_f.precision());

	int i;
	if (m_db_dxr)
		delete[] m_db_dxr;
	m_db_dxr = new double [m_nMaxIter];
	if (m_db_dxi)
		delete[] m_db_dxi;
	m_db_dxi = new double [m_nMaxIter];
	if (m_db_z)
		delete[] m_db_z;
	m_db_z = new double [m_nMaxIter];

	CFixedFloat xr = g_SeedR, xi = g_SeedI, xin, xrn, sr = xr.Square(), si = xi.Square(), xrxid = 0;
	double terminate = SMOOTH_BAILOUT*SMOOTH_BAILOUT;

	if (m_nInflections)
	{
		int inf;
		complex<CFixedFloat> c(m_rref,m_iref);
		for(inf=m_nInflections-1;inf>=0;inf--){
			complex<CFixedFloat> d = c-m_pInflections[inf];
			c=m_pInflections[inf]+d*d;
		}
		m_rref=c.m_r;
		m_iref=c.m_i;
	}

	m_nGlitchIter = m_nMaxIter + 1;
	int nMaxIter = m_nMaxIter;

	int antal = 0;
	double test1 = 0;
	double test2 = 0;

  double dr = 1, di = 0;
  long double ldr = 1, ldi = 0;

  if (m_nScalingOffset && m_nFractalType == 0 && m_nPower == 2)
  {
		bool ok = reference_double_0_2_ld(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, m_rref, m_iref, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, GetGlitchLowTolerance(), antal, test1, test2, ldr, ldi);
		assert(ok && "reference_double_0_2_ld");
	}
  else if (m_nScalingOffset && m_nFractalType == 0 && m_nPower == 3)
  {
		bool ok = reference_double_0_3_ld(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, m_rref, m_iref, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, GetGlitchLowTolerance(), antal, test1, test2, ldr, ldi);
		assert(ok && "reference_double_0_3_ld");
	}
	else if (m_nFractalType == 0 && m_nPower > 10) // FIXME derivative
	{

		double threashold = 0.0001;
		for (i = 7; i <= m_nPower; i += 2)
			threashold *= 10;
		if (GetGlitchLowTolerance()) {
			threashold = sqrt(threashold);
		}
		if (threashold>.5)
			threashold = .5;
		complex<CFixedFloat> r(m_rref, m_iref);
		complex<CFixedFloat> X(g_SeedR, g_SeedI);
		bool stored = false;
		double old_absval = 0;
		double abs_val = 0;
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			X = (X^m_nPower) + r;
			m_db_dxr[i] = X.m_r.ToDouble();
			m_db_dxi[i] = X.m_i.ToDouble();
			old_absval = abs_val;
			abs_val = (g_real * m_db_dxr[i] * m_db_dxr[i] + g_imag * m_db_dxi[i] * m_db_dxi[i]);
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

		bool ok = reference_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, m_rref, m_iref, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, GetGlitchLowTolerance(), antal, test1, test2, dr, di);
		assert(ok && "reference_double");

	}

	long double pixel_spacing = m_lPixelSpacing;
	if (m_nScalingOffset)
	{
		ldr = ldr * pixel_spacing;
		ldi = ldi * pixel_spacing;
	}
	else
	{
		ldr = dr * pixel_spacing;
		ldi = di * pixel_spacing;
	}
	double de = sqrt(test1) * log(test1) / sqrt(ldr * ldr + ldi * ldi);

	if (0 <= g_nAddRefX && g_nAddRefX < m_nX && 0 <= g_nAddRefY && g_nAddRefY < m_nY)
		OutputIterationData(g_nAddRefX, g_nAddRefY, false, antal + 1, test1, test2, de);
}
