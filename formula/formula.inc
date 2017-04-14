#include "../fraktal_sft/CFixedFloat.h"
#include "../fraktal_sft/floatexp.h"
#include "../fraktal_sft/complex.h"

static long double ConvertFromFixedFloat(const CFixedFloat &f)
{
#ifdef KF_FLOAT_BACKEND_MPFR
  return mpfr_get_ld(f.m_f.backend().data(), MPFR_RNDN);
#else
  using std::ldexp;
  signed long int e = 0;
  long double l = mpf_get_d_2exp(&e, f.m_f.backend().data());
  l = ldexp(l, e);
  if ((mpf_sgn(f.m_f.backend().data()) >= 0) != (l >= 0)) l = -l; // workaround GMP bug
  return l;
#endif
}

bool FORMULA(reference_double,TYPE,POWER)(int m_nFractalType, int m_nPower, double *m_db_dxr, double *m_db_dxi, double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, double g_SeedR, double g_SeedI, double terminate, double g_real, double g_imag)
{
  if (m_nFractalType == TYPE && m_nPower == POWER)
  {
    CFixedFloat Xr = g_SeedR, Xi = g_SeedI, Xr2 = Xr.Square(), Xi2 = Xi.Square(), Xin, Xrn;
    m_nGlitchIter = m_nMaxIter + 1;
    int nMaxIter = m_nMaxIter;
    int i;
    for (i = 0; i<nMaxIter && !m_bStop; i++)
    {

#ifdef CREFERENCE
      complex<CFixedFloat> X(Xr, Xi), C(Cr, Ci), Xn;
      {CREFERENCE(double)}
      Xrn = Xn.m_r; Xin = Xn.m_i;
#else
      {REFERENCE(double)}
#endif

      Xr = Xrn;
      Xi = Xin;
      Xr2 = Xr.Square();
      Xi2 = Xi.Square();
      m_nRDone++;
      m_db_dxr[i] = Xr.ToDouble();
      m_db_dxi[i] = Xi.ToDouble();
      double abs_val = (g_real * m_db_dxr[i] * m_db_dxr[i] + g_imag * m_db_dxi[i] * m_db_dxi[i]);
      m_db_z[i] = abs_val*GLITCH;
      if (abs_val >= terminate) {
        if (nMaxIter == m_nMaxIter) {
          nMaxIter = i + 3;
          if (nMaxIter > m_nMaxIter)
            nMaxIter = m_nMaxIter;
          m_nGlitchIter = nMaxIter;
        }
      }
    }
    return true;
  }
  return false;
}

bool FORMULA(reference_long_double,TYPE,POWER)(int m_nFractalType, int m_nPower, long double *m_ldxr, long double *m_ldxi, double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, double g_SeedR, double g_SeedI, double terminate, double g_real, double g_imag)
{
  if (m_nFractalType == TYPE && m_nPower == POWER)
  {
    CFixedFloat Xr = g_SeedR, Xi = g_SeedI, Xr2 = Xr.Square(), Xi2 = Xi.Square(), Xin, Xrn;
    m_nGlitchIter = m_nMaxIter + 1;
    int nMaxIter = m_nMaxIter;
    int i;
		for (i = 0; i<nMaxIter && !m_bStop; i++)
    {

#ifdef CREFERENCE
      complex<CFixedFloat> X(Xr, Xi), C(Cr, Ci), Xn;
      {CREFERENCE(long double)}
      Xrn = Xn.m_r; Xin = Xn.m_i;
#else
      {REFERENCE(long double)}
#endif

      Xr = Xrn;
      Xi = Xin;
      Xr2 = Xr.Square();
      Xi2 = Xi.Square();
      m_nRDone++;
      m_ldxr[i] = ConvertFromFixedFloat(Xr);
      m_ldxi[i] = ConvertFromFixedFloat(Xi);
      long double abs_val = (g_real * m_ldxr[i] * m_ldxr[i] + g_imag * m_ldxi[i] * m_ldxi[i]);
      m_db_z[i] = abs_val*GLITCH;
      if (abs_val >= terminate) {
        if (nMaxIter == m_nMaxIter) {
          nMaxIter = i + 3;
          if (nMaxIter > m_nMaxIter)
            nMaxIter = m_nMaxIter;
          m_nGlitchIter = nMaxIter;
        }
      }
		}
    return true;
  }
  return false;
}

bool FORMULA(reference_floatexp,TYPE,POWER)(int m_nFractalType, int m_nPower, floatexp *m_dxr, floatexp *m_dxi, double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, double g_SeedR, double g_SeedI, double terminate, floatexp real, floatexp imag)
{
  if (m_nFractalType == TYPE && m_nPower == POWER)
  {
    CFixedFloat Xr = g_SeedR, Xi = g_SeedI, Xr2 = Xr.Square(), Xi2 = Xi.Square(), Xin, Xrn;
    m_nGlitchIter = m_nMaxIter + 1;
    int nMaxIter = m_nMaxIter;
    int i;
		for (i = 0; i<nMaxIter && !m_bStop; i++)
    {

#ifdef CREFERENCE
      complex<CFixedFloat> X(Xr, Xi), C(Cr, Ci), Xn;
      {CREFERENCE(floatexp)}
      Xrn = Xn.m_r; Xin = Xn.m_i;
#else
      {REFERENCE(floatexp)}
#endif

      Xr = Xrn;
      Xi = Xin;
      Xr2 = Xr.Square();
      Xi2 = Xi.Square();
      m_nRDone++;
			m_dxr[i] = Xr;
			m_dxi[i] = Xi;
			double abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val*0.0000001;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter > m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
    return true;
  }
  return false;
}

bool FORMULA(perturbation_double,TYPE,POWER)(int m_nFractalType, int m_nPower, const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, double g_real, double g_imag, double xr, double xi, const double cr, const double ci)
{
  if (m_nFractalType == TYPE && m_nPower == POWER)
  {
    for (; antal < nMaxIter && test1 <= m_nBailout2; antal++)
    {
      const double Xr = m_db_dxr[antal];
      const double Xi = m_db_dxi[antal];
      const double Xxr = Xr + xr;
      const double Xxi = Xi + xi;
      test2 = test1;
      test1 = g_real * Xxr * Xxr + g_imag * Xxi * Xxi;
      if (test1 < m_db_z[antal])
      {
        if (! m_bNoGlitchDetection)
          test1 = m_nBailout2 * 2;
        bGlitch = true;
      }
      double xrn, xin;

#ifdef CPERTURBATION
      complex<double> X(Xr, Xi), x(xr, xi), c(cr, ci), xn;
      {CPERTURBATION(double)}
      xrn = xn.m_r; xin = xn.m_i;
#else
      {PERTURBATION(double)}
#endif

      xr = xrn;
      xi = xin;
    }
    return true;
  }
  return false;
}

bool FORMULA(perturbation_long_double,TYPE,POWER)(int m_nFractalType, int m_nPower, const long double *dxr, const long double *dxi, const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, double g_real, double g_imag, long double xr, long double xi, const long double cr, const long double ci)
{
  if (m_nFractalType == TYPE && m_nPower == POWER)
  {
    for (; antal < nMaxIter && test1 <= m_nBailout2; antal++)
    {
      const long double Xr = dxr[antal];
      const long double Xi = dxi[antal];
      const long double Xxr = Xr + xr;
      const long double Xxi = Xi + xi;
      test2 = test1;
      test1 = g_real * Xxr * Xxr + g_imag * Xxi * Xxi;
      if (test1 < m_db_z[antal])
      {
        if (! m_bNoGlitchDetection)
          test1 = m_nBailout2 * 2;
        bGlitch = true;
      }
      long double xrn, xin;

#ifdef CPERTURBATION
      complex<long double> X(Xr, Xi), x(xr, xi), c(cr, ci), xn;
      {CPERTURBATION(long double)}
      xrn = xn.m_r; xin = xn.m_i;
#else
      {PERTURBATION(long double)}
#endif

      xr = xrn;
      xi = xin;
    }
    return true;
  }
  return false;
}

bool FORMULA(perturbation_floatexp,TYPE,POWER)(int m_nFractalType, int m_nPower, const floatexp *m_dxr, const floatexp *m_dxi, const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, floatexp real, floatexp imag, floatexp xr, floatexp xi, const floatexp cr, const floatexp ci)
{
  if (m_nFractalType == TYPE && m_nPower == POWER)
  {
		for (; antal<nMaxIter && test1 <= m_nBailout2; antal++)
    {
      const floatexp Xr = m_dxr[antal];
      const floatexp Xi = m_dxi[antal];
      const floatexp Xxr = Xr + xr;
      const floatexp Xxi = Xi + xi;
      test2 = test1;
      test1 = (real*Xxr*Xxr + imag*Xxi*Xxi).todouble();
      if (test1<m_db_z[antal]){
        if (!m_bNoGlitchDetection)
          test1 = m_nBailout2 * 2;
        bGlitch = true;
      }
      floatexp xrn, xin;

#ifdef CPERTURBATION
      complex<floatexp> X(Xr, Xi), x(xr, xi), c(cr, ci), xn;
      {CPERTURBATION(floatexp)}
      xrn = xn.m_r; xin = xn.m_i;
#else
      {PERTURBATION(floatexp)}
#endif

      xr = xrn;
      xi = xin;
    }
    return true;
  }
  return false;
}
