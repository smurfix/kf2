#include <windows.h>
#include <stdint.h>
#include "../fraktal_sft/floatexp.h"

static inline double d_add(const double a, const double b)
{
  return a + b;
}

static inline double d_sub(const double a, const double b)
{
  return a - b;
}

static inline double d_neg(const double a)
{
  return -a;
}

static inline double d_abs(const double a)
{
  return a < 0.0 ? -a : a;
}

static inline double d_imul(const int a, const double b)
{
  return a * b;
}

static inline double d_muli(const double a, const int b)
{
  return a * b;
}

static inline double d_mul(const double a, const double b)
{
  return a * b;
}

static inline double d_pow(const double a, const int b)
{
  double r = 1.0;
  for (int i = 0; i < b; ++i)
  {
    r *= a;
  }
  return r;
}

static inline int d_cmp(const double a, const double b)
{
  return ((int)(a > b)) - ((int)(a < b));
}

static inline double d_diffabs(const double c, const double d)
{
  double cd = c + d;
  if (c >= 0.0)
  {
    if (cd >= 0.0)
    {
      return d;
    }
    else
    {
      return -d - 2.0 * c;
    }
  }
  else
  {
    if (cd > 0.0)
    {
      return d + 2.0 * c;
    }
    else
    {
      return -d;
    }
  }
}

static inline long double ld_add(const long double a, const long double b)
{
  return a + b;
}

static inline long double ld_sub(const long double a, const long double b)
{
  return a - b;
}

static inline long double ld_neg(const long double a)
{
  return -a;
}

static inline long double ld_abs(const long double a)
{
  return a < 0.0 ? -a : a;
}

static inline long double ld_imul(const int a, const long double b)
{
  return a * b;
}

static inline long double ld_muli(const long double a, const int b)
{
  return a * b;
}

static inline long double ld_mul(const long double a, const long double b)
{
  return a * b;
}

static inline long double ld_pow(const long double a, const int b)
{
  long double r = 1.0;
  for (int i = 0; i < b; ++i)
  {
    r *= a;
  }
  return r;
}

static inline int ld_cmp(const long double a, const long double b)
{
  return ((int)(a > b)) - ((int)(a < b));
}

static inline long double ld_diffabs(const long double c, const long double d)
{
  long double cd = c + d;
  if (c >= 0.0L)
  {
    if (cd >= 0.0L)
    {
      return d;
    }
    else
    {
      return - d - 2.0L * c;
    }
  }
  else
  {
    if (cd > 0.0L)
    {
      return d + 2.0L * c;
    }
    else
    {
      return -d;
    }
  }
}

static inline int64_t as_long(double d)
{
  union {
    double d;
    int64_t i;
  } u;
  u.d = d;
  return u.i;
}

static inline double as_double(int64_t i)
{
  union {
    double d;
    int64_t i;
  } u;
  u.i = i;
  return u.d;
}

static inline double fe_double(floatexp f)
{
  if (f.exp < -1020)
  {
    return 0.0;
  }
  if (f.exp > 1020)
  {
    return f.val / 0.0;
  }
  return as_double((as_long(f.val) & 0x800FFFFFFFFFFFFFLL) | ((f.exp + 1023) << 52));
}

static inline floatexp fe_floatexp(const double val, const int64_t exp)
{
  int64_t f_exp = ((as_long(val) & 0x7FF0000000000000LL) >> 52) - 1023;
  double f_val = as_double((as_long(val) & 0x800FFFFFFFFFFFFFLL) | 0x3FF0000000000000LL);
  floatexp fe(f_val, f_exp + exp, 0);
  return fe;
}

static inline floatexp fe_abs(const floatexp f)
{
  floatexp fe(d_abs(f.val), f.exp, 0);
  return fe;
}

static inline floatexp fe_neg(const floatexp f)
{
  floatexp fe(-f.val, f.exp, 0);
  return fe;
}

static inline floatexp fe_muli(const floatexp a, const int b)
{
  return fe_floatexp(a.val * b, a.exp);
}

static inline floatexp fe_imul(const int a, const floatexp b)
{
  return fe_floatexp(a * b.val, b.exp);
}

static inline floatexp fe_mul(const floatexp a, const floatexp b)
{
  return fe_floatexp(a.val * b.val, a.exp + b.exp);
}

static inline floatexp fe_mul_2si(const floatexp a, const int64_t b)
{
  floatexp fe(a.val, a.exp + b, 0);
  return fe;
}

static inline floatexp fe_div(const floatexp a, const floatexp b)
{
  return fe_floatexp(a.val / b.val, a.exp - b.exp);
}

static inline floatexp fe_div_2si(const floatexp a, const int64_t b)
{
  floatexp fe(a.val, a.exp - b, 0);
  return fe;
}

static inline floatexp fe_add(const floatexp a, const floatexp b)
{
  if (a.exp > b.exp)
  {
    floatexp c(b.val, b.exp - a.exp, 0);
    return fe_floatexp(a.val + fe_double(c), a.exp);
  }
  else
  {
    floatexp c(a.val, a.exp - b.exp, 0);
    return fe_floatexp(fe_double(c) + b.val, b.exp);
  }
}

static inline floatexp fe_sub(const floatexp a, const floatexp b)
{
  return fe_add(a, fe_neg(b));
}

static inline int fe_cmp(const floatexp a, const floatexp b)
{
  if (a.val > 0)
  {
    if (b.val <= 0)
    {
      return 1;
    }
    else if (a.exp > b.exp)
    {
      return 1;
    }
    else if (a.exp < b.exp)
    {
      return -1;
    }
    else
    {
      return d_cmp(a.val, b.val);
    }
  }
  else
  {
    if (b.val > 0)
    {
      return -1;
    }
    else if (a.exp > b.exp)
    {
      return -1;
    }
    else if (a.exp < b.exp)
    {
      return 1;
    }
    else
    {
      return d_cmp(a.val, b.val);
    }
  }
}

static inline floatexp fe_diffabs(const floatexp c, const floatexp d)
{
  floatexp cd = fe_add(c, d);
  if (c.val >= 0.0)
  {
    if (cd.val >= 0.0)
    {
      return d;
    }
    else
    {
      return fe_neg(fe_add(d, fe_mul_2si(c, 1)));
    }
  }
  else
  {
    if (cd.val > 0.0)
    {
      return fe_add(d, fe_mul_2si(c, 1));
    }
    else
    {
      return fe_neg(d);
    }
  }
}

static inline floatexp fe_pow(const floatexp a, int b)
{
  floatexp r = fe_floatexp(1.0, 0);
  for (int i = 0; i < b; ++i)
  {
    r = fe_mul(r, a);
  }
  return r;
}

typedef struct
{
  double re;
  double im;
} dcomplex;

static inline dcomplex dc_neg(const dcomplex a)
{
  dcomplex dc = { -a.re, -a.im };
  return dc;
}

static inline dcomplex dc_add(const dcomplex a, const dcomplex b)
{
  dcomplex dc = { a.re + b.re, a.im + b.im };
  return dc;
}

static inline dcomplex dc_sub(const dcomplex a, const dcomplex b)
{
  dcomplex dc = { a.re - b.re, a.im - b.im };
  return dc;
}

static inline dcomplex dc_imul(const int a, const dcomplex b)
{
  dcomplex dc = { a * b.re, a * b.im };
  return dc;
}

static inline dcomplex dc_muli(const dcomplex a, const int b)
{
  dcomplex dc = { a.re * b, a.im * b };
  return dc;
}

static inline dcomplex dc_mul(const dcomplex a, const dcomplex b)
{
  dcomplex dc = { a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re };
  return dc;
}

static inline dcomplex dc_pow(const dcomplex a, const int b)
{
  dcomplex r = { 1.0, 0.0 };
  for (int i = 0; i < b; ++i)
  {
    r = dc_mul(r, a);
  }
  return r;
}

typedef struct
{
  long double re;
  long double im;
} ldcomplex;

static inline ldcomplex ldc_neg(const ldcomplex a)
{
  ldcomplex dc = { -a.re, -a.im };
  return dc;
}

static inline ldcomplex ldc_add(const ldcomplex a, const ldcomplex b)
{
  ldcomplex dc = { a.re + b.re, a.im + b.im };
  return dc;
}

static inline ldcomplex ldc_sub(const ldcomplex a, const ldcomplex b)
{
  ldcomplex dc = { a.re - b.re, a.im - b.im };
  return dc;
}

static inline ldcomplex ldc_imul(const int a, const ldcomplex b)
{
  ldcomplex dc = { a * b.re, a * b.im };
  return dc;
}

static inline ldcomplex ldc_muli(const ldcomplex a, const int b)
{
  ldcomplex dc = { a.re * b, a.im * b };
  return dc;
}

static inline ldcomplex ldc_mul(const ldcomplex a, const ldcomplex b)
{
  ldcomplex dc = { a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re };
  return dc;
}

static inline ldcomplex ldc_pow(const ldcomplex a, const int b)
{
  ldcomplex r = { 1.0, 0.0 };
  for (int i = 0; i < b; ++i)
  {
    r = ldc_mul(r, a);
  }
  return r;
}

typedef struct
{
  floatexp re;
  floatexp im;
} fecomplex;

static inline fecomplex fec_neg(const fecomplex a)
{
  fecomplex fec = { fe_neg(a.re), fe_neg(a.im) };
  return fec;
}

static inline fecomplex fec_add(const fecomplex a, const fecomplex b)
{
  fecomplex fec = { fe_add(a.re, b.re), fe_add(a.im, b.im) };
  return fec;
}

static inline fecomplex fec_sub(const fecomplex a, const fecomplex b)
{
  fecomplex fec = { fe_sub(a.re, b.re), fe_sub(a.im, b.im) };
  return fec;
}

static inline fecomplex fec_imul(const int a, const fecomplex b)
{
  fecomplex fec = { fe_imul(a, b.re), fe_imul(a, b.im) };
  return fec;
}

static inline fecomplex fec_muli(const fecomplex a, const int b)
{
  fecomplex fec = { fe_muli(a.re, b), fe_muli(a.im, b) };
  return fec;
}

static inline fecomplex fec_mul(const fecomplex a, const fecomplex b)
{
  fecomplex fec = { fe_sub(fe_mul(a.re, b.re), fe_mul(a.im, b.im)), fe_add(fe_mul(a.re, b.im), fe_mul(a.im, b.re)) };
  return fec;
}

static inline fecomplex fec_pow(const fecomplex a, const int b)
{
  fecomplex r = { fe_floatexp(1.0, 0), fe_floatexp(0.0, 0) };
  for (int i = 0; i < b; ++i)
  {
    r = fec_mul(r, a);
  }
  return r;
}

bool optimized_reference_double_0_2(int m_nFractalType, int m_nPower, double *m_db_dxr, double *m_db_dxi, double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr0, const CFixedFloat &Ci0, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag)
{
  if (m_nFractalType == 0 && m_nPower == 2)
  {
    mpf_t Cr, Ci, Xr, Xi, Xr2, Xi2, Xin, Xrn;
    mp_bitcnt_t bits = mpf_get_prec(Cr0.m_f.backend().data());
    mpf_init2(Cr, bits);
    mpf_init2(Ci, bits);
    mpf_init2(Xr, bits);
    mpf_init2(Xi, bits);
    mpf_init2(Xr2, bits);
    mpf_init2(Xi2, bits);
    mpf_init2(Xrn, bits);
    mpf_init2(Xin, bits);
    mpf_set(Cr, Cr0.m_f.backend().data());
    mpf_set(Ci, Ci0.m_f.backend().data());
    mpf_set_d(Xr, g_SeedR);
    mpf_set_d(Xi, g_SeedI);
    mpf_mul(Xr2, Xr, Xr);
    mpf_mul(Xi2, Xi, Xi);
    m_nGlitchIter = m_nMaxIter + 1;
    int nMaxIter = m_nMaxIter;
    int i;
    for (i = 0; i < nMaxIter && !m_bStop; i++)
    {
      mpf_sub(Xrn, Xr2, Xi2);
      mpf_add(Xin, Xr2, Xi2);
      mpf_add(Xr2, Xr, Xi);
      mpf_mul(Xi2, Xr2, Xr2);
      mpf_sub(Xr2, Xi2, Xin);
      mpf_add(Xr, Xrn, Cr);
      mpf_add(Xi, Xr2, Ci);
      mpf_mul(Xr2, Xr, Xr);
      mpf_mul(Xi2, Xi, Xi);
      m_nRDone++;
      const double Xrd = mpf_get_d(Xr);
      const double Xid = mpf_get_d(Xi);
      const double abs_val = g_real * Xrd * Xrd + g_imag * Xid * Xid;
      const double Xz = abs_val * 0.0000001;
      m_db_dxr[i] = Xrd;
      m_db_dxi[i] = Xid;
      m_db_z[i] = Xz;
      if (abs_val >= terminate) {
        if (nMaxIter == m_nMaxIter) {
          nMaxIter = i + 3;
          if (nMaxIter > m_nMaxIter)
            nMaxIter = m_nMaxIter;
          m_nGlitchIter = nMaxIter;
        }
      }
    }
    mpf_clear(Cr);
    mpf_clear(Ci);
    mpf_clear(Xr);
    mpf_clear(Xi);
    mpf_clear(Xr2);
    mpf_clear(Xi2);
    mpf_clear(Xrn);
    mpf_clear(Xin);
    return true;
  }
  return false;
}
