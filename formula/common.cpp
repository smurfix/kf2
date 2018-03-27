/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2018 Claude Heiland-Allen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#include <windows.h>
#include <stdint.h>
#include "../fraktal_sft/floatexp.h"
#include "formula.h"

template <typename T> T sgn(const T &a)
{
  return (a > 0) - (a < 0);
}

static inline double d_add(const double a, const double b)
{
  return a + b;
}

static inline double d_addi(const double a, const int b)
{
  return a + b;
}

static inline double d_sub(const double a, const double b)
{
  return a - b;
}

static inline double d_isub(const int a, const double b)
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

static inline double d_sgn(const double a)
{
  return (a > 0.0) - (a < 0.0);
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

static inline long double ld_addi(const long double a, const int b)
{
  return a + b;
}

static inline long double ld_sub(const long double a, const long double b)
{
  return a - b;
}

static inline long double ld_isub(const int a, const long double b)
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

static inline long double ld_sgn(const long double a)
{
  return (a > 0.0) - (a < 0.0);
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

static inline floatexp fe_sgn(const floatexp a)
{
  return floatexp((a.val > 0.0) - (a.val < 0.0));
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

static inline floatexp fe_addi(const floatexp a, const int b)
{
  return fe_add(a, fe_floatexp(double(b), 0));
}

static inline floatexp fe_sub(const floatexp a, const floatexp b)
{
  return fe_add(a, fe_neg(b));
}

static inline floatexp fe_isub(const int a, const floatexp b)
{
  return fe_addi(fe_neg(b), a);
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

static inline dcomplex dc_addi(const dcomplex a, const int b)
{
  dcomplex dc = { a.re + b, a.im };
  return dc;
}

static inline dcomplex dc_sub(const dcomplex a, const dcomplex b)
{
  dcomplex dc = { a.re - b.re, a.im - b.im };
  return dc;
}

static inline dcomplex dc_isub(const int a, const dcomplex b)
{
  dcomplex dc = { a - b.re, - b.im };
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

static inline ldcomplex ldc_addi(const ldcomplex a, const int b)
{
  ldcomplex ldc = { a.re + b, a.im };
  return ldc;
}

static inline ldcomplex ldc_sub(const ldcomplex a, const ldcomplex b)
{
  ldcomplex dc = { a.re - b.re, a.im - b.im };
  return dc;
}

static inline ldcomplex ldc_isub(const int a, const ldcomplex b)
{
  ldcomplex dc = { a - b.re, - b.im };
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

static inline fecomplex fec_addi(const fecomplex a, const int b)
{
  fecomplex fec = { fe_addi(a.re, b), a.im };
  return fec;
}

static inline fecomplex fec_sub(const fecomplex a, const fecomplex b)
{
  fecomplex fec = { fe_sub(a.re, b.re), fe_sub(a.im, b.im) };
  return fec;
}

static inline fecomplex fec_isub(const int a, const fecomplex b)
{
  fecomplex fec = { fe_isub(a, b.re), fe_neg(b.im) };
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

static inline bool gezero(const double &x) { return x >= 0.0; }
static inline bool gtzero(const double &x) { return x >  0.0; }
static inline bool gezero(const long double &x) { return x >= 0.0; }
static inline bool gtzero(const long double &x) { return x >  0.0; }
static inline bool gezero(const floatexp &x) { return x.val >= 0.0; }
static inline bool gtzero(const floatexp &x) { return x.val >  0.0; }

template <typename T>
static inline T diffabs(const T &c, const T &d)
{
  T cd = c + d;
  if (gezero(c))
  {
    if (gezero(cd))
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
    if (gtzero(cd))
    {
      return d + 2.0 * c;
    }
    else
    {
      return -d;
    }
  }
}

static inline long double ConvertFromFixedFloat(const CFixedFloat &f)
{
  using std::ldexp;
  signed long int e = 0;
  long double l = mpfr_get_ld_2exp(&e, f.m_f.backend().data(), MPFR_RNDN);
  l = ldexp(l, e);
  return l;
}

static inline double mpfr_get(const mpfr_t x, const double &t, mpfr_rnd_t rnd)
{
  (void) t;
  return mpfr_get_d(x, rnd);
}

static inline long double mpfr_get(const mpfr_t x, const long double &t, mpfr_rnd_t rnd)
{
  (void) t;
  return mpfr_get_ld(x, rnd);
}

static inline floatexp mpfr_get(const mpfr_t x, const floatexp &t, mpfr_rnd_t rnd)
{
  (void) t;
  signed long int e = 0;
  double d = mpfr_get_d_2exp(&e, x, rnd);
  return floatexp(d, e);
}
