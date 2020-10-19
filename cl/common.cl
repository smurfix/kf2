#pragma OPENCL EXTENSION cl_khr_fp64: enable

#define assert(n) do{}while(0)

double d_add(const double a, const double b)
{
  return a + b;
}

double d_sub(const double a, const double b)
{
  return a - b;
}

double d_neg(const double a)
{
  return -a;
}

double d_abs(const double a)
{
  return a < 0.0 ? -a : a;
}

double d_sgn(const double a)
{
  return a < 0.0 ? -1.0 : 1.0;
}

double d_imul(const int a, const double b)
{
  return a * b;
}

double d_muli(const double a, const int b)
{
  return a * b;
}

double d_mul(const double a, const double b)
{
  return a * b;
}

double d_pow(const double a, const int b)
{
  double r = 1.0;
  for (int i = 0; i < b; ++i)
  {
    r *= a;
  }
  return r;
}

int d_cmp(const double a, const double b)
{
  return ((int)(a > b)) - ((int)(a < b));
}

double d_diffabs(const double c, const double d)
{
  int s = d_cmp(c, 0);
  if (s > 0)
  {
    int t = d_cmp(c + d, 0.0);
    if (t >= 0)
    {
      return d;
    }
    else
    {
      return -d - 2 * c;
    }
  }
  else if (s < 0)
  {
    int t = d_cmp(c + d, 0.0);
    if (t > 0)
    {
      return d + 2 * c;
    }
    else
    {
      return -d;
    }
  }
  return d_abs(d);
}

typedef struct
{
  double val;
  long exp;
} floatexp;

double fe_double(floatexp f)
{
  if (f.exp < -1020)
  {
    return 0.0;
  }
  if (f.exp > 1020)
  {
    return f.val / 0.0;
  }
  return as_double((as_long(f.val) & 0x800FFFFFFFFFFFFFL) | ((f.exp + 1023) << 52));
}

floatexp fe_floatexp(const double val, const long exp)
{
  long f_exp = ((as_long(val) & 0x7FF0000000000000L) >> 52) - 1023;
  double f_val = as_double((as_long(val) & 0x800FFFFFFFFFFFFFL) | 0x3FF0000000000000L);
  floatexp fe = { f_val, f_exp + exp };
  return fe;
}

floatexp fe_abs(const floatexp f)
{
  floatexp fe = { d_abs(f.val), f.exp };
  return fe;
}

floatexp fe_sgn(const floatexp f)
{
  return fe_floatexp(d_sgn(f.val), 0);
}

floatexp fe_neg(const floatexp f)
{
  floatexp fe = { -f.val, f.exp };
  return fe;
}

floatexp fe_sqr(const floatexp a)
{
  return fe_floatexp(a.val * a.val, a.exp << 1);
}


floatexp fe_muld(const floatexp a, const double b)
{
  return fe_floatexp(a.val * b, a.exp);
}

floatexp fe_dmul(const double a, const floatexp b)
{
  return fe_floatexp(a * b.val, b.exp);
}

floatexp fe_muli(const floatexp a, const int b)
{
  return fe_floatexp(a.val * b, a.exp);
}

floatexp fe_imul(const int a, const floatexp b)
{
  return fe_floatexp(a * b.val, b.exp);
}

floatexp fe_mul(const floatexp a, const floatexp b)
{
  return fe_floatexp(a.val * b.val, a.exp + b.exp);
}

floatexp fe_mul_2si(const floatexp a, const long b)
{
  floatexp fe = { a.val, a.exp + b };
  return fe;
}

floatexp fe_div(const floatexp a, const floatexp b)
{
  return fe_floatexp(a.val / b.val, a.exp - b.exp);
}

floatexp fe_div_2si(const floatexp a, const long b)
{
  floatexp fe = { a.val, a.exp - b };
  return fe;
}

floatexp fe_add(const floatexp a, const floatexp b)
{
  if (a.exp > b.exp)
  {
    floatexp c = { b.val, b.exp - a.exp };
    return fe_floatexp(a.val + fe_double(c), a.exp);
  }
  else
  {
    floatexp c = { a.val, a.exp - b.exp };
    return fe_floatexp(fe_double(c) + b.val, b.exp);
  }
}

floatexp fe_sub(const floatexp a, const floatexp b)
{
  return fe_add(a, fe_neg(b));
}

int fe_cmp(const floatexp a, const floatexp b)
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

floatexp fe_diffabs(const floatexp c, const floatexp d)
{
  int s = d_cmp(c.val, 0.0);
  if (s > 0)
  {
    int t = d_cmp(fe_add(c, d).val, 0.0);
    if (t >= 0)
    {
      return d;
    }
    else
    {
      return fe_neg(fe_add(d, fe_mul_2si(c, 1)));
    }
  }
  else if (s < 0)
  {
    int t = d_cmp(fe_add(c, d).val, 0.0);
    if (t > 0)
    {
      return fe_add(d, fe_mul_2si(c, 1));
    }
    else
    {
      return fe_neg(d);
    }
  }
  return fe_abs(d);
}

floatexp fe_pow(const floatexp a, int b)
{
  floatexp r = fe_floatexp(1.0, 0);
  for (int i = 0; i < b; ++i)
  {
    r = fe_mul(r, a);
  }
  return r;
}

typedef struct {
  uint se;
  uint m;
} softfloat;

#define SF_EXPONENT_BIAS ((1U << 30U) - 1U)

#define SF_MANTISSA_BITS 32

bool sf_sign_bit(const softfloat f)
{
  return !!(f.se & 0x80000000U);
}

uint sf_biased_exponent(const softfloat f)
{
  return f.se & 0x7FFFFFFFU;
}

uint sf_mantissa(const softfloat f)
{
  return f.m;
}

bool sf_is_zero(const softfloat f)
{
  return
    sf_biased_exponent(f) == 0 &&
    sf_mantissa(f) == 0;
}

bool sf_is_denormal(const softfloat f)
{
  return
    sf_biased_exponent(f) == 0 &&
    sf_mantissa(f) != 0;
}

bool sf_is_inf(const softfloat f)
{
  return
    sf_biased_exponent(f) == 0x7FFFFFFFU &&
    sf_mantissa(f) == 0;
}

bool sf_is_nan(const softfloat f)
{
  return
    sf_biased_exponent(f) == 0x7FFFFFFFU &&
    sf_mantissa(f) != 0;
}

bool sf_lt(const softfloat a, const softfloat b)
{
  if (sf_is_nan(a) || sf_is_nan(b))
  {
    return false;
  }
  else if (sf_sign_bit(a) && ! sf_sign_bit(b))
  {
    return true;
  }
  else if (! sf_sign_bit(a) && sf_sign_bit(b))
  {
    return false;
  }
  else if (sf_biased_exponent(a) > sf_biased_exponent(b))
  {
    return sf_sign_bit(a);
  }
  else if (sf_biased_exponent(a) < sf_biased_exponent(b))
  {
    return ! sf_sign_bit(a);
  }
  else if (sf_mantissa(a) > sf_mantissa(b))
  {
    return sf_sign_bit(a);
  }
  else if (sf_mantissa(a) < sf_mantissa(b))
  {
    return ! sf_sign_bit(a);
  }
  else
  {
    // equal
    return false;
  }
}

bool sf_gt(const softfloat a, const softfloat b)
{
  return sf_lt(b, a);
}

float sf_to_float(const softfloat f)
{
  if (sf_is_zero(f) || sf_is_denormal(f))
  {
    if (sf_sign_bit(f)) return -0.0f; else return 0.0f;
  }
  else if (sf_is_inf(f))
  {
    if (sf_sign_bit(f)) return -1.0f/0.0f; else return 1.0f/0.0f;
  }
  else if (sf_is_nan(f))
  {
    if (sf_sign_bit(f)) return -(0.0f/0.0f); else return 0.0f/0.0f;
  }
  else
  {
    float x = sf_mantissa(f);
    int e
      = convert_int_sat((long)(sf_biased_exponent(f))
      - (SF_EXPONENT_BIAS + SF_MANTISSA_BITS));
    if (sf_sign_bit(f)) return -ldexp(x, e); else return ldexp(x, e);
  }
}

double sf_to_double(const softfloat f)
{
  if (sf_is_zero(f) || sf_is_denormal(f))
  {
    if (sf_sign_bit(f)) return -0.0; else return 0.0;
  }
  else if (sf_is_inf(f))
  {
    if (sf_sign_bit(f)) return -1.0/0.0; else return 1.0/0.0;
  }
  else if (sf_is_nan(f))
  {
    if (sf_sign_bit(f)) return -(0.0/0.0); else return 0.0/0.0;
  }
  else
  {
    double x = sf_mantissa(f);
    int e
      = convert_int_sat((long)(sf_biased_exponent(f))
      - (SF_EXPONENT_BIAS + SF_MANTISSA_BITS));
    if (sf_sign_bit(f)) return -ldexp(x, e); else return ldexp(x, e);
  }
}

softfloat sf_from_float(const float x)
{
  if (isnan(x))
  {
    softfloat f = { ((uint)(!!signbit(x)) << 31) | 0x7FFFFFFFU, 0xFFFFFFFFU };
    return f;
  }
  else if (isinf(x))
  {
    softfloat f = { ((uint)(!!signbit(x)) << 31) | 0x7FFFFFFFU, 0U };
    return f;
  }
  else if (x == 0.0f)
  {
    softfloat f = { ((uint)(!!signbit(x)) << 31) | 0U, 0U };
    return f;
  }
  else
  {
    int e;
    float y = frexp(fabs(x), &e);
    float z = ldexp(y, SF_MANTISSA_BITS);
    uint mantissa = convert_uint_rtz(z);
    uint biased_e = convert_uint_sat(e + SF_EXPONENT_BIAS);
    assert(0 < biased_e);
    assert(biased_e < 0x7FFFFFFFU);
    assert((mantissa >> (SF_MANTISSA_BITS - 1)) == 1U);
    softfloat f = { ((uint)(!!signbit(x)) << 31) | biased_e, mantissa };
    return f;
  }
}

softfloat sf_from_double(const double x)
{
  if (isnan(x))
  {
    softfloat f = { ((uint)(!!signbit(x)) << 31) | 0x7FFFFFFFU, 0xFFFFFFFFU };
    return f;
  }
  else if (isinf(x))
  {
    softfloat f = { ((uint)(!!signbit(x)) << 31) | 0x7FFFFFFFU, 0U };
    return f;
  }
  else if (x == 0.0)
  {
    softfloat f = { ((uint)(!!signbit(x)) << 31) | 0U, 0U };
    return f;
  }
  else
  {
    int e;
    double y = frexp(fabs(x), &e);
    double z = ldexp(y, SF_MANTISSA_BITS);
    uint mantissa = convert_uint_rtz(z); // rte might overflow rarely
    uint biased_e = convert_uint_sat(e + SF_EXPONENT_BIAS);
    assert(0 < biased_e);
    assert(biased_e < 0x7FFFFFFFU);
    assert((mantissa >> (SF_MANTISSA_BITS - 1)) == 1U);
    softfloat f = { ((uint)(!!signbit(x)) << 31) | biased_e, mantissa };
    return f;
  }
}

softfloat sf_ldexp(const softfloat a, int e)
{
  if (sf_is_zero(a) || sf_is_inf(a) || sf_is_nan(a))
  {
    return a;
  }
  else if (e >= (int)(0x7FFFFFFFU - sf_biased_exponent(a)))
  {
    // overflow to +/-infinity
    softfloat o = { (a.se & 0x80000000U) | 0x7FFFFFFFU, 0U };
    return o;
  }
  else if ((int)(sf_biased_exponent(a)) + e <= 0)
  {
    // underfloat to 0
    softfloat o = { (a.se & 0x80000000U) | 0U, 0U };
    return o;
  }
  else
  {
    softfloat o = { (a.se & 0x80000000U) | (sf_biased_exponent(a) + e), sf_mantissa(a) };
    return o;
  }
}

softfloat sf_from_floatexp(const floatexp x)
{
  return sf_ldexp(sf_from_double(x.val), convert_int_sat(x.exp));
}

softfloat sf_zero()
{
  softfloat o = { 0, 0 };
  return o;
}

softfloat sf_one()
{
  return sf_from_float(1.0f);
}

softfloat sf_abs(const softfloat a)
{
  softfloat o = { a.se & 0x7FFFFFFFU, a.m };
  return o;
}

softfloat sf_neg(const softfloat a)
{
  softfloat o = { a.se ^ 0x80000000U, a.m };
  return o;
}

softfloat sf_sgn(const softfloat a)
{
  return sf_sign_bit(a) ? sf_neg(sf_one()) : sf_one();
}

softfloat sf_sqr(const softfloat a)
{
  if (sf_biased_exponent(a) >= ((0x7FFFFFFFU >> 1) + (SF_EXPONENT_BIAS >> 1)))
  {
    // overflow to +infinity
    softfloat o = { 0x7FFFFFFFU, sf_is_nan(a) ? 0xFFFFFFFFU : 0U };
    return o;
  }
  else if (sf_biased_exponent(a) <= (SF_EXPONENT_BIAS >> 1) + 1)
  {
    // underflow to +0
    // FIXME handle denormals
    softfloat o = { 0U, 0U };
    return o;
  }
  else
  {
    ulong m = a.m;
    uint mantissa = (m * m) >> SF_MANTISSA_BITS;
    uint biased_e = ((a.se & 0x7FFFFFFFU) << 1) - SF_EXPONENT_BIAS;
    if ((mantissa & 0x80000000U) == 0)
    {
      mantissa <<= 1;
      biased_e -= 1;
    }
    assert(0 < biased_e);
    assert(biased_e < 0x7FFFFFFFU);
    assert((mantissa >> (SF_MANTISSA_BITS - 1)) == 1U);
    softfloat o = { biased_e, mantissa };
    return o;
  }
} 

softfloat sf_mul(const softfloat a, const softfloat b)
{
  if ( sf_is_nan(a) ||
       sf_is_nan(b) ||
       (sf_is_inf(a) && sf_is_zero(b)) ||
       (sf_is_zero(a) && sf_is_inf(b))
     )
  {
    // nan
    softfloat o = { ((a.se ^ b.se) & 0x80000000U) | 0x7FFFFFFFU, 0xFFFFFFFFU };
    return o;
  }
  else if (sf_is_zero(a) || sf_is_zero(b))
  {
    // zero
    softfloat o = { ((a.se ^ b.se) & 0x80000000U) | 0U, 0U };
    return o;
  }
  else if ((sf_biased_exponent(a) + sf_biased_exponent(b)) >= (0x7FFFFFFFU + SF_EXPONENT_BIAS))
  {
    // overflow to +/-infinity
    softfloat o = { ((a.se ^ b.se) & 0x80000000U) | 0x7FFFFFFFU, 0U };
    return o;
  }
  else if ((sf_biased_exponent(a) + sf_biased_exponent(b)) <= (SF_EXPONENT_BIAS + 1))
  {
    // underflow to +/-0
    // FIXME handle denormals
    softfloat o = { ((a.se ^ b.se) & 0x80000000U) | 0U, 0U };
    return o;
  }
  else
  {
    ulong ma = a.m;
    ulong mb = b.m;
    uint mantissa = (ma * mb) >> SF_MANTISSA_BITS;
    uint biased_e = ((a.se & 0x7FFFFFFFU) + (b.se & 0x7FFFFFFFU)) - SF_EXPONENT_BIAS;
    if ((mantissa & 0x80000000U) == 0)
    {
      mantissa <<= 1;
      biased_e -= 1;
    }
    assert(0 < biased_e);
    assert(biased_e < 0x7FFFFFFFU);
    assert((mantissa >> (SF_MANTISSA_BITS - 1)) == 1U);
    softfloat o = { ((a.se ^ b.se) & 0x80000000U) | biased_e, mantissa };
    return o;
  }
}

softfloat sf_muli(const softfloat a, const int b)
{
  return sf_mul(a, sf_from_float(b));
}

softfloat sf_imul(const int a, const softfloat b)
{
  return sf_mul(sf_from_float(a), b);
}

softfloat sf_mul_2si(const softfloat a, const int b)
{
  return sf_mul(a, sf_from_float(1 << b)); // FIXME
}

softfloat sf_add_a_gt_b_gt_0(const softfloat a, const softfloat b)
{
  // same sign addition, |a| > |b| or same exponent
  uint ea = sf_biased_exponent(a);
  uint eb = sf_biased_exponent(b);
  ulong ma = sf_mantissa(a);
  ulong mb = sf_mantissa(b);
  assert(ea >= eb);
  assert(sf_sign_bit(a) == sf_sign_bit(b));
  ulong mantissa = ma + (mb >> (ea - eb));
  uint biased_e = ea;
  if (!! (mantissa & 0x100000000LU))
  {
    biased_e += 1;
    mantissa >>= 1;
  }
  if (biased_e >= 0x7FFFFFFFU)
  {
    // overflow to +/-infinity
    softfloat o = { (a.se & 0x80000000U) | 0x7FFFFFFFU, 0U };
    return o;
  }
  assert(0 < biased_e);
  assert(biased_e < 0x7FFFFFFFU);
  assert((mantissa >> (SF_MANTISSA_BITS - 1)) == 1U);
  assert((mantissa >> SF_MANTISSA_BITS) == 0U);
  softfloat o = { biased_e, mantissa };
  if (sf_sign_bit(a)) return sf_neg(o); else return o;
}

softfloat sf_add_a_gt_0_gt_b(const softfloat a, const softfloat b)
{
  // opposite sign addition, a > 0 > b, |a| > |b|
  uint ea = sf_biased_exponent(a);
  uint eb = sf_biased_exponent(b);
  ulong ma = sf_mantissa(a);
  ulong mb = sf_mantissa(b);
  assert(ea > eb);
  assert(! sf_sign_bit(a));
  assert(  sf_sign_bit(b));
  // a > 0 > b, |a| > |b|
  long smantissa = (ma << 1) - ((mb << 1) >> (ea - eb));
  assert(smantissa > 0);
  ulong mantissa = smantissa;
  long biased_e = ea - 1;
  int shift = ((int)(clz(mantissa))) - SF_MANTISSA_BITS;
  if (shift > 0)
  {
    mantissa <<= shift;
    biased_e -= shift;
  }
  else if (shift < 0)
  {
    mantissa >>= -shift;
    biased_e += -shift;
  }
  if (biased_e >= 0x7FFFFFFFU)
  {
    // overflow to +infinity, impossible?
    softfloat o = { 0x7FFFFFFFU, 0U };
    return o;
  }
  else if (biased_e <= 0)
  {
    // underflow to +0
    softfloat o = { 0U, 0U };
    return o;
  }
  assert(0 < biased_e);
  assert(biased_e < 0x7FFFFFFFU);
  assert((mantissa >> (SF_MANTISSA_BITS - 1)) == 1U);
  assert((mantissa >> SF_MANTISSA_BITS) == 0U);
  softfloat o = { biased_e, (uint)(mantissa) };
  return o;
}

softfloat sf_add(const softfloat a, const softfloat b)
{
  if ( sf_is_nan(a) ||
       sf_is_nan(b) ||
       (sf_is_inf(a) && sf_is_inf(b) && !!((a.se ^ b.se) & 0x80000000U))
     )
  {
    // nan
    softfloat o = { 0x7FFFFFFFU, 0xFFFFFFFFU };
    return o;
  }
  else if (sf_is_zero(a))
  {
    return b;
  }
  else if (sf_is_zero(b))
  {
    return a;
  }
  else if (sf_is_inf(a))
  {
    return a;
  }
  else if (sf_is_inf(b))
  {
    return b;
  }
  else if (((a.se ^ b.se) & 0x80000000U) == 0)
  {
    // same sign addition
    uint ea = sf_biased_exponent(a);
    uint eb = sf_biased_exponent(b);
    if (ea > eb + SF_MANTISSA_BITS)
    {
      return a;
    }
    else if (eb > ea + SF_MANTISSA_BITS)
    {
      return b;
    }
    else if (ea >= eb)
    {
      return sf_add_a_gt_b_gt_0(a, b);
    }
    else
    {
      return sf_add_a_gt_b_gt_0(b, a);
    }
  }
  else
  {
    // opposite sign addition
    uint ea = sf_biased_exponent(a);
    uint eb = sf_biased_exponent(b);
    if (ea > eb + SF_MANTISSA_BITS)
    {
      return a;
    }
    else if (eb > ea + SF_MANTISSA_BITS)
    {
      return b;
    }
    else if (ea == eb)
    {
      uint ma = sf_mantissa(a);
      uint mb = sf_mantissa(b);
      if (ma > mb)
      {
        uint mantissa = ma - mb;
        uint shift = clz(mantissa);
        mantissa <<= shift;
        if (ea > shift)
        {
          uint biased_e = ea - shift;
          assert(0 < biased_e);
          assert(biased_e < 0x7FFFFFFFU);
          assert((mantissa >> (SF_MANTISSA_BITS - 1)) == 1U);
          softfloat o = { biased_e, mantissa };
          if (sf_sign_bit(a)) return sf_neg(o); else return o;
        }
        else
        {
          // FIXME handle denormals
          softfloat o = { 0U, 0U };
          return o;
        }
      }
      else if (mb > ma)
      {
        uint mantissa = mb - ma;
        uint shift = clz(mantissa);
        mantissa <<= shift;
        if (eb > shift)
        {
          uint biased_e = eb - shift;
          assert(0 < biased_e);
          assert(biased_e < 0x7FFFFFFFU);
          assert((mantissa >> (SF_MANTISSA_BITS - 1)) == 1U);
          softfloat o = { biased_e, mantissa };
          if (sf_sign_bit(b)) return sf_neg(o); else return o;
        }
        else
        {
          // FIXME handle denormals
          softfloat o = { 0U, 0U };
          return o;
        }
      }
      else
      {
        // cancels to 0
        softfloat o = { 0U, 0U };
        return o;
      }
    }
    else if (ea > eb)
    {
      // |a| > |b|
      if (sf_sign_bit(a))
      {
        return sf_neg(sf_add_a_gt_0_gt_b(sf_neg(a), sf_neg(b)));
      }
      else
      {
        return sf_add_a_gt_0_gt_b(a, b);
      }
    }
    else
    {
      // |b| > |a|
      if (sf_sign_bit(b))
      {
        return sf_neg(sf_add_a_gt_0_gt_b(sf_neg(b), sf_neg(a)));
      }
      else
      {
        return sf_add_a_gt_0_gt_b(b, a);
      }
    }
  }
}

softfloat sf_sub(const softfloat a, const softfloat b)
{
  return sf_add(a, sf_neg(b));
}

int sf_cmp(const softfloat a, const softfloat b)
{
  return ((int)sf_gt(a, b)) - ((int)sf_lt(a, b));
}

softfloat sf_diffabs(const softfloat c, const softfloat d)
{
  int s = sf_cmp(c, sf_zero());
  if (s > 0)
  {
    int t = sf_cmp(sf_add(c, d), sf_zero());
    if (t >= 0)
    {
      return d;
    }
    else
    {
      return sf_neg(sf_add(d, sf_mul_2si(c, 1)));
    }
  }
  else if (s < 0)
  {
    int t = sf_cmp(sf_add(c, d), sf_zero());
    if (t > 0)
    {
      return sf_add(d, sf_mul_2si(c, 1));
    }
    else
    {
      return sf_neg(d);
    }
  }
  return sf_abs(d);
}

typedef struct
{
  double x;
  double dx[2];
} duald;

duald duald_add(duald a, duald b)
{
  duald r = { a.x + b.x, a.dx[0] + b.dx[0], a.dx[1] + b.dx[1] };
  return r;
}

duald duald_dadd(double a, duald b)
{
  duald r = { a + b.x, b.dx[0], b.dx[1] };
  return r;
}

duald duald_addd(duald a, double b)
{
  duald r = { a.x + b, a.dx[0], a.dx[1] };
  return r;
}

duald duald_sub(duald a, duald b)
{
  duald r = { a.x -b.x, a.dx[0] - b.dx[0], a.dx[1] - b.dx[1] };
  return r;
}

duald duald_dsub(double a, duald b)
{
  duald r = { a - b.x, -b.dx[0], -b.dx[1] };
  return r;
}

duald duald_subd(duald a, double b)
{
  duald r = { a.x - b, a.dx[0], a.dx[1] };
  return r;
}

duald duald_mul(duald a, duald b)
{
  duald r = { a.x * b.x, a.dx[0] * b.x + a.x * b.dx[0], a.dx[1] * b.x + a.x * b.dx[1] };
  return r;
}

duald duald_dmul(double a, duald b)
{
  duald r = { a * b.x, a * b.dx[0], a * b.dx[1] };
  return r;
}

duald duald_muld(duald a, double b)
{
  duald r = { a.x * b, a.dx[0] * b, a.dx[1] * b };
  return r;
}

duald duald_sqr(duald a)
{
  duald r = { a.x * a.x, 2.0 * a.dx[0] * a.x, 2.0 * a.dx[1] * a.x };
  return r;
}

duald duald_div(duald a, duald b)
{
  duald r;
  r.x = a.x * b.x;
  double den = 1.0 / (b.x * b.x);
  r.dx[0] = (a.dx[0] * b.x - a.x * b.dx[0]) * den;
  r.dx[1] = (a.dx[1] * b.x - a.x * b.dx[1]) * den;
  return r;
}

duald duald_divd(duald a, double b)
{
  duald r = { a.x / b, a.dx[0] / b, a.dx[1] / b };
  return r;
}

bool duald_lt(duald a, duald b)
{
  return a.x < b.x;
}

bool duald_ltd(duald a, double b)
{
  return a.x < b;
}

bool duald_gt(duald a, duald b)
{
  return a.x > b.x;
}

bool duald_gtd(duald a, double b)
{
  return a.x > b;
}

bool duald_le(duald a, duald b)
{
  return a.x <= b.x;
}

bool duald_led(duald a, double b)
{
  return a.x <= b;
}

bool duald_ge(duald a, duald b)
{
  return a.x >= b.x;
}

bool duald_ged(duald a, double b)
{
  return a.x >= b;
}

duald duald_neg(duald a)
{
  duald r = { -a.x, -a.dx[0], -a.dx[1] };
  return r;
}

duald duald_abs(duald a)
{
  return a.x < 0.0 ? duald_neg(a) : a;
}

duald duald_exp(duald a)
{
  duald r;
  r.x = exp(a.x);
  r.dx[0] = a.dx[0] * r.x;
  r.dx[1] = a.dx[1] * r.x;
  return r;
}

duald duald_cos(duald a)
{
  duald r;
  r.x = cos(a.x);
  double s = -sin(a.x);
  r.dx[0] = a.dx[0] * s;
  r.dx[1] = a.dx[1] * s;
  return r;
}

duald duald_sin(duald a)
{
  duald r;
  r.x = sin(a.x);
  double c = cos(a.x);
  r.dx[0] = a.dx[0] * c;
  r.dx[1] = a.dx[1] * c;
  return r;
}

duald duald_ddiffabs(double c, duald d)
{
  const double cd = c + d.x;
  const duald c2d = duald_dadd(2 * c, d);
  return c >= 0.0 ? cd >= 0.0 ? d : duald_neg(c2d) : cd > 0.0 ? c2d : duald_neg(d);
}

duald duald_diffabs(duald c, duald d)
{
  const double cd = c.x + d.x;
  const duald c2d = duald_add(duald_dmul(2, c), d);
  return c.x >= 0.0 ? cd >= 0.0 ? d : duald_neg(c2d) : cd > 0.0 ? c2d : duald_neg(d);
}

typedef struct
{
  double re;
  double im;
} dcomplex;

double dc_norm(const dcomplex a)
{
  return a.re * a.re + a.im * a.im;
}

double dc_abs(const dcomplex a)
{
  return sqrt(dc_norm(a));
}

dcomplex dc_neg(const dcomplex a)
{
  dcomplex dc = { -a.re, -a.im };
  return dc;
}

dcomplex dc_add(const dcomplex a, const dcomplex b)
{
  dcomplex dc = { a.re + b.re, a.im + b.im };
  return dc;
}

dcomplex dc_sub(const dcomplex a, const dcomplex b)
{
  dcomplex dc = { a.re - b.re, a.im - b.im };
  return dc;
}

dcomplex dc_imul(const int a, const dcomplex b)
{
  dcomplex dc = { a * b.re, a * b.im };
  return dc;
}

dcomplex dc_muli(const dcomplex a, const int b)
{
  dcomplex dc = { a.re * b, a.im * b };
  return dc;
}

dcomplex dc_mul(const dcomplex a, const dcomplex b)
{
  dcomplex dc = { a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re };
  return dc;
}

dcomplex dc_ddiv(const double a, const dcomplex b)
{
  double b2 = dc_norm(b);
  dcomplex dc = { (a * b.re) / b2, (-a * b.im) / b2 };
  return dc;
}

dcomplex dc_div(const dcomplex a, const dcomplex b)
{
  double b2 = dc_norm(b);
  dcomplex dc = { (a.re * b.re + a.im * b.im) / b2, (-a.re * b.im + a.im * b.re) / b2 };
  return dc;
}

dcomplex dc_pow(const dcomplex a, const int b)
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
  floatexp re;
  floatexp im;
} fecomplex;

fecomplex fec_neg(const fecomplex a)
{
  fecomplex fec = { fe_neg(a.re), fe_neg(a.im) };
  return fec;
}

fecomplex fec_add(const fecomplex a, const fecomplex b)
{
  fecomplex fec = { fe_add(a.re, b.re), fe_add(a.im, b.im) };
  return fec;
}

fecomplex fec_sub(const fecomplex a, const fecomplex b)
{
  fecomplex fec = { fe_sub(a.re, b.re), fe_sub(a.im, b.im) };
  return fec;
}

fecomplex fec_imul(const int a, const fecomplex b)
{
  fecomplex fec = { fe_imul(a, b.re), fe_imul(a, b.im) };
  return fec;
}

fecomplex fec_muli(const fecomplex a, const int b)
{
  fecomplex fec = { fe_muli(a.re, b), fe_muli(a.im, b) };
  return fec;
}

fecomplex fec_mul(const fecomplex a, const fecomplex b)
{
  fecomplex fec = { fe_sub(fe_mul(a.re, b.re), fe_mul(a.im, b.im)), fe_add(fe_mul(a.re, b.im), fe_mul(a.im, b.re)) };
  return fec;
}

fecomplex fec_pow(const fecomplex a, const int b)
{
  fecomplex r = { fe_floatexp(1.0, 0), fe_floatexp(0.0, 0) };
  for (int i = 0; i < b; ++i)
  {
    r = fec_mul(r, a);
  }
  return r;
}

typedef struct
{
  softfloat re;
  softfloat im;
} sfcomplex;

sfcomplex sfc_neg(const sfcomplex a)
{
  sfcomplex sfc = { sf_neg(a.re), sf_neg(a.im) };
  return sfc;
}

sfcomplex sfc_add(const sfcomplex a, const sfcomplex b)
{
  sfcomplex sfc = { sf_add(a.re, b.re), sf_add(a.im, b.im) };
  return sfc;
}

sfcomplex sfc_sub(const sfcomplex a, const sfcomplex b)
{
  sfcomplex sfc = { sf_sub(a.re, b.re), sf_sub(a.im, b.im) };
  return sfc;
}

sfcomplex sfc_imul(const int a, const sfcomplex b)
{
  sfcomplex sfc = { sf_imul(a, b.re), sf_imul(a, b.im) };
  return sfc;
}

sfcomplex sfc_muli(const sfcomplex a, const int b)
{
  sfcomplex sfc = { sf_muli(a.re, b), sf_muli(a.im, b) };
  return sfc;
}

sfcomplex sfc_mul(const sfcomplex a, const sfcomplex b)
{
  sfcomplex sfc = { sf_sub(sf_mul(a.re, b.re), sf_mul(a.im, b.im)), sf_add(sf_mul(a.re, b.im), sf_mul(a.im, b.re)) };
  return sfc;
}

sfcomplex sfc_pow(const sfcomplex a, const int b)
{
  sfcomplex r = { sf_one(), sf_zero() };
  for (int i = 0; i < b; ++i)
  {
    r = sfc_mul(r, a);
  }
  return r;
}


typedef struct
{
  duald re;
  duald im;
} dualdcomplex;

double dualdc_norm(const dualdcomplex a)
{
  return a.re.x * a.re.x + a.im.x * a.im.x;
}

double dualdc_abs(const dualdcomplex a)
{
  return sqrt(dualdc_norm(a));
}

dualdcomplex dualdc_conj(const dualdcomplex a)
{
  dualdcomplex r = { a.re, duald_neg(a.im) };
  return r;
}

dualdcomplex dualdc_neg(const dualdcomplex a)
{
  dualdcomplex dc = { duald_neg(a.re), duald_neg(a.im) };
  return dc;
}

dualdcomplex dualdc_add(const dualdcomplex a, const dualdcomplex b)
{
  dualdcomplex dc = { duald_add(a.re, b.re), duald_add(a.im, b.im) };
  return dc;
}

dualdcomplex dualdc_dcadd(const dcomplex a, const dualdcomplex b)
{
  dualdcomplex dc = { duald_dadd(a.re, b.re), duald_dadd(a.im, b.im) };
  return dc;
}

dualdcomplex dualdc_sub(const dualdcomplex a, const dualdcomplex b)
{
  dualdcomplex dc = { duald_sub(a.re, b.re), duald_sub(a.im, b.im) };
  return dc;
}

dualdcomplex dualdc_dmul(const double a, const dualdcomplex b)
{
  dualdcomplex dc = { duald_dmul(a, b.re), duald_dmul(a, b.im) };
  return dc;
}

dualdcomplex dualdc_muld(const dualdcomplex a, const double b)
{
  dualdcomplex dc = { duald_muld(a.re, b), duald_muld(a.im, b) };
  return dc;
}

dualdcomplex dualdc_mul(const dualdcomplex a, const dualdcomplex b)
{
  dualdcomplex dc =
    { duald_sub(duald_mul(a.re, b.re), duald_mul(a.im, b.im))
    , duald_add(duald_mul(a.re, b.im), duald_mul(a.im, b.re))
    };
  return dc;
}

dualdcomplex dualdc_dcmul(const dcomplex a, const dualdcomplex b)
{
  dualdcomplex dc =
    { duald_sub(duald_dmul(a.re, b.re), duald_dmul(a.im, b.im))
    , duald_add(duald_dmul(a.re, b.im), duald_dmul(a.im, b.re))
    };
  return dc;
}

dualdcomplex dualdc_muldc(const dualdcomplex a, const dcomplex b)
{
  dualdcomplex dc =
    { duald_sub(duald_muld(a.re, b.re), duald_muld(a.im, b.im))
    , duald_add(duald_muld(a.re, b.im), duald_muld(a.im, b.re))
    };
  return dc;
}

#if 0
dualdcomplex dualdc_ddiv(const double a, const dualdcomplex b) // FIXME verify
{
  double b2 = dualdc_norm(b);
  dualdcomplex dc =
    { duald_divd(duald_dmul(a, b.re), b2)
    , duald_divd(duald_dmul(-a, b.im), b2)
    };
  return dc;
}

dualdcomplex dualdc_div(const dualdcomplex a, const dualdcomplex b) // FIXME verify
{
  double b2 = dualdc_norm(b);
  dualdcomplex dc = dualdc_divd(dualdc_mul(a, dualdc_conj(b)), b2);
  return dc;
}
#endif

dualdcomplex dualdc_pow(const dualdcomplex a, const int b)
{
  dualdcomplex r = { { 1.0, 0.0, 0.0 }, { 0.0, 0.0, 0.0 } };
  for (int i = 0; i < b; ++i)
  {
    r = dualdc_mul(r, a);
  }
  return r;
}

typedef struct
{
  double cr;
  double ci;
  double xr;
  double xi;
  double dr;
  double di;
  double daa;
  double dab;
  double dba;
  double dbb;
  double test1;
  double test2;
  long antal;
  int bGlitch;
  double log_m_nPower;
} p_status_d;

typedef struct
{
  floatexp cr;
  floatexp ci;
  floatexp xr;
  floatexp xi;
  floatexp dr;
  floatexp di;
  floatexp daa;
  floatexp dab;
  floatexp dba;
  floatexp dbb;
  double test1;
  double test2;
  long antal;
  int bGlitch;
  double log_m_nPower;
} p_status_fe;

typedef struct
{
  softfloat cr;
  softfloat ci;
  softfloat xr;
  softfloat xi;
  softfloat dr;
  softfloat di;
  softfloat daa;
  softfloat dab;
  softfloat dba;
  softfloat dbb;
  softfloat test1;
  softfloat test2;
  long antal;
  int bGlitch;
  softfloat log_m_nPower;
} p_status_sf;


typedef struct __attribute__((packed))
{
  long BYTES;
  // for pixel -> parameter mapping
  int m_nX;
  int m_nY;
  uint JitterSeed;
  int JitterShape;
  double JitterScale;
  floatexp m_pixel_center_x;
  floatexp m_pixel_center_y;
  floatexp m_pixel_scale;
  double transform00;
  double transform01;
  double transform10;
  double transform11;
  // for result -> output mapping
  long stride_y;
  long stride_x;
  long stride_offset;
  // for iteration control
  double m_nBailout;
  double m_nBailout2;
  double log_m_nBailout;
  double log_m_nPower;
  long m_nGlitchIter;
  long m_nMaxIter;
  long nMaxIter;
  long nMinIter;
  short m_bNoGlitchDetection;
  short derivatives;
  short m_bAddReference;
  short m_nSmoothMethod;
  double g_real;
  double g_imag;
  double norm_p;
  double g_FactorAR;
  double g_FactorAI;
  double m_epsilon;
  // for series approximation
  double m_dPixelSpacing;
  floatexp m_fPixelSpacing;
  long m_nMaxApproximation;
  int m_nApproxTerms;
  int approximation_type;
  // for hybrid
  short hybrid_loop_start;
  short hybrid_nstanzas;
  int hybrid_repeats[MAX_HYBRID_STANZAS];
  int hybrid_log_powers[MAX_HYBRID_STANZAS];
  // 130kB data follows
  floatexp m_APr[MAX_APPROX_TERMS + 1];
  floatexp m_APi[MAX_APPROX_TERMS + 1];
  floatexp m_APs_s[MAX_APPROX_TERMS + 1][MAX_APPROX_TERMS + 1];
  floatexp m_APs_t[MAX_APPROX_TERMS + 1][MAX_APPROX_TERMS + 1];
} p_config;

uint burtle_hash(uint a)
{
    a = (a+0x7ed55d16) + (a<<12);
    a = (a^0xc761c23c) ^ (a>>19);
    a = (a+0x165667b1) + (a<<5);
    a = (a+0xd3a2646c) ^ (a<<9);
    a = (a+0xfd7046c5) + (a<<3);
    a = (a^0xb55a4f09) ^ (a>>16);
    return a;
}

// uniform in [0,1)
double dither(uint x, uint y, uint c)
{
  return
    burtle_hash(x +
    burtle_hash(y +
    burtle_hash(c))) / (double) (0x100000000L);
}

void GetPixelOffset
( __global const p_config *g
, const int i
, const int j
, double *x
, double *y
)
{
  uint c = g->JitterSeed;
  if (c != 0)
  {
    double s = g->JitterScale;
    double u = dither(i, j, 2 * c + 0);
    double v = dither(i, j, 2 * c + 1);
    switch (g->JitterShape)
    {
      default:
      case 0: // uniform
        {
          *x = s * (u - 0.5);
          *y = s * (v - 0.5);
        }
        break;
      case 1: // Gaussian
        {
          // FIXME cache slow trig functions for every pixel for every image?
          // https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
          double r = 0 < u && u < 1 ? sqrt(-2 * log(u)) : 0;
          double t = 2 * 3.141592653589793 * v;
          s *= 0.5;
          *x = s * r * cos(t);
          *y = s * r * sin(t);
        }
        break;
    }
  }
  else
  {
    *x = 0.0;
    *y = 0.0;
  }
}

void GetPixelCoordinates
( __global const p_config *g
, const int i
, const int j
, floatexp *x
, floatexp *y
)
{
  double di = 0;
  double dj = 0;
  GetPixelOffset(g, i, j, &di, &dj);
  double u = i - g->m_nX/2 + di;
  double v = j - g->m_nY/2 + dj;
  double p = g->transform00 * u + g->transform01 * v;
  double q = g->transform10 * u + g->transform11 * v;
  *x = fe_add(g->m_pixel_center_x, fe_muld(g->m_pixel_scale, p));
  *y = fe_add(g->m_pixel_center_y, fe_muld(g->m_pixel_scale, q));
}

void GetPixelCoordinatesM
( __global const p_config *g
, const int i
, const int j
, floatexp *x
, floatexp *y
, floatexp *daa
, floatexp *dab
, floatexp *dba
, floatexp *dbb
)
{
  double di = 0;
  double dj = 0;
  GetPixelOffset(g, i, j, &di, &dj);
  double u = i - g->m_nX/2 + di;
  double v = j - g->m_nY/2 + dj;
  double p = g->transform00 * u + g->transform01 * v;
  double q = g->transform10 * u + g->transform11 * v;
  *x = fe_add(g->m_pixel_center_x, fe_muld(g->m_pixel_scale, p));
  *y = fe_add(g->m_pixel_center_y, fe_muld(g->m_pixel_scale, q));
  *daa = fe_muld(g->m_pixel_scale, g->transform00);
  *dab = fe_muld(g->m_pixel_scale, g->transform01);
  *dba = fe_muld(g->m_pixel_scale, g->transform10);
  *dbb = fe_muld(g->m_pixel_scale, g->transform11);
}

void DoApproximationD
( __global const p_config *g
, long *antal
, const floatexp D0r
, const floatexp D0i
, floatexp *oTDnr
, floatexp *oTDni
, floatexp *oTDDnr
, floatexp *oTDDni
)
{
  const floatexp zero = fe_floatexp(0.0, 0);
  if (g->m_nMaxApproximation)
  {
    floatexp TDnr = zero;
    floatexp TDni = zero;
    floatexp TDDnr = zero;
    floatexp TDDni = zero;
    for (int k = g->m_nApproxTerms - 1; k >= 0; --k)
    {
      floatexp tr = fe_add(fe_sub(fe_mul(TDnr, D0r), fe_mul(TDni, D0i)), g->m_APr[k]);
      floatexp ti = fe_add(fe_add(fe_mul(TDnr, D0i), fe_mul(TDni, D0r)), g->m_APi[k]);
      TDnr = tr;
      TDni = ti;
      tr = fe_add(fe_sub(fe_mul(TDDnr, D0r), fe_mul(TDDni, D0i)), fe_muli(g->m_APr[k], k + 1));
      ti = fe_add(fe_add(fe_mul(TDDnr, D0i), fe_mul(TDDni, D0r)), fe_muli(g->m_APi[k], k + 1));
      TDDnr = tr;
      TDDni = ti;
    }
    floatexp tr = fe_sub(fe_mul(TDnr, D0r), fe_mul(TDni, D0i));
    floatexp ti = fe_add(fe_mul(TDnr, D0i), fe_mul(TDni, D0r));
    TDnr = tr;
    TDni = ti;
    *antal = g->m_nMaxApproximation - 1;
    *oTDnr = TDnr;
    *oTDni = TDni;
    *oTDDnr = TDDnr;
    *oTDDni = TDDni;
  }
  else
  {
    *antal = 0;
    *oTDnr = D0r;
    *oTDni = D0i;
    *oTDDnr = fe_floatexp(1.0, 0);
    *oTDDni = zero;
  }
}

void DoApproximationM
( __global const p_config *g
, long *antal
, const floatexp a
, const floatexp b
, floatexp *x
, floatexp *y
, floatexp *dxa
, floatexp *dxb
, floatexp *dya
, floatexp *dyb
)
{
    const floatexp zero = fe_floatexp(0.0, 0);
    const floatexp one  = fe_floatexp(1.0, 0);
    if (g->approximation_type == 1)
    {
      floatexp dx, dy;
      DoApproximationD(g, antal, a, b, x, y, &dx, &dy);
      // Cauchy-Riemann
      *dxa = dx;
      *dxb = fe_neg(dy);
      *dya = dy;
      *dyb = dx;
    }
    else if (g->approximation_type == 2)
    {
      if (g->m_nMaxApproximation)
      {
        floatexp an[MAX_APPROX_TERMS + 1];
        floatexp bn[MAX_APPROX_TERMS + 1];
        floatexp dan[MAX_APPROX_TERMS + 1];
        floatexp dbn[MAX_APPROX_TERMS + 1];
        an[0] = one;
        bn[0] = one;
        dan[0] = zero;
        dbn[0] = zero;
        for (int i = 1; i <= g->m_nApproxTerms; ++i)
        {
          an[i] = fe_mul(an[i - 1], a);
          bn[i] = fe_mul(bn[i - 1], b);
          dan[i] = fe_muli(an[i - 1], i);
          dbn[i] = fe_muli(bn[i - 1], i);
        }
        // x   = sum ApproxSeriesR2X[i][j]   a^i       b^j
        // dxa = sum ApproxSeriesR2X[i][j] i a^{i-1}   b^j
        // dxb = sum ApproxSeriesR2X[i][j]   a^i     j b^{j-1}
        floatexp totalX = zero;
        floatexp totalY = zero;
        floatexp totalDXA = zero;
        floatexp totalDXB = zero;
        floatexp totalDYA = zero;
        floatexp totalDYB = zero;
        for (int i = 0; i <= g->m_nApproxTerms; ++i)
        {
          floatexp subtotalX = zero;
          floatexp subtotalY = zero;
          floatexp subtotalDXB = zero;
          floatexp subtotalDYB = zero;
          for (int j = 0; j <= g->m_nApproxTerms - i; ++j)
          {
            subtotalX   = fe_add(subtotalX,   fe_mul(g->m_APs_s[i][j],  bn[j]));
            subtotalY   = fe_add(subtotalY,   fe_mul(g->m_APs_t[i][j],  bn[j]));
            subtotalDXB = fe_add(subtotalDXB, fe_mul(g->m_APs_s[i][j], dbn[j]));
            subtotalDYB = fe_add(subtotalDYB, fe_mul(g->m_APs_t[i][j], dbn[j]));
          }
          totalX   = fe_add(totalX,   fe_mul(subtotalX  ,  an[i]));
          totalY   = fe_add(totalY,   fe_mul(subtotalY  ,  an[i]));
          totalDXA = fe_add(totalDXA, fe_mul(subtotalX  , dan[i]));
          totalDYA = fe_add(totalDYA, fe_mul(subtotalY  , dan[i]));
          totalDXB = fe_add(totalDXB, fe_mul(subtotalDXB,  an[i]));
          totalDYB = fe_add(totalDYB, fe_mul(subtotalDYB,  an[i]));
        }
        *antal = g->m_nMaxApproximation - 1;
        *x   = totalX;
        *y   = totalY;
        *dxa = totalDXA;
        *dxb = totalDXB;
        *dya = totalDYA;
        *dyb = totalDYB;
      }
      else
      {
        *antal = 0;
        *x = a;
        *y = b;
        *dxa = one;
        *dxb = zero;
        *dya = zero;
        *dyb = one;
      }
    }
    else
    {
      *antal = 0;
      *x = a;
      *y = b;
      *dxa = one;
      *dxb = zero;
      *dya = zero;
      *dyb = one;
    }
}

#if 0
void DoApproximation
( __global const p_config *g
, const floatexp a
, const floatexp b
, floatexp *x
, floatexp *y
)
{
  floatexp an[MAX_APPROX_TERMS + 1];
  floatexp bn[MAX_APPROX_TERMS + 1];
  an[0] = 1;
  bn[0] = 1;
  for (int i = 1; i <= g->m_nApproxTerms; ++i)
  {
    an[i] = an[i - 1] * a;
    bn[i] = bn[i - 1] * b;
  }
  floatexp totalX = 0;
  floatexp totalY = 0;
  for (int i = 0; i <= g->m_nApproxTerms; ++i)
  {
    floatexp subtotalX = 0;
    floatexp subtotalY = 0;
    for (int j = 0; j <= g->m_nApproxTerms - i; ++j)
    {
      subtotalX += g->m_APs_s[i][j] * bn[j];
      subtotalY += g->m_APs_t[i][j] * bn[j];
    }
    totalX += subtotalX * an[i];
    totalY += subtotalY * an[i];
  }
  x = totalX;
  y = totalY;
}
#endif

#define PIXEL_UNEVALUATED (-2147483648L)
#define ISFLOATOK(x) ((! isnan(x)) && (! isinf(x)))
#define SET_TRANS_GLITCH(x) (-1.0)

// forward declaration, defined per formula
void perturbation_double_loop
( __global const p_config   *g
, __global const double     *m_db_dxr
, __global const double     *m_db_dxi
, __global const double     *m_db_z
,                p_status_d *l
);

// forward declaration, defined per formula
void perturbation_floatexp_loop
( __global const p_config    *g
, __global const floatexp    *m_db_dxr
, __global const floatexp    *m_db_dxi
, __global const double      *m_db_z
,                p_status_fe *l
);

// forward declaration, defined per formula
void perturbation_softfloat_loop
( __global const p_config    *g
, __global const softfloat   *m_db_dxr
, __global const softfloat   *m_db_dxi
, __global const softfloat   *m_db_z
,                p_status_sf *l
);

// entry point
__kernel void perturbation_double
( __global const p_config *g // configuration including series approximation coefficients
, __global const double *m_db_dxr // reference orbit re
, __global const double *m_db_dxi // reference orbit im
, __global const double *m_db_z   // reference orbit (re^2+im^2)*glitch_threshold
, __global       uint   *n1  // iteration count msb, may be null
, __global       uint   *n0  // iteration count lsb
, __global       float  *nf  // iteration count fractional part
, __global       float  *phase // final angle, may be null
, __global       float  *dex // directional de x, may be null
, __global       float  *dey // directional de y, may be null
)
{
  if (g->BYTES != sizeof(p_config)) return;
  int y = get_global_id(0);
  int x = get_global_id(1);
  long ix = y * g->stride_y + x * g->stride_x + g->stride_offset;
  long orig = 0;
  float origf = 0;
  if (n1)
  {
    orig |= ((long)(int)(n1[ix])) << 32;
    if (n0) orig |= n0[ix];
  }
  else
  {
    if (n0) orig = (long)(int)(n0[ix]); // sign extend
  }
  if (nf)
  {
    origf = nf[ix];
  }
  if (! g->m_bAddReference || orig == PIXEL_UNEVALUATED || origf < 0) // first or fresh or glitch
  {
    const floatexp zero = fe_floatexp(0.0, 0);
    const floatexp one  = fe_floatexp(1.0, 0);
    long nMaxIter = g->m_nGlitchIter < g->m_nMaxIter ? g->m_nGlitchIter : g->m_nMaxIter;
    // FIXME TODO mirroring, incremental rendering, guessing
/*
  while (!m_bStop && m_P.GetPixel(x, y, w, h, m_bMirrored)){
    int64_t nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
    if (m_nPixels[x][y] != PIXEL_UNEVALUATED){
      SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y, w, h);
      continue;
    }
    if (GuessPixel(x, y, w, h))
      continue;
*/
    // Series approximation
    floatexp D0r = zero;
    floatexp D0i = zero;
    floatexp daa0 = one;
    floatexp dab0 = zero;
    floatexp dba0 = zero;
    floatexp dbb0 = one;
    GetPixelCoordinatesM(g, x, y, &D0r, &D0i, &daa0, &dab0, &dba0, &dbb0);
    long antal;
    floatexp TDnr;
    floatexp TDni;
    floatexp dxa1, dxb1, dya1, dyb1;
    DoApproximationM(g, &antal, D0r, D0i, &TDnr, &TDni, &dxa1, &dxb1, &dya1, &dyb1);
    floatexp TDDnr = dxa1;
    floatexp TDDni = dya1;
    double test1 = 0, test2 = 0;
    bool bGlitch = false;
    double dbD0r = fe_double(D0r);
    double dbD0i = fe_double(D0i);
    double Dr = fe_double(TDnr);
    double Di = fe_double(TDni);
    double daa = fe_double(daa0);
    double dab = fe_double(dab0);
    double dba = fe_double(dba0);
    double dbb = fe_double(dbb0);
    double dr = fe_double(TDDnr);
    double di = fe_double(TDDni);
    dr *= g->m_dPixelSpacing;
    di *= g->m_dPixelSpacing;
    // in
    p_status_d l =
      { dbD0r
      , dbD0i
      , Dr
      , Di
      , dr
      , di
      , daa
      , dab
      , dba
      , dbb
      , test1
      , test2
      , antal
      , bGlitch
      , g->log_m_nPower
      };
    // core per pixel calculation
    perturbation_double_loop(g, m_db_dxr, m_db_dxi, m_db_z, &l);
    // out
    Dr = l.xr;
    Di = l.xi;
    dr = l.dr;
    di = l.di;
    test1 = l.test1;
    test2 = l.test2;
    antal = l.antal;
    bGlitch = l.bGlitch;
    dcomplex de = { 0.0, 0.0 };
    if (g->derivatives)
    {
      dcomplex z = { Dr, Di };
      dcomplex dc = { dr, di };
      de = dc_ddiv(dc_abs(z) * log(dc_abs(z)), dc);
    }
    // output iteration data
    if (antal == g->m_nGlitchIter)
      bGlitch = true;
    if (antal >= g->m_nMaxIter){
      if (n1) n1[ix] = antal >> 32;
      if (n0) n0[ix] = antal;
      if (nf) nf[ix] = 0;
      if (dex) dex[ix] = 0;
      if (dey) dey[ix] = 0;
    }
    else{
      if (n1) n1[ix] = antal >> 32;
      if (n0) n0[ix] = antal;
      if (dex) dex[ix] = de.re;
      if (dey) dey[ix] = de.im;
      if (!bGlitch && g->m_nSmoothMethod == 1){
        double p = g->norm_p;
        if (! (p < 1.0 / 0.0)) p = 1;
        double div = pow(test1, 1 / p) - pow(test2, 1 / p);
        if (div != 0)
        {
          if (nf) nf[ix] = (pow(test1, 1 / p) - g->m_nBailout) / div;
        }
        else
        {
          if (nf) nf[ix] = 0;
        }
      }
      else if (!bGlitch && g->m_nSmoothMethod == 0){
        double t = log(log(sqrt(test1)) / g->log_m_nBailout) / l.log_m_nPower;
        if (!ISFLOATOK(t))
          t = 0;
        long i = floor(t);
        antal -= i;
        t -= i;
        if (n1) n1[ix] = antal >> 32;
        if (n0) n0[ix] = antal;
        if (nf) nf[ix] = t;
      }

      if (bGlitch && !g->m_bNoGlitchDetection){
        if (nf) nf[ix] = SET_TRANS_GLITCH(test1);
      }
    }
  }
}

// entry point
__kernel void perturbation_floatexp
( __global const p_config *g // configuration including series approximation coefficients
, __global const floatexp *m_db_dxr // reference orbit re
, __global const floatexp *m_db_dxi // reference orbit im
, __global const double   *m_db_z   // reference orbit (re^2+im^2)*glitch_threshold
, __global       uint     *n1  // iteration count msb, may be null
, __global       uint     *n0  // iteration count lsb
, __global       float    *nf  // iteration count fractional part
, __global       float  *phase // final angle, may be null
, __global       float    *dex // directional de x, may be null
, __global       float    *dey // directional de y, may be null
)
{
  if (g->BYTES != sizeof(p_config)) return;
  int y = get_global_id(0);
  int x = get_global_id(1);
  long ix = y * g->stride_y + x * g->stride_x + g->stride_offset;
  long orig = 0;
  float origf = 0;
  if (n1)
  {
    orig |= ((long)(int)(n1[ix])) << 32;
    if (n0) orig |= n0[ix];
  }
  else
  {
    if (n0) orig = (long)(int)(n0[ix]); // sign extend
  }
  if (nf)
  {
    origf = nf[ix];
  }
  if (! g->m_bAddReference || orig == PIXEL_UNEVALUATED || origf < 0) // first or fresh or glitch
  {
    const floatexp zero = fe_floatexp(0.0, 0);
    const floatexp one  = fe_floatexp(1.0, 0);
    long nMaxIter = g->m_nGlitchIter < g->m_nMaxIter ? g->m_nGlitchIter : g->m_nMaxIter;
    // FIXME TODO mirroring, incremental rendering, guessing
/*
  while (!m_bStop && m_P.GetPixel(x, y, w, h, m_bMirrored)){
    int64_t nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
    if (m_nPixels[x][y] != PIXEL_UNEVALUATED){
      SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y, w, h);
      continue;
    }
    if (GuessPixel(x, y, w, h))
      continue;
*/
    // Series approximation
    floatexp D0r = zero;
    floatexp D0i = zero;
    floatexp daa0 = one;
    floatexp dab0 = zero;
    floatexp dba0 = zero;
    floatexp dbb0 = one;
    GetPixelCoordinatesM(g, x, y, &D0r, &D0i, &daa0, &dab0, &dba0, &dbb0);
    long antal;
    floatexp TDnr;
    floatexp TDni;
    floatexp dxa1, dxb1, dya1, dyb1;
    DoApproximationM(g, &antal, D0r, D0i, &TDnr, &TDni, &dxa1, &dxb1, &dya1, &dyb1);
    floatexp TDDnr = dxa1;
    floatexp TDDni = dya1;
    double test1 = 0, test2 = 0;
    bool bGlitch = false;
    // in
    p_status_fe l =
      { D0r
      , D0i
      , TDnr
      , TDni
      , fe_mul(TDDnr, g->m_fPixelSpacing)
      , fe_mul(TDDni, g->m_fPixelSpacing)
      , daa0
      , dab0
      , dba0
      , dbb0
      , test1
      , test2
      , antal
      , bGlitch
      , g->log_m_nPower
      };
    // core per pixel calculation
    perturbation_floatexp_loop(g, m_db_dxr, m_db_dxi, m_db_z, &l);
    // out
    floatexp Dr = l.xr;
    floatexp Di = l.xi;
    floatexp dr = l.dr;
    floatexp di = l.di;
    test1 = l.test1;
    test2 = l.test2;
    antal = l.antal;
    bGlitch = l.bGlitch;
    dcomplex de = { 0.0, 0.0 };
    if (g->derivatives)
    {
      dcomplex z = { fe_double(Dr), fe_double(Di) };
      dcomplex dc = { fe_double(dr), fe_double(di) };
      de = dc_ddiv(dc_abs(z) * log(dc_abs(z)), dc);
    }
    // output iteration data
    if (antal == g->m_nGlitchIter)
      bGlitch = true;
    if (antal >= g->m_nMaxIter){
      if (n1) n1[ix] = antal >> 32;
      if (n0) n0[ix] = antal;
      if (nf) nf[ix] = 0;
      if (dex) dex[ix] = 0;
      if (dey) dey[ix] = 0;
    }
    else{
      if (n1) n1[ix] = antal >> 32;
      if (n0) n0[ix] = antal;
      if (dex) dex[ix] = de.re;
      if (dey) dey[ix] = de.im;
      if (!bGlitch && g->m_nSmoothMethod == 1){
        double p = g->norm_p;
        if (! (p < 1.0 / 0.0)) p = 1;
        double div = pow(test1, 1 / p) - pow(test2, 1 / p);
        if (div != 0)
        {
          if (nf) nf[ix] = (pow(test1, 1 / p) - g->m_nBailout) / div;
        }
        else
        {
          if (nf) nf[ix] = 0;
        }
      }
      else if (!bGlitch && g->m_nSmoothMethod == 0){
        double t = log(log(sqrt(test1)) / g->log_m_nBailout) / l.log_m_nPower;
        if (!ISFLOATOK(t))
          t = 0;
        long i = floor(t);
        antal -= i;
        t -= i;
        if (n1) n1[ix] = antal >> 32;
        if (n0) n0[ix] = antal;
        if (nf) nf[ix] = t;
      }

      if (bGlitch && !g->m_bNoGlitchDetection){
        if (nf) nf[ix] = SET_TRANS_GLITCH(test1);
      }
    }
  }
}

// entry point
__kernel void perturbation_softfloat
( __global const p_config  *g // configuration including series approximation coefficients
, __global const softfloat *m_db_dxr // reference orbit re
, __global const softfloat *m_db_dxi // reference orbit im
, __global const softfloat *m_db_z   // reference orbit (re^2+im^2)*glitch_threshold
, __global       uint      *n1  // iteration count msb, may be null
, __global       uint      *n0  // iteration count lsb
, __global       float     *nf  // iteration count fractional part
, __global       float  *phase // final angle, may be null
, __global       float     *dex // directional de x, may be null
, __global       float     *dey // directional de y, may be null
)
{
  if (g->BYTES != sizeof(p_config)) return;
  int y = get_global_id(0);
  int x = get_global_id(1);
  long ix = y * g->stride_y + x * g->stride_x + g->stride_offset;
  long orig = 0;
  float origf = 0;
  if (n1)
  {
    orig |= ((long)(int)(n1[ix])) << 32;
    if (n0) orig |= n0[ix];
  }
  else
  {
    if (n0) orig = (long)(int)(n0[ix]); // sign extend
  }
  if (nf)
  {
    origf = nf[ix];
  }
  if (! g->m_bAddReference || orig == PIXEL_UNEVALUATED || origf < 0) // first or fresh or glitch
  {
    const floatexp zero = fe_floatexp(0.0, 0);
    const floatexp one  = fe_floatexp(1.0, 0);
    long nMaxIter = g->m_nGlitchIter < g->m_nMaxIter ? g->m_nGlitchIter : g->m_nMaxIter;
    // FIXME TODO mirroring, incremental rendering, guessing
/*
  while (!m_bStop && m_P.GetPixel(x, y, w, h, m_bMirrored)){
    int64_t nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
    if (m_nPixels[x][y] != PIXEL_UNEVALUATED){
      SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y, w, h);
      continue;
    }
    if (GuessPixel(x, y, w, h))
      continue;
*/
    // Series approximation
    floatexp D0r = zero;
    floatexp D0i = zero;
    floatexp daa0 = one;
    floatexp dab0 = zero;
    floatexp dba0 = zero;
    floatexp dbb0 = one;
    GetPixelCoordinatesM(g, x, y, &D0r, &D0i, &daa0, &dab0, &dba0, &dbb0);
    long antal;
    floatexp TDnr;
    floatexp TDni;
    floatexp dxa1, dxb1, dya1, dyb1;
    DoApproximationM(g, &antal, D0r, D0i, &TDnr, &TDni, &dxa1, &dxb1, &dya1, &dyb1);
    floatexp TDDnr = dxa1;
    floatexp TDDni = dya1;
    double test1 = 0.0, test2 = 0.0;
    bool bGlitch = false;
    // in
    p_status_sf l =
      { sf_from_floatexp(D0r)
      , sf_from_floatexp(D0i)
      , sf_from_floatexp(TDnr)
      , sf_from_floatexp(TDni)
      , sf_from_floatexp(fe_mul(TDDnr, g->m_fPixelSpacing))
      , sf_from_floatexp(fe_mul(TDDni, g->m_fPixelSpacing))
      , sf_from_floatexp(daa0)
      , sf_from_floatexp(dab0)
      , sf_from_floatexp(dba0)
      , sf_from_floatexp(dbb0)
      , sf_from_double(test1)
      , sf_from_double(test2)
      , antal
      , bGlitch
      , sf_from_double(g->log_m_nPower)
      };
    // core per pixel calculation
    perturbation_softfloat_loop(g, m_db_dxr, m_db_dxi, m_db_z, &l);
    // out
    softfloat Dr = l.xr;
    softfloat Di = l.xi;
    softfloat dr = l.dr;
    softfloat di = l.di;
    test1 = sf_to_double(l.test1);
    test2 = sf_to_double(l.test2);
    antal = l.antal;
    bGlitch = l.bGlitch;
    dcomplex de = { 0.0, 0.0 };
    if (g->derivatives)
    {
      dcomplex z = { sf_to_double(Dr), sf_to_double(Di) };
      dcomplex dc = { sf_to_double(dr), sf_to_double(di) };
      de = dc_ddiv(dc_abs(z) * log(dc_abs(z)), dc);
    }
    // output iteration data
    if (antal == g->m_nGlitchIter)
      bGlitch = true;
    if (antal >= g->m_nMaxIter){
      if (n1) n1[ix] = antal >> 32;
      if (n0) n0[ix] = antal;
      if (nf) nf[ix] = 0;
      if (dex) dex[ix] = 0;
      if (dey) dey[ix] = 0;
    }
    else{
      if (n1) n1[ix] = antal >> 32;
      if (n0) n0[ix] = antal;
      if (dex) dex[ix] = de.re;
      if (dey) dey[ix] = de.im;
      if (!bGlitch && g->m_nSmoothMethod == 1){
        double p = g->norm_p;
        if (! (p < 1.0 / 0.0)) p = 1;
        double div = pow(test1, 1 / p) - pow(test2, 1 / p);
        if (div != 0)
        {
          if (nf) nf[ix] = (pow(test1, 1 / p) - g->m_nBailout) / div;
        }
        else
        {
          if (nf) nf[ix] = 0;
        }
      }
      else if (!bGlitch && g->m_nSmoothMethod == 0){
        double t = log(log(sqrt(test1)) / g->log_m_nBailout) / sf_to_double(l.log_m_nPower);
        if (!ISFLOATOK(t))
          t = 0;
        long i = floor(t);
        antal -= i;
        t -= i;
        if (n1) n1[ix] = antal >> 32;
        if (n0) n0[ix] = antal;
        if (nf) nf[ix] = t;
      }

      if (bGlitch && !g->m_bNoGlitchDetection){
        if (nf) nf[ix] = SET_TRANS_GLITCH(test1);
      }
    }
  }
}

double pnorm(double g_real, double g_imag, double p, double x, double y)
{
  if (g_real == 1.0 && g_imag == 1.0 && p == 2.0)
  {
    return x * x + y * y;
  }
  if (p == 1.0)
  {
    return fabs(g_real * fabs(x) + g_imag * fabs(y));
  }
  if (p == 1.0/0.0)
  {
    return fabs(max(g_real * fabs(x), g_imag * fabs(y)));
  }
  return fabs(g_real * pow(fabs(x), p) + g_imag * pow(fabs(y), p));
}
