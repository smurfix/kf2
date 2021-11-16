#define assert(n) do{}while(0)

#define COMMA ,

mantissa d_add(const mantissa a, const mantissa b)
{
  return a + b;
}

mantissa d_sub(const mantissa a, const mantissa b)
{
  return a - b;
}

mantissa d_isub(const int a, const mantissa b)
{
  return a - b;
}

mantissa d_neg(const mantissa a)
{
  return -a;
}

mantissa d_abs(const mantissa a)
{
  return a < 0.0 ? -a : a;
}

mantissa d_sgn(const mantissa a)
{
  return a < 0.0 ? -1.0 : 1.0;
}

mantissa d_imul(const int a, const mantissa b)
{
  return a * b;
}

mantissa d_muli(const mantissa a, const int b)
{
  return a * b;
}

mantissa d_mul(const mantissa a, const mantissa b)
{
  return a * b;
}

mantissa d_div(const mantissa a, const mantissa b)
{
  return a / b;
}

mantissa d_divi(const mantissa a, const int b)
{
  return a / b;
}

mantissa d_pow(const mantissa a, const int b)
{
  mantissa r = 1.0;
  for (int i = 0; i < b; ++i)
  {
    r *= a;
  }
  return r;
}

int d_cmp(const mantissa a, const mantissa b)
{
  return ((int)(a > b)) - ((int)(a < b));
}

mantissa d_diffabs(const mantissa c, const mantissa d)
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

mantissa d_sin(const mantissa a)
{
  return sin(a);
}

mantissa d_cos(const mantissa a)
{
  return cos(a);
}

mantissa d_exp(const mantissa a)
{
  return exp(a);
}

mantissa d_log(const mantissa a)
{
  return log(a);
}

mantissa d_log1p(const mantissa a)
{
  return log1p(a);
}

mantissa d_sinh(const mantissa a)
{
  return sinh(a);
}

mantissa d_special_93_2(const mantissa XC, const mantissa Xr, const mantissa Xxr, const mantissa xr)
{
  if (Xr > -7.0/4.0)
  {
    return Xxr > -7.0/4.0 ? 2.0 * Xr + xr : -7.0/2.0 - xr;
  }
  else
  {
    return Xxr > -7.0/4.0 ? xr - 7.0/2.0 : -XC - xr;
  }
}

typedef struct __attribute__((packed))
{
  mantissa val;
  exponent exp;
} floatexp;

mantissa fe_double(const floatexp f)
{
  if (f.exp < -LARGE_EXPONENT)
  {
    return 0;
  }
  if (f.exp > LARGE_EXPONENT)
  {
    return f.val / 0;
  }
  return ldexp(f.val, f.exp);
}

floatexp fe_floatexp(const mantissa val, const exponent exp)
{
  if (val == 0)
  {
    floatexp fe = { val, EXP_MIN };
    return fe;
  }
  else if (isnan(val))
  {
    floatexp fe = { val, EXP_MIN };
    return fe;
  }
  else if (isinf(val))
  {
    floatexp fe = { val, EXP_MAX };
    return fe;
  }
  else
  {
    int e = 0;
    mantissa f_val = frexp(val, &e);
    exponent f_exp = e + exp;
    floatexp fe = { f_val, f_exp };
    return fe;
  }
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


floatexp fe_muld(const floatexp a, const mantissa b)
{
  return fe_floatexp(a.val * b, a.exp);
}

floatexp fe_dmul(const mantissa a, const floatexp b)
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

floatexp fe_mul_2si(const floatexp a, const exponent b)
{
  floatexp fe = { a.val, a.exp + b };
  return fe;
}

floatexp fe_div(const floatexp a, const floatexp b)
{
  return fe_floatexp(a.val / b.val, a.exp - b.exp);
}

floatexp fe_divi(const floatexp a, const int b)
{
  return fe_div(a, fe_floatexp(b, 0));
}

floatexp fe_div_2si(const floatexp a, const exponent b)
{
  floatexp fe = { a.val, a.exp - b };
  return fe;
}

floatexp fe_divd(const floatexp a, const mantissa b)
{
  return fe_floatexp(a.val / b, a.exp);
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

floatexp fe_isub(const int a, const floatexp b)
{
  return fe_add(fe_floatexp(a, 0), fe_neg(b));
}

floatexp fe_dsub(const mantissa a, const floatexp b)
{
  return fe_add(fe_floatexp(a, 0), fe_neg(b));
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

bool fe_lt(const floatexp a, const floatexp b)
{
  if (isnan(a.val) || isnan(b.val)) return false;
  return fe_cmp(a, b) < 0;
}

bool fe_gt(const floatexp a, const floatexp b)
{
  if (isnan(a.val) || isnan(b.val)) return false;
  return fe_cmp(a, b) > 0;
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

floatexp fe_sqrt(const floatexp a)
{
  return fe_floatexp
    ( sqrt((a.exp & 1) ? 2.0 * a.val : a.val)
    , (a.exp & 1) ? (a.exp - 1) / 2 : a.exp / 2
    );
}

floatexp fe_exp(const floatexp a)
{
  if (-53 <= a.exp && a.exp <= 8) return fe_floatexp(exp(fe_double(a)), 0);
  if (61 <= a.exp) a.val > 0.0 ? fe_floatexp(a.val / 0.0, 0) : fe_floatexp(0.0, 0);
  if (a.exp < -53) return fe_floatexp(1.0, 0);
  return fe_pow(fe_floatexp(exp(a.val), 0), 1UL << a.exp);
}

floatexp fe_expm1(const floatexp a)
{
  if (a.exp <= -120) return a; // FIXME threshold depends on number type
  if (8 <= a.exp) return fe_sub(fe_exp(a), fe_floatexp(1.0, 0));
  return fe_floatexp(expm1(fe_double(a)), 0);
}

floatexp fe_sin(const floatexp a)
{
  if (a.exp <= -120) return a; // FIXME threshold depends on number type
  return fe_floatexp(sin(fe_double(a)), 0);
}

floatexp fe_cos(const floatexp a)
{
  if (a.exp <= -120) return fe_floatexp(1, 0); // FIXME threshold depends on number type
  return fe_floatexp(cos(fe_double(a)), 0);
}

floatexp fe_log(const floatexp a)
{
  return fe_floatexp(log(a.val) + log(2.0) * a.exp, 0);
}

floatexp fe_log1p(const floatexp a)
{
  if (a.exp < -(sizeof(mantissa) == sizeof(float) ? 24 : 53))
  {
    return a;
  }
  else if (a.exp > (sizeof(mantissa) == sizeof(float) ? 24 : 53))
  {
    return fe_log(fe_add(fe_floatexp(1.0, 0),  a));
  }
  else
  {
    return fe_floatexp(log1p(fe_double(a)), 0);
  }
}

floatexp fe_sinh(const floatexp a)
{
  return fe_div(fe_expm1(fe_mul_2si(a, 1)), fe_mul_2si(fe_exp(a), 1)); // FIXME optimized for a near 0
}

floatexp fe_special_93_2(const floatexp XC, const floatexp Xr, const floatexp Xxr, const floatexp xr)
{
  const floatexp n74 = fe_floatexp(-7.0/4.0, 0);
  const floatexp n72 = fe_floatexp(-7.0/2.0, 0);
  if (fe_gt(Xr, n74))
  {
    return fe_gt(Xxr, n74) ? fe_add(fe_mul_2si(Xr, 1), xr) : fe_sub(n72, xr);
  }
  else
  {
    return fe_gt(Xxr, n74) ? fe_add(n72, xr) : fe_neg(fe_add(XC, xr));
  }
}


#if 0

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

mantissa sf_to_double(const softfloat f)
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
    mantissa x = sf_mantissa(f);
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

softfloat sf_from_double(const mantissa x)
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
    mantissa y = frexp(fabs(x), &e);
    mantissa z = ldexp(y, SF_MANTISSA_BITS);
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

#endif

typedef struct __attribute__((packed))
{
  mantissa x;
  mantissa dx[2];
} duald;

duald dd(const mantissa a, const mantissa b, const mantissa c)
{
  duald r = { a, { b, c } };
  return r;
}

duald duald_add(duald a, duald b)
{
  duald r = { a.x + b.x, { a.dx[0] + b.dx[0], a.dx[1] + b.dx[1] } };
  return r;
}

duald duald_dadd(mantissa a, duald b)
{
  duald r = { a + b.x, { b.dx[0], b.dx[1] } };
  return r;
}

duald duald_addd(duald a, mantissa b)
{
  duald r = { a.x + b, { a.dx[0], a.dx[1] } };
  return r;
}

duald duald_sub(duald a, duald b)
{
  duald r = { a.x -b.x, { a.dx[0] - b.dx[0], a.dx[1] - b.dx[1] } };
  return r;
}

duald duald_dsub(mantissa a, duald b)
{
  duald r = { a - b.x, { -b.dx[0], -b.dx[1] } };
  return r;
}

duald duald_subd(duald a, mantissa b)
{
  duald r = { a.x - b, { a.dx[0], a.dx[1] } };
  return r;
}

duald duald_mul(duald a, duald b)
{
  duald r = { a.x * b.x, { a.dx[0] * b.x + a.x * b.dx[0], a.dx[1] * b.x + a.x * b.dx[1] } };
  return r;
}

duald duald_dmul(mantissa a, duald b)
{
  duald r = { a * b.x, { a * b.dx[0], a * b.dx[1] } };
  return r;
}

duald duald_muld(duald a, mantissa b)
{
  duald r = { a.x * b, { a.dx[0] * b, a.dx[1] * b } };
  return r;
}

duald duald_sqr(duald a)
{
  duald r = { a.x * a.x, { 2.0 * a.dx[0] * a.x, 2.0 * a.dx[1] * a.x } };
  return r;
}

duald duald_div(duald a, duald b)
{
  duald r;
  r.x = a.x / b.x;
  mantissa den = 1.0 / (b.x * b.x);
  r.dx[0] = (a.dx[0] * b.x - a.x * b.dx[0]) * den;
  r.dx[1] = (a.dx[1] * b.x - a.x * b.dx[1]) * den;
  return r;
}

duald duald_divd(duald a, mantissa b)
{
  duald r = { a.x / b, { a.dx[0] / b, a.dx[1] / b } };
  return r;
}

bool duald_lt(duald a, duald b)
{
  return a.x < b.x;
}

bool duald_ltd(duald a, mantissa b)
{
  return a.x < b;
}

bool duald_gt(duald a, duald b)
{
  return a.x > b.x;
}

bool duald_gtd(duald a, mantissa b)
{
  return a.x > b;
}

bool duald_le(duald a, duald b)
{
  return a.x <= b.x;
}

bool duald_led(duald a, mantissa b)
{
  return a.x <= b;
}

bool duald_ge(duald a, duald b)
{
  return a.x >= b.x;
}

bool duald_ged(duald a, mantissa b)
{
  return a.x >= b;
}

duald duald_neg(duald a)
{
  duald r = { -a.x, { -a.dx[0], -a.dx[1] } };
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
  mantissa s = -sin(a.x);
  r.dx[0] = a.dx[0] * s;
  r.dx[1] = a.dx[1] * s;
  return r;
}

duald duald_sin(duald a)
{
  duald r;
  r.x = sin(a.x);
  mantissa c = cos(a.x);
  r.dx[0] = a.dx[0] * c;
  r.dx[1] = a.dx[1] * c;
  return r;
}

duald duald_ddiffabs(mantissa c, duald d)
{
  const mantissa cd = c + d.x;
  const duald c2d = duald_dadd(2 * c, d);
  return c >= 0.0 ? cd >= 0.0 ? d : duald_neg(c2d) : cd > 0.0 ? c2d : duald_neg(d);
}

duald duald_diffabs(duald c, duald d)
{
  const mantissa cd = c.x + d.x;
  const duald c2d = duald_add(duald_dmul(2, c), d);
  return c.x >= 0.0 ? cd >= 0.0 ? d : duald_neg(c2d) : cd > 0.0 ? c2d : duald_neg(d);
}

typedef struct __attribute__((packed))
{
  floatexp x;
  floatexp dx[2];
} dualfe;

dualfe dfe(const floatexp a, const floatexp b, const floatexp c)
{
  dualfe r = { a, { b, c } };
  return r;
}

duald dualfe_double(const dualfe a)
{
  duald r = { fe_double(a.x), { fe_double(a.dx[0]), fe_double(a.dx[1]) } };
  return r;
}

dualfe dualfe_from_duald(const duald a)
{
  dualfe r = { fe_floatexp(a.x, 0), { fe_floatexp(a.dx[0], 0), fe_floatexp(a.dx[1], 0) } };
  return r;
}

dualfe dualfe_add(dualfe a, dualfe b)
{
  dualfe r = { fe_add(a.x, b.x), { fe_add(a.dx[0], b.dx[0]), fe_add(a.dx[1], b.dx[1]) } };
  return r;
}

dualfe dualfe_feadd(floatexp a, dualfe b)
{
  dualfe r = { fe_add(a, b.x), { b.dx[0], b.dx[1] } };
  return r;
}

dualfe dualfe_addfe(dualfe a, floatexp b)
{
  dualfe r = { fe_add(a.x, b), { a.dx[0], a.dx[1] } };
  return r;
}

dualfe dualfe_sub(dualfe a, dualfe b)
{
  dualfe r = { fe_sub(a.x, b.x), { fe_sub(a.dx[0], b.dx[0]), fe_sub(a.dx[1], b.dx[1]) } };
  return r;
}

dualfe dualfe_fesub(floatexp a, dualfe b)
{
  dualfe r = { fe_sub(a, b.x), { fe_neg(b.dx[0]), fe_neg(b.dx[1]) } };
  return r;
}

dualfe dualfe_subfe(dualfe a, floatexp b)
{
  dualfe r = { fe_sub(a.x, b), { a.dx[0], a.dx[1] } };
  return r;
}

dualfe dualfe_mul(dualfe a, dualfe b)
{
  dualfe r = { fe_mul(a.x, b.x), { fe_add(fe_mul(a.dx[0], b.x), fe_mul(a.x, b.dx[0])), fe_add(fe_mul(a.dx[1], b.x), fe_mul(a.x, b.dx[1])) } };
  return r;
}

dualfe dualfe_femul(floatexp a, dualfe b)
{
  dualfe r = { fe_mul(a, b.x), { fe_mul(a, b.dx[0]), fe_mul(a, b.dx[1]) } };
  return r;
}

dualfe dualfe_mulfe(dualfe a, floatexp b)
{
  dualfe r = { fe_mul(a.x, b), { fe_mul(a.dx[0], b), fe_mul(a.dx[1], b) } };
  return r;
}

dualfe dualfe_dmul(mantissa a, dualfe b)
{
  dualfe r = { fe_dmul(a, b.x), { fe_dmul(a, b.dx[0]), fe_dmul(a, b.dx[1]) } };
  return r;
}

dualfe dualfe_muld(dualfe a, mantissa b)
{
  dualfe r = { fe_muld(a.x, b), { fe_muld(a.dx[0], b), fe_muld(a.dx[1], b) } };
  return r;
}

dualfe dualfe_mul_2si(dualfe a, int b)
{
  dualfe r = { fe_mul_2si(a.x, b), { fe_mul_2si(a.dx[0], b), fe_mul_2si(a.dx[1], b) } };
  return r;
}

dualfe dualfe_femulduald(const floatexp a, const duald b)
{
  dualfe r = { fe_muld(a, b.x), { fe_muld(a, b.dx[0]), fe_muld(a, b.dx[1]) } };
  return r;
}

dualfe dualfe_sqr(dualfe a)
{
  dualfe r = { fe_sqr(a.x), { fe_mul_2si(fe_mul(a.dx[0], a.x), 1), fe_mul_2si(fe_mul(a.dx[1], a.x), 1) } };
  return r;
}

dualfe dualfe_div(dualfe a, dualfe b)
{
  dualfe r;
  r.x = fe_div(a.x, b.x);
  floatexp den = fe_div(fe_floatexp(1.0, 0), fe_sqr(b.x));
  r.dx[0] = fe_mul(fe_sub(fe_mul(a.dx[0], b.x), fe_mul(a.x, b.dx[0])), den);
  r.dx[1] = fe_mul(fe_sub(fe_mul(a.dx[1], b.x), fe_mul(a.x, b.dx[1])), den);
  return r;
}

dualfe dualfe_divfe(dualfe a, floatexp b)
{
  dualfe r = { fe_div(a.x, b), { fe_div(a.dx[0], b), fe_div(a.dx[1], b) } };
  return r;
}

#if 0
bool duald_lt(duald a, duald b)
{
  return a.x < b.x;
}

bool duald_ltd(duald a, mantissa b)
{
  return a.x < b;
}

bool duald_gt(duald a, duald b)
{
  return a.x > b.x;
}

bool duald_gtd(duald a, mantissa b)
{
  return a.x > b;
}

bool duald_le(duald a, duald b)
{
  return a.x <= b.x;
}

bool duald_led(duald a, mantissa b)
{
  return a.x <= b;
}

bool duald_ge(duald a, duald b)
{
  return a.x >= b.x;
}

bool duald_ged(duald a, mantissa b)
{
  return a.x >= b;
}
#endif

dualfe dualfe_neg(dualfe a)
{
  dualfe r = { fe_neg(a.x), { fe_neg(a.dx[0]), fe_neg(a.dx[1]) } };
  return r;
}

dualfe dualfe_abs(dualfe a)
{
  return a.x.val < 0.0 ? dualfe_neg(a) : a;
}

#if 0
dualfe dualfe_exp(dualfe a)
{
  dualfe r;
  r.x = fe_exp(a.x);
  r.dx[0] = fe_mul(a.dx[0], r.x);
  r.dx[1] = fe_mul(a.dx[1], r.x);
  return r;
}

dualfe dualfe_cos(dualfe a)
{
  dualfe r;
  r.x = fe_cos(a.x);
  floatexp s = fe_neg(fe_sin(a.x));
  r.dx[0] = fe_mul(a.dx[0], s);
  r.dx[1] = fe_mul(a.dx[1], s);
  return r;
}

dualfe dualfe_sin(dualfe a)
{
  dualfe r;
  r.x = fe_sin(a.x);
  floatexp c = fe_cos(a.x);
  r.dx[0] = fe_mul(a.dx[0], c);
  r.dx[1] = fe_mul(a.dx[1], c);
  return r;
}
#endif

dualfe dualfe_fediffabs(floatexp c, dualfe d)
{
  const floatexp cd = fe_add(c, d.x);
  const dualfe c2d = dualfe_feadd(fe_mul_2si(c, 1), d);
  return c.val >= 0.0 ? cd.val >= 0.0 ? d : dualfe_neg(c2d) : cd.val > 0.0 ? c2d : dualfe_neg(d);
}

dualfe dualfe_diffabs(dualfe c, dualfe d)
{
  const floatexp cd = fe_add(c.x, d.x);
  const dualfe c2d = dualfe_add(dualfe_mul_2si(c, 1), d);
  return c.x.val >= 0.0 ? cd.val >= 0.0 ? d : dualfe_neg(c2d) : cd.val > 0.0 ? c2d : dualfe_neg(d);
}

typedef struct __attribute__((packed))
{
  mantissa re;
  mantissa im;
} dcomplex;

dcomplex dc(const mantissa a, const mantissa b)
{
  dcomplex d = { a, b };
  return d;
}

mantissa dc_norm(const dcomplex a)
{
  return a.re * a.re + a.im * a.im;
}

mantissa dc_abs(const dcomplex a)
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

dcomplex dc_dmul(const mantissa a, const dcomplex b)
{
  dcomplex dc = { a * b.re, a * b.im };
  return dc;
}

dcomplex dc_muld(const dcomplex a, const mantissa b)
{
  dcomplex dc = { a.re * b, a.im * b };
  return dc;
}

dcomplex dc_mul(const dcomplex a, const dcomplex b)
{
  dcomplex dc = { a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re };
  return dc;
}

dcomplex dc_sqr(const dcomplex a)
{
  dcomplex dc = { a.re * a.re - a.im * a.im, 2.0 * a.re * a.im };
  return dc;
}

dcomplex dc_ddiv(const mantissa a, const dcomplex b)
{
  mantissa b2 = dc_norm(b);
  dcomplex dc = { (a * b.re) / b2, (-a * b.im) / b2 };
  return dc;
}

dcomplex dc_div(const dcomplex a, const dcomplex b)
{
  mantissa b2 = dc_norm(b);
  dcomplex dc = { (a.re * b.re + a.im * b.im) / b2, (-a.re * b.im + a.im * b.re) / b2 };
  return dc;
}

dcomplex dc_powi(dcomplex x, int n)
{
  const dcomplex one = { 1.0, 0.0 };
  switch (n)
  {
    case 0: return one;
    case 1: return x;
    case 2: return dc_sqr(x);
    case 3: return dc_mul(x, dc_sqr(x));
    case 4: return dc_sqr(dc_sqr(x));
    case 5: return dc_mul(x, dc_sqr(dc_sqr(x)));
    case 6: return dc_sqr(dc_mul(x, dc_sqr(x)));
    case 7: return dc_mul(x, dc_sqr(dc_mul(x, dc_sqr(x))));
    case 8: return dc_sqr(dc_sqr(dc_sqr(x)));
    default:
    {
      dcomplex y = one;
      while (n > 1)
      {
        if (n & 1)
          y = dc_mul(y, x);
        x = dc_sqr(x);
        n >>= 1;
      }
      return dc_mul(x, y);
    }
  }
}

dcomplex dc_exp(const dcomplex x)
{
  const mantissa r = exp(x.re);
  const dcomplex t = { cos(x.im), sin(x.im) };
  return dc_dmul(r, t);
}

// ref: <https://github.com/JuliaLang/julia/pull/6539/files#diff-e41e6420fa56749ce528b57eb01ebec81a484488511a96466dd9319a4f19fb0eR364-R371>
dcomplex dc_expm1(const dcomplex x) // FIXME finite only
{
  const mantissa r = expm1(x.re);
  const mantissa r1 = r + 1;
  const mantissa s = sin(x.im / 2);
  const dcomplex dc = { r - 2 * r1 * s * s, r1 * sin(x.im) };
  return dc;
}

dcomplex dc_sinh(const dcomplex x) // FIXME this is optimized for x near 0
{
  return dc_div(dc_expm1(dc_dmul(2, x)), dc_dmul(2, dc_exp(x)));
}

typedef struct __attribute__((packed))
{
  floatexp re;
  floatexp im;
} fecomplex;

fecomplex fec(const floatexp a, const floatexp b)
{
  fecomplex f = { a, b };
  return f;
}

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

floatexp fec_norm(const fecomplex a)
{
  return fe_add(fe_mul(a.re, a.re), fe_mul(a.im, a.im));
}

fecomplex fec_mul(const fecomplex a, const fecomplex b)
{
  fecomplex fec = { fe_sub(fe_mul(a.re, b.re), fe_mul(a.im, b.im)), fe_add(fe_mul(a.re, b.im), fe_mul(a.im, b.re)) };
  return fec;
}

fecomplex fec_dmul(const mantissa a, const fecomplex b)
{
  fecomplex dc = { fe_dmul(a, b.re), fe_dmul(a, b.im) };
  return dc;
}

fecomplex fec_dcmul(const dcomplex a, const fecomplex b)
{
  fecomplex fec = { fe_sub(fe_dmul(a.re, b.re), fe_dmul(a.im, b.im)), fe_add(fe_dmul(a.re, b.im), fe_dmul(a.im, b.re)) };
  return fec;
}

fecomplex fec_sqr(const fecomplex a)
{
  fecomplex fec = { fe_sub(fe_sqr(a.re), fe_sqr(a.im)), fe_mul_2si(fe_mul(a.re, a.im), 1) };
  return fec;
}

fecomplex fec_powi(fecomplex x, int n)
{
  const fecomplex one = { fe_floatexp(1.0, 0), fe_floatexp(0.0, 0) };
  switch (n)
  {
    case 0: return one;
    case 1: return x;
    case 2: return fec_sqr(x);
    case 3: return fec_mul(x, fec_sqr(x));
    case 4: return fec_sqr(fec_sqr(x));
    case 5: return fec_mul(x, fec_sqr(fec_sqr(x)));
    case 6: return fec_sqr(fec_mul(x, fec_sqr(x)));
    case 7: return fec_mul(x, fec_sqr(fec_mul(x, fec_sqr(x))));
    case 8: return fec_sqr(fec_sqr(fec_sqr(x)));
    default:
    {
      fecomplex y = one;
      while (n > 1)
      {
        if (n & 1)
          y = fec_mul(y, x);
        x = fec_sqr(x);
        n >>= 1;
      }
      return fec_mul(x, y);
    }
  }
}

fecomplex fec_conj(const fecomplex a)
{
  const fecomplex f = { a.re, fe_neg(a.im) };
  return f;
}

fecomplex fec_div(const fecomplex a, const fecomplex b)
{
  const floatexp b2 = fec_norm(b);
  const fecomplex ab = fec_mul(a, fec_conj(b));
  const fecomplex f = { fe_div(ab.re, b2), fe_div(ab.im, b2) };
  return f;
}

fecomplex fec_exp(const fecomplex x)
{
  const floatexp r = fe_exp(x.re);
  const fecomplex f = { fe_mul(r, fe_cos(x.im)), fe_mul(r, fe_sin(x.im)) };
  return f;
}

fecomplex fec_expm1(const fecomplex x)
{
  const floatexp r = fe_expm1(x.re);
  const floatexp r1 = fe_add(r, fe_floatexp(1, 0));
  const floatexp s = fe_sin(fe_div_2si(x.im, 1));
  const fecomplex f = { fe_sub(r, fe_mul(fe_mul_2si(r1, 1), fe_sqr(s))), fe_mul(r1, fe_sin(x.im)) };
  return f;
}

fecomplex fec_sinh(const fecomplex x) // FIXME this is optimized for x near 0
{
  return fec_div(fec_expm1(fec_dmul(2, x)), fec_dmul(2, fec_exp(x)));
}



#if 0

typedef struct __attribute__((packed))
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

#endif

typedef struct __attribute__((packed))
{
  duald re;
  duald im;
} dualdcomplex;

dualdcomplex ddc(const duald a, const duald b)
{
  dualdcomplex r = { a, b };
  return r;
}

mantissa dualdc_norm(const dualdcomplex a)
{
  return a.re.x * a.re.x + a.im.x * a.im.x;
}

mantissa dualdc_abs(const dualdcomplex a)
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

dualdcomplex dualdc_adddc(const dualdcomplex a, const dcomplex b)
{
  dualdcomplex dc = { duald_addd(a.re, b.re), duald_addd(a.im, b.im) };
  return dc;
}

dualdcomplex dualdc_sub(const dualdcomplex a, const dualdcomplex b)
{
  dualdcomplex dc = { duald_sub(a.re, b.re), duald_sub(a.im, b.im) };
  return dc;
}

dualdcomplex dualdc_dmul(const mantissa a, const dualdcomplex b)
{
  dualdcomplex dc = { duald_dmul(a, b.re), duald_dmul(a, b.im) };
  return dc;
}

dualdcomplex dualdc_muld(const dualdcomplex a, const mantissa b)
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
dualdcomplex dualdc_ddiv(const mantissa a, const dualdcomplex b) // FIXME verify
{
  mantissa b2 = dualdc_norm(b);
  dualdcomplex dc =
    { duald_divd(duald_dmul(a, b.re), b2)
    , duald_divd(duald_dmul(-a, b.im), b2)
    };
  return dc;
}

dualdcomplex dualdc_div(const dualdcomplex a, const dualdcomplex b) // FIXME verify
{
  mantissa b2 = dualdc_norm(b);
  dualdcomplex dc = dualdc_divd(dualdc_mul(a, dualdc_conj(b)), b2);
  return dc;
}
#endif

dualdcomplex dualdc_pow(const dualdcomplex a, const int b)
{
  dualdcomplex r = { { 1.0, { 0.0, 0.0 } }, { 0.0, { 0.0, 0.0 } } };
  for (int i = 0; i < b; ++i)
  {
    r = dualdc_mul(r, a);
  }
  return r;
}


typedef struct __attribute__((packed))
{
  dualfe re;
  dualfe im;
} dualfecomplex;

dualfecomplex dfec(const dualfe a, const dualfe b)
{
  dualfecomplex r = { a, b };
  return r;
}

floatexp dualfec_norm(const dualfecomplex a)
{
  return fe_add(fe_sqr(a.re.x), fe_sqr(a.im.x));
}
#if 0
mantissa dualdc_abs(const dualdcomplex a)
{
  return sqrt(dualdc_norm(a));
}
#endif
dualfecomplex dualfec_conj(const dualfecomplex a)
{
  dualfecomplex r = { a.re, dualfe_neg(a.im) };
  return r;
}

dualfecomplex dualfec_neg(const dualfecomplex a)
{
  dualfecomplex dc = { dualfe_neg(a.re), dualfe_neg(a.im) };
  return dc;
}

dualfecomplex dualfec_add(const dualfecomplex a, const dualfecomplex b)
{
  dualfecomplex dc = { dualfe_add(a.re, b.re), dualfe_add(a.im, b.im) };
  return dc;
}

dualfecomplex dualfec_fecadd(const fecomplex a, const dualfecomplex b)
{
  dualfecomplex dc = { dualfe_feadd(a.re, b.re), dualfe_feadd(a.im, b.im) };
  return dc;
}

dualfecomplex dualfec_addfec(const dualfecomplex a, const fecomplex b)
{
  dualfecomplex dc = { dualfe_addfe(a.re, b.re), dualfe_addfe(a.im, b.im) };
  return dc;
}

#if 0
dualfecomplex dualfec_dcadd(const dcomplex a, const dualfecomplex b)
{
  dualfecomplex dc = { dualfe_dadd(a.re, b.re), dualfe_dadd(a.im, b.im) };
  return dc;
}

dualfecomplex dualfec_adddc(const dualfecomplex a, const dcomplex b)
{
  dualfecomplex dc = { dualfe_addd(a.re, b.re), dualfe_addd(a.im, b.im) };
  return dc;
}
#endif

dualfecomplex dualfec_sub(const dualfecomplex a, const dualfecomplex b)
{
  dualfecomplex dc = { dualfe_sub(a.re, b.re), dualfe_sub(a.im, b.im) };
  return dc;
}

dualfecomplex dualfec_dmul(const mantissa a, const dualfecomplex b)
{
  dualfecomplex dc = { dualfe_dmul(a, b.re), dualfe_dmul(a, b.im) };
  return dc;
}

dualfecomplex dualfec_muld(const dualfecomplex a, const mantissa b)
{
  dualfecomplex dc = { dualfe_muld(a.re, b), dualfe_muld(a.im, b) };
  return dc;
}

dualfecomplex dualfec_mul(const dualfecomplex a, const dualfecomplex b)
{
  dualfecomplex dc =
    { dualfe_sub(dualfe_mul(a.re, b.re), dualfe_mul(a.im, b.im))
    , dualfe_add(dualfe_mul(a.re, b.im), dualfe_mul(a.im, b.re))
    };
  return dc;
}

dualfecomplex dualfec_dcmul(const dcomplex a, const dualfecomplex b)
{
  dualfecomplex dc =
    { dualfe_sub(dualfe_dmul(a.re, b.re), dualfe_dmul(a.im, b.im))
    , dualfe_add(dualfe_dmul(a.re, b.im), dualfe_dmul(a.im, b.re))
    };
  return dc;
}

dualfecomplex dualfec_muldc(const dualfecomplex a, const dcomplex b)
{
  dualfecomplex dc =
    { dualfe_sub(dualfe_muld(a.re, b.re), dualfe_muld(a.im, b.im))
    , dualfe_add(dualfe_muld(a.re, b.im), dualfe_muld(a.im, b.re))
    };
  return dc;
}

dualfecomplex dualfec_fecmul(const fecomplex a, const dualfecomplex b)
{
  dualfecomplex dc =
    { dualfe_sub(dualfe_femul(a.re, b.re), dualfe_femul(a.im, b.im))
    , dualfe_add(dualfe_femul(a.re, b.im), dualfe_femul(a.im, b.re))
    };
  return dc;
}

dualfecomplex dualfec_mulfec(const dualfecomplex a, const fecomplex b)
{
  dualfecomplex dc =
    { dualfe_sub(dualfe_mulfe(a.re, b.re), dualfe_mulfe(a.im, b.im))
    , dualfe_add(dualfe_mulfe(a.re, b.im), dualfe_mulfe(a.im, b.re))
    };
  return dc;
}

#if 0
dualfecomplex dualfec_ddiv(const mantissa a, const dualfecomplex b) // FIXME verify
{
  mantissa b2 = dualfec_norm(b);
  dualfecomplex dc =
    { dualfe_divd(dualfe_dmul(a, b.re), b2)
    , dualfe_divd(dualfe_dmul(-a, b.im), b2)
    };
  return dc;
}

dualfecomplex dualfec_div(const dualfecomplex a, const dualfecomplex b) // FIXME verify
{
  mantissa b2 = dualfec_norm(b);
  dualfecomplex dc = dualfec_divd(dualfec_mul(a, dualfec_conj(b)), b2);
  return dc;
}
#endif

dualfecomplex dualfec_sqr(const dualfecomplex a)
{
  dualfecomplex dc =
    { dualfe_sub(dualfe_sqr(a.re), dualfe_sqr(a.im))
    , dualfe_mul_2si(dualfe_mul(a.re, a.im), 1)
    };
  return dc;
}

dualfecomplex dualfec_powi(dualfecomplex x, int n)
{
  const floatexp fone = fe_floatexp(1.0, 0);
  const floatexp fzero = fe_floatexp(0.0, 0);
  const dualfecomplex one = { { fone, { fzero, fzero } }, { fzero, { fzero, fzero } } };
  switch (n)
  {
    case 0: return one;
    case 1: return x;
    case 2: return dualfec_sqr(x);
    case 3: return dualfec_mul(x, dualfec_sqr(x));
    case 4: return dualfec_sqr(dualfec_sqr(x));
    case 5: return dualfec_mul(x, dualfec_sqr(dualfec_sqr(x)));
    case 6: return dualfec_sqr(dualfec_mul(x, dualfec_sqr(x)));
    case 7: return dualfec_mul(x, dualfec_sqr(dualfec_mul(x, dualfec_sqr(x))));
    case 8: return dualfec_sqr(dualfec_sqr(dualfec_sqr(x)));
    default:
    {
      dualfecomplex y = one;
      while (n > 1)
      {
        if (n & 1)
          y = dualfec_mul(y, x);
        x = dualfec_sqr(x);
        n >>= 1;
      }
      return dualfec_mul(x, y);
    }
  }
}


typedef struct __attribute__((packed))
{
  mantissa cr;
  mantissa ci;
  mantissa xr;
  mantissa xi;
  mantissa daa;
  mantissa dab;
  mantissa dba;
  mantissa dbb;
  mantissa dxa;
  mantissa dxb;
  mantissa dya;
  mantissa dyb;
  mantissa test1;
  mantissa test2;
  long antal;
  long bGlitch;
  long bNoGlitchDetection;
  mantissa log_m_nPower;
  mantissa tia;
} p_status_d;

typedef struct __attribute__((packed))
{
  floatexp cr;
  floatexp ci;
  floatexp xr;
  floatexp xi;
  floatexp daa;
  floatexp dab;
  floatexp dba;
  floatexp dbb;
  floatexp dxa;
  floatexp dxb;
  floatexp dya;
  floatexp dyb;
  mantissa test1;
  mantissa test2;
  long antal;
  long bGlitch;
  long bNoGlitchDetection;
  mantissa log_m_nPower;
  mantissa tia;
} p_status_fe;

#if 0
typedef struct __attribute__((packed))
{
  softfloat cr;
  softfloat ci;
  softfloat xr;
  softfloat xi;
  softfloat daa;
  softfloat dab;
  softfloat dba;
  softfloat dbb;
  softfloat dxa;
  softfloat dxb;
  softfloat dya;
  softfloat dyb;
  softfloat test1;
  softfloat test2;
  long antal;
  long bGlitch;
  long bNoGlitchDetection;
  softfloat log_m_nPower;
  softfloat tia;
} p_status_sf;
#endif

typedef struct __attribute__((packed))
{
  long BYTES;
  // for pixel -> parameter mapping
  int m_nX;
  int m_nY;
  uint JitterSeed;
  int JitterShape;
  mantissa JitterScale;
  floatexp m_pixel_center_x;
  floatexp m_pixel_center_y;
  floatexp m_pixel_scale;
  mantissa transform00;
  mantissa transform01;
  mantissa transform10;
  mantissa transform11;
  long ExponentialMap;
  // for result -> output mapping
  long stride_y;
  long stride_x;
  long stride_offset;
  // for iteration control
  mantissa m_nBailout;
  mantissa m_nBailout2;
  mantissa log_m_nBailout;
  mantissa log_m_nPower;
  long m_nMaxIter;
  long m_nRSize;
  long nMaxIter;
  long reference_size_x;
  short m_bNoGlitchDetection;
  short derivatives;
  short m_bAddReference;
  short m_nSmoothMethod;
  mantissa g_real;
  mantissa g_imag;
  mantissa norm_p;
  mantissa g_FactorAR;
  mantissa g_FactorAI;
  mantissa m_epsilon;
  // for series approximation
  long m_nMaxApproximation;
  int m_nApproxTerms;
  int approximation_type;
  // for guessing
  int UseGuessing;
  int GuessingPass;
  int g_nAddRefX;
  int g_nAddRefY;
  // for glitch selection
  short singleref;
  short glitch_select_argminz;
  // for ignore isolated glitches
  int ignore_isolated_neighbourhood; // 0 or 4 or 8
  // for hybrid
  short hybrid_loop_start;
  short hybrid_nstanzas;
  int hybrid_repeats[MAX_HYBRID_STANZAS];
  mantissa hybrid_log_powers[MAX_HYBRID_STANZAS];
  // 130kB (/2 for single) data follows
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
mantissa dither(uint x, uint y, uint c)
{
  return
    burtle_hash(x +
    burtle_hash(y +
    burtle_hash(c))) / (mantissa) (0x100000000L);
}

void GetPixelOffset
( __global const p_config *g
, const int i
, const int j
, mantissa *x
, mantissa *y
)
{
  uint c = g->JitterSeed;
  if (c != 0)
  {
    mantissa s = g->JitterScale;
    mantissa u = dither(i, j, 2 * c + 0);
    mantissa v = dither(i, j, 2 * c + 1);
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
          mantissa r = 0 < u && u < 1 ? sqrt(-2 * log(u)) : 0;
          mantissa t = 2 * 3.141592653589793 * v;
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
  int w = g->m_nX;
  int h = g->m_nY;
  if (g->UseGuessing)
  {
    w *= 2;
    h *= 2;
  }
  mantissa di = 0;
  mantissa dj = 0;
  GetPixelOffset(g, i, j, &di, &dj);
  mantissa u = i + di;
  mantissa v = j + dj;
  if (g->ExponentialMap)
  {
    mantissa re = -0.6931471805599453 * v / h; // log 2
    mantissa im = 6.283185307179586 * u / w; // 2 pi
    mantissa R = 0.5 * hypot((mantissa)w, (mantissa)h);
    mantissa r = exp(re);
    mantissa c = cos(im);
    mantissa s = sin(im);
    u = R * r * c;
    v = R * r * s;
  }
  else
  {
    u -= w / 2;
    v -= h / 2;
  }
  mantissa p = g->transform00 * u + g->transform01 * v;
  mantissa q = g->transform10 * u + g->transform11 * v;
  *x = fe_add(g->m_pixel_center_x, fe_muld(g->m_pixel_scale, p));
  *y = fe_add(g->m_pixel_center_y, fe_muld(g->m_pixel_scale, q));
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
#define SET_TRANS_GLITCH(x) (((x > 0) ? log2(x) : -1024.0) - 2048.0)
#define GET_TRANS_GLITCH(x) ((x) < 0.0f)

typedef struct __attribute__((packed))
{
  mantissa v[2];
} vec2;

vec2 v_normalize(const vec2 a)
{
  mantissa l = sqrt(a.v[0] * a.v[0] + a.v[1] * a.v[1]);
  if (! (l != 0.0))
  {
    l = 1.0;
  }
  vec2 r = { { a.v[0] / l, a.v[1] / l } };
  return r;
}

typedef struct __attribute__((packed))
{
  mantissa m[2][2];
} mat2;

vec2 vm_mul(const vec2 v, const mat2 m)
{
  vec2 r =
    { { v.v[0] * m.m[0][0] + v.v[1] * m.m[0][1]
    , v.v[0] * m.m[1][0] + v.v[1] * m.m[1][1]
    } };
  return r;
}

mat2 m_transpose(const mat2 m)
{
  mat2 r = { { { m.m[0][0], m.m[1][0] }, { m.m[0][1], m.m[1][1] } } };
  return r;
}

mat2 mm_mul(const mat2 a, const mat2 b)
{
  mat2 r = { { { 0.0, 0.0 }, { 0.0, 0.0 } } };
  for (int i = 0; i < 2; ++i)
  for (int j = 0; j < 2; ++j)
  for (int k = 0; k < 2; ++k)
  {
    r.m[i][j] += a.m[k][j] * b.m[i][k];
  }
  return r;
}

dcomplex d_compute_de(mantissa Dr, mantissa Di, mantissa Jxa, mantissa Jxb, mantissa Jya, mantissa Jyb, mantissa s, const mat2 TK)
{
  vec2 u = { { Dr, Di } };
  mat2 J = { { { Jxa * s, Jxb * s }, { Jya * s, Jyb * s } } };
  dcomplex v = { u.v[0], u.v[1] };
  mantissa num = dc_abs(v) * log(dc_abs(v));
  vec2 denv = vm_mul(v_normalize(u), mm_mul(m_transpose(J), TK));
  dcomplex den = { denv.v[0], denv.v[1] };
  return dc_ddiv(num, den);
}

dcomplex fe_compute_de(floatexp Dr, floatexp Di, floatexp Jxa, floatexp Jxb, floatexp Jya, floatexp Jyb, floatexp s, const mat2 TK)
{
  vec2 u = { { fe_double(Dr), fe_double(Di) } };
  mat2 J = { { { fe_double(fe_mul(Jxa, s)), fe_double(fe_mul(Jxb, s)) }, { fe_double(fe_mul(Jya, s)), fe_double(fe_mul(Jyb, s)) } } };
  dcomplex v = { u.v[0], u.v[1] };
  mantissa num = dc_abs(v) * log(dc_abs(v));
  vec2 denv = vm_mul(v_normalize(u), mm_mul(m_transpose(J), TK));
  dcomplex den = { denv.v[0], denv.v[1] };
  return dc_ddiv(num, den);
}

#if 0

dcomplex sf_compute_de(softfloat Dr, softfloat Di, softfloat Jxa, softfloat Jxb, softfloat Jya, softfloat Jyb, softfloat s, const mat2 TK)
{
  vec2 u = { { sf_to_double(Dr), sf_to_double(Di) } };
  mat2 J = { { { sf_to_double(sf_mul(Jxa, s)), sf_to_double(sf_mul(Jxb, s)) }, { sf_to_double(sf_mul(Jya, s)), sf_to_double(sf_mul(Jyb, s)) } } };
  dcomplex v = { u.v[0], u.v[1] };
  mantissa num = dc_abs(v) * log(dc_abs(v));
  vec2 denv = vm_mul(v_normalize(u), mm_mul(m_transpose(J), TK));
  dcomplex den = { denv.v[0], denv.v[1] };
  return dc_ddiv(num, den);
}

#endif

// forward declaration, defined per formula
void perturbation_double_loop
( __global const p_config   *g
, __global const mantissa   *m_refx
, __global const mantissa   *m_refy
, __global const mantissa   *m_refz
,                p_status_d *l
);

// forward declaration, defined per formula
void perturbation_floatexp_loop
( __global const p_config    *g
, __global const floatexp    *m_refx
, __global const floatexp    *m_refy
, __global const floatexp    *m_refz
,                p_status_fe *l
);

// forward declaration, defined per formula
void perturbation_scaled_loop
( __global const p_config    *g
, __global const mantissa    *m_refx
, __global const mantissa    *m_refy
, __global const mantissa    *m_refz
, __global const long        *m_refN
, __global const floatexp    *m_refX
, __global const floatexp    *m_refY
, __global const floatexp    *m_refZ
,                p_status_fe *l
);

#if 0
// forward declaration, defined per formula
void perturbation_softfloat_loop
( __global const p_config    *g
, __global const softfloat   *m_db_dxr
, __global const softfloat   *m_db_dxi
, __global const softfloat   *m_db_z
,                p_status_sf *l
);
#endif

// entry point
__kernel void guessing
( __global const p_config *g // configuration
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
  if ((x & 1) == 0 && (y & 1) == 0)
  {
    // don't guess top left of 2x2 subquad
    // it has already been computed
    return;
  }
  long ix = y * g->stride_y + x * g->stride_x + g->stride_offset;
  if (! g->m_bAddReference)
  {
    // the right/lower pixels of the 2x2 subquad have not been computed
    // yet by any reference; initialize/clear them now, guessing may
    // overwrite them with new values, otherwise the second pass will
    // fill them in
    long n = PIXEL_UNEVALUATED;
    if (n1)
    {
      n1[ix] = n >> 32;
    }
    if (n0)
    {
      n0[ix] = n;
    }
    if (nf)
    {
      nf[ix] = 0.0f;
    }
    if (phase)
    {
      phase[ix] = 0.0f;
    }
    if (dex)
    {
      dex[ix] = 0.0f;
    }
    if (dey)
    {
      dey[ix] = 0.0f;
    }
  }
  if (x + 2 >= g->m_nX)
  {
    // don't guess edge of image (simplicity)
    return;
  }
  if (y + 2 >= g->m_nY)
  {
    // don't guess edge of image (simplicity)
    return;
  }
  if (x == g->g_nAddRefX && y == g->g_nAddRefY)
  {
    // never guess reference, in case it is guessed glitched
    // prevents infinite loop in glitch correction
    return;
  }
  int x0 = x & ~1;
  int y0 = y & ~1;
  long ix0 = y0 * g->stride_y + x0 * g->stride_x + g->stride_offset;
  long ix1;
  if ((y & 1) == 0)
  {
    ix1 = y0 * g->stride_y + (x0 + 2) * g->stride_x + g->stride_offset;
  }
  else if ((x & 1) == 0)
  {
    ix1 = (y0 + 2) * g->stride_y + x0 * g->stride_x + g->stride_offset;
  }
  else
  {
    ix1 = (y0 + 2) * g->stride_y + (x0 + 2) * g->stride_x + g->stride_offset;
  }
  // read neighbouring pixels
  long n_0 = 0;
  long n_1 = 0;
  if (n1)
  {
    n_0 |= ((long)(int)(n1[ix0])) << 32;
    n_1 |= ((long)(int)(n1[ix1])) << 32;
    if (n0)
    {
      n_0 |= n0[ix0];
      n_1 |= n0[ix1];
    }
  }
  else
  {
    if (n0)
    {
      n_0 = (long)(int)(n0[ix0]); // sign extend
      n_1 = (long)(int)(n0[ix1]); // sign extend
    }
  }
  float nf_0 = 0.0f;
  float nf_1 = 0.0f;
  if (nf)
  {
    nf_0 = nf[ix0];
    nf_1 = nf[ix1];
  }
  if ( n_0 == n_1 && n_0 != PIXEL_UNEVALUATED &&
       GET_TRANS_GLITCH(nf_0) == GET_TRANS_GLITCH(nf_1) &&
       // only guess glitches or interior, not escaped exterior
       ( GET_TRANS_GLITCH(nf_0) || (n_0 >= g->m_nMaxIter) )
     )
  {
    // do the guessing
    long n = n_0;
    if (n1)
    {
      n1[ix] = n >> 32;
    }
    if (n0)
    {
      n0[ix] = n;
    }
    if (nf)
    {
      nf[ix] = (nf_0 + nf_1) * 0.5f;
    }
    if (phase)
    {
#ifdef TRIANGLE_INEQUALITY_AVERAGE
      phase[ix] = (phase[ix0] + phase[ix1]) * 0.5f;
#else
      float p0 = phase[ix0] * M_PI * 2;
      float p1 = phase[ix1] * M_PI * 2;
      float p = atan2(sin(p0) + sin(p1), cos(p0) + cos(p1)) / M_PI / 2;
      phase[ix] = p - floor(p);
#endif
    }
#ifdef KF_GUESS_DE_GEOMETRIC
#error KF_GUESS_DE_GEOMETRIC not supported in OpenCL
#endif
    if (dex)
    {
      dex[ix] = (dex[ix0] + dex[ix1]) * 0.5f;
    }
    if (dey)
    {
      dey[ix] = (dey[ix0] + dey[ix1]) * 0.5f;
    }
  }
}

// entry point
__kernel void glitch_select_2d
( __global const p_config *g // configuration
, __global const float    *nf // iteration count fractional part
, __global       float    *glitch_f // for each y, glitch flag, 1.0/0.0 = no glitch in this row
, __global       int      *glitch_x // for each y, x coordinate of selected glitch
, __global       int      *counts // for each y, number of glitches in row
)
{
  if (g->BYTES != sizeof(p_config)) return;
  int y = get_global_id(0);
  float sel_f = 1.0/0.0;
  int sel_x = 0;
  int count = 0;
  for (int x = 0; x < g->m_nX; ++x)
  {
    long ix = y * g->stride_y + x * g->stride_x + g->stride_offset;
    float f = nf[ix];
    if (GET_TRANS_GLITCH(f))
    {
      ++count;
      if (g->glitch_select_argminz)
      {
        if (f < sel_f)
        {
          sel_f = f;
          sel_x = x;
        }
      }
      else
      {
        float random = dither((uint)(x), (uint)(y), (uint)(g->m_bAddReference));
        if (random < sel_f)
        {
          sel_f = random;
          sel_x = x;
        }
      }
    }
  }
  glitch_f[y] = sel_f;
  glitch_x[y] = sel_x;
  counts[y] = count;
}

typedef struct __attribute__((packed))
{
  long count;
  int x;
  int y;
  float f;
} p_glitch;

__kernel void glitch_select_1d
( __global const p_config *g // configuration
, __global const float    *glitch_f // for each y, glitch flag, 1.0/0.0 = no glitch in this row
, __global const int      *glitch_x // for each y, x coordinate of selected glitch
, __global const int      *counts // for each y, count of glitches
, __global       p_glitch *out
)
{
  if (g->BYTES != sizeof(p_config)) return;
  long count = 0;
  float sel_f = 1.0/0.0;
  int sel_x = 0;
  int sel_y = 0;
  for (int y = 0; y < g->m_nY; ++y)
  {
    count += (long)(counts[y]);
    float f = glitch_f[y];
    if (f < sel_f)
    {
      sel_f = f;
      sel_x = glitch_x[y];
      sel_y = y;
    }
  }
  out->count = count;
  out->x = sel_x;
  out->y = sel_y;
  out->f = sel_f;
}

// entry point
__kernel void ignore_isolated_glitches
( __global const p_config *g // configuration
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
  if (! GET_TRANS_GLITCH(nf[ix])) return; // not glitched, nothing to do
  int sum = 0;
  for (int dy = -1; dy <= 1; ++dy)
  {
    int y1 = y + dy;
    if (y1 < 0) continue;
    if (y1 >= g->m_nY) continue;
    for (int dx = -1; dx <= 1; ++dx)
    {
      int x1 = x + dx;
      if (x1 < 0) continue;
      if (x1 >= g->m_nX) continue;
      if (g->ignore_isolated_neighbourhood == 4 && dy && dx) continue;
      if (dy == 0 && dx == 0) continue;
      long ix1 = y1 * g->stride_y + x1 * g->stride_x + g->stride_offset;
      sum += GET_TRANS_GLITCH(nf[ix1]) ? 1 : 0;
    }
  }
  if (sum > 0) return; // glitches in neighbourhood, nothing to do
  // average neighbours
  sum = 0;
  ulong sum_n = 0;
  float sum_nf = 0;
#ifdef TRIANGLE_INEQUALITY_AVERAGE
  float sum_phase = 0;
#else
  float sum_phase_cos = 0;
  float sum_phase_sin = 0;
#endif
  float sum_dex = 0;
  float sum_dey = 0;
  for (int dy = -1; dy <= 1; ++dy)
  {
    int y1 = y + dy;
    if (y1 < 0) continue;
    if (y1 >= g->m_nY) continue;
    for (int dx = -1; dx <= 1; ++dx)
    {
      int x1 = x + dx;
      if (x1 < 0) continue;
      if (x1 >= g->m_nX) continue;
      if (g->ignore_isolated_neighbourhood == 4 && dy && dx) continue;
      if (dy == 0 && dx == 0) continue;
      long ix1 = y1 * g->stride_y + x1 * g->stride_x + g->stride_offset;
      uint  my_n1 = 0; if (n1) my_n1 = n1[ix1];
      uint  my_n0 = 0; if (n0) my_n0 = n0[ix1];
      float my_nf = 0; if (nf) my_nf = nf[ix1];
#ifdef TRIANGLE_INEQUALITY_AVERAGE
      float my_phase = 0; if (phase) my_phase = phase[ix1];
#else
      float my_phase_cos = 0, my_phase_sin = 0; if (phase)
      {
        float my_phase = phase[ix1];
        my_phase_cos = cos(6.283185307179586 * my_phase);
        my_phase_sin = sin(6.283185307179586 * my_phase);
      }
#endif
      float my_dex = 0; if (dex) my_dex = dex[ix1];
      float my_dey = 0; if (dey) my_dey = dey[ix1];
      sum += 1;
      sum_n += (my_n1 << 32) + my_n0;
      sum_nf += my_nf;
#ifdef TRIANGLE_INEQUALITY_AVERAGE
      sum_phase += my_phase;
#else
      sum_phase_cos += my_phase_cos;
      sum_phase_sin += my_phase_sin;
#endif
      sum_dex += my_dex;
      sum_dey += my_dey;
    }
  }
  if (sum == 0) return;
  float s = 1.0 / (float)(sum);
  float avg_nf = s * ((float)(sum_n % sum) + sum_nf);
  ulong avg_n = (sum_n / sum) + (ulong)(floor(avg_nf));
  avg_nf -= floor(avg_nf);
  uint avg_n1 = avg_n >> 32;
  uint avg_n0 = avg_n & 0xFFFFFFFFU;
#ifdef TRIANGLE_INEQUALITY_AVERAGE
  float avg_phase = sum_phase * s;
#else
  float avg_phase = atan2(sum_phase_sin, sum_phase_cos);
#endif
  float avg_dex = sum_dex * s;
  float avg_dey = sum_dey * s;
  if (n1) n1[ix] = avg_n1;
  if (n0) n0[ix] = avg_n0;
  if (nf) nf[ix] = avg_nf;
  if (phase) phase[ix] = avg_phase;
  if (dex) dex[ix] = avg_dex;
  if (dey) dey[ix] = avg_dey;
}

// entry point
__kernel void perturbation_double
( __global const p_config *g // configuration including series approximation coefficients
, __global const mantissa *m_refx // reference orbit re
, __global const mantissa *m_refy // reference orbit im
, __global const mantissa *m_refz // reference orbit (re^2+im^2)*glitch_threshold
, __global       uint   *n1  // iteration count msb, may be null
, __global       uint   *n0  // iteration count lsb
, __global       float  *nf  // iteration count fractional part
, __global       float  *phase // final angle, may be null
, __global       float  *dex // directional de x, may be null
, __global       float  *dey // directional de y, may be null
// unused but needed for compatibility
, __global const long     *m_refN // reference orbit special iteration
, __global const floatexp *m_refX // reference orbit special re
, __global const floatexp *m_refY // reference orbit special im
, __global const floatexp *m_refZ // reference orbit special (re^2+im^2)*glitch_threshold
)
{
  if (g->BYTES != sizeof(p_config)) return;
  int y = get_global_id(0);
  int x = get_global_id(1);
  if (g->UseGuessing)
  {
    x *= 2;
    y *= 2;
    if ((g->GuessingPass & 1) == 1)
    {
      x += 1;
    }
    if ((g->GuessingPass & 2) == 2)
    {
      y += 1;
    }
  }
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
  bool first = ! g->m_bAddReference;
  if (g->UseGuessing)
  {
    first &= (g->GuessingPass == 0);
  }
  if (first || orig == PIXEL_UNEVALUATED || GET_TRANS_GLITCH(origf)) // first or fresh or glitch
  {
    const floatexp zero = fe_floatexp(0.0, 0);
    const floatexp one  = fe_floatexp(1.0, 0);
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
    GetPixelCoordinates(g, x, y, &D0r, &D0i);

    long antal;
    floatexp TDnr;
    floatexp TDni;
    floatexp dxa1, dxb1, dya1, dyb1;
    DoApproximationM(g, &antal, D0r, D0i, &TDnr, &TDni, &dxa1, &dxb1, &dya1, &dyb1);
    // in
    p_status_d l =
      { fe_double(D0r)
      , fe_double(D0i)
      , fe_double(TDnr)
      , fe_double(TDni)
      , fe_double(daa0)
      , fe_double(dab0)
      , fe_double(dba0)
      , fe_double(dbb0)
      , fe_double(dxa1)
      , fe_double(dxb1)
      , fe_double(dya1)
      , fe_double(dyb1)
      , 0
      , 0
      , antal
      , false
      , g->m_bNoGlitchDetection || (x == g->g_nAddRefX && y == g->g_nAddRefY)
      , g->log_m_nPower
      , 0
      };
    // core per pixel calculation
    perturbation_double_loop(g, m_refx, m_refy, m_refz, &l);
    // out
    mantissa Dr = l.xr;
    mantissa Di = l.xi;
    mantissa test1 = l.test1;
    mantissa test2 = l.test2;
    antal = l.antal;
    long bGlitch = l.bGlitch;
    dcomplex de = { 0.0, 0.0 };
    if (g->derivatives)
    {
      const mantissa s = fe_double(g->m_pixel_scale);
      const mat2 TK = { { { g->transform00, g->transform01 }, { g->transform10, g->transform11 } } };
      de = d_compute_de(Dr, Di, l.dxa, l.dxb, l.dya, l.dyb, s, TK);
    }
    // output iteration data
    if (antal >= g->m_nMaxIter){
      antal = g->m_nMaxIter;
      if (n1) n1[ix] = antal >> 32;
      if (n0) n0[ix] = antal;
      if (nf) nf[ix] = 0;
#ifdef TRIANGLE_INEQUALITY_AVERAGE
      if (phase) phase[ix] = l.tia;
#else
      if (phase) phase[ix] = 0;
#endif
      if (dex) dex[ix] = 0;
      if (dey) dey[ix] = 0;
    }
    else{
      if (x == g->g_nAddRefX && y == g->g_nAddRefY)
      {
        // never consider the pixel of the reference to be glitched
        bGlitch = false;
      }
      if (n1) n1[ix] = antal >> 32;
      if (n0) n0[ix] = antal;
      mantissa de_multiplier = 1;
      if ((dex || dey) && g->ExponentialMap)
      {
        mantissa dx = 0, dy = 0;
        GetPixelOffset(g, x, y, &dx, &dy);
        de_multiplier = exp2((y + dy) / (g->UseGuessing ? g->m_nY * 2 : g->m_nY));
      }
      if (dex) dex[ix] = de.re * de_multiplier;
      if (dey) dey[ix] = de.im * de_multiplier;
      if (phase)
      {
#ifdef TRIANGLE_INEQUALITY_AVERAGE
        phase[ix] = l.tia;
#else
        mantissa p = atan2(Di, Dr) / (M_PI * 2);
        p -= floor(p);
        phase[ix] = p;
#endif
      }
      if (!bGlitch && g->m_nSmoothMethod == 1){
        mantissa p = g->norm_p;
        if (! (p < 1.0 / 0.0)) p = 1;
        mantissa div = pow(test1, 1 / p) - pow(test2, 1 / p);
        mantissa f = (pow(test1, 1 / p) - g->m_nBailout) / div;
        if (!ISFLOATOK(f))
          f = 0;
        if (nf) nf[ix] = f;
      }
      else if (!bGlitch && g->m_nSmoothMethod == 0){
        mantissa t = log(log(sqrt(test1)) / g->log_m_nBailout) / l.log_m_nPower;
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
, __global const floatexp *m_refx // reference orbit re
, __global const floatexp *m_refy // reference orbit im
, __global const floatexp *m_refz // reference orbit (re^2+im^2)*glitch_threshold
, __global       uint     *n1  // iteration count msb, may be null
, __global       uint     *n0  // iteration count lsb
, __global       float    *nf  // iteration count fractional part
, __global       float  *phase // final angle, may be null
, __global       float    *dex // directional de x, may be null
, __global       float    *dey // directional de y, may be null
, __global const long     *m_refN // unused
, __global const floatexp *m_refX // unused
, __global const floatexp *m_refY // unused
, __global const floatexp *m_refZ // unused
)
{
  if (g->BYTES != sizeof(p_config)) return;
  int y = get_global_id(0);
  int x = get_global_id(1);
  if (g->UseGuessing)
  {
    x *= 2;
    y *= 2;
    if ((g->GuessingPass & 1) == 1)
    {
      x += 1;
    }
    if ((g->GuessingPass & 2) == 2)
    {
      y += 1;
    }
  }
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
  bool first = ! g->m_bAddReference;
  if (g->UseGuessing)
  {
    first &= (g->GuessingPass == 0);
  }
  if (first || orig == PIXEL_UNEVALUATED || origf < 0) // first or fresh or glitch
  {
    const floatexp zero = fe_floatexp(0.0, 0);
    const floatexp one  = fe_floatexp(1.0, 0);
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
    GetPixelCoordinates(g, x, y, &D0r, &D0i);

    long antal;
    floatexp TDnr;
    floatexp TDni;
    floatexp dxa1, dxb1, dya1, dyb1;
    DoApproximationM(g, &antal, D0r, D0i, &TDnr, &TDni, &dxa1, &dxb1, &dya1, &dyb1);
    // in
    p_status_fe l =
      { D0r
      , D0i
      , TDnr
      , TDni
      , daa0
      , dab0
      , dba0
      , dbb0
      , dxa1
      , dxb1
      , dya1
      , dyb1
      , 0
      , 0
      , antal
      , false
      , g->m_bNoGlitchDetection || (x == g->g_nAddRefX && y == g->g_nAddRefY)
      , g->log_m_nPower
      };
    // core per pixel calculation
    perturbation_floatexp_loop(g, m_refx, m_refy, m_refz, &l);
    // out
    floatexp Dr = l.xr;
    floatexp Di = l.xi;
    mantissa test1 = l.test1;
    mantissa test2 = l.test2;
    antal = l.antal;
    long bGlitch = l.bGlitch;
    dcomplex de = { 0.0, 0.0 };
    if (g->derivatives)
    {
      const floatexp s = g->m_pixel_scale;
      const mat2 TK = { { { g->transform00, g->transform01 }, { g->transform10, g->transform11 } } };
      de = fe_compute_de(Dr, Di, l.dxa, l.dxb, l.dya, l.dyb, s, TK);
    }
    // output iteration data
    if (antal >= g->m_nMaxIter){
      antal = g->m_nMaxIter;
      if (n1) n1[ix] = antal >> 32;
      if (n0) n0[ix] = antal;
      if (nf) nf[ix] = 0;
#ifdef TRIANGLE_INEQUALITY_AVERAGE
      if (phase) phase[ix] = l.tia;
#else
      if (phase) phase[ix] = 0;
#endif
      if (dex) dex[ix] = 0;
      if (dey) dey[ix] = 0;
    }
    else{
      if (x == g->g_nAddRefX && y == g->g_nAddRefY)
      {
        // never consider the pixel of the reference to be glitched
        bGlitch = false;
      }
      if (n1) n1[ix] = antal >> 32;
      if (n0) n0[ix] = antal;
      mantissa de_multiplier = 1;
      if ((dex || dey) && g->ExponentialMap)
      {
        mantissa dx = 0, dy = 0;
        GetPixelOffset(g, x, y, &dx, &dy);
        de_multiplier = exp2((y + dy) / (g->UseGuessing ? g->m_nY * 2 : g->m_nY));
      }
      if (dex) dex[ix] = de.re * de_multiplier;
      if (dey) dey[ix] = de.im * de_multiplier;
      if (phase)
      {
#ifdef TRIANGLE_INEQUALITY_AVERAGE
        phase[ix] = l.tia;
#else
        mantissa p = atan2(fe_double(Di), fe_double(Dr)) / (M_PI * 2);
        p -= floor(p);
        phase[ix] = p;
#endif
      }
      if (!bGlitch && g->m_nSmoothMethod == 1){
        mantissa p = g->norm_p;
        if (! (p < 1.0 / 0.0)) p = 1;
        mantissa div = pow(test1, 1 / p) - pow(test2, 1 / p);
        mantissa f = (pow(test1, 1 / p) - g->m_nBailout) / div;
        if (!ISFLOATOK(f))
          f = 0;
        if (nf) nf[ix] = f;
      }
      else if (!bGlitch && g->m_nSmoothMethod == 0){
        mantissa t = log(log(sqrt(test1)) / g->log_m_nBailout) / l.log_m_nPower;
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
__kernel void perturbation_scaled
( __global const p_config *g // configuration including series approximation coefficients
, __global const mantissa   *m_refx // reference orbit re
, __global const mantissa   *m_refy // reference orbit im
, __global const mantissa   *m_refz // reference orbit (re^2+im^2)*glitch_threshold
, __global       uint     *n1  // iteration count msb, may be null
, __global       uint     *n0  // iteration count lsb
, __global       float    *nf  // iteration count fractional part
, __global       float  *phase // final angle, may be null
, __global       float    *dex // directional de x, may be null
, __global       float    *dey // directional de y, may be null
, __global const long     *m_refN // reference orbit special iteration
, __global const floatexp *m_refX // reference orbit special re
, __global const floatexp *m_refY // reference orbit special im
, __global const floatexp *m_refZ // reference orbit special (re^2+im^2)*glitch_threshold
)
{
  if (g->BYTES != sizeof(p_config)) return;
  int y = get_global_id(0);
  int x = get_global_id(1);
  if (g->UseGuessing)
  {
    x *= 2;
    y *= 2;
    if ((g->GuessingPass & 1) == 1)
    {
      x += 1;
    }
    if ((g->GuessingPass & 2) == 2)
    {
      y += 1;
    }
  }
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
  bool first = ! g->m_bAddReference;
  if (g->UseGuessing)
  {
    first &= (g->GuessingPass == 0);
  }
  if (first || orig == PIXEL_UNEVALUATED || origf < 0) // first or fresh or glitch
  {
    const floatexp zero = fe_floatexp(0.0, 0);
    const floatexp one  = fe_floatexp(1.0, 0);
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
    floatexp daa0 = g->m_pixel_scale; // one
    floatexp dab0 = zero;
    floatexp dba0 = zero;
    floatexp dbb0 = g->m_pixel_scale; // one
    GetPixelCoordinates(g, x, y, &D0r, &D0i);

    long antal;
    floatexp TDnr;
    floatexp TDni;
    floatexp dxa1, dxb1, dya1, dyb1;
    DoApproximationM(g, &antal, D0r, D0i, &TDnr, &TDni, &dxa1, &dxb1, &dya1, &dyb1);
    // in
    p_status_fe l =
      { D0r
      , D0i
      , TDnr
      , TDni
      , daa0
      , dab0
      , dba0
      , dbb0
      , fe_mul(dxa1, g->m_pixel_scale)
      , fe_mul(dxb1, g->m_pixel_scale)
      , fe_mul(dya1, g->m_pixel_scale)
      , fe_mul(dyb1, g->m_pixel_scale)
      , 0
      , 0
      , antal
      , false
      , g->m_bNoGlitchDetection || (x == g->g_nAddRefX && y == g->g_nAddRefY)
      , g->log_m_nPower
      };
    // core per pixel calculation
    perturbation_scaled_loop(g, m_refx, m_refy, m_refz, m_refN, m_refX, m_refY, m_refZ, &l);
    // out
    floatexp Dr = l.xr;
    floatexp Di = l.xi;
    mantissa test1 = l.test1;
    mantissa test2 = l.test2;
    antal = l.antal;
    long bGlitch = l.bGlitch;
    dcomplex de = { 0.0, 0.0 };
    if (g->derivatives)
    {
      const floatexp s = one; // g->m_pixel_scale;
      const mat2 TK = { { { g->transform00, g->transform01 }, { g->transform10, g->transform11 } } };
      de = fe_compute_de(Dr, Di, l.dxa, l.dxb, l.dya, l.dyb, s, TK);
    }
    // output iteration data
    if (antal >= g->m_nMaxIter){
      antal = g->m_nMaxIter;
      if (n1) n1[ix] = antal >> 32;
      if (n0) n0[ix] = antal;
      if (nf) nf[ix] = 0;
#ifdef TRIANGLE_INEQUALITY_AVERAGE
      if (phase) phase[ix] = l.tia;
#else
      if (phase) phase[ix] = 0;
#endif
      if (dex) dex[ix] = 0;
      if (dey) dey[ix] = 0;
    }
    else{
      if (x == g->g_nAddRefX && y == g->g_nAddRefY)
      {
        // never consider the pixel of the reference to be glitched
        bGlitch = false;
      }
      if (n1) n1[ix] = antal >> 32;
      if (n0) n0[ix] = antal;
      mantissa de_multiplier = 1;
      if ((dex || dey) && g->ExponentialMap)
      {
        mantissa dx = 0, dy = 0;
        GetPixelOffset(g, x, y, &dx, &dy);
        de_multiplier = exp2((y + dy) / (g->UseGuessing ? g->m_nY * 2 : g->m_nY));
      }
      if (dex) dex[ix] = de.re * de_multiplier;
      if (dey) dey[ix] = de.im * de_multiplier;
      if (phase)
      {
#ifdef TRIANGLE_INEQUALITY_AVERAGE
        phase[ix] = l.tia;
#else
        mantissa p = atan2(fe_double(Di), fe_double(Dr)) / (M_PI * 2);
        p -= floor(p);
        phase[ix] = p;
#endif
      }
      if (!bGlitch && g->m_nSmoothMethod == 1){
        mantissa p = g->norm_p;
        if (! (p < 1.0 / 0.0)) p = 1;
        mantissa div = pow(test1, 1 / p) - pow(test2, 1 / p);
        mantissa f = (pow(test1, 1 / p) - g->m_nBailout) / div;
        if (!ISFLOATOK(f))
          f = 0;
        if (nf) nf[ix] = f;
      }
      else if (!bGlitch && g->m_nSmoothMethod == 0){
        mantissa t = log(log(sqrt(test1)) / g->log_m_nBailout) / l.log_m_nPower;
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

#if 0

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
  if (g->UseGuessing && ((x | y) & 1) == 1)
  {
    // don't calculate the right/lower pixels of 2x2 subquads until second pass
    return;
  }
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
  bool first = ! g->m_bAddReference;
  if (g->UseGuessing)
  {
    first &= g->GuessingPass == 0;
  }
  if (first || orig == PIXEL_UNEVALUATED || origf < 0) // first or fresh or glitch
  {
    const floatexp zero = fe_floatexp(0.0, 0);
    const floatexp one  = fe_floatexp(1.0, 0);
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
    GetPixelCoordinates(g, x, y, &D0r, &D0i);

    long antal;
    floatexp TDnr;
    floatexp TDni;
    floatexp dxa1, dxb1, dya1, dyb1;
    DoApproximationM(g, &antal, D0r, D0i, &TDnr, &TDni, &dxa1, &dxb1, &dya1, &dyb1);
    // in
    p_status_sf l =
      { sf_from_floatexp(D0r)
      , sf_from_floatexp(D0i)
      , sf_from_floatexp(TDnr)
      , sf_from_floatexp(TDni)
      , sf_from_floatexp(daa0)
      , sf_from_floatexp(dab0)
      , sf_from_floatexp(dba0)
      , sf_from_floatexp(dbb0)
      , sf_from_floatexp(dxa1)
      , sf_from_floatexp(dxb1)
      , sf_from_floatexp(dxa1)
      , sf_from_floatexp(dxb1)
      , sf_from_double(0.0)
      , sf_from_double(0.0)
      , antal
      , false
      , sf_from_double(g->log_m_nPower)
      , sf_from_double(0.0)
      };
    // core per pixel calculation
    perturbation_softfloat_loop(g, m_db_dxr, m_db_dxi, m_db_z, &l);
    // out
    softfloat Dr = l.xr;
    softfloat Di = l.xi;
    mantissa test1 = sf_to_double(l.test1);
    mantissa test2 = sf_to_double(l.test2);
    antal = l.antal;
    long bGlitch = l.bGlitch;
    dcomplex de = { 0.0, 0.0 };
    if (g->derivatives)
    {
      const softfloat s = sf_from_floatexp(g->m_pixel_scale);
      const mat2 TK = { { { g->transform00, g->transform01 }, { g->transform10, g->transform11 } } };
      de = sf_compute_de(Dr, Di, l.dxa, l.dxb, l.dya, l.dyb, s, TK);
    }
    // output iteration data
    if (antal >= g->m_nMaxIter){
      antal = g->m_nMaxIter;
      if (n1) n1[ix] = antal >> 32;
      if (n0) n0[ix] = antal;
      if (nf) nf[ix] = 0;
#ifdef TRIANGLE_INEQUALITY_AVERAGE
      if (phase) phase[ix] = sf_to_float(l.tia);
#else
      if (phase) phase[ix] = 0;
#endif
      if (dex) dex[ix] = 0;
      if (dey) dey[ix] = 0;
    }
    else{
      if (n1) n1[ix] = antal >> 32;
      if (n0) n0[ix] = antal;
      mantissa de_multiplier = 1;
      if ((dex || dey) && g->ExponentialMap)
      {
        mantissa dx = 0, dy = 0;
        GetPixelOffset(g, x, y, &dx, &dy);
        de_multiplier = exp2((y + dy) / g->m_nY);
      }
      if (dex) dex[ix] = de.re * de_multiplier;
      if (dey) dey[ix] = de.im * de_multiplier;
      if (phase)
      {
#ifdef TRIANGLE_INEQUALITY_AVERAGE
        phase[ix] = sf_to_float(l.tia);
#else
        mantissa p = atan2(sf_double(Di), sf_double(Dr)) / (M_PI * 2);
        p -= floor(p);
        phase[ix] = p;
#endif
      }
      if (!bGlitch && g->m_nSmoothMethod == 1){
        mantissa p = g->norm_p;
        if (! (p < 1.0 / 0.0)) p = 1;
        mantissa div = pow(test1, 1 / p) - pow(test2, 1 / p);
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
        mantissa t = log(log(sqrt(test1)) / g->log_m_nBailout) / sf_to_double(l.log_m_nPower);
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

#endif

mantissa pnorm(mantissa g_real, mantissa g_imag, mantissa p, mantissa x, mantissa y)
{
  if (g_real == 1 && g_imag == 1 && p == 2)
  {
    return x * x + y * y;
  }
  if (p == 1)
  {
    return fabs(g_real * fabs(x) + g_imag * fabs(y));
  }
  if (p == 1.0/0.0)
  {
    return fabs(max(g_real * fabs(x), g_imag * fabs(y)));
  }
  return fabs(g_real * pow(fabs(x), p) + g_imag * pow(fabs(y), p));
}
