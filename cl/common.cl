#pragma OPENCL EXTENSION cl_khr_fp64: enable

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

floatexp fe_neg(const floatexp f)
{
  floatexp fe = { -f.val, f.exp };
  return fe;
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

typedef struct
{
  double re;
  double im;
} dcomplex;

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
  int antal;
  int bGlitch;
  double test1;
  double test2;
} p_status;


typedef struct
{
  int count;
  int width;
  int height;
  int m_nX;
  int m_nY;
  int antal;
  int m_nMaxIter;
  int m_nGlitchIter;
  int m_bNoGlitchDetection;
  int m_nSmoothMethod;
  int m_nPower;
  int m_nMaxApproximation;
  int m_nTerms;
  int m_bAddReference;
  double m_nBailout;
  double m_nBailout2;
  double g_real;
  double g_imag;
  double g_FactorAR;
  double g_FactorAI;
  double m_C;
  double m_S;
} p_config;


__kernel void series_approximation_double
( __global const p_config *config0
, __global const double *m_pDX
, __global const double *m_pDY
, __global const floatexp *m_APr
, __global const floatexp *m_APi
, __global double *cx
, __global int *pixels
, __global float *trans
)
{
  p_config config = *config0;
  int ix = get_global_id(0);
  if (0 <= ix && ix < config.count)
  {
    if (! config.m_bAddReference)
    {
      pixels[ix] = -1;
      trans[ix] = 0;
    }
    if (pixels[ix] == -1)
    {
      int y = ix % config.height;
      int x = ix / config.height;
      double dbD0r = m_pDX[config.m_nX / 2] + config.m_C*(m_pDX[x] - m_pDX[config.m_nX / 2]) + config.m_S*(m_pDY[y] - m_pDY[config.m_nY / 2]);
      double dbD0i = m_pDY[config.m_nY / 2] - config.m_S*(m_pDX[x] - m_pDX[config.m_nX / 2]) + config.m_C*(m_pDY[y] - m_pDY[config.m_nY / 2]);
      floatexp D0r = fe_floatexp(dbD0r, 0);
      floatexp D0i = fe_floatexp(dbD0i, 0);
      floatexp Dr = D0r;
      floatexp Di = D0i;
      if (config.m_nMaxApproximation)
      {
        floatexp Dnr = fe_sub(fe_mul(m_APr[0] , D0r) , fe_mul(m_APi[0] , D0i));
        floatexp Dni = fe_add(fe_mul(m_APr[0] , D0i) , fe_mul(m_APi[0] , D0r));
        floatexp D_r = fe_sub(fe_mul(D0r,D0r), fe_mul(D0i,D0i));
        floatexp D_i = fe_mul_2si(fe_mul(D0r, D0i), 1);
        Dnr = fe_add(Dnr , fe_sub(fe_mul(m_APr[1] , D_r) , fe_mul(m_APi[1] , D_i)));
        Dni = fe_add(Dni , fe_add(fe_mul(m_APr[1] , D_i) , fe_mul(m_APi[1] , D_r)));
        for (int k = 2; k < config.m_nTerms; ++k)
        {
          floatexp t = fe_sub(fe_mul(D_r,D0r) , fe_mul(D_i,D0i));
          D_i =        fe_add(fe_mul(D_r,D0i) , fe_mul(D_i,D0r));
          D_r = t;
          Dnr = fe_add(Dnr, fe_sub(fe_mul(m_APr[k] , D_r) , fe_mul(m_APi[k] , D_i)));
          Dni = fe_add(Dni, fe_add(fe_mul(m_APr[k] , D_i) , fe_mul(m_APi[k] , D_r)));
        }
        Dr = Dnr;
        Di = Dni;
      }
      cx[4 * ix + 0] = dbD0r;
      cx[4 * ix + 1] = dbD0i;
      cx[4 * ix + 2] = fe_double(Dr);
      cx[4 * ix + 3] = fe_double(Di);
    }
  }
}


__kernel void series_approximation_floatexp
( __global const p_config *config0
, __global const floatexp *m_DX
, __global const floatexp *m_DY
, __global const floatexp *m_APr
, __global const floatexp *m_APi
, __global floatexp *cx
, __global int *pixels
, __global float *trans
)
{
  p_config config = *config0;
  int ix = get_global_id(0);
  if (0 <= ix && ix < config.count)
  {
    if (! config.m_bAddReference)
    {
      pixels[ix] = -1;
      trans[ix] = 0;
    }
    if (pixels[ix] == -1)
    {
      int y = ix % config.height;
      int x = ix / config.height;
      floatexp c = fe_floatexp(config.m_C, 0);
      floatexp s = fe_floatexp(config.m_S, 0);
      floatexp dbD0r = fe_add(m_DX[config.m_nX / 2] , fe_add(fe_mul(c , fe_sub(m_DX[x] , m_DX[config.m_nX / 2])) , fe_mul(s , fe_sub(m_DY[y] , m_DY[config.m_nY / 2]))));
      floatexp dbD0i = fe_sub(m_DY[config.m_nY / 2] , fe_add(fe_mul(s , fe_sub(m_DX[x] , m_DX[config.m_nX / 2])) , fe_mul(c , fe_sub(m_DY[y] , m_DY[config.m_nY / 2]))));
      floatexp D0r = dbD0r;
      floatexp D0i = dbD0i;
      floatexp Dr = D0r;
      floatexp Di = D0i;
      if (config.m_nMaxApproximation)
      {
        floatexp Dnr = fe_sub(fe_mul(m_APr[0] , D0r) , fe_mul(m_APi[0] , D0i));
        floatexp Dni = fe_add(fe_mul(m_APr[0] , D0i) , fe_mul(m_APi[0] , D0r));
        floatexp D_r = fe_sub(fe_mul(D0r,D0r), fe_mul(D0i,D0i));
        floatexp D_i = fe_mul_2si(fe_mul(D0r, D0i), 1);
        Dnr = fe_add(Dnr , fe_sub(fe_mul(m_APr[1] , D_r) , fe_mul(m_APi[1] , D_i)));
        Dni = fe_add(Dni , fe_add(fe_mul(m_APr[1] , D_i) , fe_mul(m_APi[1] , D_r)));
        for (int k = 2; k < config.m_nTerms; ++k)
        {
          floatexp t = fe_sub(fe_mul(D_r,D0r) , fe_mul(D_i,D0i));
          D_i =        fe_add(fe_mul(D_r,D0i) , fe_mul(D_i,D0r));
          D_r = t;
          Dnr = fe_add(Dnr, fe_sub(fe_mul(m_APr[k] , D_r) , fe_mul(m_APi[k] , D_i)));
          Dni = fe_add(Dni, fe_add(fe_mul(m_APr[k] , D_i) , fe_mul(m_APi[k] , D_r)));
        }
        Dr = Dnr;
        Di = Dni;
      }
      cx[4 * ix + 0] = D0r;
      cx[4 * ix + 1] = D0i;
      cx[4 * ix + 2] = Dr;
      cx[4 * ix + 3] = Di;
    }
  }
}
