<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" />
<xsl:template match="/">

#include &lt;string&gt;
#include &lt;sstream&gt;

#include "common_cl.c"

#include "double_pre_cl.c"
#include "double_pre_r_cl.c"
#include "double_pre_c_cl.c"
#include "double_pre_m_cl.c"
#include "double_post_cl.c"
#include "double_post_r_cl.c"
#include "double_post_c_cl.c"
#include "double_post_m_cl.c"

#include "floatexp_pre_cl.c"
#include "floatexp_pre_r_cl.c"
#include "floatexp_pre_c_cl.c"
#include "floatexp_pre_m_cl.c"
#include "floatexp_post_cl.c"
#include "floatexp_post_r_cl.c"
#include "floatexp_post_c_cl.c"
#include "floatexp_post_m_cl.c"

#if 0
#include "softfloat_pre_cl.c"
#include "softfloat_pre_r_cl.c"
#include "softfloat_pre_c_cl.c"
#include "softfloat_pre_m_cl.c"
#include "softfloat_post_cl.c"
#include "softfloat_post_r_cl.c"
#include "softfloat_post_c_cl.c"
#include "softfloat_post_m_cl.c"
#endif

#define STR(s) #s

static const char *perturbation_scaled_loop_empty = STR(

void perturbation_scaled_loop
( __global const p_config    *g
, __global const double      *m_refx
, __global const double      *m_refy
, __global const double      *m_refz
, __global const long        *m_refN
, __global const floatexp    *m_refX
, __global const floatexp    *m_refY
, __global const floatexp    *m_refZ
,                p_status_fe *l
)
{
}

);


<xsl:for-each select="formulas/group/formula">

static const char *perturbation_opencl_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_0 = STR(
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
{
  const dcomplex X = { Xr COMMA Xi } COMMA x = { xr COMMA xi } COMMA Xx = { Xxr COMMA Xxi } COMMA c = { cr COMMA ci };
  dcomplex xn;
@cldc {
        <xsl:value-of select="perturbation" />
      }
  xrn = xn.re;
  xin = xn.im;
}
</xsl:when>
<xsl:when test="perturbation/@t='R'">
@cld  {
        <xsl:value-of select="perturbation" />
      }
</xsl:when>
</xsl:choose>
);

static const char *perturbation_opencl_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_1 = STR(
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@cld  {
        <xsl:value-of select="derivative" />
      }
</xsl:when>
<xsl:when test="derivative/@t='C'">
@cldc {
        <xsl:value-of select="derivative" />
      }
</xsl:when>
</xsl:choose>
@cldc {
        <xsl:value-of select="perturbation" />
      }
</xsl:when>
<xsl:when test="perturbation/@t='R'">
<xsl:choose>
<xsl:when test="derivative/@t='C'">
@cldc {
        <xsl:value-of select="derivative" />
      }
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@cld  {
        <xsl:value-of select="derivative" />
      }
</xsl:when>
</xsl:choose>
@cld  {
        <xsl:value-of select="perturbation" />
      }
</xsl:when>
</xsl:choose>
);

static const char *perturbation_opencl_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_0 = STR(
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
{
  const fecomplex X = { Xr COMMA Xi } COMMA x = { xr COMMA xi } COMMA Xx = { Xxr COMMA Xxi } COMMA c = { cr COMMA ci };
  fecomplex xn;
@clfec  {
        <xsl:value-of select="perturbation" />
        }
  xrn = xn.re;
  xin = xn.im;
}
</xsl:when>
<xsl:when test="perturbation/@t='R'">
@clfe   {
        <xsl:value-of select="perturbation" />
        }
</xsl:when>
</xsl:choose>
);

static const char *perturbation_opencl_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_1 = STR(
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@clfe   {
        <xsl:value-of select="derivative" />
        }
</xsl:when>
<xsl:when test="derivative/@t='C'">
@clfec  {
        <xsl:value-of select="derivative" />
        }
</xsl:when>
</xsl:choose>
@clfec  {
        <xsl:value-of select="perturbation" />
        }
</xsl:when>
<xsl:when test="perturbation/@t='R'">
<xsl:choose>
<xsl:when test="derivative/@t='C'">
@clfec  {
        <xsl:value-of select="derivative" />
        }
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@clfe   {
        <xsl:value-of select="derivative" />
        }
</xsl:when>
</xsl:choose>
@clfe   {
        <xsl:value-of select="perturbation" />
        }
</xsl:when>
</xsl:choose>
);

#if 0
static const char *perturbation_opencl_softfloat_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_0 = STR(
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
@clsfc  {
        <xsl:value-of select="perturbation" />
        }
</xsl:when>
<xsl:when test="perturbation/@t='R'">
@clsf   {
        <xsl:value-of select="perturbation" />
        }
</xsl:when>
</xsl:choose>
);
#endif
</xsl:for-each>

#if 0
<xsl:for-each select="formulas/group/formula">
static const char *perturbation_opencl_softfloat_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_1 = STR(
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@clsf   {
        <xsl:value-of select="derivative" />
        }
</xsl:when>
<xsl:when test="derivative/@t='C'">
@clsfc  {
        <xsl:value-of select="derivative" />
        }
</xsl:when>
</xsl:choose>
@clsfc  {
        <xsl:value-of select="perturbation" />
        }
</xsl:when>
<xsl:when test="perturbation/@t='R'">
<xsl:choose>
<xsl:when test="derivative/@t='C'">
@clsfc  {
        <xsl:value-of select="derivative" />
        }
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@clsf   {
        <xsl:value-of select="derivative" />
        }
</xsl:when>
</xsl:choose>
@clsf   {
        <xsl:value-of select="perturbation" />
        }
</xsl:when>
</xsl:choose>
);
</xsl:for-each>
#endif


<xsl:for-each select="//scaled/..">
static const char *perturbation_opencl_scaled_loop_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" /> = STR(

void perturbation_double_loop
( __global const p_config    *g
, __global const double      *m_refx
, __global const double      *m_refy
, __global const double      *m_refz
,                p_status_d  *l
)
{
}

void perturbation_floatexp_loop
( __global const p_config    *g
, __global const double      *m_refx
, __global const double      *m_refy
, __global const double      *m_refz
, __global const long        *m_refN
, __global const floatexp    *m_refX
, __global const floatexp    *m_refY
, __global const floatexp    *m_refZ
,                p_status_fe *l
)
{
}

void perturbation_scaled_loop
( __global const p_config    *g
, __global const double      *m_refx
, __global const double      *m_refy
, __global const double      *m_refz
, __global const long        *m_refN
, __global const floatexp    *m_refX
, __global const floatexp    *m_refY
, __global const floatexp    *m_refZ
,                p_status_fe *l
)
{
  const floatexp zero = fe_floatexp(0.0, 0);
  const floatexp one = fe_floatexp(1.0, 0);
  const double Ar = g->g_FactorAR;
  const double Ai = g->g_FactorAI;
  const bool no_g = g->g_real == 1.0 &amp;&amp; g->g_imag == 1.0 &amp;&amp; g->norm_p == 2.0;
  const floatexp cr = l->cr;
  const floatexp ci = l->ci;
  floatexp xr = l->xr;
  floatexp xi = l->xi;
  // hybrids
  int count = 0;
  int stanza = 0;
  // conditions
  double test1 = l->test1;
  double test2 = l->test2;
  long antal = l->antal;
  floatexp Xxr = zero;
  floatexp Xxi = zero;

  long k = 0; long n = 0;
  floatexp Xrf; floatexp Xif; floatexp Xzf;
  do
  {
    if (k &lt; g->m_nRSize)
    {
      n = m_refN[k];
      Xrf = m_refX[k];
      Xif = m_refY[k];
      Xzf = m_refZ[k];
      k++;
    }
    else
    {
      n = g->nMaxIter;
    }
  }
  while (n &lt; antal);

  // rescale
  floatexp S = fe_sqrt(fe_add(fe_sqr(xr), fe_sqr(xi)));
  double s = fe_double(S);
  double wr = fe_double(fe_div(xr, S));
  double wi = fe_double(fe_div(xi, S));
  double ur = fe_double(fe_div(cr, S));
  double ui = fe_double(fe_div(ci, S));
  double u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));

  for (; antal &lt; g->nMaxIter; antal++)
  {
    if (antal &lt; n)
    {
      const long antal_min = antal - g->nMinIter;
      const double Xr = m_refx[antal_min];
      const double Xi = m_refy[antal_min];
      const double Xz = m_refz[antal_min];
      const double wr2 = wr * wr;
      (void) wr2;
      const double wi2 = wi * wi;
      (void) wi2;
      const double Xxrd = Xr + wr * s;
      const double Xxid = Xi + wi * s;
      const double Xxr2 = Xxrd * Xxrd;
      const double Xxi2 = Xxid * Xxid;
      test2 = test1;
      test1 = Xxr2 + Xxi2;
      if (test1 &lt; Xz)
      {
        l->bGlitch = true;
        if (! l->bNoGlitchDetection)
        {
          Xxr = fe_floatexp(Xxrd, 0);
          Xxi = fe_floatexp(Xxid, 0);
          break;
        }
      }
      if (! no_g)
      {
        test1 = pnorm(g->g_real, g->g_imag, g->norm_p, Xxrd, Xxid);
      }
      if (test1 &gt; g->m_nBailout2)
      {
        Xxr = fe_floatexp(Xxrd, 0);
        Xxi = fe_floatexp(Xxid, 0);
        break;
      }
      double wrn; double win;
      if (false) { }
<xsl:for-each select="scaled/threshold">
      else if (s &lt;= <xsl:value-of select="@s" /> &amp;&amp; u &lt;= <xsl:value-of select="@u" />)
      {
@cld  {
        <xsl:value-of select="." />
      }
      }
</xsl:for-each>
      else
      {
        // assert(! "scaled/threshold");
        wrn = 0;
        win = 0;
      }
      const double w2 = wrn * wrn + win * win;
      if (w2 &lt; 1.0e100) // FIXME threshold depends on power
      {
        wr = wrn;
        wi = win;
      }
      else
      {
        // rescale
        floatexp xrn = fe_muld(S, wrn);
        floatexp xin = fe_muld(S, win);
        S = fe_sqrt(fe_add(fe_sqr(xrn), fe_sqr(xin)));
        s = fe_double(S);
        wr = fe_double(fe_div(xrn, S));
        wi = fe_double(fe_div(xin, S));
        ur = fe_double(fe_div(cr, S));
        ui = fe_double(fe_div(ci, S));
        u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));
      }
    }

    else
    {
      const floatexp Xr = Xrf;
      const floatexp Xi = Xif;
      const floatexp Xz = Xzf;
      if (k &lt; g->m_nRSize)
      {
        n = m_refN[k];
        Xrf = m_refX[k];
        Xif = m_refY[k];
        Xzf = m_refZ[k];
        k++;
      }
      else
      {
        n = g->nMaxIter;
      }
      const floatexp xr = fe_muld(S, wr);
      const floatexp xi = fe_muld(S, wi);
      Xxr = fe_add(Xr, xr);
      Xxi = fe_add(Xi, xi);
      const floatexp Xxr2 = fe_sqr(Xxr);
      const floatexp Xxi2 = fe_sqr(Xxi);
      const floatexp xr2 = fe_sqr(xr);
      const floatexp xi2 = fe_sqr(xi);
      const floatexp Xr2 = fe_sqr(Xr);
      const floatexp Xi2 = fe_sqr(Xi);
      test2 = test1;
      floatexp ftest1 = fe_add(Xxr2, Xxi2);
      test1 = fe_double(ftest1);
      if (fe_lt(ftest1, Xz))
      {
        l->bGlitch = true;
        if (! l->bNoGlitchDetection)
          break;
      }
      if (! no_g)
      {
        test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(Xxr), fe_double(Xxi));
      }
      if (test1 &gt; g->m_nBailout2)
      {
        break;
      }
      floatexp xrn; floatexp xin;
      {
@clfe {
        <xsl:value-of select="perturbation" />
      }
      }
      // rescale
      S = fe_sqrt(fe_add(fe_sqr(xrn), fe_sqr(xin)));
      s = fe_double(S);
      wr = fe_double(fe_div(xrn, S));
      wi = fe_double(fe_div(xin, S));
      ur = fe_double(fe_div(cr, S));
      ui = fe_double(fe_div(ci, S));
      u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));
    }
  }
  l->antal = antal;
  l->test1 = test1;
  l->test2 = test2;
  l->xr = Xxr;
  l->xi = Xxi;
}

);


static const char *perturbation_opencl_scaled_derivatives_loop_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" /> = STR(

void perturbation_double_loop
( __global const p_config    *g
, __global const double      *m_refx
, __global const double      *m_refy
, __global const double      *m_refz
,                p_status_d  *l
)
{
}

void perturbation_floatexp_loop
( __global const p_config    *g
, __global const double      *m_refx
, __global const double      *m_refy
, __global const double      *m_refz
, __global const long        *m_refN
, __global const floatexp    *m_refX
, __global const floatexp    *m_refY
, __global const floatexp    *m_refZ
,                p_status_fe *l
)
{
}

void perturbation_scaled_loop
( __global const p_config    *g
, __global const double      *m_refx
, __global const double      *m_refy
, __global const double      *m_refz
, __global const long        *m_refN
, __global const floatexp    *m_refX
, __global const floatexp    *m_refY
, __global const floatexp    *m_refZ
,                p_status_fe *l
)
{
  const floatexp zero = fe_floatexp(0.0, 0);
  const floatexp one = fe_floatexp(1.0, 0);
  const double Ar = g->g_FactorAR;
  const double Ai = g->g_FactorAI;
  const bool no_g = g->g_real == 1.0 &amp;&amp; g->g_imag == 1.0 &amp;&amp; g->norm_p == 2.0;
  const floatexp cr = l->cr;
  const floatexp ci = l->ci;
  floatexp xr = l->xr;
  floatexp xi = l->xi;

  double test1 = l->test1;
  double test2 = l->test2;
  long antal = l->antal;
  bool bGlitch = l->bGlitch;
  floatexp XxrF = zero;
  floatexp XxiF = zero;
<xsl:choose>
<xsl:when test="derivative/@t='C'">
  assert(! "perturbation scaled derivative C implemented");
</xsl:when>
<xsl:when test="derivative/@t='R'">
  floatexp drF = l->dxa; floatexp diF = l->dya;
  const floatexp dr0F = l->daa; const floatexp di0F = l->dba;
</xsl:when>
<xsl:when test="derivative/@t='M'">
  floatexp dxaF = l->dxa; floatexp dxbF = l->dxb; floatexp dyaF = l->dya; floatexp dybF = l->dyb;
</xsl:when>
</xsl:choose>

  long K = 0; long N = 0;
  floatexp X = zero; floatexp Y = zero; floatexp Z0 = zero;
  do
  {
    if (K &lt; g->m_nRSize)
    {
      N = m_refN[K];
      X = m_refX[K];
      Y = m_refY[K];
      Z0 = m_refZ[K];
      K++;
    }
    else
    {
      N = g->nMaxIter;
    }
  }
  while (N &lt; antal);

  // rescale
  floatexp S = fe_sqrt(fe_add(fe_sqr(xr), fe_sqr(xi)));
  if (fe_cmp(S, zero) == 0)
  {
    S = fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci)));
  }
  if (fe_cmp(S, zero) == 0)
  {
    S = one;
  }
  double s = fe_double(S);
  double wr = fe_double(fe_div(xr, S));
  double wi = fe_double(fe_div(xi, S));
  double ur = fe_double(fe_div(cr, S));
  double ui = fe_double(fe_div(ci, S));
  double u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));
<xsl:choose>
<xsl:when test="derivative/@t='R'">
  floatexp J0 = fe_sqrt(fe_add(fe_sqr(drF), fe_sqr(diF)));
  if (fe_cmp(J0, one) &lt; 0)
  {
    J0 = one;
  }
  floatexp J = J0;
  double drD = fe_double(fe_div(drF, J));
  double diD = fe_double(fe_div(diF, J));
  double dr0D = fe_double(fe_div(dr0F, J));
  double di0D = fe_double(fe_div(di0F, J));
</xsl:when>
<xsl:when test="derivative/@t='M'">
  floatexp J0 = fe_sqrt(fe_add(fe_add(fe_add(fe_sqr(dxaF), fe_sqr(dxbF)), fe_sqr(dyaF)), fe_sqr(dybF)));
  if (fe_cmp(J0, one) &lt; 0)
  {
    J0 = one;
  }
  floatexp J = J0;
  double dxaD = fe_double(fe_div(dxaF, J));
  double dyaD = fe_double(fe_div(dyaF, J));
  double dxbD = fe_double(fe_div(dxbF, J));
  double dybD = fe_double(fe_div(dybF, J));
  double daaD = fe_double(fe_div(l->daa, J));
  double dbaD = fe_double(fe_div(l->dba, J));
  double dabD = fe_double(fe_div(l->dab, J));
  double dbbD = fe_double(fe_div(l->dbb, J));
</xsl:when>
</xsl:choose>

  for (; antal &lt; g->nMaxIter; antal++)
  {
    bool full_iteration = antal == N;
    if (full_iteration)
    {
      const floatexp Xr = X;
      const floatexp Xi = Y;
      const floatexp Xz = Z0;
      if (K &lt; g->m_nRSize)
      {
        N = m_refN[K];
        X = m_refX[K];
        Y = m_refY[K];
        Z0 = m_refZ[K];
        K++;
      }
      else
      {
        N = g->nMaxIter;
      }
      const floatexp xr = fe_muld(S, wr);
      const floatexp xi = fe_muld(S, wi);
      XxrF = fe_add(Xr, xr);
      XxiF = fe_add(Xi, xi);
      const floatexp Xxr2 = fe_sqr(XxrF);
      const floatexp Xxi2 = fe_sqr(XxiF);
      const floatexp xr2 = fe_sqr(xr);
      const floatexp xi2 = fe_sqr(xi);
      const floatexp Xr2 = fe_sqr(Xr);
      const floatexp Xi2 = fe_sqr(Xi);
      test2 = test1;
      floatexp ftest1 = fe_add(Xxr2, Xxi2);
      test1 = fe_double(ftest1);
      if (fe_cmp(ftest1, Xz) &lt; 0)
      {
        bGlitch = true;
        if (! g->m_bNoGlitchDetection)
          break;
      }
      if (! no_g)
      {
        test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(XxrF), fe_double(XxiF));
      }
      if (test1 &gt; g->m_nBailout2)
      {
        break;
      }
      const floatexp Xxr = XxrF;
      const floatexp Xxi = XxiF;
      floatexp xrn; floatexp xin; floatexp drn; floatexp din; floatexp dxan; floatexp dyan; floatexp dxbn; floatexp dybn;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
      {
        (void) dxan;
        (void) dyan;
        (void) dxbn;
        (void) dybn;
        const floatexp dr = fe_muld(J, drD);
        const floatexp di = fe_muld(J, diD);
        const floatexp dr0 = dr0F;
        const floatexp di0 = di0F;
@clfe     {
          <xsl:value-of select="derivative" />
        }
      }
</xsl:when>
<xsl:when test="derivative/@t='M'">
      {
        (void) drn;
        (void) din;
        const floatexp dxa = fe_muld(J, dxaD);
        const floatexp dxb = fe_muld(J, dxbD);
        const floatexp dya = fe_muld(J, dyaD);
        const floatexp dyb = fe_muld(J, dybD);
        const floatexp daa = l->daa;
        const floatexp dab = l->dab;
        const floatexp dba = l->dba;
        const floatexp dbb = l->dbb;
@clfe     {
          <xsl:value-of select="derivative" />
        }
      }
</xsl:when>
<xsl:when test="derivative/@t='C'">
      {
        assert(! "derivative type C implemented")
      }
</xsl:when>
</xsl:choose>
      {
@clfe   {
        <xsl:value-of select="perturbation" />
      }
      }
      // rescale
      S = fe_sqrt(fe_add(fe_sqr(xrn), fe_sqr(xin)));
      s = fe_double(S);
      wr = fe_double(fe_div(xrn, S));
      wi = fe_double(fe_div(xin, S));
      ur = fe_double(fe_div(cr, S));
      ui = fe_double(fe_div(ci, S));
      u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));
<xsl:choose>
<xsl:when test="derivative/@t='R'">
      J = fe_sqrt(fe_add(fe_sqr(drn), fe_sqr(din)));
      drD = fe_double(fe_div(drn, J));
      diD = fe_double(fe_div(din, J));
      dr0D = fe_double(fe_div(dr0F, J));
      di0D = fe_double(fe_div(di0F, J));
</xsl:when>
<xsl:when test="derivative/@t='M'">
      J = fe_sqrt(fe_add(fe_add(fe_add(fe_sqr(dxan), fe_sqr(dyan)), fe_sqr(dxbn)), fe_sqr(dybn)));
      dxaD = fe_double(fe_div(dxan, J));
      dyaD = fe_double(fe_div(dyan, J));
      dxbD = fe_double(fe_div(dxbn, J));
      dybD = fe_double(fe_div(dybn, J));
      daaD = fe_double(fe_div(l->daa, J));
      dbaD = fe_double(fe_div(l->dba, J));
      dabD = fe_double(fe_div(l->dab, J));
      dbbD = fe_double(fe_div(l->dbb, J));
</xsl:when>
</xsl:choose>
    }
    else
    {
      long antal_min = antal - g->nMinIter;
      const double Xr = m_refx[antal_min];
      const double Xi = m_refy[antal_min];
      const double Xz = m_refz[antal_min];
      const double wr2 = wr * wr;
      (void) wr2;
      const double wi2 = wi * wi;
      (void) wi2;
      const double Xxrd = Xr + wr * s;
      const double Xxid = Xi + wi * s;
      const double Xxr2 = Xxrd * Xxrd;
      const double Xxi2 = Xxid * Xxid;
      test2 = test1;
      test1 = Xxr2 + Xxi2;
      if (test1 &lt; Xz)
      {
        bGlitch = true;
        if (! g->m_bNoGlitchDetection)
        {
          XxrF = fe_floatexp(Xxrd, 0);
          XxiF = fe_floatexp(Xxid, 0);
          break;
        }
      }
      if (! no_g)
      {
        test1 = pnorm(g->g_real, g->g_imag, g->norm_p, Xxrd, Xxid);
      }
      if (test1 &gt; g->m_nBailout2)
      {
        XxrF = fe_floatexp(Xxrd, 0);
        XxiF = fe_floatexp(Xxid, 0);
        break;
      }
      const double Xxr = Xxrd;
      const double Xxi = Xxid;
      double drn; double din; double dxan; double dyan; double dxbn; double dybn;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
      {
        (void) dxan;
        (void) dyan;
        (void) dxbn;
        (void) dybn;
        const double dr = drD;
        const double di = diD;
        const double dr0 = dr0D;
        const double di0 = di0D;
@cld      {
          <xsl:value-of select="derivative" />
        }
      }
</xsl:when>
<xsl:when test="derivative/@t='M'">
      {
        (void) drn;
        (void) din;
        const double dxa = dxaD;
        const double dxb = dxbD;
        const double dya = dyaD;
        const double dyb = dybD;
        const double daa = daaD;
        const double dab = dabD;
        const double dba = dbaD;
        const double dbb = dbbD;
@cld      {
          <xsl:value-of select="derivative" />
        }
      }
</xsl:when>
</xsl:choose>
      double wrn; double win;
      if (false) { }
<xsl:for-each select="scaled/threshold">
      else if (s &lt;= <xsl:value-of select="@s" /> &amp;&amp; u &lt;= <xsl:value-of select="@u" />)
      {
@cld    {
        <xsl:value-of select="." />
      }
      }
</xsl:for-each>
      else
      {
        assert(! "scaled/threshold");
        wrn = 0;
        win = 0;
      }
      const double w2 = wrn * wrn + win * win;
      if (w2 &lt; 1.0e100) // FIXME threshold depends on power
      {
        wr = wrn;
        wi = win;
      }
      else
      {
        // rescale
        floatexp xrn = fe_muld(S, wrn);
        floatexp xin = fe_muld(S, win);
        S = fe_sqrt(fe_add(fe_sqr(xrn), fe_sqr(xin)));
        s = fe_double(S);
        wr = fe_double(fe_div(xrn, S));
        wi = fe_double(fe_div(xin, S));
        ur = fe_double(fe_div(cr, S));
        ui = fe_double(fe_div(ci, S));
        u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));
      }
<xsl:choose>
<xsl:when test="derivative/@t='R'">
      const double d2 = drn * drn + din * din;
      if (d2 &lt; 1.0e100) // FIXME threshold depends on power
      {
        drD = drn;
        diD = din;
      }
      else
      {
        floatexp drF = fe_muld(J, drn);
        floatexp diF = fe_muld(J, din);
        J = fe_sqrt(fe_add(fe_sqr(drF), fe_sqr(diF)));
        drD = fe_double(fe_div(drF, J));
        diD = fe_double(fe_div(diF, J));
        dr0D = fe_double(fe_div(dr0F, J));
        di0D = fe_double(fe_div(di0F, J));
      }
</xsl:when>
<xsl:when test="derivative/@t='M'">
      const double d2 = dxan * dxan + dxbn * dxbn + dyan * dyan + dybn * dybn;
      if (d2 &lt; 1.0e100) // FIXME threshold depends on power
      {
        dxaD = dxan;
        dyaD = dyan;
        dxbD = dxbn;
        dybD = dybn;
      }
      else
      {
        floatexp dxaF = fe_muld(J, dxan);
        floatexp dyaF = fe_muld(J, dyan);
        floatexp dxbF = fe_muld(J, dxbn);
        floatexp dybF = fe_muld(J, dybn);
        J = fe_sqrt(fe_add(fe_add(fe_add(fe_sqr(dxaF), fe_sqr(dxbF)), fe_sqr(dyaF)), fe_sqr(dybF)));
        dxaD = fe_double(fe_div(dxaF, J));
        dxbD = fe_double(fe_div(dxbF, J));
        dyaD = fe_double(fe_div(dyaF, J));
        dybD = fe_double(fe_div(dybF, J));
        daaD = fe_double(fe_div(l->daa, J));
        dbaD = fe_double(fe_div(l->dba, J));
        dabD = fe_double(fe_div(l->dab, J));
        dbbD = fe_double(fe_div(l->dbb, J));
      }
</xsl:when>
</xsl:choose>
    }
  }

  l->antal = antal;
  l->bGlitch = bGlitch;
  l->test1 = test1;
  l->test2 = test2;
  l->xr = XxrF;
  l->xi = XxiF;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
  l->dxa = fe_muld(J,  drD);
  l->dxb = fe_muld(J, -diD);
  l->dya = fe_muld(J,  diD);
  l->dyb = fe_muld(J,  drD);
</xsl:when>
<xsl:when test="derivative/@t='M'">
  l->dxa = fe_muld(J, dxaD);
  l->dxb = fe_muld(J, dxbD);
  l->dya = fe_muld(J, dyaD);
  l->dyb = fe_muld(J, dybD);
</xsl:when>
</xsl:choose>
}

);
</xsl:for-each>

static const std::string perturbation_opencl_error = "#error unsupported fractal type and power\n";

extern std::string perturbation_opencl(int m_nFractalType, int m_nPower, int derivatives, int scaled)
{
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
      switch (m_nPower)
      {
      <xsl:for-each select="formula">
        case <xsl:value-of select="@power" />:
        {
          std::ostringstream o;
          o &lt;&lt; perturbation_opencl_common;

          if (scaled)
          {
<xsl:for-each select="scaled">
            if (derivatives)
            {
              o &lt;&lt; perturbation_opencl_scaled_derivatives_loop_<xsl:value-of select="../../@type" />_<xsl:value-of select="../@power" />;
            }
            else
            {
              o &lt;&lt; perturbation_opencl_scaled_loop_<xsl:value-of select="../../@type" />_<xsl:value-of select="../@power" />;
            }
</xsl:for-each>
          }
          else
          {

          if (derivatives)
          {
<xsl:choose>
<xsl:when test="derivative/@t='R'">
            o &lt;&lt; perturbation_opencl_double_pre_r;
</xsl:when>
<xsl:when test="derivative/@t='C'">
            o &lt;&lt; perturbation_opencl_double_pre_c;
</xsl:when>
<xsl:when test="derivative/@t='M'">
            o &lt;&lt; perturbation_opencl_double_pre_m;
</xsl:when>
</xsl:choose>
            o &lt;&lt; perturbation_opencl_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_1;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
            o &lt;&lt; perturbation_opencl_double_post_r;
</xsl:when>
<xsl:when test="derivative/@t='C'">
            o &lt;&lt; perturbation_opencl_double_post_c;
</xsl:when>
<xsl:when test="derivative/@t='M'">
            o &lt;&lt; perturbation_opencl_double_post_m;
</xsl:when>
</xsl:choose>
          }
          else
          {
            o &lt;&lt; perturbation_opencl_double_pre;
            o &lt;&lt; perturbation_opencl_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_0;
            o &lt;&lt; perturbation_opencl_double_post;
          }

          if (derivatives)
          {
<xsl:choose>
<xsl:when test="derivative/@t='R'">
            o &lt;&lt; perturbation_opencl_floatexp_pre_r;
</xsl:when>
<xsl:when test="derivative/@t='C'">
            o &lt;&lt; perturbation_opencl_floatexp_pre_c;
</xsl:when>
<xsl:when test="derivative/@t='M'">
            o &lt;&lt; perturbation_opencl_floatexp_pre_m;
</xsl:when>
</xsl:choose>
            o &lt;&lt; perturbation_opencl_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_1;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
            o &lt;&lt; perturbation_opencl_floatexp_post_r;
</xsl:when>
<xsl:when test="derivative/@t='C'">
            o &lt;&lt; perturbation_opencl_floatexp_post_c;
</xsl:when>
<xsl:when test="derivative/@t='M'">
            o &lt;&lt; perturbation_opencl_floatexp_post_m;
</xsl:when>
</xsl:choose>
          }
          else
          {
            o &lt;&lt; perturbation_opencl_floatexp_pre;
            o &lt;&lt; perturbation_opencl_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_0;
            o &lt;&lt; perturbation_opencl_floatexp_post;
          }

#if 0
          if (derivatives)
          {
<xsl:choose>
<xsl:when test="derivative/@t='R'">
            o &lt;&lt; perturbation_opencl_softfloat_pre_r;
</xsl:when>
<xsl:when test="derivative/@t='C'">
            o &lt;&lt; perturbation_opencl_softfloat_pre_c;
</xsl:when>
<xsl:when test="derivative/@t='M'">
            o &lt;&lt; perturbation_opencl_softfloat_pre_m;
</xsl:when>
</xsl:choose>
            o &lt;&lt; perturbation_opencl_softfloat_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_1;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
            o &lt;&lt; perturbation_opencl_softfloat_post_r;
</xsl:when>
<xsl:when test="derivative/@t='C'">
            o &lt;&lt; perturbation_opencl_softfloat_post_c;
</xsl:when>
<xsl:when test="derivative/@t='M'">
            o &lt;&lt; perturbation_opencl_softfloat_post_m;
</xsl:when>
</xsl:choose>
          }
          else
          {
            o &lt;&lt; perturbation_opencl_softfloat_pre;
            o &lt;&lt; perturbation_opencl_softfloat_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_0;
            o &lt;&lt; perturbation_opencl_softfloat_post;
          }
#endif

          o &lt;&lt; perturbation_scaled_loop_empty;
          }

          return o.str();
        }
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return perturbation_opencl_error;
}

</xsl:template>
</xsl:stylesheet>
