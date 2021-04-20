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
            o &lt;&lt; perturbation_opencl_scaled_loop_<xsl:value-of select="../../@type" />_<xsl:value-of select="../@power" />;
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
