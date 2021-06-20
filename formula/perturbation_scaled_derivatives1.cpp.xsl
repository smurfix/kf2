<?xml version="1.0" encoding="UTF-8"?>
<!--
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2021 Claude Heiland-Allen

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
-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" />
<xsl:template match="/">

#include "formula.h"
#include "../fraktal_sft/floatexp.h"
#include "../fraktal_sft/complex.h"
#include "../fraktal_sft/reference.h"

<xsl:for-each select="//scaled/..">

template &lt;typename mantissa, typename exponent&gt;
bool perturbation_scaled_derivatives_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;mantissa, exponent&gt; &amp;xr0, tfloatexp&lt;mantissa, exponent&gt; &amp;xi0
  , const tfloatexp&lt;mantissa, exponent&gt; &amp;cr, const tfloatexp&lt;mantissa, exponent&gt; &amp;ci
  , tfloatexp&lt;mantissa, exponent&gt; &amp;Jxa0F, tfloatexp&lt;mantissa, exponent&gt; &amp;Jxb0F, tfloatexp&lt;mantissa, exponent&gt; &amp;Jya0F, tfloatexp&lt;mantissa, exponent&gt; &amp;Jyb0F
  , const tfloatexp&lt;mantissa, exponent&gt; &amp;daaF, const tfloatexp&lt;mantissa, exponent&gt; &amp;dabF, const tfloatexp&lt;mantissa, exponent&gt; &amp;dbaF, const tfloatexp&lt;mantissa, exponent&gt; &amp;dbbF
  )
{
  using std::abs;
  using std::sqrt;
  using std::exp;
  using std::sin;
  using std::cos;
  using std::sinh;
  using std::log;
  using std::log1p;
  using std::atan2;
  using std::floor;
  (void) Jxa0F; // -Wunused-parameter
  (void) Jxb0F; // -Wunused-parameter
  (void) Jya0F; // -Wunused-parameter
  (void) Jyb0F; // -Wunused-parameter
  (void) daaF; // -Wunused-parameter
  (void) dabF; // -Wunused-parameter
  (void) dbaF; // -Wunused-parameter
  (void) dbbF; // -Wunused-parameter
  if (m_nFractalType == <xsl:value-of select="@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
    const mantissa LARGE_MANTISSA = sizeof(mantissa) == 4 ? 1e30 : 1e300;
    const mantissa SMALL_MANTISSA = sizeof(mantissa) == 4 ? 1e-18 : 1e-154;
    (void) SMALL_MANTISSA;
    const mantissa w2threshold = exp(log(LARGE_MANTISSA) / <xsl:value-of select="@power" />);
    const mantissa d2threshold = exp(log(LARGE_MANTISSA) / (<xsl:value-of select="@power" /> - 1));
    const mantissa Ar = g_FactorAR;
    const mantissa Ai = g_FactorAI;
    (void) Ar; // -Wunused-variable
    (void) Ai; // -Wunused-variable
    const bool no_g = g_real == 1 &amp;&amp; g_imag == 1 &amp;&amp; p == 2;
    mantissa test1 = test10;
    mantissa test2 = test20;
    mantissa phase = phase0;
    int64_t antal = antal0;
    bool bGlitch = bGlitch0;
    tfloatexp&lt;mantissa, exponent&gt; XxrF = 0;
    tfloatexp&lt;mantissa, exponent&gt; XxiF = 0;
<xsl:choose>
<xsl:when test="derivative/@t='C'">
    assert(! "perturbation scaled derivative C implemented");
</xsl:when>
<xsl:when test="derivative/@t='R'">
    tfloatexp&lt;mantissa, exponent&gt; drF = Jxa0F, diF = Jya0F;
    const tfloatexp&lt;mantissa, exponent&gt; dr0F = daaF, di0F = dbaF;
</xsl:when>
<xsl:when test="derivative/@t='M'">
    tfloatexp&lt;mantissa, exponent&gt; dxaF = Jxa0F, dxbF = Jxb0F, dyaF = Jya0F, dybF = Jyb0F;
</xsl:when>
</xsl:choose>

    int64_t K = 0, N = 0;
    const int64_t K_size = reference_size_N(m_Reference);
    const int64_t *Nptr = reference_ptr_N(m_Reference);
    const tfloatexp&lt;mantissa, exponent&gt; *Xptr = reference_ptr_X&lt;mantissa, exponent&gt;(m_Reference);
    const tfloatexp&lt;mantissa, exponent&gt; *Yptr = reference_ptr_Y&lt;mantissa, exponent&gt;(m_Reference);
    const tfloatexp&lt;mantissa, exponent&gt; *Zptr = reference_ptr_Z&lt;mantissa, exponent&gt;(m_Reference);
    tfloatexp&lt;mantissa, exponent&gt; X = 0, Y = 0, Z0 = 0;
    do
    {
      if (K &lt; K_size)
      {
        N = Nptr[K];
        X = Xptr[K];
        Y = Yptr[K];
        Z0 = Zptr[K];
        K++;
      }
      else
      {
        N = nMaxIter;
      }
    }
    while (N &lt; antal);

    // rescale
    tfloatexp&lt;mantissa, exponent&gt; S = sqrt(xr0 * xr0 + xi0 * xi0);
    if (S == 0)
    {
      S = sqrt(cr * cr + ci * ci);
    }
    if (S == 0)
    {
      S = 1;
    }
    mantissa s = mantissa(S);
    mantissa wr = mantissa(xr0 / S);
    mantissa wi = mantissa(xi0 / S);
    mantissa ur = mantissa(cr / S);
    mantissa ui = mantissa(ci / S);
    mantissa u = mantissa(sqrt(cr * cr + ci * ci) / S);
<xsl:choose>
<xsl:when test="derivative/@t='R'">
    tfloatexp&lt;mantissa, exponent&gt; J0 = sqrt(drF * drF + diF * diF);
    if (J0 &lt; 1)
    {
      J0 = 1;
    }
    tfloatexp&lt;mantissa, exponent&gt; J = J0;
    mantissa drD = mantissa(drF / J);
    mantissa diD = mantissa(diF / J);
    mantissa dr0D = mantissa(dr0F / J);
    mantissa di0D = mantissa(di0F / J);
</xsl:when>
<xsl:when test="derivative/@t='M'">
    tfloatexp&lt;mantissa, exponent&gt; J0 = sqrt(dxaF * dxaF + dxbF * dxbF + dyaF * dyaF + dybF * dybF);
    if (J0 &lt; 1)
    {
      J0 = 1;
    }
    tfloatexp&lt;mantissa, exponent&gt; J = J0;
    mantissa dxaD = mantissa(dxaF / J);
    mantissa dyaD = mantissa(dyaF / J);
    mantissa dxbD = mantissa(dxbF / J);
    mantissa dybD = mantissa(dybF / J);
    mantissa daaD = mantissa(daaF / J);
    mantissa dbaD = mantissa(dbaF / J);
    mantissa dabD = mantissa(dabF / J);
    mantissa dbbD = mantissa(dbbF / J);
</xsl:when>
</xsl:choose>

    const mantissa *xptr = reference_ptr_x&lt;mantissa&gt;(m_Reference);
    const mantissa *yptr = reference_ptr_y&lt;mantissa&gt;(m_Reference);
    const mantissa *zptr = reference_ptr_z&lt;mantissa&gt;(m_Reference);
    for (; antal &lt; nMaxIter; antal++)
    {
      bool full_iteration = antal == N;
      if (full_iteration)
      {
        using T = tfloatexp&lt;mantissa, exponent&gt;;
        T dummyT;
        (void) dummyT;
        using V = tfloatexp&lt;mantissa, exponent&gt;;
        V dummyV;
        (void) dummyV;
        const tfloatexp&lt;mantissa, exponent&gt; Xr = X;
        const tfloatexp&lt;mantissa, exponent&gt; Xi = Y;
        const tfloatexp&lt;mantissa, exponent&gt; Xz = Z0;
        if (K &lt; K_size)
        {
          N = Nptr[K];
          X = Xptr[K];
          Y = Yptr[K];
          Z0 = Zptr[K];
          ++K;
        }
        else
        {
          N = nMaxIter;
        }
        const tfloatexp&lt;mantissa, exponent&gt; xr = S * wr;
        const tfloatexp&lt;mantissa, exponent&gt; xi = S * wi;
        XxrF = Xr + xr;
        XxiF = Xi + xi;
        const tfloatexp&lt;mantissa, exponent&gt; Xxr2 = XxrF * XxrF;
        const tfloatexp&lt;mantissa, exponent&gt; Xxi2 = XxiF * XxiF;
        const tfloatexp&lt;mantissa, exponent&gt; xr2 = xr * xr;
        const tfloatexp&lt;mantissa, exponent&gt; xi2 = xi * xi;
        const tfloatexp&lt;mantissa, exponent&gt; Xr2 = Xr * Xr;
        const tfloatexp&lt;mantissa, exponent&gt; Xi2 = Xi * Xi;
        test2 = test1;
        tfloatexp&lt;mantissa, exponent&gt; ftest1 = Xxr2 + Xxi2;
        test1 = mantissa(ftest1);
        if (ftest1 &lt; Xz)
        {
          bGlitch = true;
          if (! m_bNoGlitchDetection)
            break;
        }
        if (! no_g)
        {
          test1 = mantissa(pnorm(g_real, g_imag, p, XxrF, XxiF));
        }
        if (test1 &gt; m_nBailout2)
        {
          phase = atan2(mantissa(XxiF), mantissa(XxrF)) / M_PI / 2;
          phase -= floor(phase);
          break;
        }
        const tfloatexp&lt;mantissa, exponent&gt; Xxr = XxrF;
        const tfloatexp&lt;mantissa, exponent&gt; Xxi = XxiF;
        tfloatexp&lt;mantissa, exponent&gt; xrn, xin, drn, din, dxan, dyan, dxbn, dybn;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
        {
          (void) dxan;
          (void) dyan;
          (void) dxbn;
          (void) dybn;
          const tfloatexp&lt;mantissa, exponent&gt; dr = drD * J;
          const tfloatexp&lt;mantissa, exponent&gt; di = diD * J;
          const tfloatexp&lt;mantissa, exponent&gt; dr0 = dr0F;
          const tfloatexp&lt;mantissa, exponent&gt; di0 = di0F;
@d        {
            <xsl:value-of select="derivative" />
          }
        }
</xsl:when>
<xsl:when test="derivative/@t='M'">
        {
          (void) drn;
          (void) din;
          const tfloatexp&lt;mantissa, exponent&gt; dxa = dxaD * J;
          const tfloatexp&lt;mantissa, exponent&gt; dxb = dxbD * J;
          const tfloatexp&lt;mantissa, exponent&gt; dya = dyaD * J;
          const tfloatexp&lt;mantissa, exponent&gt; dyb = dybD * J;
          const tfloatexp&lt;mantissa, exponent&gt; daa = daaF;
          const tfloatexp&lt;mantissa, exponent&gt; dab = dabF;
          const tfloatexp&lt;mantissa, exponent&gt; dba = dbaF;
          const tfloatexp&lt;mantissa, exponent&gt; dbb = dbbF;
@d        {
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
@d      {
          <xsl:value-of select="perturbation" />
        }
        }
        // rescale
        S = sqrt(xrn * xrn + xin * xin);
        s = mantissa(S);
        wr = mantissa(xrn / S);
        wi = mantissa(xin / S);
        ur = mantissa(cr / S);
        ui = mantissa(ci / S);
        u = mantissa(sqrt(cr * cr + ci * ci) / S);
<xsl:choose>
<xsl:when test="derivative/@t='R'">
        J = sqrt(drn * drn + din * din);
        drD = mantissa(drn / J);
        diD = mantissa(din / J);
        dr0D = mantissa(dr0F / J);
        di0D = mantissa(di0F / J);
</xsl:when>
<xsl:when test="derivative/@t='M'">
        J = sqrt(dxan * dxan + dyan * dyan + dxbn * dxbn + dybn * dybn);
        dxaD = mantissa(dxan / J);
        dyaD = mantissa(dyan / J);
        dxbD = mantissa(dxbn / J);
        dybD = mantissa(dybn / J);
        daaD = mantissa(daaF / J);
        dbaD = mantissa(dbaF / J);
        dabD = mantissa(dabF / J);
        dbbD = mantissa(dbbF / J);
</xsl:when>
</xsl:choose>
      }
      else
      {
        using T = mantissa;
        T dummyT;
        (void) dummyT;
        using V = mantissa;
        V dummyV;
        (void) dummyV;
        const mantissa Xr = xptr[antal];
        const mantissa Xi = yptr[antal];
        const mantissa Xz = zptr[antal];
        const mantissa wr2 = wr * wr;
        (void) wr2;
        const mantissa wi2 = wi * wi;
        (void) wi2;
        const mantissa Xxrd = Xr + wr * s;
        const mantissa Xxid = Xi + wi * s;
        const mantissa Xxr2 = Xxrd * Xxrd;
        const mantissa Xxi2 = Xxid * Xxid;
        test2 = test1;
        test1 = Xxr2 + Xxi2;
        if (test1 &lt; Xz)
        {
          bGlitch = true;
          if (! m_bNoGlitchDetection)
          {
            XxrF = Xxrd;
            XxiF = Xxid;
            break;
          }
        }
        if (! no_g)
        {
          test1 = double(pnorm(g_real, g_imag, p, Xxrd, Xxid));
        }
        if (test1 &gt; m_nBailout2)
        {
          XxrF = Xxrd;
          XxiF = Xxid;
          phase = atan2(double(Xxid), double(Xxrd)) / M_PI / 2;
          phase -= floor(phase);
          break;
        }
        const mantissa Xxr = Xxrd;
        const mantissa Xxi = Xxid;
        mantissa drn, din, dxan, dyan, dxbn, dybn;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
        {
          (void) dxan;
          (void) dyan;
          (void) dxbn;
          (void) dybn;
          const mantissa dr = drD;
          const mantissa di = diD;
          const mantissa dr0 = dr0D;
          const mantissa di0 = di0D;
@d        {
            <xsl:value-of select="derivative" />
          }
        }
</xsl:when>
<xsl:when test="derivative/@t='M'">
        {
          (void) drn;
          (void) din;
          const mantissa dxa = dxaD;
          const mantissa dxb = dxbD;
          const mantissa dya = dyaD;
          const mantissa dyb = dybD;
          const mantissa daa = daaD;
          const mantissa dab = dabD;
          const mantissa dba = dbaD;
          const mantissa dbb = dbbD;
@d        {
            <xsl:value-of select="derivative" />
          }
        }
</xsl:when>
</xsl:choose>
        mantissa wrn, win;
        if (false) { }
<xsl:for-each select="scaled/threshold">
        else if (s &lt;= <xsl:value-of select="@s" /> &amp;&amp; u &lt;= <xsl:value-of select="@u" />)
        {
@d      {
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
        const mantissa w2 = wrn * wrn + win * win;
        if (w2 &lt; w2threshold) // FIXME threshold depends on power
        {
          wr = wrn;
          wi = win;
        }
        else
        {
          // rescale
          tfloatexp&lt;mantissa, exponent&gt; xrn = S * wrn;
          tfloatexp&lt;mantissa, exponent&gt; xin = S * win;
          S = sqrt(xrn * xrn + xin * xin);
          s = mantissa(S);
          wr = mantissa(xrn / S);
          wi = mantissa(xin / S);
          ur = mantissa(cr / S);
          ui = mantissa(ci / S);
          u = mantissa(sqrt(cr * cr + ci * ci) / S);
        }
<xsl:choose>
<xsl:when test="derivative/@t='R'">
        const mantissa d2 = drn * drn + din * din;
        if (d2 &lt; d2threshold) // FIXME threshold depends on power
        {
          drD = drn;
          diD = din;
        }
        else
        {
          tfloatexp&lt;mantissa, exponent&gt; drF = J * drn;
          tfloatexp&lt;mantissa, exponent&gt; diF = J * din;
          J = sqrt(drF * drF + diF * diF);
          drD = mantissa(drF / J);
          diD = mantissa(diF / J);
          dr0D = mantissa(dr0F / J);
          di0D = mantissa(di0F / J);
        }
</xsl:when>
<xsl:when test="derivative/@t='M'">
        const mantissa d2 = dxan * dxan + dxbn * dxbn + dyan * dyan + dybn * dybn;
        if (d2 &lt; d2threshold) // FIXME threshold depends on power
        {
          dxaD = dxan;
          dyaD = dyan;
          dxbD = dxbn;
          dybD = dybn;
        }
        else
        {
          tfloatexp&lt;mantissa, exponent&gt; dxaF = J * dxan;
          tfloatexp&lt;mantissa, exponent&gt; dyaF = J * dyan;
          tfloatexp&lt;mantissa, exponent&gt; dxbF = J * dxbn;
          tfloatexp&lt;mantissa, exponent&gt; dybF = J * dybn;
          J = sqrt(dxaF * dxaF + dyaF * dyaF + dxbF * dxbF + dybF * dybF);
          dxaD = mantissa(dxaF / J);
          dxbD = mantissa(dxbF / J);
          dyaD = mantissa(dyaF / J);
          dybD = mantissa(dybF / J);
          daaD = mantissa(daaF / J);
          dbaD = mantissa(dbaF / J);
          dabD = mantissa(dabF / J);
          dbbD = mantissa(dbbF / J);
        }
</xsl:when>
</xsl:choose>
      }
    }


    antal0 = antal;
    bGlitch0 = bGlitch;
    test10 = test1;
    test20 = test2;
    phase0 = phase;
    xr0 = XxrF;
    xi0 = XxiF;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
    Jxa0F =  drD * J;
    Jxb0F = -diD * J;
    Jya0F =  diD * J;
    Jyb0F =  drD * J;
</xsl:when>
<xsl:when test="derivative/@t='M'">
    Jxa0F = dxaD * J;
    Jxb0F = dxbD * J;
    Jya0F = dyaD * J;
    Jyb0F = dybD * J;
</xsl:when>
</xsl:choose>
    return true;
  }
  return false;
}

template bool perturbation_scaled_derivatives_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;float, int32_t&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;float, int32_t&gt; &amp;xr0, tfloatexp&lt;float, int32_t&gt; &amp;xi0
  , const tfloatexp&lt;float, int32_t&gt; &amp;cr, const tfloatexp&lt;float, int32_t&gt; &amp;ci
  , tfloatexp&lt;float, int32_t&gt; &amp;Jxa0F, tfloatexp&lt;float, int32_t&gt; &amp;Jxb0F, tfloatexp&lt;float, int32_t&gt; &amp;Jya0F, tfloatexp&lt;float, int32_t&gt; &amp;Jyb0F
  , const tfloatexp&lt;float, int32_t&gt; &amp;daaF, const tfloatexp&lt;float, int32_t&gt; &amp;dabF, const tfloatexp&lt;float, int32_t&gt; &amp;dbaF, const tfloatexp&lt;float, int32_t&gt; &amp;dbbF
  );
template bool perturbation_scaled_derivatives_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;double, int64_t&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;double, int64_t&gt; &amp;xr0, tfloatexp&lt;double, int64_t&gt; &amp;xi0
  , const tfloatexp&lt;double, int64_t&gt; &amp;cr, const tfloatexp&lt;double, int64_t&gt; &amp;ci
  , tfloatexp&lt;double, int64_t&gt; &amp;Jxa0F, tfloatexp&lt;double, int64_t&gt; &amp;Jxb0F, tfloatexp&lt;double, int64_t&gt; &amp;Jya0F, tfloatexp&lt;double, int64_t&gt; &amp;Jyb0F
  , const tfloatexp&lt;double, int64_t&gt; &amp;daaF, const tfloatexp&lt;double, int64_t&gt; &amp;dabF, const tfloatexp&lt;double, int64_t&gt; &amp;dbaF, const tfloatexp&lt;double, int64_t&gt; &amp;dbbF
  );

</xsl:for-each>

</xsl:template>
</xsl:stylesheet>
