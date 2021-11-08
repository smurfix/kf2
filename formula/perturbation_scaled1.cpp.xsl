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
bool perturbation_scaled_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;mantissa, exponent&gt; &amp;xr00, tfloatexp&lt;mantissa, exponent&gt; &amp;xi00
  , const tfloatexp&lt;mantissa, exponent&gt; &amp;cr, const tfloatexp&lt;mantissa, exponent&gt; &amp;ci
  , const bool singleref
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
  if (m_nFractalType == <xsl:value-of select="@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
    const mantissa LARGE_MANTISSA = sizeof(mantissa) == 4 ? 1e30 : 1e300;
    const mantissa SMALL_MANTISSA = sizeof(mantissa) == 4 ? 1e-18 : 1e-154;
    (void) SMALL_MANTISSA;
    const mantissa w2threshold = exp(log(LARGE_MANTISSA) / <xsl:value-of select="@power" />);
    const mantissa Ar = g_FactorAR;
    const mantissa Ai = g_FactorAI;
    const complex&lt;mantissa&gt; A = { Ar, Ai };
    const complex&lt;tfloatexp&lt;mantissa, exponent&gt;&gt; c = { cr, ci };
    (void) Ar; // -Wunused-variable
    (void) Ai; // -Wunused-variable
    (void) A; // -Wunused-variable
    (void) c; // -Wunused-variable
    const bool no_g = g_real == 1 &amp;&amp; g_imag == 1 &amp;&amp; p == 2;
    double test1 = test10;
    double test2 = test20;
    double phase = phase0;
    int64_t antal = antal0;
    int64_t rantal = antal;
    bool bGlitch = bGlitch0;
    tfloatexp&lt;mantissa, exponent&gt; Xxr = 0;
    tfloatexp&lt;mantissa, exponent&gt; Xxi = 0;

    const int64_t size_x = reference_size_x(m_Reference);
    const int64_t size_N = reference_size_N(m_Reference);
    const int64_t *Nptr = reference_ptr_N(m_Reference);
    const tfloatexp&lt;mantissa, exponent&gt; *Xptr = reference_ptr_X&lt;mantissa, exponent&gt;(m_Reference);
    const tfloatexp&lt;mantissa, exponent&gt; *Yptr = reference_ptr_Y&lt;mantissa, exponent&gt;(m_Reference);
    const tfloatexp&lt;mantissa, exponent&gt; *Zptr = reference_ptr_Z&lt;mantissa, exponent&gt;(m_Reference);

    int64_t k = 0, n = 0;
    tfloatexp&lt;mantissa, exponent&gt; X = 0, Y = 0, Z = 0;
    do
    {
      if (k &lt; size_N)
      {
        n = Nptr[k];
        X = Xptr[k];
        Y = Yptr[k];
        Z = Zptr[k];
        ++k;
      }
      else
      {
        n = nMaxIter;
      }
    }
    while (n &lt; rantal);

    // rescale
    tfloatexp&lt;mantissa, exponent&gt; S = sqrt(xr00 * xr00 + xi00 * xi00);
    mantissa s = mantissa(S);
    mantissa wr = mantissa(xr00 / S);
    mantissa wi = mantissa(xi00 / S);
    mantissa ur = mantissa(cr / S);
    mantissa ui = mantissa(ci / S);
    mantissa u = mantissa(sqrt(cr * cr + ci * ci) / S);

    const mantissa *xptr = reference_ptr_x&lt;mantissa&gt;(m_Reference);
    const mantissa *yptr = reference_ptr_y&lt;mantissa&gt;(m_Reference);
    const mantissa *zptr = reference_ptr_z&lt;mantissa&gt;(m_Reference);
    for (; antal &lt; nMaxIter &amp;&amp; rantal &lt; size_x; antal++)
    {
      bool full_iteration = antal == n;
      if (full_iteration)
      {
        using T = tfloatexp&lt;mantissa, exponent&gt;;
        T dummyT;
        (void) dummyT;
        using V = tfloatexp&lt;mantissa, exponent&gt;;
        V dummyV;
        (void) dummyV;
        tfloatexp&lt;mantissa, exponent&gt; Xr = X;
        tfloatexp&lt;mantissa, exponent&gt; Xi = Y;
        tfloatexp&lt;mantissa, exponent&gt; Xz = Z;
        rantal++;
        if (k &lt; size_N)
        {
          n = Nptr[k];
          X = Xptr[k];
          Y = Yptr[k];
          Z = Zptr[k];
          ++k;
        }
        else
        {
          if (singleref)
          {
            k = 0;
            n = Nptr[k];
            X = Xptr[k];
            Y = Yptr[k];
            Z = Zptr[k];
            ++k;
          }
          else
          {
            n = nMaxIter;
          }
        }
        tfloatexp&lt;mantissa, exponent&gt; xr = S * wr;
        tfloatexp&lt;mantissa, exponent&gt; xi = S * wi;
        Xxr = Xr + xr;
        Xxi = Xi + xi;
        tfloatexp&lt;mantissa, exponent&gt; Xxr2 = Xxr * Xxr;
        tfloatexp&lt;mantissa, exponent&gt; Xxi2 = Xxi * Xxi;
        tfloatexp&lt;mantissa, exponent&gt; xr2 = xr * xr;
        tfloatexp&lt;mantissa, exponent&gt; xi2 = xi * xi;
        tfloatexp&lt;mantissa, exponent&gt; Xr2 = Xr * Xr;
        tfloatexp&lt;mantissa, exponent&gt; Xi2 = Xi * Xi;
        test2 = test1;
        tfloatexp&lt;mantissa, exponent&gt; ftest1 = Xxr2 + Xxi2;
        test1 = double(ftest1);
        if (singleref)
        {
          if (! no_g)
          {
            test1 = pnorm(g_real, g_imag, p, double(Xxr), double(Xxi));
          }
          if (test1 > m_nBailout2)
          {
            phase = atan2(double(Xxi), double(Xxr)) / M_PI / 2;
            phase -= floor(phase);
            break;
          }
          if (ftest1 &lt; xr * xr + xi * xi || rantal == size_x)
          {
            xr = Xxr;
            xi = Xxi;
            rantal = 0;
            Xr = 0;
            Xi = 0;
            Xz = 0;
            Xxr = Xr + xr;
            Xxi = Xi + xi;
            xr2 = xr * xr;
            xi2 = xi * xi;
            Xr2 = Xr * Xr;
            Xi2 = Xi * Xi;
            Xxr = Xr + xr;
            Xxi = Xi + xi;
            Xxr2 = Xxr * Xxr;
            Xxi2 = Xxi * Xxi;
            ftest1 = Xxr2 + Xxi2;
            test1 = double(ftest1);
            if (! no_g)
            {
              test1 = pnorm(g_real, g_imag, p, double(Xxr), double(Xxi));
            }
          }
        }
        else
        {
          if (ftest1 &lt; Xz)
          {
            bGlitch = true;
            if (! m_bNoGlitchDetection)
              break;
          }
          if (! no_g)
          {
            test1 = double(pnorm(g_real, g_imag, p, Xxr, Xxi));
          }
          if (test1 &gt; m_nBailout2)
          {
            phase = atan2(double(Xxi), double(Xxr)) / M_PI / 2;
            phase -= floor(phase);
            break;
          }
        }
        tfloatexp&lt;mantissa, exponent&gt; xrn, xin;
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
      }
      else
      {
        using T = mantissa;
        T dummyT;
        (void) dummyT;
        using V = mantissa;
        V dummyV;
        (void) dummyV;
        mantissa Xr = xptr[rantal];
        mantissa Xi = yptr[rantal];
        mantissa Xz = zptr[rantal];
        rantal++;
        mantissa wr2 = wr * wr;
        (void) wr2;
        mantissa wi2 = wi * wi;
        (void) wi2;
        mantissa Xxrd = Xr + wr * s;
        mantissa Xxid = Xi + wi * s;
        mantissa Xxr2 = Xxrd * Xxrd;
        mantissa Xxi2 = Xxid * Xxid;
        test2 = test1;
        test1 = Xxr2 + Xxi2;
        if (singleref)
        {
          if (! no_g)
          {
            test1 = pnorm(g_real, g_imag, p, double(Xxrd), double(Xxid));
          }
          if (test1 > m_nBailout2)
          {
            Xxr = Xxrd;
            Xxi = Xxid;
            phase = atan2(double(Xxid), double(Xxrd)) / M_PI / 2;
            phase -= floor(phase);
            break;
          }
          if (test1 &lt;= s * s * (wr2 + wi2) || rantal == size_x)
          {
            const tfloatexp&lt;mantissa, exponent> xr = Xxrd;
            const tfloatexp&lt;mantissa, exponent> xi = Xxid;
            rantal = 0;
            Xr = 0;
            Xi = 0;
            Xz = 0;
            Xxr = Xr + xr;
            Xxi = Xi + xi;
            Xxrd = mantissa(Xxr);
            Xxid = mantissa(Xxi);
            Xxr2 = Xxrd * Xxrd;
            Xxi2 = Xxid * Xxid;
            test1 = Xxr2 + Xxi2;
            if (! no_g)
            {
              test1 = pnorm(g_real, g_imag, p, double(Xxrd), double(Xxid));
            }
            // rescale
            S = sqrt(xr * xr + xi * xi);
            s = mantissa(S);
            wr = mantissa(xr / S);
            wi = mantissa(xi / S);
            ur = mantissa(cr / S);
            ui = mantissa(ci / S);
            u = mantissa(sqrt(cr * cr + ci * ci) / S);
            wr2 = wr * wr;
            wi2 = wi * wi;
          }
        }
        else
        {
          if (test1 &lt; Xz)
          {
            bGlitch = true;
            if (! m_bNoGlitchDetection)
            {
              Xxr = Xxrd;
              Xxi = Xxid;
              break;
            }
          }
          if (! no_g)
          {
            test1 = double(pnorm(g_real, g_imag, p, Xxrd, Xxid));
          }
          if (test1 &gt; m_nBailout2)
          {
            Xxr = Xxrd;
            Xxi = Xxid;
            phase = atan2(double(Xxid), double(Xxrd)) / M_PI / 2;
            phase -= floor(phase);
            break;
          }
        }
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
        if (w2 &lt; w2threshold)
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
      }

    }

    antal0 = antal;
    bGlitch0 = bGlitch;
    test10 = test1;
    test20 = test2;
    phase0 = phase;
    xr00 = Xxr;
    xi00 = Xxi;
    return true;
  }
  return false;
}

template bool perturbation_scaled_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;float, int32_t&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;float, int32_t&gt; &amp;xr00, tfloatexp&lt;float, int32_t&gt; &amp;xi00
  , const tfloatexp&lt;float, int32_t&gt; &amp;cr0, const tfloatexp&lt;float, int32_t&gt; &amp;ci0
  , const bool singleref
  );
template bool perturbation_scaled_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;double, int64_t&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;double, int64_t&gt; &amp;xr00, tfloatexp&lt;double, int64_t&gt; &amp;xi00
  , const tfloatexp&lt;double, int64_t&gt; &amp;cr0, const tfloatexp&lt;double, int64_t&gt; &amp;ci0
  , const bool singleref
  );

</xsl:for-each>

</xsl:template>
</xsl:stylesheet>
