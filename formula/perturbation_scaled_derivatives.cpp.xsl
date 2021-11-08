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
bool perturbation_scaled_derivatives_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />
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
  , const bool singleref
  );
</xsl:for-each>

template &lt;typename mantissa, typename exponent&gt;
bool perturbation_scaled_derivatives
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
  , const bool singleref
  )
{
<xsl:for-each select="//scaled/..">
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
    return perturbation_scaled_derivatives_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />
      ( m_nFractalType, m_nPower
      , m_Reference
      , antal0, test10, test20, phase0, bGlitch0
      , m_nBailout2, nMaxIter
      , m_bNoGlitchDetection, g_real, g_imag, p
      , g_FactorAR, g_FactorAI
      , xr0, xi0
      , cr, ci
      , Jxa0F, Jxb0F, Jya0F, Jyb0F
      , daaF, dabF, dbaF, dbbF
      , singleref
      );
</xsl:for-each>
  return false;
}

template bool perturbation_scaled_derivatives&lt;float, int32_t&gt;
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
  , const bool singleref
  );
template bool perturbation_scaled_derivatives&lt;double, int64_t&gt;
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
  , const bool singleref
  );

</xsl:template>
</xsl:stylesheet>
