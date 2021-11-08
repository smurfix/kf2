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
#include "../fraktal_sft/reference.h"

<xsl:for-each select="//scaled/..">
template &lt;typename mantissa, typename exponent&gt;
bool perturbation_scaled_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;mantissa, exponent&gt; &amp;xr00, tfloatexp&lt;mantissa, exponent&gt; &amp;xi00
  , const tfloatexp&lt;mantissa, exponent&gt; &amp;cr, const tfloatexp&lt;mantissa, exponent&gt; &amp;ci
  , const bool singleref
  );
</xsl:for-each>

template &lt;typename mantissa, typename exponent&gt;
bool perturbation_scaled
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;mantissa, exponent&gt; &amp;xr00, tfloatexp&lt;mantissa, exponent&gt; &amp;xi00
  , const tfloatexp&lt;mantissa, exponent&gt; &amp;cr0, const tfloatexp&lt;mantissa, exponent&gt; &amp;ci0
  , const bool singleref
  )
{
<xsl:for-each select="//scaled/..">
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
    return perturbation_scaled_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />
      ( m_nFractalType, m_nPower
      , m_Reference
      , antal0, test10, test20, phase0, bGlitch0
      , m_nBailout2, nMaxIter
      , m_bNoGlitchDetection, g_real, g_imag, p
      , g_FactorAR, g_FactorAI
      , xr00, xi00
      , cr0, ci0
      , singleref
      );
</xsl:for-each>
  return false;
}

template bool perturbation_scaled&lt;float, int32_t&gt;
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
template bool perturbation_scaled&lt;double, int64_t&gt;
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

</xsl:template>
</xsl:stylesheet>
