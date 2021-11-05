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

<xsl:for-each select="formulas/group[not(@convergent='1')]/formula">
template &lt;typename T&gt;
bool perturbation_simple_derivatives_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , T &amp;xr0, T &amp;xi0
  , const T &amp;cr, const T &amp;ci
  , T &amp;Jxa0, T &amp;Jxb0, T &amp;Jya0, T &amp;Jyb0
  , const T &amp;e, const T &amp;h
  , const T &amp;daa, const T &amp;dab, const T &amp;dba, const T &amp;dbb
  , const bool noDerivativeGlitch
  , const bool singleref
  );
</xsl:for-each>

template &lt;typename T&gt;
bool perturbation_simple_derivatives
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , T &amp;xr, T &amp;xi
  , const T &amp;cr, const T &amp;ci
  , T &amp;Jxa, T &amp;Jxb, T &amp;Jya, T &amp;Jyb
  , const T &amp;e, const T &amp;h
  , const T &amp;daa, const T &amp;dab, const T &amp;dba, const T &amp;dbb
  , const bool noDerivativeGlitch
  , const bool singleref
  )
{
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group[not(@convergent='1')]">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
      switch (m_nPower)
      {
      <xsl:for-each select="formula">
        case <xsl:value-of select="@power" />:
          return perturbation_simple_derivatives_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />
            ( m_nFractalType, m_nPower
            , m_Reference
            , antal, test1, test2, phase, bGlitch
            , m_nBailout2, nMaxIter
            , m_bNoGlitchDetection, g_real, g_imag, p
            , g_FactorAR, g_FactorAI
            , xr, xi
            , cr, ci
            , Jxa, Jxb, Jya, Jyb
            , e, h
            , daa, dab, dba, dbb
            , noDerivativeGlitch
            , singleref
            );
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

template bool perturbation_simple_derivatives&lt;float&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , float &amp;xr0, float &amp;xi0
  , const float &amp;cr, const float &amp;ci
  , float &amp;Jxa0, float &amp;Jxb0, float &amp;Jya0, float &amp;Jyb0
  , const float &amp;e, const float &amp;h
  , const float &amp;daa, const float &amp;dab, const float &amp;dba, const float &amp;dbb
  , const bool noDerivativeGlitch
  , const bool singleref
  );
template bool perturbation_simple_derivatives&lt;double&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double &amp;xr0, double &amp;xi0
  , const double &amp;cr, const double &amp;ci
  , double &amp;Jxa0, double &amp;Jxb0, double &amp;Jya0, double &amp;Jyb0
  , const double &amp;e, const double &amp;h
  , const double &amp;daa, const double &amp;dab, const double &amp;dba, const double &amp;dbb
  , const bool noDerivativeGlitch
  , const bool singleref
  );
template bool perturbation_simple_derivatives&lt;long double&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , long double &amp;xr0, long double &amp;xi0
  , const long double &amp;cr, const long double &amp;ci
  , long double &amp;Jxa0, long double &amp;Jxb0, long double &amp;Jya0, long double &amp;Jyb0
  , const long double &amp;e, const long double &amp;h
  , const long double &amp;daa, const long double &amp;dab, const long double &amp;dba, const long double &amp;dbb
  , const bool noDerivativeGlitch
  , const bool singleref
  );
template bool perturbation_simple_derivatives&lt;tfloatexp&lt;float,int32_t&gt;&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;float,int32_t&gt; &amp;xr0, tfloatexp&lt;float,int32_t&gt; &amp;xi0
  , const tfloatexp&lt;float,int32_t&gt; &amp;cr, const tfloatexp&lt;float,int32_t&gt; &amp;ci
  , tfloatexp&lt;float,int32_t&gt; &amp;Jxa0, tfloatexp&lt;float,int32_t&gt; &amp;Jxb0, tfloatexp&lt;float,int32_t&gt; &amp;Jya0, tfloatexp&lt;float,int32_t&gt; &amp;Jyb0
  , const tfloatexp&lt;float,int32_t&gt; &amp;e, const tfloatexp&lt;float,int32_t&gt; &amp;h
  , const tfloatexp&lt;float,int32_t&gt; &amp;daa, const tfloatexp&lt;float,int32_t&gt; &amp;dab, const tfloatexp&lt;float,int32_t&gt; &amp;dba, const tfloatexp&lt;float,int32_t&gt; &amp;dbb
  , const bool noDerivativeGlitch
  , const bool singleref
  );
template bool perturbation_simple_derivatives&lt;tfloatexp&lt;double,int64_t&gt;&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;double,int64_t&gt; &amp;xr0, tfloatexp&lt;double,int64_t&gt; &amp;xi0
  , const tfloatexp&lt;double,int64_t&gt; &amp;cr, const tfloatexp&lt;double,int64_t&gt; &amp;ci
  , tfloatexp&lt;double,int64_t&gt; &amp;Jxa0, tfloatexp&lt;double,int64_t&gt; &amp;Jxb0, tfloatexp&lt;double,int64_t&gt; &amp;Jya0, tfloatexp&lt;double,int64_t&gt; &amp;Jyb0
  , const tfloatexp&lt;double,int64_t&gt; &amp;e, const tfloatexp&lt;double,int64_t&gt; &amp;h
  , const tfloatexp&lt;double,int64_t&gt; &amp;daa, const tfloatexp&lt;double,int64_t&gt; &amp;dab, const tfloatexp&lt;double,int64_t&gt; &amp;dba, const tfloatexp&lt;double,int64_t&gt; &amp;dbb
  , const bool noDerivativeGlitch
  , const bool singleref
  );

</xsl:template>
</xsl:stylesheet>
