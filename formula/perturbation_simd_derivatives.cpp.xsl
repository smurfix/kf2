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
#include "simd1.h"
#include "simd2.h"
#include "simd3.h"
#include "simd4.h"

<xsl:for-each select="formulas/group[not(@convergent='1')]/formula">
template &lt;typename double1, typename intN, typename doubleN&gt;
bool perturbation_SIMD_derivatives_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , intN &amp;antal0, doubleN &amp;test10, doubleN &amp;test20, doubleN &amp;phase0, intN &amp;bGlitch0
  , const double1 &amp;m_nBailout2, const int64_t nMaxIter
  , const intN &amp;m_bNoGlitchDetection, const double1 &amp;g_real, const double1 &amp;g_imag, const double1 &amp;p
  , const double1 &amp;g_FactorAR, const double1 &amp;g_FactorAI
  , doubleN &amp;xr00, doubleN &amp;xi00
  , const doubleN &amp;cr0, const doubleN &amp;ci0
  , doubleN &amp;Jxa0, doubleN &amp;Jxb0, doubleN &amp;Jya0, doubleN &amp;Jyb0
  , const double1 &amp;e, const double1 &amp;h
  , const doubleN &amp;daa0, const doubleN &amp;dab0, const doubleN &amp;dba0, const doubleN &amp;dbb0
  , const int64_t chunksize
  , const bool noDerivativeGlitch
  );
</xsl:for-each>

template &lt;typename double1, typename intN, typename doubleN&gt;
bool perturbation_SIMD_derivatives
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , intN &amp;antal0, doubleN &amp;test10, doubleN &amp;test20, doubleN &amp;phase0, intN &amp;bGlitch0
  , const double1 &amp;m_nBailout2, const int64_t nMaxIter
  , const intN &amp;m_bNoGlitchDetection, const double1 &amp;g_real, const double1 &amp;g_imag, const double1 &amp;p
  , const double1 &amp;g_FactorAR, const double1 &amp;g_FactorAI
  , doubleN &amp;xr0, doubleN &amp;xi0
  , const doubleN &amp;cr0, const doubleN &amp;ci0
  , doubleN &amp;Jxa0, doubleN &amp;Jxb0, doubleN &amp;Jya0, doubleN &amp;Jyb0
  , const double1 &amp;e, const double1 &amp;h
  , const doubleN &amp;daa0, const doubleN &amp;dab0, const doubleN &amp;dba0, const doubleN &amp;dbb0
  , const int64_t chunk_size
  , const bool noDerivativeGlitch
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
          return perturbation_SIMD_derivatives_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />
            ( m_nFractalType, m_nPower
            , m_Reference
            , antal0, test10, test20, phase0, bGlitch0
            , m_nBailout2, nMaxIter
            , m_bNoGlitchDetection, g_real, g_imag, p
            , g_FactorAR, g_FactorAI
            , xr0, xi0
            , cr0, ci0
            , Jxa0, Jxb0, Jya0, Jyb0
            , e, h
            , daa0, dab0, dba0, dbb0
            , chunk_size
            , noDerivativeGlitch
            );
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

#if KF_SIMD >= 1
template bool perturbation_SIMD_derivatives&lt;double, int2, double2&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int2 &amp;antal0, double2 &amp;test10, double2 &amp;test20, double2 &amp;phase0, int2 &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const int2 &amp;m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double2 &amp;xr0, double2 &amp;xi0
  , const double2 &amp;cr0, const double2 &amp;ci0
  , double2 &amp;Jxa0, double2 &amp;Jxb0, double2 &amp;Jya0, double2 &amp;Jyb0
  , const double &amp;e, const double &amp;h
  , const double2 &amp;daa0, const double2 &amp;dab0, const double2 &amp;dba0, const double2 &amp;dbb0
  , const int64_t chunk_size
  , const bool noDerivativeGlitch
  );
#endif

#if KF_SIMD >= 2
template bool perturbation_SIMD_derivatives&lt;double, int4, double4&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int4 &amp;antal0, double4 &amp;test10, double4 &amp;test20, double4 &amp;phase0, int4 &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const int4 &amp;m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double4 &amp;xr0, double4 &amp;xi0
  , const double4 &amp;cr0, const double4 &amp;ci0
  , double4 &amp;Jxa0, double4 &amp;Jxb0, double4 &amp;Jya0, double4 &amp;Jyb0
  , const double &amp;e, const double &amp;h
  , const double4 &amp;daa0, const double4 &amp;dab0, const double4 &amp;dba0, const double4 &amp;dbb0
  , const int64_t chunk_size
  , const bool noDerivativeGlitch
  );
#endif

#if KF_SIMD >= 3
template bool perturbation_SIMD_derivatives&lt;double, int8, double8&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int8 &amp;antal0, double8 &amp;test10, double8 &amp;test20, double8 &amp;phase0, int8 &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const int8 &amp;m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double8 &amp;xr0, double8 &amp;xi0
  , const double8 &amp;cr0, const double8 &amp;ci0
  , double8 &amp;Jxa0, double8 &amp;Jxb0, double8 &amp;Jya0, double8 &amp;Jyb0
  , const double &amp;e, const double &amp;h
  , const double8 &amp;daa0, const double8 &amp;dab0, const double8 &amp;dba0, const double8 &amp;dbb0
  , const int64_t chunk_sizes
  , const bool noDerivativeGlitch
  );
#endif

#if KF_SIMD >= 4
template bool perturbation_SIMD_derivatives&lt;double, int16, double16&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int16 &amp;antal0, double16 &amp;test10, double16 &amp;test20, double16 &amp;phase0, int16 &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const int16 &amp;m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double16 &amp;xr0, double16 &amp;xi0
  , const double16 &amp;cr0, const double16 &amp;ci0
  , double16 &amp;Jxa0, double16 &amp;Jxb0, double16 &amp;Jya0, double16 &amp;Jyb0
  , const double &amp;e, const double &amp;h
  , const double16 &amp;daa0, const double16 &amp;dab0, const double16 &amp;dba0, const double16 &amp;dbb0
  , const int64_t chunk_size
  , const bool noDerivativeGlitch
  );
#endif

</xsl:template>
</xsl:stylesheet>
