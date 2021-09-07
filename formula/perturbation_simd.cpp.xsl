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
#include "simd1.h"
#include "simd2.h"
#include "simd3.h"
#include "simd4.h"

<xsl:for-each select="formulas/group[not(@convergent='1')]/formula">
template &lt;typename double1, typename intN, typename doubleN&gt;
bool perturbation_SIMD_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , intN &amp;antal0, doubleN &amp;test10, doubleN &amp;test20, doubleN &amp;phase0, intN &amp;bGlitch0
  , const double1 &amp;m_nBailout2, const int64_t nMaxIter
  , const intN &amp;m_bNoGlitchDetection, const double1 &amp;g_real, const double1 &amp;g_imag, const double1 &amp;p
  , const double1 &amp;g_FactorAR, const double1 &amp;g_FactorAI
  , doubleN &amp;xr00, doubleN &amp;xi00
  , const doubleN &amp;cr0, const doubleN &amp;ci0
  , const int64_t chunksize
  );
</xsl:for-each>

template &lt;typename double1, typename intN, typename doubleN&gt;
bool perturbation_SIMD
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , intN &amp;antal0, doubleN &amp;test10, doubleN &amp;test20, doubleN &amp;phase0, intN &amp;bGlitch0
  , const double1 &amp;m_nBailout2, const int64_t nMaxIter
  , const intN &amp;m_bNoGlitchDetection, const double1 &amp;g_real, const double1 &amp;g_imag, const double1 &amp;p
  , const double1 &amp;g_FactorAR, const double1 &amp;g_FactorAI
  , doubleN &amp;xr, doubleN &amp;xi
  , const doubleN &amp;cr, const doubleN &amp;ci
  , const int64_t chunksize
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
          return perturbation_SIMD_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />
            ( m_nFractalType, m_nPower
            , m_Reference
            , antal0, test10, test20, phase0, bGlitch0
            , m_nBailout2, nMaxIter
            , m_bNoGlitchDetection, g_real, g_imag, p
            , g_FactorAR, g_FactorAI
            , xr, xi
            , cr, ci
            , chunksize
            );
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

// explicit template instantiation

#if KF_SIMD >= 1
template bool perturbation_SIMD&lt;double, int2, double2&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int2 &amp;antal, double2 &amp;test1, double2 &amp;test2, double2 &amp;phase, int2 &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const int2 &amp;m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double2 &amp;xr, double2 &amp;xi
  , const double2 &amp;cr, const double2 &amp;ci
  , const int64_t chunksize
  );
#endif

#if KF_SIMD >= 2
template bool perturbation_SIMD&lt;double, int4, double4&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int4 &amp;antal, double4 &amp;test1, double4 &amp;test2, double4 &amp;phase, int4 &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const int4 &amp;m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double4 &amp;xr, double4 &amp;xi
  , const double4 &amp;cr, const double4 &amp;ci
  , const int64_t chunksize
  );
#endif

#if KF_SIMD >= 3
template bool perturbation_SIMD&lt;double, int8, double8&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int8 &amp;antal, double8 &amp;test1, double8 &amp;test2, double8 &amp;phase, int8 &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const int8 &amp;m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double8 &amp;xr, double8 &amp;xi
  , const double8 &amp;cr, const double8 &amp;ci
  , const int64_t chunksize
  );
#endif

#if KF_SIMD >= 4
template bool perturbation_SIMD&lt;double, int16, double16&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int16 &amp;antal, double16 &amp;test1, double16 &amp;test2, double16 &amp;phase, int16 &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const int16 &amp;m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double16 &amp;xr, double16 &amp;xi
  , const double16 &amp;cr, const double16 &amp;ci
  , const int64_t chunksize
  );
#endif

</xsl:template>
</xsl:stylesheet>
