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
#include "../fraktal_sft/CFixedFloat.h"
#include "../fraktal_sft/reference.h"

<xsl:for-each select="formulas/group/formula">
bool reference_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />
  ( const int m_nFractalType, const int m_nPower
  , Reference *m_Reference
  , bool &amp;m_bStop, int64_t &amp;m_nRDone, int64_t &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate
  , const double m_bGlitchLowTolerance
  );
int reference_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_glitches();
</xsl:for-each>

bool reference
  ( const int m_nFractalType, const int m_nPower
  , Reference *m_Reference
  , bool &amp;m_bStop, int64_t &amp;m_nRDone, int64_t &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr, const CFixedFloat &amp;Ci
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate
  , const double m_bGlitchLowTolerance
  )
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
          return reference_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />
            ( m_nFractalType, m_nPower
            , m_Reference
            , m_bStop, m_nRDone, m_nMaxIter
            , Cr, Ci
            , g_SeedR, g_SeedI
            , g_FactorAR, g_FactorAI
            , terminate
            , m_bGlitchLowTolerance
            );
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

int reference_glitches(int m_nFractalType, int m_nPower)
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
          return reference_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_glitches();
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

</xsl:template>
</xsl:stylesheet>
