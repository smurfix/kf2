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

#include "../fraktal_sft/CFixedFloat.h"
#include "../fraktal_sft/floatexp.h"
#include "../fraktal_sft/complex.h"
#include "../fraktal_sft/reference.h"

<xsl:for-each select="formula">
bool reference_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />
  ( const int m_nFractalType, const int m_nPower
  , Reference *m_Reference
  , bool &amp;m_bStop, int64_t &amp;m_nRDone, int64_t &amp;m_nGlitchIter, int64_t &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate
  , const double m_bGlitchLowTolerance
  )
{
  if (m_nFractalType == <xsl:value-of select="@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
    m_nGlitchIter = m_nMaxIter + 1;
    int64_t nMaxIter = m_nMaxIter;
    int64_t i;
    double glitch = <xsl:value-of select="@glitch" />;
    glitch = std::exp(std::log(glitch) * (1 - m_bGlitchLowTolerance / 2));
    mp_bitcnt_t bits = mpfr_get_prec(Cr0.m_f.backend().data());
    mpfr_t Cr; mpfr_init2(Cr, bits); mpfr_set(Cr, Cr0.m_f.backend().data(), MPFR_RNDN);
    mpfr_t Ci; mpfr_init2(Ci, bits); mpfr_set(Ci, Ci0.m_f.backend().data(), MPFR_RNDN);
    mpfr_t Xr; mpfr_init2(Xr, bits); mpfr_set_d(Xr, g_SeedR, MPFR_RNDN);
    mpfr_t Xi; mpfr_init2(Xi, bits); mpfr_set_d(Xi, g_SeedI, MPFR_RNDN);
    floatexp Xrd = mpfr_get_fe(Xr);
    floatexp Xid = mpfr_get_fe(Xi);
    mpfr_t Xr2; mpfr_init2(Xr2, bits); mpfr_sqr(Xr2, Xr, MPFR_RNDN);
    mpfr_t Xi2; mpfr_init2(Xi2, bits); mpfr_sqr(Xi2, Xi, MPFR_RNDN);
    mpfr_t Xrn; mpfr_init2(Xrn, bits);
    mpfr_t Xin; mpfr_init2(Xin, bits);
    mpfr_t Ar; mpfr_init2(Ar, bits); mpfr_set_d(Ar, g_FactorAR, MPFR_RNDN);
    mpfr_t Ai; mpfr_init2(Ai, bits); mpfr_set_d(Ai, g_FactorAI, MPFR_RNDN);
<xsl:choose>
<xsl:when test="Z/@t='C'">
    complex&lt;CFixedFloat&gt; C, A, X, Xn;
    mpfr_set(C.m_r.m_f.backend().data(), Cr, MPFR_RNDN);
    mpfr_set(C.m_i.m_f.backend().data(), Ci, MPFR_RNDN);
    mpfr_set(A.m_r.m_f.backend().data(), Ar, MPFR_RNDN);
    mpfr_set(A.m_i.m_f.backend().data(), Ai, MPFR_RNDN);
</xsl:when>
</xsl:choose>

#define LOOP \
      mpfr_set(Xr, Xrn, MPFR_RNDN); \
      mpfr_set(Xi, Xin, MPFR_RNDN); \
      mpfr_sqr(Xr2, Xr, MPFR_RNDN); \
      mpfr_sqr(Xi2, Xi, MPFR_RNDN); \
      m_nRDone++; \
      Xrd = mpfr_get_fe(Xr); \
      Xid = mpfr_get_fe(Xi); \
      const floatexp abs_val = Xrd * Xrd + Xid * Xid; \
      const floatexp Xz = abs_val * glitch; \
      reference_append(m_Reference, Xrd, Xid, Xz); \
      if (abs_val &gt;= terminate){ \
        if (nMaxIter == m_nMaxIter){ \
          nMaxIter = i + 10; \
          if (nMaxIter &gt; m_nMaxIter) \
            nMaxIter = m_nMaxIter; \
          m_nGlitchIter = nMaxIter; \
        } \
      }

<xsl:choose>
<xsl:when test="Z/@t='C'">
for (i = 0; i &lt; nMaxIter &amp;&amp; !m_bStop; i++)
      {
        mpfr_set(X.m_r.m_f.backend().data(), Xr, MPFR_RNDN);
        mpfr_set(X.m_i.m_f.backend().data(), Xi, MPFR_RNDN);
        {
          <xsl:value-of select="Z" />
        }
        mpfr_set(Xrn, Xn.m_r.m_f.backend().data(), MPFR_RNDN);
        mpfr_set(Xin, Xn.m_i.m_f.backend().data(), MPFR_RNDN);
LOOP  }
</xsl:when>
<xsl:when test="Z/@t='R'">
#define DLOOP
@rr   {
        <xsl:value-of select="Z" />
      }
#undef DLOOP
</xsl:when>
</xsl:choose>

#undef LOOP
    mpfr_clear(Cr);
    mpfr_clear(Ci);
    mpfr_clear(Xr);
    mpfr_clear(Xi);
    mpfr_clear(Xr2);
    mpfr_clear(Xi2);
    mpfr_clear(Xrn);
    mpfr_clear(Xin);
    mpfr_clear(Ar);
    mpfr_clear(Ai);
    return true;
  }
  return false;
}
</xsl:for-each>

</xsl:template>
</xsl:stylesheet>
