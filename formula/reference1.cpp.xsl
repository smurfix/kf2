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
#include "../fraktal_sft/floatexp.h"
#include "../fraktal_sft/complex.h"
#include "../fraktal_sft/reference.h"

<xsl:for-each select="formula">
bool reference_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />
  ( const int m_nFractalType, const int m_nPower
  , Reference *m_Reference
  , bool &amp;m_bStop, int64_t &amp;m_nRDone, int64_t &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate
  , const double glitchLowTolerance
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
<xsl:choose>
<xsl:when test="@convergent='1'">
    const bool convergent = true;
</xsl:when>
<xsl:otherwise>
    const bool convergent = false;
</xsl:otherwise>
</xsl:choose>
    int64_t nMaxIter = m_nMaxIter;
    int64_t i;
    double glitch = <xsl:value-of select="@glitch" />;
    glitch = std::exp(std::log(glitch) * (1 - glitchLowTolerance / 2));
    mp_bitcnt_t bits = mpfr_get_prec(Cr0.m_f.backend().data());
    mpfr_t Cr; mpfr_init2(Cr, bits); mpfr_set(Cr, Cr0.m_f.backend().data(), MPFR_RNDN);
    mpfr_t Ci; mpfr_init2(Ci, bits); mpfr_set(Ci, Ci0.m_f.backend().data(), MPFR_RNDN);
    mpfr_t Xr_1; mpfr_init2(Xr_1, bits); mpfr_set_d(Xr_1, 1.0/0.0, MPFR_RNDN);
    mpfr_t Xi_1; mpfr_init2(Xi_1, bits); mpfr_set_d(Xi_1, 1.0/0.0, MPFR_RNDN);
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
<xsl:for-each select="references[@t='R']">
    mpfr_t <xsl:value-of select="@name" />, <xsl:value-of select="@update" />;
    mpfr_init2(<xsl:value-of select="@name" />, bits);
    mpfr_set_d(<xsl:value-of select="@name" />, <xsl:value-of select="@value" />, MPFR_RNDN);
    mpfr_init2(<xsl:value-of select="@update" />, bits);
</xsl:for-each>
<xsl:for-each select="references[@t='C']">
    complex&lt;CFixedFloat&gt; <xsl:value-of select="@name" /> = <xsl:value-of select="@value" />, <xsl:value-of select="@update" />;
</xsl:for-each>
<xsl:choose>
<xsl:when test="reference/@t='C'">
    complex&lt;CFixedFloat&gt; C, A, X, Xn, Xz;
    mpfr_set(C.m_r.m_f.backend().data(), Cr, MPFR_RNDN);
    mpfr_set(C.m_i.m_f.backend().data(), Ci, MPFR_RNDN);
    mpfr_set(A.m_r.m_f.backend().data(), Ar, MPFR_RNDN);
    mpfr_set(A.m_i.m_f.backend().data(), Ai, MPFR_RNDN);
</xsl:when>
</xsl:choose>

#define LOOP \
      if (convergent) \
      { \
        mpfr_set(Xr_1, Xr, MPFR_RNDN); \
        mpfr_set(Xi_1, Xi, MPFR_RNDN); \
      } \
      mpfr_set(Xr, Xrn, MPFR_RNDN); \
      mpfr_set(Xi, Xin, MPFR_RNDN); \
<xsl:for-each select="references[@t='R']"> \
      mpfr_set(<xsl:value-of select="@name" />, <xsl:value-of select="@update" />, MPFR_RNDN); \
</xsl:for-each> \
<xsl:for-each select="references[@t='C']"> \
      <xsl:value-of select="@name" /> = <xsl:value-of select="@update" />; \
</xsl:for-each> \
      mpfr_sqr(Xr2, Xr, MPFR_RNDN); \
      mpfr_sqr(Xi2, Xi, MPFR_RNDN); \
      m_nRDone++; \
      if (convergent) \
      { \
        mpfr_sub(Xrn, Xr, Xr_1, MPFR_RNDN); \
        mpfr_sub(Xin, Xi, Xi_1, MPFR_RNDN); \
        const floatexp dXrd = mpfr_get_fe(Xrn); \
        const floatexp dXid = mpfr_get_fe(Xin); \
        Xrd = mpfr_get_fe(Xr); \
        Xid = mpfr_get_fe(Xi); \
        reference_append(m_Reference, Xrd, Xid, dXrd, dXid); \
<xsl:for-each select="references[@t='R']"> \
        { \
          const floatexp Xzr = mpfr_get_fe(<xsl:value-of select="@name" />); \
          reference_append_glitch(m_Reference, <xsl:value-of select="position()" /> - 1, Xzr); \
        } \
</xsl:for-each> \
<xsl:for-each select="references[@t='C']"> \
        { \
          const floatexp Xzr = mpfr_get_fe(<xsl:value-of select="@name" />.m_r.m_f.backend().data()); \
          const floatexp Xzi = mpfr_get_fe(<xsl:value-of select="@name" />.m_i.m_f.backend().data()); \
          reference_append_glitch(m_Reference, 2 * (<xsl:value-of select="position()" /> - 1) + 0, Xzr); \
          reference_append_glitch(m_Reference, 2 * (<xsl:value-of select="position()" /> - 1) + 1, Xzi); \
        } \
</xsl:for-each> \
        if (dXrd * dXrd + dXid * dXid == floatexp(0.0)) /* FIXME threshold? */ \
        { \
          if (nMaxIter == m_nMaxIter){ \
            nMaxIter = i + 10; \
            if (nMaxIter &gt; m_nMaxIter) \
              nMaxIter = m_nMaxIter; \
          } \
        } \
      } \
      else \
      { \
        Xrd = mpfr_get_fe(Xr); \
        Xid = mpfr_get_fe(Xi); \
        const floatexp abs_val = Xrd * Xrd + Xid * Xid; \
        const floatexp Xz = abs_val * glitch; \
        reference_append(m_Reference, Xrd, Xid, Xz); \
<xsl:for-each select="references[@t='R']"> \
        reference_append_glitch(m_Reference, <xsl:value-of select="position()" />, mpfr_get_fe(<xsl:value-of select="@name" />)); \
</xsl:for-each> \
<xsl:for-each select="references[@t='C']"> \
        { \
          const floatexp Xzr = mpfr_get_fe(<xsl:value-of select="@name" />.m_r.m_f.backend().data()); \
          const floatexp Xzi = mpfr_get_fe(<xsl:value-of select="@name" />.m_i.m_f.backend().data()); \
          reference_append_glitch(m_Reference, 2 * (<xsl:value-of select="position()" /> - 1) + 1, Xzr); \
          reference_append_glitch(m_Reference, 2 * (<xsl:value-of select="position()" /> - 1) + 2, Xzi); \
        } \
</xsl:for-each> \
        if (abs_val &gt;= terminate){ \
          if (nMaxIter == m_nMaxIter){ \
            nMaxIter = i + 10; \
            if (nMaxIter &gt; m_nMaxIter) \
              nMaxIter = m_nMaxIter; \
          } \
        } \
      }

<xsl:choose>
<xsl:when test="reference/@t='C'">
for (i = 0; i &lt; nMaxIter &amp;&amp; !m_bStop; i++)
      {
        mpfr_set(X.m_r.m_f.backend().data(), Xr, MPFR_RNDN);
        mpfr_set(X.m_i.m_f.backend().data(), Xi, MPFR_RNDN);
        {
          <xsl:value-of select="reference" />
<xsl:for-each select="references[@t='C']">
          <xsl:value-of select="." />
</xsl:for-each>
        }
        mpfr_set(Xrn, Xn.m_r.m_f.backend().data(), MPFR_RNDN);
        mpfr_set(Xin, Xn.m_i.m_f.backend().data(), MPFR_RNDN);
LOOP  }
</xsl:when>
<xsl:when test="reference/@t='R'">
#define DLOOP
@rr   {
        <xsl:value-of select="reference" />
<xsl:for-each select="references[@t='R']">
        <xsl:value-of select="." />
</xsl:for-each>
      }
#undef DLOOP
</xsl:when>
</xsl:choose>

#undef LOOP
    mpfr_clear(Cr);
    mpfr_clear(Ci);
    mpfr_clear(Xr);
    mpfr_clear(Xi);
    mpfr_clear(Xr_1);
    mpfr_clear(Xi_1);
    mpfr_clear(Xr2);
    mpfr_clear(Xi2);
    mpfr_clear(Xrn);
    mpfr_clear(Xin);
    mpfr_clear(Ar);
    mpfr_clear(Ai);
<xsl:for-each select="references[@t='R']">
    mpfr_clear(<xsl:value-of select="@name" />);
    mpfr_clear(<xsl:value-of select="@update" />);
</xsl:for-each>
    return true;
  }
  return false;
}

int reference_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />_glitches()
{
  int count = 1; // Pauldelbrot
  <xsl:for-each select="references[@t='R']">count += 1;</xsl:for-each>
  <xsl:for-each select="references[@t='C']">count += 2;</xsl:for-each>
  return count;
}
</xsl:for-each>

</xsl:template>
</xsl:stylesheet>
