<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" />
<xsl:template match="/">

#include &lt;string&gt;
#include &lt;sstream&gt;

#include "common_cl.c"
#include "double_pre_cl.c"
#include "double_post_cl.c"
#include "double_post_rc_cl.c"
#include "double_post_m_cl.c"
#include "floatexp_pre_cl.c"
#include "floatexp_post_cl.c"
#include "floatexp_post_rc_cl.c"
#include "floatexp_post_m_cl.c"
#include "softfloat_pre_cl.c"
#include "softfloat_post_cl.c"
#include "softfloat_post_rc_cl.c"
#include "softfloat_post_m_cl.c"

#define STR(s) #s
<xsl:for-each select="formulas/group/formula">
static const char *perturbation_opencl_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" /> = STR(
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
#if DERIVATIVES == 1
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@cld  {
        <xsl:value-of select="derivative" />
      }
</xsl:when>
<xsl:when test="derivative/@t='C'">
@cldc {
        <xsl:value-of select="derivative" />
      }
      drn = dn.re; din = dn.im;
</xsl:when>
</xsl:choose>
#endif
@cldc {
        <xsl:value-of select="perturbation" />
      }
      xrn = xn.re; xin = xn.im;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
#if DERIVATIVES == 1
<xsl:choose>
<xsl:when test="derivative/@t='C'">
@cldc {
        <xsl:value-of select="derivative" />
      }
      drn = dn.m_r; din = dn.m_i;
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@cld  {
        <xsl:value-of select="derivative" />
      }
</xsl:when>
</xsl:choose>
#endif
@cld  {
        <xsl:value-of select="perturbation" />
      }
</xsl:when>
</xsl:choose>
);
</xsl:for-each>

<xsl:for-each select="formulas/group/formula">
static const char *perturbation_opencl_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" /> = STR(
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
#if DERIVATIVES == 1
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@clfe   {
        <xsl:value-of select="derivative" />
        }
</xsl:when>
<xsl:when test="derivative/@t='C'">
@clfec  {
        <xsl:value-of select="derivative" />
        }
      drn = dn.re; din = dn.im;
</xsl:when>
</xsl:choose>
#endif
@clfec  {
        <xsl:value-of select="perturbation" />
        }
      xrn = xn.re; xin = xn.im;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
#if DERIVATIVES == 1
<xsl:choose>
<xsl:when test="derivative/@t='C'">
@clfec  {
        <xsl:value-of select="derivative" />
        }
      drn = dn.m_r; din = dn.m_i;
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@clfe   {
        <xsl:value-of select="derivative" />
        }
</xsl:when>
</xsl:choose>
#endif
@clfe   {
        <xsl:value-of select="perturbation" />
        }
</xsl:when>
</xsl:choose>
);
</xsl:for-each>

<xsl:for-each select="formulas/group/formula">
static const char *perturbation_opencl_softfloat_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" /> = STR(
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
#if DERIVATIVES == 1
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@clsf   {
        <xsl:value-of select="derivative" />
        }
</xsl:when>
<xsl:when test="derivative/@t='C'">
@clsfc  {
        <xsl:value-of select="derivative" />
        }
      drn = dn.re; din = dn.im;
</xsl:when>
</xsl:choose>
#endif
@clsfc  {
        <xsl:value-of select="perturbation" />
        }
      xrn = xn.re; xin = xn.im;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
#if DERIVATIVES == 1
<xsl:choose>
<xsl:when test="derivative/@t='C'">
@clsfc  {
        <xsl:value-of select="derivative" />
        }
      drn = dn.m_r; din = dn.m_i;
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@clsf   {
        <xsl:value-of select="derivative" />
        }
</xsl:when>
</xsl:choose>
#endif
@clsf   {
        <xsl:value-of select="perturbation" />
        }
</xsl:when>
</xsl:choose>
);
</xsl:for-each>

static const std::string perturbation_opencl_error = "#error unsupported fractal type and power\n";

extern std::string perturbation_opencl(int m_nFractalType, int m_nPower, int derivatives)
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
        {
          std::ostringstream o;
          o &lt;&lt; perturbation_opencl_common;
          o &lt;&lt; perturbation_opencl_double_pre;
          o &lt;&lt; perturbation_opencl_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />;
          if (derivatives)
          {
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='C'">
            o &lt;&lt; perturbation_opencl_double_post_rc;
</xsl:when>
<xsl:when test="derivative/@t='M'">
            o &lt;&lt; perturbation_opencl_double_post_m;
</xsl:when>
</xsl:choose>
          }
          else
          {
            o &lt;&lt; perturbation_opencl_double_post;
          }
          o &lt;&lt; perturbation_opencl_floatexp_pre;
          o &lt;&lt; perturbation_opencl_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />;
          if (derivatives)
          {
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='C'">
            o &lt;&lt; perturbation_opencl_floatexp_post_rc;
</xsl:when>
<xsl:when test="derivative/@t='M'">
            o &lt;&lt; perturbation_opencl_floatexp_post_m;
</xsl:when>
</xsl:choose>
          }
          else
          {
            o &lt;&lt; perturbation_opencl_floatexp_post;
          }
          o &lt;&lt; perturbation_opencl_softfloat_pre;
          o &lt;&lt; perturbation_opencl_softfloat_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />;
          if (derivatives)
          {
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='C'">
            o &lt;&lt; perturbation_opencl_softfloat_post_rc;
</xsl:when>
<xsl:when test="derivative/@t='M'">
            o &lt;&lt; perturbation_opencl_softfloat_post_m;
</xsl:when>
</xsl:choose>
          }
          else
          {
            o &lt;&lt; perturbation_opencl_softfloat_post;
          }
          return o.str();
        }
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return perturbation_opencl_error;
}

</xsl:template>
</xsl:stylesheet>
