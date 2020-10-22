<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" />
<xsl:template match="/">

#include &lt;string&gt;
#include &lt;sstream&gt;

#include "common_cl.c"

#include "double_pre_cl.c"
#include "double_pre_r_cl.c"
#include "double_pre_c_cl.c"
#include "double_pre_m_cl.c"
#include "double_post_cl.c"
#include "double_post_r_cl.c"
#include "double_post_c_cl.c"
#include "double_post_m_cl.c"

#include "floatexp_pre_cl.c"
#include "floatexp_pre_r_cl.c"
#include "floatexp_pre_c_cl.c"
#include "floatexp_pre_m_cl.c"
#include "floatexp_post_cl.c"
#include "floatexp_post_r_cl.c"
#include "floatexp_post_c_cl.c"
#include "floatexp_post_m_cl.c"

#if 0
#include "softfloat_pre_cl.c"
#include "softfloat_pre_r_cl.c"
#include "softfloat_pre_c_cl.c"
#include "softfloat_pre_m_cl.c"
#include "softfloat_post_cl.c"
#include "softfloat_post_r_cl.c"
#include "softfloat_post_c_cl.c"
#include "softfloat_post_m_cl.c"
#endif

#define STR(s) #s

<xsl:for-each select="formulas/group/formula">

static const char *perturbation_opencl_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_0 = STR(
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
{
  const dcomplex X = { Xr COMMA Xi } COMMA x = { xr COMMA xi } COMMA Xx = { Xxr COMMA Xxi } COMMA c = { cr COMMA ci };
  dcomplex xn;
@cldc {
        <xsl:value-of select="perturbation" />
      }
  xrn = xn.re;
  xin = xn.im;
}
</xsl:when>
<xsl:when test="perturbation/@t='R'">
@cld  {
        <xsl:value-of select="perturbation" />
      }
</xsl:when>
</xsl:choose>
);

static const char *perturbation_opencl_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_1 = STR(
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
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
</xsl:when>
</xsl:choose>
@cldc {
        <xsl:value-of select="perturbation" />
      }
</xsl:when>
<xsl:when test="perturbation/@t='R'">
<xsl:choose>
<xsl:when test="derivative/@t='C'">
@cldc {
        <xsl:value-of select="derivative" />
      }
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@cld  {
        <xsl:value-of select="derivative" />
      }
</xsl:when>
</xsl:choose>
@cld  {
        <xsl:value-of select="perturbation" />
      }
</xsl:when>
</xsl:choose>
);

static const char *perturbation_opencl_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_0 = STR(
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
{
  const fecomplex X = { Xr COMMA Xi } COMMA x = { xr COMMA xi } COMMA Xx = { Xxr COMMA Xxi } COMMA c = { cr COMMA ci };
  fecomplex xn;
@clfec  {
        <xsl:value-of select="perturbation" />
        }
  xrn = xn.re;
  xin = xn.im;
}
</xsl:when>
<xsl:when test="perturbation/@t='R'">
@clfe   {
        <xsl:value-of select="perturbation" />
        }
</xsl:when>
</xsl:choose>
);

static const char *perturbation_opencl_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_1 = STR(
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
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
</xsl:when>
</xsl:choose>
@clfec  {
        <xsl:value-of select="perturbation" />
        }
</xsl:when>
<xsl:when test="perturbation/@t='R'">
<xsl:choose>
<xsl:when test="derivative/@t='C'">
@clfec  {
        <xsl:value-of select="derivative" />
        }
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@clfe   {
        <xsl:value-of select="derivative" />
        }
</xsl:when>
</xsl:choose>
@clfe   {
        <xsl:value-of select="perturbation" />
        }
</xsl:when>
</xsl:choose>
);

#if 0
static const char *perturbation_opencl_softfloat_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_0 = STR(
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
@clsfc  {
        <xsl:value-of select="perturbation" />
        }
</xsl:when>
<xsl:when test="perturbation/@t='R'">
@clsf   {
        <xsl:value-of select="perturbation" />
        }
</xsl:when>
</xsl:choose>
);
#endif
</xsl:for-each>

#if 0
<xsl:for-each select="formulas/group/formula">
static const char *perturbation_opencl_softfloat_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_1 = STR(
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
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
</xsl:when>
</xsl:choose>
@clsfc  {
        <xsl:value-of select="perturbation" />
        }
</xsl:when>
<xsl:when test="perturbation/@t='R'">
<xsl:choose>
<xsl:when test="derivative/@t='C'">
@clsfc  {
        <xsl:value-of select="derivative" />
        }
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@clsf   {
        <xsl:value-of select="derivative" />
        }
</xsl:when>
</xsl:choose>
@clsf   {
        <xsl:value-of select="perturbation" />
        }
</xsl:when>
</xsl:choose>
);
</xsl:for-each>
#endif

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

          if (derivatives)
          {
<xsl:choose>
<xsl:when test="derivative/@t='R'">
            o &lt;&lt; perturbation_opencl_double_pre_r;
</xsl:when>
<xsl:when test="derivative/@t='C'">
            o &lt;&lt; perturbation_opencl_double_pre_c;
</xsl:when>
<xsl:when test="derivative/@t='M'">
            o &lt;&lt; perturbation_opencl_double_pre_m;
</xsl:when>
</xsl:choose>
            o &lt;&lt; perturbation_opencl_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_1;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
            o &lt;&lt; perturbation_opencl_double_post_r;
</xsl:when>
<xsl:when test="derivative/@t='C'">
            o &lt;&lt; perturbation_opencl_double_post_c;
</xsl:when>
<xsl:when test="derivative/@t='M'">
            o &lt;&lt; perturbation_opencl_double_post_m;
</xsl:when>
</xsl:choose>
          }
          else
          {
            o &lt;&lt; perturbation_opencl_double_pre;
            o &lt;&lt; perturbation_opencl_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_0;
            o &lt;&lt; perturbation_opencl_double_post;
          }

          if (derivatives)
          {
<xsl:choose>
<xsl:when test="derivative/@t='R'">
            o &lt;&lt; perturbation_opencl_floatexp_pre_r;
</xsl:when>
<xsl:when test="derivative/@t='C'">
            o &lt;&lt; perturbation_opencl_floatexp_pre_c;
</xsl:when>
<xsl:when test="derivative/@t='M'">
            o &lt;&lt; perturbation_opencl_floatexp_pre_m;
</xsl:when>
</xsl:choose>
            o &lt;&lt; perturbation_opencl_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_1;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
            o &lt;&lt; perturbation_opencl_floatexp_post_r;
</xsl:when>
<xsl:when test="derivative/@t='C'">
            o &lt;&lt; perturbation_opencl_floatexp_post_c;
</xsl:when>
<xsl:when test="derivative/@t='M'">
            o &lt;&lt; perturbation_opencl_floatexp_post_m;
</xsl:when>
</xsl:choose>
          }
          else
          {
            o &lt;&lt; perturbation_opencl_floatexp_pre;
            o &lt;&lt; perturbation_opencl_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_0;
            o &lt;&lt; perturbation_opencl_floatexp_post;
          }

#if 0
          if (derivatives)
          {
<xsl:choose>
<xsl:when test="derivative/@t='R'">
            o &lt;&lt; perturbation_opencl_softfloat_pre_r;
</xsl:when>
<xsl:when test="derivative/@t='C'">
            o &lt;&lt; perturbation_opencl_softfloat_pre_c;
</xsl:when>
<xsl:when test="derivative/@t='M'">
            o &lt;&lt; perturbation_opencl_softfloat_pre_m;
</xsl:when>
</xsl:choose>
            o &lt;&lt; perturbation_opencl_softfloat_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_1;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
            o &lt;&lt; perturbation_opencl_softfloat_post_r;
</xsl:when>
<xsl:when test="derivative/@t='C'">
            o &lt;&lt; perturbation_opencl_softfloat_post_c;
</xsl:when>
<xsl:when test="derivative/@t='M'">
            o &lt;&lt; perturbation_opencl_softfloat_post_m;
</xsl:when>
</xsl:choose>
          }
          else
          {
            o &lt;&lt; perturbation_opencl_softfloat_pre;
            o &lt;&lt; perturbation_opencl_softfloat_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_0;
            o &lt;&lt; perturbation_opencl_softfloat_post;
          }
#endif

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
