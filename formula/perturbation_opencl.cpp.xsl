<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" />
<xsl:template match="/">

#include &lt;string&gt;
#include &lt;sstream&gt;

#include "../cl/common_cl.c"

#include "../cl/double_pre_cl.c"
#include "../cl/double_pre_r_cl.c"
#include "../cl/double_pre_c_cl.c"
#include "../cl/double_pre_m_cl.c"
#include "../cl/double_post_cl.c"
#include "../cl/double_post_r_cl.c"
#include "../cl/double_post_c_cl.c"
#include "../cl/double_post_m_cl.c"

#include "../cl/floatexp_pre_cl.c"
#include "../cl/floatexp_pre_r_cl.c"
#include "../cl/floatexp_pre_c_cl.c"
#include "../cl/floatexp_pre_m_cl.c"
#include "../cl/floatexp_post_cl.c"
#include "../cl/floatexp_post_r_cl.c"
#include "../cl/floatexp_post_c_cl.c"
#include "../cl/floatexp_post_m_cl.c"

#define STR(s) #s

const char *perturbation_decl_float =
  "#pragma OPENCL EXTENSION cl_khr_fp64: disable\n"
  "#define mantissa float\n"
  "#define exponent int\n"
  "#define LARGE_MANTISSA 1.0e30\n"
  "#define SMALL_MANTISSA 1.0e-18\n"
  "#define LARGE_EXPONENT 2000\n"
  "#define EXP_MIN (-((exponent)1 &lt;&lt; 24))\n"
  "#define EXP_MAX   ((exponent)1 &lt;&lt; 24)\n"
;

const char *perturbation_decl_double =
  "#pragma OPENCL EXTENSION cl_khr_fp64: enable\n"
  "#define mantissa double\n"
  "#define exponent long\n"
  "#define LARGE_MANTISSA 1.0e300\n"
  "#define SMALL_MANTISSA 1.0e-154\n"
  "#define LARGE_EXPONENT 2000\n"
  "#define EXP_MIN (-((exponent)1 &lt;&lt; 56))\n"
  "#define EXP_MAX   ((exponent)1 &lt;&lt; 56)\n"
;

const char *perturbation_scaled_loop_empty = STR(

void perturbation_scaled_loop
( __global const p_config    *g
, __global const mantissa    *m_refx
, __global const mantissa    *m_refy
, __global const mantissa    *m_refz
, __global const long        *m_refN
, __global const floatexp    *m_refX
, __global const floatexp    *m_refY
, __global const floatexp    *m_refZ
,                p_status_fe *l
)
{
}

);


<xsl:for-each select="formulas/group[not(@convergent='1')]/formula">
extern const int perturbation_opencl_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_derivative;
extern const char *perturbation_opencl_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_0;
extern const char *perturbation_opencl_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_1;
extern const char *perturbation_opencl_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_0;
extern const char *perturbation_opencl_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_1;
</xsl:for-each>

<xsl:for-each select="//scaled/..">
extern const char *perturbation_opencl_scaled_loop_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />;
extern const char *perturbation_opencl_scaled_derivatives_loop_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />;
</xsl:for-each>

const std::string perturbation_opencl_error = "#error unsupported fractal type and power\n";

std::string perturbation_opencl(int m_nFractalType, int m_nPower, int derivatives, int scaled, int single)
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
        {
          std::ostringstream o;
          o &lt;&lt; (single ? perturbation_decl_float : perturbation_decl_double);
          o &lt;&lt; perturbation_opencl_common;

          if (scaled)
          {
          <xsl:for-each select="scaled">
            if (derivatives)
            {
              o &lt;&lt; perturbation_opencl_scaled_derivatives_loop_<xsl:value-of select="../../@type" />_<xsl:value-of select="../@power" />;
            }
            else
            {
              o &lt;&lt; perturbation_opencl_scaled_loop_<xsl:value-of select="../../@type" />_<xsl:value-of select="../@power" />;
            }
          </xsl:for-each>
          }
          else
          {

            if (derivatives)
            {
              switch (perturbation_opencl_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_derivative)
              {
                case 'C': o &lt;&lt; perturbation_opencl_double_pre_c; break;
                case 'R': o &lt;&lt; perturbation_opencl_double_pre_r; break;
                case 'M': o &lt;&lt; perturbation_opencl_double_pre_m; break;
              }
              o &lt;&lt; perturbation_opencl_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_1;
              switch (perturbation_opencl_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_derivative)
              {
                case 'C': o &lt;&lt; perturbation_opencl_double_post_c; break;
                case 'R': o &lt;&lt; perturbation_opencl_double_post_r; break;
                case 'M': o &lt;&lt; perturbation_opencl_double_post_m; break;
              }
            }
            else
            {
              o &lt;&lt; perturbation_opencl_double_pre;
              o &lt;&lt; perturbation_opencl_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_0;
              o &lt;&lt; perturbation_opencl_double_post;
            }

            if (derivatives)
            {
              switch (perturbation_opencl_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_derivative)
              {
                case 'C': o &lt;&lt; perturbation_opencl_floatexp_pre_c; break;
                case 'R': o &lt;&lt; perturbation_opencl_floatexp_pre_r; break;
                case 'M': o &lt;&lt; perturbation_opencl_floatexp_pre_m; break;
              }
              o &lt;&lt; perturbation_opencl_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_1;
              switch (perturbation_opencl_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_derivative)
              {
                case 'C': o &lt;&lt; perturbation_opencl_floatexp_post_c; break;
                case 'R': o &lt;&lt; perturbation_opencl_floatexp_post_r; break;
                case 'M': o &lt;&lt; perturbation_opencl_floatexp_post_m; break;
              }
            }
            else
            {
              o &lt;&lt; perturbation_opencl_floatexp_pre;
              o &lt;&lt; perturbation_opencl_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_0;
              o &lt;&lt; perturbation_opencl_floatexp_post;
            }

            o &lt;&lt; perturbation_scaled_loop_empty;
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
