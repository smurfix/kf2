<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" />
<xsl:template match="/">

#define STR(s) #s

<xsl:for-each select="formula[not(@convergent='1')]">

extern const int perturbation_opencl_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />_derivative = '<xsl:value-of select="derivative/@t" />';

const char *perturbation_opencl_double_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />_0 = STR(
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

const char *perturbation_opencl_double_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />_1 = STR(
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

const char *perturbation_opencl_floatexp_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />_0 = STR(
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

const char *perturbation_opencl_floatexp_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />_1 = STR(
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

</xsl:for-each>

</xsl:template>
</xsl:stylesheet>
