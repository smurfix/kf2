<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" />
<xsl:template match="/">

#define P 0
#define P2 2
#define P3 3
#define P4 4
#define P5 5
#define P6 6
#define P7 7
#define P8 8
#define P9 9
#define P10 10

std::vector&lt;clformula&gt; compile_kernels(cl_program program)
{
  cl_int err;
  std::vector&lt;clformula&gt; fs;

<xsl:for-each select="formulas/group/formula">
  {
    cl_kernel kd = clCreateKernel(program, "perturbation_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />", &amp;err);
    if (! kd) { E(err); }
    cl_kernel kfe = clCreateKernel(program, "perturbation_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />", &amp;err);
    if (! kfe) { E(err); }
    clformula f = { <xsl:value-of select="../@type" />, P<xsl:value-of select="@power" />, kd, kfe };
    fs.push_back(f);
  }
</xsl:for-each>

  return fs;
}

</xsl:template>
</xsl:stylesheet>
