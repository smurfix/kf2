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

#include &lt;string&gt;

bool builtin_get_hybrid(const int type, const int power, std::string &amp;hybrid)
{
  <xsl:for-each select="hybrids/hybrid">
  if (type == <xsl:value-of select="@type" /> &amp;&amp; power == <xsl:value-of select="@power" />)
  {
    hybrid = "<xsl:value-of select="." />";
    return true;
  }
  </xsl:for-each>
  return "";
}

bool hybrid_get_builtin(const std::string &amp;hybrid, int &amp;type, int &amp;power)
{
  <xsl:for-each select="hybrids/hybrid">
  if (hybrid == "<xsl:value-of select="." />")
  {
    type = <xsl:value-of select="@type" />;
    power = <xsl:value-of select="@power" />;
    return true;
  }
  </xsl:for-each>
  return false;
}

</xsl:template>
</xsl:stylesheet>
