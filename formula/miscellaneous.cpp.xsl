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

#include &lt;windows.h&gt;
#include &lt;string&gt;

#include "formula.h"

bool scaling_supported(const int m_nFractalType, const int m_nPower, const bool derivatives)
{
  <xsl:for-each select="//scaled/..">
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
    return true;
  </xsl:for-each>
  return false;
}

void combo5_addstrings(HWND hWnd, const int IDC_COMBO5)
{
  SendDlgItemMessage(hWnd,IDC_COMBO5,CB_ADDSTRING,0,(LPARAM)"(Hybrid)");
  <xsl:for-each select="formulas/group">
    SendDlgItemMessage(hWnd,IDC_COMBO5,CB_ADDSTRING,0,(LPARAM)"<xsl:value-of select="@name" />"); // <xsl:value-of select="@type" />
  </xsl:for-each>
}

int validate_power_for_fractal_type(const int m_nFractalType, const int m_nPower)
{
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
      switch (m_nPower)
      {
        default:
      <xsl:for-each select="formula">
        case <xsl:value-of select="@power" />: return <xsl:value-of select="@power" />;
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
    default:
      return 2;
  }
}

void update_power_dropdown_for_fractal_type(HWND hWnd, const int IDC_COMBO3, const int m_nFractalType, const int m_nPower)
{
  SendDlgItemMessage(hWnd,IDC_COMBO3,CB_RESETCONTENT,0,0);
  int selected = 0;
  int ix = 0;
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
    {
      <xsl:for-each select="formula">
        if (<xsl:value-of select="@power" /> == m_nPower) selected = ix;
        SendDlgItemMessage(hWnd,IDC_COMBO3,CB_ADDSTRING,0,(LPARAM)"<xsl:value-of select="@power" />");
        ix++;
      </xsl:for-each>
      break;
    }
  </xsl:for-each>
  }
  SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,selected,0);
  EnableWindow(GetDlgItem(hWnd,IDC_COMBO3), ix > 1);
}

bool builtin_get_hybrid(const int type, const int power, std::string &amp;hybrid)
{
  <xsl:for-each select="formulas/group/formula[@hybrid]">
  if (type == <xsl:value-of select="../@type" /> &amp;&amp; power == <xsl:value-of select="@power" />)
  {
    hybrid = "<xsl:value-of select="@hybrid" />";
    return true;
  }
  </xsl:for-each>
  return false;
}

bool hybrid_get_builtin(const std::string &amp;hybrid, int &amp;type, int &amp;power)
{
  <xsl:for-each select="formulas/group/formula[@hybrid]">
  if (hybrid == "<xsl:value-of select="@hybrid" />")
  {
    type = <xsl:value-of select="../@type" />;
    power = <xsl:value-of select="@power" />;
    return true;
  }
  </xsl:for-each>
  return false;
}

</xsl:template>
</xsl:stylesheet>
