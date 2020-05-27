/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2020 Claude Heiland-Allen

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
*/

#include "tooltip.h"
#include <commctrl.h>

// <https://docs.microsoft.com/en-us/windows/win32/controls/create-a-tooltip-for-a-control>
HWND CreateToolTip(int toolID, HWND hDlg, const char *pszText)
{
  if (!toolID || !hDlg || !pszText)
  {
    return FALSE;
  }

  // Get the window of the tool.
  HWND hwndTool = GetDlgItem(hDlg, toolID);

  // Create the tooltip.
  HWND hwndTip = CreateWindowEx
    ( NULL, TOOLTIPS_CLASS, NULL
    , WS_POPUP | TTS_ALWAYSTIP | TTS_BALLOON
    , CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT
    , hDlg, NULL, GetModuleHandle(NULL), NULL
    );

  if (!hwndTool || !hwndTip)
  {
    return (HWND)NULL;
  }

  // Associate the tooltip with the tool.
  TOOLINFO toolInfo = { 0 };
  toolInfo.cbSize = sizeof(toolInfo);
  toolInfo.hwnd = hDlg;
  toolInfo.uFlags = TTF_IDISHWND | TTF_SUBCLASS;
  toolInfo.uId = (UINT_PTR)hwndTool;
  toolInfo.lpszText = const_cast<char *>(pszText);
  SendMessage(hwndTip, TTM_ADDTOOL, 0, (LPARAM)&toolInfo);

  return hwndTip;
}
