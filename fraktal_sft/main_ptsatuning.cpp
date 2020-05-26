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

#include "main.h"
#include "main_bailout.h"
#include "fraktal_sft.h"
#include "resource.h"
#include "../common/tooltip.h"

extern INT_PTR WINAPI PTSATuningProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
  (void) lParam;
  if(uMsg==WM_INITDIALOG){
    SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
    SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));
    InitToolTip(hWnd,GetModuleHandle(NULL),GetToolText,8);

    SendDlgItemMessage(hWnd, IDC_PTSATUNING_GLITCHTOLERANCE, BM_SETCHECK, g_SFT.GetGlitchLowTolerance() ? 1 : 0, 0);
    SetDlgItemInt(hWnd, IDC_PTSATUNING_MAXREFERENCES, g_SFT.GetMaxReferences(), FALSE);
    SendDlgItemMessage(hWnd, IDC_PTSATUNING_APPROXTOLERANCE, BM_SETCHECK, g_SFT.GetApproxLowTolerance() ? 1 : 0, 0);
    SendDlgItemMessage(hWnd, IDC_PTSATUNING_APPROXAUTO, BM_SETCHECK, g_SFT.GetAutoApproxTerms() ? 1 : 0, 0);
    SetDlgItemInt(hWnd, IDC_PTSATUNING_APPROXTERMS, g_SFT.GetApproxTerms(), FALSE);

    return 1;
  }
  else if(uMsg==WM_COMMAND){
    if(wParam==IDOK){
      g_SFT.UndoStore();
      g_bExamineDirty=TRUE;

      g_SFT.SetGlitchLowTolerance(SendDlgItemMessage(hWnd, IDC_PTSATUNING_GLITCHTOLERANCE, BM_GETCHECK, 0, 0));
      g_SFT.SetMaxReferences(GetDlgItemInt(hWnd, IDC_PTSATUNING_MAXREFERENCES, NULL, FALSE));
      g_SFT.SetApproxLowTolerance(SendDlgItemMessage(hWnd, IDC_PTSATUNING_APPROXTOLERANCE, BM_GETCHECK, 0, 0));
      g_SFT.SetApproxTerms(GetDlgItemInt(hWnd, IDC_PTSATUNING_APPROXTERMS, NULL, FALSE));
      g_SFT.SetAutoApproxTerms(SendDlgItemMessage(hWnd, IDC_PTSATUNING_APPROXAUTO, BM_GETCHECK, 0, 0));

      ExitToolTip(hWnd);
      EndDialog(hWnd, 1);
    }
    else if(wParam==IDCANCEL){
      ExitToolTip(hWnd);
      EndDialog(hWnd,0);
    }
  }
  return 0;
}

extern const char *PTSATuningToolTip(int nID)
{
  switch(nID){
  case IDC_PTSATUNING_GLITCHTOLERANCE:
    return "Use low tolerance for glitch detection.\nMay be slower but more accurate.";
  case IDC_PTSATUNING_MAXREFERENCES:
    return "Maximum number of references for glitch correction.\nHard upper limit is 10000.";
  case IDC_PTSATUNING_APPROXTOLERANCE:
    return "Use low tolerance for series approximation.\nMay be slower but more accurate.";
  case IDC_PTSATUNING_APPROXAUTO:
    return "Use automatic number of terms for series approximation.\nBased on number of pixels remaining.";
  case IDC_PTSATUNING_APPROXTERMS:
    return "Number of terms for series approximation.\nOnly used when automatic mode is disabled.";
  case IDOK:
    return "Apply and close";
  case IDCANCEL:
    return "Close and undo";
  default:
    static char tooltip[100];
    snprintf(tooltip, 100, "%d", nID);
    return tooltip;
  }
}
