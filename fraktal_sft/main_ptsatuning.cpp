/*
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
*/

#include "main.h"
#include "main_bailout.h"
#include "fraktal_sft.h"
#include "resource.h"
#include "tooltip.h"

static std::vector<HWND> tooltips;

extern INT_PTR WINAPI PTSATuningProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
  (void) lParam;
  if(uMsg==WM_INITDIALOG){
    SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
    SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));

#define T(idc,str) tooltips.push_back(CreateToolTip(idc, hWnd, str));
    T(IDC_PTSATUNING_GLITCHTOLERANCE, "Set tolerance for glitch detection.\n0 is fast but may miss glitches.\n1 is slow but should catch all glitches.\nFractional values can be used.")
    T(IDC_PTSATUNING_MAXREFERENCES, "Maximum number of references for glitch correction.\nHard upper limit is 10000.")
    T(IDC_PTSATUNING_APPROXTOLERANCE, "Set tolerance for series approximation.\n0 is fast but may be distorted.\n1 is slow but should be more accurate.\nFractional values can be used.")
    T(IDC_PTSATUNING_APPROXAUTO, "Use automatic number of terms for series approximation.\nBased on number of pixels remaining.")
    T(IDC_PTSATUNING_APPROXTERMS, "Number of terms for series approximation.\nOnly used when automatic mode is disabled.")
    T(IDC_PTSATUNING_DERIVATIVEGLITCH, "Use derivative-based glitch detection for power 2 Mandelbrot set.\nMay be faster, in rare cases may be inaccurate.")
    T(IDC_PTSATUNING_REFERENCESTRICTZERO, "Use strict zero test for reference dynamic range.\nMay be necessary for some locations (e.g. Burning Ship near the needle)\nEnabling can be slower, but disabling can give inaccurate images.")
    T(IDC_PTSATUNING_RESCALEDSERIES, "Use rescaled version of series approximation.\nFor power 2 Mandelbrot set only.\nMay be faster, but may expose bugs (test reports welcome).")
    T(IDOK, "Apply and close")
    T(IDCANCEL, "Close and undo")
#undef T

    SetDlgItemFloat(hWnd, IDC_PTSATUNING_GLITCHTOLERANCE, g_SFT.GetGlitchLowTolerance());
    SetDlgItemInt(hWnd, IDC_PTSATUNING_MAXREFERENCES, g_SFT.GetMaxReferences(), FALSE);
    SetDlgItemFloat(hWnd, IDC_PTSATUNING_APPROXTOLERANCE, g_SFT.GetApproxLowTolerance());
    SendDlgItemMessage(hWnd, IDC_PTSATUNING_APPROXAUTO, BM_SETCHECK, g_SFT.GetAutoApproxTerms() ? 1 : 0, 0);
    SetDlgItemInt(hWnd, IDC_PTSATUNING_APPROXTERMS, g_SFT.GetApproxTerms(), FALSE);
    SendDlgItemMessage(hWnd, IDC_PTSATUNING_DERIVATIVEGLITCH, BM_SETCHECK, g_SFT.GetDerivativeGlitch() ? 1 : 0, 0);
    SendDlgItemMessage(hWnd, IDC_PTSATUNING_REFERENCESTRICTZERO, BM_SETCHECK, g_SFT.GetReferenceStrictZero() ? 1 : 0, 0);
    SendDlgItemMessage(hWnd, IDC_PTSATUNING_RESCALEDSERIES, BM_SETCHECK, g_SFT.GetUseRescaledSeries() ? 1 : 0, 0);

    return 1;
  }
  else if(uMsg==WM_COMMAND){
    if (wParam == IDOK || wParam == IDCANCEL)
    {
      int retval = 0;
      if (wParam == IDOK)
      {
        g_SFT.Stop();
        g_bExamineDirty=TRUE;
        g_SFT.SetGlitchLowTolerance(GetDlgItemFloat(hWnd, IDC_PTSATUNING_GLITCHTOLERANCE));
        g_SFT.SetMaxReferences(GetDlgItemInt(hWnd, IDC_PTSATUNING_MAXREFERENCES, NULL, FALSE));
        g_SFT.SetApproxLowTolerance(GetDlgItemFloat(hWnd, IDC_PTSATUNING_APPROXTOLERANCE));
        g_SFT.SetApproxTerms(GetDlgItemInt(hWnd, IDC_PTSATUNING_APPROXTERMS, NULL, FALSE));
        g_SFT.SetAutoApproxTerms(SendDlgItemMessage(hWnd, IDC_PTSATUNING_APPROXAUTO, BM_GETCHECK, 0, 0));
        g_SFT.SetDerivativeGlitch(SendDlgItemMessage(hWnd, IDC_PTSATUNING_DERIVATIVEGLITCH, BM_GETCHECK, 0, 0));
        g_SFT.SetReferenceStrictZero(SendDlgItemMessage(hWnd, IDC_PTSATUNING_REFERENCESTRICTZERO, BM_GETCHECK, 0, 0));
        g_SFT.SetUseRescaledSeries(SendDlgItemMessage(hWnd, IDC_PTSATUNING_RESCALEDSERIES, BM_GETCHECK, 0, 0));
        g_SFT.ApplyNewSettings();
        retval = 1;
      }
      for (auto tooltip : tooltips)
      {
        DestroyWindow(tooltip);
      }
      tooltips.clear();
      EndDialog(hWnd, retval);
    }
  }
  return 0;
}
