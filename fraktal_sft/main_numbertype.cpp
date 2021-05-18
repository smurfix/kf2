/*
Kalles Fraktaler 2
Copyright (C) 2021 Claude Heiland-Allen

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
#include "main_numbertype.h"
#include "fraktal_sft.h"
#include "resource.h"
#include "tooltip.h"

static std::vector<HWND> tooltips;

extern INT_PTR WINAPI NumberTypeProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  (void) lParam;
  if( uMsg == WM_INITDIALOG)
  {
    SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
    SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));
#define T(idc,str) tooltips.push_back(CreateToolTip(idc, hWnd, str));
    T(IDC_NUMBERTYPE_SINGLE        , "Use single precision floating point.\nUntil zoom depth 1e20.\nBe vigilant: undetected glitches may occur.")
    T(IDC_NUMBERTYPE_DOUBLE        , "Use double precision floating point.\nUntil zoom depth 1e290.\nNot supported by all OpenCL devices.")
    T(IDC_NUMBERTYPE_LONGDOUBLE    , "Use x87 long double floating point.\nUntil zoom depth 1e4900.\nNot supported by any OpenCL devices.")
    T(IDC_NUMBERTYPE_FLOATEXPSINGLE, "Use single precision with extended exponent.\nBe vigilant: undetected glitches may occur.")
    T(IDC_NUMBERTYPE_FLOATEXPDOUBLE, "Use double precision with extended exponent.\nNot supported by all OpenCL devices.")
    T(IDC_NUMBERTYPE_RESCALEDSINGLE, "Use single precision with rescaled iterations\nonly Mandelbrot power 2, Mandelbrot power 3, Burning Ship power 2, and hybrid formulas.\nBe vigilant: undetected glitches may occur.")
    T(IDC_NUMBERTYPE_RESCALEDDOUBLE, "Use double precision with rescaled iterations\nonly Mandelbrot power 2, Mandelbrot power 3, Burning Ship power 2, and hybrid formulas.\nNot supported by all OpenCL devices.")
    T(IDOK, "Apply and close.")
    T(IDCANCEL, "Close and undo.")
#undef T
    NumberType n = g_SFT.GetNumberTypes();
    SendDlgItemMessage(hWnd, IDC_NUMBERTYPE_SINGLE,         BM_SETCHECK, n.Single         ? 1 : 0, 0);
    SendDlgItemMessage(hWnd, IDC_NUMBERTYPE_DOUBLE,         BM_SETCHECK, n.Double         ? 1 : 0, 0);
    SendDlgItemMessage(hWnd, IDC_NUMBERTYPE_LONGDOUBLE,     BM_SETCHECK, n.LongDouble     ? 1 : 0, 0);
    // SendDlgItemMessage(hWnd, IDC_NUMBERTYPE_QUAD,           BM_SETCHECK, n.Quad           ? 1 : 0, 0);
    SendDlgItemMessage(hWnd, IDC_NUMBERTYPE_FLOATEXPSINGLE, BM_SETCHECK, n.FloatExpSingle ? 1 : 0, 0);
    SendDlgItemMessage(hWnd, IDC_NUMBERTYPE_FLOATEXPDOUBLE, BM_SETCHECK, n.FloatExpDouble ? 1 : 0, 0);
    SendDlgItemMessage(hWnd, IDC_NUMBERTYPE_RESCALEDSINGLE, BM_SETCHECK, n.RescaledSingle ? 1 : 0, 0);
    SendDlgItemMessage(hWnd, IDC_NUMBERTYPE_RESCALEDDOUBLE, BM_SETCHECK, n.RescaledDouble ? 1 : 0, 0);
    return 1;
  }

  else if(uMsg==WM_COMMAND)
  {
    if (wParam == IDOK || wParam == IDCANCEL)
    {
      int retval = 0;
      if (wParam == IDOK)
      {
        g_SFT.UndoStore();
        NumberType n;
        n.Single         = SendDlgItemMessage(hWnd, IDC_NUMBERTYPE_SINGLE,         BM_GETCHECK, 0, 0);
        n.Double         = SendDlgItemMessage(hWnd, IDC_NUMBERTYPE_DOUBLE,         BM_GETCHECK, 0, 0);
        n.LongDouble     = SendDlgItemMessage(hWnd, IDC_NUMBERTYPE_LONGDOUBLE,     BM_GETCHECK, 0, 0);
        // n.Quad           = SendDlgItemMessage(hWnd, IDC_NUMBERTYPE_QUAD,           BM_GETCHECK, 0, 0);
        n.FloatExpSingle = SendDlgItemMessage(hWnd, IDC_NUMBERTYPE_FLOATEXPSINGLE, BM_GETCHECK, 0, 0);
        n.FloatExpDouble = SendDlgItemMessage(hWnd, IDC_NUMBERTYPE_FLOATEXPDOUBLE, BM_GETCHECK, 0, 0);
        n.RescaledSingle = SendDlgItemMessage(hWnd, IDC_NUMBERTYPE_RESCALEDSINGLE, BM_GETCHECK, 0, 0);
        n.RescaledDouble = SendDlgItemMessage(hWnd, IDC_NUMBERTYPE_RESCALEDDOUBLE, BM_GETCHECK, 0, 0);
        g_SFT.SetNumberTypes(n);
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
