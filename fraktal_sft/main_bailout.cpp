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

static void SetDlgItemInt64(HWND hWnd, int widget, int64_t value)
{
  char s[100];
  snprintf(s, sizeof(s), "%lld", value);
  SetDlgItemText(hWnd, widget, s);
}

static int64_t GetDlgItemInt64(HWND hWnd, int widget)
{
  char s[100];
  GetDlgItemText(hWnd, widget, s, sizeof(s));
  return atoll(s);
}

static void SetDlgItemFloat64(HWND hWnd, int widget, double value)
{
  char s[100];
  snprintf(s, sizeof(s), "%.18g", value);
  SetDlgItemText(hWnd, widget, s);
}

static double GetDlgItemFloat64(HWND hWnd, int widget)
{
  char s[100];
  GetDlgItemText(hWnd, widget, s, sizeof(s));
  return atof(s);
}

static void UpdateIterations(HWND hWnd)
{
  SetDlgItemInt64(hWnd, IDC_BAILOUT_ITERATIONS, g_SFT.GetIterations());
}

static void RefreshIterations(HWND hWnd)
{
  g_SFT.SetIterations(GetDlgItemInt64(hWnd, IDC_BAILOUT_ITERATIONS));
}

static void UpdateSmoothMethod(HWND hWnd)
{
  switch (g_SFT.GetSmoothMethod())
  {
    case SmoothMethod_Log:
      SendDlgItemMessage(hWnd, IDC_BAILOUT_SMOOTHMETHOD, CB_SETCURSEL, 0, 0);
      break;
    case SmoothMethod_Sqrt:
      SendDlgItemMessage(hWnd, IDC_BAILOUT_SMOOTHMETHOD, CB_SETCURSEL, 1, 0);
      break;
  }
}

static void RefreshSmoothMethod(HWND hWnd)
{
  switch (SendDlgItemMessage(hWnd, IDC_BAILOUT_SMOOTHMETHOD, CB_GETCURSEL, 0, 0))
  {
    case 0:
      g_SFT.SetSmoothMethod(SmoothMethod_Log);
      break;
    case 1:
      g_SFT.SetSmoothMethod(SmoothMethod_Sqrt);
      break;
  }
}

static void UpdateRadiusPreset(HWND hWnd)
{
  switch (g_SFT.GetBailoutRadiusPreset())
  {
    case BailoutRadius_High:
      SendDlgItemMessage(hWnd, IDC_BAILOUT_RADIUS_PRESET, CB_SETCURSEL, 0, 0);
      break;
    case BailoutRadius_2:
      SendDlgItemMessage(hWnd, IDC_BAILOUT_RADIUS_PRESET, CB_SETCURSEL, 1, 0);
      break;
    case BailoutRadius_Low:
      SendDlgItemMessage(hWnd, IDC_BAILOUT_RADIUS_PRESET, CB_SETCURSEL, 2, 0);
      break;
    case BailoutRadius_Custom:
      SendDlgItemMessage(hWnd, IDC_BAILOUT_RADIUS_PRESET, CB_SETCURSEL, 3, 0);
      break;
  }
}

static void RefreshRadiusPreset(HWND hWnd)
{
  switch (SendDlgItemMessage(hWnd, IDC_BAILOUT_RADIUS_PRESET, CB_GETCURSEL, 0, 0))
  {
    case 0:
      g_SFT.SetBailoutRadiusPreset(BailoutRadius_High);
      break;
    case 1:
      g_SFT.SetBailoutRadiusPreset(BailoutRadius_2);
      break;
    case 2:
      g_SFT.SetBailoutRadiusPreset(BailoutRadius_Low);
      break;
    case 3:
      g_SFT.SetBailoutRadiusPreset(BailoutRadius_Custom);
      break;
  }
}

static void UpdateRadiusCustom(HWND hWnd)
{
  SetDlgItemFloat64(hWnd, IDC_BAILOUT_RADIUS_CUSTOM, g_SFT.GetBailoutRadiusCustom());
}

static void RefreshRadiusCustom(HWND hWnd)
{
  g_SFT.SetBailoutRadiusCustom(GetDlgItemFloat64(hWnd, IDC_BAILOUT_RADIUS_CUSTOM));
}

static void UpdateBailoutRe(HWND hWnd)
{
  SetDlgItemFloat64(hWnd, IDC_BAILOUT_RE, g_real);
}

static void RefreshBailoutRe(HWND hWnd)
{
  g_real = GetDlgItemFloat64(hWnd, IDC_BAILOUT_RE);
}

static void UpdateBailoutIm(HWND hWnd)
{
  SetDlgItemFloat64(hWnd, IDC_BAILOUT_IM, g_imag);
}

static void RefreshBailoutIm(HWND hWnd)
{
  g_imag = GetDlgItemFloat64(hWnd, IDC_BAILOUT_IM);
}

static void UpdateBailoutNormPreset(HWND hWnd)
{
  switch (g_SFT.GetBailoutNormPreset())
  {
    case BailoutNorm_1:
      SendDlgItemMessage(hWnd, IDC_BAILOUT_NORM_PRESET, CB_SETCURSEL, 0, 0);
      break;
    case BailoutNorm_2:
      SendDlgItemMessage(hWnd, IDC_BAILOUT_NORM_PRESET, CB_SETCURSEL, 1, 0);
      break;
    case BailoutNorm_Infinity:
      SendDlgItemMessage(hWnd, IDC_BAILOUT_NORM_PRESET, CB_SETCURSEL, 2, 0);
      break;
    case BailoutNorm_Custom:
      SendDlgItemMessage(hWnd, IDC_BAILOUT_NORM_PRESET, CB_SETCURSEL, 3, 0);
      break;
  }
}

static void RefreshBailoutNormPreset(HWND hWnd)
{
  switch (SendDlgItemMessage(hWnd, IDC_BAILOUT_NORM_PRESET, CB_GETCURSEL, 0, 0))
  {
    case 0:
      g_SFT.SetBailoutNormPreset(BailoutNorm_1);
      break;
    case 1:
      g_SFT.SetBailoutNormPreset(BailoutNorm_2);
      break;
    case 2:
      g_SFT.SetBailoutNormPreset(BailoutNorm_Infinity);
      break;
    case 3:
      g_SFT.SetBailoutNormPreset(BailoutNorm_Custom);
      break;
  }
}

static void UpdateBailoutNormCustom(HWND hWnd)
{
  SetDlgItemFloat64(hWnd, IDC_BAILOUT_NORM_CUSTOM, g_SFT.GetBailoutNormCustom());
}

static void RefreshBailoutNormCustom(HWND hWnd)
{
  g_SFT.SetBailoutNormCustom(GetDlgItemFloat64(hWnd, IDC_BAILOUT_NORM_CUSTOM));
}

extern INT_PTR WINAPI BailoutProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
  (void) lParam;
  if(uMsg==WM_INITDIALOG){
    SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
    SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));
    InitToolTip(hWnd,GetModuleHandle(NULL),GetToolText,7);

    SendDlgItemMessage(hWnd, IDC_BAILOUT_SMOOTHMETHOD, CB_ADDSTRING, 0, (LPARAM) "Log");
    SendDlgItemMessage(hWnd, IDC_BAILOUT_SMOOTHMETHOD, CB_ADDSTRING, 0, (LPARAM) "Linear");

    SendDlgItemMessage(hWnd, IDC_BAILOUT_RADIUS_PRESET, CB_ADDSTRING, 0, (LPARAM) "High");
    SendDlgItemMessage(hWnd, IDC_BAILOUT_RADIUS_PRESET, CB_ADDSTRING, 0, (LPARAM) "Bailout = 2");
    SendDlgItemMessage(hWnd, IDC_BAILOUT_RADIUS_PRESET, CB_ADDSTRING, 0, (LPARAM) "Low");
    SendDlgItemMessage(hWnd, IDC_BAILOUT_RADIUS_PRESET, CB_ADDSTRING, 0, (LPARAM) "Custom");

    SendDlgItemMessage(hWnd, IDC_BAILOUT_NORM_PRESET, CB_ADDSTRING, 0, (LPARAM) "1");
    SendDlgItemMessage(hWnd, IDC_BAILOUT_NORM_PRESET, CB_ADDSTRING, 0, (LPARAM) "2");
    SendDlgItemMessage(hWnd, IDC_BAILOUT_NORM_PRESET, CB_ADDSTRING, 0, (LPARAM) "Infinity");
    SendDlgItemMessage(hWnd, IDC_BAILOUT_NORM_PRESET, CB_ADDSTRING, 0, (LPARAM) "Custom");

    UpdateIterations(hWnd);
    UpdateSmoothMethod(hWnd);
    UpdateRadiusPreset(hWnd);
    UpdateRadiusCustom(hWnd);
    UpdateBailoutRe(hWnd);
    UpdateBailoutIm(hWnd);
    UpdateBailoutNormPreset(hWnd);
    UpdateBailoutNormCustom(hWnd);
    return 1;
  }
  else if(uMsg==WM_COMMAND){
    if(wParam==IDOK){
      g_SFT.UndoStore();
      g_bExamineDirty=TRUE;
      RefreshIterations(hWnd);
      RefreshSmoothMethod(hWnd);
      RefreshRadiusPreset(hWnd);
      RefreshRadiusCustom(hWnd);
      RefreshBailoutRe(hWnd);
      RefreshBailoutIm(hWnd);
      RefreshBailoutNormPreset(hWnd);
      RefreshBailoutNormCustom(hWnd);
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

extern const char *BailoutToolTip(int nID)
{
  switch(nID){
  case IDC_BAILOUT_ITERATIONS:
    return "Maximum number of iterations.";
  case IDC_BAILOUT_SMOOTHMETHOD:
    return "Smoothing method.\nLog is better for high radius.\nLinear is better for low radius.\n";
  case IDC_BAILOUT_RADIUS_PRESET:
    return "Preset escape radius value for iterations.\nLow setting is lowest possible for Mandelbrot and varies by power.";
  case IDC_BAILOUT_RADIUS_CUSTOM:
    return "Custom escape radius value for iterations.\nOnly used when Custom setting is selected above.";
  case IDC_BAILOUT_RE:
    return "Factor for real part when checking bailout.\n";
  case IDC_BAILOUT_IM:
    return "Factor for imaginary part when checking bailout.\n";
  case IDC_BAILOUT_NORM_PRESET:
    return "Preset norm power value when checking bailout.\n";
  case IDC_BAILOUT_NORM_CUSTOM:
    return "Custom norm power value when checking bailout.\nOnly used when Custom setting is selected above.";
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
