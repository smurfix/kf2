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
#include "main_formula.h"
#include "fraktal_sft.h"
#include "resource.h"
#include "../formula/formula.h"
#include "tooltip.h"

static void UpdatePower(HWND hWnd)
{
  int i = g_SFT.GetPower();
  SetDlgItemInt(hWnd,IDC_FORMULA_POWER,i,FALSE);
}

static int RefreshPower(HWND hWnd, bool refresh = true)
{
  char szPower[256];
  GetDlgItemText(hWnd,IDC_FORMULA_POWER,szPower,sizeof(szPower));
  int i = atoi(szPower);
  if (refresh)
  {
    i = validate_power_for_fractal_type(g_SFT.GetFractalType(), i);
    g_SFT.SetPower(i);
  }
  return i;
}

static void UpdateFractalType(HWND hWnd, int i = -2, int p = -1)
{
  if (i < -1)
  {
    i = g_SFT.GetFractalType();
    if (g_SFT.GetUseHybridFormula()) i = -1;
  }
  if (p < 0) p = g_SFT.GetPower();
  SendDlgItemMessage(hWnd,IDC_FORMULA_TYPE,CB_SETCURSEL,i + 1,0);
  i = SendDlgItemMessage(hWnd,IDC_FORMULA_TYPE,CB_GETCURSEL,0,0) - 1;
  if (0 <= i)
  {
    p = validate_power_for_fractal_type(i, p);
    update_power_dropdown_for_fractal_type(hWnd, IDC_FORMULA_POWER, i, p);
    g_SFT.SetUseHybridFormula(false);
    g_SFT.SetFractalType(i);
    g_SFT.SetPower(p);
  }
  else
  {
    g_SFT.SetUseHybridFormula(true);
  }
}

static int RefreshFractalType(HWND hWnd, bool refresh = true)
{
  int i = SendDlgItemMessage(hWnd,IDC_FORMULA_TYPE,CB_GETCURSEL,0,0) - 1;
  if (refresh)
  {
    if (0 <= i)
    {
      g_SFT.SetUseHybridFormula(false);
      g_SFT.SetFractalType(i);
    }
    else
    {
      g_SFT.SetUseHybridFormula(true);
    }
  }
  return i;
}

static void UpdateSeedR(HWND hWnd)
{
  char szTmp[40];
  snprintf(szTmp,40,"%g",g_SeedR);
  SetDlgItemText(hWnd,IDC_FORMULA_SEED_RE,szTmp);
}

static void RefreshSeedR(HWND hWnd)
{
  char szTmp[40];
  GetDlgItemText(hWnd,IDC_FORMULA_SEED_RE,szTmp,sizeof(szTmp));
  g_SeedR = atof(szTmp);
}

static void UpdateSeedI(HWND hWnd)
{
  char szTmp[40];
  snprintf(szTmp,40,"%g",g_SeedI);
  SetDlgItemText(hWnd,IDC_FORMULA_SEED_IM,szTmp);
}

static void RefreshSeedI(HWND hWnd)
{
  char szTmp[40];
  GetDlgItemText(hWnd,IDC_FORMULA_SEED_IM,szTmp,sizeof(szTmp));
  g_SeedI = atof(szTmp);
}

static void UpdateFactorAR(HWND hWnd)
{
  char szTmp[40];
  snprintf(szTmp,40,"%g",g_FactorAR);
  SetDlgItemText(hWnd,IDC_FORMULA_FACTOR_A_RE,szTmp);
}

static void RefreshFactorAR(HWND hWnd)
{
  char szTmp[40];
  GetDlgItemText(hWnd,IDC_FORMULA_FACTOR_A_RE,szTmp,sizeof(szTmp));
  g_FactorAR = atof(szTmp);
}

static void UpdateFactorAI(HWND hWnd)
{
  char szTmp[40];
  snprintf(szTmp,40,"%g",g_FactorAI);
  SetDlgItemText(hWnd,IDC_FORMULA_FACTOR_A_IM,szTmp);
}

static void RefreshFactorAI(HWND hWnd)
{
  char szTmp[40];
  GetDlgItemText(hWnd,IDC_FORMULA_FACTOR_A_IM,szTmp,sizeof(szTmp));
  g_FactorAI = atof(szTmp);
}

static void UpdateJitterSeed(HWND hWnd)
{
	int i = g_SFT.GetJitterSeed();
	SetDlgItemInt(hWnd,IDC_FORMULA_JITTER_SEED,i,FALSE);
}

static void RefreshJitterSeed(HWND hWnd)
{
	char sz[256];
	GetDlgItemText(hWnd,IDC_FORMULA_JITTER_SEED,sz,sizeof(sz));
	int i = atoi(sz);
	g_SFT.SetJitterSeed(i);
}

static void UpdateJitterScale(HWND hWnd)
{
	double f = g_SFT.GetJitterScale();
	char szTmp[40];
	snprintf(szTmp,40,"%g",f);
	SetDlgItemText(hWnd,IDC_FORMULA_JITTER_SCALE,szTmp);
}

static void RefreshJitterScale(HWND hWnd)
{
	char szTmp[40];
	GetDlgItemText(hWnd,IDC_FORMULA_JITTER_SCALE,szTmp,sizeof(szTmp));
	double f = atof(szTmp);
	g_SFT.SetJitterScale(f > 0 ? f : 1);
}

static void UpdateJitterShape(HWND hWnd)
{
	int i = g_SFT.GetJitterShape();
	SendDlgItemMessage(hWnd,IDC_FORMULA_JITTER_GAUSSIAN,BM_SETCHECK,i != 0,0);
}

static void RefreshJitterShape(HWND hWnd)
{
	g_SFT.SetJitterShape(SendDlgItemMessage(hWnd,IDC_FORMULA_JITTER_GAUSSIAN,BM_GETCHECK,0,0) ? 1 : 0);
}

static void UpdateDerivatives(HWND hWnd)
{
	int i = g_SFT.GetDerivatives();
	SendDlgItemMessage(hWnd,IDC_FORMULA_DERIVATIVES,BM_SETCHECK,i != 0,0);
}

static void RefreshDerivatives(HWND hWnd)
{
	g_SFT.SetDerivatives(SendDlgItemMessage(hWnd,IDC_FORMULA_DERIVATIVES,BM_GETCHECK,0,0) ? 1 : 0);
}

static std::vector<HWND> tooltips;

extern INT_PTR WINAPI FormulaProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
  (void) lParam;
  if(uMsg==WM_INITDIALOG){
    SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
    SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));

#define T(idc,str) tooltips.push_back(CreateToolTip(idc, hWnd, str));
    T(IDC_FORMULA_TYPE, "List of type of Mandelbrot based Fractals\nSome of them have additional Power options")
    T(IDC_FORMULA_POWER, "Power of Mandelbrot function")
    T(IDC_FORMULA_SEED_RE, "Real seed value (0 is standard)")
    T(IDC_FORMULA_SEED_IM, "Imaginary seed value (0 is standard)")
    T(IDC_FORMULA_FACTOR_A_RE, "Real 'a' value (for TheRedshiftRider formulas)")
    T(IDC_FORMULA_FACTOR_A_IM, "Imaginary 'a' value (for TheRedshiftRider formulas)")
    T(IDC_FORMULA_DERIVATIVES, "Select checkbox to compute derivatives\nRequired for analytic DE")
    T(IDC_FORMULA_JITTER_SEED, "Pseudo-random number generator seed for pixel jitter\nSet to 0 to disable jitter")
    T(IDC_FORMULA_JITTER_SCALE, "Pixel jitter amount\nDefault 1.0")
    T(IDC_FORMULA_JITTER_GAUSSIAN, "Select checkbox to use Gaussian jitter\nUncheck for uniform")
    T(IDOK, "Apply and close")
    T(IDCANCEL, "Close and undo")
#undef T

    combo5_addstrings(hWnd, IDC_FORMULA_TYPE);
    SIZE sc;
    HDC hDC = GetDC(NULL);
    SelectObject(hDC,(HFONT)GetStockObject(ANSI_VAR_FONT));
    int i, nMaxWidth=0;
    for(i=0;i<SendDlgItemMessage(hWnd,IDC_FORMULA_TYPE,CB_GETCOUNT,0,0);i++){
      int n = SendDlgItemMessage(hWnd,IDC_FORMULA_TYPE,CB_GETLBTEXTLEN,i,0);
      char *szT = new char[n+1];
      SendDlgItemMessage(hWnd,IDC_FORMULA_TYPE,CB_GETLBTEXT,i,(LPARAM)szT);
      GetTextExtentPoint32A(hDC,szT,strlen(szT),&sc);
      if(sc.cx>nMaxWidth)
        nMaxWidth = sc.cx;
      delete[] szT;
    }
    SendDlgItemMessage(hWnd,IDC_FORMULA_TYPE,CB_SETDROPPEDWIDTH,nMaxWidth+8+GetSystemMetrics(SM_CXHTHUMB),0);
    UpdateFractalType(hWnd);
    RefreshFractalType(hWnd, false);
    UpdatePower(hWnd);
    UpdateSeedR(hWnd);
    UpdateSeedI(hWnd);
    UpdateFactorAR(hWnd);
    UpdateFactorAI(hWnd);
    UpdateDerivatives(hWnd);
    UpdateJitterSeed(hWnd);
    UpdateJitterScale(hWnd);
    UpdateJitterShape(hWnd);
    return 1;
  }
  else if(uMsg==WM_COMMAND){
    if (wParam == IDOK || wParam == IDCANCEL)
    {
      int retval = 0;
      if (wParam == IDOK){
        g_SFT.UndoStore();
        g_bExamineDirty=TRUE;
        RefreshSeedR(hWnd);
        RefreshSeedI(hWnd);
        RefreshFactorAR(hWnd);
        RefreshFactorAI(hWnd);
        RefreshFractalType(hWnd);
        RefreshPower(hWnd);
        RefreshDerivatives(hWnd);
        RefreshJitterSeed(hWnd);
        RefreshJitterScale(hWnd);
        RefreshJitterShape(hWnd);
        retval = 1;
      }
      for (auto tooltip : tooltips)
      {
        DestroyWindow(tooltip);
      }
      tooltips.clear();
      EndDialog(hWnd, retval);
    }
    else if(HIWORD(wParam)==CBN_SELCHANGE && LOWORD(wParam)==IDC_FORMULA_TYPE){
      int nType = RefreshFractalType(hWnd, false);
      int nPow = RefreshPower(hWnd, false);
      UpdateFractalType(hWnd, nType, nPow);
      EnableWindow(GetDlgItem(hWnd,IDC_FORMULA_POWER),nType<=4);
    }
  }
  return 0;
}
