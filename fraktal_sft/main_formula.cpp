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

static bool g_ignore_hybrids = false;

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

static void UpdateFractalTypeWidth(HWND hWnd)
{
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
}

static void UpdateFractalType(HWND hWnd, int type = -2, int p = -1)
{
  if (type < -1)
  {
    type = g_SFT.GetFractalType();
    if (g_SFT.GetUseHybridFormula()) type = -1;
  }
  if (p < 0) p = g_SFT.GetPower();
  int index = combo5_lookup_dropdown_index(hWnd, type, g_ignore_hybrids);
  SendDlgItemMessage(hWnd,IDC_FORMULA_TYPE,CB_SETCURSEL, index, 0);
  type = combo5_lookup_fractal_type(hWnd, SendDlgItemMessage(hWnd,IDC_FORMULA_TYPE,CB_GETCURSEL,0,0), g_ignore_hybrids);
  if (0 <= type)
  {
    p = validate_power_for_fractal_type(type, p);
    update_power_dropdown_for_fractal_type(hWnd, IDC_FORMULA_POWER, type, p);
    g_SFT.SetUseHybridFormula(false);
    g_SFT.SetFractalType(type);
    g_SFT.SetPower(p);
    std::string hybrid;
    EnableWindow(GetDlgItem(hWnd, IDC_FORMULA_TO_HYBRID), builtin_get_hybrid(type, p, hybrid));
  }
  else
  {
    EnableWindow(GetDlgItem(hWnd, IDC_FORMULA_TO_HYBRID), false);
    g_SFT.SetUseHybridFormula(true);
  }
  EnableWindow(GetDlgItem(hWnd, IDC_FORMULA_FROM_HYBRID), hybrid_get_builtin(to_string(g_SFT.GetHybridFormula()), type, p));
}

static int RefreshFractalType(HWND hWnd, bool refresh = true)
{
  int index = SendDlgItemMessage(hWnd,IDC_FORMULA_TYPE,CB_GETCURSEL,0,0);
  int i = combo5_lookup_fractal_type(hWnd, index, g_ignore_hybrids);
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

static void UpdateIgnoreHybrids(HWND hWnd)
{
  const int nType = RefreshFractalType(hWnd, false);
  const int nPow = RefreshPower(hWnd, false);
  std::string hybrid;
  const bool to_hybrid = builtin_get_hybrid(nType, nPow, hybrid);
  g_ignore_hybrids = SendDlgItemMessage(hWnd, IDC_FORMULA_IGNORE_HYBRIDS, BM_GETCHECK, 0, 0);
  if (g_ignore_hybrids && to_hybrid)
  {
    g_SFT.SetHybridFormula(hybrid_formula_from_string(hybrid));
    g_SFT.SetUseHybridFormula(true);
    EnableWindow(GetDlgItem(hWnd, IDC_FORMULA_FROM_HYBRID), true);
  }
  SendDlgItemMessage(hWnd, IDC_FORMULA_TYPE, CB_RESETCONTENT, 0, 0);
  combo5_addstrings(hWnd, IDC_FORMULA_TYPE, g_ignore_hybrids);
  UpdateFractalTypeWidth(hWnd);
  UpdateFractalType(hWnd);
}

static std::vector<HWND> tooltips;

extern INT_PTR WINAPI FormulaProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
  (void) lParam;
  if(uMsg==WM_INITDIALOG){
    SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
    SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));

#define T(idc,str) tooltips.push_back(CreateToolTip(idc, hWnd, str));
    T(IDC_FORMULA_IGNORE_HYBRIDS, "Don't list formulas that can be created with the Hybrid formula designer")
    T(IDC_FORMULA_TYPE, "List of type of Mandelbrot based Fractals\nSome of them have additional Power options")
    T(IDC_FORMULA_POWER, "Power of Mandelbrot function")
    T(IDC_FORMULA_SEED_RE, "Real seed value (0 is standard)")
    T(IDC_FORMULA_SEED_IM, "Imaginary seed value (0 is standard)")
    T(IDC_FORMULA_FACTOR_A_RE, "Real 'a' value (for TheRedshiftRider formulas)")
    T(IDC_FORMULA_FACTOR_A_IM, "Imaginary 'a' value (for TheRedshiftRider formulas)")
    T(IDC_FORMULA_DERIVATIVES, "Select checkbox to compute derivatives\nRequired for analytic DE")
    T(IDC_FORMULA_JITTER_SEED, "Pseudo-random number generator seed for pixel jitter\nSet to 0 to disable jitter")
    T(IDC_FORMULA_JITTER_SCALE, "Pixel jitter amount\nDefault 1.0")
    T(IDC_FORMULA_JITTER_GAUSSIAN, "Select checkbox to use Gaussian jitter\nUncheck for uniform (recommended)")
    T(IDC_FORMULA_FROM_HYBRID, "Transfer from hybrid formula designer (if possible)")
    T(IDC_FORMULA_TO_HYBRID, "Transfer to hybrid formula designer (if possible)")
    T(IDOK, "Apply and close")
    T(IDCANCEL, "Close and undo")
#undef T

    SendDlgItemMessage(hWnd, IDC_FORMULA_TYPE, CB_RESETCONTENT, 0, 0);
    g_ignore_hybrids = SendDlgItemMessage(hWnd, IDC_FORMULA_IGNORE_HYBRIDS, BM_GETCHECK, 0, 0);
    combo5_addstrings(hWnd, IDC_FORMULA_TYPE, g_ignore_hybrids);
    UpdateFractalTypeWidth(hWnd);
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
    else if (wParam == IDC_FORMULA_TO_HYBRID)
    {
      const int nType = RefreshFractalType(hWnd, false);
      const int nPow = RefreshPower(hWnd, false);
      std::string hybrid;
      const bool to_hybrid = builtin_get_hybrid(nType, nPow, hybrid);
      if (to_hybrid)
      {
        g_SFT.SetHybridFormula(hybrid_formula_from_string(hybrid));
        g_SFT.SetUseHybridFormula(true);
        UpdateFractalType(hWnd);
        EnableWindow(GetDlgItem(hWnd, IDC_FORMULA_FROM_HYBRID), true);
      }
    }
    else if (wParam == IDC_FORMULA_FROM_HYBRID)
    {
      SendDlgItemMessage(hWnd, IDC_FORMULA_IGNORE_HYBRIDS, BM_SETCHECK, 0, 0);
      UpdateIgnoreHybrids(hWnd);
      std::string hybrid = to_string(g_SFT.GetHybridFormula());
      int nType = -2;
      int nPow = -1;
      const bool from_hybrid = hybrid_get_builtin(hybrid, nType, nPow);
      if (from_hybrid)
      {
        g_SFT.SetUseHybridFormula(false);
        g_SFT.SetFractalType(nType);
        g_SFT.SetPower(nPow);
        UpdateFractalType(hWnd);
        EnableWindow(GetDlgItem(hWnd, IDC_FORMULA_TO_HYBRID), true);
      }
    }
    else if (HIWORD(wParam) == BN_CLICKED && LOWORD(wParam) == IDC_FORMULA_IGNORE_HYBRIDS)
    {
      UpdateIgnoreHybrids(hWnd);
    }
    else if(HIWORD(wParam)==CBN_SELCHANGE && LOWORD(wParam)==IDC_FORMULA_TYPE){
      int nType = RefreshFractalType(hWnd, false);
      int nPow = RefreshPower(hWnd, false);
      UpdateFractalType(hWnd, nType, nPow);
    }
  }
  return 0;
}
