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
#include "main_information.h"
#include "fraktal_sft.h"
#include "resource.h"
#include "tooltip.h"

#include <inttypes.h>

static int g_nPrevCalc = -1;

static void UpdateMinIterations(HWND hWnd, int64_t i)
{
  char s[100];
  snprintf(s, 100, PRId64, i);
  SetDlgItemText(hWnd,IDC_INFORMATION_MIN_ITERS,s);
}

static void UpdateMaxIterations(HWND hWnd, int64_t i)
{
  char s[100];
  snprintf(s, 100, PRId64, i);
  SetDlgItemText(hWnd,IDC_INFORMATION_MAX_ITERS,s);
}

static void UpdateApproxIterations(HWND hWnd)
{

  int64_t i = g_SFT.GetMaxApproximation();
  char s[100];
  snprintf(s, 100, PRId64, i);
  SetDlgItemText(hWnd,IDC_INFORMATION_APPROX_ITERS,s);
}

static void UpdateApproxTerms(HWND hWnd)
{
  int i = g_SFT.GetApproxTerms();
  SetDlgItemInt(hWnd,IDC_INFORMATION_APPROX_TERMS,i,FALSE);
}

static void UpdateCalculations(HWND hWnd, int nCalc, int nType)
{
  char szCalc[128];
  wsprintf(szCalc,"%d",nCalc);
  int k=strlen(szCalc);
  while(k>3){
    int n=strlen(szCalc);
    while(n>k-4){
      szCalc[n+1]=szCalc[n];
      n--;
    }
    szCalc[k-3]=' ';
    k-=3;
  }
  if(nType)
    strcat(szCalc," 000 000");
  if(g_nPrevCalc!=-1){
    int nC = nCalc-g_nPrevCalc;
    if(!nType)
      nC/=1000000;
    wsprintf(szCalc+strlen(szCalc),", %d M/s",nC);
  }
  g_nPrevCalc=nCalc;
  SetDlgItemText(hWnd,IDC_INFORMATION_CALCULATIONS,szCalc);
}

static std::vector<HWND> tooltips;

extern INT_PTR WINAPI InformationProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
  (void) lParam;
  if(uMsg==WM_INITDIALOG || uMsg==WM_TIMER){
    if(uMsg==WM_INITDIALOG){
      SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
      SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));

#define T(idc,str) tooltips.push_back(CreateToolTip(idc, hWnd, str));
      T(IDC_INFORMATION_MIN_ITERS, "Minimum number of iteration in current view")
      T(IDC_INFORMATION_MAX_ITERS, "Maximum number of iteration in current view")
      T(IDC_INFORMATION_APPROX_ITERS, "Iterations skipped by Series Approximation")
      T(IDC_INFORMATION_APPROX_TERMS, "Terms for Series approximation.\nMore terms usually yield more skipped iterations and faster rendering,\nhowever is more time consuming to be processed")
      T(IDC_INFORMATION_CALCULATIONS, "Display number of calculations performed")
#undef T

      SetTimer(hWnd,0,1000,NULL);
    }
    int64_t nMin, nMax;
    int nCalc=0,nType=0;

    g_SFT.GetIterations(nMin,nMax,&nCalc,&nType);
    UpdateMinIterations(hWnd, nMin);
    UpdateMaxIterations(hWnd, nMax);
    UpdateApproxIterations(hWnd);
    UpdateApproxTerms(hWnd);
    if (uMsg==WM_TIMER)
      UpdateCalculations(hWnd,nCalc,nType);
    else
      g_nPrevCalc=-1;
    return 1;
  }
  else if(uMsg==WM_COMMAND){
    if(wParam==IDOK || wParam==IDCANCEL){
      for (auto tooltip : tooltips)
      {
        DestroyWindow(tooltip);
      }
      tooltips.clear();
      EndDialog(hWnd, 1);
    }
  }
  return 0;
}

