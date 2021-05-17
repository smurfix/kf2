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
#include "main_transformation.h"
#include "fraktal_sft.h"
#include "resource.h"
#include "tooltip.h"
#include "../common/bitmap.h"
#include "../common/timer.h"
#include "newton.h" // for progress_t and get_formula
#define KF_MAIN 1
#include "../formula/generated/formula.h" // for struct formula
#undef SIZE

#include <commctrl.h>

static bool g_transformation_useddz = false;
static volatile int g_transformation_running = 0;
static volatile bool g_transformation_still_running = false;

static polar2 original_transformation = polar2(1, 0, 1, 0);
static polar2 current_transformation = polar2(1, 0, 1, 0);

static std::vector<HWND> tooltips;

static const double deg = 360 / 6.283185307179586;

static double g_skew[4];

static std::string s_skew;

static DWORD WINAPI ThSkewProgress(progress_t *progress)
{
  while (progress->running)
  {
    Sleep(250);
    progress->elapsed_time = get_wall_time() - progress->start_time;
    int iters = progress->counters[0];
    int iter = progress->counters[1];
    char status[100];
    snprintf(status, 100, "Skew %d%% (%ds)", (int) (iter * 100.0 / iters), (int) progress->elapsed_time);
    s_skew = status;
    SetDlgItemText(progress->hWnd, IDC_TRANSFORMATION_STATUS, status);
  }
  SetEvent(progress->hDone);
  return 0;
}

static int WINAPI ThSkew(HWND hWnd)
{
  SetThreadPriority(GetCurrentThread(), THREAD_MODE_BACKGROUND_BEGIN);
  SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_LOWEST);
  const int type = g_SFT.GetFractalType();
  const int power = g_SFT.GetPower();
  const struct formula *f = get_formula(type, power);

  const int64_t iters = g_SFT.GetIterations();
  std::string g_szZoom = g_SFT.GetZoom();
  const char *e = strstr(g_szZoom.c_str(),"E");
  if(!e) e = strstr(g_szZoom.c_str(),"e");
  int expo = (e?atoi(e+1):0);
  unsigned uprec = expo + 6;
  Precision prec(uprec);
  complex<CDecNumber> center(g_SFT.GetRe(), g_SFT.GetIm());

  g_skew[0] = 1;
  g_skew[1] = 0;
  g_skew[2] = 0;
  g_skew[3] = 1;
  // fork progress updater
  progress_t progress = { { int(std::min(iters, int64_t(INT_MAX))), 0, 0, 0 }, true, hWnd, CreateEvent(NULL, 0, 0, NULL), get_wall_time(), 0 };
  HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE) ThSkewProgress, (LPVOID) &progress, 0, NULL);
  CloseHandle(hThread);
  // find skew
  bool ok = false;
  if (g_SFT.GetUseHybridFormula())
  {
    ok = hybrid_skew(g_SFT.GetHybridFormula(), iters, center.m_r.m_dec, center.m_i.m_dec, g_transformation_useddz, &g_skew[0], &g_transformation_running, &progress.counters[0]);
  }
  else if (f && f->skew)
  {
    ok = f->skew(iters, g_FactorAR, g_FactorAI, center.m_r.m_dec.backend().data(), center.m_i.m_dec.backend().data(), g_transformation_useddz, &g_skew[0], &g_transformation_running, &progress.counters[0]);
  }
  // join progress updater
  progress.running = false;
  WaitForMultipleObjects(1, &progress.hDone, TRUE, INFINITE);
  CloseHandle(progress.hDone);
  char status[100];
  snprintf(status, 100, "Skew %d%% (%ds)", (int) (progress.counters[1] * 100.0 / progress.counters[0]), (int) progress.elapsed_time);
  s_skew = status;
  SetDlgItemText(hWnd, IDC_TRANSFORMATION_STATUS, status);
  // report success
  g_transformation_running = false;
  g_transformation_still_running = false;
  PostMessage(hWnd, WM_USER + 2, 0, ok);
  mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE);
  return 0;
}

static bool refreshing = false; // prevent infinite update loop
extern INT_PTR WINAPI TransformationProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
  (void) lParam;
  if(uMsg==WM_INITDIALOG){
    SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
    SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));

#define T(idc,str) tooltips.push_back(CreateToolTip(idc, hWnd, str));
#define T2(idc1,idc2,str) T(idc1, str) T(idc2, str)
    T2(IDC_TRANSFORMATION_ROTATEANGLE, IDC_TRANSFORMATION_ROTATEANGLE_SPIN, "Rotation angle in degrees.\nUse left mouse button to rotate image.")
    T2(IDC_TRANSFORMATION_STRETCHANGLE, IDC_TRANSFORMATION_STRETCHANGLE_SPIN, "Stretch angle in degrees.\nUse right mouse button to stretch image.")
    T2(IDC_TRANSFORMATION_STRETCHAMOUNT, IDC_TRANSFORMATION_STRETCHAMOUNT_SPIN, "Stretch amount in cents of powers of two.\nUse right mouse button to stretch image.")
    T(IDC_TRANSFORMATION_AUTOSKEW, "Calculate transformation that unskews stretched features.\nBased on the derivatives of the center of the image.\nAuto skew for minibrots is available via Newton-Raphson Zooming")
    T(IDC_TRANSFORMATION_USEDDZ, "Also use derivative with respect to Z.\nThe derivative with respect to C is always used.")
    T(IDOK, "Apply and close")
    T(IDCANCEL, "Close and undo")
#undef T2
#undef T
		SendDlgItemMessage(hWnd, IDC_TRANSFORMATION_ROTATEANGLE_SPIN, UDM_SETRANGE, 0, MAKELONG(360, -360));
		SendDlgItemMessage(hWnd, IDC_TRANSFORMATION_STRETCHANGLE_SPIN, UDM_SETRANGE, 0, MAKELONG(360, -360));
		SendDlgItemMessage(hWnd, IDC_TRANSFORMATION_STRETCHAMOUNT_SPIN, UDM_SETRANGE, 0, MAKELONG(360,-360));

    original_transformation = g_SFT.GetTransformPolar();
    current_transformation = polar2(1, 0, 1, 0);
    SetDlgItemFloat(hWnd, IDC_TRANSFORMATION_ROTATEANGLE, current_transformation.rotate * deg);
    SetDlgItemFloat(hWnd, IDC_TRANSFORMATION_STRETCHANGLE, current_transformation.stretch_angle * deg);
    SetDlgItemFloat(hWnd, IDC_TRANSFORMATION_STRETCHAMOUNT, std::log2(current_transformation.stretch_factor) * 100);
    SendDlgItemMessage(hWnd, IDC_TRANSFORMATION_USEDDZ, BM_SETCHECK, g_transformation_useddz, 0);
    return 1;
  }
  else if (uMsg == WM_COMMAND)
  {
    if ((! refreshing && (
        LOWORD(wParam) == IDC_TRANSFORMATION_ROTATEANGLE ||
        LOWORD(wParam) == IDC_TRANSFORMATION_ROTATEANGLE_SPIN ||
        LOWORD(wParam) == IDC_TRANSFORMATION_STRETCHANGLE ||
        LOWORD(wParam) == IDC_TRANSFORMATION_STRETCHANGLE_SPIN ||
        LOWORD(wParam) == IDC_TRANSFORMATION_STRETCHAMOUNT ||
        LOWORD(wParam) == IDC_TRANSFORMATION_STRETCHAMOUNT_SPIN)) ||
        wParam == IDOK)
    {
      double rotate = GetDlgItemFloat(hWnd, IDC_TRANSFORMATION_ROTATEANGLE) / deg;
      double stretch_angle = GetDlgItemFloat(hWnd, IDC_TRANSFORMATION_STRETCHANGLE) / deg;
      double stretch_factor = std::exp2(GetDlgItemFloat(hWnd, IDC_TRANSFORMATION_STRETCHAMOUNT) / 100);
      current_transformation = polar2(1, rotate, stretch_factor, stretch_angle);
      TransformRefresh(polar2(1, 0, 1, 0));
      HWND parent = GetParent(hWnd);
      HDC hDC = GetDC(parent);
      RECT r;
      GetClientRect(parent, &r);
      RECT sr;
      GetWindowRect(g_hwStatus, &sr);
      sr.bottom -= sr.top;
      r.bottom -= sr.bottom;
      TransformBlit(hDC, r.right, r.bottom);
      ReleaseDC(hWnd, hDC);
    }
    if (wParam == IDOK || wParam == IDCANCEL)
    {
      int retval = 0;
      if (wParam == IDOK)
      {
        g_SFT.UndoStore();
        g_bExamineDirty=TRUE;
        g_SFT.SetTransformMatrix(polar_composition(current_transformation) * polar_composition(original_transformation));
        PostMessage(GetParent(hWnd), WM_KEYDOWN, VK_F5, 0);
        retval = 1;
      }
      else
      {
        current_transformation = polar2(1, 0, 1, 0);
        TransformRefresh(polar2(1, 0, 1, 0));
        HWND parent = GetParent(hWnd);
        HDC hDC = GetDC(parent);
        RECT r;
        GetClientRect(parent, &r);
        RECT sr;
        GetWindowRect(g_hwStatus, &sr);
        sr.bottom -= sr.top;
        r.bottom -= sr.bottom;
        TransformBlit(hDC, r.right, r.bottom);
        ReleaseDC(hWnd, hDC);
      }
      for (auto tooltip : tooltips)
      {
        DestroyWindow(tooltip);
      }
      tooltips.clear();
      g_bTransformationDialogIsOpen = false;
      CheckMenuItem(GetMenu(GetParent(hWnd)), ID_TRANSFORMATION, MF_BYCOMMAND | MF_UNCHECKED);
      EndDialog(hWnd, retval);
    }
    if (wParam == IDC_TRANSFORMATION_AUTOSKEW)
    {
      if (g_transformation_still_running)
      {
        // signal stop and wait for thread to finish
        g_transformation_running = false;
        while (g_transformation_still_running)
        {
          MSG msg;
          while (PeekMessage(&msg, nullptr, 0, 0, PM_REMOVE))
          {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
          }
          Sleep(1);
        }
        SetDlgItemText(hWnd, IDC_TRANSFORMATION_AUTOSKEW, "Auto Skew");
      }
      else
      {
        // launch thread
        g_transformation_useddz = SendDlgItemMessage(hWnd, IDC_TRANSFORMATION_USEDDZ, BM_GETCHECK, 0, 0);
        g_transformation_running = true;
        g_transformation_still_running = true;
        DWORD dw;
        HANDLE hThread = CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)ThSkew,hWnd,0,&dw);
        CloseHandle(hThread);
        SetDlgItemText(hWnd, IDC_TRANSFORMATION_AUTOSKEW, "Stop");
      }
    }
  }
  else if (uMsg == WM_USER + 2)
  {
    if (lParam)
    {
      // autoskew success
      SetDlgItemText(hWnd, IDC_TRANSFORMATION_STATUS, (s_skew + "\nDone").c_str());
      current_transformation = polar_decomposition(mat2(g_skew[0], g_skew[1], g_skew[2], g_skew[3]) * glm::inverse(polar_composition(original_transformation)));
      TransformRefresh(polar2(1, 0, 1, 0));
      HWND parent = GetParent(hWnd);
      HDC hDC = GetDC(parent);
      RECT r;
      GetClientRect(parent, &r);
      RECT sr;
      GetWindowRect(g_hwStatus, &sr);
      sr.bottom -= sr.top;
      r.bottom -= sr.bottom;
      TransformBlit(hDC, r.right, r.bottom);
      ReleaseDC(hWnd, hDC);
    }
    else
    {
      SetDlgItemText(hWnd, IDC_TRANSFORMATION_STATUS, (s_skew + "\nFailed").c_str());
    }
    SetDlgItemText(hWnd, IDC_TRANSFORMATION_AUTOSKEW, "Auto Skew");
  }
  return 0;
}

extern void TransformApply(const polar2 &P)
{
  current_transformation = polar_decomposition(polar_composition(P) * polar_composition(current_transformation));
}

extern void TransformRefresh(const polar2 &P)
{
  refreshing = true;
  polar2 new_transformation = polar_decomposition(polar_composition(P) * polar_composition(current_transformation));
  SetDlgItemFloat(g_hwTransformationDialog, IDC_TRANSFORMATION_ROTATEANGLE, new_transformation.rotate * deg);
  SetDlgItemFloat(g_hwTransformationDialog, IDC_TRANSFORMATION_STRETCHANGLE, new_transformation.stretch_angle * deg);
  SetDlgItemFloat(g_hwTransformationDialog, IDC_TRANSFORMATION_STRETCHAMOUNT, std::log2(new_transformation.stretch_factor) * 100);
  refreshing = false;
}

extern void TransformImage(HBITMAP bmBkg, HBITMAP bmBkgDraw, POINT pm)
{
  double rotate = GetDlgItemFloat(g_hwTransformationDialog, IDC_TRANSFORMATION_ROTATEANGLE) / deg;
  double stretch_angle = GetDlgItemFloat(g_hwTransformationDialog, IDC_TRANSFORMATION_STRETCHANGLE) / deg;
  double stretch_factor = std::exp2(GetDlgItemFloat(g_hwTransformationDialog, IDC_TRANSFORMATION_STRETCHAMOUNT) / 100);
  mat2 m = polar_composition(polar2(1, rotate, stretch_factor, stretch_angle));

  HDC hDC = GetDC(NULL);
  BYTE *lpBits=NULL;
  BITMAPINFOHEADER bmi={sizeof(BITMAPINFOHEADER)};
  int row;
  GetDIBits(hDC,bmBkg,0,0,NULL,(LPBITMAPINFO)&bmi,DIB_RGB_COLORS);
  bmi.biCompression=bmi.biClrUsed=bmi.biClrImportant=0;
  bmi.biBitCount = 24;
  row = ((((bmi.biWidth*(DWORD)bmi.biBitCount)+31)&~31) >> 3);
  bmi.biSizeImage=row*bmi.biHeight;
  BYTE *lpOrgBits = new BYTE[bmi.biSizeImage];
  GetDIBits(hDC,bmBkg,0,bmi.biHeight,lpOrgBits,
      (LPBITMAPINFO)&bmi,DIB_RGB_COLORS);

  BITMAPINFOHEADER bmiDraw={sizeof(BITMAPINFOHEADER)};
  GetDIBits(hDC,bmBkgDraw,0,0,NULL,(LPBITMAPINFO)&bmiDraw,DIB_RGB_COLORS);
  bmiDraw.biCompression=bmiDraw.biClrUsed=bmiDraw.biClrImportant=0;
  bmiDraw.biBitCount = 24;
  int rowDraw = ((((bmiDraw.biWidth*(DWORD)bmiDraw.biBitCount)+31)&~31) >> 3);
  bmiDraw.biSizeImage=rowDraw*bmiDraw.biHeight;
  lpBits = new BYTE[bmiDraw.biSizeImage];
  memset(lpBits,50,bmiDraw.biSizeImage);

  POINT p1, p2;
  for(p1.y=0;p1.y<bmiDraw.biHeight;p1.y++){
    for(p1.x=0;p1.x<bmiDraw.biWidth;p1.x++){
      lpBits[p1.x*3 + (bmiDraw.biHeight-p1.y-1)*rowDraw] = lpOrgBits[0];
      lpBits[p1.x*3 + (bmiDraw.biHeight-p1.y-1)*rowDraw+1] = lpOrgBits[1];
      lpBits[p1.x*3 + (bmiDraw.biHeight-p1.y-1)*rowDraw+2] = lpOrgBits[2];

      p2 = p1;

      double u = p2.x - pm.x;
      double v = p2.y - pm.y;
      double x = m[0][0] * u + m[0][1] * v; // FIXME check transpose
      double y = m[1][0] * u + m[1][1] * v;
      double dx = x + pm.x;
      double dy = y + pm.y;

      if (dx < 0 || dy < 0)
        continue;
      p2.x = std::floor(dx);
      p2.y = std::floor(dy);
      if(p2.x<0 || p2.x>=bmi.biWidth || p2.y<0 || p2.y>=bmi.biHeight)
        continue;
      int R, Rx, Ry, Rz, G, Gx, Gy, Gz, B, Bx, By, Bz;
      double X, Xn, Y, Yn, Z, Zn;
      Xn = dx - (int)dx;
      X = 1 - Xn;
      Yn = dy - (int)dy;
      Y = 1 - Yn;
      Zn = (Xn+Yn)/2;
      Z = 1 - Zn;
      R = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row];
      G = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row + 1];
      B = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row + 2];
      p2.x++;
      if(p2.x<0 || p2.x>=bmi.biWidth || p2.y<0 || p2.y>=bmi.biHeight){
        Rx = R;
        Gx = G;
        Bx = B;
      }
      else{
        Rx = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row];
        Gx = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row + 1];
        Bx = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row + 2];
      }
      p2.x--;
      p2.y++;
      if(p2.x<0 || p2.x>=bmi.biWidth || p2.y<0 || p2.y>=bmi.biHeight){
        Ry = R;
        Gy = G;
        By = B;
      }
      else{
        Ry = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row];
        Gy = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row + 1];
        By = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row + 2];
      }
      p2.x++;
      if(p2.x<0 || p2.x>=bmi.biWidth || p2.y<0 || p2.y>=bmi.biHeight){
        Rz = R;
        Gz = G;
        Bz = B;
      }
      else{
        Rz = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row];
        Gz = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row + 1];
        Bz = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row + 2];
      }

      R = (int)(((double)R*X + (double)Rx*Xn + (double)R*Y + (double)Ry*Yn + (double)R*Z + (double)Rz*Zn)/3);
      G = (int)(((double)G*X + (double)Gx*Xn + (double)G*Y + (double)Gy*Yn + (double)G*Z + (double)Gz*Zn)/3);
      B = (int)(((double)B*X + (double)Bx*Xn + (double)B*Y + (double)By*Yn + (double)B*Z + (double)Bz*Zn)/3);
      lpBits[p1.x*3 + (bmiDraw.biHeight-p1.y-1)*rowDraw] = R;
      lpBits[p1.x*3 + (bmiDraw.biHeight-p1.y-1)*rowDraw+1] = G;
      lpBits[p1.x*3 + (bmiDraw.biHeight-p1.y-1)*rowDraw+2] = B;

//      memcpy(&lpBits[p1.x*3 + (bmiDraw.biHeight-p1.y-1)*rowDraw],&lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row],3);
    }
  }
  SetDIBits(hDC,bmBkgDraw,0,bmiDraw.biHeight,lpBits,
      (LPBITMAPINFO)&bmiDraw,DIB_RGB_COLORS);
  delete[] lpBits;
  delete[] lpOrgBits;
  ReleaseDC(NULL,hDC);
}

extern void TransformImage(HBITMAP bmBmp)
{
  HDC hDC = GetDC(NULL);
  BITMAP bm;
  GetObject(bmBmp,sizeof(BITMAP),&bm);
  HBITMAP bmNew = create_bitmap(hDC,bm.bmWidth,bm.bmHeight);
  POINT pm = {bm.bmWidth/2,bm.bmHeight/2};

  TransformImage(bmBmp, bmNew, pm);

  HDC dcNew = CreateCompatibleDC(hDC);
  HBITMAP bmOldNew = (HBITMAP)SelectObject(dcNew,bmNew);
  HDC dcBmp = CreateCompatibleDC(hDC);
  HBITMAP bmOldBmp = (HBITMAP)SelectObject(dcBmp,bmBmp);
  SetStretchBltMode(dcBmp,HALFTONE);

  RECT rc = {0,0,bm.bmWidth,bm.bmHeight};
  FillRect(dcBmp,&rc,(HBRUSH)GetStockObject(BLACK_BRUSH));
  StretchBlt(dcBmp,0,0,bm.bmWidth,bm.bmHeight,dcNew,0,0,bm.bmWidth,bm.bmHeight,SRCCOPY);
  SelectObject(dcNew,bmOldNew);
  SelectObject(dcBmp,bmOldBmp);
  DeleteDC(dcNew);
  DeleteDC(dcBmp);
  DeleteObject(bmNew);
  ReleaseDC(NULL,hDC);
}

extern void TransformBlit(HDC hDC, int w, int h)
{
  HBITMAP bmBmp = create_bitmap(hDC, g_SFT.GetWidth(), g_SFT.GetHeight());
  POINT pm = { g_SFT.GetWidth() / 2, g_SFT.GetHeight() / 2 };
  TransformImage(g_SFT.GetBitmap(), bmBmp, pm);
  HDC dcBmp = CreateCompatibleDC(hDC);
  HBITMAP bmOld = (HBITMAP) SelectObject(dcBmp, bmBmp);
  SetStretchBltMode(dcBmp, HALFTONE);
  SetStretchBltMode(hDC, HALFTONE);
  StretchBlt(hDC, 0, 0, w, h, dcBmp, 0, 0, g_SFT.GetWidth(), g_SFT.GetHeight(), SRCCOPY);
  SelectObject(dcBmp, bmOld);
  DeleteObject(dcBmp);
  DeleteObject(bmBmp);
}

#if 0
static void UnSkewImage(HBITMAP bmBmp)
{
  HDC hDC = GetDC(NULL);
  BITMAP bm;
  GetObject(bmBmp,sizeof(BITMAP),&bm);
  HBITMAP bmNew = create_bitmap(hDC,bm.bmWidth,bm.bmHeight);

  int nWidth = (10000/g_nSkewStretch)*bm.bmWidth/100;
  HDC dcNew = CreateCompatibleDC(hDC);
  HBITMAP bmOldNew = (HBITMAP)SelectObject(dcNew,bmNew);
  HDC dcBmp = CreateCompatibleDC(hDC);
  HBITMAP bmOldBmp = (HBITMAP)SelectObject(dcBmp,bmBmp);
  SetStretchBltMode(dcNew,HALFTONE);

  RECT rc = {0,0,bm.bmWidth,bm.bmHeight};
  FillRect(dcNew,&rc,(HBRUSH)GetStockObject(BLACK_BRUSH));
  StretchBlt(dcNew,(bm.bmWidth-nWidth)/2,0,nWidth,bm.bmHeight,dcBmp,0,0,bm.bmWidth,bm.bmHeight,SRCCOPY);
  SelectObject(dcNew,bmOldNew);
  SelectObject(dcBmp,bmOldBmp);
  DeleteDC(dcNew);
  DeleteDC(dcBmp);
  POINT pm = {bm.bmWidth/2,bm.bmHeight/2};
  double r=-pi*(double)g_nSkewRotate/180;
  RotateImage(bmNew,bmBmp,pm,r);

  DeleteObject(bmNew);
  ReleaseDC(NULL,hDC);
}
BOOL g_DialogInit=0;
POINT g_Cross[4];
int g_nCrossPos=0;
double g_nTestDegree;
double g_nTestRatio;
#endif
