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
#include "main_size.h"
#include "fraktal_sft.h"
#include "resource.h"
#include "tooltip.h"

static int64_t gcd(int64_t a, int64_t b)
{
  if (a < 0 || b < 0) return gcd(std::abs(a), std::abs(b));
  if (a == 0 || b == 0) return 1;
  if (a < b) return gcd(b, a);
  int64_t d = a % b;
  if (d == 0) return b;
  return gcd(b, d);
}

const struct { const char *name; int64_t num; int64_t den; } aspect_preset[] =
{ { "Custom", 0, 0 }
, { "768:85 (for exp map)", 768, 85 }
, { "9:1 (for exp map)", 9, 1 }
, { "2:1 (wide)", 2, 1 }
, { "16:9", 16, 9 }
, { "3:2", 3, 2 }
, { "7:5", 7, 5 }
, { "4:3", 4, 3 }
, { "1:1 (square)", 1, 1 }
, { "3:4", 3, 4 }
, { "5:7", 5, 7 }
, { "2:3", 2, 3 }
, { "9:16", 9, 16 }
, { "1:2 (tall)", 1, 2 }
};
const int naspect_presets = sizeof(aspect_preset) / sizeof(aspect_preset[0]);

const struct { const char *name; int64_t height; } target_preset[] =
{ { "Custom", 0 }
, { "85p (for exp map)", 85 }
, { "360p", 360 }
, { "480p (NTSC DVD)", 480 }
, { "576p (PAL DVD)", 576 }
, { "720p", 720 }
, { "1080p (HD)", 1080 }
, { "1440p", 1440 }
, { "2160p (4K)", 2160 }
, { "2880p", 2880 }
, { "4320p (8K)", 4320 }
};
const int ntarget_presets = sizeof(target_preset) / sizeof(target_preset[0]);

const struct { const char *name; int64_t supers; } supers_preset[] =
{ { "Custom", 0 }
, { "1x1 (1 sample, low quality)", 1 }
, { "2x2 (4 samples)", 2 }
, { "3x3 (9 samples)", 3 }
, { "4x4 (16 samples)", 4 }
, { "5x5 (25 samples)", 5 }
, { "6x6 (36 samples)", 6 }
, { "8x8 (64 samples)", 8 }
, { "12x12 (144 samples)", 12 }
, { "16x16 (256 samples)", 16 }
};
const int nsupers_presets = sizeof(supers_preset) / sizeof(supers_preset[0]);

const struct { const char *name; int64_t num; int64_t den; } window_preset[] =
{ { "Custom", 0, 0 }
, { "1/8 (smaller)", 1, 8 }
, { "1/6", 1, 6 }
, { "1/4", 1, 4 }
, { "1/3", 1, 3 }
, { "1/2", 1, 2 }
, { "1/1 (target size)", 1, 1 }
, { "2/1", 2, 1 }
, { "3/1", 3, 1 }
, { "4/1 (bigger)", 4, 1 }
};
const int nwindow_presets = sizeof(window_preset) / sizeof(window_preset[0]);

void UpdateDisplays(HWND hWnd, bool set)
{
  int preset = SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET, CB_GETCURSEL, 0, 0);
  int64_t aspect_num = aspect_preset[preset].num;
  int64_t aspect_den = aspect_preset[preset].den;
  if (preset == 0)
  {
    aspect_num = std::max(1, int(GetDlgItemInt(hWnd, IDC_SIZE_ASPECT_CUSTOM_N, 0, 0)));
    aspect_den = std::max(1, int(GetDlgItemInt(hWnd, IDC_SIZE_ASPECT_CUSTOM_D, 0, 0)));
  }
  preset = SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET, CB_GETCURSEL, 0, 0);
  int64_t target_height = target_preset[preset].height;
  if (preset == 0)
  {
    target_height = std::max(1, int(GetDlgItemInt(hWnd, IDC_SIZE_TARGET_CUSTOM, 0, 0)));
  }
  preset = SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET, CB_GETCURSEL, 0, 0);
  int64_t target_supersample = supers_preset[preset].supers;
  if (preset == 0)
  {
    target_supersample = std::max(1, int(GetDlgItemInt(hWnd, IDC_SIZE_SUPERS_PRESET, 0, 0)));
  }
  preset = SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET, CB_GETCURSEL, 0, 0);
  int64_t window_num = window_preset[preset].num;
  int64_t window_den = window_preset[preset].den;
  if (preset == 0)
  {
    window_num = std::max(1, int(GetDlgItemInt(hWnd, IDC_SIZE_WINDOW_CUSTOM_N, 0, 0)));
    window_den = std::max(1, int(GetDlgItemInt(hWnd, IDC_SIZE_WINDOW_CUSTOM_D, 0, 0)));
  }
  int64_t target_width = std::max(int64_t(std::round(target_height * aspect_num / (double) aspect_den)), 1LL);
  int64_t image_width = std::max(target_width * target_supersample, 1LL);
  int64_t image_height = std::max(target_height * target_supersample, 1LL);
  int64_t window_width = std::max(int64_t(std::round(target_width * window_num / (double) window_den)), 1LL);
  int64_t window_height = std::max(int64_t(std::round(target_height * window_num / (double) window_den)), 1LL);
  SetDlgItemInt(hWnd, IDC_SIZE_DISPLAY_TARGET_WIDTH, target_width, 0);
  SetDlgItemInt(hWnd, IDC_SIZE_DISPLAY_TARGET_HEIGHT, target_height, 0);
  SetDlgItemFloat(hWnd, IDC_SIZE_DISPLAY_TARGET_MPIXELS, (double) target_width * target_height / (1024 * 1024));
  SetDlgItemInt(hWnd, IDC_SIZE_DISPLAY_IMAGE_WIDTH, image_width, 0);
  SetDlgItemInt(hWnd, IDC_SIZE_DISPLAY_IMAGE_HEIGHT, image_height, 0);
  SetDlgItemFloat(hWnd, IDC_SIZE_DISPLAY_IMAGE_MPIXELS, (double) image_width * image_height / (1024 * 1024));
  SetDlgItemInt(hWnd, IDC_SIZE_DISPLAY_WINDOW_WIDTH, window_width, 0);
  SetDlgItemInt(hWnd, IDC_SIZE_DISPLAY_WINDOW_HEIGHT, window_height, 0);
  SetDlgItemFloat(hWnd, IDC_SIZE_DISPLAY_WINDOW_MPIXELS, (double) window_width * window_height / (1024 * 1024));
  bool bad_size =
    image_width <= 0 || image_width >= 65536 ||
    image_height <= 0 || image_height >= 65536 ||
    image_width * image_height * 3 >= (1LL << 31);
  EnableWindow(GetDlgItem(hWnd, IDOK), ! bad_size);
  if (set)
  {
    g_SFT.SetTargetDimensions(target_width, target_height, target_supersample);
    g_SFT.SetWindowWidth(window_width);
    g_SFT.SetWindowHeight(window_height);
  }
}

static std::vector<HWND> tooltips;

static bool refreshing = false;

extern INT_PTR WINAPI WindowSizeProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  (void) lParam;
  if (uMsg == WM_INITDIALOG)
  {
    refreshing = true;
    SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
    SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));

#define T(idc,str) tooltips.push_back(CreateToolTip(idc, hWnd, str));
    T(IDC_SIZE_ASPECT_PRESET, "Set output aspect ratio")
    T(IDC_SIZE_ASPECT_CUSTOM_N, "Set custom output aspect ratio numerator (width)")
    T(IDC_SIZE_ASPECT_CUSTOM_D, "Set custom output aspect ratio denominator (height)")
    T(IDC_SIZE_TARGET_PRESET, "Set output target size height")
    T(IDC_SIZE_TARGET_CUSTOM, "Set custom target size height in pixels")
    T(IDC_SIZE_SUPERS_PRESET, "Set super sampling")
    T(IDC_SIZE_SUPERS_CUSTOM, "Set custom super sampling value (this number squared samples per pixel)")
    T(IDC_SIZE_WINDOW_PRESET, "Set window scale")
    T(IDC_SIZE_WINDOW_CUSTOM_N, "Set custom window scale numerator")
    T(IDC_SIZE_WINDOW_CUSTOM_D, "Set custom window scale denominator")
    T(IDC_SIZE_DISPLAY_IMAGE_WIDTH, "Displays computed bitmap width")
    T(IDC_SIZE_DISPLAY_IMAGE_HEIGHT, "Displays computed bitmap height")
    T(IDC_SIZE_DISPLAY_IMAGE_MPIXELS, "Displays computed bitmap megapixels\nKF cannot handle bitmaps bigger than 666 MP\nzoomasm may be limited to 16 MP depending on hardware")
    T(IDC_SIZE_DISPLAY_TARGET_WIDTH, "Displays computed target width")
    T(IDC_SIZE_DISPLAY_TARGET_HEIGHT, "Displays computed target height")
    T(IDC_SIZE_DISPLAY_TARGET_MPIXELS, "Displays computed target megapixels")
    T(IDC_SIZE_DISPLAY_WINDOW_WIDTH, "Displays computed window width")
    T(IDC_SIZE_DISPLAY_WINDOW_HEIGHT, "Displays computed window height")
    T(IDC_SIZE_DISPLAY_WINDOW_MPIXELS, "Displays computed window megapixels")
    T(IDOK, "Apply and close")
    T(IDCANCEL, "Close and undo")
#undef T

    int64_t window_height = g_SFT.GetWindowHeight();
    int64_t target_width = 640, target_height = 360, target_supersample = 1;
    g_SFT.GetTargetDimensions(&target_width, &target_height, &target_supersample);
    int64_t d = gcd(target_width, target_height);
    int64_t aspect_num = target_width / d;
    int64_t aspect_den = target_height / d;

    int preset = 0;
    for (int p = 0; p < naspect_presets; ++p)
    {
      SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET, CB_ADDSTRING, 0, (LPARAM) aspect_preset[p].name);
      if (aspect_num == aspect_preset[p].num && aspect_den == aspect_preset[p].den)
      {
        preset = p;
      }
    }
    SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET, CB_SETCURSEL, preset, 0);
    SetDlgItemInt(hWnd, IDC_SIZE_ASPECT_CUSTOM_N, aspect_num, 0);
    SetDlgItemInt(hWnd, IDC_SIZE_ASPECT_CUSTOM_D, aspect_den, 0);
    EnableWindow(GetDlgItem(hWnd, IDC_SIZE_ASPECT_CUSTOM_N), preset == 0);
    EnableWindow(GetDlgItem(hWnd, IDC_SIZE_ASPECT_CUSTOM_D), preset == 0);

    preset = 0;
    for (int p = 0; p < ntarget_presets; ++p)
    {
      SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET, CB_ADDSTRING, 0, (LPARAM) target_preset[p].name);
      if (target_height == target_preset[p].height)
      {
        preset = p;
      }
    }
    SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET, CB_SETCURSEL, preset, 0);
    SetDlgItemInt(hWnd, IDC_SIZE_TARGET_CUSTOM, target_height, 0);
    EnableWindow(GetDlgItem(hWnd, IDC_SIZE_TARGET_CUSTOM), preset == 0);

    preset = 0;
    for (int p = 0; p < nsupers_presets; ++p)
    {
      SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET, CB_ADDSTRING, 0, (LPARAM) supers_preset[p].name);
      if (target_supersample == supers_preset[p].supers)
      {
        preset = p;
      }
    }
    SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET, CB_SETCURSEL, preset, 0);
    SetDlgItemInt(hWnd, IDC_SIZE_SUPERS_CUSTOM, target_supersample, 0);
    EnableWindow(GetDlgItem(hWnd, IDC_SIZE_SUPERS_CUSTOM), preset == 0);

    d = gcd(target_height, window_height);
    int64_t window_num = window_height / d;
    int64_t window_den = target_height / d;
    preset = 0;
    for (int p = 0; p < nwindow_presets; ++p)
    {
      SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET, CB_ADDSTRING, 0, (LPARAM) window_preset[p].name);
      if (window_num == window_preset[p].num && window_den == window_preset[p].den)
      {
        preset = p;
      }
    }
    SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET, CB_SETCURSEL, preset, 0);
    SetDlgItemInt(hWnd, IDC_SIZE_WINDOW_CUSTOM_N, window_num, 0);
    SetDlgItemInt(hWnd, IDC_SIZE_WINDOW_CUSTOM_D, window_den, 0);
    EnableWindow(GetDlgItem(hWnd, IDC_SIZE_WINDOW_CUSTOM_N), preset == 0);
    EnableWindow(GetDlgItem(hWnd, IDC_SIZE_WINDOW_CUSTOM_D), preset == 0);

    int64_t iw = target_width * target_supersample;
    int64_t ih = target_height * target_supersample;
    double imp = iw * ih / double(1024 * 1024);
    SetDlgItemInt(hWnd, IDC_SIZE_DISPLAY_IMAGE_WIDTH, iw, 0);
    SetDlgItemInt(hWnd, IDC_SIZE_DISPLAY_IMAGE_HEIGHT, ih, 0);
    SetDlgItemFloat(hWnd, IDC_SIZE_DISPLAY_IMAGE_MPIXELS, imp);

    int64_t ww = target_width * window_num / window_den;
    int64_t wh = target_height * window_num / window_den;
    double wmp = ww * wh / double(1024 * 1024);
    SetDlgItemInt(hWnd, IDC_SIZE_DISPLAY_WINDOW_WIDTH, ww, 0);
    SetDlgItemInt(hWnd, IDC_SIZE_DISPLAY_WINDOW_HEIGHT, wh, 0);
    SetDlgItemFloat(hWnd, IDC_SIZE_DISPLAY_WINDOW_MPIXELS, wmp);

    refreshing = false;
    return 1;
  }
  else if (HIWORD(wParam) == LBN_SELCHANGE && LOWORD(wParam) == IDC_SIZE_ASPECT_PRESET)
  {
    int preset = SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET, CB_GETCURSEL, 0, 0);
    EnableWindow(GetDlgItem(hWnd, IDC_SIZE_ASPECT_CUSTOM_N), preset == 0);
    EnableWindow(GetDlgItem(hWnd, IDC_SIZE_ASPECT_CUSTOM_D), preset == 0);
    UpdateDisplays(hWnd, false);
  }
  else if (HIWORD(wParam) == LBN_SELCHANGE && LOWORD(wParam) == IDC_SIZE_TARGET_PRESET)
  {
    int preset = SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET, CB_GETCURSEL, 0, 0);
    EnableWindow(GetDlgItem(hWnd, IDC_SIZE_TARGET_CUSTOM), preset == 0);
    UpdateDisplays(hWnd, false);
  }
  else if (HIWORD(wParam) == LBN_SELCHANGE && LOWORD(wParam) == IDC_SIZE_SUPERS_PRESET)
  {
    int preset = SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET, CB_GETCURSEL, 0, 0);
    EnableWindow(GetDlgItem(hWnd, IDC_SIZE_SUPERS_CUSTOM), preset == 0);
    UpdateDisplays(hWnd, false);
  }
  else if (HIWORD(wParam) == LBN_SELCHANGE && LOWORD(wParam) == IDC_SIZE_WINDOW_PRESET)
  {
    int preset = SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET, CB_GETCURSEL, 0, 0);
    EnableWindow(GetDlgItem(hWnd, IDC_SIZE_WINDOW_CUSTOM_N), preset == 0);
    EnableWindow(GetDlgItem(hWnd, IDC_SIZE_WINDOW_CUSTOM_D), preset == 0);
    UpdateDisplays(hWnd, false);
  }
  else if (uMsg == WM_COMMAND)
  {
    if (! refreshing)
    {
      refreshing = true;
      UpdateDisplays(hWnd, false);
      refreshing = false;
    }
    if (wParam == IDOK || wParam == IDCANCEL)
    {
      int retval = 0;
      if (wParam == IDOK)
      {
        g_SFT.UndoStore();
        if (! refreshing)
        {
          refreshing = true;
          UpdateDisplays(hWnd, true);
          refreshing = false;
        }
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
