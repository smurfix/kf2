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

static std::vector<HWND> tooltips;

extern INT_PTR WINAPI WindowSizeProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  (void) lParam;
  if (uMsg == WM_INITDIALOG)
  {
    SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
    SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));

#define T(idc,str) tooltips.push_back(CreateToolTip(idc, hWnd, str));
    T(IDC_SIZE_ASPECT_PRESET_1, "Set output aspect ratio to 1:2 (tall)")
    T(IDC_SIZE_ASPECT_PRESET_2, "Set output aspect ratio to 9:16")
    T(IDC_SIZE_ASPECT_PRESET_3, "Set output aspect ratio to 2:3")
    T(IDC_SIZE_ASPECT_PRESET_4, "Set output aspect ratio to 3:4")
    T(IDC_SIZE_ASPECT_PRESET_5, "Set output aspect ratio to 1:1 (square)")
    T(IDC_SIZE_ASPECT_PRESET_6, "Set output aspect ratio to 4:3")
    T(IDC_SIZE_ASPECT_PRESET_7, "Set output aspect ratio to 3:2")
    T(IDC_SIZE_ASPECT_PRESET_8, "Set output aspect ratio to 16:9")
    T(IDC_SIZE_ASPECT_PRESET_9, "Set output aspect ratio to 2:1 (wide)")
    T(IDC_SIZE_ASPECT_PRESET_CUSTOM, "Set custom aspect ratio")
    T(IDC_SIZE_ASPECT_PRESET_CUSTOM_N, "Set custom output aspect ratio numerator")
    T(IDC_SIZE_ASPECT_PRESET_CUSTOM_D, "Set custom output aspect ratio denominator")
    T(IDC_SIZE_TARGET_PRESET_1, "Set output target size height to 360p (small)")
    T(IDC_SIZE_TARGET_PRESET_2, "Set output target size height to 480p")
    T(IDC_SIZE_TARGET_PRESET_3, "Set output target size height to 576p")
    T(IDC_SIZE_TARGET_PRESET_4, "Set output target size height to 720p")
    T(IDC_SIZE_TARGET_PRESET_5, "Set output target size height to 1080p (HD)")
    T(IDC_SIZE_TARGET_PRESET_6, "Set output target size height to 1440p")
    T(IDC_SIZE_TARGET_PRESET_7, "Set output target size height to 2160p (4k)")
    T(IDC_SIZE_TARGET_PRESET_8, "Set output target size height to 2880p")
    T(IDC_SIZE_TARGET_PRESET_9, "Set output target size height to 4320p (8k)")
    T(IDC_SIZE_TARGET_PRESET_CUSTOM, "Set custom target size")
    T(IDC_SIZE_TARGET_PRESET_CUSTOM_H, "Set custom target size height in pixels")
    T(IDC_SIZE_SUPERS_PRESET_1, "Set super sampling to 1x1, 1 sample per pixel (low quality)")
    T(IDC_SIZE_SUPERS_PRESET_2, "Set super sampling to 2x2, 4 samples per pixel")
    T(IDC_SIZE_SUPERS_PRESET_3, "Set super sampling to 3x3, 9 samples per pixel")
    T(IDC_SIZE_SUPERS_PRESET_4, "Set super sampling to 4x4, 16 samples per pixel")
    T(IDC_SIZE_SUPERS_PRESET_5, "Set super sampling to 5x5, 25 samples per pixel")
    T(IDC_SIZE_SUPERS_PRESET_6, "Set super sampling to 6x6, 36 samples per pixel")
    T(IDC_SIZE_SUPERS_PRESET_7, "Set super sampling to 7x7, 49 samples per pixel")
    T(IDC_SIZE_SUPERS_PRESET_8, "Set super sampling to 8x8, 64 samples per pixel")
    T(IDC_SIZE_SUPERS_PRESET_9, "Set super sampling to 9x9, 81 samples per pixel (high quality)")
    T(IDC_SIZE_SUPERS_PRESET_CUSTOM, "Set custom super sampling")
    T(IDC_SIZE_SUPERS_PRESET_CUSTOM_S, "Set custom super sampling value (this number squared samples per pixel)")
    T(IDC_SIZE_WINDOW_PRESET_1, "Set window scale to 1/8 (smaller)")
    T(IDC_SIZE_WINDOW_PRESET_2, "Set window scale to 1/6")
    T(IDC_SIZE_WINDOW_PRESET_3, "Set window scale to 1/4")
    T(IDC_SIZE_WINDOW_PRESET_4, "Set window scale to 1/3")
    T(IDC_SIZE_WINDOW_PRESET_5, "Set window scale to 1/2")
    T(IDC_SIZE_WINDOW_PRESET_6, "Set window scale to 1 (target size)")
    T(IDC_SIZE_WINDOW_PRESET_7, "Set window scale to 2")
    T(IDC_SIZE_WINDOW_PRESET_8, "Set window scale to 3")
    T(IDC_SIZE_WINDOW_PRESET_9, "Set window scale to 4 (larger)")
    T(IDC_SIZE_WINDOW_PRESET_CUSTOM, "Set custom window scale")
    T(IDC_SIZE_WINDOW_PRESET_CUSTOM_N, "Set custom window scale numerator")
    T(IDC_SIZE_WINDOW_PRESET_CUSTOM_D, "Set custom window scale denominator")
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

    bool preset = false;
    preset |= (aspect_num == 1 && aspect_den == 2);
    preset |= (aspect_num == 9 && aspect_den == 16);
    preset |= (aspect_num == 2 && aspect_den == 3);
    preset |= (aspect_num == 3 && aspect_den == 4);
    preset |= (aspect_num == 1 && aspect_den == 1);
    preset |= (aspect_num == 4 && aspect_den == 3);
    preset |= (aspect_num == 3 && aspect_den == 2);
    preset |= (aspect_num == 16 && aspect_den == 9);
    preset |= (aspect_num == 2 && aspect_den == 1);
    SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_1, BM_SETCHECK, (aspect_num == 1 && aspect_den == 2), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_2, BM_SETCHECK, (aspect_num == 9 && aspect_den == 16), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_3, BM_SETCHECK, (aspect_num == 2 && aspect_den == 3), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_4, BM_SETCHECK, (aspect_num == 3 && aspect_den == 4), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_5, BM_SETCHECK, (aspect_num == 1 && aspect_den == 1), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_6, BM_SETCHECK, (aspect_num == 4 && aspect_den == 3), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_7, BM_SETCHECK, (aspect_num == 3 && aspect_den == 2), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_8, BM_SETCHECK, (aspect_num == 16 && aspect_den == 9), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_9, BM_SETCHECK, (aspect_num == 2 && aspect_den == 1), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_CUSTOM, BM_SETCHECK, ! preset, 0);
    SetDlgItemInt(hWnd, IDC_SIZE_ASPECT_PRESET_CUSTOM_N, aspect_num, 0);
    SetDlgItemInt(hWnd, IDC_SIZE_ASPECT_PRESET_CUSTOM_D, aspect_den, 0);

    preset = false;
    preset |= (target_height == 360);
    preset |= (target_height == 480);
    preset |= (target_height == 576);
    preset |= (target_height == 720);
    preset |= (target_height == 1080);
    preset |= (target_height == 1440);
    preset |= (target_height == 2160);
    preset |= (target_height == 2880);
    preset |= (target_height == 4320);
    SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_1, BM_SETCHECK, (target_height == 360), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_2, BM_SETCHECK, (target_height == 480), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_3, BM_SETCHECK, (target_height == 576), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_4, BM_SETCHECK, (target_height == 720), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_5, BM_SETCHECK, (target_height == 1080), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_6, BM_SETCHECK, (target_height == 1440), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_7, BM_SETCHECK, (target_height == 2160), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_8, BM_SETCHECK, (target_height == 2880), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_9, BM_SETCHECK, (target_height == 4320), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_CUSTOM, BM_SETCHECK, ! preset, 0);
    SetDlgItemInt(hWnd, IDC_SIZE_TARGET_PRESET_CUSTOM_H, target_height, 0);

    preset = false;
    preset |= (target_supersample == 1);
    preset |= (target_supersample == 2);
    preset |= (target_supersample == 3);
    preset |= (target_supersample == 4);
    preset |= (target_supersample == 5);
    preset |= (target_supersample == 6);
    preset |= (target_supersample == 7);
    preset |= (target_supersample == 8);
    preset |= (target_supersample == 9);
    SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_1, BM_SETCHECK, (target_supersample == 1), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_2, BM_SETCHECK, (target_supersample == 2), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_3, BM_SETCHECK, (target_supersample == 3), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_4, BM_SETCHECK, (target_supersample == 4), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_5, BM_SETCHECK, (target_supersample == 5), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_6, BM_SETCHECK, (target_supersample == 6), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_7, BM_SETCHECK, (target_supersample == 7), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_8, BM_SETCHECK, (target_supersample == 8), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_9, BM_SETCHECK, (target_supersample == 9), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_CUSTOM, BM_SETCHECK, ! preset, 0);
    SetDlgItemInt(hWnd, IDC_SIZE_SUPERS_PRESET_CUSTOM_S, target_supersample, 0);

    d = gcd(target_height, window_height);
    int64_t window_num = window_height / d;
    int64_t window_den = target_height / d;
    preset = false;
    preset |= (window_num == 1 && window_den == 8);
    preset |= (window_num == 1 && window_den == 6);
    preset |= (window_num == 1 && window_den == 4);
    preset |= (window_num == 1 && window_den == 3);
    preset |= (window_num == 1 && window_den == 2);
    preset |= (window_num == 1 && window_den == 1);
    preset |= (window_num == 2 && window_den == 1);
    preset |= (window_num == 3 && window_den == 1);
    preset |= (window_num == 4 && window_den == 1);
    SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_1, BM_SETCHECK, (window_num == 1 && window_den == 8), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_2, BM_SETCHECK, (window_num == 1 && window_den == 6), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_3, BM_SETCHECK, (window_num == 1 && window_den == 4), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_4, BM_SETCHECK, (window_num == 1 && window_den == 3), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_5, BM_SETCHECK, (window_num == 1 && window_den == 2), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_6, BM_SETCHECK, (window_num == 1 && window_den == 1), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_7, BM_SETCHECK, (window_num == 2 && window_den == 1), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_8, BM_SETCHECK, (window_num == 3 && window_den == 1), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_9, BM_SETCHECK, (window_num == 4 && window_den == 1), 0);
    SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_CUSTOM, BM_SETCHECK, ! preset, 0);
    SetDlgItemInt(hWnd, IDC_SIZE_WINDOW_PRESET_CUSTOM_N, window_num, 0);
    SetDlgItemInt(hWnd, IDC_SIZE_WINDOW_PRESET_CUSTOM_D, window_den, 0);

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

    return 1;
  }
  else if (uMsg == WM_COMMAND)
  {
    static bool refreshing = false;
    if (refreshing) return 0;
    refreshing = true;

    int aspect_num = 16, aspect_den = 9;
    if (SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_1, BM_GETCHECK, 0, 0)) { aspect_num = 1; aspect_den = 2; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_2, BM_GETCHECK, 0, 0)) { aspect_num = 9; aspect_den = 16; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_3, BM_GETCHECK, 0, 0)) { aspect_num = 2; aspect_den = 3; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_4, BM_GETCHECK, 0, 0)) { aspect_num = 3; aspect_den = 4; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_5, BM_GETCHECK, 0, 0)) { aspect_num = 1; aspect_den = 1; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_6, BM_GETCHECK, 0, 0)) { aspect_num = 4; aspect_den = 3; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_7, BM_GETCHECK, 0, 0)) { aspect_num = 3; aspect_den = 2; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_8, BM_GETCHECK, 0, 0)) { aspect_num = 16; aspect_den = 9; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_9, BM_GETCHECK, 0, 0)) { aspect_num = 2; aspect_den = 1; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_ASPECT_PRESET_CUSTOM, BM_GETCHECK, 0, 0))
    {
      aspect_num = std::max(int(GetDlgItemInt(hWnd, IDC_SIZE_ASPECT_PRESET_CUSTOM_N, 0, 0)), 1);
      aspect_den = std::max(int(GetDlgItemInt(hWnd, IDC_SIZE_ASPECT_PRESET_CUSTOM_D, 0, 0)), 1);
    }

    int64_t target_height = 360;
    if (SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_1, BM_GETCHECK, 0, 0)) { target_height = 360; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_2, BM_GETCHECK, 0, 0)) { target_height = 480; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_3, BM_GETCHECK, 0, 0)) { target_height = 576; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_4, BM_GETCHECK, 0, 0)) { target_height = 720; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_5, BM_GETCHECK, 0, 0)) { target_height = 1080; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_6, BM_GETCHECK, 0, 0)) { target_height = 1440; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_7, BM_GETCHECK, 0, 0)) { target_height = 2160; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_8, BM_GETCHECK, 0, 0)) { target_height = 2880; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_9, BM_GETCHECK, 0, 0)) { target_height = 4320; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_TARGET_PRESET_CUSTOM, BM_GETCHECK, 0, 0))
    {
      target_height = std::max(int(GetDlgItemInt(hWnd, IDC_SIZE_TARGET_PRESET_CUSTOM_H, 0, 0)), 1);
    }

    int64_t target_supersample = 1;
    if (SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_1, BM_GETCHECK, 0, 0)) { target_supersample = 1; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_2, BM_GETCHECK, 0, 0)) { target_supersample = 2; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_3, BM_GETCHECK, 0, 0)) { target_supersample = 3; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_4, BM_GETCHECK, 0, 0)) { target_supersample = 4; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_5, BM_GETCHECK, 0, 0)) { target_supersample = 5; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_6, BM_GETCHECK, 0, 0)) { target_supersample = 6; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_7, BM_GETCHECK, 0, 0)) { target_supersample = 7; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_8, BM_GETCHECK, 0, 0)) { target_supersample = 8; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_9, BM_GETCHECK, 0, 0)) { target_supersample = 9; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_SUPERS_PRESET_CUSTOM, BM_GETCHECK, 0, 0))
    {
      target_supersample = std::max(int(GetDlgItemInt(hWnd, IDC_SIZE_SUPERS_PRESET_CUSTOM_S, 0, 0)), 1);
    }

    int64_t window_num = 1, window_den = 1;
    if (SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_1, BM_GETCHECK, 0, 0)) { window_num = 1; window_den = 8; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_2, BM_GETCHECK, 0, 0)) { window_num = 1; window_den = 6; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_3, BM_GETCHECK, 0, 0)) { window_num = 1; window_den = 4; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_4, BM_GETCHECK, 0, 0)) { window_num = 1; window_den = 3; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_5, BM_GETCHECK, 0, 0)) { window_num = 1; window_den = 2; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_6, BM_GETCHECK, 0, 0)) { window_num = 1; window_den = 1; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_7, BM_GETCHECK, 0, 0)) { window_num = 2; window_den = 1; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_8, BM_GETCHECK, 0, 0)) { window_num = 3; window_den = 1; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_9, BM_GETCHECK, 0, 0)) { window_num = 4; window_den = 1; } else
    if (SendDlgItemMessage(hWnd, IDC_SIZE_WINDOW_PRESET_CUSTOM, BM_GETCHECK, 0, 0))
    {
      window_num = std::max(int(GetDlgItemInt(hWnd, IDC_SIZE_WINDOW_PRESET_CUSTOM_N, 0, 0)), 1);
      window_den = std::max(int(GetDlgItemInt(hWnd, IDC_SIZE_WINDOW_PRESET_CUSTOM_D, 0, 0)), 1);
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
    HWND okButton = GetDlgItem(hWnd, IDOK);
    if (okButton)
    {
      EnableWindow(okButton, ! bad_size);
    }

    refreshing = false;
    if (wParam == IDOK || wParam == IDCANCEL)
    {
      int retval = 0;
      if (wParam == IDOK)
      {
        g_SFT.UndoStore();
        g_SFT.SetTargetDimensions(target_width, target_height, target_supersample);
        g_SFT.SetWindowWidth(window_width);
        g_SFT.SetWindowHeight(window_height);
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
