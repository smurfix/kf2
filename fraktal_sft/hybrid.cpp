/*
Kalles Fraktaler 2
Copyright (C) 2020 Claude Heiland-Allen

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

#include <windows.h>

#include <cstdlib>
#include <iomanip>
#include <sstream>
#include <string>
#include <vector>

#include "fraktal_sft.h"
#include "hybrid.h"
#include "main.h"
#include "resource.h"
#include "tooltip.h"
#include "dual.h"

std::vector<std::string> split(std::string s, char sep)
{
  std::vector<std::string> r;
  while (true)
  {
    std::size_t ix = s.find(sep);
    r.push_back(s.substr(0, ix));
    if (ix == std::string::npos)
    {
      break;
    }
    else
    {
      s = s.substr(ix + 1);
    }
  }
  return r;
}

extern std::string to_string(const hybrid_operator &h)
{
  std::ostringstream o;
  o << (h.abs_x ? 1 : 0) << ',';
  o << (h.abs_y ? 1 : 0) << ',';
  o << (h.neg_x ? 1 : 0) << ',';
  o << (h.neg_y ? 1 : 0) << ',';
  o << h.pow << ',';
  o << std::setprecision(17) << h.mul_re << ',';
  o << std::setprecision(17) << h.mul_im;
  return o.str();
}

extern std::string to_string(const hybrid_combine &h)
{
  std::ostringstream o;
  o << int(h);
  return o.str();
}

extern std::string to_string(const hybrid_line &h)
{
  std::ostringstream o;
  o << to_string(h.one) << ';';
  o << to_string(h.two) << ';';
  o << to_string(h.mode);
  return o.str();
}

extern std::string to_string(const hybrid_stanza &h)
{
  std::ostringstream o;
  for (size_t i = 0; i < h.size(); ++i)
  {
    if (i)
    {
      o << '|';
    }
    o << to_string(h[i]);
  }
  return o.str();
}

extern std::string to_string(const hybrid_formula &h)
{
  std::ostringstream o;
  for (size_t i = 0; i < h.size(); ++i)
  {
    if (i) o << '/';
    o << to_string(h[i]);
  }
  return o.str();
}

extern hybrid_operator hybrid_operator_from_string(const std::string &s)
{
  hybrid_operator r = { false, false, false, false, 0, 0.0, 0.0 };
  std::vector<std::string> v = split(s, ',');
  if (v.size() > 0) r.abs_x = std::stoi(v[0]);
  if (v.size() > 1) r.abs_y = std::stoi(v[1]);
  if (v.size() > 2) r.neg_x = std::stoi(v[2]);
  if (v.size() > 3) r.neg_y = std::stoi(v[3]);
  if (v.size() > 4) r.pow = std::stoi(v[4]);
  if (v.size() > 5) r.mul_re = std::stof(v[5]);
  if (v.size() > 6) r.mul_im = std::stof(v[6]);
  return r;
}

extern hybrid_combine hybrid_combine_from_string(const std::string &s)
{
  hybrid_combine r = hybrid_combine_add;
  switch (std::stoi(s))
  {
    case 0: r = hybrid_combine_add; break;
    case 1: r = hybrid_combine_sub; break;
    case 2: r = hybrid_combine_mul; break;
    case 3: r = hybrid_combine_div; break;
  }
  return r;
}

extern hybrid_line hybrid_line_from_string(const std::string &s)
{
  hybrid_line r;
  std::vector<std::string> v = split(s, ';');
  if (v.size() > 0) r.one = hybrid_operator_from_string(v[0]);
  if (v.size() > 1) r.two = hybrid_operator_from_string(v[1]);
  if (v.size() > 2) r.mode = hybrid_combine_from_string(v[2]);
  return r;
}

extern hybrid_stanza hybrid_stanza_from_string(const std::string &s)
{
  hybrid_stanza r;
  std::vector<std::string> v = split(s, '|');
  for (auto l : v)
  {
    r.push_back(hybrid_line_from_string(l));
  }
  return r;
}

extern hybrid_formula hybrid_formula_from_string(const std::string &s)
{
  hybrid_formula r;
  std::vector<std::string> v = split(s, '/');
  for (auto l : v)
  {
    r.push_back(hybrid_stanza_from_string(l));
  }
  return r;
}

static std::vector<HWND> tooltips;

extern INT_PTR WINAPI HybridProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg)
  {
    case WM_INITDIALOG:
    {
      SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
      SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));

      // set widgets according to formula
      hybrid_formula h0 = g_SFT.GetHybridFormula();
      std::vector<hybrid_stanza> h = h0;
      bool a1 = h.size() > 0 && h[0].size() > 0;
      bool b1 = h.size() > 0 && h[0].size() > 1;
      bool a2 = h.size() > 1 && h[1].size() > 0;
      bool b2 = h.size() > 1 && h[1].size() > 1;
      bool a3 = h.size() > 2 && h[2].size() > 0;
      bool b3 = h.size() > 2 && h[2].size() > 1;
      bool a4 = h.size() > 3 && h[3].size() > 0;
      bool b4 = h.size() > 3 && h[3].size() > 1;
      hybrid_operator a11  = a1 ? h[0][0].one : (hybrid_operator){ false, false, false, false, 2, 1.0, 0.0 };
      hybrid_operator a12  = a1 ? h[0][0].two : (hybrid_operator){ false, false, false, false, 0, 0.0, 0.0 };
      hybrid_combine a1op = a1 ? h[0][0].mode : hybrid_combine_add;
      hybrid_operator b11  = b1 ? h[0][1].one : (hybrid_operator){ false, false, false, false, 2, 1.0, 0.0 };
      hybrid_operator b12  = b1 ? h[0][1].two : (hybrid_operator){ false, false, false, false, 0, 0.0, 0.0 };
      hybrid_combine b1op = b1 ? h[0][1].mode : hybrid_combine_add;
      hybrid_operator a21  = a2 ? h[1][0].one : (hybrid_operator){ false, false, false, false, 2, 1.0, 0.0 };
      hybrid_operator a22  = a2 ? h[1][0].two : (hybrid_operator){ false, false, false, false, 0, 0.0, 0.0 };
      hybrid_combine a2op = a2 ? h[1][0].mode : hybrid_combine_add;
      hybrid_operator b21  = b2 ? h[1][1].one : (hybrid_operator){ false, false, false, false, 2, 1.0, 0.0 };
      hybrid_operator b22  = b2 ? h[1][1].two : (hybrid_operator){ false, false, false, false, 0, 0.0, 0.0 };
      hybrid_combine b2op = b2 ? h[1][1].mode : hybrid_combine_add;
      hybrid_operator a31  = a3 ? h[2][0].one : (hybrid_operator){ false, false, false, false, 2, 1.0, 0.0 };
      hybrid_operator a32  = a3 ? h[2][0].two : (hybrid_operator){ false, false, false, false, 0, 0.0, 0.0 };
      hybrid_combine a3op = a3 ? h[2][0].mode : hybrid_combine_add;
      hybrid_operator b31  = b3 ? h[2][1].one : (hybrid_operator){ false, false, false, false, 2, 1.0, 0.0 };
      hybrid_operator b32  = b3 ? h[2][1].two : (hybrid_operator){ false, false, false, false, 0, 0.0, 0.0 };
      hybrid_combine b3op = b3 ? h[2][1].mode : hybrid_combine_add;
      hybrid_operator a41  = a4 ? h[3][0].one : (hybrid_operator){ false, false, false, false, 2, 1.0, 0.0 };
      hybrid_operator a42  = a4 ? h[3][0].two : (hybrid_operator){ false, false, false, false, 0, 0.0, 0.0 };
      hybrid_combine a4op = a4 ? h[3][0].mode : hybrid_combine_add;
      hybrid_operator b41  = b4 ? h[3][1].one : (hybrid_operator){ false, false, false, false, 2, 1.0, 0.0 };
      hybrid_operator b42  = b4 ? h[3][1].two : (hybrid_operator){ false, false, false, false, 0, 0.0, 0.0 };
      hybrid_combine b4op = b4 ? h[3][1].mode : hybrid_combine_add;
#define E(idc, enable) EnableWindow(GetDlgItem(hWnd, idc), true);
#define T(idc, str) tooltips.push_back(CreateToolTip(idc, hWnd, str));
#define B(idc, enable, value, str) T(idc, str) SendDlgItemMessage(hWnd, idc, BM_SETCHECK, value, 0); E(idc, enable)
#define N(idc, enable, value, str) T(idc, str) SetDlgItemInt(hWnd, idc, value, 0); E(idc, enable)
#define R(idc, enable, value, str) T(idc, str) SetDlgItemFloat(hWnd, idc, value); E(idc, enable)
#define O(idc, enable, value, str) \
  T(idc, str) \
  SendDlgItemMessage(hWnd, idc, CB_ADDSTRING, 0, (LPARAM) "+"); \
  SendDlgItemMessage(hWnd, idc, CB_ADDSTRING, 0, (LPARAM) "-"); \
  SendDlgItemMessage(hWnd, idc, CB_ADDSTRING, 0, (LPARAM) "*"); \
  SendDlgItemMessage(hWnd, idc, CB_ADDSTRING, 0, (LPARAM) "/"); \
  SendDlgItemMessage(hWnd, idc, CB_SETCURSEL, value, 0); \
  E(idc, enable)

      B(IDC_HYBRID_1_ACTIVE, true, h.size() > 0, "Group 1")
      B(IDC_HYBRID_1A_ABSX1, a1, a11.abs_x, "Abs X")
      B(IDC_HYBRID_1A_ABSY1, a1, a11.abs_y, "Abs Y")
      B(IDC_HYBRID_1A_NEGX1, a1, a11.neg_x, "Neg X")
      B(IDC_HYBRID_1A_NEGY1, a1, a11.neg_y, "Neg Y")
      N(IDC_HYBRID_1A_POW1, a1, a11.pow, "Power")
      R(IDC_HYBRID_1A_ARE1, a1, a11.mul_re, "A Real")
      R(IDC_HYBRID_1A_AIM1, a1, a11.mul_im, "A Imag")
      O(IDC_HYBRID_1A_OP, a1, a1op, "Operator")
      B(IDC_HYBRID_1A_ABSX2, a1, a12.abs_x, "Abs X")
      B(IDC_HYBRID_1A_ABSY2, a1, a12.abs_y, "Abs Y")
      B(IDC_HYBRID_1A_NEGX2, a1, a12.neg_x, "Neg X")
      B(IDC_HYBRID_1A_NEGY2, a1, a12.neg_y, "Neg Y")
      N(IDC_HYBRID_1A_POW2, a1, a12.pow, "Power")
      R(IDC_HYBRID_1A_ARE2, a1, a12.mul_re, "A Real")
      R(IDC_HYBRID_1A_AIM2, a1, a12.mul_im, "A Imag")
      B(IDC_HYBRID_1A_ACTIVE, true, a1, "Step 1")
      B(IDC_HYBRID_1B_ABSX1, b1, b11.abs_x, "Abs X")
      B(IDC_HYBRID_1B_ABSY1, b1, b11.abs_y, "Abs Y")
      B(IDC_HYBRID_1B_NEGX1, b1, b11.neg_x, "Neg X")
      B(IDC_HYBRID_1B_NEGY1, b1, b11.neg_y, "Neg Y")
      N(IDC_HYBRID_1B_POW1, b1, b11.pow, "Power")
      R(IDC_HYBRID_1B_ARE1, b1, b11.mul_re, "A Real")
      R(IDC_HYBRID_1B_AIM1, b1, b11.mul_im, "A Imag")
      O(IDC_HYBRID_1B_OP, b1, b1op, "Operator")
      B(IDC_HYBRID_1B_ABSX2, b1, b12.abs_x, "Abs X")
      B(IDC_HYBRID_1B_ABSY2, b1, b12.abs_y, "Abs Y")
      B(IDC_HYBRID_1B_NEGX2, b1, b12.neg_x, "Neg X")
      B(IDC_HYBRID_1B_NEGY2, b1, b12.neg_y, "Neg Y")
      N(IDC_HYBRID_1B_POW2, b1, b12.pow, "Power")
      R(IDC_HYBRID_1B_ARE2, b1, b12.mul_re, "A Real")
      R(IDC_HYBRID_1B_AIM2, b1, b12.mul_im, "A Imag")
      B(IDC_HYBRID_1B_ACTIVE, true, b1, "Step 2")

      B(IDC_HYBRID_2_ACTIVE, true, h.size() > 1, "Group 2")
      B(IDC_HYBRID_2A_ABSX1, a2, a21.abs_x, "Abs X")
      B(IDC_HYBRID_2A_ABSY1, a2, a21.abs_y, "Abs Y")
      B(IDC_HYBRID_2A_NEGX1, a2, a21.neg_x, "Neg X")
      B(IDC_HYBRID_2A_NEGY1, a2, a21.neg_y, "Neg Y")
      N(IDC_HYBRID_2A_POW1, a2, a21.pow, "Power")
      R(IDC_HYBRID_2A_ARE1, a2, a21.mul_re, "A Real")
      R(IDC_HYBRID_2A_AIM1, a2, a21.mul_im, "A Imag")
      O(IDC_HYBRID_2A_OP, a2, a2op, "Operator")
      B(IDC_HYBRID_2A_ABSX2, a2, a22.abs_x, "Abs X")
      B(IDC_HYBRID_2A_ABSY2, a2, a22.abs_y, "Abs Y")
      B(IDC_HYBRID_2A_NEGX2, a2, a22.neg_x, "Neg X")
      B(IDC_HYBRID_2A_NEGY2, a2, a22.neg_y, "Neg Y")
      N(IDC_HYBRID_2A_POW2, a2, a22.pow, "Power")
      R(IDC_HYBRID_2A_ARE2, a2, a22.mul_re, "A Real")
      R(IDC_HYBRID_2A_AIM2, a2, a22.mul_im, "A Imag")
      B(IDC_HYBRID_2A_ACTIVE, true, a2, "Step 1")
      B(IDC_HYBRID_2B_ABSX1, b2, b21.abs_x, "Abs X")
      B(IDC_HYBRID_2B_ABSY1, b2, b21.abs_y, "Abs Y")
      B(IDC_HYBRID_2B_NEGX1, b2, b21.neg_x, "Neg X")
      B(IDC_HYBRID_2B_NEGY1, b2, b21.neg_y, "Neg Y")
      N(IDC_HYBRID_2B_POW1, b2, b21.pow, "Power")
      R(IDC_HYBRID_2B_ARE1, b2, b21.mul_re, "A Real")
      R(IDC_HYBRID_2B_AIM1, b2, b21.mul_im, "A Imag")
      O(IDC_HYBRID_2B_OP, b2, b2op, "Operator")
      B(IDC_HYBRID_2B_ABSX2, b2, b22.abs_x, "Abs X")
      B(IDC_HYBRID_2B_ABSY2, b2, b22.abs_y, "Abs Y")
      B(IDC_HYBRID_2B_NEGX2, b2, b22.neg_x, "Neg X")
      B(IDC_HYBRID_2B_NEGY2, b2, b22.neg_y, "Neg Y")
      N(IDC_HYBRID_2B_POW2, b2, b22.pow, "Power")
      R(IDC_HYBRID_2B_ARE2, b2, b22.mul_re, "A Real")
      R(IDC_HYBRID_2B_AIM2, b2, b22.mul_im, "A Imag")
      B(IDC_HYBRID_2B_ACTIVE, true, b2, "Step 2")

      B(IDC_HYBRID_3_ACTIVE, true, h.size() > 2, "Group 3")
      B(IDC_HYBRID_3A_ABSX1, a3, a31.abs_x, "Abs X")
      B(IDC_HYBRID_3A_ABSY1, a3, a31.abs_y, "Abs Y")
      B(IDC_HYBRID_3A_NEGX1, a3, a31.neg_x, "Neg X")
      B(IDC_HYBRID_3A_NEGY1, a3, a31.neg_y, "Neg Y")
      N(IDC_HYBRID_3A_POW1, a3, a31.pow, "Power")
      R(IDC_HYBRID_3A_ARE1, a3, a31.mul_re, "A Real")
      R(IDC_HYBRID_3A_AIM1, a3, a31.mul_im, "A Imag")
      O(IDC_HYBRID_3A_OP, a3, a3op, "Operator")
      B(IDC_HYBRID_3A_ABSX2, a3, a32.abs_x, "Abs X")
      B(IDC_HYBRID_3A_ABSY2, a3, a32.abs_y, "Abs Y")
      B(IDC_HYBRID_3A_NEGX2, a3, a32.neg_x, "Neg X")
      B(IDC_HYBRID_3A_NEGY2, a3, a32.neg_y, "Neg Y")
      N(IDC_HYBRID_3A_POW2, a3, a32.pow, "Power")
      R(IDC_HYBRID_3A_ARE2, a3, a32.mul_re, "A Real")
      R(IDC_HYBRID_3A_AIM2, a3, a32.mul_im, "A Imag")
      B(IDC_HYBRID_3A_ACTIVE, true, a3, "Step 1")
      B(IDC_HYBRID_3B_ABSX1, b3, b31.abs_x, "Abs X")
      B(IDC_HYBRID_3B_ABSY1, b3, b31.abs_y, "Abs Y")
      B(IDC_HYBRID_3B_NEGX1, b3, b31.neg_x, "Neg X")
      B(IDC_HYBRID_3B_NEGY1, b3, b31.neg_y, "Neg Y")
      N(IDC_HYBRID_3B_POW1, b3, b31.pow, "Power")
      R(IDC_HYBRID_3B_ARE1, b3, b31.mul_re, "A Real")
      R(IDC_HYBRID_3B_AIM1, b3, b31.mul_im, "A Imag")
      O(IDC_HYBRID_3B_OP, b3, b3op, "Operator")
      B(IDC_HYBRID_3B_ABSX2, b3, b32.abs_x, "Abs X")
      B(IDC_HYBRID_3B_ABSY2, b3, b32.abs_y, "Abs Y")
      B(IDC_HYBRID_3B_NEGX2, b3, b32.neg_x, "Neg X")
      B(IDC_HYBRID_3B_NEGY2, b3, b32.neg_y, "Neg Y")
      N(IDC_HYBRID_3B_POW2, b3, b32.pow, "Power")
      R(IDC_HYBRID_3B_ARE2, b3, b32.mul_re, "A Real")
      R(IDC_HYBRID_3B_AIM2, b3, b32.mul_im, "A Imag")
      B(IDC_HYBRID_3B_ACTIVE, true, b3, "Step 2")

      B(IDC_HYBRID_4_ACTIVE, true, h.size() > 3, "Group 4")
      B(IDC_HYBRID_4A_ABSX1, a4, a41.abs_x, "Abs X")
      B(IDC_HYBRID_4A_ABSY1, a4, a41.abs_y, "Abs Y")
      B(IDC_HYBRID_4A_NEGX1, a4, a41.neg_x, "Neg X")
      B(IDC_HYBRID_4A_NEGY1, a4, a41.neg_y, "Neg Y")
      N(IDC_HYBRID_4A_POW1, a4, a41.pow, "Power")
      R(IDC_HYBRID_4A_ARE1, a4, a41.mul_re, "A Real")
      R(IDC_HYBRID_4A_AIM1, a4, a41.mul_im, "A Imag")
      O(IDC_HYBRID_4A_OP, a4, a4op, "Operator")
      B(IDC_HYBRID_4A_ABSX2, a4, a42.abs_x, "Abs X")
      B(IDC_HYBRID_4A_ABSY2, a4, a42.abs_y, "Abs Y")
      B(IDC_HYBRID_4A_NEGX2, a4, a42.neg_x, "Neg X")
      B(IDC_HYBRID_4A_NEGY2, a4, a42.neg_y, "Neg Y")
      N(IDC_HYBRID_4A_POW2, a4, a42.pow, "Power")
      R(IDC_HYBRID_4A_ARE2, a4, a42.mul_re, "A Real")
      R(IDC_HYBRID_4A_AIM2, a4, a42.mul_im, "A Imag")
      B(IDC_HYBRID_4A_ACTIVE, true, a4, "Step 1")
      B(IDC_HYBRID_4B_ABSX1, b4, b41.abs_x, "Abs X")
      B(IDC_HYBRID_4B_ABSY1, b4, b41.abs_y, "Abs Y")
      B(IDC_HYBRID_4B_NEGX1, b4, b41.neg_x, "Neg X")
      B(IDC_HYBRID_4B_NEGY1, b4, b41.neg_y, "Neg Y")
      N(IDC_HYBRID_4B_POW1, b4, b41.pow, "Power")
      R(IDC_HYBRID_4B_ARE1, b4, b41.mul_re, "A Real")
      R(IDC_HYBRID_4B_AIM1, b4, b41.mul_im, "A Imag")
      O(IDC_HYBRID_4B_OP, b4, b4op, "Operator")
      B(IDC_HYBRID_4B_ABSX2, b4, b42.abs_x, "Abs X")
      B(IDC_HYBRID_4B_ABSY2, b4, b42.abs_y, "Abs Y")
      B(IDC_HYBRID_4B_NEGX2, b4, b42.neg_x, "Neg X")
      B(IDC_HYBRID_4B_NEGY2, b4, b42.neg_y, "Neg Y")
      N(IDC_HYBRID_4B_POW2, b4, b42.pow, "Power")
      R(IDC_HYBRID_4B_ARE2, b4, b42.mul_re, "A Real")
      R(IDC_HYBRID_4B_AIM2, b4, b42.mul_im, "A Imag")
      B(IDC_HYBRID_4B_ACTIVE, true, b4, "Step 2")

#undef B
#undef N
#undef R
#undef O
#undef T
#undef E
      return 1;
    }
    break;
    case WM_COMMAND:
    {
      if (wParam == IDOK || wParam == IDCANCEL)
      {
        int retval = 0;
        if (wParam == IDOK)
        {
          g_SFT.UndoStore();
          g_bExamineDirty=TRUE;

          // copy formula from UI
#define B(idc, f) f = SendDlgItemMessage(hWnd, idc, BM_GETCHECK, 0, 0);
#define N(idc, f) f = GetDlgItemInt(hWnd, idc, 0, 0);
#define R(idc, f) f = GetDlgItemFloat(hWnd, idc);
#define O(idc, f) f = SendDlgItemMessage(hWnd, idc, CB_GETCURSEL, 0, 0);
          bool group1 = false;
          bool group2 = false;
          bool group3 = false;
          bool group4 = false;
          bool a1 = false;
          bool b1 = false;
          bool a2 = false;
          bool b2 = false;
          bool a3 = false;
          bool b3 = false;
          bool a4 = false;
          bool b4 = false;
          hybrid_operator a11 = { false, false, false, false, 0, 0.0, 0.0 };
          hybrid_operator a12 = { false, false, false, false, 0, 0.0, 0.0 };
          hybrid_operator b11 = { false, false, false, false, 0, 0.0, 0.0 };
          hybrid_operator b12 = { false, false, false, false, 0, 0.0, 0.0 };
          hybrid_operator a21 = { false, false, false, false, 0, 0.0, 0.0 };
          hybrid_operator a22 = { false, false, false, false, 0, 0.0, 0.0 };
          hybrid_operator b21 = { false, false, false, false, 0, 0.0, 0.0 };
          hybrid_operator b22 = { false, false, false, false, 0, 0.0, 0.0 };
          hybrid_operator a31 = { false, false, false, false, 0, 0.0, 0.0 };
          hybrid_operator a32 = { false, false, false, false, 0, 0.0, 0.0 };
          hybrid_operator b31 = { false, false, false, false, 0, 0.0, 0.0 };
          hybrid_operator b32 = { false, false, false, false, 0, 0.0, 0.0 };
          hybrid_operator a41 = { false, false, false, false, 0, 0.0, 0.0 };
          hybrid_operator a42 = { false, false, false, false, 0, 0.0, 0.0 };
          hybrid_operator b41 = { false, false, false, false, 0, 0.0, 0.0 };
          hybrid_operator b42 = { false, false, false, false, 0, 0.0, 0.0 };
          int a1op = 0;
          int b1op = 0;
          int a2op = 0;
          int b2op = 0;
          int a3op = 0;
          int b3op = 0;
          int a4op = 0;
          int b4op = 0;

          B(IDC_HYBRID_1_ACTIVE, group1)
          B(IDC_HYBRID_1A_ABSX1, a11.abs_x)
          B(IDC_HYBRID_1A_ABSY1, a11.abs_y)
          B(IDC_HYBRID_1A_NEGX1, a11.neg_x)
          B(IDC_HYBRID_1A_NEGY1, a11.neg_y)
          N(IDC_HYBRID_1A_POW1, a11.pow)
          R(IDC_HYBRID_1A_ARE1, a11.mul_re)
          R(IDC_HYBRID_1A_AIM1, a11.mul_im)
          O(IDC_HYBRID_1A_OP, a1op)
          B(IDC_HYBRID_1A_ABSX2, a12.abs_x)
          B(IDC_HYBRID_1A_ABSY2, a12.abs_y)
          B(IDC_HYBRID_1A_NEGX2, a12.neg_x)
          B(IDC_HYBRID_1A_NEGY2, a12.neg_y)
          N(IDC_HYBRID_1A_POW2, a12.pow)
          R(IDC_HYBRID_1A_ARE2, a12.mul_re)
          R(IDC_HYBRID_1A_AIM2, a12.mul_im)
          B(IDC_HYBRID_1A_ACTIVE, a1)
          B(IDC_HYBRID_1B_ABSX1, b11.abs_x)
          B(IDC_HYBRID_1B_ABSY1, b11.abs_y)
          B(IDC_HYBRID_1B_NEGX1, b11.neg_x)
          B(IDC_HYBRID_1B_NEGY1, b11.neg_y)
          N(IDC_HYBRID_1B_POW1, b11.pow)
          R(IDC_HYBRID_1B_ARE1, b11.mul_re)
          R(IDC_HYBRID_1B_AIM1, b11.mul_im)
          O(IDC_HYBRID_1B_OP, b1op)
          B(IDC_HYBRID_1B_ABSX2, b12.abs_x)
          B(IDC_HYBRID_1B_ABSY2, b12.abs_y)
          B(IDC_HYBRID_1B_NEGX2, b12.neg_x)
          B(IDC_HYBRID_1B_NEGY2, b12.neg_y)
          N(IDC_HYBRID_1B_POW2, b12.pow)
          R(IDC_HYBRID_1B_ARE2, b12.mul_re)
          R(IDC_HYBRID_1B_AIM2, b12.mul_im)
          B(IDC_HYBRID_1B_ACTIVE, b1)

          B(IDC_HYBRID_2_ACTIVE, group2)
          B(IDC_HYBRID_2A_ABSX1, a21.abs_x)
          B(IDC_HYBRID_2A_ABSY1, a21.abs_y)
          B(IDC_HYBRID_2A_NEGX1, a21.neg_x)
          B(IDC_HYBRID_2A_NEGY1, a21.neg_y)
          N(IDC_HYBRID_2A_POW1, a21.pow)
          R(IDC_HYBRID_2A_ARE1, a21.mul_re)
          R(IDC_HYBRID_2A_AIM1, a21.mul_im)
          O(IDC_HYBRID_2A_OP, a2op)
          B(IDC_HYBRID_2A_ABSX2, a22.abs_x)
          B(IDC_HYBRID_2A_ABSY2, a22.abs_y)
          B(IDC_HYBRID_2A_NEGX2, a22.neg_x)
          B(IDC_HYBRID_2A_NEGY2, a22.neg_y)
          N(IDC_HYBRID_2A_POW2, a22.pow)
          R(IDC_HYBRID_2A_ARE2, a22.mul_re)
          R(IDC_HYBRID_2A_AIM2, a22.mul_im)
          B(IDC_HYBRID_2A_ACTIVE, a2)
          B(IDC_HYBRID_2B_ABSX1, b21.abs_x)
          B(IDC_HYBRID_2B_ABSY1, b21.abs_y)
          B(IDC_HYBRID_2B_NEGX1, b21.neg_x)
          B(IDC_HYBRID_2B_NEGY1, b21.neg_y)
          N(IDC_HYBRID_2B_POW1, b21.pow)
          R(IDC_HYBRID_2B_ARE1, b21.mul_re)
          R(IDC_HYBRID_2B_AIM1, b21.mul_im)
          O(IDC_HYBRID_2B_OP, b2op)
          B(IDC_HYBRID_2B_ABSX2, b22.abs_x)
          B(IDC_HYBRID_2B_ABSY2, b22.abs_y)
          B(IDC_HYBRID_2B_NEGX2, b22.neg_x)
          B(IDC_HYBRID_2B_NEGY2, b22.neg_y)
          N(IDC_HYBRID_2B_POW2, b22.pow)
          R(IDC_HYBRID_2B_ARE2, b22.mul_re)
          R(IDC_HYBRID_2B_AIM2, b22.mul_im)
          B(IDC_HYBRID_2B_ACTIVE, b2)

          B(IDC_HYBRID_3_ACTIVE, group3)
          B(IDC_HYBRID_3A_ABSX1, a31.abs_x)
          B(IDC_HYBRID_3A_ABSY1, a31.abs_y)
          B(IDC_HYBRID_3A_NEGX1, a31.neg_x)
          B(IDC_HYBRID_3A_NEGY1, a31.neg_y)
          N(IDC_HYBRID_3A_POW1, a31.pow)
          R(IDC_HYBRID_3A_ARE1, a31.mul_re)
          R(IDC_HYBRID_3A_AIM1, a31.mul_im)
          O(IDC_HYBRID_3A_OP, a3op)
          B(IDC_HYBRID_3A_ABSX2, a32.abs_x)
          B(IDC_HYBRID_3A_ABSY2, a32.abs_y)
          B(IDC_HYBRID_3A_NEGX2, a32.neg_x)
          B(IDC_HYBRID_3A_NEGY2, a32.neg_y)
          N(IDC_HYBRID_3A_POW2, a32.pow)
          R(IDC_HYBRID_3A_ARE2, a32.mul_re)
          R(IDC_HYBRID_3A_AIM2, a32.mul_im)
          B(IDC_HYBRID_3A_ACTIVE, a3)
          B(IDC_HYBRID_3B_ABSX1, b31.abs_x)
          B(IDC_HYBRID_3B_ABSY1, b31.abs_y)
          B(IDC_HYBRID_3B_NEGX1, b31.neg_x)
          B(IDC_HYBRID_3B_NEGY1, b31.neg_y)
          N(IDC_HYBRID_3B_POW1, b31.pow)
          R(IDC_HYBRID_3B_ARE1, b31.mul_re)
          R(IDC_HYBRID_3B_AIM1, b31.mul_im)
          O(IDC_HYBRID_3B_OP, b3op)
          B(IDC_HYBRID_3B_ABSX2, b32.abs_x)
          B(IDC_HYBRID_3B_ABSY2, b32.abs_y)
          B(IDC_HYBRID_3B_NEGX2, b32.neg_x)
          B(IDC_HYBRID_3B_NEGY2, b32.neg_y)
          N(IDC_HYBRID_3B_POW2, b32.pow)
          R(IDC_HYBRID_3B_ARE2, b32.mul_re)
          R(IDC_HYBRID_3B_AIM2, b32.mul_im)
          B(IDC_HYBRID_3B_ACTIVE, b3)

          B(IDC_HYBRID_4_ACTIVE, group4)
          B(IDC_HYBRID_4A_ABSX1, a41.abs_x)
          B(IDC_HYBRID_4A_ABSY1, a41.abs_y)
          B(IDC_HYBRID_4A_NEGX1, a41.neg_x)
          B(IDC_HYBRID_4A_NEGY1, a41.neg_y)
          N(IDC_HYBRID_4A_POW1, a41.pow)
          R(IDC_HYBRID_4A_ARE1, a41.mul_re)
          R(IDC_HYBRID_4A_AIM1, a41.mul_im)
          O(IDC_HYBRID_4A_OP, a4op)
          B(IDC_HYBRID_4A_ABSX2, a42.abs_x)
          B(IDC_HYBRID_4A_ABSY2, a42.abs_y)
          B(IDC_HYBRID_4A_NEGX2, a42.neg_x)
          B(IDC_HYBRID_4A_NEGY2, a42.neg_y)
          N(IDC_HYBRID_4A_POW2, a42.pow)
          R(IDC_HYBRID_4A_ARE2, a42.mul_re)
          R(IDC_HYBRID_4A_AIM2, a42.mul_im)
          B(IDC_HYBRID_4A_ACTIVE, a4)
          B(IDC_HYBRID_4B_ABSX1, b41.abs_x)
          B(IDC_HYBRID_4B_ABSY1, b41.abs_y)
          B(IDC_HYBRID_4B_NEGX1, b41.neg_x)
          B(IDC_HYBRID_4B_NEGY1, b41.neg_y)
          N(IDC_HYBRID_4B_POW1, b41.pow)
          R(IDC_HYBRID_4B_ARE1, b41.mul_re)
          R(IDC_HYBRID_4B_AIM1, b41.mul_im)
          O(IDC_HYBRID_4B_OP, b4op)
          B(IDC_HYBRID_4B_ABSX2, b42.abs_x)
          B(IDC_HYBRID_4B_ABSY2, b42.abs_y)
          B(IDC_HYBRID_4B_NEGX2, b42.neg_x)
          B(IDC_HYBRID_4B_NEGY2, b42.neg_y)
          N(IDC_HYBRID_4B_POW2, b42.pow)
          R(IDC_HYBRID_4B_ARE2, b42.mul_re)
          R(IDC_HYBRID_4B_AIM2, b42.mul_im)
          B(IDC_HYBRID_4B_ACTIVE, b4)

#undef B
#undef R
#undef N
#undef O
          hybrid_formula h;

          if (group1)
          {
            hybrid_stanza s;
            if (a1)
            {
              hybrid_line l = { a11, a12, hybrid_combine(a1op) };
              s.push_back(l);
            }
            if (b1)
            {
              hybrid_line l = { b11, b12, hybrid_combine(b1op) };
              s.push_back(l);
            }
            if (s.size() > 0)
            {
              h.push_back(s);
            }
          }

          if (group2)
          {
            hybrid_stanza s;
            if (a2)
            {
              hybrid_line l = { a21, a22, hybrid_combine(a2op) };
              s.push_back(l);
            }
            if (b2)
            {
              hybrid_line l = { b21, b22, hybrid_combine(b2op) };
              s.push_back(l);
            }
            if (s.size() > 0)
            {
              h.push_back(s);
            }
          }

          if (group3)
          {
            hybrid_stanza s;
            if (a3)
            {
              hybrid_line l = { a31, a32, hybrid_combine(a3op) };
              s.push_back(l);
            }
            if (b3)
            {
              hybrid_line l = { b31, b32, hybrid_combine(b3op) };
              s.push_back(l);
            }
            if (s.size() > 0)
            {
              h.push_back(s);
            }
          }

          if (group4)
          {
            hybrid_stanza s;
            if (a4)
            {
              hybrid_line l = { a41, a42, hybrid_combine(a4op) };
              s.push_back(l);
            }
            if (b4)
            {
              hybrid_line l = { b41, b42, hybrid_combine(b4op) };
              s.push_back(l);
            }
            if (s.size() > 0)
            {
              h.push_back(s);
            }
          }

          retval = h.size() > 0;
          if (retval)
          {
            g_SFT.SetHybridFormula(h);
            g_SFT.SetUseHybridFormula(true);
          }
        }
        for (auto tooltip : tooltips)
        {
          DestroyWindow(tooltip);
        }
        tooltips.clear();
        EndDialog(hWnd, retval);
      }
    }
    break;
  }
  return 0;
}

extern bool hybrid_newton(const hybrid_formula &h, int maxsteps, int period, CDecNumber &cr0, CDecNumber &ci0, const CDecNumber &epsilon2, volatile int *running, int *progress)
{
  Precision prec(std::max(cr0.m_dec.precision(), ci0.m_dec.precision()));
  using N = int;
  using R = dual<2, CDecNumber>;
  using C = complex<R>;
  CDecNumber cr = cr0;
  CDecNumber ci = ci0;
  double lepsilon2 = double(log(epsilon2)); // FIXME slow? precision?
  double ldelta0 = 0;
  double ldelta1 = 0;
  int eta = 0;
  bool converged = false;
  for (N j = 0; j < maxsteps && *running && ! converged; ++j)
  {
    progress[0] = eta;
    progress[1] = j;
    progress[2] = period;
    progress[3] = 0;
    R cx(cr); cx.dx[0] = 1;
    R cy(ci); cy.dx[1] = 1;
    C c(cx, cy);
    C z(0, 0);
    // iteration
    for (N i = 0; i < period && *running; ++i)
    {
      progress[3] = i;
      z = hybrid_f(h[i % h.size()], z, c);
    }
    if (*running)
    {
      const CDecNumber &x = z.m_r.x;
      const CDecNumber &y = z.m_i.x;
      const CDecNumber &dxa = z.m_r.dx[0];
      const CDecNumber &dxb = z.m_r.dx[1];
      const CDecNumber &dya = z.m_i.dx[0];
      const CDecNumber &dyb = z.m_i.dx[1];
      // Newton step
      CDecNumber det = dxa * dyb - dxb * dya;
      CDecNumber u = -( dyb * x - dxb * y) / det;
      CDecNumber v = -(-dya * x + dxa * y) / det;
      cr = cr + u;
      ci = ci + v;
      // check convergence
      CDecNumber delta = u * u + v * v; // FIXME slow? precision?
      converged = delta < epsilon2;
      ldelta0 = ldelta1;
      ldelta1 = double(log(delta)); // FIXME slow? precision?
      eta = ceil(log2((lepsilon2 - ldelta0) / (ldelta1 - ldelta0)));
    }
  }
  if (converged)
  {
    cr0 = cr;
    ci0 = ci;
    return true;
  }
  return false;
}

extern int hybrid_period(const hybrid_formula &h, int N, const CDecNumber &A, const CDecNumber &B, const CDecNumber &S, const double *K, volatile int *running, int *progress)
{
  using R = dual<2, CDecNumber>;
  using C = complex<R>;
  R cx(A); cx.dx[0] = 1;
  R cy(B); cy.dx[1] = 1;
  C c(cx, cy);
  C z(0, 0);
  // K^{-1}
  double ai = K[0];
  double aj = K[1];
  double bi = K[2];
  double bj = K[3];
  double detK = ai * bj - aj * bi;
  double ai1 =  bj / detK;
  double aj1 = -bi / detK;
  double bi1 = -aj / detK;
  double bj1 =  ai / detK;
  double z2 = 0;
  double r2 = 1e50;
  bool p = true;
  int i = 0;
  while (i < N && z2 < r2 && p && *running)
  {
    progress[0] = N;
    progress[1] = i;
    // formula
    z = hybrid_f(h[i % h.size()], z, c);
    const CDecNumber &x = z.m_r.x;
    const CDecNumber &y = z.m_i.x;
    const CDecNumber &xa = z.m_r.dx[0];
    const CDecNumber &xb = z.m_r.dx[1];
    const CDecNumber &ya = z.m_i.dx[0];
    const CDecNumber &yb = z.m_i.dx[1];
    // J^{-1}
    const CDecNumber detJ = xa * yb - xb * ya;
    const CDecNumber xa1 =  yb / detJ;
    const CDecNumber xb1 = -ya / detJ;
    const CDecNumber ya1 = -xb / detJ;
    const CDecNumber yb1 =  xa / detJ;
    // (u0 v0) = J^{-1} * (x y)
    const CDecNumber u0 = xa1 * x + xb1 * y;
    const CDecNumber v0 = ya1 * x + yb1 * y;
    // (u1 v1) = s^{-1} K^{-1} * (u0 v0)
    const CDecNumber u1 = (ai1 * u0 + aj1 * v0) / S;
    const CDecNumber v1 = (bi1 * u0 + bj1 * v0) / S;
    double u2 = double(u1);
    double v2 = double(v1);
    double uv = u2 * u2 + v2 * v2;
    p = 1 <= uv;
    ++i;
    double xf = double(x);
    double yf = double(y);
    z2 = xf * xf + yf * yf;
  }
  if (i == N || r2 <= z2 || p || ! *running)
  {
    i = -1;
  }
  return i;
}

extern bool hybrid_size(const hybrid_formula &h, int period, const CDecNumber &A, const CDecNumber &B, CDecNumber &S, double *K, volatile int *running, int *progress)
{
  // compute average degree
  // the degree of each stanza is the *lowest* non-linear power
  double degree = 0;
  double count = 0;
  for (auto s : h)
  {
    double degs = 1;
    for (auto l : s)
    {
      double deg = 1.0/0.0;
      double deg1 = l.one.pow;
      double deg2 = l.two.pow;
      switch (l.mode)
      {
        case hybrid_combine_add:
        case hybrid_combine_sub:
          if (deg1 > 1 && (l.one.mul_re != 0 || l.one.mul_im != 0))
            deg = std::min(deg, deg1);
          if (deg2 > 1 && (l.two.mul_re != 0 || l.two.mul_im != 0))
            deg = std::min(deg, deg2);
          break;
        case hybrid_combine_mul:
          deg = deg1 + deg2;
          break;
        case hybrid_combine_div:
          deg = deg1 - deg2;
          break;
      }
      degs *= deg;
    }
    degree += log(degs);
    count += 1;
  }
  degree = exp(degree / count);
  double deg = degree / (degree - 1);
  if (isnan(deg) || isinf(deg)) deg = 0;
  using R = dual<2, CDecNumber>;
  using C = complex<R>;
  // FIXME check if this init and formula below is equivalent to the et version...
  R x(A); x.dx[0] = 1;
  R y(B); y.dx[1] = 1;
  C z(x, y);
  C c(A, B);
  CDecNumber bxa = 1;
  CDecNumber bxb = 0;
  CDecNumber bya = 0;
  CDecNumber byb = 1;
  int j = 1;
  while (j < period && *running)
  {
    progress[0] = period;
    progress[1] = j;
    // formula
    z = hybrid_f(h[j % h.size()], z, c);
    const CDecNumber &lxa = z.m_r.dx[0];
    const CDecNumber &lxb = z.m_r.dx[1];
    const CDecNumber &lya = z.m_i.dx[0];
    const CDecNumber &lyb = z.m_i.dx[1];
    // step b
    const CDecNumber det = lxa * lyb - lxb * lya;
    bxa = bxa + lyb / det;
    bxb = bxb - lxb / det;
    bya = bya - lya / det;
    byb = byb + lxa / det;
    ++j;
  }
  // l^d b
  if (*running)
  {
    const CDecNumber &lxa = z.m_r.dx[0];
    const CDecNumber &lxb = z.m_r.dx[1];
    const CDecNumber &lya = z.m_i.dx[0];
    const CDecNumber &lyb = z.m_i.dx[1];
    const CDecNumber l = sqrt(abs(lxa * lyb - lxb * lya));
    const CDecNumber beta = sqrt(abs(bxa * byb - bxb * bya));
    const CDecNumber llb = exp(log(l) * deg) * beta;
    S = 1 / llb;
    byb =   byb / beta;
    bxb = - bxb / beta;
    bya = - bya / beta;
    bxa =   bxa / beta;
    K[0] = double(byb);
    K[1] = double(bxb);
    K[2] = double(bya);
    K[3] = double(bxa);
    return true;
  }
  return false;
}
