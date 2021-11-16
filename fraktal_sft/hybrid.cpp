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
  o << h.repeats;
  for (size_t i = 0; i < h.lines.size(); ++i)
  {
    o << '|';
    o << to_string(h.lines[i]);
  }
  return o.str();
}

extern std::string to_string(const hybrid_formula &h)
{
  std::ostringstream o;
  o << h.loop_start;
  for (size_t i = 0; i < h.stanzas.size(); ++i)
  {
    o << '/';
    o << to_string(h.stanzas[i]);
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
  r.repeats = 0;
  if (v.size() > 0)
  {
    r.repeats = std::stoi(v[0]);
  }
  for (int l = 1; l < (ssize_t) v.size(); ++l)
  {
    r.lines.push_back(hybrid_line_from_string(v[l]));
  }
  return r;
}

extern hybrid_formula hybrid_formula_from_string(const std::string &s)
{
  hybrid_formula r;
  std::vector<std::string> v = split(s, '/');
  r.loop_start = 0;
  if (v.size() > 0)
  {
    r.loop_start = std::stoi(v[0]);
  }
  for (int l = 1; l < (ssize_t) v.size(); ++l)
  {
    r.stanzas.push_back(hybrid_stanza_from_string(v[l]));
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
      const hybrid_formula h = g_SFT.GetHybridFormula();
      bool loop_start_1 = h.loop_start == 0;
      bool loop_start_2 = h.loop_start == 1;
      bool loop_start_3 = h.loop_start == 2;
      bool loop_start_4 = h.loop_start == 3;
      bool a1 = h.stanzas.size() > 0 && h.stanzas[0].lines.size() > 0;
      bool b1 = h.stanzas.size() > 0 && h.stanzas[0].lines.size() > 1;
      bool a2 = h.stanzas.size() > 1 && h.stanzas[1].lines.size() > 0;
      bool b2 = h.stanzas.size() > 1 && h.stanzas[1].lines.size() > 1;
      bool a3 = h.stanzas.size() > 2 && h.stanzas[2].lines.size() > 0;
      bool b3 = h.stanzas.size() > 2 && h.stanzas[2].lines.size() > 1;
      bool a4 = h.stanzas.size() > 3 && h.stanzas[3].lines.size() > 0;
      bool b4 = h.stanzas.size() > 3 && h.stanzas[3].lines.size() > 1;
      hybrid_operator a11 = a1 ? h.stanzas[0].lines[0].one : (hybrid_operator){ false, false, false, false, 2, 1.0, 0.0 };
      hybrid_operator a12 = a1 ? h.stanzas[0].lines[0].two : (hybrid_operator){ false, false, false, false, 0, 0.0, 0.0 };
      hybrid_combine a1op = a1 ? h.stanzas[0].lines[0].mode : hybrid_combine_add;
      hybrid_operator b11 = b1 ? h.stanzas[0].lines[1].one : (hybrid_operator){ false, false, false, false, 2, 1.0, 0.0 };
      hybrid_operator b12 = b1 ? h.stanzas[0].lines[1].two : (hybrid_operator){ false, false, false, false, 0, 0.0, 0.0 };
      hybrid_combine b1op = b1 ? h.stanzas[0].lines[1].mode : hybrid_combine_add;
      hybrid_operator a21 = a2 ? h.stanzas[1].lines[0].one : (hybrid_operator){ false, false, false, false, 2, 1.0, 0.0 };
      hybrid_operator a22 = a2 ? h.stanzas[1].lines[0].two : (hybrid_operator){ false, false, false, false, 0, 0.0, 0.0 };
      hybrid_combine a2op = a2 ? h.stanzas[1].lines[0].mode : hybrid_combine_add;
      hybrid_operator b21 = b2 ? h.stanzas[1].lines[1].one : (hybrid_operator){ false, false, false, false, 2, 1.0, 0.0 };
      hybrid_operator b22 = b2 ? h.stanzas[1].lines[1].two : (hybrid_operator){ false, false, false, false, 0, 0.0, 0.0 };
      hybrid_combine b2op = b2 ? h.stanzas[1].lines[1].mode : hybrid_combine_add;
      hybrid_operator a31 = a3 ? h.stanzas[2].lines[0].one : (hybrid_operator){ false, false, false, false, 2, 1.0, 0.0 };
      hybrid_operator a32 = a3 ? h.stanzas[2].lines[0].two : (hybrid_operator){ false, false, false, false, 0, 0.0, 0.0 };
      hybrid_combine a3op = a3 ? h.stanzas[2].lines[0].mode : hybrid_combine_add;
      hybrid_operator b31 = b3 ? h.stanzas[2].lines[1].one : (hybrid_operator){ false, false, false, false, 2, 1.0, 0.0 };
      hybrid_operator b32 = b3 ? h.stanzas[2].lines[1].two : (hybrid_operator){ false, false, false, false, 0, 0.0, 0.0 };
      hybrid_combine b3op = b3 ? h.stanzas[2].lines[1].mode : hybrid_combine_add;
      hybrid_operator a41 = a4 ? h.stanzas[3].lines[0].one : (hybrid_operator){ false, false, false, false, 2, 1.0, 0.0 };
      hybrid_operator a42 = a4 ? h.stanzas[3].lines[0].two : (hybrid_operator){ false, false, false, false, 0, 0.0, 0.0 };
      hybrid_combine a4op = a4 ? h.stanzas[3].lines[0].mode : hybrid_combine_add;
      hybrid_operator b41 = b4 ? h.stanzas[3].lines[1].one : (hybrid_operator){ false, false, false, false, 2, 1.0, 0.0 };
      hybrid_operator b42 = b4 ? h.stanzas[3].lines[1].two : (hybrid_operator){ false, false, false, false, 0, 0.0, 0.0 };
      hybrid_combine b4op = b4 ? h.stanzas[3].lines[1].mode : hybrid_combine_add;
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

      N(IDC_HYBRID_1_ACTIVE, true, h.stanzas.size() > 0 ? h.stanzas[0].repeats : 1, "Repeats")
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
      B(IDC_HYBRID_1A_ACTIVE, true, true, "Step 1")
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

      N(IDC_HYBRID_2_ACTIVE, true, h.stanzas.size() > 1 ? h.stanzas[1].repeats : 0, "Repeats")
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
      B(IDC_HYBRID_2A_ACTIVE, true, true, "Step 1")
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

      N(IDC_HYBRID_3_ACTIVE, true, h.stanzas.size() > 2 ? h.stanzas[2].repeats : 0, "Repeats")
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
      B(IDC_HYBRID_3A_ACTIVE, true, true, "Step 1")
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

      N(IDC_HYBRID_4_ACTIVE, true, h.stanzas.size() > 3 ? h.stanzas[3].repeats : 0, "Repeats")
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
      B(IDC_HYBRID_4A_ACTIVE, true, true, "Step 1")
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

      B(IDC_HYBRID_LOOP_START_1, true, loop_start_1, "Start Loop")
      B(IDC_HYBRID_LOOP_START_2, true, loop_start_2, "Start Loop")
      B(IDC_HYBRID_LOOP_START_3, true, loop_start_3, "Start Loop")
      B(IDC_HYBRID_LOOP_START_4, true, loop_start_4, "Start Loop")
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
          bool loop_start_1 = false;
          bool loop_start_2 = false;
          bool loop_start_3 = false;
          bool loop_start_4 = false;
          int group1 = 0;
          int group2 = 0;
          int group3 = 0;
          int group4 = 0;
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

          N(IDC_HYBRID_1_ACTIVE, group1)
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

          N(IDC_HYBRID_2_ACTIVE, group2)
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

          N(IDC_HYBRID_3_ACTIVE, group3)
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

          N(IDC_HYBRID_4_ACTIVE, group4)
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

          B(IDC_HYBRID_LOOP_START_1, loop_start_1)
          B(IDC_HYBRID_LOOP_START_2, loop_start_2)
          B(IDC_HYBRID_LOOP_START_3, loop_start_3)
          B(IDC_HYBRID_LOOP_START_4, loop_start_4)
#undef B
#undef R
#undef N
#undef O
          hybrid_formula h;
          h.loop_start = loop_start_1 ? 0 : loop_start_2 ? 1 : loop_start_3 ? 2 : loop_start_4 ? 3 : 0;

          if (group1 > 0)
          {
            hybrid_stanza s;
            s.repeats = group1;
            if (a1)
            {
              hybrid_line l = { a11, a12, hybrid_combine(a1op) };
              s.lines.push_back(l);
            }
            if (b1)
            {
              hybrid_line l = { b11, b12, hybrid_combine(b1op) };
              s.lines.push_back(l);
            }
            if (s.lines.size() > 0)
            {
              h.stanzas.push_back(s);
            }
          }

          if (group2 > 0)
          {
            hybrid_stanza s;
            s.repeats = group2;
            if (a2)
            {
              hybrid_line l = { a21, a22, hybrid_combine(a2op) };
              s.lines.push_back(l);
            }
            if (b2)
            {
              hybrid_line l = { b21, b22, hybrid_combine(b2op) };
              s.lines.push_back(l);
            }
            if (s.lines.size() > 0)
            {
              h.stanzas.push_back(s);
            }
          }

          if (group3 > 0)
          {
            hybrid_stanza s;
            s.repeats = group3;
            if (a3)
            {
              hybrid_line l = { a31, a32, hybrid_combine(a3op) };
              s.lines.push_back(l);
            }
            if (b3)
            {
              hybrid_line l = { b31, b32, hybrid_combine(b3op) };
              s.lines.push_back(l);
            }
            if (s.lines.size() > 0)
            {
              h.stanzas.push_back(s);
            }
          }

          if (group4 > 0)
          {
            hybrid_stanza s;
            s.repeats = group4;
            if (a4)
            {
              hybrid_line l = { a41, a42, hybrid_combine(a4op) };
              s.lines.push_back(l);
            }
            if (b4)
            {
              hybrid_line l = { b41, b42, hybrid_combine(b4op) };
              s.lines.push_back(l);
            }
            if (s.lines.size() > 0)
            {
              h.stanzas.push_back(s);
            }
          }

          retval = valid(h);
          if (retval)
          {
            g_SFT.Stop();
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
    int count = 0;
    int stanza = 0;
    for (N i = 0; i < period && *running; ++i)
    {
      progress[3] = i;
      z = hybrid_f(h.stanzas[stanza], z, c);
      if (++count >= h.stanzas[stanza].repeats)
      {
        count = 0;
        if (++stanza >= (ssize_t) h.stanzas.size())
        {
          stanza = h.loop_start;
        }
      }
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

extern bool hybrid_skew(const hybrid_formula &h, int maxiters, const CDecNumber &cr, const CDecNumber &ci, bool useDZ, double *skew_matrix, volatile int *running, int *progress)
{
  Precision prec(std::max(cr.m_dec.precision(), ci.m_dec.precision()));
  using N = int;
  using R = dual<2, CDecNumber>;
  using C = complex<R>;
  int count = 0;
  int stanza = 0;
  // FIXME assumes seed is 0+0i and/or first iteration result is C
  R cx_dc(cr); cx_dc.dx[0] = 1;
  R cy_dc(ci); cy_dc.dx[1] = 1;
  C c_dc(cx_dc, cy_dc);
  C z_dc(c_dc);
  R cx_dz(cr);
  R cy_dz(ci);
  C c_dz(cx_dz, cy_dz);
  C z_dz(c_dc);
  for (N j = 0; j < maxiters && *running; ++j)
  {
    progress[0] = j;
    progress[1] = 0;
    progress[2] = 0;
    progress[3] = 0;
    if (++count >= h.stanzas[stanza].repeats)
    {
      count = 0;
      if (++stanza >= (ssize_t) h.stanzas.size())
      {
        stanza = h.loop_start;
      }
    }
    if (sqr(z_dc.m_r.x) + sqr(z_dc.m_i.x) > 65536.0)
    {
      break;
    }
    z_dc = hybrid_f(h.stanzas[stanza], z_dc, c_dc);
    if (useDZ)
    {
      z_dz = hybrid_f(h.stanzas[stanza], z_dz, c_dz);
    }
  }
  if (running)
  {
    const CDecNumber &dxa = z_dc.m_r.dx[0];
    const CDecNumber &dxb = z_dc.m_r.dx[1];
    const CDecNumber &dya = z_dc.m_i.dx[0];
    const CDecNumber &dyb = z_dc.m_i.dx[1];
    CDecNumber sii, sij, sji, sjj;
    if (useDZ)
    {
      const CDecNumber &dxx = z_dz.m_r.dx[0];
      const CDecNumber &dxy = z_dz.m_r.dx[1];
      const CDecNumber &dyx = z_dz.m_i.dx[0];
      const CDecNumber &dyy = z_dz.m_i.dx[1];
      sii = dyb * dxx - dxb * dyx;
      sij = dyb * dxy - dxb * dyy;
      sji = dxa * dyx - dya * dxx;
      sjj = dxa * dyy - dya * dxy;
    }
    else
    {
      sii =   dyb;
      sij = - dxb;
      sji = - dya;
      sjj =   dxa;
    }
    CDecNumber det = sqrt(abs(sii * sjj - sij * sji));
    sii = sii / det;
    sij = sij / det;
    sji = sji / det;
    sjj = sjj / det;
    skew_matrix[0] = double(sii);
    skew_matrix[1] = double(sij);
    skew_matrix[2] = double(sji);
    skew_matrix[3] = double(sjj);
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
  int count = 0;
  int stanza = 0;
  while (i < N && z2 < r2 && p && *running)
  {
    progress[0] = N;
    progress[1] = i;
    // formula
    z = hybrid_f(h.stanzas[stanza], z, c);
    if (++count >= (ssize_t) h.stanzas[stanza].repeats)
    {
      count = 0;
      if (++stanza >= (ssize_t) h.stanzas.size())
      {
        stanza = h.loop_start;
      }
    }
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
  double degree = 0;
  double ndegree = 0;
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
  int count = 0;
  int stanza = 0;
  while (j < period && *running)
  {
    progress[0] = period;
    progress[1] = j;
    // formula
    if (++count >= h.stanzas[stanza].repeats) // FIXME should this be after formula?
    {
      count = 0;
      if (++stanza >= (ssize_t) h.stanzas.size())
      {
        stanza = h.loop_start;
      }
    }
    z = hybrid_f(h.stanzas[stanza], z, c);
    // the degree of each stanza is the *lowest* non-linear power
    double degs = 1;
    for (auto l : h.stanzas[stanza].lines)
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
    ndegree += 1;
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
  degree = exp(degree / ndegree);
  double deg = degree / (degree - 1);
  if (isnan(deg) || isinf(deg)) deg = 0;
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

extern bool hybrid_domain_size(const hybrid_formula &h, int period, const CDecNumber &A, const CDecNumber &B, CDecNumber &S, volatile int *running, int *progress)
{
  using R = dual<2, CDecNumber>;
  using C = complex<R>;
  R x(A); x.dx[0] = 1;
  R y(B); y.dx[1] = 1;
  C c(x, y);
  C z(c);
  int j = 2;
  int count = 0;
  int stanza = 0;
  CDecNumber zq2 = sqr(z.m_r.x) + sqr(z.m_i.x);
  while (j <= period && *running)
  {
    progress[0] = period;
    progress[1] = j;
    // formula
    if (++count >= h.stanzas[stanza].repeats)
    {
      count = 0;
      if (++stanza >= (ssize_t) h.stanzas.size())
      {
        stanza = h.loop_start;
      }
    }
    z = hybrid_f(h.stanzas[stanza], z, c);
    // capture penultimate maximum |z|
    CDecNumber zp2 = sqr(z.m_r.x) + sqr(z.m_i.x);
    if (j < period && zp2 < zq2)
    {
      zq2 = zp2;
    }
    ++j;
  }
  if (*running)
  {
    const CDecNumber &lxa = z.m_r.dx[0];
    const CDecNumber &lxb = z.m_r.dx[1];
    const CDecNumber &lya = z.m_i.dx[0];
    const CDecNumber &lyb = z.m_i.dx[1];
    const CDecNumber det = lxa * lyb - lxb * lya;
    S = sqrt(zq2) / sqrt(abs(det));
    return true;
  }
  return false;
}

// double opencl

extern std::string hybrid_f_opencl_double(const hybrid_operator &h, const std::string &ret, const std::string &Z)
{
  std::ostringstream o;
  if (h.pow == 0)
  {
    o << ret << " = dc\n";
    o << "  ( " << std::scientific << std::setprecision(18) << h.mul_re << "\n";
    o << "  , " << std::scientific << std::setprecision(18) << h.mul_im << "\n";
    o << "  );\n";
    return o.str();
  }
  o << "{\n";
  o << "  dcomplex B = " << Z << ";\n";
  if (h.abs_x)
  {
  o << "  B.re = d_abs(B.re);\n";
  }
  if (h.abs_y)
  {
  o << "  B.im = d_abs(B.im);\n";
  }
  if (h.neg_x)
  {
  o << "  B.re = d_neg(B.re);\n";
  }
  if (h.neg_y)
  {
  o << "  B.im = d_neg(B.im);\n";
  }
  o << "  {";
  o << "    dcomplex M = { 1.0, 0.0 };\n";
  for (int i = 0; i < h.pow; ++i)
  {
  o << "    M = dc_mul(M, B);\n";
  }
  o << "    B = M;\n";
  o << "  }";
  if (h.mul_re == 1.0 && h.mul_im == 0.0)
  {
    o << "  " << ret << " = B;\n";
  }
  else
  {
    o << "  dcomplex za = {" << std::scientific << std::setprecision(18) << h.mul_re << ", " << std::scientific << std::setprecision(18) << h.mul_im << "};\n";
    o << "  " << ret << " = dc_mul(za, B);\n";
  }
  o << "}\n";
  return o.str();
}

extern std::string hybrid_f_opencl_double_dual(const hybrid_operator &h, const std::string &ret, const std::string &Z)
{
  std::ostringstream o;
  if (h.pow == 0)
  {
    o << ret << " = ddc\n";
    o << "  ( dd( " << std::scientific << std::setprecision(18) << h.mul_re << ", 0.0, 0.0 )\n";
    o << "  , dd( " << std::scientific << std::setprecision(18) << h.mul_im << ", 0.0, 0.0 )\n";
    o << "  );\n";
    return o.str();
  }
  o << "{\n";
  o << "  dualdcomplex B = " << Z << ";\n";
  if (h.abs_x)
  {
  o << "  B.re = duald_abs(B.re);\n";
  }
  if (h.abs_y)
  {
  o << "  B.im = duald_abs(B.im);\n";
  }
  if (h.neg_x)
  {
  o << "  B.re = duald_neg(B.re);\n";
  }
  if (h.neg_y)
  {
  o << "  B.im = duald_neg(B.im);\n";
  }
  o << "  {";
  o << "    dualdcomplex M = { { 1.0, { 0.0, 0.0 } }, { 0.0, { 0.0, 0.0 } } };\n";
  for (int i = 0; i < h.pow; ++i)
  {
  o << "    M = dualdc_mul(M, B);\n";
  }
  o << "    B = M;\n";
  o << "  }";
  if (h.mul_re == 1.0 && h.mul_im == 0.0)
  {
    o << "  " << ret << " = B;\n";
  }
  else
  {
    o << "  dcomplex za = {" << std::scientific << std::setprecision(18) << h.mul_re << ", " << std::scientific << std::setprecision(18) << h.mul_im << "};\n";
    o << "  " << ret << " = dualdc_dcmul(za, B);\n";
  }
  o << "}\n";
  return o.str();
}

extern std::string hybrid_pf_opencl_double(const hybrid_operator &h, const std::string &ret, const std::string &Z, const std::string &z)
{
  std::ostringstream o;
  if (h.pow == 0)
  {
    o << ret << " = dc(0.0, 0.0);\n";
    return o.str();
  }
  o << "{\n";
  o << "  mantissa zX = " << Z << ".re;\n";
  o << "  mantissa zY = " << Z << ".im;\n";
  o << "  mantissa zx = " << z << ".re;\n";
  o << "  mantissa zy = " << z << ".im;\n";
  o << "  dcomplex W = dc_add(" << Z << ", " << z << ");\n";
  o << "  dcomplex B = " << Z << ";\n";
  if (h.abs_x)
  {
  o << "  zx = d_diffabs(zX, zx);\n";
  o << "  W.re = d_abs(W.re);\n";
  o << "  B.re = d_abs(B.re);\n";
  }
  if (h.abs_y)
  {
  o << "  zy = d_diffabs(zY, zy);\n";
  o << "  W.im = d_abs(W.im);\n";
  o << "  B.im = d_abs(B.im);\n";
  }
  if (h.neg_x)
  {
  o << "  zx = d_neg(zx);\n";
  o << "  W.re = d_neg(W.re);\n";
  o << "  B.re = d_neg(B.re);\n";
  }
  if (h.neg_y)
  {
  o << "  zy = d_neg(zy);\n";
  o << "  W.im = d_neg(W.im);\n";
  o << "  B.im = d_neg(B.im);\n";
  }
  o << "  dcomplex P = { zx, zy };\n";
  o << "  dcomplex Wp[" << h.pow << "];\n";
  o << "  {";
  o << "    dcomplex M = { 1.0, 0.0 };\n";
  o << "    Wp[0] = M;\n";
  if (1 < h.pow)
  {
    o << "    Wp[1] = W;\n";
    o << "    M = W;\n";
    for (int i = 2; i < h.pow; ++i)
    {
      o << "    M = dc_mul(M, W);\n";
      o << "    Wp[" << i << "] = M;\n";
    }
  }
  o << "  }";
  o << "  dcomplex Bp[" << h.pow << "];\n";
  o << "  {";
  o << "    dcomplex M = { 1.0, 0.0 };\n";
  o << "    Bp[0] = M;\n";
  if (1 < h.pow)
  {
    o << "    Bp[1] = B;\n";
    o << "    M = B;\n";
    for (int i = 2; i < h.pow; ++i)
    {
      o << "    M = dc_mul(M, B);\n";
      o << "    Bp[" << i << "] = M;\n";
    }
  }
  o << "  }";
  if (h.pow == 1)
  {
    if (h.mul_re == 1.0 && h.mul_im == 0.0)
    {
      o << "  " << ret << " = P;\n";
    }
    else
    {
      o << "  dcomplex za = {" << std::scientific << std::setprecision(18) << h.mul_re << ", " << std::scientific << std::setprecision(18) << h.mul_im << "};\n";
      o << "  " << ret << " = dc_mul(za, P);\n";
    }
  }
  else
  {
    for (int i = 0; i <= h.pow - 1; ++i)
    {
      int j = h.pow - 1 - i;
      // S += pow(W, i) * pow(B, j);
      if (i == 0)
      {
        o << "  dcomplex S = Bp[" << j << "];\n";
      }
      else if (j == 0)
      {
        o << "  S = dc_add(S, Wp[" << i << "]);\n";
      }
      else
      {
        o << "  S = dc_add(S, dc_mul(Wp[" << i << "], " << "Bp[" << j << "]));\n";
      }
    }
    if (h.mul_re == 1.0 && h.mul_im == 0.0)
    {
      o << "  " << ret << " = dc_mul(P, S);\n";
    }
    else
    {
      o << "  dcomplex za = {" << std::scientific << std::setprecision(18) << h.mul_re << ", " << std::scientific << std::setprecision(18) << h.mul_im << "};\n";
      o << "  " << ret << " = dc_mul(za, dc_mul(P, S));\n";
    }
  }
  o << "}\n";
  return o.str();
}

extern std::string hybrid_pf_opencl_double_dual(const hybrid_operator &h, const std::string &ret, const std::string &Z, const std::string &z)
{
  std::ostringstream o;
  if (h.pow == 0)
  {
    o << ret << " = ddc(dd(0.0,0.0,0.0),dd(0.0,0.0,0.0))\n";
    return o.str();
  }
  o << "{\n";
  o << "  mantissa X = " << Z << ".re;\n";
  o << "  mantissa Y = " << Z << ".im;\n";
  o << "  duald x = " << z << ".re;\n";
  o << "  duald y = " << z << ".im;\n";
  o << "  dualdcomplex W = dualdc_dcadd(" << Z << ", " << z << ");\n";
  o << "  dcomplex B = " << Z << ";\n";
  if (h.abs_x)
  {
  o << "  x = duald_ddiffabs(X, x);\n";
  o << "  W.re = duald_abs(W.re);\n";
  o << "  B.re = d_abs(B.re);\n";
  }
  if (h.abs_y)
  {
  o << "  y = duald_ddiffabs(Y, y);\n";
  o << "  W.im = duald_abs(W.im);\n";
  o << "  B.im = d_abs(B.im);\n";
  }
  if (h.neg_x)
  {
  o << "  x = duald_neg(x);\n";
  o << "  W.re = duald_neg(W.re);\n";
  o << "  B.re = d_neg(B.re);\n";
  }
  if (h.neg_y)
  {
  o << "  y = duald_neg(y);\n";
  o << "  W.im = duald_neg(W.im);\n";
  o << "  B.im = d_neg(B.im);\n";
  }
  o << "  dualdcomplex P = { x, y };\n";
  o << "  dualdcomplex Wp[" << h.pow << "];\n";
  o << "  {";
  o << "    dualdcomplex M = { { 1.0, { 0.0, 0.0 } }, { 0.0, { 0.0, 0.0 } } };\n";
  o << "    Wp[0] = M;\n";
  if (1 < h.pow)
  {
    o << "    Wp[1] = W;\n";
    o << "    M = W;\n";
    for (int i = 2; i < h.pow; ++i)
    {
      o << "    M = dualdc_mul(M, W);\n";
      o << "    Wp[" << i << "] = M;\n";
    }
  }
  o << "  }";
  o << "  dcomplex Bp[" << h.pow << "];\n";
  o << "  {";
  o << "    dcomplex M = { 1.0, 0.0 };\n";
  o << "    Bp[0] = M;\n";
  if (1 < h.pow)
  {
    o << "    Bp[1] = B;\n";
    o << "    M = B;\n";
    for (int i = 2; i < h.pow; ++i)
    {
      o << "    M = dc_mul(M, B);\n";
      o << "    Bp[" << i << "] = M;\n";
    }
  }
  o << "  }";
  if (h.pow == 1)
  {
    if (h.mul_re == 1.0 && h.mul_im == 0.0)
    {
      o << "  " << ret << " = P;\n";
    }
    else
    {
      o << "  dcomplex a = {" << std::scientific << std::setprecision(18) << h.mul_re << ", " << std::scientific << std::setprecision(18) << h.mul_im << "};\n";
      o << "  " << ret << " = dualdc_dcmul(a, P);\n";
    }
  }
  else
  {
    for (int i = 0; i <= h.pow - 1; ++i)
    {
      int j = h.pow - 1 - i;
      // S += pow(W, j) * pow(B, i);
      if (i == 0)
      {
        o << "  dualdcomplex S = Wp[" << j << "];\n";
      }
      else if (j == 0)
      {
        o << "  S = dualdc_adddc(S, Bp[" << i << "]);\n";
      }
      else
      {
        o << "  S = dualdc_add(S, dualdc_muldc(Wp[" << j << "], " << "Bp[" << i << "]));\n";
      }
    }
    if (h.mul_re == 1.0 && h.mul_im == 0.0)
    {
      o << "  " << ret << " = dualdc_mul(P, S);\n";
    }
    else
    {
      o << "  dcomplex a = {" << std::scientific << std::setprecision(18) << h.mul_re << ", " << std::scientific << std::setprecision(18) << h.mul_im << "};\n";
      o << "  " << ret << " = dualdc_dcmul(a, dualdc_mul(P, S));\n";
    }
  }
  o << "}\n";
  return o.str();
}

std::string hybrid_f_opencl_double(const hybrid_line &h, const std::string &ret, const std::string &Z)
{
  std::ostringstream o;
  o << "{\n";
  o << "  dcomplex fone;\n";
  o << hybrid_f_opencl_double(h.one, "fone", Z);
  if (h.two.pow == 0 && h.two.mul_re == 0.0 && h.two.mul_im == 0.0 && (h.mode == hybrid_combine_add || h.mode == hybrid_combine_sub))
  {
    o << "  " << ret << " = fone;\n";
  o << "}\n";
    return o.str();
  }
  o << "  dcomplex ftwo;\n";
  o << hybrid_f_opencl_double(h.two, "ftwo", Z);
  switch (h.mode)
  {
    case hybrid_combine_add:
    {
      o << "  " << ret << " = dc_add(fone, ftwo);\n";
      break;
    }
    case hybrid_combine_sub:
    {
      o << "  " << ret << " = dc_sub(fone, ftwo);\n";
      break;
    }
    case hybrid_combine_mul:
    {
      o << "  " << ret << " = dc_mul(fone, ftwo);\n";
      break;
    }
    case hybrid_combine_div:
    {
      o << "  " << ret << " = dc_div(fone, ftwo);\n";
      break;
    }
  }
  o << "}\n";
  return o.str();
}

std::string hybrid_pf_opencl_double(const hybrid_line &h, const std::string &ret, const std::string &Z, const std::string &z)
{
  std::ostringstream o;
  o << "{\n";
  o << "  dcomplex pfone;\n";
  o << hybrid_pf_opencl_double(h.one, "pfone", Z, z);
  if (h.two.pow == 0 && h.two.mul_re == 0.0 && h.two.mul_im == 0.0 && (h.mode == hybrid_combine_add || h.mode == hybrid_combine_sub))
  {
    o << "  " << ret << " = pfone;\n";
  o << "}\n";
    return o.str();
  }
  o << "  dcomplex pftwo;\n";
  o << hybrid_pf_opencl_double(h.two, "pftwo", Z, z);
  switch (h.mode)
  {
    case hybrid_combine_add:
    {
      o << "  " << ret << " = dc_add(pfone, pftwo);\n";
      break;
    }
    case hybrid_combine_sub:
    {
      o << "  " << ret << " = dc_sub(pfone, pftwo);\n";
      break;
    }
    case hybrid_combine_mul:
    {
      o << "  dcomplex Zz = dc_add(" << Z << ", " << z << ");\n";
      o << "  dcomplex ftwo;\n";
      o << hybrid_f_opencl_double(h.two, "ftwo", "Zz");
      o << "  dcomplex fone;\n";
      o << hybrid_f_opencl_double(h.one, "fone", Z);
      o << "  " << ret << " = dc_add(dc_mul(pfone, ftwo), dc_mul(fone, pftwo));\n";
      break;
    }
  }
  o << "}\n";
  return o.str();
}

std::string hybrid_pf_opencl_double_dual(const hybrid_line &h, const std::string &ret, const std::string &Z, const std::string &z)
{
  std::ostringstream o;
  o << "{\n";
  o << "  dualdcomplex pfone;\n";
  o << hybrid_pf_opencl_double_dual(h.one, "pfone", Z, z);
  if (h.two.pow == 0 && h.two.mul_re == 0.0 && h.two.mul_im == 0.0 && (h.mode == hybrid_combine_add || h.mode == hybrid_combine_sub))
  {
    o << "  " << ret << " = pfone;\n";
  o << "}\n";
    return o.str();
  }
  o << "  dualdcomplex pftwo;\n";
  o << hybrid_pf_opencl_double_dual(h.two, "pftwo", Z, z);
  switch (h.mode)
  {
    case hybrid_combine_add:
    {
      o << "  " << ret << " = dualdc_add(pfone, pftwo);\n";
      break;
    }
    case hybrid_combine_sub:
    {
      o << "  " << ret << " = dualdc_sub(pfone, pftwo);\n";
      break;
    }
    case hybrid_combine_mul:
    {
      o << "  dualdcomplex Zz = dualdc_dcadd(" << Z << ", " << z << ");\n";
      o << "  dualdcomplex ftwo;\n";
      o << hybrid_f_opencl_double_dual(h.two, "ftwo", "Zz");
      o << "  dcomplex fone;\n";
      o << hybrid_f_opencl_double(h.one, "fone", Z);
      o << "  " << ret << " = dualdc_add(dualdc_mul(pfone, ftwo), dualdc_dcmul(fone, pftwo));\n";
      break;
    }
  }
  o << "}\n";
  return o.str();
}

std::string hybrid_pf_opencl_double(const hybrid_stanza &h, const std::string &ret, const std::string &Z, const std::string &z, const std::string &c)
{
  std::ostringstream o;
  o << "{\n";
  const int k = h.lines.size();
  for (int i = 0; i < k; ++i)
  {
    o << "  {\n";
    o << "    dcomplex znext;\n";
    o << hybrid_pf_opencl_double(h.lines[i], "znext", Z, z);
    o << "    " << z << " = znext;\n";
    o << "    dcomplex Znext;\n";
    o << hybrid_f_opencl_double(h.lines[i], "Znext", Z); // space vs work tradeoff; should be fine at low precision as there is no +C ?
    o << "    " << Z << " = Znext;\n";
    o << "  }\n";
  }
  o << "  " << ret << " = dc_add(" << z << ", " << c << ");\n";
  o << "}\n";
  return o.str();
}

std::string hybrid_pf_opencl_double_dual(const hybrid_stanza &h, const std::string &ret, const std::string &Z, const std::string &z, const std::string &c)
{
  std::ostringstream o;
  o << "{\n";
  const int k = h.lines.size();
  for (int i = 0; i < k; ++i)
  {
    o << "  {\n";
    o << "    dualdcomplex znext;\n";
    o << hybrid_pf_opencl_double_dual(h.lines[i], "znext", Z, z);
    o << "    " << z << " = znext;\n";
    o << "    dcomplex Znext;\n";
    o << hybrid_f_opencl_double(h.lines[i], "Znext", Z); // space vs work tradeoff; should be fine at low precision as there is no +C ?
    o << "    " << Z << " = Znext;\n";
    o << "  }\n";
  }
  o << "  " << ret << " = dualdc_add(" << z << ", " << c << ");\n";
  o << "}\n";
  return o.str();
}

extern std::string hybrid_perturbation_double_opencl(const hybrid_formula &h, bool derivatives)
{
  std::ostringstream o;
  o << "  {\n";
  o << "    if (++count >= g->hybrid_repeats[stanza])\n";
  o << "    {\n";
  o << "      count = 0;\n";
  o << "      if (++stanza >= (int) g->hybrid_nstanzas)\n";
  o << "      {\n";
  o << "        stanza = g->hybrid_loop_start;\n";
  o << "      }\n";
  o << "    }\n";
  o << "    l->log_m_nPower = g->hybrid_log_powers[stanza];\n";
  o << "    dcomplex Xd = { Xr, Xi };\n";
  if (derivatives)
  {
  o << "    dualdcomplex cd = { { cr, { 1.0, 0.0 } }, { ci, { 0.0, 1.0 } } };\n";
  o << "    dualdcomplex xd = { { xr, { dxa, dxb } }, { xi, { dya, dyb } } };\n";
  o << "    dualdcomplex xdn = { { 0.0, { 0.0, 0.0 } }, { 0.0, { 0.0, 0.0 } } };\n";
  }
  else
  {
  o << "    dcomplex cd = { cr, ci };\n";
  o << "    dcomplex xd = { xr, xi };\n";
  o << "    dcomplex xdn = { 0.0, 0.0 };\n";
  }
  o << "    switch (stanza)\n";
  o << "    {\n";
  for (int stanza = 0; stanza < (int) h.stanzas.size(); ++stanza)
  {
  o << "      case " << stanza << ":\n";
  o << "      {\n";
  if (derivatives)
  {
  o << hybrid_pf_opencl_double_dual(h.stanzas[stanza], "xdn", "Xd", "xd", "cd");
  }
  else
  {
  o << hybrid_pf_opencl_double(h.stanzas[stanza], "xdn", "Xd", "xd", "cd");
  }
  o << "        break;\n";
  o << "      }\n";
  }
  o << "    }\n";
  if (derivatives)
  {
    o << "    xrn = xdn.re.x;\n";
    o << "    xin = xdn.im.x;\n";
    o << "    dxan = xdn.re.dx[0];\n";
    o << "    dxbn = xdn.re.dx[1];\n";
    o << "    dyan = xdn.im.dx[0];\n";
    o << "    dybn = xdn.im.dx[1];\n";
  }
  else
  {
    o << "    xrn = xdn.re;\n";
    o << "    xin = xdn.im;\n";
  }
  o << "  }\n";
  return o.str();
}

// floatexp opencl

extern std::string hybrid_f_opencl_floatexp(const hybrid_operator &h, const std::string &ret, const std::string &Z)
{
  std::ostringstream o;
  if (h.pow == 0)
  {
    o << ret << " = fec\n";
    o << "  ( fe_floatexp(" << std::scientific << std::setprecision(18) << h.mul_re << ", 0)\n";
    o << "  , fe_floatexp(" << std::scientific << std::setprecision(18) << h.mul_im << ", 0)\n";
    o << "  );\n";
    return o.str();
  }
  o << "{\n";
  o << "  fecomplex B = " << Z << ";\n";
  if (h.abs_x)
  {
  o << "  B.re = fe_abs(B.re);\n";
  }
  if (h.abs_y)
  {
  o << "  B.im = fe_abs(B.im);\n";
  }
  if (h.neg_x)
  {
  o << "  B.re = fe_neg(B.re);\n";
  }
  if (h.neg_y)
  {
  o << "  B.im = fe_neg(B.im);\n";
  }
  o << "  {";
  o << "    fecomplex M = { one, zero };\n";
  for (int i = 0; i < h.pow; ++i)
  {
  o << "    M = fec_mul(M, B);\n";
  }
  o << "    B = M;\n";
  o << "  }";
  if (h.mul_re == 1.0 && h.mul_im == 0.0)
  {
    o << "  " << ret << " = B;\n";
  }
  else
  {
    o << "  dcomplex za =\n";
    o << "{ " << std::scientific << std::setprecision(18) << h.mul_re << "\n";
    o << ", " << std::scientific << std::setprecision(18) << h.mul_im << "\n";
    o << "};\n";
    o << "  " << ret << " = fec_dcmul(za, B);\n";
  }
  o << "}\n";
  return o.str();
}

extern std::string hybrid_f_opencl_floatexp_dual(const hybrid_operator &h, const std::string &ret, const std::string &Z)
{
  std::ostringstream o;
  if (h.pow == 0)
  {
    o << ret << " = dfec(\n";
    o << "  ( dfe( fe_floatexp(" << std::scientific << std::setprecision(18) << h.mul_re << ", 0), zero, zero )\n";
    o << "  , dfe( fe_floatexp(" << std::scientific << std::setprecision(18) << h.mul_im << ", 0), zero, zero )\n";
    o << "  );\n";
    return o.str();
  }
  o << "{\n";
  o << "  dualfecomplex B = " << Z << ";\n";
  if (h.abs_x)
  {
  o << "  B.re = dualfe_abs(B.re);\n";
  }
  if (h.abs_y)
  {
  o << "  B.im = dualfe_abs(B.im);\n";
  }
  if (h.neg_x)
  {
  o << "  B.re = dualfe_neg(B.re);\n";
  }
  if (h.neg_y)
  {
  o << "  B.im = dualfe_neg(B.im);\n";
  }
  o << "  {";
  o << "    dualfecomplex M = { { one, { zero, zero } }, { zero, { zero, zero } } };\n";
  for (int i = 0; i < h.pow; ++i)
  {
  o << "    M = dualfec_mul(M, B);\n";
  }
  o << "    B = M;\n";
  o << "  }";
  if (h.mul_re == 1.0 && h.mul_im == 0.0)
  {
    o << "  " << ret << " = B;\n";
  }
  else
  {
    o << "  dcomplex za =\n";
    o << "{ " << std::scientific << std::setprecision(18) << h.mul_re << "\n";
    o << ", " << std::scientific << std::setprecision(18) << h.mul_im << "\n";
    o << "};\n";
    o << "  " << ret << " = dualfec_dcmul(za, B);\n";
  }
  o << "}\n";
  return o.str();
}

extern std::string hybrid_pf_opencl_floatexp(const hybrid_operator &h, const std::string &ret, const std::string &Z, const std::string &z)
{
  std::ostringstream o;
  if (h.pow == 0)
  {
    o << ret << " = fec(zero, zero);\n";
    return o.str();
  }
  o << "{\n";
  o << "  floatexp zX = " << Z << ".re;\n";
  o << "  floatexp zY = " << Z << ".im;\n";
  o << "  floatexp zx = " << z << ".re;\n";
  o << "  floatexp zy = " << z << ".im;\n";
  o << "  fecomplex W = fec_add(" << Z << ", " << z << ");\n";
  o << "  fecomplex B = " << Z << ";\n";
  if (h.abs_x)
  {
  o << "  zx = fe_diffabs(zX, zx);\n";
  o << "  W.re = fe_abs(W.re);\n";
  o << "  B.re = fe_abs(B.re);\n";
  }
  if (h.abs_y)
  {
  o << "  zy = fe_diffabs(zY, zy);\n";
  o << "  W.im = fe_abs(W.im);\n";
  o << "  B.im = fe_abs(B.im);\n";
  }
  if (h.neg_x)
  {
  o << "  zx = fe_neg(zx);\n";
  o << "  W.re = fe_neg(W.re);\n";
  o << "  B.re = fe_neg(B.re);\n";
  }
  if (h.neg_y)
  {
  o << "  zy = fe_neg(zy);\n";
  o << "  W.im = fe_neg(W.im);\n";
  o << "  B.im = fe_neg(B.im);\n";
  }
  o << "  fecomplex P = { zx, zy };\n";
  o << "  fecomplex Wp[" << h.pow << "];\n";
  o << "  {";
  o << "    fecomplex M = { one, zero };\n";
  o << "    Wp[0] = M;\n";
  if (1 < h.pow)
  {
    o << "    Wp[1] = W;\n";
    o << "    M = W;\n";
    for (int i = 2; i < h.pow; ++i)
    {
      o << "    M = fec_mul(M, W);\n";
      o << "    Wp[" << i << "] = M;\n";
    }
  }
  o << "  }";
  o << "  fecomplex Bp[" << h.pow << "];\n";
  o << "  {";
  o << "    fecomplex M = { one, zero };\n";
  o << "    Bp[0] = M;\n";
  if (1 < h.pow)
  {
    o << "    Bp[1] = B;\n";
    o << "    M = B;\n";
    for (int i = 2; i < h.pow; ++i)
    {
      o << "    M = fec_mul(M, B);\n";
      o << "    Bp[" << i << "] = M;\n";
    }
  }
  o << "  }";
  if (h.pow == 1)
  {
    if (h.mul_re == 1.0 && h.mul_im == 0.0)
    {
      o << "  " << ret << " = P;\n";
    }
    else
    {
      o << "  dcomplex za = {" << std::scientific << std::setprecision(18) << h.mul_re << ", " << std::scientific << std::setprecision(18) << h.mul_im << "};\n";
      o << "  " << ret << " = fec_dcmul(za, P);\n";
    }
  }
  else
  {
    for (int i = 0; i <= h.pow - 1; ++i)
    {
      int j = h.pow - 1 - i;
      // S += pow(W, i) * pow(B, j);
      if (i == 0)
      {
        o << "  fecomplex S = Bp[" << j << "];\n";
      }
      else if (j == 0)
      {
        o << "  S = fec_add(S, Wp[" << i << "]);\n";
      }
      else
      {
        o << "  S = fec_add(S, fec_mul(Wp[" << i << "], " << "Bp[" << j << "]));\n";
      }
    }
    if (h.mul_re == 1.0 && h.mul_im == 0.0)
    {
      o << "  " << ret << " = fec_mul(P, S);\n";
    }
    else
    {
      o << "  dcomplex za = {" << std::scientific << std::setprecision(18) << h.mul_re << ", " << std::scientific << std::setprecision(18) << h.mul_im << "};\n";
      o << "  " << ret << " = fec_dcmul(za, fec_mul(P, S));\n";
    }
  }
  o << "}\n";
  return o.str();
}

extern std::string hybrid_pf_opencl_floatexp_dual(const hybrid_operator &h, const std::string &ret, const std::string &Z, const std::string &z)
{
  std::ostringstream o;
  if (h.pow == 0)
  {
    o << ret << " = dfec\n";
    o << "  ( dfe( zero, zero, zero )\n";
    o << "  , dfe( zero, zero, zero )\n";
    o << "  );\n";
    return o.str();
  }
  o << "{\n";
  o << "  floatexp X = " << Z << ".re;\n";
  o << "  floatexp Y = " << Z << ".im;\n";
  o << "  dualfe x = " << z << ".re;\n";
  o << "  dualfe y = " << z << ".im;\n";
  o << "  dualfecomplex W = dualfec_fecadd(" << Z << ", " << z << ");\n";
  o << "  fecomplex B = " << Z << ";\n";
  if (h.abs_x)
  {
  o << "  x = dualfe_fediffabs(X, x);\n";
  o << "  W.re = dualfe_abs(W.re);\n";
  o << "  B.re = fe_abs(B.re);\n";
  }
  if (h.abs_y)
  {
  o << "  y = dualfe_fediffabs(Y, y);\n";
  o << "  W.im = dualfe_abs(W.im);\n";
  o << "  B.im = fe_abs(B.im);\n";
  }
  if (h.neg_x)
  {
  o << "  x = dualfe_neg(x);\n";
  o << "  W.re = dualfe_neg(W.re);\n";
  o << "  B.re = fe_neg(B.re);\n";
  }
  if (h.neg_y)
  {
  o << "  y = dualfe_neg(y);\n";
  o << "  W.im = dualfe_neg(W.im);\n";
  o << "  B.im = fe_neg(B.im);\n";
  }
  o << "  dualfecomplex P = { x, y };\n";
  o << "  dualfecomplex Wp[" << h.pow << "];\n";
  o << "  {";
  o << "    dualfecomplex M = { { one, { zero, zero } }, { zero, { zero, zero } } };\n";
  o << "    Wp[0] = M;\n";
  if (1 < h.pow)
  {
    o << "    Wp[1] = W;\n";
    o << "    M = W;\n";
    for (int i = 2; i < h.pow; ++i)
    {
      o << "    M = dualfec_mul(M, W);\n";
      o << "    Wp[" << i << "] = M;\n";
    }
  }
  o << "  }";
  o << "  fecomplex Bp[" << h.pow << "];\n";
  o << "  {";
  o << "    fecomplex M = { one, zero };\n";
  o << "    Bp[0] = M;\n";
  if (1 < h.pow)
  {
    o << "    Bp[1] = B;\n";
    o << "    M = B;\n";
    for (int i = 2; i < h.pow; ++i)
    {
      o << "    M = fec_mul(M, B);\n";
      o << "    Bp[" << i << "] = M;\n";
    }
  }
  o << "  }";
  if (h.pow == 1)
  {
    if (h.mul_re == 1.0 && h.mul_im == 0.0)
    {
      o << "  " << ret << " = P;\n";
    }
    else
    {
      o << "  dcomplex za = {" << std::scientific << std::setprecision(18) << h.mul_re << ", " << std::scientific << std::setprecision(18) << h.mul_im << "};\n";
      o << "  " << ret << " = dualfec_dcmul(za, P);\n";
    }
  }
  else
  {
    for (int i = 0; i <= h.pow - 1; ++i)
    {
      int j = h.pow - 1 - i;
      // S += pow(W, j) * pow(B, i);
      if (i == 0)
      {
        o << "  dualfecomplex S = Wp[" << j << "];\n";
      }
      else if (j == 0)
      {
        o << "  S = dualfec_addfec(S, Bp[" << i << "]);\n";
      }
      else
      {
        o << "  S = dualfec_add(S, dualfec_mulfec(Wp[" << j << "], " << "Bp[" << i << "]));\n";
      }
    }
    if (h.mul_re == 1.0 && h.mul_im == 0.0)
    {
      o << "  " << ret << " = dualfec_mul(P, S);\n";
    }
    else
    {
      o << "  dcomplex za = {" << std::scientific << std::setprecision(18) << h.mul_re << ", " << std::scientific << std::setprecision(18) << h.mul_im << "};\n";
      o << "  " << ret << " = dualfec_dcmul(za, dualfec_mul(P, S));\n";
    }
  }
  o << "}\n";
  return o.str();
}

std::string hybrid_f_opencl_floatexp(const hybrid_line &h, const std::string &ret, const std::string &Z)
{
  std::ostringstream o;
  o << "{\n";
  o << "  fecomplex fone;\n";
  o << hybrid_f_opencl_floatexp(h.one, "fone", Z);
  if (h.two.pow == 0 && h.two.mul_re == 0.0 && h.two.mul_im == 0.0 && (h.mode == hybrid_combine_add || h.mode == hybrid_combine_sub))
  {
    o << "  " << ret << " = fone;\n";
  o << "}\n";
    return o.str();
  }
  o << "  fecomplex ftwo;\n";
  o << hybrid_f_opencl_floatexp(h.two, "ftwo", Z);
  switch (h.mode)
  {
    case hybrid_combine_add:
    {
      o << "  " << ret << " = fec_add(fone, ftwo);\n";
      break;
    }
    case hybrid_combine_sub:
    {
      o << "  " << ret << " = fec_sub(fone, ftwo);\n";
      break;
    }
    case hybrid_combine_mul:
    {
      o << "  " << ret << " = fec_mul(fone, ftwo);\n";
      break;
    }
  }
  o << "}\n";
  return o.str();
}

std::string hybrid_pf_opencl_floatexp(const hybrid_line &h, const std::string &ret, const std::string &Z, const std::string &z)
{
  std::ostringstream o;
  o << "{\n";
  o << "  fecomplex pfone;\n";
  o << hybrid_pf_opencl_floatexp(h.one, "pfone", Z, z);
  if (h.two.pow == 0 && h.two.mul_re == 0.0 && h.two.mul_im == 0.0 && (h.mode == hybrid_combine_add || h.mode == hybrid_combine_sub))
  {
    o << "  " << ret << " = pfone;\n";
  o << "}\n";
    return o.str();
  }
  o << "  fecomplex pftwo;\n";
  o << hybrid_pf_opencl_floatexp(h.two, "pftwo", Z, z);
  switch (h.mode)
  {
    case hybrid_combine_add:
    {
      o << "  " << ret << " = fec_add(pfone, pftwo);\n";
      break;
    }
    case hybrid_combine_sub:
    {
      o << "  " << ret << " = fec_sub(pfone, pftwo);\n";
      break;
    }
    case hybrid_combine_mul:
    {
      o << "  fecomplex Zz = fec_add(" << Z << ", " << z << ");\n";
      o << "  fecomplex ftwo;\n";
      o << hybrid_f_opencl_floatexp(h.two, "ftwo", "Zz");
      o << "  fecomplex fone;\n";
      o << hybrid_f_opencl_floatexp(h.one, "fone", Z);
      o << "  " << ret << " = fec_add(fec_mul(pfone, ftwo), fec_mul(fone, pftwo));\n";
      break;
    }
  }
  o << "}\n";
  return o.str();
}

std::string hybrid_pf_opencl_floatexp_dual(const hybrid_line &h, const std::string &ret, const std::string &Z, const std::string &z)
{
  std::ostringstream o;
  o << "{\n";
  o << "  dualfecomplex pfone;\n";
  o << hybrid_pf_opencl_floatexp_dual(h.one, "pfone", Z, z);
  if (h.two.pow == 0 && h.two.mul_re == 0.0 && h.two.mul_im == 0.0 && (h.mode == hybrid_combine_add || h.mode == hybrid_combine_sub))
  {
    o << "  " << ret << " = pfone;\n";
  o << "}\n";
    return o.str();
  }
  o << "  dualfecomplex pftwo;\n";
  o << hybrid_pf_opencl_floatexp_dual(h.two, "pftwo", Z, z);
  switch (h.mode)
  {
    case hybrid_combine_add:
    {
      o << "  " << ret << " = dualfec_add(pfone, pftwo);\n";
      break;
    }
    case hybrid_combine_sub:
    {
      o << "  " << ret << " = dualfec_sub(pfone, pftwo);\n";
      break;
    }
    case hybrid_combine_mul:
    {
      o << "  dualfecomplex Zz = dualfec_fecadd(" << Z << ", " << z << ");\n";
      o << "  dualfecomplex ftwo;\n";
      o << hybrid_f_opencl_floatexp_dual(h.two, "ftwo", "Zz");
      o << "  fecomplex fone;\n";
      o << hybrid_f_opencl_floatexp(h.one, "fone", Z);
      o << "  " << ret << " = dualfec_add(dualfec_mul(pfone, ftwo), dualfec_fecmul(fone, pftwo));\n";
      break;
    }
  }
  o << "}\n";
  return o.str();
}

std::string hybrid_pf_opencl_floatexp(const hybrid_stanza &h, const std::string &ret, const std::string &Z, const std::string &z, const std::string &c)
{
  std::ostringstream o;
  o << "{\n";
  const int k = h.lines.size();
  for (int i = 0; i < k; ++i)
  {
    o << "  {\n";
    o << "    fecomplex znext;\n";
    o << hybrid_pf_opencl_floatexp(h.lines[i], "znext", Z, z);
    o << "    " << z << " = znext;\n";
    o << "    fecomplex Znext;\n";
    o << hybrid_f_opencl_floatexp(h.lines[i], "Znext", Z); // space vs work tradeoff; should be fine at low precision as there is no +C ?
    o << "    " << Z << " = Znext;\n";
    o << "  }\n";
  }
  o << "  " << ret << " = fec_add(" << z << ", " << c << ");\n";
  o << "}\n";
  return o.str();
}

std::string hybrid_pf_opencl_floatexp_dual(const hybrid_stanza &h, const std::string &ret, const std::string &Z, const std::string &z, const std::string &c)
{
  std::ostringstream o;
  o << "{\n";
  const int k = h.lines.size();
  for (int i = 0; i < k; ++i)
  {
    o << "  {\n";
    o << "    dualfecomplex znext;\n";
    o << hybrid_pf_opencl_floatexp_dual(h.lines[i], "znext", Z, z);
    o << "    " << z << " = znext;\n";
    o << "    fecomplex Znext;\n";
    o << hybrid_f_opencl_floatexp(h.lines[i], "Znext", Z); // space vs work tradeoff; should be fine at low precision as there is no +C ?
    o << "    " << Z << " = Znext;\n";
    o << "  }\n";
  }
  o << "  " << ret << " = dualfec_add(" << z << ", " << c << ");\n";
  o << "}\n";
  return o.str();
}

extern std::string hybrid_perturbation_floatexp_opencl(const hybrid_formula &h, bool derivatives)
{
  std::ostringstream o;
  o << "  {\n";
  o << "    if (++count >= g->hybrid_repeats[stanza])\n";
  o << "    {\n";
  o << "      count = 0;\n";
  o << "      if (++stanza >= (int) g->hybrid_nstanzas)\n";
  o << "      {\n";
  o << "        stanza = g->hybrid_loop_start;\n";
  o << "      }\n";
  o << "    }\n";
  o << "    l->log_m_nPower = g->hybrid_log_powers[stanza];\n";
  o << "    fecomplex Xd = { Xr, Xi };\n";
  if (derivatives)
  {
  o << "    dualfecomplex cd = { { cr, { one, zero } }, { ci, { zero, one } } };\n";
  o << "    dualfecomplex xd = { { xr, { dxa, dxb } }, { xi, { dya, dyb } } };\n";
  o << "    dualfecomplex xdn = { { zero, { zero, zero } }, { zero, { zero, zero } } };\n";
  }
  else
  {
  o << "    fecomplex cd = { cr, ci };\n";
  o << "    fecomplex xd = { xr, xi };\n";
  o << "    fecomplex xdn = { zero, zero };\n";
  }
  o << "    switch (stanza)\n";
  o << "    {\n";
  for (int stanza = 0; stanza < (int) h.stanzas.size(); ++stanza)
  {
  o << "      case " << stanza << ":\n";
  o << "      {\n";
  if (derivatives)
  {
  o << hybrid_pf_opencl_floatexp_dual(h.stanzas[stanza], "xdn", "Xd", "xd", "cd");
  }
  else
  {
  o << hybrid_pf_opencl_floatexp(h.stanzas[stanza], "xdn", "Xd", "xd", "cd");
  }
  o << "        break;\n";
  o << "      }\n";
  }
  o << "    }\n";
  if (derivatives)
  {
    o << "    xrn = xdn.re.x;\n";
    o << "    xin = xdn.im.x;\n";
    o << "    dxan = xdn.re.dx[0];\n";
    o << "    dxbn = xdn.re.dx[1];\n";
    o << "    dyan = xdn.im.dx[0];\n";
    o << "    dybn = xdn.im.dx[1];\n";
  }
  else
  {
    o << "    xrn = xdn.re;\n";
    o << "    xin = xdn.im;\n";
  }
  o << "  }\n";
  return o.str();
}

// scaled double opencl

extern std::string hybrid_pf_opencl_double_scaled(const hybrid_operator &h, const std::string &ret, const std::string &Z, const std::string &z, const std::string &s)
{
  std::ostringstream o;
  if (h.pow == 0)
  {
    o << ret << " = dc(0.0, 0.0);\n";
    return o.str();
  }
  o << "{\n";
  o << "  mantissa zX = " << Z << ".re;\n";
  o << "  mantissa zY = " << Z << ".im;\n";
  o << "  mantissa zx = " << z << ".re;\n";
  o << "  mantissa zy = " << z << ".im;\n";
  o << "  dcomplex W = dc_add(" << Z << ", dc_muld(" << z << ", " << s << "));\n";
  o << "  dcomplex B = " << Z << ";\n";
  if (h.abs_x)
  {
  o << "  zx = d_diffabs(d_div(zX, " << s << "), zx);\n";
  o << "  W.re = d_abs(W.re);\n";
  o << "  B.re = d_abs(B.re);\n";
  }
  if (h.abs_y)
  {
  o << "  zy = d_diffabs(d_div(zY, " << s << "), zy);\n";
  o << "  W.im = d_abs(W.im);\n";
  o << "  B.im = d_abs(B.im);\n";
  }
  if (h.neg_x)
  {
  o << "  zx = d_neg(zx);\n";
  o << "  W.re = d_neg(W.re);\n";
  o << "  B.re = d_neg(B.re);\n";
  }
  if (h.neg_y)
  {
  o << "  zy = d_neg(zy);\n";
  o << "  W.im = d_neg(W.im);\n";
  o << "  B.im = d_neg(B.im);\n";
  }
  o << "  dcomplex P = { zx, zy };\n";
  o << "  dcomplex Wp[" << h.pow << "];\n";
  o << "  {";
  o << "    dcomplex M = { 1.0, 0.0 };\n";
  o << "    Wp[0] = M;\n";
  if (1 < h.pow)
  {
    o << "    Wp[1] = W;\n";
    o << "    M = W;\n";
    for (int i = 2; i < h.pow; ++i)
    {
      o << "    M = dc_mul(M, W);\n";
      o << "    Wp[" << i << "] = M;\n";
    }
  }
  o << "  }";
  o << "  dcomplex Bp[" << h.pow << "];\n";
  o << "  {";
  o << "    dcomplex M = { 1.0, 0.0 };\n";
  o << "    Bp[0] = M;\n";
  if (1 < h.pow)
  {
    o << "    Bp[1] = B;\n";
    o << "    M = B;\n";
    for (int i = 2; i < h.pow; ++i)
    {
      o << "    M = dc_mul(M, B);\n";
      o << "    Bp[" << i << "] = M;\n";
    }
  }
  o << "  }";
  if (h.pow == 1)
  {
    if (h.mul_re == 1.0 && h.mul_im == 0.0)
    {
      o << "  " << ret << " = P;\n";
    }
    else
    {
      o << "  dcomplex za = {" << std::scientific << std::setprecision(18) << h.mul_re << ", " << std::scientific << std::setprecision(18) << h.mul_im << "};\n";
      o << "  " << ret << " = dc_mul(za, P);\n";
    }
  }
  else
  {
    for (int i = 0; i <= h.pow - 1; ++i)
    {
      int j = h.pow - 1 - i;
      // S += pow(W, i) * pow(B, j);
      if (i == 0)
      {
        o << "  dcomplex S = Bp[" << j << "];\n";
      }
      else if (j == 0)
      {
        o << "  S = dc_add(S, Wp[" << i << "]);\n";
      }
      else
      {
        o << "  S = dc_add(S, dc_mul(Wp[" << i << "], " << "Bp[" << j << "]));\n";
      }
    }
    if (h.mul_re == 1.0 && h.mul_im == 0.0)
    {
      o << "  " << ret << " = dc_mul(P, S);\n";
    }
    else
    {
      o << "  dcomplex za = {" << std::scientific << std::setprecision(18) << h.mul_re << ", " << std::scientific << std::setprecision(18) << h.mul_im << "};\n";
      o << "  " << ret << " = dc_mul(za, dc_mul(P, S));\n";
    }
  }
  o << "}\n";
  return o.str();
}

extern std::string hybrid_pf_opencl_double_dual_scaled(const hybrid_operator &h, const std::string &ret, const std::string &Z, const std::string &z, const std::string &s)
{
  std::ostringstream o;
  if (h.pow == 0)
  {
    o << ret << " = ddc(dd(0.0,0.0,0.0),dd(0.0,0.0,0.0))\n";
    return o.str();
  }
  o << "{\n";
  o << "  mantissa X = " << Z << ".re;\n";
  o << "  mantissa Y = " << Z << ".im;\n";
  o << "  duald x = " << z << ".re;\n";
  o << "  duald y = " << z << ".im;\n";
  o << "  dualdcomplex W = dualdc_dcadd(" << Z << ", dualdc_muld(" << z << "," << s <<"));\n";
  o << "  dcomplex B = " << Z << ";\n";
  if (h.abs_x)
  {
  o << "  x = duald_ddiffabs(d_div(X, " << s << "), x);\n";
  o << "  W.re = duald_abs(W.re);\n";
  o << "  B.re = d_abs(B.re);\n";
  }
  if (h.abs_y)
  {
  o << "  y = duald_ddiffabs(d_div(Y, " << s << "), y);\n";
  o << "  W.im = duald_abs(W.im);\n";
  o << "  B.im = d_abs(B.im);\n";
  }
  if (h.neg_x)
  {
  o << "  x = duald_neg(x);\n";
  o << "  W.re = duald_neg(W.re);\n";
  o << "  B.re = d_neg(B.re);\n";
  }
  if (h.neg_y)
  {
  o << "  y = duald_neg(y);\n";
  o << "  W.im = duald_neg(W.im);\n";
  o << "  B.im = d_neg(B.im);\n";
  }
  o << "  dualdcomplex P = { x, y };\n";
  o << "  dualdcomplex Wp[" << h.pow << "];\n";
  o << "  {";
  o << "    dualdcomplex M = { { 1.0, { 0.0, 0.0 } }, { 0.0, { 0.0, 0.0 } } };\n";
  o << "    Wp[0] = M;\n";
  if (1 < h.pow)
  {
    o << "    Wp[1] = W;\n";
    o << "    M = W;\n";
    for (int i = 2; i < h.pow; ++i)
    {
      o << "    M = dualdc_mul(M, W);\n";
      o << "    Wp[" << i << "] = M;\n";
    }
  }
  o << "  }";
  o << "  dcomplex Bp[" << h.pow << "];\n";
  o << "  {";
  o << "    dcomplex M = { 1.0, 0.0 };\n";
  o << "    Bp[0] = M;\n";
  if (1 < h.pow)
  {
    o << "    Bp[1] = B;\n";
    o << "    M = B;\n";
    for (int i = 2; i < h.pow; ++i)
    {
      o << "    M = dc_mul(M, B);\n";
      o << "    Bp[" << i << "] = M;\n";
    }
  }
  o << "  }";
  if (h.pow == 1)
  {
    if (h.mul_re == 1.0 && h.mul_im == 0.0)
    {
      o << "  " << ret << " = P;\n";
    }
    else
    {
      o << "  dcomplex a = {" << std::scientific << std::setprecision(18) << h.mul_re << ", " << std::scientific << std::setprecision(18) << h.mul_im << "};\n";
      o << "  " << ret << " = dualdc_dcmul(a, P);\n";
    }
  }
  else
  {
    for (int i = 0; i <= h.pow - 1; ++i)
    {
      int j = h.pow - 1 - i;
      // S += pow(W, j) * pow(B, i);
      if (i == 0)
      {
        o << "  dualdcomplex S = Wp[" << j << "];\n";
      }
      else if (j == 0)
      {
        o << "  S = dualdc_adddc(S, Bp[" << i << "]);\n";
      }
      else
      {
        o << "  S = dualdc_add(S, dualdc_muldc(Wp[" << j << "], " << "Bp[" << i << "]));\n";
      }
    }
    if (h.mul_re == 1.0 && h.mul_im == 0.0)
    {
      o << "  " << ret << " = dualdc_mul(P, S);\n";
    }
    else
    {
      o << "  dcomplex a = {" << std::scientific << std::setprecision(18) << h.mul_re << ", " << std::scientific << std::setprecision(18) << h.mul_im << "};\n";
      o << "  " << ret << " = dualdc_dcmul(a, dualdc_mul(P, S));\n";
    }
  }
  o << "}\n";
  return o.str();
}

std::string hybrid_pf_opencl_double_scaled(const hybrid_line &h, const std::string &ret, const std::string &Z, const std::string &z, const std::string &s)
{
  std::ostringstream o;
  o << "{\n";
  o << "  dcomplex pfone;\n";
  o << hybrid_pf_opencl_double_scaled(h.one, "pfone", Z, z, s);
  if (h.two.pow == 0 && h.two.mul_re == 0.0 && h.two.mul_im == 0.0 && (h.mode == hybrid_combine_add || h.mode == hybrid_combine_sub))
  {
    o << "  " << ret << " = pfone;\n";
  o << "}\n";
    return o.str();
  }
  o << "  dcomplex pftwo;\n";
  o << hybrid_pf_opencl_double_scaled(h.two, "pftwo", Z, z, s);
  switch (h.mode)
  {
    case hybrid_combine_add:
    {
      o << "  " << ret << " = dc_add(pfone, pftwo);\n";
      break;
    }
    case hybrid_combine_sub:
    {
      o << "  " << ret << " = dc_sub(pfone, pftwo);\n";
      break;
    }
    case hybrid_combine_mul:
    {
      o << "  dcomplex Zz = dc_add(" << Z << ", dc_muld(" << z << ", " << s << "));\n";
      o << "  dcomplex ftwo;\n";
      o << hybrid_f_opencl_double(h.two, "ftwo", "Zz");
      o << "  dcomplex fone;\n";
      o << hybrid_f_opencl_double(h.one, "fone", Z);
      o << "  " << ret << " = dc_add(dc_mul(pfone, ftwo), dc_mul(fone, pftwo));\n";
      break;
    }
  }
  o << "}\n";
  return o.str();
}

std::string hybrid_pf_opencl_double_dual_scaled(const hybrid_line &h, const std::string &ret, const std::string &Z, const std::string &z, const std::string &s)
{
  std::ostringstream o;
  o << "{\n";
  o << "  dualdcomplex pfone;\n";
  o << hybrid_pf_opencl_double_dual_scaled(h.one, "pfone", Z, z, s);
  if (h.two.pow == 0 && h.two.mul_re == 0.0 && h.two.mul_im == 0.0 && (h.mode == hybrid_combine_add || h.mode == hybrid_combine_sub))
  {
    o << "  " << ret << " = pfone;\n";
  o << "}\n";
    return o.str();
  }
  o << "  dualdcomplex pftwo;\n";
  o << hybrid_pf_opencl_double_dual_scaled(h.two, "pftwo", Z, z, s);
  switch (h.mode)
  {
    case hybrid_combine_add:
    {
      o << "  " << ret << " = dualdc_add(pfone, pftwo);\n";
      break;
    }
    case hybrid_combine_sub:
    {
      o << "  " << ret << " = dualdc_sub(pfone, pftwo);\n";
      break;
    }
    case hybrid_combine_mul:
    {
      o << "  dualdcomplex Zz = dualdc_dcadd(" << Z << ", dualdc_dcmuld(" << z << ", " << s << "));\n";
      o << "  dualdcomplex ftwo;\n";
      o << hybrid_f_opencl_double_dual(h.two, "ftwo", "Zz");
      o << "  dcomplex fone;\n";
      o << hybrid_f_opencl_double(h.one, "fone", Z);
      o << "  " << ret << " = dualdc_add(dualdc_mul(pfone, ftwo), dualdc_dcmul(fone, pftwo));\n";
      break;
    }
  }
  o << "}\n";
  return o.str();
}

std::string hybrid_pf_opencl_double_scaled(const hybrid_stanza &h, const std::string &ret, const std::string &Z, const std::string &z, const std::string &c, const std::string &s)
{
  std::ostringstream o;
  o << "{\n";
  const int k = h.lines.size();
  for (int i = 0; i < k; ++i)
  {
    o << "  {\n";
    o << "    dcomplex znext;\n";
    o << hybrid_pf_opencl_double_scaled(h.lines[i], "znext", Z, z, s);
    o << "    " << z << " = znext;\n";
    o << "    dcomplex Znext;\n";
    o << hybrid_f_opencl_double(h.lines[i], "Znext", Z); // space vs work tradeoff; should be fine at low precision as there is no +C ?
    o << "    " << Z << " = Znext;\n";
    o << "  }\n";
  }
  o << "  " << ret << " = dc_add(" << z << ", " << c << ");\n";
  o << "}\n";
  return o.str();
}

std::string hybrid_pf_opencl_double_dual_scaled(const hybrid_stanza &h, const std::string &ret, const std::string &Z, const std::string &z, const std::string &c, const std::string &s)
{
  std::ostringstream o;
  o << "{\n";
  const int k = h.lines.size();
  for (int i = 0; i < k; ++i)
  {
    o << "  {\n";
    o << "    dualdcomplex znext;\n";
    o << hybrid_pf_opencl_double_dual_scaled(h.lines[i], "znext", Z, z, s);
    o << "    " << z << " = znext;\n";
    o << "    dcomplex Znext;\n";
    o << hybrid_f_opencl_double(h.lines[i], "Znext", Z); // space vs work tradeoff; should be fine at low precision as there is no +C ?
    o << "    " << Z << " = Znext;\n";
    o << "  }\n";
  }
  o << "  " << ret << " = dualdc_add(" << z << ", " << c << ");\n";
  o << "}\n";
  return o.str();
}

#define STR(s) #s

extern std::string hybrid_perturbation_scaled_opencl(const hybrid_formula &h, const bool derivatives)
{
  const bool simple1 =
    h.stanzas.size() == 1 &&
    h.stanzas[0].lines.size() == 1 &&
    h.stanzas[0].lines[0].mode == hybrid_combine_add &&
    h.stanzas[0].lines[0].two.mul_re == 0 &&
    h.stanzas[0].lines[0].two.mul_im == 0;
  const bool simple2 =
    h.stanzas.size() == 1 &&
    h.stanzas[0].lines.size() == 2 &&
    h.stanzas[0].lines[0].mode == hybrid_combine_add &&
    h.stanzas[0].lines[0].two.mul_re == 0 &&
    h.stanzas[0].lines[0].two.mul_im == 0 &&
    h.stanzas[0].lines[1].mode == hybrid_combine_add &&
    h.stanzas[0].lines[1].two.mul_re == 0 &&
    h.stanzas[0].lines[1].two.mul_im == 0;
  hybrid_operator op1 = {0}, op2 = {0};
  if (h.stanzas[0].lines.size() > 0) op1 = h.stanzas[0].lines[0].one;
  if (h.stanzas[0].lines.size() > 1) op2 = h.stanzas[0].lines[1].one;

  if (derivatives)
  {

  std::ostringstream o;
  o << STR(

void perturbation_double_loop
( __global const p_config    *g
, __global const mantissa    *m_refx
, __global const mantissa    *m_refy
, __global const mantissa    *m_refz
,                p_status_d  *l
)
{
}

void perturbation_floatexp_loop
( __global const p_config    *g
, __global const floatexp    *m_refX
, __global const floatexp    *m_refY
, __global const floatexp    *m_refZ
,                p_status_fe *l
)
{
}

void perturbation_scaled_loop
( __global const p_config    *g
, __global const mantissa    *m_refx
, __global const mantissa    *m_refy
, __global const mantissa    *m_refz
, __global const long        *m_refN
, __global const floatexp    *m_refX
, __global const floatexp    *m_refY
, __global const floatexp    *m_refZ
,                p_status_fe *l
)
{
  const int pwr =

  ) << hybrid_power_inf(h) << STR(;

  const mantissa w2threshold = exp(log(LARGE_MANTISSA) / pwr);
  const mantissa d2threshold = exp(log(LARGE_MANTISSA) / (pwr - 1));
  const floatexp zero = fe_floatexp(0.0, 0);
  const floatexp one = fe_floatexp(1.0, 0);
  const bool no_g = g->g_real == 1.0 && g->g_imag == 1.0 && g->norm_p == 2.0;
  const dualfe cr = dfe(l->cr, l->daa, l->dab);
  const dualfe ci = dfe(l->ci, l->dba, l->dbb);
  dualfe xr = dfe(l->xr, l->dxa, l->dxb);
  dualfe xi = dfe(l->xi, l->dya, l->dyb);
  int count = 0;
  int stanza = 0;
  mantissa test1 = l->test1;
  mantissa test2 = l->test2;
  long antal = l->antal;
  long rantal = antal;
  l->log_m_nPower = g->hybrid_log_powers[0];
  dualfe Xxr = dfe(zero, zero, zero);
  dualfe Xxi = dfe(zero, zero, zero);
  long k = 0; long n = 0;
  floatexp Xrf; floatexp Xif; floatexp Xzf;
  do
  {
    if (k < g->m_nRSize)
    {
      n = m_refN[k];
      Xrf = m_refX[k];
      Xif = m_refY[k];
      Xzf = m_refZ[k];
      k++;
    }
    else
    {
      n = g->nMaxIter;
    }
  }
  while (n < antal);
  floatexp S = fe_sqrt(fe_add(fe_sqr(xr.x), fe_sqr(xi.x)));
  mantissa s = fe_double(S);
  duald wr = dualfe_double(dualfe_divfe(xr, S));
  duald wi = dualfe_double(dualfe_divfe(xi, S));
  duald ur = dualfe_double(dualfe_divfe(cr, S));
  duald ui = dualfe_double(dualfe_divfe(ci, S));
  for (; antal < g->nMaxIter && rantal < g->reference_size_x; antal++)
  {
    const bool full_iteration = rantal == n;
    if (full_iteration)
    {
      rantal++;
      floatexp Xr = Xrf;
      floatexp Xi = Xif;
      floatexp Xz = Xzf;
      if (k < g->m_nRSize)
      {
        n = m_refN[k];
        Xrf = m_refX[k];
        Xif = m_refY[k];
        Xzf = m_refZ[k];
        k++;
      }
      else
      {
        if (g->singleref)
        {
          k = 0;
          n = m_refN[k];
          Xrf = m_refX[k];
          Xif = m_refY[k];
          Xzf = m_refZ[k];
          k++;
        }
        else
        {
          n = g->nMaxIter;
        }
      }
      dualfe xr = dualfe_femulduald(S, wr);
      dualfe xi = dualfe_femulduald(S, wi);
      Xxr = dualfe_feadd(Xr, xr);
      Xxi = dualfe_feadd(Xi, xi);
      floatexp Xxr2 = fe_sqr(Xxr.x);
      floatexp Xxi2 = fe_sqr(Xxi.x);
      test2 = test1;
      floatexp ftest1 = fe_add(Xxr2, Xxi2);
      test1 = fe_double(ftest1);
      if (g->singleref)
      {
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(Xxr.x), fe_double(Xxi.x));
        }
        if (test1 > g->m_nBailout2)
        {
          break;
        }
        if (fe_lt(ftest1, fe_add(fe_sqr(xr.x), fe_sqr(xi.x))) || rantal == g->reference_size_x)
        {
          xr = Xxr;
          xi = Xxi;
          rantal = 0;
          Xr = zero;
          Xi = zero;
          Xz = zero;
          Xxr = dualfe_feadd(Xr, xr);
          Xxi = dualfe_feadd(Xi, xi);
          Xxr2 = fe_sqr(Xxr.x);
          Xxi2 = fe_sqr(Xxi.x);
          ftest1 = fe_add(Xxr2, Xxi2);
          test1 = fe_double(ftest1);
          if (! no_g)
          {
            test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(Xxr.x), fe_double(Xxi.x));
          }
        }
      }
      else
      {
        if (fe_lt(ftest1, Xz))
        {
          l->bGlitch = true;
          if (! l->bNoGlitchDetection)
          {
            break;
          }
        }
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(Xxr.x), fe_double(Xxi.x));
        }
        if (test1 > g->m_nBailout2)
        {
          break;
        }
      }
      const fecomplex Z = fec(Xr, Xi);
      const dualfecomplex z = dfec(xr, xi);
      const dualfecomplex c = dfec(cr, ci);
      dualfecomplex zn;
  );

      if (simple1)
      {
        o << hybrid_pf_opencl_floatexp_dual(op1, "zn", "Z", "z");
        o << "zn = dualfec_add(zn, c);\n";
      }
      else if (simple2)
      {
        o << "fecomplex Z1;\n";
        o << "dualfecomplex z1;\n";
        o << hybrid_f_opencl_floatexp(op1, "Z1", "Z");
        o << hybrid_pf_opencl_floatexp_dual(op1, "z1", "Z", "z");
        o << hybrid_pf_opencl_floatexp_dual(op2, "zn", "Z1", "z1");
        o << "zn = dualfec_add(zn, c);\n";
      }
      else
      {
        o << STR(

        if (++count >= g->hybrid_repeats[stanza])
        {
          count = 0;
          if (++stanza >= g->hybrid_nstanzas)
          {
            stanza = g->hybrid_loop_start;
          }
        }
        l->log_m_nPower = g->hybrid_log_powers[stanza];
        switch (stanza)
        {

        );
        for (size_t stanza = 0; stanza < h.stanzas.size(); ++stanza)
        {
          o << "case " << stanza << ": {\n"
            << hybrid_pf_opencl_floatexp_dual(h.stanzas[stanza], "zn", "Z", "z", "c")
            << "break;}\n";
        }
        o << STR(

        }

        );
      }

  o << STR(

      const dualfe xrn = zn.re;
      const dualfe xin = zn.im;
      S = fe_sqrt(fe_add(fe_sqr(xrn.x), fe_sqr(xin.x)));
      s = fe_double(S);
      wr = dualfe_double(dualfe_divfe(xrn, S));
      wi = dualfe_double(dualfe_divfe(xin, S));
      ur = dualfe_double(dualfe_divfe(cr, S));
      ui = dualfe_double(dualfe_divfe(ci, S));
    }
    else
    {
      mantissa Xr = m_refx[rantal];
      mantissa Xi = m_refy[rantal];
      mantissa Xz = m_refz[rantal];
      rantal++;
      duald Xxrd = duald_dadd(Xr, duald_muld(wr, s));
      duald Xxid = duald_dadd(Xi, duald_muld(wi, s));
      Xxr = dualfe_from_duald(Xxrd);
      Xxi = dualfe_from_duald(Xxid);
      mantissa Xxr2 = Xxrd.x * Xxrd.x;
      mantissa Xxi2 = Xxid.x * Xxid.x;
      test2 = test1;
      test1 = Xxr2 + Xxi2;
      if (g->singleref)
      {
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, Xxrd.x, Xxid.x);
        }
        if (test1 > g->m_nBailout2)
        {
          break;
        }
        if (test1 < s * s * (wr.x * wr.x + wi.x * wi.x) || rantal == g->reference_size_x)
        {
          dualfe xr = Xxr;
          dualfe xi = Xxi;
          rantal = 0;
          Xr = 0;
          Xi = 0;
          Xz = 0;
          Xxrd = duald_dadd(Xr, fe_double(xr.x));
          Xxid = duald_dadd(Xim, fe_double(xi.x));
          Xxr = dualfe_from_duald(Xxrd);
          Xxi = dualfe_from_duald(Xxid);
          Xxr2 = Xxrd.x * Xxrd.x;
          Xxi2 = Xxid.x * Xxid.x;
          test1 = Xxr2 + Xxi2;
          if (! no_g)
          {
            test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(Xxr.x), fe_double(Xxi.x));
          }
          // rescale
          S = fe_sqrt(fe_add(fe_sqr(xr.x), fe_sqr(xi.x)));
          s = fe_double(S);
          wr = dualfe_double(dualfe_divfe(xr, S));
          wi = dualfe_double(dualfe_divfe(xi, S));
          ur = dualfe_double(dualfe_divfe(cr, S));
          ui = dualfe_double(dualfe_divfe(ci, S));
        }
      }
      else
      {
        if (test1 < Xz)
        {
          l->bGlitch = true;
          if (! l->bNoGlitchDetection)
          {
            break;
          }
        }
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, Xxrd.x, Xxid.x);
        }
        if (test1 > g->m_nBailout2)
        {
          break;
        }
      }
      dcomplex Z = dc(Xr, Xi);
      dualdcomplex w = ddc(wr, wi);
      dualdcomplex u = ddc(ur, ui);
      dualdcomplex wn;

  );

      if (simple1)
      {
        o << hybrid_pf_opencl_double_dual_scaled(op1, "wn", "Z", "w", "s");
        o << "wn = dualdc_add(wn, u);\n";
      }
      else if (simple2)
      {
        o << "dcomplex Z1;\n";
        o << "dualdcomplex w1;\n";
        o << hybrid_f_opencl_double(op1, "Z1", "Z");
        o << hybrid_pf_opencl_double_dual_scaled(op1, "w1", "Z", "w", "s");
        o << hybrid_pf_opencl_double_dual_scaled(op2, "wn", "Z1", "w1", "s");
        o << "wn = dualfec_add(wn, u);\n";
      }
      else
      {
        o << STR(

        if (++count >= g->hybrid_repeats[stanza])
        {
          count = 0;
          if (++stanza >= g->hybrid_nstanzas)
          {
            stanza = g->hybrid_loop_start;
          }
        }
        l->log_m_nPower = g->hybrid_log_powers[stanza];
        switch (stanza)
        {

        );
        for (size_t stanza = 0; stanza < h.stanzas.size(); ++stanza)
        {
          o << "case " << stanza << ": {\n"
            << hybrid_pf_opencl_double_scaled(h.stanzas[stanza], "wn", "Z", "w", "u", "s")
            << "break;}\n";
        }
        o << STR(

        }

        );

      }

  o << STR(

      const duald wrn = wn.re;
      const duald win = wn.im;

      const mantissa w2 = wrn.x * wrn.x + win.x * win.x;
      const mantissa d2 = wrn.dx[0] * wrn.dx[0] + wrn.dx[1] * wrn.dx[1] + win.dx[0] * win.dx[0] + win.dx[1] * win.dx[1];
      if (w2 < w2threshold && d2 < d2threshold)
      {
        wr = wrn;
        wi = win;
      }
      else
      {
        dualfe xrn = dualfe_femulduald(S, wrn);
        dualfe xin = dualfe_femulduald(S, win);
        S = fe_sqrt(fe_add(fe_sqr(xrn.x), fe_sqr(xin.x)));
        s = fe_double(S);
        wr = dualfe_double(dualfe_divfe(xrn, S));
        wi = dualfe_double(dualfe_divfe(xin, S));
        ur = dualfe_double(dualfe_divfe(cr, S));
        ui = dualfe_double(dualfe_divfe(ci, S));
      }
    }
  }
  l->antal = antal;
  l->test1 = test1;
  l->test2 = test2;
  l->xr = Xxr.x;
  l->xi = Xxi.x;
  l->dxa = Xxr.dx[0];
  l->dxb = Xxr.dx[1];
  l->dya = Xxi.dx[0];
  l->dyb = Xxi.dx[1];
}

  );
  return o.str();

  }
  else
  {

  std::ostringstream o;
  o << STR(

void perturbation_double_loop
( __global const p_config    *g
, __global const mantissa    *m_refx
, __global const mantissa    *m_refy
, __global const mantissa    *m_refz
,                p_status_d  *l
)
{
}

void perturbation_floatexp_loop
( __global const p_config    *g
, __global const floatexp    *m_refX
, __global const floatexp    *m_refY
, __global const floatexp    *m_refZ
,                p_status_fe *l
)
{
}

void perturbation_scaled_loop
( __global const p_config    *g
, __global const mantissa    *m_refx
, __global const mantissa    *m_refy
, __global const mantissa    *m_refz
, __global const long        *m_refN
, __global const floatexp    *m_refX
, __global const floatexp    *m_refY
, __global const floatexp    *m_refZ
,                p_status_fe *l
)
{
  const int pwr =

  ) << hybrid_power_inf(h) << STR(;

  const mantissa w2threshold = exp(log(LARGE_MANTISSA) / pwr);
  const floatexp zero = fe_floatexp(0.0, 0);
  const floatexp one = fe_floatexp(1.0, 0);
  const bool no_g = g->g_real == 1.0 && g->g_imag == 1.0 && g->norm_p == 2.0;
  const floatexp cr = l->cr;
  const floatexp ci = l->ci;
  floatexp xr = l->xr;
  floatexp xi = l->xi;
  int count = 0;
  int stanza = 0;
  mantissa test1 = l->test1;
  mantissa test2 = l->test2;
  long antal = l->antal;
  long rantal = antal;
  l->log_m_nPower = g->hybrid_log_powers[0];
  floatexp Xxr = zero;
  floatexp Xxi = zero;
  long k = 0; long n = 0;
  floatexp Xrf; floatexp Xif; floatexp Xzf;
  do
  {
    if (k < g->m_nRSize)
    {
      n = m_refN[k];
      Xrf = m_refX[k];
      Xif = m_refY[k];
      Xzf = m_refZ[k];
      k++;
    }
    else
    {
      n = g->nMaxIter;
    }
  }
  while (n < antal);
  floatexp S = fe_sqrt(fe_add(fe_sqr(xr), fe_sqr(xi)));
  mantissa s = fe_double(S);
  mantissa wr = fe_double(fe_div(xr, S));
  mantissa wi = fe_double(fe_div(xi, S));
  mantissa ur = fe_double(fe_div(cr, S));
  mantissa ui = fe_double(fe_div(ci, S));
  mantissa u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));
  for (; antal < g->nMaxIter; antal++)
  {
    const bool full_iteration = rantal == n;
    if (full_iteration)
    {
      rantal++;
      floatexp Xr = Xrf;
      floatexp Xi = Xif;
      floatexp Xz = Xzf;
      if (k < g->m_nRSize)
      {
        n = m_refN[k];
        Xrf = m_refX[k];
        Xif = m_refY[k];
        Xzf = m_refZ[k];
        k++;
      }
      else
      {
        if (g->singleref)
        {
          k = 0;
          n = m_refN[k];
          Xrf = m_refX[k];
          Xif = m_refY[k];
          Xzf = m_refZ[k];
          k++;
        }
        else
        {
          n = g->nMaxIter;
        }
      }
      floatexp xr = fe_muld(S, wr);
      floatexp xi = fe_muld(S, wi);
      Xxr = fe_add(Xr, xr);
      Xxi = fe_add(Xi, xi);
      floatexp Xxr2 = fe_sqr(Xxr);
      floatexp Xxi2 = fe_sqr(Xxi);
      test2 = test1;
      floatexp ftest1 = fe_add(Xxr2, Xxi2);
      test1 = fe_double(ftest1);
      if (g->singleref)
      {
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(Xxr), fe_double(Xxi));
        }
        if (test1 > g->m_nBailout2)
        {
          break;
        }
        if (fe_lt(ftest1, fe_add(fe_sqr(xr), fe_sqr(xi))) || rantal == g->reference_size_x)
        {
          xr = Xxr;
          xi = Xxi;
          rantal = 0;
          Xr = zero;
          Xi = zero;
          Xz = zero;
          Xxr = fe_add(Xr, xr);
          Xxi = fe_add(Xi, xi);
          Xxr2 = fe_sqr(Xxr);
          Xxi2 = fe_sqr(Xxi);
          ftest1 = fe_add(Xxr2, Xxi2);
          test1 = fe_double(ftest1);
          if (! no_g)
          {
            test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(Xxr), fe_double(Xxi));
          }
        }
      }
      else
      {
        if (fe_lt(ftest1, Xz))
        {
          l->bGlitch = true;
          if (! l->bNoGlitchDetection)
          {
            break;
          }
        }
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(Xxr), fe_double(Xxi));
        }
        if (test1 > g->m_nBailout2)
        {
          break;
        }
      }
      const fecomplex Z = fec(Xr, Xi);
      const fecomplex z = fec(xr, xi);
      const fecomplex c = fec(cr, ci);
      fecomplex zn;
  );

      if (simple1)
      {
        o << hybrid_pf_opencl_floatexp(op1, "zn", "Z", "z");
        o << "zn = fec_add(zn, c);\n";
      }
      else if (simple2)
      {
        o << "fecomplex Z1;\n";
        o << "fecomplex z1;\n";
        o << hybrid_f_opencl_floatexp(op1, "Z1", "Z");
        o << hybrid_pf_opencl_floatexp(op1, "z1", "Z", "z");
        o << hybrid_pf_opencl_floatexp(op2, "zn", "Z1", "z1");
        o << "zn = fec_add(zn, c);\n";
      }
      else
      {
        o << STR(

        if (++count >= g->hybrid_repeats[stanza])
        {
          count = 0;
          if (++stanza >= g->hybrid_nstanzas)
          {
            stanza = g->hybrid_loop_start;
          }
        }
        l->log_m_nPower = g->hybrid_log_powers[stanza];
        switch (stanza)
        {

        );
        for (size_t stanza = 0; stanza < h.stanzas.size(); ++stanza)
        {
          o << "case " << stanza << ": {\n"
            << hybrid_pf_opencl_floatexp(h.stanzas[stanza], "zn", "Z", "z", "c")
            << "break;}\n";
        }
        o << STR(

        }

        );
      }

  o << STR(

      const floatexp xrn = zn.re;
      const floatexp xin = zn.im;
      S = fe_sqrt(fe_add(fe_sqr(xrn), fe_sqr(xin)));
      s = fe_double(S);
      wr = fe_double(fe_div(xrn, S));
      wi = fe_double(fe_div(xin, S));
      ur = fe_double(fe_div(cr, S));
      ui = fe_double(fe_div(ci, S));
    }
    else
    {
      mantissa Xr = m_refx[rantal];
      mantissa Xi = m_refy[rantal];
      mantissa Xz = m_refz[rantal];
      rantal++;
      mantissa Xxrd = Xr + wr * s;
      mantissa Xxid = Xi + wi * s;
      Xxr = fe_floatexp(Xxrd, 0);
      Xxi = fe_floatexp(Xxid, 0);
      mantissa Xxr2 = Xxrd * Xxrd;
      mantissa Xxi2 = Xxid * Xxid;
      test2 = test1;
      test1 = Xxr2 + Xxi2;
      if (g->singleref)
      {
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, Xxrd, Xxid);
        }
        if (test1 > g->m_nBailout2)
        {
          break;
        }
        if (test1 < s * s * (wr * wr + wi * wi) || rantal == g->reference_size_x)
        {
          floatexp xr = Xxr;
          floatexp xi = Xxi;
          rantal = 0;
          Xr = 0;
          Xi = 0;
          Xz = 0;
          Xxr = xr;
          Xxi = xi;
          Xxrd = fe_double(Xxr);
          Xxid = fe_double(Xxi);
          Xxr2 = Xxrd * Xxrd;
          Xxi2 = Xxid * Xxid;
          test1 = Xxr2 + Xxi2;
          if (! no_g)
          {
            test1 = pnorm(g->g_real, g->g_imag, g->norm_p, Xxrd, Xxid);
          }
          // rescale
          S = fe_sqrt(fe_add(fe_sqr(xr), fe_sqr(xi)));
          s = fe_double(S);
          wr = fe_double(fe_div(xr, S));
          wi = fe_double(fe_div(xi, S));
          ur = fe_double(fe_div(cr, S));
          ui = fe_double(fe_div(ci, S));
        }
      }
      else
      {
        if (test1 < Xz)
        {
          l->bGlitch = true;
          if (! l->bNoGlitchDetection)
          {
            break;
          }
        }
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, Xxrd, Xxid);
        }
        if (test1 > g->m_nBailout2)
        {
          break;
        }
      }
      dcomplex Z = dc(Xr, Xi);
      dcomplex w = dc(wr, wi);
      dcomplex u = dc(ur, ui);
      dcomplex wn;

  );

      if (simple1)
      {
        o << hybrid_pf_opencl_double_scaled(op1, "wn", "Z", "w", "s");
        o << "wn = dc_add(wn, u);\n";
      }
      else if (simple2)
      {
        o << "dcomplex Z1;\n";
        o << "dcomplex w1;\n";
        o << hybrid_f_opencl_double(op1, "Z1", "Z");
        o << hybrid_pf_opencl_double_scaled(op1, "w1", "Z", "w", "s");
        o << hybrid_pf_opencl_double_scaled(op2, "wn", "Z1", "w1", "s");
        o << "wn = dc_add(wn, u);\n";
      }
      else
      {
        o << STR(

        if (++count >= g->hybrid_repeats[stanza])
        {
          count = 0;
          if (++stanza >= g->hybrid_nstanzas)
          {
            stanza = g->hybrid_loop_start;
          }
        }
        l->log_m_nPower = g->hybrid_log_powers[stanza];
        switch (stanza)
        {

        );
        for (size_t stanza = 0; stanza < h.stanzas.size(); ++stanza)
        {
          o << "case " << stanza << ": {\n"
            << hybrid_pf_opencl_double_scaled(h.stanzas[stanza], "wn", "Z", "w", "u", "s")
            << "break;}\n";
        }
        o << STR(

        }

        );

      }

  o << STR(

      const mantissa wrn = wn.re;
      const mantissa win = wn.im;

      const mantissa w2 = wrn * wrn + win * win;
      if (w2 < w2threshold)
      {
        wr = wrn;
        wi = win;
      }
      else
      {
        floatexp xrn = fe_muld(S, wrn);
        floatexp xin = fe_muld(S, win);
        S = fe_sqrt(fe_add(fe_sqr(xrn), fe_sqr(xin)));
        s = fe_double(S);
        wr = fe_double(fe_div(xrn, S));
        wi = fe_double(fe_div(xin, S));
        ur = fe_double(fe_div(cr, S));
        ui = fe_double(fe_div(ci, S));
      }
    }
  }
  l->antal = antal;
  l->test1 = test1;
  l->test2 = test2;
  l->xr = Xxr;
  l->xi = Xxi;
}

  );
  return o.str();
  }
}

static void pow(mpfr_t X, mpfr_t Y, int p, mpfr_t T1, mpfr_t T2, mpfr_t T3, mpfr_t T4, mpfr_t R, mpfr_t S)
{
  bool rs_initialized = false;
  // r = 1, s = 0
  while (p)
  {
    if (p & 1)
    {
      // rs *= xy
      if (rs_initialized)
      {
        mpfr_mul(T1, R, X, MPFR_RNDN);
        mpfr_mul(T2, S, Y, MPFR_RNDN);
        mpfr_mul(T3, R, Y, MPFR_RNDN);
        mpfr_mul(T4, S, X, MPFR_RNDN);
        mpfr_sub(R, T1, T2, MPFR_RNDN);
        mpfr_add(S, T3, T4, MPFR_RNDN);
      }
      else
      {
        rs_initialized = true;
        mpfr_set(R, X, MPFR_RNDN);
        mpfr_set(S, Y, MPFR_RNDN);
      }
    }
    p /= 2;
    if (p)
    {
      // xy = xy^2
      mpfr_sqr(T1, X, MPFR_RNDN);
      mpfr_sqr(T2, Y, MPFR_RNDN);
      mpfr_mul(T3, X, Y, MPFR_RNDN);
      mpfr_sub(X, T1, T2, MPFR_RNDN);
      mpfr_mul_2exp(Y, T3, 1, MPFR_RNDN);
    }
  }
  // return rs
  if (rs_initialized)
  {
    mpfr_set(X, R, MPFR_RNDN);
    mpfr_set(Y, S, MPFR_RNDN);
  }
  else
  {
    mpfr_set_ui(X, 1, MPFR_RNDN);
    mpfr_set_ui(Y, 0, MPFR_RNDN);
  }
}

extern bool reference_hybrid
  ( const hybrid_formula &h
  , Reference *m_Reference
  , bool &m_bStop, int64_t &m_nRDone, int64_t &m_nMaxIter
  , const CFixedFloat &Cr0, const CFixedFloat &Ci0
  , const double g_SeedR, const double g_SeedI
  , const double terminate
  , const double m_bGlitchLowTolerance
  )
{
    int64_t nMaxIter = m_nMaxIter;
    int64_t i;
    int power = hybrid_power_inf(h);
    double glitches[] = { 1e-7, 1e-6, 1e-5, 1e-4, 1e-4, 1e-3, 1e-3, 1e-2 };
    double glitch = glitches[std::min(std::max(0, power - 2), 7)];
    glitch = std::exp(std::log(glitch) * (1 - m_bGlitchLowTolerance / 2));
    mpfr_prec_t prec = mpfr_get_prec(Cr0.m_f.backend().data());
    mpfr_t A, B, X, Y, X1, Y1, X2, Y2, T1, T2, T3, T4, T5, T6;
    mpfr_init2(A, prec); mpfr_set(A, Cr0.m_f.backend().data(), MPFR_RNDN);
    mpfr_init2(B, prec); mpfr_set(B, Ci0.m_f.backend().data(), MPFR_RNDN);
    mpfr_init2(X, prec); mpfr_set_d(X, g_SeedR, MPFR_RNDN);
    mpfr_init2(Y, prec); mpfr_set_d(Y, g_SeedI, MPFR_RNDN);
    mpfr_init2(X1, prec);
    mpfr_init2(Y1, prec);
    mpfr_init2(X2, prec);
    mpfr_init2(Y2, prec);
    mpfr_init2(T1, prec);
    mpfr_init2(T2, prec);
    mpfr_init2(T3, prec);
    mpfr_init2(T4, prec);
    mpfr_init2(T5, prec);
    mpfr_init2(T6, prec);
    int count = 0;
    int stanza = 0;
    for (i = 0; i < nMaxIter && !m_bStop; ++i)
    {
      // X = hybrid_f(h.stanzas[stanza], X, C); // formula
      for (auto line : h.stanzas[stanza].lines)
      {
        // X, Y contains input to line
        // operator 1
        if (line.one.mul_re == 0 && line.one.mul_im == 0)
        {
          mpfr_set_ui(X1, 0, MPFR_RNDN);
          mpfr_set_ui(Y1, 0, MPFR_RNDN);
        }
        else
        {
          mpfr_set(X1, X, MPFR_RNDN);
          mpfr_set(Y1, Y, MPFR_RNDN);
          if (line.one.abs_x) mpfr_abs(X1, X1, MPFR_RNDN);
          if (line.one.abs_y) mpfr_abs(Y1, Y1, MPFR_RNDN);
          if (line.one.neg_x) mpfr_neg(X1, X1, MPFR_RNDN);
          if (line.one.neg_y) mpfr_neg(Y1, Y1, MPFR_RNDN);
          pow(X1, Y1, line.one.pow, T1, T2, T3, T4, T5, T6);
          if (line.one.mul_re == 1 && line.one.mul_im == 0)
          {
            // nop
          }
          else
          {
            mpfr_mul_d(T1, X1, line.one.mul_re, MPFR_RNDN);
            mpfr_mul_d(T2, Y1, line.one.mul_im, MPFR_RNDN);
            mpfr_mul_d(T3, X1, line.one.mul_im, MPFR_RNDN);
            mpfr_mul_d(T4, Y1, line.one.mul_re, MPFR_RNDN);
            mpfr_sub(X1, T1, T2, MPFR_RNDN);
            mpfr_add(Y1, T3, T4, MPFR_RNDN);
          }
        }
        // X1, Y1 contains operator 1 result
        // operator 2
        if (line.two.mul_re == 0 && line.two.mul_im == 0)
        {
          mpfr_set_ui(X2, 0, MPFR_RNDN);
          mpfr_set_ui(Y2, 0, MPFR_RNDN);
        }
        else
        {
          mpfr_set(X2, X, MPFR_RNDN);
          mpfr_set(Y2, Y, MPFR_RNDN);
          if (line.two.abs_x) mpfr_abs(X2, X2, MPFR_RNDN);
          if (line.two.abs_y) mpfr_abs(Y2, Y2, MPFR_RNDN);
          if (line.two.neg_x) mpfr_neg(X2, X2, MPFR_RNDN);
          if (line.two.neg_y) mpfr_neg(Y2, Y2, MPFR_RNDN);
          pow(X2, Y2, line.two.pow, T1, T2, T3, T4, T5, T6);
          if (line.two.mul_re == 1 && line.two.mul_im == 0)
          {
            // nop
          }
          else
          {
            mpfr_mul_d(T1, X2, line.two.mul_re, MPFR_RNDN);
            mpfr_mul_d(T2, Y2, line.two.mul_im, MPFR_RNDN);
            mpfr_mul_d(T3, X2, line.two.mul_im, MPFR_RNDN);
            mpfr_mul_d(T4, Y2, line.two.mul_re, MPFR_RNDN);
            mpfr_sub(X2, T1, T2, MPFR_RNDN);
            mpfr_add(Y2, T3, T4, MPFR_RNDN);
          }
        }
        // X2, Y2 contains operator 2 result
        switch (line.mode)
        {
          case hybrid_combine_add:
            mpfr_add(X, X1, X2, MPFR_RNDN);
            mpfr_add(Y, Y1, Y2, MPFR_RNDN);
            break;
          case hybrid_combine_sub:
            mpfr_sub(X, X1, X2, MPFR_RNDN);
            mpfr_sub(Y, Y1, Y2, MPFR_RNDN);
            break;
          case hybrid_combine_mul:
            mpfr_mul(T1, X1, X2, MPFR_RNDN);
            mpfr_mul(T2, Y1, Y2, MPFR_RNDN);
            mpfr_mul(T3, X1, Y2, MPFR_RNDN);
            mpfr_mul(T4, Y1, X2, MPFR_RNDN);
            mpfr_sub(X, T1, T2, MPFR_RNDN);
            mpfr_add(Y, T3, T4, MPFR_RNDN);
            break;
        }
        // X,Y contains output from line
      }
      mpfr_add(X, X, A, MPFR_RNDN);
      mpfr_add(Y, Y, B, MPFR_RNDN);
      if (++count >= h.stanzas[stanza].repeats)
      {
        count = 0;
        if (++stanza >= (ssize_t) h.stanzas.size())
        {
          stanza = h.loop_start;
        }
      }
      m_nRDone++;
      const floatexp Xrd = mpfr_get_fe(X);
      const floatexp Xid = mpfr_get_fe(Y);
      const floatexp abs_val = Xrd * Xrd + Xid * Xid;
      const floatexp Xz = abs_val * glitch;
      reference_append(m_Reference, Xrd, Xid, Xz);
      if (double(abs_val) >= terminate){
        if (nMaxIter == m_nMaxIter){
          nMaxIter = i + 10;
          if (nMaxIter > m_nMaxIter)
            nMaxIter = m_nMaxIter;
        }
      }
    }
    mpfr_clear(A);
    mpfr_clear(B);
    mpfr_clear(X);
    mpfr_clear(Y);
    mpfr_clear(X1);
    mpfr_clear(Y1);
    mpfr_clear(X2);
    mpfr_clear(Y2);
    mpfr_clear(T1);
    mpfr_clear(T2);
    mpfr_clear(T3);
    mpfr_clear(T4);
    mpfr_clear(T5);
    mpfr_clear(T6);
    return true;
}
