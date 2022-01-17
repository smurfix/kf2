/*
Kalles Fraktaler 2
Copyright (C) 2020,2021 Claude Heiland-Allen

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

#ifndef KF_HYBRID_DEF_H
#define KF_HYBRID_DEF_H

#include <string>
#include <vector>

#include "../formula/formula.h"
#include "floatexp.h"
#include "complex.h"
#include "dual.h"
#include "reference.h"

// FIXME TODO check that input from KFR files does not exceed this
#define MAX_HYBRID_STANZAS 4

// *sigh* default == operators would be nice, but require c++2a
//
struct hybrid_operator
{
  bool abs_x;
  bool abs_y;
  bool neg_x;
  bool neg_y;
  int pow;
  double mul_re;
  double mul_im;

  hybrid_operator() {}
  hybrid_operator(bool ax, bool ay, bool nx, bool ny, int p, double mr, double mi) {
    abs_x=ax; abs_y=ay; neg_x=nx; neg_y=ny; pow=p; mul_re=mr; mul_im=mi;
  }
  hybrid_operator(std::string_view);
  std::string to_string() const;

  bool operator==(const hybrid_operator &other) const;
  bool operator==(const hybrid_operator &&other) const;
};

enum hybrid_combine
{
  hybrid_combine_add = 0,
  hybrid_combine_sub = 1,
  hybrid_combine_mul = 2,
  hybrid_combine_div = 3
};

struct hybrid_line
{
  hybrid_operator one;
  hybrid_operator two;
  hybrid_combine mode;

  hybrid_line() {}
  hybrid_line(hybrid_operator a, hybrid_operator b, enum hybrid_combine c) {
    one = a; two = b; mode = c;
  }
  hybrid_line(std::string_view);
  std::string to_string() const;

  bool operator==(const hybrid_line &other) const;
  bool operator==(const hybrid_line &&other) const;
};

struct hybrid_stanza
{
  std::vector<hybrid_line> lines;
  int repeats;

  hybrid_stanza() {};
  hybrid_stanza(std::string_view);
  std::string to_string() const;

  bool operator==(const hybrid_stanza &other) const;
  bool operator==(const hybrid_stanza &&other) const;
};

struct hybrid_formula
{
  std::vector<hybrid_stanza> stanzas;
  int loop_start;

  hybrid_formula() {};
  hybrid_formula(std::string_view);
  std::string to_string() const;
  bool is_valid() const;

  bool operator==(const hybrid_formula &other) const;
  bool operator==(const hybrid_formula &&other) const;
};

#endif // KF_HYBRID_DEF_H
