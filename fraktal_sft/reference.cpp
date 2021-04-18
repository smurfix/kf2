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

#include "reference.h"

#include <vector>
#include "floatexp.h"

#include <iostream>

/*
strict_zero:
invariant:
  x[n], y[n], z[n] = 0 or denormal (underflow)
<=>
  N[k] = n
  X[k], Y[k], Z[k] != 0 (prevented underflow)

!strict_zero:
invariant:
  z[n] = 0 or denormal (underflow)
<=>
  N[k] = n
  Z[k] != 0 (prevented underflow)
*/

struct Reference
{
  bool strict_zero;
  std::vector<double> x, y, z;
  std::vector<int64_t> N;
  std::vector<floatexp> X, Y, Z;
  Reference(int64_t capacity, bool strict_zero)
  : strict_zero(strict_zero)
  , x(0)
  , y(0)
  , z(0)
  , N(0)
  , X(0)
  , Y(0)
  , Z(0)
  {
    x.reserve(capacity);
    y.reserve(capacity);
    z.reserve(capacity);
  }
};

Reference *reference_new(const int64_t capacity, const bool strict_zero)
{
  return new Reference(capacity, strict_zero);
}

void reference_delete(Reference *R)
{
  delete R;
}

void reference_append(Reference *R, const double x, const double y, const double z)
{
  R->x.push_back(x);
  R->y.push_back(y);
  R->z.push_back(z);
}

void reference_append(Reference *R, const floatexp &X, const floatexp &Y, const floatexp &Z)
{
  const double f = 2.2250738585072014e-308; // smallest normalized double
  const double F = 8.98846567431158e307; // largest finite power of 2
  const double x = double(X);
  const double y = double(Y);
  const double z = double(Z);
  if ( R->strict_zero
     ? (std::abs(x) < f || std::abs(y) < f || std::abs(z) < f)
     : (std::abs(z) < f)
     )
  {
    R->X.push_back(X);
    R->Y.push_back(Y);
    R->Z.push_back(Z);
    R->N.push_back(R->x.size());
    R->x.push_back(x);
    R->y.push_back(y);
    R->z.push_back(z);
  }
  else if (F < std::abs(x) || F < std::abs(y) || F < std::abs(z))
  {
    R->X.push_back(X);
    R->Y.push_back(Y);
    R->Z.push_back(Z);
    R->N.push_back(R->x.size());
    R->x.push_back(x);
    R->y.push_back(y);
    R->z.push_back(z);
  }
  else
  {
    reference_append(R, x, y, z);
  }
}

bool reference_get(const Reference *R, const int64_t n, double &x, double &y, double &z)
{
  if (0 <= n && n < int64_t(R->x.size()))
  {
    x = R->x[n];
    y = R->y[n];
    z = R->z[n];
    return true;
  }
  x = R->x.back();
  y = R->y.back();
  z = R->z.back();
  return false;
}

bool reference_get(const Reference *R, const int64_t k, int64_t &N, floatexp &X, floatexp &Y, floatexp &Z)
{
  if (0 <= k && k < int64_t(R->N.size()))
  {
    N = R->N[k];
    X = R->X[k];
    Y = R->Y[k];
    Z = R->Z[k];
    return true;
  }
  return false;
}

int64_t reference_size_x(const Reference *R) { return R->x.size(); }
const double *reference_ptr_x(const Reference *R) { return &R->x[0]; }
const double *reference_ptr_y(const Reference *R) { return &R->y[0]; }
const double *reference_ptr_z(const Reference *R) { return &R->z[0]; }
int64_t reference_size_N(const Reference *R) { return R->N.size(); }
const int64_t *reference_ptr_N(const Reference *R) { return &R->N[0]; }
const floatexp *reference_ptr_X(const Reference *R) { return &R->X[0]; }
const floatexp *reference_ptr_Y(const Reference *R) { return &R->Y[0]; }
const floatexp *reference_ptr_Z(const Reference *R) { return &R->Z[0]; }
