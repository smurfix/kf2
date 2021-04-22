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
  std::vector<long double> lx, ly, lz;
  std::vector<floatexp> fx, fy, fz;
  Reference(int64_t capacity, bool strict_zero)
  : strict_zero(strict_zero)
  , x(0)
  , y(0)
  , z(0)
  , N(0)
  , X(0)
  , Y(0)
  , Z(0)
  , lx(0)
  , ly(0)
  , lz(0)
  , fx(0)
  , fy(0)
  , fz(0)
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
int64_t reference_size_N(const Reference *R) { return R->N.size(); }
const int64_t *reference_ptr_N(const Reference *R) { return &R->N[0]; }
const floatexp *reference_ptr_X(const Reference *R) { return &R->X[0]; }
const floatexp *reference_ptr_Y(const Reference *R) { return &R->Y[0]; }
const floatexp *reference_ptr_Z(const Reference *R) { return &R->Z[0]; }

void reference_cook_long_double(Reference *R)
{
  const int64_t capacity = R->x.size();
  R->lx.reserve(capacity);
  R->ly.reserve(capacity);
  R->lz.reserve(capacity);
  for (int64_t k = 0; k < capacity; ++k)
  {
    R->lx[k] = R->x[k];
    R->ly[k] = R->y[k];
    R->lz[k] = R->z[k];
  }
  const int64_t full = R->N.size();
  for (int64_t k = 0; k < full; ++k)
  {
    const int64_t n = R->N[k];
    R->lx[n] = (long double)(R->X[k]);
    R->ly[n] = (long double)(R->Y[k]);
    R->lz[n] = (long double)(R->Z[k]);
  }
}

void reference_cook_floatexp(Reference *R)
{
  const int64_t capacity = R->x.size();
  R->fx.reserve(capacity);
  R->fy.reserve(capacity);
  R->fz.reserve(capacity);
  for (int64_t k = 0; k < capacity; ++k)
  {
    R->fx[k] = R->x[k];
    R->fy[k] = R->y[k];
    R->fz[k] = R->z[k];
  }
  const int64_t full = R->N.size();
  for (int64_t k = 0; k < full; ++k)
  {
    const int64_t n = R->N[k];
    R->fx[n] = R->X[k];
    R->fy[n] = R->Y[k];
    R->fz[n] = R->Z[k];
  }
}

template <> const double *reference_ptr_x<double>(const Reference *R) { return &R->x[0]; }
template <> const double *reference_ptr_y<double>(const Reference *R) { return &R->y[0]; }
template <> const double *reference_ptr_z<double>(const Reference *R) { return &R->z[0]; }
template <> const long double *reference_ptr_x<long double>(const Reference *R) { return &R->lx[0]; }
template <> const long double *reference_ptr_y<long double>(const Reference *R) { return &R->ly[0]; }
template <> const long double *reference_ptr_z<long double>(const Reference *R) { return &R->lz[0]; }
template <> const floatexp *reference_ptr_x<floatexp>(const Reference *R) { return &R->fx[0]; }
template <> const floatexp *reference_ptr_y<floatexp>(const Reference *R) { return &R->fy[0]; }
template <> const floatexp *reference_ptr_z<floatexp>(const Reference *R) { return &R->fz[0]; }
