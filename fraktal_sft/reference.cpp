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
#include <iostream>

/*
for scaled types only:

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
  enum Reference_Type reftype;
  std::vector<float> fx, fy, fz;
  std::vector<double> dx, dy, dz;
  std::vector<long double> lx, ly, lz;
  std::vector<int64_t> fN;
  std::vector<tfloatexp<float, int32_t>> fX, fY, fZ;
  std::vector<int64_t> dN;
  std::vector<tfloatexp<double, int64_t>> dX, dY, dZ;
  Reference(int64_t capacity, bool strict_zero, enum Reference_Type reftype)
  : strict_zero(strict_zero)
  , reftype(reftype)
  , fx(0)
  , fy(0)
  , fz(0)
  , dx(0)
  , dy(0)
  , dz(0)
  , lx(0)
  , ly(0)
  , lz(0)
  , fN(0)
  , fX(0)
  , fY(0)
  , fZ(0)
  , dN(0)
  , dX(0)
  , dY(0)
  , dZ(0)
  {
    switch (reftype)
    {
      case Reference_Float:
      case Reference_ScaledFloat:
      {
        fx.reserve(capacity);
        fy.reserve(capacity);
        fz.reserve(capacity);
        break;
      }
      case Reference_Double:
      case Reference_ScaledDouble:
      {
        dx.reserve(capacity);
        dy.reserve(capacity);
        dz.reserve(capacity);
        break;
      }
      case Reference_LongDouble:
      {
        lx.reserve(capacity);
        ly.reserve(capacity);
        lz.reserve(capacity);
        break;
      }
      case Reference_FloatExpFloat:
      {
        fX.reserve(capacity);
        fY.reserve(capacity);
        fZ.reserve(capacity);
        break;
      }
      case Reference_FloatExpDouble:
      {
        dX.reserve(capacity);
        dY.reserve(capacity);
        dZ.reserve(capacity);
        break;
      }
    }
  }
};

Reference *reference_new(const int64_t capacity, const bool strict_zero, const enum Reference_Type reftype)
{
  return new Reference(capacity, strict_zero, reftype);
}

void reference_delete(Reference *R)
{
  delete R;
}

void reference_append(Reference *R, const floatexp &X, const floatexp &Y, const floatexp &Z)
{
  switch (R->reftype)
  {
    case Reference_Float:
    {
      R->fx.push_back(float(X));
      R->fy.push_back(float(Y));
      R->fz.push_back(float(Z));
      break;
    }
    case Reference_Double:
    {
      R->dx.push_back(double(X));
      R->dy.push_back(double(Y));
      R->dz.push_back(double(Z));
      break;
    }
    case Reference_ScaledFloat:
    {
      float x = float(X);
      float y = float(Y);
      float z = float(Z);
      const float e = 1.1754944e-38;
      if ( R->strict_zero
         ? (std::abs(x) <= e || std::abs(y) <= e || std::abs(z) <= e)
         : (std::abs(z) <= e)
         )
      {
        R->fX.push_back(tfloatexp<float, int32_t>(X));
        R->fY.push_back(tfloatexp<float, int32_t>(Y));
        R->fZ.push_back(tfloatexp<float, int32_t>(Z));
        R->fN.push_back(R->fx.size());
      }
      R->fx.push_back(x);
      R->fy.push_back(y);
      R->fz.push_back(z);
    }
    // fall through
    case Reference_ScaledDouble:
    {
      double x = double(X);
      double y = double(Y);
      double z = double(Z);
      const double e = 2.2250738585072014e-308;
      if ( R->strict_zero
         ? (std::abs(x) <= e || std::abs(y) <= e || std::abs(z) <= e)
         : (std::abs(z) <= e)
         )
      {
        R->dX.push_back(X);
        R->dY.push_back(Y);
        R->dZ.push_back(Z);
        R->dN.push_back(R->dx.size());
      }
      R->dx.push_back(x);
      R->dy.push_back(y);
      R->dz.push_back(z);
      break;
    }
    case Reference_LongDouble:
    {
      R->lx.push_back((long double)(X));
      R->ly.push_back((long double)(Y));
      R->lz.push_back((long double)(Z));
      break;
    }
    case Reference_FloatExpFloat:
    {
      R->fX.push_back(tfloatexp<float, int32_t>(X));
      R->fY.push_back(tfloatexp<float, int32_t>(Y));
      R->fZ.push_back(tfloatexp<float, int32_t>(Z));
    }
    // fall through
    case Reference_FloatExpDouble:
    {
      R->dX.push_back(X);
      R->dY.push_back(Y);
      R->dZ.push_back(Z);
      break;
    }
  }
}

enum Reference_Type reference_type(const struct Reference *R)
{
  return R->reftype;
}

int64_t reference_size_x(const Reference *R)
{
  switch (R->reftype)
  {
    case Reference_Float:
    case Reference_ScaledFloat:
      return R->fx.size();
    case Reference_Double:
    case Reference_ScaledDouble:
      return R->dx.size();
    case Reference_LongDouble:
      return R->lx.size();
    case Reference_FloatExpFloat:
      return R->fX.size();
    case Reference_FloatExpDouble:
      return R->dX.size();
  }
  return 0;
}

int64_t reference_size_N(const Reference *R) { return (R->reftype == Reference_Float || R->reftype == Reference_FloatExpFloat || R->reftype == Reference_ScaledFloat) ? R->fN.size() : R->dN.size(); }
template <> const float *reference_ptr_x<float>(const Reference *R) { return (R->reftype == Reference_Float || R->reftype == Reference_ScaledFloat) ? &R->fx[0] : nullptr; }
template <> const float *reference_ptr_y<float>(const Reference *R) { return (R->reftype == Reference_Float || R->reftype == Reference_ScaledFloat) ? &R->fy[0] : nullptr; }
template <> const float *reference_ptr_z<float>(const Reference *R) { return (R->reftype == Reference_Float || R->reftype == Reference_ScaledFloat) ? &R->fz[0] : nullptr; }
template <> const double *reference_ptr_x<double>(const Reference *R) { return (R->reftype == Reference_Double || R->reftype == Reference_ScaledDouble) ? &R->dx[0] : nullptr; }
template <> const double *reference_ptr_y<double>(const Reference *R) { return (R->reftype == Reference_Double || R->reftype == Reference_ScaledDouble) ? &R->dy[0] : nullptr; }
template <> const double *reference_ptr_z<double>(const Reference *R) { return (R->reftype == Reference_Double || R->reftype == Reference_ScaledDouble) ? &R->dz[0] : nullptr; }
template <> const long double *reference_ptr_x<long double>(const Reference *R) { return (R->reftype == Reference_LongDouble) ? &R->lx[0] : nullptr; }
template <> const long double *reference_ptr_y<long double>(const Reference *R) { return (R->reftype == Reference_LongDouble) ? &R->ly[0] : nullptr; }
template <> const long double *reference_ptr_z<long double>(const Reference *R) { return (R->reftype == Reference_LongDouble) ? &R->lz[0] : nullptr; }
template <> const tfloatexp<float, int32_t> *reference_ptr_x<tfloatexp<float, int32_t>>(const Reference *R) { return (R->reftype == Reference_FloatExpFloat) ? &R->fX[0] : nullptr; }
template <> const tfloatexp<float, int32_t> *reference_ptr_y<tfloatexp<float, int32_t>>(const Reference *R) { return (R->reftype == Reference_FloatExpFloat) ? &R->fY[0] : nullptr; }
template <> const tfloatexp<float, int32_t> *reference_ptr_z<tfloatexp<float, int32_t>>(const Reference *R) { return (R->reftype == Reference_FloatExpFloat) ? &R->fZ[0] : nullptr; }
template <> const tfloatexp<double, int64_t> *reference_ptr_x<tfloatexp<double, int64_t>>(const Reference *R) { return (R->reftype == Reference_FloatExpDouble) ? &R->dX[0] : nullptr; }
template <> const tfloatexp<double, int64_t> *reference_ptr_y<tfloatexp<double, int64_t>>(const Reference *R) { return (R->reftype == Reference_FloatExpDouble) ? &R->dY[0] : nullptr; }
template <> const tfloatexp<double, int64_t> *reference_ptr_z<tfloatexp<double, int64_t>>(const Reference *R) { return (R->reftype == Reference_FloatExpDouble) ? &R->dZ[0] : nullptr; }
const int64_t *reference_ptr_N(const Reference *R) { return (R->reftype == Reference_ScaledFloat) ? &R->fN[0] : (R->reftype == Reference_ScaledDouble) ? &R->dN[0] : nullptr; }
template <> const tfloatexp<float, int32_t> *reference_ptr_X<float, int32_t>(const Reference *R) { return (R->reftype == Reference_ScaledFloat) ? &R->fX[0] : nullptr; }
template <> const tfloatexp<float, int32_t> *reference_ptr_Y<float, int32_t>(const Reference *R) { return (R->reftype == Reference_ScaledFloat) ? &R->fY[0] : nullptr; }
template <> const tfloatexp<float, int32_t> *reference_ptr_Z<float, int32_t>(const Reference *R) { return (R->reftype == Reference_ScaledFloat) ? &R->fZ[0] : nullptr; }
template <> const tfloatexp<double, int64_t> *reference_ptr_X<double, int64_t>(const Reference *R) { return (R->reftype == Reference_ScaledDouble) ? &R->dX[0] : nullptr; }
template <> const tfloatexp<double, int64_t> *reference_ptr_Y<double, int64_t>(const Reference *R) { return (R->reftype == Reference_ScaledDouble) ? &R->dY[0] : nullptr; }
template <> const tfloatexp<double, int64_t> *reference_ptr_Z<double, int64_t>(const Reference *R) { return (R->reftype == Reference_ScaledDouble) ? &R->dZ[0] : nullptr; }

void reference_get(const Reference *R, int64_t &k, int64_t n, tfloatexp<double, int64_t> &x, tfloatexp<double, int64_t> &y)
{
  switch (R->reftype)
  {
    case Reference_Float:
    {
      x = tfloatexp<double, int64_t>(R->fx[n]);
      y = tfloatexp<double, int64_t>(R->fy[n]);
      break;
    }
    case Reference_Double:
    {
      x = tfloatexp<double, int64_t>(R->dx[n]);
      y = tfloatexp<double, int64_t>(R->dy[n]);
      break;
    }
    case Reference_LongDouble:
    {
      x = tfloatexp<double, int64_t>(R->lx[n]);
      y = tfloatexp<double, int64_t>(R->ly[n]);
      break;
    }
    case Reference_FloatExpFloat:
#if 0
    {
      x = tfloatexp<double, int64_t>(R->fX[n]);
      y = tfloatexp<double, int64_t>(R->fY[n]);
      break;
    }
#endif
    case Reference_FloatExpDouble:
    {
      x = tfloatexp<double, int64_t>(R->dX[n]);
      y = tfloatexp<double, int64_t>(R->dY[n]);
      break;
    }
    case Reference_ScaledFloat:
#if 0
    {
      if (k < int64_t(R->fN.size()) && R->fN[k] == n)
      {
        x = tfloatexp<double, int64_t>(R->fX[k]);
        y = tfloatexp<double, int64_t>(R->fY[k]);
        ++k;
      }
      else
      {
        x = tfloatexp<double, int64_t>(R->fx[n]);
        y = tfloatexp<double, int64_t>(R->fy[n]);
      }
      break;
    }
#endif
    case Reference_ScaledDouble:
    {
      if (k < int64_t(R->dN.size()) && R->dN[k] == n)
      {
        x = tfloatexp<double, int64_t>(R->dX[k]);
        y = tfloatexp<double, int64_t>(R->dY[k]);
        ++k;
      }
      else
      {
        x = tfloatexp<double, int64_t>(R->dx[n]);
        y = tfloatexp<double, int64_t>(R->dy[n]);
      }
      break;
    }
  }
}