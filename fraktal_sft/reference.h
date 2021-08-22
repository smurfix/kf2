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

#ifndef KF_REFERENCE_H
#define KF_REFERENCE_H 1

#include "floatexp.h"

enum Reference_Type
{
  Reference_Float = 0,
  Reference_Double = 1,
  Reference_LongDouble = 2,
  Reference_ScaledFloat = 3,
  Reference_ScaledDouble = 4,
  Reference_FloatExpFloat = 5,
  Reference_FloatExpDouble = 6
};

struct Reference;
struct Reference *reference_new(const int64_t capacity, const bool strict_zero, const enum Reference_Type reftype, bool convergent, int glitches);
void reference_delete(struct Reference *R);
void reference_append(struct Reference *R, const floatexp &X, const floatexp &Y, const floatexp &Z);
void reference_append(struct Reference *R, const floatexp &X, const floatexp &Y, const floatexp &Wx, const floatexp &Wy);
void reference_append_glitch(struct Reference *R, int index, const floatexp &Z);
enum Reference_Type reference_type(const struct Reference *R);

int64_t reference_size_x(const struct Reference *R);
template <typename T> const T *reference_ptr_x(const struct Reference *R);
template <typename T> const T *reference_ptr_y(const struct Reference *R);
template <typename T> const T *reference_ptr_z(const struct Reference *R, int index = 0);
template <typename T> const T *reference_ptr_wx(const struct Reference *R);
template <typename T> const T *reference_ptr_wy(const struct Reference *R);
int64_t reference_size_N(const struct Reference *R);
const int64_t *reference_ptr_N(const struct Reference *R);
template <typename mantissa, typename exponent> const tfloatexp<mantissa, exponent> *reference_ptr_X(const struct Reference *R);
template <typename mantissa, typename exponent> const tfloatexp<mantissa, exponent> *reference_ptr_Y(const struct Reference *R);
template <typename mantissa, typename exponent> const tfloatexp<mantissa, exponent> *reference_ptr_Z(const struct Reference *R, int index = 0);
template <typename mantissa, typename exponent> const tfloatexp<mantissa, exponent> *reference_ptr_Wx(const struct Reference *R);
template <typename mantissa, typename exponent> const tfloatexp<mantissa, exponent> *reference_ptr_Wy(const struct Reference *R);

template <> const float *reference_ptr_x<float>(const Reference *R);
template <> const float *reference_ptr_y<float>(const Reference *R);
template <> const float *reference_ptr_z<float>(const Reference *R, int index);
template <> const float *reference_ptr_wx<float>(const Reference *R);
template <> const float *reference_ptr_wy<float>(const Reference *R);
template <> const double *reference_ptr_x<double>(const Reference *R);
template <> const double *reference_ptr_y<double>(const Reference *R);
template <> const double *reference_ptr_z<double>(const Reference *R, int index);
template <> const double *reference_ptr_wx<double>(const Reference *R);
template <> const double *reference_ptr_wy<double>(const Reference *R);
template <> const long double *reference_ptr_x<long double>(const Reference *R);
template <> const long double *reference_ptr_y<long double>(const Reference *R);
template <> const long double *reference_ptr_z<long double>(const Reference *R, int index);
template <> const long double *reference_ptr_wx<long double>(const Reference *R);
template <> const long double *reference_ptr_wy<long double>(const Reference *R);
template <> const tfloatexp<float, int32_t> *reference_ptr_x<tfloatexp<float, int32_t>>(const Reference *R);
template <> const tfloatexp<float, int32_t> *reference_ptr_y<tfloatexp<float, int32_t>>(const Reference *R);
template <> const tfloatexp<float, int32_t> *reference_ptr_z<tfloatexp<float, int32_t>>(const Reference *R, int index);
template <> const tfloatexp<float, int32_t> *reference_ptr_wx<tfloatexp<float, int32_t>>(const Reference *R);
template <> const tfloatexp<float, int32_t> *reference_ptr_wy<tfloatexp<float, int32_t>>(const Reference *R);
template <> const tfloatexp<double, int64_t> *reference_ptr_x<tfloatexp<double, int64_t>>(const Reference *R);
template <> const tfloatexp<double, int64_t> *reference_ptr_y<tfloatexp<double, int64_t>>(const Reference *R);
template <> const tfloatexp<double, int64_t> *reference_ptr_z<tfloatexp<double, int64_t>>(const Reference *R, int index);
template <> const tfloatexp<double, int64_t> *reference_ptr_wx<tfloatexp<double, int64_t>>(const Reference *R);
template <> const tfloatexp<double, int64_t> *reference_ptr_wy<tfloatexp<double, int64_t>>(const Reference *R);
template <> const tfloatexp<float, int32_t> *reference_ptr_X<float, int32_t>(const Reference *R);
template <> const tfloatexp<float, int32_t> *reference_ptr_Y<float, int32_t>(const Reference *R);
template <> const tfloatexp<float, int32_t> *reference_ptr_Z<float, int32_t>(const Reference *R, int index);
template <> const tfloatexp<float, int32_t> *reference_ptr_Wx<float, int32_t>(const Reference *R);
template <> const tfloatexp<float, int32_t> *reference_ptr_Wy<float, int32_t>(const Reference *R);
template <> const tfloatexp<double, int64_t> *reference_ptr_X<double, int64_t>(const Reference *R);
template <> const tfloatexp<double, int64_t> *reference_ptr_Y<double, int64_t>(const Reference *R);
template <> const tfloatexp<double, int64_t> *reference_ptr_Z<double, int64_t>(const Reference *R, int index);
template <> const tfloatexp<double, int64_t> *reference_ptr_Wx<double, int64_t>(const Reference *R);
template <> const tfloatexp<double, int64_t> *reference_ptr_Wy<double, int64_t>(const Reference *R);

void reference_get(const Reference *R, int64_t &k, int64_t n, tfloatexp<double, int64_t> &x, tfloatexp<double, int64_t> &y);

#endif
