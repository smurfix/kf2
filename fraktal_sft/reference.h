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

#include <cstdint>

struct floatexp;

struct Reference;
struct Reference *reference_new(const int64_t capacity, const bool strict_zero);
void reference_delete(struct Reference *R);
void reference_append(struct Reference *R, const double x, const double y, const double z);
void reference_append(struct Reference *R, const floatexp &X, const floatexp &Y, const floatexp &Z);
void reference_cook_long_double(struct Reference *R);
void reference_cook_floatexp(struct Reference *R);
bool reference_get(const struct Reference *R, const int64_t n, double &x, double &y, double &z);
bool reference_get(const struct Reference *R, const int64_t k, int64_t &N, floatexp &X, floatexp &Y, floatexp &Z);
int64_t reference_size_x(const struct Reference *R);
template <typename T>
const T *reference_ptr_x(const struct Reference *R);
template <typename T>
const T *reference_ptr_y(const struct Reference *R);
template <typename T>
const T *reference_ptr_z(const struct Reference *R);
int64_t reference_size_N(const struct Reference *R);
const int64_t *reference_ptr_N(const struct Reference *R);
const floatexp *reference_ptr_X(const struct Reference *R);
const floatexp *reference_ptr_Y(const struct Reference *R);
const floatexp *reference_ptr_Z(const struct Reference *R);

template <> const double *reference_ptr_x<double>(const Reference *R);
template <> const double *reference_ptr_y<double>(const Reference *R);
template <> const double *reference_ptr_z<double>(const Reference *R);
template <> const long double *reference_ptr_x<long double>(const Reference *R);
template <> const long double *reference_ptr_y<long double>(const Reference *R);
template <> const long double *reference_ptr_z<long double>(const Reference *R);
template <> const floatexp *reference_ptr_x<floatexp>(const Reference *R);
template <> const floatexp *reference_ptr_y<floatexp>(const Reference *R);
template <> const floatexp *reference_ptr_z<floatexp>(const Reference *R);

#endif
