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

#ifndef KF_DUAL_H
#define KF_DUAL_H 1

#include "complex.h"

template <int D, typename T>
struct dual
{
  T x;
  T dx[D];
  inline dual() noexcept
  : x(0)
  {
    for (int d = 0; d < D; ++d)
    {
      dx[d] = 0;
    }
  }
  template <typename S>
  inline dual(const S &a) noexcept
  : x(a)
  {
    for (int d = 0; d < D; ++d)
    {
      dx[d] = 0;
    }
  }
  template <typename S>
  explicit inline dual(const dual<D, S> &a) noexcept
  : x(T(a.x))
  {
    for (int d = 0; d < D; ++d)
    {
      dx[d] = T(a.dx[d]);
    }
  }
};

template <int D, typename T>
inline dual<D, T> operator+(const dual<D, T> &a, const dual<D, T> &b) noexcept
{
  dual<D, T> r;
  r.x = a.x + b.x;
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = a.dx[d] + b.dx[d];
  }
  return r;
}

template <int D, typename T, typename S>
inline dual<D, T> operator+(const dual<D, T> &a, const S &b) noexcept
{
  dual<D, T> r;
  r.x = a.x + b;
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = a.dx[d];
  }
  return r;
}

template <int D, typename T, typename S>
inline dual<D, T> operator+(const S &a, const dual<D, T> &b) noexcept
{
  dual<D, T> r;
  r.x = a + b.x;
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = b.dx[d];
  }
  return r;
}
template <int D, typename T, typename S>
dual<D, T> operator-(const S &a, const dual<D, T> &b) noexcept
{
  dual<D, T> r;
  r.x = a - b.x;
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = -b.dx[d];
  }
  return r;
}

template <int D, typename T, typename S>
inline dual<D, T> operator-(const dual<D, T> &a, const S &b) noexcept
{
  dual<D, T> r;
  r.x = a.x - b;
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = a.dx[d];
  }
  return r;
}

template <int D, typename T>
inline dual<D, T> operator-(const dual<D, T> &a, const dual<D, T> &b) noexcept
{
  dual<D, T> r;
  r.x = a.x - b.x;
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = a.dx[d] - b.dx[d];
  }
  return r;
}

template <int D, typename T>
inline dual<D, T> operator-(const dual<D, T> &a, const T &b) noexcept
{
  dual<D, T> r;
  r.x = a.x - b;
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = a.dx[d];
  }
  return r;
}

template <int D, typename T>
inline dual<D, T> operator-(const dual<D, T> &b) noexcept
{
  dual<D, T> r;
  r.x = - b.x;
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = - b.dx[d];
  }
  return r;
}

template <int D, typename T>
inline dual<D, T> operator*(const dual<D, T> &a, const dual<D, T> &b) noexcept
{
  dual<D, T> r;
  r.x = a.x * b.x;
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = a.dx[d] * b.x + a.x * b.dx[d];
  }
  return r;
}

template <int D, typename T>
inline dual<D, T> sqr(const dual<D, T> &a) noexcept
{
  dual<D, T> r;
  r.x = sqr(a.x);
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = 2 * a.dx[d] * a.x;
  }
  return r;
}

template <int D, typename T>
inline dual<D, T> operator/(const dual<D, T> &a, const dual<D, T> &b) noexcept
{
  dual<D, T> r;
  r.x = a.x * b.x;
  T den = 1 / (b.x * b.x);
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = (a.dx[d] * b.x - a.x * b.dx[d]) * den;
  }
  return r;
}

template <int D, typename T, typename S>
inline dual<D, T> operator*(const S &a, const dual<D, T> &b) noexcept
{
  dual<D, T> r;
  r.x = a * b.x;
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = a * b.dx[d];
  }
  return r;
}

template <int D, typename T, typename S>
inline dual<D, T> operator*(const dual<D, T> &a, const S &b) noexcept
{
  dual<D, T> r;
  r.x = a.x * b;
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = a.dx[d] * b;
  }
  return r;
}

template <int D, typename T, typename S>
inline dual<D, T> operator/(const dual<D, T> &a, const S &b) noexcept
{
  dual<D, T> r;
  r.x = a.x / b;
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = a.dx[d] / b;
  }
  return r;
}

template <int D, typename T, typename S>
inline bool operator<(const dual<D, T> &a, const S &b) noexcept
{
  return a.x < b;
}

template <int D, typename T, typename S>
inline bool operator>(const dual<D, T> &a, const S &b) noexcept
{
  return a.x > b;
}

template <int D, typename T, typename S>
inline bool operator<=(const dual<D, T> &a, const S &b) noexcept
{
  return a.x <= b;
}

template <int D, typename T, typename S>
inline bool operator>=(const dual<D, T> &a, const S &b) noexcept
{
  return a.x >= b;
}

template <int D, typename T>
inline bool operator<=(const dual<D, T> &a, const dual<D, T> &b) noexcept
{
  return a.x <= b.x;
}

template <int D, typename T>
inline bool operator>=(const dual<D, T> &a, const dual<D, T> &b) noexcept
{
  return a.x >= b.x;
}

template <int D, typename T>
inline bool operator<(const dual<D, T> &a, const dual<D, T> &b) noexcept
{
  return a.x < b.x;
}

template <int D, typename T>
inline bool operator>(const dual<D, T> &a, const dual<D, T> &b) noexcept
{
  return a.x > b.x;
}

template <int D, typename T>
inline dual<D, T> abs(const dual<D, T> &a) noexcept
{
  return a.x < 0 ? -a : a;
}

template <int D, typename T>
inline dual<D, T> exp(const dual<D, T> &a) noexcept
{
  using std::exp;
  dual<D,T> r;
  r.x = exp(a.x);
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = a.dx[d] * r.x;
  }
  return r;
}

template <int D, typename T>
inline dual<D, T> cos(const dual<D, T> &a) noexcept
{
  using std::cos;
  using std::sin;
  dual<D,T> r;
  r.x = cos(a.x);
  const T s = -sin(a.x);
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = a.dx[d] * s;
  }
  return r;
}

template <int D, typename T>
inline dual<D, T> sin(const dual<D, T> &a) noexcept
{
  using std::cos;
  using std::sin;
  dual<D,T> r;
  r.x = sin(a.x);
  const T c = cos(a.x);
  for (int d = 0; d < D; ++d)
  {
    r.dx[d] = a.dx[d] * c;
  }
  return r;
}

template <int D, typename T, typename S>
inline dual<D, T> &operator-=(dual<D, T> &me, const S &you) noexcept
{
  return me = me - you;
}

template <int D, typename T, typename S>
inline dual<D, T> &operator+=(dual<D, T> &me, const S &you) noexcept
{
  return me = me + you;
}

template <int D, typename T>
inline dual<D, T> diffabs(const T &c, const dual<D, T> &d) noexcept
{
  const T cd = c + d.x;
  const dual<D, T> c2d = 2 * c + d;
  return c >= 0.0 ? cd >= 0.0 ? d : -c2d : cd > 0.0 ? c2d : -d;
}

template <int D, typename T>
inline dual<D, T> diffabs(const dual<D, T> &c, const dual<D, T> &d) noexcept
{
  const T cd = c + d.x;
  const dual<D, T> c2d = 2 * c + d;
  return c >= 0.0 ? cd >= 0.0 ? d : -c2d : cd > 0.0 ? c2d : -d;
}

template <int D, typename R>
inline complex<dual<D, R>> operator+(const complex<R> &a, const complex<dual<D,R>> &b) noexcept
{
	return complex<dual<D, R>>(a.m_r + b.m_r, a.m_i + b.m_i);
}

template <int D, typename R>
inline complex<dual<D, R>> operator-(const complex<R> &a, const complex<dual<D,R>> &b) noexcept
{
	return complex<dual<D, R>>(a.m_r - b.m_r, a.m_i - b.m_i);
}

template <int D, typename R>
inline complex<dual<D, R>> operator*(const complex<dual<D, R>> &a, const complex<R> &b) noexcept
{
	return complex<dual<D, R>>(a.m_r * b.m_r - a.m_i * b.m_i, a.m_r * b.m_i + a.m_i * b.m_r);
}

template <int D, typename R>
inline complex<dual<D, R>> operator*(const complex<R> &a, const complex<dual<D, R>> &b) noexcept
{
	return complex<dual<D, R>>(a.m_r * b.m_r - a.m_i * b.m_i, a.m_r * b.m_i + a.m_i * b.m_r);
}

#endif
