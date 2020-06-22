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

#ifndef KF_DUAL_H
#define KF_DUAL_H 1

template <int D, typename T>
struct dual
{
  T x;
  T dx[D];
  dual()
  {
    // no init;
  }
  template <typename S>
  dual(const S &a)
  : x(a)
  {
    for (int d = 0; d < D; ++d)
    {
      dx[d] = 0;
    }
  }
  template <typename S>
  dual(const dual<D, S> &a)
  : x(a.x)
  {
    for (int d = 0; d < D; ++d)
    {
      dx[d] = a.dx[d];
    }
  }
};

template <int D, typename T>
dual<D, T> operator+(const dual<D, T> &a, const dual<D, T> &b)
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
dual<D, T> operator+(const dual<D, T> &a, const S &b)
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
dual<D, T> operator+(const S &a, const dual<D, T> &b)
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
dual<D, T> operator-(const S &a, const dual<D, T> &b)
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
dual<D, T> operator-(const dual<D, T> &a, const S &b)
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
dual<D, T> operator-(const dual<D, T> &a, const dual<D, T> &b)
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
dual<D, T> operator-(const dual<D, T> &a, const T &b)
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
dual<D, T> operator-(const dual<D, T> &b)
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
dual<D, T> operator*(const dual<D, T> &a, const dual<D, T> &b)
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
dual<D, T> operator/(const dual<D, T> &a, const dual<D, T> &b)
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
dual<D, T> operator*(const S &a, const dual<D, T> &b)
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
dual<D, T> operator*(const dual<D, T> &a, const S &b)
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
dual<D, T> operator/(const dual<D, T> &a, const S &b)
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
bool operator<(const dual<D, T> &a, const S &b)
{
  return a.x < b;
}

template <int D, typename T, typename S>
bool operator>(const dual<D, T> &a, const S &b)
{
  return a.x > b;
}

template <int D, typename T, typename S>
bool operator<=(const dual<D, T> &a, const S &b)
{
  return a.x <= b;
}

template <int D, typename T, typename S>
bool operator>=(const dual<D, T> &a, const S &b)
{
  return a.x >= b;
}

template <int D, typename T>
bool operator<=(const dual<D, T> &a, const dual<D, T> &b)
{
  return a.x <= b.x;
}

template <int D, typename T>
bool operator>=(const dual<D, T> &a, const dual<D, T> &b)
{
  return a.x >= b.x;
}

template <int D, typename T>
bool operator<(const dual<D, T> &a, const dual<D, T> &b)
{
  return a.x < b.x;
}

template <int D, typename T>
bool operator>(const dual<D, T> &a, const dual<D, T> &b)
{
  return a.x > b.x;
}

template <int D, typename T>
dual<D, T> abs(const dual<D, T> &a)
{
  return a.x < 0 ? -a : a;
}

template <int D, typename T>
dual<D, T> exp(const dual<D, T> &a)
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
dual<D, T> cos(const dual<D, T> &a)
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
dual<D, T> sin(const dual<D, T> &a)
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
dual<D, T> &operator-=(dual<D, T> &me, const S &you)
{
  return me = me - you;
}

template <int D, typename T, typename S>
dual<D, T> &operator+=(dual<D, T> &me, const S &you)
{
  return me = me + you;
}

template <int D, typename T>
dual<D, T> diffabs(const dual<D, T> &c, const dual<D, T> &d)
{
  const dual<D, T> cd = c + d;
  const dual<D, T> c2d = 2 * c + d;
  return c >= 0.0 ? cd >= 0.0 ? d : -c2d : cd > 0.0 ? c2d : -d;
}

#endif
