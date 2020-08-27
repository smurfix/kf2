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

#ifndef KF_HYBRID_H
#define KF_HYBRID_H 1

#include <string>
#include <vector>

#include "complex.h"
#include "dual.h"

struct hybrid_operator
{
  bool abs_x;
  bool abs_y;
  bool neg_x;
  bool neg_y;
  int pow;
  double mul_re;
  double mul_im;
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
};

typedef std::vector<hybrid_line> hybrid_stanza;
typedef std::vector<hybrid_stanza> hybrid_formula;

static inline bool valid(const hybrid_formula &h)
{
  if (h.size() == 0)
  {
    return false;
  }
  for (auto s : h)
  {
    if (s.size() == 0)
    {
      return false;
    }
  }
  return true;
}

extern std::string to_string(const hybrid_operator &h);
extern std::string to_string(const hybrid_combine &h);
extern std::string to_string(const hybrid_line &h);
extern std::string to_string(const hybrid_stanza &h);
extern std::string to_string(const hybrid_formula &h);

extern hybrid_operator hybrid_operator_from_string(const std::string &s);
extern hybrid_combine hybrid_combine_from_string(const std::string &s);
extern hybrid_line hybrid_line_from_string(const std::string &s);
extern hybrid_stanza hybrid_stanza_from_string(const std::string &s);
extern hybrid_formula hybrid_formula_from_string(const std::string &s);

template <typename R>
inline complex<R> hybrid_f(const hybrid_operator &h, const complex<R> &Z)
{
  using std::abs;
  using std::pow;
  if (h.pow == 0)
  {
    complex<R> a(h.mul_re, h.mul_im);
    return a;
  }
  R x = Z.m_r;
  R y = Z.m_i;
  if (h.abs_x) x = abs(x);
  if (h.abs_y) y = abs(y);
  if (h.neg_x) x = -x;
  if (h.neg_y) y = -y;
  complex<R> W(x, y);
  if (h.mul_re == 1.0 && h.mul_im == 0.0)
  {
    return pow(W, h.pow);
  }
  else
  {
    complex<R> a(h.mul_re, h.mul_im);
    return a * pow(W, h.pow);
  }
}

template <typename R>
inline complex<R> hybrid_f(const hybrid_line &h, const complex<R> &Z)
{
  complex<R> one = hybrid_f(h.one, Z);
  if (h.two.pow == 0 && h.two.mul_re == 0.0 && h.two.mul_im == 0.0 && (h.mode == hybrid_combine_add || h.mode == hybrid_combine_sub))
  {
    return one;
  }
  complex<R> two = hybrid_f(h.two, Z);
  switch (h.mode)
  {
    case hybrid_combine_add: return one + two;
    case hybrid_combine_sub: return one - two;
    case hybrid_combine_mul: return one * two;
    case hybrid_combine_div: return one / two;
  }
  return 0;
}

template <typename R>
inline complex<R> hybrid_f(const hybrid_stanza &h, complex<R> Z, const complex<R> &C)
{
  const int k = h.size();
  for (int i = 0; i < k; ++i)
  {
    Z = hybrid_f(h[i], Z);
  }
  return Z + C;
}

template <typename R>
inline complex<dual<2,R>> hybrid_pf(const hybrid_operator &h, const complex<R> &Z, const complex<dual<2, R>> &z)
{
  using std::pow;
  if (h.pow == 0)
  {
    complex<R> a(h.mul_re, h.mul_im);
    return a;
  }
  R X = Z.m_r;
  R Y = Z.m_i;
  dual<2, R> x = z.m_r;
  dual<2, R> y = z.m_i;
  complex<dual<2, R>> W = Z + z;
  complex<R> B = Z;
  if (h.abs_x)
  {
    x = diffabs(X, x);
    W.m_r = abs(W.m_r);
    B.m_r = abs(B.m_r);
  }
  if (h.abs_y)
  {
    y = diffabs(Y, y);
    W.m_i = abs(W.m_i);
    B.m_i = abs(B.m_i);
  }
  if (h.neg_x)
  {
    x = -x;
    W.m_r = -W.m_r;
    B.m_r = -B.m_r;
  }
  if (h.neg_y)
  {
    y = -y;
    W.m_i = -W.m_i;
    B.m_i = -B.m_i;
  }
  complex<dual<2, R>> P(x, y);
  complex<dual<2, R>> S = 0;
  for (int i = 0; i <= h.pow - 1; ++i)
  {
    int j = h.pow - 1 - i;
    S += pow(W, i) * pow(B, j); // FIXME cache reference with powers? or for SIMD?
  }
  if (h.mul_re == 1.0 && h.mul_im == 0.0)
  {
    return P * S;
  }
  else
  {
    complex<R> a(h.mul_re, h.mul_im);
    return a * P * S;
  }
}

template <typename R>
inline complex<R> hybrid_pf(const hybrid_operator &h, const complex<R> &Z, const complex<R> &z)
{
  complex<R> a(h.mul_re, h.mul_im);
  if (h.pow == 0)
  {
    return a;
  }
  R X = Z.m_r;
  R Y = Z.m_i;
  R x = z.m_r;
  R y = z.m_i;
  complex<R> W = Z + z;
  complex<R> B = Z;
  if (h.abs_x)
  {
    x = diffabs(X, x);
    W.m_r = abs(W.m_r);
    B.m_r = abs(B.m_r);
  }
  if (h.abs_y)
  {
    y = diffabs(Y, y);
    W.m_i = abs(W.m_i);
    B.m_i = abs(B.m_i);
  }
  if (h.neg_x)
  {
    x = -x;
    W.m_r = -W.m_r;
    B.m_r = -B.m_r;
  }
  if (h.neg_y)
  {
    y = -y;
    W.m_i = -W.m_i;
    B.m_i = -B.m_i;
  }
  complex<R> P(x, y);
  complex<R> S = 0;
  for (int i = 0; i <= h.pow - 1; ++i)
  {
    int j = h.pow - 1 - i;
    S += pow(W, i) * pow(B, j); // FIXME cache reference with powers? or for SIMD?
  }
  if (h.mul_re == 1.0 && h.mul_im == 0.0)
  {
    return P * S;
  }
  else
  {
    complex<R> a(h.mul_re, h.mul_im);
    return a * P * S;
  }
}

template <typename R>
inline complex<R> hybrid_pf(const hybrid_line &h, const complex<R> &Z, const complex<R> &z)
{
  if (h.two.pow == 0 && h.two.mul_re == 0.0 && h.two.mul_im == 0.0 && (h.mode == hybrid_combine_add || h.mode == hybrid_combine_sub))
  {
    return hybrid_pf(h.one, Z, z);
  }
  switch (h.mode)
  {
    case hybrid_combine_add: return hybrid_pf(h.one, Z, z) + hybrid_pf(h.two, Z, z);
    case hybrid_combine_sub: return hybrid_pf(h.one, Z, z) - hybrid_pf(h.two, Z, z);
    case hybrid_combine_mul: return hybrid_pf(h.one, Z, z) * hybrid_f(h.two, Z + z) + hybrid_f(h.one, Z) * hybrid_pf(h.two, Z, z);
    case hybrid_combine_div:
    {
      complex<R> B = hybrid_f(h.two, Z);
      return (hybrid_pf(h.one, Z, z) * B - hybrid_f(h.one, Z) * hybrid_pf(h.two, Z, z)) / (B * hybrid_f(h.two, Z + z));
    }
  }
  return 0;
}

template <typename R>
inline complex<dual<2,R>> hybrid_pf(const hybrid_line &h, const complex<R> &Z, const complex<dual<2,R>> &z)
{
  switch (h.mode)
  {
    case hybrid_combine_add: return hybrid_pf(h.one, Z, z) + hybrid_pf(h.two, Z, z);
    case hybrid_combine_sub: return hybrid_pf(h.one, Z, z) - hybrid_pf(h.two, Z, z);
    case hybrid_combine_mul: return hybrid_pf(h.one, Z, z) * hybrid_f(h.two, Z + z) + hybrid_f(h.one, Z) * hybrid_pf(h.two, Z, z);
    case hybrid_combine_div:
    {
      complex<R> B = hybrid_f(h.two, Z);
      return (hybrid_pf(h.one, Z, z) * B - hybrid_f(h.one, Z) * hybrid_pf(h.two, Z, z)) / (B * hybrid_f(h.two, Z + z));
    }
  }
  return 0;
}

template <typename R>
inline complex<R> hybrid_pf(const hybrid_stanza &h, complex<R> Z, complex<R> z, const complex<R> &c)
{
  const int k  = h.size();
  for (int i = 0; i < k; ++i)
  {
    z = hybrid_pf(h[i], Z, z);
    Z = hybrid_f(h[i], Z); // space vs work tradeoff; should be fine at low precision as there is no +C ?
  }
  return z + c;
}

template <typename R>
inline complex<dual<2,R>> hybrid_pf(const hybrid_stanza &h, complex<R> Z, complex<dual<2,R>> z, const complex<dual<2,R>> &c)
{
  const int k  = h.size();
  for (int i = 0; i < k; ++i)
  {
    z = hybrid_pf(h[i], Z, z);
    Z = hybrid_f(h[i], Z); // space vs work tradeoff; should be fine at low precision as there is no +C ?
  }
  return z + c;
}

template <typename R>
inline bool perturbation(const hybrid_formula &h, const R &Cx, const R &Cy, const R *X, const R *Y, const double *G, int64_t &antal0, double &test10, double &test20, double &phase0, bool &bGlitch, const double &nBailout2, const int64_t &nMaxIter, const bool &bNoGlitchDetection, const double &g_real, const double &g_imag, const double &p, R &xr0, R &xi0, const R &cr0, const R &ci0)
{
  const int k = h.size();
  if (k == 0)
  {
    return false;
  }
  const bool simple1 =
    h.size() == 1 &&
    h[0].size() == 1 &&
    h[0][0].mode == hybrid_combine_add &&
    h[0][0].two.mul_re == 0 &&
    h[0][0].two.mul_im == 0;
  const bool simple2 =
    h.size() == 1 &&
    h[0].size() == 2 &&
    h[0][0].mode == hybrid_combine_add &&
    h[0][0].two.mul_re == 0 &&
    h[0][0].two.mul_im == 0 &&
    h[0][1].mode == hybrid_combine_add &&
    h[0][1].two.mul_re == 0 &&
    h[0][1].two.mul_im == 0;
  hybrid_operator op1 = {0}, op2 = {0};
  if (h[0].size() > 0) op1 = h[0][0].one;
  if (h[0].size() > 1) op2 = h[0][1].one;
  const bool no_g = g_real == 1.0 && g_imag == 1.0 && p == 2.0;
  int64_t antal = antal0;
  double test1 = test10;
  double test2 = test20;
  double phase = phase0;
  const complex<R> C(Cx, Cy);
  const complex<R> c(cr0, ci0);
  R xr = xr0;
  R xi = xi0;
  R Xxr = 0;
  R Xxi = 0;
  for (; antal < nMaxIter; ++antal)
  {
    const R Xr = X[antal];
    const R Xi = Y[antal];
    const double Xz = G[antal];
    Xxr = Xr + xr;
    Xxi = Xi + xi;
    test2 = test1;
    test1 = double(Xxr * Xxr + Xxi * Xxi);
    if (test1 < Xz)
    {
      bGlitch = true;
      if (! bNoGlitchDetection)
        break;
    }
    if (! no_g)
    {
      test1 = double(pnorm(g_real, g_imag, p, Xxr, Xxi));
    }
    if (test1 > nBailout2)
    {
      phase = std::atan2(double(Xxi), double(Xxr)) / M_PI / 2;
      phase -= std::floor(phase);
      break;
    }
    complex<R> Z(Xr, Xi);
    complex<R> z(xr, xi);
    if (simple1)
    {
      z = hybrid_pf(op1, Z, z) + c;
    }
    else if (simple2)
    {
      z = hybrid_pf(op2, hybrid_f(op1, Z), hybrid_pf(op1, Z, z)) + c;
    }
    else
    {
      bool glitch = false;
      z = hybrid_pf(h[(antal + 1) % k], Z, z, c);
      if (false && glitch)
      {
        bGlitch = true;
        if (! bNoGlitchDetection)
          break;
      }
    }
    xr = z.m_r;
    xi = z.m_i;
  }
  antal0 = antal;
  test10 = test1;
  test20 = test2;
  phase0 = phase;
  xr0 = Xxr;
  xi0 = Xxi;
  return true;
}

template <typename R>
inline bool perturbation(const hybrid_formula &h, const R &Cx, const R &Cy, const R *X, const R *Y, const double *G, int64_t &antal0, double &test10, double &test20, double &phase0, bool &bGlitch, const double &nBailout2, const int64_t &nMaxIter, const bool &bNoGlitchDetection, const double &g_real, const double &g_imag, const double &p, dual<2, R> &xr0, dual<2, R> &xi0, const dual<2, R> &cr0, const dual<2, R> &ci0)
{
  const int k = h.size();
  if (k == 0)
  {
    return false;
  }
  const bool simple1 =
    h.size() == 1 &&
    h[0].size() == 1 &&
    h[0][0].mode == hybrid_combine_add &&
    h[0][0].two.mul_re == 0 &&
    h[0][0].two.mul_im == 0;
  const bool simple2 =
    h.size() == 1 &&
    h[0].size() == 2 &&
    h[0][0].mode == hybrid_combine_add &&
    h[0][0].two.mul_re == 0 &&
    h[0][0].two.mul_im == 0 &&
    h[0][1].mode == hybrid_combine_add &&
    h[0][1].two.mul_re == 0 &&
    h[0][1].two.mul_im == 0;
  hybrid_operator op1 = {0}, op2 = {0};
  if (h[0].size() > 0) op1 = h[0][0].one;
  if (h[0].size() > 1) op2 = h[0][1].one;
  const bool no_g = g_real == 1.0 && g_imag == 1.0 && p == 2.0;
  int64_t antal = antal0;
  double test1 = test10;
  double test2 = test20;
  double phase = phase0;
  const complex<R> C(Cx, Cy);
  const complex<dual<2, R>> c(cr0, ci0);
  dual<2, R> xr = xr0;
  dual<2, R> xi = xi0;
  dual<2, R> Xxr = 0;
  dual<2, R> Xxi = 0;
  for (; antal < nMaxIter; ++antal)
  {
    const R Xr = X[antal];
    const R Xi = Y[antal];
    const double Xz = G[antal];
    const R Xxr1 = Xr + xr.x;
    const R Xxi1 = Xi + xi.x;
    test2 = test1;
    test1 = double(Xxr1 * Xxr1 + Xxi1 * Xxi1);
    if (test1 < Xz)
    {
      bGlitch = true;
      if (! bNoGlitchDetection)
      {
        Xxr = Xr + xr;
        Xxi = Xi + xi;
        break;
      }
    }
    if (! no_g)
    {
      test1 = double(pnorm(g_real, g_imag, p, Xxr1, Xxi1));
    }
    if (test1 > nBailout2)
    {
      phase = std::atan2(double(Xxi1), double(Xxr1)) / M_PI / 2;
      phase -= std::floor(phase);
      Xxr = Xr + xr;
      Xxi = Xi + xi;
      break;
    }
    complex<R> Z(Xr, Xi);
    complex<dual<2, R>> z(xr, xi);
    if (simple1)
    {
      z = hybrid_pf(op1, Z, z) + c;
    }
    else if (simple2)
    {
      z = hybrid_pf(op2, hybrid_f(op1, Z), hybrid_pf(op1, Z, z)) + c;
    }
    else
    {
      bool glitch = false;
      z = hybrid_pf(h[(antal + 1) % k], Z, z, c);
      if (false && glitch)
      {
        bGlitch = true;
        if (! bNoGlitchDetection)
        {
          Xxr = Xr + xr;
          Xxi = Xi + xi;
          break;
        }
      }
    }
    if (antal == nMaxIter - 1)
    {
      Xxr = Xr + xr;
      Xxi = Xi + xi;
    }
    xr = z.m_r;
    xi = z.m_i;
  }
  antal0 = antal;
  test10 = test1;
  test20 = test2;
  phase0 = phase;
  xr0 = Xxr;
  xi0 = Xxi;
  return true;
}

template <typename R>
inline bool reference
  ( const hybrid_formula &h
  , R *m_db_dxr, R *m_db_dxi, double *m_db_z
  , bool &m_bStop, int64_t &m_nRDone, int64_t &m_nGlitchIter, int64_t &m_nMaxIter
  , const CFixedFloat &Cr0, const CFixedFloat &Ci0
  , const double g_SeedR, const double g_SeedI
  , const double terminate
  , const bool m_bGlitchLowTolerance
  )
{
    m_nGlitchIter = m_nMaxIter + 1;
    int64_t nMaxIter = m_nMaxIter;
    int64_t i;
    double glitch = 1e-6;
    if (m_bGlitchLowTolerance) {
      glitch = std::sqrt(glitch);
    }
    complex<CFixedFloat> C, X;
    mpfr_set(C.m_r.m_f.backend().data(), Cr0.m_f.backend().data(), MPFR_RNDN);
    mpfr_set(C.m_i.m_f.backend().data(), Ci0.m_f.backend().data(), MPFR_RNDN);
    mpfr_set_d(X.m_r.m_f.backend().data(), g_SeedR, MPFR_RNDN);
    mpfr_set_d(X.m_i.m_f.backend().data(), g_SeedI, MPFR_RNDN);
    for (i = 0; i < nMaxIter && !m_bStop; ++i)
    {
      X = hybrid_f(h[i % h.size()], X, C); // formula
      m_nRDone++;
      R Xrd = R(mpfr_get_fe(X.m_r.m_f.backend().data())); // FIXME mpfr_get()
      R Xid = R(mpfr_get_fe(X.m_i.m_f.backend().data())); // FIXME mpfr_get()
      const double abs_val = double(Xrd * Xrd + Xid * Xid);
      const double Xz = abs_val * glitch;
      m_db_dxr[i] = Xrd;
      m_db_dxi[i] = Xid;
      m_db_z[i] = Xz;
      if (abs_val >= terminate){
        if (nMaxIter == m_nMaxIter){
          nMaxIter = i + 3;
          if (nMaxIter > m_nMaxIter)
            nMaxIter = m_nMaxIter;
          m_nGlitchIter = nMaxIter;
        }
      }
    }
    return true;
}

extern INT_PTR WINAPI HybridProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

extern bool hybrid_newton(const hybrid_formula &h, int maxsteps, int period, CDecNumber &cr0, CDecNumber &ci0, const CDecNumber &epsilon2, volatile int *running, int *progress);
extern int hybrid_period(const hybrid_formula &h, int N, const CDecNumber &A, const CDecNumber &B, const CDecNumber &S, const double *K, volatile int *running, int *progress);
extern bool hybrid_size(const hybrid_formula &h, int period, const CDecNumber &A, const CDecNumber &B, CDecNumber &S, double *K, volatile int *running, int *progress);

#endif
