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

#ifndef KF_HYBRID_H
#define KF_HYBRID_H 1

#include <string>
#include <vector>

#include "complex.h"
#include "dual.h"

#include "../formula/formula.h"

// FIXME TODO check that input from KFR files does not exceed this
#define MAX_HYBRID_STANZAS 4

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
inline bool operator==(const hybrid_operator &a, const hybrid_operator &b)
{
  return
    a.abs_x == b.abs_x &&
    a.abs_y == b.abs_y &&
    a.neg_x == b.neg_x &&
    a.neg_y == b.neg_y &&
    a.pow == b.pow &&
    a.mul_re == b.mul_re &&
    a.mul_im == b.mul_im ;
}

enum hybrid_combine
{
  hybrid_combine_add = 0,
  hybrid_combine_sub = 1,
  hybrid_combine_mul = 2
};

struct hybrid_line
{
  hybrid_operator one;
  hybrid_operator two;
  hybrid_combine mode;
};
inline bool operator==(const hybrid_line &a, const hybrid_line &b)
{
  return
    a.one == b.one &&
    a.two == b.two &&
    a.mode == b.mode ;
}

struct hybrid_stanza
{
  std::vector<hybrid_line> lines;
  int repeats;
};
inline bool operator==(const hybrid_stanza &a, const hybrid_stanza &b)
{
  return
    a.lines == b.lines &&
    a.repeats == b.repeats ;
}

struct hybrid_formula
{
  std::vector<hybrid_stanza> stanzas;
  int loop_start;
};
inline bool operator==(const hybrid_formula &a, const hybrid_formula &b)
{
  return
    a.stanzas == b.stanzas &&
    a.loop_start == b.loop_start ;
}

static inline bool valid(const hybrid_formula &h)
{
  if (h.loop_start < 0)
  {
    return false;
  }
  if ((ssize_t) h.stanzas.size() <= h.loop_start)
  {
    return false;
  }
  if (h.stanzas.size() <= 0)
  {
    return false;
  }
  for (auto s : h.stanzas)
  {
    if (s.repeats <= 0)
    {
      return false;
    }
    if (s.lines.size() <= 0)
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
  }
  return complex<R>();
}

template <typename R>
inline complex<R> hybrid_f(const hybrid_stanza &h, complex<R> Z, const complex<R> &C)
{
  const int k = h.lines.size();
  for (int i = 0; i < k; ++i)
  {
    Z = hybrid_f(h.lines[i], Z);
  }
  return Z + C;
}

template <typename R>
inline complex<dual<2,R>> hybrid_pf(const hybrid_operator &h, const complex<R> &Z, const complex<dual<2, R>> &z)
{
  using std::pow;
  if (h.pow == 0)
  {
    return complex<dual<2, R>>();
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
  complex<dual<2, R>> S((complex<dual<2, R>>()));
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
    return complex<R>(R(0), R(0));
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
  complex<R> S((complex<R>()));
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
  }
  return complex<R>();
}

template <typename R>
inline complex<dual<2,R>> hybrid_pf(const hybrid_line &h, const complex<R> &Z, const complex<dual<2,R>> &z)
{
  switch (h.mode)
  {
    case hybrid_combine_add: return hybrid_pf(h.one, Z, z) + hybrid_pf(h.two, Z, z);
    case hybrid_combine_sub: return hybrid_pf(h.one, Z, z) - hybrid_pf(h.two, Z, z);
    case hybrid_combine_mul: return hybrid_pf(h.one, Z, z) * hybrid_f(h.two, Z + z) + hybrid_f(h.one, Z) * hybrid_pf(h.two, Z, z);
  }
  return complex<dual<2,R>>();
}

template <typename R>
inline complex<R> hybrid_pf(const hybrid_stanza &h, complex<R> Z, complex<R> z, const complex<R> &c)
{
  const int k = h.lines.size();
  for (int i = 0; i < k; ++i)
  {
    z = hybrid_pf(h.lines[i], Z, z);
    Z = hybrid_f(h.lines[i], Z); // space vs work tradeoff; should be fine at low precision as there is no +C ?
  }
  return z + c;
}

template <typename R>
inline complex<dual<2,R>> hybrid_pf(const hybrid_stanza &h, complex<R> Z, complex<dual<2,R>> z, const complex<dual<2,R>> &c)
{
  const int k = h.lines.size();
  for (int i = 0; i < k; ++i)
  {
    z = hybrid_pf(h.lines[i], Z, z);
    Z = hybrid_f(h.lines[i], Z); // space vs work tradeoff; should be fine at low precision as there is no +C ?
  }
  return z + c;
}

static inline int hybrid_power_inf(const hybrid_operator &h)
{
  if (h.mul_re == 0 && h.mul_im == 0)
  {
    return 1;
  }
  return h.pow;
}

static inline int hybrid_power_inf(const hybrid_line &h)
{
  switch (h.mode)
  {
    case hybrid_combine_add:
    case hybrid_combine_sub:
      return std::max(hybrid_power_inf(h.one), hybrid_power_inf(h.two));
    case hybrid_combine_mul:
      return hybrid_power_inf(h.one) * hybrid_power_inf(h.two);
  }
  return 1;
}

static inline int hybrid_power_inf(const hybrid_stanza &h)
{
  int power = 1;
  const int k = h.lines.size();
  for (int i = 0; i < k; ++i)
  {
    power *= hybrid_power_inf(h.lines[i]);
  }
  return power;
}

static inline int hybrid_power_inf(const hybrid_formula &h)
{
  int power = 1;
  const int k = h.stanzas.size();
  for (int i = 0; i < k; ++i)
  {
    power = std::max(power, hybrid_power_inf(h.stanzas[i]));
  }
  return power;
}

template <typename R>
inline bool perturbation_hybrid(const hybrid_formula &h, const Reference *m_Reference, int64_t &antal0, double &test10, double &test20, double &phase0, bool &bGlitch, const double &nBailout2, const int64_t &nMaxIter, const bool &bNoGlitchDetection, const double &g_real, const double &g_imag, const double &p, R &xr0, R &xi0, const R &cr0, const R &ci0, int &power)
{
  const int k = h.stanzas.size();
  if (k == 0)
  {
    return false;
  }
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
  const bool no_g = g_real == 1.0 && g_imag == 1.0 && p == 2.0;
  int64_t antal = antal0;
  double test1 = test10;
  double test2 = test20;
  double phase = phase0;
  const complex<R> c(cr0, ci0);
  R xr = xr0;
  R xi = xi0;
  R Xxr = 0;
  R Xxi = 0;
  int count = 0;
  int stanza = 0;
  const R *Xrd = reference_ptr_x<R>(m_Reference);
  const R *Xid = reference_ptr_y<R>(m_Reference);
  const R *Xzd = reference_ptr_z<R>(m_Reference);
  for (; antal < nMaxIter; ++antal)
  {
    const R Xr = Xrd[antal];
    const R Xi = Xid[antal];
    const R Xz = Xzd[antal];
    Xxr = Xr + xr;
    Xxi = Xi + xi;
    test2 = test1;
    const R ttest1 = Xxr * Xxr + Xxi * Xxi;
    test1 = double(ttest1);
    if (ttest1 < Xz)
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
      if (++count >= h.stanzas[stanza].repeats)
      {
        count = 0;
        if (++stanza >= (ssize_t) h.stanzas.size())
        {
          stanza = h.loop_start;
        }
      }
      z = hybrid_pf(h.stanzas[stanza], Z, z, c);
    }
    xr = z.m_r;
    xi = z.m_i;
  }
  power = hybrid_power_inf(h.stanzas[stanza]);
  antal0 = antal;
  test10 = test1;
  test20 = test2;
  phase0 = phase;
  xr0 = Xxr;
  xi0 = Xxi;
  return true;
}

template <typename R>
inline bool perturbation_dual_hybrid(const hybrid_formula &h, const Reference *m_Reference, int64_t &antal0, double &test10, double &test20, double &phase0, bool &bGlitch, const double &nBailout2, const int64_t &nMaxIter, const bool &bNoGlitchDetection, const double &g_real, const double &g_imag, const double &p, dual<2, R> &xr0, dual<2, R> &xi0, const dual<2, R> &cr0, const dual<2, R> &ci0, int &power)
{
  const int k = h.stanzas.size();
  if (k == 0)
  {
    return false;
  }
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
  const bool no_g = g_real == 1.0 && g_imag == 1.0 && p == 2.0;
  int64_t antal = antal0;
  double test1 = test10;
  double test2 = test20;
  double phase = phase0;
  const complex<dual<2, R>> c(cr0, ci0);
  dual<2, R> xr = xr0;
  dual<2, R> xi = xi0;
  dual<2, R> Xxr = 0;
  dual<2, R> Xxi = 0;
  int count = 0;
  int stanza = 0;
  const R *Xrd = reference_ptr_x<R>(m_Reference);
  const R *Xid = reference_ptr_y<R>(m_Reference);
  const R *Xzd = reference_ptr_z<R>(m_Reference);
  for (; antal < nMaxIter; ++antal)
  {
    const R Xr = Xrd[antal];
    const R Xi = Xid[antal];
    const R Xz = Xzd[antal];
    const R Xxr1 = Xr + xr.x;
    const R Xxi1 = Xi + xi.x;
    test2 = test1;
    const R ttest1 = Xxr1 * Xxr1 + Xxi1 * Xxi1;
    test1 = double(ttest1);
    if (ttest1 < Xz)
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
      if (++count >= h.stanzas[stanza].repeats)
      {
        count = 0;
        if (++stanza >= (ssize_t) h.stanzas.size())
        {
          stanza = h.loop_start;
        }
      }
      z = hybrid_pf(h.stanzas[stanza], Z, z, c);
    }
    if (antal == nMaxIter - 1)
    {
      Xxr = Xr + xr;
      Xxi = Xi + xi;
    }
    xr = z.m_r;
    xi = z.m_i;
  }
  power = hybrid_power_inf(h.stanzas[stanza]);
  antal0 = antal;
  test10 = test1;
  test20 = test2;
  phase0 = phase;
  xr0 = Xxr;
  xi0 = Xxi;
  return true;
}

inline bool reference
  ( const hybrid_formula &h
  , Reference *m_Reference
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
    int power = hybrid_power_inf(h);
    double glitches[] = { 1e-7, 1e-6, 1e-5, 1e-4, 1e-4, 1e-3, 1e-3, 1e-2 };
    double glitch = glitches[std::min(std::max(0, power - 2), 7)];
    if (m_bGlitchLowTolerance) {
      glitch = std::sqrt(glitch);
    }
    complex<CFixedFloat> C, X;
    mpfr_set(C.m_r.m_f.backend().data(), Cr0.m_f.backend().data(), MPFR_RNDN);
    mpfr_set(C.m_i.m_f.backend().data(), Ci0.m_f.backend().data(), MPFR_RNDN);
    mpfr_set_d(X.m_r.m_f.backend().data(), g_SeedR, MPFR_RNDN);
    mpfr_set_d(X.m_i.m_f.backend().data(), g_SeedI, MPFR_RNDN);
    int count = 0;
    int stanza = 0;
    for (i = 0; i < nMaxIter && !m_bStop; ++i)
    {
      X = hybrid_f(h.stanzas[stanza], X, C); // formula
      if (++count >= h.stanzas[stanza].repeats)
      {
        count = 0;
        if (++stanza >= (ssize_t) h.stanzas.size())
        {
          stanza = h.loop_start;
        }
      }
      m_nRDone++;
      const floatexp Xrd = mpfr_get_fe(X.m_r.m_f.backend().data());
      const floatexp Xid = mpfr_get_fe(X.m_i.m_f.backend().data());
      const floatexp abs_val = Xrd * Xrd + Xid * Xid;
      const floatexp Xz = abs_val * glitch;
      reference_append(m_Reference, Xrd, Xid, Xz);
      if (double(abs_val) >= terminate){
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
extern bool hybrid_skew(const hybrid_formula &h, int maxiters, const CDecNumber &cr, const CDecNumber &ci, bool useDZ, double *skew_matrix, volatile int *running, int *progress);

extern std::string hybrid_perturbation_double_opencl(const hybrid_formula &h, bool derivatives);
extern std::string hybrid_perturbation_floatexp_opencl(const hybrid_formula &h, bool derivatives);

#endif
