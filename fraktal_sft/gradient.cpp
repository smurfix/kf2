/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2018 Claude Heiland-Allen

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

#include <gsl/gsl_linalg.h>

#if 0

// neighbourhood size
#define S 8

// https://github.com/numpy/numpy/blob/020f2ad4e133b7500c7062d89368d38f19e836fd/numpy/linalg/linalg.py#L1681
// https://www.gnu.org/software/gsl/doc/html/linalg.html#c.gsl_linalg_SV_decomp
static void pinv(double out[S][S], const double in[S][S])
{
  double work_data[S];
  double u[S][S], s[S], v[S][S];
  // u = in
  for (int i = 0; i < S; ++i)
  for (int j = 0; j < S; ++j)
    u[i][j] = in[i][j];
  // initialize GSL
  gsl_vector work = { S, 1, &work_data[0], nullptr, 0 };
  gsl_matrix m_u = { S, S, S, &u[0][0], nullptr, 0 };
  gsl_matrix m_v = { S, S, S, &v[0][0], nullptr, 0 };
  gsl_vector v_s = { S, 1, &s[0], nullptr, 0 };
  // u, s, v = svd(a)
  gsl_linalg_SV_decomp(&m_u, &m_v, &v_s, &work);
  // s = 1/s
  double smax = s[0];
  const double epsilon = 1e-15;
  double cutoff = epsilon * S * smax;
  for (int i = 0; i < S; ++i)
    if (s[i] > cutoff)
      s[i] = 1 / s[i];
    else
      s[i] = 0;
  // su = multiply(s[..., newaxis], transpose(u)))
  double su[S][S];
  for (int i = 0; i < S; ++i)
  for (int j = 0; j < S; ++j)
    su[i][j] = s[i] * u[j][i]; // FIXME check
  // res = matmul(v, su)
  for (int i = 0; i < S; ++i)
  for (int j = 0; j < S; ++j)
  {
    out[i][j] = 0;
    for (int k = 0; k < S; ++k)
      out[i][j] += v[i][k] * su[k][j];
  }
}

// https://stackoverflow.com/questions/40338386/calculating-a-3d-gradient-with-unevenly-spaced-points/40520133#40520133
extern void compute_gradient_3x3(const double p[3][3], const double px[3][3], const double py[3][3], double &dx_out, double &dy_out)
{
  // inputs
  double C[2] = { px[1][1], py[1][1] };
  double Tc = p[1][1];
  double X[S][2]
    = { { px[0][0], py[0][0] }
      , { px[0][1], py[0][1] }
      , { px[0][2], py[0][2] }
      , { px[1][0], py[1][0] }
      , { px[1][2], py[1][2] }
      , { px[2][0], py[2][0] }
      , { px[2][1], py[2][1] }
      , { px[2][2], py[2][2] }
      };
  double T[S]
    = { p[0][0]
      , p[0][1]
      , p[0][2]
      , p[1][0]
      , p[1][2]
      , p[2][0]
      , p[2][1]
      , p[2][2]
      };
  // dX = X - C
  double dX[S][2];
  for (int i = 0; i < S; ++i)
  for (int j = 0; j < 2; ++j)
    dX[i][j] = X[i][j] - C[j];
  // G = dot(dX, dX.T)
  double G[S][S];
  for (int i = 0; i < S; ++i)
  for (int j = 0; j < S; ++j)
    G[i][j] = dX[i][0] * dX[j][0] + dX[i][1] * dX[j][1];
  // F = multiply(G, G)
  double F[S][S];
  for (int i = 0; i < S; ++i)
  for (int j = 0; j < S; ++j)
    F[i][j] = G[i][j] * G[i][j];
  // v = diag(G)
  double v[S];
  for (int i = 0; i < S; ++i)
    v[i] = G[i][i];
  // G1 = pinv(G)
  double G1[S][S];
  pinv(G1, G);
  // N = dot(G1, G)
  double N[S][S];
  for (int i = 0; i < S; ++i)
{
  for (int j = 0; j < S; ++j)
  {
    N[i][j] = 0;
    for (int k = 0; k < S; ++k)
      N[i][j] += G1[i][k] * G[k][j];
}
  }
  // N = eye(k) - N
  for (int i = 0; i < S; ++i)
  for (int j = 0; j < S; ++j)
    N[i][j] = (i == j ? 1 : 0) - N[i][j];
  // FN = dot(F, N)
  double FN[S][S];
  for (int i = 0; i < S; ++i)
  for (int j = 0; j < S; ++j)
  for (int k = 0; k < S; ++k)
  {
    FN[i][j] = 0;
    for (int k = 0; k < S; ++k)
      FN[i][j] += F[i][k] * N[k][j];
  }
  // FN1 = pinv(FN)
  double FN1[S][S];
  pinv(FN1, FN);
  // M = dot(N, FN1)
  double M[S][S];
  for (int i = 0; i < S; ++i)
  for (int j = 0; j < S; ++j)
  {
    M[i][j] = 0;
    for (int k = 0; k < S; ++k)
      M[i][j] += N[i][k] * FN1[k][j];
  }
  // a = dot(M, v)
  double a[S];
  for (int i = 0; i < S; ++i)
  {
    a[i] = 0;
    for (int j = 0; j < S; ++j)
      a[i] += M[i][j] * v[j];
  }
  // dT = T - Tc
  double dT[S];
  for (int i = 0; i < S; ++i)
    dT[i] = T[i] - Tc;
  // dT = multiply(a, dT)
  for (int i = 0; i < S; ++i)
    dT[i] = a[i] * dT[i];
  // y = dot(dX.T, a)
  double y[2];
  for (int i = 0; i < 2; ++i)
  {
    y[i] = 0;
    for (int j = 0; j < S; ++j)
      y[i] += dX[j][i] * dT[j];
  }
  // output
  dx_out = y[0];
  dy_out = y[1];
}

#endif

extern void compute_gradient_2x2(const double p[3][3], const double px[3][3], const double py[3][3], double &dx_out, double &dy_out)
{
  // find weighted average of function values
  double num = 0, den = 0;
  for (int i = 0; i < 2; ++i)
    for (int j = 0; j < 2; ++j)
    {
      double qx = px[i][j] + 0.5;
      double qy = py[i][j] + 0.5;
      double w = 1 / (qx * qx + qy * qy + 1e-100);
      num += w * p[i][j];
      den += w;
    }
  double p0 = num / den;
  // initialize system A x = b for solving x = [ dF/dx ; dF/dy ]
  double A[4][2];
  double b[4];
  int k = 0;
  for (int i = 0; i < 2; ++i)
    for (int j = 0; j < 2; ++j)
    {
      A[k][0] = px[i][j] + 0.5;
      A[k][1] = py[i][j] + 0.5;
      b[k] = p[i][j] - p0;
      ++k;
    }
  // least-squares solve overdetermined A x = b via QR decomposition
  double tau[2] = { 0, 0 };
  double x[2] = { 0, 0 };
  double e[4] = { 0, 0, 0, 0 };
  gsl_matrix A_m   = { 4, 2, 2, &A[0][0], nullptr, 0 };
  gsl_vector b_v   = { 4, 1, &b[0], nullptr, 0 };
  gsl_vector tau_v = { 2, 1, &tau[0], nullptr, 0 };
  gsl_vector x_v   = { 2, 1, &x[0], nullptr, 0 };
  gsl_vector e_v   = { 4, 1, &e[0], nullptr, 0 };
  gsl_linalg_QR_decomp(&A_m, &tau_v);
  gsl_linalg_QR_lssolve(&A_m, &tau_v, &b_v, &x_v, &e_v);
  // output
  dx_out = x[0];
  dy_out = x[1];
}

extern void compute_gradient_3x3(const double p[3][3], const double px[3][3], const double py[3][3], double &dx_out, double &dy_out)
{
  double p0 = p[1][1];
  // initialize system A x = b for solving x = [ dF/dx ; dF/dy ]
  double A[8][2];
  double b[8];
  int k = 0;
  for (int i = 0; i < 3; ++i)
    for (int j = 0; j < 3; ++j)
    {
      if (i == 1 && j == 1) continue;
      A[k][0] = px[i][j] - px[1][1];
      A[k][1] = py[i][j] - py[1][1];
      b[k] = p[i][j] - p0;
      ++k;
    }
  // least-squares solve overdetermined A x = b via QR decomposition
  double tau[2] = { 0, 0 };
  double x[2] = { 0, 0 };
  double e[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
  gsl_matrix A_m   = { 8, 2, 2, &A[0][0], nullptr, 0 };
  gsl_vector b_v   = { 8, 1, &b[0], nullptr, 0 };
  gsl_vector tau_v = { 2, 1, &tau[0], nullptr, 0 };
  gsl_vector x_v   = { 2, 1, &x[0], nullptr, 0 };
  gsl_vector e_v   = { 8, 1, &e[0], nullptr, 0 };
  gsl_linalg_QR_decomp(&A_m, &tau_v);
  gsl_linalg_QR_lssolve(&A_m, &tau_v, &b_v, &x_v, &e_v);
  // output
  dx_out = x[0];
  dy_out = x[1];
}
