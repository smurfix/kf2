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

#include "matrix.h"

#include <cmath>

mat2 polar_composition(const polar2 &P)
{
  using std::cos;
  using std::sin;
  using glm::transpose;
  const double r = P.rotate;
  const double a = -P.stretch_angle;
  const mat2 R( cos(r), -sin(r), sin(r), cos(r) );
  const mat2 S( P.stretch_factor, 0, 0, 1/P.stretch_factor );
  const mat2 T( cos(a), -sin(a), sin(a), cos(a) );
  const mat2 G( 1, 0, 0, P.sign );
  return R * T * S * transpose(T) * P.scale * G;
}

polar2 polar_decomposition(const mat2 &M)
{
  using std::sqrt;
  using std::abs;
  using std::atan2;
  using glm::inverse;
  using glm::transpose;
  using glm::determinant;
  const double mdet = determinant(M);
  const double sign = mdet > 0 ? 1 : -1;
  const double scale = sqrt(abs(mdet));
  if (scale != 0)
  {
    const mat2 A = M / scale;
    const mat2 B = A + inverse(transpose(A));
    const double b = sqrt(abs(determinant(B)));
    const mat2 V = B / b;
    const double rotate = sign * atan2(V[1][0], V[0][0]);
    const mat2 P = (transpose(A) * A + mat2(1,0,0,1)) / b;
    // [U,D] = eig(P);
    const double pa = P[0][0];
    const double pb = P[0][1];
    const double pc = P[1][0];
    const double pd = P[1][1];
    const double ptr2 = (pa + pd) / 2;
    const double pdet = pa * pd - pb * pc;
    const double pdisc = ptr2 * ptr2 - pdet;
    if (pdisc > 0)
    {
      const double d1 = ptr2 + sqrt(pdisc);
      const double d2 = ptr2 - sqrt(pdisc);
      double ua, ub, uc, ud;
      if (pb != 0)
      {
        ua = pb;
        ub = pb;
        uc = d1 - pa;
        ud = d2 - pa;
      }
      else if (pc != 0)
      {
        ua = d1 - pd;
        ub = d2 - pd;
        uc = pc;
        ud = pc;
      }
      else
      {
        ua = 1;
        ub = 0;
        uc = 0;
        ud = 1;
      }
      double stretch_factor = d1;
      double stretch_angle = sign * atan2(uc, ua);
      (void) ub;
      (void) ud;
      if (stretch_factor < 1)
      {
        stretch_factor = 1 / stretch_factor;
        stretch_angle += 1.5707963267948966;
      }
      return polar2(sign, scale, rotate, stretch_factor, stretch_angle);
    }
    return polar2(sign, scale, rotate, 1, 0);
  }
  return polar2();
}
