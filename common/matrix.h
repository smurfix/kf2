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

#ifndef MATRIX_H
#define MATRIX_H 1

#include <glm/glm.hpp>

typedef glm::dvec2 vec2;
typedef glm::dmat2 mat2;

struct polar2
{
  double sign, scale, rotate, stretch_factor, stretch_angle;
  polar2(double g, double s, double r, double sf, double sa)
  : sign(g)
  , scale(s)
  , rotate(r)
  , stretch_factor(sf)
  , stretch_angle(sa)
  { };
  polar2() : polar2(1, 1, 0, 1, 0) { };
};

mat2 polar_composition(const polar2 &P);
polar2 polar_decomposition(const mat2 &M);

#endif
