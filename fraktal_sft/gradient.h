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

#ifndef KF_GRADIENT_H
#define KF_GRADIENT_H 1

extern void compute_gradient_3x3(const double p[3][3], const double px[3][3], const double py[3][3], double &dx_out, double &dy_out);
extern void compute_gradient_2x2(const double p[3][3], const double px[3][3], const double py[3][3], double &dx_out, double &dy_out);

#endif
