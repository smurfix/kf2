/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2021 Claude Heiland-Allen

incorporating components derived from:

et -- escape time fractals
Copyright (C) 2018-2021 Claude Heiland-Allen

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

#ifndef KF_GENERATED_FORMULA_H
#define KF_GENERATED_FORMULA_H 1

#include <assert.h>
#include <math.h>
#include <mpfr.h>

#define abs(x) ((x)<0?-(x):(x))

#define sgn(x) (((x)>=0)-((x)<0))

static inline float diffabsf(float c, float d) {
  if (c >= 0.0f) {
    if (c + d >= 0.0f) { return d; }
    else { return -(2.0f * c + d); }
  } else {
    if (c + d > 0.0f) { return 2.0f * c + d; }
    else { return -d; }
  }
}

static inline double diffabs(double c, double d) {
  if (c >= 0.0) {
    if (c + d >= 0.0) { return d; }
    else { return -(2.0 * c + d); }
  } else {
    if (c + d > 0.0) { return 2.0 * c + d; }
    else { return -d; }
  }
}

static inline long double diffabsl(long double c, long double d) {
  if (c >= 0.0L) {
    if (c + d >= 0.0L) { return d; }
    else { return -(2.0L * c + d); }
  } else {
    if (c + d > 0.0L) { return 2.0L * c + d; }
    else { return -d; }
  }
}

typedef int f_plainf(int,float,float*,float,float,float,float,float*,volatile int*);
typedef int f_plain(int,double,double*,double,double,double,double,double*,volatile int*);
typedef int f_plainl(int,long double,long double*,long double,long double,long double,long double,long double*,volatile int*);
typedef int f_referencef(int,float,float,float,mpfr_t,mpfr_t,float*,volatile int*);
typedef int f_reference(int,double,double,double,mpfr_t,mpfr_t,double*,volatile int*);
typedef int f_referencel(int,long double,long double,long double,mpfr_t,mpfr_t,long double*,volatile int*);
typedef int f_perturbationf(int,int,float,float*,float,float,float,float,float*,float*,volatile int*);
typedef int f_perturbation(int,int,double,double*,double,double,double,double,double*,double*,volatile int*);
typedef int f_perturbationl(int,int,long double,long double*,long double,long double,long double,long double,long double*,long double*,volatile int*);
typedef int f_period_tri(int,double,double,double,mpfr_t,mpfr_t,mpfr_t,volatile int*,int*);
typedef int f_period_jsk(int,double,double,double,mpfr_t,mpfr_t,mpfr_t,double*,volatile int*,int*);
typedef int f_newton(int,int,double,double,mpfr_t,mpfr_t,mpfr_t,volatile int*,int*);
typedef int f_size(int,double,double,mpfr_t,mpfr_t,mpfr_t,double*,volatile int*,int*);
typedef int f_skew(int,double,double,mpfr_t,mpfr_t,int,double*,volatile int*,int*);
typedef int f_domain_size(int,double,double,mpfr_t,mpfr_t,mpfr_t,volatile int*);

#ifndef KF_MAIN
static f_plainf plainf;
static f_plain plain;
static f_plainl plainl;
static f_referencef referencef;
static f_reference reference;
static f_referencel referencel;
static f_perturbationf perturbationf;
static f_perturbation perturbation;
static f_perturbationl perturbationl;
static f_period_tri period_tri;
static f_period_jsk period_jsk;
static f_newton newton;
static f_size size;
static f_skew skew;
static f_domain_size domain_size;
//static const char name[];
//static const char source[];
#endif

#define MAGIC ((int)(0xC01dCaf3))
#define SIZE ((int)(sizeof(struct formula)))
#define VERSION 7

struct formula
{
  int magic;
  int ssize;
  int version;
#if 0
  const char *name;
  const char *source;
  double degree;
  f_plainf *plainf;
  f_plain *plain;
  f_plainl *plainl;
  f_referencef *referencef;
  f_reference *reference;
  f_referencel *referencel;
  f_perturbationf *perturbationf;
  f_perturbation *perturbation;
  f_perturbationl *perturbationl;
#endif
  f_period_tri *period_tri;
  f_period_jsk *period_jsk;
  f_newton *newton;
  f_size *size;
  f_skew *skew;
#if 0
  f_domain_size *domain_size;
#endif
};

#if 1
#define FORMULA(name,source,degree) \
extern "C" { __declspec(dllexport) struct formula et = \
{ MAGIC, SIZE, VERSION \
, &period_tri, &period_jsk, &newton, &size, &skew \
}; }
#else
#define FORMULA(name,source,degree) \
struct formula et = \
{ MAGIC, SIZE, VERSION \
, name, source, degree \
, &plainf, &plain, &plainl \
, &referencef, &reference, &referencel \
, &perturbationf, &perturbation, &perturbationl \
, &period_tri, &period_jsk, &newton, &size, &skew, &domain_size \
};
#endif

#endif
