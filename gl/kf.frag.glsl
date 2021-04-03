// implement this
vec3 colour(void);

///=====================================================================
/// public API

uniform ivec2 ImageSize;

uniform sampler1D KFP_Palette;

uniform vec3 KFP_InteriorColor;

uniform bool KFP_ShowGlitches;

uniform uvec2 KFP_Iterations;
uniform uvec2 KFP_IterationsMin;
uniform uvec2 KFP_IterationsMax;

uniform uint KFP_JitterSeed;
uniform int KFP_JitterShape;
uniform float KFP_JitterScale;

uniform float KFP_IterDiv;
uniform float KFP_ColorOffset;

uniform int KFP_ColorMethod;
#define ColorMethod_Standard 0
#define ColorMethod_SquareRoot 1
#define ColorMethod_CubicRoot 2
#define ColorMethod_Logarithm 3
#define ColorMethod_Stretched 4
#define ColorMethod_DistanceLinear 5
#define ColorMethod_DEPlusStandard 6
#define ColorMethod_DistanceLog 7
#define ColorMethod_DistanceSqrt 8
#define ColorMethod_LogLog 9
#define ColorMethod_ATan 10
#define ColorMethod_FourthRoot 11

uniform int KFP_Differences;
#define Differences_Traditional 0
#define Differences_Forward3x3 1
#define Differences_Central3x3 2
#define Differences_Diagonal2x2 3
#define Differences_LeastSquares2x2 4
#define Differences_LeastSquares3x3 5
#define Differences_Laplacian3x3 6
#define Differences_Analytic 7

uniform float KFP_PhaseColorStrength;

uniform bool KFP_Smooth;
uniform bool KFP_Flat;
uniform bool KFP_InverseTransition;

uniform bool KFP_MultiWavesEnabled;
uniform bool KFP_MultiWavesBlend;
uniform int KFP_MultiWavesCount;
#define KFP_MultiWavesCountMax 32
uniform ivec3 KFP_MultiWaves[KFP_MultiWavesCountMax];

uniform bool KFP_Slopes;
uniform float KFP_SlopePower;
uniform float KFP_SlopeRatio;
uniform vec2 KFP_SlopeDir;

uniform sampler2D KFP_Texture;
uniform bool KFP_TextureEnabled;
uniform float KFP_TextureMerge;
uniform float KFP_TexturePower;
uniform float KFP_TextureRatio;

uniform bool KFP_sRGB;

/// end of public API
///=====================================================================

#if __VERSION__ >= 330
layout(location = 0, index = 0) out vec4 Internal_Colour;
#else
#define Internal_Colour gl_FragColor
#endif

uniform usampler2D Internal_N1;
uniform usampler2D Internal_N0;
uniform sampler2D Internal_NF;
uniform sampler2D Internal_T;
uniform sampler2D Internal_DEX;
uniform sampler2D Internal_DEY;

uniform ivec2 Internal_TilePadding;
uniform ivec2 Internal_TileOrigin;
uniform ivec2 Internal_TileSize;

uniform float Internal_ZoomLog2;

// hack to force explicit evaluation order
uniform float Internal_Zero;
float Internal_One = 1.0;
float EXACT(float a) { return Internal_One * a; }

#if __VERSION__ >= 400
float _builtin_ldexp(float a, int b) { return ldexp(a, b); }
float ldexp(float a, int b) { return _builtin_ldexp(a, b); }
#else
float ldexp(float a, int b) { return a * exp2(b); }
#endif

///=====================================================================
/// overload abs()

int _builtin_abs(int a) { return abs(a); }
ivec2 _builtin_abs(ivec2 a) { return abs(a); }
ivec3 _builtin_abs(ivec3 a) { return abs(a); }
ivec4 _builtin_abs(ivec4 a) { return abs(a); }
float _builtin_abs(float a) { return abs(a); }
vec2 _builtin_abs(vec2 a) { return abs(a); }
vec3 _builtin_abs(vec3 a) { return abs(a); }
vec4 _builtin_abs(vec4 a) { return abs(a); }
#if __VERSION__ >= 400
double _builtin_abs(double a) { return abs(a); }
dvec2 _builtin_abs(dvec2 a) { return abs(a); }
dvec3 _builtin_abs(dvec3 a) { return abs(a); }
dvec4 _builtin_abs(dvec4 a) { return abs(a); }
#endif

int abs(int a) { return _builtin_abs(a); }
ivec2 abs(ivec2 a) { return _builtin_abs(a); }
ivec3 abs(ivec3 a) { return _builtin_abs(a); }
ivec4 abs(ivec4 a) { return _builtin_abs(a); }
float abs(float a) { return _builtin_abs(a); }
vec2 abs(vec2 a) { return _builtin_abs(a); }
vec3 abs(vec3 a) { return _builtin_abs(a); }
vec4 abs(vec4 a) { return _builtin_abs(a); }
#if __VERSION__ >= 400
double abs(double a) { return _builtin_abs(a); }
dvec2 abs(dvec2 a) { return _builtin_abs(a); }
dvec3 abs(dvec3 a) { return _builtin_abs(a); }
dvec4 abs(dvec4 a) { return _builtin_abs(a); }
#endif

///=====================================================================
/// overload sqrt()

float _builtin_sqrt(float a) { return sqrt(a); }
vec2 _builtin_sqrt(vec2 a) { return sqrt(a); }
vec3 _builtin_sqrt(vec3 a) { return sqrt(a); }
vec4 _builtin_sqrt(vec4 a) { return sqrt(a); }
#if __VERSION__ >= 400
double _builtin_sqrt(double a) { return sqrt(a); }
dvec2 _builtin_sqrt(dvec2 a) { return sqrt(a); }
dvec3 _builtin_sqrt(dvec3 a) { return sqrt(a); }
dvec4 _builtin_sqrt(dvec4 a) { return sqrt(a); }
#endif

float sqrt(float a) { return _builtin_sqrt(a); }
vec2 sqrt(vec2 a) { return _builtin_sqrt(a); }
vec3 sqrt(vec3 a) { return _builtin_sqrt(a); }
vec4 sqrt(vec4 a) { return _builtin_sqrt(a); }
#if __VERSION__ >= 400
double sqrt(double a) { return _builtin_sqrt(a); }
dvec2 sqrt(dvec2 a) { return _builtin_sqrt(a); }
dvec3 sqrt(dvec3 a) { return _builtin_sqrt(a); }
dvec4 sqrt(dvec4 a) { return _builtin_sqrt(a); }
#endif

///=====================================================================
/// overload floor()

float _builtin_floor(float a) { return floor(a); }
vec2 _builtin_floor(vec2 a) { return floor(a); }
vec3 _builtin_floor(vec3 a) { return floor(a); }
vec4 _builtin_floor(vec4 a) { return floor(a); }
#if __VERSION__ >= 400
double _builtin_floor(double a) { return floor(a); }
dvec2 _builtin_floor(dvec2 a) { return floor(a); }
dvec3 _builtin_floor(dvec3 a) { return floor(a); }
dvec4 _builtin_floor(dvec4 a) { return floor(a); }
#endif

float floor(float a) { return _builtin_floor(a); }
vec2 floor(vec2 a) { return _builtin_floor(a); }
vec3 floor(vec3 a) { return _builtin_floor(a); }
vec4 floor(vec4 a) { return _builtin_floor(a); }
#if __VERSION__ >= 400
double floor(double a) { return _builtin_floor(a); }
dvec2 floor(dvec2 a) { return _builtin_floor(a); }
dvec3 floor(dvec3 a) { return _builtin_floor(a); }
dvec4 floor(dvec4 a) { return _builtin_floor(a); }
#endif

///=====================================================================
/// overload ceil()

float _builtin_ceil(float a) { return ceil(a); }
vec2 _builtin_ceil(vec2 a) { return ceil(a); }
vec3 _builtin_ceil(vec3 a) { return ceil(a); }
vec4 _builtin_ceil(vec4 a) { return ceil(a); }
#if __VERSION__ >= 400
double _builtin_ceil(double a) { return ceil(a); }
dvec2 _builtin_ceil(dvec2 a) { return ceil(a); }
dvec3 _builtin_ceil(dvec3 a) { return ceil(a); }
dvec4 _builtin_ceil(dvec4 a) { return ceil(a); }
#endif

float ceil(float a) { return _builtin_ceil(a); }
vec2 ceil(vec2 a) { return _builtin_ceil(a); }
vec3 ceil(vec3 a) { return _builtin_ceil(a); }
vec4 ceil(vec4 a) { return _builtin_ceil(a); }
#if __VERSION__ >= 400
double ceil(double a) { return _builtin_ceil(a); }
dvec2 ceil(dvec2 a) { return _builtin_ceil(a); }
dvec3 ceil(dvec3 a) { return _builtin_ceil(a); }
dvec4 ceil(dvec4 a) { return _builtin_ceil(a); }
#endif

///=====================================================================
/// overload exp()

float _builtin_exp(float a) { return exp(a); }
vec2 _builtin_exp(vec2 a) { return exp(a); }
vec3 _builtin_exp(vec3 a) { return exp(a); }
vec4 _builtin_exp(vec4 a) { return exp(a); }

float exp(float a) { return _builtin_exp(a); }
vec2 exp(vec2 a) { return _builtin_exp(a); }
vec3 exp(vec3 a) { return _builtin_exp(a); }
vec4 exp(vec4 a) { return _builtin_exp(a); }

///=====================================================================
/// overload log()

float _builtin_log(float a) { return log(a); }
vec2 _builtin_log(vec2 a) { return log(a); }
vec3 _builtin_log(vec3 a) { return log(a); }
vec4 _builtin_log(vec4 a) { return log(a); }

float log(float a) { return _builtin_log(a); }
vec2 log(vec2 a) { return _builtin_log(a); }
vec3 log(vec3 a) { return _builtin_log(a); }
vec4 log(vec4 a) { return _builtin_log(a); }

///=====================================================================
/// overload sin()

float _builtin_sin(float a) { return sin(a); }
vec2 _builtin_sin(vec2 a) { return sin(a); }
vec3 _builtin_sin(vec3 a) { return sin(a); }
vec4 _builtin_sin(vec4 a) { return sin(a); }

float sin(float a) { return _builtin_sin(a); }
vec2 sin(vec2 a) { return _builtin_sin(a); }
vec3 sin(vec3 a) { return _builtin_sin(a); }
vec4 sin(vec4 a) { return _builtin_sin(a); }

///=====================================================================
/// overload cos()

float _builtin_cos(float a) { return cos(a); }
vec2 _builtin_cos(vec2 a) { return cos(a); }
vec3 _builtin_cos(vec3 a) { return cos(a); }
vec4 _builtin_cos(vec4 a) { return cos(a); }

float cos(float a) { return _builtin_cos(a); }
vec2 cos(vec2 a) { return _builtin_cos(a); }
vec3 cos(vec3 a) { return _builtin_cos(a); }
vec4 cos(vec4 a) { return _builtin_cos(a); }

///=====================================================================
/// overload tan()

float _builtin_tan(float a) { return tan(a); }
vec2 _builtin_tan(vec2 a) { return tan(a); }
vec3 _builtin_tan(vec3 a) { return tan(a); }
vec4 _builtin_tan(vec4 a) { return tan(a); }

float tan(float a) { return _builtin_tan(a); }
vec2 tan(vec2 a) { return _builtin_tan(a); }
vec3 tan(vec3 a) { return _builtin_tan(a); }
vec4 tan(vec4 a) { return _builtin_tan(a); }

///=====================================================================
/// overload sinh()

float _builtin_sinh(float a) { return sinh(a); }
vec2 _builtin_sinh(vec2 a) { return sinh(a); }
vec3 _builtin_sinh(vec3 a) { return sinh(a); }
vec4 _builtin_sinh(vec4 a) { return sinh(a); }

float sinh(float a) { return _builtin_sinh(a); }
vec2 sinh(vec2 a) { return _builtin_sinh(a); }
vec3 sinh(vec3 a) { return _builtin_sinh(a); }
vec4 sinh(vec4 a) { return _builtin_sinh(a); }

///=====================================================================
/// overload cosh()

float _builtin_cosh(float a) { return cosh(a); }
vec2 _builtin_cosh(vec2 a) { return cosh(a); }
vec3 _builtin_cosh(vec3 a) { return cosh(a); }
vec4 _builtin_cosh(vec4 a) { return cosh(a); }

float cosh(float a) { return _builtin_cosh(a); }
vec2 cosh(vec2 a) { return _builtin_cosh(a); }
vec3 cosh(vec3 a) { return _builtin_cosh(a); }
vec4 cosh(vec4 a) { return _builtin_cosh(a); }

///=====================================================================
/// overload tanh()

float _builtin_tanh(float a) { return tanh(a); }
vec2 _builtin_tanh(vec2 a) { return tanh(a); }
vec3 _builtin_tanh(vec3 a) { return tanh(a); }
vec4 _builtin_tanh(vec4 a) { return tanh(a); }

float tanh(float a) { return _builtin_tanh(a); }
vec2 tanh(vec2 a) { return _builtin_tanh(a); }
vec3 tanh(vec3 a) { return _builtin_tanh(a); }
vec4 tanh(vec4 a) { return _builtin_tanh(a); }

///=====================================================================
/// overload asin()

float _builtin_asin(float a) { return asin(a); }
vec2 _builtin_asin(vec2 a) { return asin(a); }
vec3 _builtin_asin(vec3 a) { return asin(a); }
vec4 _builtin_asin(vec4 a) { return asin(a); }

float asin(float a) { return _builtin_asin(a); }
vec2 asin(vec2 a) { return _builtin_asin(a); }
vec3 asin(vec3 a) { return _builtin_asin(a); }
vec4 asin(vec4 a) { return _builtin_asin(a); }

///=====================================================================
/// overload acos()

float _builtin_acos(float a) { return acos(a); }
vec2 _builtin_acos(vec2 a) { return acos(a); }
vec3 _builtin_acos(vec3 a) { return acos(a); }
vec4 _builtin_acos(vec4 a) { return acos(a); }

float acos(float a) { return _builtin_acos(a); }
vec2 acos(vec2 a) { return _builtin_acos(a); }
vec3 acos(vec3 a) { return _builtin_acos(a); }
vec4 acos(vec4 a) { return _builtin_acos(a); }

///=====================================================================
/// overload atan()

float _builtin_atan(float a) { return atan(a); }
vec2 _builtin_atan(vec2 a) { return atan(a); }
vec3 _builtin_atan(vec3 a) { return atan(a); }
vec4 _builtin_atan(vec4 a) { return atan(a); }

float _builtin_atan(float a, float b) { return atan(a, b); }
vec2 _builtin_atan(vec2 a, vec2 b) { return atan(a, b); }
vec3 _builtin_atan(vec3 a, vec3 b) { return atan(a, b); }
vec4 _builtin_atan(vec4 a, vec4 b) { return atan(a, b); }

float atan(float a) { return _builtin_atan(a); }
vec2 atan(vec2 a) { return _builtin_atan(a); }
vec3 atan(vec3 a) { return _builtin_atan(a); }
vec4 atan(vec4 a) { return _builtin_atan(a); }

float atan(float a, float b) { return _builtin_atan(a, b); }
vec2 atan(vec2 a, vec2 b) { return _builtin_atan(a, b); }
vec3 atan(vec3 a, vec3 b) { return _builtin_atan(a, b); }
vec4 atan(vec4 a, vec4 b) { return _builtin_atan(a, b); }

///=====================================================================
/// overload asinh()

float _builtin_asinh(float a) { return asinh(a); }
vec2 _builtin_asinh(vec2 a) { return asinh(a); }
vec3 _builtin_asinh(vec3 a) { return asinh(a); }
vec4 _builtin_asinh(vec4 a) { return asinh(a); }

float asinh(float a) { return _builtin_asinh(a); }
vec2 asinh(vec2 a) { return _builtin_asinh(a); }
vec3 asinh(vec3 a) { return _builtin_asinh(a); }
vec4 asinh(vec4 a) { return _builtin_asinh(a); }

///=====================================================================
/// overload acosh()

float _builtin_acosh(float a) { return acosh(a); }
vec2 _builtin_acosh(vec2 a) { return acosh(a); }
vec3 _builtin_acosh(vec3 a) { return acosh(a); }
vec4 _builtin_acosh(vec4 a) { return acosh(a); }

float acosh(float a) { return _builtin_acosh(a); }
vec2 acosh(vec2 a) { return _builtin_acosh(a); }
vec3 acosh(vec3 a) { return _builtin_acosh(a); }
vec4 acosh(vec4 a) { return _builtin_acosh(a); }

///=====================================================================
/// overload atanh()

float _builtin_atanh(float a) { return atanh(a); }
vec2 _builtin_atanh(vec2 a) { return atanh(a); }
vec3 _builtin_atanh(vec3 a) { return atanh(a); }
vec4 _builtin_atanh(vec4 a) { return atanh(a); }

float atanh(float a) { return _builtin_atanh(a); }
vec2 atanh(vec2 a) { return _builtin_atanh(a); }
vec3 atanh(vec3 a) { return _builtin_atanh(a); }
vec4 atanh(vec4 a) { return _builtin_atanh(a); }

///=====================================================================
/// overload pow()

float _builtin_pow(float a, float b) { return pow(a, b); }
vec2 _builtin_pow(vec2 a, vec2 b) { return pow(a, b); }
vec3 _builtin_pow(vec3 a, vec3 b) { return pow(a, b); }
vec4 _builtin_pow(vec4 a, vec4 b) { return pow(a, b); }

float pow(float a, float b) { return _builtin_pow(a, b); }
vec2 pow(vec2 a, vec2 b) { return _builtin_pow(a, b); }
vec3 pow(vec3 a, vec3 b) { return _builtin_pow(a, b); }
vec4 pow(vec4 a, vec4 b) { return _builtin_pow(a, b); }

///=====================================================================
/// overload max()

float _builtin_max(float a, float b) { return max(a, b); }
vec2 _builtin_max(vec2 a, vec2 b) { return max(a, b); }
vec3 _builtin_max(vec3 a, vec3 b) { return max(a, b); }
vec4 _builtin_max(vec4 a, vec4 b) { return max(a, b); }
vec2 _builtin_max(vec2 a, float b) { return max(a, b); }
vec3 _builtin_max(vec3 a, float b) { return max(a, b); }
vec4 _builtin_max(vec4 a, float b) { return max(a, b); }
int _builtin_max(int a, int b) { return max(a, b); }
ivec2 _builtin_max(ivec2 a, ivec2 b) { return max(a, b); }
ivec3 _builtin_max(ivec3 a, ivec3 b) { return max(a, b); }
ivec4 _builtin_max(ivec4 a, ivec4 b) { return max(a, b); }
ivec2 _builtin_max(ivec2 a, int b) { return max(a, b); }
ivec3 _builtin_max(ivec3 a, int b) { return max(a, b); }
ivec4 _builtin_max(ivec4 a, int b) { return max(a, b); }
uint _builtin_max(uint a, uint b) { return max(a, b); }
uvec2 _builtin_max(uvec2 a, uvec2 b) { return max(a, b); }
uvec3 _builtin_max(uvec3 a, uvec3 b) { return max(a, b); }
uvec4 _builtin_max(uvec4 a, uvec4 b) { return max(a, b); }
uvec2 _builtin_max(uvec2 a, uint b) { return max(a, b); }
uvec3 _builtin_max(uvec3 a, uint b) { return max(a, b); }
uvec4 _builtin_max(uvec4 a, uint b) { return max(a, b); }
#if __VERSION__ >= 400
double _builtin_max(double a, double b) { return max(a, b); }
dvec2 _builtin_max(dvec2 a, dvec2 b) { return max(a, b); }
dvec3 _builtin_max(dvec3 a, dvec3 b) { return max(a, b); }
dvec4 _builtin_max(dvec4 a, dvec4 b) { return max(a, b); }
dvec2 _builtin_max(dvec2 a, double b) { return max(a, b); }
dvec3 _builtin_max(dvec3 a, double b) { return max(a, b); }
dvec4 _builtin_max(dvec4 a, double b) { return max(a, b); }
#endif

float max(float a, float b) { return _builtin_max(a, b); }
vec2 max(vec2 a, vec2 b) { return _builtin_max(a, b); }
vec3 max(vec3 a, vec3 b) { return _builtin_max(a, b); }
vec4 max(vec4 a, vec4 b) { return _builtin_max(a, b); }
vec2 max(vec2 a, float b) { return _builtin_max(a, b); }
vec3 max(vec3 a, float b) { return _builtin_max(a, b); }
vec4 max(vec4 a, float b) { return _builtin_max(a, b); }
int max(int a, int b) { return _builtin_max(a, b); }
ivec2 max(ivec2 a, ivec2 b) { return _builtin_max(a, b); }
ivec3 max(ivec3 a, ivec3 b) { return _builtin_max(a, b); }
ivec4 max(ivec4 a, ivec4 b) { return _builtin_max(a, b); }
ivec2 max(ivec2 a, int b) { return _builtin_max(a, b); }
ivec3 max(ivec3 a, int b) { return _builtin_max(a, b); }
ivec4 max(ivec4 a, int b) { return _builtin_max(a, b); }
uint max(uint a, uint b) { return _builtin_max(a, b); }
uvec2 max(uvec2 a, uvec2 b) { return _builtin_max(a, b); }
uvec3 max(uvec3 a, uvec3 b) { return _builtin_max(a, b); }
uvec4 max(uvec4 a, uvec4 b) { return _builtin_max(a, b); }
uvec2 max(uvec2 a, uint b) { return _builtin_max(a, b); }
uvec3 max(uvec3 a, uint b) { return _builtin_max(a, b); }
uvec4 max(uvec4 a, uint b) { return _builtin_max(a, b); }
#if __VERSION__ >= 400
double max(double a, double b) { return _builtin_max(a, b); }
dvec2 max(dvec2 a, dvec2 b) { return _builtin_max(a, b); }
dvec3 max(dvec3 a, dvec3 b) { return _builtin_max(a, b); }
dvec4 max(dvec4 a, dvec4 b) { return _builtin_max(a, b); }
dvec2 max(dvec2 a, double b) { return _builtin_max(a, b); }
dvec3 max(dvec3 a, double b) { return _builtin_max(a, b); }
dvec4 max(dvec4 a, double b) { return _builtin_max(a, b); }
#endif

///=====================================================================
/// BEGIN qd-2.3.22+dfsg.1
/// via apt-get source qd on Debian Bullseye
///=====================================================================

#if __VERSION__ >= 400
#define QD_FMS(a,b,s) fma(a,b,-s)
#endif

#define QD_IEEE_ADD

struct float49 { float x[2]; };
float49 float49_() { return float49(float[2](0.0, 0.0)); }
float49 float49_(float hi) { return float49(float[2](hi, 0.0)); }
float49 float49_(float hi, float lo) { return float49(float[2](hi, lo)); }

///=====================================================================
/// qd-2.3.22+dfsg.1/COPYING
///=====================================================================

/*
This work was supported by the Director, Office of Science, Division
of Mathematical, Information, and Computational Sciences of the
U.S. Department of Energy under contract numbers DE-AC03-76SF00098 and
DE-AC02-05CH11231.

Copyright (c) 2003-2009, The Regents of the University of California,
through Lawrence Berkeley National Laboratory (subject to receipt of
any required approvals from U.S. Dept. of Energy) All rights reserved.

By downloading or using this software you are agreeing to the modified
BSD license that is in file "BSD-LBNL-License.doc" in the main ARPREC
directory. If you wish to use the software for commercial purposes
please contact the Technology Transfer Department at TTD@lbl.gov or
call 510-286-6457."
*/

///=====================================================================
/// qd-2.3.22+dfsg.1/BSD-LBNL-License.doc
///=====================================================================

/*
1. Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

(1) Redistributions of source code must retain the copyright notice,
this list of conditions and the following disclaimer.

(2) Redistributions in binary form must reproduce the copyright notice,
this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

(3) Neither the name of the University of California, Lawrence Berkeley
National Laboratory, U.S. Dept. of Energy nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

2. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

3. You are under no obligation whatsoever to provide any bug fixes,
patches, or upgrades to the features, functionality or performance of
the source code ("Enhancements") to anyone; however, if you choose to
make your Enhancements available either publicly, or directly to
Lawrence Berkeley National Laboratory, without imposing a separate
written license agreement for such Enhancements, then you hereby grant
the following license: a non-exclusive, royalty-free perpetual license
to install, use, modify, prepare derivative works, incorporate into
other computer software, distribute, and sublicense such enhancements
or derivative works thereof, in binary and source code form.
*/

///=====================================================================
/// qd-2.3.22+dfsg.1/include/qd/inline.h
///=====================================================================

/*
 * include/inline.h
 *
 * This work was supported by the Director, Office of Science, Division
 * of Mathematical, Information, and Computational Sciences of the
 * U.S. Department of Energy under contract number DE-AC03-76SF00098.
 *
 * Copyright (c) 2000-2001
 *
 * This file contains the basic functions used both by double-double
 * and quad-double package.  These are declared as inline functions as
 * they are the smallest building blocks of the double-double and
 * quad-double arithmetic.
 */

#define _QD_SPLITTER     4097.0       // = 2^12 + 1
#define _QD_SPLIT_THRESH 4.1538375e34 // 2^115

const float _d_nan = 0.0 / 0.0;
const float _d_inf = 1.0 / 0.0;

/*
ff :: Double -> (Float, Float)
ff d =
  let hi :: Float
      hi = realToFrac d
  in (hi, realToFrac (d - realToFrac hi))
*/
const float49 f49_nan = float49(float[2](_d_nan, _d_nan));
const float49 f49_inf = float49(float[2](_d_inf, _d_inf));
const float49 f49_0 = float49(float[2](0.0, 0.0));
const float49 f49_1 = float49(float[2](1.0, 0.0));
const float49 f49_e = float49(float[2](2.7182817,8.2548404e-8));
const float49 f49_log2 = float49(float[2](0.6931472,-1.9046542e-9));
const float49 f49_log10 = float49(float[2](2.3025851,-3.1975436e-8));
const float49 f49_2pi = float49(float[2](6.2831855,-1.7484555e-7));
const float49 f49_pi = float49(float[2](3.1415927,-8.742278e-8));
const float49 f49_3pi4 = float49(float[2](2.3561945,-5.9624403e-9));
const float49 f49_pi2 = float49(float[2](1.5707964,-4.371139e-8));
const float49 f49_pi4 = float49(float[2](0.7853982,-2.1855694e-8));
const float49 f49_pi16 = float49(float[2](0.19634955,-5.4639235e-9));

const float f49_eps = 1.4210855e-14; // 2^-46
const float f49_min_normalized = 1.9721523e-31; // 2^(-126 + 24)
const float49 f49_max = float49(float[2](3.4028235e38, 2.0282408e31));
const float49 f49_safe_max = float49(float[2](3.401993e38, 2.0282408e31));
const int f49_ndigits = 13;

/*********** Basic Functions ************/
/* Computes fl(a+b) and err(a+b).  Assumes |a| >= |b|. */
float quick_two_sum(float a, float b, out float err) {
  float s = EXACT(a + b);
  err = EXACT(b - EXACT(s - a));
  return s;
}

/* Computes fl(a-b) and err(a-b).  Assumes |a| >= |b| */
float quick_two_diff(float a, float b, out float err) {
  float s = EXACT(a - b);
  err = EXACT(EXACT(a - s) - b);
  return s;
}

/* Computes fl(a+b) and err(a+b).  */
float two_sum(float a, float b, out float err) {
  float s = EXACT(a + b);
  float bb = EXACT(s - a);
  err = EXACT(EXACT(a - EXACT(s - bb)) + EXACT(b - bb));
  return s;
}

/* Computes fl(a-b) and err(a-b).  */
float two_diff(float a, float b, out float err) {
  float s = EXACT(a - b);
  float bb = EXACT(s - a);
  err = EXACT(EXACT(a - EXACT(s - bb)) - EXACT(b + bb));
  return s;
}

#ifndef QD_FMS
/* Computes high word and lo word of a */
void split(float a, out float hi, out float lo) {
  float temp;
  if (a > _QD_SPLIT_THRESH || a < -_QD_SPLIT_THRESH) {
    a *= 3.7252902984619140625e-09;  // 2^-28
    temp = _QD_SPLITTER * a;
    hi = EXACT(temp - EXACT(temp - a));
    lo = EXACT(a - hi);
    hi *= 268435456.0;          // 2^28
    lo *= 268435456.0;          // 2^28
  } else {
    temp = _QD_SPLITTER * a;
    hi = EXACT(temp - EXACT(temp - a));
    lo = EXACT(a - hi);
  }
}
#endif

/* Computes fl(a*b) and err(a*b). */
float two_prod(float a, float b, out float err) {
#ifdef QD_FMS
  float p = EXACT(a * b);
  err = QD_FMS(a, b, p);
  return p;
#else
  float a_hi, a_lo, b_hi, b_lo;
  float p = EXACT(a * b);
  split(a, a_hi, a_lo);
  split(b, b_hi, b_lo);
  err = EXACT(EXACT(EXACT(a_hi * b_hi) - p) + a_hi * b_lo + a_lo * b_hi) + a_lo * b_lo;
  return p;
#endif
}

/* Computes fl(a*a) and err(a*a).  Faster than the above method. */
float two_sqr(float a, out float err) {
#ifdef QD_FMS
  float p = EXACT(a * a);
  err = QD_FMS(a, a, p);
  return p;
#else
  float hi, lo;
  float q = EXACT(a * a);
  split(a, hi, lo);
  err = EXACT(EXACT(EXACT(hi * hi) - q) + 2.0 * hi * lo) + lo * lo;
  return q;
#endif
}

/* Computes the nearest integer to d. */
float nint(float d) {
  if (d == floor(d))
    return d;
  return floor(d + 0.5);
}

/* Computes the truncated integer. */
float aint(float d) {
  return (d >= 0.0) ? floor(d) : ceil(d);
}

/* These are provided to give consistent
   interface for float with float-float and quad-float. */
void sincosh(float t, out float sinh_t, out float cosh_t) {
  sinh_t = sinh(t);
  cosh_t = cosh(t);
}

float sqr(float t) {
  return t * t;
}

float to_float(float a) { return a; }
int    to_int(float a) { return int(a); }

///=====================================================================
/// qd-2.3.22+dfsg.1/src/dd_const.cpp
///=====================================================================

/*
 * src/dd_const.cc
 *
 * This work was supported by the Director, Office of Science, Division
 * of Mathematical, Information, and Computational Sciences of the
 * U.S. Department of Energy under contract number DE-AC03-76SF00098.
 *
 * Copyright (c) 2000-2007
 */

///=====================================================================
/// qd-2.3.22+dfsg.1/include/qd/dd_inline.h
///=====================================================================

/*
 * include/dd_inline.h
 *
 * This work was supported by the Director, Office of Science, Division
 * of Mathematical, Information, and Computational Sciences of the
 * U.S. Department of Energy under contract number DE-AC03-76SF00098.
 *
 * Copyright (c) 2000-2001
 *
 * Contains small functions (suitable for inlining) in the float-float
 * arithmetic package.
 */

/*********** Additions ************/
/* float-float = float + float */
float49 f49_add(float a, float b) {
  float s, e;
  s = two_sum(a, b, e);
  return float49_(s, e);
}

/* float-float + float */
float49 add(const float49 a, float b) {
  float s1, s2;
  s1 = two_sum(a.x[0], b, s2);
  s2 = EXACT(s2 + a.x[1]);
  s1 = quick_two_sum(s1, s2, s2);
  return float49_(s1, s2);
}

/* float-float + float-float */
float49 ieee_add(const float49 a, const float49 b) {
  /* This one satisfies IEEE style error bound,
     due to K. Briggs and W. Kahan.                   */
  float s1, s2, t1, t2;

  s1 = two_sum(a.x[0], b.x[0], s2);
  t1 = two_sum(a.x[1], b.x[1], t2);
  s2 = EXACT(s2 + t1);
  s1 = quick_two_sum(s1, s2, s2);
  s2 = EXACT(s2 + t2);
  s1 = quick_two_sum(s1, s2, s2);
  return float49_(s1, s2);
}

float49 sloppy_add(const float49 a, const float49 b) {
  /* This is the less accurate version ... obeys Cray-style
     error bound. */
  float s, e;

  s = two_sum(a.x[0], b.x[0], e);
  e = EXACT(e + EXACT(a.x[1] + b.x[1]));
  s = quick_two_sum(s, e, e);
  return float49_(s, e);
}

float49 add(const float49 a, const float49 b) {
#ifndef QD_IEEE_ADD
  return sloppy_add(a, b);
#else
  return ieee_add(a, b);
#endif
}

/* float + float-float */
float49 add(float a, const float49 b) {
  return add(b, a);
}


/*********** Self-Additions ************/
/* float-float += float */
void add_set(inout float49 self, float a) {
  float s1, s2;
  s1 = two_sum(self.x[0], a, s2);
  s2 = EXACT(s2 + self.x[1]);
  self.x[0] = quick_two_sum(s1, s2, self.x[1]);
}

/* float-float += float-float */
void add_set(inout float49 self, const float49 a) {
#ifndef QD_IEEE_ADD
  float s, e;
  s = two_sum(self.x[0], a.x[0], e);
  e = EXACT(e + self.x[1]);
  e = EXACT(e + a.x[1]);
  self.x[0] = quick_two_sum(s, e, self.x[1]);
#else
  float s1, s2, t1, t2;
  s1 = two_sum(self.x[0], a.x[0], s2);
  t1 = two_sum(self.x[1], a.x[1], t2);
  s2 = EXACT(s2 + t1);
  s1 = quick_two_sum(s1, s2, s2);
  s2 = EXACT(s2 + t2);
  self.x[0] = quick_two_sum(s1, s2, self.x[1]);
#endif
}

/*********** Subtractions ************/
/* float-float = float - float */
float49 f49_sub(float a, float b) {
  float s, e;
  s = two_diff(a, b, e);
  return float49_(s, e);
}

/* float-float - float */
float49 sub(const float49 a, float b) {
  float s1, s2;
  s1 = two_diff(a.x[0], b, s2);
  s2 = EXACT(s2 + a.x[1]);
  s1 = quick_two_sum(s1, s2, s2);
  return float49_(s1, s2);
}

/* float-float - float-float */
float49 sub(const float49 a, const float49 b) {
#ifndef QD_IEEE_ADD
  float s, e;
  s = two_diff(a.x[0], b.x[0], e);
  e = EXACT(e + a.x[1]);
  e = EXACT(e - b.x[1]);
  s = quick_two_sum(s, e, e);
  return float49_(s, e);
#else
  float s1, s2, t1, t2;
  s1 = two_diff(a.x[0], b.x[0], s2);
  t1 = two_diff(a.x[1], b.x[1], t2);
  s2 = EXACT(s2 + t1);
  s1 = quick_two_sum(s1, s2, s2);
  s2 = EXACT(s2 + t2);
  s1 = quick_two_sum(s1, s2, s2);
  return float49_(s1, s2);
#endif
}

/* float - float-float */
float49 sub(float a, const float49 b) {
  float s1, s2;
  s1 = two_diff(a, b.x[0], s2);
  s2 = EXACT(s2 - b.x[1]);
  s1 = quick_two_sum(s1, s2, s2);
  return float49_(s1, s2);
}

/*********** Self-Subtractions ************/
/* float-float -= float */
void sub_set(inout float49 self, float a) {
  float s1, s2;
  s1 = two_diff(self.x[0], a, s2);
  s2 = EXACT(s2 + self.x[1]);
  self.x[0] = quick_two_sum(s1, s2, self.x[1]);
}

/* float-float -= float-float */
void sub_set(inout float49 self, const float49 a) {
#ifndef QD_IEEE_ADD
  float s, e;
  s = two_diff(self.x[0], a.x[0], e);
  e = EXACT(e + self.x[1]);
  e = EXACT(e - a.x[1]);
  self.x[0] = quick_two_sum(s, e, self.x[1]);
#else
  float s1, s2, t1, t2;
  s1 = two_diff(self.x[0], a.x[0], s2);
  t1 = two_diff(self.x[1], a.x[1], t2);
  s2 = EXACT(s2 + t1);
  s1 = quick_two_sum(s1, s2, s2);
  s2 = EXACT(s2 + t2);
  self.x[0] = quick_two_sum(s1, s2, self.x[1]);
#endif
}

/*********** Unary Minus ***********/
float49 neg(const float49 a) {
  return float49_(-a.x[0], -a.x[1]);
}

/*********** Multiplications ************/
/* float-float = float * float */
float49 f49_mul(float a, float b) {
  float p, e;
  p = two_prod(a, b, e);
  return float49_(p, e);
}

/* float-float * (2.0 ^ exp) */
float49 ldexp(const float49 a, int exp) {
  return float49_(ldexp(a.x[0], exp), ldexp(a.x[1], exp));
}

/* float-float * float,  where float is a power of 2. */
float49 mul_pwr2(const float49 a, float b) {
  return float49_(a.x[0] * b, a.x[1] * b);
}

/* float-float * float */
float49 mul(const float49 a, float b) {
  float p1, p2;

  p1 = two_prod(a.x[0], b, p2);
  p2 = EXACT(p2 + EXACT(a.x[1] * b));
  p1 = quick_two_sum(p1, p2, p2);
  return float49_(p1, p2);
}

/* float-float * float-float */
float49 mul(const float49 a, const float49 b) {
  float p1, p2;

  p1 = two_prod(a.x[0], b.x[0], p2);
  p2 = EXACT(p2 + EXACT(a.x[0] * b.x[1] + a.x[1] * b.x[0]));
  p1 = quick_two_sum(p1, p2, p2);
  return float49_(p1, p2);
}

/* float * float-float */
float49 mul(float a, const float49 b) {
  return mul(b, a);
}

/*********** Self-Multiplications ************/
/* float-float *= float */
void mul_set(inout float49 self, float a) {
  float p1, p2;
  p1 = two_prod(self.x[0], a, p2);
  p2 = EXACT(p2 + EXACT(self.x[1] * a));
  self.x[0] = quick_two_sum(p1, p2, self.x[1]);
}

/* float-float *= float-float */
void mul_set(inout float49 self, const float49 a) {
  float p1, p2;
  p1 = two_prod(self.x[0], a.x[0], p2);
  p2 = EXACT(p2 + EXACT(a.x[1] * self.x[0]));
  p2 = EXACT(p2 + EXACT(a.x[0] * self.x[1]));
  self.x[0] = quick_two_sum(p1, p2, self.x[1]);
}

/*********** Divisions ************/
float49 f49_div(float a, float b) {
  float q1, q2;
  float p1, p2;
  float s, e;

  q1 = EXACT(a / b);

  /* Compute  a - q1 * b */
  p1 = two_prod(q1, b, p2);
  s = two_diff(a, p1, e);
  e = EXACT(e - p2);

  /* get next approximation */
  q2 = EXACT(EXACT(s + e) / b);

  s = quick_two_sum(q1, q2, e);

  return float49_(s, e);
}

/* float-float / float */
float49 div(const float49 a, float b) {

  float q1, q2;
  float p1, p2;
  float s, e;
  float49 r;

  q1 = EXACT(a.x[0] / b);   /* approximate quotient. */

  /* Compute  this - q1 * d */
  p1 = two_prod(q1, b, p2);
  s = two_diff(a.x[0], p1, e);
  e = EXACT(e + a.x[1]);
  e = EXACT(e - p2);

  /* get next approximation. */
  q2 = EXACT(EXACT(s + e) / b);

  /* renormalize */
  r.x[0] = quick_two_sum(q1, q2, r.x[1]);

  return r;
}

float49 sloppy_div(const float49 a, const float49 b) {
  float s1, s2;
  float q1, q2;
  float49 r;

  q1 = EXACT(a.x[0] / b.x[0]);  /* approximate quotient */

  /* compute  this - q1 * dd */
  r = mul(b, q1);
  s1 = two_diff(a.x[0], r.x[0], s2);
  s2 = EXACT(s2 - r.x[1]);
  s2 = EXACT(s2 + a.x[1]);

  /* get next approximation */
  q2 = EXACT(EXACT(s1 + s2) / b.x[0]);

  /* renormalize */
  r.x[0] = quick_two_sum(q1, q2, r.x[1]);
  return r;
}

float49 accurate_div(const float49 a, const float49 b) {
  float q1, q2, q3;
  float49 r;

  q1 = EXACT(a.x[0] / b.x[0]);  /* approximate quotient */

  r = sub(a, mul(q1, b));

  q2 = EXACT(r.x[0] / b.x[0]);
  sub_set(r, mul(q2, b));

  q3 = EXACT(r.x[0] / b.x[0]);

  q1 = quick_two_sum(q1, q2, q2);
  r = add(float49_(q1, q2), q3);
  return r;
}

/* float-float / float-float */
float49 div(const float49 a, const float49 b) {
#ifdef QD_SLOPPY_DIV
  return sloppy_div(a, b);
#else
  return accurate_div(a, b);
#endif
}

/* float / float-float */
float49 div(float a, const float49 b) {
  return div(float49_(a), b);
}

float49 inv(float49 a) {
  return div(1.0, a);
}

/*********** Self-Divisions ************/
/* float-float /= float */
void div_set(inout float49 self, float a) {
  self = div(self, a);
}

/* float-float /= float-float */
void div_set(inout float49 self, const float49 a) {
  self = div(self, a);
}

/*********** Squaring **********/
float49 sqr(const float49 a) {
  float p1, p2;
  float s1, s2;
  p1 = two_sqr(a.x[0], p2);
  p2 = EXACT(p2 + EXACT(2.0 * a.x[0] * a.x[1]));
  p2 = EXACT(p2 + EXACT(a.x[1] * a.x[1]));
  s1 = quick_two_sum(p1, p2, s2);
  return float49_(s1, s2);
}

float49 f49_sqr(float a) {
  float p1, p2;
  p1 = two_sqr(a, p2);
  return float49_(p1, p2);
}

/*********** Micellaneous ************/
/*  this == 0 */
bool is_zero(const float49 self) {
  return (self.x[0] == 0.0);
}

/*  this == 1 */
bool is_one(const float49 self) {
  return (self.x[0] == 1.0 && self.x[1] == 0.0);
}

/*  this > 0 */
bool is_positive(const float49 self) {
  return (self.x[0] > 0.0);
}

/* this < 0 */
bool is_negative(const float49 self) {
  return (self.x[0] < 0.0);
}

/* Absolute value */
float49 abs(const float49 a) {
  return (a.x[0] < 0.0) ? neg(a) : a;
}

float49 fabs(const float49 a) {
  return abs(a);
}


/* Computes the n-th power of a float-float number.
   NOTE:  0^0 causes an error.                         */
float49 npwr(const float49 a, int n) {

  if (n == 0) {
    if (is_zero(a)) {
      return f49_nan;
    }
    return f49_1;
  }

  float49 r = a;
  float49 s = f49_1;
  int N = abs(n);

  if (N > 1) {
    /* Use binary exponentiation */
    while (N > 0) {
      if (N % 2 == 1) {
        mul_set(s, r);
      }
      N /= 2;
      if (N > 0)
        r = sqr(r);
    }
  } else {
    s = r;
  }

  /* Compute the reciprocal if n is negative. */
  if (n < 0)
    return inv(s);

  return s;
}

/********** Exponentiation **********/
float49 pow(const float49 a, int n) {
  return npwr(a, n);
}


/*********** Assignments ************/
/* float-float = float */
void set(out float49 self, float a) {
  self.x[0] = a;
  self.x[1] = 0.0;
}

/*********** Equality Comparisons ************/
/* float-float == float */
bool eq(const float49 a, float b) {
  return (a.x[0] == b && a.x[1] == 0.0);
}

/* float-float == float-float */
bool eq(const float49 a, const float49 b) {
  return (a.x[0] == b.x[0] && a.x[1] == b.x[1]);
}

/* float == float-float */
bool eq(float a, const float49 b) {
  return (a == b.x[0] && b.x[1] == 0.0);
}

/*********** Greater-Than Comparisons ************/
/* float-float > float */
bool gt(const float49 a, float b) {
  return (a.x[0] > b || (a.x[0] == b && a.x[1] > 0.0));
}

/* float-float > float-float */
bool gt(const float49 a, const float49 b) {
  return (a.x[0] > b.x[0] || (a.x[0] == b.x[0] && a.x[1] > b.x[1]));
}

/* float > float-float */
bool gt(float a, const float49 b) {
  return (a > b.x[0] || (a == b.x[0] && b.x[1] < 0.0));
}

/*********** Less-Than Comparisons ************/
/* float-float < float */
bool lt(const float49 a, float b) {
  return (a.x[0] < b || (a.x[0] == b && a.x[1] < 0.0));
}

/* float-float < float-float */
bool lt(const float49 a, const float49 b) {
  return (a.x[0] < b.x[0] || (a.x[0] == b.x[0] && a.x[1] < b.x[1]));
}

/* float < float-float */
bool lt(float a, const float49 b) {
  return (a < b.x[0] || (a == b.x[0] && b.x[1] > 0.0));
}

/*********** Greater-Than-Or-Equal-To Comparisons ************/
/* float-float >= float */
bool ge(const float49 a, float b) {
  return (a.x[0] > b || (a.x[0] == b && a.x[1] >= 0.0));
}

/* float-float >= float-float */
bool ge(const float49 a, const float49 b) {
  return (a.x[0] > b.x[0] || (a.x[0] == b.x[0] && a.x[1] >= b.x[1]));
}

/*********** Less-Than-Or-Equal-To Comparisons ************/
/* float-float <= float */
bool le(const float49 a, float b) {
  return (a.x[0] < b || (a.x[0] == b && a.x[1] <= 0.0));
}

/* float-float <= float-float */
bool le(const float49 a, const float49 b) {
  return (a.x[0] < b.x[0] || (a.x[0] == b.x[0] && a.x[1] <= b.x[1]));
}

/* float <= float-float */
bool le(float a, const float49 b) {
  return ge(b, a);
}

/* float >= float-float */
bool ge(float a, const float49 b) {
  return le(b, a);
}

/*********** Not-Equal-To Comparisons ************/
/* float-float != float */
bool ne(const float49 a, float b) {
  return (a.x[0] != b || a.x[1] != 0.0);
}

/* float-float != float-float */
bool ne(const float49 a, const float49 b) {
  return (a.x[0] != b.x[0] || a.x[1] != b.x[1]);
}

/* float != float-float */
bool ne(float a, const float49 b) {
  return (a != b.x[0] || b.x[1] != 0.0);
}


float49 max(float a, const float49 b)
{
  if (gt(a, b))
  {
    return float49_(a);
  }
  else
  {
    return b;
  }
}

float49 max(const float49 b, float a)
{
  if (gt(a, b))
  {
    return float49_(a);
  }
  else
  {
    return b;
  }
}

float49 max(const float49 a, const float49 b)
{
  if (gt(a, b))
  {
    return a;
  }
  else
  {
    return b;
  }
}

/* Round to Nearest integer */
float49 nint(const float49 a) {
  float hi = nint(a.x[0]);
  float lo;

  if (hi == a.x[0]) {
    /* High word is an integer already.  Round the low word.*/
    lo = nint(a.x[1]);

    /* Renormalize. This is needed if x[0] = some integer, x[1] = 1/2.*/
    hi = quick_two_sum(hi, lo, lo);
  } else {
    /* High word is not an integer. */
    lo = 0.0;
    if (abs(hi-a.x[0]) == 0.5 && a.x[1] < 0.0) {
      /* There is a tie in the high word, consult the low word
         to break the tie. */
      hi -= 1.0;      /* NOTE: This does not cause INEXACT. */
    }
  }

  return float49_(hi, lo);
}

float49 floor(const float49 a) {
  float hi = floor(a.x[0]);
  float lo = 0.0;

  if (hi == a.x[0]) {
    /* High word is integer already.  Round the low word. */
    lo = floor(a.x[1]);
    hi = quick_two_sum(hi, lo, lo);
  }

  return float49_(hi, lo);
}

float49 ceil(const float49 a) {
  float hi = ceil(a.x[0]);
  float lo = 0.0;

  if (hi == a.x[0]) {
    /* High word is integer already.  Round the low word. */
    lo = ceil(a.x[1]);
    hi = quick_two_sum(hi, lo, lo);
  }

  return float49_(hi, lo);
}

float49 aint(const float49 a) {
  return (a.x[0] >= 0.0) ? floor(a) : ceil(a);
}

/********** Remainder **********/
float49 drem(const float49 a, const float49 b) {
  float49 n = nint(div(a, b));
  return sub(a, mul(n, b));
}

float49 divrem(const float49 a, const float49 b, out float49 r) {
  float49 n = nint(div(a, b));
  r = sub(a, mul(n, b));
  return n;
}

/* Cast to float. */
float to_float(const float49 a) {
  return a.x[0];
}

/* Cast to int. */
int to_int(const float49 a) {
  return int(a.x[0]);
}

///=====================================================================
/// qd-2.3.22+dfsg.1/src/dd_real.cpp
///=====================================================================

/*
 * src/dd_real.cc
 *
 * This work was supported by the Director, Office of Science, Division
 * of Mathematical, Information, and Computational Sciences of the
 * U.S. Department of Energy under contract number DE-AC03-76SF00098.
 *
 * Copyright (c) 2000-2007
 *
 * Contains implementation of non-inlined functions of float-float
 * package.  Inlined functions are found in dd_inline.h (in include directory).
 */

/* Computes the square root of the float-float number dd.
   NOTE: dd must be a non-negative number.                   */
float49 sqrt(const float49 a) {
  /* Strategy:  Use Karp's trick:  if x is an approximation
     to sqrt(a), then

        sqrt(a) = a*x + [a - (a*x)^2] * x / 2   (approx)

     The approximation is accurate to twice the accuracy of x.
     Also, the multiplication (a*x) and [-]*x can be done with
     only half the precision.
  */

  if (is_zero(a))
    return f49_0;

  if (is_negative(a)) {
    return f49_nan;
  }

  float x = EXACT(1.0 / sqrt(a.x[0]));
  float ax = EXACT(a.x[0] * x);
  return f49_add(ax, sub(a, f49_sqr(ax)).x[0] * (x * 0.5));
}

/* Computes the square root of a float in float-float precision.
   NOTE: d must not be negative.                                   */
float49 f49_sqrt(float d) {
  return sqrt(float49_(d));
}

/* Computes the n-th root of the float-float number a.
   NOTE: n must be a positive integer.
   NOTE: If n is even, then a must not be negative.       */
float49 nroot(const float49 a, int n) {
  /* Strategy:  Use Newton iteration for the function

          f(x) = x^(-n) - a

     to find its root a^{-1/n}.  The iteration is thus

          x' = x + x * (1 - a * x^n) / n

     which converges quadratically.  We can then find
    a^{1/n} by taking the reciprocal.
  */

  if (n <= 0) {
    return f49_nan;
  }

  if (n%2 == 0 && is_negative(a)) {
    return f49_nan;
  }

  if (n == 1) {
    return a;
  }
  if (n == 2) {
    return sqrt(a);
  }

  if (is_zero(a))
    return f49_0;

  /* Note  a^{-1/n} = exp(-log(a)/n) */
  float49 r = abs(a);
  float49 x; set(x, exp(-log(r.x[0]) / n));

  /* Perform Newton's iteration. */
  add_set(x, div(mul(x, sub(1.0, mul(r, npwr(x, n)))), float(n)));
  if (a.x[0] < 0.0)
    x = neg(x);
  return inv(x);
}

// mapM_ putStrLn [ "  , float49(float[2](" ++ show (ff (1 / factorial (2 + m))) ++ "))" | m <- [1..15] ]
const int n_inv_fact = 15;
const float49 inv_fact[n_inv_fact] = float49[n_inv_fact]
  ( float49(float[2](0.16666667,-4.967054e-9))
  , float49(float[2](4.1666668e-2,-1.2417635e-9))
  , float49(float[2](8.333334e-3,-4.346172e-10))
  , float49(float[2](1.3888889e-3,-3.3631094e-11))
  , float49(float[2](1.984127e-4,-2.7255969e-12))
  , float49(float[2](2.4801588e-5,-3.406996e-13))
  , float49(float[2](2.7557319e-6,3.7935712e-14))
  , float49(float[2](2.755732e-7,-7.575112e-15))
  , float49(float[2](2.5052108e-8,4.417623e-16))
  , float49(float[2](2.0876756e-9,1.108284e-16))
  , float49(float[2](1.6059044e-10,-5.3525265e-18))
  , float49(float[2](1.1470745e-11,2.3722077e-19))
  , float49(float[2](7.6471636e-13,1.22007105e-20))
  , float49(float[2](4.7794773e-14,7.625444e-22))
  , float49(float[2](2.8114574e-15,-1.0462085e-22))
  );

/* Exponential.  Computes exp(x) in float-float precision. */
float49 exp(const float49 a) {
  /* Strategy:  We first reduce the size of x by noting that

          exp(kr + m * log(2)) = 2^m * exp(r)^k

     where m and k are integers.  By choosing m appropriately
     we can make |kr| <= log(2) / 2 = 0.347.  Then exp(r) is
     evaluated using the familiar Taylor series.  Reducing the
     argument substantially speeds up the convergence.       */

  const float k = 512.0;
  const float inv_k = 1.0 / k;

  if (a.x[0] <= -709.0)
    return f49_0;

  if (a.x[0] >=  709.0)
    return f49_inf;

  if (is_zero(a))
    return f49_1;

  if (is_one(a))
    return f49_e;

  float m = floor(a.x[0] / f49_log2.x[0] + 0.5);
  float49 r = mul_pwr2(sub(a, mul(f49_log2, m)), inv_k);
  float49 s, t, p;

  p = sqr(r);
  s = add(r, mul_pwr2(p, 0.5));
  mul_set(p, r);
  t = mul(p, inv_fact[0]);
  int i = 0;
  do {
    add_set(s, t);
    mul_set(p, r);
    ++i;
    t = mul(p, inv_fact[i]);
  } while (abs(to_float(t)) > inv_k * f49_eps && i < 5);

  add_set(s, t);

  s = add(mul_pwr2(s, 2.0), sqr(s));
  s = add(mul_pwr2(s, 2.0), sqr(s));
  s = add(mul_pwr2(s, 2.0), sqr(s));
  s = add(mul_pwr2(s, 2.0), sqr(s));
  s = add(mul_pwr2(s, 2.0), sqr(s));
  s = add(mul_pwr2(s, 2.0), sqr(s));
  s = add(mul_pwr2(s, 2.0), sqr(s));
  s = add(mul_pwr2(s, 2.0), sqr(s));
  s = add(mul_pwr2(s, 2.0), sqr(s));
  add_set(s, 1.0);

  return ldexp(s, int(m));
}

/* Logarithm.  Computes log(x) in float-float precision.
   This is a natural logarithm (i.e., base e).            */
float49 log(const float49 a) {
  /* Strategy.  The Taylor series for log converges much more
     slowly than that of exp, due to the lack of the factorial
     term in the denominator.  Hence this routine instead tries
     to determine the root of the function

         f(x) = exp(x) - a

     using Newton iteration.  The iteration is given by

         x' = x - f(x)/f'(x)
            = x - (1 - a * exp(-x))
            = x + a * exp(-x) - 1.

     Only one iteration is needed, since Newton's iteration
     approximately doubles the number of digits per iteration. */

  if (is_one(a)) {
    return f49_0;
  }

  if (a.x[0] <= 0.0) {
    return f49_nan;
  }

  float49 x;
  set(x, log(a.x[0]));   /* Initial approximation */

  x = sub(add(x, mul(a, exp(neg(x)))), 1.0);
  return x;
}

float49 log10(const float49 a) {
  return div(log(a), f49_log10);
}

float49 pow(const float49 a, const float49 b) {
  return exp(mul(b, log(a)));
}

/* Table of sin(k * pi/16) and cos(k * pi/16). */
const float49 sin_table[4] = float49[4]
  ( float49(float[2](0.19509032,-1.6704715e-9))
  , float49(float[2](0.38268343,6.2233507e-9))
  , float49(float[2](0.55557024,-1.1769521e-8))
  , float49(float[2](0.70710677,1.21016175e-8))
  );
const float49 cos_table[4] = float49[4]
  ( float49(float[2](0.98078525,2.9739473e-8))
  , float49(float[2](0.9238795,2.830749e-8))
  , float49(float[2](0.8314696,1.6870263e-8))
  , float49(float[2](0.70710677,1.21016175e-8))
  );

/* Computes sin(a) using Taylor series.
   Assumes |a| <= pi/32.                           */
float49 sin_taylor(const float49 a) {
  float thresh = 0.5 * abs(to_float(a)) * f49_eps;
  float49 r, s, t, x;

  if (is_zero(a)) {
    return f49_0;
  }

  int i = 0;
  x = neg(sqr(a));
  s = a;
  r = a;
  do {
    mul_set(r, x);
    t = mul(r, inv_fact[i]);
    add_set(s, t);
    i += 2;
  } while (i < n_inv_fact && abs(to_float(t)) > thresh);

  return s;
}

float49 cos_taylor(const float49 a) {
  const float thresh = 0.5 * f49_eps;
  float49 r, s, t, x;

  if (is_zero(a)) {
    return f49_1;
  }

  x = neg(sqr(a));
  r = x;
  s = add(1.0, mul_pwr2(r, 0.5));
  int i = 1;
  do {
    mul_set(r, x);
    t = mul(r, inv_fact[i]);
    add_set(s, t);
    i += 2;
  } while (i < n_inv_fact && abs(to_float(t)) > thresh);

  return s;
}

void sincos_taylor(const float49 a, out float49 sin_a, out float49 cos_a) {
  if (is_zero(a)) {
    set(sin_a, 0.0);
    set(cos_a, 1.0);
    return;
  }

  sin_a = sin_taylor(a);
  cos_a = sqrt(sub(1.0, sqr(sin_a)));
}


float49 sin(const float49 a) {

  /* Strategy.  To compute sin(x), we choose integers a, b so that

       x = s + a * (pi/2) + b * (pi/16)

     and |s| <= pi/32.  Using the fact that

       sin(pi/16) = 0.5 * sqrt(2 - sqrt(2 + sqrt(2)))

     we can compute sin(x) from sin(s), cos(s).  This greatly
     increases the convergence of the sine Taylor series. */

  if (is_zero(a)) {
    return f49_0;
  }

  // approximately reduce modulo 2*pi
  float49 z = nint(div(a, f49_2pi));
  float49 r = sub(a, mul(f49_2pi, z));

  // approximately reduce modulo pi/2 and then modulo pi/16.
  float49 t;
  float q = floor(r.x[0] / f49_pi2.x[0] + 0.5);
  t = sub(r, mul(f49_pi2, q));
  int j = int(q);
  q = floor(t.x[0] / f49_pi16.x[0] + 0.5);
  sub_set(t, mul(f49_pi16, q));
  int k = int(q);
  int abs_k = abs(k);

  if (j < -2 || j > 2) {
    return f49_nan;
  }

  if (abs_k > 4) {
    return f49_nan;
  }

  if (k == 0) {
    switch (j) {
      case 0:
        return sin_taylor(t);
      case 1:
        return cos_taylor(t);
      case -1:
        return neg(cos_taylor(t));
      default:
        return neg(sin_taylor(t));
    }
  }

  float49 u = cos_table[abs_k-1];
  float49 v = sin_table[abs_k-1];
  float49 sin_t, cos_t;
  sincos_taylor(t, sin_t, cos_t);
  if (j == 0) {
    if (k > 0) {
      r = add(mul(u, sin_t), mul(v, cos_t));
    } else {
      r = sub(mul(u, sin_t), mul(v, cos_t));
    }
  } else if (j == 1) {
    if (k > 0) {
      r = sub(mul(u, cos_t), mul(v, sin_t));
    } else {
      r = add(mul(u, cos_t), mul(v, sin_t));
    }
  } else if (j == -1) {
    if (k > 0) {
      r = sub(mul(v, sin_t), mul(u, cos_t));
    } else if (k < 0) {
      r = neg(add(mul(u, cos_t), mul(v, sin_t)));
    }
  } else {
    if (k > 0) {
      r = neg(add(mul(u, sin_t), mul(v, cos_t)));
    } else {
      r = sub(mul(v, cos_t), mul(u, sin_t));
    }
  }

  return r;
}

float49 cos(const float49 a) {

  if (is_zero(a)) {
    return f49_1;
  }

  // approximately reduce modulo 2*pi
  float49 z = nint(div(a, f49_2pi));
  float49 r = sub(a, mul(z, f49_2pi));

  // approximately reduce modulo pi/2 and then modulo pi/16
  float49 t;
  float q = floor(r.x[0] / f49_pi2.x[0] + 0.5);
  t = sub(r, mul(f49_pi2, q));
  int j = int(q);
  q = floor(t.x[0] / f49_pi16.x[0] + 0.5);
  sub_set(t, mul(f49_pi16, q));
  int k = int(q);
  int abs_k = abs(k);

  if (j < -2 || j > 2) {
    return f49_nan;
  }

  if (abs_k > 4) {
    return f49_nan;
  }

  if (k == 0) {
    switch (j) {
      case 0:
        return cos_taylor(t);
      case 1:
        return neg(sin_taylor(t));
      case -1:
        return sin_taylor(t);
      default:
        return neg(cos_taylor(t));
    }
  }

  float49 sin_t, cos_t;
  sincos_taylor(t, sin_t, cos_t);
  float49 u = cos_table[abs_k-1];
  float49 v = sin_table[abs_k-1];

  if (j == 0) {
    if (k > 0) {
      r = sub(mul(u, cos_t), mul(v, sin_t));
    } else {
      r = add(mul(u, cos_t), mul(v, sin_t));
    }
  } else if (j == 1) {
    if (k > 0) {
      r = neg(add(mul(u, sin_t), mul(v, cos_t)));
    } else {
      r = sub(mul(v, cos_t), mul(u, sin_t));
    }
  } else if (j == -1) {
    if (k > 0) {
      r = add(mul(u, sin_t), mul(v, cos_t));
    } else {
      r = sub(mul(u, sin_t), mul(v, cos_t));
    }
  } else {
    if (k > 0) {
      r = sub(mul(v, sin_t), mul(u, cos_t));
    } else {
      r = neg(add(mul(u, cos_t), mul(v, sin_t)));
    }
  }

  return r;
}

void sincos(const float49 a, out float49 sin_a, out float49 cos_a) {

  if (is_zero(a)) {
    set(sin_a, 0.0);
    set(cos_a, 1.0);
    return;
  }

  // approximately reduce modulo 2*pi
  float49 z = nint(div(a, f49_2pi));
  float49 r = sub(a, mul(f49_2pi, z));

  // approximately reduce module pi/2 and pi/16
  float49 t;
  float q = floor(r.x[0] / f49_pi2.x[0] + 0.5);
  t = sub(r, mul(f49_pi2, q));
  int j = int(q);
  int abs_j = abs(j);
  q = floor(t.x[0] / f49_pi16.x[0] + 0.5);
  sub_set(t, mul(f49_pi16, q));
  int k = int(q);
  int abs_k = abs(k);

  if (abs_j > 2) {
    cos_a = sin_a = f49_nan;
    return;
  }

  if (abs_k > 4) {
    cos_a = sin_a = f49_nan;
    return;
  }

  float49 sin_t, cos_t;
  float49 s, c;

  sincos_taylor(t, sin_t, cos_t);

  if (abs_k == 0) {
    s = sin_t;
    c = cos_t;
  } else {
    float49 u = cos_table[abs_k-1];
    float49 v = sin_table[abs_k-1];

    if (k > 0) {
      s = add(mul(u, sin_t), mul(v, cos_t));
      c = sub(mul(u, cos_t), mul(v, sin_t));
    } else {
      s = sub(mul(u, sin_t), mul(v, cos_t));
      c = add(mul(u, cos_t), mul(v, sin_t));
    }
  }

  if (abs_j == 0) {
    sin_a = s;
    cos_a = c;
  } else if (j == 1) {
    sin_a = c;
    cos_a = neg(s);
  } else if (j == -1) {
    sin_a = neg(c);
    cos_a = s;
  } else {
    sin_a = neg(s);
    cos_a = neg(c);
  }

}

float49 atan(const float49 y, const float49 x) {
  /* Strategy: Instead of using Taylor series to compute
     arctan, we instead use Newton's iteration to solve
     the equation

        sin(z) = y/r    or    cos(z) = x/r

     where r = sqrt(x^2 + y^2).
     The iteration is given by

        z' = z + (y - sin(z)) / cos(z)          (for equation 1)
        z' = z - (x - cos(z)) / sin(z)          (for equation 2)

     Here, x and y are normalized so that x^2 + y^2 = 1.
     If |x| > |y|, then first iteration is used since the
     denominator is larger.  Otherwise, the second is used.
  */

  if (is_zero(x)) {

    if (is_zero(y)) {
      /* Both x and y is zero. */
      return f49_nan;
    }

    return (is_positive(y)) ? f49_pi2 : neg(f49_pi2);
  } else if (is_zero(y)) {
    return (is_positive(x)) ? f49_0 : f49_pi;
  }

  if (eq(x, y)) {
    return (is_positive(y)) ? f49_pi4 : neg(f49_3pi4);
  }

  if (eq(x, neg(y))) {
    return (is_positive(y)) ? f49_3pi4 : neg(f49_pi4);
  }

  float49 r = sqrt(add(sqr(x), sqr(y)));
  float49 xx = div(x, r);
  float49 yy = div(y, r);

  /* Compute float precision approximation to atan. */
  float49 z; set(z, atan(to_float(y), to_float(x)));
  float49 sin_z, cos_z;

  if (abs(xx.x[0]) > abs(yy.x[0])) {
    /* Use Newton iteration 1.  z' = z + (y - sin(z)) / cos(z)  */
    sincos(z, sin_z, cos_z);
    add_set(z, div(sub(yy, sin_z), cos_z));
  } else {
    /* Use Newton iteration 2.  z' = z - (x - cos(z)) / sin(z)  */
    sincos(z, sin_z, cos_z);
    sub_set(z, div(sub(xx, cos_z), sin_z));
  }

  return z;
}

float49 atan(const float49 a) {
  return atan(a, f49_1);
}

float49 tan(const float49 a) {
  float49 s, c;
  sincos(a, s, c);
  return div(s, c);
}

float49 asin(const float49 a) {
  float49 abs_a = abs(a);

  if (gt(abs_a, 1.0)) {
    return f49_nan;
  }

  if (is_one(abs_a)) {
    return (is_positive(a)) ? f49_pi2 : neg(f49_pi2);
  }

  return atan(a, sqrt(sub(1.0, sqr(a))));
}

float49 acos(const float49 a) {
  float49 abs_a = abs(a);

  if (gt(abs_a, 1.0)) {
    return f49_nan;
  }

  if (is_one(abs_a)) {
    return (is_positive(a)) ? f49_0 : f49_pi;
  }

  return atan(sqrt(sub(1.0, sqr(a))), a);
}

float49 sinh(const float49 a) {
  if (is_zero(a)) {
    return f49_0;
  }

  if (gt(abs(a), 0.05)) {
    float49 ea = exp(a);
    return mul_pwr2(sub(ea, inv(ea)), 0.5);
  }

  /* since a is small, using the above formula gives
     a lot of cancellation.  So use Taylor series.   */
  float49 s = a;
  float49 t = a;
  float49 r = sqr(t);
  float m = 1.0;
  float thresh = abs((to_float(a)) * f49_eps);

  do {
    m += 2.0;
    mul_set(t, r);
    div_set(t, (m-1) * m);

    add_set(s, t);
  } while (gt(abs(t), thresh));

  return s;

}

float49 cosh(const float49 a) {
  if (is_zero(a)) {
    return f49_1;
  }

  float49 ea = exp(a);
  return mul_pwr2(add(ea, inv(ea)), 0.5);
}

float49 tanh(const float49 a) {
  if (is_zero(a)) {
    return f49_0;
  }

  if (abs(to_float(a)) > 0.05) {
    float49 ea = exp(a);
    float49 inv_ea = inv(ea);
    return div(sub(ea, inv_ea), add(ea, inv_ea));
  } else {
    float49 s, c;
    s = sinh(a);
    c = sqrt(add(1.0, sqr(s)));
    return div(s, c);
  }
}

void sincosh(const float49 a, out float49 s, out float49 c) {
  if (abs(to_float(a)) <= 0.05) {
    s = sinh(a);
    c = sqrt(add(1.0, sqr(s)));
  } else {
    float49 ea = exp(a);
    float49 inv_ea = inv(ea);
    s = mul_pwr2(sub(ea, inv_ea), 0.5);
    c = mul_pwr2(add(ea, inv_ea), 0.5);
  }
}

float49 asinh(const float49 a) {
  return log(add(a, sqrt(add(sqr(a), 1.0))));
}

float49 acosh(const float49 a) {
  if (lt(a, 1.0)) {
    return f49_nan;
  }

  return log(add(a, sqrt(sub(sqr(a), 1.0))));
}

float49 atanh(const float49 a) {
  if (ge(abs(a), 1.0)) {
    return f49_nan;
  }

  return mul_pwr2(log(div(add(1.0, a), sub(1.0, a))), 0.5);
}

float49 fmod(const float49 a, const float49 b) {
  float49 n = aint(div(a, b));
  return sub(a, mul(b, n));
}

///=====================================================================
/// END qd-2.3.22+dfsg.1
///=====================================================================

const float nan = 0.0 / 0.0;
const float pi = 3.141592653;

float hypot1(float x, float y) { return sqrt(x * x + y * y); }
float hypot2(float x, float y) { return x * x + y * y; }

float srgb2lrgb(float s)
{
  if (s <= 0.04045)
    return s / 12.92;
  return pow((s + 0.055) / 1.055, 2.4);
}

vec3 srgb2lrgb(vec3 s)
{
  return vec3(srgb2lrgb(s.x), srgb2lrgb(s.y), srgb2lrgb(s.z));
}

float lrgb2srgb(float l)
{
  if (l <= 0.0031308)
    return l * 12.92;
  return 1.055  * pow(l, 1.0 / 2.4) - 0.055;
}

vec3 lrgb2srgb(vec3 s)
{
  return vec3(lrgb2srgb(s.x), lrgb2srgb(s.y), lrgb2srgb(s.z));
}

// <http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl>
vec3 hsv2rgb(vec3 c)
{
  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
  vec3 rgb = c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
  if (KFP_sRGB)
  {
    rgb = srgb2lrgb(rgb);
  }
  return rgb;
}

vec3 KF_Palette(float ix)
{
  // map [0..1) to [0..1024)
  ix -= floor(ix);
  ix *= 1024.0;
  // to match KF's regular colouring, need to sin-interpolate c0, c1
  // to get neighbouring colours in 1024-palette
  // and then linear-interpolate those (if smoothing is desired)
  // c0, c1 are neighbouring colours in n-palette with interpolant cf
  int m_nParts = textureSize(KFP_Palette, 0);
  vec3 c0, c1;
  {
    int i = int(floor(ix));
    float temp = float(i) * float(m_nParts) / 1024.0;
    int p = int(temp);
    int pn = (p + 1) % m_nParts;
    temp -= float(p);
    temp = sin((temp - 0.5) * pi) / 2.0 + 0.5;
    c0 = mix
      ( texelFetch(KFP_Palette, p, 0).rgb
      , texelFetch(KFP_Palette, pn, 0).rgb
      , temp
      );
  }
  if (KFP_Smooth)
  {
    int i = (int(floor(ix)) + 1) % 1024;
    float temp = float(i) * float(m_nParts) / 1024.0;
    int p = int(temp);
    int pn = (p + 1) % m_nParts;
    temp -= float(p);
    temp = sin((temp - 0.5) * pi) / 2.0 + 0.5;
    c1 = mix
      ( texelFetch(KFP_Palette, p, 0).rgb
      , texelFetch(KFP_Palette, pn, 0).rgb
      , temp
      );
    ix -= floor(ix);
    return mix(c0, c1, ix);
  }
  else
  {
    return c0;
  }
}

uint hash(uint a)
{
  a = (a+0x7ed55d16u) + (a<<12);
  a = (a^0xc761c23cu) ^ (a>>19);
  a = (a+0x165667b1u) + (a<<5);
  a = (a+0xd3a2646cu) ^ (a<<9);
  a = (a+0xfd7046c5u) + (a<<3);
  a = (a^0xb55a4f09u) ^ (a>>16);
  return a;
}

// uniform in [0,1)
float dither(int x, int y, int c)
{
  return float(hash(uint(x) + hash(uint(y) + hash(uint(c))))) / exp2(32.0);
}

vec2 GetPixelOffset(ivec2 ix)
{
  float x = 0.0, y = 0.0;
  int c = int(KFP_JitterSeed);
  if (c != 0)
  {
    float s = KFP_JitterScale;
    float u = dither(ix.x, ix.y, 2 * c + 0);
    float v = dither(ix.x, ix.y, 2 * c + 1);
    switch (KFP_JitterShape)
    {
      default:
      case 0: // uniform
        {
          x = s * (u - 0.5);
          y = s * (v - 0.5);
        }
        break;
      case 1: // Gaussian
        {
          // https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
          float r = 0.0 < u && u < 1.0 ? sqrt(-2.0 * log(u)) : 0.0;
          float t = 2.0 * 3.141592653589793 * v;
          s *= 0.5;
          x = s * r * cos(t);
          y = s * r * sin(t);
        }
        break;
    }
  }
  else
  {
    x = 0.0;
    y = 0.0;
  }
  return vec2(x, y);
}

float49 cbrt(float49 a) { return nroot(a, 3); }
float49 wrap(float49 a) { return sub(a, floor(a)); }

float49 float49_(uint a)
{
  float hi = float(a);
  float lo = float(a - uint(hi));
  return float49_(hi, lo);
}
float49 float49_(uint a, uint b) { return add(ldexp(float49_(a), 32), float49_(b)); }
float49 float49_(uint a, uint b, float c) { return add(float49_(a, b), c); }

ivec2 Internal_TexCoord = Internal_TilePadding.yx + ivec2(Internal_TileSize.y - 1 - 2 * Internal_TilePadding.y - int(gl_FragCoord.y), int(gl_FragCoord.x));
ivec2 Internal_PixelCoord = Internal_TileOrigin.xy + ivec2(int(gl_FragCoord.x), Internal_TileSize.y - 1 - 2 * Internal_TilePadding.y - int(gl_FragCoord.y));

uint    getN1 (ivec2 offset) { return texelFetch(Internal_N1,  Internal_TexCoord + offset.yx, 0).r; }
uint    getN0 (ivec2 offset) { return texelFetch(Internal_N0,  Internal_TexCoord + offset.yx, 0).r; }
float   getNF (ivec2 offset) { return texelFetch(Internal_NF,  Internal_TexCoord + offset.yx, 0).r; }
float   getT  (ivec2 offset) { return texelFetch(Internal_T,   Internal_TexCoord + offset.yx, 0).r; }
float   getDEX(ivec2 offset) { return texelFetch(Internal_DEX, Internal_TexCoord + offset.yx, 0).r; }
float   getDEY(ivec2 offset) { return texelFetch(Internal_DEY, Internal_TexCoord + offset.yx, 0).r; }
uint    getN1 (void) { return getN1 (ivec2(0, 0)); }
uint    getN0 (void) { return getN0 (ivec2(0, 0)); }
float   getNF (void) { return getNF (ivec2(0, 0)); }
float   getT  (void) { return getT  (ivec2(0, 0)); }
float   getDEX(void) { return getDEX(ivec2(0, 0)); }
float   getDEY(void) { return getDEY(ivec2(0, 0)); }
bool    getGlitch  (ivec2 offset) { return getNF(offset) < 0.0; }
bool    getInterior(ivec2 offset) { return uvec2(getN0(offset), getN1(offset)) == KFP_Iterations; }
float49 getN  (ivec2 offset) { return float49_(getN1(offset), getN0(offset), 1.0 - getNF(offset)); }
vec2    getDE (ivec2 offset) { return vec2(getDEX(offset), getDEY(offset)); }
bool    getGlitch  (void) { return getGlitch(ivec2(0, 0)); }
bool    getInterior(void) { return getInterior(ivec2(0, 0)); }
float49 getN  (void) { return getN (ivec2(0, 0)); }
vec2    getDE (void) { return getDE(ivec2(0, 0)); }

bool inImage(ivec2 offset)
{
  ivec2 pixel = Internal_PixelCoord + offset;
  return 0 <= pixel.x && pixel.x < ImageSize.x &&
         0 <= pixel.y && pixel.y < ImageSize.y;
}

vec2 getJitter(ivec2 offset)
{
  if (inImage(offset))
  {
    return GetPixelOffset(Internal_PixelCoord + offset);
  }
  else
  {
    return vec2(0.0, 0.0); // FIXME reflect at image boundaries
  }
}

vec2 getJitter(void)
{
  return getJitter(ivec2(0, 0));
}

vec2 getCoord(void)
{
  return vec2(Internal_TileOrigin.xy) + gl_FragCoord.xy;
}

float getZoomLog2(void)
{
  return Internal_ZoomLog2;
}

float getZoomLog2(ivec2 offset)
{
  return Internal_ZoomLog2;
}

float49 getN3x3(inout mat3 p, inout mat3 px, inout mat3 py)
{
  // load 3x3 stencil around the pixel
  float49 N = getN();
  for (int dj = -1; dj <= 1; ++dj)
  {
    for (int di = -1; di <= 1; ++di)
    {
      ivec2 offset = ivec2(di, dj);
      p[di + 1][dj + 1] = sub(getN(offset), N).x[0];
      vec2 delta = getJitter(offset);
      px[di + 1][dj + 1] = float(di) + delta.x;
      py[di + 1][dj + 1] = float(dj) + delta.y;
    }
  }
  // reflect at image boundaries if necessary
  // this will break (result is infinite or NaN) for image size of 1 pixel
  p[1][1] *= 2.0;
  px[1][1] *= 2.0;
  py[1][1] *= 2.0;
  if (isnan(p[0][0])) {p[0][0] = p[1][1] - p[2][2];px[0][0] = px[1][1] - px[2][2];py[0][0] = py[1][1] - py[2][2];}
  if (isnan(p[0][1])) {p[0][1] = p[1][1] - p[2][1];px[0][1] = px[1][1] - px[2][1];py[0][1] = py[1][1] - py[2][1];}
  if (isnan(p[0][2])) {p[0][2] = p[1][1] - p[2][0];px[0][2] = px[1][1] - px[2][0];py[0][2] = py[1][1] - py[2][0];}
  if (isnan(p[1][0])) {p[1][0] = p[1][1] - p[1][2];px[1][0] = px[1][1] - px[1][2];py[1][0] = py[1][1] - py[1][2];}
  if (isnan(p[1][2])) {p[1][2] = p[1][1] - p[1][0];px[1][2] = px[1][1] - px[1][0];py[1][2] = py[1][1] - py[1][0];}
  if (isnan(p[2][0])) {p[2][0] = p[1][1] - p[0][2];px[2][0] = px[1][1] - px[0][2];py[2][0] = py[1][1] - py[0][2];}
  if (isnan(p[2][1])) {p[2][1] = p[1][1] - p[0][1];px[2][1] = px[1][1] - px[0][1];py[2][1] = py[1][1] - py[0][1];}
  if (isnan(p[2][2])) {p[2][2] = p[1][1] - p[0][0];px[2][2] = px[1][1] - px[0][0];py[2][2] = py[1][1] - py[0][0];}
  p[1][1] *= 0.5;
  px[1][1] *= 0.5;
  py[1][1] *= 0.5;
  return N;
}

float KF_Traditional(mat3 p, mat3 px, mat3 py)
{
  // traditional method reverse engineered from original code
  float gx = (p[0][1] - p[1][1]) * 1.414 / hypot1(px[0][1] - px[1][1], py[0][1] - py[1][1]);
  float gy = (p[1][0] - p[1][1]) * 1.414 / hypot1(px[1][0] - px[1][1], py[1][0] - py[1][1]);
  float gu = (p[0][0] - p[1][1]) * 1.414 / hypot1(px[0][0] - px[1][1], py[0][0] - py[1][1]);
  float gv = (p[0][2] - p[1][1]) * 1.414 / hypot1(px[0][2] - px[1][1], py[0][2] - py[1][1]);
  float g = abs(gx) + abs(gy) + abs(gu) + abs(gv);
  return 1.0 / g;
}

float KF_Traditional(void)
{
  mat3 p = mat3(nan, nan, nan, nan, nan, nan, nan, nan, nan);
  mat3 px = mat3(0.0);
  mat3 py = mat3(0.0);
  getN3x3(p, px, py);
  return KF_Traditional(p, px, py);
}

float KF_Forward3x3(mat3 p, mat3 px, mat3 py)
{
  // forward differencing in 8 directions from the target point
  float gx0 = sqr(p[0][1] - p[1][1]) / hypot2(px[0][1] - px[1][1], py[0][1] - py[1][1]);
  float gx2 = sqr(p[2][1] - p[1][1]) / hypot2(px[2][1] - px[1][1], py[2][1] - py[1][1]);
  float gy0 = sqr(p[1][0] - p[1][1]) / hypot2(px[1][0] - px[1][1], py[1][0] - py[1][1]);
  float gy2 = sqr(p[1][2] - p[1][1]) / hypot2(px[1][2] - px[1][1], py[1][2] - py[1][1]);
  float gu0 = sqr(p[0][0] - p[1][1]) / hypot2(px[0][0] - px[1][1], py[0][0] - py[1][1]);
  float gu2 = sqr(p[2][2] - p[1][1]) / hypot2(px[2][2] - px[1][1], py[2][2] - py[1][1]);
  float gv0 = sqr(p[2][0] - p[1][1]) / hypot2(px[2][0] - px[1][1], py[2][0] - py[1][1]);
  float gv2 = sqr(p[0][2] - p[1][1]) / hypot2(px[0][2] - px[1][1], py[0][2] - py[1][1]);
  float g = sqrt(0.25 * (gx0 + gx2 + gy0 + gy2 + gu0 + gu2 + gv0 + gv2));
  return 1.0 / (g * 2.8284271247461903);
}

float KF_Forward3x3(void)
{
  mat3 p = mat3(nan, nan, nan, nan, nan, nan, nan, nan, nan);
  mat3 px = mat3(0.0);
  mat3 py = mat3(0.0);
  getN3x3(p, px, py);
  return KF_Forward3x3(p, px, py);
}

float KF_Central3x3(mat3 p, mat3 px, mat3 py)
{
  // gerrit's central difference formula
  float gx = sqr(p[2][1] - p[0][1]) / hypot2(px[2][1] - px[0][1], py[2][1] - py[0][1]);
  float gy = sqr(p[1][2] - p[1][0]) / hypot2(px[1][2] - px[1][0], py[1][2] - py[1][0]);
  float g1 = sqr(p[2][2] - p[0][0]) / hypot2(px[2][2] - px[0][0], py[2][2] - py[0][0]);
  float g2 = sqr(p[0][2] - p[2][0]) / hypot2(px[0][2] - px[2][0], py[0][2] - py[2][0]);
  float g = sqrt(0.5 * (gx + gy + g1 + g2));
  return 1.0 / (g * 2.8284271247461903);
}

float KF_Central3x3(void)
{
  mat3 p = mat3(nan, nan, nan, nan, nan, nan, nan, nan, nan);
  mat3 px = mat3(0.0);
  mat3 py = mat3(0.0);
  getN3x3(p, px, py);
  return KF_Central3x3(p, px, py);
}

float KF_Diagonal2x2(mat3 p, mat3 px, mat3 py)
{
  // forward differencing in 2 diagonals of a 2x2 substencil
  if (KFP_JitterSeed == 0u)
  {
    float gu = sqr(p[0][0] - p[1][1]) / hypot2(px[0][0] - px[1][1], py[0][0] - py[1][1]);
    float gv = sqr(p[0][1] - p[1][0]) / hypot2(px[0][1] - px[1][0], py[0][1] - py[1][0]);
    float g = sqrt(gu + gv);
    return 1.0 / (g * 2.8284271247461903);
  }
  else
  {
    // with displacement correction by gerrit
    float nux = px[0][0] - px[1][1];
    float nuy = py[0][0] - py[1][1];
    float nvx = px[1][0] - px[0][1];
    float nvy = py[1][0] - py[0][1];
    float nu = hypot1(nux, nuy);
    float nv = hypot1(nvx, nvy);
    nux /= nu;
    nuy /= nu;
    nvx /= nv;
    nvy /= nv;
    float u = (p[0][0] - p[1][1]) / nu;
    float v = (p[1][0] - p[0][1]) / nv;
    float dotnunv = nux * nvx + nuy * nvy;
    float crossnunv = nux * nvy - nuy * nvx;
    float g = sqrt((u * u + v * v - 2 * u * v * dotnunv) / sqr(crossnunv));
    return 1.0 / (g * 2.8284271247461903);
  }
}

float KF_Diagonal2x2(void)
{
  mat3 p = mat3(nan, nan, nan, nan, nan, nan, nan, nan, nan);
  mat3 px = mat3(0.0);
  mat3 py = mat3(0.0);
  getN3x3(p, px, py);
  return KF_Diagonal2x2(p, px, py);
}

#if 0
            case Differences_LeastSquares3x3:
            {
              float gx = 0;
              float gy = 0;
              // compute_gradient_3x3(p, px, py, gx, gy); FIXME
              float g = hypot1(gx, gy);
              iter = float49_(g * 2.8284271247461903);
            }
            break;
            case Differences_LeastSquares2x2:
            {
              float gx = 0;
              float gy = 0;
              // compute_gradient_2x2(p, px, py, gx, gy); FIXME
              float g = hypot1(gx, gy);
              iter = float49_(g * 2.8284271247461903);
            }
            break;
#endif

float KF_Laplacian3x3(mat3 p, mat3 px, mat3 py)
{
  float L = 0.0;
  L +=  1.0 * p[0][0];
  L +=  4.0 * p[0][1];
  L +=  1.0 * p[0][2];
  L +=  4.0 * p[1][0];
  L -= 20.0 * p[1][1];
  L +=  4.0 * p[1][2];
  L +=  1.0 * p[2][0];
  L +=  4.0 * p[2][1];
  L +=  1.0 * p[2][2];
  L /=  6.0;
#define INV_LOG_2 1.4426950408889634
  float g = sqrt(abs(L * INV_LOG_2));
#undef INV_LOG_2
  return 1.0 / (g * 2.8284271247461903);
}

float KF_Laplacian3x3(void)
{
  mat3 p = mat3(nan, nan, nan, nan, nan, nan, nan, nan, nan);
  mat3 px = mat3(0.0);
  mat3 py = mat3(0.0);
  getN3x3(p, px, py);
  return KF_Laplacian3x3(p, px, py);
}

float KF_Analytic(ivec2 offset)
{
  return length(getDE(offset));
}

float KF_Analytic(void)
{
  return KF_Analytic(ivec2(0, 0));
}

float KF_DE(int method)
{
  switch (method)
  {
    default:
    case Differences_Traditional: return KF_Traditional();
    case Differences_Forward3x3: return KF_Forward3x3();
    case Differences_Central3x3: return KF_Central3x3();
    case Differences_Diagonal2x2: return KF_Diagonal2x2();
#if 0
    case Differences_LeastSquares2x2: return KF_LeastSquares2x2();
    case Differences_LeastSquares3x3: return KF_LeastSquares3x3();
#endif
    case Differences_Laplacian3x3: return KF_Laplacian3x3();
    case Differences_Analytic: return KF_Analytic();
  }
  return 0.0;
}

vec3 KF_InfiniteWaves(bool Smooth, float49 iter)
{
  float nH = 0.0, nS = 0.0, nB = 0.0;
  int nDR = 0, nDG = 0, nDB = 0;
  for (int i = 0; i < KFP_MultiWavesCount; i++){
    float nPeriod = float(KFP_MultiWaves[i].x);
    float g;
    if (Smooth)
      g = sin(2.0 * pi * wrap(div(iter, nPeriod * 2.0)).x[0]) / 2.0 + 0.5;
    else
      g = sin(2.0 * pi * wrap(div(floor(iter), nPeriod * 2.0)).x[0]) / 2.0 + 0.5;
    if (nPeriod < 0)
      g = -nPeriod / 100.0;
    int type = KFP_MultiWaves[i].z;
    if (type == 0){
      nH += g;
      nDR++;
    }
    if (type == 1){
      nS += g;
      nDG++;
    }
    if (type == 2){
      nB += g;
      nDB++;
    }
  }
  if (nDR > 0)
    nH /= float(nDR);
  if (nDG > 0)
    nS /= float(nDG);
  if (nDB > 0)
    nB /= float(nDB);
  return hsv2rgb(vec3(nH, nS, nB));
}

vec2 KF_TextureWarp(float TexturePower, float TextureRatio, vec2 SlopeDir)
{
  mat3 p = mat3(nan, nan, nan, nan, nan, nan, nan, nan, nan);
  mat3 px = mat3(0.0);
  mat3 py = mat3(0.0);
  getN3x3(p, px, py);
  float nImgOffs = TexturePower / 64.0;
  float diffx = p[0][1] - p[1][1];
  float diffy = p[1][0] - p[1][1];
  float diff = dot(vec2(diffx, diffy), SlopeDir);
  diff  = 1.0 + diff;
  diffx = 1.0 + diffx;
  diffy = 1.0 + diffy;
  diff  = pow(diff,  TexturePower);
  diffx = pow(diffx, TexturePower);
  diffy = pow(diffy, TexturePower);
  float sx = 1.0;
  float  sy = 1.0;
  if (diff  <= 1.0) { diff  = 1.0 / diff; }
  if (diffx <= 1.0) { diffx = 1.0 / diffx; sx = -sx; }
  if (diffy <= 1.0) { diffy = 1.0 / diffy; sy = -sy; }
  diff  = (atan(diff)  - pi / 4.0) / (pi / 4.0);
  diffx = (atan(diffx) - pi / 4.0) / (pi / 4.0);
  diffy = (atan(diffy) - pi / 4.0) / (pi / 4.0);
  diff  *= TextureRatio / 100.0;
  diffx *= TextureRatio / 100.0;
  diffy *= TextureRatio / 100.0;
  float dx = nImgOffs + sx * TexturePower * diffx;
  float dy = nImgOffs - sy * TexturePower * diffy;
  return vec2(dx, dy);
}

vec4 KF_Slopes(bool analytic, vec2 SlopeDir, float Power, float Ratio)
{
  Power *= float(ImageSize.x) / 640.0;
  Ratio /= 100.0;
  vec2 vdiff;
  if (analytic)
  {
    vec2 DE = getDE();
    vdiff = vec2(1.0, -1.0) * DE / dot(DE, DE);
  }
  else
  {
    float49 N = getN();
    vdiff.x = -sub(getN(ivec2(1, 0)), N).x[0];
    vdiff.y = sub(getN(ivec2(0, -1)), N).x[0];
  }
  float diff = dot(vdiff, SlopeDir);
  diff *= Power;
  if (diff >= 0.0)
  {
    diff = atan(diff) / (pi / 2.0);
    diff = diff * Ratio;
    return vec4(vec3(0.0), diff);
  }
  else
  {
    diff = -diff;
    diff = atan(diff) / (pi / 2.0);
    diff = diff * Ratio;
    return vec4(vec3(1.0), diff);
  }
}

float49 KF_InverseTransition(float49 iter)
{
  float49 iter_ = floor(iter);
  float offs = sub(iter, iter_).x[0];
  iter = add(iter_, 1.0 - offs);
  return iter;
}

float49 KF_IterTransform(float49 iter0)
{
  float49 iter = iter0;
  switch (KFP_ColorMethod)
  {
    default:
    case ColorMethod_Standard: iter = iter; break;
    case ColorMethod_SquareRoot: iter = sqrt(max(0.0, iter)); break;
    case ColorMethod_CubicRoot: iter = cbrt(max(0.0, iter)); break;
    case ColorMethod_Logarithm: iter = log(max(1.0, iter)); break;
    case ColorMethod_LogLog: iter = log(add(1.0, log(add(1.0, iter)))); break;
    case ColorMethod_ATan: iter = atan(iter); break;
    case ColorMethod_FourthRoot: iter = sqrt(sqrt(max(0.0, iter))); break;
    case ColorMethod_Stretched:
    {
      float49 imin = float49_(KFP_IterationsMin[1], KFP_IterationsMin[0]);
      float49 imax = float49_(KFP_IterationsMax[1], KFP_IterationsMax[0]);
      iter = mul(1024.0, div(sub(iter, imin), sub(imax, imin))); break;
    }
    case ColorMethod_DistanceLinear:
    case ColorMethod_DEPlusStandard:
    case ColorMethod_DistanceLog:
    case ColorMethod_DistanceSqrt:
    {
      iter = float49_(1.0 / KF_DE(KFP_Differences));
      // post differencing transfer functions
      iter = mul(iter, float(ImageSize.x) / 640.0);
      if (KFP_ColorMethod == ColorMethod_DistanceSqrt || KFP_ColorMethod == ColorMethod_DEPlusStandard)
        iter = sqrt(max(0.0, iter));
      else if (KFP_ColorMethod == ColorMethod_DistanceLog)
        iter = log(max(1.0, add(1.0, iter)));
      if(gt(iter, 1024.0))
        iter = float49_(1024.0);
      if(KFP_ColorMethod == ColorMethod_DEPlusStandard && gt(iter, KFP_IterDiv))
        iter = iter0;
      break;
    }
  }
  iter = div(iter, KFP_IterDiv);
  iter = add(iter, KFP_ColorOffset);
  if (KFP_InverseTransition)
  {
    iter = KF_InverseTransition(iter);
  }
  return iter;
}

vec3 KF_Colour(void)
{
  vec3 s = vec3(0.0);
  if (! KFP_ShowGlitches && getGlitch())
  {
    discard;
  }
  if (getInterior())
  {
    s = KFP_InteriorColor;
  }
  else
  {
    float49 iter = float49_(getN1(), getN0(), KFP_Flat ? 0.0 : 1.0 - getNF());
    iter = KF_IterTransform(iter);
    if (KFP_MultiWavesEnabled)
    {
      vec3 nRGB = KF_InfiniteWaves(KFP_Smooth, iter);
      if (KFP_MultiWavesBlend)
      {
        iter = add(iter, KFP_PhaseColorStrength / 100.0 * 1024.0 * getT());
        if (KFP_InverseTransition)
        {
          iter = KF_InverseTransition(iter);
        }
        vec3 nRGB2 = KF_Palette(wrap(div(iter, 1024.0)).x[0]);
        s = mix(nRGB, nRGB2, 0.5);
      }
      else
      {
        s = nRGB;
      }
    }
    else
    {
      iter = add(iter, KFP_PhaseColorStrength / 100.0 * 1024.0 * getT());
      if (KFP_InverseTransition)
      {
        iter = KF_InverseTransition(iter);
      }
      s = KF_Palette(wrap(div(iter, 1024.0)).x[0]);
    }
  }
  if (KFP_TextureEnabled)
  {
    vec2 tc = getCoord() + KF_TextureWarp(KFP_TexturePower, KFP_TextureRatio, KFP_SlopeDir);
    tc /= vec2(ImageSize.xy);
    s = mix(s, texture(KFP_Texture, tc).rgb, KFP_TextureMerge);
  }
  if (KFP_Slopes)
  {
    vec4 slope = KF_Slopes(KFP_Differences == Differences_Analytic, KFP_SlopeDir, KFP_SlopePower, KFP_SlopeRatio);
    s = mix(s, slope.rgb, slope.a);
  }
  if (getInterior() && ! KFP_TextureEnabled)
  {
    s = KFP_InteriorColor;
  }
  return s;
}

void main(void)
{
  Internal_One = Internal_Zero + ImageSize.x / ImageSize.x;
  Internal_Colour = vec4(clamp(colour(), vec3(0.0), vec3(65504.0)), 1.0);
}

#line 0 1
