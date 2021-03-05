/*
Kalles Fraktaler 2
Copyright (C) 2017-2021 Claude Heiland-Allen

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

#ifndef KF_OPENGL_H
#define KF_OPENGL_H 1

#include <string>
#include <vector>

#include <glad/glad.h>

#include "fraktal_sft.h"
#include "fifo.h"

enum request_t
{
  request_quit,
  request_init,
  request_deinit,
  request_compile,
  request_configure,
  request_render
};

enum response_t
{
  response_quit,
  response_init,
  response_deinit,
  response_compile,
  response_configure,
  response_render
};

struct response_init_t
{
  bool success;
  int major;
  int minor;
  std::string message;
};

struct request_compile_t
{
  std::string fragment_src;
};

struct response_compile_t
{
  bool success;
  std::string vertex_log;
  std::string fragment_log;
  std::string link_log;
};

struct request_configure_t
{
  int64_t jitter_seed;
  int jitter_shape;
  double jitter_scale;
  bool show_glitches;
  bool inverse_transition;
  int64_t iterations;
  int64_t iterations_min;
  int64_t iterations_max;
  double iter_div;
  double color_offset;
  ColorMethod color_method;
  Differences differences;
  double color_phase_strength;
  std::vector<unsigned char> colors; // multiple of 3
  unsigned char interior_color[3];
  bool smooth;
  bool flat;
  // infinite waves
  bool multiwaves_enabled;
  bool multiwaves_blend;
  std::vector<GLint> multiwaves; // multiple of 3
  // slopes
  bool slopes;
  double slope_power;
  double slope_ratio;
  double slope_angle;
  // image texture
  bool texture_enabled;
  double texture_merge;
  double texture_power;
  double texture_ratio;
  int64_t texture_width;
  int64_t texture_height;
  unsigned char *texture;
  bool use_srgb;
  double zoom_log2;
};

struct request_render_t
{
  int64_t width;
  int64_t height;
  const uint32_t *n_msb;
  const uint32_t *n_lsb;
  const float *n_f;
  const float *t;
  const float *dex;
  const float *dey;
  half *rgb16;
  unsigned char *rgb8;
};

struct request
{
  request_t tag;
  struct
  {
    request_compile_t compile;
    request_configure_t configure;
    request_render_t render;
  } u;
};

struct response
{
  response_t tag;
  struct
  {
    response_init_t init;
    response_compile_t compile;
  } u;
};

extern fifo<request> to_opengl;
extern fifo<response> from_opengl;
void opengl_thread(fifo<request> &requests, fifo<response> &responses);

#endif
