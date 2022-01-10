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

#ifndef WINVER
#include <thread>
#include <mutex>
#endif

#include <half.h>
#include <glad/glad.h>
#include <GLFW/glfw3.h>

#include "defs.h"

enum request_t
{
  request_quit,
  request_init,
  request_deinit,
  request_compile,
  request_configure,
  request_render
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
  union {
    request_compile_t *compile;
    request_configure_t *configure;
    request_render_t *render;
  };
};

struct response
{
  union {
    response_init_t *init;
    response_compile_t *compile;
  };
};

class OpenGL_processor
{
private:
  const int64_t max_tile_width = 1024;
  const int64_t max_tile_height = 1024;
  const int tu_n_msb = 1, tu_n_lsb = 2, tu_n_f = 3, tu_t = 4, tu_dex = 5, tu_dey = 6, tu_rgb16 = 7,tu_rgb8 = 8, tu_texture = 9, tu_palette = 10;
  GLuint t_n_msb = 0, t_n_lsb = 0, t_n_f = 0, t_t = 0, t_dex = 0, t_dey = 0, t_rgb16 = 0, t_rgb8 = 0, t_texture = 0, t_palette = 0;
  std::string version;
  GLuint p_colour = 0, p_blit = 0, f_linear = 0, f_srgb = 0;
  GLuint vao = 0;
  GLFWwindow *window = nullptr;
  // should we do gamma-correct linear-light blending?
  // default is false (incorrect blending) for historical reasons
  bool sRGB = false;
  volatile request_t tag;

#ifdef WINVER
  HANDLE hDone;
  HANDLE hThread;
#else
  std::thread opengl_thread;
#endif

#ifdef WINVER
  HANDLE req_lock;
  HANDLE resp_lock;
#else
  std::mutex req_lock;
  std::mutex resp_lock;
#endif


  // Initial state: mutexes are locked. Handler waits for req_lock.
  // Client fills req, unlocks req_in, waits for resp_lock.
  // Handler processes req, fill response, re-locks and waits for req_lock.
  // Client reads resp, re-locks resp_lock.

  void handle_init();
  void handle_deinit();
  void handle_compile();
  void handle_configure();
  void handle_render();

  void process_request();

public:
  request req;
  response resp;

  OpenGL_processor();
  ~OpenGL_processor();

  // only one of these may run at any time. 
  bool init(response_init_t &resp);
  void deinit();
  bool compile(request_compile_t &req, response_compile_t &resp);
  void configure(request_configure_t &req);
  void render(request_render_t &req);

private:
  void quit();

public:
  void th_handler(); // task's main loop
};

#endif
