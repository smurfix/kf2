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

#include <algorithm>
#include "opengl.h"
#include <GLFW/glfw3.h>

#include "colour.h"

#include "../gl/kf_vert_glsl.h"
#include "../gl/kf_frag_glsl.h"

#define D { GLint e = 0; while ((e = glGetError())) { std::cerr << "OpenGL error " << e << " at line " << __LINE__ << std::endl; } }

static bool debug_program(GLuint program, std::string &log) {
  GLint status = 0;
  glGetProgramiv(program, GL_LINK_STATUS, &status);
  GLint length = 0;
  glGetProgramiv(program, GL_INFO_LOG_LENGTH, &length);
  char *info = 0;
  if (length) {
    info = (char *)malloc(length + 1);
    info[0] = 0;
    glGetProgramInfoLog(program, length, 0, info);
    info[length] = 0;
  }
  if ((info && info[0]) || ! status) {
    log += "link info:\n" + std::string(info ? info : "(no info log)") + "\n";
  }
  if (info) {
    free(info);
  }
  return status;
}

static bool debug_shader(GLuint shader, GLenum type, std::string &log) {
  GLint status = 0;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
  GLint length = 0;
  glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &length);
  char *info = 0;
  if (length) {
    info = (char *)malloc(length + 1);
    info[0] = 0;
    glGetShaderInfoLog(shader, length, 0, info);
    info[length] = 0;
  }
  if ((info && info[0]) || ! status) {
    std::string type_str = "unknown";
    switch (type) {
      case GL_VERTEX_SHADER: type_str = "vertex"; break;
      case GL_FRAGMENT_SHADER: type_str = "fragment"; break;
    }
    log += type_str + " info:\n" + (info ? info : "(no info log)") + "\n";
  }
  if (info) {
    free(info);
  }
  return status;
}

static GLuint vertex_fragment_shader(const char *version, const char *vert, const char *frag, const char *frag2, std::string &vert_log, std::string &frag_log, std::string &link_log)
{
  bool ok = true;
  GLuint program = glCreateProgram();
  {
    GLuint shader = glCreateShader(GL_VERTEX_SHADER);
    const char *sources[] = { version, vert };
    glShaderSource(shader, 2, sources, 0);
    glCompileShader(shader);
    ok &= debug_shader(shader, GL_VERTEX_SHADER, vert_log);
    glAttachShader(program, shader);
    glDeleteShader(shader);
  }
  {
    GLuint shader = glCreateShader(GL_FRAGMENT_SHADER);
    const char *sources[] = { version, frag, frag2 };
    glShaderSource(shader, 3, sources, 0);
    glCompileShader(shader);
    ok &= debug_shader(shader, GL_FRAGMENT_SHADER, frag_log);
    glAttachShader(program, shader);
    glDeleteShader(shader);
  }
  glLinkProgram(program);
  ok &= debug_program(program, link_log);
  if (! ok)
  {
    glDeleteProgram(program);
    program = 0;
  }
  return program;
}

const char *blit_vert =
      "void main(void) {\n"
      "  switch (gl_VertexID) {\n"
      "  default:\n"
      "  case 0:\n"
      "    gl_Position = vec4(-1.0, -1.0, 0.0, 1.0);\n"
      "    break;\n"
      "  case 1:\n"
      "    gl_Position = vec4( 1.0, -1.0, 0.0, 1.0);\n"
      "    break;\n"
      "  case 2:\n"
      "    gl_Position = vec4(-1.0,  1.0, 0.0, 1.0);\n"
      "    break;\n"
      "  case 3:\n"
      "    gl_Position = vec4( 1.0,  1.0, 0.0, 1.0);\n"
      "    break;\n"
      "  }\n"
      "}\n"
  ;
const char *blit_frag =
      "#if __VERSION__ >= 330\n"
      "layout(location = 0, index = 0) out vec4 colour;\n"
      "#else\n"
      "#define colour gl_FragColor\n"
      "#endif\n"
      "uniform sampler2D t;\n"
      "void main(void) { colour = texelFetch(t, ivec2(gl_FragCoord.xy), 0); }\n"
  ;


  bool OpenGL_processor::init(int &r_major, int &r_minor, std::string &r_message)
      {
        if (! glfwInit())
        {
          std::cerr << "error: glfwInit()" << std::endl;
          return false;
        }
        const int nversions = 11;
        const int major[] = { 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3 };
        const int minor[] = { 6, 5, 4, 3, 2, 1, 0, 3, 2, 1, 0 };
        const int glslv[] = { 460, 450, 440, 430, 420, 410, 400, 330, 150, 140, 130 };
        r_major = 0;
        r_minor = 0;
        for (int k = 0; k < nversions; ++k)
        {
          glfwDefaultWindowHints();
          glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE);
          glfwWindowHint(GLFW_CLIENT_API, GLFW_OPENGL_API);
          glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, major[k]);
          glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, minor[k]);
          if (major[k] > 3 || (major[k] == 3 && minor[k] >= 3))
          {
            glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
            glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GLFW_TRUE);
            std::ostringstream o;
            o << "#version " << glslv[k] << " core\n";
            version = o.str();
          }
          else
          {
            std::ostringstream o;
            o << "#version " << glslv[k] << "\n";
            version = o.str();
          }
          window = glfwCreateWindow(1, 1, "", nullptr, nullptr);
          if (window)
          {
            r_major = major[k];
            r_minor = minor[k];
            break;
          }
        }
        if (! window)
        {
          glfwTerminate();
          r_message = "error: could not create OpenGL context with version 3.0 or greater\n";
          return false;
        }
        glfwMakeContextCurrent(window);
        if (! gladLoadGLLoader((GLADloadproc) glfwGetProcAddress))
        {
          glfwDestroyWindow(window);
          window = nullptr;
          glfwTerminate();
          r_message = "error: could not initialize OpenGL context with version 3.0 or greater\n";
          return false;
        }

        std::string vertex_log, fragment_log, link_log;
        p_blit = vertex_fragment_shader(version.c_str(), blit_vert, blit_frag, "", vertex_log, fragment_log, link_log);
        if (! p_blit)
        {
          const std::string nl = "\n";
          glfwDestroyWindow(window);
          window = nullptr;
          glfwTerminate();
          r_message =
            "error: could not compile internal shader:\n" +
            vertex_log + nl +
            fragment_log + nl +
            link_log + nl;
          return false;
        }
        glUseProgram(p_blit);
        glUniform1i(glGetUniformLocation(p_blit, "t"), tu_rgb16);
        D

        const GLuint zeroui = 0;
        const GLfloat zerof = 0;

        glActiveTexture(GL_TEXTURE0 + tu_n_msb);
        D
        glGenTextures(1, &t_n_msb);
        D
        glBindTexture(GL_TEXTURE_2D, t_n_msb);
        D
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        D
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        D
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        D
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        D
        glTexImage2D(GL_TEXTURE_2D, 0, GL_R32UI, 1, 1, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, &zeroui);
        D

        glActiveTexture(GL_TEXTURE0 + tu_n_lsb);
        D
        glGenTextures(1, &t_n_lsb);
        D
        glBindTexture(GL_TEXTURE_2D, t_n_lsb);
        D
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        D
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        D
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        D
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        D
        glTexImage2D(GL_TEXTURE_2D, 0, GL_R32UI, 1, 1, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, &zeroui);
        D

        glActiveTexture(GL_TEXTURE0 + tu_n_f);
        glGenTextures(1, &t_n_f);
        glBindTexture(GL_TEXTURE_2D, t_n_f);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, 1, 1, 0, GL_RED, GL_FLOAT, &zerof);
        D

        glActiveTexture(GL_TEXTURE0 + tu_t);
        glGenTextures(1, &t_t);
        glBindTexture(GL_TEXTURE_2D, t_t);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, 1, 1, 0, GL_RED, GL_FLOAT, &zerof);
        D

        glActiveTexture(GL_TEXTURE0 + tu_dex);
        glGenTextures(1, &t_dex);
        glBindTexture(GL_TEXTURE_2D, t_dex);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, 1, 1, 0, GL_RED, GL_FLOAT, &zerof);
        D

        glActiveTexture(GL_TEXTURE0 + tu_dey);
        glGenTextures(1, &t_dey);
        glBindTexture(GL_TEXTURE_2D, t_dey);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, 1, 1, 0, GL_RED, GL_FLOAT, &zerof);
        D

        glActiveTexture(GL_TEXTURE0 + tu_rgb16);
        glGenTextures(1, &t_rgb16);
        glBindTexture(GL_TEXTURE_2D, t_rgb16);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB16F, max_tile_width, max_tile_height, 0, GL_RGB, GL_HALF_FLOAT, nullptr);
        D

        glActiveTexture(GL_TEXTURE0 + tu_rgb8);
        glGenTextures(1, &t_rgb8);
        glBindTexture(GL_TEXTURE_2D, t_rgb8);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexImage2D(GL_TEXTURE_2D, 0, sRGB ? GL_SRGB : GL_RGB, max_tile_width, max_tile_height, 0, GL_RGB, GL_UNSIGNED_BYTE, nullptr);
        D

        glActiveTexture(GL_TEXTURE0 + tu_texture);
        glGenTextures(1, &t_texture);
        glBindTexture(GL_TEXTURE_2D, t_texture);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexImage2D(GL_TEXTURE_2D, 0, sRGB ? GL_SRGB : GL_RGB, 1, 1, 0, GL_RGB, GL_UNSIGNED_BYTE, &zeroui);
        D

        glActiveTexture(GL_TEXTURE0 + tu_palette);
        glGenTextures(1, &t_palette);
        glBindTexture(GL_TEXTURE_1D, t_palette);
        glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexImage1D(GL_TEXTURE_1D, 0, sRGB ? GL_SRGB : GL_RGB, 1, 0, GL_RGB, GL_UNSIGNED_BYTE, &zeroui);
        D

        glGenFramebuffers(1, &f_linear);
        glBindFramebuffer(GL_FRAMEBUFFER, f_linear);
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, t_rgb16, 0);
        glGenFramebuffers(1, &f_srgb);
        glBindFramebuffer(GL_FRAMEBUFFER, f_srgb);
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, t_rgb8, 0);
        glBindFramebuffer(GL_FRAMEBUFFER, 0);
        D

        glGenVertexArrays(1, &vao);
        glBindVertexArray(vao);
        D

        return true;
      }

  OpenGL_processor::~OpenGL_processor()
      {
        if (vao)
        {
          glDeleteVertexArrays(1, &vao);
          vao = 0;
        }
        if (f_linear)
        {
          glDeleteFramebuffers(1, &f_linear);
          f_linear = 0;
        }
        if (f_srgb)
        {
          glDeleteFramebuffers(1, &f_srgb);
          f_srgb = 0;
        }
        if (t_n_msb)
        {
          glDeleteTextures(1, &t_n_msb);
          t_n_msb = 0;
        }
        if (t_n_lsb)
        {
          glDeleteTextures(1, &t_n_lsb);
          t_n_lsb = 0;
        }
        if (t_n_f)
        {
          glDeleteTextures(1, &t_n_f);
          t_n_f = 0;
        }
        if (t_t)
        {
          glDeleteTextures(1, &t_t);
          t_t = 0;
        }
        if (t_dex)
        {
          glDeleteTextures(1, &t_dex);
          t_dex = 0;
        }
        if (t_dey)
        {
          glDeleteTextures(1, &t_dey);
          t_dey = 0;
        }
        if (t_rgb16)
        {
          glDeleteTextures(1, &t_rgb16);
          t_rgb16 = 0;
        }
        if (t_rgb8)
        {
          glDeleteTextures(1, &t_rgb8);
          t_rgb8 = 0;
        }
        if (t_texture)
        {
          glDeleteTextures(1, &t_texture);
          t_texture = 0;
        }
        if (t_palette)
        {
          glDeleteTextures(1, &t_palette);
          t_palette = 0;
        }
        D
        if (window)
        {
          glfwDestroyWindow(window);
          window = nullptr;
        }
        glfwTerminate();
      }

  bool OpenGL_processor::compile(const std::string &fragment_src)
      {
        std::string vertex_log;
        std::string fragment_log;
        std::string link_log;
        if (m_fragment_src == fragment_src) {
          return (m_fragment_src.length() > 0);
        }
        p_colour = vertex_fragment_shader(version.c_str(), kf_vert_glsl, kf_frag_glsl, fragment_src.c_str(), vertex_log, fragment_log, link_log);
        D
        bool res = p_colour > 0;
        if (res)
          m_fragment_src = fragment_src;
        return res;
      }

  bool OpenGL_processor::configure(const request_configure_t &req)
      {
        glUseProgram(p_colour);
        glUniform1i(glGetUniformLocation(p_colour, "Internal_N1"), tu_n_msb);
        glUniform1i(glGetUniformLocation(p_colour, "Internal_N0"), tu_n_lsb);
        glUniform1i(glGetUniformLocation(p_colour, "Internal_NF"), tu_n_f);
        glUniform1i(glGetUniformLocation(p_colour, "Internal_T"), tu_t);
        glUniform1i(glGetUniformLocation(p_colour, "Internal_DEX"), tu_dex);
        glUniform1i(glGetUniformLocation(p_colour, "Internal_DEY"), tu_dey);
        glUniform1i(glGetUniformLocation(p_colour, "KFP_Texture"), tu_texture);
        glUniform1i(glGetUniformLocation(p_colour, "KFP_Palette"), tu_palette);
        glUniform2ui(glGetUniformLocation(p_colour, "KFP_Iterations"), req.iterations & 0xFFFFffffu, (req.iterations >> 32) && 0xFFFFffffu);
        glUniform2ui(glGetUniformLocation(p_colour, "KFP_IterationsMin"), req.iterations_min & 0xFFFFffffu, (req.iterations_min >> 32) && 0xFFFFffffu);
        glUniform2ui(glGetUniformLocation(p_colour, "KFP_IterationsMax"), req.iterations_max & 0xFFFFffffu, (req.iterations_max >> 32) && 0xFFFFffffu);
        glUniform1ui(glGetUniformLocation(p_colour, "KFP_JitterSeed"), req.jitter_seed);
        glUniform1i(glGetUniformLocation(p_colour, "KFP_JitterShape"), req.jitter_shape);
        glUniform1f(glGetUniformLocation(p_colour, "KFP_JitterScale"), req.jitter_scale);
        glUniform1f(glGetUniformLocation(p_colour, "KFP_IterDiv"), req.iter_div);
        glUniform1f(glGetUniformLocation(p_colour, "KFP_ColorOffset"), req.color_offset);
        glUniform1i(glGetUniformLocation(p_colour, "KFP_ColorMethod"), req.color_method);
        glUniform1i(glGetUniformLocation(p_colour, "KFP_Differences"), req.differences);
        glUniform1f(glGetUniformLocation(p_colour, "KFP_PhaseColorStrength"), req.color_phase_strength);
        glUniform1f(glGetUniformLocation(p_colour, "Internal_ZoomLog2"), req.zoom_log2);
        D
        sRGB = req.use_srgb;
        glActiveTexture(GL_TEXTURE0 + tu_rgb8);
        glTexImage2D(GL_TEXTURE_2D, 0, sRGB ? GL_SRGB : GL_RGB, max_tile_width, max_tile_height, 0, GL_RGB, GL_UNSIGNED_BYTE, nullptr);
        glUniform1i(glGetUniformLocation(p_colour, "KFP_sRGB"), sRGB);
        // palette
        glActiveTexture(GL_TEXTURE0 + tu_palette);
        glTexImage1D(GL_TEXTURE_1D, 0, sRGB ? GL_SRGB : GL_RGB, req.colors.size() / 3, 0, GL_BGR, GL_UNSIGNED_BYTE, &req.colors[0]);
        D
        srgb sinterior =
          { req.interior_color[2] / 255.0f
          , req.interior_color[1] / 255.0f
          , req.interior_color[0] / 255.0f
          };
        if (sRGB)
        {
          lrgb linterior = srgb2lrgb(sinterior);
          glUniform3f(glGetUniformLocation(p_colour, "KFP_InteriorColor"), linterior.r, linterior.g, linterior.b);
        }
        else
        {
          glUniform3f(glGetUniformLocation(p_colour, "KFP_InteriorColor"), sinterior.r, sinterior.g, sinterior.b);
        }
        glUniform1i(glGetUniformLocation(p_colour, "KFP_Smooth"), req.smooth);
        glUniform1i(glGetUniformLocation(p_colour, "KFP_Flat"), req.flat);
        // multi waves
        glUniform1i(glGetUniformLocation(p_colour, "KFP_MultiWavesEnabled"), req.multiwaves_enabled);
        glUniform1i(glGetUniformLocation(p_colour, "KFP_MultiWavesBlend"), req.multiwaves_blend);
        glUniform1i(glGetUniformLocation(p_colour, "KFP_MultiWavesCount"), req.multiwaves.size() / 3);
        if (req.multiwaves.size() / 3 > 0)
        {
          glUniform3iv(glGetUniformLocation(p_colour, "KFP_MultiWaves"), req.multiwaves.size() / 3, &req.multiwaves[0]);
        }
        // slopes
        glUniform1i(glGetUniformLocation(p_colour, "KFP_Slopes"), req.slopes);
        glUniform1f(glGetUniformLocation(p_colour, "KFP_SlopePower"), req.slope_power);
        glUniform1f(glGetUniformLocation(p_colour, "KFP_SlopeRatio"), req.slope_ratio);
        glUniform2f(glGetUniformLocation(p_colour, "KFP_SlopeDir"), cos(2.0 * pi * req.slope_angle / 360.0), sin(2.0 * pi * req.slope_angle / 360.0));
        // image texture
        glUniform1i(glGetUniformLocation(p_colour, "KFP_TextureEnabled"), req.texture_enabled);
        glUniform1f(glGetUniformLocation(p_colour, "KFP_TextureMerge"), req.texture_merge);
        glUniform1f(glGetUniformLocation(p_colour, "KFP_TexturePower"), req.texture_power);
        glUniform1f(glGetUniformLocation(p_colour, "KFP_TextureRatio"), req.texture_ratio);
        D
        if (req.texture_enabled)
        {
          glActiveTexture(GL_TEXTURE0 + tu_texture);
          glTexImage2D(GL_TEXTURE_2D, 0, sRGB ? GL_SRGB : GL_RGB, req.texture_width, req.texture_height, 0, GL_BGR, GL_UNSIGNED_BYTE, req.texture);
        }
        D
        glUniform1i(glGetUniformLocation(p_colour, "KFP_Texture"), tu_texture);
        D
        return true;
      }

  bool OpenGL_processor::render(const request_render_t &req)
      {
        const int padding = 1;
        uint32_t *n_msb = req.n_msb ? new uint32_t[max_tile_width * max_tile_height] : nullptr;
        uint32_t *n_lsb = req.n_lsb ? new uint32_t[max_tile_width * max_tile_height] : nullptr;
        float    *n_f   = req.n_f   ? new float   [max_tile_width * max_tile_height] : nullptr;
        float    *n_t   = req.t     ? new float   [max_tile_width * max_tile_height] : nullptr;
        float    *n_dex = req.dex   ? new float   [max_tile_width * max_tile_height] : nullptr;
        float    *n_dey = req.dey   ? new float   [max_tile_width * max_tile_height] : nullptr;

        for (int64_t tile_y = req.height + padding; tile_y > 0 - padding; tile_y -= (max_tile_height - 2 * padding))
        {
          const int tile_height = std::min(max_tile_height, tile_y + padding);
          for (int64_t tile_x = 0 - padding; tile_x < req.width + padding; tile_x += (max_tile_width - 2 * padding))
          {
            const int tile_width = std::min(max_tile_width, req.width - tile_x + padding);
            const int64_t skipX = tile_x + padding;
            const int64_t skipY = req.height + padding - tile_y;

            // copy from arrays to tile; reflect data at image boundaries
            // FIXME assumes tile size >= 3x3
            for (int64_t x = tile_x; x < tile_x + tile_width; ++x)
            {
              for (int64_t y = tile_y - tile_height; y < tile_y; ++y)
              {
                uint32_t msb = 0;
                uint32_t lsb = 0;
                float f = 0;
                float t = 0;
                float dex = 0;
                float dey = 0;

#define X(dx,dy) \
  int64_t k1 = (x + (dx)) * req.height + (y + (dy)); \
  int64_t k2 = (x + 2*(dx)) * req.height + (y + 2*(dy)); \
  int64_t neighbour_n = 0; \
  float neighbour_nf = 0; \
  int64_t next_n = 0; \
  float next_nf = 0; \
  if (n_msb) \
  { \
    neighbour_n += int64_t(req.n_msb[k1]) << 32; \
    next_n += int64_t(req.n_msb[k2]) << 32; \
  } \
  if (n_lsb) \
  { \
    neighbour_n += int64_t(req.n_lsb[k1]); \
    next_n += int64_t(req.n_lsb[k2]); \
  } \
  if (n_f) \
  { \
    neighbour_nf = 1.0f - req.n_f[k1]; \
    next_nf = 1.0f - req.n_f[k2]; \
  } \
  f = 2.0f * neighbour_nf - next_nf; \
  int64_t n = neighbour_n + (neighbour_n - next_n); \
  n += std::floor(f); \
  f -= std::floor(f); \
  f = 1.0f - f; \
  msb = uint32_t(n >> 32) & 0xFFFFffffu; \
  lsb = uint32_t(n      ) & 0xFFFFffffu; \
  if (n_t) \
  { \
    float neighbour = 6.283185307179586f * req.t[k1]; \
    float next = 6.283185307179586f * req.t[k2]; \
    t = std::atan2(2.0 * std::sin(neighbour) - std::sin(next), 2.0 * std::cos(neighbour) - std::cos(next)) / 6.283185307179586f; \
    t -= std::floor(t); \
  } \
  if (n_dex) \
  { \
    float neighbour = req.dex[k1]; \
    float next = req.dex[k2]; \
    dex = 2.0 * neighbour - next; \
  } \
  if (n_dey) \
  { \
    float neighbour = req.dey[k1]; \
    float next = req.dey[k2]; \
    dey = 2.0 * neighbour - next; \
  }

                if (0 <= x && x < req.width && 0 <= y && y < req.height)
                {
                  // middle of tile
                  int64_t k0 = (x) * req.height + (y);
                  if (n_msb)
                  {
                    msb = req.n_msb[k0];
                  }
                  if (n_lsb)
                  {
                    lsb = req.n_lsb[k0];
                  }
                  if (n_f)
                  {
                    f = req.n_f[k0];
                  }
                  if (n_t)
                  {
                    t = req.t[k0];
                  }
                  if (n_dex)
                  {
                    dex = req.dex[k0];
                  }
                  if (n_dey)
                  {
                    dey = req.dey[k0];
                  }
                }
                else if (x < 0 && y < 0)
                {
                  X(1, 1)
                }
                else if (x < 0 && y < req.height)
                {
                  X(1, 0)
                }
                else if (x < 0 && req.height <= y)
                {
                  X(1, -1)
                }
                else if (x < req.width && y < 0)
                {
                  X(0, 1)
                }
                else if (x < req.width && req.height <= y)
                {
                  X(0, -1)
                }
                else if (req.width <= x && y < 0)
                {
                  X(-1, 1)
                }
                else if (req.width <= x && y < req.height)
                {
                  X(-1, 0)
                }
                else if (req.width <= x && req.height <= y)
                {
                  X(-1, -1)
                }

#undef X

                int64_t k = (x - tile_x) * tile_height + (y - (tile_y - tile_height));
                if (n_msb)
                {
                  n_msb[k] = msb;
                }
                if (n_lsb)
                {
                  n_lsb[k] = lsb;
                }
                if (n_f)
                {
                  n_f[k] = f;
                }
                if (n_t)
                {
                  n_t[k] = t;
                }
                if (n_dex)
                {
                  n_dex[k] = dex;
                }
                if (n_dey)
                {
                  n_dey[k] = dey;
                }
              }
            }

            // upload textures
            if (n_msb)
            {
              glActiveTexture(GL_TEXTURE0 + tu_n_msb);
              D
              glTexImage2D(GL_TEXTURE_2D, 0, GL_R32UI, tile_height, tile_width, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, n_msb);
              D
            }
            if (n_lsb)
            {
              glActiveTexture(GL_TEXTURE0 + tu_n_lsb);
              D
              glTexImage2D(GL_TEXTURE_2D, 0, GL_R32UI, tile_height, tile_width, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, n_lsb);
              D
            }
            if (n_f)
            {
              glActiveTexture(GL_TEXTURE0 + tu_n_f);
              glTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, tile_height, tile_width, 0, GL_RED, GL_FLOAT, n_f);
            }
            D
            if (n_t)
            {
              glActiveTexture(GL_TEXTURE0 + tu_t);
              glTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, tile_height, tile_width, 0, GL_RED, GL_FLOAT, n_t);
            }
            D
            if (n_dex)
            {
              glActiveTexture(GL_TEXTURE0 + tu_dex);
              glTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, tile_height, tile_width, 0, GL_RED, GL_FLOAT, n_dex);
            }
            D
            if (n_dey)
            {
              glActiveTexture(GL_TEXTURE0 + tu_dey);
              glTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, tile_height, tile_width, 0, GL_RED, GL_FLOAT, n_dey);
            }
            D

            // render to 16bit
            glBindFramebuffer(GL_FRAMEBUFFER, f_linear);
            glViewport(0, 0, tile_width - 2 * padding, tile_height - 2 * padding);
            glUseProgram(p_colour);
            glUniform2i(glGetUniformLocation(p_colour, "ImageSize"), req.width, req.height);
            glUniform2i(glGetUniformLocation(p_colour, "Internal_TileOrigin"), tile_x + padding, req.height + padding - tile_y);
            glUniform2i(glGetUniformLocation(p_colour, "Internal_TilePadding"), padding, padding);
            glUniform2i(glGetUniformLocation(p_colour, "Internal_TileSize"), tile_width, tile_height);
            glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
            if (req.rgb16)
            {
              glPixelStorei(GL_PACK_ALIGNMENT, 1);
              glPixelStorei(GL_PACK_ROW_LENGTH, req.width);
              glPixelStorei(GL_PACK_SKIP_PIXELS, skipX);
              glPixelStorei(GL_PACK_SKIP_ROWS, skipY);
              glReadPixels(0, 0, tile_width - 2 * padding, tile_height - 2 * padding, GL_RGB, GL_HALF_FLOAT, req.rgb16);
            }
            D

            // copy to 8bit
            glBindFramebuffer(GL_FRAMEBUFFER, f_srgb);
            if (sRGB)
            {
              glEnable(GL_FRAMEBUFFER_SRGB);
            }
            glUseProgram(p_blit);
            glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
            if (req.rgb8)
            {
              glPixelStorei(GL_PACK_ALIGNMENT, 4);
              glPixelStorei(GL_PACK_ROW_LENGTH, req.width);
              glPixelStorei(GL_PACK_SKIP_PIXELS, skipX);
              glPixelStorei(GL_PACK_SKIP_ROWS, skipY);
              glReadPixels(0, 0, tile_width - 2 * padding, tile_height - 2 * padding, GL_BGR, GL_UNSIGNED_BYTE, req.rgb8);
            }
            if (sRGB)
            {
              glDisable(GL_FRAMEBUFFER_SRGB);
            }
            D
          }
        }

        delete[] n_msb;
        delete[] n_lsb;
        delete[] n_f;
        delete[] n_t;
        delete[] n_dex;
        delete[] n_dey;

        glPixelStorei(GL_PACK_ALIGNMENT, 4);
        glPixelStorei(GL_PACK_ROW_LENGTH, 0);
        glPixelStorei(GL_PACK_SKIP_PIXELS, 0);
        glPixelStorei(GL_PACK_SKIP_ROWS, 0);
        D

        // flip half image vertically (EXR origin top left, BMP bottom left)
        if (req.rgb16)
        {
          int64_t count = 3 * req.width;
          int64_t bytes = count * sizeof(half);
          half *tmp = new half[count];
          for (int64_t y1 = 0; y1 < req.height / 2; ++y1)
          {
            int64_t y2 = req.height - 1 - y1;
            int64_t skip1 = y1 * count;
            int64_t skip2 = y2 * count;
            std::memcpy(tmp, req.rgb16 + skip1, bytes);
            std::memcpy(req.rgb16 + skip1, req.rgb16 + skip2, bytes);
            std::memcpy(req.rgb16 + skip2, tmp, bytes);
          }
          delete[] tmp;
        }

        return true;
      }

