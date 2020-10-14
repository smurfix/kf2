/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2020 Claude Heiland-Allen

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
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <utility> // pair
#include <vector>

#include "../common/matrix.h"

#include <boost/multiprecision/mpfr.hpp>
typedef boost::multiprecision::number<boost::multiprecision::mpfr_float_backend<53>> mpfr53;
typedef boost::multiprecision::number<boost::multiprecision::mpfr_float_backend<0>> mpfr;
#define LOG_10_2 0.30102999566398114
static const double deg = 360 / 6.283185307179586;

std::string fixed(const mpfr &f)
{
  std::ostringstream os;
  os << std::fixed << std::setprecision(f.precision() + 3) << f;
  std::string s = os.str();
  std::string::size_type e = s.find('e');
  if (e != std::string::npos)
    s[e] = 'E';
  return s;
}

std::string scientific(const mpfr53 &f)
{
  std::ostringstream os;
  os << std::scientific << std::setprecision(int(LOG_10_2 * 53 + 3)) << f;
  std::string s = os.str();
  std::string::size_type e = s.find('e');
  if (e != std::string::npos)
    s[e] = 'E';
  e = s.find('E');
  if (e != std::string::npos)
    if (s[e + 1] == '+')
      s.erase(e + 1, 1);
  return s;
}

void usage(const char *progname)
{
  std::cerr << "usage: " << progname << " settings.kfs parameter.kfr factor [--auto|--adjacent|--stratify]" << std::endl;
}

typedef std::pair<std::string, std::string> KV;
typedef std::vector<KV> KVV;

KVV read(const std::string &filename)
{
  KVV kvv;
  std::ifstream input(filename);
  std::string line;
  while (std::getline(input, line))
  {
    std::string::size_type ix = line.find(':');
    if (ix != std::string::npos)
    {
      std::string key = line.substr(0, ix);
      std::string val = line.substr(ix + 2, std::string::npos);
      KV kv(key, val);
      kvv.push_back(kv);
    }
  }
  return kvv;
}

void write(const std::string &filename, const KVV &kvv)
{
  std::ofstream output(filename);
  for (auto kv : kvv)
    output << kv.first << ": " << kv.second << std::endl;
}

std::string lookup(const KVV &kvv, const std::string &key)
{
  for (auto kv : kvv)
    if (kv.first == key)
      return kv.second;
  return "";
}

KVV &update(KVV &kvv, const std::string &key, const std::string &val)
{
  for (size_t i = 0; i < kvv.size(); ++i)
    if (kvv[i].first == key)
    {
      kvv[i].second = val;
      return kvv;
    }
  // not found, insert it
  KV kv(key, val);
  kvv.push_back(kv);
  return kvv;
}

extern int main(int argc, char **argv)
{
  if (argc != 4 && argc != 5)
  {
    usage(argv[0]);
    return 1;
  }
  const std::string kfsname(argv[1]);
  const std::string kfrname(argv[2]);
  const int factor = std::atoi(argv[3]);
  if (factor <= 0)
  {
    std::cerr << argv[0] << ": factor must be > 0 (got `" << factor << "')" << std::endl;
    return 1;
  }
  bool force_adjacent = false;
  bool force_stratify = false;
  if (argc == 5)
  {
    std::string arg = std::string(argv[4]);
    if (arg == "--adjacent")
    {
      force_adjacent = true;
    }
    else if (arg == "--stratify")
    {
      force_stratify = true;
    }
    else if (arg != "--auto")
    {
      std::cerr << argv[0] << ": unknown mode switch" << std::endl;
      return 1;
    }
  }
  // parse settings
  KVV kfs = read(kfsname);
  const bool expmap = std::atoi(lookup(kfs, "ExponentialMap").c_str());
  if (expmap)
  {
    std::cerr << argv[0] << ": can't tile (ExponentialMap)\n" << std::endl;
    return 1;
  }
  const int width = std::atoi(lookup(kfs, "ImageWidth").c_str());
  const int height = std::atoi(lookup(kfs, "ImageHeight").c_str());
  const int seed = std::atoi(lookup(kfs, "JitterSeed").c_str());
  const double jscale = std::atof(lookup(kfs, "JitterScale").c_str());
  // parse parameter
  KVV kfr = read(kfrname);
  const std::string ratio = lookup(kfr, "Ratio");
  if (ratio != "" && std::atof(ratio.c_str()) != 360.0)
  {
    std::cerr << argv[0] << ": can't tile (old-style skew, Ratio != 360)\n" << std::endl;
    return 1;
  }
  const std::string rotate = lookup(kfr, "Rotate");
  if (rotate != "" && std::atof(rotate.c_str()) != 0.0)
  {
    std::cerr << argv[0] << ": can't tile (old-style angle, Rotate != 0)\n" << std::endl;
    return 1;
  }
  mpfr53 zoom(lookup(kfr, "Zoom"));
  int bits = std::max(53, int(53 + mpfr_get_exp(zoom.backend().data()) + std::ceil(std::log2(double(factor)))));
  int precision = std::ceil(LOG_10_2 * bits);
  mpfr::default_precision(precision);
  mpfr re(lookup(kfr, "Re"));
  mpfr im(lookup(kfr, "Im"));
  int colormethod = std::atoi(lookup(kfr, "ColorMethod").c_str());
  int differences = std::atoi(lookup(kfr, "Differences").c_str());
  int slopes = std::atoi(lookup(kfr, "Slopes").c_str());
#if 0
  double coloroffset = std::atof(lookup(kfr, "ColorOffset").c_str());
#endif
  double iterdiv = std::atof(lookup(kfr, "IterDiv").c_str());
  if (iterdiv == 0.0) iterdiv = 1.0;

  double rotate_angle = std::atof(lookup(kfr, "RotateAngle").c_str()) / deg;
  double stretch_angle = std::atof(lookup(kfr, "StretchAngle").c_str()) / deg;
  double stretch_factor = std::exp2(std::atof(lookup(kfr, "StretchAmount").c_str()));
  mat2 transform = glm::transpose(polar_composition(polar2(1, rotate_angle, stretch_factor, stretch_angle)));

  double aspect = double(width) / (double) height;
  // tile
  std::string stem;
  std::string::size_type ix = kfrname.rfind('.');
  if (ix == std::string::npos)
    stem = kfrname;
  else
    stem = kfrname.substr(0, ix);
  bool adjacent = (slopes && differences != 7) || (5 <= colormethod && colormethod <= 8 && differences != 7) || force_adjacent;
  bool stratify = ! adjacent || force_stratify;
  if (stratify)
  {
    std::cerr << "using stratified tiling (best quality)" << std::endl
              << "after rendering to EXR, assemble final image with exrtactile:" << std::endl
              << "exrtactile " << stem << " " << factor << " 1 " << stem << ".exr" << std::endl;
    std::ostringstream sjscale;
    sjscale << (jscale / factor);
    update(kfs, "JitterScale", sjscale.str());
    for (int j = 0; j < factor; ++j)
      for (int i = 0; i < factor; ++i)
      {
        double u = ((i + 0.5) / factor - 0.5) * 4 / height;
        double v = ((j + 0.5) / factor - 0.5) * 4 / height;
        vec2 p = transform * vec2(u, v);
        mpfr tre(re + mpfr(mpfr53(p.x) / zoom));
        mpfr tim(im + mpfr(mpfr53(p.y) / zoom));
        // output
        std::ostringstream sjseed;
        sjseed << (seed ? seed * factor * factor + j * factor + i : 0);
        std::ostringstream s;
        s << stem << "-" << std::setfill('0') << std::setw(4) << j << "-" << std::setfill('0') << std::setw(4) << i << ".kfs";
        write(s.str(), update(kfs, "JitterSeed", sjseed.str()));
        std::ostringstream r;
        r << stem << "-" << std::setfill('0') << std::setw(4) << j << "-" << std::setfill('0') << std::setw(4) << i << ".kfr";
        write(r.str(), update(update(kfr, "Re", fixed(tre)), "Im", fixed(tim)));
      }
  }
  else
  {
    std::cerr << "using adjacent tiling (seams may be visible)" << std::endl
              << (slopes && differences != 7 ? "because of: numerical differencing for slopes (try analytic instead)\n" : "")
              << (5 <= colormethod && colormethod <= 8 && differences != 7 ? "because of: numerical differencing for DE (try analytic instead)\n" : "")
              << "after rendering to EXR, assemble final image with exrtactile:" << std::endl
              << "exrtactile " << stem << " " << factor << " 0 " << stem << ".exr" << std::endl
              << "or, after rendering to PNG, assemble final image with ImageMagick:" << std::endl
              << "montage " << stem << "-*-*.png -mode Concatenate -tile " << factor << "x" << factor << " " << stem << ".png" << std::endl;
#if 0
    // makes tiled image colours more similar to one-shot huge image colours
    // not necessarily desireable...
    if (colormethod == 7)
    {
      std::ostringstream tcoloroffset;
      tcoloroffset << (coloroffset + log(factor) / iterdiv);
      update(kfr, "ColorOffset", tcoloroffset.str());
    }
#endif
    mpfr53 tzoom(zoom * factor);
    update(kfr, "Zoom", scientific(tzoom));
    for (int j = 0; j < factor; ++j)
      for (int i = 0; i < factor; ++i)
      {
        double u = ((i + 0.5) / factor - 0.5) * 4 * aspect;
        double v = ((j + 0.5) / factor - 0.5) * 4;
        vec2 p = transform * vec2(u, v);
        mpfr tre(re + mpfr(mpfr53(p.x) / zoom));
        mpfr tim(im + mpfr(mpfr53(p.y) / zoom));
        // output
        std::ostringstream sjseed;
        sjseed << (seed ? seed * factor * factor + j * factor + i : 0);
        std::ostringstream s;
        s << stem << "-" << std::setfill('0') << std::setw(4) << j << "-" << std::setfill('0') << std::setw(4) << i << ".kfs";
        write(s.str(), update(kfs, "JitterSeed", sjseed.str()));
        std::ostringstream r;
        r << stem << "-" << std::setfill('0') << std::setw(4) << j << "-" << std::setfill('0') << std::setw(4) << i << ".kfr";
        write(r.str(), update(update(kfr, "Re", fixed(tre)), "Im", fixed(tim)));
      }
  }
  return 0;
}
