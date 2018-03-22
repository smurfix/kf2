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

#include <boost/multiprecision/mpfr.hpp>
typedef boost::multiprecision::number<boost::multiprecision::mpfr_float_backend<53>> mpfr53;
typedef boost::multiprecision::number<boost::multiprecision::mpfr_float_backend<0>> mpfr;
#define LOG_10_2 0.30102999566398114

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
  std::cerr << "usage: " << progname << " width height factor input.kfr" << std::endl;
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
  if (argc != 5)
  {
    usage(argv[0]);
    return 1;
  }
  const int width = std::atoi(argv[1]);
  const int height = std::atoi(argv[2]);
  const int factor = std::atoi(argv[3]);
  const std::string filename(argv[4]);
  if (width <= 0)
  {
    std::cerr << argv[0] << ": width must be > 0 (got `" << width << "')" << std::endl;
    return 1;
  }
  if (height <= 0)
  {
    std::cerr << argv[0] << ": height must be > 0 (got `" << height << "')" << std::endl;
    return 1;
  }
  if (factor <= 0)
  {
    std::cerr << argv[0] << ": factor must be > 0 (got `" << factor << "')" << std::endl;
    return 1;
  }
  KVV kvv = read(filename);
  // parse
  mpfr53 zoom(lookup(kvv, "Zoom"));
  int bits = std::max(53, int(53 + mpfr_get_exp(zoom.backend().data()) + std::ceil(std::log2(double(factor)))));
  int precision = std::ceil(LOG_10_2 * bits);
  mpfr::default_precision(precision);
  mpfr re(lookup(kvv, "Re"));
  mpfr im(lookup(kvv, "Im"));
  double rotate = std::atof(lookup(kvv, "Rotate").c_str());
  double co = cos(rotate);
  double si = sin(rotate);
  double ratio = std::atof(lookup(kvv, "Ratio").c_str());
  if (ratio != 0 && ratio != 360)
  {
    std::cerr << argv[0] << ": skew ratio must be 360 (got `" << ratio << "')" << std::endl;
    return 1;
  }
  double aspect = double(width) / (double) height;
  // tile
  std::string stem;
  std::string::size_type ix = filename.rfind('.');
  if (ix == std::string::npos)
    stem = filename;
  else
    stem = filename.substr(0, ix);
  mpfr53 tzoom(zoom * factor);
  for (int j = 0; j < factor; ++j)
    for (int i = 0; i < factor; ++i)
    {
      double x = ((i + 0.5) / factor - 0.5) * 4 * aspect;
      double y = ((j + 0.5) / factor - 0.5) * 4;
      mpfr tre(re + mpfr(mpfr53(  co * x + si * y) / zoom));
      mpfr tim(im + mpfr(mpfr53(- si * x + co * y) / zoom));
      std::ostringstream o;
      o << stem << "-" << std::setfill('0') << std::setw(4) << j << "-" << std::setfill('0') << std::setw(4) << i << ".kfr";
      write(o.str(), update(update(update(kvv, "Re", fixed(tre)), "Im", fixed(tim)), "Zoom", scientific(tzoom)));
    }
  return 0;
}
