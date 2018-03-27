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
  std::cerr << "usage: " << progname << " settings.kfs parameter.kfr factor" << std::endl;
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
  if (argc != 4)
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
  // parse settings
  KVV kfs = read(kfsname);
  const int width = std::atoi(lookup(kfs, "ImageHeight").c_str());
  const int height = std::atoi(lookup(kfs, "ImageWidth").c_str());
  const int seed = std::atoi(lookup(kfs, "JitterSeed").c_str());
  const double jscale = std::atof(lookup(kfs, "JitterScale").c_str());
  std::ostringstream sjscale;
  sjscale << (jscale / factor);
  update(kfs, "JitterScale", sjscale.str());
  // parse parameter
  KVV kfr = read(kfrname);
  mpfr53 zoom(lookup(kfr, "Zoom"));
  int bits = std::max(53, int(53 + mpfr_get_exp(zoom.backend().data()) + std::ceil(std::log2(double(factor)))));
  int precision = std::ceil(LOG_10_2 * bits);
  mpfr::default_precision(precision);
  mpfr re(lookup(kfr, "Re"));
  mpfr im(lookup(kfr, "Im"));
  int colormethod = std::atoi(lookup(kfr, "ColorMethod").c_str());
  int differences = std::atoi(lookup(kfr, "Differences").c_str());
  double coloroffset = std::atof(lookup(kfr, "ColorOffset").c_str());
  double iterdiv = std::atof(lookup(kfr, "IterDiv").c_str());
  if (iterdiv == 0.0) iterdiv = 1.0;
  double rotate = std::atof(lookup(kfr, "Rotate").c_str());
  double co = cos(rotate);
  double si = sin(rotate);
  double ratio = std::atof(lookup(kfr, "Ratio").c_str());
  if (ratio != 0 && ratio != 360)
  {
    std::cerr << argv[0] << ": skew ratio must be 360 (got `" << ratio << "')" << std::endl;
    return 1;
  }
  double aspect = double(width) / (double) height;
  // tile
  std::string stem;
  std::string::size_type ix = kfrname.rfind('.');
  if (ix == std::string::npos)
    stem = kfrname;
  else
    stem = kfrname.substr(0, ix);
  bool stratify = colormethod == 7 && differences == 7;
  if (stratify)
  {
    mpfr53 tpx(8 / (zoom * height));
    for (int j = 0; j < factor; ++j)
      for (int i = 0; i < factor; ++i)
      {
        double x = ((i + 0.5) / factor - 0.5);
        double y = ((j + 0.5) / factor - 0.5);
        mpfr tre(re + mpfr(mpfr53(  co * x + si * y) * tpx));
        mpfr tim(im + mpfr(mpfr53(- si * x + co * y) * tpx));
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
    if (colormethod == 7)
    {
      std::ostringstream tcoloroffset;
      tcoloroffset << (coloroffset + log(factor) / iterdiv);
      update(kfr, "ColorOffset", tcoloroffset.str());
    }
    mpfr53 tzoom(zoom * factor);
    update(kfr, "Zoom", scientific(tzoom));
    for (int j = 0; j < factor; ++j)
      for (int i = 0; i < factor; ++i)
      {
        double x = ((i + 0.5) / factor - 0.5) * 4 * aspect;
        double y = ((j + 0.5) / factor - 0.5) * 4;
        mpfr tre(re + mpfr(mpfr53(  co * x + si * y) / zoom));
        mpfr tim(im + mpfr(mpfr53(- si * x + co * y) / zoom));
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
