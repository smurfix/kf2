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

#ifndef KF_CDECNUMBER_H
#define KF_CDECNUMBER_H

#include <boost/multiprecision/mpfr.hpp>
typedef boost::multiprecision::number<boost::multiprecision::mpfr_float_backend<0>> decNumber;

#define LOW_PRECISION 20u

#include <cassert>
#include <string>

class Precision
{
	unsigned digits10;
public:
	inline Precision(unsigned digits10)
	: digits10(digits10)
	{
		decNumber::default_precision(digits10);
	};
	inline ~Precision()
	{
		decNumber::default_precision(digits10);
	};
};

class CDecNumber
{
public:
	decNumber m_dec;

  inline CDecNumber()
  {
		m_dec.precision(std::max(decNumber::default_precision(), LOW_PRECISION));
		m_dec = 0;
	};
  inline CDecNumber(const CDecNumber &a)
  {
		m_dec.precision(std::max(decNumber::default_precision(), a.m_dec.precision()));
		m_dec = a.m_dec;
	};
  inline CDecNumber(const decNumber &a)
  {
		m_dec.precision(std::max(decNumber::default_precision(), a.precision()));
		m_dec = a;
	};
  inline CDecNumber(const char *a)
  {
		m_dec.precision(std::max(decNumber::default_precision(), unsigned(strlen(a))));
		Precision p(m_dec.precision());
		m_dec = decNumber(a);
	};
  inline CDecNumber(const std::string &a)
  : CDecNumber(a.c_str())
  {
	};
  inline CDecNumber(double a)
  {
		m_dec.precision(std::max(decNumber::default_precision(), LOW_PRECISION));
		m_dec = a;
	};
  inline CDecNumber(int a)
  {
		m_dec.precision(std::max(decNumber::default_precision(), LOW_PRECISION));
		m_dec = a;
	};
  inline ~CDecNumber()
  {
	};

  inline CDecNumber &operator*=(double b)
  {
		m_dec *= b;
		return *this;
	}

  inline friend CDecNumber operator-(const CDecNumber &a);
  inline friend CDecNumber operator+(const CDecNumber &a, const CDecNumber &b);
  inline friend CDecNumber operator-(const CDecNumber &a, const CDecNumber &b);
  inline friend CDecNumber operator*(const CDecNumber &a, const CDecNumber &b);
  inline friend CDecNumber operator/(const CDecNumber &a, const CDecNumber &b);
  inline friend CDecNumber operator^(const CDecNumber &a, int b);

  inline friend bool operator>(const CDecNumber &a, int b);
  inline friend bool operator<(const CDecNumber &a, int b);
  inline friend bool operator==(const CDecNumber &a, int b);
  inline friend bool operator<(const CDecNumber &a, const CDecNumber &b);

  inline friend CDecNumber sqrt(const CDecNumber &a);

  std::string ToText() const;

  inline int ToInt() const
  {
		return int(m_dec);
	};
  explicit inline operator double() const
  {
	return mpfr_get_d(m_dec.backend().data(), MPFR_RNDN);
  };
};

inline CDecNumber operator-(const CDecNumber &a)
{
	Precision p(std::max(decNumber::default_precision(), a.m_dec.precision()));
	return CDecNumber(-a.m_dec);
}

inline CDecNumber operator+(const CDecNumber &a, const CDecNumber &b)
{
	Precision p(std::max(decNumber::default_precision(), std::max(a.m_dec.precision(), b.m_dec.precision())));
	return CDecNumber(a.m_dec + b.m_dec);
}

inline CDecNumber operator+(const CDecNumber &a, double b)
{
	Precision p(std::max(decNumber::default_precision(), a.m_dec.precision()));
	return CDecNumber(a.m_dec + b);
}

inline CDecNumber operator-(const CDecNumber &a, const CDecNumber &b)
{
	Precision p(std::max(decNumber::default_precision(), std::max(a.m_dec.precision(), b.m_dec.precision())));
	return CDecNumber(a.m_dec - b.m_dec);
}

inline CDecNumber operator*(const CDecNumber &a, const CDecNumber &b)
{
	Precision p(std::max(decNumber::default_precision(), std::max(a.m_dec.precision(), b.m_dec.precision())));
	return CDecNumber(a.m_dec * b.m_dec);
}

inline CDecNumber operator*(const double &a, const CDecNumber &b)
{
	Precision p(std::max(decNumber::default_precision(), std::max(53u, b.m_dec.precision())));
	return CDecNumber(a * b.m_dec);
}

inline CDecNumber operator*(const CDecNumber &b, const double &a)
{
	Precision p(std::max(decNumber::default_precision(), std::max(53u, b.m_dec.precision())));
	return CDecNumber(b.m_dec * a);
}

inline CDecNumber operator/(const CDecNumber &a, const CDecNumber &b)
{
	Precision p(std::max(decNumber::default_precision(), std::max(a.m_dec.precision(), b.m_dec.precision())));
	if (b.m_dec == 0)
		return CDecNumber(0);
	return CDecNumber(a.m_dec / b.m_dec);
}

inline CDecNumber operator^(const CDecNumber &a, int b)
{
	Precision p(std::max(decNumber::default_precision(), a.m_dec.precision()));
	using std::pow;
	return CDecNumber(pow(a.m_dec, b));
}

inline bool operator>(const CDecNumber &a, int b)
{
	return a.m_dec > b;
}

inline bool operator<(const CDecNumber &a, int b)
{
	return a.m_dec < b;
}

inline bool operator==(const CDecNumber &a, int b)
{
	return a.m_dec == b;
}

inline bool operator<(const CDecNumber &a, const CDecNumber &b)
{
	return a.m_dec < b.m_dec;
}

inline CDecNumber sqrt(const CDecNumber &a)
{
	Precision p(std::max(decNumber::default_precision(), a.m_dec.precision()));
	using std::sqrt;
	return CDecNumber(sqrt(a.m_dec));
}

inline CDecNumber abs(const CDecNumber &a)
{
	Precision p(std::max(decNumber::default_precision(), a.m_dec.precision()));
	using std::abs;
	return CDecNumber(abs(a.m_dec));
}

inline CDecNumber log(const CDecNumber &a)
{
	Precision p(std::max(decNumber::default_precision(), a.m_dec.precision()));
	using std::log;
	return CDecNumber(log(a.m_dec));
}

inline CDecNumber exp(const CDecNumber &a)
{
	Precision p(std::max(decNumber::default_precision(), a.m_dec.precision()));
	using std::exp;
	return CDecNumber(exp(a.m_dec));
}

#endif
