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

#endif
