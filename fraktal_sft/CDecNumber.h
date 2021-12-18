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

#include "floatexp.h"

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

	template <typename mantissa, typename exponent>
	inline CDecNumber(const tfloatexp<mantissa, exponent> &a) noexcept
	{
		m_dec.precision(std::max(decNumber::default_precision(), LOW_PRECISION));
		if (a.exp > exponent(INT_MAX))
		{
			m_dec = 1.0 / 0.0;
		}
		else if (a.exp < exponent(INT_MIN))
		{
			m_dec = 0.0;
		}
		else
		{
			m_dec = a.val;
			if (a.exp >= 0)
				mpfr_mul_2ui(m_dec.backend().data(), m_dec.backend().data(), a.exp, MPFR_RNDN);
			else
				mpfr_div_2ui(m_dec.backend().data(), m_dec.backend().data(), -a.exp, MPFR_RNDN);
		}
	}

	template <typename mantissa, typename exponent>
	explicit inline operator tfloatexp<mantissa, exponent>() noexcept
	{
		signed long int e = 0;
		double v = mpfr_get_d_2exp(&e, m_dec.backend().data(), MPFR_RNDN);
		return tfloatexp<mantissa, exponent>(v, e);
	}

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

inline CDecNumber operator*(const int &a, const CDecNumber &b)
{
	Precision p(std::max(decNumber::default_precision(), std::max(53u, b.m_dec.precision())));
	return CDecNumber(a * b.m_dec);
}

inline CDecNumber operator*(const CDecNumber &b, const int &a)
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

inline CDecNumber sqr(const CDecNumber &a)
{
	Precision p(std::max(decNumber::default_precision(), a.m_dec.precision()));
	CDecNumber r;
	mpfr_sqr(r.m_dec.backend().data(), a.m_dec.backend().data(), MPFR_RNDN);
	return r;
}

inline std::ostream& operator<<(std::ostream& a, const CDecNumber& b) noexcept
{
	return a << b.m_dec;
}

#endif
