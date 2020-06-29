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

#ifndef KF_COMPLEX_H
#define KF_COMPLEX_H

#include "../formula/formula.h"
#include "floatexp.h"

#include <algorithm>

template <class tt> class complex
{
public:
	tt m_r, m_i;
	inline complex()
	: m_r(0)
	, m_i(0)
	{
	}
	inline complex(const tt &r, const tt &i)
	: m_r(r)
	, m_i(i)
	{
	}
	inline complex(const int &r) : m_r(r), m_i(0) { }
	inline complex(const int64_t &r) : m_r(r), m_i(0) { }
	inline complex(const float &r) : m_r(r), m_i(0) { }
	inline complex(const double &r) : m_r(r), m_i(0) { }
	inline complex(const long double &r) : m_r(r), m_i(0) { }
	inline complex(const floatexp &r) : m_r(r), m_i(0) { }
	inline complex(const complex<double> &a)
	: m_r(a.m_r)
	, m_i(a.m_i)
	{
	}
	inline complex(const complex<long double> &a)
	: m_r(a.m_r)
	, m_i(a.m_i)
	{
	}
	inline complex(const complex<floatexp> &a)
	: m_r(a.m_r)
	, m_i(a.m_i)
	{
	}
	explicit inline operator complex<floatexp>() const
	{
		return complex<floatexp>(floatexp(m_r), floatexp(m_i));
	}
	inline complex &operator =(const complex &a)
	{
		m_r = a.m_r;
		m_i = a.m_i;
		return *this;
	}
	inline complex operator *(const complex &a) const
	{
		complex<tt> r;
		r.m_r = m_r*a.m_r - m_i*a.m_i;
		r.m_i = m_r*a.m_i + m_i*a.m_r;
		return r;
	}
	inline complex &operator *=(const complex &a)
	{
		return *this = *this * a;
	}
	inline complex operator +(const int &a) const
	{
		complex<tt> r;
		r.m_r = m_r + a;
		r.m_i = m_i;
		return r;
	}
	inline complex operator +(const complex &a) const
	{
		complex<tt> r;
		r.m_r = m_r + a.m_r; 
		r.m_i = m_i + a.m_i;
		return r;
	}
	inline complex operator -(const complex &a) const
	{
		complex<tt> r;
		r.m_r = m_r - a.m_r; 
		r.m_i = m_i - a.m_i;
		return r;
	}
	inline complex &operator +=(const complex &a)
	{
		m_r += a.m_r; 
		m_i += a.m_i;
		return *this;
	}
	inline complex &operator -=(const complex &a)
	{
		m_r -= a.m_r; 
		m_i -= a.m_i;
		return *this;
	}
	inline complex operator ^(int exp) const
	{
		return pow(*this, exp);
	}
	inline complex &operator /=(const complex &b)
	{
		return *this = *this / b;
	}
};

template <class tt>
inline complex<tt> operator*(int a, const complex<tt> &b)
{
	return complex<tt>(a * b.m_r, a * b.m_i);
}

template <class tt>
inline complex<tt> operator*(const complex<tt> &b, int a)
{
	return complex<tt>(a * b.m_r, a * b.m_i);
}

template <class tt>
inline complex<tt> operator*(const tt &a, const complex<tt> &b)
{
	return complex<tt>(a * b.m_r, a * b.m_i);
}

template <class tt>
inline complex<tt> operator/(const complex<tt> &a, const tt &b)
{
	return complex<tt>(a.m_r / b, a.m_i / b);
}

template <class tt>
inline complex<tt> operator/(const complex<tt> &a, const complex<tt> &b)
{
	tt div = (b.m_r*b.m_r + b.m_i*b.m_i);
	return complex<tt>(a.m_r*b.m_r + a.m_i*b.m_i, a.m_i*b.m_r - a.m_r*b.m_i)/div;
}

template <class tt>
inline complex<tt> operator/(const tt &a, const complex<tt> &b)
{
	return complex<tt>(a) / b;
}

template <class tt>
inline complex<tt> operator/(const int &a, const complex<tt> &b)
{
	return complex<tt>(a) / b;
}

template <class tt>
inline complex<tt> operator/(const complex<tt> &a, const int &b)
{
	return complex<tt>(a.m_r / b, a.m_i / b);
}

template <typename ss, typename tt>
inline complex<tt> operator*(const complex<ss> &a, const complex<tt> &b)
{
	return complex<tt>(a) * b;
}

template <class tt>
inline complex<tt> operator-(int a, const complex<tt> &b)
{
	return complex<tt>(a - b.m_r, -b.m_i);
}

template <class tt>
inline complex<tt> operator+(int a, const complex<tt> &b)
{
	return complex<tt>(a + b.m_r, b.m_i);
}

template <class tt>
inline complex<tt> operator-(const complex<tt> &b, int a)
{
	return complex<tt>(b.m_r - a, b.m_i);
}


template <class tt>
inline complex<tt> operator-(const complex<tt> &b)
{
	return complex<tt>(-b.m_r, -b.m_i);
}

template <class tt>
inline tt norm(const complex<tt> &a)
{
	return a.m_r * a.m_r + a.m_i * a.m_i;
}

template <class tt>
inline tt abs(const complex<tt> &a)
{
	return sqrt(norm(a));
}

template <class tt>
inline complex<tt> sqrt(const complex<tt> &a)
{
	using std::sqrt;
	using std::max;
	tt r = abs(a);
	return complex<tt>(sqrt(max(0.0, (r + a.m_r)*0.5)), ((a.m_i >= 0) - (0 > a.m_i)) * sqrt(max(0.0, (r - a.m_r)*0.5)));
}

template <typename tt>
inline complex<tt> cinfnan_to_zero(const complex<tt> &a)
{
	return complex<tt>(infnan_to_zero(a.m_r), infnan_to_zero(a.m_i));
}

template <class tt>
inline complex<tt> exp(const complex<tt> &a)
{
	return cinfnan_to_zero(exp(a.m_r) * complex<tt>(cos(a.m_i), sin(a.m_i)));
}

template <class tt>
inline tt cosm1(const tt &x)
{
	tt s(sin(x / 2));
	return -2 * s * s;
}

template <class tt>
inline complex<tt> expm1(const complex<tt> &a)
{
	return cinfnan_to_zero(complex<tt>(expm1(a.m_r) * cos(a.m_i) + cosm1(a.m_i), exp(a.m_r) * sin(a.m_i)));
}

template <class tt>
inline complex<tt> sinh(const complex<tt> &a)
{
	return (expm1(a) - expm1(-a)) / 2;
}

template <typename R>
inline complex<R> sqr(const complex<R> &a) noexcept
{
	return complex<R>(sqr(a.m_r) - sqr(a.m_i), 2 * a.m_r * a.m_i);
}

#endif
