/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
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

#ifndef KF_COMPLEX_H
#define KF_COMPLEX_H

#include <algorithm>

template <class tt> class complex
{
public:
	tt m_r, m_i;
	inline complex() noexcept
	: m_r(0)
	, m_i(0)
	{
	}
	inline complex(const tt &r, const tt &i) noexcept
	: m_r(r)
	, m_i(i)
	{
	}
	inline complex(const tt &r) noexcept : m_r(r), m_i(0) { }
	inline complex(const complex<tt> &a) noexcept
	: m_r(a.m_r)
	, m_i(a.m_i)
	{
	}
	inline complex(const int &r) noexcept : m_r(r), m_i(0) { }
	template <typename ss>
	explicit inline operator complex<ss>() const
	{
		return complex<ss>(ss(m_r), ss(m_i));
	}
	inline complex &operator =(const complex &a) noexcept
	{
		m_r = a.m_r;
		m_i = a.m_i;
		return *this;
	}
	inline complex operator *(const complex &a) const noexcept
	{
		return complex<tt>(m_r*a.m_r - m_i*a.m_i, m_r*a.m_i + m_i*a.m_r);
	}
	inline complex &operator *=(const complex &a) noexcept
	{
		return *this = *this * a;
	}
	inline complex operator +(const int &a) const noexcept
	{
		return complex<tt>(m_r + a, m_i);
	}
	inline complex operator -(const int &a) const noexcept
	{
		return complex<tt>(m_r - a, m_i);
	}
	inline complex operator +(const complex &a) const noexcept
	{
		return complex<tt>(m_r + a.m_r, m_i + a.m_i);
	}
	inline complex operator -(const complex &a) const noexcept
	{
		return complex<tt>(m_r - a.m_r, m_i - a.m_i);
	}
	inline complex &operator +=(const complex &a) noexcept
	{
		m_r += a.m_r; 
		m_i += a.m_i;
		return *this;
	}
	inline complex &operator -=(const complex &a) noexcept
	{
		m_r -= a.m_r; 
		m_i -= a.m_i;
		return *this;
	}
	inline complex operator ^(int exp) const noexcept
	{
		return pow(*this, exp);
	}
	inline complex &operator /=(const complex &b) noexcept
	{
		return *this = *this / b;
	}
};

template <typename S, typename T>
inline complex<T> operator*(const S &a, const complex<T> &b) noexcept
{
	return complex<T>(a * b.m_r, a * b.m_i);
}

template <typename S, typename T>
inline complex<T> operator*(const complex<T> &b, const S &a) noexcept
{
	return complex<T>(b.m_r * a, b.m_i * a);
}

template <class tt>
inline complex<tt> operator/(const complex<tt> &a, const tt &b) noexcept
{
	return complex<tt>(a.m_r / b, a.m_i / b);
}

template <class tt>
inline complex<tt> operator/(const complex<tt> &a, const complex<tt> &b) noexcept
{
	tt div = (b.m_r*b.m_r + b.m_i*b.m_i);
	return complex<tt>(a.m_r*b.m_r + a.m_i*b.m_i, a.m_i*b.m_r - a.m_r*b.m_i)/div;
}

template <class tt>
inline complex<tt> operator/(const tt &a, const complex<tt> &b) noexcept
{
	return complex<tt>(a) / b;
}

template <class tt>
inline complex<tt> operator/(const int &a, const complex<tt> &b) noexcept
{
	return complex<tt>(a) / b;
}

template <class tt>
inline complex<tt> operator/(const complex<tt> &a, const int &b) noexcept
{
	return complex<tt>(a.m_r / b, a.m_i / b);
}

template <class tt>
inline complex<tt> operator-(int a, const complex<tt> &b) noexcept
{
	return complex<tt>(a - b.m_r, -b.m_i);
}

template <class tt>
inline complex<tt> operator+(int a, const complex<tt> &b) noexcept
{
	return complex<tt>(a + b.m_r, b.m_i);
}


template <class tt>
inline complex<tt> operator-(const complex<tt> &b) noexcept
{
	return complex<tt>(-b.m_r, -b.m_i);
}

template <class tt>
inline tt norm(const complex<tt> &a) noexcept
{
	return a.m_r * a.m_r + a.m_i * a.m_i;
}

template <class tt>
inline tt abs(const complex<tt> &a) noexcept
{
	return sqrt(norm(a));
}

template <class tt>
inline complex<tt> sqrt(const complex<tt> &a) noexcept
{
	using std::sqrt;
	using std::max;
	tt r = abs(a);
	return complex<tt>(sqrt(max(0.0, (r + a.m_r)*0.5)), ((a.m_i >= 0) - (0 > a.m_i)) * sqrt(max(0.0, (r - a.m_r)*0.5)));
}

template <typename tt>
inline complex<tt> cinfnan_to_zero(const complex<tt> &a) noexcept
{
	return complex<tt>(infnan_to_zero(a.m_r), infnan_to_zero(a.m_i));
}

template <class tt>
inline complex<tt> exp(const complex<tt> &a) noexcept
{
	return cinfnan_to_zero(exp(a.m_r) * complex<tt>(cos(a.m_i), sin(a.m_i)));
}

template <class tt>
inline tt cosm1(const tt &x) noexcept
{
	tt s(sin(x / 2));
	return -2 * s * s;
}

template <class tt>
inline complex<tt> expm1(const complex<tt> &a) noexcept
{
	return cinfnan_to_zero(complex<tt>(expm1(a.m_r) * cos(a.m_i) + cosm1(a.m_i), exp(a.m_r) * sin(a.m_i)));
}

template <class tt>
inline complex<tt> sin(const complex<tt> &a) noexcept
{
	using std::cos;
	using std::sin;
	using std::cosh;
	using std::sinh;
	return complex<tt>(sin(a.m_r) * cosh(a.m_i), cos(a.m_r) * sinh(a.m_i));
}

template <class tt>
inline complex<tt> cos(const complex<tt> &a) noexcept
{
	using std::cos;
	using std::sin;
	using std::cosh;
	using std::sinh;
	return complex<tt>(cos(a.m_r) * cosh(a.m_i), sin(a.m_r) * sinh(a.m_i));
}

template <class tt>
inline complex<tt> sinh(const complex<tt> &a) noexcept
{
	return (expm1(a) - expm1(-a)) / 2;
}

template <class tt>
inline complex<tt> cosh(const complex<tt> &a) noexcept
{
	return (expm1(a) + expm1(-a)) / 2;
}

template <typename R>
inline complex<R> sqr(const complex<R> &a) noexcept
{
	return complex<R>(sqr(a.m_r) - sqr(a.m_i), 2 * a.m_r * a.m_i);
}

template <typename R>
inline std::ostream& operator<<(std::ostream& a, const complex<R>& b) noexcept
{
	return a << "(" << b.m_r << " " << b.m_i << ")";
}


#endif
