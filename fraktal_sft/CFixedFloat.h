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

#ifndef KF_CFIXEDFLOAT_H
#define KF_CFIXEDFLOAT_H

#include "CDecNumber.h"

typedef decNumber FixedFloat;

class CFixedFloat
{
public:
	FixedFloat m_f;

	inline CFixedFloat()
	{
		unsigned p = FixedFloat::default_precision();
		Precision q(p);
		m_f.precision(p);
		m_f = 0;
	};

	inline CFixedFloat(const CFixedFloat &a)
	{
		unsigned p = a.m_f.precision();
		Precision q(p);
		m_f.precision(p);
		m_f = a.m_f;
	};

	inline CFixedFloat(const FixedFloat &a)
	{
		unsigned p = a.precision();
		Precision q(p);
		m_f.precision(p);
		m_f = a;
	};

	inline CFixedFloat(const char *sz)
	{
		m_f.precision(std::max(FixedFloat::default_precision(), unsigned(strlen(sz))));
		Precision p(m_f.precision());
		m_f = FixedFloat(sz);
	};

	inline CFixedFloat(const std::string &sz)
	{
		m_f.precision(std::max(FixedFloat::default_precision(), unsigned(sz.length())));
		Precision p(m_f.precision());
		m_f = FixedFloat(sz);
	};

	inline CFixedFloat(int a)
	{
		unsigned p = FixedFloat::default_precision();
		Precision q(p);
		m_f.precision(p);
		m_f = a;
	};

	inline CFixedFloat(double a)
	{
		unsigned p = FixedFloat::default_precision();
		Precision q(p);
		m_f.precision(p);
		m_f = a;
	};

	template <typename mantissa, typename exponent>
	inline CFixedFloat(const tfloatexp<mantissa, exponent> &a) noexcept
	{
		m_f.precision(std::max(FixedFloat::default_precision(), LOW_PRECISION));
		if (a.exp > exponent(INT_MAX))
		{
			m_f = 1.0 / 0.0;
		}
		else if (a.exp < exponent(INT_MIN))
		{
			m_f = 0.0;
		}
		else
		{
			m_f = a.val;
			if (a.exp >= 0)
				mpfr_mul_2ui(m_f.backend().data(), m_f.backend().data(), a.exp, MPFR_RNDN);
			else
				mpfr_div_2ui(m_f.backend().data(), m_f.backend().data(), -a.exp, MPFR_RNDN);
		}
	}

	template <typename mantissa, typename exponent>
	explicit inline operator tfloatexp<mantissa, exponent>() noexcept
	{
		signed long int e = 0;
		double v = mpfr_get_d_2exp(&e, m_f.backend().data(), MPFR_RNDN);
		return tfloatexp<mantissa, exponent>(v, e);
	}

	inline ~CFixedFloat()
	{
	};

	std::string ToText() const;

	inline int ToInt()
	{
		return int(m_f);
	};

	inline double ToDouble(int nScaling = 0) const
	{
		using std::pow;
		unsigned p = LOW_PRECISION;
		Precision q(p);
		FixedFloat f;
		f.precision(p);
		f = m_f;
		return double(FixedFloat(f * pow(FixedFloat(10), nScaling)));
	};
	inline explicit operator double () const { return ToDouble(); }
	inline long double ToLongDouble(int nScaling = 0) const
	{
		using std::pow;
		unsigned p = LOW_PRECISION;
		Precision q(p);
		FixedFloat f;
		f.precision(p);
		f = m_f;
		return (long double)(FixedFloat(f * pow(FixedFloat(10), nScaling)));
	};
	inline explicit operator long double () const { return ToLongDouble(); }

	inline CFixedFloat Add(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f + A.m_f));
	};

	inline CFixedFloat Subtract(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f - A.m_f));
	};

	inline CFixedFloat Multiply(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f * A.m_f));
	};

	inline CFixedFloat Square() const
	{
		Precision p(m_f.precision());
		FixedFloat r(m_f);
		mpfr_sqr(r.backend().data(), r.backend().data(), MPFR_RNDN);
		return CFixedFloat(r);
	};

	inline CFixedFloat Divide(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f / A.m_f));
	};

	inline CFixedFloat &Double()
	{
		mpfr_mul_2ui(m_f.backend().data(), m_f.backend().data(), 1, MPFR_RNDN);
		return *this;
	};

	inline CFixedFloat &AbsAdd(CFixedFloat &a, CFixedFloat &b) // TODO FIXME avoid copies
	{
		Precision p(std::max(a.m_f.precision(), b.m_f.precision()));
		using std::abs;
		m_f = abs(a.m_f) + abs(b.m_f);
		return *this;
	};

	inline CFixedFloat &Abs()
	{
		mpfr_abs(m_f.backend().data(), m_f.backend().data(), MPFR_RNDN);
		return *this;
	};

	inline bool operator>(const CFixedFloat &A) const
	{
		return m_f > A.m_f;
	};

	inline bool operator<(const CFixedFloat &A) const
	{
		return m_f < A.m_f;
	};

	inline bool operator==(const CFixedFloat &A) const
	{
		return m_f == A.m_f;
	};

	inline CFixedFloat &operator=(const CFixedFloat &A)
	{
		m_f.precision(A.m_f.precision());
		m_f = A.m_f;
		return *this;
	};

	inline CFixedFloat &operator=(const std::string &sz)
	{
		*this = CFixedFloat(sz);
		return *this;
	};

	inline CFixedFloat &operator=(const char *sz)
	{
		*this = CFixedFloat(sz);
		return *this;
	};

	inline CFixedFloat &operator=(int a)
	{
		*this = CFixedFloat(a);
		return *this;
	};

	inline CFixedFloat &operator=(double a)
	{
		*this = CFixedFloat(a);
		return *this;
	};

	inline CFixedFloat operator*(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f * A.m_f));
	};

	inline CFixedFloat operator/(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f / A.m_f));
	};

	inline CFixedFloat operator+(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f + A.m_f));
	};

	inline CFixedFloat operator-(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f - A.m_f));
	};

	inline CFixedFloat operator-() const
	{
		Precision p(m_f.precision());
		return CFixedFloat(FixedFloat(-m_f));
	};

	inline CFixedFloat &operator*=(const CFixedFloat &A)
	{
		*this = *this * A;
		return *this;
	};

	inline CFixedFloat &operator/=(const CFixedFloat &A)
	{
		*this = *this / A;
		return *this;
	};

	inline CFixedFloat &operator+=(const CFixedFloat &A)
	{
		*this = *this + A;
		return *this;
	};

	inline CFixedFloat &operator-=(const CFixedFloat &A)
	{
		*this = *this - A;
		return *this;
	};

	inline CFixedFloat &operator*=(int A)
	{
		m_f *= A;
		return *this;
	};

	inline CFixedFloat &operator/=(int A)
	{
		m_f /= A;
		return *this;
	};

	inline CFixedFloat &operator+=(int A)
	{
		m_f += A;
		return *this;
	};

	inline CFixedFloat &operator-=(int A)
	{
		m_f -= A;
		return *this;
	};

	inline CFixedFloat &operator*=(double A)
	{
		m_f *= A;
		return *this;
	};

	inline CFixedFloat &operator/=(double A)
	{
		m_f /= A;
		return *this;
	};

	inline CFixedFloat &operator+=(double A)
	{
		m_f += A;
		return *this;
	};

	inline CFixedFloat &operator-=(double A)
	{
		m_f -= A;
		return *this;
	};

};

inline CFixedFloat abs(const CFixedFloat &A)
{
	using std::abs;
	Precision p(A.m_f.precision());
	return CFixedFloat(abs(A.m_f));
}
inline CFixedFloat min(const CFixedFloat &A, const CFixedFloat &B)
{
	using std::min;
	Precision p(std::max(A.m_f.precision(), B.m_f.precision()));
	return CFixedFloat(min(A.m_f, B.m_f));
}
inline CFixedFloat max(const CFixedFloat &A, const CFixedFloat &B)
{
	using std::max;
	Precision p(std::max(A.m_f.precision(), B.m_f.precision()));
	return CFixedFloat(max(A.m_f, B.m_f));
}

inline bool operator==(const CFixedFloat &A,int nB)
{
	return A.m_f == nB;
}

inline bool operator==(int nB,const CFixedFloat &A)
{
	return nB == A.m_f;
}

inline bool operator>(const CFixedFloat &A,int nB)
{
	return A.m_f > nB;
}

inline bool operator>(int nB,const CFixedFloat &A)
{
	return nB > A.m_f;
}

inline bool operator>(const CFixedFloat &A,double nB)
{
	return A.m_f > nB;
}

inline bool operator>(double nB,const CFixedFloat &A)
{
	return nB > A.m_f;
}

inline bool operator<(const CFixedFloat &A,int nB)
{
	return A.m_f < nB;
}

inline bool operator<(int nB,const CFixedFloat &A)
{
	return nB < A.m_f;
}

inline bool operator<(const CFixedFloat &A,double nB)
{
	return A.m_f < nB;
}

inline bool operator<(double nB,const CFixedFloat &A)
{
	return nB < A.m_f;
}

inline CFixedFloat operator*(const CFixedFloat &A,long nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f * nB));
}

inline CFixedFloat operator*(long nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB * A.m_f));
}

inline CFixedFloat operator*(const CFixedFloat &A,int nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f * nB));
}

inline CFixedFloat operator*(int nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB * A.m_f));
}

inline CFixedFloat operator*(const CFixedFloat &A,double nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f * nB));
}

inline CFixedFloat operator*(double nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB * A.m_f));
}

inline CFixedFloat operator/(const CFixedFloat &A,int nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f / nB));
}

inline CFixedFloat operator/(int nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB / A.m_f));
}

inline CFixedFloat operator/(const CFixedFloat &A,double nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f / nB));
}

inline CFixedFloat operator/(double nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB / A.m_f));
}

inline CFixedFloat operator+(const CFixedFloat &A,int nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f + nB));
}

inline CFixedFloat operator+(int nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB + A.m_f));
}

inline CFixedFloat operator+(const CFixedFloat &A,double nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f + nB));
}

inline CFixedFloat operator+(double nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB + A.m_f));
}

inline CFixedFloat operator-(const CFixedFloat &A,int nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f - nB));
}

inline CFixedFloat operator-(int nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB - A.m_f));
}

inline CFixedFloat operator-(const CFixedFloat &A,double nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f - nB));
}

inline CFixedFloat operator-(double nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB - A.m_f));
}

inline CFixedFloat operator^(const CFixedFloat &A,long nB)
{
	using std::pow;
	Precision p(A.m_f.precision());
	return CFixedFloat(FixedFloat(pow(A.m_f, nB)));
}

inline CFixedFloat sqr(const CFixedFloat &A)
{
	return A.Square();
}

inline CFixedFloat exp(const CFixedFloat &A)
{
	using std::exp;
	Precision p(A.m_f.precision());
	return CFixedFloat(FixedFloat(exp(A.m_f)));
}

inline CFixedFloat expm1(const CFixedFloat &A)
{
	using std::expm1;
	Precision p(A.m_f.precision());
	return CFixedFloat(FixedFloat(expm1(A.m_f)));
}

inline CFixedFloat log(const CFixedFloat &A)
{
	using std::log;
	Precision p(A.m_f.precision());
	return CFixedFloat(FixedFloat(log(A.m_f)));
}

inline CFixedFloat sin(const CFixedFloat &A)
{
	using std::sin;
	Precision p(A.m_f.precision());
	return CFixedFloat(FixedFloat(sin(A.m_f)));
}

inline CFixedFloat cos(const CFixedFloat &A)
{
	using std::cos;
	Precision p(A.m_f.precision());
	return CFixedFloat(FixedFloat(cos(A.m_f)));
}

inline CFixedFloat sinh(const CFixedFloat &A)
{
	using std::sin;
	Precision p(A.m_f.precision());
	return CFixedFloat(FixedFloat(sinh(A.m_f)));
}

inline CFixedFloat cosh(const CFixedFloat &A)
{
	using std::cosh;
	Precision p(A.m_f.precision());
	return CFixedFloat(FixedFloat(cosh(A.m_f)));
}

inline bool isnan(const CFixedFloat &A)
{
	return isnan(A.m_f);
}

inline bool isinf(const CFixedFloat &A)
{
	return isinf(A.m_f);
}

inline std::ostream& operator<<(std::ostream& a, const CFixedFloat& b) noexcept
{
	return a << b.m_f;
}

#define I(T) inline T infnan_to_zero(const T &a) { return (isinf(a) || isnan(a)) ? 0 : a; }
I(CFixedFloat)
#undef I

#endif
