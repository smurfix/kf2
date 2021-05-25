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

#ifndef KF_FLOATEXP_H
#define KF_FLOATEXP_H

#include <cmath>
#include <cstdint>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <limits>
#include <algorithm>
#include "CFixedFloat.h"

// #define KF_FLOATEXP_ACCURATE 1

template <typename mantissa, typename exponent>
struct tfloatexp
{
public:
	tfloatexp() = default; // POD
	tfloatexp(const tfloatexp &a) = default; // POD
	tfloatexp(tfloatexp &&a) = default; // POD
	tfloatexp &operator=(const tfloatexp &a) = default; // POD
	tfloatexp &operator=(tfloatexp &&a) = default; // POD
	~tfloatexp() = default; // POD;

	mantissa val;
	exponent exp;

	static constexpr exponent EXPONENT_MASK   = sizeof(mantissa) == 8 ? 0x7FF0000000000000LL : 0x7F800000;
	static constexpr exponent EXPONENT_UNMASK = sizeof(mantissa) == 8 ? 0x800FFFFFFFFFFFFFLL : 0x803FFFFF;
	static constexpr exponent EXPONENT_SET    = sizeof(mantissa) == 8 ? 0x3FF0000000000000LL : 0x3F800000;
	static constexpr int      EXPONENT_SHIFT  = sizeof(mantissa) == 8 ?                   52 :         23;
	static constexpr int      EXPONENT_BIAS   = sizeof(mantissa) == 8 ?                 1023 :        127;

	static constexpr exponent MAX_PREC = sizeof(mantissa) == 8 ? 53 : 24;
	// MIN_EXPONENT is smaller than you might expect, this is to give headroom for
	// avoiding overflow in + and other functions. it is the exponent for 0.0
	static constexpr exponent EXP_MIN = sizeof(exponent) == 8 ? exponent(-0x800000000000000LL) : exponent(-0x8000000);
	static constexpr exponent EXP_MAX = -EXP_MIN;

	inline void align() noexcept
	{
		exponent val_i;
		__builtin_memcpy(&val_i, &val, sizeof(val_i));
		const exponent e = val_i & EXPONENT_MASK;
		if (__builtin_expect(e == 0, 0)) // zero, denormalized
		{
			val = 0; // no denormals
			exp = EXP_MIN;
		}
#ifdef KF_FLOATEXP_ACCURATE
		else
		if (__builtin_expect(e == EXPONENT_MASK, 0)) // inf, nan
		{
			val = 0; // no inf, nan
			exp = EXP_MIN;
		}
#endif
		else
		{
			exp += (e >> EXPONENT_SHIFT) - EXPONENT_BIAS;
#ifdef KF_FLOATEXP_ACCURATE
			if (__builtin_expect(exp < EXP_MIN, 0))
			{
				// underflow
				val = 0;
				exp = EXP_MIN;
			}
			else
			if (__builtin_expect(EXP_MAX < exp, 0))
			{
				// overflow
				val = 0; // no inf
				exp = EXP_MIN;
			}
			else
#endif
			{
				val_i = (val_i & EXPONENT_UNMASK) | EXPONENT_SET;
				__builtin_memcpy(&val, &val_i, sizeof(val));
			}
		}
	}

	static inline mantissa setExp(mantissa val, exponent exp) noexcept
	{
		exponent val_i;
		__builtin_memcpy(&val_i, &val, sizeof(val_i));
		val_i = (val_i & EXPONENT_UNMASK) | ((exp + EXPONENT_BIAS) << EXPONENT_SHIFT);
		__builtin_memcpy(&val, &val_i, sizeof(val));
		return val;
	}

	tfloatexp(int32_t a) noexcept
	{
		val = a;
		exp = 0;
		align();
	}
	tfloatexp(int64_t a) noexcept
	{
		val = a;
		exp = 0;
		align();
	}
	tfloatexp(float a) noexcept
	{
		using std::frexp;
		int e = 0;
		a = frexp(a, &e);
		val = a;
		exp = e;
		align();
	}
	tfloatexp(double a) noexcept
	{
		using std::frexp;
		int e = 0;
		a = frexp(a, &e);
		val = a;
		exp = e;
		align();
	}
	tfloatexp(long double a) noexcept
	{
		using std::frexp;
		int e = 0;
		a = frexp(a, &e);
		val = a;
		exp = e;
		align();
	}
	inline tfloatexp(mantissa a, exponent e) noexcept
	{
		val = a;
		exp = e;
		align();
	}
	inline tfloatexp(mantissa a, exponent e, int dummy) noexcept
	{
		val = a;
		exp = e;
		(void) dummy;
	}
	template <typename mantissa2, typename exponent2>
	explicit inline tfloatexp(const tfloatexp<mantissa2, exponent2> &a) noexcept
	: tfloatexp(mantissa(a.val), exponent(std::min(std::max(a.exp, exponent2(EXP_MIN)), exponent2(EXP_MAX))))
	{
	}

	inline tfloatexp &operator =(int32_t a) noexcept
	{
		*this = tfloatexp(a);
		return *this;
	}
	inline tfloatexp &operator =(int64_t a) noexcept
	{
		*this = tfloatexp(a);
		return *this;
	}
	inline tfloatexp &operator =(float a) noexcept
	{
		*this = tfloatexp(a);
		return *this;
	}
	inline tfloatexp &operator =(double a) noexcept
	{
		*this = tfloatexp(a);
		return *this;
	}
	inline tfloatexp &operator =(long double a) noexcept
	{
		*this = tfloatexp(a);
		return *this;
	}

	inline tfloatexp &operator *=(const tfloatexp a) noexcept
	{
		val *= a.val;
		exp += a.exp; // saturated in align below
		align();
		return *this;
	}
	inline tfloatexp operator *(const tfloatexp a) const noexcept
	{
		return tfloatexp(*this) *= a;
	}

	inline tfloatexp &operator /=(const tfloatexp a) noexcept
	{
		val /= a.val;
		exp -= a.exp; // saturated in align below
		align();
		return *this;
	}
	inline tfloatexp operator /(const tfloatexp a) const noexcept
	{
		return tfloatexp(*this) /= a;
	}

	__attribute__ ((warn_unused_result))
	inline tfloatexp mul2() const noexcept
	{
		tfloatexp r;
		r.val = val;
		r.exp = exp + 1; // FIXME saturate
		return r;
	}

	inline tfloatexp operator +(const tfloatexp a) const noexcept
	{
		tfloatexp r;
		exponent diff;
		if(exp>a.exp){
			diff = exp-a.exp;
			r.exp = exp;
			if(diff>MAX_PREC)
				r.val=val;
			else{
				mantissa aval = setExp(a.val,-diff);
				r.val = val+aval;
			}
		}
		else{
			diff = a.exp-exp;
			r.exp = a.exp;
			if(diff>MAX_PREC)
				r.val=a.val;
			else{
				mantissa aval = setExp(val,-diff);
				r.val = a.val+aval;
			}
		}
		r.align();
		return r;
	}

	inline tfloatexp operator -() const noexcept
	{
		tfloatexp r=*this;
		r.val=-r.val;
		return r;
	}
	inline tfloatexp &operator +=(const tfloatexp &a) noexcept
	{
		*this = *this+a;
		return *this;
	}
	inline tfloatexp operator -(const tfloatexp &a) const noexcept
	{
		tfloatexp r;
		exponent diff;
		if(exp>a.exp){
			diff = exp-a.exp;
			r.exp = exp;
			if(diff>MAX_PREC)
				r.val = val;
			else{
				mantissa aval = setExp(a.val,-diff);
				r.val = val-aval;
			}
		}
		else{
			diff = a.exp-exp;
			r.exp = a.exp;
			if(diff>MAX_PREC)
				r.val=-a.val;
			else{
				mantissa aval = setExp(val,-diff);
				r.val = aval-a.val;
			}
		}
		r.align();
		return r;
	}
	inline tfloatexp &operator -=(const tfloatexp &a) noexcept
	{
		*this = *this-a;
		return *this;
	}
	inline bool operator >(const tfloatexp &a) const noexcept
	{
		if(val>0){
			if(a.val<0)
				return true;
			if(exp>a.exp)
				return true;
			else if(exp<a.exp)
				return false;
			return val>a.val;
		}
		else{
			if(a.val>0)
				return false;
			if(exp>a.exp)
				return false;
			else if(exp<a.exp)
				return true;
			return val>a.val;
		}
	}
	inline bool operator <(const tfloatexp &a) const noexcept
	{
		if(val>0){
			if(a.val<0)
				return false;
			if(exp>a.exp)
				return false;
			else if(exp<a.exp)
				return true;
			return val<a.val;
		}
		else{
			if(a.val>0)
				return true;
			if(exp>a.exp)
				return true;
			else if(exp<a.exp)
				return false;
			return val<a.val;
		}
	}
	inline bool operator <=(const tfloatexp &a) const noexcept
	{
		return (*this<a || *this==a);
	}
	inline bool operator >=(const tfloatexp &a) const noexcept
	{
		return (*this>a || *this==a);
	}
	inline bool operator <=(const int a) const noexcept
	{
		return (*this<a || *this==a);
	}
	inline bool operator ==(const tfloatexp &a) const noexcept
	{
		if (val != 0 && ! std::isinf(val) && exp != a.exp)
			return false;
		return val==a.val;
	}
	inline explicit operator float () const noexcept
	{
		if (exp > exponent(INT_MAX))
		{
			return float(val) / 0.0; // overflow
		}
		if (exp < exponent(INT_MIN))
		{
			return float(val) * 0.0; // underflow
		}
		return std::ldexp(float(val), int(exp));
	}
	inline explicit operator double () const noexcept
	{
		if (exp > exponent(INT_MAX))
		{
			return double(val) / 0.0; // overflow
		}
		if (exp < exponent(INT_MIN))
		{
			return double(val) * 0.0; // underflow
		}
		return std::ldexp(double(val), int(exp));
	}
	inline explicit operator long double () const noexcept
	{
		if (exp > exponent(INT_MAX))
		{
			return (long double)(val) / 0.0; // overflow
		}
		if (exp < exponent(INT_MIN))
		{
			return (long double)(val) * 0.0; // underflow
		}
		return std::ldexp((long double)(val), int(exp));
	}

	inline tfloatexp &operator =(const CFixedFloat &a) noexcept
	{
		signed long int e = 0;
		double v = mpfr_get_d_2exp(&e, a.m_f.backend().data(), MPFR_RNDN);
		*this = tfloatexp(v, e);
		return *this;
	}
	inline tfloatexp(const CFixedFloat &a) noexcept
	{
		*this = a;
	}
	inline tfloatexp(const CDecNumber &a) noexcept
	{
		signed long int e = 0;
		double v = mpfr_get_d_2exp(&e, a.m_dec.backend().data(), MPFR_RNDN);
		*this = tfloatexp(v, e);
	}
	inline void ToFixedFloat(CFixedFloat &a) const noexcept
	{
		a = val;
		if (exp > exponent(INT_MAX))
		{
			a = 1.0 / 0.0;
		}
		else if (exp < exponent(INT_MIN))
		{
			a = 0.0;
		}
		else
		{
			a = val;
			if (exp >= 0)
				mpfr_mul_2ui(a.m_f.backend().data(), a.m_f.backend().data(), exp, MPFR_RNDN);
			else
				mpfr_div_2ui(a.m_f.backend().data(), a.m_f.backend().data(), -exp, MPFR_RNDN);
		}
	}
	inline explicit operator CFixedFloat() const noexcept
	{
		CFixedFloat a;
		ToFixedFloat(a);
		return a;
	}

  inline std::string toString(int digits = 0) const noexcept
  {
		/*
		  f = val 2^exp
		  log10 f = log10 val + exp log10 2
		  e10 = floor(log10 f)
		  d10 = 10^(log10 f - e10)
		  d10 \in [1, 10)
		*/
		if (std::isnan(val)) return "nan";
		if (std::isinf(val)) return val > 0 ? "+inf" : "-inf";
		mantissa lf = std::log10(std::abs(val)) + exp * std::log10(2.0);
		exponent e10 = exponent(std::floor(lf));
		mantissa d10 = std::pow(10, lf - e10) * ((val > 0) - (val < 0));
		if (val == 0) { d10 = 0; e10 = 0; }
		std::ostringstream os; os
		  << std::setprecision(digits ? digits : (std::numeric_limits<mantissa>::digits10 + 1))
		  << std::fixed
		  << d10 << 'E' << e10;
		return os.str();
	}
} __attribute__((packed));

template <typename mantissa, typename exponent>
inline std::ostream& operator<<(std::ostream& a, const tfloatexp<mantissa, exponent>& b) noexcept
{
	return a << b.toString();
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> operator*(mantissa a, const tfloatexp<mantissa, exponent> &b) noexcept
{
	return tfloatexp<mantissa, exponent>(a) * b;
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> operator*(const tfloatexp<mantissa, exponent> &b, mantissa a) noexcept
{
	return b * tfloatexp<mantissa, exponent>(a);
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> operator*(int a, const tfloatexp<mantissa, exponent> &b) noexcept
{
	return tfloatexp<mantissa, exponent>(a) * b;
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> operator*(const tfloatexp<mantissa, exponent> &b, int a) noexcept
{
	return b * tfloatexp<mantissa, exponent>(a);
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> operator+(double a, const tfloatexp<mantissa, exponent> &b) noexcept
{
	return tfloatexp<mantissa, exponent>(a) + b;
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> operator+(const tfloatexp<mantissa, exponent> &b, double a) noexcept
{
	return tfloatexp<mantissa, exponent>(a) + b;
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> operator-(mantissa a, tfloatexp<mantissa, exponent> b) noexcept
{
	return tfloatexp<mantissa, exponent>(a) - b;
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> abs(tfloatexp<mantissa, exponent> a) noexcept
{
	return a.val < 0 ? -a : a;
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> sqrt(tfloatexp<mantissa, exponent> a) noexcept
{
  return tfloatexp<mantissa, exponent>
    ( std::sqrt((a.exp & 1) ? 2 * a.val : a.val)
    , (a.exp & 1) ? (a.exp - 1) / 2 : a.exp / 2
    );
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> log(tfloatexp<mantissa, exponent> a) noexcept
{
	return tfloatexp<mantissa, exponent>(std::log(a.val) + std::log(mantissa(2)) * a.exp);
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> log2(tfloatexp<mantissa, exponent> a) noexcept
{
	return tfloatexp<mantissa, exponent>(std::log2(a.val) + a.exp);
}

inline double sqr(double a) noexcept
{
	return a * a;
}

inline long double sqr(long double a) noexcept
{
	return a * a;
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> sqr(tfloatexp<mantissa, exponent> a) noexcept
{
	return a * a;
}

template <typename T> T pow(T x, uint64_t n) noexcept
{
	switch (n)
	{
		case 0: return T(1);
		case 1: return x;
		case 2: return sqr(x);
		case 3: return x * sqr(x);
		case 4: return sqr(sqr(x));
		case 5: return x * sqr(sqr(x));
		case 6: return sqr(x * sqr(x));
		case 7: return x * sqr(x * sqr(x));
		case 8: return sqr(sqr(sqr(x)));
		default:
		{
		  T y(1);
		  while (n > 1)
		  {
		    if (n & 1)
		      y *= x;
		    x = sqr(x);
		    n >>= 1;
			}
		  return x * y;
		}
	}
}

template <typename mantissa, typename exponent>
inline bool isnan(const tfloatexp<mantissa, exponent> &a) noexcept
{
	return isnan(a.val);
}

template <typename mantissa, typename exponent>
inline bool isinf(const tfloatexp<mantissa, exponent> &a) noexcept
{
	return isinf(a.val);
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> infnan_to_zero(const tfloatexp<mantissa, exponent> &a) noexcept
{
	return isinf(a.val) ? tfloatexp<mantissa, exponent>(copysign(1e30, a.val)) : isnan(a.val) ? tfloatexp<mantissa, exponent>(0) : a;
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> exp(tfloatexp<mantissa, exponent> a) noexcept;
template <>
inline tfloatexp<float, int32_t> exp(tfloatexp<float, int32_t> a) noexcept // FIXME magic numbers
{
	using std::exp;
	using std::ldexp;
	using std::pow;
  if (-53 <= a.exp && a.exp <= 8) return tfloatexp<float, int32_t>(exp(ldexp(a.val, a.exp)));
  if (61 <= a.exp) return tfloatexp<float, int32_t>(a.val > 0.0f ? a.val / 0.0f : 0.0f);
  if (a.exp < -53) return tfloatexp<float, int32_t>(1.0f);
  return pow(tfloatexp<float, int32_t>(exp(a.val)), 1ULL << a.exp);
}
template <>
inline tfloatexp<double, int64_t> exp(tfloatexp<double, int64_t> a) noexcept
{
	using std::exp;
	using std::ldexp;
	using std::pow;
  if (-53 <= a.exp && a.exp <= 8) return tfloatexp<double, int64_t>(exp(ldexp(a.val, a.exp)));
  if (61 <= a.exp) return tfloatexp<double, int64_t>(a.val > 0.0 ? a.val / 0.0 : 0.0);
  if (a.exp < -53) return tfloatexp<double, int64_t>(1.0);
  return pow(tfloatexp<double, int64_t>(exp(a.val)), 1ULL << a.exp);
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> expm1(tfloatexp<mantissa, exponent> a) noexcept
{
	using std::expm1;
	using std::ldexp;
  if (a.exp <= -120) return a; // FIXME threshold depends on number type
  if (8 <= a.exp) return exp(a) - 1;
  return tfloatexp<mantissa, exponent>(expm1(ldexp(a.val, a.exp)));
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> sin(tfloatexp<mantissa, exponent> a) noexcept
{
	using std::sin;
  if (a.exp <= -120) return a; // FIXME threshold depends on number type
	return tfloatexp<mantissa, exponent>(sin(ldexp(a.val, a.exp)));
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> cos(tfloatexp<mantissa, exponent> a) noexcept
{
	using std::cos;
  if (a.exp <= -120) return 1; // FIXME threshold depends on number type
	return tfloatexp<mantissa, exponent>(cos(ldexp(a.val, a.exp)));
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> diffabs(const tfloatexp<mantissa, exponent> &c, const tfloatexp<mantissa, exponent> &d) noexcept
{
  const tfloatexp<mantissa, exponent> cd = c + d;
  const tfloatexp<mantissa, exponent> c2d = c.mul2() + d;
  return c.val >= 0.0 ? cd.val >= 0.0 ? d : -c2d : cd.val > 0.0 ? c2d : -d;
}

template <typename mantissa, typename exponent>
inline tfloatexp<mantissa, exponent> mpfr_get_tfe(const mpfr_t value) noexcept
{
	signed long int e = 0;
	double l = mpfr_get_d_2exp(&e, value, MPFR_RNDN);
	return tfloatexp<mantissa, exponent>(l, e);
}

template <typename mantissa, typename exponent>
inline void mpfr_set_fe(mpfr_t value, tfloatexp<mantissa, exponent> fe) noexcept
{
	mpfr_set_ld(value, fe.val, MPFR_RNDN);
	if (fe.exp >= 0)
	{
		mpfr_mul_2ui(value, value, fe.exp, MPFR_RNDN);
	}
	else
	{
		mpfr_div_2ui(value, value, -fe.exp, MPFR_RNDN);
	}
}

inline long double mpfr_get_ld(const mpfr_t value) noexcept
{
	using std::ldexp;
	signed long int e = 0;
	long double l = mpfr_get_ld_2exp(&e, value, MPFR_RNDN);
	if (l == 0.0L)
		return 0.0L;
	if (e >= INT_MAX)
		return l / 0.0L;
	if (e <= INT_MIN)
		return l * 0.0L;
	l = ldexp(l, e);
	return l;
}

using floatexp = tfloatexp<double, int64_t>;
inline floatexp mpfr_get_fe(const mpfr_t value) { return mpfr_get_tfe<double, int64_t>(value); }

using floatexpf = tfloatexp<float, int32_t>;
inline floatexpf mpfr_get_fef(const mpfr_t value) { return mpfr_get_tfe<float, int32_t>(value); }

#endif
