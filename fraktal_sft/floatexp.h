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

#ifndef KF_FLOATEXP_H
#define KF_FLOATEXP_H

#include <math.h>
#include <stdint.h>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <limits>
#include "CFixedFloat.h"

#define MAX_PREC 1020
// this has two fewer 0 than you might expect, this is to give headroom for
// avoiding overflow in + and other functions. it is the exponent for 0.0
#define EXP_MIN (-0x80000000000000LL)

class floatexp
{
public:
	double val;
	int64_t exp;
	inline void align()
	{
		if (val != 0)
		{
			union { double d; int64_t i; } u;
			u.d = val;
			exp += ((u.i & 0x7FF0000000000000LL) >> 52) - 1023;
			u.i = (u.i & 0x800FFFFFFFFFFFFFLL) | 0x3FF0000000000000LL;
			val = u.d;
		}
		else
		{
			val = 0;
			exp = EXP_MIN;
		}
	}
	inline floatexp &abs()
	{
		if(val<0)
			val=-val;
		return *this;
	}
	inline void initFromDouble(double a)
	{
		val=a;
		exp=0;
		align();
	}
	inline void initFromLongDouble(long double a)
	{
		using std::frexp;
		int e = 0;
		a = frexp(a, &e);
		val=a;
		exp=e;
		align();
	}
	inline double setExp(double newval,int64_t newexp) const
	{
//		int64_t tmpval = (*((int64_t*)&newval) & 0x800FFFFFFFFFFFFF) | ((newexp+1023)<<52);
//		memcpy(&newval,&tmpval,sizeof(double));
//		return newval;
		union { double d; int64_t i; } u;
		u.d = newval;
		u.i = (u.i & 0x800FFFFFFFFFFFFFLL) | ((newexp + 1023) << 52);
		newval = u.d;
		return newval;
	}
	inline floatexp()
	{
		val = 0;
		exp = EXP_MIN;
	}
	inline floatexp(int a)
	{
		initFromDouble(a);
	}
	inline floatexp(double a)
	{
		initFromDouble(a);
	}
	inline floatexp(double a, int64_t e)
	{
		val = a;
		exp = e;
		align();
	}
	inline floatexp(double a, int64_t e, int dummy)
	{
		(void) dummy;
		val = a;
		exp = e;
	}
	inline floatexp(long double a)
	{
		initFromLongDouble(a);
	}
	inline floatexp &operator =(const floatexp &a)
	{
		val=a.val;
		exp=a.exp;
		return *this;
	}
	inline floatexp &operator =(int a)
	{	
		initFromDouble((double)a);
		return *this;
	}
	inline floatexp &operator =(double a)
	{
		initFromDouble(a);
		return *this;
	}
	inline floatexp &operator =(long double a)
	{
		initFromLongDouble(a);
		return *this;
	}
	inline floatexp operator *(const floatexp &a) const
	{
		floatexp r;
		r.val = a.val*val;
		r.exp = a.exp+exp;
		r.align();
		return r;
	}
	inline floatexp operator /(const floatexp &a) const
	{
		floatexp r;
		r.val = val/a.val;
		r.exp = exp - a.exp;
		r.align();
		return r;
	}
	__attribute__ ((warn_unused_result))
	inline floatexp mul2() const
	{
		floatexp r;
		r.val = val;
		r.exp = exp + 1;
		return r;
	}
	__attribute__ ((warn_unused_result))
	inline floatexp mul4() const
	{
		floatexp r;
		r.val = val;
		r.exp = exp + 2;
		return r;
	}
	inline floatexp operator +(const floatexp &a) const
	{
		floatexp r;
		int64_t diff;
		if(exp>a.exp){
			diff = exp-a.exp;
			r.exp = exp;
			if(diff>MAX_PREC)
				r.val=val;
			else{
				double aval = setExp(a.val,-diff);
				r.val = val+aval;
			}
		}
		else{
			diff = a.exp-exp;
			r.exp = a.exp;
			if(diff>MAX_PREC)
				r.val=a.val;
			else{
				double aval = setExp(val,-diff);
				r.val = a.val+aval;
			}
		}
		r.align();
		return r;
	}
	inline floatexp operator -() const
	{
		floatexp r=*this;
		r.val=-r.val;
		return r;
	}
	inline floatexp &operator +=(const floatexp &a)
	{
		*this = *this+a;
		return *this;
	}
	inline floatexp operator -(const floatexp &a) const
	{
		floatexp r;
		int64_t diff;
		if(exp>a.exp){
			diff = exp-a.exp;
			r.exp = exp;
			if(diff>MAX_PREC)
				r.val = val;
			else{
				double aval = setExp(a.val,-diff);
				r.val = val-aval;
			}
		}
		else{
			diff = a.exp-exp;
			r.exp = a.exp;
			if(diff>MAX_PREC)
				r.val=-a.val;
			else{
				double aval = setExp(val,-diff);
				r.val = aval-a.val;
			}
		}
		r.align();
		return r;
	}
	inline floatexp &operator -=(const floatexp &a)
	{
		*this = *this-a;
		return *this;
	}
	inline bool operator >(const floatexp &a) const
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
	inline bool operator <(const floatexp &a) const
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
	inline bool operator <=(const floatexp &a) const
	{
		return (*this<a || *this==a);
	}
	inline bool operator >=(const floatexp &a) const
	{
		return (*this>a || *this==a);
	}
	inline bool operator <=(const int a) const
	{
		return (*this<a || *this==a);
	}
	inline bool operator ==(const floatexp &a) const
	{
		if(exp!=a.exp)
			return false;
		return val==a.val;
	}
	inline bool iszero() const
	{
		return (val==0 && exp==0);
	}
	inline double todouble() const
	{
		if(exp<-MAX_PREC || exp>MAX_PREC)
			return 0;
		return setExp(val,exp);
	}
	inline explicit operator double () const
	{
		return todouble();
	}
	inline double todouble(int nScaling) const
	{
		if(!nScaling)
			return todouble();
		floatexp ret = *this;
		while(nScaling>9){
			ret.val*=1e10;
			ret.align();
			nScaling-=10;
		}
		while(nScaling>2){
			ret.val*=1e3;
			ret.align();
			nScaling-=3;
		}
		while(nScaling>0){
			ret.val*=1e1;
			ret.align();
			nScaling--;
		}
		while(nScaling<-9){
			ret.val/=1e10;
			ret.align();
			nScaling+=10;
		}
		while(nScaling<-2){
			ret.val/=1e3;
			ret.align();
			nScaling+=3;
		}
		while(nScaling<0){
			ret.val/=1e1;
			ret.align();
			nScaling++;
		}
		if(ret.exp<-MAX_PREC || ret.exp>MAX_PREC)
			return 0;
		return setExp(ret.val,ret.exp);
	}
	inline floatexp &operator /=(double a)
	{
		val/=a;
		align();
		return *this;
	}
	inline floatexp &operator *=(double a)
	{
		val*=a;
		align();
		return *this;
	}
	inline floatexp &operator *=(floatexp a)
	{
		return *this = *this * a;
	}
	inline floatexp &operator *=(long double a)
	{
		return *this *= floatexp(a);
	}

	inline floatexp &operator =(const CFixedFloat &a)
	{
		signed long int e = 0;
		val = mpfr_get_d_2exp(&e, a.m_f.backend().data(), MPFR_RNDN);
		exp = e;
		align();
		return *this;
	}
	inline void ToFixedFloat(CFixedFloat &a) const
	{
		a = val;
		if (exp >= 0)
			mpfr_mul_2ui(a.m_f.backend().data(), a.m_f.backend().data(), exp, MPFR_RNDN);
		else
			mpfr_div_2ui(a.m_f.backend().data(), a.m_f.backend().data(), -exp, MPFR_RNDN);
	}

	inline floatexp setLongDouble(long double a)
	{
		int e = 0;
		val = std::frexp(a, &e);
		exp = e;
		align();
		return *this;
	}
	inline long double toLongDouble() const
	{
		if (val == 0.0L)
			return 0.0L;
		if (exp >= INT_MAX)
			return (val / 0.0L); // infinity
		if (exp <= INT_MIN)
			return (val * 0.0L); // zero
		return std::ldexp((long double) val, exp);
	}
	inline long double toLongDouble(int nScaling) const
	{
		if(!nScaling)
			return toLongDouble();
		floatexp ret = *this;
		// FIXME risky to go higher than this? 1e300 might be ok?
		while(nScaling>99){
			ret.val*=1e100;
			ret.align();
			nScaling-=100;
		}
		while(nScaling>29){
			ret.val*=1e30;
			ret.align();
			nScaling-=30;
		}
		while(nScaling>9){
			ret.val*=1e10;
			ret.align();
			nScaling-=10;
		}
		while(nScaling>2){
			ret.val*=1e3;
			ret.align();
			nScaling-=3;
		}
		while(nScaling){
			ret.val*=1e1;
			ret.align();
			nScaling--;
		}
		return ret.toLongDouble();
	}
	inline explicit operator long double () const
	{
		return toLongDouble();
	}

  inline std::string toString() const
  {
		/*
		  f = val 2^exp
		  log10 f = log10 val + exp log10 2
		  e10 = floor(log10 f)
		  d10 = 10^(log10 f - e10)
		  d10 \in [1, 10)
		*/
		double lf = std::log10(std::abs(val)) + exp * std::log10(2.0);
		int64_t e10 = std::floor(lf);
		double d10 = std::pow(10, lf - e10) * ((val > 0) - (val < 0));
		if (val == 0) { d10 = 0; e10 = 0; }
		std::ostringstream os; os
		  << std::setprecision(std::numeric_limits<double>::digits10 + 1)
		  << std::fixed
		  << d10 << 'E' << e10;
		return os.str();
	}
};

inline std::ostream& operator<<(std::ostream& a, const floatexp& b)
{
	return a << b.toString();
}

inline floatexp operator*(double a, floatexp b)
{
	return floatexp(a) * b;
}

inline floatexp operator*(floatexp b, double a)
{
	return floatexp(a) * b;
}

inline floatexp operator*(long double a, floatexp b)
{
	return floatexp(a) * b;
}

inline floatexp operator*(floatexp b, long double a)
{
	return floatexp(a) * b;
}

inline floatexp operator+(double a, floatexp b)
{
	return floatexp(a) + b;
}

inline floatexp operator+(floatexp b, double a)
{
	return floatexp(a) + b;
}

inline floatexp operator*(int a, floatexp b)
{
	return double(a) * b;
}

inline floatexp operator*(floatexp b, int a)
{
	return double(a) * b;
}

inline floatexp abs(floatexp a)
{
	return a.abs();
}

inline floatexp sqrt(floatexp a)
{
  return floatexp
    ( std::sqrt((a.exp & 1) ? 2.0 * a.val : a.val)
    , (a.exp & 1) ? (a.exp - 1) / 2 : a.exp / 2
    );
}

inline floatexp log(floatexp a)
{
	return floatexp(std::log(a.val) + std::log(2.0) * a.exp);
}

inline floatexp mpfr_get_fe(const mpfr_t value)
{
	signed long int e = 0;
	double l = mpfr_get_d_2exp(&e, value, MPFR_RNDN);
	return floatexp(l, e);
}

inline void mpfr_set_fe(mpfr_t value, floatexp fe)
{
	mpfr_set_d(value, fe.val, MPFR_RNDN);
	if (fe.exp >= 0)
	{
		mpfr_mul_2ui(value, value, fe.exp, MPFR_RNDN);
	}
	else
	{
		mpfr_div_2ui(value, value, -fe.exp, MPFR_RNDN);
	}
}

inline long double mpfr_get_ld(const mpfr_t value)
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

#endif
