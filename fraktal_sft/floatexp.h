#include <windows.h>
#include <math.h>
#ifndef _GCC_
#include "CFixedFloat.h"
#endif
#ifndef __FLOATEXP_H__
#define __FLOATEXP_H__
#define MAX_PREC 1020

#define _ALIGN_(val,exp) exp += ((*((__int64*)&val) & 0x7FF0000000000000LL)>>52) - 1023; *((__int64*)&val) = (*((__int64*)&val) & 0x800FFFFFFFFFFFFFLL) | 0x3FF0000000000000LL;
class floatexp
{
public:
	double val;
	__int64 exp;
	__inline floatexp &abs()
	{
		if(val<0)
			val=-val;
		return *this;
	}
	inline void initFromDouble(double a)
	{
		val=a;
		exp=0;
		_ALIGN_(val,exp)
	}
	inline void align()
	{
		exp += ((*((__int64*)&val) & 0x7FF0000000000000LL)>>52) - 1023;
//		__int64 tmpval = (*((__int64*)&val) & 0x800FFFFFFFFFFFFF) | 0x3FF0000000000000;
//		memcpy(&val,&tmpval,sizeof(double));
		*((__int64*)&val) = (*((__int64*)&val) & 0x800FFFFFFFFFFFFFLL) | 0x3FF0000000000000LL;
	}
	inline double setExp(double newval,__int64 newexp) const
	{
//		__int64 tmpval = (*((__int64*)&newval) & 0x800FFFFFFFFFFFFF) | ((newexp+1023)<<52);
//		memcpy(&newval,&tmpval,sizeof(double));
//		return newval;
		*((__int64*)&newval) = (*((__int64*)&newval) & 0x800FFFFFFFFFFFFFLL) | ((newexp+1023)<<52);
		return newval;
	}
	inline floatexp()
	{
		val = 0;
		exp = 0;
	}
	inline floatexp(double a)
	{
		initFromDouble(a);
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
	inline floatexp operator *(const floatexp &a) const
	{
		floatexp r;
		r.val = a.val*val;
		r.exp = a.exp+exp;
		_ALIGN_(r.val,r.exp)
		return r;
	}
	inline floatexp operator /(const floatexp &a) const
	{
		floatexp r;
		r.val = val/a.val;
		r.exp = exp - a.exp;
		_ALIGN_(r.val,r.exp)
		return r;
	}
	inline floatexp &mul2()
	{
		exp++;
		return *this;
	}
	inline floatexp &mul4()
	{
		exp+=2;
		return *this;
	}
	inline floatexp operator +(const floatexp &a) const
	{
		floatexp r;
		__int64 diff;
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
		_ALIGN_(r.val,r.exp)
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
		__int64 diff;
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
		_ALIGN_(r.val,r.exp)
		return r;
	}
	inline floatexp &operator -=(const floatexp &a)
	{
		*this = *this-a;
		return *this;
	}
	inline BOOL operator >(const floatexp &a) const
	{
		if(val>0){
			if(a.val<0)
				return TRUE;
			if(exp>a.exp)
				return TRUE;
			else if(exp<a.exp)
				return FALSE;
			return val>a.val;
		}
		else{
			if(a.val>0)
				return FALSE;
			if(exp>a.exp)
				return FALSE;
			else if(exp<a.exp)
				return TRUE;
			return val>a.val;
		}
	}
	inline BOOL operator <(const floatexp &a) const
	{
		if(val>0){
			if(a.val<0)
				return FALSE;
			if(exp>a.exp)
				return FALSE;
			else if(exp<a.exp)
				return TRUE;
			return val<a.val;
		}
		else{
			if(a.val>0)
				return TRUE;
			if(exp>a.exp)
				return TRUE;
			else if(exp<a.exp)
				return FALSE;
			return val<a.val;
		}
	}
	inline BOOL operator <=(const int a) const
	{
		floatexp aa(a);
		return (*this<a || *this==a);
	}
	inline BOOL operator ==(const floatexp &a) const
	{
		if(exp!=a.exp)
			return FALSE;
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
	inline double todouble(int nScaling) const
	{
		if(!nScaling)
			return todouble();
		floatexp ret = *this;
		while(nScaling>9){
			ret.val*=1e10;
			_ALIGN_(ret.val,ret.exp)
			nScaling-=10;
		}
		while(nScaling>2){
			ret.val*=1e3;
			_ALIGN_(ret.val,ret.exp)
			nScaling-=3;
		}
		while(nScaling){
			ret.val*=1e1;
			_ALIGN_(ret.val,ret.exp)
			nScaling--;
		}
		if(ret.exp<-MAX_PREC || ret.exp>MAX_PREC)
			return 0;
		return setExp(ret.val,ret.exp);
	}
	inline floatexp &operator /=(double a)
	{
		val/=a;
		_ALIGN_(val,exp)
		return *this;
	}
	inline floatexp &operator *=(double a)
	{
		val*=a;
		_ALIGN_(val,exp)
		return *this;
	}
#ifndef _GCC_
	inline floatexp &operator =(const CFixedFloat &a)
	{
		signed long int e = 0;
		val = mpf_get_d_2exp(&e, a.m_f.backend().data());
		if ((mpf_sgn(a.m_f.backend().data()) >= 0) != (val >= 0))
			val = -val;
		exp = e;
		_ALIGN_(val, exp);
		if ((a > 0) != (val > 0))
			val = -val;
		return *this;
	}
	inline void ToFixedFloat(CFixedFloat &a) const
	{
		a = val;
		mpf_mul_2exp(a.m_f.backend().data(), a.m_f.backend().data(), exp);
	}
#else
	inline floatexp setLongDouble(long double a)
	{
		__int64 val[2]={0};
		memcpy(val,(void*)&a,sizeof(val));
		exp = (val[1]&0x7FFF)-16383;
		val[1] = (val[1]&0x8000) + 16383;
		if(val[0] && val[1]){
			memcpy((void*)&a,val,sizeof(val));
			this->val = (double)a;
		}
		else{
			this->val=1;
			exp=-16383;
		}
		return *this;
	}
	inline long double toLongDouble() const
	{
		long double ret = val;
		__int64 *v = (__int64*)&ret;
		v[1] = (v[1]&0x8000) | (exp + 16383);
		return ret;
	}
#endif
};

inline floatexp operator*(double a, floatexp b)
{
	return floatexp(a) * b;
}

inline floatexp operator*(int a, floatexp b)
{
	return double(a) * b;
}

inline floatexp abs(floatexp a)
{
	return a.abs();
}

#endif //__FLOATEXP_H__
