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
		unsigned p = LOW_PRECISION;
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

	inline CFixedFloat(int a)
	{
		unsigned p = LOW_PRECISION;
		Precision q(p);
		m_f.precision(p);
		m_f = a;
	};

	inline CFixedFloat(double a)
	{
		unsigned p = LOW_PRECISION;
		Precision q(p);
		m_f.precision(p);
		m_f = a;
	};

	inline ~CFixedFloat()
	{
	};

	std::string ToText() const;

	inline int ToInt()
	{
		return int(m_f);
	};

	inline double ToDouble(int nScaling = 0)
	{
		using std::pow;
		unsigned p = LOW_PRECISION;
		Precision q(p);
		FixedFloat f;
		f.precision(p);
		f = m_f;
		return double(FixedFloat(f * pow(FixedFloat(10), nScaling)));
	};

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
		mpf_mul(r.backend().data(), r.backend().data(), r.backend().data());
		return CFixedFloat(r);
	};

	inline CFixedFloat Divide(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f / A.m_f));
	};

	inline CFixedFloat &Double()
	{
		mpf_mul_2exp(m_f.backend().data(), m_f.backend().data(), 1);
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
		mpf_abs(m_f.backend().data(), m_f.backend().data());
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

	friend class floatexp;
	friend class floatexp2;
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
	return CFixedFloat(FixedFloat(A.m_f * nB));
}

inline CFixedFloat operator+(double nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB * A.m_f));
}

inline CFixedFloat operator-(const CFixedFloat &A,int nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f * nB));
}

inline CFixedFloat operator-(int nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB * A.m_f));
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

#endif
