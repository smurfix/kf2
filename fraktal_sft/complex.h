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

#ifndef KF_COMPLEX_H
#define KF_COMPLEX_H

template <class tt> class complex
{
public:
	tt m_r, m_i;
	inline complex()
	{
		m_r = 0;
		m_i = 0;
	}
	inline complex(const tt &r)
	{
		m_r = r;
		m_i = 0;
	}
	inline complex(const tt &r, const tt &i)
	{
		m_r = r;
		m_i = i;
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
		complex<tt> r;
		tt tmp;
		if(exp==0){
			r.m_r=1;
			r.m_i=0;
			return r;
		}
		r.m_r = m_r; 
		r.m_i = m_i;
		int i;
		for(i=1;i<exp;i++){
			tmp = r.m_r*m_r - r.m_i*m_i;
			r.m_i = r.m_r*m_i + r.m_i*m_r;
			r.m_r=tmp;
		}
		return r;
	}
	inline complex operator /(const complex &b) const
	{
		complex <tt> r(0,0);
		tt div = (b.m_r*b.m_r + b.m_i*b.m_i);
		if(!(div==0)){
			r.m_r = (m_r*b.m_r + m_i*b.m_i)/div;
			r.m_i = (m_i*b.m_r - m_r*b.m_i)/div;
		}
		return r;
	}
	__inline complex abs()
	{
		complex <tt> r;
		r.m_r=(m_r>0?m_r:-m_r);
		r.m_i=(m_i>0?m_i:-m_i);
		return r;
	}
	__inline complex abs_re()
	{
		complex <tt> r;
		r.m_r=(m_r>0?m_r:-m_r);
		r.m_i=m_i;
		return r;
	}
	__inline complex abs_im()
	{
		complex <tt> r;
		r.m_r=m_r;
		r.m_i=(m_i>0?m_i:-m_i);
		return r;
	}
	__inline complex re()
	{
		complex <tt> r;
		r.m_r=m_r;
		r.m_i=0;
		return r;
	}
	__inline complex im()
	{
		complex <tt> r;
		r.m_r=0;
		r.m_i=m_i;
		return r;
	}
};

template <class tt>
inline complex<tt> operator*(int a, const complex<tt> &b)
{
	return complex<tt>(a * b.m_r, a * b.m_i);
}

template <class tt>
inline complex<tt> operator*(const tt &a, const complex<tt> &b)
{
	return complex<tt>(a * b.m_r, a * b.m_i);
}

template <class tt>
inline complex<tt> operator-(int a, const complex<tt> &b)
{
	return complex<tt>(tt(a) - b.m_r, -b.m_i);
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

#endif
