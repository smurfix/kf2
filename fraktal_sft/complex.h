#ifndef __COMPLEX_H__
#define __COMPLEX_H__
template <class tt> class complex
{
public:
	tt m_r, m_i;
	complex()
	{
		m_r = 0;
		m_i = 0;
	}
	complex(tt r, tt i)
	{
		m_r = r;
		m_i = i;
	}
	__inline complex &operator =(complex &a)
	{
		m_r = a.m_r;
		m_i = a.m_i;
		return *this;
	}
	__inline complex operator *(complex &a)
	{
		complex<tt> r;
		r.m_r = m_r*a.m_r - m_i*a.m_i;
		r.m_i = m_r*a.m_i + m_i*a.m_r;
		return r;
	}
	__inline complex operator +(complex &a)
	{
		complex<tt> r;
		r.m_r = m_r + a.m_r; 
		r.m_i = m_i + a.m_i;
		return r;
	}
	__inline complex operator -(complex &a)
	{
		complex<tt> r;
		r.m_r = m_r - a.m_r; 
		r.m_i = m_i - a.m_i;
		return r;
	}
	__inline complex &operator +=(complex &a)
	{
		m_r += a.m_r; 
		m_i += a.m_i;
		return *this;
	}
	__inline complex operator ^(int exp)
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
	__inline complex operator /(complex &b)
	{
		complex <tt> r;
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
#endif //__COMPLEX_H__
