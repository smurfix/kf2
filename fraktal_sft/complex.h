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
		tt ratio, den;
		tt abr, abi, cr, ci;

		if( (abr = b.m_r) < 0.)
			abr = - abr;
		if( (abi = b.m_i) < 0.)
			 abi = - abi;
		if (abi == 0) {
			fprintf(stderr, "z_div.c: division by zero\n");
			complex <tt> r;
			return r;
		}   
		if( abr < abi ) {
			ratio = b.m_r / b.m_i ;
			den = b.m_i * (1 + ratio*ratio);
			cr = (m_r*ratio + m_i) / den;
			ci = (m_i*ratio - m_r) / den;
		} else {
			ratio = b.m_i / b.m_r ;
			den = b.m_r * (1 + ratio*ratio);
			cr = (m_r + m_i*ratio) / den;
			ci = (m_i - m_r*ratio) / den;
		}
		complex <tt> r(cr,ci);
		return r;
	}
};
