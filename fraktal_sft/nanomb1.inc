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

/*
Largely based on knighty's public domain nanomb1/nanomb2 code from:
<https://fractalforums.org/f/28/t/277/msg8132#msg8132>
<https://fractalforums.org/f/28/t/277/msg7983#msg7983>
*/

#include "nanomb1.h"
#include "Settings.h"
#define re m_r
#define im m_i
// type aliases
typedef int N;
typedef floatexp R_lo;
typedef decNumber R_hi;
typedef complex<R_lo> C_lo;
typedef complex<R_hi> C_hi;

#define TABSIZ (MAX_APPROX_TERMS + 1)

// m.cpp 2018 Knighty
// based on some code by Claude Heiland-Allen
// LICENSE: public domain, cc0, whatever
// acceleration method for rendering Mandelbrot set
// based on approximation of n iterations around minibrot of period n using bivariate polynomial
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// Knighty (2018-07-10): Edited the code in order to make it possible to render locations where the
//                       center is not exactly on the nucleus:
//                       - added the tmpPolyClass: used to find the position if the nucleus.
//                       - added code to compute the reference orbit wrt the nucleus.
//                       - added iteratePtDE2() which is almost exactly the same as iteratePtDE() but
//                         designed to take into account the new reference orbit.
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// compile:
//  g++ -std=c++11 -Ofast -mfpmath=387 -march=native mb.cpp -lmpfr -lgmp -Wall -Wextra -pedantic -fopenmp
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// run:
//  ./a --help
// view results:
//  display out.ppm
//#undef RC_INVOKED
//#undef __STRICT_ANSI__
//#include <float.h>

//#include <cassert>
#include <cstdlib>
#include <cstring>

#include <limits>

#include <iostream>
#include <fstream>

#if 0
#include <complex>
#define COMPLEX complex
#else
//A little problem with mpreal which includes <complex> so I had to change the name of the custom complex class to fcomplex
#include "complex.h"
#define COMPLEX fcomplex
#endif

#include <vector>

#include "mpfr/mpreal.h"

using namespace std;
using namespace mpfr;

#include "floatexp.h"

//--------------------------------------------------------------------------------------------------------
// Conversion routines

inline double r_lo(const R_hi &z, const double &dummy)
{
  (void) dummy;
  return mpfr_get_ld(z.mpfr_srcptr(), MPFR_RNDN);
}

inline long double r_lo(const R_hi &z, const long double &dummy)
{
  (void) dummy;
  return mpfr_get_ld(z.mpfr_srcptr(), MPFR_RNDN);
}

inline floatexp r_lo(const R_hi &z, const floatexp &dummy)
{
  (void) dummy;
  return mpfr_get_fe(z.mpfr_srcptr(), MPFR_RNDN);
}

template<typename R>
inline R r_lo(const char *s, const R &dummy)
{
  return r_lo(R_hi(s), dummy);
}

template <typename R>
inline COMPLEX<R> c_lo(const C_hi &z, const R &dummy)
{
  return COMPLEX<R>(r_lo(real(z), dummy), r_lo(imag(z), dummy));
}

//--------------------------------------------------------------------------------------------------------
// simple RGB24 image
class image
{
public:
  N width;
  N height;
  vector<uint8_t> rgb;

  // construct empty image
  image(N width, N height)
  : width(width)
  , height(height)
  , rgb(width * height * 3)
  { };

  // plot a point
  void plot(N x, N y, N r, N g, N b)
  {
    N k = (y * width + x) * 3;
    rgb[k++] = r;
    rgb[k++] = g;
    rgb[k++] = b;
  };

  // save to PPM format
  void save(const char *filename)
  {
    FILE *out = fopen(filename, "wb");
    fprintf(out, "P6\n%d %d\n255\n", width, height);
    fwrite(&rgb[0], width * height * 3, 1, out);
    fflush(out);
    fclose(out);
  }
};
//--------------------------------------------------------------------------------------------------------
template <typename R_lo>
class refClass{
	typedef COMPLEX<R_lo> C_lo;
	N m_n;
	vector<C_lo> m_v;
public:
	refClass(): m_n(0), m_v(0) {}
	void adde(C_lo c){ m_v.push_back(c); m_n++;}
	const C_lo & operator[](N i) const { return m_v[i % m_n];}
};

template <typename R_lo>
class perturbationClass{
	typedef COMPLEX<R_lo> C_lo;
	C_lo m_d0;
	C_lo m_d;
	N    m_n0;
	R_lo m_col;
	bool m_escaped;
public:
	perturbationClass(C_lo d0, C_lo d, N n0): m_d0(d0), m_d(d), m_n0(n0), m_col(0), m_escaped(false) {}
	void run(const refClass<R_lo> &ref, N maxiter){
		for(N i=m_n0; i < maxiter; i++){
			C_lo zn(ref[i]);
			m_d = m_d * (R_lo(2) * zn + m_d) + m_d0;
			R_lo r(norm(zn + m_d));
			if (r > R_lo(256)){
				m_escaped = true;
				m_col = R_lo(i+1) - R_lo(std::log(std::log(double(r)))/std::log(2.0));
				return;
			}
		}
	}
	R_lo getCol(){return m_col;}
	bool haveEscaped(){return m_escaped;}
};


template <typename R_lo>
class perturbationClassD{
	typedef COMPLEX<R_lo> C_lo;
	C_lo m_d0;
	C_lo m_d;
	C_lo m_dd;
	N    m_n0;
	R_lo m_col;
	bool m_escaped;
	int   m_iters;
	float m_trans;
public:
	perturbationClassD(C_lo d0, C_lo d, C_lo dd, N n0): m_d0(d0), m_d(d), m_dd(dd), m_n0(n0), m_col(0), m_escaped(false) {}
	void run(const refClass<R_lo> &ref, N maxiter){
		for(N i=m_n0; i < maxiter; i++){
			C_lo zn(ref[i]);
			m_dd = R_lo(2) * m_dd * (m_d + zn) + R_lo(1);
			m_d = m_d * (R_lo(2) * zn + m_d) + m_d0;
			R_lo r(norm(zn + m_d));
			if (r > R_lo(1e10)){
				m_escaped = true;
				double j = i+1 - log(log(double(r)))/log(2.0);
				m_iters = floor(j);
				m_trans = j - m_iters;
				m_col = R_lo(0.25 * std::sqrt(double(r)) * std::log(double(r))) / abs(m_dd);
				return;
			}
		}
	}
	int   getIters(){return m_iters;}
	float getTrans(){return m_trans;}
	R_lo  getCol(){return m_col;}
	bool haveEscaped(){return m_escaped;}
};

template <typename R_lo> class uniPolyClass;
template <typename R_lo> class tmpPolyClass;
template <typename R_lo>
class biPolyClass {
	friend class uniPolyClass<R_lo>;
	friend class tmpPolyClass<R_lo>;
	typedef COMPLEX<R_lo> C_lo;
	N m_m, m_n;
	C_lo tab[128][128];
	C_lo ttab[128][128];
	void mcopy(){
		for(N l=0; l <= m_m; l++)
			for(N c=0; c<= m_n; c++)
				ttab[l][c] = tab[l][c];
	}
	C_lo csqrc(N k, N l){
		C_lo v(0);
		for(N i=0; i <= k; i++)
			for(N j=0; j <= l; j++)
				v += ttab[i][j] * ttab[k-i][l-j];
		return v;
	}
public:
	biPolyClass(N m, N n): m_m(m), m_n(n) {
		for(N l=0; l <= m_m; l++)
			for(N c=0; c<= m_n; c++)
				tab[l][c] = C_lo(0);
		tab[1][0] = C_lo(1);
	}

	void sqr(){
		mcopy();
		for(N i=0; i <= m_m; i++)
			for(N j=0; j <= m_n; j++)
				tab[i][j] = csqrc(i,j);
	}

	void cstep(C_lo z){
		sqr();
		tab[0][0]  = z;
		tab[0][1] += C_lo(1);
	}

	C_lo eval(C_lo u, C_lo v){
		C_lo r(0);
		C_lo ui(1);
		for(N i=0; i <= m_m; i+=2){
			C_lo vj(ui);
			for(N j=0; j <= m_n; j++){
				r += tab[i][j] * vj;
				vj *= v;
			}
			ui *= u*u;
		}
		return r;
	}

	C_lo eval_dc(C_lo u, C_lo v){
		C_lo r(0);
		C_lo ui(1);
		for(N i=0; i <= m_m; i+=2){
			C_lo vj(ui);
			for(N j=1; j <= m_n; j++){
				r += R_lo(j) * tab[i][j] * vj;
				vj *= v;
			}
			ui *= u*u;
		}
		return r;
	}

	C_lo eval_dz(C_lo u, C_lo v){
		C_lo r(0);
		C_lo ui(u);
		for(N i=2; i <= m_m; i+=2){
			C_lo vj(C_lo(i) * ui);
			for(N j=0; j <= m_n; j++){
				r += tab[i][j] * vj;
				vj *= v;
			}
			ui *= u*u;
		}
		return r;
	}

	void print(){
		for(N i=0; i <= m_m; i++){
			for(N j=0; j <= m_n; j++){
				cout << "i: " << i << "\tj: " << j << "\tval: " << tab[i][j] << endl;
			}
			cout << "-----------------------" << endl;
		}
	}

	R_lo getRadius(){
		//return abs(tab[0][1])/abs(tab[0][2]);
		R_lo r(0);
		for(int i = 0; i < 10; i++){
			C_lo den(0);
			R_lo rr(1);
			for(int j = 2; j <=m_n; j++){
				den += tab[0][j] * rr;
				rr  *= r;
			}
			r = abs(tab[0][1])/abs(den);
		}
		return R_lo(0.5) * r;
	}
};

//temporary poly class for solving for nucleus relative position. It is initialized with the part of the SSA that depends only on c.
template <typename R_lo>
class tmpPolyClass {
	typedef COMPLEX<R_lo> C_lo;
	N m_m;
	C_lo b[128];
public:
	tmpPolyClass(const biPolyClass<R_lo> &p): m_m(p.m_n) {
		for(N i = 0; i <= m_m; i++)
			b[i] = p.tab[0][i];
	}
	
	//evaluation function. It would be nice to add an ()-operator... or not. :)
	C_lo eval(C_lo u){
		C_lo r(0);
		C_lo ui(1);
		for(N i=0; i <= m_m; i++){
			r += b[i] * ui;
			ui *= u;
		}
		return r;
	}
	
	//evaluate derivative.
	C_lo evalD(C_lo u){
		C_lo r(0);
		C_lo ui(1);
		for(N i=1; i <= m_m; i++){
			r += R_lo(i) * b[i] * ui;
			ui *= u;
		}
		return r;
	}
	
	//Gives the nearest root to the 0. To use if and when applicable (that is the reference is near 0... atom domain thing!)
	//Newton should do the job (otherwise IA-Newton ?).
	C_lo getRoot(){
		C_lo rt(0);
		//R_lo t = abs(eval(rt));
		for(N i=0; i<30; i++){
			C_lo num = eval(rt);
			C_lo den = evalD(rt);
			C_lo delta = num / den;
			num = rt;
			rt -= delta;
			if( rt.re == num.re && rt.im == num.im) break;
		}
		return rt;
	}
};

// https://fractalforums.org/f/28/t/277/msg7952#msg7952
template <typename R_lo>
class uniPolyClass {
	typedef COMPLEX<R_lo> C_lo;
	N m_m;
	C_lo b[128];
	C_lo dbdc[128];
public:
	uniPolyClass(const biPolyClass<R_lo> &p, C_lo c): m_m(p.m_m) {
		for(N i = 0; i <= m_m; i += 2)
		{
		  C_lo s(0), ds(0), cj(1), cj1(0);
		  for (N j = 0; j <= p.m_n; ++j)
		  {
		    s += p.tab[i][j] * cj;
		    ds += C_lo(j) * p.tab[i][j] * cj1;
		    cj *= c;
		    cj1 *= c;
		    if (j == 0) cj1 = C_lo(1);
		  }
		  b[i] = s;
		  dbdc[i] = ds;
		}
	}

	void eval(C_lo &z) const {
	  C_lo zs(0), zi(1);
	  for (N i = 0; i <= m_m; i += 2)
	  {
	    zs += b[i] * zi;
	    zi *= z * z;
	  }
	  z = zs;
	}

	void eval(C_lo &z, C_lo &dc) const {
	  C_lo zs(0), dcs(0), zi(1), zi1(0);
	  for (N i = 0; i <= m_m; i += 2)
	  {
	    dcs += C_lo(i) * b[i] * zi1 * dc + dbdc[i] * zi;
	    zs += b[i] * zi;
	    zi *= z * z;
	    zi1 *= z * z;
	    if (i == 0) zi1 = z;
	  }
	  z = zs;
	  dc = dcs;
	}

	void eval_dz(C_lo &z, C_lo &dz) const {
	  C_lo zs(0), dzs(0), zi(1), zi1(0);
	  for (N i = 0; i <= m_m; i += 2)
	  {
	    dzs += C_lo(i) * b[i] * zi1 * dz;
	    zs += b[i] * zi;
	    zi *= z * z;
	    zi1 *= z * z;
	    if (i == 0) zi1 = z;
	  }
	  z = zs;
	  dz = dzs;
	}

	void eval(C_lo &z, C_lo &dz, C_lo &dc, C_lo &dzdz, C_lo &dcdz) const {
	  C_lo zs(0), dzs(0), dcs(0), dzdzs(0), dcdzs(0), zi(1), zi1(0), zi2(0);
	  for (N i = 0; i <= m_m; i += 2)
	  {
	    dcdzs += C_lo(i) * C_lo(i - 1) * b[i] * zi2 * dz * dc + C_lo(i) * b[i] * zi1 * dcdz + C_lo(i) * dbdc[i] * zi1 * dz;
	    dzdzs += C_lo(i) * C_lo(i - 1) * b[i] * zi2 * dz * dz + C_lo(i) * b[i] * zi1 * dzdz;
	    dcs += C_lo(i) * b[i] * zi1 * dc + dbdc[i] * zi;
	    dzs += C_lo(i) * b[i] * zi1 * dz;
	    zs += b[i] * zi;
	    zi *= z * z;
	    zi1 *= z * z;
	    zi2 *= z * z;
	    if (i == 0) zi1 = z;
	    if (i == 0) zi2 = C_lo(1);
	  }
	  z = zs;
	  dz = dzs;
	  dc = dcs;
	  dzdz = dzdzs;
	  dcdz = dcdzs;
	}
};


template <typename R_lo>
R_lo iteratePt( COMPLEX<R_lo> d0, biPolyClass<R_lo> &fp, refClass<R_lo> &ref, N period, N maxiter, R_lo Bout, N &si ){
	typedef COMPLEX<R_lo> C_lo;
	C_lo d(0);
	C_lo ld(0);
	N i(0);
	si = N(0);
	if(abs(d0)<Bout){
		while(i<maxiter && norm(d)<Bout){
			d = fp.eval(d,d0);
			i+=period;
			si++;
		}
	}
	if(i>maxiter)
		return R_lo(-1);
	perturbationClass<R_lo> p(d0,d,i);
	p.run(ref,maxiter);
	if(p.haveEscaped())
		return p.getCol();
	else
	    return R_lo(-1);
}

template <typename R_lo>
R_lo iteratePtDE( const COMPLEX<R_lo> &d0, const biPolyClass<R_lo> &fp, const refClass<R_lo> &ref, N period, N maxiter, R_lo Bout, N &si, int &iters, float &trans){
	typedef COMPLEX<R_lo> C_lo;
	C_lo d(0);
	C_lo dd(0);
	N i(0);
	si = N(0);
	R_lo d_norm(1.0/0.0);
	if(abs(d0)<Bout){
	  uniPolyClass<R_lo> up(fp, d0);
		while(i<maxiter && norm(d)<Bout){
			up.eval(d, dd);
			i+=period;
			si++;
			if (norm(d) < d_norm)
			{
			  d_norm = norm(d);
			  // check interior: newton iterations for w0 = f^(si*p)(w0, d0)
			  C_lo w0(d);
			  C_lo dw;
			  for (int step = 0; step < 16; ++step)
			  {
			    C_lo w(w0);
			    dw = C_lo(1);
			    for (int n = 0; n < si; ++n)
			    {
			      up.eval_dz(w, dw);
			    }
			    C_lo w0_next = w0 - (w - w0) / (dw - C_lo(1));
			    R_lo delta = norm(w0_next - w0);
			    w0 = w0_next;
			    R_lo epsilon2(0); // FIXME
			    if (delta <= epsilon2) break; // converged
			  }
			  if (norm(dw) < R_lo(1))
			  {
			    // is interior, calculate interior DE
			    C_lo z(w0), dz(1), dc(0), dzdz(0), dcdz(0);
			    for (int n = 0; n < si; ++n)
			      up.eval(z, dz, dc, dzdz, dcdz);
			    R_lo de = (R_lo(1) - norm(dz)) / abs(dcdz + (dzdz * dc) / (C_lo(1) - dz));
			    iters = si * period;
			    trans = 0.0f;
			    return -de;
			  }
			}
		}
	} //else dd = C_lo(1);//knighty: That was a mistake. We begin iterating at 0 so derivative doesn't have to be changed. 
	if(i>=maxiter)
	{
	  iters = maxiter;
	  trans = 0.0f;
	  return R_lo(0);
	}
	perturbationClassD<R_lo> p(d0,d,dd,i);
	p.run(ref,maxiter);
	if(p.haveEscaped())
	{
	  iters = p.getIters();
	  trans = p.getTrans();
	  return p.getCol();
	}
	else
	{
	  iters = maxiter;
	  trans = 0.0f;
	  return R_lo(0);
	}
}

template <typename R_lo>
R_lo iteratePtDE2( const COMPLEX<R_lo> &d0, const biPolyClass<R_lo> &fp, const refClass<R_lo> &ref, COMPLEX<R_lo> nucleusPos, N period, N maxiter, R_lo Bout, N &si, int &iters, float &trans){
	typedef COMPLEX<R_lo> C_lo;
	C_lo d(0);
	C_lo dd(0);
	N i(0);
	si = N(0);
	R_lo d_norm(1.0/0.0);
	if(abs(d0)<Bout){
	  uniPolyClass<R_lo> up(fp, d0);
		while(i<maxiter && norm(d)<Bout){
			up.eval(d, dd);
			i+=period;
			si++;
			if (norm(d) < d_norm)
			{
			  d_norm = norm(d);
			  // check interior: newton iterations for w0 = f^(si*p)(w0, d0)
			  C_lo w0(d);
			  C_lo dw;
			  bool converged = false;//------------------
			  for (int step = 0; step < 16; ++step)
			  {
			    C_lo w(w0);
			    dw = C_lo(1);
			    for (int n = 0; n < si; ++n)
			    {
			      up.eval_dz(w, dw);
			    }
			    C_lo w0_next = w0 - (w - w0) / (dw - C_lo(1));
			    //R_lo delta = norm(w0_next - w0);
				C_lo delta = (w0_next - w0);//-----------------
			    w0 = w0_next;
			    //R_lo epsilon2(0); // FIXME
			    //if (delta <= epsilon2) break; // converged
				if (abs(delta.re) < abs(w0.re) * R_lo(1e-6) && abs(delta.im) < abs(w0.im) * R_lo(1e-6)) { converged = true; break;}//-----------------------
			  }
			  if (converged && norm(dw) < R_lo(1))
			  {
			    // is interior, calculate interior DE
			    C_lo z(w0), dz(1), dc(0), dzdz(0), dcdz(0);
			    for (int n = 0; n < si; ++n)
			      up.eval(z, dz, dc, dzdz, dcdz);
			    R_lo de = (R_lo(1) - norm(dz)) / abs(dcdz + (dzdz * dc) / (C_lo(1) - dz));
			    iters = si * period;
			    trans = 0.0f;
			    return -de;
			  }
			}
		}
	} //else dd = C_lo(1);//knighty: That was a mistake. We begin iterating at 0 so derivative doesn't have to be changed. 
	if(i>=maxiter)
	{
	  iters = maxiter;
	  trans = 0.0f;
	  return R_lo(0);
	}
	//d0 and d have to be transformed into ref frame.
	C_lo d0_(d0 - nucleusPos);
	d  -= ref[i];
	perturbationClassD<R_lo> p(d0_,d,dd,i);
	p.run(ref,maxiter);
	if(p.haveEscaped())
	{
	  iters = p.getIters();
	  trans = p.getTrans();
	  return p.getCol();
	}
	else
	{
	  iters = maxiter;
	  trans = 0.0f;
	  return R_lo(0);
	}
}

void plot(image &i,N x, N y, double v, N si)
{
  (void) si;
  if(v<0){
	i.plot(x, y, 0, 0, 0);
	return;
  }
  //v=std::sqrt(v);
  N r(N(255.*(.5*sin(0.1*v)+.5) ));
  N g(N(255.*(.5*sin(0.09*v)+.5) ));
  N b(N(255.*(.5*sin(0.12*v)+.5) ));
  //N r((si & 1) * 255);
  i.plot(x, y, r, g, b);
}

template <typename R_lo>
void plotDE(image &i,N x, N y, R_lo u, R_lo pixelScale)
{
  double v = 16 * log(abs(double(u * pixelScale))+1e-10);
  //if(u<0) v*=0.01;
  N r(N(255.*(.5*sin(0.1*v)+.5) ));
  N g(N(255.*(.5*sin(0.09*v)+.5) ));
  N b(N(255.*(.5*sin(0.12*v)+.5) ));
  i.plot(x, y, r, g, b);
}

static uint32_t burtle_hash(uint32_t a)
{
    a = (a+0x7ed55d16) + (a<<12);
    a = (a^0xc761c23c) ^ (a>>19);
    a = (a+0x165667b1) + (a<<5);
    a = (a+0xd3a2646c) ^ (a<<9);
    a = (a+0xfd7046c5) + (a<<3);
    a = (a^0xb55a4f09) ^ (a>>16);
    return a;
}

// uniform in [0,1)
static double jitter(uint32_t x, uint32_t y, uint32_t c)
{
  return burtle_hash(x + burtle_hash(y + burtle_hash(c))) / (double) (0x100000000LL);
}

struct kfb
{
  int w;
  int h;
  int n;
  int **iters;
  float **trans;
  float **de;
  kfb(int _w, int _h, int _n) : w(_w), h(_h), n(_n)
  {
    iters = new int  *[w];
    trans = new float*[w];
    de    = new float*[w];
    for (int x = 0; x < w; ++x)
    {
      iters[x] = new int  [h];
      trans[x] = new float[h];
      de   [x] = new float[h];
    }
  }
  ~kfb()
  {
    for (int x = 0; x < w; ++x)
    {
      delete[] iters[x];
      delete[] trans[x];
      delete[] de   [x];
    }
    delete[] iters;
    delete[] trans;
    delete[] de;
  }
  void plot(int x, int y, int _iters, float _trans, float _de)
  {
    iters[x][y] = _iters;
    trans[x][y] = _trans;
    de   [x][y] = _de;
  }
  void save(const char *filename)
  {
    FILE *f = fopen(filename, "wb");
    fwrite("KFB", 3, 1, f);
    fwrite(&w, sizeof(w), 1, f);
    fwrite(&h, sizeof(h), 1, f);
    for (int x = 0; x < w; ++x) fwrite(iters[x], sizeof(iters[x][0]) * h, 1, f);
    int iterdiv = 1;
    fwrite(&iterdiv, sizeof(iterdiv), 1, f);
    int colours = 2;
    fwrite(&colours, sizeof(colours), 1, f);
    unsigned char keys[] = { 0, 0, 0, 255, 255, 255 };
    fwrite(keys, sizeof(keys), 1, f);
    fwrite(&n, sizeof(n), 1, f);
    for (int x = 0; x < w; ++x) fwrite(trans[x], sizeof(trans[x][0]) * h, 1, f);
    for (int x = 0; x < w; ++x) fwrite(de   [x], sizeof(de   [x][0]) * h, 1, f);
    fclose(f);
  }
};

//-------------------------------------------------------------------------------------------------------------
// entry point
template <typename R_lo>
void main1(const C_hi &c, N bm, N bn, N period, N maxiters, N width, N height, const char *filename, const char *kfb_filename, const R_lo &tmax)
{
  typedef COMPLEX<R_lo> C_lo;
  //Compute bivariate polynomial approximating period iterations--------------------------
  biPolyClass<R_lo> fp(bm,bn);
  refClass<R_lo> ref;
  C_hi z(0);
  C_lo zlo(0);
  int64_t lastpc = -1;
  for(N i = 0; i < period; i++){
	ref.adde(zlo);
	z = z*z + c;
	zlo = c_lo(z, tmax);
	fp.cstep(zlo);
	int64_t pc = ((i + 1) * 100) / period;
	if (pc > lastpc)
	{
	  cerr << "\rreference " << pc << "%";
	  lastpc = pc;
	}
  }
  cerr << "__" << c_lo(z, tmax) << endl;
  fp.print();
  cerr << "R == " << fp.getRadius() << endl;
  R_lo Bout = fp.getRadius();
  
  //return 1;
  
  //--------------------------------------------------------------------------------------
  //In case the location is not "exactly" at a nucleus, we need to "correct" the perturbation reference.
  //This is because the reference orbit is computed for only one period. If the reference c is outside the mbset, it will eventually escape or be likely off 0.
  //Find the nucleus of the minibrot.
  tmpPolyClass<R_lo> tp(fp);
  C_lo nucleusPos = tp.getRoot();
  //cout << "----------------" << endl << "nucleus rel pos: " << nucleusPos << endl;
  //Rebase the reference orbit to the nucleus
  refClass<R_lo> ref1;
  C_lo zlo1(0);
  for(N i = 0; i < period; i++){
	zlo = ref[i];
	ref1.adde(zlo+zlo1);
	zlo1 = zlo1 * (zlo1 + R_lo(2) * zlo) + nucleusPos;
  }
  //At this point zlo+zlo1 should be very close to 0
  //zlo = c_lo(z, tmax);
  //cout << "----------------" << endl << "at iteration: " << period << ", zlo1 = " << zlo1 << endl;
  //cout << "zlo1+zlo = " << zlo1+zlo << endl;
  //--------------------------------------------------------------------------------------
  image i(width, height);
  kfb *k = 0;
  if (kfb_filename)
  {
    k = new kfb(width, height, maxiters);
  }
  N progress = 0;
  #pragma omp parallel for schedule(dynamic)
  for (N y = 0; y < height; ++y)
  {
    for (N x = 0; x < width; ++x)
    {
      C_lo dc(R_lo(2) * tmax *
        C_lo(width * ((x + jitter(x, y, 0)) / width - 0.5) / height, (y + jitter(x, y, 1)) / height - 0.5));
	  N si(0);
	  int iters = 0;
	  float trans = 0.0f;
	  //we need to send the nucleus position wrt the center of the picture.
	  R_lo v = iteratePtDE2( dc, fp, ref1, nucleusPos, period, maxiters, Bout, si, iters, trans);
	  //R_lo v = iteratePtDE( dc, fp, ref, period, maxiters, Bout, si, iters, trans);
	  plotDE(i, x, y, v, R_lo(60. * height)/tmax);
	  if (k)
	    k->plot(x, y, iters, trans, float(double(v * height / tmax)));
    }
    #pragma omp critical
    cerr << "\rimage scanline " << ++progress;
  }
  cerr << endl;

  // save final image
  i.save(filename);
  if (k)
  {
    k->save(kfb_filename);
    delete k;
  }
}

int main(int argc, char **argv)
{
  // initial defaults
  N width = 640;
  N height = 360;
  N maxiters = 1000;
  N bm = 16, bn = 8;
  N precision = 1000;
  const char *sre = "0";
  const char *sim = "0";
  N period = 1;
  const char *kfb_filename = 0;
  const char *filename = "out.ppm";

  // parse arguments
  // TODO FIXME bad arguments can read past end of argv
  char *memblock = 0;
  floatexp radius = 2;
  string force_type = "";
  for (N a = 1; a < argc; ++a)
  {
	if (!strcmp("--kfr", argv[a]))
    {
      const char *kfrfile = argv[++a];
      size_t size;
      ifstream file(kfrfile, ios::in|ios::binary|ios::ate);
      if (file.is_open())
      {
        size = file.tellg();
        memblock = new char [size + 1];
        file.seekg (0, ios::beg);
        file.read (memblock, size);
        file.close();
        memblock[size] = 0;

        for (size_t i = 0; i < size; ++i)
          switch (memblock[i])
          {
            case ' ':
            case '\r':
            case '\n':
              memblock[i] = 0;
          }

        for (size_t i = 0; i < size; )
        {
          if (!strcmp("Re:", memblock + i))
            sre = memblock + i + strlen(memblock + i) + 1;
          else
          if (!strcmp("Im:", memblock + i))
            sim = memblock + i + strlen(memblock + i) + 1;
          else
          if (!strcmp("Zoom:", memblock + i))
            radius = floatexp(2) / r_lo(memblock + i + strlen(memblock + i) + 1, floatexp(0));
          else
          if (!strcmp("Iterations:", memblock + i))
            maxiters = atoi(memblock + i + strlen(memblock + i) + 1);
          i = i + strlen(memblock + i) + 1;
        }
      }
      else
      {
        cerr << "Unable to open file `" << kfrfile << "'\n";
        abort();
      }
    }
    else
    if (!strcmp("--width", argv[a]))
      width = atoi(argv[++a]);
    else
    if (!strcmp("--height", argv[a]))
      height = atoi(argv[++a]);
    else
    if (!strcmp("--maxiters", argv[a]))
      maxiters = atoi(argv[++a]);
    else
    if (!strcmp("--orderM", argv[a]))
      bm = atoi(argv[++a]);
    else
    if (!strcmp("--orderN", argv[a]))
      bn = atoi(argv[++a]);
    else
    if (!strcmp("--period", argv[a]))
      period = atoi(argv[++a]);
    else
    if (!strcmp("--re", argv[a]))
      sre = argv[++a];
    else
    if (!strcmp("--im", argv[a]))
      sim = argv[++a];
    else
    if (!strcmp("--radius", argv[a]))
      radius = r_lo(argv[++a], floatexp(0));
    else
    if (!strcmp("--output_ppm", argv[a]))
      filename = argv[++a];
    else
    if (!strcmp("--output_kfb", argv[a]))
      kfb_filename = argv[++a];
    else
    if (!strcmp("--force_type", argv[a]))
      force_type = argv[++a];
    else
    {
      cerr << argv[0] << ": unexpected: " << argv[a] << endl;
      cerr
        << "usage:" << endl
        << "  --kfr s.kfr (should be first so the other option can override) " << endl
        << "  --re f" << endl
        << "  --im f" << endl
        << "  --radius f" << endl
        << "  --maxiters n" << endl
	<< "  --period n" << endl
	<< "  --width n" << endl
        << "  --height n" << endl
        << "  --orderM n" << endl
	<< "  --orderN n" << endl
        << "  --output_ppm s.ppm" << endl
        << "  --output_kfb s.kfb" << endl
        << "  --force_type 'double'|'long double'|'floatexp'" << endl
        ;
      return 1;
    }
  }
  // render
  floatexp tmax(radius);

  precision = N(-tmax.exp) + 64;
  cerr << "prec " << precision << endl;
  mpreal::set_default_prec(precision);//Was: mpfr_float::default_precision(precision);
  C_hi c((R_hi(sre)), (R_hi(sim)));

  if (force_type == "double")
    main1(c, bm, bn, period, maxiters, width, height, filename, kfb_filename, double(tmax));
  else
  if (force_type == "long double")
    main1(c, bm, bn, period, maxiters, width, height, filename, kfb_filename, (long double)(tmax));
  else
  if (force_type == "floatexp")
    main1(c, bm, bn, period, maxiters, width, height, filename, kfb_filename, floatexp(tmax));
  else
  if ((precision - 64) * 3 * (bm+2*bn) < 500 * 20 ){ // * (bm+bn) because scaling is not yet implemented 
    cerr << "using double ..." << endl;
	main1(c, bm, bn, period, maxiters, width, height, filename, kfb_filename, double(tmax));
  }
  else
  if ((precision - 64) * 3 * (bm+2*bn) < 8000  * 20 ){ // * (bm+bn) because scaling is not yet implemented
    cerr << "using long double ..." << endl;
	main1(c, bm, bn, period, maxiters, width, height, filename, kfb_filename, (long double)(tmax));
  }	
  else
  if (precision < INT_MAX){
	cerr << "using long double ..." << endl;
    main1(c, bm, bn, period, maxiters, width, height, filename, kfb_filename, floatexp(tmax));
  }
  else
  {
    cerr << "bad precision" << endl;
    abort();
  }
  return 0;
}
