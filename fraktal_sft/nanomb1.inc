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
typedef int64_t N;
typedef floatexp R_lo;
typedef decNumber R_hi;
typedef complex<R_lo> C_lo;
typedef complex<R_hi> C_hi;

static inline R_lo r_lo(const R_hi &x)
{
  return mpfr_get_fe(x.backend().data());
}

static inline C_lo c_lo(const C_hi &z)
{
  return C_lo(r_lo(z.re), r_lo(z.im));
}

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

#include <vector>

//--------------------------------------------------------------------------------------------------------
class refElem{
public:
	C_lo z;
	R_lo g;
        refElem(const C_lo &z, const R_lo &t)
	: z(z)
	, g(t * norm(z))
        { };
};
class refClass{
	R_lo m_tolerance;
	N m_n;
	std::vector<refElem> m_v;
public:
	refClass(R_lo tolerance): m_tolerance(tolerance), m_n(0) {}
	R_lo getTolerance() { return m_tolerance; }
	void adde(C_lo c){ m_v.push_back(refElem(c, m_tolerance)); m_n++;}
	const refElem & operator[](N i) const { return m_v[i % m_n];}
};

class perturbationClassD{
	C_lo m_d0;
	C_lo m_d;
	C_lo m_dd;
	N    m_n0;
	R_lo m_col;
	bool m_escaped;
	bool m_glitched;
	int64_t m_iters;
	R_lo m_test1;
	R_lo m_test2;
	double m_phase;
	C_lo m_de;
public:
	perturbationClassD(C_lo d0, C_lo d, C_lo dd, N n0): m_d0(d0), m_d(d), m_dd(dd), m_n0(n0), m_col(0), m_escaped(false), m_glitched(false), m_iters(0), m_test1(0), m_test2(0), m_phase(0), m_de(0) {}
	void run(const refClass &ref, N maxiter, R_lo pixel_spacing, double g_real, double g_imag, double pval, double mBailout2, bool singleref){
		const bool no_g = g_real == 1.0 && g_imag == 1.0 && pval == 2.0;
		N j = m_n0 - 1;
		for(N i = m_n0 ; i < maxiter && j < maxiter; i++){
			C_lo zn(ref[j].z);
			j++;
			C_lo zme(zn + m_d);
			R_lo r2(norm(zme));
			m_test2 = m_test1;
			m_test1 = r2;
			if (singleref)
			{
				R_lo mtest0 = m_test1;
				if (! no_g)
				{
					m_test1 = r2 = pnorm(g_real, g_imag, pval, zme.m_r, zme.m_i);
				}
				if (double(r2) > mBailout2){
					m_escaped = true;
					m_iters = i - 1; // this offset makes it match the regular (non-nanomb, series+perturbation) colouring
					m_phase = atan2(double(zme.m_i), double(zme.m_r)) / M_PI / 2;
					m_phase -= floor(m_phase);
					m_de = abs(zme) * log(abs(zme)) / (m_dd * pixel_spacing);
					return;
				}
				if (mtest0 < norm(m_d))
				{
					m_d = zme;
					zn = 0;
					j = 1;
				}
			}
			else
			{
				if (r2 < ref[j].g)
				{
					m_glitched = true;
					m_iters = i - 1; // this offset makes it match the regular (non-nanomb, series+perturbation) colouring
					return;
				}
				if (! no_g)
				{
					m_test1 = r2 = pnorm(g_real, g_imag, pval, zme.m_r, zme.m_i);
				}
				if (double(r2) > mBailout2){
					m_escaped = true;
					m_iters = i - 1; // this offset makes it match the regular (non-nanomb, series+perturbation) colouring
					m_phase = atan2(double(zme.m_i), double(zme.m_r)) / M_PI / 2;
					m_phase -= floor(m_phase);
					m_de = abs(zme) * log(abs(zme)) / (m_dd * pixel_spacing);
					return;
				}
			}
			m_dd = R_lo(2) * m_dd * zme + R_lo(1);
			m_d = m_d * (R_lo(2) * zn + m_d) + m_d0;
		}
	}
	bool haveEscaped(){return m_escaped;}
	bool haveGlitched(){return m_glitched;}
	int64_t getIters(){return m_iters;}
	R_lo getTest1(){return m_test1;}
	R_lo getTest2(){return m_test2;}
	double getPhase(){return m_phase;}
	C_lo getDE(){return m_de;}
};

class uniPolyClass;
class tmpPolyClass;
class biPolyClass {
	friend class uniPolyClass;
	friend class tmpPolyClass;
	N m_m, m_n;
	C_lo tab[TABSIZ][TABSIZ];
	C_lo ttab[TABSIZ][TABSIZ];
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
class tmpPolyClass {
	N m_m;
	C_lo b[TABSIZ];
public:
	tmpPolyClass(const biPolyClass &p): m_m(p.m_n) {
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
class uniPolyClass {
	N m_m;
	C_lo b[TABSIZ];
	C_lo dbdc[TABSIZ];
public:
	uniPolyClass(const biPolyClass &p, C_lo c): m_m(p.m_m) {
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

void iteratePtDE2(const C_lo &d0, const biPolyClass &fp, const refClass &ref, const C_lo &nucleusPos, N period, const R_lo &Bout, const R_lo &er2, const R_lo &pixel_spacing, N maxiter, bool &bGlitch, int64_t &antal, double &test1, double &test2, double &phase, complex<double> &de, bool interior_checking, double g_real, double g_imag, double pval, double mBailout2, bool singleref)
{
	C_lo d(0);
	C_lo dd(0);
	N i(0);
	N si(0);
	R_lo d_norm(1.0/0.0);
	if(abs(d0)<Bout){
	  uniPolyClass up(fp, d0);
		while(i<maxiter && norm(d)<Bout){
			up.eval(d, dd);
			i+=period;
			si++;
			if (interior_checking)
			{
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
			    for (int64_t n = 0; n < si; ++n)
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
			    bGlitch = 0;
			    antal = maxiter;
			    test1 = 0;
			    test2 = 0;
			    phase = 0;
			    de = 0;
			    return;
#if 0
			    // is interior, calculate interior DE
			    C_lo z(w0), dz(1), dc(0), dzdz(0), dcdz(0);
			    for (int n = 0; n < si; ++n)
			      up.eval(z, dz, dc, dzdz, dcdz);
			    R_lo de = (R_lo(1) - norm(dz)) / abs(dcdz + (dzdz * dc) / (C_lo(1) - dz));
			    iters = si * period;
			    trans = 0.0f;
			    return -de;
#endif
			  }
			}
		        }
		}
	} //else dd = C_lo(1);//knighty: That was a mistake. We begin iterating at 0 so derivative doesn't have to be changed. 
	if(i>=maxiter)
	{
	  bGlitch = 0;
          antal = maxiter;
          test1 = 0;
          test2 = 0;
	  phase = 0;
          de = 0;
          return;
	}
	//d0 and d have to be transformed into ref frame.
	C_lo d0_(d0 - nucleusPos); // FIXME unused?
	//Use perturbation to finish the job.
	//ToInvestigate: Usually when arriving here (same thing with SA) delta_c (d0 here) is too small wrt d.
	//               One can use perturbation without adding delta_c. Some other optimizations may become possible. (from comment by Pauldelbrot @fractalforums.com)
	d -= ref[i].z; //put d in reference orbit's local frame
	perturbationClassD p(d0,d,dd,i);//We use original d0 because the reference orbit was computed there.
	p.run(ref, maxiter, pixel_spacing, g_real, g_imag, pval, mBailout2, singleref);
	if (p.haveGlitched())
	{
	  bGlitch = 1;
	  antal = p.getIters();
	  test1 = double(p.getTest1());
	  test2 = double(p.getTest2());
	  phase = p.getPhase();
	  de = complex<double>(double(p.getDE().m_r), double(p.getDE().m_i));
	  return;
	}
	else if(p.haveEscaped())
	{
	  bGlitch = 0;
	  antal = p.getIters();
	  test1 = double(p.getTest1());
	  test2 = double(p.getTest2());
	  phase = p.getPhase();
	  de = complex<double>(double(p.getDE().m_r), double(p.getDE().m_i));
	  return;
	}
	else
	{
	  antal = maxiter;
	  test1 = 0;
	  test2 = 0;
	  phase = 0;
	  de = 0;
	  return;
	}
}

#if 0
  floatexp tmax(radius);
  precision = N(-tmax.exp) + 64;
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
#endif


//-------------------------------------------------------------------------------------------------------------
// entry point

struct NanoMB1_Reference
{
  biPolyClass SSA;
  refClass ref;
  C_lo nucleusPos;
  N period;
  R_lo Bout;
  R_lo er2;
  NanoMB1_Reference(const biPolyClass &SSA, const refClass &ref, const C_lo &nucleusPos, N period, const R_lo &Bout, const R_lo &er2)
  : SSA(SSA)
  , ref(ref)
  , nucleusPos(nucleusPos)
  , period(period)
  , Bout(Bout)
  , er2(er2)
  { };
};

NanoMB1_Reference *NanoMB1_Reference_Calculate(const C_hi &c, N bm, N bn, N period, N maxiters, R_lo r0, double er2, double lowGlitchTol, volatile bool &stop, int64_t &m_nRDone)
{
  double glitch_threshold = 0.0000001;
  glitch_threshold = std::exp(std::log(glitch_threshold) * (1 - lowGlitchTol / 2));
  //Compute bivariate polynomial approximating period iterations--------------------------
  biPolyClass fp(bm,bn);
  refClass ref(glitch_threshold);
  C_hi z(0);
  C_lo zlo(0);
  for(N i = 0; i < period; i++){
	ref.adde(zlo);
	z = z*z + c;
	zlo = c_lo(z);
	fp.cstep(zlo);
	m_nRDone++;
  }
  R_lo Bout = fp.getRadius();
  //--------------------------------------------------------------------------------------
  //In case the location is not "exactly" at a nucleus, we need to "correct" the perturbation reference.
  //This is because the reference orbit is computed for only one period. If the reference c is outside the mbset, it will eventually escape or be likely off 0.
  //Find the nucleus of the minibrot.
  tmpPolyClass tp(fp);
  C_lo nucleusPos = tp.getRoot();
  //Rebase the reference orbit to the nucleus
  refClass ref1(glitch_threshold);
  C_lo zlo1(0);
  for(N i = 0; i < period; i++){
	zlo = ref[i].z;
	ref1.adde(zlo+zlo1);
	zlo1 = zlo1 * (zlo1 + R_lo(2) * zlo) + nucleusPos;
  }
  //At this point zlo+zlo1 should be very close to 0
  //zlo = c_lo(z, tmax);
  //cout << "----------------" << endl << "at iteration: " << period << ", zlo1 = " << zlo1 << endl;
  //cout << "zlo1+zlo = " << zlo1+zlo << endl;
  //--------------------------------------------------------------------------------------
  if (stop)
    return nullptr;
  else
    return new NanoMB1_Reference(fp, ref, nucleusPos, period, Bout, er2);
}

void NanoMB1_Pixel(const NanoMB1_Reference *ctx, C_lo dc, R_lo pixel_spacing, N maxiters, bool &bGlitch, int64_t &antal, double &test1, double &test2, double &phase, complex<double> &de, bool interior_checking, double g_real, double g_imag, double pval, double mBailout2, bool singleref)
{
  iteratePtDE2(dc, ctx->SSA, ctx->ref, ctx->nucleusPos, ctx->period, ctx->Bout, ctx->er2, pixel_spacing, maxiters, bGlitch, antal, test1, test2, phase, de, interior_checking, g_real, g_imag, pval, mBailout2, singleref);
}
