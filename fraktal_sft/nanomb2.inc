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

#include "nanomb2.h"
#include "Settings.h"
#define re m_r
#define im m_i
// type aliases
typedef int64_t N;
typedef floatexp R_lo;
typedef decNumber R_hi;
typedef complex<R_lo> C_lo;
typedef complex<R_hi> C_hi;

#define TABSIZ (MAX_APPROX_TERMS + 1)

//--------------------------------------------------------------------------------------------------------
// Conversion routines
static inline R_lo r_lo(const R_hi &z)
{
  return mpfr_get_fe(z.backend().data());
}

static inline C_lo c_lo(const C_hi &z)
{
  return C_lo(r_lo(z.m_r), r_lo(z.m_i));
}

//--------------------------------------------------------------------------------------------------------
class refClass{
	N m_n;
	std::vector<C_lo> m_v;
public:
	refClass(): m_n(0), m_v(0) {}
	void adde(C_lo c){ m_v.push_back(c); m_n++;}
	C_lo & operator[](N i){ return m_v[i % m_n];}
	const C_lo & operator[](N i) const { return m_v[i % m_n];}
};

class perturbationClassD{
	C_lo m_d0;
	C_lo m_d;
	C_lo m_dd;
	N    m_si;
	N    m_n0;
	bool m_escaped;
	int64_t m_iters;
  R_lo m_test1;
  R_lo m_test2;
	double m_phase;
	C_lo m_de;
public:

	perturbationClassD(C_lo d0, C_lo d, C_lo dd, N si, N n0): m_d0(d0), m_d(d), m_dd(dd), m_si(si), m_n0(n0), m_escaped(false), m_iters(0), m_test1(0), m_test2(0), m_phase(0), m_de(0) {}

	//Experimental: Usually, after series approximation delta_0 (m_d0 here) have no effect.
	//              Here we do a first loop where we add delta_0 then, when delta_0 is too small,
	//             a second loop is performed where delta_0 is not used anymore. (idea from Pauldelbrot)
	// It works! ToDo: ...
#if 0
	// FIXME add p-norm bailout stuff
	void run1(const refClass &ref, N maxsi, N maxiter, R_lo pixel_spacing){
		N si = m_si, i = m_n0;
		for( ; si < maxsi && i < maxiter; si++, i++){
			C_lo zn(ref[i]);
			if(abs(m_d0.re)+abs(m_d0.im) < 1e-20 * (abs(zn.re)+abs(zn.im))) break;
			m_dd = R_lo(2) * m_dd * (m_d + zn) + R_lo(1);
			m_d = m_d * (R_lo(2) * zn + m_d) + m_d0;
			C_lo zme(zn + m_d);
			R_lo r(norm(zme));
      m_test2 = m_test1;
      m_test1 = r;
			if (r > R_lo(1e10)){
				m_escaped = true;
				m_iters = i;
				m_phase = atan2(double(zme.m_i), double(zme.m_r)) / M_PI / 2;
				m_phase -= floor(m_phase);
				m_de = abs(zme) * log(abs(zme)) / (m_dd * pixel_spacing);
				return;
			}
		}
		for( ; si < maxsi && i < maxiter; si++, i++){
			C_lo zn(ref[i]);
			m_dd = R_lo(2) * m_dd * (m_d + zn);// + R_lo(1);
			m_d = m_d * (R_lo(2) * zn + m_d);// + m_d0;
			C_lo zme(zn + m_d);
			R_lo r(norm(zme));
      m_test2 = m_test1;
      m_test1 = r;
			if (r > R_lo(1e10)){
				m_escaped = true;
				m_iters = i;
				m_phase = atan2(double(zme.m_i), double(zme.m_r)) / M_PI / 2;
				m_phase -= floor(m_phase);
				m_de = abs(zme) * log(abs(zme)) / (m_dd * pixel_spacing);
				return;
			}
		}
	}
#endif

	void run(const refClass &ref, N maxsi, N maxiter, R_lo pixel_spacing, double g_real, double g_imag, double pval, double nBailout2){
		const bool no_g = g_real == 1.0 && g_imag == 1.0 && pval == 2.0;
		for(N si = m_si, i = m_n0 ; si < maxsi && i < maxiter; si++, i++){
			C_lo zn(ref[i]);
			C_lo zme(zn + m_d);
			R_lo r2(norm(zme));
			m_test2 = m_test1;
			m_test1 = r2;
			// TODO perturbation glitch detection goes here?
			if (! no_g)
			{
				m_test1 = pnorm(g_real, g_imag, pval, zme.m_r, zme.m_i);
			}
			if (double(m_test1) > nBailout2){
				m_escaped = true;
				m_iters = i;
				m_phase = atan2(double(zme.m_i), double(zme.m_r)) / M_PI / 2;
				m_phase -= floor(m_phase);
				m_de = /*abs*/(zme) * log(abs(zme)) / (m_dd * pixel_spacing);
				return;
			}
			m_dd = R_lo(2) * m_dd * zme + R_lo(1);
			m_d = m_d * (R_lo(2) * zn + m_d) + m_d0;
		}
	}
	bool haveEscaped(){return m_escaped;}
	int64_t getIters(){return m_iters;}
	R_lo getTest1(){return m_test1;}
	R_lo getTest2(){return m_test2;}
	double getPhase(){return m_phase;}
	C_lo getDE(){return m_de;}
};

//--------------------------------------------------------------------------------------------
//Polynomial class
//--------------------------------------------------------------------------------------------
class polyClass{
	N m_m; //highest exponent
	C_lo tab[TABSIZ];//to store the coefficients. It is more than big enought.
public:
	//Constructor
	polyClass(N m): m_m(m) {
		for(N l=0; l <= m_m; l++)
			tab[l] = C_lo(0);
	}
	//Copy constructor
	polyClass(const polyClass& p): m_m(p.m_m) {
		for(N l=0; l <= m_m; l++)
			tab[l] = p.tab[l];
	}
	
	//evaluation function. It would be nice to add an ()-operator.
	C_lo eval(C_lo u) const{
		C_lo r(0);
		C_lo ui(1);
		for(N i=0; i <= m_m; i++){
			r += tab[i] * ui;
			ui *= u;
		}
		return r;
	}
	
	//evaluat derivative.
	C_lo evalD(C_lo u) const{
		C_lo r(0);
		C_lo ui(1);
		for(N i=1; i <= m_m; i++){
			r += R_lo(i) * tab[i] * ui;
			ui *= u;
		}
		return r;
	}
	
	//Gives the nearest root to the 0. To use if and when applicable (that is the reference is near 0... atom domain thing!)
	//Newton should do the job (otherwise IA-Newton ?).
	C_lo getRoot() const{
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
	
	//Shift the polynomial to the given (relative) origin point
	//given a polynomial p(z) this function returns the polynomial q(z)==p(z+shift)
	polyClass shift(C_lo shift) const{
		polyClass res(m_m);
		R_lo bino[TABSIZ];//vector<R_lo> bino(m_m + 2);//bino could be precomuted.
		for(int i=0;i<=m_m;i++)
			bino[i]=1;
		for(int i=0; i<=m_m; i++){
			C_lo v(0);
			for(int j=m_m-i; j>=0; j--)
				v = tab[j+i] * bino[j] + shift * v;
			for(int j=1; j<=m_m; j++)
				bino[j] += bino[j-1];
			res.tab[i] = v;
		}
		return res;
	}
	
	friend class biPolyClass;
};

//--------------------------------------------------------------------------------------------
//Bivariate polynomial class. Used for SSA.
//--------------------------------------------------------------------------------------------
class biPolyClass {
	N    m_m, m_n;
	C_lo tab[TABSIZ][TABSIZ];
	C_lo ttab[TABSIZ][TABSIZ];
	C_lo m_shift;
	N    m_period;
	R_lo m_escRadius;
	
	//copy tab into ttab. used for the squaring.
	void mcopy(){
		for(N l=0; l <= m_m; l++)
			for(N c=0; c<= m_n; c++)
				ttab[l][c] = tab[l][c];
	}
	
	//for the squaring operation: ttab will be the square of tab
	C_lo csqrc(N k, N l) const{
		C_lo v(0);
		for(N i=0; i <= k; i++)
			for(N j=0; j <= l; j++)
				v += ttab[i][j] * ttab[k-i][l-j];
		return v;
	}
public:
	//Constructor
	biPolyClass(N m, N n): m_m(m), m_n(n), m_shift(0), m_period(0), m_escRadius(0) {
		for(N l=0; l <= m_m; l++)
			for(N c=0; c<= m_n; c++)
				tab[l][c] = C_lo(0);
		tab[1][0] = C_lo(1);
	}
	
	C_lo getShift()const{ return m_shift;}
	N    getPeriod()const{ return m_period;}
	void setPeriod(N period){ m_period = period;}
	void setEscRadius(R_lo multiplier = R_lo(.1)){
		m_escRadius = multiplier * getRadius();
	}
	R_lo getEscRadius()const{ return m_escRadius;}
	
	//squaring
	void sqr(){
		mcopy();
		for(N i=0; i <= m_m; i++)
			for(N j=0; j <= m_n; j++)
				tab[i][j] = csqrc(i,j);
	}
	
	//Apply one Mandelbrot iteration
	void cstep(C_lo z){
		sqr();
		tab[0][0]  = z;
		tab[0][1] += C_lo(1);
	}
	
	//Evaluate the SSA polynomial
	C_lo eval(C_lo u, C_lo v)const{
		//v -= m_shift;//Take into account the shift
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
	
	//Evaluate the derivative wrt c
	C_lo eval_dc(C_lo u, C_lo v)const{
		//v -= m_shift;
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
	
	//Evaluate the derivative wrt z
	C_lo eval_dz(C_lo u, C_lo v)const{
		//v -= m_shift;
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
	
	//gives the escape radius for this SSA.
	//based on polynomial roots properties: Rouché theorem.
	//ToDo: Document the method by which we find the radius.
	R_lo getRadius()const{
		if(abs(tab[0][2]) < R_lo(1.0, -1100100100)) return 1e30;
#if 0
		return abs(tab[0][1])/abs(tab[0][2]);//Is actually accurate enough
#else
		R_lo r(0);
		for(int i = 0; i < m_n * m_n; i++){
			C_lo den(0);
			R_lo rr(1);
			for(int j = 2; j <=m_n; j++){
				den += tab[0][j] * rr;
				rr  *= r;
			}
			r = abs(tab[0][1])/abs(den);
		}
std::cerr << "period " << m_period << " radius " << r.toString() << std::endl;
		return r;
#endif
	}
#if 0
	//version that gives an estimate of the bail out radius in dynamic plane depending on v.
	R_lo getRadius(C_lo v)const{
		C_lo t2(0), t4(0), vi(1);
		for(N i = 0; i <= m_n; i++){
			t2 += tab[2][i] * vi;
			t4 += tab[4][i] * vi;
			vi *= v;
		}
		return abs(t2)/abs(R_lo(2)*t4);//Is actually accurate enough... 
	}
#endif
	//Just give the part that depends on v only. It can be used as the classical series approximation.
	polyClass getSAPoly()const{
		polyClass SApol(m_n);
		for(N i=0; i<=m_n; i++)
			SApol.tab[i] = tab[0][i];
		return SApol;
	}
	
	//Partially evaluate the biPoly for the given v value. 
	polyClass getEvalPoly(C_lo v)const{
		polyClass SApol(m_m);
		//v -= m_shift;
		for(N i=0; i<=m_m; i++){
			C_lo vj(1);
			C_lo r(0);
			for(N j=0; j <= m_n; j++){
				r  += tab[i][j] * vj;
				vj *= v;
			}
			SApol.tab[i] = r;
		}
		return SApol;
	}
	
	//Partially evaluate the biPoly for the given v value. This is for derivative evaluation.
	polyClass getEvalPoly_dz(C_lo v)const{
		polyClass SApol = getEvalPoly(v);
		//v -= m_shift;
		for(N i=0; i<m_m; i++){
			SApol.tab[i] = SApol.tab[i+1] * R_lo(i+1);
		}
		SApol.m_m -= 1;
		return SApol;
	}
	
	//Partially evaluate the biPoly for the given v value. This is for derivative evaluation.
	//To implement
	polyClass getEvalPoly_dc(C_lo v)const{
		polyClass SApol(m_m);
		//v -= m_shift;
		for(N i=0; i<=m_m; i++){
			C_lo vi(1);
			C_lo r(0);
			for(N j=1; j<=m_n; j++){
				r  += R_lo(j) * vi * tab[i][j];
				vi *= v;
			}
			SApol.tab[i] = r;
		}
		return SApol;
	}
	
	//gives a shifted biPoly.We shift only wrt v param therefore we will use the shift function of polyClass.
	biPolyClass getShiftedBP(C_lo sv)const{
		biPolyClass SBP(m_m,m_n);
		for(N i=0; i<=m_m; i++){
			polyClass pol(m_n);
			for(N j=0; j<=m_n; j++)
				pol.tab[j] = tab[i][j];
			polyClass popol = pol.shift(sv);
			for(N j=0; j<=m_n; j++)
				SBP.tab[i][j] = popol.tab[j];
		}
		SBP.m_shift = m_shift + sv;//ToDo: verify if itis necessary to do add m_shift . This function is meant to be called only for the main SSA where shift==0. Just in case...
		return SBP;
	}
};

//---------------------------------------------------------------------------------------------------
// iterate using the given list of SSAs.
// d0 is relative to the center of the screen. The SSAs transparently transform the point into their own reference frame when called thanks to their m_shift member variable.
//---------------------------------------------------------------------------------------------------

static void iteratePtDE1( C_lo d0, const std::vector<biPolyClass> &SSAs, const refClass &ref, N maxsi, N maxiter, R_lo pixel_spacing, int64_t &antal, double &test1, double &test2, double &phase, complex<double> &de, bool interior_checking, double g_real, double g_imag, double pval, double nBailout2){
	//try the SSAs from the latest (deepest) to the first. if not possible use perturbation until we get near to 0 again then repeat.
	//ToDo: edit refClass to add another run(inside radius) function that verify if the iterated point returns near 0
	//ToDo2: we need to test for escapes AFTER the iteration --> edit biPolyClass to compute the "other" bailout.
	
	C_lo d(0);
	C_lo dd(0);
	N i(0);
	N si(0);
	N nbrSSAs(SSAs.size());
	N curSSA(-1);
	
	//first iteration with SSA. It uses bailout radius in parameter space.
	for(N k = nbrSSAs - 1; k>=0; k--){
		C_lo d00 = d0 - SSAs[k].getShift();//delta_c coordinates have de be wrt current SSA
		if(abs(d00)<SSAs[k].getEscRadius()){
			dd = dd * SSAs[k].eval_dz(d,d00) + SSAs[k].eval_dc(d,d00);
			d = SSAs[k].eval(d,d00);
			i+= SSAs[k].getPeriod();
			si++;
			curSSA = k;
			break;
		}
	}
	N firstSSA = curSSA;
	R_lo d_norm(1.0/0.0);
	//Use SSAs from lower to upper to iterate
	for(; curSSA>=0 && SSAs[curSSA].getPeriod() >= 1; curSSA--){
             	C_lo d00 = d0 - SSAs[curSSA].getShift();//delta_c coordinates have de be wrt current SSA
		while(si < maxsi && i<maxiter && norm(d)<SSAs[curSSA].getEscRadius()) {// norm(d)<SSAs[curSSA].getEscRadius() <=> abs(d)<sqrt(SSAs[curSSA].getEscRadius())
		if (interior_checking)
		{
			if (curSSA == firstSSA && norm(d) < d_norm)
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
			      dw = dw * SSAs[curSSA].eval_dz(w, d00);
			      w = SSAs[curSSA].eval(w, d00);
			    }
			    C_lo w0_next = w0 - (w - w0) / (dw - C_lo(1));
			    R_lo delta = norm(w0_next - w0);
			    //C_lo delta = (w0_next - w0);//-----------------
			    w0 = w0_next;
			    R_lo epsilon2(0); // FIXME
			    if (delta <= epsilon2) { converged = true;break; }; // converged
			    //if (abs(delta.re) < abs(w0.re) * R_lo(1e-6) && abs(delta.im) < abs(w0.im) * R_lo(1e-6)) { converged = true; break;}//-----------------------
			  }
			  if (converged && norm(dw) < R_lo(1))
			  {
          antal = maxiter;
          test1 = 0;
          test2 = 0;
	  phase = 0;
          de = 0;
          return;
#if 0			    
			    // is interior, calculate interior DE
			    C_lo z(w0), dz(1), dc(0), dzdz(0), dcdz(0);
			    for (int64_t n = 0; n < si; ++n)
			      up.eval(z, dz, dc, dzdz, dcdz);
			    R_lo de = (R_lo(1) - norm(dz)) / abs(dcdz + (dzdz * dc) / (C_lo(1) - dz));
			    iters = si * period;
			    trans = 0.0f;
			    return -de;
#endif
			  }
			}
		}
			dd = dd * SSAs[curSSA].eval_dz(d,d00) + SSAs[curSSA].eval_dc(d,d00);
			d  = SSAs[curSSA].eval(d,d00);
			i += SSAs[curSSA].getPeriod();
			si++;
		}
		if(si >= maxsi || i >= maxiter) break;
	}
	
	//We are out of SSAs or maxiter attained
	//R_lo f(1);
	//if(i == 0) {dd = C_lo(1); f = R_lo(.5);}
	if(si >= maxsi || i>=maxiter)
	{
	  antal = maxiter;
	  test1 = 0;
    test2 = 0;
	  phase = 0;
    de = 0;
	  return;
	}
	
	//Use perturbation to finish the job.
	//ToInvestigate: Usually when arriving here (same thing with SA) delta_c (d0 here) is too small wrt d.
	//               One can use perturbation without adding delta_c. Some other optimizations may become possible. (from comment by Pauldelbrot @fractalforums.com)
	d -= ref[i]; //put d in reference orbit's local frame
	perturbationClassD p(d0,d,dd,si,i);//We use original d0 because the reference orbit was computed there.
	p.run(ref,maxsi,maxiter, pixel_spacing, g_real, g_imag, pval, nBailout2);
	
	if(p.haveEscaped())
	{
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

//-------------------------------------------------------------------------------------------------------------
// entry point

struct NanoMB2_Reference
{
  std::vector<biPolyClass> SSAs;
  refClass ref;
  NanoMB2_Reference(std::vector<biPolyClass> SSAs, refClass ref)
  : SSAs(SSAs)
  , ref(ref)
  { };
};

NanoMB2_Reference *NanoMB2_Reference_Calculate(const C_hi &c, N bm, N bn, N maxperiod, R_lo r0, volatile bool &stop, int64_t &m_nRDone, R_lo radius_scale)
{
  R_lo tmax = r0;

  //Compute bivariate polynomial approximating period iterations--------------------------
  //ToDo: compute also the bivariate polynomial for the "upper level" hyperbolic components centres (HCC):
  //   - Use the running bivariate polynomial... actually the part that doesn't depend on "u" (the SA part)
  //   - Find the upper HCCs by solving the zroes of the SA poly when applicable.
  //   - We will not suppose that "c" is at the center of a minibrot anymore.
  //   - The last (deepest) HC is the one which center is inside or near the rendered area
  //   This procedure may fail when the SA part of tha bipoly is no longer accurate while we haven't found
  //  an HCC inside the rendered area and that area is too small to be handled by the previous HCC
  //  (insufficient accuracy). Maybe a kind of "super" perturbation?
  //    For now we will suppose we have a good deepest HCC.  
  //ToDo: what about the case where no deepest minibrot is found... We still need good reference points... maybe subdivide the rendered area?
  biPolyClass fp(bm,bn);
  std::vector<biPolyClass> SSAs;//to store the set of the relevent hyperbolic components' SSAs from lowest to highest period.
  R_lo smallest_ref_rad_so_far(1000);//for atom domain
  refClass ref;
  C_hi z(0);
  C_lo zlo(0);
  bool bNotFinished = true;
  bool stopRecSSAs = false;
  
  for(N i = 0; i < maxperiod && bNotFinished && !stop; i++){
	ref.adde(zlo);
	z = z*z + c;
	zlo = c_lo(z);
	fp.cstep(zlo);
	//See if we have a new atom domain... Atom domain does work but not ver well
	//R_lo eRad = fp.getRadius() * R_lo(1000.);//scale down in order to not choose too far HC center. ToVerify... worse!
	//doing "if(zmag < /*eRad*/ R_lo(.5)*smallest_ref_rad_so_far)" is better (with the .5 factor which removes a lot of SSAs)
	R_lo zmag = abs(zlo);
	
	if(zmag < /*eRad*/ R_lo(.5)*smallest_ref_rad_so_far /*&& i>7*/ && !stopRecSSAs){
		//compute the SSA for current hyperbolic component and store it in SSAs:		
		
		//get the SA
		polyClass SA = fp.getSAPoly();
		
		//compute the corresponding HC center
		C_lo HCcentre = SA.getRoot();//HCcentre is relative to c.
		
		//get shifted SSA
		biPolyClass SSA = fp.getShiftedBP(HCcentre);
		SSA.setPeriod(i+1);
		SSA.setEscRadius(radius_scale);
		
		if(SSA.getEscRadius() > R_lo(0.1) * tmax){
			//Store the shifted SSA into SSAs
			//We don't need HC with too low period. They may be slower than using perturbation.
			if(i>7)//---------------------------------------------- 
				SSAs.push_back(SSA);
		
			//if HCcentre is inside or near the rendered area we should stop!
			//--> We will consider that HCcenter is near when it's magnitude is less than say 1000th the rendered area width? to study!!!
			//if(abs(HCcentre) < R_lo(100) * tmax) bNotFinished = false;
		
		} else stopRecSSAs = true;
		//
		smallest_ref_rad_so_far = zmag;
	}
  m_nRDone++;
  }
  if (stop)
    return nullptr;
  else
    return new NanoMB2_Reference(SSAs, ref);
}

void NanoMB2_Pixel(const NanoMB2_Reference *ctx, C_lo dc, R_lo pixel_spacing, N maxsi, N maxiters, bool &bGlitch, int64_t &antal, double &test1, double &test2, double &phase, complex<double> &de, bool interior_checking, double g_real, double g_imag, double pval, double nBailout2)
{
  iteratePtDE1(dc, ctx->SSAs, ctx->ref, maxsi, maxiters, pixel_spacing, antal, test1, test2, phase, de, interior_checking, g_real, g_imag, pval, nBailout2);
  bGlitch = false;
}
