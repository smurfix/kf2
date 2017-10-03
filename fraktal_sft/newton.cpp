#include <windows.h>
#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include <time.h>
#include <float.h>
#include "resource.h"
#include "complex.h"
#include "fraktal_sft.h"
#include "CDecNumber.h"

#ifdef KF_THREADED_REFERENCE_BARRIER
#include "../common/barrier.h"
#endif

extern CFraktalSFT g_SFT;
BOOL g_bNewtonRunning=FALSE;
BOOL g_bNewtonStop=FALSE;
BOOL g_bNewtonExit=FALSE;
char g_szProgress[128];
char *g_szRe=NULL;
char *g_szIm=NULL;
char *g_szZoom=NULL;
BOOL g_nMinibrotPos=0;
extern double g_nZoomSize;


#define flyttyp CDecNumber

complex<flyttyp> _2(2,0);
complex<flyttyp> _1(1,0);

static inline int sgn(flyttyp z) {
  if (z > 0) { return  1; }
  if (z < 0) { return -1; }
  return 0;
}

static inline bool odd(int a) {
  return a & 1;
}

static inline floatexp cabs2(complex<floatexp> z) {
  return z.m_r * z.m_r + z.m_i * z.m_i;
}
static inline flyttyp cabs2(complex<flyttyp> z) {
  return z.m_r * z.m_r + z.m_i * z.m_i;
}
static inline bool isfinite(flyttyp a){
	return true;
/*	if (a <= DBL_MAX && a >= -DBL_MAX)
		return true;
	return false;*/
}
static inline bool cisfinite(complex<flyttyp> z) {
  return isfinite(z.m_r) && isfinite(z.m_i);
}

static const flyttyp pi = 3.141592653589793;
static const flyttyp twopi = 6.283185307179586;

static flyttyp cross(const complex<flyttyp> &a, const complex<flyttyp> &b) {
	return a.m_i * b.m_r - a.m_r * b.m_i;
}

static bool crosses_positive_real_axis(const complex<flyttyp> &a, const complex<flyttyp> &b) {
  if (sgn(a.m_i) != sgn(b.m_i)) {
    complex<flyttyp> d = b - a;
    int s = sgn(d.m_i);
    int t = sgn(cross(d, a));
    return s == t;
  }
  return false;
}

static bool surrounds_origin(const complex<flyttyp> &a, const complex<flyttyp> &b, const complex<flyttyp> &c, const complex<flyttyp> &d) {
  return odd
    ( crosses_positive_real_axis(a, b)
    + crosses_positive_real_axis(b, c)
    + crosses_positive_real_axis(c, d)
    + crosses_positive_real_axis(d, a)
    );
}

struct m_d_box_period {
  complex<flyttyp> c[4];
  complex<flyttyp> z[4];
  int p;
};

m_d_box_period *m_d_box_period_new(const complex<flyttyp> &center, const flyttyp &radius) {
  m_d_box_period *box = new m_d_box_period;
  if (! box) {
    return 0;
  }
  box->z[0] = box->c[0] = center + complex<flyttyp>(-radius, -radius);
  box->z[1] = box->c[1] = center + complex<flyttyp>(radius, -radius);
  box->z[2] = box->c[2] = center + complex<flyttyp>(radius, radius);
  box->z[3] = box->c[3] = center + complex<flyttyp>(-radius, radius);
  box->p = 1;
  return box;
}

void m_d_box_period_delete(m_d_box_period *box) {
  if (box) {
    delete box;
  }
}

bool m_d_box_period_step(m_d_box_period *box) {
  if (! box) {
    return false;
  }
  bool ok = true;
  for (int i = 0; i < 4; ++i) {
    box->z[i] = box->z[i] * box->z[i] + box->c[i];
    ok = ok && cisfinite(box->z[i]);
  }
  box->p = box->p + 1;
  return ok;
}

bool m_d_box_period_have_period(m_d_box_period *box) {
  if (! box) {
    return true;
  }
  return surrounds_origin(box->z[0], box->z[1], box->z[2], box->z[3]);
}

int m_d_box_period_get_period(const m_d_box_period *box) {
  if (! box) {
    return 0;
  }
  return box->p;
}

#ifdef KF_THREADED_REFERENCE_BARRIER
struct BoxPeriod
{
  int threadid;
  barrier *barrier;
  volatile BOOL *stop;
  int maxperiod;
  mpf_t zr, zi, zr2, zi2, zri, cr, ci, t, *z2r, *z2i;
  int *crossing[4];
  int *haveperiod;
  int *period;
  HANDLE hDone;
  HWND hWnd;
};
DWORD WINAPI ThBoxPeriod(BoxPeriod *b)
{
  int t = b->threadid;
  barrier *barrier = b->barrier;
  volatile BOOL *stop = b->stop;
  int *haveperiod = b->haveperiod;
  int *period = b->period;
  char szStatus[300];
  uint32_t last;
  if (t == 0)
  {
    wsprintf(szStatus,"Finding period, 0...");
    SetDlgItemText(b->hWnd,IDC_EDIT1,szStatus);
    last = GetTickCount();
  }
  for (int i = 1; i < b->maxperiod; ++i)
  {
    // *b->crossing[t] = crosses_positive_real_axis(&b->zr, &b->zi, b->z2r, b->z2i);
    bool crossing = false;
    if (mpf_sgn(b->zi) != mpf_sgn(*b->z2i))
    {
      // d = b - a;
      mpf_sub(b->zr2, *b->z2r, b->zr);
      mpf_sub(b->zi2, *b->z2i, b->zi);
      int fs = mpf_sgn(b->zi2);
      // t = cross(d, a);
      mpf_mul(b->zri, b->zi2, b->zr);
      mpf_mul(b->zi2, b->zr2, b->zi);
      mpf_sub(b->t, b->zri, b->zi2);
      int ft = mpf_sgn(b->t);
      crossing = fs == ft;
    }
    *b->crossing[t] = crossing;
    if (barrier->wait(stop)) break;
    if (t == 0)
    {
      if(i%100==0){
	uint32_t now = GetTickCount();
	if (now - last > 250){
	  wsprintf(szStatus,"Finding period, %d...",i);
	  SetDlgItemText(b->hWnd,IDC_EDIT1,szStatus);
	  last = now;
	}
      }
      int surround = 0;
      for (int s = 0; s < 4; ++s)
      {
	surround += *b->crossing[s];
      }
      if (surround & 1)
      {
	*haveperiod = true;
	*period = i;
      }
    }
    // z = z * z + c
    mpf_mul(b->zr2, b->zr, b->zr);
    mpf_mul(b->zri, b->zr, b->zi);
    mpf_mul(b->zi2, b->zi, b->zi);
    mpf_sub(b->t, b->zr2, b->zi2);
    mpf_add(b->zr, b->t, b->cr);
    mpf_mul_2exp(b->zri, b->zri, 1);
    mpf_add(b->zi, b->zri, b->ci);
    if (barrier->wait(stop)) break;
    if (*haveperiod) break;
  }
  SetEvent(b->hDone);
  return 0;
}
#endif

 int m_d_box_period_do(const complex<flyttyp> &center, flyttyp radius, int maxperiod,int &steps,HWND hWnd) {
	 radius = flyttyp(4)/radius;

#ifdef KF_THREADED_REFERENCE_BARRIER

  mp_bitcnt_t bits = mpf_get_prec(center.m_r.m_dec.backend().data());
  barrier bar(4);
  complex<flyttyp> c[4];
  c[0] = center + complex<flyttyp>(-radius, -radius);
  c[1] = center + complex<flyttyp>(radius, -radius);
  c[2] = center + complex<flyttyp>(radius, radius);
  c[3] = center + complex<flyttyp>(-radius, radius);
  int crossing[4] = { 0, 0, 0, 0 };
  int haveperiod = false;
  int period = 0;
  HANDLE hDone[4];
  // prepare threads
  BoxPeriod box[4];
  for (int t = 0; t < 4; ++t)
  {
    box[t].threadid = t;
    box[t].barrier = &bar;
    box[t].maxperiod = maxperiod;
    mpf_init2(box[t].cr, bits); mpf_set(box[t].cr, c[t].m_r.m_dec.backend().data());
    mpf_init2(box[t].ci, bits); mpf_set(box[t].ci, c[t].m_i.m_dec.backend().data());
    mpf_init2(box[t].zr, bits); mpf_set(box[t].zr, c[t].m_r.m_dec.backend().data());
    mpf_init2(box[t].zi, bits); mpf_set(box[t].zi, c[t].m_i.m_dec.backend().data());
    mpf_init2(box[t].zr2, bits);
    mpf_init2(box[t].zi2, bits);
    mpf_init2(box[t].zri, bits);
    mpf_init2(box[t].t, bits);
    box[t].z2r = &box[(t + 1) % 4].zr;
    box[t].z2i = &box[(t + 1) % 4].zi;
    box[t].stop = &g_bNewtonStop;
    box[t].crossing[0] = &crossing[0];
    box[t].crossing[1] = &crossing[1];
    box[t].crossing[2] = &crossing[2];
    box[t].crossing[3] = &crossing[3];
    box[t].haveperiod = &haveperiod;
    box[t].period = &period;
    box[t].hDone = hDone[t] = CreateEvent(NULL, 0, 0, NULL);
    box[t].hWnd = hWnd;
  }
  // spawn threads
  for (int i = 0; i < 4; i++)
  {
    DWORD dw;
    HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)ThBoxPeriod, (LPVOID)&box[i], 0, &dw);
#ifdef KF_THREAD_AFFINITY
    SetThreadAffinityMask(hThread, 1<<i);
#endif
    CloseHandle(hThread);
  }
  // wait for threads to complete
  WaitForMultipleObjects(4, hDone, TRUE, INFINITE);
  for (int i = 0; i < 4; i++)
  {
    CloseHandle(hDone[i]);
    mpf_clear(box[i].cr);
    mpf_clear(box[i].ci);
    mpf_clear(box[i].zr);
    mpf_clear(box[i].zi);
    mpf_clear(box[i].zr2);
    mpf_clear(box[i].zi2);
    mpf_clear(box[i].zri);
    mpf_clear(box[i].t);
  }
  steps = period;
  return haveperiod ? period : 0;

#else

  m_d_box_period *box = m_d_box_period_new(center, radius);
  if (! box) {
    return 0;
  }
  int period = 0;
  int i;
  char szStatus[256];
  wsprintf(szStatus,"Finding period, 0...");
  SetDlgItemText(hWnd,IDC_EDIT1,szStatus);
  uint32_t last = GetTickCount();
  for (i = 0; i < maxperiod && !g_bNewtonStop; ++i) {
    if(i%100==0){
      uint32_t now = GetTickCount();
      if (now - last > 250){
	wsprintf(szStatus,"Finding period, %d...",i);
	SetDlgItemText(hWnd,IDC_EDIT1,szStatus);
	last = now;
      }
    }
    if (m_d_box_period_have_period(box)) {
      period = m_d_box_period_get_period(box);
      break;
    }
    if (! m_d_box_period_step(box)) {
      break;
    }
  }
  steps=i;
  m_d_box_period_delete(box);
  return period;

#endif
}

#ifdef KF_THREADED_REFERENCE_EVENT
struct STEP_STRUCT
{
	int nType;
	complex<flyttyp> *z;
	complex<flyttyp> *dc;
	complex<flyttyp> *zr;
	complex<flyttyp> *c_guess;
	HANDLE hWait;
	HANDLE hExit;
	HANDLE hDone;
};
DWORD WINAPI ThStep(STEP_STRUCT *pMC)
{
	HANDLE hW[2];
	hW[0] = pMC->hWait;
	hW[1] = pMC->hExit;
	while (WaitForMultipleObjects(2, hW, FALSE, INFINITE) == WAIT_OBJECT_0){
		if (pMC->nType == 0)
			(*pMC->dc) = _2 * (*pMC->z) * (*pMC->dc) + _1;
		else
			(*pMC->zr) = (*pMC->z) * (*pMC->z) + (*pMC->c_guess);
		SetEvent(pMC->hDone);
	}
	SetEvent(pMC->hDone);
	return 0;
}
#else
#ifdef KF_THREADED_REFERENCE_BARRIER
struct STEP_STRUCT_COMMON
{
	HWND hWnd;
	barrier *barrier;
	volatile BOOL *stop;
	int newtonStep;
	int period;
	mpf_t zr, zi, zrn, zin, zr2, zi2, cr, ci, dcr, dci, dcrn, dcin, dcrzr, dcrzi, dcizr, dcizi;
};
struct STEP_STRUCT
{
	int nType;
	HANDLE hDone;
	STEP_STRUCT_COMMON *common;
};
DWORD WINAPI ThStep(STEP_STRUCT *t0)
{
  struct STEP_STRUCT_COMMON *t = t0->common;
  char szStatus[256];
  uint32_t last = GetTickCount();
  switch (t0->nType)
  {
    case 0: // zr
      for (int i = 0; i < t->period; ++i)
      {
	mpf_mul(t->zr2, t->zr, t->zr);
	mpf_mul(t->zi2, t->zi, t->zi);
	mpf_sub(t->zrn, t->zr2, t->zi2);
	if (t->barrier->wait(t->stop)) break;
	mpf_add(t->zr, t->zrn, t->cr);
	if (t->barrier->wait(t->stop)) break;
      }
      break;
    case 1: // zi
      for (int i = 0; i < t->period; ++i)
      {
	if(i%100==0){
		uint32_t now = GetTickCount();
		if (now - last > 250)
		{
			wsprintf(szStatus,"Newton-Raphson %d(%d%%) %s",t->newtonStep,i*100/t->period,g_szProgress);
			SetDlgItemText(t->hWnd,IDC_EDIT1,szStatus);
			last = now;
		}
	}
	mpf_mul(t->zin, t->zr, t->zi);
	mpf_mul_2exp(t->zin, t->zin, 1);
	if (t->barrier->wait(t->stop)) break;
	mpf_add(t->zi, t->zin, t->ci);
	if (t->barrier->wait(t->stop)) break;
      }
      break;
    case 2: // dcr
      for (int i = 0; i < t->period; ++i)
      {
	mpf_mul(t->dcrzr, t->dcr, t->zr);
	mpf_mul(t->dcizi, t->dci, t->zi);
	mpf_sub(t->dcrn, t->dcrzr, t->dcizi);
	mpf_mul_2exp(t->dcrn, t->dcrn, 1);
	if (t->barrier->wait(t->stop)) break;
	mpf_add_ui(t->dcr, t->dcrn, 1);
	if (t->barrier->wait(t->stop)) break;
      }
      break;
    case 3: // dci
      for (int i = 0; i < t->period; ++i)
      {
	mpf_mul(t->dcrzi, t->dcr, t->zi);
	mpf_mul(t->dcizr, t->dci, t->zr);
	mpf_add(t->dcin, t->dcrzi, t->dcizr);
	mpf_mul_2exp(t->dcin, t->dcin, 1);
	if (t->barrier->wait(t->stop)) break;
	mpf_set(t->dci, t->dcin);
	if (t->barrier->wait(t->stop)) break;
      }
      break;
  }
  SetEvent(t0->hDone);
  return 0;
}
#endif
#endif
extern int m_d_nucleus_step(complex<flyttyp> *c_out, const complex<flyttyp> &c_guess, int period,flyttyp &epsilon2,HWND hWnd,int newtonStep) {
  complex<flyttyp> z(0,0);
  complex<flyttyp> zr(0,0);
  complex<flyttyp> dc(0,0);
  int i;

#ifdef KF_THREADED_REFERENCE_EVENT
	int threads = 2;
	STEP_STRUCT mc[2];
	HANDLE hWait[2];
	HANDLE hExit[2];
	HANDLE hDone[2];
	for (i = 0; i<2; i++){
		mc[i].z = &z;
		mc[i].zr = &zr;
		mc[i].dc = &dc;
		mc[i].c_guess = &c_guess;
		hDone[i] = mc[i].hDone = CreateEvent(NULL, 0, 0, NULL);
		hWait[i] = mc[i].hWait = CreateEvent(NULL, 0, 0, NULL);
		hExit[i] = mc[i].hExit = CreateEvent(NULL, 0, 0, NULL);
		mc[i].nType = i;
	}
#else

#ifdef KF_THREADED_REFERENCE_BARRIER
	int threads = 4;
	mp_bitcnt_t bits = mpf_get_prec(c_guess.m_r.m_dec.backend().data());
	barrier bar(4);
	STEP_STRUCT_COMMON m;
	m.hWnd = hWnd;
	m.barrier = &bar;
	m.stop = &g_bNewtonStop;
	m.newtonStep = newtonStep;
	m.period = period;
	mpf_init2(m.zr, bits); mpf_set_ui(m.zr, 0);
	mpf_init2(m.zi, bits); mpf_set_ui(m.zi, 0);
	mpf_init2(m.zrn, bits);
	mpf_init2(m.zin, bits);
	mpf_init2(m.zr2, bits);
	mpf_init2(m.zi2, bits);
	mpf_init2(m.cr, bits); mpf_set(m.cr, c_guess.m_r.m_dec.backend().data());
	mpf_init2(m.ci, bits); mpf_set(m.ci, c_guess.m_i.m_dec.backend().data());
	mpf_init2(m.dcr, bits); mpf_set_ui(m.dcr, 0);
	mpf_init2(m.dci, bits); mpf_set_ui(m.dci, 0);
	mpf_init2(m.dcrn, bits);
	mpf_init2(m.dcin, bits);
	mpf_init2(m.dcrzr, bits);
	mpf_init2(m.dcrzi, bits);
	mpf_init2(m.dcizr, bits);
	mpf_init2(m.dcizi, bits);
	STEP_STRUCT mc[4];
	HANDLE hDone[4];
	for (i = 0; i<4; i++){
		mc[i].nType =i;
		hDone[i] = mc[i].hDone = CreateEvent(NULL, 0, 0, NULL);
		mc[i].common = &m;
	}

#endif
#endif

	HANDLE hThread;
	DWORD dw;
	for (i = 0; i<threads; i++){
		hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)ThStep, (LPVOID)&mc[i], 0, &dw);
#ifdef KF_THREAD_AFFINITY
		SetThreadAffinityMask(hThread, 1<<i);
#endif
		CloseHandle(hThread);
	}

#ifdef KF_THREADED_REFERENCE_EVENT
  uint32_t last = GetTickCount();
  for (i = 0; i < period && !g_bNewtonStop; ++i) {
	  if(i%100==0){
		  uint32_t now = GetTickCount();
		  if (now - last > 250)
		  {
		    char szStatus[256];
		    wsprintf(szStatus,"Newton-Raphson %d(%d%%) %s",newtonStep,i*100/period,g_szProgress);
		    SetDlgItemText(hWnd,IDC_EDIT1,szStatus);
		    last = now;
		  }
	  }
//	  dc = _2 * z * dc + _1;
//	  z = z * z + c_guess;
		SetEvent(hWait[0]);
		SetEvent(hWait[1]);
		WaitForMultipleObjects(2, hDone, TRUE, INFINITE);
		z=zr;
  }
	SetEvent(hExit[0]);
	SetEvent(hExit[1]);
#endif

	WaitForMultipleObjects(threads, hDone, TRUE, INFINITE);
	for (i = 0; i<threads; i++){
		CloseHandle(hDone[i]);
#ifdef KF_THREADED_REFERENCE_EVENT
		CloseHandle(hWait[i]);
		CloseHandle(hExit[i]);
#endif
	}

#ifdef KF_THREADED_REFERENCE_BARRIER
	mpf_set(z.m_r.m_dec.backend().data(), m.zr);
	mpf_set(z.m_i.m_dec.backend().data(), m.zi);
	mpf_set(dc.m_r.m_dec.backend().data(), m.dcr);
	mpf_set(dc.m_i.m_dec.backend().data(), m.dci);
	mpf_clear(m.zr);
	mpf_clear(m.zi);
	mpf_clear(m.zrn);
	mpf_clear(m.zin);
	mpf_clear(m.zr2);
	mpf_clear(m.zi2);
	mpf_clear(m.cr);
	mpf_clear(m.ci);
	mpf_clear(m.dcr);
	mpf_clear(m.dci);
	mpf_clear(m.dcrn);
	mpf_clear(m.dcin);
	mpf_clear(m.dcrzr);
	mpf_clear(m.dcrzi);
	mpf_clear(m.dcizr);
	mpf_clear(m.dcizi);
#endif

  SetDlgItemText(hWnd,IDC_EDIT4,"");
  flyttyp ad = 1/cabs2(dc);
  if (ad < epsilon2) {
    *c_out = c_guess;
    return 0;
  }
  if(dc.m_r==0 && dc.m_i==0)
	  return -1;
  complex<flyttyp> c_new = c_guess - z / dc;
  complex<flyttyp> d = c_new - c_guess;
  ad = cabs2(d);

  *g_szProgress=0;
  std::string szAD = ad.ToText();
  const char *szE1 = strstr(szAD.c_str(),"e");
  if(!szE1)
	  szE1 = strstr(szAD.c_str(),"E");
  if(szE1){
	  szE1++;
	  if(*szE1=='-')
		  szE1++;
	  strcat(g_szProgress,szE1);
  }
  std::string szEP = epsilon2.ToText();
  szE1 = strstr(szEP.c_str(),"e");
  if(!szE1)
	  szE1 = strstr(szEP.c_str(),"E");
  if(szE1){
	  szE1++;
	  if(*szE1=='-')
		  szE1++;
	  strcat(g_szProgress,"/");
	  strcat(g_szProgress,szE1);
  }

  if (ad < epsilon2) {
    *c_out = c_new;
    return 0;
  }
  if (cisfinite(d)) {
    *c_out = c_new;
    return 1;
  } else {
    *c_out = c_guess;
    return -1;
  }
}

extern int m_d_nucleus(complex<flyttyp> *c_out, complex<flyttyp> c_guess, int period, int maxsteps,int &steps,flyttyp radius,HWND hWnd) {
  int result = -1, i;
  complex<flyttyp> c = c_guess;

  flyttyp epsilon2 = flyttyp(1)/(radius*radius*radius);
  for (i = 0; i < maxsteps && !g_bNewtonStop && !g_bNewtonExit; ++i) {
    if (1 != (result = m_d_nucleus_step(&c, c, period,epsilon2,hWnd,i)))
      break;
  }
  steps = i;
  *c_out = c;
  if(g_bNewtonExit)
	  result=0;
  return result;
}

static inline complex<floatexp> fec(const complex<flyttyp> &z)
{
  return complex<floatexp>(mpf_get_fe(z.m_r.m_dec.backend().data()), mpf_get_fe(z.m_i.m_dec.backend().data()));
}

complex<floatexp> m_d_size(const complex<flyttyp> &nucleus, int period,HWND hWnd)
{
  complex<floatexp> fec1(1,0);
  complex<floatexp> fec2(2,0);
  complex<floatexp> l(1,0);
  complex<floatexp> b(1,0);
  complex<flyttyp> z(0,0);
  char szStatus[256];
  uint32_t last = GetTickCount();
  for (int i = 1; i < period && !g_bNewtonStop; ++i) {
	  if(i%100==0){
		uint32_t now = GetTickCount();
		if (now - last > 250)
		{
		  wsprintf(szStatus,"Determine size %d%%...",100*i/period);
		  SetDlgItemText(hWnd,IDC_EDIT1,szStatus);
		  last = now;
		}
	  }
    z = z * z + nucleus;
    complex<floatexp> zlo = fec(z);
    l = fec2 * zlo * l;
    b = b + fec1 / l;
  }  
  return fec1 / (b * l * l);
}

int g_period;
int WINAPI ThNewton(HWND hWnd)
{
	char szStatus[300];

	flyttyp radius = g_szZoom;
	radius*=g_nZoomSize;
	char *e = strstr(g_szZoom,"E");
	if(!e)
		e = strstr(g_szZoom,"e");
	int exp = (e?atoi(e+1):0);
	unsigned uprec = exp + 6;
	Precision prec(uprec);
	complex<flyttyp> center(g_szRe,g_szIm);

	char szVal[25];
	int i;
	for(i=0;i<24 && g_szZoom[i] && g_szZoom[i]!='e' && g_szZoom[i]!='E' && g_szZoom[i]!='+' && g_szZoom[i]!='-';i++)
		szVal[i]=g_szZoom[i];
	szVal[i]=0;
	e = strstr(g_szZoom,"E");
	if(!e)
		e = strstr(g_szZoom,"e");
	int startZooms = (e?atof(e+1)/0.30103:0) + log10(atof(szVal));

	int steps;
	if(SendDlgItemMessage(hWnd,IDC_CHECK1,BM_GETCHECK,0,0)){
		g_period = GetDlgItemInt(hWnd,IDC_EDIT3,NULL,0);
		uprec *= 3;
	}
	else{
		g_period= m_d_box_period_do(center,radius,100000000,steps,hWnd);
		uprec *= 2;
	}
	Precision prec2(uprec);

	SetDlgItemInt(hWnd,IDC_EDIT3,g_period,0);
	BOOL bOK=FALSE;
	if(g_period){
		sprintf(szStatus,"period=%d (steps:%d)\n",g_period,steps);
		SetDlgItemText(hWnd,IDC_EDIT1,szStatus);
		complex<flyttyp> c;
		int test = m_d_nucleus(&c,center,g_period,100,steps,radius,hWnd);

		if(test==0 && steps){
			delete[] g_szRe;
			std::string sz = c.m_r.ToText();
			g_szRe = new char[strlen(sz.c_str())+1];
			strcpy(g_szRe,sz.c_str());

			delete[] g_szIm;
			sz = c.m_i.ToText();
			g_szIm = new char[strlen(sz.c_str())+1];
			strcpy(g_szIm,sz.c_str());

			Precision prec3(exp + 6);
			complex<floatexp>size = m_d_size(c,g_period,hWnd);
			floatexp msizefe = floatexp(.25)/sqrt(cabs2(size));
			flyttyp msize = 0;
			mpf_set_fe(msize.m_dec.backend().data(), msizefe);

			std::string sszSize = msize.ToText();
			char *szSize0 = strdup(sszSize.c_str());
			char *szSize = szSize0;
			char szTmpSize[200];
			double zooms;
			if(!strstr(szSize,"e") && !strstr(szSize,"E")){
				char *szP = strstr(szSize, ".");
				if (szP)
					*szP = 0;
				int exp = strlen(szSize) - 1;
				if (exp>2){
					int end = 12;
					if (end>exp)
						end = exp;
					szSize[end + 1] = 0;
					while (end>1){
						szSize[end] = szSize[end - 1];
						end--;
					}
					szSize[end] = '.';
					strcpy(szTmpSize,szSize);
					char szNum[20];
					sprintf(szNum, "E%d", exp);
					strcat(szTmpSize, szNum);
					szSize = szTmpSize;
				}
			}
			if(strstr(szSize,"e") || strstr(szSize,"E")){
				int i;
				for(i=0;i<24 && szSize[i] && szSize[i]!='e' && szSize[i]!='E' && szSize[i]!='+' && szSize[i]!='-';i++)
					szVal[i]=szSize[i];
				szVal[i]=0;
				e = strstr(szSize,"E");
				if(!e)
					e = strstr(szSize,"e");
				zooms = (e?atof(e+1)/0.30103:0) + log10(atof(szVal));
			}
			else
				zooms = log10(atof(szSize))/0.30103;
			std::string sradius;
			if(g_nMinibrotPos){
				if(g_nMinibrotPos==1)
					zooms = 3*zooms/4;
				else if(g_nMinibrotPos==2)
					zooms = 7*zooms/8;
				else if(g_nMinibrotPos==3)
					zooms = 15*zooms/16;
				radius = flyttyp(2)^zooms;
				sradius = radius.ToText();
				szSize = strdup(sradius.c_str());
			}
			delete[] g_szZoom;
			g_szZoom = new char[strlen(szSize)+1];
			strcpy(g_szZoom,szSize);
			free(szSize0);
			if(g_nMinibrotPos)
			  free(szSize);
			if(zooms>startZooms)
				bOK=TRUE;
		}
	}
	g_bNewtonRunning=FALSE;
	PostMessage(hWnd,WM_USER+2,0,bOK);
	return 0;
}
__int64 t1, t2;
SYSTEMTIME st1, st2;
int WINAPI NewtonProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	if(uMsg==WM_INITDIALOG){
		if(g_nMinibrotPos==1)
			SendDlgItemMessage(hWnd,IDC_RADIO2,BM_SETCHECK,1,0);
		else if(g_nMinibrotPos==2)
			SendDlgItemMessage(hWnd,IDC_RADIO3,BM_SETCHECK,1,0);
		else if(g_nMinibrotPos==3)
			SendDlgItemMessage(hWnd,IDC_RADIO4,BM_SETCHECK,1,0);
		else
			SendDlgItemMessage(hWnd,IDC_RADIO1,BM_SETCHECK,1,0);
		return 1;
	}
	if(uMsg==WM_COMMAND && wParam==IDCANCEL){
		if(g_bNewtonRunning){
			g_bNewtonStop=TRUE;
			while(g_bNewtonRunning){
				MSG msg;
				while(PeekMessage(&msg,NULL,0,0,PM_REMOVE)){
					TranslateMessage(&msg);
					DispatchMessage(&msg);
				}
				Sleep(1);
			}
			SetDlgItemText(hWnd,IDCANCEL,"Close");
		}
		else{
			HWND hMain = GetParent(hWnd);
			PostMessage(hMain,WM_COMMAND,ID_SPECIAL_NEWTON,0);
			DestroyWindow(hWnd);
		}
	}
	if(uMsg==WM_COMMAND && wParam==IDCANCEL2){
		if(g_bNewtonRunning){
			g_bNewtonExit=TRUE;
			EnableWindow(GetDlgItem(hWnd,IDCANCEL2),FALSE);
		}
		else
			MessageBeep((UINT)-1);
	}
	else if(uMsg==WM_USER+1){
		if(!g_bNewtonRunning){
			RECT r = *(RECT*)lParam;
			char *sz = g_SFT.GetRe(r.left,r.top,r.right,r.bottom);
			if(g_szRe)
				delete[] g_szRe;
			g_szRe = new char[strlen(sz)+1];
			strcpy(g_szRe,sz);
			sz = g_SFT.GetIm(r.left,r.top,r.right,r.bottom);
			if(g_szIm)
				delete[] g_szIm;
			g_szIm = new char[strlen(sz)+1];
			strcpy(g_szIm,sz);
			sz = g_SFT.GetZoom();
			if(g_szZoom)
				delete[] g_szZoom;
			g_szZoom = new char[strlen(sz)+1];
			strcpy(g_szZoom,sz);
			g_nMinibrotPos=0;
			if(SendDlgItemMessage(hWnd,IDC_RADIO2,BM_GETCHECK,0,0))
				g_nMinibrotPos=1;
			else if(SendDlgItemMessage(hWnd,IDC_RADIO3,BM_GETCHECK,0,0))
				g_nMinibrotPos=2;
			else if(SendDlgItemMessage(hWnd,IDC_RADIO4,BM_GETCHECK,0,0))
				g_nMinibrotPos=3;
			DWORD dw;
			g_bNewtonStop=FALSE;
			g_bNewtonExit=FALSE;
			*g_szProgress=0;
			GetLocalTime(&st1);
			HANDLE hThread = CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)ThNewton,hWnd,0,&dw);
			CloseHandle(hThread);
			g_bNewtonRunning=TRUE;
			SetDlgItemText(hWnd,IDCANCEL,"Stop");
		}
	}
	else if(uMsg==WM_USER+2){
		SetDlgItemText(hWnd,IDCANCEL,"Close");
		g_bNewtonRunning=FALSE;
		if(lParam){
			g_SFT.SetPosition(g_szRe,g_szIm,g_szZoom);
			g_SFT.SetIterations(3*g_SFT.GetIterations()/2);
			PostMessage(GetParent(hWnd),WM_KEYDOWN,VK_F5,0);
			if(SendDlgItemMessage(hWnd,IDC_CHECK9,BM_GETCHECK,0,0)){
				GetLocalTime(&st2);
				SystemTimeToFileTime(&st1,(LPFILETIME)&t1);
				SystemTimeToFileTime(&st2,(LPFILETIME)&t2);
				__int64 nT2 = t2-t1;
				FileTimeToSystemTime((LPFILETIME)&nT2,&st1);
				char szResult[256];
				wsprintf(szResult,"Time: %02d:%02d:%02d.%03d.%05d\nPeriod: %d",st1.wHour,st1.wMinute,st1.wSecond,st1.wMilliseconds,(int)(nT2%10000),g_period);
				MessageBox(hWnd,szResult,"Result",MB_OK);
			}
		}
		PostMessage(GetParent(hWnd),WM_COMMAND,ID_SPECIAL_NEWTON,0);
		if(!lParam && !g_bNewtonStop)
			MessageBox(GetParent(hWnd),"Could not apply Newton-Raphson\nYou may zoom in a little and try again","Error",MB_OK|MB_ICONSTOP);
	}
	return 0;
}