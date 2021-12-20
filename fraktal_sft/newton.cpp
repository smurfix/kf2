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

#include <windows.h>
#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include <time.h>
#include <float.h>
#include "resource.h"
#include "CDecNumber.h"
#include "complex.h"
#include "fraktal_sft.h"
#include "main.h"
#include "../common/barrier.h"
#include "../common/StringVector.h"
#include "../common/timer.h"
#include "newton.h"
#include "hybrid.h"
#include "tooltip.h"
#include <string>
#include <fstream>

#ifdef KF_EMBED

#include <thread>

#else

#ifndef THREAD_MODE_BACKGROUND_BEGIN
#define THREAD_MODE_BACKGROUND_BEGIN PROCESS_MODE_BACKGROUND_BEGIN
#define THREAD_MODE_BACKGROUND_END PROCESS_MODE_BACKGROUND_END
#endif
#endif

#define KF_MAIN 1
#include "../formula/generated/formula.h"
#undef abs
#undef sgn

const struct formula *get_formula(int type, int power)
{
  std::string name = "formula_" + std::to_string(type) + "_" + std::to_string(power);
  const struct formula *f = (const struct formula *) GetProcAddress(GetModuleHandle(nullptr), name.c_str());
  if (f) return f;
  name = "formula_" + std::to_string(type);
  f = (const struct formula *) GetProcAddress(GetModuleHandle(nullptr), name.c_str());
  return f;
}

extern CFraktalSFT g_SFT;
extern HICON g_hIcon;

static volatile int running = 0;
bool g_bNewtonRunning = false;
bool g_bNewtonStop = false;
static bool g_bNewtonExit = false;
bool g_bJustDidNewton = false;

static int g_nr_zoom_target = 0;
static int g_nr_folding = 1;
static double g_nr_folding_custom = 1;
static int g_nr_size_power = 3;
static double g_nr_size_power_custom = 1;
static int g_nr_size_factor = 1;
static double g_nr_size_factor_custom = 1;
static int g_nr_action = 2;
static bool g_nr_ball_method = true;

static floatexp g_fNewtonDelta2[2];
static int g_nNewtonETA = 0;
static int64_t g_iterations = 0;

// progress reporting threads

static std::string s_period, s_center, s_size, s_skew;
static char g_szProgress[128];

#ifndef KF_EMBED
static DWORD WINAPI ThPeriodProgress(progress_t *progress)
{
	while (progress->running)
	{
		Sleep(250);
		progress->elapsed_time = get_wall_time() - progress->start_time;
		int limit = progress->counters[0];
		int iter = progress->counters[1];
		char status[100];
		snprintf(status, 100, "Period %d (%d%%) (%ds)\n", iter, (int) (iter * 100.0 / limit), (int) progress->elapsed_time);
		s_period = status;
		SetDlgItemText(progress->hWnd, IDC_NR_ZOOM_STATUS, (s_period + s_center + s_size + s_skew).c_str());
	}
	SetEvent(progress->hDone);
	return 0;
}

static DWORD WINAPI ThNewtonProgress(progress_t *progress)
{
	while (progress->running)
	{
		Sleep(250);
		progress->elapsed_time = get_wall_time() - progress->start_time;
		int eta = progress->counters[0];
		int step = progress->counters[1];
		int period = progress->counters[2];
		int iter = progress->counters[3];
		char status[200];
		snprintf(status, sizeof(status), "Center %d/%d (%d%%) (%s) (%ds)\r\n", step, eta >= 0 ? step + eta : 0, (int) (iter * 100.0 / period), g_szProgress, (int) progress->elapsed_time);
		s_center = status;
		SetDlgItemText(progress->hWnd, IDC_NR_ZOOM_STATUS, (s_period + s_center + s_size + s_skew).c_str());
	}
	SetEvent(progress->hDone);
	return 0;
}

static DWORD WINAPI ThSizeProgress(progress_t *progress)
{
	while (progress->running)
	{
		Sleep(250);
		progress->elapsed_time = get_wall_time() - progress->start_time;
		int period = progress->counters[0];
		int iter = progress->counters[1];
		char status[100];
		snprintf(status, sizeof(status), "Size %d%% (%ds)\r\n", (int) (iter * 100.0 / period), (int) progress->elapsed_time);
		s_size = status;
		SetDlgItemText(progress->hWnd, IDC_NR_ZOOM_STATUS, (s_period + s_center + s_size + s_skew).c_str());
	}
	SetEvent(progress->hDone);
	return 0;
}
#endif

static int newtonETA(const floatexp &delta0, const floatexp &delta1, const floatexp &epsilon)
{
  floatexp l0 = log(delta0);
  floatexp l1 = log(delta1);
  floatexp e  = log(epsilon);
  // (e - l0) = 1 (l1 - l0) + 2 (l1 - l0) + 4 (l1 - l0) + ... 2^N (l1 - l0)
  // (e - l0) / (l1 - l0) = sum_0^N 2^i
  floatexp N = log2((e - l0) / (l1 - l0));
  return double(N);
}

static std::string g_szRe;
static std::string g_szIm;
static std::string g_szZoom;
static std::string g_sMinibrotSourceZoom;

#define flyttyp CDecNumber

const complex<flyttyp> _2(2,0);
const complex<flyttyp> _1(1,0);

static inline int sgn(const flyttyp &z) {
  if (z > 0) { return  1; }
  if (z < 0) { return -1; }
  return 0;
}

static inline bool odd(int a) {
  return a & 1;
}

static inline floatexp cabs2(const complex<floatexp> &z) {
  return z.m_r * z.m_r + z.m_i * z.m_i;
}
static inline flyttyp cabs2(const complex<flyttyp> &z) {
  return z.m_r * z.m_r + z.m_i * z.m_i;
}
static inline bool isfinite(const flyttyp &a){
	(void) a;
	return true;
/*	if (a <= DBL_MAX && a >= -DBL_MAX)
		return true;
	return false;*/
}
static inline bool cisfinite(const complex<flyttyp> &z) {
  return isfinite(z.m_r) && isfinite(z.m_i);
}

struct BallPeriodCommon
{
  barrier_t *barrier;
  volatile bool *stop;
  bool *haveperiod;
  int64_t *period;
  int64_t maxperiod;
  mpfr_t cr, ci, zr, zi, zr2, zi2, zri, t;
  floatexp zradius;
  progress_t *progress;
};

struct BallPeriod
{
  int threadid;
#ifdef KF_EMBED
  std::thread hDone;
#else
  HANDLE hDone;
#endif
  BallPeriodCommon *c;
};

static DWORD WINAPI ThBallPeriod(BallPeriod *b)
{
#ifndef KF_EMBED
  SetThreadPriority(GetCurrentThread(), THREAD_MODE_BACKGROUND_BEGIN);
  SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_LOWEST);
#endif
  int t = b->threadid;
  barrier_t *barrier = b->c->barrier;
  volatile bool *stop = b->c->stop;
  bool *haveperiod = b->c->haveperiod;
  int64_t *period = b->c->period;
  progress_t *progress = b->c->progress;
  floatexp r = b->c->zradius;
  complex<floatexp> z(0.0, 0.0);
  complex<floatexp> dz(0.0, 0.0);
  floatexp rz = 0.0;
  floatexp rdz = 0.0;
  floatexp Ei = 0.0;
  floatexp rr = 0.0;
  if (t == 0)
  {
    progress->counters[0] = b->c->maxperiod;
  }
  for (int64_t i = 1; i < b->c->maxperiod; ++i)
  {

    if (t == 0)
    {
      progress->counters[1] = i;
      Ei = rdz * rdz + (2 * rz + r * (2 * rdz + r * Ei)) * Ei;
      dz = 2 * z * dz + complex<floatexp>(1.0, 0.0);
      z = complex<floatexp>(mpfr_get_fe(b->c->zr), mpfr_get_fe(b->c->zi));
      rdz = abs(dz);
      rz = abs(z);
      rr = r * (rdz + r * Ei);
      if (double(rz - rr) > 2) // outside everything
      {
	*haveperiod = true;
	*period = 0;
      }
      if (rz - rr <= 0) // surrounds 0
      {
	*haveperiod = true;
	*period = i;
      }
    }
    switch (t)
    {
      case 0:
      {
        mpfr_sqr(b->c->zr2, b->c->zr, MPFR_RNDN);
        mpfr_sqr(b->c->zi2, b->c->zi, MPFR_RNDN);
	break;
      }
      case 1:
      {
        mpfr_mul(b->c->zri, b->c->zr, b->c->zi, MPFR_RNDN);
	break;
      }
    }
    if (barrier->wait(stop)) break;
    if (*haveperiod) break;
    switch (t)
    {
      case 0:
        mpfr_sub(b->c->t, b->c->zr2, b->c->zi2, MPFR_RNDN);
        mpfr_add(b->c->zr, b->c->t, b->c->cr, MPFR_RNDN);
	break;
      case 1:
        mpfr_mul_2ui(b->c->zri, b->c->zri, 1, MPFR_RNDN);
        mpfr_add(b->c->zi, b->c->zri, b->c->ci, MPFR_RNDN);
	break;
    }
    if (barrier->wait(stop)) break;
  }
  if (t == 0)
  {
    if (*period == 0)
    {
      *haveperiod = false;
    }
  }
#ifndef KF_EMBED
  SetEvent(b->hDone);
#endif
  mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE);
  return 0;
}

static int64_t ball_period_do(const complex<flyttyp> &center, flyttyp radius, int64_t maxperiod,int &steps, progress_t *progress)
{
  radius = flyttyp(4)/radius;
  mp_bitcnt_t bits = mpfr_get_prec(center.m_r.m_dec.backend().data());
  barrier_t bar(2);
  bool haveperiod = false;
  int64_t period = 0;
#ifndef KF_EMBED
  HANDLE hDone[2];
#endif
  // prepare threads
  BallPeriod ball[2];
  BallPeriodCommon c;
  c.progress = progress;
  c.maxperiod = maxperiod;
  c.barrier = &bar;
  c.stop = &g_bNewtonStop;
  c.haveperiod = &haveperiod;
  c.period = &period;
  c.maxperiod = maxperiod;
  mpfr_init2(c.cr, bits); mpfr_set(c.cr, center.m_r.m_dec.backend().data(), MPFR_RNDN);
  mpfr_init2(c.ci, bits); mpfr_set(c.ci, center.m_i.m_dec.backend().data(), MPFR_RNDN);
  mpfr_init2(c.zr, bits); mpfr_set(c.zr, center.m_r.m_dec.backend().data(), MPFR_RNDN);
  mpfr_init2(c.zi, bits); mpfr_set(c.zi, center.m_i.m_dec.backend().data(), MPFR_RNDN);
  mpfr_init2(c.zr2, bits);
  mpfr_init2(c.zi2, bits);
  mpfr_init2(c.zri, bits);
  mpfr_init2(c.t, bits);
  c.zradius = mpfr_get_fe(radius.m_dec.backend().data());
  for (int t = 0; t < 2; ++t)
  {
    ball[t].threadid = t;
#ifndef KF_EMBED
    ball[t].hDone = hDone[t] = CreateEvent(NULL, 0, 0, NULL);
#endif
    ball[t].c = &c;
  }
  // spawn threads
  for (int i = 0; i < 2; i++)
  {
#ifdef KF_EMBED
    ball[i].hDone = std::thread(ThBallPeriod,&ball[i]);
#else
    DWORD dw;
    HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)ThBallPeriod, (LPVOID)&ball[i], 0, &dw);
    CloseHandle(hThread);
#endif
  }
  // wait for threads to complete
#ifndef KF_EMBED
  WaitForMultipleObjects(2, hDone, TRUE, INFINITE);
#endif
  for (int i = 0; i < 2; i++)
  {
#ifdef KF_EMBED
    ball[i].hDone.join();
#else
    CloseHandle(hDone[i]);
#endif
  }
  mpfr_clear(c.cr);
  mpfr_clear(c.ci);
  mpfr_clear(c.zr);
  mpfr_clear(c.zi);
  mpfr_clear(c.zr2);
  mpfr_clear(c.zi2);
  mpfr_clear(c.zri);
  mpfr_clear(c.t);
  steps = period;
  return haveperiod ? period : 0;
}

struct STEP_STRUCT_COMMON
{
	HWND hWnd;
	barrier_t *barrier;
	volatile bool *stop;
	int newtonStep;
	int64_t period;
	mpfr_t zr, zi, zrn, zin, zr2, zi2, cr, ci, dcr, dci, dcrn, dcin, dcrzr, dcrzi, dcizr, dcizi;
	progress_t *progress;
};
struct STEP_STRUCT
{
	int nType;
#ifdef KF_EMBED
    std::thread hDone;
#else
	HANDLE hDone;
#endif
	STEP_STRUCT_COMMON *common;
};
static DWORD WINAPI ThStep(STEP_STRUCT *t0)
{
#ifndef KF_EMBED
  SetThreadPriority(GetCurrentThread(), THREAD_MODE_BACKGROUND_BEGIN);
  SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_LOWEST);
#endif
  struct STEP_STRUCT_COMMON *t = t0->common;
  switch (t0->nType)
  {
    case 0: // zr
      for (int64_t i = 0; i < t->period; ++i)
      {
	mpfr_sqr(t->zr2, t->zr, MPFR_RNDN);
	mpfr_sqr(t->zi2, t->zi, MPFR_RNDN);
	mpfr_sub(t->zrn, t->zr2, t->zi2, MPFR_RNDN);
	if (t->barrier->wait(t->stop)) break;
	mpfr_add(t->zr, t->zrn, t->cr, MPFR_RNDN);
	if (t->barrier->wait(t->stop)) break;
      }
      break;
    case 1: // zi
      for (int64_t i = 0; i < t->period; ++i)
      {
	t->progress->counters[3] = i + 1;
	mpfr_mul(t->zin, t->zr, t->zi, MPFR_RNDN);
	mpfr_mul_2ui(t->zin, t->zin, 1, MPFR_RNDN);
	if (t->barrier->wait(t->stop)) break;
	mpfr_add(t->zi, t->zin, t->ci, MPFR_RNDN);
	if (t->barrier->wait(t->stop)) break;
      }
      break;
    case 2: // dcr
      for (int64_t i = 0; i < t->period; ++i)
      {
	mpfr_mul(t->dcrzr, t->dcr, t->zr, MPFR_RNDN);
	mpfr_mul(t->dcizi, t->dci, t->zi, MPFR_RNDN);
	mpfr_sub(t->dcrn, t->dcrzr, t->dcizi, MPFR_RNDN);
	mpfr_mul_2ui(t->dcrn, t->dcrn, 1, MPFR_RNDN);
	if (t->barrier->wait(t->stop)) break;
	mpfr_add_ui(t->dcr, t->dcrn, 1, MPFR_RNDN);
	if (t->barrier->wait(t->stop)) break;
      }
      break;
    case 3: // dci
      for (int64_t i = 0; i < t->period; ++i)
      {
	mpfr_mul(t->dcrzi, t->dcr, t->zi, MPFR_RNDN);
	mpfr_mul(t->dcizr, t->dci, t->zr, MPFR_RNDN);
	mpfr_add(t->dcin, t->dcrzi, t->dcizr, MPFR_RNDN);
	mpfr_mul_2ui(t->dcin, t->dcin, 1, MPFR_RNDN);
	if (t->barrier->wait(t->stop)) break;
	mpfr_set(t->dci, t->dcin, MPFR_RNDN);
	if (t->barrier->wait(t->stop)) break;
      }
      break;
  }
#ifndef KF_EMBED
  SetEvent(t0->hDone);
#endif
  mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE);
  return 0;
}

extern floatexp m_d_domain_size(const complex<flyttyp> &c, int period, progress_t *progress)
{
  complex<flyttyp> z = c;
  complex<floatexp> dc = complex<floatexp>(1);
  floatexp zq2 = cabs2(complex<floatexp>(floatexp(z.m_r), floatexp(z.m_i)));
  for (int q = 2; q <= period; ++q)
  {
    progress->counters[1] = q;
    dc = 2 * complex<floatexp>(floatexp(z.m_r), floatexp(z.m_i)) * dc + 1;
    z = sqr(z) + c;
    floatexp zp2 = cabs2(complex<floatexp>(floatexp(z.m_r), floatexp(z.m_i)));
    if (q < period && zp2 < zq2)
      zq2 = zp2;
  }
  return sqrt(zq2) / sqrt(cabs2(dc));
}

static int m_d_nucleus_step(complex<flyttyp> *c_out, const complex<flyttyp> &c_guess, const int64_t period,const flyttyp &epsilon2,HWND hWnd,int newtonStep, const flyttyp &radius2, progress_t *progress) {
  complex<flyttyp> z(0,0);
  complex<flyttyp> zr(0,0);
  complex<flyttyp> dc(0,0);
  int i;

	int threads = 4;
	mp_bitcnt_t bits = mpfr_get_prec(c_guess.m_r.m_dec.backend().data());
	barrier_t bar(4);
	STEP_STRUCT_COMMON m;
	m.hWnd = hWnd;
	m.barrier = &bar;
	m.stop = &g_bNewtonStop;
	m.newtonStep = newtonStep;
	m.period = period;
	m.progress = progress;
	mpfr_init2(m.zr, bits); mpfr_set_ui(m.zr, 0, MPFR_RNDN);
	mpfr_init2(m.zi, bits); mpfr_set_ui(m.zi, 0, MPFR_RNDN);
	mpfr_init2(m.zrn, bits);
	mpfr_init2(m.zin, bits);
	mpfr_init2(m.zr2, bits);
	mpfr_init2(m.zi2, bits);
	mpfr_init2(m.cr, bits); mpfr_set(m.cr, c_guess.m_r.m_dec.backend().data(), MPFR_RNDN);
	mpfr_init2(m.ci, bits); mpfr_set(m.ci, c_guess.m_i.m_dec.backend().data(), MPFR_RNDN);
	mpfr_init2(m.dcr, bits); mpfr_set_ui(m.dcr, 0, MPFR_RNDN);
	mpfr_init2(m.dci, bits); mpfr_set_ui(m.dci, 0, MPFR_RNDN);
	mpfr_init2(m.dcrn, bits);
	mpfr_init2(m.dcin, bits);
	mpfr_init2(m.dcrzr, bits);
	mpfr_init2(m.dcrzi, bits);
	mpfr_init2(m.dcizr, bits);
	mpfr_init2(m.dcizi, bits);
	STEP_STRUCT mc[4];
#ifndef KF_EMBED
	HANDLE hDone[4];
#endif
	for (i = 0; i<4; i++){
		mc[i].nType =i;
#ifndef KF_EMBED
		hDone[i] = mc[i].hDone = CreateEvent(NULL, 0, 0, NULL);
#endif
		mc[i].common = &m;
	}

#ifndef KF_EMBED
	HANDLE hThread;
	DWORD dw;
#endif
	for (i = 0; i<threads; i++){
#ifdef KF_EMBED
		mc[i].hDone = std::thread(ThStep, &mc[i]);
#else
		hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)ThStep, (LPVOID)&mc[i], 0, &dw);
		CloseHandle(hThread);
#endif
	}

#ifndef KF_EMBED
	WaitForMultipleObjects(threads, hDone, TRUE, INFINITE);
#endif
	for (i = 0; i<threads; i++){
#ifdef KF_EMBED
		mc[i].hDone.join();
#else
		CloseHandle(hDone[i]);
#endif
	}

	mpfr_set(z.m_r.m_dec.backend().data(), m.zr, MPFR_RNDN);
	mpfr_set(z.m_i.m_dec.backend().data(), m.zi, MPFR_RNDN);
	mpfr_set(dc.m_r.m_dec.backend().data(), m.dcr, MPFR_RNDN);
	mpfr_set(dc.m_i.m_dec.backend().data(), m.dci, MPFR_RNDN);
	mpfr_clear(m.zr);
	mpfr_clear(m.zi);
	mpfr_clear(m.zrn);
	mpfr_clear(m.zin);
	mpfr_clear(m.zr2);
	mpfr_clear(m.zi2);
	mpfr_clear(m.cr);
	mpfr_clear(m.ci);
	mpfr_clear(m.dcr);
	mpfr_clear(m.dci);
	mpfr_clear(m.dcrn);
	mpfr_clear(m.dcin);
	mpfr_clear(m.dcrzr);
	mpfr_clear(m.dcrzi);
	mpfr_clear(m.dcizr);
	mpfr_clear(m.dcizi);
#ifndef KF_EMBED
  SetDlgItemText(hWnd,IDC_EDIT4,"");
#endif
  flyttyp ad;
#if 0
  = 1/cabs2(dc);
  if (ad < epsilon2) {
    *c_out = c_guess;
    return 0;
  }
#endif
  if(dc.m_r==0 && dc.m_i==0)
	  return -1;
  complex<flyttyp> c_new = c_guess - z / dc;
  complex<flyttyp> d = c_new - c_guess;
  ad = cabs2(d);

  // progress reporting
  floatexp delta = floatexp(CFixedFloat(ad.m_dec));
  floatexp epsilon = floatexp(CFixedFloat(epsilon2.m_dec));
  std::string elast = sqrt(floatexp(4.0)/delta).toString(1);
  std::string etarget = sqrt(floatexp(4.0)/epsilon).toString(1);
  snprintf(g_szProgress, sizeof(g_szProgress) - 1, "%s %s %s", elast.c_str(), ad < epsilon2 ? ">" : "<", etarget.c_str());
  g_fNewtonDelta2[0] = g_fNewtonDelta2[1];
  g_fNewtonDelta2[1] = delta;
  if (g_fNewtonDelta2[0].val > 0)
    g_nNewtonETA = newtonETA(g_fNewtonDelta2[0], g_fNewtonDelta2[1], epsilon);


  if (ad < epsilon2) {
    *c_out = c_new;
    return 0;
  }
  if (cisfinite(d) && cabs2(c_new) < 16 && ad < radius2) {
    *c_out = c_new;
    return 1;
  } else {
    *c_out = c_guess;
    return -1;
  }
}

bool SaveNewtonBackup(const std::string &szFile, const std::string &re, const std::string &im, const std::string &zoom, int64_t period)
{
	if (! g_SFT.GetSaveNewtonProgress())
		return true;
#if 1
	bool overwrite = true; // FIXME
#endif
	CStringTable stSave;
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Re");
	stSave.AddString(stSave.GetCount() - 1, re);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Im");
	stSave.AddString(stSave.GetCount() - 1, im);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Zoom");
	stSave.AddString(stSave.GetCount() - 1, zoom);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Period");
	stSave.AddInt   (stSave.GetCount() - 1, period);
	char *szData = stSave.ToText(": ", "\n");
	std::ofstream hFile(szFile);
	if(!hFile )
	{
		stSave.DeleteToText(szData);
		return false;
	}
	hFile.write(szData, strlen(szData));
	hFile.close();
	stSave.DeleteToText(szData);
	return true;
}

bool SaveNewtonBackup(const complex<flyttyp> &c_new, const complex<flyttyp> &c_old, int64_t period, int step)
{
  complex<flyttyp> delta = c_new - c_old;
  complex<floatexp> delta_lo = complex<floatexp>(floatexp(delta.m_r), floatexp(delta.m_i));
  std::string re = c_new.m_r.ToText();
  std::string im = c_new.m_i.ToText();
  std::string zoom = sqrt(floatexp(4.0) / cabs2(delta_lo)).toString();
  char extension[100];
  snprintf(extension, sizeof(extension) - 1, "newton-%04d.kfr", step);
  return SaveNewtonBackup(g_szFile == "" ? extension : replace_path_extension(g_szFile, extension), re, im, zoom, period);
}

static int m_d_nucleus(complex<flyttyp> *c_out, const complex<flyttyp> &c_guess, int64_t period, int maxsteps,int &steps,const flyttyp &radius,HWND hWnd, progress_t *progress) {
  int result = -1, i;
  complex<flyttyp> c = c_guess;
  complex<flyttyp> c_new;

  flyttyp epsilon2 = flyttyp(1)/(radius*radius*radius);
  flyttyp radius2 = radius * radius;
  g_nNewtonETA = -1;
  for (i = 0; i < maxsteps && !g_bNewtonStop && !g_bNewtonExit; ++i) {
    progress->counters[0] = g_nNewtonETA;
    progress->counters[1] = i + 1;
    progress->counters[2] = period;
    progress->counters[3] = 0;
    result = m_d_nucleus_step(&c_new, c, period,epsilon2,hWnd,i,radius2, progress);
    SaveNewtonBackup(c_new, c, period, i + 1);
    c = c_new;
    if (result != 1)
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
  return complex<floatexp>(mpfr_get_fe(z.m_r.m_dec.backend().data()), mpfr_get_fe(z.m_i.m_dec.backend().data()));
}

static complex<floatexp> m_d_size(const complex<flyttyp> &nucleus, int64_t period,HWND hWnd)
{
  complex<floatexp> fec1(1,0);
  complex<floatexp> fec2(2,0);
  complex<floatexp> l(1,0);
  complex<floatexp> b(1,0);
  complex<flyttyp> z(0,0);
  char szStatus[256];
  uint32_t last = GetTickCount();
  for (int64_t i = 1; i < period && !g_bNewtonStop; ++i) {
	  if(i%100==0){
		uint32_t now = GetTickCount();
		if (now - last > 250)
		{
		  wsprintf(szStatus,"Determine size %" PRId64 "%%...",100*i/period);
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

static double g_skew[4];
int64_t g_period = 0;

#ifdef KF_EMBED
void ThNewton(PAR_SFT HWND hWnd)
#else
static int WINAPI ThNewton(HWND hWnd)
#endif
{
#ifndef KF_EMBED
  SetThreadPriority(GetCurrentThread(), THREAD_MODE_BACKGROUND_BEGIN);
  SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_LOWEST);
#endif
  const int type = g_SFT.GetFractalType();
  const int power = g_SFT.GetPower();
  const struct formula *f = get_formula(type, power);
  g_skew[0] = 1;
  g_skew[1] = 0;
  g_skew[2] = 0;
  g_skew[3] = 1;

	flyttyp radius = g_SFT.GetZoom();
	radius*=g_SFT.GetZoomSize();
	const char *e = strstr(g_szZoom.c_str(),"E");
	if(!e)
		e = strstr(g_szZoom.c_str(),"e");
	int expo = (e?atoi(e+1):0);
	unsigned uprec = expo + 6;
	Precision prec(uprec);
	complex<flyttyp> center(g_szRe,g_szIm);

	int steps = 0;
	{
	  // fork progress updater
	  progress_t progress = { { 0, 0, 0, 0 }, true,
#ifndef KF_EMBED
		hWnd, CreateEvent(NULL, 0, 0, NULL),
#endif
		get_wall_time(), 0 };
#ifndef KF_EMBED
	  HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE) ThPeriodProgress, (LPVOID) &progress, 0, NULL);
	  CloseHandle(hThread);
#endif
	  if (g_SFT.GetUseHybridFormula())
	  {
		  flyttyp r = flyttyp(4) / radius;
		  g_period = hybrid_period(g_SFT.GetHybridFormula(), INT_MAX, center.m_r, center.m_i, r, &g_skew[0], &running, &progress.counters[0]);
	  }
	  else if (type == 0 && power == 2)
	  {
		int64_t maxperiod = INT_MAX; // FIXME
		g_period = ball_period_do(center,radius,maxperiod,steps,&progress);
	  }
	  else
	  {
		if (f)
		{
		  flyttyp r = flyttyp(4) / radius;
		  if (g_nr_ball_method)
		  {
		    g_period = f->period_jsk(INT_MAX, 1e50, g_FactorAR, g_FactorAI, center.m_r.m_dec.backend().data(), center.m_i.m_dec.backend().data(), r.m_dec.backend().data(), &g_skew[0], &running, &progress.counters[0]);
		  }
		  else
		  {
		    g_period = f->period_tri(INT_MAX, 1e50, g_FactorAR, g_FactorAI, center.m_r.m_dec.backend().data(), center.m_i.m_dec.backend().data(), r.m_dec.backend().data(), &running, &progress.counters[0]);
		  }
		  if (g_period < 0) g_period = 0;
		}
		else
		{
		  g_period = 0;
		}
	  }
	  // join progress updater
	  progress.running = false;
#ifndef KF_EMBED
	  WaitForMultipleObjects(1, &progress.hDone, TRUE, INFINITE);
	  CloseHandle(progress.hDone);
#endif
	  if (g_period < 0) g_period = 0;
	  uprec *= 2;
	  progress.elapsed_time = get_wall_time() - progress.start_time;
	  char status[100];
	  snprintf(status, 100, "Period %d (%d%%) (%ds)\r\n", (int) g_period, (int) (g_period * 100.0 / INT_MAX), (int) progress.elapsed_time);
	  s_period = status;
	  SetDlgItemText(hWnd, IDC_NR_ZOOM_STATUS, (s_period + s_center + s_size + s_skew).c_str());
	}
	Precision prec2(uprec);

	int bOK = 1;
	if (g_period > 0 && ! g_bNewtonStop && g_nr_action >= 1)
	{
		// fork progress updater
	    progress_t progress = { { 0, 0, 0, 0 }, true,
#ifndef KF_EMBED
		  hWnd, CreateEvent(NULL, 0, 0, NULL),
#endif
		  get_wall_time(), 0 };
#ifndef KF_EMBED
		HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE) ThNewtonProgress, (LPVOID) &progress, 0, NULL);
		CloseHandle(hThread);
#endif
		complex<flyttyp> c;
		int test = 1;
		if (g_SFT.GetUseHybridFormula())
		{
		    // c = center; // seems to copy precision too, so do it low-level...
		    mpfr_set(c.m_r.m_dec.backend().data(), center.m_r.m_dec.backend().data(), MPFR_RNDN);
		    mpfr_set(c.m_i.m_dec.backend().data(), center.m_i.m_dec.backend().data(), MPFR_RNDN);
		    flyttyp epsilon2 = flyttyp(1)/(radius*radius*radius);
		    test = hybrid_newton(g_SFT.GetHybridFormula(), 100, g_period, c.m_r, c.m_i, epsilon2, &running, &progress.counters[0]) ? 0 : 1;
		    flyttyp r = flyttyp(4) / radius;
		    if (! (cabs2(c - center) < r * r))
		      test = 1;
		    steps = 1;
		}
		else if (type == 0 && power == 2)
		{
		  int maxsteps = INT_MAX; // FIXME
		  test = m_d_nucleus(&c,center,g_period,maxsteps,steps,radius,hWnd,&progress);
		}
		else
		{
		  if (f)
		  {
		    // c = center; // seems to copy precision too, so do it low-level...
		    mpfr_set(c.m_r.m_dec.backend().data(), center.m_r.m_dec.backend().data(), MPFR_RNDN);
		    mpfr_set(c.m_i.m_dec.backend().data(), center.m_i.m_dec.backend().data(), MPFR_RNDN);
		    flyttyp epsilon2 = flyttyp(1)/(radius*radius*radius);
		    test = f->newton(100, g_period, g_FactorAR, g_FactorAI, c.m_r.m_dec.backend().data(), c.m_i.m_dec.backend().data(), epsilon2.m_dec.backend().data(), &running, &progress.counters[0]) ? 0 : 1;
		    flyttyp r = flyttyp(4) / radius;
		    if (! (cabs2(c - center) < r * r))
		      test = 1;
		    steps = 1;
		  }
		}
		// join progress updater
		progress.running = false;
#ifndef KF_EMBED
		WaitForMultipleObjects(1, &progress.hDone, TRUE, INFINITE);
		CloseHandle(progress.hDone);
#endif
		{
			progress.elapsed_time = get_wall_time() - progress.start_time;
			int eta = progress.counters[0];
			int step = progress.counters[1];
			int period = progress.counters[2];
			int iter = progress.counters[3];
			char status[100];
			snprintf(status, 100, "Center %d/%d (%d%%) (%ds)\r\n", step, eta >= 0 ? step + eta : 0, (int) (iter * 100.0 / period), (int) progress.elapsed_time);
			s_center = status;
			SetDlgItemText(hWnd, IDC_NR_ZOOM_STATUS, (s_period + s_center + s_size + s_skew).c_str());
		}

		if (! g_bNewtonStop && ! test)
		{
			if (g_nr_action >= 2)
			{
				g_szRe = c.m_r.ToText();
				g_szIm = c.m_i.ToText();

				// fork progress updater
				progress_t progress = { { int(g_period), 0, 0, 0 }, true,
#ifndef KF_EMBED
					hWnd, CreateEvent(NULL, 0, 0, NULL),
#endif
					get_wall_time(), 0 };
#ifndef KF_EMBED
				HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE) ThSizeProgress, (LPVOID) &progress, 0, NULL);
				CloseHandle(hThread);
#endif
				Precision prec3(expo + 6);
				flyttyp msize = 0;
				if (g_period <= 1)
				{
				  msize = 1;
				}
				else
				if (g_SFT.GetUseHybridFormula())
				{
					if (g_nr_zoom_target <= 1)
					{
						hybrid_size(g_SFT.GetHybridFormula(), g_period, c.m_r, c.m_i, msize, &g_skew[0], &running, &progress.counters[0]);
					}
					else
					{
						hybrid_domain_size(g_SFT.GetHybridFormula(), g_period, c.m_r, c.m_i, msize, &running, &progress.counters[0]);
					}
				}
				else if (type == 0 && power == 2)
				{
					if (g_nr_zoom_target <= 1)
					{
						complex<floatexp> size = m_d_size(c,g_period,hWnd);
						floatexp msizefe = sqrt(cabs2(size));
						mpfr_set_fe(msize.m_dec.backend().data(), msizefe);
					}
					else
					{
						complex<floatexp> size = m_d_domain_size(c,g_period,&progress);
						floatexp msizefe = sqrt(cabs2(size));
						mpfr_set_fe(msize.m_dec.backend().data(), msizefe);
					}
				}
				else
				{
					if (f)
					{
						if (g_nr_zoom_target <= 1)
						{
							f->size(g_period, g_FactorAR, g_FactorAI, c.m_r.m_dec.backend().data(), c.m_i.m_dec.backend().data(), msize.m_dec.backend().data(), &g_skew[0], &running, &progress.counters[0]);
						}
						else
						{
							// f->domain_size(g_period, g_FactorAR, g_FactorAI, c.m_r.m_dec.backend().data(), c.m_i.m_dec.backend().data(), msize.m_dec.backend().data(), &g_skew[0], &running, &progress.counters[0]); // FIXME implement this in et formula generator
						}
					}
				}
				// join progress updater
				progress.running = false;
#ifndef KF_EMBED
				WaitForMultipleObjects(1, &progress.hDone, TRUE, INFINITE);
				CloseHandle(progress.hDone);
#endif
				{
					progress.elapsed_time = get_wall_time() - progress.start_time;
					int period = progress.counters[0];
					int iter = progress.counters[1];
					char status[100];
					snprintf(status, 100, "Size %d%% (%ds)\r\n", (int) (iter * 100.0 / period), (int) progress.elapsed_time);
					s_size = status;
					SetDlgItemText(hWnd, IDC_NR_ZOOM_STATUS, (s_period + s_center + s_size + s_skew).c_str());
				}
				if (0 < msize && msize < 2)
				{
					double size_factors[] = { g_nr_size_factor_custom, 10, 4, 1, 0.25, 0.1 };
					double factor = size_factors[std::min(std::max(g_nr_size_factor, 0), 5)];
					if (g_nr_zoom_target >= 1)
					{
						double size_powers[] = { g_nr_size_power_custom, 0.75, 0.875, 1, 1.125, 1.25 };
						double power = size_powers[std::min(std::max(g_nr_size_power, 0), 5)];
						floatexp s = floatexp(msize);
						floatexp r = factor * exp(log(s) * power);
						g_szZoom = (floatexp(2) / r).toString();
						g_iterations = (g_nr_zoom_target >= 2 ? 10 : 100) * g_period; // FIXME
						bOK = 1;
					}
					else
					{
						double foldings[] = { g_nr_folding_custom, 0.5, 0.75, 0.875, 0.9375, 1 };
						double power = foldings[std::min(std::max(g_nr_folding, 0), 5)];
						floatexp start = floatexp(flyttyp(g_szZoom));
						floatexp target = floatexp(2 / factor) / floatexp(msize);
						g_szZoom = (exp(log(start) * (1 - power) + power * log(target))).toString();
						double start_iterations = g_SFT.GetIterations();
						double target_iterations = 100 * g_period;
						g_iterations = std::min(std::max(start_iterations * log(g_SFT.GetPower() / (1 - std::min(1.0, power))) / log(g_SFT.GetPower()), start_iterations), target_iterations);
						bOK = 1;
					}
				}
				else
				{
					bOK = -1;
				}
			}
		}
		else
		{
			bOK = -1;
		}
	}
	else
	{
		bOK = -1;
	}
	g_bNewtonRunning=FALSE;
#ifdef KF_EMBED
	(void)bOK;
#else
	PostMessage(hWnd,WM_USER+2,0,bOK);
#endif
	mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE);

#ifndef KF_EMBED
	return 0;
#endif
}

#ifndef KF_EMBED
const struct { const char *name; } action_preset[] =
{ { "Period" }
, { "Center" }
, { "Zoom" }
, { "Skew" }
};
const int naction_presets = sizeof(action_preset) / sizeof(action_preset[0]);

const struct { const char *name; double folding; } folding2_preset[] =
{ { "Custom", 0 }
, { "0.5 (2x)", 0.5 }
, { "0.75 (4x)", 0.75 }
, { "0.875 (8x)", 0.875 }
, { "0.9375 (16x)", 0.9375 }
, { "1.0 (Minibrot)", 1.0 }
};
const int nfolding2_presets = sizeof(folding2_preset) / sizeof(folding2_preset[0]);

const struct { const char *name; double power; } power2_preset[] =
{ { "Custom", 0 }
, { "0.75 (zoomed out)", 0.75 }
, { "0.875", 0.875 }
, { "1.0 (actual size)", 0.875 }
, { "1.125", 1.125 }
, { "1.25 (zoomed in)", 1.25 }
};
const int npower2_presets = sizeof(power2_preset) / sizeof(power2_preset[0]);

const struct { const char *name; double factor; } factor_preset[] =
{ { "Custom", 0 }
, { "10 (zoomed out)", 10 }
, { "4", 4 }
, { "1 (actual size)", 1 }
, { "0.25", 0.25 }
, { "0.1 (zoomed in)", 0.1 }
};
const int nfactor_presets = sizeof(factor_preset) / sizeof(factor_preset[0]);

const struct { const char *name; } target_preset[] =
{ { "Relative (minibrot)" }
, { "Absolute (minibrot)" }
, { "Absolute (atom domain)" }
};
const int ntarget_presets = sizeof(target_preset) / sizeof(target_preset[0]);

static std::vector<HWND> tooltips;

extern void NewtonReadWindows(HWND hWnd)
{
	g_nr_zoom_target = SendDlgItemMessage(hWnd, IDC_NR_ZOOM_TARGET_PRESET, CB_GETCURSEL, 0, 0);
	g_nr_folding = SendDlgItemMessage(hWnd, IDC_NR_ZOOM_RELATIVE_FOLDING_PRESET, CB_GETCURSEL, 0, 0);
	g_nr_size_power = SendDlgItemMessage(hWnd, IDC_NR_ZOOM_ABSOLUTE_POWER_PRESET, CB_GETCURSEL, 0, 0);
	g_nr_size_factor = SendDlgItemMessage(hWnd, IDC_NR_ZOOM_SIZE_FACTOR_PRESET, CB_GETCURSEL, 0, 0);
	{
		char s[256];
		GetDlgItemText(hWnd, IDC_NR_ZOOM_RELATIVE_FOLDING_CUSTOM_EDIT, s, sizeof(s));
		g_nr_folding_custom = atof(s);
		GetDlgItemText(hWnd, IDC_NR_ZOOM_ABSOLUTE_POWER_CUSTOM_EDIT, s, sizeof(s));
		g_nr_size_power_custom = atof(s);
		GetDlgItemText(hWnd, IDC_NR_ZOOM_SIZE_FACTOR_CUSTOM_EDIT, s, sizeof(s));
		g_nr_size_factor_custom = atof(s);
		GetDlgItemText(hWnd, IDC_NR_ZOOM_RELATIVE_START, s, sizeof(s));
		if (std::string(s) == "")
		{
			g_szZoom = g_SFT.GetZoom();
		}
		else
		{
			g_szZoom = s;
		}
	}
	g_nr_action = SendDlgItemMessage(hWnd, IDC_NR_ZOOM_ACTION_PRESET, CB_GETCURSEL, 0, 0);
	g_nr_ball_method = SendDlgItemMessage(hWnd, IDC_NR_ZOOM_BALL_METHOD, BM_GETCHECK, 0, 0);
	g_SFT.SetSaveNewtonProgress(SendDlgItemMessage(hWnd, IDC_NR_ZOOM_SAVE_PROGRESS, BM_GETCHECK, 0, 0));
}

extern void NewtonEnableWindows(HWND hWnd)
{
	// dis/enable optional windows
	EnableWindow(GetDlgItem(hWnd, IDC_NR_ZOOM_RELATIVE_START), g_nr_zoom_target == 0);
	EnableWindow(GetDlgItem(hWnd, IDC_NR_ZOOM_RELATIVE_START_CAPTURE), g_nr_zoom_target == 0);
	EnableWindow(GetDlgItem(hWnd, IDC_NR_ZOOM_RELATIVE_FOLDING_PRESET), g_nr_zoom_target == 0);
	EnableWindow(GetDlgItem(hWnd, IDC_NR_ZOOM_RELATIVE_FOLDING_CUSTOM_EDIT), g_nr_folding == 0 && g_nr_zoom_target == 0);
	EnableWindow(GetDlgItem(hWnd, IDC_NR_ZOOM_ABSOLUTE_POWER_PRESET), g_nr_zoom_target != 0);
	EnableWindow(GetDlgItem(hWnd, IDC_NR_ZOOM_ABSOLUTE_POWER_CUSTOM_EDIT), g_nr_size_power == 0 && g_nr_zoom_target != 0);
	EnableWindow(GetDlgItem(hWnd, IDC_NR_ZOOM_SIZE_FACTOR_CUSTOM_EDIT), g_nr_size_factor == 0);
}

extern int WINAPI NewtonProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	if (uMsg == WM_INITDIALOG)
	{
		SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
		SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));

#define T(idc,str) tooltips.push_back(CreateToolTip(idc, hWnd, str));
T(IDC_NR_ZOOM_TARGET_PRESET                   , "Relative zooming is between current zoom level and the minibrot.\r\nAbsolute zooming is relative to the size of the minibrot or atom domain.")
T(IDC_NR_ZOOM_RELATIVE_START                  , "Override current zoom level for relative zooming.\r\nLeave empty use current zoom level at start time.")
T(IDC_NR_ZOOM_RELATIVE_START_CAPTURE          , "Click to capture current zoom level for relative zooming.")
T(IDC_NR_ZOOM_RELATIVE_FOLDING_PRESET         , "Power for relative doubling/quadrupling/etc the current pattern.")
T(IDC_NR_ZOOM_RELATIVE_FOLDING_CUSTOM_EDIT    , "Enter custom relative power here.\r\nEnter 0.5 for 2x folding.\nTry -1 to zoom out.")
T(IDC_NR_ZOOM_ABSOLUTE_POWER_PRESET           , "Power for absolute double/quadrupling/etc of embedded Julia set features.")
T(IDC_NR_ZOOM_ABSOLUTE_POWER_CUSTOM_EDIT      , "Enter custom absolute power here.\r\nEnter 1 to zoom to target.\nValues greater than 1 are not useful for Minibrot (Absolute).")
T(IDC_NR_ZOOM_SIZE_FACTOR_PRESET              , "Factor for scaling the zoom after power is applied.\r\nBigger than 1 zooms out, smaller than 1 zooms in.")
T(IDC_NR_ZOOM_SIZE_FACTOR_CUSTOM_EDIT         , "Enter custom size factor here.\r\nEnter 1 for actual size.")
T(IDC_NR_ZOOM_BALL_METHOD                     , "When checked, use ball method for finding periods.\r\nOtherwise use box method.\nA different method (Taylor ball) is always used for power 2 Mandelbrot.")
T(IDC_NR_ZOOM_ACTION_PRESET                   , "Stop after action in the sequence:\r\n- Find the period of the lowest period minibrot in the clicked region.\n- Center the view on the minibrot.\n- Zoom to the specified power and size factor.\n- Automatically skew the view.")
T(IDC_NR_ZOOM_SAVE_PROGRESS                   , "When checked, save progress snapshots when finding the center.\r\nResuming is not yet automatic\nNewton zooming from a snapshot will do more iterations than necessary.")
T(IDC_NR_ZOOM_STATUS                          , "Progress messages are displayed here.")
T(IDCANCEL2                                   , "Click to cancel the Newton-Raphson zooming calculations.")
#undef T

		// populate combo boxes
		for (int p = 0; p < naction_presets; ++p)
		{
			SendDlgItemMessage(hWnd, IDC_NR_ZOOM_ACTION_PRESET, CB_ADDSTRING, 0, (LPARAM) action_preset[p].name);
		}
		for (int p = 0; p < nfolding2_presets; ++p)
		{
			SendDlgItemMessage(hWnd, IDC_NR_ZOOM_RELATIVE_FOLDING_PRESET, CB_ADDSTRING, 0, (LPARAM) folding2_preset[p].name);
		}
		for (int p = 0; p < npower2_presets; ++p)
		{
			SendDlgItemMessage(hWnd, IDC_NR_ZOOM_ABSOLUTE_POWER_PRESET, CB_ADDSTRING, 0, (LPARAM) power2_preset[p].name);
		}
		for (int p = 0; p < nfactor_presets; ++p)
		{
			SendDlgItemMessage(hWnd, IDC_NR_ZOOM_SIZE_FACTOR_PRESET, CB_ADDSTRING, 0, (LPARAM) factor_preset[p].name);
		}
		for (int p = 0; p < ntarget_presets; ++p)
		{
			SendDlgItemMessage(hWnd, IDC_NR_ZOOM_TARGET_PRESET, CB_ADDSTRING, 0, (LPARAM) target_preset[p].name);
		}

		// select combo boxes
		SendDlgItemMessage(hWnd, IDC_NR_ZOOM_TARGET_PRESET, CB_SETCURSEL, g_nr_zoom_target, 0);
		SendDlgItemMessage(hWnd, IDC_NR_ZOOM_ACTION_PRESET, CB_SETCURSEL, g_nr_action, 0);
		SendDlgItemMessage(hWnd, IDC_NR_ZOOM_RELATIVE_FOLDING_PRESET, CB_SETCURSEL, g_nr_folding, 0);
		SendDlgItemMessage(hWnd, IDC_NR_ZOOM_ABSOLUTE_POWER_PRESET, CB_SETCURSEL, g_nr_size_power, 0);
		SendDlgItemMessage(hWnd, IDC_NR_ZOOM_SIZE_FACTOR_PRESET, CB_SETCURSEL, g_nr_size_factor, 0);

		// select widgets
		SendDlgItemMessage(hWnd, IDC_NR_ZOOM_BALL_METHOD, BM_SETCHECK, g_nr_ball_method, 0);
		SendDlgItemMessage(hWnd, IDC_NR_ZOOM_SAVE_PROGRESS, BM_SETCHECK, g_SFT.GetSaveNewtonProgress(), 0);

		// clear custom fields
		SetDlgItemText(hWnd, IDC_NR_ZOOM_RELATIVE_START, "");
		SetDlgItemText(hWnd, IDC_NR_ZOOM_RELATIVE_FOLDING_CUSTOM_EDIT, "");
		SetDlgItemText(hWnd, IDC_NR_ZOOM_ABSOLUTE_POWER_CUSTOM_EDIT, "");
		SetDlgItemText(hWnd, IDC_NR_ZOOM_SIZE_FACTOR_CUSTOM_EDIT, "");

		NewtonEnableWindows(hWnd);

		SetDlgItemText(hWnd, IDC_NR_ZOOM_STATUS, "Click the fractal to start.\r\nZoom size affects the\nregion to search.");
		return 1;
	}
	if (uMsg == WM_COMMAND && HIWORD(wParam) == LBN_SELCHANGE && (
	  LOWORD(wParam) == IDC_NR_ZOOM_ABSOLUTE_POWER_PRESET ||
	  LOWORD(wParam) == IDC_NR_ZOOM_RELATIVE_FOLDING_PRESET ||
	  LOWORD(wParam) == IDC_NR_ZOOM_SIZE_FACTOR_PRESET ||
	  LOWORD(wParam) == IDC_NR_ZOOM_TARGET_PRESET))
	{
		NewtonReadWindows(hWnd);
		NewtonEnableWindows(hWnd);
	}
	if (uMsg == WM_COMMAND && wParam == IDC_NR_ZOOM_RELATIVE_START_CAPTURE)
	{
		SetDlgItemText(hWnd, IDC_NR_ZOOM_RELATIVE_START, floatexp(CDecNumber(g_SFT.GetZoom())).toString(5).c_str());
	}
	if (uMsg == WM_COMMAND && (wParam == IDOK || wParam == IDCANCEL || wParam == IDCANCEL2))
	{
		if(g_bNewtonRunning){
			running = 0;
			g_bNewtonStop=TRUE;
			while(g_bNewtonRunning){
				MSG msg;
				while(PeekMessage(&msg,NULL,0,0,PM_REMOVE)){
					TranslateMessage(&msg);
					DispatchMessage(&msg);
				}
				Sleep(1);
			}
		}
		if (wParam == IDCANCEL)
		{
			for (auto tooltip : tooltips)
			{
			  DestroyWindow(tooltip);
			}
			tooltips.clear();
			HWND hMain = GetParent(hWnd);
			PostMessage(hMain,WM_COMMAND,ID_SPECIAL_NEWTON,0);
			DestroyWindow(hWnd);
		}
	}
	if(uMsg==WM_USER+1){
		if(!g_bNewtonRunning){
			mat2 m = g_SFT.GetTransformMatrix();
			g_skew[0] = m[0][0];
			g_skew[1] = m[0][1];
			g_skew[2] = m[1][0];
			g_skew[3] = m[1][1];
			RECT r = *(RECT*)lParam;
			g_szRe = g_SFT.GetRe(r.left,r.top,r.right,r.bottom);
			g_szIm = g_SFT.GetIm(r.left,r.top,r.right,r.bottom);

			NewtonReadWindows(hWnd);

			s_period = "";
			s_center = "";
			s_size = "";
			s_skew = "";
			SetDlgItemText(hWnd, IDC_NR_ZOOM_STATUS, (s_period + s_center + s_size + s_skew).c_str());

			DWORD dw;
			g_bNewtonStop=FALSE;
			g_bNewtonExit=FALSE;
			*g_szProgress=0;
			g_nNewtonETA = -1;
			g_fNewtonDelta2[0] = 0;
			g_fNewtonDelta2[1] = 0;
			running = 1;
			HANDLE hThread = CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)ThNewton,hWnd,0,&dw);
			CloseHandle(hThread);
			g_bNewtonRunning=TRUE;
		}
	}
	if(uMsg==WM_USER+2){
		g_bNewtonRunning=FALSE;
		if(lParam == 1){
			// newton success
			SetDlgItemText(hWnd, IDC_NR_ZOOM_STATUS, (s_period + s_center + s_size + s_skew + "Done").c_str());
			g_SFT.UndoStore();
			g_SFT.Stop();
			if (g_nr_action >= 3)
			{
				g_SFT.SetTransformMatrix(mat2(g_skew[0], g_skew[1], g_skew[2], g_skew[3]));
			}
			if (g_nr_action >= 2)
			{
				g_SFT.SetPosition(g_szRe,g_szIm,g_szZoom);
				if (g_iterations)
				{
					g_SFT.SetIterations(g_iterations);
				}
				char s[256];
				GetDlgItemText(hWnd, IDC_NR_ZOOM_RELATIVE_START, s, sizeof(s));
				if (std::string(s) != "")
				{
					SetDlgItemText(hWnd, IDC_NR_ZOOM_RELATIVE_START, floatexp(CDecNumber(g_szZoom)).toString(5).c_str());
				}
				g_bJustDidNewton = true;
			}
			else if (g_nr_action >= 1)
			{
				g_SFT.SetPosition(g_szRe,g_szIm,g_SFT.GetZoom());
				g_bJustDidNewton = true;
			}
			PostMessage(GetParent(hWnd),WM_KEYDOWN,VK_F5,0);
		}
		if(lParam == -1 && !g_bNewtonStop)
			MessageBox(GetParent(hWnd),"Could not apply Newton-Raphson\r\nYou may zoom in a little and try again","Error",MB_OK|MB_ICONSTOP);
		if((lParam == 0 || lParam < -1 || lParam > 1))
			MessageBox(GetParent(hWnd),"Unexpected stop message parameter (internal error)\r\n","Error",MB_OK|MB_ICONSTOP);
	}
	return 0;
}
#endif
