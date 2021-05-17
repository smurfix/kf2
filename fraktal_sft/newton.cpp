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
#include "complex.h"
#include "fraktal_sft.h"
#include "CDecNumber.h"
#include "main.h"
#include "../common/barrier.h"
#include "../common/StringVector.h"
#include "../common/timer.h"
#include "newton.h"
#include "hybrid.h"
#include "tooltip.h"
#include <iostream>
#include <string>

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
// 0: minibrot old (zooms between current and minibrot)
// 1: minibrot (zooms relative to final size only)
// 2: atom domain (zooms relative to final size)
static int g_nr_folding = 0;
// 0: 0.5
// 1: 0.75
// 2: 0.875
// 3: 0.9375
// 4: 1
// 5: custom
static double g_nr_folding_custom = 1;
static int g_nr_size_power = 2;
// 0: 0.75
// 1: 0.875
// 2: 1
// 3: 1.125
// 4: 1.25
// 5: custom
static double g_nr_size_power_custom = 1;
static int g_nr_size_factor = 0;
// 0: 10
// 1: 4
// 2: 1
// 3: 0.25
// 4: 0.1
// 5: custom
static double g_nr_size_factor_custom = 1;
static int g_nr_action = 2;
// 0: period
// 1: center
// 2: size
// 3: skew
static bool g_nr_ball_method = true;

static floatexp g_fNewtonDelta2[2];
static int g_nNewtonETA = 0;
static int64_t g_iterations = 0;

// progress reporting threads

static std::string s_period, s_center, s_size, s_skew;
static char g_szProgress[128];

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
		SetDlgItemText(progress->hWnd, IDC_NR_STATUS, (s_period + s_center + s_size + s_skew).c_str());
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
		char status[100];
		snprintf(status, 100, "Center %d/%d (%d%%) (%s) (%ds)\n", step, eta >= 0 ? step + eta : 0, (int) (iter * 100.0 / period), g_szProgress, (int) progress->elapsed_time);
		s_center = status;
		SetDlgItemText(progress->hWnd, IDC_NR_STATUS, (s_period + s_center + s_size + s_skew).c_str());
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
		snprintf(status, 100, "Size %d%% (%ds)\n", (int) (iter * 100.0 / period), (int) progress->elapsed_time);
		s_size = status;
		SetDlgItemText(progress->hWnd, IDC_NR_STATUS, (s_period + s_center + s_size + s_skew).c_str());
	}
	SetEvent(progress->hDone);
	return 0;
}

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
  HWND hWnd;
  barrier *barrier;
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
  HANDLE hDone;
  BallPeriodCommon *c;
};

static DWORD WINAPI ThBallPeriod(BallPeriod *b)
{
  SetThreadPriority(GetCurrentThread(), THREAD_MODE_BACKGROUND_BEGIN);
  SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_LOWEST);
  int t = b->threadid;
  barrier *barrier = b->c->barrier;
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
      if (rz - rr > 2) // outside everything
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
  SetEvent(b->hDone);
  mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE);
  return 0;
}

static int64_t ball_period_do(const complex<flyttyp> &center, flyttyp radius, int64_t maxperiod,int &steps,HWND hWnd, progress_t *progress)
{
  radius = flyttyp(4)/radius;
  mp_bitcnt_t bits = mpfr_get_prec(center.m_r.m_dec.backend().data());
  barrier bar(2);
  bool haveperiod = false;
  int64_t period = 0;
  HANDLE hDone[2];
  // prepare threads
  BallPeriod ball[2];
  BallPeriodCommon c;
  c.progress = progress;
  c.maxperiod = maxperiod;
  c.hWnd = hWnd;
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
    ball[t].hDone = hDone[t] = CreateEvent(NULL, 0, 0, NULL);
    ball[t].c = &c;
  }
  // spawn threads
  for (int i = 0; i < 2; i++)
  {
    DWORD dw;
    HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)ThBallPeriod, (LPVOID)&ball[i], 0, &dw);
    CloseHandle(hThread);
  }
  // wait for threads to complete
  WaitForMultipleObjects(2, hDone, TRUE, INFINITE);
  for (int i = 0; i < 2; i++)
  {
    CloseHandle(hDone[i]);
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
	barrier *barrier;
	volatile bool *stop;
	int newtonStep;
	int64_t period;
	mpfr_t zr, zi, zrn, zin, zr2, zi2, cr, ci, dcr, dci, dcrn, dcin, dcrzr, dcrzi, dcizr, dcizi;
	progress_t *progress;
};
struct STEP_STRUCT
{
	int nType;
	HANDLE hDone;
	STEP_STRUCT_COMMON *common;
};
static DWORD WINAPI ThStep(STEP_STRUCT *t0)
{
  SetThreadPriority(GetCurrentThread(), THREAD_MODE_BACKGROUND_BEGIN);
  SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_LOWEST);
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
  SetEvent(t0->hDone);
  mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE);
  return 0;
}

extern floatexp m_d_domain_size(const complex<flyttyp> &c, int period, progress_t *progress)
{
  complex<flyttyp> z = c;
  complex<floatexp> dc = complex<floatexp>(1);
  floatexp zq2 = cabs2(complex<floatexp>(z));
  for (int q = 2; q <= period; ++q)
  {
    progress->counters[1] = q;
    dc = 2 * complex<floatexp>(z) * dc + 1;
    z = sqr(z) + c;
    floatexp zp2 = cabs2(complex<floatexp>(z));
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
	barrier bar(4);
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
	HANDLE hDone[4];
	for (i = 0; i<4; i++){
		mc[i].nType =i;
		hDone[i] = mc[i].hDone = CreateEvent(NULL, 0, 0, NULL);
		mc[i].common = &m;
	}

	HANDLE hThread;
	DWORD dw;
	for (i = 0; i<threads; i++){
		hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)ThStep, (LPVOID)&mc[i], 0, &dw);
		CloseHandle(hThread);
	}

	WaitForMultipleObjects(threads, hDone, TRUE, INFINITE);
	for (i = 0; i<threads; i++){
		CloseHandle(hDone[i]);
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

  SetDlgItemText(hWnd,IDC_EDIT4,"");
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
  if (g_fNewtonDelta2[0] > 0)
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
	bool overwrite = true; // FIXME
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
	char *szData = stSave.ToText(": ", "\r\n");
	HANDLE hFile = CreateFile(szFile.c_str(), GENERIC_WRITE, 0, NULL, overwrite ? CREATE_ALWAYS : CREATE_NEW, 0, NULL);
	if (hFile == INVALID_HANDLE_VALUE)
	{
		stSave.DeleteToText(szData);
		return false;
	}
	DWORD dw;
	WriteFile(hFile, szData, strlen(szData), &dw, NULL);
	CloseHandle(hFile);
	stSave.DeleteToText(szData);
	return true;
}

bool SaveNewtonBackup(const complex<flyttyp> &c_new, const complex<flyttyp> &c_old, int64_t period, int step)
{
  complex<flyttyp> delta = c_new - c_old;
  complex<floatexp> delta_lo = complex<floatexp>(delta);
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

static double g_skew[4];
int64_t g_period = 0;

static int WINAPI ThNewton(HWND hWnd)
{
  SetThreadPriority(GetCurrentThread(), THREAD_MODE_BACKGROUND_BEGIN);
  SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_LOWEST);
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
	  progress_t progress = { { 0, 0, 0, 0 }, true, hWnd, CreateEvent(NULL, 0, 0, NULL), get_wall_time(), 0 };
	  HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE) ThPeriodProgress, (LPVOID) &progress, 0, NULL);
	  CloseHandle(hThread);
	  if (g_SFT.GetUseHybridFormula())
	  {
		  flyttyp r = flyttyp(4) / radius;
		  g_period = hybrid_period(g_SFT.GetHybridFormula(), INT_MAX, center.m_r, center.m_i, r, &g_skew[0], &running, &progress.counters[0]);
	  }
	  else if (type == 0 && power == 2)
	  {
		int64_t maxperiod = INT_MAX; // FIXME
		g_period = ball_period_do(center,radius,maxperiod,steps,hWnd,&progress);
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
	  WaitForMultipleObjects(1, &progress.hDone, TRUE, INFINITE);
	  CloseHandle(progress.hDone);
	  if (g_period < 0) g_period = 0;
	  uprec *= 2;
	  progress.elapsed_time = get_wall_time() - progress.start_time;
	  char status[100];
	  snprintf(status, 100, "Period %d (%d%%) (%ds)\n", (int) g_period, (int) (g_period * 100.0 / INT_MAX), (int) progress.elapsed_time);
	  s_period = status;
	  SetDlgItemText(hWnd, IDC_NR_STATUS, (s_period + s_center + s_size + s_skew).c_str());
	}
	Precision prec2(uprec);

	int bOK = 1;
	if (g_period && ! g_bNewtonStop && g_nr_action >= 1)
	{
		// fork progress updater
		progress_t progress = { { 0, 0, 0, 0 }, true, hWnd, CreateEvent(NULL, 0, 0, NULL), get_wall_time(), 0 };
		HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE) ThNewtonProgress, (LPVOID) &progress, 0, NULL);
		CloseHandle(hThread);
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
		WaitForMultipleObjects(1, &progress.hDone, TRUE, INFINITE);
		CloseHandle(progress.hDone);
		{
			progress.elapsed_time = get_wall_time() - progress.start_time;
			int eta = progress.counters[0];
			int step = progress.counters[1];
			int period = progress.counters[2];
			int iter = progress.counters[3];
			char status[100];
			snprintf(status, 100, "Center %d/%d (%d%%) (%ds)\n", step, eta >= 0 ? step + eta : 0, (int) (iter * 100.0 / period), (int) progress.elapsed_time);
			s_center = status;
			SetDlgItemText(hWnd, IDC_NR_STATUS, (s_period + s_center + s_size + s_skew).c_str());
		}

		if (! g_bNewtonStop && ! test)
		{
			if (g_nr_action >= 2)
			{
				g_szRe = c.m_r.ToText();
				g_szIm = c.m_i.ToText();

				// fork progress updater
				progress_t progress = { { int(g_period), 0, 0, 0 }, true, hWnd, CreateEvent(NULL, 0, 0, NULL), get_wall_time(), 0 };
				HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE) ThSizeProgress, (LPVOID) &progress, 0, NULL);
				CloseHandle(hThread);

				Precision prec3(expo + 6);
				flyttyp msize = 0;
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
				WaitForMultipleObjects(1, &progress.hDone, TRUE, INFINITE);
				CloseHandle(progress.hDone);
				{
					progress.elapsed_time = get_wall_time() - progress.start_time;
					int period = progress.counters[0];
					int iter = progress.counters[1];
					char status[100];
					snprintf(status, 100, "Size %d%% (%ds)\n", (int) (iter * 100.0 / period), (int) progress.elapsed_time);
					s_size = status;
					SetDlgItemText(hWnd, IDC_NR_STATUS, (s_period + s_center + s_size + s_skew).c_str());
				}
				if (0 < msize && msize < 2)
				{
					double size_factors[] = { 10, 4, 1, 0.25, 0.1, g_nr_size_factor_custom };
					double factor = size_factors[std::min(std::max(g_nr_size_factor, 0), 5)];
					if (g_nr_zoom_target >= 1)
					{
						double size_powers[] = { 0.75, 0.875, 1, 1.125, 1.25, g_nr_size_power_custom };
						double power = size_powers[std::min(std::max(g_nr_size_power, 0), 5)];
						floatexp s = msize;
						floatexp r = factor * exp(log(s) * power);
						g_szZoom = (floatexp(2) / r).toString();
						g_iterations = (g_nr_zoom_target >= 2 ? 10 : 100) * g_period; // FIXME
						bOK = 1;
					}
					else
					{
						double foldings[] = { 0.5, 0.75, 0.875, 0.9375, 1, g_nr_folding_custom };
						double power = foldings[std::min(std::max(g_nr_folding, 0), 5)];
						floatexp start = floatexp(flyttyp(g_szZoom));
						floatexp target = floatexp(2 / factor) / msize;
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
	g_bNewtonRunning=FALSE;
	PostMessage(hWnd,WM_USER+2,0,bOK);
	mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE);
	return 0;
}

static std::vector<HWND> tooltips;

extern int WINAPI NewtonProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	if (uMsg == WM_INITDIALOG)
	{
		SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
		SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));

#define T(idc,str) tooltips.push_back(CreateToolTip(idc, hWnd, str));
		T(IDC_NR_TARGET_MINIBROT_RELATIVE, "Relative zooming between current zoom level and the minibrot.\nUse Relative Folding below left to control zoom power.\nThis is similar to the method used in kf-2.15.2 and earlier.\nTransform old custom factors by new=1-2*(1-old).")
		T(IDC_NR_RELATIVE_START_ZOOM, "Override current zoom level for relative zooming.\nLeave empty use current zoom level at start time.")
		T(IDC_NR_RELATIVE_START_ZOOM_CAPTURE, "Click to capture current zoom level for relative zooming.")
		T(IDC_NR_TARGET_MINIBROT_ABSOLUTE, "Absolute zooming relative to the minibrot.\nUse Absolute Power below middle to control zoom power.")
		T(IDC_NR_TARGET_DOMAIN_ABSOLUTE, "Absolute zooming relative to the atom domain.\nUse Absolute Power below middle to control zoom power.\nOnly for Mandelbrot power 2 and Hybrid formula editor.")
		T(IDC_NR_FOLDING_2, "Current pattern doubled.")
		T(IDC_NR_FOLDING_4, "Current pattern quadrupled.")
		T(IDC_NR_FOLDING_8, "Current pattern 8-fold.")
		T(IDC_NR_FOLDING_16, "Current pattern 16-fold.")
		T(IDC_NR_FOLDING_MINIBROT, "All the way to the minibrot.")
		T(IDC_NR_FOLDING_CUSTOM, "Use the value from the custom edit box to the right.")
		T(IDC_NR_FOLDING_CUSTOM_EDIT, "Enter custom relative power here.\nEnter 0.5 for 2x folding.\nTry -1 to zoom out.")
		T(IDC_NR_SIZE_POWER_075, "Near embedded Julia set for Minibrot (Absolute).")
		T(IDC_NR_SIZE_POWER_0875, "Zoomed out from Minibrot or Atom Domain.")
		T(IDC_NR_SIZE_POWER_1, "Zoom to target (Minibrot or Atom Domain).")
		T(IDC_NR_SIZE_POWER_1125, "Near Julia morphing for Atom Domain (Absolute).\nNot useful for Minibrot (Absolute).")
		T(IDC_NR_SIZE_POWER_125, "Zoomed in from Atom Domain.\nNot useful for Minibrot (Absolute).")
		T(IDC_NR_SIZE_POWER_CUSTOM, "Use the value from the custom edit box to the right.")
		T(IDC_NR_SIZE_POWER_CUSTOM_EDIT, "Enter custom absolute power here.\nEnter 1 to zoom to target.\nValues greater than 1 are not useful for Minibrot (Absolute).")
		T(IDC_NR_SIZE_FACTOR_10, "Zoomed out 10x.")
		T(IDC_NR_SIZE_FACTOR_4, "Zoomed out 4x.")
		T(IDC_NR_SIZE_FACTOR_1, "Zoomed actual size.")
		T(IDC_NR_SIZE_FACTOR_025, "Zoomed in 4x.")
		T(IDC_NR_SIZE_FACTOR_01, "Zoomed in 10x.")
		T(IDC_NR_SIZE_FACTOR_CUSTOM, "Use the value from the custom edit box to the right.")
		T(IDC_NR_SIZE_FACTOR_CUSTOM_EDIT, "Enter custom size factor here.\nEnter 1 for actual size.")
		T(IDC_NR_ACTION_PERIOD, "Find the period of the lowest period minibrot in the clicked region.")
		T(IDC_NR_ACTION_CENTER, "As Period, and also center the view on the minibrot.")
		T(IDC_NR_ACTION_SIZE, "As Center, and also zoom to the specified power and size factor relative to the minibrot or atom domain.")
		T(IDC_NR_ACTION_AUTOSKEW, "As Size, and also automatically skew the view.")
		T(IDC_NR_BALL_METHOD, "When checked, use ball method for finding periods.\nOtherwise use box method.\nA different method (Taylor ball) is always used for power 2 Mandelbrot.");
		T(IDC_NR_SAVE_PROGRESS, "When checked, save progress snapshots when finding the center.\nResuming is not yet automatic\nNewton zooming from a snapshot will do more iterations than necessary.");
		T(IDCANCEL2, "Click to cancel the Newton-Raphson zooming calculations.");
#undef T

		SendDlgItemMessage(hWnd, IDC_NR_TARGET_MINIBROT_RELATIVE, BM_SETCHECK, g_nr_zoom_target == 0, 0);
		SendDlgItemMessage(hWnd, IDC_NR_TARGET_MINIBROT_ABSOLUTE, BM_SETCHECK, g_nr_zoom_target == 1, 0);
		SendDlgItemMessage(hWnd, IDC_NR_TARGET_DOMAIN_ABSOLUTE, BM_SETCHECK, g_nr_zoom_target == 2, 0);

		SendDlgItemMessage(hWnd, IDC_NR_FOLDING_2, BM_SETCHECK, g_nr_folding == 0, 0);
		SendDlgItemMessage(hWnd, IDC_NR_FOLDING_4, BM_SETCHECK, g_nr_folding == 1, 0);
		SendDlgItemMessage(hWnd, IDC_NR_FOLDING_8, BM_SETCHECK, g_nr_folding == 2, 0);
		SendDlgItemMessage(hWnd, IDC_NR_FOLDING_16, BM_SETCHECK, g_nr_folding == 3, 0);
		SendDlgItemMessage(hWnd, IDC_NR_FOLDING_MINIBROT, BM_SETCHECK, g_nr_folding == 4, 0);
		SendDlgItemMessage(hWnd, IDC_NR_FOLDING_CUSTOM, BM_SETCHECK, g_nr_folding == 5, 0);

		SendDlgItemMessage(hWnd, IDC_NR_SIZE_POWER_075, BM_SETCHECK, g_nr_size_power == 0, 0);
		SendDlgItemMessage(hWnd, IDC_NR_SIZE_POWER_0875, BM_SETCHECK, g_nr_size_power == 1, 0);
		SendDlgItemMessage(hWnd, IDC_NR_SIZE_POWER_1, BM_SETCHECK, g_nr_size_power == 2, 0);
		SendDlgItemMessage(hWnd, IDC_NR_SIZE_POWER_1125, BM_SETCHECK, g_nr_size_power == 3, 0);
		SendDlgItemMessage(hWnd, IDC_NR_SIZE_POWER_125, BM_SETCHECK, g_nr_size_power == 4, 0);
		SendDlgItemMessage(hWnd, IDC_NR_SIZE_POWER_CUSTOM, BM_SETCHECK, g_nr_size_power == 5, 0);

		SendDlgItemMessage(hWnd, IDC_NR_SIZE_FACTOR_10, BM_SETCHECK, g_nr_size_factor == 0, 0);
		SendDlgItemMessage(hWnd, IDC_NR_SIZE_FACTOR_4, BM_SETCHECK, g_nr_size_factor == 1, 0);
		SendDlgItemMessage(hWnd, IDC_NR_SIZE_FACTOR_1, BM_SETCHECK, g_nr_size_factor == 2, 0);
		SendDlgItemMessage(hWnd, IDC_NR_SIZE_FACTOR_025, BM_SETCHECK, g_nr_size_factor == 3, 0);
		SendDlgItemMessage(hWnd, IDC_NR_SIZE_FACTOR_01, BM_SETCHECK, g_nr_size_factor == 4, 0);
		SendDlgItemMessage(hWnd, IDC_NR_SIZE_FACTOR_CUSTOM, BM_SETCHECK, g_nr_size_factor == 5, 0);

		SendDlgItemMessage(hWnd, IDC_NR_ACTION_PERIOD, BM_SETCHECK, g_nr_action == 0, 0);
		SendDlgItemMessage(hWnd, IDC_NR_ACTION_CENTER, BM_SETCHECK, g_nr_action == 1, 0);
		SendDlgItemMessage(hWnd, IDC_NR_ACTION_SIZE, BM_SETCHECK, g_nr_action == 2, 0);
		SendDlgItemMessage(hWnd, IDC_NR_ACTION_AUTOSKEW, BM_SETCHECK, g_nr_action == 3, 0);

		SendDlgItemMessage(hWnd, IDC_NR_BALL_METHOD, BM_SETCHECK, g_nr_ball_method, 0);
		SendDlgItemMessage(hWnd, IDC_NR_SAVE_PROGRESS, BM_SETCHECK, g_SFT.GetSaveNewtonProgress(), 0);

		SetDlgItemText(hWnd, IDC_NR_RELATIVE_START_ZOOM, "");
		SetDlgItemText(hWnd, IDC_NR_STATUS, "Click the fractal to start.\nZoom size affects the\nregion to search.");
		{
			std::ostringstream s;
			s << g_nr_folding_custom;
			SetDlgItemText(hWnd, IDC_NR_FOLDING_CUSTOM_EDIT, s.str().c_str());
		}
		{
			std::ostringstream s;
			s << g_nr_size_power_custom;
			SetDlgItemText(hWnd, IDC_NR_SIZE_POWER_CUSTOM_EDIT, s.str().c_str());
		}
		{
			std::ostringstream s;
			s << g_nr_size_factor_custom;
			SetDlgItemText(hWnd, IDC_NR_SIZE_FACTOR_CUSTOM_EDIT, s.str().c_str());
		}
		return 1;
	}
	if (uMsg == WM_COMMAND && wParam == IDC_NR_RELATIVE_START_ZOOM_CAPTURE)
	{
		SetDlgItemText(hWnd, IDC_NR_RELATIVE_START_ZOOM, floatexp(CDecNumber(g_SFT.GetZoom())).toString(5).c_str());
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
			if (SendDlgItemMessage(hWnd, IDC_NR_TARGET_MINIBROT_RELATIVE, BM_GETCHECK, 0, 0)) g_nr_zoom_target = 0;
			if (SendDlgItemMessage(hWnd, IDC_NR_TARGET_MINIBROT_ABSOLUTE, BM_GETCHECK, 0, 0)) g_nr_zoom_target = 1;
			if (SendDlgItemMessage(hWnd, IDC_NR_TARGET_DOMAIN_ABSOLUTE, BM_GETCHECK, 0, 0)) g_nr_zoom_target = 2;

			if (SendDlgItemMessage(hWnd, IDC_NR_FOLDING_2, BM_GETCHECK, 0, 0)) g_nr_folding = 0;
			if (SendDlgItemMessage(hWnd, IDC_NR_FOLDING_4, BM_GETCHECK, 0, 0)) g_nr_folding = 1;
			if (SendDlgItemMessage(hWnd, IDC_NR_FOLDING_8, BM_GETCHECK, 0, 0)) g_nr_folding = 2;
			if (SendDlgItemMessage(hWnd, IDC_NR_FOLDING_16, BM_GETCHECK, 0, 0)) g_nr_folding = 3;
			if (SendDlgItemMessage(hWnd, IDC_NR_FOLDING_MINIBROT, BM_GETCHECK, 0, 0)) g_nr_folding = 4;
			if (SendDlgItemMessage(hWnd, IDC_NR_FOLDING_CUSTOM, BM_GETCHECK, 0, 0)) g_nr_folding = 5;

			if (SendDlgItemMessage(hWnd, IDC_NR_SIZE_POWER_075, BM_GETCHECK, 0, 0)) g_nr_size_power = 0;
			if (SendDlgItemMessage(hWnd, IDC_NR_SIZE_POWER_0875, BM_GETCHECK, 0, 0)) g_nr_size_power = 1;
			if (SendDlgItemMessage(hWnd, IDC_NR_SIZE_POWER_1, BM_GETCHECK, 0, 0)) g_nr_size_power = 2;
			if (SendDlgItemMessage(hWnd, IDC_NR_SIZE_POWER_1125, BM_GETCHECK, 0, 0)) g_nr_size_power = 3;
			if (SendDlgItemMessage(hWnd, IDC_NR_SIZE_POWER_125, BM_GETCHECK, 0, 0)) g_nr_size_power = 4;
			if (SendDlgItemMessage(hWnd, IDC_NR_SIZE_POWER_CUSTOM, BM_GETCHECK, 0, 0)) g_nr_size_power = 5;

			if (SendDlgItemMessage(hWnd, IDC_NR_SIZE_FACTOR_10, BM_GETCHECK, 0, 0)) g_nr_size_factor = 0;
			if (SendDlgItemMessage(hWnd, IDC_NR_SIZE_FACTOR_4, BM_GETCHECK, 0, 0)) g_nr_size_factor = 1;
			if (SendDlgItemMessage(hWnd, IDC_NR_SIZE_FACTOR_1, BM_GETCHECK, 0, 0)) g_nr_size_factor = 2;
			if (SendDlgItemMessage(hWnd, IDC_NR_SIZE_FACTOR_025, BM_GETCHECK, 0, 0)) g_nr_size_factor = 3;
			if (SendDlgItemMessage(hWnd, IDC_NR_SIZE_FACTOR_01, BM_GETCHECK, 0, 0)) g_nr_size_factor = 4;
			if (SendDlgItemMessage(hWnd, IDC_NR_SIZE_FACTOR_CUSTOM, BM_GETCHECK, 0, 0)) g_nr_size_factor = 5;

			{
				char s[256];
				GetDlgItemText(hWnd, IDC_NR_FOLDING_CUSTOM_EDIT, s, sizeof(s));
				g_nr_folding_custom = atof(s);
				GetDlgItemText(hWnd, IDC_NR_SIZE_POWER_CUSTOM_EDIT, s, sizeof(s));
				g_nr_size_power_custom = atof(s);
				GetDlgItemText(hWnd, IDC_NR_SIZE_FACTOR_CUSTOM_EDIT, s, sizeof(s));
				g_nr_size_factor_custom = atof(s);
				GetDlgItemText(hWnd, IDC_NR_RELATIVE_START_ZOOM, s, sizeof(s));
				if (std::string(s) == "")
				{
					g_szZoom = g_SFT.GetZoom();
				}
				else
				{
					g_szZoom = s;
				}
			}
			if (SendDlgItemMessage(hWnd, IDC_NR_ACTION_PERIOD, BM_GETCHECK, 0, 0)) g_nr_action = 0;
			if (SendDlgItemMessage(hWnd, IDC_NR_ACTION_CENTER, BM_GETCHECK, 0, 0)) g_nr_action = 1;
			if (SendDlgItemMessage(hWnd, IDC_NR_ACTION_SIZE, BM_GETCHECK, 0, 0)) g_nr_action = 2;
			if (SendDlgItemMessage(hWnd, IDC_NR_ACTION_AUTOSKEW, BM_GETCHECK, 0, 0)) g_nr_action = 3;

			g_nr_ball_method = SendDlgItemMessage(hWnd, IDC_NR_BALL_METHOD, BM_GETCHECK, 0, 0);
			g_SFT.SetSaveNewtonProgress(SendDlgItemMessage(hWnd, IDC_NR_SAVE_PROGRESS, BM_GETCHECK, 0, 0));

			s_period = "";
			s_center = "";
			s_size = "";
			s_skew = "";
			SetDlgItemText(hWnd, IDC_NR_STATUS, (s_period + s_center + s_size + s_skew).c_str());

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
			SetDlgItemText(hWnd, IDC_NR_STATUS, (s_period + s_center + s_size + s_skew + "Done").c_str());
			g_SFT.UndoStore();
			if (g_nr_action >= 3)
			{
				g_SFT.SetTransformMatrix(mat2(g_skew[0], g_skew[1], g_skew[2], g_skew[3]));
			}
			if (g_nr_action >= 2)
			{
				g_SFT.SetPosition(g_szRe,g_szIm,g_szZoom);
				g_SFT.SetIterations(g_iterations);
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
			MessageBox(GetParent(hWnd),"Could not apply Newton-Raphson\nYou may zoom in a little and try again","Error",MB_OK|MB_ICONSTOP);
		if((lParam == 0 || lParam < -1 || lParam > 1))
			MessageBox(GetParent(hWnd),"Unexpected stop message parameter (internal error)\n","Error",MB_OK|MB_ICONSTOP);
	}
	return 0;
}
