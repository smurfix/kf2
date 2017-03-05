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

extern decContext g_set;
extern CFraktalSFT g_SFT;
BOOL g_bNewtonRunning=FALSE;
BOOL g_bNewtonStop=FALSE;
char *g_szRe=NULL;
char *g_szIm=NULL;
char *g_szZoom=NULL;
BOOL g_b3_4=FALSE;
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

static flyttyp cross(complex<flyttyp> a, complex<flyttyp> b) {
	return a.m_i * b.m_r - a.m_r * b.m_i;
}

static bool crosses_positive_real_axis(complex<flyttyp> a, complex<flyttyp> b) {
  if (sgn(a.m_i) != sgn(b.m_i)) {
    complex<flyttyp> d = b - a;
    int s = sgn(d.m_i);
    int t = sgn(cross(d, a));
    return s == t;
  }
  return false;
}

static bool surrounds_origin(complex<flyttyp> a, complex<flyttyp> b, complex<flyttyp> c, complex<flyttyp> d) {
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

m_d_box_period *m_d_box_period_new(complex<flyttyp> center, flyttyp radius) {
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

 int m_d_box_period_do(complex<flyttyp> center, flyttyp radius, int maxperiod,int &steps,HWND hWnd) {
	 radius = flyttyp(4)/radius;
  m_d_box_period *box = m_d_box_period_new(center, radius);
  if (! box) {
    return 0;
  }
  int period = 0;
  int i;
  char szStatus[256];
  for (i = 0; i < maxperiod && !g_bNewtonStop; ++i) {
	  if(i%100==0){
		  wsprintf(szStatus,"Finding period, %d...",i);
		  SetDlgItemText(hWnd,IDC_EDIT1,szStatus);
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
}

extern int m_d_nucleus_step(complex<flyttyp> *c_out, complex<flyttyp> c_guess, int period,flyttyp &epsilon2,HWND hWnd,int newtonStep) {
  complex<flyttyp> z(0,0);
  complex<flyttyp> dc(0,0);
  char szStatus[256];
  for (int i = 0; i < period && !g_bNewtonStop; ++i) {
	  if(i%100==0){
		  wsprintf(szStatus,"Newton-Raphson %d(%d%%)\r\n",newtonStep,i*100/period);
		  SetDlgItemText(hWnd,IDC_EDIT1,szStatus);
	  }
	  dc = _2 * z * dc + _1;
	  z = z * z + c_guess;
  }
  SetDlgItemText(hWnd,IDC_EDIT4,"");
  if (cabs2(dc) < epsilon2) {
    *c_out = c_guess;
    return 0;
  }
  if(dc.m_r==0 || dc.m_i==0)
	  return -1;
  complex<flyttyp> c_new = c_guess - z / dc;
  complex<flyttyp> d = c_new - c_guess;
  if (cabs2(d) < epsilon2) {
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
  int nNextlog=1;
  for (i = 0; i < maxsteps && !g_bNewtonStop; ++i) {
    if (1 != (result = m_d_nucleus_step(&c, c, period,epsilon2,hWnd,i)))
      break;
  }
  steps = i;
  *c_out = c;
  return result;
}

complex<flyttyp> m_d_size(complex<flyttyp> nucleus, int period,HWND hWnd)
{
  complex<flyttyp> l(1,0);
  complex<flyttyp> b(1,0);
  complex<flyttyp> z(0,0);
  char szStatus[256];
  for (int i = 1; i < period && !g_bNewtonStop; ++i) {
	  if(i%100==0){
		wsprintf(szStatus,"Determine size %d%%...",100*i/period);
		SetDlgItemText(hWnd,IDC_EDIT1,szStatus);
	  }
    z = z * z + nucleus;
    l = _2 * z * l;
    b = b + _1 / l;
  }  
  return _1 / (b * l * l);
}

int WINAPI ThNewton(HWND hWnd)
{
	char szStatus[300];
	complex<flyttyp> center(g_szRe,g_szIm);

	int digits = g_set.digits;
	flyttyp radius = g_szZoom;
	radius*=g_nZoomSize;
	char *e = strstr(g_szZoom,"E");
	if(!e)
		e = strstr(g_szZoom,"e");
	g_set.digits=(e?2*atoi(e+1):0) + 20;

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
	int period = m_d_box_period_do(center,radius,100000000,steps,hWnd);
	SetDlgItemInt(hWnd,IDC_EDIT3,period,0);
	BOOL bOK=FALSE;
	if(period){
		sprintf(szStatus,"period=%d (steps:%d)\n",period,steps);
		SetDlgItemText(hWnd,IDC_EDIT1,szStatus);
		complex<flyttyp> c;
		int test = m_d_nucleus(&c,center,period,10000,steps,radius,hWnd);

		if(test==0 && steps){
			delete g_szRe;
			char *sz = c.m_r.ToText();
			g_szRe = new char[strlen(sz)+1];
			strcpy(g_szRe,sz);

			delete g_szIm;
			sz = c.m_i.ToText();
			g_szIm = new char[strlen(sz)+1];
			strcpy(g_szIm,sz);

			complex<flyttyp>size = m_d_size(c,period,hWnd);
			flyttyp msize = flyttyp(.25)/size.m_i;
			if(msize<0)
				msize = -msize;

			char *szSize = msize.ToText();
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
			if(g_b3_4){
				zooms = 3*zooms/4;
				radius = flyttyp(2)^zooms;
				szSize = radius.ToText();
			}
			delete g_szZoom;
			g_szZoom = new char[strlen(szSize)+1];
			strcpy(g_szZoom,szSize);
			if(zooms>startZooms)
				bOK=TRUE;
		}
	}
	g_set.digits=digits;
	g_bNewtonRunning=FALSE;
	PostMessage(hWnd,WM_USER+2,0,bOK);
	return 0;
}
int WINAPI NewtonProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	if(uMsg==WM_INITDIALOG){
		if(g_b3_4)
			SendDlgItemMessage(hWnd,IDC_RADIO2,BM_SETCHECK,1,0);
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
	else if(uMsg==WM_USER+1){
		if(!g_bNewtonRunning){
			RECT r = *(RECT*)lParam;
			char *sz = g_SFT.GetRe(r.left,r.top,r.right,r.bottom);
			if(g_szRe)
				delete g_szRe;
			g_szRe = new char[strlen(sz)+1];
			strcpy(g_szRe,sz);
			sz = g_SFT.GetIm(r.left,r.top,r.right,r.bottom);
			if(g_szIm)
				delete g_szIm;
			g_szIm = new char[strlen(sz)+1];
			strcpy(g_szIm,sz);
			sz = g_SFT.GetZoom();
			if(g_szZoom)
				delete g_szZoom;
			g_szZoom = new char[strlen(sz)+1];
			strcpy(g_szZoom,sz);
			g_b3_4 = SendDlgItemMessage(hWnd,IDC_RADIO2,BM_GETCHECK,0,0);
			DWORD dw;
			g_bNewtonStop=FALSE;
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
		}
		PostMessage(GetParent(hWnd),WM_COMMAND,ID_SPECIAL_NEWTON,0);
		if(!lParam && !g_bNewtonStop)
			MessageBox(GetParent(hWnd),"Could not apply Newton-Raphson\nYou may zoom in a little and try again","Error",MB_OK|MB_ICONSTOP);
	}
	return 0;
}