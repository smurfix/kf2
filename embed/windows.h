#ifndef KF_PY_WINDOWS_H
#define KF_PY_WINDOWS_H
// Old Windows code: sporting the most useless definitions ever.

#define WINAPI
#include <inttypes.h>
#include <time.h>
#include <memory.h>
#include <stdlib.h>
#include <string.h>
#include <sched.h>
#include <unistd.h>
#include <dlfcn.h>

typedef int64_t __int64;

// #define VOID void
// typedef char CHAR;
// typedef short SHORT;
typedef long LONG;
// typedef int INT;
typedef unsigned int UINT;
typedef unsigned long ULONG, *PULONG;
// typedef unsigned short USHORT;
// typedef USHORT *PUSHORT;
// typedef unsigned char UCHAR;
// typedef UCHAR *PUCHAR;
// typedef char *PSZ;

typedef char TCHAR;
typedef unsigned char BYTE;
typedef unsigned char byte;
typedef unsigned short WORD;
typedef unsigned long DWORD;
// typedef float FLOAT;
// typedef FLOAT *PFLOAT;
// typedef BYTE *PBYTE;
// typedef BYTE *LPBYTE;
// typedef int *PINT;
// typedef int *LPINT;
// typedef WORD *PWORD;
// typedef WORD *LPWORD;
// typedef long *LPLONG;
// typedef DWORD *PDWORD;
// typedef DWORD *LPDWORD;
typedef void *LPVOID;

typedef bool BOOL;
#define TRUE true
#define FALSE false

typedef void *HANDLE;
typedef HANDLE HGLOBAL;
// typedef HANDLE *PHANDLE;
// typedef BYTE FCHAR;
// typedef WORD FSHORT;
// typedef DWORD FLONG;
// typedef LONG HRESULT;

typedef size_t SIZE_T;

#define INFINITE 0xffffffff

typedef long long INT_PTR,*PINT_PTR;
typedef unsigned long long UINT_PTR,*PUINT_PTR;
typedef long long LONG_PTR,*PLONG_PTR;
typedef unsigned long long ULONG_PTR,*PULONG_PTR;

typedef UINT_PTR WPARAM;
typedef LONG_PTR LPARAM;
typedef LONG_PTR LRESULT;
typedef ULONG_PTR DWORD_PTR,*PDWORD_PTR;

typedef struct tagPOINT {
  LONG x;
  LONG y;
} POINT,*PPOINT,*NPPOINT,*LPPOINT;

typedef struct tagSIZE {
  LONG cx;
  LONG cy;
} SIZE,*PSIZE,*LPSIZE;

typedef struct tagRECT {
  LONG left;
  LONG top;
  LONG right;
  LONG bottom;
} RECT,*PRECT,*NPRECT,*LPRECT;

typedef DWORD COLORREF;

typedef struct tagBITMAP {
  LONG bmType;
  LONG bmWidth;
  LONG bmHeight;
  LONG bmWidthBytes;
  WORD bmPlanes;
  WORD bmBitsPixel;
  LPVOID bmBits;
} BITMAP,*PBITMAP,*NPBITMAP,*LPBITMAP;

#if 0

struct HBITMAP__ { int unused; }; typedef struct HBITMAP__ *HBITMAP;
struct HDC__ { int unused; }; typedef struct HDC__ *HDC;
struct HFONT__ { int unused; }; typedef struct HFONT__ *HFONT;
struct HICON__ { int unused; }; typedef struct HICON__ *HICON;
struct HINSTANCE__ { int unused; }; typedef struct HINSTANCE__ *HINSTANCE;

#endif

typedef struct tagBITMAPINFOHEADER {
  LONG biWidth;
  LONG biHeight;
  WORD biBitCount;
  DWORD biSizeImage;
} BITMAPINFOHEADER;

#if 0

typedef struct tagBITMAPINFO {
  BITMAPINFOHEADER bmiHeader;
  RGBQUAD bmiColors[1];
} BITMAPINFO,*LPBITMAPINFO,*PBITMAPINFO;

#endif

typedef struct tagRGBQUAD {
  BYTE rgbBlue;
  BYTE rgbGreen;
  BYTE rgbRed;
  BYTE rgbReserved;
} RGBQUAD,*LPRGBQUAD;

// // // clock // // //
static inline clock_t GetTickCount() {
  return clock();
}

// // // memory // // //
static inline HGLOBAL GlobalAlloc(UINT uFlags, SIZE_T dwBytes)
{
  (void)uFlags;
  return malloc(dwBytes);
}

static inline void GlobalFree(HGLOBAL hdl)
{
  free(hdl);
}

static inline void DeleteObject(HGLOBAL hdl)
{
  free(hdl);
}

// // // system info // // //

typedef struct _SYSTEM_INFO {
#if 0
    __extension__ union {
      DWORD dwOemId;
      __extension__ struct {
 WORD wProcessorArchitecture;
 WORD wReserved;
      } ;
    } ;
    DWORD dwPageSize;
    LPVOID lpMinimumApplicationAddress;
    LPVOID lpMaximumApplicationAddress;
    DWORD_PTR dwActiveProcessorMask;
    DWORD dwProcessorType;
    DWORD dwAllocationGranularity;
    WORD wProcessorLevel;
    WORD wProcessorRevision;
#endif
    DWORD dwNumberOfProcessors;
} SYSTEM_INFO, *LPSYSTEM_INFO;

static inline void GetSystemInfo(SYSTEM_INFO *info)
{
  memset(info, 0, sizeof(*info));

  cpu_set_t cs;
  CPU_ZERO(&cs);
  sched_getaffinity(0, sizeof(cs), &cs);

  info->dwNumberOfProcessors = CPU_COUNT(&cs);
}

// // // misc // // //
static inline void Beep(int a, int b) { (void)a; (void)b; }
static inline void Sleep(int n) { usleep(n*1000); }
static inline LONG InterlockedIncrement(volatile LONG *ptr)
{
  return __atomic_add_fetch(ptr, 1, __ATOMIC_ACQ_REL);
}
static inline LONG InterlockedDecrement(volatile LONG *ptr)
{
  return __atomic_add_fetch(ptr, -1, __ATOMIC_ACQ_REL);
}
static inline void SwitchToThread() { usleep(1000); }
static inline void *GetModuleHandle(void *x) { (void)x; return NULL; }
static inline void *GetProcAddress(void *x, const char *name) { return dlsym(x,name); }
static inline void SetDlgItemText(void *hwnd, int dlgID, const char *str) {
  (void)hwnd;
  (void)dlgID;
  (void)str;
}

// // // UI // // //
//
#define WM_INITDIALOG 10001
#define WM_USER 20000

#define MessageBox(a,b,c,d) do{}while(0)
#define PostMessage(a,b,c,d) do{}while(0)


#define wsprintf sprintf

#endif // KF_PY_WINDOWS_H
