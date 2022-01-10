#include "defs.h"


CPixels::CPixels()
: mutex()
{
	m_pPixels = NULL;
	m_nX = 0;
	m_nY = 0;
	m_nY2 = 0;
	m_nPixels = 0;
	m_nNextPixel = 0;
}

struct CPixel
{
	uint16_t x, y; uint8_t w, h;
	CPixel(int x, int y, int w, int h) : x(x), y(y), w(w), h(h) { }
	CPixel() : CPixel(0, 0, 0, 0) { }
};

struct CPixelComparator {
	double x;
	double y;
	CPixelComparator(int x, int y) : x(x), y(y) { }
  bool operator() (const CPixel &a, const CPixel &b)
  {
		double dax = a.x - x;
		double day = a.y - y;
		double dbx = b.x - x;
		double dby = b.y - y;
		double da = dax * dax + day * day;
		double db = dbx * dbx + dby * dby;
		return da < db;
	}
};

void CPixels::Init(int width, int height, bool interactive)
{
	m_nNextPixel = -1;
	if (m_nX == width && m_nY == height && m_pPixels)
		return;
	m_nX = width;
	m_nY = height;
	m_nY2 = m_nY >> 1;
	m_nPixels = m_nX*m_nY;
	if (m_pPixels)
		delete[] m_pPixels;
	CPixel *pixels = m_pPixels = new CPixel[m_nPixels];

  // Adam7-style interlacing
    CPixelComparator cmp(width >> 1, height >> 1);
	int step = 1 << 7;
	int ix = 0;
	int begin = ix;
	int w = interactive ? step : 1;
	int h = interactive ? step : 1;
	for (int y = 0; y < height; y += step)
		for (int x = 0; x < width; x += step)
			pixels[ix++] = CPixel(x, y, w, h);
	int end = ix;
	if (interactive)
		std::sort(&pixels[0], &pixels[end], cmp);
    for (; step > 1; step >>= 1)
    {
		begin = ix;
		int w = interactive ? step >> 1 : 1;
		int h = interactive ? step : 1;
		for (int y = 0;  y < height;  y += step)
			for (int x = step >> 1; x < width; x += step)
				pixels[ix++] = CPixel(x, y, w, h);
		end = ix;
		if (interactive) std::sort(&pixels[begin], &pixels[end], cmp);
		begin = ix;
		w = interactive ? step >> 1 : 1;
		h = interactive ? step >> 1 : 1;
		for (int y = step >> 1; y < height; y += step)
			for (int x = 0; x < width; x += step >> 1)
				pixels[ix++] = CPixel(x, y, w, h);
		end = ix;
		if (interactive) std::sort(&pixels[begin], &pixels[end], cmp);
    }
	//assert(ix == width * height);
}

BOOL CPixels::GetPixel(int &rx, int &ry, int &rw, int &rh, BOOL bMirrored)
{
	do{
		int nNext = InterlockedIncrement(&m_nNextPixel);
		if (nNext < m_nPixels){
			rx = m_pPixels[nNext].x;
			ry = m_pPixels[nNext].y;
			rw = m_pPixels[nNext].w;
			rh = m_pPixels[nNext].h;
			if (bMirrored && ry>m_nY2)
				continue;
			return TRUE;
		}
		return FALSE;
	} while (bMirrored);
	return FALSE;
}


const std::string version = "2.15.5+dev";
const int kfr_version_number = 2150500;
const int kfs_version_number = 2150500;
