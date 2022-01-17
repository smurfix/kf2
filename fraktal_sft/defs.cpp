#include <stdarg.h>

#include "defs.h"
#include "StringHelper.h"

#include <vector>


std::string COLOR14::to_string() const
{
	return str_format("%d,%d,%d", r,g,b);
}

static char getByte(const char *x)
{
	int r = atoi(x);
	if(r < 0 || r > 255)
		throw_invalid("COLOR",x);
	return r;
}

static char getByte(const std::string& x)
{
	return getByte(x.c_str());
}

extern int MakePrime(int n)
{
	if (n < 1)
		return 1;
	else if (n < 4)
		return n;
	// poor man's integer square root
	int nE = 1<<((33-__builtin_clz(n))/2);

	n |= 1; // must be odd
	int i = 3;
	while (true){
		BOOL bDone = TRUE;
		for (i = 3; i<nE; i += 2)
			if (n%i == 0){
				bDone = FALSE;
				break;
			}
		if (bDone)
			break;
		n += 2;
	}
	return n;
}

COLOR14::COLOR14(std::string_view rgb)
: COLOR14()
{
	auto it = str_iter(rgb,",");
	auto ipos = it.begin();
	auto iend = it.end();
	if(ipos == iend) throw_invalid("COLOR14",rgb);
	r = getByte(*ipos++);
	if(ipos == iend) throw_invalid("COLOR14",rgb);
	g = getByte(*ipos++);
	if(ipos == iend) throw_invalid("COLOR14",rgb);
	b = getByte(*ipos);
}

std::string ColorArray::to_string(int n) const
{
	std::string res;
	res.reserve(n*10); // 111,22,33,
	for(int i=0;i<n;i++) {
		COLOR14 c = at(i);
		res += c.to_string();
		res += ",";
	}
	return res;
}

int ColorArray::from_string(const std::string_view rgb)
{
	size_t found = rgb.find(",");
	size_t startIndex = 0;
	COLOR14 c;
	int n=0, p=0;

	while(found != std::string::npos)
	{
		int v = str_atoi(std::string_view(rgb.begin()+startIndex, found));
		if (v < 0 || v > 255) throw_invalid("ColorArray:",rgb);
		startIndex = found + 1;
		found = rgb.find(",", startIndex);
		switch(p) {
			case 0: c.r = v; p = 1; break;
			case 1: c.g = v; p = 2; break;
			case 2: c.b = v; at(n) = c; n += 1; p = 0; break;
		}
	}
	if(startIndex != rgb.size()) {
		int v = str_atoi(std::string_view(rgb.begin()+startIndex, rgb.size()-startIndex));
		if (v < 0 || v > 255) throw_invalid("ColorArray:",rgb);
		if (p != 2) throw_invalid("ColorArray:",rgb);
		c.b = v; at(n) = c;
		n += 1;
	}
	return n;
}


std::string MULTIWAVE::to_string() const
{
	return str_format("%d,%d,%d", nPeriod,nStart,nType);
}

void MULTIWAVE::from_string(std::string_view pst)
{
	std::vector<std::string> res = str_split(pst,",");
	if(res.size() != 3)
		throw_invalid("MultiWave:",pst);
	int c;
#define C(var,n) \
		c = std::stoi(res[n]); \
		var = c;
	C(nPeriod,0);
	C(nStart,1);
	C(nType,2);
}
#undef C

std::string MultiWaveArray::to_string(int n) const
{
	std::string res;
	res.reserve(n*10); // 111,22,33,
	for(int i=0;i<n;i++) {
		MULTIWAVE c = at(i);
		res += c.to_string();
		res += "\t";
	}
	return res;
}

int MultiWaveArray::from_string(std::string_view pst)
{
	int n=0;

	for (const auto& it : str_iter(pst, "\t"))
	{
		if(it.length() == 0)
			continue;
		at(n).from_string(it);
		n++;
	}
	return n;
}


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
