#ifndef KF_COLOUR_H
#define KF_COLOUR_H 1

// linear rgb in 0..1

struct lrgb
{
  float r, g, b;
};

inline lrgb blend(const lrgb &a, const lrgb &b, float t)
{
  float t1 = 1.0f - t;
  lrgb o;
  o.r = a.r * t1 + t * b.r;
  o.r = a.g * t1 + t * b.g;
  o.r = a.b * t1 + t * b.b;
  return o;
}

// srgb in 0..1

struct srgb
{
  float r, g, b;
};

inline float srgb2lrgb1(float s)
{
  if (s <= 0.04045f)
    return s / 12.92f;
  return pow((s + 0.055f) / 1.055f, 2.4f);
}

inline lrgb srgb2lrgb(const srgb &a)
{
  lrgb o;
  o.r = srgb2lrgb1(a.r);
  o.g = srgb2lrgb1(a.g);
  o.b = srgb2lrgb1(a.b);
  return o;
}

inline float lrgb2srgb1(float l)
{
  if (l <= 0.0031308f)
    return l * 12.92f;
  return 1.055f  * pow(l, 1.0f / 2.4f) - 0.055f;
}

inline srgb lrgb2srgb(const lrgb &a)
{
  srgb o;
  o.r = lrgb2srgb1(a.r);
  o.g = lrgb2srgb1(a.g);
  o.b = lrgb2srgb1(a.b);
  return o;
}

inline srgb blend(const srgb &a, const srgb &b, float t)
{
  return lrgb2srgb(blend(srgb2lrgb(a), srgb2lrgb(b), t));
}

// srgb in 0..255

struct srgb8
{
  unsigned char r, g, b;
};

// http://pippin.gimp.org/a_dither/
inline srgb8 dither(const srgb &s, int x, int y)
{
  float mask[3];
  for (int c = 0; c < 3; ++c)
  {
    mask[c] = ((((x + c * 67) + y * 236) * 119) & 255) / 256.0f;
  }
  srgb8 o;
  o.r = floor(255.0f * s.r + mask[0]);
  o.g = floor(255.0f * s.g + mask[1]);
  o.b = floor(255.0f * s.b + mask[2]);
  return o;
}

// hsv in 0..1

struct hsv
{
  float h, s, v;
};

// HSV is a bit of a hack, assume it defines srgb for simplicity
inline srgb hsv2rgb(const hsv &a)
{
	float hue = a.h * 6.0f;
  float sat = a.s;
  float bri = a.v;
	int i = (int) floor(hue);
	float f = hue - i;
	if (! (i & 1))
		f = 1.0f - f;
	float m = bri * (1.0f - sat);
	float n = bri * (1.0f - sat * f);
  srgb cPos;
	switch (i)
	{
	case 6:
	case 0: cPos.b = bri;
		cPos.g = n;
		cPos.r = m;
		break;
	case 1: cPos.b = n;
		cPos.g = bri;
		cPos.r = m;
		break;
	case 2: cPos.b = m;
		cPos.g = bri;
		cPos.r = n;
		break;
	case 3: cPos.b = m;
		cPos.g = n;
		cPos.r = bri;
		break;
	case 4: cPos.b = n;
		cPos.g = m;
		cPos.r = bri;
		break;
	case 5: cPos.b = bri;
		cPos.g = m;
		cPos.r = n;
	}
  return cPos;
}

#endif
