uint palette_seed = uint(KFP_ColorOffset);

vec2 conj(vec2 a)
{
  return vec2(a.x, -a.y);
}

vec2 cmul(vec2 a, vec2 b)
{
  return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}

vec2 cdiv(vec2 a, vec2 b)
{
  return cmul(a, conj(b)) / dot(b, b);
}

vec2 clog(vec2 a)
{
  return vec2(log(length(a)), atan(a.y, a.x));
}

vec2 cexp(vec2 a)
{
  return exp(a.x) * vec2(cos(a.y), sin(a.y));
}

float U(uint a)
{
  return float(hash(a)) / 4294967296.0;
}

float U(uint a, int b)
{
  return U(a ^ hash(uint(b)));
}

float U(uint a, int b, int c)
{
  return U(a ^ hash(uint(b)), c);
}

vec3 random_colour(uint seed, float sat_range, float val_range, float val_offset)
{
  vec3 c;
  float r = sat_range * U(seed, 1);
  float t = 2.0 * 3.141592653589793 * U(seed, 2);
  c[0] = r * cos(t);
  c[1] = r * sin(t);
  c[2] = val_range * U(seed, 3) - val_offset;
  return c;
}

vec3 blend(vec3 a, vec3 b, float t)
{
  // geometric interpolation of chroma to preserve saturation
  vec2 cb = clog(cdiv(b.xy, a.xy));
  vec2 ca = clog(a.xy);
  vec2 cc = cexp(ca + cb * t);
  vec3 c = vec3(cc.xy, mix(a.z, b.z, t));
  // linear interpolation of chroma without problems at opposite chroma
  vec3 d = mix(a, b, t);
  // use cosine of chroma angle to blend between geometric and linear
  float s = dot(a.xy, b.xy) / (length(a.xy) * length(b.xy));
  return mix(c, d, (1.0 - s) * 0.5);
}

vec3 blend(vec3 a, vec3 b, vec3 c, vec3 d, float t)
{
  vec4 u = vec4(1.0, t, t * t, t * t * t);
  mat4 M = mat4
    (  0.0,  2.0,  0.0,  0.0
    , -1.0,  0.0,  1.0,  0.0
    ,  2.0, -5.0,  4.0, -1.0
    , -1.0,  3.0, -3.0,  1.0
    );
  mat3x4 p = mat3x4(a, b, c, d);
  return u * M * p * 0.5;
}

vec3 to_linearRGB(vec3 a)
{
  vec3 grey   = vec3(0.5, 0.5, 0.5);
  vec3 red    = vec3(1.0, 0.0, 0.0);
  vec3 green  = vec3(0.0, 1.0, 0.0);
  vec3 yellow = vec3(1.0, 1.0, 0.0);
  vec3 blue   = vec3(0.0, 0.0, 1.0);
  float r = length(a.xy);
  float s = tanh(r) / r;
  float u = s * a[0];
  float v = s * a[1];
  float l = exp2(a[2]);
  vec3 c =
      mix( u < 0 ? mix(grey, green, -u) : mix(grey, red, u)
         , v < 0 ? mix(grey, blue, -v) : mix(grey, yellow, v)
         , 0.5) * l;
  return c;
}

#define MAX_COLOURS 8

struct wave
{
  float offset;
  float period;
  int ncolours;
  vec3 colours[MAX_COLOURS];
};

vec3 wave_colour(wave w, float49 n)
{
  float49 m = add(w.offset, div(n, w.period));
  int k = to_int(floor(m));
  float t = wrap(m).x[0];
  k -= 1;
  k %= w.ncolours;
  k += w.ncolours; // in case of negatives
  k %= w.ncolours;
  return blend
    ( w.colours[(k    )             ]
    , w.colours[(k + 1) % w.ncolours]
    , w.colours[(k + 2) % w.ncolours]
    , w.colours[(k + 3) % w.ncolours]
    , t
    );
}

#define MAX_WAVES 8

struct multiwave
{
  int nwaves;
  vec3 grey_point;
  float blend[MAX_WAVES];
  wave waves[MAX_WAVES];
};

vec3 multiwave_colour(multiwave w, float49 n)
{
  vec3 c = vec3(1.0e-6, 1.0e-6, 0);
  for (int k = w.nwaves - 1; k >= 0; --k)
  {
    c = blend(c, wave_colour(w.waves[k], n), w.blend[k]);
  }
  return to_linearRGB(c);
}

multiwave random_multiwave(uint seed)
{
  multiwave mw;
  mw.nwaves = MAX_WAVES;
  float period = 1.0;
  for (int w = 0; w < mw.nwaves; ++w)
  {
    mw.blend[w] = 0.5 * U(seed, w, 1);
    mw.waves[w].ncolours = int(floor(3.0 + U(seed, w, 2) * (float(MAX_COLOURS) - 3.0)));
    mw.waves[w].offset = U(seed, w, 3) * mw.waves[w].ncolours;
    mw.waves[w].period = period;
    period *= (2 + 6 * U(seed, w, 4));
    for (int k = 0; k < mw.waves[w].ncolours; ++k)
    {
      mw.waves[w].colours[k] = random_colour(hash(seed ^ hash(uint(w * MAX_COLOURS + k))), 16.0, 16.0, 8.0);
    }
  }
  int iters = 16;
  vec3 l = vec3(0.0, 0.0, 0.0);
  for (int m = 0, n = 1; m < iters; ++m, n <<= 1)
  {
    l += multiwave_colour(mw, float49_(uint(n)));
  }
  l /= float(iters);
  mw.grey_point = l;
  return mw;
}

struct palette
{
  multiwave n;
  multiwave s;
  float blend_ns;
  float slope_x;
  float slope_y;
  float slope_power;
  vec3 slope_light;
  vec3 slope_dark;
  vec3 slope_grey;
  float blend_slope;
};

palette random_palette(uint seed)
{
  palette p;
  p.n = random_multiwave(hash(seed ^ 3951282789u));
  p.s = random_multiwave(hash(seed ^ 1784947738u));
  p.blend_ns = U(seed, 1);
  float t = 2 * 3.141592653589793 * U(seed, 2);
  p.slope_x = cos(t);
  p.slope_y = sin(t);
  p.slope_power = U(seed, 3) * 10;
  p.slope_light = random_colour(hash(seed ^ 1741549489u), 4, 8, -8);
  p.slope_dark  = random_colour(hash(seed ^ 3137718159u), 4, 8, -16);
  p.slope_grey = (p.slope_light + p.slope_dark) * 0.5;
  p.blend_slope = U(seed, 4);
  return p;
}

vec3 palette_colour(palette p, float49 n, float s, vec2 de)
{
  float d = p.slope_power * dot(cdiv(vec2(1.0, 0.0), de), vec2(p.slope_x, p.slope_y));
  vec3 cslope;
  if (d >= 0)
  {
    d = atan(d) / (3.141592653589793 / 2.0);
    cslope = p.slope_dark;
  }
  else
  {
    d = atan(-d) / (3.141592653589793 / 2.0);
    cslope = p.slope_light;
  }
  if (isinf(d) || isnan(d))
  {
    d = 1.0;
  }
  cslope = cslope / p.slope_grey;
  vec3 cstripe = multiwave_colour(p.s, float49_(s));
  cstripe = cstripe / p.s.grey_point;
  vec3 citer   = multiwave_colour(p.n, n);
  citer = citer / p.n.grey_point;
  return blend
    (blend(citer, cstripe, p.blend_ns), cslope, p.blend_slope * d);
}

palette P = random_palette(palette_seed);

vec3 colour(void)
{
  float49 n = getN();
  vec2 DE = getDE();
  float d = length(DE);
  float val = min(d * 0.5, 1.0);
  float avg = 0.0;
  float desat = min(pow(d / 1920.0, 4.0), 1.0);
  vec3 c = palette_colour(P, mul(16.0,  n), 4.0 * avg, DE);
  return mix(c, vec3(1.0), desat) * val;
}
