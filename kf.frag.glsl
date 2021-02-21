#version 330 core

layout(location = 0, index = 0) out vec4 Internal_Colour;

uniform usampler2D Internal_N1;
uniform usampler2D Internal_N0;
uniform sampler2D Internal_NF;
uniform sampler2D Internal_T;
uniform sampler2D Internal_DEX;
uniform sampler2D Internal_DEY;
uniform sampler2D Internal_Texture;
uniform sampler1D Internal_Palette;

uniform ivec2 Internal_TilePadding;

uniform ivec2 KFP_ImageSize;

uniform uvec2 KFP_Iterations;
uniform uvec2 KFP_IterationsMin;
uniform uvec2 KFP_IterationsMax;
uniform uint KFP_JitterSeed;
uniform float KFP_IterDiv;
uniform float KFP_ColorOffset;

uniform int KFP_ColorMethod;
#define ColorMethod_Standard 0
#define ColorMethod_SquareRoot 1
#define ColorMethod_CubicRoot 2
#define ColorMethod_Logarithm 3
#define ColorMethod_Stretched 4
#define ColorMethod_DistanceLinear 5
#define ColorMethod_DEPlusStandard 6
#define ColorMethod_DistanceLog 7
#define ColorMethod_DistanceSqrt 8
#define ColorMethod_LogLog 9
#define ColorMethod_ATan 10
#define ColorMethod_FourthRoot 11

uniform int KFP_Differences;
#define Differences_Traditional 0
#define Differences_Forward3x3 1
#define Differences_Central3x3 2
#define Differences_Diagonal2x2 3
#define Differences_LeastSquares2x2 4
#define Differences_LeastSquares3x3 5
#define Differences_Laplacian3x3 6
#define Differences_Analytic 7

uniform float KFP_PhaseColorStrength;
uniform bool KFP_ShowGlitches;
uniform vec3 KFP_InteriorColor;

uniform bool KFP_Smooth;
uniform bool KFP_Flat;
uniform bool KFP_InverseTransition;

uniform bool KFP_MultiWavesEnabled;
uniform bool KFP_MultiWavesBlend;
uniform int KFP_MultiWavesCount;
uniform ivec3 KFP_MultiWaves[32];

uniform bool KFP_Slopes;
uniform float KFP_SlopePower;
uniform float KFP_SlopeRatio;
uniform vec2 KFP_SlopeDir;

uniform bool KFP_TextureEnabled;
uniform float KFP_TextureMerge;
uniform float KFP_TexturePower;
uniform float KFP_TextureRatio;

const float pi = 3.141592653;

float sqr(float x) { return x * x; }
float hypot1(float x, float y) { return sqrt(x * x + y * y); }
float hypot2(float x, float y) { return x * x + y * y; }

// <http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl>
vec3 hsv2rgb(vec3 c)
{
  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

float srgb2lrgb(float s)
{
  if (s <= 0.04045)
    return s / 12.92;
  return pow((s + 0.055) / 1.055, 2.4);
}

vec3 srgb2lrgb(vec3 s)
{
  return vec3(srgb2lrgb(s.x), srgb2lrgb(s.y), srgb2lrgb(s.z));
}

float lrgb2srgb(float l)
{
  if (l <= 0.0031308)
    return l * 12.92;
  return 1.055  * pow(l, 1.0 / 2.4) - 0.055;
}

vec3 lrgb2srgb(vec3 s)
{
  return vec3(lrgb2srgb(s.x), lrgb2srgb(s.y), lrgb2srgb(s.z));
}

vec3 palette(float ix)
{
  // map [0..1) to [0..1024)
  ix -= floor(ix);
  ix *= 1024.0;
  // to match KF's regular colouring, need to sin-interpolate c0, c1
  // to get neighbouring colours in 1024-palette
  // and then linear-interpolate those (if smoothing is desired)
  // c0, c1 are neighbouring colours in n-palette with interpolant cf
  int m_nParts = textureSize(Internal_Palette, 0);
  vec3 c0, c1;
  {
    int i = int(floor(ix));
    float temp = float(i) * float(m_nParts) / 1024.0;
    int p = int(temp);
    int pn = (p + 1) % m_nParts;
    temp -= float(p);
    temp = sin((temp - 0.5) * pi) / 2.0 + 0.5;
    c0 = mix
      ( texelFetch(Internal_Palette, p, 0).rgb
      , texelFetch(Internal_Palette, pn, 0).rgb
      , temp
      );
  }
  if (KFP_Smooth)
  {
    int i = (int(floor(ix)) + 1) % 1024;
    float temp = float(i) * float(m_nParts) / 1024.0;
    int p = int(temp);
    int pn = (p + 1) % m_nParts;
    temp -= float(p);
    temp = sin((temp - 0.5) * pi) / 2.0 + 0.5;
    c1 = mix
      ( texelFetch(Internal_Palette, p, 0).rgb
      , texelFetch(Internal_Palette, pn, 0).rgb
      , temp
      );
    ix -= floor(ix);
    return mix(c0, c1, ix);
  }
  else
  {
    return c0;
  }
}

#define Float4 float
float add(float a, float b) { return a + b; }
float sub(float a, float b) { return a - b; }
float mul(float a, float b) { return a * b; }
float div(float a, float b) { return a / b; }
float sqrt_(float a) { return sqrt(a); }
float cbrt_(float a) { return pow(a, 1.0 / 3.0); }
float log_(float a) { return log(a); }
float atan_(float a) { return atan(a); }
float floor_(float a) { return floor(a); }
float max_(float a, float b) { return max(a, b); }
float wrap(float a) { return sub(a, floor_(a)); }

float float4(uint a) { return float(a); }
float float4(float a) { return a; }
float float4(uint a, uint b) { return float(a) * pow(2.0, 32.0) + float(b); }
float float4(uint a, uint b, float c) { return float4(a, b) + c; }

int diff(uint a, uint b)
{
  if (a >= b)
  {
    return int(a - b);
  }
  else
  {
    return -int(b - a);
  }
}

void main(void)
{
  ivec2 tc = Internal_TilePadding + ivec2(KFP_ImageSize.y - 1 - int(gl_FragCoord.y), int(gl_FragCoord.x));
  vec3 s = vec3(0.0);
  uint N1 = texelFetch(Internal_N1, tc, 0).r;
  uint N0 = texelFetch(Internal_N0, tc, 0).r;
  float NF = texelFetch(Internal_NF, tc, 0).r;
  Float4 N = float4(N1, N0, 1.0 - NF);
  float T = texelFetch(Internal_T, tc, 0).r;
  vec2 DE = vec2(texelFetch(Internal_DEX, tc, 0).r, texelFetch(Internal_DEY, tc, 0).r);
  if (! KFP_ShowGlitches && NF < 0.0)
  {
    discard;
  }
  if (uvec2(N0, N1) == KFP_Iterations)
  {
    s = KFP_InteriorColor;
  }
  else
  {
    Float4 iter = float4(0.0);
    switch (KFP_ColorMethod)
    {
      default:
      case ColorMethod_Standard: iter = N; break;
      case ColorMethod_SquareRoot: iter = sqrt_(max_(0.0, N)); break;
      case ColorMethod_CubicRoot: iter = cbrt_(max_(0.0, N)); break;
      case ColorMethod_Logarithm: iter = log_(max_(1.0, N)); break;
      case ColorMethod_LogLog: iter = log_(add(1.0, log_(add(1.0, N)))); break;
      case ColorMethod_ATan: iter = atan_(N); break;
      case ColorMethod_FourthRoot: iter = sqrt_(sqrt_(max_(0.0, N))); break;
      case ColorMethod_Stretched:
      {
        Float4 imin = float4(KFP_IterationsMin[1], KFP_IterationsMin[0]);
        Float4 imax = float4(KFP_IterationsMax[1], KFP_IterationsMax[0]);
        iter = mul(1024.0, div(sub(N, imin), sub(imax, imin))); break;
      }
      case ColorMethod_DistanceLinear:
      case ColorMethod_DEPlusStandard:
      case ColorMethod_DistanceLog:
      case ColorMethod_DistanceSqrt:
      {
        iter = float4(0.0);
        if (KFP_Differences == Differences_Analytic)
        {
          iter = float4(1.0 / length(DE));
        }
        else
        {
          // load 3x3 stencil around the pixel
          mat3 p = mat3(0.0);
          mat3 px = mat3(0.0);
          mat3 py = mat3(0.0);
          for (int dj = -1; dj <= 1; ++dj)
          {
            for (int di = -1; di <= 1; ++di)
            {
              ivec2 tc1 = tc + ivec2(-dj, di);
              p[dj + 1][di + 1] = sub(float4
                ( texelFetch(Internal_N1, tc1, 0).r
                , texelFetch(Internal_N0, tc1, 0).r
                , 1.0 - texelFetch(Internal_NF, tc1, 0).r
                ), N)/*.f[0]*/;
              px[dj + 1][di + 1] = float(di);
              py[dj + 1][di + 1] = float(dj);
              // GetPixelOffset(x    , y    , px[1][1], py[1][1]); px += di py += dj// FIXME jitter coords
            }
          }
          // tile boundaries are clamp to edge
          // need to render overlapping padded tiles for correctness
          switch (KFP_Differences)
          {
            case Differences_Laplacian3x3:
            {
              float L = 0.0;
              L +=  1.0 * p[0][0];
              L +=  4.0 * p[0][1];
              L +=  1.0 * p[0][2];
              L +=  4.0 * p[1][0];
              L -= 20.0 * p[1][1];
              L +=  4.0 * p[1][2];
              L +=  1.0 * p[2][0];
              L +=  4.0 * p[2][1];
              L +=  1.0 * p[2][2];
              L /=  6.0;
  #define INV_LOG_2 1.4426950408889634
              float g = sqrt(abs(L * INV_LOG_2));
  #undef INV_LOG_2
              iter = float4(g * 2.8284271247461903);
            }
            break;
            case Differences_LeastSquares3x3:
            {
              float gx = 0;
              float gy = 0;
              // compute_gradient_3x3(p, px, py, gx, gy); FIXME
              float g = hypot1(gx, gy);
              iter = float4(g * 2.8284271247461903);
            }
            break;
            case Differences_LeastSquares2x2:
            {
              float gx = 0;
              float gy = 0;
              // compute_gradient_2x2(p, px, py, gx, gy); FIXME
              float g = hypot1(gx, gy);
              iter = float4(g * 2.8284271247461903);
            }
            break;
            case Differences_Central3x3:
            {
              // gerrit's central difference formula
              float gx = sqr(p[2][1] - p[0][1]) / hypot2(px[2][1] - px[0][1], py[2][1] - py[0][1]);
              float gy = sqr(p[1][2] - p[1][0]) / hypot2(px[1][2] - px[1][0], py[1][2] - py[1][0]);
              float g1 = sqr(p[2][2] - p[0][0]) / hypot2(px[2][2] - px[0][0], py[2][2] - py[0][0]);
              float g2 = sqr(p[0][2] - p[2][0]) / hypot2(px[0][2] - px[2][0], py[0][2] - py[2][0]);
              float g = sqrt(0.5 * (gx + gy + g1 + g2));
              iter = float4(g * 2.8284271247461903);
            }
            break;
            case Differences_Forward3x3:
            {
              // forward differencing in 8 directions from the target point
              float gx0 = sqr(p[0][1] - p[1][1]) / hypot2(px[0][1] - px[1][1], py[0][1] - py[1][1]);
              float gx2 = sqr(p[2][1] - p[1][1]) / hypot2(px[2][1] - px[1][1], py[2][1] - py[1][1]);
              float gy0 = sqr(p[1][0] - p[1][1]) / hypot2(px[1][0] - px[1][1], py[1][0] - py[1][1]);
              float gy2 = sqr(p[1][2] - p[1][1]) / hypot2(px[1][2] - px[1][1], py[1][2] - py[1][1]);
              float gu0 = sqr(p[0][0] - p[1][1]) / hypot2(px[0][0] - px[1][1], py[0][0] - py[1][1]);
              float gu2 = sqr(p[2][2] - p[1][1]) / hypot2(px[2][2] - px[1][1], py[2][2] - py[1][1]);
              float gv0 = sqr(p[2][0] - p[1][1]) / hypot2(px[2][0] - px[1][1], py[2][0] - py[1][1]);
              float gv2 = sqr(p[0][2] - p[1][1]) / hypot2(px[0][2] - px[1][1], py[0][2] - py[1][1]);
              float g = sqrt(0.25 * (gx0 + gx2 + gy0 + gy2 + gu0 + gu2 + gv0 + gv2));
              iter = float4(g * 2.8284271247461903);
            }
            break;
            case Differences_Diagonal2x2: // aka Roberts Cross
            {
              // forward differencing in 2 diagonals of a 2x2 substencil
              if (KFP_JitterSeed == 0u)
              {
                float gu = sqr(p[0][0] - p[1][1]) / hypot2(px[0][0] - px[1][1], py[0][0] - py[1][1]);
                float gv = sqr(p[0][1] - p[1][0]) / hypot2(px[0][1] - px[1][0], py[0][1] - py[1][0]);
                float g = sqrt(gu + gv);
                iter = float4(g * 2.8284271247461903);
              }
              else
              {
                // with displacement correction by gerrit
                float nux = px[0][0] - px[1][1];
                float nuy = py[0][0] - py[1][1];
                float nvx = px[1][0] - px[0][1];
                float nvy = py[1][0] - py[0][1];
                float nu = hypot1(nux, nuy);
                float nv = hypot1(nvx, nvy);
                nux /= nu;
                nuy /= nu;
                nvx /= nv;
                nvy /= nv;
                float u = (p[0][0] - p[1][1]) / nu;
                float v = (p[1][0] - p[0][1]) / nv;
                float dotnunv = nux * nvx + nuy * nvy;
                float crossnunv = nux * nvy - nuy * nvx;
                float g = sqrt((u * u + v * v - 2 * u * v * dotnunv) / sqr(crossnunv));
                iter = float4(g * 2.8284271247461903);
              }
            }
            break;
            default:
            case Differences_Traditional:
            {
              // traditional method reverse engineered from original code
              float gx = (p[0][1] - p[1][1]) * 1.414 / hypot1(px[0][1] - px[1][1], py[0][1] - py[1][1]);
              float gy = (p[1][0] - p[1][1]) * 1.414 / hypot1(px[1][0] - px[1][1], py[1][0] - py[1][1]);
              float gu = (p[0][0] - p[1][1]) * 1.414 / hypot1(px[0][0] - px[1][1], py[0][0] - py[1][1]);
              float gv = (p[0][2] - p[1][1]) * 1.414 / hypot1(px[0][2] - px[1][1], py[0][2] - py[1][1]);
              float g = abs(gx) + abs(gy) + abs(gu) + abs(gv);
              iter = float4(g);
            }
            break;
          }
        }
        // post differencing transfer functions
        iter = mul(iter, float(KFP_ImageSize.x) / 640.0);
        if (KFP_ColorMethod == ColorMethod_DistanceSqrt || KFP_ColorMethod == ColorMethod_DEPlusStandard)
          iter = sqrt_(max_(0.0, iter));
        else if (KFP_ColorMethod == ColorMethod_DistanceLog)
          iter = log_(max_(1.0, add(1.0, iter)));
        if(iter/*.f[0]*/ > 1024.0)
          iter = float4(1024.0);
        if(KFP_ColorMethod == ColorMethod_DEPlusStandard && iter/*.f[0]*/ > KFP_IterDiv)
          iter = N;
        break;
      }
    }
    iter = div(iter, KFP_IterDiv);
    iter = add(iter, KFP_ColorOffset);
    Float4 iter_ = floor_(iter);
    float offs = sub(iter, iter_)/*.f[0]*/;
    if (KFP_InverseTransition)
      iter = add(iter_, 1.0 - offs);
    if (KFP_MultiWavesEnabled){
      float nH = 0.0, nS = 0.0, nB = 0.0;
      int nDR = 0, nDG = 0, nDB = 0;
      for (int i = 0; i < KFP_MultiWavesCount; i++){
        float nPeriod = float(KFP_MultiWaves[i].x);
        float g;
        if (KFP_Smooth)
          g = sin(2.0 * pi * wrap(div(iter, nPeriod * 2.0))/*.f[0]*/) / 2.0 + 0.5;
        else
          g = sin(2.0 * pi * wrap(div(floor_(iter), nPeriod * 2.0))/*.f[0]*/) / 2.0 + 0.5;
        if (nPeriod < 0)
          g = -nPeriod / 100.0;
        int type = KFP_MultiWaves[i].z;
        if (type == 0){
          nH += g;
          nDR++;
        }
        if (type == 1){
          nS += g;
          nDG++;
        }
        if (type == 2){
          nB += g;
          nDB++;
        }
      }
      if (nDR > 0)
        nH /= float(nDR);
      if (nDG > 0)
        nS /= float(nDG);
      if (nDB > 0)
        nB /= float(nDB);
      vec3 nRGB = /*srgb2lrgb*/(hsv2rgb(vec3(nH, nS, nB)));
      if (KFP_MultiWavesBlend)
      {
        iter = add(iter, KFP_PhaseColorStrength / 100.0 * 1024.0 * T);
        iter_ = floor_(iter);
        offs = sub(iter, iter_)/*.f[0]*/;
        if (KFP_InverseTransition)
          iter = add(iter_, 1.0 - offs);
        vec3 nRGB2 = palette(wrap(div(iter, 1024.0))/*.f[0]*/);
        s = mix(nRGB, nRGB2, 0.5);
      }
      else
      {
        s = nRGB;
      }
    }
    else{
      iter = add(iter, KFP_PhaseColorStrength / 100.0 * 1024.0 * T);
      iter_ = floor_(iter);
      offs = sub(iter, iter_)/*.f[0]*/;
      if (KFP_InverseTransition)
        iter = add(iter_, 1.0 - offs);
      s = palette(wrap(div(iter, 1024.0))/*.f[0]*/);
    }
  }
  if(KFP_TextureEnabled)
  {
    // SetTexture(nIndex,x,y,s); FIXME
  }
  if (KFP_Slopes)
  {
    vec2 vdiff;
    if (KFP_Differences == Differences_Analytic)
    {
      vdiff = vec2(1.0, -1.0) * DE / dot(DE, DE);
    }
    else
    {
      vdiff.x = sub(float4
        ( texelFetch(Internal_N1, tc + ivec2(0, 1), 0).r
        , texelFetch(Internal_N0, tc + ivec2(0, 1), 0).r
        , 1.0 - texelFetch(Internal_NF, tc + ivec2(0, 1), 0).r
        ), N)/*.f[0]*/;
      vdiff.y = -sub(float4
        ( texelFetch(Internal_N1, tc + ivec2(-1, 0), 0).r
        , texelFetch(Internal_N0, tc + ivec2(-1, 0), 0).r
        , 1.0 - texelFetch(Internal_NF, tc + ivec2(-1, 0), 0).r
        ), N)/*.f[0]*/;
    }
    float diff = dot(vdiff, KFP_SlopeDir);
    diff *= KFP_SlopePower * float(KFP_ImageSize.x) / 640.0;
    if (diff >= 0.0)
    {
      diff = atan(diff) / (pi / 2.0);
      diff = diff * KFP_SlopeRatio / 100.0;
      s = (1 - diff) * s;
    }
    else
    {
      diff = -diff;
      diff = atan(diff) / (pi / 2.0);
      diff = diff * KFP_SlopeRatio / 100.0;
      s = (1 - diff) * s + vec3(diff);
    }
  }
  if (uvec2(N0, N1) == KFP_Iterations && ! KFP_TextureEnabled)
  {
    s = KFP_InteriorColor;
  }
  Internal_Colour = vec4(clamp(s, vec3(0.0), vec3(65504.0)), 1.0);
}
