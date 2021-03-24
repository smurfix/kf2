#if 1
const float ZoomOffset = 6.5;
const vec3 TextBorder = vec3(1.0);
const float TextBorderWidth = 0.1;
const vec3 TextColour = vec3(0.0);
const int Decimals = 3;
const float HorizontalAlign = 1;
const float GlyphScale = 1.5;
const float StrokeLength = 0.7;
const float StrokeWidth = 0.2;
#else
uniform float ZoomOffset; // slider[0,6.5,16]
uniform vec3 TextBorder; // color[1,1,1]
uniform float TextBorderWidth; // slider[0,0.1,2]
uniform vec3 TextColour; // color[0,0,0]
uniform int Decimals; // slider[0,3,10]
uniform float HorizontalAlign; // slider[0,1,1]
uniform float GlyphScale; // slider[0,1.5,2]
uniform float StrokeLength; // slider[0,0.7,1]
uniform float StrokeWidth; // slider[0,0.2,1]
#endif

// format and display numbers in scientific notation

#define DIGIT_NONE -1
// digits are 0..9
#define DIGIT_TIMES 10
#define DIGIT_POWER 11
#define DIGIT_MINUS 12
#define DIGIT_POINT 13

#define DECIMALS_MAX 7
#define EXPONENT_MAX 10
#define SCIENTIFIC_MAX (3 + DECIMALS_MAX + 5 + EXPONENT_MAX)

// format to scientific, with log-domain input (useful for representing very big/small values)

int[SCIENTIFIC_MAX] formatScientificLog2(int sign_of_number, float number_log2, int decimals)
{
  // output string
  int[SCIENTIFIC_MAX] string;
  for (int i = 0; i < SCIENTIFIC_MAX; ++i)
  {
    string[i] = DIGIT_NONE;
  }
  int cursor = 0;

  // handle 0
  if (sign_of_number == 0)
  {
    string[cursor++] = 0;
    return string;
  }

  // handle special values
  if (isnan(number_log2) || isinf(number_log2))
  {
    string[cursor++] = DIGIT_TIMES;
    string[cursor++] = DIGIT_TIMES;
    string[cursor++] = DIGIT_TIMES;
    return string;
  }

  // handle negative number
  if (sign_of_number < 0)
  {
    string[cursor++] = DIGIT_MINUS;
  }

  // split log-domain scientific to decimal mantissa and exponent
  decimals = clamp(decimals, 0, DECIMALS_MAX);
  float number_log10 = number_log2 * log(2.0) / log(10.0);
  float mantissa = pow(10.0, number_log10 - floor(number_log10)); // [1..10)
  int exponent = int(floor(number_log10));

  // get decimal digits of mantissa
  float number = mantissa * pow(10.0, float(decimals));
  for (int d = decimals; d >= 0; --d)
  {
    string[cursor + d + (d > 0 ? 1 : 0)] = int(floor(mod(number, 10.0)));
    number /= 10.0;
  }
  if (decimals > 0)
  {
    string[cursor + 1] = DIGIT_POINT;
    cursor++;
  }
  cursor += decimals + 1;

  // check for special exponents
  if (exponent == 0)
  {
    return string;
  }
  string[cursor++] = DIGIT_TIMES;
  string[cursor++] = 1;
  string[cursor++] = 0;
  if (exponent == 1)
  {
    return string;
  }
  string[cursor++] = DIGIT_POWER;
  if (exponent < 0)
  {
    string[cursor++] = DIGIT_MINUS;
    exponent = -exponent;
    if (exponent < 0)
    {
      // INT_MIN, gracefully underflow...
      for (int i = 0; i < SCIENTIFIC_MAX; ++i)
      {
        string[i] = DIGIT_NONE;
      }
      string[0] = 0;
      return string;
    }
  }

  // get decimal digits of exponent
  int[10] edigits; // int is 32bit signed, so 10 digits is enough
  int enumber = exponent;
  for (int d = 9; d >= 0; --d)
  {
    edigits[d] = enumber % 10;
    enumber /= 10;
  }

  // copy to output, dropping leading zeros
  bool copying = false;
  for (int d = 0; d < 10; ++d)
  {
    if (edigits[d] != 0)
    {
      copying = true;
    }
    if (copying)
    {
      string[cursor++] = edigits[d];
    }
  }
  if (! copying)
  {
    // should never happen, implies exponent was 0
    string[cursor++] = 0;
  }

  // done!
  return string;
}

// format a float as-is

int[SCIENTIFIC_MAX] formatScientific(float number, int decimals)
{
  if (number == 0)
  {
    return formatScientificLog2(0, 0.0, decimals);
  }
  else
  {
    return formatScientificLog2(number > 0 ? 1 : -1, log2(abs(number)), decimals);
  }
}

// https://iquilezles.org/www/articles/distfunctions2d/distfunctions2d.htm
float sdOrientedBox( in vec2 p, in vec2 a, in vec2 b, float th )
{
    float l = length(b-a);
    vec2  d = (b-a)/l;
    vec2  q = (p-(a+b)*0.5);
          q = mat2(d.x,-d.y,d.y,d.x)*q;
          q = abs(q)-vec2(l,th)*0.5;
    return length(max(q,0.0)) + min(max(q.x,q.y),0.0);
}

// enhancement ideas: line cap styles, dashing patterns, etc...
float sdStroke(vec2 p, vec2 a, vec2 b, float strokeLength, float strokeWidth)
{
  float l = (1.0 - strokeLength) / 2.0;
  vec2 ab = mix(a, b, l);
  vec2 ba = mix(b, a, l);
  return sdOrientedBox(p, ab, ba, strokeWidth);
}

// display a digit (or other special character) in calculator style

float sdDigit(vec2 p, int digit, float strokeLength, float strokeWidth)
{
  float dist = 1.0 / 0.0;
  if (digit == DIGIT_POINT)
  {
    dist = min(dist, length(p - vec2(0.0, -1.0)) - strokeWidth);
    return dist;
  }
  if (digit == DIGIT_TIMES)
  {
    dist = min(dist, sdStroke(p, vec2(-0.5,  0.5), vec2(0.0, 0.0), strokeLength, strokeWidth));
    dist = min(dist, sdStroke(p, vec2( 0.5,  0.5), vec2(0.0, 0.0), strokeLength, strokeWidth));
    dist = min(dist, sdStroke(p, vec2(-0.5, -0.5), vec2(0.0, 0.0), strokeLength, strokeWidth));
    dist = min(dist, sdStroke(p, vec2( 0.5, -0.5), vec2(0.0, 0.0), strokeLength, strokeWidth));
    return dist;
  }
  if (digit == DIGIT_POWER)
  {
    dist = min(dist, sdStroke(p, vec2(-0.5, 0.0), vec2(0.0, 0.5), strokeLength, strokeWidth));
    dist = min(dist, sdStroke(p, vec2( 0.5, 0.0), vec2(0.0, 0.5), strokeLength, strokeWidth));
    return dist;
  }
  if (digit == DIGIT_MINUS)
  {
    dist = min(dist, sdStroke(p, vec2(-0.5, 0.0), vec2(0.5, 0.0), strokeLength, strokeWidth));
    return dist;
  }
  if (0 <= digit && digit < 10)
  {
    // top
    if (digit != 1 && digit != 4)
    {
      dist = min(dist, sdStroke(p, vec2(-0.5, 1.0), vec2(0.5, 1.0), strokeLength, strokeWidth));
    }
    // top left
    if (digit != 1 && digit != 2 && digit != 3)
    {
      dist = min(dist, sdStroke(p, vec2(-0.5, 1.0), vec2(-0.5, 0.0), strokeLength, strokeWidth));
    }
    // top right
    if (digit != 5 && digit != 6)
    {
      dist = min(dist, sdStroke(p, vec2(0.5, 1.0), vec2(0.5, 0.0), strokeLength, strokeWidth));
    }
    // middle
    if (digit != 0 && digit != 1 && digit != 7)
    {
      dist = min(dist, sdStroke(p, vec2(-0.5, 0.0), vec2(0.5, 0.0), strokeLength, strokeWidth));
    }
    // bottom left
    if (digit == 0 || digit == 2 || digit == 6 || digit == 8)
    {
      dist = min(dist, sdStroke(p, vec2(-0.5, 0.0), vec2(-0.5, -1.0), strokeLength, strokeWidth));
    }
    // bottom right
    if (digit != 2)
    {
      dist = min(dist, sdStroke(p, vec2(0.5, 0.0), vec2(0.5, -1.0), strokeLength, strokeWidth));
    }
    // bottom
    if (digit != 1 && digit != 4 && digit != 7)
    {
      dist = min(dist, sdStroke(p, vec2(-0.5, -1.0), vec2(0.5, -1.0), strokeLength, strokeWidth));
    }
  }
  return dist;
}

float sdDigits(vec2 p, int[SCIENTIFIC_MAX] digits, float halign, float strokeLength, float strokeWidth, float glyphScale)
{
  // compute translation for alignment
  int strlen = SCIENTIFIC_MAX;
  for (int i = 0; i < SCIENTIFIC_MAX; ++i)
  {
    if (digits[i] == DIGIT_NONE)
    {
      strlen = i;
      break;
    }
  }
  p.x = mix(p.x, p.x + float(strlen), halign);

  // draw digit
  float dist = 1.0 / 0.0;
  int ix = int(floor(p.x));
  if (0 <= ix && ix < strlen && -1.0 <= p.y && p.y <= 1.0)
  {
    // draw digit using local coordinates
    int digit = digits[ix];
    vec2 uv = vec2(p.x - floor(p.x) - 0.5, p.y);
    dist = min(dist, sdDigit(uv * glyphScale, digit, strokeLength, strokeWidth) / glyphScale);
  }
  return dist;
}

vec3 colour(void)
{
  // background colour
  vec3 RGB = KF_Colour();
  
  // position near the top right of 16:9 frame
  vec2 p = ((getCoord() / vec2(ImageSize)) * vec2(16.0, 16.0) + vec2(-16.0, -15.0)) * 2.0;
  vec2 dx = vec2(1.0 / ImageSize.x *  16.0 * 2.0, 0.0);
  vec2 dy = vec2(0.0, 1.0 / ImageSize.y * 16.0 * 2.0);
  float value = getZoomLog2();

  // draw number distance field
  float dist = 1.0/0.0;
  dist = min(dist, sdDigits(p - vec2(0.0, 1.0), formatScientificLog2(1, value, Decimals), HorizontalAlign, StrokeLength, StrokeWidth, GlyphScale));

  // draw text
  float aaWidth = length(vec4(dx, dy));
  float border = smoothstep(0.0 - aaWidth, 0.0 + aaWidth, TextBorderWidth - dist);
  float fill = smoothstep(0.0 - aaWidth, 0.0 + aaWidth, -dist);
  return mix(mix(RGB, TextBorder, border), TextColour, fill);
}
