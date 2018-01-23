#ifndef SCALED_LONG_DOUBLE_H
#define SCALED_LONG_DOUBLE_H 1

#define SLDOUBLE_SCALE 1e-4900L
#define S SLDOUBLE_SCALE

class sldouble
{
public:
  long double x;
};

inline sldouble operator+(const sldouble &a, const sldouble &b)
{
  sldouble r;
  r.x = a.x + b.x;
  return r;
}

inline sldouble operator-(const sldouble &a, const sldouble &b)
{
  sldouble r;
  r.x = a.x - b.x;
  return r;
}

inline long double operator+(const long double &a, const sldouble &b)
{
  long double r;
  r = a + S * b.x;
  return r;
}

inline long double operator-(const long double &a, const sldouble &b)
{
  long double r;
  r = a - S * b.x;
  return r;
}

inline long double operator+(const sldouble &a, const long double &b)
{
  long double r;
  r = S * a.x + b;
  return r;
}

inline long double operator-(const sldouble &a, const long double &b)
{
  long double r;
  r = S * a.x - b;
  return r;
}

inline sldouble operator*(const long double &a, const sldouble &b)
{
  sldouble r;
  r.x = a * b.x;
  return r;
}

inline sldouble operator*(const sldouble &a, const long double &b)
{
  sldouble r;
  r.x = a.x * b;
  return r;
}

#undef S

#endif
