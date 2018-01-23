#ifndef SCALED_DOUBLE_H
#define SCALED_DOUBLE_H 1

#define SDOUBLE_SCALE 1e-290
#define S SDOUBLE_SCALE

class sdouble
{
public:
  double x;
};

inline sdouble operator+(const sdouble &a, const sdouble &b)
{
  sdouble r;
  r.x = a.x + b.x;
  return r;
}

inline sdouble operator-(const sdouble &a, const sdouble &b)
{
  sdouble r;
  r.x = a.x - b.x;
  return r;
}

inline double operator+(const double &a, const sdouble &b)
{
  double r;
  r = a + S * b.x;
  return r;
}

inline double operator-(const double &a, const sdouble &b)
{
  double r;
  r = a - S * b.x;
  return r;
}

inline double operator+(const sdouble &a, const double &b)
{
  double r;
  r = S * a.x + b;
  return r;
}

inline double operator-(const sdouble &a, const double &b)
{
  double r;
  r = S * a.x - b;
  return r;
}

inline sdouble operator*(const double &a, const sdouble &b)
{
  sdouble r;
  r.x = a * b.x;
  return r;
}

inline sdouble operator*(const sdouble &a, const double &b)
{
  sdouble r;
  r.x = a.x * b;
  return r;
}

#undef S

#endif
