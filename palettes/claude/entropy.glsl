vec3 colour()
{
  if (getInterior()) return KFP_InteriorColor;
  mat3 N = mat3(0.0);
  mat3 x = mat3(0.0);
  mat3 y = mat3(0.0);
  float49 N_center = getN3x3(N, x, y);
  float minN = 1.0 / 0.0;
  for (int i = 0; i < 3; ++i) for (int j = 0; j < 3; ++j) minN = min(minN, N[i][j]);
  for (int i = 0; i < 3; ++i) for (int j = 0; j < 3; ++j) N[i][j] -= minN;
  float sumN = 0.0;
  for (int i = 0; i < 3; ++i) for (int j = 0; j < 3; ++j) sumN += N[i][j];
  for (int i = 0; i < 3; ++i) for (int j = 0; j < 3; ++j) N[i][j] /= sumN;
  float total = 0.0;
  float count = 0.0;
  for (int i = 0; i < 3; ++i) for (int j = 0; j < 3; ++j)
  {
    float s = N[i][j] * log(N[i][j]);
    if (N[i][j] <= 0.0) s = 0.0;
    if (! isnan(s) && ! isinf(s))
    {
      total += s;
      count += 1.0;
    }
  }
  if (count <= 1.0) count = exp(1.0);
  total /= log(count);
  return KF_Palette(6.0 * total);
}
