// recommended bailout settings:
// linear smoothing, custom radius 25
vec3 colour(void)
{
  if (getInterior())
  {
    return KFP_InteriorColor;
  }
  bool cell = getT() > 0.5;
  return vec3(cell ? 1.0 : 0.0);
}
