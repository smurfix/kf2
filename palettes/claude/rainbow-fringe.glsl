// black on white with a rainbow fringe
vec3 colour(void)
{
  vec2 DE = getDE() / (float(KFP_ImageSize.x) / 640.0);
  float hue = atan(DE.y, DE.x) / (2.0 * pi);
  hue -= floor(hue);
  float sat = clamp(1.0 / (1.0 + length(DE)), 0.0, 1.0);
  float val = clamp(length(DE), 0.0, 1.0);
  return hsv2rgb(vec3(hue, sat, val));
}
