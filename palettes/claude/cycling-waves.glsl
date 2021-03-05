float wave(float p)
{
  return 0.5 + 0.5 * cos(6.283185307179586 * p);
}

vec3 colour()
{
  float time = KFP_ColorOffset / 1024.0;
  // interior
  if (getInterior())
  {
    return vec3(0.0);
  }
  float49 NNF = getN();
  // colour cycling
  float C0 = mod(time * 48000.0 / 20197.0 / 128.0, 1.0);
  float C1 = mod(time * 48000.0 / 20197.0 / 512.0, 1.0);
  float C2 = mod(time * 48000.0 / 20197.0 / 256.0, 1.0);
  float C3 = mod(time * 48000.0 / 20197.0 /   4.0, 1.0);
  // repeating cycles
  float N0 = wrap(div(floor(add(NNF, C0 * 257.0)), 257.0)).x[0];
  float N1 = wrap(div(floor(add(NNF, C1 * 126.0)), 126.0)).x[0];
  float N2 = wrap(div(floor(add(NNF, C2 *  26.0)), 26.0)).x[0];
  float N3 = wrap(div(floor(add(NNF, C3 *   5.0)), 5.0)).x[0];
  // infinite waves colour
  vec3 h = hsv2rgb(vec3
    ( wave(N1)
    , wave(N2)
    , wave(N3)
    ));
  // slopes
  vec2 DE = getDE();
  float slope = dot(normalize(DE), vec2(1.0, 1.0));
  float strength = abs(slope) / (1.0 + length(DE));
  if (slope < 0.0) h = mix(h, vec3(0.0), vec3(clamp(strength, 0.0, 1.0)));
  if (slope > 0.0) h = mix(h, vec3(1.0), vec3(clamp(strength, 0.0, 1.0)));
  return h;
}
