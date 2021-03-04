vec3 colour(void)
{
  if (getInterior()) { return KFP_InteriorColor; }
  float logDE = KFP_ColorOffset / 1024.0 - log(length(getDE())) / KFP_IterDiv;
  if (KFP_Flat)
  {
    float s = float(textureSize(KFP_Palette, 0)) ;
     logDE = floor(logDE * s) / s;
  }
  return KF_Palette(logDE);
}
