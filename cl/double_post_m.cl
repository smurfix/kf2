// ...continued
    xr = xrn;
    xi = xin;
    dxa = dxan;
    dxb = dxbn;
    dya = dyan;
    dyb = dybn;
#ifdef TRIANGLE_INEQUALITY_AVERAGE
    {
      const mantissa abs_z = sqrt(Xxr2 + Xxi2);
      const mantissa zsubcr = Xxr - Ccr;
      const mantissa zsubci = Xxi - Cci;
      const mantissa abs_z_sub_c = sqrt(zsubcr * zsubcr + zsubci * zsubci);
      const mantissa abs_z_sub_c_sub_abs_c = fabs(abs_z_sub_c - abs_c);
      const mantissa abs_z_sub_c_add_abs_c = abs_z_sub_c + abs_c;
      const mantissa tia_inc = (abs_z - abs_z_sub_c_sub_abs_c) / (abs_z_sub_c_add_abs_c - abs_z_sub_c_sub_abs_c);
      if (antal != 0 && antal != g->nMaxIter - 1 && ! isnan(tia_inc) && ! isinf(tia_inc))
      {
        tia_sum_old = tia_sum;
        tia_sum += tia_inc;
        tia_count += 1;
      }
    }
#endif
  }
  l->antal = antal;
  l->test1 = test1;
  l->test2 = test2;
  l->xr = Xxr;
  l->xi = Xxi;
  l->dxa = dxa;
  l->dxb = dxb;
  l->dya = dya;
  l->dyb = dyb;
#ifdef TRIANGLE_INEQUALITY_AVERAGE
  {
    const mantissa avg = tia_sum / tia_count;
    const mantissa avg_old = tia_sum_old / (tia_count - 1);
    const mantissa abs_z = sqrt(Xxr * Xxr + Xxi * Xxi);
    const mantissa abs_R = sqrt(g->m_nBailout2);
    const mantissa p = exp(l->log_m_nPower);
    mantissa f = (log(log(abs_z)) - log(log(abs_R))) / log(p);
    if (abs_z < abs_R)
    {
      f = 0;
    }
    const mantissa tia = avg + (avg_old - avg) * f;
    l->tia = tia;
  }
#endif
}
