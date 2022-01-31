/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2021 Claude Heiland-Allen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#include "kf2.h"
#include "kf2/params.h"

#include "../fraktal_sft/fraktal_sft.h"

extern "C" void kf2_get_version(int *major, int *minor, int *revision, int *patch)
{
  if (major) *major = KF2_VERSION_MAJOR;
  if (minor) *minor = KF2_VERSION_MINOR;
  if (revision) *revision = KF2_VERSION_REVISION;
  if (patch) *patch = KF2_VERSION_PATCH;
}

extern "C" const char *kf2_get_version_string(void)
{
  return KF2_VERSION_STRING;
}

enum kf2_state
{
  kf2_state_idle = 0,
  kf2_state_rendering = 1,
  kf2_state_stopping = 2
};

struct kf2_t
{
  CFraktalSFT *g_SFT;
  kf2_state state;
};

extern "C" struct kf2_t *kf2_new(void)
{
  struct kf2_t *kf2 = new kf2_t();
  kf2->g_SFT = new CFraktalSFT();
  kf2->state = kf2_state_idle;
  return kf2;
}

extern "C" void kf2_delete(struct kf2_t *kf2)
{
  assert(kf2);
  assert(kf2->state == kf2_state_idle);
  assert(kf2->g_SFT);
  delete kf2->g_SFT;
  kf2->g_SFT = nullptr;
  delete kf2;
}

extern "C" void kf2_start(struct kf2_t *kf2)
{
  assert(kf2);
  assert(kf2->state == kf2_state_idle);
  assert(kf2->g_SFT);
  int64_t w = 0, h = 0, s = 0;
  kf2->g_SFT->GetTargetDimensions(&w, &h, &s);
  kf2->g_SFT->SetImageSize(w * s, h * s);
  kf2->state = kf2_state_rendering;
  kf2->g_SFT->Render();
}

extern "C" int kf2_stop(struct kf2_t *kf2)
{
  assert(kf2);
  assert(kf2->g_SFT);
  if(kf2->g_SFT->Stop(true)) {
    kf2->state = kf2_state_stopping;
    return 1;
  } else {
    kf2->state = kf2_state_idle;
    return 0;
  }
}

extern "C" int kf2_wait(struct kf2_t *kf2)
{
  assert(kf2);
  assert(kf2->g_SFT);
  bool res = kf2->g_SFT->Wait();
  kf2->state = kf2_state_idle;
  return res;
}

/******************* Logging *******************/

extern "C" void kf2_set_log_cb(struct kf2_t *kf2, kf2_log_cb log_cb, void *arg) // FIXME
{
  assert(kf2);
  assert(kf2->state == kf2_state_idle);
  assert(kf2->g_SFT);
  log_cb(arg, KF2_LOGLEVEL_ERROR, "kf2_set_log_cb() not yet implemented");
}

void kf2_get_log_cb(const struct kf2_t *kf2, kf2_log_cb *log_cb, void **arg) // FIXME
{
  assert(kf2);
  assert(kf2->state == kf2_state_idle);
  assert(kf2->g_SFT);
  if (log_cb) *log_cb = nullptr;
  if (arg) *arg = nullptr;
}

/******************* Settings *******************/

struct kf2_params_t
{
  SP_Settings S;
};


struct kf2_param_info_t *kf2_param_info(int pos)
{
    assert(pos >= 0);
    if(pos >= nSettings)
        return nullptr;
    return &SettingsData[pos].P;
}


struct kf2_params_t *kf2_get_params(kf2_t *kf2)
{
    assert(kf2);
    assert(kf2->g_SFT);

    auto kf2p = new kf2_params_t;
    kf2p->S = kf2->g_SFT->ModSettings();
    return kf2p;
}

struct kf2_params_t *kf2_default_params(void)
{
    auto kf2p = new kf2_params_t;
    kf2p->S = NEW_SETTINGS();
    return kf2p;
}

int kf2_apply_params(struct kf2_t *kf2, struct kf2_params_t *kf2p)
{
    assert(kf2);
    assert(kf2->g_SFT);
    assert(kf2p);
    return !kf2->g_SFT->ApplySettings(kf2p->S);
}

void kf2_param_free(struct kf2_params_t *kf2p)
{
    kf2p->S = nullptr;
    delete kf2p;
}

char *kf2_param_get(struct kf2_params_t *kf2p, char *name)
{
    return strdup(kf2p->S.GetValue(name).c_str());
}

void kf2_param_start(struct kf2_params_t *kf2p)
{
    kf2p->S->StartSetting();
}

int kf2_param_set(struct kf2_params_t *kf2p, char *name, char *value)
{
    return !kf2p->S->SetValue(name, value);
}

int kf2_params_end(struct kf2_params_t *kf2p)
{
    return kf2p->S->FinishSetting();
}


extern "C" int kf2_params_set_text(struct kf2_t *kf2, const char *text)
{
  assert(kf2);
  assert(kf2->state == kf2_state_idle);
  assert(kf2->g_SFT);
  const bool settings_ok = kf2->g_SFT->SetSettings(std::string(text));
  const bool parameter_ok = kf2->g_SFT->OpenString(std::string(text));
  return settings_ok && parameter_ok;
}

extern "C" size_t kf2_params_get_text(const struct kf2_t *kf2, char *buffer, size_t bytes)
{
  assert(kf2);
  assert(kf2->state == kf2_state_idle);
  assert(kf2->g_SFT);
  const std::string parameter = kf2->g_SFT->ToText();
  const std::string settings = kf2->g_SFT->GetSettings();
  const std::string data = parameter + settings;
  size_t bytes_required = std::strlen(data.c_str()) + 1;
  if (buffer)
  {
    if (bytes_required <= bytes)
    {
      memcpy(buffer, data.c_str(), bytes_required);
      return bytes_required;
    }
    else
    {
      return 0;
    }
  }
  else
  {
    return bytes_required;
  }
}


struct kf2_progress_t
{
  int state;
  int64_t iterations;
  int64_t reference;
  int64_t approximation;
  int64_t pixels;
  int64_t good_guessed;
  int64_t good;
  int64_t todo;
  int64_t bad;
  int64_t bad_guessed;
  int pass;
};

extern "C" void kf2_progress(const struct kf2_t *kf2, kf2_progress_cb progress_cb, void *arg)
{
  assert(kf2);
  assert(kf2->g_SFT);
  assert(progress_cb);
  int64_t iters = kf2->g_SFT->m_nMaxIter;
  if ((kf2->g_SFT->GetUseNanoMB1() || kf2->g_SFT->GetUseNanoMB2()) && kf2->g_SFT->m_bAutoGlitch == 1) iters = kf2->g_SFT->N.g_period;
  if (iters <= 0) iters = 1;
  struct kf2_progress_t progress =
    { kf2->g_SFT->m_state
    , iters
    , kf2->g_SFT->m_nRDone
    , kf2->g_SFT->m_nApprox
    , kf2->g_SFT->m_nX * (int64_t) kf2->g_SFT->m_nY
    , kf2->g_SFT->m_count_good_guessed
    , kf2->g_SFT->m_count_good
    , 0
    , kf2->g_SFT->m_count_bad
    , kf2->g_SFT->m_count_bad_guessed
    , kf2->g_SFT->m_bAutoGlitch
    };
  progress.todo = progress.pixels - (progress.good_guessed + progress.good + progress.bad + progress.bad_guessed);
  progress_cb(arg, &progress);
}

extern "C" int kf2_progress_get_state(const struct kf2_progress_t *progress)
{
  assert(progress);
  return progress->state;
}

extern "C" int kf2_progress_get_pass(const struct kf2_progress_t *progress)
{
  assert(progress);
  return progress->pass;
}

extern "C" int64_t kf2_progress_get_iterations(const struct kf2_progress_t *progress)
{
  assert(progress);
  return progress->iterations;
}

extern "C" int64_t kf2_progress_get_reference(const struct kf2_progress_t *progress)
{
  assert(progress);
  return progress->reference;
}

extern "C" int64_t kf2_progress_get_approximation(const struct kf2_progress_t *progress)
{
  assert(progress);
  return progress->approximation;
}

extern "C" int64_t kf2_progress_get_pixels(const struct kf2_progress_t *progress)
{
  assert(progress);
  return progress->pixels;
}

extern "C" int64_t kf2_progress_get_good_guessed(const struct kf2_progress_t *progress)
{
  assert(progress);
  return progress->good_guessed;
}

extern "C" int64_t kf2_progress_get_good(const struct kf2_progress_t *progress)
{
  assert(progress);
  return progress->good;
}

extern "C" int64_t kf2_progress_get_todo(const struct kf2_progress_t *progress)
{
  assert(progress);
  return progress->todo;
}

extern "C" int64_t kf2_progress_get_bad(const struct kf2_progress_t *progress)
{
  assert(progress);
  return progress->bad;
}

extern "C" int64_t kf2_progress_get_bad_guessed(const struct kf2_progress_t *progress)
{
  assert(progress);
  return progress->bad_guessed;
}

struct kf2_image_t
{
  CFraktalSFT *g_SFT;
};

extern "C" void kf2_image(const struct kf2_t *kf2, kf2_image_cb image_cb, void *arg)
{
  assert(kf2);
  assert(kf2->g_SFT);
  struct kf2_image_t image = { kf2->g_SFT };
  image_cb(arg, &image);
}

extern "C" int64_t kf2_image_get_width(const struct kf2_image_t *image)
{
  assert(image);
  assert(image->g_SFT);
  return image->g_SFT->GetImageWidth();
}

extern "C" int64_t kf2_image_get_height(const struct kf2_image_t *image)
{
  assert(image);
  assert(image->g_SFT);
  return image->g_SFT->GetImageHeight();
}

const uint32_t *kf2_image_get_n0 (const struct kf2_image_t *image, ptrdiff_t *stride_x, ptrdiff_t *stride_y)
{
  assert(image);
  assert(image->g_SFT);
  assert(stride_x);
  assert(stride_y);
  assert(stride_x != stride_y);
  if (image->g_SFT->m_nPixels_LSB)
  {
    *stride_x = image->g_SFT->m_nY;
    *stride_y = 1;
    return image->g_SFT->m_nPixels_LSB;
  }
  else
  {
    return nullptr;
  }
}

const uint32_t *kf2_image_get_n1 (const struct kf2_image_t *image, ptrdiff_t *stride_x, ptrdiff_t *stride_y)
{
  assert(image);
  assert(image->g_SFT);
  assert(stride_x);
  assert(stride_y);
  assert(stride_x != stride_y);
  if (image->g_SFT->m_nPixels_MSB)
  {
    *stride_x = image->g_SFT->m_nY;
    *stride_y = 1;
    return image->g_SFT->m_nPixels_MSB;
  }
  else
  {
    return nullptr;
  }
}

const float    *kf2_image_get_nf (const struct kf2_image_t *image, ptrdiff_t *stride_x, ptrdiff_t *stride_y)
{
  assert(image);
  assert(image->g_SFT);
  assert(stride_x);
  assert(stride_y);
  assert(stride_x != stride_y);
  if (image->g_SFT->m_nTrans && image->g_SFT->m_nTrans[0])
  {
    *stride_x = &image->g_SFT->m_nTrans[1][0] - &image->g_SFT->m_nTrans[0][0];
    *stride_y = &image->g_SFT->m_nTrans[0][1] - &image->g_SFT->m_nTrans[0][0];
    return &image->g_SFT->m_nTrans[0][0];
  }
  else
  {
    return nullptr;
  }
}

const float    *kf2_image_get_dex(const struct kf2_image_t *image, ptrdiff_t *stride_x, ptrdiff_t *stride_y)
{
  assert(image);
  assert(image->g_SFT);
  assert(stride_x);
  assert(stride_y);
  assert(stride_x != stride_y);
  if (image->g_SFT->GetDerivatives() && image->g_SFT->m_nDEx && image->g_SFT->m_nDEx[0])
  {
    *stride_x = &image->g_SFT->m_nDEx[1][0] - &image->g_SFT->m_nDEx[0][0];
    *stride_y = &image->g_SFT->m_nDEx[0][1] - &image->g_SFT->m_nDEx[0][0];
    return &image->g_SFT->m_nDEx[0][0];
  }
  else
  {
    return nullptr;
  }
}

const float    *kf2_image_get_dey(const struct kf2_image_t *image, ptrdiff_t *stride_x, ptrdiff_t *stride_y)
{
  assert(image);
  assert(image->g_SFT);
  assert(stride_x);
  assert(stride_y);
  assert(stride_x != stride_y);
  if (image->g_SFT->GetDerivatives() && image->g_SFT->m_nDEy && image->g_SFT->m_nDEy[0])
  {
    *stride_x = &image->g_SFT->m_nDEy[1][0] - &image->g_SFT->m_nDEy[0][0];
    *stride_y = &image->g_SFT->m_nDEy[0][1] - &image->g_SFT->m_nDEy[0][0];
    return &image->g_SFT->m_nDEy[0][0];
  }
  else
  {
    return nullptr;
  }
}

const float    *kf2_image_get_t(const struct kf2_image_t *image, ptrdiff_t *stride_x, ptrdiff_t *stride_y)
{
  assert(image);
  assert(image->g_SFT);
  assert(stride_x);
  assert(stride_y);
  assert(stride_x != stride_y);
  if (image->g_SFT->m_nPhase && image->g_SFT->m_nPhase[0])
  {
    *stride_x = &image->g_SFT->m_nPhase[1][0] - &image->g_SFT->m_nPhase[0][0];
    *stride_y = &image->g_SFT->m_nPhase[0][1] - &image->g_SFT->m_nPhase[0][0];
    return &image->g_SFT->m_nPhase[0][0];
  }
  else
  {
    return nullptr;
  }
}

extern "C" const half *kf2_image_get_half_rgb(const struct kf2_image_t *image, ptrdiff_t *stride_x, ptrdiff_t *stride_y, ptrdiff_t *stride_c)
{
  assert(image);
  assert(image->g_SFT);
  assert(stride_x);
  assert(stride_y);
  assert(stride_c);
  assert(stride_x != stride_y);
  assert(stride_x != stride_c);
  assert(stride_y != stride_c);
  if (image->g_SFT->m_imageHalf)
  {
    *stride_x = 3;
    *stride_y = 3 * image->g_SFT->m_nX;
    *stride_c = 1;
    return image->g_SFT->m_imageHalf;
  }
  else
  {
    return nullptr;
  }
}

extern "C" const uint8_t *kf2_image_get_byte_rgb(const struct kf2_image_t *image, ptrdiff_t *stride_x, ptrdiff_t *stride_y, ptrdiff_t *stride_c)
{
  assert(image);
  assert(image->g_SFT);
  assert(stride_x);
  assert(stride_y);
  assert(stride_c);
  assert(stride_x != stride_y);
  assert(stride_x != stride_c);
  assert(stride_y != stride_c);
  if (image->g_SFT->m_lpBits)
  {
    *stride_x = 4; // FIXME verify
    *stride_y = image->g_SFT->m_row; // FIXME verify
    *stride_c = -1;
    return image->g_SFT->m_lpBits + 2; // FIXME check format spec, is it B,G,R,A or A,B,G,R ?
  }
  else
  {
    return nullptr;
  }
}
