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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include <kf2.h>

void log_cb(void *arg, int level, const char *message)
{
  const char *levelname = "UNKNOWN";
  switch (level)
  {
    case KF2_LOGLEVEL_DEBUG:  levelname = "DEBUG "; break;
    case KF2_LOGLEVEL_STATUS: levelname = "STATUS"; break;
    case KF2_LOGLEVEL_INFO:   levelname = "INFO  "; break;
    case KF2_LOGLEVEL_WARN:   levelname = "WARN  "; break;
    case KF2_LOGLEVEL_ERROR:  levelname = "ERROR "; break;
  }
  fprintf(stderr, "%s %s %s\n", (const char *) arg, levelname, message);
}

void progress_cb(void *arg, const struct kf2_progress_t *progress)
{
  int pass = kf2_progress_get_pass(progress);
  int64_t iterations = kf2_progress_get_iterations(progress);
  double ref = kf2_progress_get_reference(progress) * 100.0 / iterations;
  double apx = kf2_progress_get_reference(progress) * 100.0 / iterations;
  int64_t pixels = kf2_progress_get_pixels(progress);
  int64_t todo = kf2_progress_get_todo(progress);
  double ptb = (pixels - todo) * 100.0 / pixels;
  fprintf
    ( stderr
    , "%s %d [R%3.1f] [A%3.1f] [P%3.1f] "
      "(%3.1f %3.1f %3.1f %3.1f %3.1f)\n"
    , (const char *) arg, pass, ref, apx, ptb
    , kf2_progress_get_good_guessed(progress) * 100.0 / pixels
    , kf2_progress_get_good(progress) * 100.0 / pixels
    , todo * 100.0 / pixels
    , kf2_progress_get_bad(progress) * 100.0 / pixels
    , kf2_progress_get_bad_guessed(progress) * 100.0 / pixels
    );
}

void image_cb(void *arg, const struct kf2_image_t *image)
{
  (void) arg;
  int width = kf2_image_get_width(image);
  int height = kf2_image_get_height(image);
  ptrdiff_t x = 0, y = 0, c = 0;
  const uint8_t *pixels = kf2_image_get_byte_rgb(image, &x, &y, &c);
  assert(pixels);
  printf("%d %d\n255\n", width, height);
  size_t bytes = 3 * (size_t) width * (size_t) height;
  uint8_t *ppm = malloc(bytes);
  assert(ppm);
  for (int j = 0; j < height; ++j)
  {
    for (int i = 0; i < width; ++i)
    {
      for (int k = 0; k < 3; ++k)
      {
        ppm[3 * (width * j + i) + k] = pixels[j * y + i * x + k * c];
      }
    }
  }
  fwrite(ppm, bytes, 1, stdout);
  free(ppm);
}

int main(int argc, char **argv)
{
  (void) argc;
  struct kf2_t *kf2 = kf2_new();
  if (! kf2)
  {
    return 1;
  }
  kf2_set_log_cb(kf2, log_cb, argv[0]);
  kf2_set_text(kf2,
    "Re: -0.75\r\n"
    "Im: 0.0\r\n"
    "Zoom: 1.5\r\n"
    "Iterations: 1000\r\n"
    "ImageWidth: 1024\r\n"
    "ImageHeight: 576\r\n"
  );
  // render
  kf2_start(kf2);
  while (kf2_wait(kf2, 250000000))
  {
    kf2_progress(kf2, progress_cb, argv[0]);
  }
  // save PPM
  printf("P6\n");
  // embed KFR+KFS in PPM comment
  size_t bytes = kf2_get_text(kf2, NULL, 0);
  if (bytes > 0)
  {
    char *buffer = malloc(bytes);
    if (buffer)
    {
      if (bytes == kf2_get_text(kf2, buffer, bytes))
      {
        int linestart = 1;
        for (int i = 0; buffer[i]; ++i)
        {
          if (linestart)
          {
            putchar('#');
            putchar(' ');
            linestart = 0;
          }
          putchar(buffer[i]);
          if (buffer[i] == '\n')
          {
            linestart = 1;
          }
        }
        if (linestart == 0)
        {
          putchar('\n');
        }
      }
      free(buffer);
    }
  }
  // save raster
  kf2_image(kf2, image_cb, argv[0]);
  fflush(stdout);
  kf2_delete(kf2);
  return 0;
}
