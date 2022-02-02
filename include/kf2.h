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

#ifndef KF2_H
#define KF2_H 1

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
class half; /* e.g. implemented in #include <OpenEXR/half.h> */
extern "C"
{
#else
struct half;
typedef struct half half;
#endif

/*

version information:
- macros say which version was used at compile time
- function says which version was used at run time

*/

#define KF2_VERSION_MAJOR 2
#define KF2_VERSION_MINOR 16
#define KF2_VERSION_REVISION 0
#define KF2_VERSION_PATCH 0
#define KF2_VERSION_STRING "2.16"

void kf2_get_version(int *major, int *minor, int *revision, int *patch) __attribute__ ((visibility ("default") ));
const char *kf2_get_version_string(void) __attribute__ ((visibility ("default") ));

/*

operations are not thread safe
i.e. if using multiple threads for one kf2_t you must use your own lock

kf2_t state machine:

old state | action   | new state | return value
---------:|----------|:----------|:--------------
START     | new      | idle      | -
idle      | delete   | END       | -
idle      | start    | rendering | -
idle      | stop     | idle      | 0
idle      | wait     | idle      | 0
rendering | stop     | stopping  | 1, render thread is still running
rendering | stop     | idle      | 0, render thread has terminated
rendering | wait     | idle      | 0 if the render was stopped, otherwise 1
stopping  | wait     | idle      | 0
*         | progress | (unchanged)
*         | image    | (unchanged)
*         | *        | ERROR

ERROR is typically program abort via assertion failure.

A render thread may also be stopped if you update settings values which affect it.
*/

struct kf2_t;

struct kf2_t *kf2_new(void) __attribute__ ((visibility ("default") ));

void kf2_delete(struct kf2_t *kf2) __attribute__ ((visibility ("default") ));

void kf2_start(struct kf2_t *kf2) __attribute__ ((visibility ("default") ));

int kf2_stop(struct kf2_t *kf2) __attribute__ ((visibility ("default") ));

int kf2_wait(struct kf2_t *kf2) __attribute__ ((visibility ("default") ));

/*
log callbacks can be called:
- at any time
- from any thread
- simultaneously
use your own mutexes
*/

enum KF2_LogLevel
{
  KF2_Log_Trace = 1,
  KF2_Log_Debug = 2,
  KF2_Log_Status = 3,
  KF2_Log_Info = 4,
  KF2_Log_Warn = 5,
  KF2_Log_Error = 6,
  KF2_Log_Fatal = 7
};

typedef void (*kf2_log_cb_t)(void *arg, enum KF2_LogLevel level, const char *message);

void kf2_set_log_cb(struct kf2_t *kf2, kf2_log_cb_t log_cb, void *arg) __attribute__ ((visibility ("default") ));

enum KF2_Event {
    KF2_Event_RenderStart = 1, // starts the first pass
    KF2_Event_RenderStep = 2,  // starts the next pass
    KF2_Event_RenderRef = 3,   // Reference calculation done
    KF2_Event_RenderCalc = 5,  // raw image data calculated
    KF2_Event_RenderColor = 7, // image pixels colored
    KF2_Event_RenderDone = 8,  // done, image complete
    KF2_Event_RenderAbort = 9, // render thread stopped

    KF2_Event_NeedRender = 16, // needs re-rendering

    KF2_Event_Shift = 8,       // event# is &((1<<KF2_Event_Shift)-1)

};

typedef void (*kf2_event_cb_t)(void *user, unsigned int evt, intptr_t param);

void kf2_set_event_cb(struct kf2_t *kf2, kf2_event_cb_t event_cb, void *arg) __attribute__ ((visibility ("default") ));


/*
access progress data
the kf2_progress_t pointer is valid only during the callback
*/

struct kf2_progress_t;

typedef void (*kf2_progress_cb)(void *arg, const struct kf2_progress_t *progress);

void kf2_progress(const struct kf2_t *kf2, kf2_progress_cb progress_cb, void *arg) __attribute__ ((visibility ("default") ));

/*
current state
*/
#define KF2_STATE_IDLE 0
#define KF2_STATE_REFERENCE 1
#define KF2_STATE_APPROXIMATION 2
#define KF2_STATE_PERTURBATION 3
#define KF2_STATE_GLITCH 4
#define KF2_STATE_COLOURING 5
int kf2_progress_get_state(const struct kf2_progress_t *progress) __attribute__ ((visibility ("default") ));

/*
iteration counts
*/
int64_t kf2_progress_get_iterations(const struct kf2_progress_t *progress) __attribute__ ((visibility ("default") ));
int64_t kf2_progress_get_reference(const struct kf2_progress_t *progress) __attribute__ ((visibility ("default") ));
int64_t kf2_progress_get_approximation(const struct kf2_progress_t *progress) __attribute__ ((visibility ("default") ));

/*
pixel counts
*/
int64_t kf2_progress_get_pixels(const struct kf2_progress_t *progress) __attribute__ ((visibility ("default") ));
int64_t kf2_progress_get_good_guessed(const struct kf2_progress_t *progress) __attribute__ ((visibility ("default") ));
int64_t kf2_progress_get_good(const struct kf2_progress_t *progress) __attribute__ ((visibility ("default") ));
int64_t kf2_progress_get_todo(const struct kf2_progress_t *progress) __attribute__ ((visibility ("default") ));
int64_t kf2_progress_get_bad(const struct kf2_progress_t *progress) __attribute__ ((visibility ("default") ));
int64_t kf2_progress_get_bad_guessed(const struct kf2_progress_t *progress) __attribute__ ((visibility ("default") ));

/*
0 for single reference method
+1 for each additional reference used when glitch correcting
*/
int kf2_progress_get_pass(const struct kf2_progress_t *progress) __attribute__ ((visibility ("default") ));

/*
access image data
the kf2_image_t pointer is valid only during the callback
*/

struct kf2_image_t;

typedef void (*kf2_image_cb)(void *arg, const struct kf2_image_t *image);

void kf2_image(const struct kf2_t *kf2, kf2_image_cb image_cb, void *arg) __attribute__ ((visibility ("default") ));

int64_t kf2_image_get_width(const struct kf2_image_t *image) __attribute__ ((visibility ("default") ));
int64_t kf2_image_get_height(const struct kf2_image_t *image) __attribute__ ((visibility ("default") ));

/*
returns NULL if data plane does not exist in image
stride pointers must be valid
pixel data is at ptr[stride_x * x + stride_y * y]
for RGB only, c is 0 for R, 1 for G, 2 for B; and
channel data is at ptr[stride_x * x + stride_y * y + stride_c * c]
*/
const uint32_t *kf2_image_get_n0 (const struct kf2_image_t *image, ptrdiff_t *stride_x, ptrdiff_t *stride_y) __attribute__ ((visibility ("default") ));
const uint32_t *kf2_image_get_n1 (const struct kf2_image_t *image, ptrdiff_t *stride_x, ptrdiff_t *stride_y) __attribute__ ((visibility ("default") ));
const float    *kf2_image_get_nf (const struct kf2_image_t *image, ptrdiff_t *stride_x, ptrdiff_t *stride_y) __attribute__ ((visibility ("default") ));
const float    *kf2_image_get_t  (const struct kf2_image_t *image, ptrdiff_t *stride_x, ptrdiff_t *stride_y) __attribute__ ((visibility ("default") ));
const float    *kf2_image_get_dex(const struct kf2_image_t *image, ptrdiff_t *stride_x, ptrdiff_t *stride_y) __attribute__ ((visibility ("default") ));
const float    *kf2_image_get_dey(const struct kf2_image_t *image, ptrdiff_t *stride_x, ptrdiff_t *stride_y) __attribute__ ((visibility ("default") ));
const half     *kf2_image_get_half_rgb(const struct kf2_image_t *image, ptrdiff_t *stride_x, ptrdiff_t *stride_y, ptrdiff_t *strice_c) __attribute__ ((visibility ("default") ));
const uint8_t  *kf2_image_get_byte_rgb(const struct kf2_image_t *image, ptrdiff_t *stride_x, ptrdiff_t *stride_y, ptrdiff_t *stride_c) __attribute__ ((visibility ("default") ));

#ifdef __cplusplus
} // extern "C"
#endif

#endif
