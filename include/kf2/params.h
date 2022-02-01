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

#ifndef KF2_SETTINGS_H
#define KF2_SETTINGS_H

#include <stddef.h>
#include <stdint.h>

#include "kf2.h"

enum ParamType {
    KF_is_Setting = 0,  // general
    KF_is_Param = 1,    // Fractal specific, excluding location
    KF_is_Location = 2, // location
};

enum ParamFlags {
    KF_use_Settings = 1,
    KF_use_Params = 2,
    KF_use_Location = 4,
    KF_use_all = 7,

    KF_reset_Params = 256,

    // reserved: flags from 1<<16 are used in Settings.h
};


#ifdef __cplusplus
extern "C"
{
#endif

struct kf2_param_info_t {
    const char type;   
    const char flags;   
    const char *const ctype;
    const char *const name;
    const char *const initial;
    const char *const descr;
};

/*

version information:
- macros say which version was used at compile time
- function says which version was used at run time

*/

struct kf2_params_t;

struct kf2_params_t *kf2_get_params(kf2_t *) __attribute__ ((visibility ("default") ));

struct kf2_params_t *kf2_default_params(void) __attribute__ ((visibility ("default") ));

int kf2_apply_params(struct kf2_t * kf2, struct kf2_params_t *kf2p) __attribute__ ((visibility ("default") ));

void kf2_param_free(struct kf2_params_t *kf2p) __attribute__ ((visibility ("default") ));

/*
Enumerate possible parameters. Start with zero. Returns NULL when at the end.
*/

struct kf2_param_info_t *kf2_param_info(int pos) __attribute__ ((visibility ("default") ));
// static

/*
returns / sets a single parameter value
*/
char *kf2_param_get(struct kf2_params_t *kf2p, char *name) __attribute__ ((visibility ("default") ));
// Caller frees


void kf2_params_start(struct kf2_params_t *kf2p);

int kf2_param_set(struct kf2_params_t *kf2p, char *name, char *value) __attribute__ ((visibility ("default") ));
// name / value are not consumed

int kf2_params_end(struct kf2_params_t *kf2p);

/*
set complete set of parameters. Format is the same as .KFR file:
NAME: VALUE\r\n
*/
int kf2_params_set_text(struct kf2_params_t *kf2p, const char *text) __attribute__ ((visibility ("default") ));

/*
get complete set of parameters
if passed NULL, returns buffer size needed
otherwise returns number of bytes written, including terminating NUL
*/

size_t kf2_params_get_text(const struct kf2_params_t *kf2, char *buffer, size_t bytes) __attribute__ ((visibility ("default") ));

#ifdef __cplusplus
}
#endif


#endif // KF2_SETTINGS_H
