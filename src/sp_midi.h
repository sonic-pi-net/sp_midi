#pragma once

#include <erl_nif.h>

#ifdef WIN32
    #define DllExport   __declspec( dllexport )
#else
    #define DllExport
#endif

#ifdef __cplusplus
extern "C" {
#endif

    DllExport int sp_midi_init();
    DllExport void sp_midi_deinit();

    DllExport void sp_midi_send(const char *c_message, unsigned int size);

    DllExport ERL_NIF_TERM sp_midi_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    DllExport ERL_NIF_TERM sp_midi_deinit_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    DllExport ERL_NIF_TERM sp_midi_send_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#ifdef __cplusplus
}
#endif

