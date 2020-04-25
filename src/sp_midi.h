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

    // TODO: These are exported for C tests. Once we are happy that it's working they should not be exported
    DllExport int sp_midi_init();
    DllExport void sp_midi_deinit();
    DllExport void sp_midi_send(const char *c_message, unsigned int size);
    DllExport char **sp_midi_outs(int *n_list);
    DllExport char **sp_midi_ins(int *n_list);

    // Aux helper function
    ERL_NIF_TERM c_str_list_to_erlang(int n_midi_outs, ErlNifEnv* env, char** midi_outs);
    
    // Erlang NIFs
    DllExport ERL_NIF_TERM sp_midi_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    DllExport ERL_NIF_TERM sp_midi_deinit_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    DllExport ERL_NIF_TERM sp_midi_send_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);	
	DllExport ERL_NIF_TERM sp_midi_outs_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    DllExport ERL_NIF_TERM sp_midi_ins_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#ifdef __cplusplus
}
#endif

