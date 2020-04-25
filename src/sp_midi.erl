-module(sp_midi).
-export([midi_init/0, midi_deinit/0, midi_send/1, midi_ins/0, midi_outs/0]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("D:/projects/sp_midi/build/Debug/sp_midi", 0).

midi_init() ->
    exit(nif_library_not_loaded).
midi_deinit() ->
    exit(nif_library_not_loaded).
midi_send(_X) ->
    exit(nif_library_not_loaded).
midi_ins() ->
    exit(nif_library_not_loaded).
midi_outs() ->
    exit(nif_library_not_loaded).

