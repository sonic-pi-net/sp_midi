-module(sp_midi).
-export([midi_init/0, midi_deinit/0, midi_send/1, midi_ins/0, midi_outs/0, have_my_pid/0]).
-on_load(init/0).

init() ->
    case os:type() of
    {win32, _} ->
        ok = erlang:load_nif("D:/projects/sp_midi/build/Debug/libsp_midi", 0);
    _Else ->
        ok = erlang:load_nif("/home/luis/projects/sp_midi/build/libsp_midi", 0)
    end.

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
have_my_pid() ->
    exit(nif_library_not_loaded).

