-module(sp_midi_test).
-export([start/0]).

start() ->
%    cd("d:/projects/sp_midi/src").
    compile:file(sp_midi),


    Aon = binary:list_to_bin("/*/note_on"),
    Mon = <<Aon/binary, <<0, 0, 44, 105, 105, 105, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 64, 0, 0, 0, 100>>/binary >>,
    Aoff = binary:list_to_bin("/*/note_off"),
    Moff = << Aoff/binary, <<0, 44, 105, 105, 105, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 64, 0, 0, 0, 100>>/binary >>,

    sp_midi:midi_init(),

    io:fwrite("Sending note ON and waiting 3 seconds~n"),
    sp_midi:midi_send(Mon),

    timer:sleep(3000),

    io:fwrite("Sending note OFF~n"),
    sp_midi:midi_send(Moff),

    sp_midi:midi_deinit().
