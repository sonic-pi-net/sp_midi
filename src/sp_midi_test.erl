-module(sp_midi_test).
-export([start/0, midi_process/0]).


midi_process() ->
    sp_midi:have_my_pid(),

    receive
        <<Midi_event/binary>> ->
            io:format("Received midi_in message~n->~p~n", [Midi_event]);
        _ ->
            io:format("Received something (not a binary)~n")

    end,
    midi_process().


start() ->
%    cd("d:/projects/sp_midi/src").
    compile:file(sp_midi),

    Aon = binary:list_to_bin("/*/note_on"),
    Mon = <<Aon/binary, <<0, 0, 44, 105, 105, 105, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 64, 0, 0, 0, 100>>/binary >>,
    Aoff = binary:list_to_bin("/*/note_off"),
    Moff = << Aoff/binary, <<0, 44, 105, 105, 105, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 64, 0, 0, 0, 100>>/binary >>,

    sp_midi:midi_init(),

    spawn(sp_midi_test, midi_process, []),

    INS = sp_midi:midi_ins(),
    OUTS = sp_midi:midi_outs(),

    io:fwrite("MIDI INs:~p~n", [INS]),

    io:fwrite("MIDI OUTs:~p~n", [OUTS]),
    
    io:fwrite("Sending note ON and waiting 3 seconds~n"),
    sp_midi:midi_send(Mon),

    timer:sleep(3000),

    io:fwrite("Sending note OFF~n"),
    sp_midi:midi_send(Moff),


    timer:sleep(10000),

    sp_midi:midi_deinit().
