-module(sp_midi_test).
-export([start/0, midi_process/0, test_get_current_time_microseconds/2, test_scheduler_callback_process/0]).


midi_process() ->
    %sp_midi:have_my_pid(),

    receive
        <<Midi_event/binary>> ->
            io:format("Received midi_in message~n->~p~n", [Midi_event]);
        _ ->
            io:format("Received something (not a binary)~n")

    end,
    midi_process().

test_scheduler_callback_process() ->
    receive
        X when is_integer(X) ->
            Treceived = sp_midi:get_current_time_microseconds(),
            io:fwrite("Received callback message   : ~p~n", [X]),
            io:fwrite("Received at                 : ~p~n", [Treceived]);
        _ ->
            io:fwrite("Received something else~n")
    end,
    test_scheduler_callback_process().


test_get_current_time_microseconds(0, _) ->
    done;
test_get_current_time_microseconds(Count, SleepMillis) ->
    T = sp_midi:get_current_time_microseconds(),
    io:fwrite("Time in microseconds: ~p~n", [T]),
    timer:sleep(SleepMillis),
    test_get_current_time_microseconds(Count-1, SleepMillis).


start() ->
%    cd("d:/projects/sp_midi/src").
    compile:file(sp_midi),

    io:fwrite("Testing NIF function to return current time in microseconds. The values should be around 1000 miliseconds away~n"),
    test_get_current_time_microseconds(3, 1000),

    Aon = binary:list_to_bin("/*/note_on"),
    Mon = <<Aon/binary, <<0, 0, 44, 105, 105, 105, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 64, 0, 0, 0, 100>>/binary >>,
    Aoff = binary:list_to_bin("/*/note_off"),
    Moff = << Aoff/binary, <<0, 44, 105, 105, 105, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 64, 0, 0, 0, 100>>/binary >>,

    sp_midi:midi_init(),

    PidTestSchedulerCallback = spawn(sp_midi_test, test_scheduler_callback_process, []),
    io:fwrite("PidTestSchedulerCallback ~p~n", [PidTestSchedulerCallback]),
    T = sp_midi:get_current_time_microseconds(),
    Tcallback = T + 3000000,
    io:fwrite("Time now is                 : ~p~n", [T]),
    io:fwrite("Want to fire the callback at: ~p~n", [Tcallback]),
    sp_midi:schedule_callback(Tcallback, PidTestSchedulerCallback, 42),
    sp_midi:schedule_callback(Tcallback + 10000, PidTestSchedulerCallback, 43), 
    sp_midi:schedule_callback(Tcallback + 20000, PidTestSchedulerCallback, 44), 
    sp_midi:schedule_callback(Tcallback - 10000, PidTestSchedulerCallback, 45), 

    Pid = spawn(sp_midi_test, midi_process, []),
    sp_midi:set_this_pid(Pid),

    INS = sp_midi:midi_ins(),
    OUTS = sp_midi:midi_outs(),

    %io:fwrite("MIDI INs:~p~n", [INS]),

    %io:fwrite("MIDI OUTs:~p~n", [OUTS]),
    
    io:fwrite("Sending note ON and waiting 3 seconds~n"),
    sp_midi:midi_send(Mon),

    timer:sleep(3000),

    io:fwrite("Sending note OFF~n"),
    sp_midi:midi_send(Moff),


    timer:sleep(10000),

    sp_midi:midi_deinit().
