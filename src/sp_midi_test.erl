cd("d:/projects/sp_midi/src).
c(sp_midi).

Aon = list_to_binary("/*/note_on").
Mon = <<Aon/binary, <<0, 0, 44, 105, 105, 105, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 64, 0, 0, 0, 100>>/binary >>.
Aoff = list_to_binary("/*/note_off").
Moff = << Aoff / binary, <<0, 44, 105, 105, 105, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 64, 0, 0, 0, 100>>/binary >> .

sp_midi:midi_init().

sp_midi:midi_send(Mon).

