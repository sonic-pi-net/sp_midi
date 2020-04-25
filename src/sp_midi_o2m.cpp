#include "sp_midi.h"
// MIT License

// Copyright (c) 2016-2020 Luis Lloret

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include <stdexcept>
#include <chrono>
#include <iostream>
#include <atomic>
#include "sp_midi.h"
#include "midiout.h"
#include "midiin.h"
#include "oscin.h"
#include "oscout.h"
#include "oscinprocessor.h"
#include "osc/OscOutboundPacketStream.h"
#include "version.h"
#include "utils.h"
#include "monitorlogger.h"

static const int MONITOR_LEVEL = 0;

static std::unique_ptr<OscInProcessor> oscInputProcessor;

using namespace std;

static mutex g_oscinMutex;

static void prepareOscProcessorOutputs(unique_ptr<OscInProcessor>& oscInputProcessor)
{
    // Open all MIDI devices. This is what Sonic Pi does
    vector<string> midiOutputsToOpen = MidiOut::getOutputNames();
    {
        lock_guard<mutex> lock(g_oscinMutex);
        oscInputProcessor->prepareOutputs(midiOutputsToOpen);
    }
}


void sp_midi_send(const char* c_message, unsigned int size)
{
    oscInputProcessor->ProcessMessage(c_message, size);
}

int sp_midi_init()
{
    MonitorLogger::getInstance().setLogLevel(MONITOR_LEVEL);

     oscInputProcessor = make_unique<OscInProcessor>();
    // Prepare the OSC input and MIDI outputs
    try {
        prepareOscProcessorOutputs(oscInputProcessor);
    } catch (const std::out_of_range&) {
        cout << "Error opening MIDI outputs" << endl;
        return -1;
    }
    return 0;
}

void sp_midi_deinit()
{
    oscInputProcessor.reset(nullptr);
}

static char **vector_str_to_c(const vector<string>& vector_str)
{
    char **c_str_list;

    c_str_list = (char **)malloc(vector_str.size() * sizeof(char*));
    for (int i = 0; i < vector_str.size(); i++) {
        c_str_list[i] = (char*)malloc((vector_str[i].size() + 1) * sizeof(char));
        strcpy(c_str_list[i], vector_str[i].c_str());
    }

    return c_str_list;
}

char **sp_midi_outs(int *n_list)
{
    auto outputs = MidiOut::getOutputNames();
    char **c_str_list = vector_str_to_c(outputs);
    *n_list = outputs.size();
    return c_str_list;
}

char **sp_midi_ins(int *n_list)
{
    auto inputs = MidiIn::getInputNames();
    char **c_str_list = vector_str_to_c(inputs);
    *n_list = inputs.size();
    return c_str_list;
}


// NIF functions
ERL_NIF_TERM sp_midi_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret = sp_midi_init();
    return enif_make_int(env, ret);
}

ERL_NIF_TERM sp_midi_deinit_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    sp_midi_deinit();
    return enif_make_int(env, 0);
}

ERL_NIF_TERM sp_midi_send_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{    
    ErlNifBinary bin;
    int ret = enif_inspect_binary(env, argv[0], &bin);
    if (!ret)
    {
        return enif_make_int(env, 1);
    }
    const char *c_message = (char *)bin.data;
    int size = bin.size;

    sp_midi_send(c_message, size);
    return enif_make_int(env, 0);
}

ERL_NIF_TERM c_str_list_to_erlang(ErlNifEnv* env, int n, char **c_str_list)
{
    ERL_NIF_TERM *terms = (ERL_NIF_TERM*)malloc(n * sizeof(ERL_NIF_TERM));
    for (int i = 0; i < n; i++) {
        terms[i] = enif_make_string(env, c_str_list[i], ERL_NIF_LATIN1);
    }

    ERL_NIF_TERM string_array = enif_make_list_from_array(env, terms, n);

    for (int i = 0; i < n; i++) {
        free(c_str_list[i]);
    }
    free(c_str_list);
    free(terms);

    return string_array;
}

ERL_NIF_TERM sp_midi_outs_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int n_midi_outs;    
    char **midi_outs = sp_midi_outs(&n_midi_outs);
    return c_str_list_to_erlang(env, n_midi_outs, midi_outs);
}

ERL_NIF_TERM sp_midi_ins_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int n_midi_ins;
    char **midi_ins = sp_midi_ins(&n_midi_ins);
    return c_str_list_to_erlang(env, n_midi_ins, midi_ins);
}


static ErlNifFunc nif_funcs[] = {
    {"midi_init", 0, sp_midi_init_nif},
    {"midi_deinit", 0, sp_midi_deinit_nif},
    {"midi_send", 1, sp_midi_send_nif},
    {"midi_outs", 0, sp_midi_outs_nif},
    {"midi_ins", 0, sp_midi_ins_nif}
};

ERL_NIF_INIT(sp_midi, nif_funcs, NULL, NULL, NULL, NULL);