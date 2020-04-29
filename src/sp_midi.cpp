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
#include "message_thread.h"
#include "midiout.h"
#include "midiin.h"
#include "oscinprocessor.h"
#include "midiinprocessor.h"
#include "osc/OscOutboundPacketStream.h"
#include "version.h"
#include "utils.h"
#include "monitorlogger.h"

static int g_monitor_level = 6;

using namespace std;

// MIDI out
static std::unique_ptr<OscInProcessor> oscInputProcessor;

// MIDI in
static vector<unique_ptr<MidiInProcessor> > midiInputProcessors;

OscMessageThread *msg_thread = nullptr;


static ErlNifPid midi_process_pid;

static mutex g_oscinMutex;

static atomic<bool> g_already_initialized = false;

static void prepareOscProcessorOutputs(unique_ptr<OscInProcessor>& oscInputProcessor)
{
    // Open all MIDI devices. This is what Sonic Pi does
    vector<string> midiOutputsToOpen = MidiOut::getOutputNames();
    {
        lock_guard<mutex> lock(g_oscinMutex);
        oscInputProcessor->prepareOutputs(midiOutputsToOpen);
    }
}


void prepareMidiProcessors(vector<unique_ptr<MidiInProcessor> >& midiInputProcessors)
{
    // Should we open all devices, or just the ones passed as parameters?
    vector<string> midiInputsToOpen = MidiIn::getInputNames();

    for (const auto& input : midiInputsToOpen) {
        try {
            auto midiInputProcessor = make_unique<MidiInProcessor>(input, false);
            midiInputProcessors.push_back(std::move(midiInputProcessor));
        } catch (const std::out_of_range&) {
            cout << "The device " << input << " does not exist";
            throw;
        }
    }
}


struct timestamp {
    char type;
    int id;
    int64 t;
};

vector<timestamp> timestamps;

void print_time_stamp(char type)
{
    static int id_A = 0;
    static int id_B = 0;
    static int id_C = 0;
    auto now = chrono::high_resolution_clock::now();
    auto duration = now.time_since_epoch();
    auto micros = std::chrono::duration_cast<std::chrono::microseconds>(duration).count();
    timestamp ts{type, (type == 'A' ? id_A++ : type == 'B'? id_B++ : id_C++), micros};
    timestamps.push_back(ts);
}

void output_time_stamps()
{
    for (auto ts : timestamps) {
        cout << ts.type << "," << ts.id << "," << ts.t << endl;
    }
}

void sp_midi_send(const char* c_message, unsigned int size)
{
    print_time_stamp('A');
    // This calls the ProcessMessage asynchronously on the message manager, which has its own thread
    msg_thread->callAsync([c_message, size]() {
      oscInputProcessor->ProcessMessage(c_message, size);
    });
}

int sp_midi_init()
{
    if (g_already_initialized){
        return 0;
    }
    g_already_initialized = true;
    MonitorLogger::getInstance().setLogLevel(g_monitor_level);

    oscInputProcessor = make_unique<OscInProcessor>();
    // Prepare the MIDI outputs
    try {
        prepareOscProcessorOutputs(oscInputProcessor);
    } catch (const std::out_of_range&) {
        cout << "Error opening MIDI outputs" << endl;
        return -1;
    }

    // Prepare the MIDI inputs
    try{
        prepareMidiProcessors(midiInputProcessors);
    } catch (const std::out_of_range&) {
        cout << "Error opening MIDI inputs" << endl;
        return -1;
    }

    msg_thread = new OscMessageThread;
    msg_thread->startThread();
        
    while (!msg_thread->isReady());

    return 0;
}

void sp_midi_deinit()
{
    if (!g_already_initialized){
        return;
    }
    g_already_initialized = false;
    //output_time_stamps();
    msg_thread->stopDispatchLoop();
    bool rc = msg_thread->stopThread(500);
    delete msg_thread;
    oscInputProcessor.reset(nullptr);    
    midiInputProcessors.clear();
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
    *n_list = (int)outputs.size();
    return c_str_list;
}

char **sp_midi_ins(int *n_list)
{
    auto inputs = MidiIn::getInputNames();
    char **c_str_list = vector_str_to_c(inputs);
    *n_list = (int)inputs.size();
    return c_str_list;
}



// NIF helper functions
ERL_NIF_TERM c_str_list_to_erlang(ErlNifEnv *env, int n, char **c_str_list)
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


// NIF functions
ERL_NIF_TERM sp_midi_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret = sp_midi_init();
    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM sp_midi_deinit_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    sp_midi_deinit();
    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM sp_midi_send_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{    
    ErlNifBinary bin;
    int ret = enif_inspect_binary(env, argv[0], &bin);
    if (!ret)
    {
        return enif_make_badarg(env);
    }
    const char *c_message = (char *)bin.data;
    int size = (int)bin.size;
        
    sp_midi_send(c_message, size);
    return enif_make_atom(env, "ok");
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

ERL_NIF_TERM sp_midi_have_my_pid_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_self(env, &midi_process_pid)){
        return enif_make_badarg(env);        
    }
    return enif_make_atom(env, "ok");
}


ERL_NIF_TERM sp_midi_set_this_pid_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_pid(env, argv[0])){
        return enif_make_badarg(env);
    }

    int rc = enif_get_local_pid(env, argv[0], &midi_process_pid);
    return enif_make_atom(env, (rc ? "ok" : "error"));
}


ERL_NIF_TERM sp_midi_set_log_level(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int rc = enif_get_int(env, argv[0], &g_monitor_level);
    MonitorLogger::getInstance().setLogLevel(g_monitor_level);
    return enif_make_atom(env, (rc ? "ok" : "error"));
}


ERL_NIF_TERM sp_midi_get_current_time_microseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    auto now = chrono::high_resolution_clock::now();
    auto duration = now.time_since_epoch();
    int64 micros = std::chrono::duration_cast<std::chrono::microseconds>(duration).count();
    return enif_make_int64(env, micros);
}

int send_midi_osc_to_erlang(const char *data, size_t size)
{
    ErlNifEnv *msg_env = enif_alloc_env();
    ERL_NIF_TERM term;
    unsigned char *term_bin = enif_make_new_binary(msg_env, size, &term);
    memcpy(term_bin, data, size);

    int rc = enif_send(NULL, &midi_process_pid, msg_env, term);
    enif_free_env(msg_env);
    return rc;
}


static ErlNifFunc nif_funcs[] = {
    {"midi_init", 0, sp_midi_init_nif},
    {"midi_deinit", 0, sp_midi_deinit_nif},
    {"midi_send", 1, sp_midi_send_nif},
    {"midi_outs", 0, sp_midi_outs_nif},
    {"midi_ins", 0, sp_midi_ins_nif},
    {"have_my_pid", 0, sp_midi_have_my_pid_nif},
    {"set_this_pid", 1, sp_midi_set_this_pid_nif},
    {"set_log_level", 1, sp_midi_set_log_level},
    {"get_current_time_microseconds", 0, sp_midi_get_current_time_microseconds}
};

ERL_NIF_INIT(sp_midi, nif_funcs, NULL, NULL, NULL, NULL);