// MIT License

// Copyright (c) 2016-2021 Luis Lloret

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
#include <vector>
#include <algorithm>
#include "sp_midi.h"
#include "midiinputs.h"
#include "midiout.h"
#include "midiin.h"
#include "midisendprocessor.h"
#include "version.h"
#include "utils.h"
#include "monitorlogger.h"
#include "midi_port_info.h"

using namespace std;

static int g_monitor_level = 6;

// FIXME: need to test what happens when MIDI devices are already in use by another application
// and sp_midi cannot open them
// MIDI out
std::unique_ptr<MidiSendProcessor> midiSendProcessor;

// Threading
std::atomic<bool> g_threadsShouldFinish { false };

static ErlNifPid midi_process_pid;

static atomic<bool> g_already_initialized { false };

void prepareMidiSendProcessorOutputs(unique_ptr<MidiSendProcessor>& midiSendProcessor)
{
    // Open all MIDI devices. This is what Sonic Pi does
    vector<MidiPortInfo> connectedOutputPortsInfo = MidiOut::getOutputPortInfo();
    {
        midiSendProcessor->prepareOutputs(connectedOutputPortsInfo);
    }
}

int sp_midi_open_all_inputs()
{
    // Prepare the MIDI inputs
    try{
        MidiInputs::getInstance().prepareMidiInputs(true, vector<string>());
    } catch (const std::out_of_range&) {
        cout << "Error opening MIDI inputs" << endl;
        return -1;
    }
    return 0;
}

int sp_midi_open_some_inputs(char **input_names, int len)
{
    vector<string> selected_midi_inputs;    
    for (int i = 0; i < len; i++){
        selected_midi_inputs.push_back(input_names[i]);
    }
    try{
        MidiInputs::getInstance().prepareMidiInputs(false, selected_midi_inputs);
    } catch (const std::out_of_range&) {
        cout << "Error opening MIDI inputs" << endl;
        return -1;
    }
    return 0;
}



// This was used for some timing tests. Leave it here for reference, in case it is useful in the future
struct timestamp {
    char type;
    int id;
    long long t;
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


int sp_midi_send(const char* device_name, const unsigned char* c_message, unsigned int size)
{
    midiSendProcessor->addMessage(device_name, c_message, size);

    return 0;
}

int sp_midi_init()
{
    if (g_already_initialized){
        return 0;
    }
    g_already_initialized = true;
    g_threadsShouldFinish = false;
    MonitorLogger::getInstance().setLogLevel(g_monitor_level);

    midiSendProcessor = make_unique<MidiSendProcessor>();
    // Prepare the MIDI outputs
    try {
        prepareMidiSendProcessorOutputs(midiSendProcessor);
    } catch (const std::out_of_range&) {
        cout << "Error opening MIDI outputs" << endl;
        return -1;
    }

    midiSendProcessor->startThread();

    return 0;
}

void sp_midi_deinit()
{
    if (!g_already_initialized){
        return;
    }
    g_already_initialized = false;
    //output_time_stamps();

    // We tell the threads that we are going to exit
    g_threadsShouldFinish = true;

    // We give them some time to exit
    std::this_thread::sleep_for(std::chrono::milliseconds(1000));
    
    // And we stop them
    MidiInputs::getInstance().clear();
    midiSendProcessor.reset(nullptr);
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
    auto outputs = MidiOut::getNormalizedOutputNames();
    char **c_str_list = vector_str_to_c(outputs);
    *n_list = (int)outputs.size();
    return c_str_list;
}

char **sp_midi_all_ins(int *n_list)
{
    auto inputs = MidiIn::getNormalizedInputNames();
    char **c_str_list = vector_str_to_c(inputs);
    *n_list = (int)inputs.size();
    return c_str_list;
}

char **sp_midi_selected_ins(int *n_list)
{
    auto inputs = MidiInputs::getInstance().getSelectedMidiInputs();
    char **c_str_list = vector_str_to_c(inputs);
    *n_list = (int)inputs.size();
    return c_str_list;
}

long long sp_midi_get_current_time_microseconds()
{
    auto now = chrono::high_resolution_clock::now();
    auto duration = now.time_since_epoch();
    long long micros = std::chrono::duration_cast<std::chrono::microseconds>(duration).count();
    return micros;
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

ERL_NIF_TERM sp_midi_open_all_inputs_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret = sp_midi_open_all_inputs();
    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM sp_midi_open_some_inputs_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int len;
    int ret = enif_get_list_length(env, argv[0], &len);
    if (!ret){
        return enif_make_badarg(env);
    }
    char **c_str_list = (char **)malloc(len * sizeof(char*));

    const ERL_NIF_TERM *list = &argv[0];
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail;
    char device_name[256];
    int i = 0;
    while (enif_get_list_cell(env, *list, &head, &tail)){
        int str_len = enif_get_string(env, head, device_name, 256, ERL_NIF_LATIN1);
        if (!str_len){
            return enif_make_badarg(env);
        }  
        c_str_list[i] = (char*)malloc(str_len * sizeof(char));
        strcpy(c_str_list[i], device_name);
        list = &tail;
        i++;
    }

    ret = sp_midi_open_some_inputs(c_str_list, len);
    for (unsigned int j = 0; j < len; j++) {
        free(c_str_list[j]);
    }
    free(c_str_list);

    return enif_make_atom(env, "ok");
}


ERL_NIF_TERM sp_midi_send_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    char device_name[256];

    int ret = enif_get_string(env, argv[0], device_name, 256, ERL_NIF_LATIN1);
    if (!ret){
        return enif_make_badarg(env);
    }

    ret = enif_inspect_binary(env, argv[1], &bin);
    if (!ret){
        return enif_make_badarg(env);
    }

    const unsigned char *c_message = bin.data;
    int size = (int)bin.size;

    int rc = sp_midi_send(device_name, c_message, size);
    if (rc != 0){
        return enif_make_atom(env, "warning");
    }
    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM sp_midi_flush_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    midiSendProcessor->flushMessages();
    return enif_make_atom(env, "ok");
}


ERL_NIF_TERM sp_midi_outs_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int n_midi_outs;
    char **midi_outs = sp_midi_outs(&n_midi_outs);
    return c_str_list_to_erlang(env, n_midi_outs, midi_outs);
}

ERL_NIF_TERM sp_midi_all_ins_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int n_midi_ins;
    char **midi_ins = sp_midi_all_ins(&n_midi_ins);
    return c_str_list_to_erlang(env, n_midi_ins, midi_ins);
}

ERL_NIF_TERM sp_midi_selected_ins_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int n_midi_ins;
    char **midi_ins = sp_midi_selected_ins(&n_midi_ins);
    return c_str_list_to_erlang(env, n_midi_ins, midi_ins);
}

ERL_NIF_TERM sp_midi_refresh_devices(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        MidiInputs::getInstance().prepareMidiInputs();
    } catch (const std::out_of_range&) {
        std::cout << "Error opening MIDI inputs" << std::endl;
        return enif_make_atom(env, "error");
    }

    try {
        prepareMidiSendProcessorOutputs(midiSendProcessor);
    } catch (const std::out_of_range&) {
        std::cout << "Error opening MIDI outputs" << std::endl;
        return enif_make_atom(env, "error");
    }
    return enif_make_atom(env, "ok");
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


ERL_NIF_TERM sp_midi_set_log_level_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int rc = enif_get_int(env, argv[0], &g_monitor_level);
    MonitorLogger::getInstance().setLogLevel(g_monitor_level);
    return enif_make_atom(env, (rc ? "ok" : "error"));
}


ERL_NIF_TERM sp_midi_get_current_time_microseconds_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int64(env, sp_midi_get_current_time_microseconds());
}

int send_midi_data_to_erlang(const char *device_name, const unsigned char *data, size_t size)
{
    ErlNifEnv *msg_env = enif_alloc_env();
    ERL_NIF_TERM term1;
    ERL_NIF_TERM term2;
    ERL_NIF_TERM term3;
    ERL_NIF_TERM term4;

    term1 = enif_make_atom(msg_env, "midi_in");
    term2 = enif_make_string(msg_env, device_name, ERL_NIF_LATIN1);
    unsigned char *term_bin = enif_make_new_binary(msg_env, size, &term3);
    memcpy(term_bin, data, size);

    term4 = enif_make_tuple3(msg_env, term1, term2, term3);
    int rc = enif_send(NULL, &midi_process_pid, msg_env, term4);
    enif_free_env(msg_env);
    return rc;
}


static ErlNifFunc nif_funcs[] = {
    {"midi_init", 0, sp_midi_init_nif},
    {"midi_deinit", 0, sp_midi_deinit_nif},
    {"midi_open_all_inputs", 0, sp_midi_open_all_inputs_nif},
    {"midi_open_some_inputs", 1, sp_midi_open_some_inputs_nif},
    {"midi_send", 2, sp_midi_send_nif},
    {"midi_flush", 0, sp_midi_flush_nif},
    {"midi_outs", 0, sp_midi_outs_nif},
    {"midi_all_ins", 0, sp_midi_all_ins_nif},
    {"midi_selected_ins", 0, sp_midi_selected_ins_nif},
    {"midi_refresh_devices", 0, sp_midi_refresh_devices},
    {"have_my_pid", 0, sp_midi_have_my_pid_nif},
    {"set_this_pid", 1, sp_midi_set_this_pid_nif},
    {"set_log_level", 1, sp_midi_set_log_level_nif},
    {"get_current_time_microseconds", 0, sp_midi_get_current_time_microseconds_nif}
};

ERL_NIF_INIT(sp_midi, nif_funcs, NULL, NULL, NULL, NULL);
