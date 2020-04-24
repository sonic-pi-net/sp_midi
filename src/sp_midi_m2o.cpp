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
#include <iostream>
#include "midiin.h"
#include "oscout.h"
#include "midiinprocessor.h"
#include "osc/OscOutboundPacketStream.h"
#include "version.h"
#include "utils.h"

using namespace std;

// -b -o -m

// The forever living variables that the C API will access
// midiInputProcessors will contain the list of active MidiIns at a given time
static vector<unique_ptr<MidiInProcessor> > midiInputProcessors;

// oscOutputs will contain the list of active OSC output ports
static vector<shared_ptr<OscOutput> > oscOutputs;

void listAvailablePorts()
{
    auto inputs = MidiIn::getInputNames();
    cout << "Found " << inputs.size() << " MIDI inputs." << endl;
    for (unsigned int i = 0; i < inputs.size(); i++) {
        cout << "   (" << i << "): " << inputs[i] << endl;
    }
}

struct ProgramOptions {
    vector<string> midiInputNames;
    bool allMidiInputs;
    string oscOutputHost;
    vector<int> oscOutputPorts;
    bool useOscTemplate;
    string oscTemplate;
    bool oscRawMidiMessage;
    bool oscHeartbeat;
    bool useVirtualPort;
    string virtualPortName;
    unsigned int monitor;
    bool listPorts;
};

void showVersion()
{
    cout << "m2o version " << M2O_VERSION << endl;
}


void prepareMidiProcessors(vector<unique_ptr<MidiInProcessor> >& midiInputProcessors, vector<shared_ptr<OscOutput> >& oscOutputs)
{
    // Should we open all devices, or just the ones passed as parameters?
    vector<string> midiInputsToOpen = MidiIn::getInputNames();

    for (auto& input : midiInputsToOpen) {
        try {
            auto midiInputProcessor = make_unique<MidiInProcessor>(input, oscOutputs, false);
            midiInputProcessors.push_back(std::move(midiInputProcessor));
        } catch (const std::out_of_range&) {
            cout << "The device " << input << " does not exist";
            throw;
        }
    }
}

static std::atomic<bool> g_wantToExit(false);


void sendHeartBeat(const vector<unique_ptr<MidiInProcessor> >& midiProcessors, const vector<shared_ptr<OscOutput> >& oscOutputs)
{
    char buffer[2048];
    osc::OutboundPacketStream p(buffer, 2048);
    p << osc::BeginMessage("/m2o/heartbeat");
    for (const auto& midiProcessor : midiProcessors) {

      p << midiProcessor->getInputId() << midiProcessor->getInputPortname().c_str() << midiProcessor->getInputNormalizedPortName().c_str();
    }
    p << osc::EndMessage;
    MonitorLogger::getInstance().debug("sending OSC: [/m2o/heartbeat] -> ");
    for (const auto& midiProcessor : midiProcessors) {
        MonitorLogger::getInstance().debug("   {}, {}", midiProcessor->getInputId(), midiProcessor->getInputPortname());
    }

    for (auto& output : oscOutputs) {
        output->sendUDP(p.Data(), p.Size());
        local_utils::logOSCMessage(p.Data(), p.Size());
    }
}

int m2o_sp_init()
{
    MonitorLogger::getInstance().setLogLevel(6);

    // Open the OSC output port
    auto oscOutput = make_shared<OscOutput>("127.0.0.1", 4562);
    oscOutputs.push_back(std::move(oscOutput));

    // Will configure logging on the first OSC port (may want to change this in the future, so that it sends to every port, or be able to select one)
    MonitorLogger::getInstance().setOscOutput(oscOutputs[0]);

// Create the virtual output port?
#ifndef WIN32
    unique_ptr<MidiInProcessor> virtualIn;
    if (popts.useVirtualPort) {
        virtualIn = make_unique<MidiInProcessor>(popts.virtualPortName, oscOutputs, true);
    }
#endif

    // Open the MIDI input ports
    try {
        prepareMidiProcessors(midiInputProcessors, oscOutputs);
    } catch (const std::out_of_range&) {
        return -1;
    }

    // For hotplugging
    vector<string> lastAvailablePorts = MidiIn::getInputNames();
    while (!g_wantToExit) {
        std::chrono::milliseconds timespan(1000);
        std::this_thread::sleep_for(timespan);
        vector<string> newAvailablePorts = MidiIn::getInputNames();
        // Was something added or removed?
        if (newAvailablePorts != lastAvailablePorts) {
            midiInputProcessors.clear();
            prepareMidiProcessors(midiInputProcessors, popts, oscOutputs);
            lastAvailablePorts = newAvailablePorts;
            listAvailablePorts();
        }
        if (popts.oscHeartbeat)
            sendHeartBeat(midiInputProcessors, oscOutputs);
    }
}
