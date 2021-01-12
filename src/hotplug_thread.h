#pragma once
#include <vector>
#include <memory>
#include <string>
#include "midiin.h"
#include "midisendprocessor.h"

extern std::atomic<bool> g_threadsShouldFinish;

// FIXME: this should go into a header file
void prepareMidiInputs(std::vector<std::unique_ptr<MidiIn> >& midiInputs);
extern std::vector<std::unique_ptr<MidiIn> > midiInputs;
void prepareMidiSendProcessorOutputs(std::unique_ptr<MidiSendProcessor>& midiSendProcessor);
extern std::unique_ptr<MidiSendProcessor> midiSendProcessor;

class HotPlugThread
{
public:
    ~HotPlugThread()
    {
        if (m_thread.joinable()){
            m_thread.join();
        }
    }

    void startThread(){
        m_thread = std::thread(&HotPlugThread::run, this);
    }

    void run()
    {
        std::vector<std::string> lastAvailableInputPorts = MidiIn::getInputNames();
        std::vector<std::string> lastAvailableOutputPorts = MidiOut::getOutputNames();
        while (!g_threadsShouldFinish){
            std::this_thread::sleep_for(std::chrono::milliseconds(500));
            auto newAvailableInputPorts = MidiIn::getInputNames();
            // Was something added or removed?
            if (newAvailableInputPorts != lastAvailableInputPorts) {
                prepareMidiInputs(midiInputs);
                lastAvailableInputPorts = newAvailableInputPorts;
            }

            auto newAvailableOutputPorts = MidiOut::getOutputNames();
            // Was something added or removed?
            if (newAvailableOutputPorts != lastAvailableOutputPorts) {
                prepareMidiSendProcessorOutputs(midiSendProcessor);
                lastAvailableOutputPorts = newAvailableOutputPorts;
            }

        }
    }
private:
    std::thread m_thread;
};
