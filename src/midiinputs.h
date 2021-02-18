#pragma once

#include <vector>
#include <memory>

#include "midiin.h"

class MidiInputs {
public:
    MidiInputs(MidiInputs const&) = delete;
    void operator=(MidiInputs const&) = delete;

    inline static MidiInputs& getInstance()
    {
        static MidiInputs instance;
        return instance;
    }


    // This version is the one that we configure on the main path
    void prepareMidiInputs(bool allMidiInputs, const std::vector<std::string> &selectedMidiInputs)
    {
        m_wantAllMidiInputs = allMidiInputs;
        m_selectedMidiInputs = selectedMidiInputs;
        prepareMidiInputs();
    }

    // This version is the one we execute on the hotplug thread
    void prepareMidiInputs()
    {    
        std::vector<MidiPortInfo> allInputPortsInfo = MidiIn::getInputPortInfo();

        m_midiInputs.clear();
        for (const auto& input : allInputPortsInfo) {
            const auto& v = m_selectedMidiInputs; // for brevity's sake
            if (m_wantAllMidiInputs || std::find(v.begin(), v.end(), input.normalizedPortName) != v.end()){
                try {
                    auto midiInput = std::make_unique<MidiIn>(input.portName, input.normalizedPortName, input.portId, false);
                    m_midiInputs.push_back(std::move(midiInput));
                } catch (const RtMidiError& e) {
                    std::cout << "Could not open input device " << input.portName << ": " << e.what() << std::endl;
                    //throw;
                }
            }
        }
    }

    const std::vector<std::string>& getSelectedMidiInputs(){
        return m_selectedMidiInputs;
    } 


private:
    MidiInputs(): m_wantAllMidiInputs(false)
    {
        
    }

    // MIDI in
    std::vector<std::unique_ptr<MidiIn> > m_midiInputs;
    bool m_wantAllMidiInputs;
    std::vector<std::string> m_selectedMidiInputs;
};
