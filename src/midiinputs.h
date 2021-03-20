#pragma once

#include <vector>
#include <memory>

#include "midiin.h"

class MidiInputs {
public:
    MidiInputs(MidiInputs const&) = delete;
    void operator=(MidiInputs const&) = delete;

    inline static MidiInputs& getInstance() {
        static MidiInputs instance;
        return instance;
    }

    // This version is the one that we configure on the main path
    void prepareMidiInputs(bool allMidiInputs, const std::vector<std::string> &selectedMidiInputs);

    // This version is the one we execute when we are asked to refresh the devices
    void prepareMidiInputs();

    void clear();

    const std::vector<std::string>& getSelectedMidiInputs();


private:
    MidiInputs(): m_wantAllMidiInputs(false){}

    // MIDI in
    std::vector<std::unique_ptr<MidiIn> > m_midiInputs;
    bool m_wantAllMidiInputs;
    std::vector<std::string> m_selectedMidiInputs;
};
