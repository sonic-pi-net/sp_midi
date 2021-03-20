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
#include "midiinputs.h"

// This version is the one that we configure on the main path
void MidiInputs::prepareMidiInputs(bool allMidiInputs, const std::vector<std::string> &selectedMidiInputs)
{
    m_wantAllMidiInputs = allMidiInputs;
    m_selectedMidiInputs = selectedMidiInputs;
    prepareMidiInputs();
}

// This version is the one we execute when we are asked to refresh the devices
void MidiInputs::prepareMidiInputs()
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

void MidiInputs::clear()
{
    m_midiInputs.clear();
}

const std::vector<std::string>& MidiInputs::getSelectedMidiInputs(){
    return m_selectedMidiInputs;
} 
