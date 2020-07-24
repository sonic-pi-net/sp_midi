// MIT License

// Copyright (c) 2016 Luis Lloret

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

#include <iostream>
#include "sp_midi.h"
#include "midiin.h"
#include "osc/OscOutboundPacketStream.h"
#include "utils.h"

using namespace std;
using namespace juce;

MidiIn::MidiIn(const string& portName, bool isVirtual) : m_oscRawMidiMessage(false)
{
    m_logger.debug("MidiIn constructor for {}", portName);
    updateMidiDevicesNamesMapping();
    m_portName = portName;
    m_normalizedPortName = portName;
    local_utils::safeOscString(m_normalizedPortName);

    if (!nameInStickyTable(m_portName))
        m_stickyId = addNameToStickyTable(m_portName);
    else
        m_stickyId = getStickyIdFromName(m_portName);

    // FIXME: need to check if name does not exist
    if (!isVirtual) {
        m_rtMidiId = getRtMidiIdFromName(m_portName);
        m_midiIn = make_unique<RtMidiIn>();
        m_midiIn->openPort(m_rtMidiId);
        m_midiIn->ignoreTypes( false, false, false );
    }
// TODO: do the virtual ports
#if 0    
    else {
#ifndef WIN32
        m_logger.trace("*** Creating new MIDI device: ", m_portName);
        m_midiIn = MidiInput::createNewDevice(m_portName, midiInputCallback);
#else
        m_logger.error("Virtual MIDI ports are not supported on Windows");
        exit(-1);
#endif
    }
#endif

    m_midiIn->setCallback(MidiIn::staticMidiCallback, this);
}

MidiIn::~MidiIn()
{
    m_logger.trace("MidiIn destructor for {}", m_portName);
    m_midiIn->closePort();
    //m_midiIn->stop();
}


void MidiIn::staticMidiCallback(double timeStamp, std::vector< unsigned char > *midiMessage, void *userData)
{    
    cout << "MIDI CALLBACK!!" << endl;
    MidiIn *midiIn = (MidiIn *)userData;
    midiIn->midiCallback(timeStamp, midiMessage);
}

void MidiIn::midiCallback(double timeStamp, std::vector< unsigned char > *midiMessage)
{
    lock_guard<mutex> lock(m_cb_mutex);
    unsigned char channel = 0xff, status = 0;
    string message_type;
    const uint8_t* message = midiMessage->data();
    int nBytes = midiMessage->size();

    assert(nBytes > 0);

    if ((message[0] & 0xf0) != 0xf0) {
        channel = message[0] & 0x0f;
        channel++; // Make channel 1-16, instead of 0-15
        status = message[0] & 0xf0;
    } else {
        status = message[0];
    }

    m_logger.info("received MIDI message: ");
    for (int i = 0; i < nBytes; i++) {
        m_logger.info("   [{:02x}]", (unsigned int)message[i]);
    }

    // Process the message
    switch (status) {
    case 0x80:
        message_type = "note_off";
        assert(nBytes == 3);
        break;

    case 0x90:
        message_type = "note_on";
        assert(nBytes == 3);
        break;

    case 0xA0:
        message_type = "polyphonic_key_pressure";
        assert(nBytes == 3);
        break;

    case 0xB0:
        message_type = "control_change";
        assert(nBytes == 3);
        break;

    case 0xC0:
        message_type = "program_change";
        assert(nBytes == 2);
        break;

    case 0xD0:
        message_type = "channel_pressure";
        assert(nBytes == 2);
        break;

    case 0xE0:
        message_type = "pitch_bend";
        assert(nBytes == 3);
        break;

    case 0xF0:
        message_type = "sysex";
        // Remove the end of message marker if raw message is not specified
        if (!m_oscRawMidiMessage)
            nBytes--;
        break;

    case 0xF1:
        message_type = "MTC";
        assert(nBytes == 2);
        break;

    case 0xF2:
        message_type = "song_position";
        assert(nBytes == 3);
        break;

    case 0xF3:
        message_type = "song_select";
        assert(nBytes == 2);
        break;

    case 0xF4:
    case 0xF5:
        message_type = "syscommon_undefined";
        assert(nBytes == 1);
        break;

    case 0xF6:
        message_type = "tune_request";
        assert(nBytes == 1);
        break;

    case 0xF8:
        message_type = "clock";
        assert(nBytes == 1);
        break;

    case 0xF9:
    case 0xFD:
        message_type = "sysrt_undefined";
        assert(nBytes == 1);
        break;

    case 0xFA:
        message_type = "start";
        assert(nBytes == 1);
        break;

    case 0xFB:
        message_type = "continue";
        assert(nBytes == 1);
        break;

    case 0xFC:
        message_type = "stop";
        assert(nBytes == 1);
        break;

    case 0xFE:
        message_type = "active_sensing";
        assert(nBytes == 1);
        break;

    default:
        message_type = "unknown_message";
        break;
    }

    // Prepare the OSC address
    stringstream path;
    string normalizedPortName(getNormalizedPortName());
    int portId = getPortId();
    path << "/midi:" << normalizedPortName << ":" << portId << ":";
    if (channel != 0xff) {
        path << static_cast<int>(channel);
    }
    path << "/" << message_type;

    // And now prepare the OSC message body
    char buffer[1024];
    osc::OutboundPacketStream p(buffer, 1024);
    p << osc::BeginMessage(path.str().c_str());

    // send the raw midi message as part of the body
    // TODO: do we want a raw midi message?
    if (m_oscRawMidiMessage) {
        if (nBytes > 0) {
            p << osc::Blob(message, static_cast<osc::osc_bundle_element_size_t>(nBytes));
        }
    } else {
        // We treat the pitch bend differently. Instead of sending the bytes separately,
        // we send the processed 14 bits value        
        if (message[0] & 0xf0 == 0xe0){
            p << (int)(message[1] | (message[2] << 7));
        }
        else {
            for (int i = 1; i < nBytes; i++) {
                p << (int)message[i];
            }
        }
    }
    p << osc::EndMessage;

    // Dump the OSC message
    m_logger.info("sending OSC: [{}] -> {}, {}", path.str(), portId, normalizedPortName);
    if (m_oscRawMidiMessage) {
        if (nBytes > 0) {
            m_logger.info("  <raw_midi_message>");
        }
    } else {
        for (int i = 1; i < nBytes; i++) {
            m_logger.info("   [{:02x}]", (int)message[i]);
        }
    }

    // And send the message to the erlang process
    send_midi_osc_to_erlang(p.Data(), p.Size());
}



vector<string> MidiIn::getInputNames()
{
    RtMidiIn ins;
    int nPorts = ins.getPortCount();
    vector<string> names(nPorts);

    for (int i = 0; i < nPorts; i++) {
        names[i] = ins.getPortName(i);
    }
    return names;
}

void MidiIn::updateMidiDevicesNamesMapping()
{
    m_midiRtMidiIdToName = MidiIn::getInputNames();
    for (int i = 0; i < m_midiRtMidiIdToName.size(); i++) {
        m_midiNameToRtMidiId[m_midiRtMidiIdToName[i]] = i;
    }
}
