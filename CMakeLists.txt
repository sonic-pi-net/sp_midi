project (sp_midi)
cmake_minimum_required (VERSION 3.0)

set(CMAKE_VERBOSE_MAKEFILE ON)

if(NOT MSVC)
    if(APPLE)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -stdlib=libc++")
    else(APPLE)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
    endif(APPLE)
endif(NOT MSVC)

if(APPLE)

elseif(UNIX)
    find_package(ALSA REQUIRED)
endif(APPLE)


set(oscpack_path ${PROJECT_SOURCE_DIR}/external_libs/oscpack_1_1_0)
add_subdirectory(${oscpack_path})
include_directories(${oscpack_path} ${PROJECT_SOURCE_DIR}/external_libs/spdlog-0.11.0/include JuceLibraryCode JuceLibraryCode/modules  ${PROJECT_SOURCE_DIR}/external_libs/cxxopts)

set(sp_midi_sources
    src/sp_midi_o2m.cpp
    src/midiin.cpp
    src/midiout.cpp
    src/midiinprocessor.cpp
    src/midicommon.cpp
    src/oscinprocessor.cpp
    src/utils.cpp
)



if(APPLE)
    set(juce_sources
        JuceLibraryCode/include_juce_audio_basics.mm
        JuceLibraryCode/include_juce_audio_devices.mm
        JuceLibraryCode/include_juce_audio_formats.mm
        JuceLibraryCode/include_juce_core.mm
        JuceLibraryCode/include_juce_data_structures.mm
        JuceLibraryCode/include_juce_events.mm
        JuceLibraryCode/include_juce_osc.cpp
    )
else(APPLE)
    set(juce_sources
        JuceLibraryCode/include_juce_audio_basics.cpp
        JuceLibraryCode/include_juce_audio_devices.cpp
        JuceLibraryCode/include_juce_audio_formats.cpp
        JuceLibraryCode/include_juce_core.cpp
        JuceLibraryCode/include_juce_data_structures.cpp
        JuceLibraryCode/include_juce_events.cpp
        JuceLibraryCode/include_juce_osc.cpp
    )
endif(APPLE)

# sp_midi_sources
add_library(sp_midi SHARED ${sp_midi_sources} ${juce_sources})
target_link_libraries(sp_midi oscpack)

add_definitions(-DJUCE_ALSA_MIDI_NAME="sp_midi")

if(MSVC)
    add_definitions(-D_WIN32_WINNT=0x0600 -DJUCER_VS2015_78A5022=1 -DJUCE_APP_VERSION=1.0.0 -DJUCE_APP_VERSION_HEX=0x10000)
    include_directories("C:/Program Files/erl10.7/usr/include")
    target_link_libraries(sp_midi winmm)
elseif(APPLE)
    add_definitions(-DNDEBUG=1 -DJUCER_XCODE_MAC_F6D2F4CF=1 -DJUCE_APP_VERSION=1.0.0 -DJUCE_APP_VERSION_HEX=0x10000)
    set_target_properties(sp_midi PROPERTIES XCODE_ATTRIBUTE_CLANG_LINK_OBJC_RUNTIME "NO")
    set(CMAKE_EXE_LINKER_FLAGS "-framework CoreMIDI -framework CoreAudio -framework CoreFoundation -framework Accelerate -framework QuartzCore -framework AudioToolbox -framework IOKit -framework DiscRecording -framework Cocoa")
elseif(UNIX)
    add_definitions(-DLINUX=1 -DNDEBUG=1 -DJUCER_LINUX_MAKE_6D53C8B4=1 -DJUCE_APP_VERSION=1.0.0 -DJUCE_APP_VERSION_HEX=0x10000)
    target_link_libraries(sp_midi pthread ${ALSA_LIBRARY} dl rt X11)
endif(MSVC)

# example test exe
add_executable(sp_midi_test src/sp_midi_test.c)
target_link_libraries(sp_midi_test sp_midi)

