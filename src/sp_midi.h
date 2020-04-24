#pragma once

#ifdef WIN32
    #define DllExport   __declspec( dllexport )
#else
    #define DllExport
#endif

#ifdef __cplusplus
extern "C" {
#endif

    DllExport int sp_midi_init();
    DllExport int sp_midi_send(const char *c_message, unsigned int size);

#ifdef __cplusplus
}
#endif

