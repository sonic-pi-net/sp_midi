#pragma once

#include <iostream>

#include "../JuceLibraryCode/JuceHeader.h"

class OscMessageThread : public Thread
{
public:
  OscMessageThread() : Thread("message thread"), ready(false) {
      setPriority(10);
  };

    void run() override
    {
        message_manager = MessageManager::getInstance();
        message_manager->setCurrentThreadAsMessageThread();
        ready = true;
        message_manager->runDispatchLoop();
    }

    ~OscMessageThread()
    {
        MessageManager::deleteInstance();
    }

    void stopDispatchLoop()
    {
        message_manager->stopDispatchLoop();
    }

    bool isReady() {
        return ready;
    }

    // TODO: make private after testing
    MessageManager* message_manager;
private:
    std::atomic<bool> ready;

};

