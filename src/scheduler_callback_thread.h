#pragma once

#include <erl_nif.h>
#include <tuple>
#include <set>

#include "../JuceLibraryCode/JuceHeader.h"

// FIXME: this should go into a header file
extern int64 sp_midi_get_current_time_microseconds();


// FIXME: make sure this is thread-safe (i.e. protect the m_pending_messages accesses)
class SchedulerCallbackThread : public Thread
{
    using TupleTimePidInteger = std::tuple<int64, ErlNifPid, int64>;
public:
    SchedulerCallbackThread() : Thread("scheduler callback thread") { };

    void run() override
    {
        int64 next_timer_shot = 0;
        while (!threadShouldExit()){
            // If there are no pending messages, do nothing
            if (m_pending_messages.empty()) {
                wait(10);
                continue;
            }

            // When is the next message due?
            auto first_pending_message = m_pending_messages.cbegin();
            next_timer_shot = first_pending_message->get_time();

            // How far is it in the future?
            int64 current_time = sp_midi_get_current_time_microseconds();
            int64 time_to_next_message_millis =
                (next_timer_shot - current_time) / 1000;

            // If it is less than 1ms away or in the past, fire it
            if (time_to_next_message_millis < 1) {
                time_to_next_message_millis = LONG_MAX;
                ErlNifPid pid = first_pending_message->get_pid();
                send_integer_to_erlang_process(pid, first_pending_message->get_integer());
                // ... and remove it from the pending messages
                m_pending_messages.erase(first_pending_message);
            }
            
            // Wait for 10 ms if there is nothing happening soon, or as necessary if there is something less than 10ms away
            wait(jmin(static_cast<int64>(10), time_to_next_message_millis));
        }
    }

    void trigger_callback_at(int64 when, ErlNifPid pid, int64 integer)
    {   
        TupleTimePidIntegerWrapper element(when, pid, integer);        
        m_pending_messages.insert(element);
    }

private:

    class TupleTimePidIntegerWrapper {
    public:
        TupleTimePidIntegerWrapper(int64 when, ErlNifPid pid, int64 integer) {
            tuple = std::make_tuple(when, pid, integer);
        }
        int64 get_time() const { return std::get<0>(tuple); };
        ErlNifPid get_pid() const { return std::get<1>(tuple); };
        int64 get_integer() const { return std::get<2>(tuple); };

    private:
        TupleTimePidInteger tuple;
        friend bool operator<(const TupleTimePidIntegerWrapper& l, const TupleTimePidIntegerWrapper& r) {
            return std::get<0>(l.tuple) < std::get<0>(r.tuple);
        }
    };

    std::set<TupleTimePidIntegerWrapper> m_pending_messages;

    int send_integer_to_erlang_process(ErlNifPid pid, int64 integer)
    {
        ErlNifEnv *msg_env = enif_alloc_env();
        ERL_NIF_TERM term;

        term = enif_make_int64(msg_env, integer);
        int rc = enif_send(NULL, &pid, msg_env, term);
        enif_free_env(msg_env);
        return rc;
    }

};

