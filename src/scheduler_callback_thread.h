#pragma once

#include <erl_nif.h>
#include <set>

#include "../JuceLibraryCode/JuceHeader.h"

// FIXME: this should go into a header file
extern int64 sp_midi_get_current_time_microseconds();


// FIXME: make sure this is thread-safe (i.e. protect the m_pending_messages accesses)
class SchedulerCallbackThread : public Thread
{
    using TupleTimePidInteger = std::tuple<int64, ErlNifPid, int64>;
public:
    SchedulerCallbackThread() : Thread("scheduler callback thread") {
        setPriority(10);
    };

    void run() override
    {
        int64 next_timer_shot = 0;
        while (!threadShouldExit()){
            // If there are no pending messages, do nothing
            bool is_empty;
            {
                std::lock_guard<std::mutex> guard(m_messages_mutex);
                is_empty = m_pending_messages.empty();
            }

            if (is_empty) {
                wait(10);
                continue;
            }

            // When is the next message due?
            auto first_pending_message = m_pending_messages.cbegin();
            next_timer_shot = first_pending_message->get_time();

            // How far is it in the future?
            int64 current_time = sp_midi_get_current_time_microseconds();
            int64 time_to_next_message_millis = (next_timer_shot - current_time) / 1000;

            // If it is less than 1ms away or in the past, fire it
            if (time_to_next_message_millis < 1) {
                time_to_next_message_millis = LONG_MAX;
                {
                  std::lock_guard<std::mutex> guard(m_messages_mutex);
                    ErlNifPid pid = first_pending_message->get_pid();
                  send_integer_to_erlang_process(pid, first_pending_message->get_integer(), current_time);
                    // ... and remove it from the pending messages
                    m_pending_messages.erase(first_pending_message);

                    // ...and check if there is something that we need to fire now or soon (we do not call the wait)
                    continue;
                }
            }
            
            // Wait for 10 ms if there is nothing happening soon, or as necessary if there is something less than 10ms away
            wait(jmin(static_cast<int64>(10), time_to_next_message_millis));
        }
    }

    void trigger_callback_at(int64 when, ErlNifPid pid, int64 integer)
    {   
        TupleTimePidIntegerWrapper element(when, pid, integer);
        std::lock_guard<std::mutex> guard(m_messages_mutex);  
        m_pending_messages.insert(element);
    }

private:

    class TupleTimePidIntegerWrapper {
    public:
        TupleTimePidIntegerWrapper(int64 when, ErlNifPid pid, int64 integer) : m_when(when), m_pid(pid), m_integer(integer) { };
        int64 get_time() const { return m_when; };
        ErlNifPid get_pid() const { return m_pid; };
        int64 get_integer() const { return m_integer; };

    private:
        TupleTimePidInteger tuple;
        int64 m_when;
        ErlNifPid m_pid;
        int64 m_integer;

        friend bool operator<(const TupleTimePidIntegerWrapper& l, const TupleTimePidIntegerWrapper& r) {
            return l.m_when < r.m_when;
        }
    };

    std::set<TupleTimePidIntegerWrapper> m_pending_messages;
    std::mutex m_messages_mutex;

    int send_integer_to_erlang_process(ErlNifPid pid, int64 integer, int64 time)
    {
        ErlNifEnv *msg_env = enif_alloc_env();
        ERL_NIF_TERM term, term2, list;

        term = enif_make_int64(msg_env, integer);
        term2 = enif_make_int64(msg_env, time);
        list = enif_make_list2(msg_env, term, term2);
        int rc = enif_send(NULL, &pid, msg_env, list);
        enif_free_env(msg_env);
        return rc;
    }

};

