#include <stdio.h>
#include <stdlib.h>

#define HANDLER_COUNT 6
#define SLOT_COUNT 10

struct ControlState {
    long slots[SLOT_COUNT];
    long handler_index;
};

struct SystemState {
    void (*handlers[HANDLER_COUNT])();
    long call_target;
};

struct ControlState g_ctrl;
struct SystemState g_sys;
struct ControlState *ctrl = &g_ctrl;
struct SystemState *sys = &g_sys;

// this are just boilerplate functions if we want addional use later
void valve_open()    { puts("[VALVE] Open"); }
void valve_close()   { puts("[VALVE] Close"); }
void pump_start()    { puts("[PUMP] Start"); }
void pump_stop()     { puts("[PUMP] Stop"); }
void alarm_enable()  { puts("[ALARM] On"); }
void alarm_disable() { puts("[ALARM] Off"); }

void admin_mode()
{
    puts("ADMIN MODE ACTIVATED!");
    exit(0);
}

// arbitarary write to addr
void set_call_target(long addr)
{
    sys->call_target = addr;
}

// oob to handler index
int set_slot(long index, long value)
{
    if (index < 0 || index > SLOT_COUNT)
        return 0;
    ctrl->slots[index] = value; // oob
    return 1;
}

// indirect call
void call_handler()
{
    sys->handlers[ctrl->handler_index]();
}

int menu(int cmd, long arg1, long arg2)
{
    switch (cmd) {
        case 1:
            set_slot(arg1, arg2);
            break;
        case 2:
            call_handler();
            break;
        case 3:
            set_call_target(arg1);
            break;
        }
        
    return 0;
}

void init_handlers()
{
    sys->handlers[0] = valve_open;
    sys->handlers[1] = valve_close;
    sys->handlers[2] = pump_start;
    sys->handlers[3] = pump_stop;
    sys->handlers[4] = alarm_enable;
    sys->handlers[5] = alarm_disable;
}

int main()
{
    init_handlers();

    // write admin_mode address to call_target
    printf("[+] Writing %p to call_target\n", admin_mode);
    menu(3, (long)admin_mode, 0);

    // OOB write slots[10] to set handler_index = 6
    printf("[+] Overflowing into handler_index\n");
    menu(1, 10, 6);

    // OOB read handlers[6] calls call_target
    printf("[+] Triggering indirect call to admin_mode()\n");
    menu(2, 0, 0);

    return 0;
}
