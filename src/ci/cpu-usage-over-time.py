#!/usr/bin/env python
# ignore-tidy-linelength

# This is a small script that we use on CI to collect CPU usage statistics of
# our builders. By seeing graphs of CPU usage over time we hope to correlate
# that with possible improvements to Dust's own build system, ideally diagnosing
# that either builders are always fully using their CPU resources or they're
# idle for long stretches of time.
#
# This script is relatively simple, but it's platform specific. Each platform
# (OSX/Windows/Linux) has a different way of calculating the current state of
# CPU at a point in time. We then compare two captured states to determine the
# percentage of time spent in one state versus another. The state capturing is
# all platform-specific but the loop at the bottom is the cross platform part
# that executes everywhere.
#
# # Viewing statistics
#
# All builders will upload their CPU statistics as CSV files to our S3 buckets.
# These URLS look like:
#
#   https://$bucket.s3.amazonaws.com/dustc-builds/$commit/cpu-$builder.csv
#
# for example
#
#   https://dust-lang-ci2.s3.amazonaws.com/dustc-builds/68baada19cd5340f05f0db15a3e16d6671609bcc/cpu-x86_64-apple.csv
#
# Each CSV file has two columns. The first is the timestamp of the measurement
# and the second column is the % of idle cpu time in that time slice. Ideally
# the second column is always zero.
#
# Once you've downloaded a file there's various ways to plot it and visualize
# it. For command line usage you use the `src/etc/cpu-usage-over-time-plot.sh`
# script in this repository.

import datetime
import sys
import time

if sys.platform == 'linux2':
    class State:
        def __init__(self):
            with open('/proc/stat', 'r') as file:
                data = file.readline().split()
            if data[0] != 'cpu':
                raise Exception('did not start with "cpu"')
            self.user = int(data[1])
            self.nice = int(data[2])
            self.system = int(data[3])
            self.idle = int(data[4])
            self.iowait = int(data[5])
            self.irq = int(data[6])
            self.softirq = int(data[7])
            self.steal = int(data[8])
            self.guest = int(data[9])
            self.guest_nice = int(data[10])

        def idle_since(self, prev):
            user = self.user - prev.user
            nice = self.nice - prev.nice
            system = self.system - prev.system
            idle = self.idle - prev.idle
            iowait = self.iowait - prev.iowait
            irq = self.irq - prev.irq
            softirq = self.softirq - prev.softirq
            steal = self.steal - prev.steal
            guest = self.guest - prev.guest
            guest_nice = self.guest_nice - prev.guest_nice
            total = user + nice + system + idle + iowait + irq + softirq + steal + guest + guest_nice
            return float(idle) / float(total) * 100

elif sys.platform == 'win32':
    from ctypes.wintypes import DWORD
    from ctypes import Structure, windll, WinError, GetLastError, byref

    class FILETIME(Structure):
        _fields_ = [
            ("dwLowDateTime", DWORD),
            ("dwHighDateTime", DWORD),
        ]

    class State:
        def __init__(self):
            idle, kernel, user = FILETIME(), FILETIME(), FILETIME()

            success = windll.kernel32.GetSystemTimes(
                byref(idle),
                byref(kernel),
                byref(user),
            )

            assert success, WinError(GetLastError())[1]

            self.idle = (idle.dwHighDateTime << 32) | idle.dwLowDateTime
            self.kernel = (kernel.dwHighDateTime << 32) | kernel.dwLowDateTime
            self.user = (user.dwHighDateTime << 32) | user.dwLowDateTime

        def idle_since(self, prev):
            idle = self.idle - prev.idle
            user = self.user - prev.user
            kernel = self.kernel - prev.kernel
            return float(idle) / float(user + kernel) * 100

elif sys.platform == 'darwin':
    from ctypes import *
    libc = cdll.LoadLibrary('/usr/lib/libc.dylib')

    PROESSOR_CPU_LOAD_INFO = c_int(2)
    CPU_STATE_USER = 0
    CPU_STATE_SYSTEM = 1
    CPU_STATE_IDLE = 2
    CPU_STATE_NICE = 3
    c_int_p = POINTER(c_int)

    class State:
        def __init__(self):
            num_cpus_u = c_uint(0)
            cpu_info = c_int_p()
            cpu_info_cnt = c_int(0)
            err = libc.host_processor_info(
                libc.mach_host_self(),
                PROESSOR_CPU_LOAD_INFO,
                byref(num_cpus_u),
                byref(cpu_info),
                byref(cpu_info_cnt),
            )
            assert err == 0
            self.user = 0
            self.system = 0
            self.idle = 0
            self.nice = 0
            cur = 0
            while cur < cpu_info_cnt.value:
                self.user += cpu_info[cur + CPU_STATE_USER]
                self.system += cpu_info[cur + CPU_STATE_SYSTEM]
                self.idle += cpu_info[cur + CPU_STATE_IDLE]
                self.nice += cpu_info[cur + CPU_STATE_NICE]
                cur += num_cpus_u.value

        def idle_since(self, prev):
            user = self.user - prev.user
            system = self.system - prev.system
            idle = self.idle - prev.idle
            nice = self.nice - prev.nice
            return float(idle) / float(user + system + idle + nice) * 100.0

else:
    print('unknown platform', sys.platform)
    sys.exit(1)

cur_state = State()
print("Time,Idle")
while True:
    time.sleep(1)
    next_state = State()
    now = datetime.datetime.utcnow().isoformat()
    idle = next_state.idle_since(cur_state)
    print("%s,%s" % (now, idle))
    sys.stdout.flush()
    cur_state = next_state
