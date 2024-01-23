#!/usr/bin/env python3
'''
Run an hspec test suite but don't wait for the spec to exit. Instead,
search the output for either /examples?, 0 failure/ or /examples?, [1-9][0-9]* failure/
and determine the correct exit code based on that. Then promptly kill the test executable
and exit.
'''

import sys
import subprocess
import time
import re

USAGE = '''\
USAGE: %s [TEST_EXECUTABLE [ARGS...]]
'''

if len(sys.argv) < 2:
    print(USAGE, file=sys.stderr)

test_executable = sys.argv[1]
args = sys.argv[2:]

start_time = time.time()
proc = subprocess.Popen(
    [test_executable, '--color', *args],
    stdout=subprocess.PIPE,
    encoding='utf8',
)


def cleanup_and_exit(retcode):
    print('\x1b[0m', end='')  # Reset terminal colors
    print('\x1b[?25h', end='')  # Show cursor
    print()
    print('Terminating test...')
    proc.terminate()

    t = time.time()
    while proc.poll() is None and time.time() - t < 5:
        time.sleep(0.1)

    if proc.poll() is None:
        print('Tests did not terminate. Killing instead...')
        proc.kill()
        print('done!')
    else:
        print('Tests terminated successfully')

    exit(retcode)


if (ret := proc.poll()) is not None:
    cleanup_and_exit(ret)

for line in proc.stdout:
    print(line, end='')
    if re.search(r'examples?, 0 failure', line):
        cleanup_and_exit(0)

    if re.search(r'examples?, [1-9][0-9]* failure', line):
        cleanup_and_exit(1)

    if (ret := proc.poll()) is not None:
        cleanup_and_exit(ret)

print('Bad output from tests?', file=sys.stderr)
cleanup_and_exit(1)
