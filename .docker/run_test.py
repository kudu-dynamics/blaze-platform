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


def cleanup_and_exit(reason, retcode):
    print('\x1b[0m', end='')  # Reset terminal colors
    print('\x1b[?25h', end='')  # Show cursor
    print()
    print(reason)
    print('Terminating test suite...')
    proc.terminate()

    t = time.time()
    while proc.poll() is None and time.time() - t < 5:
        time.sleep(0.1)

    if proc.poll() is None:
        print('Test suite did not terminate. Killing instead...')
        proc.kill()
        print('Killed test suite')
    else:
        print('Terminated test suite')

    exit(retcode)


if (ret := proc.poll()) is not None:
    cleanup_and_exit('Test suite exited with no output', ret)

for line in proc.stdout:
    print(line, end='')
    if re.search(r'examples?, 0 failure', line):
        cleanup_and_exit('Detected test suite success', 0)

    if re.search(r'examples?, [1-9][0-9]* failure', line):
        cleanup_and_exit('Detected test suite failure(s)', 1)

    if proc.poll() is not None:
        break

if (ret := proc.poll()) is not None:
    cleanup_and_exit(
        f'Test suite {test_executable} exited with status code {ret}, but we did not detect the Hspec summary. '
        'Maybe there was an error?',
        ret
    )

cleanup_and_exit(
    f'Test suite {test_executable} closed stdout but did not exit and we did not detect the Hspec summary. '
    'Maybe there was an error?',
    1
)
