#!/usr/bin/env python3
'''
Run `stack test` but don't wait for the spec to exit. Instead,
search the output for either /examples?, 0 failure/ or /examples?, [1-9][0-9]* failure/
and determine the correct exit code based on that. Then promptly killall stack and the test executable
because otherwise killing just stack leaves orphaned processes that would otherwise never
exit.

For this reason, it is recommended to not run this script on a dev machine locally,
as any process matching `stack` or or the test executable will be killed, not necessarily just
children of this process. If you really want to anyway, set the environment variable
CI=true
'''

import sys
import os
import subprocess
import time
import re

if os.environ.get('CI') != 'true':
    print(
        'It is not advised to run this script except in a container. See the docstring of this script',
        file=sys.stderr)
    exit(1)

USAGE = '''\
USAGE: %s STACK_TEST_EXECUTABLE [STACK_ARGS...]
'''

if len(sys.argv) < 2:
    print(USAGE, file=sys.stderr)

stack_test_executable = sys.argv[1]
stack_args = sys.argv[2:]

job_timeout = int(os.environ.get('STACK_TEST_TIMEOUT', 0))
start_time = time.time()
stack = subprocess.Popen(
    [
        'stack', 'test', \
        '--ghc-options', '-fdiagnostics-color=always', \
        '--test-arguments', '--color', \
        *stack_args
    ],
    stdout=subprocess.PIPE,
    encoding='utf8')


def cleanup_and_exit(retcode):
    print('\x1b[0m', end='')  # Reset terminal colors
    print()
    print('Terminating stack...')
    stack.terminate()

    t = time.time()
    while stack.poll() is None and time.time() - t < 5:
        time.sleep(0.1)

    if stack.poll() is None:
        print('Stack did not terminate. Killing instead...')
        stack.kill()
    else:
        print('Stack terminated successfully')

    print('Reaping any hanging processes')
    print('+ killall stack')
    subprocess.run(['killall', 'stack'])
    print(f'+ killall {stack_test_executable}')
    subprocess.run(['killall', stack_test_executable])
    print('done!')

    exit(retcode)


if (ret := stack.poll()) is not None:
    cleanup_and_exit(ret)

for line in stack.stdout:
    print(line, end='')
    if re.search(r'examples?, 0 failure', line):
        cleanup_and_exit(0)

    if re.search(r'examples?, [1-9][0-9]* failure', line):
        cleanup_and_exit(1)

    if (ret := stack.poll()) is not None:
        cleanup_and_exit(ret)

print('Bad output from stack?', file=sys.stderr)
cleanup_and_exit(1)
