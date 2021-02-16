#!/usr/bin/env python3

import sys
import os
import subprocess
import time
import re

if os.environ['CI'] != 'true':
    print('It is not advised to run this script except in a container',
          file=sys.stderr)
    exit(1)

job_timeout = int(os.environ.get('STACK_TEST_TIMEOUT', 0))
start_time = time.time()
stack = subprocess.Popen(
    [
        'stack', 'test', \
        '--ghc-options', '-fdiagnostics-color=always', \
        '--test-arguments', '--color', \
        *sys.argv[1:]
    ],
    stdout=subprocess.PIPE,
    encoding='utf8')


def cleanup_and_exit(retcode):
    print('\x1b[0m', end='')  # Reset terminal colors
    print('Terminating stack...')
    stack.terminate()
    if stack.poll() is None:
        print('Stack did not terminate. Killing instead...')
        stack.kill()

    print('pkill stack')
    subprocess.run(['pkill', 'stack'])
    print('pkill binja-test')
    subprocess.run(['pkill', 'binja-test'])
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
