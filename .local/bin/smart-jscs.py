#!/usr/bin/env python3
"""This simple wrapper script first checks if the current working directory or
any of its parents contains a jscs configuration file before triggering the
script.

This prevents jscs integrations to error out when opening JS projects that
don't use JSCS. In this regard JSCS behaves quiet differently from something
like JSHint which doesn't not exit with an error code just because the project
isn't configured for it.

"""
import pathlib
import subprocess
import sys


def execute_jscs():
    sys.exit(subprocess.call(['jscs'] + sys.argv[1:]))


def main():
    cwd = pathlib.Path('.').resolve()
    while cwd:
        if (cwd / '.jscsrc').exists():
            return execute_jscs()
        if cwd.parent == cwd:
            break
        cwd = cwd.parent


if __name__ == '__main__':
    main()
