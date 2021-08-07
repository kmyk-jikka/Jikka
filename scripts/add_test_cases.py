#!/usr/bin/env python3
import argparse
import json
import pathlib
import shutil
import subprocess
import sys
from logging import DEBUG, basicConfig, getLogger
from typing import *

logger = getLogger(__name__)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument('url')
    parser.add_argument('--name', metavar='STEM', help='The stem name of files. The problem IDs (e.g. abc123_d, agc001_a) are often used.')
    parser.add_argument('--only-sample-cases', action='store_true')
    parser.add_argument('-n', '--dry-run', action='store_true')
    args = parser.parse_args()

    basicConfig(level=DEBUG)

    if args.name is not None:
        name = args.name
    elif 'atcoder.jp' in args.url:
        name = args.url.split('/')[-1]
    elif 'yukicoder.me' in args.url:
        name = 'yukicoder-' + args.url.split('/')[-1]
    else:
        logger.error('--name=STEM is required')
        sys.exit(1)

    if shutil.which('oj-api') is None:
        logger.error('Please install `oj-api` command with: $ pip3 install online-judge-api-client')
        sys.exit(1)
    if shutil.which('oj-template') is None:
        logger.error('Please install `oj-template` command with: $ pip3 install online-judge-template-generator')
        sys.exit(1)

    data = json.loads(subprocess.check_output(['oj-api', 'get-problem', args.url]))
    if data['status'] != 'ok':
        logger.error('failed to download the problem info: %s', data)
        sys.exit(1)

    plan: Dict[pathlib.Path, bytes] = {}
    examples_dir = pathlib.Path('examples')

    # collect files
    for i, testcase in enumerate(data['result']['tests']):
        plan[examples_dir / 'data' / '{}.sample-{}.in'.format(name, i + 1)] = testcase['input'].encode()
        plan[examples_dir / 'data' / '{}.sample-{}.out'.format(name, i + 1)] = testcase['output'].encode()
    if not args.only_sample_cases:
        plan[examples_dir / '{}.py'.format(name)] = subprocess.check_output(['oj-template', '-t', 'main.py', args.url])
        plan[examples_dir / 'data' / '{}.solver.cpp'.format(name)] = subprocess.check_output(['oj-template', '-t', 'main.cpp', args.url]).replace(b'#include <bits/stdc++.h>\n', b'#include <cstdint>\n#include <iostream>\n#include <vector>\n')  # Replace bits/stdc++.h to compile the solver on Windows.
        plan[examples_dir / 'data' / '{}.large.generator.py'.format(name)] = subprocess.check_output(['oj-template', '-t', 'generate.py', args.url])

    # check files
    for path in plan:
        if path.exists():
            logger.error('file already exists: %s', str(path))
            sys.exit(1)

    # write files
    for path, content in plan.items():
        logger.info('write file: %s', str(path))
        if not args.dry_run:
            with open(path, 'wb') as fh:
                fh.write(content)


if __name__ == '__main__':
    main()
