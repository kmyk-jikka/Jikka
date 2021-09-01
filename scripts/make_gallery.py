#!/usr/bin/env python3
import argparse
import concurrent.futures
import json
import os
import pathlib
import subprocess
import sys
from logging import DEBUG, basicConfig, getLogger
from typing import *

logger = getLogger(__name__)


def transpile_python_script(path: pathlib.Path, *, target: str, executable: pathlib.Path) -> bytes:
    logger.info('%s: Transpiling to %s...', str(path), target)
    result = subprocess.check_output([str(executable), 'convert', '--target', target, '--no-bundle-runtime-headers', '--no-embed-original-code', str(path)])
    if target == 'cxx':
        logger.info('%s: Running clang-format...', str(path))
        result = subprocess.check_output(['clang-format'], input=result)
    return result


def read_transpilation_error(path: pathlib.Path, *, executable: pathlib.Path) -> bytes:
    logger.info('%s: Read error messages...', str(path))
    proc = subprocess.run([str(executable), 'convert', str(path)], stderr=subprocess.PIPE)
    if proc.returncode == 0:
        raise RuntimeError('unexpected success: {}'.format(str(path)))
    return proc.stderr


def get_local_install_root() -> pathlib.Path:
    s = subprocess.check_output(['stack', '--system-ghc', 'path', '--local-install-root'])
    return pathlib.Path(s.decode().strip())


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument('-j', '--jobs', type=int, default=os.cpu_count())
    args = parser.parse_args()

    basicConfig(level=DEBUG)

    subprocess.check_call(['stack', '--system-ghc', 'build'])
    executable = get_local_install_root() / 'bin' / 'jikka'
    futures: Dict[Tuple[pathlib.Path, str], concurrent.futures.Future] = {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as executor:
        for path in list(pathlib.Path('examples').glob('*.py')) + list(pathlib.Path('examples', 'wip', 'tle').glob('*.py')):
            for target in ('rpython', 'core', 'cxx'):
                futures[(path, target)] = executor.submit(transpile_python_script, path, target=target, executable=executable)
        for path in pathlib.Path('examples', 'errors').glob('*.py'):
            futures[(path, 'error')] = executor.submit(read_transpilation_error, path, executable=executable)

    examples = []
    for path in sorted(pathlib.Path('examples').glob('*.py')) + sorted(pathlib.Path('examples', 'wip', 'tle').glob('*.py')):
        with open(path) as fh:
            code = fh.read()
        examples.append({
            'path': str(path),
            'python': code,
            'rpython': futures[(path, 'rpython')].result().decode(),
            'core': futures[(path, 'core')].result().decode(),
            'cxx': futures[(path, 'cxx')].result().decode(),
        })
    errors = []
    for path in sorted(pathlib.Path('examples', 'errors').glob('*.py')):
        with open(path) as fh:
            code = fh.read()
        errors.append({
            'path': str(path),
            'python': code,
            'error': futures[(path, 'error')].result().decode(),
        })

    json.dump({
        'examples': examples,
        'errors': errors,
    }, sys.stdout)


if __name__ == '__main__':
    main()
