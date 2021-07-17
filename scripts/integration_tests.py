#!/usr/bin/env python3
import argparse
import concurrent.futures
import glob
import os
import pathlib
import platform
import subprocess
import sys
import tempfile
from logging import DEBUG, basicConfig, getLogger
from typing import *

logger = getLogger(__name__)

CXX_ONLY = 'large'
NO_RPYTHON = 'medium'
TIMEOUT_FACTOR = 1 if platform.system() == 'Linux' else 10


def collect_input_cases(script: pathlib.Path, *, tempdir: pathlib.Path) -> List[pathlib.Path]:
    inputcases: List[pathlib.Path] = []

    # text files
    for path in pathlib.Path('examples', 'data').iterdir():
        if path.name[:-len(''.join(path.suffixes))] != script.stem:
            continue
        if path.suffix != '.in':
            continue
        inputcases.append(path)

    # using generators
    for generator_path in pathlib.Path('examples', 'data').glob(glob.escape(script.stem) + '*.generator.py'):
        _, testset_name, _, _ = generator_path.name.split('.')

        solver_path = pathlib.Path('examples', 'data', script.stem + '.' + testset_name + '.solver.py')
        if not solver_path.exists():
            logger.error('%s: failed to find the solver', str(script))
            return []

        for i in range(20):
            inputcase = tempdir / "{}.{}-{}.in".format(script.stem, testset_name, i)
            outputcase = tempdir / "{}.{}-{}.out".format(script.stem, testset_name, i)
            with open(inputcase, 'wb') as fh:
                try:
                    subprocess.check_call([sys.executable, str(generator_path)], stdout=fh, timeout=5 * TIMEOUT_FACTOR)
                except subprocess.SubprocessError as e:
                    logger.error('%s: %s: failed to generate an input of a random case: %s', str(script), str(inputcase), e)
                    return []
            with open(inputcase, 'rb') as fh1:
                with open(outputcase, 'wb') as fh2:
                    try:
                        subprocess.check_call([sys.executable, str(solver_path)], stdin=fh1, stdout=fh2, timeout=5 * TIMEOUT_FACTOR)
                    except subprocess.SubprocessError as e:
                        logger.error('%s: %s: failed to generate an output of a random case: %s', str(script), str(inputcase), e)
                        return []
            inputcases.append(inputcase)

    return inputcases


def run_integration_test(script: pathlib.Path, *, executable: pathlib.Path) -> bool:
    with tempfile.TemporaryDirectory() as tempdir_:
        tempdir = pathlib.Path(tempdir_)

        logger.info('%s: compiling...', str(script))
        with open(tempdir / 'main.cpp', 'wb') as fh:
            try:
                subprocess.check_call([str(executable), 'convert', str(script)], stdout=fh, timeout=20 * TIMEOUT_FACTOR)
            except subprocess.SubprocessError as e:
                logger.error('%s: failed to compile from Python to C++: %s', str(script), e)
                return False
        try:
            subprocess.check_call(['g++', '-std=c++17', '-Wall', '-O2', '-I', str(pathlib.Path('runtime', 'include')), '-o', str(tempdir / 'a.exe'), str(tempdir / 'main.cpp')], timeout=20 * TIMEOUT_FACTOR)
        except subprocess.SubprocessError as e:
            logger.error('%s: failed to compile from C++ to executable: %s', str(script), e)
            return False

        inputcases = collect_input_cases(script, tempdir=tempdir)
        if not inputcases:
            logger.error('%s: no input cases', str(script))
            return False
        for inputcase in inputcases:
            outputcase = inputcase.with_suffix('.out')
            with open(outputcase, 'rb') as fh:
                expected = fh.read()

            matrix: List[Tuple[str, List[str]]] = []
            matrix.append(('restricted Python', [str(executable), 'execute', '--target', 'rpython', str(script)]))
            matrix.append(('core', [str(executable), 'execute', '--target', 'core', str(script)]))
            matrix.append(('C++', [str(tempdir / 'a.exe')]))
            for title, command in matrix:
                if title == 'restricted Python' and (NO_RPYTHON in inputcase.name or CXX_ONLY in inputcase.name):
                    continue
                if title == 'core' and CXX_ONLY in inputcase.name:
                    continue
                if 'wip' in inputcase.name:
                    continue

                logger.info('%s: %s: running as %s...', str(script), str(inputcase), title)
                with open(inputcase, 'rb') as fh:
                    try:
                        actual = subprocess.check_output(command, stdin=fh, timeout=20 * TIMEOUT_FACTOR)
                    except subprocess.SubprocessError as e:
                        logger.error('%s: %s: failed to run as %s: %s', str(script), str(inputcase), title, e)
                        return False
                    if actual.decode().split() != expected.decode().split():
                        logger.error('%s: %s: wrong answer: %s is expected, but actually got %s', str(script), str(inputcase), expected.decode().split(), actual.decode().split())
                        return False

    logger.info('%s: accepted', str(script))
    return True


def run_integration_test_about_error(script: pathlib.Path, *, executable: pathlib.Path) -> bool:
    logger.info('%s: compiling...', str(script))
    proc = subprocess.run([str(executable), 'convert', str(script)], stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    if proc.returncode == 0:
        logger.info('%s: unexpectedly succeeded to compile', str(script))
        return False

    actual_lines = proc.stderr.splitlines()
    with open(pathlib.Path('examples', 'data', script.stem + '.txt'), 'rb') as fh:
        expected_lines = fh.read().splitlines()
    for line in expected_lines:
        if line not in actual_lines:
            logger.info("%s: expectedly failed to compile, but didn't print expected error messages: expected = %s, actual = %s", str(script), expected_lines, actual_lines)
            return False

    logger.info('%s: expectedly failed to compile', str(script))
    return True


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
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as executor:
        futures = []
        for path in pathlib.Path('examples').glob('*.py'):
            futures.append(executor.submit(run_integration_test, path, executable=executable))
        for path in pathlib.Path('examples', 'errors').glob('*.py'):
            futures.append(executor.submit(run_integration_test_about_error, path, executable=executable))
    cnt = [future.result() for future in futures].count(True)
    logger.info('%d/%d tests succeeded', cnt, len(futures))
    if cnt < len(futures):
        sys.exit(1)


if __name__ == '__main__':
    main()
