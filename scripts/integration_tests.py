#!/usr/bin/env python3
import argparse
import concurrent.futures
import functools
import glob
import json
import os
import pathlib
import platform
import re
import subprocess
import sys
import tempfile
from logging import DEBUG, basicConfig, getLogger
from typing import *

logger = getLogger(__name__)

CXX_ONLY = 'large'
NO_PYTHON = 'medium'
TIMEOUT_FACTOR = 1 if os.environ.get('CI') is None else (5 if platform.system() == 'Linux' else 20)


def compile_cxx(src_path: pathlib.Path, dst_path: pathlib.Path):
    CXX = os.environ.get('CXX', 'g++')
    CXXFLAGS = ['-std=c++17', '-Wall', '-O2', '-I', str(pathlib.Path('runtime', 'ac-library'))]
    command = [CXX, *CXXFLAGS, '-o', str(dst_path), str(src_path)]
    subprocess.check_call(command, timeout=5 * TIMEOUT_FACTOR)


@functools.lru_cache(maxsize=None)
def get_alias_mapping() -> Dict[str, str]:
    with open(pathlib.Path('examples', 'data', 'aliases.json')) as fh:
        return json.load(fh)


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

        for solver_ext in ('.py', '.cpp'):
            solver_path = pathlib.Path('examples', 'data', script.stem + '.solver' + solver_ext)
            if solver_path.exists():
                break
        else:
            logger.error('%s: failed to find the solver', str(script))
            return []
        if solver_path.suffix == '.py':
            solver_command = [sys.executable, str(solver_path)]
        elif solver_path.suffix == '.cpp':
            try:
                compile_cxx(src_path=solver_path, dst_path=tempdir / 'expected.exe')
            except subprocess.SubprocessError as e:
                logger.error('%s: failed to compile the expected solver from C++ to executable: %s', str(script), e)
                return []
            solver_command = [str(tempdir / 'expected.exe')]
        else:
            assert False

        logger.info('%s: generating input cases...', str(script))
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
                        subprocess.check_call(solver_command, stdin=fh1, stdout=fh2, timeout=5 * TIMEOUT_FACTOR)
                    except subprocess.SubprocessError as e:
                        logger.error('%s: %s: failed to generate an output of a random case: %s', str(script), str(inputcase), e)
                        return []
            inputcases.append(inputcase)

    # resolve alias
    aliases = get_alias_mapping()
    if script.stem in aliases:
        if inputcases:
            logger.error("%s: there must not be test cases when it uses an alias: %s", str(script), list(map(str, inputcases)))
            return []
        return collect_input_cases(script.parent / (aliases[script.stem] + script.suffix), tempdir=tempdir)

    return inputcases


def run_integration_test(script: pathlib.Path, *, executable: pathlib.Path) -> Optional[str]:
    with tempfile.TemporaryDirectory() as tempdir_:
        tempdir = pathlib.Path(tempdir_)

        logger.info('%s: compiling...', str(script))
        with open(tempdir / 'main.cpp', 'wb') as fh:
            try:
                subprocess.check_call([str(executable), 'convert', str(script)], stdout=fh, timeout=20 * TIMEOUT_FACTOR)
            except subprocess.SubprocessError as e:
                msg = 'failed to compile from Python to C++: {}'.format(e)
                logger.error('%s: %s', str(script), msg)
                return msg
        try:
            compile_cxx(src_path=tempdir / 'main.cpp', dst_path=tempdir / 'a.exe')
        except subprocess.SubprocessError as e:
            msg = 'failed to compile from C++ to executable: {}'.format(e)
            logger.error('%s: %s', str(script), msg)
            return msg

        with open(script, 'rb') as fh:
            code = fh.read()
        use_standard_python = bool(re.search(rb'\bdef +main *\(', code)) and not bool(re.search(rb'\bjikka\b', code))

        inputcases = collect_input_cases(script, tempdir=tempdir)
        if not inputcases:
            msg = 'no input cases'
            logger.error('%s: %s', str(script), msg)
            return msg
        for inputcase in inputcases:
            outputcase = inputcase.with_suffix('.out')
            with open(outputcase, 'rb') as fh:
                expected = fh.read()

            matrix: List[Tuple[str, List[str]]] = []
            if use_standard_python:
                matrix.append(('standard Python', [sys.executable, str(script)]))
            matrix.append(('restricted Python', [str(executable), 'execute', '--target', 'rpython', str(script)]))
            matrix.append(('core', [str(executable), 'execute', '--target', 'core', str(script)]))
            matrix.append(('C++', [str(tempdir / 'a.exe')]))
            for title, command in matrix:
                if 'Python' in title and (NO_PYTHON in inputcase.name or CXX_ONLY in inputcase.name):
                    continue
                if title == 'core' and CXX_ONLY in inputcase.name:
                    continue
                if 'wip' in inputcase.name:
                    continue

                logger.info('%s: %s: running as %s...', str(script), str(inputcase), title)
                with open(inputcase, 'rb') as fh:
                    try:
                        actual = subprocess.check_output(command, stdin=fh, timeout=(2 if title == 'C++' else 20) * TIMEOUT_FACTOR)
                    except subprocess.SubprocessError as e:
                        msg = '{}: failed to run as {}: {}'.format(str(inputcase), title, e)
                        logger.error('%s: %s', str(script), msg)
                        return msg
                    if actual.decode().split() != expected.decode().split():
                        msg = '{}: wrong answer: {} is expected, but actually got {}'.format(str(inputcase), expected.decode().split(), actual.decode().split())
                        logger.error('%s: %s', str(script), msg)
                        return msg

    logger.info('%s: accepted', str(script))
    return None


def run_integration_test_about_error(script: pathlib.Path, *, executable: pathlib.Path) -> Optional[str]:
    logger.info('%s: compiling...', str(script))
    proc = subprocess.run([str(executable), 'convert', str(script)], stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    if proc.returncode == 0:
        msg = 'unexpectedly succeeded to compile'
        logger.error('%s: %s', str(script), msg)
        return msg

    actual_lines = proc.stderr.splitlines()
    with open(pathlib.Path('examples', 'data', script.stem + '.txt'), 'rb') as fh:
        expected_lines = fh.read().splitlines()
    for line in expected_lines:
        if line not in actual_lines:
            msg = "expectedly failed to compile, but didn't print expected error messages: expected = {}, actual = {}".format(expected_lines, actual_lines)
            logger.error('%s: %s', str(script), msg)
            return msg

    logger.info('%s: expectedly failed to compile', str(script))
    return None


def get_local_install_root() -> pathlib.Path:
    s = subprocess.check_output(['stack', '--system-ghc', 'path', '--local-install-root'])
    return pathlib.Path(s.decode().strip())


def find_unused_test_cases() -> List[pathlib.Path]:
    scripts = list(pathlib.Path('examples').glob('*.py'))
    errors = list(pathlib.Path('examples', 'errors').glob('*.py'))
    unused = []
    for path in pathlib.Path('examples', 'data').glob('*'):
        if path.name == 'aliases.json':
            continue
        name = path.name[:-len(''.join(path.suffixes))]
        if name not in [script.stem for script in scripts + errors]:
            unused.append(path)
            continue
        if path.suffix == '.in':
            pass
        elif path.suffix == '.out':
            pass
        elif path.name.endswith('.generator.py'):
            pass
        elif path.name.endswith('.generator.cpp'):
            pass
        elif path.name.endswith('.solver.py'):
            pass
        elif path.name.endswith('.solver.cpp'):
            pass
        elif path.suffix == '.txt' and name in [script.stem for script in errors]:
            pass
        else:
            unused.append(path)
            continue
    for path in unused:
        logger.error('unused file: %s', str(path))
    return unused


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument('-j', '--jobs', type=int, default=os.cpu_count())
    parser.add_argument('-k', type=str)
    args = parser.parse_args()

    basicConfig(level=DEBUG)

    if find_unused_test_cases():
        sys.exit(1)
    subprocess.check_call(['stack', '--system-ghc', 'build'])

    # run tests
    executable = get_local_install_root() / 'bin' / 'jikka'
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as executor:
        futures = {}
        for path in pathlib.Path('examples').glob('*.py'):
            if args.k and args.k not in path.name:
                continue
            futures[path] = executor.submit(run_integration_test, path, executable=executable)
        for path in pathlib.Path('examples', 'errors').glob('*.py'):
            if args.k and args.k not in path.name:
                continue
            futures[path] = executor.submit(run_integration_test_about_error, path, executable=executable)

    # report results
    succeeded = 0
    for path, future in futures.items():
        if future.result() is None:
            succeeded += 1
        else:
            # This is the workflow command of GitHub Actions. See https://docs.github.com/en/actions/reference/workflow-commands-for-github-actions#setting-an-error-message
            print('::error file={},line=1,col=1::{}'.format(str(path), future.result()))
    logger.info('%d/%d tests succeeded', succeeded, len(futures))
    if succeeded < len(futures):
        sys.exit(1)


if __name__ == '__main__':
    main()
