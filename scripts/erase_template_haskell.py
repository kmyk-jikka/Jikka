#!/usr/bin/env python3
import argparse
import pathlib
import re
from logging import DEBUG, basicConfig, getLogger
from typing import *

logger = getLogger(__name__)


class Splice(NamedTuple):
    path: pathlib.Path
    line: int  # 0-based
    column: int  # 0-based
    width: int
    before: str
    after: str


def parse_splice(*, lines: List[str]) -> Splice:
    # header
    logger.info('%s', lines[0].rstrip())
    m = re.match(r'(.*):(\d+):(\d+)-(\d+): Splicing.*', lines[0])
    assert m
    path = pathlib.Path(m.group(1))
    line = int(m.group(2)) - 1
    column = int(m.group(3)) - 1
    width = int(m.group(4)) - column

    # before
    before = ''
    i = 1
    while lines[i].strip() != '======>':
        before += lines[i]
        i += 1
    i += 1

    # after
    after = ''.join(lines[i:])

    return Splice(
        path=path,
        line=line,
        column=column,
        width=width,
        before=before,
        after=after,
    )


def parse_splices(*, content: str) -> List[Splice]:
    splices: List[Splice] = []
    lines = content.splitlines(keepends=True)
    l = 0
    while l < len(lines):
        r = l + 1
        while r < len(lines) and lines[r][0].isspace():
            r += 1
        splices.append(parse_splice(lines=lines[l:r]))
        l = r
    return splices


def apply_splices(*, code: str, splices: List[Splice]) -> str:
    offset = [0]
    for line in code.splitlines(keepends=True):
        offset.append(offset[-1] + len(line))

    buf = list(code)
    for splice in reversed(splices):
        magic = 3
        l = offset[splice.line] + splice.column
        r = l + splice.width
        buf[l - magic:r] = splice.after.strip()

    return ''.join(buf)


# Before running this script, you need to run $ stack build --ghc-options=-ddump-splices --ghc-options=-ddump-to-file
def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument('--dist-directory', type=pathlib.Path, default=pathlib.Path('.stack-work', 'dist'))
    parser.add_argument('--source-directory', type=pathlib.Path, default=pathlib.Path('src'))
    parser.add_argument('--rewrite', action='store_true')
    args = parser.parse_args()

    basicConfig(level=DEBUG)

    for dump_splices_path in args.dist_directory.glob('**/*.dump-splices'):
        with open(dump_splices_path) as fh:
            content = fh.read()
        splices = parse_splices(content=content)
        with open(splices[0].path) as fh:
            code = fh.read()
        code = apply_splices(code=code, splices=splices)
        if args.rewrite:
            logger.info('rewrite %s', str(splices[0].path))
            with open(splices[0].path, 'w') as fh:
                fh.write(code)
        else:
            print(code)


if __name__ == '__main__':
    main()
