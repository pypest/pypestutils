#!/usr/bin/env python3
"""Stamp pypestutils/version.py with a version for a branch build.

The version is the base version (without any ".devN" suffix or local label)
with a local label made from the branch name and the build number, for
example "0.3.1+feat.iid2.42".  This is used by the "Build wheels" workflow so
that wheels published for a branch are distinguishable from releases.  It is
not a pre-release, so pip installs it without a version pin or "--pre"; it
ranks above the base version, and later builds rank above earlier ones.

Usage: python scripts/stamp_branch_version.py BRANCH BUILD_NUMBER
"""

import re
import sys
from pathlib import Path

VERSION_FILE = Path(__file__).resolve().parent.parent / "pypestutils" / "version.py"
VERSION_RE = re.compile(r'^__version__ = "([^"]+)"$', re.MULTILINE)


def branch_version(base: str, branch: str, build: int) -> str:
    base = re.sub(r"(\.dev\d+)?(\+.*)?$", "", base)
    local = re.sub(r"[^a-z0-9]+", ".", branch.lower()).strip(".")
    if not local:
        raise ValueError(f"cannot make a local version label from {branch!r}")
    return f"{base}+{local}.{build}"


def main():
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    branch, build = sys.argv[1], int(sys.argv[2])
    text = VERSION_FILE.read_text()
    match = VERSION_RE.search(text)
    if match is None:
        sys.exit(f"__version__ not found in {VERSION_FILE}")
    version = branch_version(match.group(1), branch, build)
    VERSION_FILE.write_text(VERSION_RE.sub(f'__version__ = "{version}"', text))
    print(version)


if __name__ == "__main__":
    main()
