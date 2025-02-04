"""Tests for example notebooks."""

from pathlib import Path
from subprocess import run

import pytest

try:
    import nbformat
except ImportError:
    pytest.skip("requires nbformat", allow_module_level=True)

examples_dir = Path(__file__).parent.parent / "examples"


@pytest.mark.parametrize("nb_file", [pth.name for pth in examples_dir.glob("*.ipynb")])
def test_notebooks(nb_file):
    with open(examples_dir / nb_file) as f:
        nb = nbformat.read(f, as_version=4)
    for cell in nb["cells"]:
        for line in cell["source"].splitlines():
            if line.startswith("import ") and "ppu_helpers" not in line:
                module = line.split()[1]
                pytest.importorskip(module)

    run(
        [
            "jupyter",
            "nbconvert",
            "--execute",
            "--ExecutePreprocessor.timeout=180000",
            "--inplace",
            nb_file,
        ],
        cwd=examples_dir,
        check=True,
    )
    run(
        [
            "jupyter",
            "nbconvert",
            "--ClearOutputPreprocessor.enabled=True",
            "--ClearMetadataPreprocessor.enabled=True",
            "--inplace",
            nb_file,
        ],
        cwd=examples_dir,
        check=True,
    )
