from pathlib import Path
from subprocess import run

notebook_count = 0
for nb_file in Path(__file__).parent.rglob("*.ipynb"):
    print("clearing", nb_file)
    run(
        [
            "jupyter",
            "nbconvert",
            "--ClearOutputPreprocessor.enabled=True",
            "--ClearMetadataPreprocessor.enabled=True",
            "--inplace",
            nb_file,
        ],
        check=True,
    )
    notebook_count += 1
print(notebook_count, "notebooks cleared")
