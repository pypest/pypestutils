import shutil
from pathlib import Path
from subprocess import run

clear = False
pdf = False
allow_errors = True

exec_cmds = [
    "jupyter",
    "nbconvert",
    "--execute",
    "--ExecutePreprocessor.timeout=180000",
]
if allow_errors:
    exec_cmds.append("--allow-errors")
exec_cmds.append("--inplace")


def run_nb(nb_file, nb_dir):
    for worker_dir in nb_dir.rglob("worker*"):
        if worker_dir.isdir() and worker_dir.name.startswith("worker"):
            shutil.rmtree(worker_dir)
    run(exec_cmds + [nb_file], cwd=nb_dir, check=True)
    if pdf:
        run(["jupyter", "nbconvert", "--to", "pdf", nb_file], cwd=nb_dir, check=True)
    if clear:
        run(
            [
                "jupyter",
                "nbconvert",
                "--ClearOutputPreprocessor.enabled=True",
                "--ClearMetadataPreprocessor.enabled=True",
                "--inplace",
                nb_file,
            ],
            cwd=nb_dir,
            check=True,
        )
    return


nb_dir = Path(__file__).parent
nb_files = sorted(nb_dir.rglob("*.ipynb"))
assert len(nb_files) > 0
for nb_file in nb_files:
    run_nb(nb_file, nb_dir)
