import os
import shutil

cwd = os.getcwd()

clear = False
pdf = False
allow_errors = True

def run_nb(nb_file, nb_dir): 
    os.chdir(nb_dir)
    worker_dirs = [d for d in os.listdir(".") if os.path.isdir(d) and d.startswith("worker")]
    for worker_dir in worker_dirs:
        shutil.rmtree(worker_dir)
    if allow_errors:
        os.system("jupyter nbconvert --execute --ExecutePreprocessor.timeout=180000 --allow-errors --inplace {0}".format(nb_file))
    else:
        os.system("jupyter nbconvert --execute --ExecutePreprocessor.timeout=180000 --inplace {0}".format(nb_file))
    if pdf:
        os.system("jupyter nbconvert --to pdf {0}".format(nb_file))
    if clear:
        os.system("jupyter nbconvert --ClearOutputPreprocessor.enabled=True --ClearMetadataPreprocessor.enabled=True --allow-errors --inplace {0}".format(nb_file))
    os.chdir(cwd)
    return


nb_dir = "."
nb_files = [f for f in os.listdir(nb_dir) if f.endswith(".ipynb")]
assert len(nb_files) > 0
nb_files.sort()
for nb_file in nb_files:
    run_nb(nb_file, nb_dir)




