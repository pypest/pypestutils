"""Tests for example notebooks."""
import logging
import pytest
import os


# @pytest.mark.parametrize(
#     "nb_file",
#     [
#         pytest.param("exploring_lowlevel_pypestutils_functions.ipynb",id="el"),
#         pytest.param("exploring_pypestutils_helpers.ipynb",id="eh"),
#         pytest.param("pypestutils_pstfrom_structured_mf6.ipynb",id="fs"),
#         pytest.param("pypestutils_pstfrom_quadtree_mf6.ipynb",id="fu"),
#     ])
# def test_notebook(nb_file):
#     nb_dir = os.path.join("examples")
#     #cwd = os.getcwd()
#     #os.chdir(nb_dir)
#     #try:
#         #os.system("jupyter nbconvert --execute --ExecutePreprocessor.timeout=180000 --inplace {0}".format(nb_file))
#     pyemu.os_utils.run("jupyter nbconvert --execute --ExecutePreprocessor.timeout=180000 --inplace {0}".format(nb_file),cwd=nb_dir)
#         #os.system("jupyter nbconvert --ClearOutputPreprocessor.enabled=True --ClearMetadataPreprocessor.enabled=True --allow-errors --inplace {0}".format(nb_file))
#     pyemu.os_utils.run("jupyter nbconvert --ClearOutputPreprocessor.enabled=True --ClearMetadataPreprocessor.enabled=True --allow-errors --inplace {0}".format(nb_file),cwd=nb_dir)
#     #except Exception as e:
#     #    os.chdir(cwd)
#     #    raise Exception("notebook {0} failed: {1}".format(nb_file,str(e)))
#     #os.chdir(cwd)
#     return

def test_notebooks():
    pyemu = pytest.importorskip("pyemu")

    nb_dir = os.path.join("examples")
    nb_files = [f for f in os.listdir(nb_dir) if f.endswith(".ipynb")]
    for nb_file in nb_files:
        pyemu.os_utils.run("jupyter nbconvert --execute --ExecutePreprocessor.timeout=180000 --inplace {0}".format(nb_file),cwd=nb_dir)
        pyemu.os_utils.run("jupyter nbconvert --ClearOutputPreprocessor.enabled=True --ClearMetadataPreprocessor.enabled=True --allow-errors --inplace {0}".format(nb_file),cwd=nb_dir)
    return



#if __name__ == "__main__":
    #test_notebook("pypestutils_pstfrom_structured_mf6.ipynb")
