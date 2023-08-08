import os
import shutil

#os.chdir("scripts")
#os.system("python prep_src_dir.py "+os.path.join("..","org_from_john"))
#os.system("python prep_src_dir.py "+os.path.join("..","org_from_john_4jul2023"))
#os.system("python prep_src_dir.py "+os.path.join("..","org_from_john_7aug2023"))
#os.chdir("..")
shutil.rmtree("builddir")
os.system("meson setup builddir")
os.system("meson compile -C builddir")