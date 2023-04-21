import os

os.chdir("etc")
os.system("python prep_src_dir.py")
os.chdir("..")
os.system("meson setup builddir")
os.system("meson compile -C builddir")