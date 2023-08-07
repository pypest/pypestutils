#!/usr/bin/env python
"""Prepare 'src' directory and meson.build from upstream source."""
import sys
import shutil
from pathlib import Path

# org_d = os.path.join("..","org_from_john_4jul2023")
org_d = Path(sys.argv[1])
print("original directory:", org_d)

root_d = Path(globals().get("__file__", ".")).parent.parent
new_d = root_d / "src"

if new_d.exists():
    shutil.rmtree(new_d)
new_d.mkdir()

f90_files = []
driver_files = []
lib_files = []
for pth in sorted(org_d.iterdir()):
    name = pth.name
    if pth.suffix in [".f90", ".F90",".F"] and "gslib" not in name.lower():
        f90_files.append(name)
        # rewrite, normalizing line endings
        txt = pth.read_text()
        if not txt.endswith("\n"):
            # add ending EOL, if it was missing
            txt += "\n"
        newpth = new_d / name
        newpth.write_text(txt)
    else:
        continue
    if name.startswith("driver") and "subs" not in name:
        driver_files.append(name)
    elif not name.startswith("driver") and "newfunc" not in name:
        lib_files.append(name)

fmt_lib_files = ",\n".join([f"  '{lib_file}'" for lib_file in lib_files])
txt = f"""\
src_dir = '.'
lib_sources = files(
{fmt_lib_files}
)

shared_library('pestutils', lib_sources,
  name_prefix : host_machine.system() == 'windows' ? '' : 'lib',
  install : true)

pulib = static_library('pestutils_core', lib_sources)

"""
for driver_file in driver_files:
    driver_name = driver_file.split(".")[0]
    txt += (
        f"executable('{driver_name}', ['{driver_file}', 'driversubs.f90'], "
        "link_with : [pulib], install : true)\n"
    )
meson_build = new_d / "meson.build"
meson_build.write_text(txt)
print("wrote:", meson_build)
