import os
import sys
import shutil


#org_d = os.path.join("..","org_from_john_4jul2023")
org_d = sys.argv[1]
print(org_d)
new_d = os.path.join("..","src")
if os.path.exists(new_d):
    shutil.rmtree(new_d)
os.makedirs(new_d)

f90_files = [f for f in os.listdir(org_d) if f.lower().endswith(".f90")]
driver_files = [f for f in f90_files if f.lower().startswith("driver") and "subs" not in f]
lib_files = [f for f in f90_files if not f.lower().startswith("driver") and "newfunc" not in f]



for f in f90_files:
    shutil.copy2(os.path.join(org_d,f),os.path.join(new_d,f))

# bd = os.getcwd()
# os.chdir(new_d)
# for f in f90_files:
#    os.system("fprettify {0}".format(f))
# os.chdir(bd)

with open(os.path.join(new_d,"meson.build"),'w') as f:
    f.write("src_dir='.'\n")
    #f.write("lib_sources = files(src_dir,recursive=false)\n")
    #f.write("lib_sources = [s for s in sources if s[-4:] == '.f90']\n")
    f.write("lib_sources = files(\n")
    for lib_file in lib_files:
        f.write("    '{0}',\n".format(lib_file))
    f.write(")\n")
    f.write("ppucore = static_library('ppucore',lib_sources)\n")
    f.write("library('ppu',lib_sources)\n")
    for driver_file in driver_files:
        f.write("{0}exe = executable('{0}',['{1}','driversubs.f90'],link_with: [ppucore])\n".format(driver_file.split(".")[0],driver_file))



