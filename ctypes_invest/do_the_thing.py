import os

import platform

if "darwin" in platform.platform().lower():
    libname = "libfib.dylib"
elif "win" in platform.platform().lower():
    libname = 'libfib.dll'
else:
    libname = "libfib.so"

fort_comp = "ifort"
c_comp = "gcc"

tags = ["dylib","so","a","dll","o","mod"]
del_files = [f for f in os.listdir(".") if f.split('.')[0] in tags]
for f in del_files:
    os.remove(f)

os.system("{0} -c fib1.f90".format(fort_comp))
os.system("{0} -fPIC -shared -o {1} fibby_int.c fib1.o".format(c_comp,libname))

from ctypes import CDLL, POINTER, c_int, c_double
import numpy as np
fibby = CDLL(libname)
a = np.zeros(7)
fibby.c_fib(a.ctypes.data_as(POINTER(c_double)), c_int(7))
print(a)

