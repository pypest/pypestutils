"""Locate pestutils shared library by any means necessary."""
import ctypes
import os
import platform
from ctypes.util import find_library
from pathlib import Path

# the current working directory of this file
pkg_dir = Path(__file__).parent

# generate a bunch of candidate locations where the
# shared library *might* be hanging out
_candidates = [
    os.environ.get("PESTUTILS_LIBRARY", None),
    str(pkg_dir / "lib"),  # see scripts/build_lib.sh
    str(pkg_dir.parent / "inst" / ("bin" if os.name == "nt" else "lib")),
    ".",
]


def load() -> ctypes.CDLL:
    """Load the pestutils shared library.

    Returns
    -------
    ctypes.CDLL
        Loaded shared library
    """
    if os.name == "nt":
        lib_name = "pestutils.dll"

        # get the current PATH
        oldenv = os.environ.get("PATH", "").strip().rstrip(";")
        # run through our list of candidate locations
        for path in _candidates:
            if not path or not os.path.exists(path):
                continue
            # temporarily add the path to the PATH environment variable
            # so Windows can find additional DLL dependencies.
            os.environ["PATH"] = ";".join([path, oldenv])
            try:
                rt = ctypes.cdll.LoadLibrary(os.path.join(path, lib_name))
                if rt is not None:
                    print("lib found at",path)
                    return rt
            except OSError:
                pass
            except BaseException as err:
                print(f"pypestutils.finder unexpected error: {err!s}")
            finally:
                os.environ["PATH"] = oldenv
        raise OSError(f"could not find or load {lib_name}")

    elif os.name == "posix":
        # posix includes both mac and linux
        # use the extension for the specific platform
        if platform.system() == "Darwin":
            # macos shared libraries are `.dylib`
            lib_name = "libpestutils.dylib"
        else:
            # linux shared libraries are `.so`
            lib_name = "libpestutils.so"

        # get the starting working directory
        cwd = os.getcwd()
        for cand in _candidates:
            if cand is None:
                continue
            elif os.path.isdir(cand):
                # if our candidate is a directory use best guess
                path = cand
                target = os.path.join(cand, lib_name)
            elif os.path.isfile(cand):
                # if candidate is just a file use that
                path = os.path.split(cand)[0]
                target = cand
            else:
                continue

            if not os.path.exists(target):
                continue

            try:
                # move to the location we're checking
                os.chdir(path)
                # try loading the target file candidate
                rt = ctypes.cdll.LoadLibrary(target)
                if rt is not None:
                    print("lib found at",path)
                    return rt
            except BaseException as err:
                print(f"pypestutils.finder ({target}) unexpected error: {err!s}")
            finally:
                os.chdir(cwd)

    try:
        # try loading library using LD path search
        path = find_library("libpestutils")
        if path is not None:
            print("lib found at",path)
            return ctypes.cdll.LoadLibrary(path)

    except BaseException:
        pass

    raise OSError("Could not load pestutils library")
