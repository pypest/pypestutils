# PyPestUtils

Suite of Python tools based on PEST utilities to support parameterization with pilot points, drawing stochastic realizations, and MODFLOW post-processing for structured and unstructured grids. This package consists of a (pre-)compiled shared fortran library, and a corresponding low-level python wrapper around the library functions.  There are also higher-level "helper" functions to further abstract the granular low-level function calls for common workflow elements.  

The low-level functions are relatively strict in their argument types - this is required to effectively pass the array-type data container references to the fortran library.  As such, the low-level python functions perform considerable type checking.  The higher-level helpers attempt to support a variety of argument types and will attempt coerce to the required type.  

This package is currently in pre-alpha development, and is not suitable for use, but early adopters are welcome to have a go.

## Examples

There are several jupyter notebook examples of using pypestutils for a structured and quadtree Freyberg model.  These notebooks rely on both [flopy](https://github.com/modflowpy/flopy) and [pyEMU](https://github.com/pypest/pyemu) to help with visualization and processing.

The use the low-level python interface to the shared fortran library, you create a `PESTUTILSLIB` instance and then can directly call the shared library routines:

```
from pypestutils.pestutilslib import PestUtilsLib
lib = PestUtilsLib() #the constructor searches for the shared lib
grid_info = lib.install_mf6_grid_from_file("grid","freyberg6.disv.grb")
easting,northing,elev = lib.get_cell_centres_mf6("grid",grid_info["ncells"])
```

The higher-level helper functions obsecure the calls the fortran library and string together multiple low-level function calls:

```
import pypestutils.helpers as helpers
grid_info = helpers.get_grid_info_from_file("freyberg6.disv.grb")
```

If a shared library routine reports an error, the low-level python function raises a `PestUtilsLibError` containing the library's error message. `PestUtilsLibError` is a subclass of `Exception`, so it can be caught with `except Exception` or explicitly:

```
from pypestutils.pestutilslib import PestUtilsLib, PestUtilsLibError
lib = PestUtilsLib()
try:
    grid_info = lib.install_mf6_grid_from_file("grid","freyberg6.disv.grb")
except PestUtilsLibError as err:
    print(f"pestutils error: {err}")
```

Messages go to stdout through a logger named "PestUtilsLib". Its default level is "WARNING", so routine messages are not echoed; set it to "INFO" to echo each library call. The level ("DEBUG", "INFO", "WARNING", "ERROR" or "CRITICAL") can be set for the session, for a block of code, for a low-level library instance, or for a single helper call:

```
from pypestutils.logger import set_logger_level, temporary_logger_level
set_logger_level("INFO")                         # for the rest of the session
with temporary_logger_level("INFO"):             # within this block only
    grid_info = helpers.get_grid_info_from_mf6_grb("freyberg6.disv.grb")
lib = PestUtilsLib(logger_level="INFO")          # when creating a library instance
grid_info = helpers.get_grid_info_from_mf6_grb("freyberg6.disv.grb", logger_level="INFO")  # one helper call
```

## Documentation

The documentation for pypestutils can be found [here](https://pypest.github.io/pypestutils/)

The documentation for the shared FORTRAN library can be found [here](docs/pestutilslib/fortran_library_documentation.md)

## Installation

### Dependencies

pypestutils requires `numpy` and `pandas`

### Easy way

Use `pip` to install a built distribution for Windows, Linux or macOS:

    pip install pypestutils

to also include optional requirements use:

    pip install pypestutils[optional]

### From a development branch

Pushing to a branch named `feat*` builds wheels (including the shared library) and publishes them to a GitHub pre-release named `branch-<branch name>`. To install (or update to) the latest build of a branch, for example `feat_iid2`:

    pip install -U pypestutils --find-links https://github.com/pypest/pypestutils/releases/expanded_assets/branch-feat_iid2

Note that `pip install git+https://...@branch` installs without the shared library.

### From source

Installation from source requires a Fortran compiler and build tools. See [BUILD.md](BUILD.md) for details.


