# ChangeLog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- `fieldgen2d_sva_z()` and `fieldgen3d_sva_z()`: generate stochastic fields from user-supplied standard normal deviates instead of internally drawn random numbers; new example notebook `fieldgen_with_supplied_z.ipynb`
- Helpers `generate_2d_grid_field_from_z()` and `generate_3d_grid_field_from_z()`: map z values (array, Series or DataFrame) to a property field, for use in forward run scripts where z values are adjustable parameters
- Example notebook `pestpp_ies_with_z_parameters.ipynb`: history matching per-cell z parameters and hyper parameter pilot points with `pestpp-ies` on the PEST++ synth2d model
- Logging control: `pypestutils.logger.set_logger_level()` and `temporary_logger_level()`, and a `logger_level` argument on the helper functions that applies for the duration of the call
### Changed
- The "PestUtilsLib" logger now defaults to the WARNING level, so library calls are no longer echoed to stdout; use `set_logger_level("INFO")` (or a `logger_level` argument) to echo them
- `PestUtilsLib(logger_level=...)` now defaults to None, which leaves the shared "PestUtilsLib" logger level unchanged; previously every new instance (including those created inside helpers) reset it to INFO
- `fieldgen2d_sva()` and `fieldgen3d_sva()` now draw deviates and delegate to the `_z` variants (output is unchanged for a given seed)
### Fixed
- `PestUtilsLibError` now subclasses `Exception` instead of `BaseException`, so `except Exception` catches library errors
- `calc_mf6_interp_factors()` freed `cellymax` twice and never freed `cellymin` on exit
- `mod2obs_mf6()` and `get_grid_info_from_mf6_grb()` now always uninstall the MODFLOW 6 grid and free library memory, so a failed call no longer makes later calls fail with 'already-installed MODFLOW 6 grid'

## [0.3.0] - 2025-02-12
### Changed
- Set minimum Python 3.9 (#36)
- Updated cibuildwheel workflow, build macos arm64 wheels from GHA (#30, #32)
- Fix finder.load() for conda and LD path search
- Fix typos (#37)
- Use ruff format, apply ruff check rules, add pre-commit (#38, #45)

### Fixed
- Fix in `mod2obs_mf6` with different depvar names (#33)

## [0.2.1] - 2024-01-23
### Fixed
- Fix AttributeError: 'DataFrame' object has no attribute 'site' (#21)
- Fix finder.load() for conda and LD path search

### Changed
- Aniso default to 1 in non hyperpar pp interp (#19)

## [0.2.0] - 2023-10-27
### Added
- Start this change log file
- Add `get_cell_centres_structured` and `get_cell_centres_mf6`
- Add basic docs and notebook examples

### Changed
- Use `vs_module_defs` instead of compiler directives (#12)

## [0.1.0] - 2023-09-13
### Added
- Initial pre-alpha release

[Unreleased]: https://github.com/pypest/pypestutils/compare/v0.3.0...develop
[0.3.0]: https://github.com/pypest/pypestutils/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/pypest/pypestutils/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/pypest/pypestutils/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/pypest/pypestutils/releases/tag/v0.1.0
