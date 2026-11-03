# ChangeLog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- MODFLOW 6 DISU grids: `install_mf6_grid_from_file` reads DISU GRB files, the
  new `install_mf6_disu_layers` gives each node a layer, and
  `calc_mf6_interp_factors` then interpolates layer by layer as for DISV;
  `get_cell_centres_mf6` returns DISU node centres
### Changed
### Fixed

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
