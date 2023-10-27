"""Common functions for tests. For pytest fixtures, see conftest.py."""
from pathlib import Path

# key is name, value is number of arguments
pestutils_function_names = {
    "inquire_modflow_binary_file_specs": 7,
    "retrieve_error_message": 1,
    "install_structured_grid": 10,
    "get_cell_centres_structured": 4,
    "uninstall_structured_grid": 1,
    "free_all_memory": 0,
    "interp_from_structured_grid": 15,
    "interp_to_obstime": 13,
    "install_mf6_grid_from_file": 7,
    "get_cell_centres_mf6": 5,
    "uninstall_mf6_grid": 1,
    "calc_mf6_interp_factors": 9,
    "interp_from_mf6_depvar_file": 12,
    "extract_flows_from_cbc_file": 15,
    "calc_kriging_factors_2d": 19,
    "calc_kriging_factors_auto_2d": 14,
    "calc_kriging_factors_3d": 28,
    "krige_using_file": 10,
    "build_covar_matrix_2d": 12,
    "build_covar_matrix_3d": 16,
    "calc_structural_overlay_factors": 15,
    "interpolate_blend_using_file": 10,
    "ipd_interpolate_2d": 14,
    "ipd_interpolate_3d": 20,
    "initialize_randgen": 1,
    "fieldgen2d_sva": 16,
    "fieldgen3d_sva": 21,
}

data_dir = Path(__file__).parent / "data"
