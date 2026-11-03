"""MODFLOW 6 DISU grids: install, cell centres, node layers and interpolation factors.

The grids are layered models built with vorflow and run with MODFLOW 6.7:

- vorflow_refill: three layers; the third has a finer plan grid inside a pit.
- vorflow_sublayers: one layer split into three sublayers inside the pit, so five
  layers, two of which have cells only inside the pit.

Each ``.layers`` file gives the (one-based) layer of each node.
"""

from pathlib import Path

import numpy as np
import pytest

from pypestutils.pestutilslib import PestUtilsLib, PestUtilsLibError

data_dir = Path(__file__).parent / "data"
GRIDS = ["vorflow_refill", "vorflow_sublayers"]


def node_layers(name):
    return np.loadtxt(data_dir / f"{name}.layers", dtype=np.int32)


def read_factors(path):
    """One list of (zero-based node, weight) per point from a text factor file."""
    lines = Path(path).read_text().splitlines()
    npts = int(lines[0].split()[0])
    out = []
    for line in lines[1 + npts : 1 + 2 * npts]:
        words = line.split()
        out.append(
            [
                (int(words[1 + 2 * k]) - 1, float(words[2 + 2 * k]))
                for k in range(int(words[0]))
            ]
        )
    return out


def read_disu_heads(path):
    """The last record of a MODFLOW 6 DISU head file (one array of all nodes)."""
    raw = Path(path).read_bytes()
    header = np.dtype(
        [
            ("kstp", "<i4"),
            ("kper", "<i4"),
            ("pertim", "<f8"),
            ("totim", "<f8"),
            ("text", "S16"),
            ("ncol", "<i4"),
            ("nrow", "<i4"),
            ("ilay", "<i4"),
        ]
    )
    pos, heads = 0, None
    while pos < len(raw):
        head = np.frombuffer(raw, header, 1, pos)[0]
        pos += header.itemsize
        n = int(head["ncol"]) * int(head["nrow"])
        heads = np.frombuffer(raw, "<f8", n, pos)
        pos += 8 * n
    return heads


def random_points(layers, n=40, seed=0):
    rng = np.random.default_rng(seed)
    x = rng.uniform(15.0, 185.0, n * layers.max())
    y = rng.uniform(15.0, 105.0, n * layers.max())
    return x, y, np.repeat(np.arange(1, layers.max() + 1), n).astype(np.int32)


@pytest.mark.parametrize(
    "name, ncells", [("vorflow_refill", 549), ("vorflow_sublayers", 558)]
)
def test_install_disu_grid(name, ncells):
    lib = PestUtilsLib()
    inst = lib.install_mf6_grid_from_file(name, data_dir / f"{name}.disu.grb")
    assert inst == {
        "idis": 3,
        "ncells": ncells,
        "ndim1": ncells,
        "ndim2": 1,
        "ndim3": 1,
    }
    x, y, z = lib.get_cell_centres_mf6(name, ncells)
    # vorflow puts the generators of edge cells on the domain outline.
    assert np.all((x >= 0) & (x <= 200) & (y >= 0) & (y <= 120))
    # Layers are numbered from the top: the mean mid-depth falls with each layer.
    layers = node_layers(name)
    means = [z[layers == k].mean() for k in range(1, layers.max() + 1)]
    assert np.all(np.diff(means) < 0)
    assert lib.get_mf6_grid_crs(name) == ""
    lib.uninstall_mf6_grid(name)


def test_install_mf6_disu_layers_checks_its_input():
    lib = PestUtilsLib()
    lib.install_mf6_grid_from_file("disu", data_dir / "vorflow_refill.disu.grb")
    layers = node_layers("vorflow_refill")
    with pytest.raises(PestUtilsLibError, match="NCELLS"):
        lib.install_mf6_disu_layers("disu", layers[:-1])
    with pytest.raises(PestUtilsLibError, match="zero or less"):
        lib.install_mf6_disu_layers("disu", layers - 1)
    with pytest.raises(PestUtilsLibError, match="not correspond"):
        lib.install_mf6_disu_layers("other", layers)
    lib.uninstall_mf6_grid("disu")
    lib.install_mf6_grid_from_file("disv", data_dir / "vdl.disv.grb")
    with pytest.raises(PestUtilsLibError, match="not a DISU grid"):
        lib.install_mf6_disu_layers("disv", layers)
    lib.uninstall_mf6_grid("disv")


@pytest.mark.parametrize("name", GRIDS)
def test_disu_factors_use_the_point_layer_and_are_linear(tmp_path, name):
    lib = PestUtilsLib()
    inst = lib.install_mf6_grid_from_file(name, data_dir / f"{name}.disu.grb")
    layers = node_layers(name)
    lib.install_mf6_disu_layers(name, layers)
    cx, cy, _ = lib.get_cell_centres_mf6(name, inst["ncells"])
    x, y, layer = random_points(layers)
    factorfile = tmp_path / "factors.txt"
    success = lib.calc_mf6_interp_factors(
        name, x, y, layer, factorfile, "text", tmp_path / "f.bln"
    )
    lib.uninstall_mf6_grid(name)
    # A field linear within each layer, with a different offset per layer.
    field = 3.0 * cx - 2.0 * cy + 100.0 * layers
    found = 0
    for i, factors in enumerate(read_factors(factorfile)):
        assert bool(factors) == bool(success[i])
        if not factors:
            continue
        node = np.array([n for n, _ in factors])
        weight = np.array([w for _, w in factors])
        assert np.all(layers[node] == layer[i])
        np.testing.assert_allclose(weight.sum(), 1.0, atol=1e-6)
        if len(factors) <= 4:  # barycentric weights reproduce a linear field
            found += 1
            np.testing.assert_allclose(
                weight @ field[node],
                3.0 * x[i] - 2.0 * y[i] + 100.0 * layer[i],
                atol=1e-9,
            )
    assert found > 0.8 * success.sum()
    # Points in layers present everywhere are all found; pit-only layers miss the rest.
    full = [k for k in range(1, layers.max() + 1) if (layers == k).sum() >= 166]
    assert success[np.isin(layer, full)].all()


@pytest.mark.parametrize("name", GRIDS)
def test_interp_from_disu_heads_applies_the_factors(tmp_path, name):
    lib = PestUtilsLib()
    lib.install_mf6_grid_from_file(name, data_dir / f"{name}.disu.grb")
    layers = node_layers(name)
    lib.install_mf6_disu_layers(name, layers)
    x, y, layer = random_points(layers, n=10, seed=1)
    factorfile = tmp_path / "factors.bin"
    lib.calc_mf6_interp_factors(
        name, x, y, layer, factorfile, "binary", tmp_path / "b.bln"
    )
    lib.uninstall_mf6_grid(name)
    res = lib.interp_from_mf6_depvar_file(
        data_dir / f"{name}.hds",
        factorfile,
        "binary",
        1,
        "head",
        1e20,
        0,
        -999.0,
        len(x),
    )
    assert res["nproctime"] == 1
    textfile = tmp_path / "factors.txt"
    lib.install_mf6_grid_from_file(name, data_dir / f"{name}.disu.grb")
    lib.install_mf6_disu_layers(name, layers)
    lib.calc_mf6_interp_factors(name, x, y, layer, textfile, "text", tmp_path / "t.bln")
    lib.uninstall_mf6_grid(name)
    heads = read_disu_heads(data_dir / f"{name}.hds")
    expected = [
        sum(w * heads[n] for n, w in f) if f else -999.0 for f in read_factors(textfile)
    ]
    np.testing.assert_allclose(res["simstate"][0], expected, rtol=1e-12)


def test_disu_without_layers_is_a_single_layer(tmp_path):
    lib = PestUtilsLib()
    lib.install_mf6_grid_from_file("disu", data_dir / "vorflow_refill.disu.grb")
    with pytest.raises(PestUtilsLibError, match="greater than NLAY"):
        lib.calc_mf6_interp_factors(
            "disu", [100.0], [60.0], [2], tmp_path / "f.txt", "text", tmp_path / "f.bln"
        )
    success = lib.calc_mf6_interp_factors(
        "disu", [100.0], [60.0], [1], tmp_path / "f.txt", "text", tmp_path / "f.bln"
    )
    assert success[0] == 1
    lib.uninstall_mf6_grid("disu")
