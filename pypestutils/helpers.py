import os
import numpy as np
import pandas as pd

from .pestutilslib import PestUtilsLib


def get_grid_info_from_gridspec(gridspec_fname):
    sr = SpatialReference.from_gridspec(gridspec_fname)
    return {
        "easting": sr.xcentergrid.flatten(),
        "northing": sr.ycentergrid.flatten(),
        "area": sr.areagrid.flatten(),
        "nrow": sr.nrow,
        "ncol": sr.ncol,
    }


def get_grid_info_from_mf6_grb(grb_fname):
    pass


def get_2d_pp_info_structured_grid(
    pp_space,
    gridspec_fname,
    zone_array=None,
    existing_array=None,
    name_prefix="pp",
    bearing_array=None,
    aniso_array=None,
    corrlen_array=None,
):
    sr = SpatialReference.from_gridspec(gridspec_fname)
    pname, px, py, pzone, pval = [], [], [], [], []
    pi, pj = [], []
    count = 0
    for i in range(int(pp_space / 2), sr.nrow, pp_space):
        for j in range(int(pp_space / 2), sr.ncol, pp_space):
            if zone_array is not None and zone_array[i, j] <= 0:
                continue

            if existing_array is not None:
                pval.append(existing_array[i, j])
            else:
                pval.append(1)
            px.append(sr.xcentergrid[i, j])
            py.append(sr.ycentergrid[i, j])
            if zone_array is not None:
                pzone.append(zone_array[i, j])
            else:
                pzone.append(1)
            pname.append(name_prefix + "{0}".format(count))
            pi.append(i)
            pj.append(j)
            count += 1
    df = pd.DataFrame(
        {
            "name": pname,
            "x": px,
            "y": py,
            "zone": pzone,
            "value": pval,
            "i": pi,
            "j": pj,
        },
        index=pname,
    )
    df.loc[:, "bearing"] = 0.0
    if bearing_array is not None:
        df.loc[:, "bearing"] = bearing_array[df.i, df.j]
    df.loc[:, "aniso"] = 1.0
    if aniso_array is not None:
        df.loc[:, "aniso"] = aniso_array[df.i, df.j]
    df.loc[:, "corrlen"] = (
        max(sr.xcentergrid.max(), sr.ycentergrid.max()) * pp_space * 5
    )  # ?
    if corrlen_array is not None:
        df.loc[:, "corrlen"] = corrlen_array[df.i, df.j]
    df["zone"] = df.zone.astype(int)

    return df


def interpolate_with_sva_pilotpoints_2d(
    pp_info,
    gridinfo_fname,
    vartype="exp",
    krigtype="ordinary",
    vartransform="none",
    max_pts=50,
    min_pts=1,
    search_dist=1e30,
    zone_array=1,
):
    # todo somechecks on pp_info
    req_cols = ["name", "x", "y", "value"]
    missing = []
    for req_col in req_cols:
        if req_col not in pp_info.columns:
            missing.append(req_col)
    if len(missing) > 0:
        raise Exception(
            "the following required columns are not in pp_info:{0}".format(
                ",".join(missing)
            )
        )

    if "zone" not in pp_info:
        pp_info.loc[:, "zone"] = 1

    nnodes, nrow, ncol = None, None, None
    easting, northing, area = None, None, None
    try:
        sr = SpatialReference.from_gridspec(gridinfo_fname)
        nnodes = sr.nrow * sr.ncol
        nrow = sr.nrow
        ncol = sr.ncol
        easting = sr.xcentergrid
        northing = sr.ycentergrid
        area = sr.areagrid
    except Exception as e:
        # some messaging here
        # then try for an mf6 unstructured grid

        raise Exception(
            "failed to load grid spec file {0}: {1}".format(gridspec_fname, str(e))
        )

    if not isinstance(zone_array, np.ndarray):
        zone_array = np.ones((nnodes), dtype=int)
    elif zone_array.dtype != int:
        # TODO warn here
        zone_array = zone_array.astype(int)

    lib = PestUtilsLib()

    hyperfac_fname = "temp.fac"
    hyperfac_ftype = "text"
    hyperbearing = 0.0
    hyperaniso = 1.0
    hypervartype = "exp"
    hyperkrigtype = "ordinary"
    hypertrans = "none"

    fac_files = []

    bearing = np.zeros_like(easting)
    if "bearing" in pp_info.columns:
        hypernoint = pp_info.bearing.mean()
        hyperfac_fname = "tempbearing.fac"
        npts = lib.calc_kriging_factors_auto_2d(
            pp_info.x.values,
            pp_info.y.values,
            pp_info.zone.values.astype(int),
            easting.flatten(),
            northing.flatten(),
            zone_array.flatten().astype(int),
            hyperkrigtype,
            hyperaniso,
            hyperbearing,
            hyperfac_fname,
            hyperfac_ftype,
        )
        result = lib.krige_using_file(
            hyperfac_fname,
            hyperfac_ftype,
            nnodes,
            hyperkrigtype,
            hypertrans,
            pp_info.bearing.values,
            hypernoint,
            hypernoint,
        )
        bearing = result["targval"]
        fac_files.append(hyperfac_fname)

    aniso = np.zeros_like(easting)
    if "aniso" in pp_info.columns:
        hypernoint = pp_info.aniso.mean()
        hyperfac_fname = "tempaniso.fac"
        npts = lib.calc_kriging_factors_auto_2d(
            pp_info.x.values,
            pp_info.y.values,
            pp_info.zone.values,
            easting.flatten(),
            northing.flatten(),
            zone_array.flatten().astype(int),
            hyperkrigtype,
            hyperaniso,
            hyperbearing,
            hyperfac_fname,
            hyperfac_ftype,
        )
        result = lib.krige_using_file(
            hyperfac_fname,
            hyperfac_ftype,
            nnodes,
            hyperkrigtype,
            hypertrans,
            pp_info.aniso.values,
            hypernoint,
            hypernoint,
        )
        aniso = result["targval"]

    use_auto = False
    corrlen = None
    if "corrlen" in pp_info.columns:
        hypernoint = pp_info.corrlen.mean()
        hyperfac_fname = "tempcorrlen.fac"
        npts = lib.calc_kriging_factors_auto_2d(
            pp_info.x.values,
            pp_info.y.values,
            pp_info.zone.values,
            easting.flatten(),
            northing.flatten(),
            zone_array.flatten().astype(int),
            hyperkrigtype,
            hyperaniso,
            hyperbearing,
            hyperfac_fname,
            hyperfac_ftype,
        )
        result = lib.krige_using_file(
            hyperfac_fname,
            hyperfac_ftype,
            nnodes,
            hyperkrigtype,
            hypertrans,
            pp_info.corrlen.values,
            hypernoint,
            hypernoint,
        )
        corrlen = result["targval"]
        fac_files.append(hyperfac_fname)
        use_auto = False

    for fac_file in fac_files:
        try:
            os.remove(fac_file)
        except Exception as e:
            pass

    # todo: maybe make these args?
    fac_fname = "var.fac"
    fac_ftype = "binary"
    noint = pp_info.loc[:, "value"].mean()
    if use_auto:
        npts = lib.calc_kriging_factors_auto_2d(
            pp_info.x.values,
            pp_info.y.values,
            pp_info.zone.values,
            easting.flatten(),
            northing.flatten(),
            zone_array.flatten().astype(int),
            krigtype,
            aniso.flatten(),
            bearing.flatten(),
            fac_fname,
            fac_ftype,
        )
    else:
        npts = lib.calc_kriging_factors_2d(
            pp_info.x.values,
            pp_info.y.values,
            pp_info.zone.values,
            easting.flatten(),
            northing.flatten(),
            zone_array.flatten().astype(int),
            vartype,
            krigtype,
            corrlen.flatten(),
            aniso.flatten(),
            bearing.flatten(),
            search_dist,
            max_pts,
            min_pts,
            fac_fname,
            fac_ftype,
        )

    result = lib.krige_using_file(
        fac_fname,
        fac_ftype,
        nnodes,
        krigtype,
        vartransform,
        pp_info.loc[:, "value"].values,
        noint,
        noint,
    )

    if nrow is not None:
        arr = result["targval"].reshape(nrow, ncol)
    else:
        arr = results[targval]
    return arr


def generate_2d_grid_realizations(
    gridinfo_fname,
    num_reals=100,
    variotype="exp",
    mean=1.0,
    variance=1.0,
    variorange=None,
    variotransform="none",
    zone_array=None,
    varioaniso=1.0,
    variobearing=0.0,
    random_seed=12345,
):
    nrow, ncol = None, None
    easting, northing, area = None, None, None
    try:
        sr = SpatialReference.from_gridspec(gridinfo_fname)
        nnodes = sr.nrow * sr.ncol
        nrow = sr.nrow
        ncol = sr.ncol
        easting = sr.xcentergrid
        northing = sr.ycentergrid
        area = sr.areagrid

    except Exception as e:
        # some messaging here
        # then try for an mf6 unstructured grid
        raise Exception(
            "failed to load grid spec file {0}: {1}".format(gridspec_fname, str(e))
        )

    if not isinstance(mean, np.ndarray):
        mean = np.zeros((nnodes)) + mean
    if not isinstance(variance, np.ndarray):
        variance = np.zeros((nnodes)) + variance
    if variorange is None:
        variorange = np.zeros((nnodes)) + max(sr.delc.max(), sr.delr.max()) * 3  # ?
    elif not isinstance(variorange, np.ndarray):
        variorange = np.zeros((nnodes)) + variorange

    if not isinstance(variobearing, np.ndarray):
        variobearing = np.zeros((nnodes)) + variobearing

    if not isinstance(varioaniso, np.ndarray):
        varioaniso = np.zeros((nnodes)) + varioaniso

    if not isinstance(zone_array, np.ndarray):
        zone_array = np.ones((nnodes), dtype=int)
    elif zone_array.dtype != int:
        # TODO warn here
        zone_array = zone_array.astype(int)

    lib = PestUtilsLib()
    lib.initialize_randgen(random_seed)

    power = 1.0

    reals = lib.fieldgen2d_sva(
        easting.flatten(),
        northing.flatten(),
        area.flatten(),
        zone_array.flatten(),
        mean.flatten(),
        variance.flatten(),
        variorange.flatten(),
        varioaniso.flatten(),
        variobearing.flatten(),
        variotransform,
        variotype,
        power,
        num_reals,
    )
    if nrow is not None:
        return reals.transpose().reshape((num_reals, nrow, ncol))
    else:
        return reals.transpose()


class SpatialReference(object):
    """
    a class to locate a structured model grid in x-y space.

    Args:

        delr (`numpy ndarray`): the model discretization delr vector (An array of spacings along a row)
        delc (`numpy ndarray`): the model discretization delc vector (An array of spacings along a column)
        xul (`float`): The x coordinate of the upper left corner of the grid. Enter either xul and yul or xll and yll.
        yul (`float`): The y coordinate of the upper left corner of the grid. Enter either xul and yul or xll and yll.
        rotation (`float`): The counter-clockwise rotation (in degrees) of the grid
    """

    def __init__(self, delr, delc, xul, yul, rotation=0.0):
        self.xul = float(xul)
        self.yul = float(yul)
        self.rotation = float(rotation)
        for delrc in [delr, delc]:
            if isinstance(delrc, float) or isinstance(delrc, int):
                msg = (
                    "delr and delcs must be an array or sequences equal in "
                    "length to the number of rows/columns."
                )
                raise TypeError(msg)

        self.delc = np.atleast_1d(np.array(delc)).astype(np.float64)
        self.delr = np.atleast_1d(np.array(delr)).astype(np.float64)

        self._xgrid = None
        self._ygrid = None
        self._xcentergrid = None
        self._ycentergrid = None

    @property
    def xll(self):
        # calculate coords for lower left corner
        xll = self.xul - (np.sin(self.theta) * self.yedge[0])
        return xll

    @property
    def yll(self):
        yll = self.yul - (np.cos(self.theta) * self.yedge[0])
        return yll

    @property
    def nrow(self):
        return self.delc.shape[0]

    @property
    def ncol(self):
        return self.delr.shape[0]

    @classmethod
    def from_gridspec(cls, gridspec_file):
        f = open(gridspec_file, "r")
        raw = f.readline().strip().split()
        nrow = int(raw[0])
        ncol = int(raw[1])
        raw = f.readline().strip().split()
        xul, yul, rot = float(raw[0]), float(raw[1]), float(raw[2])
        delr = []
        j = 0
        while j < ncol:
            raw = f.readline().strip().split()
            for r in raw:
                if "*" in r:
                    rraw = r.split("*")
                    for n in range(int(rraw[0])):
                        delr.append(float(rraw[1]))
                        j += 1
                else:
                    delr.append(float(r))
                    j += 1
        delc = []
        i = 0
        while i < nrow:
            raw = f.readline().strip().split()
            for r in raw:
                if "*" in r:
                    rraw = r.split("*")
                    for n in range(int(rraw[0])):
                        delc.append(float(rraw[1]))
                        i += 1
                else:
                    delc.append(float(r))
                    i += 1
        f.close()
        return cls(np.array(delr), np.array(delc), xul=xul, yul=yul, rotation=rot)

    @property
    def theta(self):
        return -self.rotation * np.pi / 180.0

    @property
    def xedge(self):
        return self.get_xedge_array()

    @property
    def yedge(self):
        return self.get_yedge_array()

    @property
    def xgrid(self):
        if self._xgrid is None:
            self._set_xygrid()
        return self._xgrid

    @property
    def ygrid(self):
        if self._ygrid is None:
            self._set_xygrid()
        return self._ygrid

    @property
    def xcenter(self):
        return self.get_xcenter_array()

    @property
    def ycenter(self):
        return self.get_ycenter_array()

    @property
    def ycentergrid(self):
        if self._ycentergrid is None:
            self._set_xycentergrid()
        return self._ycentergrid

    @property
    def xcentergrid(self):
        if self._xcentergrid is None:
            self._set_xycentergrid()
        return self._xcentergrid

    @property
    def areagrid(self):
        dr, dc = np.meshgrid(self.delr, self.delc)
        return dr * dc

    def _set_xycentergrid(self):
        self._xcentergrid, self._ycentergrid = np.meshgrid(self.xcenter, self.ycenter)
        self._xcentergrid, self._ycentergrid = self.transform(
            self._xcentergrid, self._ycentergrid
        )

    def _set_xygrid(self):
        self._xgrid, self._ygrid = np.meshgrid(self.xedge, self.yedge)
        self._xgrid, self._ygrid = self.transform(self._xgrid, self._ygrid)

    def get_xedge_array(self):
        """
        Return a numpy one-dimensional float array that has the cell edge x
        coordinates for every column in the grid in model space - not offset
        or rotated.  Array is of size (ncol + 1)

        """
        assert self.delr is not None and len(self.delr) > 0, (
            "delr not passed to " "spatial reference object"
        )
        xedge = np.concatenate(([0.0], np.add.accumulate(self.delr)))
        return xedge

    def get_yedge_array(self):
        """
        Return a numpy one-dimensional float array that has the cell edge y
        coordinates for every row in the grid in model space - not offset or
        rotated. Array is of size (nrow + 1)

        """
        assert self.delc is not None and len(self.delc) > 0, (
            "delc not passed to " "spatial reference object"
        )
        length_y = np.add.reduce(self.delc)
        yedge = np.concatenate(([length_y], length_y - np.add.accumulate(self.delc)))
        return yedge

    def get_xcenter_array(self):
        """
        Return a numpy one-dimensional float array that has the cell center x
        coordinate for every column in the grid in model space - not offset or rotated.

        """
        assert self.delr is not None and len(self.delr) > 0, (
            "delr not passed to " "spatial reference object"
        )
        x = np.add.accumulate(self.delr) - 0.5 * self.delr
        return x

    def get_ycenter_array(self):
        """
        Return a numpy one-dimensional float array that has the cell center x
        coordinate for every row in the grid in model space - not offset of rotated.

        """
        assert self.delc is not None and len(self.delc) > 0, (
            "delc not passed to " "spatial reference object"
        )
        Ly = np.add.reduce(self.delc)
        y = Ly - (np.add.accumulate(self.delc) - 0.5 * self.delc)
        return y

    @staticmethod
    def rotate(x, y, theta, xorigin=0.0, yorigin=0.0):
        """
        Given x and y array-like values calculate the rotation about an
        arbitrary origin and then return the rotated coordinates.  theta is in
        degrees.

        """
        # jwhite changed on Oct 11 2016 - rotation is now positive CCW
        # theta = -theta * np.pi / 180.
        theta = theta * np.pi / 180.0

        xrot = xorigin + np.cos(theta) * (x - xorigin) - np.sin(theta) * (y - yorigin)
        yrot = yorigin + np.sin(theta) * (x - xorigin) + np.cos(theta) * (y - yorigin)
        return xrot, yrot

    def transform(self, x, y, inverse=False):
        """
        Given x and y array-like values, apply rotation, scale and offset,
        to convert them from model coordinates to real-world coordinates.
        """
        if isinstance(x, list):
            x = np.array(x)
            y = np.array(y)
        if not np.isscalar(x):
            x, y = x.copy(), y.copy()

        if not inverse:
            x += self.xll
            y += self.yll
            x, y = SpatialReference.rotate(
                x, y, theta=self.rotation, xorigin=self.xll, yorigin=self.yll
            )
        else:
            x, y = SpatialReference.rotate(x, y, -self.rotation, self.xll, self.yll)
            x -= self.xll
            y -= self.yll
        return x, y

    def get_extent(self):
        """
        Get the extent of the rotated and offset grid

        """
        x0 = self.xedge[0]
        x1 = self.xedge[-1]
        y0 = self.yedge[0]
        y1 = self.yedge[-1]

        # upper left point
        x0r, y0r = self.transform(x0, y0)

        # upper right point
        x1r, y1r = self.transform(x1, y0)

        # lower right point
        x2r, y2r = self.transform(x1, y1)

        # lower left point
        x3r, y3r = self.transform(x0, y1)

        xmin = min(x0r, x1r, x2r, x3r)
        xmax = max(x0r, x1r, x2r, x3r)
        ymin = min(y0r, y1r, y2r, y3r)
        ymax = max(y0r, y1r, y2r, y3r)

        return (xmin, xmax, ymin, ymax)

    def get_vertices(self, i, j):
        """Get vertices for a single cell or sequence if i, j locations."""
        pts = []
        xgrid, ygrid = self.xgrid, self.ygrid
        pts.append([xgrid[i, j], ygrid[i, j]])
        pts.append([xgrid[i + 1, j], ygrid[i + 1, j]])
        pts.append([xgrid[i + 1, j + 1], ygrid[i + 1, j + 1]])
        pts.append([xgrid[i, j + 1], ygrid[i, j + 1]])
        pts.append([xgrid[i, j], ygrid[i, j]])
        if np.isscalar(i):
            return pts
        else:
            vrts = np.array(pts).transpose([2, 0, 1])
            return [v.tolist() for v in vrts]

    def get_ij(self, x, y):
        """Return the row and column of a point or sequence of points
        in real-world coordinates.

        Args:
            x (`float`): scalar or sequence of x coordinates
            y (`float`): scalar or sequence of y coordinates

        Returns:
            tuple of

            - **int** : row or sequence of rows (zero-based)
            - **int** : column or sequence of columns (zero-based)
        """
        if np.isscalar(x):
            c = (np.abs(self.xcentergrid[0] - x)).argmin()
            r = (np.abs(self.ycentergrid[:, 0] - y)).argmin()
        else:
            xcp = np.array([self.xcentergrid[0]] * (len(x)))
            ycp = np.array([self.ycentergrid[:, 0]] * (len(x)))
            c = (np.abs(xcp.transpose() - x)).argmin(axis=0)
            r = (np.abs(ycp.transpose() - y)).argmin(axis=0)
        return r, c

    def write_gridspec(self, filename):
        """write a PEST-style grid specification file"""
        f = open(filename, "w")
        f.write("{0:10d} {1:10d}\n".format(self.delc.shape[0], self.delr.shape[0]))
        f.write(
            "{0:15.6E} {1:15.6E} {2:15.6E}\n".format(
                self.xul,
                self.yul,
                self.rotation,
            )
        )

        for r in self.delr:
            f.write("{0:15.6E} ".format(r))
        f.write("\n")
        for c in self.delc:
            f.write("{0:15.6E} ".format(c))
        f.write("\n")
        return


if __name__ == "__main__":
    pass
