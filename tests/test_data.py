"""Tests for data module."""
import numpy as np
import pytest

from pypestutils.data import ManyArrays, validate_scalar
from pypestutils.enum import KrigType, Prec


def test_validate_scalar():
    # test each part/keyword
    validate_scalar("foo", 2.3)
    with pytest.raises(TypeError, match="not a scalar value"):
        validate_scalar("foo", [2.3])

    validate_scalar("foo", 2.3, isfinite=True)
    with pytest.raises(TypeError, match="isfinite must be True"):
        validate_scalar("foo", 2.3, isfinite=1)
    with pytest.raises(ValueError, match="must be finite"):
        validate_scalar("foo", np.nan, isfinite=True)

    validate_scalar("foo", 5.1, gt=5.0)
    with pytest.raises(ValueError, match="must be greater than 5"):
        validate_scalar("foo", 5.0, gt=5.0)

    validate_scalar("foo", 5.0, ge=5.0)
    with pytest.raises(ValueError, match="must be greater than or equal"):
        validate_scalar("foo", 4.9, ge=5.0)

    validate_scalar("foo", 4.9, lt=5.0)
    with pytest.raises(ValueError, match="must be less than 5"):
        validate_scalar("foo", 5.0, lt=5.0)

    validate_scalar("foo", 5.0, le=5.0)
    with pytest.raises(ValueError, match="must be less than or equal"):
        validate_scalar("foo", 5.1, le=5.0)

    validate_scalar("foo", 8, isin=[1, 4, 8, 22])
    with pytest.raises(ValueError, match="must be in "):
        validate_scalar("foo", 6, isin=[1, 4, 8, 22])

    validate_scalar("foo", 1, enum=KrigType)
    validate_scalar("foo", KrigType(1), enum=KrigType)
    with pytest.raises(TypeError, match="enum must be a subclass of "):
        validate_scalar("foo", 2, enum=[2])
    with pytest.raises(TypeError, match="enum must be a subclass of "):
        validate_scalar("foo", 2, enum=int)
    with pytest.raises(TypeError, match="enum value must be either"):
        validate_scalar("foo", "simple", enum=KrigType)
    with pytest.raises(TypeError, match="is not an enum KrigType"):
        validate_scalar("foo", Prec(1), enum=KrigType)
    with pytest.raises(ValueError, match="must be in enum KrigType 0 "):
        validate_scalar("foo", 2, enum=KrigType)

    with pytest.raises(NotImplementedError, match="unhandled kwarg"):
        validate_scalar("foo", 2, notimpl=True)


def test_manyarrays_float_arrays():
    ar = ManyArrays()
    assert len(ar) == 1

    ar = ManyArrays({"ar1": [8, 4]})
    np.testing.assert_array_equal(ar.ar1, [8.0, 4.0])
    assert len(ar) == 2
    assert ar.ar1.dtype == np.float64

    ar = ManyArrays(float_any={"ar1": [5] * 2, "ar2": [3] * 2})
    np.testing.assert_array_equal(ar.ar1, [5.0] * 2)
    np.testing.assert_array_equal(ar.ar2, [3.0] * 2)

    with pytest.raises(TypeError, match="'ar_len' must be int"):
        ManyArrays(ar_len=1.0)

    with pytest.raises(ValueError, match=r"expected 'ar1' ndim to be 1"):
        ManyArrays({"ar1": 10})

    with pytest.raises(ValueError, match=r"expected 'ar1' ndim to be 1"):
        ManyArrays({"ar1": 10}, ar_len=1)

    with pytest.raises(ValueError, match=r"expected 'ar1' shape to be \(2,\)"):
        ManyArrays({"ar1": [10]}, ar_len=2)

    with pytest.raises(ValueError, match=r"expected 'ar2' shape to be \(2,\)"):
        ManyArrays({"ar1": [5] * 2, "ar2": [3] * 3})


def test_manyarrays_float_any():
    ar = ManyArrays(float_any={"ar1": 10})
    np.testing.assert_array_equal(ar.ar1, [10.0])
    assert len(ar) == 1
    assert ar.ar1.dtype == np.float64

    ar = ManyArrays(float_any={"ar1": 10.0}, ar_len=1)
    np.testing.assert_array_equal(ar.ar1, [10.0])

    ar = ManyArrays(float_any={"ar1": 10}, ar_len=3)
    np.testing.assert_array_equal(ar.ar1, [10.0] * 3)
    assert len(ar) == 3

    ar = ManyArrays(float_any={"ar1": 10.0, "ar2": [4, 2]})
    np.testing.assert_array_equal(ar.ar1, [10.0] * 2)
    np.testing.assert_array_equal(ar.ar2, [4.0, 2.0])

    ar = ManyArrays(float_any={"ar1": [8.0, 4.0]})
    np.testing.assert_array_equal(ar.ar1, [8.0, 4.0])

    ar = ManyArrays(float_any={"ar1": [8.0, 4.0]}, ar_len=2)
    np.testing.assert_array_equal(ar.ar1, [8.0, 4.0])

    ar = ManyArrays(float_any={"ar1": [5] * 2, "ar2": [3] * 2})
    np.testing.assert_array_equal(ar.ar1, [5.0] * 2)
    np.testing.assert_array_equal(ar.ar2, [3.0] * 2)

    with pytest.raises(KeyError, match="'ar1' defined more than once"):
        ManyArrays({"ar1": [1]}, float_any={"ar1": [2]})

    with pytest.raises(ValueError, match=r"expected 'ar1' shape to be \(2,\)"):
        ManyArrays(float_any={"ar1": [10.0]}, ar_len=2)

    with pytest.raises(ValueError, match=r"expected 'ar2' shape to be \(2,\)"):
        ManyArrays(float_any={"ar1": [5] * 2, "ar2": [3] * 3})


def test_manyarrays_int_any():
    ar = ManyArrays(int_any={"ar1": 10})
    np.testing.assert_array_equal(ar.ar1, [10])
    assert len(ar) == 1
    assert ar.ar1.dtype == np.int32

    ar = ManyArrays(int_any={"ar1": 10}, ar_len=1)
    np.testing.assert_array_equal(ar.ar1, [10.0])

    ar = ManyArrays(int_any={"ar1": 10}, ar_len=3)
    np.testing.assert_array_equal(ar.ar1, [10] * 3)
    assert len(ar) == 3

    ar = ManyArrays(int_any={"ar1": 10, "ar2": [4, 2]})
    np.testing.assert_array_equal(ar.ar1, [10] * 2)
    np.testing.assert_array_equal(ar.ar2, [4, 2])

    ar = ManyArrays(int_any={"ar1": [8, 4]})
    np.testing.assert_array_equal(ar.ar1, [8, 4])

    ar = ManyArrays(int_any={"ar1": [8, 4]}, ar_len=2)
    np.testing.assert_array_equal(ar.ar1, [8, 4])

    ar = ManyArrays(int_any={"ar1": [5] * 2, "ar2": [3] * 2})
    np.testing.assert_array_equal(ar.ar1, [5] * 2)
    np.testing.assert_array_equal(ar.ar2, [3] * 2)

    with pytest.raises(ValueError, match="expected 'ar1' to be integer type"):
        ManyArrays(int_any={"ar1": [2.0]})

    with pytest.raises(KeyError, match="'ar1' defined more than once"):
        ManyArrays({"ar1": [1]}, int_any={"ar1": [2]})

    with pytest.raises(ValueError, match=r"expected 'ar1' shape to be \(2,\)"):
        ManyArrays(int_any={"ar1": [10]}, ar_len=2)

    with pytest.raises(ValueError, match=r"expected 'ar2' shape to be \(2,\)"):
        ManyArrays(int_any={"ar1": [5] * 2, "ar2": [3] * 3})


def test_manyarrays_validate():
    # test each part/keyword
    with pytest.raises(TypeError, match="not a scalar value"):
        validate_scalar("foo", [2.3])

    ar = ManyArrays({"ar1": [2.0, 3.0], "ar2": [4.0, -np.inf]})
    ar.validate("ar1", isfinite=True)
    with pytest.raises(TypeError, match="isfinite must be True"):
        ar.validate("ar1", isfinite=1)
    with pytest.raises(ValueError, match="'ar2' must be finite"):
        ar.validate("ar2", isfinite=True)

    ar = ManyArrays({"ar1": [5.1], "ar2": [5.0]})
    ar.validate("ar1", gt=5.0)
    with pytest.raises(ValueError, match="'ar2' must be greater than 5"):
        ar.validate("ar2", gt=5.0)

    ar = ManyArrays({"ar1": [5.0], "ar2": [4.9]})
    ar.validate("ar1", ge=5.0)
    with pytest.raises(ValueError, match="'ar2' must be greater than or equal"):
        ar.validate("ar2", ge=5.0)

    ar = ManyArrays({"ar1": [4.9], "ar2": [5.0]})
    ar.validate("ar1", lt=5.0)
    with pytest.raises(ValueError, match="'ar2' must be less than 5"):
        ar.validate("ar2", lt=5.0)

    ar = ManyArrays({"ar1": [5.0], "ar2": [5.1]})
    ar.validate("ar1", le=5.0)
    with pytest.raises(ValueError, match="'ar2' must be less than or equal"):
        ar.validate("ar2", le=5.0)

    ar = ManyArrays(int_any={"ar1": [1, 8], "ar2": [5, 8]})
    ar.validate("ar1", isin=[1, 4, 8, 22])
    with pytest.raises(ValueError, match="'ar2' must be in "):
        ar.validate("ar2", isin=[1, 4, 8, 22])

    ar = ManyArrays(
        int_any={
            "ar1": [KrigType(1), KrigType(0)],
            "ar2": [0, 1],
            "ar3": [0, 2],
        },
        float_any={"ar4": [1.0, 2.0]},
    )
    ar.validate("ar1", enum=KrigType)
    ar.validate("ar2", enum=KrigType)
    with pytest.raises(TypeError, match="enum must be a subclass of "):
        ar.validate("ar1", enum=[2])
    with pytest.raises(TypeError, match="enum must be a subclass of "):
        ar.validate("ar1", enum=int)
    with pytest.raises(TypeError, match="'ar4' values must be integer type"):
        ar.validate("ar4", enum=KrigType)
    with pytest.raises(ValueError, match="must be in enum KrigType 0 "):
        ar.validate("ar3", enum=KrigType)

    with pytest.raises(NotImplementedError, match="unhandled kwarg"):
        ar.validate("ar3", notimpl=True)
