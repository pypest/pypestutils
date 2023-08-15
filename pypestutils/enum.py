"""Enumeration module."""
from enum import IntEnum


class ParamEnum(IntEnum):
    """Wraps IntEnum to provide validation of a requested item.

    Intended for enums used for function parameters.

    Use enum.get_value(item) for this behavior instead of builtin enum[item].
    """

    @classmethod
    def get_value(cls, item: str):
        """Validate item and raise a ValueError with valid options."""
        try:
            return cls[item].value
        except KeyError:
            valid_options = dict((e.value, e.name) for e in cls)
            valid_options_list = [
                f"'{n}' ({v})"
                for (v, n) in dict(
                    sorted(valid_options.items(), key=lambda item: item[0])
                ).items()
            ]
            raise ValueError(
                "{}: '{}' is not a valid option, must be one of {}".format(
                    cls.__name__, item, ", ".join(valid_options_list)
                )
            )


class Prec(ParamEnum):
    """Floating precision type, where 1 is single and 2 is double precision."""

    single = 1
    double = 2


class KrigType(ParamEnum):
    """Kriging type, where 0 is simple and 1 is ordinary."""

    simple = 0
    ordinary = 1


class VarioType(ParamEnum):
    """Variogram type, where 1:spher, 2:exp, 3:gauss and 4:pow."""

    spher = 1
    exp = 2
    gauss = 3
    pow = 4


class FactorFileType(ParamEnum):
    """Factor file type, where 0 is binary and 1 is text."""

    binary = 0
    text = 1


class StrucType(ParamEnum):
    """Structure type, where 0 is for polylinear and 1 for polygonal."""

    polylinear = 0
    polygonal = 1


class TransType(ParamEnum):
    """Enable log-transformation of values."""

    none = 0
    log = 1
