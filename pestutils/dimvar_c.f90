module dimvar_c
  use iso_c_binding, only: c_int
  use dimvar, only: LENFILENAME, LENMESSAGE, LENGRIDNAME
  use deftypes, only: MAXLENCRS
  implicit none

  integer(c_int), bind(C, name="LENFILENAME") :: LENFILENAME_C = LENFILENAME

  integer(c_int), bind(C, name="LENMESSAGE") :: LENMESSAGE_C = LENMESSAGE

  integer(c_int), bind(C, name="LENGRIDNAME") :: LENGRIDNAME_C = LENGRIDNAME

  integer(c_int), bind(C, name="MAXLENCRS") :: MAXLENCRS_C = MAXLENCRS

end module dimvar_c
