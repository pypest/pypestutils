module dimvar_c
  use iso_c_binding, only: c_int
  use dimvar, only: LENFILENAME, LENMESSAGE, LENGRIDNAME
  implicit none

  integer(c_int), bind(C, name="LENFILENAME") :: LENFILENAME_C = LENFILENAME

  integer(c_int), bind(C, name="LENMESSAGE") :: LENMESSAGE_C = LENMESSAGE

  integer(c_int), bind(C, name="LENGRIDNAME") :: LENGRIDNAME_C = LENGRIDNAME

end module dimvar_c
