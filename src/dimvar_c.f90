module dimvar_c
  use iso_c_binding, only: c_int
  use dimvar, only: LENFILENAME, LENMESSAGE, LENGRIDNAME
  implicit none

  integer(c_int), bind(C, name="LENFILENAME") :: LENFILENAME_C = LENFILENAME
  !DIR$ ATTRIBUTES DLLEXPORT :: LENFILENAME

  integer(c_int), bind(C, name="LENMESSAGE") :: LENMESSAGE_C = LENMESSAGE
  !DIR$ ATTRIBUTES DLLEXPORT :: LENMESSAGE

  integer(c_int), bind(C, name="LENGRIDNAME") :: LENGRIDNAME_C = LENGRIDNAME
  !DIR$ ATTRIBUTES DLLEXPORT :: LENGRIDNAME

end module dimvar_c
