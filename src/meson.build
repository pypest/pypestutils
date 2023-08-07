src_dir = '.'
lib_sources = files(
  'deftypes.f90',
  'dimvar.f90',
  'dimvar_c.f90',
  'funcproc1.f90',
  'function_interfaces.f90',
  'utl.f90',
  'utl_high.f90'
)

shared_library('pestutils', lib_sources,
  name_prefix : host_machine.system() == 'windows' ? '' : 'lib',
  install : true)

pulib = static_library('pestutils_core', lib_sources)

executable('driver1', ['driver1.f90', 'driversubs.f90'], link_with : [pulib], install : true)
executable('driver2', ['driver2.f90', 'driversubs.f90'], link_with : [pulib], install : true)
executable('driver3', ['driver3.f90', 'driversubs.f90'], link_with : [pulib], install : true)
executable('driver4', ['driver4.f90', 'driversubs.f90'], link_with : [pulib], install : true)
executable('driver5', ['driver5.f90', 'driversubs.f90'], link_with : [pulib], install : true)
