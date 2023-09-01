src_dir = '.'
lib_sources = files(
  'deftypes.f90',
  'dimvar.f90',
  'dimvar_c.f90',
  'funcproc1.f90',
  'funcproc2.f90',
  'function_interfaces.f90',
  'lapack1.F',
  'sgsim_code.f90',
  'utl.f90',
  'utl_high.f90'
)

lib = shared_library('pestutils', lib_sources,
  name_prefix : host_machine.system() == 'windows' ? '' : 'lib',
  install : true)

executable('driver1', ['driver1.f90', 'driversubs.f90'], link_with : [lib], install : true)
executable('driver2', ['driver2.f90', 'driversubs.f90'], link_with : [lib], install : true)
executable('driver3', ['driver3.f90', 'driversubs.f90'], link_with : [lib], install : true)
executable('driver4', ['driver4.f90', 'driversubs.f90'], link_with : [lib], install : true)
executable('driver5', ['driver5.f90', 'driversubs.f90'], link_with : [lib], install : true)
executable('driver6', ['driver6.f90', 'driversubs.f90'], link_with : [lib], install : true)
executable('driver7', ['driver7.f90', 'driversubs.f90'], link_with : [lib], install : true)
executable('driver8', ['driver8.f90', 'driversubs.f90'], link_with : [lib], install : true)
executable('driver9', ['driver9.f90', 'driversubs.f90'], link_with : [lib], install : true)
executable('driver10', ['driver10.f90', 'driversubs.f90'], link_with : [lib], install : true)
executable('driver11', ['driver11.f90', 'driversubs.f90'], link_with : [lib], install : true)
executable('driver12', ['driver12.f90', 'driversubs.f90'], link_with : [lib], install : true)
executable('driver13', ['driver13.f90', 'driversubs.f90'], link_with : [lib], install : true)
executable('driver14', ['driver14.f90', 'driversubs.f90'], link_with : [lib], install : true)
executable('driver15', ['driver15.f90', 'driversubs.f90'], link_with : [lib], install : true)
