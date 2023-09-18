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
  name_prefix: host_machine.system() == 'windows' ? '': 'lib',
  vs_module_defs: 'pestutils.def',
  install: true)

# Set RUNPATH/RPATH for install_rpath for only Linux and macOS
libdir = get_option('libdir')
if target_machine.system() == 'linux'
  inst_rpath = '$ORIGIN/..' / libdir
elif target_machine.system() == 'darwin'
  inst_rpath = '@loader_path/..' / libdir
else
  inst_rpath = ''
endif

# By default, don't install test driver programs.
# To build them, use: meson setup/configure -Dinstall_drivers=true ...

inst_drvs = get_option('install_drivers')

executable('driver1', ['driver1.f90', 'driversubs.f90'],
  link_with: [lib], install_rpath: inst_rpath, install: inst_drvs)
executable('driver2', ['driver2.f90', 'driversubs.f90'],
  link_with: [lib], install_rpath: inst_rpath, install: inst_drvs)
executable('driver3', ['driver3.f90', 'driversubs.f90'],
  link_with: [lib], install_rpath: inst_rpath, install: inst_drvs)
executable('driver4', ['driver4.f90', 'driversubs.f90'],
  link_with: [lib], install_rpath: inst_rpath, install: inst_drvs)
executable('driver5', ['driver5.f90', 'driversubs.f90'],
  link_with: [lib], install_rpath: inst_rpath, install: inst_drvs)
executable('driver6', ['driver6.f90', 'driversubs.f90'],
  link_with: [lib], install_rpath: inst_rpath, install: inst_drvs)
executable('driver7', ['driver7.f90', 'driversubs.f90'],
  link_with: [lib], install_rpath: inst_rpath, install: inst_drvs)
executable('driver8', ['driver8.f90', 'driversubs.f90'],
  link_with: [lib], install_rpath: inst_rpath, install: inst_drvs)
executable('driver9', ['driver9.f90', 'driversubs.f90'],
  link_with: [lib], install_rpath: inst_rpath, install: inst_drvs)
executable('driver10', ['driver10.f90', 'driversubs.f90'],
  link_with: [lib], install_rpath: inst_rpath, install: inst_drvs)
executable('driver11', ['driver11.f90', 'driversubs.f90'],
  link_with: [lib], install_rpath: inst_rpath, install: inst_drvs)
executable('driver12', ['driver12.f90', 'driversubs.f90'],
  link_with: [lib], install_rpath: inst_rpath, install: inst_drvs)
executable('driver13', ['driver13.f90', 'driversubs.f90'],
  link_with: [lib], install_rpath: inst_rpath, install: inst_drvs)
executable('driver14', ['driver14.f90', 'driversubs.f90'],
  link_with: [lib], install_rpath: inst_rpath, install: inst_drvs)
executable('driver15', ['driver15.f90', 'driversubs.f90'],
  link_with: [lib], install_rpath: inst_rpath, install: inst_drvs)

