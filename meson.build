project(
  'PYPESTUTILS',
  'fortran',
  version: '0.0.0',
  meson_version: '>= 0.59.0',
  default_options : [
    'b_vscrt=static_from_buildtype', # Link runtime libraries statically on Windows
    'optimization=2',
])

if get_option('optimization') == '3'
  error('Only optimization levels <= 2 are supported')
endif

if get_option('optimization') == '2'
  profile = 'release'
else
  profile = 'develop'
endif

message('The used profile is:', profile)

# Gather host-system options
system = build_machine.system()
fc = meson.get_compiler('fortran')
fc_id = fc.get_id()
compile_args = []
link_args = []

# Command line options for gfortran
if fc_id == 'gcc'
  # General options
  compile_args += [
                   '-cpp',
                   '-Wcharacter-truncation',
                   '-Wno-maybe-uninitialized',
                   '-Wno-unused-label',
                   '-Wno-unused-variable',
                   ]

  # Options specific to profile
  if profile == 'release'
    compile_args += ['-ffpe-summary=overflow', '-ffpe-trap=overflow,zero,invalid']
  elif profile == 'develop'
    compile_args += ['-fcheck=all', '-ffpe-trap=overflow,zero,invalid,denormal']
  endif

  # Define OS with gfortran for OS specific code
  # These are identical to pre-defined macros available with ifort
  if system == 'linux'
    compile_args += '-D__linux__'
  elif system == 'darwin'
    compile_args += '-D__APPLE__'
  elif system == 'windows'
    compile_args += '-D_WIN32'
    link_args += [
      '-static-libgcc',
      '-static-libgfortran',
    ]
  endif
endif

# Command line options for ifort
if fc_id == 'intel-cl'
  # windows
  compile_args += ['/fpe:0',              # Activate all floating point exceptions
                   '/heap-arrays:0',
                   '/traceback',
                   '/fpp',                # Activate preprocessing
                   '/Qdiag-disable:7416', # f2008 warning
                   '/Qdiag-disable:7025', # f2008 warning
                   '/Qdiag-disable:5268', # Line too long
                   '/c',
                   '/fpp'
                  ]
  link_args += ['/ignore:4217', # access through ddlimport might be inefficient
                '/ignore:4286'  # same as 4217, but more general
               ]
elif fc_id == 'intel'
  # linux and macOS
  compile_args += ['-fpe0',              # Activate all floating point exceptions
                   '-no-heap-arrays',
                   '-traceback',
                   '-diag-disable:7416',  # f2008 warning
                   '-diag-disable:7025',  # f2008 warning
                   '-diag-disable:5268',  # Line too long
                   '-c',
                   '-fpp'
                  ]
  link_args += '-static-intel'
endif

add_project_arguments(fc.get_supported_arguments(compile_args), language: 'fortran')
add_project_link_arguments(fc.get_supported_arguments(link_args), language: 'fortran')

subdir('src')

# meson tests to evaluate installation success
#test('Test installation version', driver1exe, args : ['-v',])
#test('Test installation compiler', driver1exe, args : ['-c',])
#test('Test installation help', driver1exe, args : ['-h',])
