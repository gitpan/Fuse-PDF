use Module::Build;

Module::Build->new(
   module_name => 'Fuse::PDF',
   dist_author => 'Chris Dolan <cdolan@cpan.org>',
   license => 'perl',
   requires => {
      'perl' => '5.6.0',
      'Fuse' => '0.08',
      'CAM::PDF' => '1.00',
      'Readonly' => 0,
   },
   recommends => {
      # Needed for scripts
      'Getopt::Long' => '2.0',    
      'Pod::Usage' => '1.0',

      # Optional in tests, skipped if not present
      'Test::Pod' => 0,
      'Test::Pod::Coverage' => 0,
      'Test::Memory::Cycle' => '1.02',
   },
   build_requires => {
      'Test::More' => 0.62,  # needs to be v0.62 for Test::Memory::Cycle v1.02
      'Test::Class' => 0,
   },
   script_files => [qw(
      bin/mount_pdf
   )],
   create_readme => 1,
   create_makefile_pl => 'traditional',
)->create_build_script;
