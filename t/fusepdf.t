use warnings;
use strict;
use Test::More;
use File::Spec;
use FindBin qw($Bin);
use lib File::Spec->catdir($Bin, 'lib'), File::Spec->catdir($Bin, '..', 'inc');

use Test::Fuse::PDF;

Test::Class->runtests;

1;
