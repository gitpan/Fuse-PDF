##no critic(Tidy)
#######################################################################
#      $URL: svn+ssh://equilibrious@equilibrious.net/home/equilibrious/svnrepos/chrisdolan/Fuse-PDF/lib/Fuse/PDF/FS.pm $
#     $Date: 2007-11-12 01:57:00 -0600 (Mon, 12 Nov 2007) $
#   $Author: equilibrious $
# $Revision: 694 $
########################################################################

package Fuse::PDF::FS;

use warnings;
use strict;

use Readonly;
use POSIX qw(:errno_h);
use Fcntl qw(:mode);
use English qw(-no_match_vars);
use CAM::PDF::Node;

our $VERSION = '0.01';

# integer, increases when we break file format backward compatibility
Readonly::Scalar my $COMPATIBILITY_VERSION => 1;

Readonly::Scalar my $PATHLEN => 255;
Readonly::Scalar my $BLOCKSIZE => 4096;
Readonly::Scalar my $ELOOP_LIMIT => 100;
Readonly::Scalar my $ROOT_FS_PERMS => oct 777;
Readonly::Scalar my $DEFAULT_SYMLINK_PERMS => oct 777;

Readonly::Scalar my $FREE_FILES => 1_000_000;
Readonly::Scalar my $MAX_BLOCKS => 1_000_000;
Readonly::Scalar my $FREE_BLOCKS => 500_000;

# --------------------------------------------------

sub new {
   my ($pkg, $options) = @_;
   return if ! $options;
   return if ! $options->{pdf};

   my $self = bless {
      # Order matters!
      compact => 1,
      autosave_filename => undef,
      fs_name => 'FusePDF_FS',
      %{$options},
      dirty => 0,
   }, $pkg;

   # lookup/create fs object in PDF
   my $root = $options->{pdf}->getRootDict();
   my ($o, $g) = ($root->{objnum}, $root->{gennum});

   if ($root->{$self->{fs_name}}) {
      $self->{fs} = $self->{pdf}->getValue($root->{$self->{fs_name}});
   } else {
      my $fs = CAM::PDF::Node->new('object', CAM::PDF::Node->new('dictionary', {
         nfiles => CAM::PDF::Node->new('number', 1),
         maxinode => CAM::PDF::Node->new('number', 0),
         root => $self->_newdir($root, $ROOT_FS_PERMS),
      }));
      # Don't bother marking the FS dirty unless we actually put something in it
      my $objnum = $self->{pdf}->appendObject(undef, $fs, 1);
      $root->{$self->{fs_name}} = CAM::PDF::Node->new('reference', $objnum);
      $self->{fs} = $fs->{value}->{value};
   }

   return $self;
}

sub DESTROY {
   my ($self) = @_;
   if (defined $self->{autosave_filename}) {
      $self->save($self->{autosave_filename});
   }
   return;
}

sub compact { ## no critic(ArgUnpacking)
   my ($self, $boolean) = @_;
   return $self->{compact} if @_ == 1;
   $self->{compact} = $boolean ? 1 : undef;
   return;
}

sub autosave_filename { ## no critic(ArgUnpacking)
   my ($self, $filename) = @_;
   return $self->{autosave_filename} if @_ == 1;
   $self->{autosave_filename} = $filename;
   return;
}

# subclasses may wish to override this
sub _software_name {
   my ($self) = @_;
   return ref $self;
}

sub save {
   my ($self, $filename) = @_;
   if ($self->{dirty}) {
      # TODO: atomically?
      $self->{fs}->{creator} = CAM::PDF::Node->new('string', $self->_software_name);
      $self->{fs}->{version} = CAM::PDF::Node->new('string', $self->VERSION);
      $self->{fs}->{compatibility} = CAM::PDF::Node->new('number', $COMPATIBILITY_VERSION);
      if ($self->{compact}) {
         $self->{pdf}->cleanse();
         $self->{pdf}->clean();
      }
      $self->{pdf}->output($filename);
      $self->{dirty} = 0;
   }
   return;
}

# --------------------------------------------------

sub fs_getattr {
   my ($self, $abspath) = @_;
   my $f = $self->_file($abspath);
   return -$f if !ref $f;
   my $type = $f->{type}->{value};
   my $size = 'd' eq $type ? 0 : length $f->{content}->{value};
   my $blocks = 0 == $size ? 0 : (($size - 1) % $BLOCKSIZE) + 1;  # round up
   return
       0, # dev
       $f->{inode}->{value},
       $f->{mode}->{value},
       $f->{nlink}->{value},
       $EFFECTIVE_USER_ID, # uid
       $EFFECTIVE_GROUP_ID, # gid
       0, #rdev
       $size,
       $f->{mtime}->{value}, # atime not preserved
       $f->{mtime}->{value},
       $f->{ctime}->{value},
       $BLOCKSIZE,
       $blocks;
}

sub fs_readlink {
   my ($self, $abspath) = @_;
   my $f = $self->_file($abspath);
   return -$f if !ref $f;
   my $type = $f->{type}->{value};
   return -EINVAL() if 'l' ne $type;
   return $f->{content}->{value};
}

sub fs_getdir {
   my ($self, $abspath) = @_;
   my $f = $self->_file($abspath);
   return -$f if !ref $f;
   return q{.}, q{..}, (keys %{$f->{content}->{value}}), 0;
}

sub fs_mknod {
   my ($self, $abspath, $perms, $dev) = @_;
   my ($p, $name) = $self->_parent($abspath);
   return -$p if !ref $p;
   return -EEXIST() if q{.}  eq $name;
   return -EEXIST() if q{..} eq $name;
   my $f = $p->{content}->{value}->{$name};
   return -EEXIST() if $f;

   # don't support special files
   my $is_special = !S_ISREG($perms) && !S_ISDIR($perms) && !S_ISLNK($perms);
   return -EIO() if $is_special;

   $p->{content}->{value}->{$name} = $self->_newfile($p, $perms);
   $p->{content}->{value}->{$name}->{value}->{inode}->{value} = ++$self->{fs}->{maxinode}->{value};
   #$p->{nlink}->{value}++;
   $p->{mtime}->{value} = time;
   $self->{fs}->{nfiles}->{value}++;
   $self->{dirty} = 1;
   return 0;
}

sub fs_mkdir {
   my ($self, $abspath, $perm) = @_;
   my ($p, $name) = $self->_parent($abspath);
   return -$p if !ref $p;
   return -EEXIST() if q{.}  eq $name;
   return -EEXIST() if q{..} eq $name;
   my $f = $p->{content}->{value}->{$name};
   return -EEXIST() if $f;
   $p->{content}->{value}->{$name} = $self->_newdir($p, $perm);
   $p->{content}->{value}->{$name}->{value}->{inode}->{value} = ++$self->{fs}->{maxinode}->{value};
   $p->{nlink}->{value}++;
   $p->{mtime}->{value} = time;
   $self->{fs}->{nfiles}->{value}++;
   $self->{dirty} = 1;
   return 0;
}

sub fs_unlink {
   my ($self, $abspath) = @_;
   my ($p, $name) = $self->_parent($abspath);
   return -$p if !ref $p;
   use Data::Dumper; print STDERR "$name vs. ".Dumper($p);
   my $f = $p->{content}->{value}->{$name};
   return -ENOENT() if !ref $f;
   $f = $f->{value};
   my $type = $f->{type}->{value};
   return -ENOENT() if 'd' eq $type;  # TODO: is this the right errno??

   # TODO: worry about open files?

   delete $p->{content}->{value}->{$name};
   #$p->{nlink}->{value}--;
   $p->{mtime}->{value} = time;
   $self->{dirty} = 1;
   return 0;
}

sub fs_rmdir {
   my ($self, $abspath) = @_;
   my ($p, $name) = $self->_parent($abspath);
   return -$p if !ref $p;
   my $f = $p->{content}->{value}->{$name};
   return -ENOENT() if !ref $f;
   $f = $f->{value};
   my $type = $f->{type}->{value};
   return -ENOTDIR() if 'd' ne $type;
   return -ENOTEMPTY() if 0 != scalar keys %{ $f->{content}->{value} };
   delete $p->{content}->{value}->{$name};
   $p->{nlink}->{value}--;
   $p->{mtime}->{value} = time;
   $self->{dirty} = 1;
   return 0;
}

sub fs_symlink {
   my ($self, $link, $abspath) = @_;
   my ($p, $name) = $self->_parent($abspath);
   return -$p if !ref $p;
   return -EEXIST() if q{.}  eq $name;
   return -EEXIST() if q{..} eq $name;
   my $f = $p->{content}->{value}->{$name};
   return -EEXIST() if $f;
   $p->{content}->{value}->{$name} = $self->_newsymlink($p, $link);
   #$p->{nlink}->{value}++;
   $p->{mtime}->{value} = time;
   $self->{fs}->{nfiles}->{value}++;
   $self->{dirty} = 1;
   return 0;
}

sub fs_rename {
   return -EIO();
}

sub fs_link {
   return -EIO();
}

sub fs_chmod {
   my ($self, $abspath, $perms) = @_;
   my $f = $self->_file($abspath);
   return -$f if !ref $f;
   $f->{mode}->{value} = $perms;
   $self->{dirty} = 1;
   return 0;
}

sub fs_chown {
   my ($self, $abspath, $uid, $gid) = @_;
   my $f = $self->_file($abspath);
   return -$f if !ref $f;
   #$f->{uid}->{value} = $uid;
   #$f->{gid}->{value} = $gid;
   #$self->{dirty} = 1;
   return 0;
}

sub fs_truncate {
   my ($self, $abspath, $length) = @_;
   my $f = $self->_file($abspath);
   return -$f if !ref $f;
   my $type = $f->{type}->{value};
   return -EISDIR() if 'd' eq $type;
   if ($length <= 0) {
      $f->{content}->{value} = q{};
   } else {
      my $l = length $f->{content}->{value};
      if ($length < $l) {
         $f->{content}->{value} = substr $f->{content}->{value}, 0, $length;
      } elsif ($length > $l) {
         $f->{content}->{value} .= "\0" x ($length - $l);
      }
   }
   $f->{mtime}->{value} = time;
   $self->{dirty} = 1;
   return 0;
}

sub fs_utime {
   my ($self, $abspath, $atime, $utime) = @_;
   my $f = $self->_file($abspath);
   return -$f if !ref $f;

   # Ignore atime

   # Set utime, if changed
   if ($f->{utime}->{value} != $utime) {
      $f->{utime}->{value} = $utime;
      $self->{dirty} = 1;
   }
   return 0;
}

sub fs_open {
   my ($self, $abspath, $flags) = @_;
   my $f = $self->_file($abspath);
   return -$f if !ref $f;
   # check flags?
   return 0;
}

sub fs_read {
   my ($self, $abspath, $size, $offset) = @_;
   my $f = $self->_file($abspath);
   return -$f if !ref $f;
   return substr $f->{content}->{value}, $offset, $size;
}

sub fs_write {
   my ($self, $abspath, $str, $offset) = @_;
   my $f = $self->_file($abspath);
   return -$f if !ref $f;
   my $size = length $str;
   substr($f->{content}->{value}, $offset, $size) = $str;    ##no critic(ProhibitLvalueSubstr)
   $f->{mtime}->{value} = time;
   $self->{dirty} = 1;
   return $size;
}

sub fs_statfs {
   my ($self) = @_;
   return $PATHLEN, $self->{fs}->{nfiles}->{value}, $FREE_FILES, $MAX_BLOCKS, $FREE_BLOCKS, $BLOCKSIZE;
}

sub fs_flush {
   my ($self, $file) = @_;
   # TODO
   return 0;
}

sub fs_release {
   my ($self, $file, $flags) = @_;
   # TODO
   return 0;
}

sub fs_fsync {
   my ($self, $file, $flags) = @_;
   # TODO
   return 0;
}

sub fs_setxattr {
   my ($self, $abspath, $key, $value, $flags) = @_;
   my $f = $self->_file($abspath);
   return -$f if !ref $f;
   my $xattr = $f->{xattr};
   my ($o, $g) = ($f->{type}->{objnum}, $f->{type}->{gennum});
   if (!$xattr) {
      $xattr = $f->{xattr} = CAM::PDF::Node->new('dictionary', {}, $o, $g);
   }
   if ($flags->{create}) {
      return -EEXIST() if exists $xattr->{value}->{$key};
   } elsif ($flags->{replace}) {
      return -ENOATTR() if !exists $xattr->{value}->{$key};
   }
   $xattr->{value}->{$key} = CAM::PDF::Node->new('string', $value, $o, $g);
   $self->{dirty} = 1;
   return 0;
}

sub fs_getxattr {
   my ($self, $abspath, $key) = @_;
   my $f = $self->_file($abspath);
   return -$f if !ref $f;
   my $xattr = $f->{xattr};
   return 0 if !$xattr;
   return 0 if !exists $xattr->{value}->{$key};
   return $xattr->{value}->{$key};
}

sub fs_listxattr {
   my ($self, $abspath, $key) = @_;
   my $f = $self->_file($abspath);
   return -$f if !ref $f;
   my $xattr = $f->{xattr};
   return $xattr ? keys %{ $xattr->{value} } : ();
}

sub fs_removexattr {
   my ($self, $abspath, $key) = @_;
   my $f = $self->_file($abspath);
   return -$f if !ref $f;
   my $xattr = $f->{xattr};
   return -ENOATTR() if !$xattr;
   return -ENOATTR() if !exists $xattr->{value}->{$key};
   delete $xattr->{value}->{$key};
   $self->{dirty} = 1;
   return 0;
}

# --------------------------------------------------

sub _parent {
   my ($self, $path) = @_;
   $path =~ s{/+\z}{}xms;
   return ENOTDIR() if q{} eq $path;
   if ($path =~ s{([^/]+)\z}{.}xms) {
      my $name = $1;
      return $self->_file($path, 1), $name;
   } else {
      return ENOTDIR();  # should never get here
   }
}

sub _file {
   my ($self, $path, $follow_top_symlink) = @_;

   my $nsymlinks = 0;

   my @dirs = ($self->{fs}->{root}->{value});
   my @path = split m{/}xms, $path;

   for (my $i = 0; $i < @path; ++$i) {    ##no critic(ProhibitCStyleForLoops)
      my $entry = $path[$i];
      next if q{} eq $entry;

      my $type = $dirs[-1]->{type}->{value};
      return ENOTDIR() if 'd' ne $type;
      next if q{.} eq $entry;
      if (q{..} eq $entry) {
         pop @dirs;
         return EACCESS() if !@dirs;      # tried to get parent of root
      }

      my $next = $dirs[-1]->{content}->{value}->{$entry};
      return ENOENT() if !$next;
      my $f = $next->{value};
      if ('l' eq $f->{type}->{value}) {
         if ($follow_top_symlink || $i != $#path) {
            return ELOOP() if ++$nsymlinks >= $ELOOP_LIMIT;
            my $linkpath = $f->{content}->{value};

            # cannot leave the filesystem
            return EACCESS() if $linkpath =~ m{\A /}xms;

            splice @path, $i + 1, 0, split m{/}xms, $linkpath;
         }
      }
      push @dirs, $f;
   }

   return $dirs[-1] || ENOENT();
}

sub _newfile {
   my ($self, $parent, $perm) = @_;
   my ($o, $g) = ($parent->{objnum}, $parent->{gennum});
   return $self->_newentry($o, $g, S_IFREG | $perm,
      'f', CAM::PDF::Node->new('string', q{}, $o, $g));
}

sub _newsymlink {
   my ($self, $parent, $src) = @_;
   my ($o, $g) = ($parent->{objnum}, $parent->{gennum});
   return $self->_newentry($o, $g, S_IFLNK | $DEFAULT_SYMLINK_PERMS,
      'l', CAM::PDF::Node->new('string', $src, $o, $g));
}

sub _newdir {
   my ($self, $parent, $perm) = @_;
   my ($o, $g) = ($parent->{objnum}, $parent->{gennum});
   my $dir = $self->_newentry($o, $g, S_IFDIR() | $perm,
      'd', CAM::PDF::Node->new('dictionary', {}, $o, $g));
   $dir->{value}->{nlink}->{value}++;
   return $dir;
}

sub _newentry {    ##no critic(ProhibitManyArgs)
   my ($self, $o, $g, $perm, $type, $content) = @_;
   my $now = time;
   return CAM::PDF::Node->new('dictionary', {
      content => $content,
      type    => CAM::PDF::Node->new('string', $type, $o, $g),
      inode   => CAM::PDF::Node->new('number', 0, $o, $g),
      mode    => CAM::PDF::Node->new('number', $perm, $o, $g),
      nlink   => CAM::PDF::Node->new('number', 1, $o, $g),
      mtime   => CAM::PDF::Node->new('number', $now, $o, $g),
      ctime   => CAM::PDF::Node->new('number', $now, $o, $g),
   }, $o, $g);
}

1;

__END__

=pod

=for stopwords pdf

=head1 NAME

Fuse::PDF::FS - In-PDF implementation of a filesystem.

=head1 SYNOPSIS

    use Fuse::PDF::FS;
    my $fs = Fuse::PDF::FS->new({pdf => CAM::PDF->new('my_doc.pdf')});
    $fs->fs_mkdir('/foo');
    $fs->fs_write('/foo/bar', 'Hello world!', 0);
    $fs->save();

=head1 LICENSE

Copyright 2007 Chris Dolan, I<cdolan@cpan.org>

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=head1 DESCRIPTION

This is an implementation of a filesystem inside of a PDF file.
Contrary to the package name, this module is actually independent of
FUSE, but is meant to map cleanly onto the FUSE API.  See L<Fuse::PDF>
and the F<mount_pdf> front-end.

=head1 METHODS

=over

=item $pkg->new($hash_of_options)

Create a new filesystem instance.  This method creates a new root
filesystem node in the PDF if one does not already exist.  The only
required option is the C<pdf> key, like so:

   my $fs = Fuse::PDF::FS->new({pdf => $pdf});

Supported options:

=over

=item pdf => $pdf

Specify a L<CAM::PDF> instance.  Fuse::PDF::FS is highly dependent on
the architecture of CAM::PDF, so swapping in another PDF
implementation is not likely to be feasible with substantial rewriting
or bridging.

=item compact => $boolean

Specifies whether the PDF should be compacted upon save.  Defaults to
true.  If this option is turned off, then previous revisions of the
filesystem can be retrieved via standard PDF revert tools, like
F<revertpdf.pl> from the L<CAM::PDF> distribution.  But that can lead
to rather large PDFs.

=item autosave_filename => undef | $filename

If this option is set to a filename, the PDF will be automatically
saved when this instance is garbage collected.  Otherwise, the client
must explicitly call C<save()>.  Defaults to C<undef>.

=item fs_name => $name

This specifies the key where the filesystem data is stored inside the
PDF data structure.  Defaults to 'FusePDF_FS', Note that it is
possible to have multiple independent filesystems embedded in the same
PDF at once by choosing another name.  However, mounting more than one
at a time will almost certainly cause data loss.

=back

=item $self->save($filename);

Explicitly trigger a save to the specified filename.  If
C<autosave_filename> is defined, then this method is called via
C<DESTROY()>.

=item $self->autosave_filename()

=item $self->autosave_filename($filename)

Accessor/mutator for the C<autosave_filename> property described above.

=item $self->compact()

=item $self->compact($boolean)

Accessor/mutator for the C<compact> property described above.

=back

=head1 FUSE-COMPATIBLE METHODS

The following methods are independent of L<Fuse>, but uses almost the
exact same API expected by that package (except for fs_setxattr), so
they can easily be converted to a FUSE implementation.

=over

=item $self->fs_getattr($file)

=item $self->fs_readlink($file)

=item $self->fs_getdir($file)

=item $self->fs_mknod($file, $modes, $dev)

=item $self->fs_mkdir($file, $perms)

=item $self->fs_unlink($file)

=item $self->fs_rmdir($file)

=item $self->fs_symlink($link, $file)

=item $self->fs_rename($oldfile, $file)

=item $self->fs_link($srcfile, $file)

=item $self->fs_chmod($file, $perms)

=item $self->fs_chown($file, $uid, $gid)

=item $self->fs_truncate($file, $length)

=item $self->fs_utime($file, $atime, $utime)

=item $self->fs_open($file, $mode)

=item $self->fs_read($file, $size, $offset)

=item $self->fs_write($file, $str, $offset)

=item $self->fs_statfs()

=item $self->fs_flush($file)

=item $self->fs_release($file, $mode)

=item $self->fs_fsync($file, $flags)

=item $self->fs_setxattr($file, $key, $value, \%flags)

=item $self->fs_getxattr($file, $key)

=item $self->fs_listxattr($file)

=item $self->fs_removexattr($file, $key)

=back

=head1 SEE ALSO

L<Fuse::PDF>

L<CAM::PDF>

=head1 AUTHOR

Chris Dolan, I<cdolan@cpan.org>

=cut

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 3
#   fill-column: 78
#   indent-tabs-mode: nil
#   c-indentation-style: bsd
# End:
# ex: set ts=8 sts=4 sw=4 tw=78 ft=perl expandtab :
