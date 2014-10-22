#######################################################################
#      $URL: svn+ssh://equilibrious@equilibrious.net/home/equilibrious/svnrepos/chrisdolan/Fuse-PDF/lib/Fuse/PDF.pm $
#     $Date: 2007-11-15 00:43:02 -0600 (Thu, 15 Nov 2007) $
#   $Author: equilibrious $
# $Revision: 702 $
########################################################################

package Fuse::PDF;

use warnings;
use strict;
use English qw(-no_match_vars);
use Carp qw(croak);
use Fuse qw(:xattr);
use CAM::PDF;
use Fuse::PDF::FS;

our $VERSION = '0.02';

sub new {
   my ($pkg, $filename, $extra) = @_;
   croak 'No PDF filename specified' if !defined $filename;
   croak 'No PDF file found'         if !-e $filename;
   $extra ||= {};
   my $pdf = CAM::PDF->new($filename, @{ $extra->{pdf_constructor} || [] });
   return if !$pdf;
   my $self = bless {
      save_filename => $filename,
      compact       => 0,
      fs_name       => undef,
      %{$extra},
      pdf => $pdf,
   }, $pkg;
   return $self;
}

sub fs {
   my ($self) = @_;

   my $fs = Fuse::PDF::FS->new({
      pdf => $self->{pdf},
      fs_name => $self->{fs_name},
   }) or croak 'Failed to open the PDF or create a filesystem inside';

   if ($self->{revision}) {
      my @revisions = $fs->all_revisions;
      if ($self->{revision} =~ m/\D/xms && $self->{revision} <= @revisions) {
         $fs = $revisions[@revisions - $self->{revision}];
      } else {
         croak 'Invalid filesystem revision: ' . $self->{revision};
      }
   }

   return $fs;
}

sub mount {
   my ($self, $mountdir, $options) = @_;
   $options ||= {};

   my $fs = $self->fs;
   $fs->autosave_filename($self->{save_filename});
   $fs->compact($self->{compact});
   $fs->backup($self->{backup});

   # perform mount
   if (!-d $mountdir) {
      mkdir $mountdir;
      if (!-d $mountdir) {
         croak 'Failed to create mount directory: ' . $OS_ERROR;
      }
   }

   Fuse::main(
      mountpoint => $mountdir,

      getattr => sub {
         my ($file) = @_;
         return $fs->fs_getattr($file);
      },
      readlink => sub {
         my ($file) = @_;
         return $fs->fs_readlink($file);
      },
      getdir => sub {
         my ($file) = @_;
         return $fs->fs_getdir($file);
      },
      mknod => sub {
         my ($file, $modes, $dev) = @_;
         return $fs->fs_mknod($file, $modes, $dev);
      },
      mkdir => sub {
         my ($file, $perms) = @_;
         return $fs->fs_mkdir($file, $perms);
      },
      unlink => sub {
         my ($file) = @_;
         return $fs->fs_unlink($file);
      },
      rmdir => sub {
         my ($file) = @_;
         return $fs->fs_rmdir($file);
      },
      symlink => sub {
         my ($link, $file) = @_;
         return $fs->fs_symlink($link, $file);
      },
      rename => sub {
         my ($oldfile, $file) = @_;
         return $fs->fs_rename($oldfile, $file);
      },
      link => sub {
         my ($srcfile, $file) = @_;
         return $fs->fs_link($srcfile, $file);
      },
      chmod => sub {
         my ($file, $perms) = @_;
         return $fs->fs_chmod($file, $perms);
      },
      chown => sub {
         my ($file, $uid, $gid) = @_;
         return $fs->fs_chown($file, $uid, $gid);
      },
      truncate => sub {
         my ($file, $length) = @_;
         return $fs->fs_truncate($file, $length);
      },
      utime => sub {
         my ($file, $atime, $utime) = @_;
         return $fs->fs_utime($file, $atime, $utime);
      },
      open => sub {
         my ($file, $mode) = @_;
         return $fs->fs_open($file, $mode);
      },
      read => sub {
         my ($file, $size, $offset) = @_;
         return $fs->fs_read($file, $size, $offset);
      },
      write => sub {
         my ($file, $str, $offset) = @_;
         return $fs->fs_write($file, $str, $offset);
      },
      statfs => sub {
         return $fs->fs_statfs();
      },
      flush => sub {
         my ($file) = @_;
         return $fs->fs_flush($file);
      },
      release => sub {
         my ($file, $mode) = @_;
         return $fs->fs_release($file, $mode);
      },
      fsync => sub {
         my ($file, $flags) = @_;
         return $fs->fs_fsync($file, $flags);
      },
      setxattr => sub {
         my ($file, $key, $value, $flags) = @_;
         my %flags = (
            create  => $flags & XATTR_CREATE,
            replace => $flags & XATTR_REPLACE,
         );
         return $fs->fs_setxattr($file, $key, $value, \%flags);
      },
      getxattr => sub {
         my ($file, $key) = @_;
         return $fs->fs_getxattr($file, $key);
      },
      listxattr => sub {
         my ($file, $key) = @_;
         return $fs->fs_listxattr($file);
      },
      removexattr => sub {
         my ($file, $key) = @_;
         return $fs->fs_removexattr($file, $key);
      },
      threaded => 0,
      %{$options},
   );
   return;
}

1;

__END__

=pod

=head1 NAME

Fuse::PDF - Filesystem embedded in a PDF document

=head1 SYNOPSIS

    use Fuse::PDF;
    my $fs = Fuse::PDF->new('my_doc.pdf');
    $fs->mount('/mnt/pdf');
    # blocks until the filesystem is unmounted

See also the F<mount_pdf> front-end.

=head1 LICENSE

Copyright 2007 Chris Dolan, I<cdolan@cpan.org>

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=head1 DESCRIPTION

The Adobe Portable Document Format is an arbitrary collection of nodes
which support a tree structure.  Most of that data is oriented toward
document rendering, but it's legal to add arbitrarily complex data
virtually anywhere in the document structure.  Adobe Illustrator does
this to embed lots of metadata in it's "PDF-compatible" Illustrator
document format.

By deciding on a convention for representing a filesystem data and
leveraging the FUSE (Filesystem in Userspace) library, we map
filesystem calls to PDF edits.

=head1 BUGS AND CAVEATS

B<Saving:> B<No data is saved until you unmount the filesystem!>
Hopefully I can fix this in future releases.  The saving is not yet
atomic.  That is, if you have a failure, the old PDF may be
deleted before the new one is saved.

B<Resources:> The B<entire> PDF is loaded into RAM in C<new()>.  If
your filesystem grows too large, this will lead to obvious problems!

B<Tests:> There are no automated tests yet.  That's next, I promise!

B<Hangs:> While FUSE is quite mature, I found it to be fairly easy to hang the
filesystem while creating this package.  I only needed to actually
reboot once, but if that causes you concern you may wish to avoid FUSE
in general.

B<Operating systems:> I've only tested this software with the Google build of MacFUSE 1.1.0
(PowerPC, 10.4, L<http://code.google.com/p/macfuse/>).  Notably, I
have not tried the Linux implementation of FUSE.  If you have other
experiences to add, email me or (even better) post comments to
L<http://annocpan.org/>.

B<Fuse.pm:> As of this writing, the L<Fuse> module (v0.09_01) fails all tests on Mac.  The
module actually I<works> great, but the F<Makefile.PL> and the tests are very
Linux-centric.  Hopefully that will improve as MacFUSE matures.

B<PDF versions:> This package relies on L<CAM::PDF> to read and write PDFs.  While that
module supports all of the core PDF syntax, it's stricter than many other
PDF implementations and may fail to open PDFs that, say, Acrobat or
Preview.app can open.  In particular, "Print to PDF" on Mac OS X 10.4
often generates bad PDFs.

B<Threading:> I've explicitly set the FUSE default to single-threaded
mode, so performance may be terrible in some scenarios.  I hope to add
support for threaded Perl in a future release.  Patches welcome
(remove the C<threaded =E<gt> 0> line from this file and add locking
to L<Fuse::PDF::FS>).

B<Unsupported:> special files (named pipes, etc.), following symlinks
out of the filesystem, permission enforcement, C<chown>, C<flush>,
reading from unlinked filehandles.

B<Performance:> Don't expect this to be fast!

B<find:> I think I have the C<nlinks> count wrong.  According to the FUSE FAQ,
this can confuse the 'find' command:
L<http://fuse.sourceforge.net/wiki/index.php/FAQ#Why_doesnx27.t_find_work_on_my_filesystemx3f.>

B<Hard links:> I have not yet implemented hard links.  I'll consider
implementing compressed streams at the same time.

=head1 METHODS

=over

=item $pkg->new($pdf_filename)

=item $pkg->new($pdf_filename, $hash_of_options)

Create a new filesystem instance.  This method opens and parses the
PDF document.  If there is an error opening or parsing the PDF
document, this will return C<undef>.

The options hash supports the following extra arguments:

=over

=item pdf_constructor

An arrayref of extra arguments to pass to the L<CAM::PDF> constructor.
In particular, the first arguments are the owner and user password
which can be used to open encrypted PDFs.

=item save_filename

The string representing the path where filesystem changes should be
saved.  By default this is the C<$pdf_filename> passed to C<new()>.

=item compact

A boolean indicating whether to discard old filesystem data saved via
version infrastructure described in the PDF specification.
Defaults to false.  If left false, then the PDF will grow with every
mount, but only by as much as you changed it.  See C<rewritepdf.pl
--cleanse> from the L<CAM::PDF> distribution to perform the compaction
manually.  See also C<revertpdf.pl> to roll back to those older
versions.

=item fs_name

Fuse::PDF can embed multiple filesystems in a single PDF distinguished
by name.  This string specifies which filesystem to use.  It uses the
Fuse::PDF::FS default if a name is not explicitly provided.

=item revision

A version number indicating which filesystem version to roll back to
before mounting.  Use C<fs()> and the L<Fuse::PDF::FS> API to learn
what revisions are available in a PDF filesystem.

=back

=item $self->mount($mount_path)

=item $self->mount($mount_path, $hash_of_fuse_options)

Calls into Fuse to mount the filesystem to the specified mount point.
On unmount, a new PDF will be saved with any filesystem changes.

If the mount point does not exist, this package will try to create it
as a directory via a simple C<mkdir()>.  If the C<mkdir()> fails, or
if the mount point exists but is not a directory, C<mount()> will
C<croak()>.

If the PDF has an existing filesystem which is incompatible with
this version of the software, C<mount()> will C<croak()>.

If the mount is successful, we establish callbacks and hand control to
the FUSE library.  FUSE blocks until the filesystem is unmounted.  If
this blocking is a problem for you, consider daemonizing the process
like so:

    use Fuse::PDF;
    use Net::Server::Daemonize qw();
    my ($pdffilename, $mountdir) = @ARGV;
    my $fs = Fuse::PDF->new($pdffilename);
    if (Net::Server::Daemonize::safe_fork()) {
        exit 0; # parent process or failure
    }
    Net::Server::Daemonize::daemonize('www', 'www');
    $fs->mount($mountdir);

The C<mount()> method cleans up after itself sufficiently that you may
call it again immediately after unmounting.

The options hash is passed directly to C<Fuse::main()>.  See the
L<Fuse> documentation for the allowed keys.  A simple example is:

    $fs->mount($mountdir, {debug => 1});

=item $self->fs()

Return a fresh copy of the L<Fuse::PDF::FS> data structure representing this
PDF.  You should not try to manipulate this object while the filesystem is
mounted.  This module is not yet thread-safe!

=back

=head1 SEE ALSO

L<CAM::PDF>

L<Fuse>

F<mount_pdf>

=head1 AUTHOR

Chris Dolan, I<cdolan@cpan.org>

=head1 CREDITS

Thanks to the Madison Perl Mongers for thinking the idea was stupid
enough that I was inspired to implement it!

=cut

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 3
#   fill-column: 78
#   indent-tabs-mode: nil
#   c-indentation-style: bsd
# End:
# ex: set ts=8 sts=4 sw=4 tw=78 ft=perl expandtab :
