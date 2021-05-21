#!/usr/bin/env perl

=encoding UTF-8

=head1 NAME

pandoc-self-links.pl - generate self links in Pandoc headings

=head1 VERSION

0.001

=head1 SYNOPSIS

    pandoc -F pandoc-self-links.pl [OPTIONS] FILE ...

=cut

# More documentation after the code!

use utf8;
use autodie 2.26;
use 5.010001;
use strict;
use warnings;
use warnings  qw(FATAL utf8);

use Carp qw[ carp croak ];

use Pandoc::Elements 0.25;
use Pandoc::Walker 0.25 qw[ action transform ];

my $format = shift @ARGV;
my $json = <>;
my $doc = pandoc_json($json);

# If we don't want to do anything with this doc
unless ( 1 ) {
    print $json;
    exit 0;
}

my %actions = (
    'Header' => sub {
        state $attr = attributes {};
        my($elem, $action) = @_;
        my $id = $elem->id // return;
        my $link = Link $attr, $elem->content, [ "#$id", "" ];
        $elem->content([$link]);
        return;
    },
);

my $action = action \%actions;

# Allow applying the action recursively
$doc->transform($action, $action);

print $doc->to_json;

__END__


# # DOCUMENTATION # #

=head1 DESCRIPTION

This L<< pandoc|http://pandoc.org >> filter generates self links
in headings which have an C<id> attribute (which in recent
versions of pandoc all headings get automatically if you don't
specify one yourself), that is

    ## Foo

becomes

    ## [Foo](#foo) {#foo}

when generating Markdown, and

    <h2 id="foo"><a href="#foo">Foo</a></h2>

when generating HTML.

=head1 WARNING

You should not use this when generating LaTeX/PDF.
It will break the LaTeX C<hyperref> package.

=head1 PREREQUISITES

=over

=item *

pandoc 1.16 or higher

and the Perl modules

=item *

perl 5.10.1 or higher

=over

=item *

Pandoc::Elements

=item *

Mojo::DOM

=back

=back

=head2 New to Perl?

This program requires L<< perl|https://www.perl.org/about.html >>
(minimum version as given above) and the Perl modules listed above to
function. If you haven't used Perl before information on how to
getE<0x2f>install perl andE<0x2f>or Perl modules can be found at the
URLS below, which lead to the official information on these topics.

Don't worry! If your operating system is Linux or Mac you probably
already have a new enough version of perl installed. If you don't or if
your operating system is Windows it is easy to install a recent version,
and once you have perl installed installing modules is very easy. Just
follow the instructions linked to below.

=over

=item Getting perl

L<< https:E<0x2f>E<0x2f>www.perl.orgE<0x2f>get.html|https://www.perl.org/get.html >>

(For Windows I recommend Strawberry Perl as module installation is
easier there.)

=item Installing Perl modules

L<< http:E<0x2f>E<0x2f>www.cpan.orgE<0x2f>modulesE<0x2f>INSTALL.html|http://www.cpan.org/modules/INSTALL.html >>

=back



=head1 AUTHOR

Benct Philip Jonsson (bpjonsson@gmail.com,
L<< https:E<0x2f>E<0x2f>github.comE<0x2f>bpj|https://github.com/bpj >>)

=head1 COPYRIGHT

Copyright 2016- Benct Philip Jonsson

=head1 LICENSE

This is free software; you can redistribute it andE<0x2f>or modify it
under the same terms as the Perl 5 programming language system itself.
See
L<< http:E<0x2f>E<0x2f>dev.perl.orgE<0x2f>licensesE<0x2f>|http://dev.perl.org/licenses/ >>.

=cut

