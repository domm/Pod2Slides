use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Pod2Slides',
    license             => '',
    dist_author         => 'Thomas Klausner <domm@cpan.org>',
    dist_version_from   => 'lib/Pod2Slides.pm',
    requires => {
        'Test::More' => 0,
        'Pod::Simple'=>3.00,
        'Getopt::Long'=>2.34,
        'Template'=>2.10,
        'Class::Accessor'=>0.22,
    },
    tt_files=>{
        'templates/boilerplate.tt'=>'tt/boilerplate.tt',
        'templates/default.tt'=>'tt/default.tt',
        'templates/all_in_one.tt'=>'tt/all_in_one.tt',
        'templates/style.css'=>'tt/style.css',
    },
    script_files=>[qw(bin/pod2slides.pl)],
    install_path=>{tt=>$ENV{HOME}.'/.pod2slides'},
    add_to_cleanup      => [ 'Pod2Slides-*' ],
    fix_shebang_line=>['bin/pod2slides.pl'],
);

$builder->add_build_element('tt');

$builder->create_build_script();
