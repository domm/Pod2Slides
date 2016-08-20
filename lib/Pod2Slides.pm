package Pod2Slides;
use warnings;
use strict;

use Pod::Simple::SimpleTree;
use Template;
use Data::Dumper;
use Getopt::Long;
use base qw(Class::Accessor);
use Carp;
use File::Spec::Functions qw(rel2abs);
use Text::Autoformat;
use Path::Class;
use utf8;

__PACKAGE__->mk_accessors(qw(file podfile poddir cnt node opts thistext oldtext  tt root meta title head_level current_heading boilerplate has_locallib));

=head1 NAME

Pod2Slides - Make presentation slides out of POD

=head1 VERSION

Version 0.01

=cut

our $VERSION = '1.01';

=head1 SYNOPSIS

=head1 METHODS

=head3 new

=cut

sub new {
    my $class=shift;

    my $me=bless {
        cnt=>0,
    },$class;

    # parse options
    my $opts={template=>'default.tt'};
    GetOptions($opts,
        qw(quiet irc bremse),
        qw(template=s include_path=s preview=s),
    );
    $me->opts($opts);
    
    # get podfile
    my $file=rel2abs($ARGV[0]);
    croak("cannot read podfile $file") unless -e $file;
    $me->file($file);
    $me->podfile($file);
    my $dir=$file;
    $dir=~s|/[^/]+$||;
    $me->poddir($dir);
    
    # set up template
    $me->tt(Template->new({
                TRIM=>1,PRE_CHOMP=>1,
                            ENCODING           => 'utf-8',
                INCLUDE_PATH=>[
                    $ENV{HOME}.'/.pod2slides/',
                #'/home/domm/perl/Pod2Slides/templates',
                    $me->opts->{'include_path'},    
                ],
            }));

    if (-d 'locallib') {
        $me->has_locallib(1);
    }

    return $me;
}

=head3 generate

Generates the slides. Iterates over the POD structure and writes slides to
filesystem.

=cut

sub generate {
    my $me=shift;

    my $parser=Pod::Simple::SimpleTree->new;
    $parser->accept_targets('*');
    my $root=$parser->parse_file($me->podfile)->root;
    $me->root($root);

    # WHY?
    # remove first two items
    shift(@$root);shift(@$root);

    while (@$root) {
        $me->process(shift(@$root));
    }
    $me->write_lastpage;

    $me->make_tarball;
}

sub process {
    my $me=shift;
    my $node=shift;

    $me->node($node);

    $me->inc_cnt;
    my $text=$me->stringify_node;

    my $type='handle_'.lc($node->[0]);
    $type=$type."_".$node->[1]->{'target'} if $type eq 'handle_for'; 
    $type=~s/-.*//;
    if ($type=~/head([2345])/) {
        $type='handle_head';
        $me->head_level($1);
    }

    if ($me->can($type)) {
        $me->rawtext($text);
        $me->$type($text);
    } else {
        print "Cannot handle $type\n";
    }
}

=head3 stringify_node

Turns a POD-formatted string into HTML

=cut
sub stringify_node {
    my $me=shift;
    my $node=$me->node;
    my $type=$node->[0];
    
    return if $type eq 'Verbatim';
    return "cannot stringify node type $type" unless $type=~/head/ || $type eq 'Para' || $type =~ /^item/;
    
    my $string;
    for(my$i=2;$i<@$node;$i++) {
        my $part=$node->[$i];
        if (ref($part)) {
            my $subtype=$part->[0];
            if ($subtype eq 'I' || $subtype eq 'B') {
                my $tag=lc($subtype);
                $string.="<$tag>".$part->[2]."</$tag>";
            } elsif ($subtype eq 'F' || $subtype eq 'C') {
                my $content = join('',@$part[2 .. scalar @$part - 1]);
                $string.="<code>".$content."</code>";
            } elsif ($subtype eq 'E') {
                $string.="&".$part->[2].";";
            } else {
                die "unsupported formatting code: $subtype - ".Dumper($part);
            }
        } else {
            $string.=$part;
        }
    }
    return $string;
}

=head3 write_slide

Writes a single slide

=cut
sub write_slide {
    my $me=shift;
    my $tmpl=shift || $me->opts->{'template'};
    
    my $done;
    $me->tt->process(
		 $tmpl,
            { 
                me=>$me,
            },
                \$done)
          ||  return die "Template Error: ".$me->tt->error."\n";

    open(OUT,">:encoding(UTF-8)",$me->this_slide);
    print OUT $done;
    close OUT;
    print "slide ".$me->this_slide."\n" unless $me->opts->{quiet};

    $me->addtext($me->thistext);

}

sub inc_cnt {
    my $me=shift;
    $me->cnt($me->cnt +1);
    return $me->cnt;
}

sub dec_cnt {
    my $me=shift;
    $me->cnt($me->cnt -1);
    return $me->cnt;
}
=head2 Convertors

=cut

sub handle_head1 {
    my ($me,$text)=@_;
    my $n=$me->node;
    my $root=$me->root;
    my %meta;
    
    $me->title($me->stringify_node);
        
    my $meta=shift(@$root);
    if ($meta->[0] eq 'head2' && $meta->[2] eq 'META') {
        while (1) {
            my $key=shift(@$root);
            unless ($key->[0] eq 'head3' || $key->[0] eq 'Para') {
                unshift(@$root,$key);
                last;
            }
            my $value=shift(@$root);
            $meta{$key->[2]}=$value->[2];
        }
        $me->meta(\%meta);
        $meta{URL_slides}=~m|/([^/]*)/?$|;
        $meta{archive_name}=$1;

        $me->podfile=~m|/([\w]+\.pod)$|;
        $meta{source_pod}=$1;
        
        my $boilerplate;
        $me->tt->process(
		 'boilerplate.tt',
            { 
                title=>$me->title,
                me=>$me,
            },
                \$boilerplate)
          ||  return die "Template Error: ".$me->tt->error."\n";
        $me->thistext($boilerplate);
        $me->boilerplate($boilerplate);
        $me->rawtext($me->title);
        $me->write_slide(); 
    } else {
        $me->oldtext('');
        $me->thistext("<h1>".$n->[2]."</h1>");
        $me->write_slide;
        unshift(@$root,$meta);
    }
}

sub handle_head {
    my ($me,$text)=@_;
    $me->oldtext('');
    my $l=$me->head_level;
    my $h="<h$l>$text</h$l>";
    $me->rawtext("*$text*");
    $me->current_heading($h);
    $me->oldtext('');$me->thistext('');
    $me->write_slide;
}

sub handle_para {
    my ($me,$text)=@_;
    $text=~s|^(https?://.*)|<a href="$1">$1</a>|;
    $me->thistext("<li>$text</li>\n");
    $me->write_slide;
}
sub handle_item { shift->handle_para(@_) }

sub handle_verbatim {
    my $me=shift;
    my $text=$me->node->[2];
    $me->rawtext($text);
    $me->thistext("<pre>".$me->highlight($text)."</pre>");
    $me->write_slide;
}

sub handle_over {
    my ($me,$text)=@_;
    $me->addtext("<ul>");
    $me->dec_cnt;
    my $n=$me->node;
    for (my $i=2;$i<@$n;$i++) {
        $me->process($n->[$i]);
    }
    $me->addtext("</ul>");
}

sub handle_for_newslide {
    my $me=shift;
    $me->dec_cnt;
    $me->oldtext('');
}

sub handle_for_html {
    my $me=shift;
    my $text=$me->node->[2][2];
    $me->rawtext($text);
    $me->thistext($text);
    $me->write_slide;
}
sub handle_for_include_html {
    my $me=shift;
    my @text=$me->include_file($me->node->[2][2]);
    my $text=join('',@text);
    $me->rawtext($text);
    $me->thistext($text);
    $me->write_slide;
}

sub handle_for_img {
    my $me=shift;
    my $img=$me->node->[2][2];
    $me->rawtext($me->meta->{URL_slides}.'/'.$img);
    $me->thistext("<img src='$img'><br><br>");
    $me->write_slide;
}

sub handle_for_include_code {
    my $me=shift;
    my $inced=$me->include_file($me->node->[2][2]);
    $me->rawtext($me->node->[2][2].":\n".$inced);
    my $fontsize='100';
    my @lines = split(/\n/,$inced);
    if (@lines > 24) {
        $fontsize = int(100 * 24 / @lines);
    }
    $inced=~s/</&lt;/g;
    $inced=~s/>/&gt;/g;
    $inced=~s{^(\s?\d+:) }{<font color='999999'>$1</font> }gm;
    $me->thistext("<pre><font color='#aa0000'>file: ".$me->node->[2][2]."</font>\n<div  style='font-size:$fontsize".q{%'>}."$inced</div></pre>");
    $me->write_slide;
}

sub handle_for_include_fragment {
    my $me=shift;
    my @opts=split(/\n/,$me->node->[2][2]);
    my $file=shift(@opts);
    my $get_lines=shift(@opts);
    my $comment;
    if (@opts && $opts[$#opts] =~ /^#/) {
        $comment=pop(@opts);
        $comment=~s/^#\s+//;
    }
    my @lines=$me->include_file($file);
    
    my $size=@lines;
    my $stellen=length($size);
    my $format="%".$stellen."i: ";
    
    my (@selected,$from,$to);
    if ($get_lines eq 'all') {
        @selected=@lines;
        $from=1;
    }
    else {
        ($from,$to)=split(/\-/,$get_lines);
        @selected=splice(@lines,$from-1,$to-$from+1);
    }
    my %hl;
    foreach(@opts) {
        if ($_ =~ /^hl/) {
            my ($hl,@lines)=split(/[:,]/,$_);
            my %highlight = map { $_=>1} @lines;

            for my $linenum ($from .. $to) {
                $hl{$linenum } = ["1-100:".($highlight{$linenum} ? "red" : "grey")];
            }
        }
        else {
            my ($ln,$def)=$_=~/^([\d\.]+):(.*)/;
            if ($ln =~ /\.\./) {
                my ($ln_f,$ln_t)=$ln=~/(\d+)\.\.(\d+)/;
                foreach my $tln ($ln_f .. $ln_t) {
                    $hl{$tln}=[reverse split(/;/,$def)];
                }
            } else {
                $hl{$ln}=[reverse split(/;/,$def)];
            }
        }
    }

    # add line numbers and coloring
    my $linenum=$from;

    foreach my $line (@selected) {
        if (my $colored=$hl{$linenum}) {
            foreach my $col (@$colored) {
                my ($start,$end,$color)=$col=~/(\d+)-(\d+):(\w+)/;
                $start--;
                $end=length($line)-1 if $end>length($line);
                my $bold = $color eq 'grey' ? '' : 'BOLD';
                substr($line,$end,0,'////END'.$bold);
                substr($line,$start,0,"////$color/$bold/");
                warn $line;
            }
        }
        $line=sprintf($format,$linenum).$line;

        $linenum++;
    }

    my $code=join('',@selected);

    $me->rawtext($file.":\n".$code.($comment?"\n$comment":''));
    $code=~s/</&lt;/g;
    $code=~s/>/&gt;/g;
    $code=~s{////ENDBOLD}{</b></font>}g;
    $code=~s{////END}{</font>}g;
    $code=~s{////(\w+)/BOLD/}{<font color="$1"><b>}g;
    $code=~s{////(\w+)//}{<font color="$1">}g;
    $code=~s{^(\s?\d+:) }{<font color='999999'>$1</font> }gm;
    
    $me->thistext("<pre><font color='#aa0000'>file: ".$file."</font>\n$code</pre>".($comment?"<p>$comment</p>":''));
    $me->write_slide;

}

sub handle_for_run_code {
    my $me=shift;

    my ($codefile,@args)=split(' ',$me->node->[2][2]);
    my $args=join(' ' ,@args);
    my ($dir,$file,$run,$noperl)=$me->run_file($codefile,$args);

    my $perl = $noperl ? '' : 'perl ';
    $me->rawtext($dir."/".$file.":\n".$run);
    $me->thistext("<pre><font color='#00cc00'>~/$dir\$</font> ".$perl."$file $args\n$run</pre>");
    $me->write_slide;
}

sub handle_for_include_and_run {
    my $me=shift;
    my ($codefile,@args)=split(' ',$me->node->[2][2]);
    my $args=join(' ',@args);
    my $inced=$me->include_file($codefile);
    $inced=~s/</&lt;/g;
    $inced=~s/>/&gt;/g;
    $inced=~s{^(\s?\d+:) }{<font color='dddddd'>$1</font> }gm;

    $me->rawtext($codefile.":\n".$inced);
    $me->thistext("<pre><font color='#aa0000'>file: $codefile</font>\n$inced</pre>");
    $me->write_slide;

    $me->inc_cnt;

    my ($dir,$file,$run,$noperl)=$me->run_file($codefile,join(' ',@args));
    $me->rawtext($dir."/".$file.":\n".$run);
    my $perl = $noperl ? '' : 'perl ';
    $me->thistext("<pre><font color='#00cc00'>~/$dir\$</font> ".$perl."$file $args\n$run</pre>");
    $me->write_slide;

}

sub handle_for_run_and_ignore {
    my $me=shift;

    my ($codefile,@args)=split(' ',$me->node->[2][2]);
    my $args=join(' ' ,@args);
    my ($dir,$file,$run)=$me->run_file($codefile,$args);
    $me->dec_cnt;
}

sub handle_for_ignore { }

sub addtext {
    my ($me,$text)=@_;
    $me->oldtext(($me->oldtext||'')."\n".$text);
}

sub highlight {
    my $me=shift;
    my $text=shift;
    $text=~s/>/&gt;/g;
	$text=~s/</&lt;/g;
    $text=~s{^(\s+)([~/](.*?)\$)(.*)}{$1<font color='#00cc00'>$2</font><font color='#cc0000'>$4</font>}mg;
    $text=~s{^(\s+)([\w\.]+:[~/](.*?)\$)(.*)}{$1<font color='#00cc00'>$2</font><font color='#cc0000'>$4</font>}mg;
	$text=~s{^(\s+)(file:.*)}{$1<font color='#aa0000'>$2</font>}g;
	$text=~s{\*\*(.*?)\*\*}{<b><font color='#ff0000'>$1</font></b>}gs;
	$text=~s{\%\%(.*?)\%\%}{<b><font color='#0000ff'>$1</font></b>}gs;
	$text=~s{\@\@(.*?)\@\@}{<b><font color='#ff00ff'>$1</font></b>}gs;
    return $text;
}

sub include_file {
    my ($me,$file,$raw)=@_;
    my @lines;
    if ($file =~ / @([0-9a-f]+)$/) {
        my $commit = $1;
        $file =~ s/ @.*$//;
        my $target = file($me->poddir.'/'.$file);
        my $basename = $target->basename;
        chdir($target->parent);

        my $content = `git show $commit:./$basename`;
        chdir($me->poddir);
        @lines = map { $_."\n" } split(/\n/,$content);
    }
    else {
        $file=$me->poddir."/".$file;

        open(my $in,$file) || die "cannot include $file: $!";
        @lines = <$in>;
        close $in;
    }

    my $text;
    if (wantarray) {
        return @lines;
    } else {
        my $line=1;
        my $size=@lines;
        my $stellen=length($size);
        my $format="%".$stellen."i: ";
        foreach (@lines) {
            $text.=sprintf($format,$line).$_;
            $line++;
        }
        return $text;
    }
}

sub run_file {
    my $me=shift;
    my $file=shift;
    my $args=shift || '';
    my $rv;
    my $dir='';
    my $noperl=0;
    if ($file =~m|^(.*)/(.*?)$|) {
        $dir=$1;
        $file=$2;
        my $poddir=$me->poddir."/".$dir;
        $rv=`cd $poddir;$^X $file $args 1>&1 2>&1`;
    } elsif ($file eq 'prove') {
        my $prove = $^X;
        $prove=~s/\/perl$/\/prove/;
        $rv=`$prove $args 2>&1`;
    } elsif (-e $file) {
        $rv=`$^X $file $args 2>&1`;
    } else {
        $rv=`$file $args 2>&1`;
        $noperl=1;
    }
    my @rv=split(/\n/,$rv);
    foreach (@rv) {
        s/</&lt;/g;
        s/>/&gt;/g;
        if (length($_) > 75) {
            $_=autoformat($_,{right=>55});
            $_=~s/\n+$//gs;
        }
    }
    return ($dir,$file,join("\n",@rv),$noperl);
}

sub write_lastpage {
    my $me=shift;
    $me->inc_cnt;
    $me->oldtext('');$me->current_heading('');$me->root(undef);
    $me->thistext($me->boilerplate."<h1>__END__</h1>");
    $me->rawtext($me->title." __END__");
    $me->write_slide;
}

sub make_tarball {
    my $me=shift;
    
    system("ln -s s00001.html index.html") unless -e 'index.html';

    my $arch=$me->meta->{archive_name}.".tar.gz";
    unlink $arch if -e $arch;

    my $this_dir=`pwd`;;
    $this_dir=~s|^.*/||;
    chdir('..');
    system("tar czf ".$arch." --exclude='.svn' --exclude=*.tar.gz --exclude=bin/local ".$this_dir);
    system("mv ".$arch." $this_dir");

    print "created archive $arch\n" unless $me->opts->{quiet};
}

=head2 Accessors

=head3 this_slide, next_slide, prev_slide

Return the filename of the current, the previous or the next slide.

=cut

sub this_slide {
    return sprintf("s%05d",shift->cnt).".html";
}

sub next_slide {
    my $me=shift;
    return unless $me->root;
    my $cnt=$me->cnt;
    return sprintf("s%05d",$cnt+1).".html";
}

sub prev_slide {
    my $cnt=shift->cnt;
    return sprintf("s%05d",($cnt)-1).".html" if $cnt-1;
}

sub rawtext {
    my $me=shift;
    my $val=shift;
    return $me->{rawtext} unless $val;
    $val=~s/\*\*//g;
    $val=~s/\%\%//g;
    $val=~s/\@\@//g;
    return $me->{rawtext}=$val;
}

=head1 AUTHOR

Thomas Klausner, C<< <domm@cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-pod2slides@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org>.  I will be notified, and then you'll automatically
be notified of progress on your bug as I make changes.

=head1 COPYRIGHT & LICENSE

Copyright 2006 Thomas Klausner, All Rights Reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1; # End of Pod2Slides
