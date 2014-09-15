use Test::More tests => 1;

BEGIN {
use_ok( 'Pod2Slides' );
}

Pod2Slides->process;

diag( "Testing Pod2Slides $Pod2Slides::VERSION" );
