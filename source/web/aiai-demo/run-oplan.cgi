#!/local/bin/perl

# Based on form-mail.pl

# This is, amazingly enough, just to set DISPLAY.  /\/

# Updated: Mon Nov 14 21:37:58 1994 by Jeff Dalton

# Print out a content-type for HTTP/1.0 compatibility
# print "Content-type: text/html\n\n";

# Get the input
# read(STDIN, $buffer, $ENV{'CONTENT_LENGTH'});
$buffer = $ENV{'QUERY_STRING'};

# Split the name-value pairs
@pairs = split(/&/, $buffer);

foreach $pair (@pairs)
{
    ($name, $value) = split(/=/, $pair);

    # Un-Webify plus signs and %-encoding
    $value =~ tr/+/ /;
    $value =~ s/%([a-fA-F0-9][a-fA-F0-9])/pack("C", hex($1))/eg;

    # Stop people from using subshells to execute commands
    # Not a big deal when using sendmail, but very important
    # when using UCB mail (aka mailx).
    # $value =~ s/~!/ ~!/g; 

    # Uncomment for debugging purposes
    # print "Setting $name to $value<P>";

    $FORM{$name} = $value;
}

chdir "/home/oplan/development/jeff/2.x/source";

$ENV{'DISPLAY'} = $FORM{'DISPLAY'};

$ENV{'PATH'} = "$ENV{'PATH'}:/local/bin/X11.latest";

exec "web/aiai-demo/run-oplan-in-background";



