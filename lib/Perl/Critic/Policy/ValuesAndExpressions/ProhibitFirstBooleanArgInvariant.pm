package Perl::Critic::Policy::ValuesAndExpressions::ProhibitFirstBooleanArgInvariant;

use 5.006001;
use strict;
use warnings;
use Readonly;
use Scalar::Util qw( refaddr );

use Perl::Critic::Utils qw{ :severities :data_conversion :classification :ppi };
use base 'Perl::Critic::Policy';

our $VERSION = '1.121_01';

#-----------------------------------------------------------------------------

Readonly::Scalar my $DESC => q{First argument '%s' of '%s' appears to be always true or always false};
Readonly::Scalar my $EXPL => q{Pass something variable, or use a different function};

#-----------------------------------------------------------------------------

sub supported_parameters { return ()                         }
sub default_severity     { return $SEVERITY_HIGH             }
sub default_themes       { return qw( core bugs )            }
sub applies_to           { return 'PPI::Token::Word' }

# TODO: allow calls to check to be configured

#-----------------------------------------------------------------------------

my %CALLS_TO_CHECK = hashify( qw(
    ok Test::More::ok
    assert Carp::Assert::assert
    affirm Carp::Assert::affirm
    assert_defined Carp::Assert::More::assert_defined
    assert_undefined Carp::Assert::More::assert_undefined
    assert_nonblank Carp::Assert::More::assert_nonblank
    assert_integer Carp::Assert::More::assert_integer
    assert_nonzero Carp::Assert::More::assert_nonzero
    assert_positive Carp::Assert::More::assert_positive
    assert_nonnegative Carp::Assert::More::assert_nonnegative
    assert_negative Carp::Assert::More::assert_negative
    assert_nonzero_integer Carp::Assert::More::assert_nonzero_integer
    assert_positive_integer Carp::Assert::More::assert_positive_integer
    assert_nonnegative_integer Carp::Assert::More::assert_nonnegative_integer
    assert_negative_integer Carp::Assert::More::assert_negative_integer
    assert_nonempty Carp::Assert::More::assert_nonempty
    assert_nonref Carp::Assert::More::assert_nonref
    assert_hashref Carp::Assert::More::assert_hashref
    assert_listref Carp::Assert::More::assert_listref
    lives_ok Test::Exception::lives_ok
    dies_ok Test::Exception::dies_ok
    throws_ok Test::Exception::throws_ok
) );


sub violates {
    my ( $self, $elem, undef ) = @_;

    if ( exists $CALLS_TO_CHECK{ $elem->content() } && is_any_call($elem) ) {
        my @args = _parse_arg_list( $elem );
#use Data::Dumper;
#print Dumper \@args;

        # Each item from parse_arg_list() usually is wrapped in a listref, but not always.
        # E.g.: "ok( require filetest, 'required pragma successfully' );"
        # Need to research how/whether to do checking in the non-listref case.
        if ( @args ) {
            my $first_arg = $args[0];  # can't use first_arg()
            if ( element_is_invariant($first_arg) ) {
                my $content;
                if ( ref($first_arg) eq 'ARRAY' ) {
                    $content = join( ' ', @$first_arg );
                    $first_arg = $first_arg->[0];
                }
                return $self->violation( sprintf( $DESC, $content, $elem->content() ), $EXPL, $first_arg );
            }
        }
    }

    return;
}


# TODO: '!' operator can make something invariant if its value is known
# to be true or false.


my %CHECK_ALL_CHILDREN_TOKENS = hashify(
    qw(
        PPI::Statement
        PPI::Statement::Compound
        PPI::Statement::Expression
        PPI::Structure::Block
        PPI::Structure::Constructor
        PPI::Structure::List 
        PPI::Structure::Subscript
    )
);

# Tokens that are always invariant
my %INVARIANT_TOKENS = hashify(
    qw(
        PPI::Statement::Package
        PPI::Token::Attribute
        PPI::Token::Cast
        PPI::Token::Data
        PPI::Token::End
        PPI::Token::Label
        PPI::Token::Number
        PPI::Token::Number::Binary
        PPI::Token::Number::Exp
        PPI::Token::Number::Float
        PPI::Token::Number::Hex
        PPI::Token::Number::Octal
        PPI::Token::Number::Version
        PPI::Token::Pod
        PPI::Token::Prototype
        PPI::Token::Quote::Literal
        PPI::Token::Quote::Single
        PPI::Token::QuoteLike::Words
        PPI::Token::Structure
        PPI::Token::Whitespace
    )
);

# Tokens that are never invariant
my %VARIANT_TOKENS = hashify(
    qw(
        PPI::Statement::Include
        PPI::Token::ArrayIndex
        PPI::Token::Magic
        PPI::Token::QuoteLike::Backtick
        PPI::Token::QuoteLike::Command
        PPI::Token::QuoteLike::Readline
        PPI::Token::Symbol
        PPI::Token::Unknown
    )
);

# Tokens that are variant depending on their content/arguments.
my %POSSIBLE_VARIANT_TOKENS = hashify(
    qw(
        PPI::Statement::Variable
        PPI::Token::HereDoc
        PPI::Token::Quote::Double
        PPI::Token::Quote::Interpolate
        PPI::Token::QuoteLike::Regexp
        PPI::Token::Regexp::Match
        PPI::Token::Regexp::Transliterate
    )
);

# TODO: PPI::Statement::Variable

# TODO: is the \ operator always-variant? i.e.: do we care about the
# address is returns or the contents of what it points to?

my %VARIANT_OPERATORS = hashify(
    qw(
        -r
        -w
        -x
        -o
        -R
        -W
        -X
        -O
        -e
        -z
        -s
        -f
        -d
        -l
        -p
        -S
        -b
        -c
        -t
        -u
        -g
        -k
        -T
        -B
        -M
        -A
        -C
    )
);

# Functions that are invariant if their arguments are invariant.
# Notes/TODOs:
#    'pack'/'unpack' - results are architecture-dependent, even w/ constant args
#    'delete' - how to tell that "delete { a=> 'b' }->{a}" is invariant?

# TODO: decide whether require/use/no should be forced to invariant.

# TODO: return value of chop() only depends on the last element of the
# array, not every element in the array.

# TODO: any call that uses the implicit $_ should be considered variant.

# TODO: make caller() variant depending on context:
# caller EXPR
# caller  Returns the context of the current subroutine call. In scalar
#         context, returns the caller's package name if there is a caller,
#         that is, if we're in a subroutine or "eval" or "require", and the
#         undefined value otherwise. In list context, returns

# TODO: is die() possible-variant because of side effects like setting
# $@ insdie an eval?

# TODO: whether 'do' is variant depends on whether it takes a block, a
# subroutine, or an expression.

# TODO: what to do with 'my', 'our', 'local', 'state'? Declarations don't make
# anything variant. Their initializations do, of course.

# TODO: what to do with 'import'?  Is there any spec out there?

# TODO: handle the fact that ref( \@{ $listref } ) is invariant.
# Also detect ref( sub { $a } ), ref( \my $var ), etc.

# TODO: detect reverse() in list context--list context only--with no parms as invariant.

# TODO: handle special nature of $a and $b in sort

# TODO: study returns undef/''(?) when the arg is invalid or empty, 1 when valid and non-empty.

# TODO: support 5.14+ allowing a naked listref with pop, etc.?

# 'q' is not here because it's covered by PPI::Token::Quote::Single.
# 'qq' is not here because it's covered by PPI::Token::Quote::Double.
# 'qw' is not here because it's covered by PPI::Token::Quote::Words.
# 'srand' - return value undocumented in 5.10.1, always 1 in 5.10.1.  Documented in 5.14 to return the seed, so it's invariant if called with an invariant arg.
my %POSSIBLE_VARIANT_FUNCTIONS = hashify(
    qw(
        chr crypt hex index lc lcfirst length oct ord reverse rindex sprintf substr tr uc ucfirst y

        m quotemeta s split

        abs atan2 cos exp hex int log oct sin sqrt

        pop push shift splice unshift

        grep join map sort

        delete each exists keys values

        vec

        defined eval local scalar

        bless ref untie

        return

        chomp chop

        srand
    )
);


# TODO:
#
#    Keywords related to the control flow of your Perl program
#        "sub"
#
#    Keywords related to switch
#        "given", "when", "default"
#        (These are only available if you enable the "switch" feature. See
#        feature and "Switch statements" in perlsyn.)

# Functions that are invariant regardless of their arguments.
# Notes:
#     'qr' - in all cases where the regex complies, an object is returned, and that object will stringize to a true value.
#     'study' - return value undocumented, always false in 5.10.1.
#     'undef' - documented to always return undef.
#     'format' - return value undocumented
#     'formline' - documented to always return 1
my %INVARIANT_FUNCTIONS = (
    'die' => {},
    'dump' => {},
    'exit' => { maxargs => 1 },
    'format' => {},
    'formline' => {},
    'qr' => { maxargs => 0 },
    'reset' => { maxargs => 1 },
    'study' => { maxargs => 1 },
    'undef' => { maxargs => 1 },
);


# Functions that are never invariant regardless of their arguments.
# 'qx' is not here because it's covered by PPI::Token::QuoteLike::Command.
my %VARIANT_FUNCTIONS = hashify(
    qw(
        binmode close closedir dbmclose dbmopen eof fileno flock getc print printf read readdir readline readpipe rewinddir say seek seekdir select syscall sysread sysseek syswrite tell telldir truncate warn write

        read syscall sysread syswrite

        chdir chmod chown chroot fcntl glob ioctl link lstat mkdir open opendir readlink rename rmdir stat symlink sysopen umask unlink utime

        wantarray

        alarm exec fork getpgrp getppid getpriority kill pipe setpgrp setpriority sleep system times wait waitpid

        accept bind connect getpeername getsockname getsockopt listen recv send setsockopt shutdown socket socketpair

        msgctl msgget msgrcv msgsnd semctl semget semop shmctl shmget shmread shmwrite

        endgrent endhostent endnetent endpwent getgrent getgrgid getgrnam getlogin getpwent getpwnam getpwuid setgrent setpwent

        endprotoent endservent gethostbyaddr gethostbyname gethostent getnetbyaddr getnetbyname getnetent getprotobyname getprotobynumber getprotoent getservbyname getservbyport getservent sethostent setnetent setprotoent setservent

        gmtime localtime time

        caller

        pos

        prototype

        rand

        tie tied
    )
);

# TODO: handle the case of prototype( sub { whatever } ), as opposed to prototype( \&mysub ).

# TODO: use the knowledge that if push has a second argument that the
# return value will be a true value. Also, if there is no second
# argument, we know that the array has not changed. That doesn't mean
# the return from push() is invariant, but it might still be useful.

# TODO: handle the fact that scalar ( $a, $b, 9 ) always returns 9 (see
# perldoc).  scalar $a, $b, 9 does this, too.

my %VARIANT_IF_FEWER_THAN_ONE_ARG = hashify(
    qw(
        abs
        alarm
        chomp
        chop
        chr
        cos
        defined
        eval
        exp
        hex
        int
        lc
        lcfirst
        length
        log
        oct
        ord
        pop
        quotemeta
        reverse
        shift
        sin
        sqrt
        srand
        uc
        ucfirst
        unshift
    )
);

my %VARIANT_IF_FEWER_THAN_TWO_ARGS = hashify(
    qw(
        split
    )
);


sub element_is_invariant {
    my $elem = shift;

    my $class = ref $elem;
print
    "$class ",
    ($class eq 'ARRAY' ? '' : $elem->content()),
    "\n"
;

    if ( $INVARIANT_TOKENS{ $class } ) {
        return 1;
    }
    elsif ( $VARIANT_TOKENS{ $class } ) {
        return 0;
    }
    elsif ( $class eq 'ARRAY' ) {
        return _all_are_invariant( $elem );  # check everything in the arrayref
    }
    elsif ( $class eq 'PPI::Token::Operator' ) {
        # Operators like the filetest operators can be operating on
        # constant values and still not have invariant results.
        my $operator = $elem->content();
        return !$VARIANT_OPERATORS{$operator};
    }
    elsif ( $CHECK_ALL_CHILDREN_TOKENS{$class} ) {
        return _all_are_invariant( [ $elem->children() ] );
    }
    elsif ( $POSSIBLE_VARIANT_TOKENS{ $class } ) {
        if ( $class eq 'PPI::Token::HereDoc' && $elem->content =~ /^<<\s*'/ ) {
            return 1;
        }
        elsif ( $class eq 'PPI::Token::Regexp::Match' ) {
            my $sprev = $elem->sprevious_sibling();
            # If sprev is not there or is not a match operator, then it's
            # matching against $_, so it's always # variant. Otherwise fall
            # through.
            if ( !($sprev && ($sprev eq '=~' || $sprev eq '!~')) ) {
                return 0;
            }
        }
        elsif ( $class eq 'PPI::Statement::Variable' ) {
            # variable statements consist of a word or list,
            # an operator, and then initializers. The latter two
            # are optional.  Only the initializers can be variant.
            die if ref $elem->child(0) ne 'PPI::Token::Word';
            my @children = $elem->children();
            while ( @children ) {
                my $token = shift @children;
                if ( $token->isa( 'PPI::Token::Operator' ) ) {
                    # If children are invariant, cause all the tokens
                    # associated with the variable declaration to be skipped.
                    return _all_are_invariant( \@children ) ? refaddr($children[-1]) : 0;
                }
            }
            return 1;
        }
        return !_has_interpolation( $elem );
    }
    elsif ( $class eq 'PPI::Token::Word' ) {
        if ( is_any_call($elem) ) {
            if ( $INVARIANT_FUNCTIONS{$elem} ) {
                my @args = _parse_arg_list( $elem );

                my $maxargs = $INVARIANT_FUNCTIONS{$elem}->{maxargs};
                if ( (defined $maxargs && $maxargs == 0) || scalar(@args) == 0 ) {
                    # Function will eat no args, so we don't need to
                    # return the element to ignore through.
                    return 1;
                }
                my $lastarg = (!defined $maxargs || ($maxargs >= scalar(@args))) ? scalar(@args)-1 : $maxargs-1;

                # Return the address of the last element of the
                # argument that the function call will eat, so the
                # caller will know which element to ignore through.
                return refaddr( $args[$lastarg]->[-1] );
            }
            elsif ( $POSSIBLE_VARIANT_FUNCTIONS{$elem} ) {
                my @args = _parse_arg_list( $elem );

                my $func = $elem->content();

                # Special handling for stringy eval, which we currently
                # consider to be variant, even though we could find some
                # invariant ones if we snooped inside the string.
                if ( $func eq 'eval' ) {
                    if( my $next = $elem->snext_sibling() ) {
                        if ( ref($next) ne 'PPI::Structure::Block' ) {
                            return 0;
                        }
                    }
                }

                if (
                    $VARIANT_IF_FEWER_THAN_ONE_ARG{$func} && scalar(@args) < 1
                    || $VARIANT_IF_FEWER_THAN_TWO_ARGS{$func} && scalar(@args) < 2
                ) {
                    return 0;
                }

                return _all_are_invariant( \@args );
            }
            else {
                return 0;
            }
        }
        else {
            return 0;
        }
    }

print "unhandled $class\n";
    return 0;
}


sub _all_are_invariant {
    my $elements = shift;

    my $skip_through_elem_address;
    foreach my $e ( @$elements ) {
        if ( $skip_through_elem_address ) {
            if ( $skip_through_elem_address != refaddr($e) ) {
                next;
            }
            else {
                $skip_through_elem_address = undef;
                next;
            }
        }
        my $is = element_is_invariant( $e );
        return 0 if !$is;
        if ( $is != 1 ) {
            # Flag that we need to skip up to and including this element.
            $skip_through_elem_address = $is;
            next;
        }
    }

    return 1;
}


# From ValuesAndExpressions::ProhibitInterpolationOfLiterals.
# See also Perl::Critic::Utils::is_ppi_constant_element().
sub _has_interpolation {
    my $elem = shift;

    my $content = $elem->isa( 'PPI::Token::HereDoc' ) ? join("\n", $elem->heredoc()) : $elem->content();
    return $content =~ m<
        (?: \A | [^\\] )
        (?: \\{2} )*
        (?: [\$\@] \S+ | \\[tnrfbae0xcNLuLUEQ] )
    >xmso;
}


sub is_any_call {
    my $elem = shift;
    my $is = is_method_call($elem) || is_function_call($elem);
    return $is;
}


my %_DECL_WORDS = hashify( qw( my local our state ) );
sub _parse_arg_list {
    my $elem = shift;

    my $sib  = $elem->snext_sibling();
    return if !$sib;
    return if $sib->isa('PPI::Token::Operator');

    my @args = parse_arg_list( $elem );
use Data::Dumper;
    foreach my $arg ( @args ) {
        my $first_token = ref($arg) eq 'ARRAY' ? $arg->[0] : $arg;
        if ( $first_token->isa('PPI::Token::Word') && $_DECL_WORDS{$first_token} ) {
            my $prev = $first_token->parent();
            if ( $prev && $prev->isa('PPI::Statement::Variable') ) {
                unshift @$arg, $prev;
            }
        }
    }
print STDERR '# ' . Dumper( \@args );
    return @args;
}


1;

__END__

#-----------------------------------------------------------------------------

=pod

=head1 NAME

Perl::Critic::Policy::ValuesAndExpressions::ProhibitFirstBooleanArgInvariant - e.g. catch errors like C< assert( 'assertion comment' ); >.


=head1 AFFILIATION

This Policy is part of the core L<Perl::Critic|Perl::Critic>
distribution.


=head1 DESCRIPTION

Carp::Assert::assert and functions like it exist to report or perform
some other operation based on the true/false value of their first
argument.  It rarely makes sense for that first argument's
value to be invariant. A invariant value is often the sign of
a bug, e.g. omitting passing the value to test to assert: "assert( 'assertion message' );".

=head1 CONFIGURATION

This Policy is not configurable except for the standard options.


=head1 AUTHOR

Mike O'Regan


=head1 COPYRIGHT

Copyright (c) 2014 Mike O'Regan.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.  The full text of this license
can be found in the LICENSE file included with this module.

=cut

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 78
#   indent-tabs-mode: nil
#   c-indentation-style: bsd
# End:
# ex: set ts=8 sts=4 sw=4 tw=78 ft=perl expandtab shiftround :
