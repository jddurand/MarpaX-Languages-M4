use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Regexp implementation

class MarpaX::Languages::M4::Impl::Regexp {
    use MarpaX::Languages::M4::Role::Regexp;
    use MarpaX::Languages::M4::Type::Regexp -all;
    #
    # This is just to load re::engine::GNU once
    # without affecting $^H{regcomp}
    #
    our $HAVE_re__engine__GNU;

    BEGIN {
        my $hasPreviousRegcomp = exists( $^H{regcomp} );
        my $previousRegcomp = $hasPreviousRegcomp ? $^H{regcomp} : undef;
        $HAVE_re__engine__GNU = eval 'use re::engine::GNU; 1;' || 0;
        #
        # Always restore previous regcomp engine
        #
        if ($hasPreviousRegcomp) {
            $^H{regcomp} = $previousRegcomp;
        }
    }

    # VERSION

    # AUTHORITY

    has syntax => (
        is       => 'ro',
        isa      => Str,
        required => 1
    );

    has pattern => (
        is       => 'ro',
        isa      => Str,
        required => 1
    );

    has _regexp => (
        is      => 'rwp',
        isa     => RegexpRef,
        default => undef
    );

    has regexp_errstr => (
        is       => 'rwp',
        isa      => Str,
        default => ''
    );

    method regexp_comp (Str $regexpString --> Bool) {
        my $hasPreviousRegcomp = exists( $^H{regcomp} );
        my $previousRegcomp = $hasPreviousRegcomp ? $^H{regcomp} : undef;

        if ( $self->syntax ne 'perl' ) {
            if ( !$HAVE_re__engine__GNU ) {
                #
                # No need to go further
                #
                $self->_set_regexp_errstr('re::engine::GNU cannot be loaded');
                return false;
            }
        }

        my $ok = true;
        try {
            if ( $self->syntax eq 'perl' ) {
                #
                # regexp can be empty and perl have a very special
                # behaviour in this case. Avoid empty regexp.
                #
                delete( $^H{regcomp} );
                $self->_set__regexp(qr/$regexpString(?#)/sm);
            }
            else {
                $^H{regcomp} = $re::engine::GNU::ENGINE;
                $self->_set__regexp(qr/$regexpString/sm);
            }
        }
        catch {
            $self->_set_regexp_errstr("$_");
            $ok = false;
        };
        #
        # Always restore previous regcomp engine
        #
        if ($hasPreviousRegcomp) {
            $^H{regcomp} = $previousRegcomp;
        }

        return $ok;
    }

    #
    # Return -1 if error (then regexp_errstr is filled).
    # Returns 0 if no match.
    # Returns 1 if match.
    #
    method regexp_exec(Str $target --> Int) {
        my $hasPreviousRegcomp = exists( $^H{regcomp} );
        my $previousRegcomp = $hasPreviousRegcomp ? $^H{regcomp} : undef;

        if ( $self->syntax ne 'perl' ) {
            if ( !$HAVE_re__engine__GNU ) {
                #
                # No need to go further
                #
                $self->_set_regexp_errstr('re::engine::GNU cannot be loaded');
                return -1;
            }
        }

        my $rc;
        try {
          if ( $self->syntax eq 'perl' ) {
            delete( $^H{regcomp} );
          } else {
            $^H{regcomp} = $re::engine::GNU::ENGINE;
          }

          if ($target =~ $self->_regexp) {
            $rc = 1;
          } else {
            $rc = 0;
          }
        } catch {
          $self->_set_regexp_errstr("$_");
          $rc = -1;
        };

        #
        # Always restore previous regcomp engine
        #
        if ($hasPreviousRegcomp) {
            $^H{regcomp} = $previousRegcomp;
        }

        return $rc;
    }

    with 'MarpaX::Languages::M4::Role::Regexp';
}

1;
