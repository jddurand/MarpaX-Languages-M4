use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Regexp generic implementation

class MarpaX::Languages::M4::Impl::Regexp {

    # VERSION

    # AUTHORITY

    use MarpaX::Languages::M4::Role::Regexp;
    use MarpaX::Languages::M4::Type::Regexp -all;
    use MooX::HandlesVia;

    has _engine => (
        is       => 'rwp',
        isa      => M4RegexpEngine
    );

    has _replacement => (
        is       => 'rwp',
        isa      => M4RegexpReplacement,
    );

    has regexp_errstr => (
        is       => 'rwp',
        isa      => Str
    );

    has _regexp => (
        is       => 'rwp',
        isa      => RegexpRef
    );

    has regexp_lpos => (
        is       => 'rwp',
        isa      => ArrayRef,
                        handles_via => 'Array',
                        handles => {
                                    'regexp_lpos_count' => 'count',
                                    'regexp_lpos_get' => 'get'
                                   }
    );

    has regexp_rpos => (
        is       => 'rwp',
        isa      => ArrayRef,
                        handles_via => 'Array',
                        handles => {
                                    'regexp_rpos_count' => 'count',
                                    'regexp_rpos_get' => 'get'
                                   }
    );

    method regexp_compile (ConsumerOf['MarpaX::Languages::M4::Role::Imp'] $impl, M4RegexpEngine $engine, Str $regexpString --> Bool) {

        my $regexp;

        my $hasPreviousRegcomp = exists( $^H{regcomp} );
        my $previousRegcomp
          = $hasPreviousRegcomp ? $^H{regcomp} : undef;

        try {
            if ( $engine eq 'perl' ) {
                #
                # Just make sure this really is perl
                #
                delete( $^H{regcomp} );
                #
                # regexp can be empty and perl have a very special
                # behaviour in this case. Avoid empty regexp.
                #
                $regexp = qr/$regexpString(?#)/sm;
            }
            else {
                use re::engine::GNU -debug => 1;
                $regexp = qr/$regexpString/sm;
                no re::engine::GNU;
            }

        }
        catch {
            $impl->logger_error( '%s: %s',
                $impl->impl_quote($regexpString), $_ );
        };

        $hasPreviousRegcomp
          ? $^H{regcomp} = $previousRegcomp
            : delete( $^H{regcomp} );

        if (defined($regexp)) {
          $self->_set__regexp($regexp);
          $self->_set__engine($engine);
          return true;
        } else {
          return false;
        }
    }

    method regexp_exec (ConsumerOf['MarpaX::Languages::M4::Role::Imp'] $impl, Str $string --> Bool) {

        my $rc = false;

        #
        # Just make sure this really is perl
        #
        my $hasPreviousRegcomp = exists( $^H{regcomp} );
        my $previousRegcomp
          = $hasPreviousRegcomp ? $^H{regcomp} : undef;

        #
        # Note: this looks like duplicated code, and it is.
        # But this cannot be avoided because $-/$+ are
        # lexically scoped, and our scope depend on the engine
        #
        try {
            if ( $self->_engine eq 'perl' ) {
                #
                # Just make sure this really is perl
                #
                delete( $^H{regcomp} );
                #
                # Execute perl engine
                #
                if ($string =~ $self->_regexp) {
                  $rc = true;
                  my @lpos = ();
                  my @rpos = ();
                  map {($lpos[$_], $rpos[$_]) = ($-[$_], $+[$_])} (0..$#-);
                  $self->_set_regexp_lpos(\@lpos);
                  $self->_set_regexp_rpos(\@rpos);
                }
            }
            else {
                use re::engine::GNU -debug => 1;
                #
                # Execute re::engine::GNU engine
                #
                if ($string =~ $self->_regexp) {
                  $rc = true;
                  my @lpos = ();
                  my @rpos = ();
                  map {($lpos[$_], $rpos[$_]) = ($-[$_], $+[$_])} (0..$#-);
                  $self->_set_regexp_lpos(\@lpos);
                  $self->_set_regexp_rpos(\@rpos);
                }
                no re::engine::GNU;
            }
        }
        catch {
            $impl->logger_error( '%s =~ %s: %s',
                $impl->impl_quote($self->_regexp), $impl->impl_quote($string), $_ );
        };

        $hasPreviousRegcomp
          ? $^H{regcomp} = $previousRegcomp
            : delete( $^H{regcomp} );

        return $rc;
    }


    method _allowBlockInSanitizedString (Str $string, RegexpRef $regexp, CodeRef $dollarOneToIndiceRef, CodeRef $indiceToReplacementRef, HashRef $wantedIndicesRef, Ref['SCALAR'] $maxArgumentIndiceRef --> Str) {

        #
        # Note: we know we are using perl regexp here
        #
        $string =~ s/$regexp/
          {
           #
           # Writen like this to show that this is a BLOCK on the right-side of eval
           #
           my $indice = $self->$dollarOneToIndiceRef(substr($string, $-[1], $+[1] - $-[1]));
           if ($indice > ${$maxArgumentIndiceRef}) {
             ${$maxArgumentIndiceRef} = $indice;
           }
           $self->$indiceToReplacementRef($indice, $wantedIndicesRef);
          }/eg;

        return $string;
    }

    method regexp_replace (ConsumerOf['MarpaX::Languages::M4::Role::Imp'] $impl, M4RegexpReplacement $replacement --> Bool) {
      #
      # Sanitize
      #
      my $safeReplacement = quotemeta($replacement);
      #
      # We allow only:
      # * $&                       quotemeta: \$\&
      # * $digits                  quotemeta: \$digits
      # * ${&}                     quotemeta: \$\{\&\}
      # * ${digits}                quotemeta: \$\{digits\}
      #
      # We want to warn about unexpanded thingies so, as in
      # _expansion2CodeRef we will count expected
      # replacements.
      #
      my $maxRegexpIndice    = -1;
      my %wantedRegexpIndice = ();
      $safeReplacement = $self->_allowBlockInSanitizedString
        (
         $safeReplacement,
         #
         # In perl mode this $& $1 etc... or ${&}, ${1} etc...
         #
         ($replacement eq 'perl') ?
         qr/\\\$((?:\\\&|\\\{\\\&\\\})|(?:[0-9]+|\\\{[0-9]+\\\}))/ :
         #
         # In extended GNU mode this \& \1 etc... or \{&}, \{1} etc...
         #
         ($replacement eq 'GNUext') ?
         qr/\\\\((?:\\\&|\\\{\\\&\\\})|(?:[0-9]+|\\\{[0-9]+\\\}))/ :
         #
         # In emacs mode this \& \1 etc...
         #
         qr/\\\\(\\\&|[0-9]+)/
         ,
         \&_regexpDollarOneToIndice,
         \&_regexpIndiceToReplacement,
         \%wantedRegexpIndice,
         \$maxRegexpIndice
        );
      my @match = ();
      my $maxPosIndice = $self->regexp_lpos_count - 1;
      foreach ( 0 .. $maxRegexpIndice ) {
        if ( $_ <= $maxPosIndice ) {
          my $l = $self->regexp_lpos_get($_);
          my $r = $self->regexp_rpos_get($_);
          $match[$_]
            = substr( $l, $r - $l );
        }
        else {
          $impl->logger_warn(
                            '%s: sub-expression number %d not present',
                            $impl->impl_quote('regexp'), $_ );
          $match[$_] = '';
        }
      }
      my $rc = eval "\"$safeReplacement\"";
      if ($@) {
        #
        # Should not happen, we have sanitized the replacement string
        #
        $impl->logger_error( '%s: Internal error %s',
                             $impl->impl_quote('regexp'), $@ );
        $rc = '';
      }

      return $rc;
    }

   with 'MarpaX::Languages::M4::Role::Regexp';
}

1;
