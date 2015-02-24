use Moops;

# PODCLASSNAME

# ABSTRACT: Base conversion util class

class MarpaX::Languages::M4::Impl::Default::BaseConversion {

    # VERSION

    # AUTHORITY

    #
    # Eval: constants for radix and the grammar
    #
    our @nums = ( 0 .. 9, 'a' .. 'z', 'A' .. 'Z' );
    our %nums = map { $nums[$_] => $_ } 0 .. $#nums;

    # Adaptation of http://www.perlmonks.org/?node_id=27148
    method to_base (ClassName $class: Int $base, Int $number, Int $min --> Str) {

        return $nums[0] if ( $number == 0 );

        my $rep = '';    # this will be the end value.
                         #
                         # Per construction $base is in the range [1..36]
                         #
        if ( $base == 1 ) {
            if ( $number < 0 ) {
                $rep .= '-';
                $number = -$number;
            }
            while ( $min-- - $number > 0 ) {
                $rep .= '0';
            }
            while ( $number-- != 0 ) {
                $rep .= '1';
            }
        }
        else {
            my $s      = '';
            my $signed = ( $number < 0 ) ? 1 : 0;
            my $abs    = abs($number);
            while ( $abs > 0 ) {
                $s   = $nums[ $abs % $base ] . $s;
                $abs = int( $abs / $base );
            }
            if ($signed) {
                $s = "-$s";
            }
            if ( substr( $s, 0, 1 ) eq '-' ) {
                $rep .= '-';
                substr( $s, 0, 1, '' );
            }
            for ( $min -= length($s); --$min >= 0; ) {
                $rep .= '0';
            }
            $rep .= $s;
        }

        return $rep;
    }

    method fr_base (ClassName $class: Int $base, Int $rep) {
        my $number = 0;
        for ( $rep =~ /./g ) {
            $number *= $base;
            $number += $nums{$_};
        }
        return $number;
    }
}

1;
