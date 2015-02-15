use Moops;

role MarpaX::Languages::M4::Roles::Value {
    requires 'concat';
    requires 'push';
    requires 'unshift';
    requires 'elements';
    requires 'firstElement';
}
