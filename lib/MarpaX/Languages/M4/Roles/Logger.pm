# PODNAME: MarpaX::Languages::M4::Roles::Logger

use Moops;

role MarpaX::Languages::M4::Roles::Logger {
    requires 'logger_error';
    requires 'logger_warn';
    requires 'logger_debug';
}
