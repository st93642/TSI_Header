/**
 * Factor Code Base Generator
 * Generates boilerplate code for Factor projects
 */

function generateFactorCodeBase() {
    return `! Basic Factor program

USING: io kernel sequences ;

! Main word - entry point
: hello-world ( -- )
    "Hello, World!" print
    "This is a basic Factor program." print ;

! Application logic
: run-app ( -- )
    "1.0" "Version: " prepend print

    ! Example with arrays
    { "Factor" "Forth" "Joy" } "Languages:" print
    [ "  " prepend print ] each

    ! Example with associative arrays
    H{ { "debug" t } { "port" 8080 } { "features" { "logging" "caching" } } }
    "Configuration:" print
    "  Debug: " write dup "debug" of . print
    "  Port: " write "port" of . print ;

! Utility words
: greet ( name -- greeting )
    "Hello, " prepend "!" append ;

: get-info ( -- info )
    "TSI Application v1.0" ;

! Main execution
hello-world
run-app

! Example usage
! "TSI Student" greet print
! get-info print`;
}

module.exports = {
    generateFactorCodeBase
};