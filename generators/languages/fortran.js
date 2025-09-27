/**
 * Fortran Code Base Generator
 * Generates boilerplate Fortran code
 */

function generateFortranCodeBase() {
    return `\n! Basic Fortran program\n\nPROGRAM HelloWorld\n    implicit none\n    \n    ! Variable declarations\n    character(len=50) :: message\n    character(len=50) :: program_desc\n    \n    ! Initialize variables\n    message = 'Hello, World!'\n    program_desc = 'This is a basic Fortran program.'\n    \n    ! Print messages\n    write(*,*) trim(message)\n    write(*,*) trim(program_desc)\n    \nEND PROGRAM HelloWorld\n\n! Alternative main program\n! program main\n!     write(*,*) 'Hello, World!'\n!     write(*,*) 'This is a basic Fortran program.'\n! end program main\n\n! Example subroutine (uncomment to use)\n! subroutine greet(name)\n!     character(len=*), intent(in) :: name\n!     write(*,*) 'Hello, ', trim(name), '!'\n! end subroutine greet\n\n! Example usage\n! call greet('TSI Student')\n\n! Example with arrays\n! program array_example\n!     implicit none\n!     integer :: i\n!     real, dimension(5) :: numbers = (/1.0, 2.0, 3.0, 4.0, 5.0/)\n!     \n!     do i = 1, 5\n!         write(*,*) 'Number:', numbers(i)\n!     end do\n! end program array_example\n`;
}

module.exports = {
    generateFortranCodeBase
};