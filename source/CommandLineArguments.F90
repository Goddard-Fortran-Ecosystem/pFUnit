module pf_CommandLineArguments_mod
   use pf_StringVector_mod

contains

   function get_command_line_arguments() result(arguments)
      type (StringVector) :: arguments

      integer :: n_arguments
      integer :: i
      
      n_arguments = command_argument_count()
      do i = 1, n_arguments
         call arguments%push_back(get_argument(i))
      end do

   end function get_command_line_arguments


   function get_argument(i) result(argument)
      character(:), allocatable :: argument
      integer, intent(in) :: i

      integer :: length_of_argument
      call get_command_argument(i, length=length_of_argument)
      allocate(character(length_of_argument) :: argument)
      call get_command_argument(i, value=argument)

   end function get_argument

end module pf_CommandLineArguments_mod
