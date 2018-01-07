! Based upon python's OptParse package

#include "unused_dummy.fh"
module pf_OptionParser_mod
   use pf_Option_mod
   use pf_OptionVector_mod
   use pf_StringVector_mod
   use pf_StringUnlimitedMap_mod
   use pf_KeywordEnforcer_mod
   implicit none
   private

   public :: OptionParser
   public :: NULL_OBJECT

   type :: OptionParser
      private
      type (OptionVector) :: options
   contains
      generic :: add_option => add_option_, add_option_as_attributes
      procedure :: add_option_
      procedure :: add_option_as_attributes
      procedure :: parse
      procedure :: get_option_matching
   end type OptionParser

   interface OptionParser
      module procedure new_OptionParser_list
      module procedure new_OptionParser_vector
   end interface OptionParser

   type :: NULL_TYPE
   end type NULL_TYPE

   type (NULL_TYPE), parameter :: NULL_OBJECT = NULL_TYPE()

contains


   function new_OptionParser_list(option_array) result(parser)
      type (OptionParser) :: parser
      type (Option), intent(in) :: option_array(:)

      integer :: i
      
      do i = 1, size(option_array)
         call parser%add_option(option_array(i))
      end do

   end function new_OptionParser_list


   function new_OptionParser_vector(option_list) result(parser)
      type (OptionParser) :: parser
      type (OptionVector), intent(in) :: option_list
      parser%options = option_list
   end function new_OptionParser_vector


   subroutine add_option_(this, opt)
      class (OptionParser), intent(inout) :: this
      type (Option), intent(in) :: opt

      call this%options%push_back(opt)
   end subroutine add_option_


   ! Allow up to 3 variants of option string.  Can extend if more are needed
   subroutine add_option_as_attributes(this, opt_string_1, opt_string_2, opt_string_3, unused, action, type, dest, default, const)
      class (OptionParser), intent(inout) :: this
      character(len=*), intent(in) :: opt_string_1
      character(len=*), optional, intent(in) :: opt_string_2
      character(len=*), optional, intent(in) :: opt_string_3
      class (KeywordEnforcer), optional, intent(in) :: unused

      character(len=*), optional, intent(in) :: action
      character(len=*), optional, intent(in) :: type
      character(len=*), optional, intent(in) :: dest
      character(len=*), optional, intent(in) :: default
      character(len=*), optional, intent(in) :: const

      type (Option) :: opt

      _UNUSED_DUMMY(unused)

      opt = opt%make_option(opt_string_1, opt_string_2, opt_string_2, opt_string_2, &
           & unused, action, type, dest, default, const)
      call this%add_option(opt)
      
   end subroutine add_option_as_attributes

   function parse(this, arguments, unused, unprocessed) result(option_values)
      type (StringUnlimitedMap) :: option_values
      class (OptionParser), intent(in) :: this
      type (StringVector), target, optional, intent(in) :: arguments
      class (KeywordEnforcer), optional, intent(in) :: unused
      type (StringVector), optional, intent(out) :: unprocessed

      type (StringVectorIterator) :: iter
      character(:), pointer :: argument

      type (Option), pointer :: opt
      integer :: arg_value_int
      real :: arg_value_real
      type (OptionVectorIterator) :: opt_iter
      character(:), allocatable :: dest
      
      iter = arguments%begin()
      do while (iter /= arguments%end())
         argument => iter%get()

         opt => this%get_option_matching(argument)
         if (associated(opt)) then

            select case (opt%get_action())
            case ('store')

               ! Get next argument as value
               call iter%next()
               argument => iter%get()
               select case (opt%get_type())
               case ('string')
                  call option_values%insert(opt%get_destination(), argument)
               case ('integer')
                  read(argument,*) arg_value_int
                  call option_values%insert(opt%get_destination(), arg_value_int)
               case ('real')
                  read(argument,*) arg_value_real
                  call option_values%insert(opt%get_destination(), arg_value_real)
               end select

            case ('store_true')
               call option_values%insert(opt%get_destination(), .true.)
            case ('store_false')
               call option_values%insert(opt%get_destination(), .false.)
            case default
               call option_values%insert(opt%get_destination(), NULL_OBJECT)
            end select
         end if

         call iter%next()
      end do


   end function parse

   function get_option_matching(this, argument) result(opt)
      type (Option), pointer :: opt
      class (OptionParser), target, intent(in) :: this
      character(*), intent(in) :: argument

      type (OptionVectorIterator) :: iter_opt
      type (StringVectorIterator) :: iter_opt_string

      character(:), pointer :: opt_string
      type (StringVector), pointer :: opt_strings
      
      opt => null() ! unless ...
      
      iter_opt = this%options%begin()
      do while (iter_opt /= this%options%end())
         opt => iter_opt%get()

         opt_strings => opt%get_option_strings()
         iter_opt_string = opt_strings%begin()
         do while (iter_opt_string /= opt_strings%end())
            opt_string => iter_opt_string%get()

            if (opt_string == argument) then
               return
            end if

            call iter_opt_string%next()
         end do

         call iter_opt%next()
      end do

   end function get_option_matching

end module pf_OptionParser_mod
