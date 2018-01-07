!----------------------------------------------------------------------------
! Currently this implementation is closer to the python's (obsolete)
! OptParse package.   Names have been changed in the hopes of heading
! towards something that works like the newere ArgParse package.
!
! Some of the functionality is overkill for pFUnit, but adding features
! could make this a nice separate package for use in othe Fortran projects.
!----------------------------------------------------------------------------

#include "unused_dummy.fh"
module pf_ArgParser_mod
   use pf_Arg_mod
   use pf_ArgVector_mod
   use pf_StringVector_mod
   use pf_StringUnlimitedMap_mod
   use pf_KeywordEnforcer_mod
   use pf_Null_mod
   implicit none
   private

   public :: ArgParser

   type :: ArgParser
      private
      type (ArgVector) :: options
   contains
      generic :: add_option => add_option_, add_option_as_attributes
      procedure :: parse
      procedure :: get_defaults
      procedure :: get_option_matching
      procedure :: add_option_
      procedure :: add_option_as_attributes
   end type ArgParser

   interface ArgParser
      module procedure new_ArgParser_list
      module procedure new_ArgParser_vector
   end interface ArgParser

contains


   function new_ArgParser_list(option_array) result(parse)
      type (ArgParser) :: parse
      type (Arg), intent(in) :: option_array(:)

      integer :: i
      
      do i = 1, size(option_array)
         call parse%add_option(option_array(i))
      end do

   end function new_ArgParser_list


   function new_ArgParser_vector(option_list) result(parse)
      type (ArgParser) :: parse
      type (ArgVector), intent(in) :: option_list
      parse%options = option_list
   end function new_ArgParser_vector


   subroutine add_option_(this, opt)
      class (ArgParser), intent(inout) :: this
      type (Arg), intent(in) :: opt

      call this%options%push_back(opt)
   end subroutine add_option_


   ! Allow up to 3 variants of option string.  Can extend if more are needed
   subroutine add_option_as_attributes(this, &
        & opt_string_1, opt_string_2, opt_string_3, opt_string_4, &  ! Positional arguments
        & unused, &                                    ! Keyword enforcer
        & action, type, dest, default, const, description) ! Keyword arguments

      class (ArgParser), intent(inout) :: this
      character(*), intent(in) :: opt_string_1
      character(*), optional, intent(in) :: opt_string_2
      character(*), optional, intent(in) :: opt_string_3
      character(*), optional, intent(in) :: opt_string_4
      class (KeywordEnforcer), optional, intent(in) :: unused

      character(*), optional, intent(in) :: action
      character(*), optional, intent(in) :: type
      character(*), optional, intent(in) :: dest
      character(*), optional, intent(in) :: const
      character(*), optional, intent(in) :: description
      class(*), optional, intent(in) :: default

      type (Arg) :: opt

      _UNUSED_DUMMY(unused)

      opt = opt%make_option(opt_string_1, opt_string_2, opt_string_3, opt_string_4, &
           & action=action, type=type, dest=dest, default=default, const=const, description=description)
      call this%add_option(opt)
      
   end subroutine add_option_as_attributes

   function parse(this, arguments, unused, unprocessed) result(option_values)
      type (StringUnlimitedMap) :: option_values
      class (ArgParser), intent(in) :: this
      type (StringVector), target, optional, intent(in) :: arguments
      class (KeywordEnforcer), optional, intent(in) :: unused
      type (StringVector), optional, intent(out) :: unprocessed

      type (StringVectorIterator) :: iter
      character(:), pointer :: argument

      type (Arg), pointer :: opt
      integer :: arg_value_int
      real :: arg_value_real
      type (ArgVectorIterator) :: opt_iter
      character(:), allocatable :: dest
      character(:), target, allocatable :: embedded_value

      option_values=this%get_defaults()
      
      iter = arguments%begin()
      do while (iter /= arguments%end())
         argument => iter%get()

         opt => this%get_option_matching(argument, embedded_value)
         if (associated(opt)) then
            select case (opt%get_action())
            case ('store')

               if (embedded_value /= '') then
                  argument => embedded_value
               else
                  ! Get next argument as value
                  call iter%next()
                  argument => iter%get()
               end if
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

               deallocate(embedded_value)

            case ('store_true')
               call option_values%insert(opt%get_destination(), .true.)
            case ('store_false')
               call option_values%insert(opt%get_destination(), .false.)
            case default
               call option_values%insert(opt%get_destination(), NULL)
            end select
         end if

         call iter%next()
      end do


   end function parse

   function get_defaults(this) result(option_values)
      type (StringUnlimitedMap) :: option_values
      class (ArgParser), intent(in) :: this

      type (Arg), pointer :: opt
      type (ArgVectorIterator) :: opt_iter
      class(*), allocatable :: default
      
      opt_iter = this%options%begin()
      do while (opt_iter /= this%options%end())
         opt => opt_iter%get()
         default = opt%get_default()
         if (allocated(default)) then
            call option_values%insert(opt%get_destination(), default)
         end if
         call opt_iter%next()
      end do
      
   end function get_defaults

   function get_option_matching(this, argument, embedded_value) result(opt)
      type (Arg), pointer :: opt
      class (ArgParser), target, intent(in) :: this
      character(*), intent(in) :: argument
      character(:), allocatable, intent(out) :: embedded_value

      type (ArgVectorIterator) :: iter_opt
      type (StringVectorIterator) :: iter_opt_string

      character(:), pointer :: opt_string
      type (StringVector), pointer :: opt_strings

      intrinsic :: null
      integer :: n

      iter_opt = this%options%begin()
      do while (iter_opt /= this%options%end())
         opt => iter_opt%get()

         opt_strings => opt%get_option_strings()
         iter_opt_string = opt_strings%begin()
         do while (iter_opt_string /= opt_strings%end())
            opt_string => iter_opt_string%get()

            n = len(opt_string)
            if (len(argument) >= n) then ! cannot rely on short-circuit
               if (opt_string == argument(1:n)) then ! matches

                  if (opt%is_short_option_string(opt_string)) then
                     embedded_value = argument(n+1:)
                  else
                     if (len(argument) >= n+1) then
                        if (argument(n+1:n+1) == '=') then
                           embedded_value = argument(n+2:)
                        end if
                     else
                        embedded_value = ''
                     end if
                  end if

                  return
                     
               end if
            end if

            call iter_opt_string%next()
         end do

         call iter_opt%next()
      end do

      ! not found
      opt => null()

   end function get_option_matching

end module pf_ArgParser_mod
