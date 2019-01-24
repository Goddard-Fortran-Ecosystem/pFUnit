#include "unused_dummy.fh"

module pf_Arg
   use pf_KeywordEnforcer
   use gFTL_StringVectorMod
   use pf_None
   implicit none
   private

   public :: Arg

   type ::Arg
      private
      character(:), allocatable :: destination
      type (StringVector) :: option_strings
      character(:), allocatable :: action
      character(:), allocatable :: type
      class(*), allocatable :: default
      character(:), allocatable :: help
   contains
      procedure, nopass :: make_option
      procedure :: get_destination
      procedure :: get_action
      procedure :: get_type
      procedure :: get_option_strings
      procedure :: get_default
      procedure :: has_default
      procedure :: get_help
      procedure :: print_help

      procedure :: matches
      procedure, nopass :: is_legal_option_string
      procedure, nopass :: is_short_option_string
      procedure, nopass :: is_long_option_string
   end type Arg

contains

   function make_option( &
        ! Positional arguments
        & opt_string_1, opt_string_2, opt_string_3, opt_string_4, & ! enough is enough
        ! Keyword enforcer
        & unused, &
        ! Keyword arguments
        & action, type, dest, default, const, help) result(an_option)
      type (Arg), target :: an_option

      character(len=*), intent(in) :: opt_string_1
      character(len=*), optional, intent(in) :: opt_string_2
      character(len=*), optional, intent(in) :: opt_string_3
      character(len=*), optional, intent(in) :: opt_string_4
      class (KeywordEnforcer), optional, intent(in) :: unused

      character(len=*), optional, intent(in) :: action
      character(len=*), optional, intent(in) :: type
      character(len=*), optional, intent(in) :: dest
      character(len=*), optional, intent(in) :: const
      class(*), optional, intent(in) :: default
      character(len=*), optional, intent(in) :: help

      type (StringVectorIterator) :: iter
      character(:), pointer :: opt_string

      _UNUSED_DUMMY(unused)
      _UNUSED_DUMMY(const)
      
      call an_option%option_strings%push_back(opt_string_1)
      if (present(opt_string_2)) call an_option%option_strings%push_back(opt_string_2)
      if (present(opt_string_3)) call an_option%option_strings%push_back(opt_string_3)
      if (present(opt_string_4)) call an_option%option_strings%push_back(opt_string_4)

      if (present(dest)) then
         an_option%destination = dest
      else
         iter = an_option%option_strings%begin()
         do while (iter /= an_option%option_strings%end())
            
            opt_string => iter%get()
            if (is_long_option_string(opt_string)) then
               an_option%destination =  opt_string(3:)
               exit
            else if (is_short_option_string(opt_string)) then
               ! Is a short opt string - possibly default unless earlier short was found
               if (.not. allocated(an_option%destination)) then
                  an_option%destination = opt_string(2:2)
               end if
               ! Either way - keep trying for a long opt string
            end if
            call iter%next()
         end do
      end if

      if (present(action)) then
         an_option%action = action
      else
         an_option%action = 'store'
      end if

      if (present(type)) then
         an_option%type = type
      else
         an_option%type = 'string' ! default
      end if

      if (present(help)) then
         an_option%help = help
      end if

      if (present(default)) then
         an_option%default = default
      else ! leave it deallocated (questionable?)
      end if

   end function make_option


   function get_destination(this) result(destination)
      character(:), allocatable :: destination
      class (Arg), intent(in) :: this

      destination = this%destination
   end function get_destination


   function get_help(this) result(help)
      character(:), allocatable :: help
      class (Arg), intent(in) :: this

      help = this%help
   end function get_help


   function get_action(this) result(action)
      character(:), allocatable :: action
      class (Arg), intent(in) :: this

      action = this%action
   end function get_action


   function get_type(this) result(type)
      character(:), allocatable :: type
      class (Arg), intent(in) :: this

      type = this%type
   end function get_type



   function get_option_strings(this) result(option_strings)
      type (StringVector), pointer :: option_strings
      class (Arg), target, intent(in) :: this

      option_strings => this%option_strings

   end function get_option_strings

   logical function has_default(this)
      class(Arg), intent(in) :: this

      has_default = allocated(this%default)

   end function has_default

   function get_default(this) result(default)
      class(*), allocatable :: default
      class(Arg), intent(in) :: this

      if (this%has_default()) default = this%default
   end function get_default



   logical function matches(this, argument)
      class (Arg), intent(in) :: this
      character(len=*), intent(in) :: argument
      ! TODO: unimplemented
   end function matches



   ! Arg strings must either start with '-' or '--' and
   ! have at least one more word character
   logical function is_legal_option_string(string)
      character(len=*), intent(in) :: string

      character(*), parameter :: LETTERS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
      character(*), parameter :: ALLOWED_CHARACTERS = LETTERS // '0123456789_-'

      select case (len(string))
      case (0,1)
         is_legal_option_string = .false.
      case (2)
         is_legal_option_string = (string(1:1) == '-' .and. verify(string(2:2), LETTERS) == 0)
      case (3:)
         is_legal_option_string = (string(1:2) == '--' .and. verify(string(3:), ALLOWED_CHARACTERS) == 0)
      end select

   end function is_legal_option_string

   logical function is_short_option_string(string)
      character(len=*), intent(in) :: string

      is_short_option_string = .false. ! unless
      if (is_legal_option_string(string)) then
         is_short_option_string = (len(string) == 2)
      end if

   end function is_short_option_string

   logical function is_long_option_string(string)
      character(len=*), intent(in) :: string

      is_long_option_string = .false. ! unless
      if (is_legal_option_string(string)) then
         is_long_option_string = (len(string) > 2)
      end if

   end function is_long_option_string


   subroutine print_help(this)
      class (Arg), target, intent(in) :: this

      character(:), allocatable :: line
      type (StringVectorIterator) :: iter

      line = '  '
      
      iter = this%option_strings%begin()
      line = line // iter%get()
      call iter%next()
      do while (iter /= this%option_strings%end())
         line = line // ', ' //iter%get()
         call iter%next()
      end do

      if (allocated(this%help)) then
         write(*,'(a,T30,a)') line, this%help
      else
         print*, line
      end if

   end subroutine print_help

end module pf_Arg
