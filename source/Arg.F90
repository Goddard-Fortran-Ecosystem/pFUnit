module pf_Arg_mod
   use pf_KeywordEnforcer_mod
   use pf_StringVector_mod
   use pf_Null_mod
   implicit none
   private

   public :: Arg

   ! Supported actions
   enum, bind(c)
      enumerator :: STORE ! default
      enumerator :: STORE_CONST
      enumerator :: STORE_TRUE
      enumerator :: STORE_FALSE
      enumerator :: APPEND
      enumerator :: APPEND_CONST
      enumerator :: COUNT
      enumerator :: CALLBACK
      enumerator :: HELP
   end enum

   type ::Arg
      private
      character(:), allocatable :: destination
      character(:), allocatable :: description
      type (StringVector) :: option_strings
      character(:), allocatable :: action
      character(:), allocatable :: type
      class(*), allocatable :: default
   contains
      procedure, nopass :: make_option
      procedure :: get_destination
      procedure :: get_description
      procedure :: get_action
      procedure :: get_type
      procedure :: get_option_strings
      procedure :: get_default

      procedure :: matches
      procedure, nopass :: is_legal_option_string
      procedure, nopass :: is_short_option_string
      procedure, nopass :: is_long_option_string
   end type Arg

contains

   function make_option( &
        ! Positional arguments
        & opt_string_1, opt_string_2, opt_string_3, & ! enough is enough
        ! Keyword enforcer
        & unused, &
        ! Keyword arguments
        & action, type, dest, default, const) result(an_option)
      type (Arg), target :: an_option

      character(len=*), intent(in) :: opt_string_1
      character(len=*), optional, intent(in) :: opt_string_2
      character(len=*), optional, intent(in) :: opt_string_3
      class (KeywordEnforcer), optional, intent(in) :: unused

      character(len=*), optional, intent(in) :: action
      character(len=*), optional, intent(in) :: type
      character(len=*), optional, intent(in) :: dest
      character(len=*), optional, intent(in) :: const
      class(*), optional, intent(in) :: default

      type (StringVectorIterator) :: iter
      character(:), pointer :: opt_string
      
      call an_option%option_strings%push_back(opt_string_1)
      if (present(opt_string_2)) call an_option%option_strings%push_back(opt_string_2)
      if (present(opt_string_3)) call an_option%option_strings%push_back(opt_string_3)

      if (present(dest)) then
         an_option%destination = dest
      else
         iter = an_option%option_strings%begin()
         do while (iter /= an_option%option_strings%end())
            opt_string => iter%get()
            if (is_long_option_string(opt_string)) then
               an_option%destination = opt_string(3:)
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


   function get_description(this) result(description)
      character(:), allocatable :: description
      class (Arg), intent(in) :: this

      description = this%description
   end function get_description


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

   function get_default(this) result(default)
      class(*), allocatable :: default
      class(Arg), intent(in) :: this

      if (allocated(this%default)) default = this%default
   end function get_default


   logical function matches(this, argument)
      class (Arg), intent(in) :: this
      character(len=*), intent(in) :: argument
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

end module pf_Arg_mod
