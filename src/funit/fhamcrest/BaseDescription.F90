module pf_BaseDescription
  use pf_MatcherDescription
  use pf_SelfDescribing
  use pf_SelfDescribingVector
  use, intrinsic :: iso_fortran_env
   implicit none
   private

   public :: BaseDescription
   public :: description_of

   type, abstract, extends(MatcherDescription) :: BaseDescription
      private
   contains
      procedure :: append_text
      procedure :: append_description_of
      procedure :: append_value_scalar
      procedure :: append_value_list
      procedure :: append_string
      procedure(append_character), deferred :: append_character
      generic :: append => append_string
      generic :: append => append_character
      procedure :: append_list_array
      procedure :: append_list_vector
   end type BaseDescription

   abstract interface 
      subroutine append_character(this, char)
        import BaseDescription
        class (BaseDescription), intent(inout) :: this
        character(len=1), intent(in) :: char(1)
      end subroutine append_character
   end interface

   interface description_of
      module procedure description_of_logical
      module procedure description_of_int32
      module procedure description_of_int64
      module procedure description_of_real32
      module procedure description_of_real64
      module procedure description_of_real128
      module procedure description_of_complex32
      module procedure description_of_complex64
      module procedure description_of_complex128
   end interface description_of
   
contains


   subroutine append_text(this, text)
      class (BaseDescription), intent(inout) :: this
      character(*), intent(in) :: text

      call this%append(text)

   end subroutine append_text


   subroutine append_description_of(this, value)
     class (BaseDescription), intent(inout) :: this
     class (SelfDescribing), intent(in) :: value

     call value%type_unsafe_describe_to(this)

   end subroutine append_description_of


   recursive subroutine append_value_scalar(this, value)
     use pf_Matchable
     use pf_Array
     class (BaseDescription), intent(inout) :: this
     class(*), intent(in) :: value

     select type (value)
     type is (character(*))
        call this%append('"'//value// '"')
     type is (logical)
        call this%append(description_of(value))
     type is (integer)
        call this%append('<')
        call this%append(description_of(value))
        call this%append('>')
#if (defined(_ISO_INT32) && (_ISO_INT32 != _INT_DEFAULT_KIND))
     type is (integer(kind=INT32))
        call this%append('<')
        call this%append(description_of(value))
        call this%append('_int32>')
#endif
#if (defined(_ISO_INT64) && (_ISO_INT64 != _INT_DEFAULT_KIND))
     type is (integer(kind=INT64))
        call this%append('<')
        call this%append(description_of(value))
        call this%append('_int64>')
#endif
     type is (real)
        call this%append('<')
        call this%append(description_of(value))
        call this%append('>')
     type is (real(kind(1.0d0)))
        call this%append('<')
        call this%append(description_of(value))
        call this%append('D>')
#if (defined(_ISO_REAL32) && (_ISO_REAL32 != _REAL_DEFAULT_KIND) && (_ISO_REAL32 != _DOUBLE_DEFAULT_KIND))
     type is (real(kind=REAL32))
        call this%append('<')
        call this%append(description_of(value))
        call this%append('_real32>')
#endif
#if (defined(_ISO_REAL64) && (_ISO_REAL64 != _REAL_DEFAULT_KIND) && (_ISO_REAL64 != _DOUBLE_DEFAULT_KIND))
     type is (real(kind=REAL64))
        call this%append('<')
        call this%append(description_of(value))
        call this%append('_real64>')
#endif
#if (defined(_ISO_REAL128) && (_ISO_REAL64 != _REAL_DEFAULT_KIND) && (_ISO_REAL128 != _DOUBLE_DEFAULT_KIND))
     type is (real(kind=REAL128))
        call this%append('<')
        call this%append(description_of(value))
        call this%append('_real128>')
#endif
     type is (complex)
        call this%append('<')
        call this%append(description_of(value))
        call this%append('>')
     type is (complex(kind=kind(1.d0)))
        call this%append('<')
        call this%append(description_of(value))
        call this%append('D>')
#if (defined(_ISO_REAL32) && (_ISO_REAL32 != _REAL_DEFAULT_KIND) && (_ISO_REAL32 != _DOUBLE_DEFAULT_KIND))
     type is (complex(kind=REAL32))
        call this%append('<')
        call this%append(description_of(value))
        call this%append('_real32>')
#endif
#if (defined(_ISO_REAL64) && (_ISO_REAL64 != _REAL_DEFAULT_KIND) && (_ISO_REAL64 != _DOUBLE_DEFAULT_KIND))
     type is (complex(kind=REAL64))
        call this%append('<')
        call this%append(description_of(value))
        call this%append('_real64>')
#endif
#if (defined(_ISO_REAL128) && (_ISO_REAL128 != _REAL_DEFAULT_KIND) && (_ISO_REAL128 != _DOUBLE_DEFAULT_KIND))
     type is (complex(kind=REAL128))
        call this%append('<')
        call this%append(description_of(value))
        call this%append('_real128>')
#endif
     class is (SelfDescribing)
        call this%append('<')
        call value%type_unsafe_describe_to(this)
        call this%append('>')
     class is (internal_array)
        select type (value)
        class is (internal_array_1d)
           call this%append_value_list("[",", ","]",value%items)
        class default
           ERROR STOP __FILE__ // ' :: unsupported rank'
        end select
     class default
        ERROR STOP __FILE__ // ' :: unsupported type'
     end select

    end subroutine append_value_scalar

    subroutine append_value_list(this, start, separator, end, values)
     use pf_Matchable
     use pf_Array
     class (BaseDescription), intent(inout) :: this
     character(*), intent(in) :: start
     character(*), intent(in) :: separator
     character(*), intent(in) :: end
     class(*), intent(in) :: values(:)

     integer :: i
     logical :: separate

     separate = .false.

     select type (values)
     class is (SelfDescribing)
        call this%append_list(start, separator, end, values)
     class default
        call this%append(start)
        do i = 1, size(values)
           if (separate) call this%append(separator)
           call this%append_value_scalar(values(i))
           separate = .true.
        end do
        call this%append(end)
     end select

   end subroutine append_value_list


    ! JUnit does this, but it seems overkill. Currently am not planning
    ! to intercept special characters.   Will be overridden in StringDescription.
    subroutine append_string(this, text)
      class(BaseDescription), intent(inout) :: this
      character(*), intent(in) :: text

      integer :: i

      do i = 1, len(text)
         call this%append([text(i:i)])
      end do
      
    end subroutine append_string

    subroutine append_list_array(this, start, separator, end, values)
      class(BaseDescription), intent(inout) :: this
      character(*), intent(in) :: start
      character(*), intent(in) :: separator
      character(*), intent(in) :: end
      class(SelfDescribing), intent(in) :: values(:)

      integer :: i
      logical :: separate

      separate = .false.

      call this%append(start)
      do i = 1, size(values)
         if (separate) call this%append(separator)
         call this%append_description_of(values(i))
         separate = .true.
      end do
      call this%append(end)

    end subroutine append_list_array

    subroutine append_list_vector(this, start, separator, end, values)
      class(BaseDescription), intent(inout) :: this
      character(*), intent(in) :: start
      character(*), intent(in) :: separator
      character(*), intent(in) :: end
      class(SelfDescribingVector), intent(in) :: values

      logical :: separate
      type (SelfDescribingVectorIterator) :: iter


      call this%append(start)

      separate = .false.
      iter = values%begin()
      do while (iter /= values%end())
         if (separate) call this%append(separator)
         call this%append_description_of(iter%get())
         separate = .true.
         call iter%next()
      end do
      call this%append(end)

    end subroutine append_list_vector


    function description_of_logical(value) result(string)
      use pf_Matchable
      character(:), allocatable :: string
      logical, intent(in) :: value

      if (value) then
         string = '.true.'
      else
         string = '.false.'
      end if

    end function description_of_logical

    function description_of_int32(value) result(string)
      use pf_Matchable
      character(:), allocatable :: string
      integer(kind=INT32), intent(in) :: value

      character(128) :: buffer
      write(buffer,'(i0)') value
      string = trim(buffer)
    end function description_of_int32

    function description_of_int64(value) result(string)
      use pf_Matchable
      character(:), allocatable :: string
      integer(kind=INT64), intent(in) :: value

      character(128) :: buffer
      write(buffer,'(i0)') value
      string = trim(buffer)
    end function description_of_int64

    function description_of_real32(value) result(string)
      use pf_Matchable
      character(:), allocatable :: string
      real(kind=REAL32), intent(in) :: value

      character(128) :: buffer
      write(buffer,'(g0)') value
      string = trim(buffer)
    end function description_of_real32

    function description_of_real64(value) result(string)
      use pf_Matchable
      character(:), allocatable :: string
      real(kind=REAL64), intent(in) :: value

      character(128) :: buffer
      write(buffer,'(g0)') value
      string = trim(buffer)
    end function description_of_real64


    function description_of_real128(value) result(string)
      use pf_Matchable
      character(:), allocatable :: string
      real(kind=REAL128), intent(in) :: value

      character(128) :: buffer
      write(buffer,'(g0)') value
      string = trim(buffer)
    end function description_of_real128

    function description_of_complex32(value) result(string)
      use pf_Matchable
      character(:), allocatable :: string
      complex(kind=REAL32), intent(in) :: value

      character(128) :: buffer
      write(buffer,'(g0)') value
      string = trim(buffer)
    end function description_of_complex32

    function description_of_complex64(value) result(string)
      use pf_Matchable
      character(:), allocatable :: string
      complex(kind=REAL64), intent(in) :: value

      character(128) :: buffer
      write(buffer,'(g0)') value
      string = trim(buffer)
    end function description_of_complex64


    function description_of_complex128(value) result(string)
      use pf_Matchable
      character(:), allocatable :: string
      complex(kind=REAL128), intent(in) :: value

      character(128) :: buffer
      write(buffer,'(g0)') value
      string = trim(buffer)
    end function description_of_complex128

    
end module pf_BaseDescription
