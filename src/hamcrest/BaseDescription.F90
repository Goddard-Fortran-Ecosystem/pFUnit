module pf_BaseDescription_mod
   use pf_MatcherDescription_mod
   implicit none
   private

   public :: BaseDescription

   type, abstract, extends(MatcherDescription) :: BaseDescription
      private
   contains
      procedure :: append_text
      procedure :: append_description_of
      procedure :: append_value_scalar
      procedure :: append_string
      procedure(append_character), deferred :: append_character
      generic :: append => append_string
      generic :: append => append_character
      procedure :: append_list
   end type BaseDescription

   abstract interface 
      subroutine append_character(this, char)
        import BaseDescription
        class (BaseDescription), intent(inout) :: this
        character(len=1), intent(in) :: char(1)
      end subroutine append_character
   end interface

   
contains


   subroutine append_text(this, text)
      class (BaseDescription), intent(inout) :: this
      character(*), intent(in) :: text

      call this%append(text)

   end subroutine append_text


   subroutine append_description_of(this, value)
     class (BaseDescription), intent(inout) :: this
     class (SelfDescribing), intent(in) :: value

     call value%describe_to(this)
   end subroutine append_description_of

   subroutine append_value_scalar(this, value)
     class (BaseDescription), intent(inout) :: this
     class(*), intent(in) :: value

     select type (value)
     type is (character(*))
        call this%append('"'//value// '"')
     type is (integer)
        call this%append('<')
        call this%append(description_of(value))
        call this%append('>')
     class default
        ERROR STOP __FILE__
     end select

    end subroutine append_value_scalar


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

    subroutine append_list(this, start, separator, end, values)
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
    end subroutine append_list
    
    function description_of(value) result(string)
      character(:), allocatable :: string
      class(*), intent(in) :: value

      character(128) :: buffer
      
      select type (value)
      type is (integer)
         write(buffer,'(i0)') value
         string = trim(buffer)
      class default
         print*,' not implemented ', __FILE__,__LINE__
      end select
    end function description_of

    

end module pf_BaseDescription_mod
