module ESMF_TestParameter_mod
   use pfunit_mod, only: MpiTestParameter
   implicit none

   private

   public :: ESMF_TestParameter
   
   type, extends(MpiTestParameter) :: ESMF_TestParameter
      integer :: numPETsRequested
   contains
      procedure :: setNumPetsRequested
      procedure :: getNumPetsRequested
      procedure :: toString
      procedure :: toStringActual
   end type ESMF_TestParameter

   interface ESMF_TestParameter
      module procedure :: newESMF_TestParameter
   end interface ESMF_TestParameter

contains

   !---------------------------------
   ! TestParameter procedures
   !---------------------------------
   
   ! Note that npes requested may not be available. 
   function newESMF_TestParameter(numPEtsRequested) result(testParameter)
      type (ESMF_TestParameter) :: testParameter
      integer, intent(in) :: numPETsRequested
      
      call testParameter%setNumPETsRequested(numPETsRequested)
      
   end function newESMF_TestParameter

   pure subroutine setNumPETsRequested(this, numPETsRequested)
      class (ESMF_TestParameter), intent(inout) :: this
      integer, intent(in) :: numPETsRequested
      this%numPETsRequested = numPETsRequested
   end subroutine setNumPETsRequested


   ! This function ensures that "npes = #" is included in the message string 
   ! for each exception.   It should rarely be overridden.
   function toStringActual(this) result(string)
      class (ESMF_TestParameter), intent(in) :: this
      character(:), allocatable :: string

      character(len=8) :: numPETsString
      character(:), allocatable :: tmp

      write(numPETsString,'(i0)') this%numPETsRequested

      string = 'numPETs=' // trim(numPETsString) 
      tmp = this%toString()

      if (len_trim(tmp) > 0) then
         string = string // ' :: ' // trim(tmp)
      end if

   end function toStringActual

   
   ! Provide a default empty string.  It is expected that this function
   ! will be overridden for user defined test cases.
   function toString(this) result(string)
      class (ESMF_TestParameter), intent(in) :: this
      character(:), allocatable :: string

      string = ''

   end function toString


   pure integer function getNumPETsRequested(this) result(numPETsRequested)
      class (ESMF_TestParameter), intent(in) :: this
      numPETsRequested = this%numPETsRequested
   end function getNumPETsRequested

   
end module ESMF_TestParameter_mod
