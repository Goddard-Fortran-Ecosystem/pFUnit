module PF_ExceptionList
   use PF_SourceLocation
   use PF_Exception, only: Exception
   use PF_ExceptionVector
   implicit none
   private

   public :: ExceptionList

   ! global static methods
   public :: throw
   public :: catch
   public :: catchNext
   public :: anyExceptions
   public :: anyErrors
   public :: gatherExceptions
   public :: clearAll
   public :: getExceptions
   public :: getNumExceptions

   type, extends(ExceptionVector) :: ExceptionList
      private
   contains
      generic :: throw => throw_exception, throw_message
      generic :: catch => catch_message
      generic :: catch => catch_any
      procedure :: gather

      procedure :: throw_exception
      procedure :: throw_message
      procedure :: catch_message
      procedure :: catch_next
      procedure :: catch_any

      procedure :: get_num_exceptions
      procedure :: get_exceptions
   end type ExceptionList

   type (ExceptionList), save :: global_exception_list

  interface throw
     module procedure throwMessage
  end interface

  interface catch
     module procedure catchAny
     module procedure catchMessage
  end interface catch

  interface anyExceptions
     module procedure anyExceptions_local
     module procedure anyExceptions_context
  end interface anyExceptions

  interface getNumExceptions
     module procedure getNumExceptions_local
     module procedure getNumExceptions_context
  end interface getNumExceptions


contains


   subroutine throw_exception(this, anException)
      class (ExceptionList), intent(inout) :: this
      class (Exception), intent(in) :: anException

      call this%push_back(anException)

   end subroutine throw_exception


   subroutine throw_message(this, message, location)
      class (ExceptionList), intent(inOut) :: this
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call this%throw(Exception(message, location))

   end subroutine throw_message


   ! Helper function
   ! Fortran does not require "short-circuit" so be careful with
   ! evaluation of optional arguments.
   logical function preserveMessage(preserve)
      logical, optional, intent(in) :: preserve

      preserveMessage = .false. ! default
      if (present(preserve)) preserveMessage = preserve

   end function preserveMessage


   logical function catch_message(this, message, preserve) result(found)
      class (ExceptionList), intent(inout) :: this
      character(len=*), intent(in) :: message
      logical, optional, intent(in) :: preserve

      type (ExceptionVectorIterator) :: iter
      class (Exception), pointer :: e

      iter = this%begin()
      do while (iter /= this%end())
         e => iter%get()
         if (e%getMessage() == message) then
            found = .true.
            if (.not. preserveMessage(preserve)) call this%erase(iter)
            return
         end if
         call iter%next()
      end do

      found = .false.
      
   end function catch_message

   function catch_next(this, preserve) result(anException)
      class (ExceptionList), intent(inOut) :: this
      logical, optional, intent(in) :: preserve
      type (Exception) :: anException

      
      if (.not. this%empty()) then
         ! TLC: Compiler workaround for gfortran 7.2 OSX (1/22/17)
#ifdef __GFORTRAN__
         block
           type (Exception), pointer :: tmp
           tmp => this%front()
           anException = tmp
         end block
#else
         anException = this%front()
#endif
         if (preserveMessage(preserve)) return
         
         call this%erase(this%begin())
      end if

   end function catch_next

   subroutine gather(this, context)
      use PF_ParallelContext
      class (ExceptionList), intent(inOut) :: this
      class (ParallelContext), intent(in) :: context

      integer :: i, j, n

      integer, allocatable :: buffer(:)
      integer, allocatable :: b(:)
      integer :: empty(0)

      class (Exception), pointer :: p
!!$      class (Exception) :: t
      ! TODO:  if we want variant subclasses of exception, then
      ! work needs to be done for deserialize.  Some sort of protocol layer
      ! to encode the subclass.
      type (Exception) :: t

      buffer = empty
      do i = 1, this%size()
         p => this%at(i)
         t = p
         t%message = context%labelProcess(t%message)
         b = t%serialize()
         buffer = [buffer, size(b), b]
      end do

      call this%clear()
      buffer = context%gatherInteger(buffer)

      j = 1
      if (context%isRootProcess()) then
         do while (j < size(buffer))
            n = buffer(j); j = j + 1
            call this%push_back(t%deserialize(buffer(j:j+n-1)))
            j = j + n
         end do
      end if
         
   end subroutine gather


   !------------------------
   ! public static methods |
   !------------------------

   subroutine throwMessage(message, location)
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      !$omp critical
      call global_exception_list%throw(message, location)
      !$omp end critical

   end subroutine throwMessage


   function catchNext(preserve) result(anException)
      logical, optional, intent(in) :: preserve
      type (Exception) :: anException

      anException = global_exception_list%catch_next(preserve)

   end function catchNext


   logical function catch_any(this, preserve)
      class (ExceptionList), intent(inout) :: this
      logical, optional, intent(in) :: preserve

      catch_any = .not. global_exception_list%empty()
      if (catch_any) then
         if (.not. preserveMessage(preserve)) then
            call this%pop_back()
         end if
      end if
         

   end function catch_any

   logical function catchAny(preserve)
      logical, optional, intent(in) :: preserve

      catchAny = global_exception_list%catch(preserve)

   end function catchAny


   logical function catchMessage(message, preserve)
      character(len=*), intent(in) :: message
      logical, optional, intent(in) :: preserve

      catchMessage = global_exception_list%catch(message, preserve)

   end function catchMessage


   subroutine clearAll()

      call global_exception_list%clear()

   end subroutine clearAll


   logical function anyExceptions_local() result(any)

      any = .not. global_exception_list%empty()

   end function anyExceptions_local


   logical function anyExceptions_context(context) result(any)
      use PF_ParallelContext, only: ParallelContext
      class (ParallelContext), intent(in) :: context

      any = context%allReduce(anyExceptions())

   end function anyExceptions_context


   logical function anyErrors()
      integer :: i
      integer :: n

      class (Exception), pointer :: e
      
      do i = 1, global_exception_list%size()
         e => global_exception_list%at(i)
         n = min(14,len(e%message))
         if (e%message(1:n) == 'RUNTIME-ERROR:') then
            anyErrors = .true.
            return
         end if
      end do
      anyErrors = .false.
   end function anyErrors


   integer function getNumExceptions_local() result(numExceptions)

      numExceptions = global_exception_list%size()

   end function getNumExceptions_local



   integer function get_num_exceptions(this) result(numExceptions)
      class (ExceptionList), intent(in) :: this

      numExceptions = this%size()

   end function get_num_exceptions



   integer function getNumExceptions_context(context) result(numExceptions)
      use PF_ParallelContext, only: ParallelContext
      class (ParallelContext), intent(in) :: context

      numExceptions = context%sum(getNumExceptions())
      
   end function getNumExceptions_context




   subroutine gatherExceptions(context)
      use PF_ParallelContext
      class (ParallelContext), intent(in) :: context

      call global_exception_list%gather(context)

   end subroutine gatherExceptions


   function get_exceptions(this) result(exceptions)
      type (ExceptionList) :: exceptions
      class (ExceptionList), intent(inOut) :: this

      ! Uses move_alloc() under the hood and leaves "this" empty
      call this%ExceptionVector%swap(exceptions%ExceptionVector)

   end function get_exceptions


   function getExceptions() result(exceptions)
      type (ExceptionList) :: exceptions

      exceptions = global_exception_list%get_Exceptions()
         
   end function getExceptions


end module PF_ExceptionList
