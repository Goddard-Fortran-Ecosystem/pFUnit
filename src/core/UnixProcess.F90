!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: UnixProcess
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune, NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 07 Nov 2013 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
! This module encapsulates the ability to issue background system commands
! Unix pipes are used under the hood soo that results from such commands
! can be returned to the program for further processing.
! One can also check to see if the process has terminated, or optionally terminate
! the process.
!
! This is needed by the framework to manage a separate exeuctable that
! can be monitored for actual crashes as opposed to mere Assert
! failures.
!
! Although Fortran 2008 introduces the ability to spawn a process, it
! still provides no ability to return data directly back to the
! Fortran program.  So, with much regret, this module represents a bit
! of a departure from standard conforming Fortran.  It should be
! portable on any Unix system, and easily adapted to Windows by
! someone with relevant expertise.

module PF_UnixProcess
   use, intrinsic :: iso_c_binding
   implicit none
   private

   public :: UnixProcess
#if defined(Intel) || defined(PGI)
   public :: execute_command_line
#endif
   

   type UnixProcess
      private
      type (C_PTR) :: file = C_NULL_PTR
      integer :: pid = -1
   contains
      procedure :: get_line
      procedure :: is_active
      procedure :: terminate
      procedure :: get_pid
   end type UnixProcess

   interface UnixProcess
      module procedure newProcess
   end interface UnixProcess

contains

   function newProcess(command, runInBackground) result(process)
      use PF_UnixPipeInterfaces, only: popen
      use PF_StringUtilities, only: nullTerminate
      use PF_ExceptionList, only: throw
      type (UnixProcess) :: process
      character(len=*), intent(in) :: command
      logical, optional, intent(in) :: runInBackground

      character(len=:), allocatable :: fullCommand
      character(len=:), allocatable :: mode

      integer, parameter :: MAX_LEN = 80
      character(len=:), allocatable :: string

      !print *,'z00000'

      fullCommand = makeCommand(command, runInBackground)
      mode = nullTerminate('r')

      process%file = popen(fullCommand, mode)
      if (.not. c_associated(process%file)) then
         !print *,'z01000'
         call throw('Unsuccessful call to popen.')
         return
      end if

      if (present(runInBackground)) then
         if (runInBackground) then
            string = process%get_line()
            read(string,*) process%pid
         else
            process%pid = -1
         end if
      end if

   end function newProcess

   ! Background commands must return a PID for further interactions.
   ! Also commands need to be null-terminated to send to C procedures.
   function makeCommand(baseCommand, runInBackground) result(command)
      use PF_StringUtilities, only: nullTerminate
      character(len=:), allocatable :: command
      character(len=*), intent(in) :: baseCommand
      logical, optional, intent(in) :: runInBackground

      logical :: runInBackground_

      runInBackground_ = .false.
      if (present(runInBackground)) runInBackground_ = runInBackground

      command = baseCommand
      if (runInBackground_) then
         command = command // '& echo $!'
      end if
      command = nullTerminate(command)

   end function makeCommand


   logical function is_active(this)
      class (UnixProcess), intent(in) :: this
      
      integer, parameter :: MAX_LEN = 40
      character(len=MAX_LEN) :: command
      integer :: stat, cstat

      if (this%pid >=0) then
         write(command, '("kill -0 ",i0," > /dev/null 2>&1")') this%pid
         call execute_command_line(trim(command), exitStat=stat, cmdStat=cstat)
         is_active = (stat == 0)
      else
         is_active = .false.
      end if

   end function is_active

   subroutine terminate(this)
      class (UnixProcess), intent(inout) :: this

      integer, parameter :: MAX_LEN = 120
      character(len=MAX_LEN) :: command
      integer :: stat, cstat

      if (this%pid >=0) then
         write(command, '("kill -15 ",i0," > /dev/null 2>&1; ")') this%pid
         call execute_command_line(command, exitStat=stat, cmdStat=cstat)
         if (stat /= 0) then
            ERROR STOP "unable to terminate remote process"
         end if
      end if

   end subroutine terminate

   function get_line(this) result(line)
      use PF_UnixPipeInterfaces, only: getline
      use PF_UnixPipeInterfaces, only: free
      class (UnixProcess) :: this
      character(len=:), allocatable :: line

      type (C_PTR) :: pBuffer
      integer, parameter :: MAX_BUFFER_SIZE = 100000
      character(len=MAX_BUFFER_SIZE), pointer :: buffer
      integer (kind=C_SIZE_T) :: length
      integer (kind=C_SIZE_T) :: rc

      pBuffer = C_NULL_PTR
      rc = getline(pBuffer, length, this%file)
      if (length >= MAX_BUFFER_SIZE) then
         print*,'Error - need to increase MAX_BUFFER_SIZE in UnixProcess::getLine().'
      end if

      call c_f_pointer(pBuffer, buffer)
      ! drop newline and delimeter
      line = buffer(1:rc-1)

      call free(pBuffer)

    end function get_line

   integer function get_pid(this) result(pid)
      class (UnixProcess), intent(in) :: this
      pid = this%pid
   end function get_pid


end module PF_UnixProcess
