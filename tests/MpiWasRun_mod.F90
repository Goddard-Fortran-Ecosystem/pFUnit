module MpiWasRun_mod
  Use pFUnit
  implicit none
  private

  character(len=*), parameter :: template_fname = 'mpi_test_file_name_'
  character(len=*), parameter :: template_msg   = 'Hello world from process'

  public :: fname
  public :: assumed_msg
  public :: HelloWorld
  public :: Broken_MPI_Method
  public :: MPI_trivial

  public :: MPI_method
  public :: setUp, tearDown
  public :: new, delete
  public :: fixture

  public :: template_fname
  
  type fixture
     private
     character(len=80) :: log
     type (fixture), pointer :: reference => null()
  end type fixture

contains

  subroutine HelloWorld(info) ! mpi tests receive a comm
     use IO_Utilities_mod
     type (TestInfo_type), intent(in) :: info


     integer :: npes
     integer :: rank
     integer :: stat
     integer :: unit
     integer :: ier

     
    unit = OpenFile(fname(template_fname,processRank(info)), form='formatted', status='new')
    write(unit,*)template_msg, processRank(info), numProcesses(info)
    close(unit)

  end subroutine HelloWorld

  function fname(base, proc)
    character(len=*) :: base
    integer, intent(in) :: proc
    character(len=len(base)+4) fname
    
    write(fname,'(a,i4.4)') trim(base),proc
      
  end function fname
  
  function assumed_msg(proc, npes)
    integer, intent(in) :: proc
    integer, intent(in) :: npes
    character(len=len(template_msg)+36) assumed_msg
    
    write(assumed_msg,*) template_msg, proc, npes
    
  end function assumed_msg

  subroutine Broken_MPI_Method(info)
     type (TestInfo_type) :: info

    integer :: rank

    rank = processRank(info)
    if (mod(rank,2)==1) call RaiseException('Broken MPI method.')
    
  end subroutine Broken_MPI_Method

  ! do nothing
  subroutine MPI_trivial(comm)
    integer, intent(in) :: comm
  end subroutine MPI_trivial

  ! Warning the following setup/teardown pair intentionally create
  ! a file that must be deleted by the test driver.   This file is
  ! used to verify that the standard setup/method/teardown sequence
  ! is invoked.

  subroutine setup(self, info)
    type (fixture) :: self
    type (TestInfo_type) :: info

    integer :: stat

    self%log = 'MPI setup'

  end subroutine setup

  subroutine teardown(self, info)
     use IO_Utilities_mod
    type (TestInfo_type) :: info
    type (fixture) :: self
    type (Exception_type) :: ex
    integer :: stat
    integer :: rank
    integer :: unit
    integer :: ier

    self%log = trim(self%log) // ' MPI teardown'

    rank = processRank(info)

    unit = OpenFile(fileName=fname(template_fname, rank), form='formatted', status='new')
    if (catch(preserve=.true.)) return
    write(unit,*) trim(self%log)
    close(unit)

  end subroutine teardown

  subroutine MPI_method(self, comm)
    integer :: comm
    type (fixture) :: self
    self%log = trim(self%log) // ' MPI method'
  end subroutine MPI_method

  subroutine new(addr)
     use BaseAddress_mod
    type (BaseAddress_type), intent(out) :: addr
    type (fixture),  pointer :: aFixture
    
    type (BaseAddress_type) :: BaseAddress

    allocate(aFixture)
    aFixture%reference => aFixture ! self reference for cleanup
    addr=BaseAddress(aFixture)

  end subroutine new

  subroutine delete(self)
    type (fixture) :: self
    deallocate(self%reference)

  end subroutine delete

end module MpiWasRun_mod

