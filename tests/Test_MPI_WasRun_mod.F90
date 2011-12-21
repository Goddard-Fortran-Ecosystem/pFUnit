module Test_MPI_WasRun_mod
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

  subroutine HelloWorld(comm) ! mpi tests receive a comm
     use IO_Utilities_mod
    integer, intent(in) :: comm

    integer :: rank
    integer :: stat
    integer :: unit
    integer :: ier

#ifdef USE_MPI
    call mpi_comm_rank(comm, rank, ier)
#else
    rank = 0
#endif

    unit = OpenFile(fname(template_fname,rank), form='formatted', status='new')
    write(unit,*)template_msg,rank
    close(unit)

  end subroutine HelloWorld

  function fname(base, proc)
    character(len=*) :: base
    integer, intent(in) :: proc
    character(len=len(base)+4) fname
    
    write(fname,'(a,i4.4)') trim(base),proc
      
  end function fname
  
  function assumed_msg(proc)
    integer, intent(in) :: proc
    character(len=len(template_msg)+24) assumed_msg
    
    write(assumed_msg,*) template_msg, proc
    
  end function assumed_msg

  subroutine Broken_MPI_Method(comm)
    integer, intent(in) :: comm

    integer :: rank, ier


#ifdef USE_MPI
    call mpi_comm_rank(comm, rank, ier)
#else
    rank = 0
#endif

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

  subroutine setup(self, comm)
    type (fixture) :: self
    integer, intent(in) :: comm

    integer :: stat

    self%log = 'MPI setup'

  end subroutine setup

  subroutine teardown(self, comm)
     use IO_Utilities_mod
    integer, intent(in) :: comm
    type (fixture) :: self

    integer :: stat
    integer :: rank
    integer :: unit
    integer :: ier

    self%log = trim(self%log) // ' MPI teardown'

#ifdef USE_MPI
    call mpi_comm_rank(comm, rank, ier)
#else
    rank = 0
#endif

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

end module Test_MPI_WasRun_mod
