module MPI_Methods_mod
  use pfunit
  private

  public :: HelloWorld
  include 'mpif.h'

contains

  subroutine HelloWorld(comm) ! mpi tests receive a comm
     use IO_Utilities_mod
    integer, intent(in) :: comm

    integer :: rank
    integer :: stat
    integer :: unit
    integer :: ier

    call mpi_comm_rank(comm, rank, ier)

    unit = OpenFile(fname('mpidso.',rank), form='formatted', status='new')
    write(unit,*)'Hello world from process',rank
    close(unit)

  end subroutine HelloWorld

  function fname(base, proc)
    character(len=*) :: base
    integer, intent(in) :: proc
    character(len=len(base)+4) fname
    
    write(fname,'(a,i4.4)') trim(base),proc
      
  end function fname
  

end module MPI_Methods_mod
