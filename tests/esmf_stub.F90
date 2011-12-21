subroutine esmf_initialize()
  integer :: ier
  call mpi_init(ier)
end subroutine esmf_initialize

subroutine esmf_finalize()
  integer :: ier
  call mpi_finalize(ier)
end subroutine esmf_finalize
