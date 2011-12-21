!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: pFUnit
!
!> @brief
!! Initailzes and finalizes pFUnit using Message Passing Interface (MPI) only.
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 16 Oct 2006
!!
! REVISION HISTORY:
! 16 Oct 2006 - Initial Version
! 13 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module pFUnit
   use Params_mod
   use Report_mod
   use Exception_mod
   use pFUnitException_mod
   use Assert_mod
   use BaseAddress_mod
   use TestResult_mod
   use TestMethodInfo_mod
   use TestCase_mod
   use ParameterizedTestCase_mod
   use TestSuite_mod
   use IO_Utilities_mod
   use FortranNameMangle_mod, only: lowerCase
#ifdef USE_DSO
   use pFUnitDriver_mod
#endif

   use TestInfo_mod
   use MpiServices_mod

#ifdef USE_MPI
   use MpiTestCase_mod
   use MpiClient_mod
   use MpiServer_mod
#endif

   implicit none

contains

   !---------------------------------------------------------------------------
   !> Initializes pFUnit when using MPI only for MPI initialization.  Placing 
   !! a call to this routine at the top of main enables the executable to 
   !! invoke itself.  This turn enables pFunit to launch MPI-based tests.
   !---------------------------------------------------------------------------
   subroutine pFUnit_Init()
#if defined(USE_MPI) | defined(USE_ESMF)
      use MpiServer_mod, only: amMpiServer
      integer :: ier
      if (amMpiServer()) then
         !----------------------------------------------------------------
         ! I should be an mpi session execd by the primary executable
         !----------------------------------------------------------------
         call runMpiServer()
         stop
      else if (isNewMpiDriver()) then
#ifdef USE_ESMF
         call esmf_initialize()
#else
         call mpi_init(ier)
#endif

         return
      else ! top-level process
         return 
      end if
#endif

   end subroutine pFUnit_Init

   !---------------------------------------------------------------------------
   !> Finalizes pFUnit when using MPI for launching.    
   !---------------------------------------------------------------------------
   subroutine pFUnit_Finalize()
#if defined(USE_MPI) | defined(USE_ESMF)
      use MpiServer_mod, only: amMpiServer
      integer :: ier
      if (isNewMpiDriver()) then
#ifdef USE_ESMF
         call esmf_finalize()
#else
         call mpi_finalize(ier)
#endif
         return
      else ! top-level process
         return 
      end if
#endif
   end subroutine pFUnit_Finalize

end module pFUnit
