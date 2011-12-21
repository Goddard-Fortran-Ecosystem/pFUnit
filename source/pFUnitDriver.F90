program pFUnitDriver
   use pFUnit
   implicit none

   type (TestResult_type) :: testResult
   type (Report_type) :: rprt
   character(len=100) :: libraryName
   character(len=100) :: pattern
   Character(Len=100) :: testSummary

   call pFUnit_init()

   loop_forever: do
      write(*,'("Enter name of library file: ",a)',ADVANCE='no')
      read(*,'(a)') libraryName
      if (len_trim(libraryName) == 0) exit ! done
      write(*,*)' '
      write(*,'("  Enter test pattern: ",a)',ADVANCE='no')
      read(*,'(a)') pattern

      testResult = newTestResult(mode=MODE_USE_STDOUT+MODE_USE_LOGFILE)
      call runDsoTests(trim(libraryName), trim(pattern), testResult)

      testSummary = Trim(Summary(testResult))
      print*
      print*, testSummary

      call clean(testResult)

      ! Catch remaining exceptions
      if (catch(preserve=.true.)) then
         print*,'Remaining exception: '
         rprt = GenerateExceptionReport()
         call Print(rprt)
         call clean(rprt)
      end if

   end do loop_forever

  call pFUnit_finalize()

end program pFUnitDriver
