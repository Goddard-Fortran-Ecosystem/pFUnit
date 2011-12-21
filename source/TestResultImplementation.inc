function newTestResult()
   type (TestResult), pointer :: newTestResult
   allocate(newTestResult)
   allocate(newTestResult%listeners(0))
   allocate(newTestResult%failures(0))
end function newTestResult

subroutine addFailure(this, aTest, anException)
   use Exception_mod, only: Exception
   use TestFailure_mod
   class (TestResult), intent(inout) :: this
   class (Test), intent(in) :: aTest
   type (Exception), intent(in) :: anException

   integer :: i, n
   type (TestFailure), allocatable :: tmp(:)

   n = this%numFailed
   allocate(tmp(n))
   tmp(1:n) = this%failures(1:n)
   deallocate(this%failures)
   allocate(this%failures(n+1))
   this%failures(1:n) = tmp
   deallocate(tmp)
   this%failures(n+1)%exception = anException
   this%failures(n+1)%testName = aTest%getName()

   this%numFailed = n + 1
   do i = 1, size(this%listeners)
     call this%listeners(i)%pListener%addFailure(aTest%getName(), anException)
   end do

end subroutine addFailure

integer function failureCount(this)
   class (TestResult), intent(in) :: this
   failureCount = this%numFailed
end function failureCount

subroutine startTest(this, aTest)
   class (TestResult), intent(inout) :: this
   class (Test), intent(in) :: aTest

   integer :: i

   this%numRun = this%numRun + 1
   do i = 1, size(this%listeners)
     call this%listeners(i)%pListener%startTest(aTest%getName())
   end do
end subroutine startTest

subroutine endTest(this, aTest)
   class (TestResult), intent(inout) :: this
   class (Test), intent(in) :: aTest

   integer :: i

   do i = 1, size(this%listeners)
     call this%listeners(i)%pListener%endTest(aTest%getName())
   end do
end subroutine endTest

integer function runCount(this)
   class (TestResult), intent(in) :: this
   runCount = this%numRun
end function runCount

subroutine run_TestResult(this, test, context)
   use Exception_mod
   use ParallelContext_mod
   class (TestResult), intent(inout) :: this
   class (GenericTestCase), intent(inout) :: test
   class (ParallelContext), intent(in) :: context

   type (Exception) :: anException
   if (context%isRootProcess()) call this%startTest(test)
   call test%runBare()

   call gatherExceptions(context)

   do
     anException = catchAny()
     if (anException%isNull()) exit
     call this%addFailure(test, anException)
   end do

   if (context%isRootProcess()) call this%endTest(test)

end subroutine run_TestResult

subroutine addListener(this, listener)
   use TestListener_mod, only: TestListener
   class (TestResult), intent(inOut) :: this
   class (TestListener), target, intent(in) :: listener
   
   integer :: n

   call extend(this%listeners)
   n = size(this%listeners)
   this%listeners(n)%pListener => listener

contains

   subroutine extend(listeners)
     type (ListenerPointer), allocatable, intent(inout) :: listeners(:)
     type (ListenerPointer) :: temp(size(listeners))
     integer :: n

     temp = listeners

     deallocate(listeners)
     n = size(listeners)
     allocate(listeners(n+1))

     listeners(:n) = temp

   end subroutine extend

end subroutine addListener

function getIthFailure(this, i) result(failure)
   class (TestResult), intent(in) :: this
   integer, intent(in) :: i
   type (TestFailure) :: failure

   failure = this%failures(i)
   
end function getIthFailure

logical function wasSuccessful(this)
   class (TestResult), intent(in) :: this
   wasSuccessful = (this%failureCount() ==  0)
end function wasSuccessful
