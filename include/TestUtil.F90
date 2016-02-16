module TestUtil
    implicit none

    private
    public getResource

contains

    function getResource(path) result(absolutePath)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: absolutePath

        integer :: status, n

        n = len(__PROJECT_DIR__ // "/" // trim(adjustl(path)))
        allocate(character(len=n) :: absolutePath, stat=status)
        if (status == 0) then
            absolutePath = __PROJECT_DIR__ // "/" // trim(adjustl(path))
        end if

    end function getResource

end module TestUtil
