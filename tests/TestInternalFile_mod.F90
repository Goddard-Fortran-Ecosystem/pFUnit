module TestInternalFile_mod
  use InternalFile_mod
  use Assert_mod
  implicit none
  private

  public :: test_NumLines
  public :: test_AppendLine
  public :: test_LoadFile
  public :: test_WriteFile
  public :: setup, teardown, fixture


  type fixture
     private

     type (InternalFile_type) :: file

  end type fixture

contains

  subroutine setup(self)
    type (fixture) :: self

    self%file = InternalFile((/ 'line 1', 'line 2' /))

  end subroutine setup

  subroutine teardown(self)
    type (fixture) :: self
    call clean(self%file)
  end subroutine teardown

  subroutine test_NumLines(self)
    type (fixture) :: self

    call AssertEqual(2, NumLines(self%file))

  end subroutine test_NumLines

  subroutine test_AppendLine(self)
    type (fixture) :: self

    call AppendLine(self%file,'new line')

    call AssertEqual(GetLine(self%file,ith=3),'new line')
    call AssertEqual(GetLine(self%file,ith=1),'line 1')

  end subroutine test_AppendLine

  subroutine test_LoadFile(self)
    type (fixture) :: self
    type (InternalFile_type) :: file

    file = LoadFile('tests/test_data/test_loadfile')

    call AssertEqual(2, NumLines(file))
    call AssertEqual('line 1', GetLine(file,ith=1))
    call AssertEqual('line 2', GetLine(file,ith=2))

    call Clean(file)
    
  end subroutine test_LoadFile

  subroutine test_WriteFile(self)
    type (fixture) :: self
    type (InternalFile_type) :: file, file_check

    file = InternalFile( (/ 'line 1', 'line 2' /) )
    call WriteFile(file, 'tmpfile')
    file_check=LoadFile('tmpfile')

    call AssertEqual('line 1', GetLine(file_check,ith=1))
    call AssertEqual('line 2', GetLine(file_check,ith=2))

    call Clean(file)
    call Clean(file_check)
    
  end subroutine test_WriteFile

end module TestInternalFile_mod
