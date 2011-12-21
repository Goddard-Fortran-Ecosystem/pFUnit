public :: Test
public :: MAXLEN_NAME

integer, parameter :: MAXLEN_NAME = 32

type, abstract :: Test
   character(len=MAXLEN_NAME) :: name = 'noname'
contains
!!$   procedure (I_CountTestCases), deferred :: countTestCases
!!$   procedure (I_Run), deferred :: run
   procedure :: countTestCases
   procedure :: run => irun
   procedure :: getName
   procedure :: setName
end type Test
