module test_cString_mod

  use pfunit
  use cString_mod

  implicit none
  private

  public :: test_cString
  public :: test_cStringTrim

contains

  subroutine test_cString()
     character(len=*), parameter :: stringA ='a'
     character(len=*), parameter :: stringB ='abcdefghijklmnopqrstuvwxyz'
     character(len=*), parameter :: stringC ='a ' ! note trailing blank

     call assertEqual (stringA // NULL, cString(stringA))
     call assertEqual (stringB // NULL, cString(stringB))

  end subroutine test_cString

  subroutine test_cStringTrim()
     character(len=*), parameter :: stringA ='a ' ! note trailing blank

     call assertEqual ('a' // NULL, cString(stringA))

  end subroutine test_cStringTrim

end module test_cString_mod
