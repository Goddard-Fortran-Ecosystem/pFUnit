

! Note: maybe have multiple expectation types for subroutines, classes, etc.
! 

module PF_Expectation
  implicit none
  private

  public :: Expectation
  public :: Predicate
  public :: Subject
  public :: wasCalled, wasNotCalled, wasCalledOnce

  integer, parameter :: MAXLEN_NAME = 80
  type :: Subject
     character(len=MAXLEN_NAME) :: name
     procedure(subVoid), pointer, nopass :: ptr
  end type Subject

  interface Subject
     module procedure new_Subject
     module procedure new_Subject_name_only
  end interface Subject

  interface 
     subroutine subVoid
     end subroutine subVoid
  end interface


  type, abstract :: Predicate
  contains
     procedure(get_name), nopass, deferred :: get_name
  end type Predicate

  abstract interface
     function get_name() result(name)
        character(:), allocatable :: name
     end function get_name
  end interface

  type, extends(Predicate) :: WasCalled
  contains
     procedure, nopass :: get_name => get_name_was_called
  end type WasCalled

  type, extends(Predicate) :: WasNotCalled
  contains
     procedure, nopass :: get_name => get_name_was_not_called
  end type WasNotCalled

  type, extends(Predicate) :: WasCalledonce
  contains
     procedure, nopass :: get_name => get_name_was_called_once
  end type WasCalledonce
  
!!$  interface Predicate
!!$     module procedure new_predicate
!!$  end interface Predicate

! TDD
!!$  type(Predicate), parameter :: wasCalled     = Predicate('wasCalled')
!!$  type(Predicate), parameter :: wasNotCalled  = Predicate('wasNotCalled')
!!$  type(Predicate), parameter :: wasCalledOnce = Predicate('wasCalledOnce')
! todo:  
!    checking expectation sub called with right value (important for sci.)
!    syntax for distinguishing arguments -- (position/keys)
!    combined expectations -- one on method, one on argument
!    -- or combined in the text...
! todo expectation augment
!    - vary numbers & kinds of arguments 
! todo:  automatic generation -- for proposal
! todo:  a trivial example of interleaved method calls
! 
! todo question: !    how to require mock functions to return certain values

  type :: Expectation
     type(Subject) :: subj
     class (Predicate), allocatable :: pred
  end type Expectation

  interface Expectation
     module procedure new_expectation
  end interface Expectation

  
contains

  type(Subject) function new_Subject(name,sub) result(subj_)
    character(*) :: name
    procedure(subVoid), pointer :: sub
    subj_%name = name
    subj_%ptr => sub
    ! maybe include a reference too
  end function new_Subject

  type(Subject) function new_Subject_name_only(name) result(subj_)
    character(*) :: name
    subj_%name = name
    ! subj_%ptr => sub ! Maybe nullify...
    nullify(subj_%ptr)
    ! maybe include a reference too
  end function new_Subject_name_only

!  type(Subject) function newSubject(name) result(subj_)

  type(Expectation) function new_Expectation(subj, pred) result(exp_)
    type(Subject), intent(in) :: subj
    class (Predicate), intent(in) :: pred
    exp_%subj = subj
    exp_%pred = pred
 end function new_Expectation


 function get_name_was_called() result(name)
    character(:), allocatable :: name
    name = 'wasCalled'
 end function get_name_was_called

 function get_name_was_not_called() result(name)
    character(:), allocatable :: name
    name = 'wasNotCalled'
 end function get_name_was_not_called

 function get_name_was_called_once() result(name)
    character(:), allocatable :: name
    name = 'wasCalledOnce'
 end function get_name_was_called_once

end module PF_Expectation
