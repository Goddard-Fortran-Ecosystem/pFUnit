

! Note: maybe have multiple expectation types for subroutines, classes, etc.
! 

module Expectation_mod
  use StringConversionUtilities_mod, only : MAXLEN_STRING
  implicit none
  private

  public :: Expectation, newExpectation
  public :: Predicate, newPredicate
  public :: Subject, newSubject, newSubjectNameOnly
  public :: wasCalled, wasNotCalled, wasCalledOnce

  type :: Subject
     character(len=MAXLEN_STRING) :: name
     procedure(subVoid), pointer, nopass :: ptr
  end type Subject

  interface 
     subroutine subVoid
     end subroutine subVoid
  end interface


  type :: Predicate
     character(len=MAXLEN_STRING) :: name
  end type Predicate

  type(Predicate), parameter :: wasCalled     = Predicate('wasCalled')
  type(Predicate), parameter :: wasNotCalled  = Predicate('wasNotCalled')
  type(Predicate), parameter :: wasCalledOnce = Predicate('wasCalledOnce')

  type :: Expectation
     character(len=MAXLEN_STRING) :: name
     type(Subject) :: subj
     type(Predicate) :: pred
  end type Expectation

contains

  type(Predicate) function newPredicate(name) result(pred_)
    character(*) :: name
    pred_%name = name
  end function newPredicate

  type(Subject) function newSubject(name,sub) result(subj_)
    character(*) :: name
    procedure(subVoid), pointer :: sub
    subj_%name = name
    subj_%ptr => sub
    ! maybe include a reference too
  end function newSubject

  type(Subject) function newSubjectNameOnly(name) result(subj_)
    character(*) :: name
    procedure(subVoid), pointer :: sub
    subj_%name = name
    ! subj_%ptr => sub ! Maybe nullify...
    nullify(subj_%ptr)
    ! maybe include a reference too
  end function newSubjectNameOnly

!  type(Subject) function newSubject(name) result(subj_)

  type(Expectation) function newExpectation(name, subj, pred) result(exp_)
    character(*) :: name
    type(Subject), intent(in) :: subj
    type(Predicate), intent(in) :: pred
    exp_%name = name
    exp_%subj = subj
    exp_%pred = pred
  end function newExpectation

end module Expectation_mod
