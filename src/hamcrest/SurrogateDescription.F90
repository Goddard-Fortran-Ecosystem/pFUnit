! Surrogate design pattern to circumvent lack of forward-references
! in Fortran.
!
! Rouson, Damian & Adalsteinsson, Helgi & Xia, Jim. (2010). Design
! Patterns for Multiphysics Modeling in Fortran 2003 and C++. ACM
! Trans. Math. Softw.. 37. 10.1145/1644001.1644004.
!
!
module pf_SurrogateDescription
  implicit none
  private

  public :: SurrogateDescription

  type, abstract :: SurrogateDescription
  end type SurrogateDescription

end module pf_SurrogateDescription
