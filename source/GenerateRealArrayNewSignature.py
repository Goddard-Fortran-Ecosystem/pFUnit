#!/usr/bin/env python

# 
# Generate AssertRealArrays.F90, which provides assertEqual for arrays.
# 
# Usage:  ./GenerateRealArrayNewSignature.py
#
# M. Rilee
#    Initial: 2013-0304
# 

##### utility code #####

import Utilities
import CodeUtilities
import textwrap
import random
import copy

def indentKluge(indentString,txt):
    wrapper = textwrap.TextWrapper(initial_indent=indentString, subsequent_indent=indentString);
    txtList=map(wrapper.fill,str.splitlines(txt))
    return "\n".join(txtList)

def iterateOverMultiRank(nr,variableName,shapeName,centralText):
    indent= str(' '*(3*(nr+1)));
    txt = indentKluge(indent,centralText)
    r = range(nr); rrev = range(nr); rrev.reverse();
    codeSnippet = ''.join([' '*(3*(nr-i))+'do '+variableName+str(i+1)+'= 1,'+shapeName+'('+str(i+1)+')\n' for i in rrev])+txt+'\n'+''.join([' '*(3*(nr-i))+'end do\n' for i in r])
    #    print codeSnippet
    return codeSnippet

##### begin generation code #####

def DIMS(rank):
    if rank > 0:
        return '('+','.join([':' for i in range(rank) ])+')'
    else:
        return ''

def DIMS_SET(dims):
    "Return a comma separated list of dimensions, delineated by parentheses."
    retStr = ''
    if len(dims) > 0:
        retStr = '('+','.join([str(i) for i in dims])+')'
    return retStr

def DIMS_RANDOM_INTS(rank,maxDim):
    return [random.randint(1,maxDim) for i in range(rank)]

def RANDOM_INDEX(dims):
    return [random.randint(1,dims[i]) for i in range(len(dims))]

def DIMS_RANDOM(rank,maxDim):
    return DIMS_SET(DIMS_RANDOM_INTS(rank,maxDim))

def DIMS_IncrementRandomElement(dims):
    newDims = copy.copy(dims)
    i = random.randint(0,len(dims)-1)
    newDims[i] = newDims[i] + 1
    return DIMS_SET(newDims)

def FULLTYPE(fType):
    fTypes = { 'int' : 'integer',
              'char' : 'character' }
    if fType in fTypes:
        ret = fTypes[fType]
    else:
        ret = fType
    return ret

def KINDATTRIBUTE0(fType,precision):
    ret = ''
    if fType.lower() == 'real' :
        ret = 'kind=r'+str(precision)+''
    elif fType.lower() == 'complex' :
        ret = 'kind=r'+str(precision)+''
    return ret

def KINDATTRIBUTE(fType,precision):
    ret = ''
    if fType.lower() == 'real' :
        ret = '(kind=r'+str(precision)+')'
    elif fType.lower() == 'complex' :
        ret = '(kind=r'+str(precision)+')'
    return ret

def testKINDATTRIBUTE():
    print 'COMPLEX,32 -> ' + KINDATTRIBUTE('COMPLEX',32)
    print 'REAL,32 -> ' + KINDATTRIBUTE('REAL',32)
    print 'real,32 -> ' + KINDATTRIBUTE('real',32)
    print 'integer,32 -> ' + KINDATTRIBUTE('integer',32)

def DECLARE(variableName,fType,precision,rank,opts=', intent(in)'):
    return FULLTYPE(fType)+KINDATTRIBUTE(fType,precision)+opts+' :: '+variableName+DIMS(rank)

def DECLARESCALAR(variableName,fType,precision,rank):
    return FULLTYPE(fType)+KINDATTRIBUTE(fType,precision)+' :: '+variableName

def testDECLARE():
    print 'xVar,real,32,3 -> ' + DECLARE('xVar','real',32,3)
    print 'iVar,int,32,3 -> ' + DECLARE('iVar','int',32,3)
    print 'iVar,int,32,0 -> ' + DECLARE('iVar','int',32,0)
    print 'iVar,int,default,1 -> ' + DECLARE('iVar','int','default',1)

def OVERLOAD(routineName,fType,precision,rank):
    routineNameModifier=str(fType)+'_'+str(precision)+'_'+str(rank)
    return routineName+'_'+routineNameModifier.lower()+'D'

def testOVERLOAD():
    print 'testRoutine,real,32,2 -> '+OVERLOAD('testRoutine','real',32,2)
    print 'testRoutine,integer,64,0 -> '+OVERLOAD('testRoutine','integer',64,0)
    print 'testRoutine,int,32,4 -> '+OVERLOAD('testRoutine','int',32,4)

def DECLAREPOINTER(pointerName,fType,precision,rank):
    return FULLTYPE(fType)+KINDATTRIBUTE(fType,precision)+', pointer :: '+ \
        OVERLOAD(pointerName,fType,precision,rank)+DIMS(rank)+' = null()'

def testDECLAREPOINTER():
    print 'd-pointer: p,real,32,0 -> '+DECLAREPOINTER('p','real',32,0)
    print 'd-pointer: p,integer, 64, 1 -> '+DECLAREPOINTER('p','integer',64,1)
    print 'd-pointer: p,integer, 64, 3 -> '+DECLAREPOINTER('p','integer',64,3)

def NAME(fType,kind,rank):
    if fType == 'real' :
        fTypeToken = 'r'
    elif fType == 'complex' :
        fTypeToken = 'c'
    else:
        fTypeToken = 'int'
    if kind == 'default':
        kindToken = ''
    else:
        kindToken = str(kind)
    return fTypeToken+kindToken+'_'+str(rank)+'D'

def testNAME():
    print 'real,32,2 -> '+NAME('real',32,2)
    print 'integer,64,0 -> '+NAME('integer',64,0)

def EXPANDSHAPE(rank, variableName):
    if rank == 0:
        return ''
    elif rank == 1:
        return '(size('+variableName+'))'
    else:
        return '('+','.join(['size('+variableName+','+str(i)+')' for i in range(1,rank+1)])+')'

def testEXPANDSHAPE():
    print '0,test -> ' + str(EXPANDSHAPE(0,'test'))
    print '1,test -> ' + str(EXPANDSHAPE(1,'test'))
    print '3,test -> ' + str(EXPANDSHAPE(3,'test'))
    print '5,test -> ' + str(EXPANDSHAPE(5,'test'))

class ArrayDescription:
    def __init__(self,fType,kind,rank):
        self.fType = fType
        self.kind = kind
        self.rank = rank
        if rank == 0:
            print 'ArrayDescription:Warning: rank == 0!!!'
    def NAME(self):
        return NAME(self.fType, self.kind, self.rank)
    def DECLARE(self,variableName):
        return DECLARE(variableName,self.fType, self.kind, self.rank)
    def DECLARESCALAR(self,variableName):
        return DECLARESCALAR(variableName,self.fType, self.kind, 0)
    def KIND(self):
        return self.kind
    def RANK(self):
        return self.rank
    def FTYPE(self):
        return self.fType
    def EXPANDSHAPE(self,variableName):
        return EXPANDSHAPE(self.rank,variableName)
    def FailureMessageFork(self,messageForRank1,messageOtherwise):
        if self.rank == 1 :
            return messageForRank1
        else:
            return messageOtherwise

def AddBlockSymbols(predicate, blockSymbols, inStr):
    # Note a big change in how this works -- this will actually remove str...
    retStr = '' 
    if predicate :
        if blockSymbols :
            retStr = blockSymbols[0] + inStr + blockSymbols[1]
    return retStr
        
def MakeNamesWithRank(variableName, rank):
    retStr = ''
    if rank != 0:
        retStr = ','.join([variableName+str(i+1) for i in range(rank)])
    else:
        retStr = ''.join([variableName+str(rank+1)])
    return retStr

def compareELEMENTS(varName1,varName2,itername,shape,rank):
    if rank == 0:
        retStr = ''

def elideIfZero(test, insert):
    if test == 0:
        retString = ''
    else:
        retString = insert
    return retString

def testElideIfZero():
    print '0,test -> '+elideIfZero(0,'test'+',')
    print '1,test -> '+elideIfZero(1,'test'+',')

def ifZeroElse(test, ifTrue, ifFalse):
    if test == 0:
        return ifTrue
    else:
        return ifFalse

def ifElseString(test, string1, string2):
    retstr = ''
    if test:
        retstr = string1
    else:
        retstr = string2
    return retstr

def tolDECLARE(tolerance,descr,opts=', optional, intent(in)'):
    retStr = ''
    if tolerance == 0 :
        retStr = DECLARE('tolerance',descr.FTYPE(),descr.KIND(),0,opts=opts)
    else:
        retStr = """real(kind=r"""+str(tolerance)+""")"""+opts+""" :: tolerance"""
    return retStr

def makeSubroutineName(expectedName,foundName,tolerance):
    return \
       """assertEqual_""" + \
       expectedName + """_""" + foundName + """_tol""" + tolerance

       #
       # What does it mean to compare a 0D with a 1D array? MLR ***
       #

def generateASSERTEQUAL(expectedDescr, foundDescr, tolerance):
    subroutineName = makeSubroutineName(expectedDescr.NAME(), \
                                        foundDescr.NAME(), \
                                        str(tolerance))
        # Maybe set up an object where comments have some extra meaning.
    commentPreambleString = \
"""
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------
"""

    retString = \
        commentPreambleString + \
"""
   subroutine """+subroutineName+"""( &
   &  expected, found, message, tolerance, location )
     implicit none\n""" + \
"     " + expectedDescr.DECLARE('expected') + "\n" + \
"     " + foundDescr.DECLARE('found') + "\n" +\
"     " + tolDECLARE(tolerance,foundDescr,opts=', optional, intent(in)') + """
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,"""+KINDATTRIBUTE0(foundDescr.FTYPE(),foundDescr.KIND())+""")
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call """+subroutineName+"""_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine
""" + \
"""
   subroutine """+subroutineName+"""_internal( &
   &  expected, found, tolerance, message, location )
     implicit none\n""" + \
"     " + foundDescr.DECLARE('found') + "\n" +\
"     " + tolDECLARE(tolerance,foundDescr,opts=', intent(in)') + """
     real(kind=kind(tolerance)) :: tolerance_\n""" + \
"     " + expectedDescr.DECLARE('expected') + "\n" + \
"""
     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta"""+ foundDescr.EXPANDSHAPE('found') + \
"""
     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape("""+ str(expectedDescr.RANK()) + """)
     integer :: foundShape(""" + str(foundDescr.RANK()) + """)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: """ + MakeNamesWithRank('idx',foundDescr.RANK()) + """ ! for iter.
""" + \
"""
! Scalar "temp" variables
""" + \
"      " + expectedDescr.DECLARESCALAR('expected0') + "\n" + \
"      " + foundDescr.DECLARESCALAR('found0') + "\n" + \
"""
!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation("""+ \
        ifElseString(foundDescr.RANK()==0,'1',str(foundDescr.RANK())) + """)
!
""" + \
"""
      foundShape = shape(found)
""" + \
ifElseString(tolerance == 0, """
      ! Case:  tolerance == 0
      tolerance_ = DEFAULT_TOLERANCE
""", """
      ! Case:  tolerance !== 0
      tolerance_ = tolerance
""" ) + \
elideIfZero(expectedDescr.RANK(), \
"""
  
   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.
   !

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

!mlr-      ! SAME SIZE
!mlr-      if(size(expected) /= size(found)) then
!mlr-         call throwNonConformable(shape(expected), shape(found), location=location)
!mlr-         ! Test failed... So return?
!mlr-         return
!mlr-      end if
!mlr-
      ! SAME SHAPE
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
!mlr-   !   foundShape = shape(found)
!mlr-      do i = 1, size(expectedShape)
!mlr-         if( expectedShape(i) /= foundShape(i) ) then
!mlr-            call throwNonConformable(expectedShape, foundShape, location=location)
!mlr-            return ! bail
!mlr-         end if
!mlr-      end do
""" ) + \
"""
   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

""" + \
iterateOverMultiRank(foundDescr.RANK(),"idx","foundShape","""
   expected0 = expected""" + \
AddBlockSymbols(expectedDescr.RANK() > 0, \
                ['(',')'], \
                MakeNamesWithRank("idx",foundDescr.RANK())) \
                +"""
   found0 = found""" + \
AddBlockSymbols(foundDescr.RANK() > 0, \
                ['(',')'], \
                MakeNamesWithRank("idx",foundDescr.RANK())) \
                +"""
   if (expected0 /= found0 ) then """ + \
ifElseString(foundDescr.RANK() > 0, """
      idxLocation = (/ """ + MakeNamesWithRank("idx",foundDescr.RANK())+""" /) """, """
      idxLocation = (/ 0 /) """ ) + \
"""
!???      tolerance_ = 0.0
      call throwDifferentValuesWithLocation( &
      &       expected0, &
      &       found0, &
      &       idxLocation, & 
      &       tolerance_, &
      &       location )
      return ! bail
   end if

""") + """
contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   use AssertReal_mod, only : differenceReport, valuesReport
   implicit none\n""" + \
"   " + expectedDescr.DECLARESCALAR('expected') + "\n" + \
"   " + foundDescr.DECLARESCALAR('found') + "\n" + \
ifElseString(tolerance == 0,\
"""
   real(kind=kind(found)) :: tolerance""", \
"""
   real(kind=r"""+str(tolerance)+"""), intent(in) :: tolerance""" \
) + \
"""
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
!   call throw( &
!      & trim(valuesReport(real(expected), real(found))) // &
!      & trim(differenceReport(real(found - expected), real(tolerance))), &
!      & location=location &
!      & )

! Should fix the real() call below.  This is just reporting so we're okay for now.
    call throwDifferentValuesString( &
    &  real(expected),real(found),trim(locationInArray),location=location, &
    &  tolerance = real(tolerance_) )

!    call throw( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(real(expected), real(found))) // &
!         & trim(differenceReport(real(found - expected), real(tolerance))), &
!         & location=location &
!         & )
         
end subroutine throwDifferentValuesWithLocation

end subroutine """ + subroutineName + """_internal
"""
    return retString

class constraintASSERTEQUAL(CodeUtilities.routineUnit):
    def __init__(self, expectedDescr, foundDescr, tolerance):
        self.expectedDescr = expectedDescr
        self.foundDescr = foundDescr
        self.name = makeSubroutineName( \
                                        expectedDescr.NAME(), \
                                        foundDescr.NAME(), \
                                        str(tolerance) )
        #! bad... too simple...
        ## Add in the extra module procedures... If needed...
        self.setDeclaration(CodeUtilities.declaration(self.name,self.name))
        self.setImplementation(generateASSERTEQUAL(expectedDescr, \
                                                   foundDescr, \
                                                   tolerance ))
        self.tolerance = tolerance
        return
    
    def generateNonConformableTest(self):
        "Generate a module subroutine that tests correct handling of conconform. arrays."
        er = self.expectedDescr.RANK()
        fr = self.foundDescr.RANK()
        subName = self.name
        testName = 'test'+subName+'HandlesNonConformableArrays'
        eDims = DIMS_RANDOM_INTS(er)
        if er == fr:
            # enforce non conformity
            fDims = DIMS_IncrementRandomElement(eDims)
        else:
            # Question:  What about er or fr equalling zero?
            fDims = DIMS_RANDOM_INTS(fr)
        eDimsDeclared = DIMS_SET(eDims)
        fDimsDeclared = DIMS_SET(fDims)
        
        return \
"""
   subroutine """+testName+"""()
      use Exception_mod, only: new_line('$')
      use Assert_mod, only:    assertEqual

      real, dimension"""+eDimsDeclared+""" :: expected
      real, dimension"""+fDimsDeclared+""" :: found

      call assertEqual(expected, found)

      call assertCatch( &
         & 'Assertion failed: non-conformable real arrays.' // &
         & 'trim(shapeReport(shape(expected),shape(found))) &
         & )
         
   end subroutine """+testName+"""

"""

    def generateOneElementDifferent(self):
        "Generate a module subroutine that tests correct handling of one element being greatly different."
        er = self.expectedDescr.RANK()
        fr = self.foundDescr.RANK()
        subName = self.name
        testName = 'test'+subName+'HandlesOneElementBeingDifferent'
        eDims = DIMS_RANDOM_INTS(er,3)
        fDims = DIMS_RANDOM_INTS(fr,3)
        eDimsDeclared = DIMS_SET(eDims)
        fDimsDeclared = DIMS_SET(fDims)
        iRandomIndex = DIMS_SET(RANDOM_INDEX(fDims))
        
        return \
"""
   subroutine """+testName+"""()
      use Exception_mod, only: new_line('$')
      use Assert_mod, only:    assertEqual

      real, dimension"""+eDimsDeclared+""" :: expected
      real, dimension"""+fDimsDeclared+""" :: found
      real :: expectedScalar, foundScalar

      expectedScalar = 0.0
      foundScalar = 1.0

      expected = expectedScalar
      found"""+iRandomIndex+""" = foundScalar

      call assertEqual(expected, found)

      call assertCatch( &
         & 'Assertion failed: non-conformable real arrays.' // &
         & 'trim(shapeReport(shape(expected),shape(found))) &
         & )
         
   end subroutine """+testName+"""

"""

    def generateAssertCatch(self):
        "Check that the test result is as expected.  Might be refactored."
        return \
"""
! NOTE:  No apparent dependencies on arrays...  Might be refactored.
   subroutine assertCatch(string)
      use Exception_mod, only: getNumExceptions, Exception, catchAny
      use Assert_mod, only:    assertEqual
      character(len=*), intent(in) :: string
      type (Exception) :: anException

      if (getNumExceptions() > 0) then
         anException = catchAny()
         call assertEqual(string, anException%getMessage()) !, 'exceptions don't match'\rightParen
      else
         call assertEqual(string, ' ')!, 'missing exception'\rightParen
      end if
   end subroutine assertCatch
"""

    def generate_testIncludePreamble(self):
        "For the #includes..."
        return \
"""
#include "reflection.h"
"""

    def generate_testUSE(self):
        "Generate use-dependencies for the test suite."
        return testUSES()
    def generate_testDiscipline(self):
        return testDiscipline()
    def ADD(self,method):
        "Add a test method to the test suite.  Has some hardcoded dependencies."
        return \
            "call suite%addTest(newSimpleTestMethod(REFLECT("+method+")))\n"
    def generate_testMakeSuite(self,TestMethods):
        "Construct the test suite.  Add methods to call via 'TestMethods'. See 'ADD'. A good way to do this would be to pass in a list, and then map ADD onto that list, generating the text to be inserted below."
        return \
"""
   function suite()
      use TestSuite_mod, only:         newTestSuite, TestSuite
      use SimpleTestMethod_mod, only:  newSimpleTestMethod
      type (TestSuite), pointer ::     suite
      allocate(suite)
      suite => newTestSuite('assertRealArraySuite') ! note name """ + \
      TestMethods + \
"""      
   end function suite
"""


def constructAssertEqualInterfaceBlock():
    AssertEqualInterfaceBlock = CodeUtilities.interfaceBlock('assertEqual')
    [AssertEqualInterfaceBlock.addRoutineUnit(r) for r in constructASSERTS()]
    return AssertEqualInterfaceBlock

def testgenerateASSERTEQUAL():
    print 'real,32,3,int,default,3,32 -> ' + \
        generateASSERTEQUAL(ArrayDescription('real',32,3), \
                            ArrayDescription('int','default',3), \
                            32)

def VECTOR_NORM_NAME(rank):
    return """vectorNorm_"""+str(rank)+"""D"""    
    
def generateVECTOR_NORM(rank):
    subroutineName = VECTOR_NORM_NAME(rank)
    dimStr = DIMS(rank)
    retstr = \
"""
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = """+str(rank)+""".
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function """+subroutineName+"""(x, norm) result(y)
    real (kind=r64), intent(in) :: x"""+DIMS(rank)+"""
    integer :: norm
    real (kind=r64) :: y
""" + ifElseString(rank == 0, \
"""
    y = abs(x) ! independent of norm for rank=0 (scalar) case.
""", \
"""
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
       y = sqrt(sum(x**2))
    end select
""") + \
"""
  end function """ + subroutineName + """
"""
    return retstr

def testGenerateVECTOR_NORM():
    print "0, 3 -> "
    print generateVECTOR_NORM(0)
    print generateVECTOR_NORM(3)
    return

class VECTOR_NORM(CodeUtilities.routineUnit):
    def __init__(self,rank):
        self.rank = rank
        self.name = VECTOR_NORM_NAME(rank)
        self.declaration = CodeUtilities.declaration(self.name,self.name)
        self.declarations = [self.declaration]
        self.implementation \
            = CodeUtilities.implementation(self.name, generateVECTOR_NORM(rank))
        return

def constructVectorNormInterfaceBlock():
    VectorNormInterface = CodeUtilities.interfaceBlock('vectorNorm')
    map(VectorNormInterface.addRoutineUnit,[VECTOR_NORM(i) for i in range(6)])
    return VectorNormInterface

def isWithinToleranceName(rank):
    return """isWithinTolerance_"""+str(rank)+"""D"""

def generateIsWithinTolerance(rank):
    subroutineName = isWithinToleranceName(rank)
    dimStr = DIMS(rank)
    
    retstr = \
"""
   logical function """+subroutineName+"""(x, tolerance, norm)
     real (kind=r64), intent(in) :: x"""+dimStr+"""
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     """+subroutineName+""" = ( vectorNorm(x, norm) <= tolerance )

   end function """+subroutineName+"""
"""
    return retstr

class IsWithinTolerance(CodeUtilities.routineUnit):
    def __init__(self,rank):
        self.rank = rank
        self.name = isWithinToleranceName(rank)
        self.declaration = CodeUtilities.declaration(self.name, self.name)
        self.declarations = [self.declaration]
        self.implementation \
            = CodeUtilities.implementation(self.name, \
                                           generateIsWithinTolerance(self.rank))
        return

def constructIsWithinToleranceInterfaceBlock():
    iwt_InterfaceBlock = CodeUtilities.interfaceBlock('isWithinTolerance')
    map(iwt_InterfaceBlock.addRoutineUnit, [IsWithinTolerance(i) for i in range(6)])
    return iwt_InterfaceBlock

def testGenerateIsWithinTolerance():
    print "0,1 -> "
    print generateIsWithinTolerance(0)
    print generateIsWithinTolerance(1)
    return

def makeValuesReport():
    runit = CodeUtilities.routineUnit('valuesReport', \
"""
      character(len=MAXLEN_MESSAGE) function valuesReport(expected, found)
      real, intent(in) :: expected
      real, intent(in) :: found

      valuesReport = 'expected: <' // trim(toString(expected)) // &
      & '> but found: <' // trim(toString(found)) // '>'
   end function valuesReport
""")
    runit.setDeclaration(CodeUtilities.declaration(runit.getName(),'public '+runit.getName()))
    return runit

def makeDifferenceReport():
    runit = CodeUtilities.routineUnit('differenceReport', \
"""
   character(len=MAXLEN_MESSAGE) function differenceReport(difference, tolerance)
      real, intent(in) :: difference
      real, optional, intent(in) :: tolerance
      differenceReport = '    difference: |' // trim(toString(difference)) // &
      & '| > tolerance:' // trim(toString(tolerance))
   end function differenceReport
""")
    runit.setDeclaration(CodeUtilities.declaration(runit.getName(),'public '+runit.getName()))
    return runit

def makeCompareElements():
    runit = CodeUtilities.routineUnit('compareElements', \
"""
      subroutine compareElements(expected, found, at)
         real, intent(in) :: expected
         real, intent(in) :: found
         integer, intent(in) :: at
         
         if (expected /= found) then
            call throwDifferentValues(expected, found, at)
         end if

      end subroutine compareElements
""")
#    runit.clearDeclarations() # or set to public?
    runit.setDeclaration(CodeUtilities.declaration(runit.getName(),'public '+runit.getName()))
    return runit

def makeThrowDifferentValues():
    runit = CodeUtilities.routineUnit('throwDifferentValues', \
"""
      subroutine throwDifferentValues( &
      & expected, found, at, location, tolerance)
         real, intent(in) :: expected
         real, intent(in) :: found
         integer, intent(in) :: at
         type (SourceLocation), optional, intent(in) :: location
         real, optional, intent(in) :: tolerance
         real :: tolerance_
         character(len=MAXLEN_SHAPE) :: locationInArray

         if(present(tolerance))then
            tolerance_ = tolerance
         else
            tolerance_ = 0.0
         end if

         write(locationInArray,'("[",i0,"]")') at

         call throw( &
              & trim(valuesReport(expected, found)) // &
              & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
              & '; first difference at element <'//trim(toString(at))//'>.', &
              & location = location &
              )
      end subroutine throwDifferentValues
""")
    runit.setDeclaration(CodeUtilities.declaration(runit.getName(),'public '+runit.getName()))
    return runit


def makeThrowDifferentValuesString():
    runit = CodeUtilities.routineUnit('throwDifferentValuesString', \
"""
      subroutine throwDifferentValuesString(expected, found, at, location, tolerance)
         real, intent(in) :: expected
         real, intent(in) :: found
         character(len=*), intent(in) :: at
         type (SourceLocation), optional, intent(in) :: location
         real, optional, intent(in) :: tolerance
         real :: tolerance_

         if(present(tolerance))then
            tolerance_ = tolerance
         else
            tolerance_ = 0.0
         end if

         call throw( &
              & trim(valuesReport(expected, found)) // &
              & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
              & ';  first difference at element <'//trim(at)//'>.', &
              & location = location &
              )
      end subroutine throwDifferentValuesString
""")
    runit.setDeclaration(CodeUtilities.declaration(runit.getName(),'public '+runit.getName()))
    return runit

def makeModule0():
    mod0 = CodeUtilities.module('test_mod_0')
    return mod0

def makeModuleVectorNorm():
    mod1 = CodeUtilities.module('vector_norm_test')
    VectorNormInterface = CodeUtilities.interfaceBlock('vector_norm')
    map(VectorNormInterface.addRoutineUnit,[VECTOR_NORM(i) for i in range(6)])
    mod1.addInterfaceBlock(VectorNormInterface)
    return mod1

def testMakeModule():
    # mod = makeModule0()
    # mod = makeModuleVectorNorm()
    mod = constructModule()
    # print '\n'.join(mod.generate())
    # print mod
    print 'testMakeModule: opening    '+mod.getFileName()
    with open(mod.getFileName(),'w') as f:
        print 'testMakeModule: writing to '+mod.getFileName()
        f.write('\n'.join(mod.generate()))
        f.close()
    print 'testMakeModule: done'
    return

def declareUSES():
    return \
"""
   use Params_mod
   use AssertBasic_mod
   use Exception_mod
   use SourceLocation_mod
   use ThrowFundamentalTypes_mod, only : throwNonConformable
   use StringUtilities_mod
!   use AssertReal_mod, only : differenceReport, valuesReport
"""

def declareDISCIPLINE():
    return \
"""
   implicit none
   private
"""

def declareEXPORTS():
    return \
"""
   public :: assertEqual
   public :: vectorNorm
   public :: isWithinTolerance
 
   public :: L_INFINITY_NORM
   public :: L1_NORM
   public :: L2_NORM

!   public :: valuesReport
!   public :: differenceReport
"""

def declareEXPORTS_PARAMETERS():
    return \
"""
   integer, parameter :: L_INFINITY_NORM = 0
   integer, parameter :: L1_NORM         = 1
   integer, parameter :: L2_NORM         = 2

   integer, parameter :: MAXLEN_SHAPE = 80
"""

def makeExpectedFTypes(expectedPrecision):
    "A very application-specific mapping to construct an fType list for expected."
    retStr = 'makeExpectedFType::ERROR'
    if expectedPrecision == 'default':
        retStr='int'
    elif expectedPrecision == 32 or expectedPrecision == 64:
        retStr='real'
    return [retStr]

def makeExpectedRanks(foundRank):
    ranks = [0]
    if foundRank != 0:
        ranks.append(foundRank)
    return ranks

def makeTolerances(expectedP, foundP) :
    tol = -1
    if type(expectedP) is list :
        ep = expectedP
    else :
        ep = [expectedP]
    if type(foundP) is list :
        fp = foundP
    else:
        fp = [foundP]
    lp = []
    if not 'default' in ep :
        lp = lp + ep 
    if not 'default' in fp :
        lp = lp + fp
    if lp == [] :
        print 'tolerance error! setting lp to 64.'
        lp = [64]
    tol = max(lp)
    return [tol]

class AssertRealArrayArgument:
    def __init__(self,eft,ep,er,fft,fp,fr,tol):
        print ' ',eft,ep,er,fft,fp,fr,tol
        self.expectedFType = eft
        self.expectedPrecision = ep
        self.expectedRank = er
        self.foundFType = fft
        self.foundPrecision = fp
        self.foundRank = fr
        self.tolerance = tol
        # ArrayDescriptions
        self.expectedDescription = None
        self.foundDescription = None
        # Now set them...
        self.updateDescriptions()

    def updateDescriptions(self):
        self.expectedDescription = ArrayDescription( \
                                                     self.expectedFType, \
                                                     self.expectedPrecision, \
                                                     self.expectedRank )
        self.foundDescription = ArrayDescription( \
                                                  self.foundFType, \
                                                  self.foundPrecision, \
                                                  self.foundRank )
        return

    def getExpectedDescription(self):
        return self.expectedDescription
    def getFoundDescription(self):
        return self.foundDescription
    def getTolerance(self):
        return self.tolerance

def makeAssertRealArrays():

    AssertList = []

    # Note:  expectedPrecision <= foundPrecision
 
    # expectedFTypes -> 'int' if expectedPrecision 'default' else 'real'
    # was tolerances = [32,64], but replaced by the following:
    # tolerances = max expectedPrecisions & foundPrecisions
    expectedPrecisions = ['default',32,64]
    # expectedRanks(foundRank) -> [0,foundRank]
    # foundFTypes = ['real','complex']
    foundFTypes = ['real']
    foundPrecisions = [32,64]
    foundRanks = [0,1,2,3,4,5]

# -> foundFTypes --> adding 'complex'

# THE MAIN LOOP.
    AssertList = \
    [ \
      #test      a \
      constraintASSERTEQUAL(a.getExpectedDescription(),\
                            a.getFoundDescription(),\
                            a.getTolerance()\
                            ) \
    for a in \
    Utilities.flattened( \
               [[[[[[[ \
                       AssertRealArrayArgument(eft,ep,er,fft,fp,fr,tol) \
                       for eft in makeExpectedFTypes(ep) ]  \
                       for tol in makeTolerances(ep,fp) ] \
                       for ep in expectedPrecisions  ] \
                       for er in makeExpectedRanks(fr) ] \
                       for fft in foundFTypes ] \
                       for fp in foundPrecisions ] \
                       for fr in foundRanks ] \
                       )]
    return AssertList

def constructASSERTS():
    asserts = makeAssertRealArrays()
    return asserts

def constructDeclarations():
    "Construct declarations to be used at the beginning of the Module."
    declarations = \
        [ \
          CodeUtilities.declaration('uses',declareUSES()), \
          CodeUtilities.declaration('discipline',declareDISCIPLINE()), \
          CodeUtilities.declaration('exports',declareEXPORTS()), \
          CodeUtilities.declaration('exportsParameters',declareEXPORTS_PARAMETERS()) \
          ]
    return declarations

def constructModule0():
    "An initial test of module construction. Note that routines added through interface blocks are handled differently."
    m1 = CodeUtilities.module('AssertRealArray')
    [m1.addDeclaration(d) for d in constructDeclarations()]
    # add asserts
    AssertEqualInterfaceBlock = CodeUtilities.interfaceBlock('assertEqual')
    [AssertEqualInterfaceBlock.addRoutineUnit(r) for r in constructASSERTS()]
    m1.addInterfaceBlock(AssertEqualInterfaceBlock)
    return m1

def constructModule():
    "A main test of how to construct the module."
    m1 = CodeUtilities.module('AssertRealArrays_mod')
    m1.setFileName('AssertRealArrays.F90')
    [m1.addDeclaration(d) for d in constructDeclarations()]
    # add interface blocks (and the implementations)
    m1.addInterfaceBlock(constructVectorNormInterfaceBlock())
    m1.addInterfaceBlock(constructIsWithinToleranceInterfaceBlock())
    m1.addInterfaceBlock(constructAssertEqualInterfaceBlock())
    m1.addRoutineUnit(makeValuesReport())
    m1.addRoutineUnit(makeDifferenceReport())
    m1.addRoutineUnit(makeCompareElements())
    m1.addRoutineUnit(makeThrowDifferentValues())
    m1.addRoutineUnit(makeThrowDifferentValuesString())
    return m1

def checkMakeAssertionAndAddToModule():
    "Debugging how to add assertions to the module."
    aList = makeAssertRealArrays()
    a = aList[0]
    print 'a: ',a
    print 'decl: ',a.declaration.generate()
    print 'impl: ',a.implementation.generate()
    # Make an interface block for overloading.  Add subroutines...
    # Then add the interface block to the module.
    AssertEqualInterfaceBlock = CodeUtilities.interfaceBlock('assertEqual')
    AssertEqualInterfaceBlock.addRoutineUnit(a)
    m1 = CodeUtilities.module('AssertRealArray')
    m1.addInterfaceBlock(AssertEqualInterfaceBlock)
    print 'm1: ',m1
    print '\n'.join(m1.generate())
    return

def checkMakeAssertRealArrays():
    "A look into how to make assertions."
    AssertList = makeAssertRealArrays()
    print 'al: ',AssertList
    print 'al[0]: ',AssertList[0]
    return

def main():
    # The development path taken... Unit testing would have helped.
    # testDECLARE()
    # testEXPANDSHAPE()
    # testgenerateASSERTEQUAL()
    # testGenerateVECTOR_NORM()
    # testGenerateIsWithinTolerance()
    #-
    testMakeModule()
    return

if __name__ == "__main__":
    main()

