#!/usr/bin/env python
# For python 2.6-2.7
from __future__ import print_function
# For python2.5
# from __future__ import with_statement

# 
# Generate AssertRealArrays.F90, which provides assertEqual for arrays.
# 
# Usage:  ./GenerateRealArrayNewSignature.py
#
# M. Rilee
#    Initial: 2013-0304
# 
#    2013-0814:  Added default r64 to call from assertEqual_w/o_tol to internal proc.
#                Added logical to makeExpectedFTypes - but not for prime time.
# 

##### utility code #####

from Utilities import *
from CodeUtilities import *
import textwrap
import random
import copy

##### begin generation code #####


### Restrictions on types and type combinations.

def dr_TolAllowedPrecisions(t,pFound='64') :
    "returns a list of strings corresponding to the precisions 'tolerance' may take on. \
    dr_ refers to 'Difference Report.'  Please see the DifferenceReport routines."
    allowed = []
    if t == 'logical' :
        allowed = []
    elif t == 'integer' :
        allowed = ['32','64']
    elif t == 'real' or 'complex' :
        if pFound == '32' :
            allowed = ['32']
        elif pFound == '64' :
            allowed = ['32','64']
        else :
            raise ValueError("dr_TolAllowedPrecisions: Bad value of pFound.")
    return allowed

def dr_TolAllowedPrecisions_orig(t,pFound='64') :
    "returns a list of strings corresponding to the precisions 'tolerance' may take on. \
    dr_ refers to 'Difference Report.'  Please see the DifferenceReport routines."
    allowed = []
    if t == 'logical' :
        allowed = []
    elif t == 'integer' :
        allowed = ['64']
    else:
        allowed = ['32','64']
    return allowed

def allowedPrecisions(t,pFound='64') :
    "returns a list of strings corresponding to the precisions 'expected' may take on."
    allowed = []
    if t == 'logical' :
        allowed = []
    elif t == 'integer' :
        allowed = ['default']
    elif t == 'real' or 'complex' :
        if pFound == '32' :
            allowed = ['32']
        elif pFound == '64' : 
            allowed = ['32','64']
    return allowed

def allowedExpected(tFound) :
    # allowed = []
    if tFound in 'integer' :
        allowed = []
    elif tFound in 'integer' :
        allowed = ['integer']
    elif tFound == 'real' :
        allowed = ['integer','real']
    elif tFound == 'complex' :
        allowed = ['integer','real','complex']
    else :
        allowed = []
    return allowed

#### Type coercions

def coerceReal(x,kind='r32') :
    if kind == 'ckDefault' :
        kind = 'r32'
    return 'real('+x+',kind='+kind+')'

def coerceComplex(x,kind='c32') :
    if kind == 'ckDefault' :
        kind = 'c32'
    return 'cmplx('+x+',kind='+kind+')'

def coerceKind(x,kind='ckDefault',t='real'):
    coerceStr = x
    if t == 'real' :
        coerceStr = coerceReal(x,kind=kind)
    elif t == 'complex' :
        coerceStr = coerceComplex(x,kind=kind)
    elif t == 'integer':
        coerceStr = coerceReal(x,kind=kind)
    else:
        coerceStr = 'coerceKind: ERROR - t = '+t+', kind = '+kind+', x = '+x
    return coerceStr

####

def tolDECLARE(tolerance,descr,opts=', optional, intent(in)',name='tolerance'):
    retStr = ''
    if tolerance == 0 :
        retStr = DECLARE(name,descr.FTYPE(),descr.KIND(),0,opts=opts)
    else:
        retStr = """real(kind=r"""+str(tolerance)+""")"""+opts+""" :: """+name
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

    declareExpected = \
"     " + expectedDescr.DECLARE('expected') + "\n"
    declareFound = \
"     " + foundDescr.DECLARE('found') + "\n"
    declareTolerance = \
"     " + tolDECLARE(tolerance,foundDescr,opts=', intent(in)')
    declareTolerance_orig = \
"     " + tolDECLARE(tolerance,foundDescr,opts=', optional, intent(in)')

    toleranceKind = KINDATTRIBUTE0(foundDescr.FTYPE(),foundDescr.KIND())

    declareExpectedScalar_expected = \
"      " + expectedDescr.DECLARESCALAR('expected') + "\n"
    declareFoundScalar_found = \
"      " + foundDescr.DECLARESCALAR('found') + "\n"

    declareExpectedScalar_expected0 = \
"      " + expectedDescr.DECLARESCALAR('expected0') + "\n"
    declareFoundScalar_found0 = \
"      " + foundDescr.DECLARESCALAR('found0') + "\n"

    # eType = expectedDescr.FTYPE(); fType=foundDescr.FTYPE()
    # ePrec = expectedDescr.KIND();  fPrec=foundDescr.KIND()
    internalSubroutineName = makeAssertEqualInternalName(expectedDescr,foundDescr,tolerance)

    if 'complex' in [foundDescr.FTYPE(),expectedDescr.FTYPE()] :
        declareDelta = \
"      " + "complex(kind=kind(found)) :: delta"+foundDescr.EXPANDSHAPE('found') +"\n"+ \
"      " + "complex(kind=kind(found)) :: delta1" +"\n"
    else:
        declareDelta = \
"      " + "real(kind=kind(found)) :: delta"+foundDescr.EXPANDSHAPE('found') +"\n"+ \
"      " + "real(kind=kind(found)) :: delta1" +"\n"

# Need to handle scalar case...
    foundFirstElt = DIMS_SET([1]*foundDescr.RANK())
    expectedFirstElt = DIMS_SET([1]*expectedDescr.RANK())
    
    retString = \
        commentPreambleString + \
"""
   subroutine """+subroutineName+"""( &
   &  expected, found, tolerance, message, location )
! was tolerance, message -- need to propagate changes... e.g. to test files
     implicit none\n""" + \
     declareExpected + \
     declareFound + \
     declareTolerance + """
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

! Tolerance is now not optional.
!     if(present(tolerance)) then
        tolerance_ = tolerance
!     else
!        tolerance_ = real(0."""+ifElseString(toleranceKind,', '+toleranceKind,'')+""")
!     end if

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

    call assertSameShape(shape(expected),shape(found), message=message_, location=location_)
    if (anyExceptions()) return

! Next allow call to here...
!mlr-NextStep-Begin
     call """+internalSubroutineName+"""(&
     &  expected, shape(expected), found, shape(found), &
     &  tolerance_, message_, location_, EQP )
!mlr-NextStep-End
     
   end subroutine
"""

    declareTolerance_ = \
"     " + tolDECLARE(tolerance,foundDescr,opts='',name='tolerance_')

    retString += \
        commentPreambleString + \
"""
   subroutine """+subroutineName+'_WithoutTolerance'+"""( &
   &  expected, found, message, location )
     implicit none\n""" + \
     declareExpected + \
     declareFound + """
!     character(len=*), intent(in) :: message  ! not used yet!
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     call """+subroutineName+"""(&
   &   expected, found, &
   &   tolerance=real(0."""+ifElseString(toleranceKind,', '+toleranceKind,', '+KINDATTRIBUTE0('real',64))+"""), &
   &   message=message, location=location )
     
   end subroutine
"""

    return retString

def makeAssertEqualInternalName(eDescr,fDescr,tolerance):
    eType=eDescr.FTYPE(); fType=fDescr.FTYPE()
    ePrec=eDescr.KIND();  fPrec=fDescr.KIND()
    eRank=min(eDescr.RANK(),1)
    fRank=min(fDescr.RANK(),1)
    subroutineName = \
        'assertEqual' + \
        '_e'+ str(eRank) +'_'+eType+str(ePrec)+ \
        '_f'+ str(fRank) +'_'+fType+str(fPrec)+ \
        '_tol'+str(tolerance)+'_'
    return subroutineName

def makeAssertEqualInternal_type(eDescr,fDescr,tolerance):
    eType=eDescr.FTYPE(); fType=fDescr.FTYPE()
    ePrec=eDescr.KIND();  fPrec=fDescr.KIND()
    eRank=min(eDescr.RANK(),1)
    fRank=min(fDescr.RANK(),1)

    subroutineName = makeAssertEqualInternalName(eDescr,fDescr,tolerance)

    if(eRank != 0):
        expected_i = 'expected(i)'
        eOpts = ", dimension(product(eShape))"
    else:
        expected_i = 'expected'
        eOpts = ""
        
    if(fRank != 0):
        found_i = 'found(i)'
        fOpts = ", dimension(product(fShape))"
    else:
        found_i = 'found'
        fOpts = ""

    if(eRank != 0 or fRank != 0):
        all_i = "all"
    else:
        all_i = ""

    expectedDeclaration = \
"    " + DECLARE('expected',eType,ePrec,0,\
                                  opts=eOpts+', intent(in)') + "\n" + \
"    " + DECLARE('expected_',eType,ePrec,0,\
                                  opts='') + "\n" 

    foundDeclaration = \
"    " + DECLARE('found',fType,fPrec,0,\
                               opts=fOpts+', intent(in)') + "\n" + \
"    " + DECLARE('found_',fType,fPrec,0,\
                               opts= '' ) + "\n" + \
"    " + DECLARE('delta1',fType,fPrec,0,\
                               opts= '' ) + "\n"
    
    toleranceDeclaration = \
ifElseString(tolerance == 0,\
"""
    real(kind=kind(found)) :: tolerance
""", \
"""
    real(kind=r"""+str(tolerance)+"""), intent(in) :: tolerance
""" ) 

# Start to define routine...
    retStr = """
    subroutine """+subroutineName+"""( &
    & expected,eShape,found,fShape,tolerance,message,location, &
    & comparison )""" + \
"""
    use Params_mod
    use Exception_mod
    use StringConversionUtilities_mod
    use ThrowFundamentalTypes_mod, only : locationFormat
    implicit none
    integer, intent(in), dimension(:) :: eShape, fShape
    character(len=*), intent(in) :: message
    type (SourceLocation), intent(in) :: location
    integer, intent(in) :: comparison
""" + \
    expectedDeclaration + \
    foundDeclaration + \
    toleranceDeclaration + """
    
! mlr 2013-0908 Note:  Perhaps have tolerance_ with a type depending on found... incl. logical or int.
    real(kind=kind(tolerance)) :: tolerance_
!---    real(kind=kind(expected)) :: expected_
!---    real(kind=kind(found)) :: found_
    integer :: i,m,ir
    logical OK
    integer, dimension(size(fShape)) :: iLocation
    character(len=MAXLEN_SHAPE) :: locationInArray

    ! Return immediately if the two are precisely equal.
    ! This is necessary to deal with identical infinities, which cannot be
    ! subtracted.
    if (""" + all_i + """(expected == found)) return

! MLR: The following just might work...
    tolerance_ = tolerance

! fType != 'complex' = """ + str(fType != 'complex') + """

! Note:  Could assert size(expected) = size(found) and fShape = eShape...

!    print *,'0800 ',product(fShape),fShape
    m = product(fShape)
    i = 0
    OK = .true.
    
! Note:  Comparison occurs here.  Could use isWithinTolerance or other comparison function.
! mlr 2013-0908 Other comparisons:  tolerance-less integer comparison... logical...
    if( m > 0 )then
       do while ( i < m .and. OK )
         i = i + 1

         delta1 = """ + expected_i + """-""" + found_i + """

         select case (comparison)
            case (EQP)
              OK = &
              &  isWithinTolerance( &
              &    delta1, &
              &    real(tolerance_,kind=r64), &
              &    L_INFINITY_NORM )
            case (NEQP)
              OK = &
              &  .not. &
              &  isWithinTolerance( &
              &    delta1, &
              &    real(tolerance_,kind=r64), &
              &    L_INFINITY_NORM ) """ + \
ifElseString(fType != 'complex', \
"""
            case (GTP)
              OK = delta1 .gt. 0
            case (GEP)
              OK = delta1 .ge. 0
            case (LTP)
              OK = delta1 .lt. 0
            case (LEP)
              OK = delta1 .le. 0 """,'') + \
"""
            case (RELEQP)
              if ( abs("""+expected_i+""") > 0 ) then
                 OK = &
              &  isWithinTolerance( &
              &    delta1 / """+expected_i+""", &
              &    real(tolerance_,kind=r64), &
              &    L_INFINITY_NORM )
              else
                 OK = &
              &  isWithinTolerance( &
              &    delta1, &
              &    real(tolerance_,kind=r64), &
              &    L_INFINITY_NORM )
              end if
            case default
              print *,'select-error-1'
         end select
         
!         OK = .not. ( expected(i) /= found(i) )
!         OK = .not. ( """+expected_i+""" /= """+found_i+""" )
       end do
    else
!         i = 1
         delta1 = """ + expected_i + """-""" + found_i + """    

         select case (comparison)
            case (EQP)
              OK = &
              &  isWithinTolerance( &
              &    delta1, &
              &    real(tolerance_,kind=r64), &
              &    L_INFINITY_NORM )
            case (NEQP)
              OK = &
              &  .not. &
              &  isWithinTolerance( &
              &    delta1, &
              &    real(tolerance_,kind=r64), &
              &    L_INFINITY_NORM ) """ + \
ifElseString(fType != 'complex', \
"""
            case (GTP)
              OK = delta1 .gt. 0
            case (GEP)
              OK = delta1 .ge. 0
            case (LTP)
              OK = delta1 .lt. 0
            case (LEP)
              OK = delta1 .le. 0 """,'') + \
"""
            case (RELEQP)
              if ( abs("""+expected_i+""") > 0 ) then
                 OK = &
              &  isWithinTolerance( &
              &    delta1 / """+expected_i+""", &
              &    real(tolerance_,kind=r64), &
              &    L_INFINITY_NORM )
              else
                 OK = &
              &  isWithinTolerance( &
              &    delta1, &
              &    real(tolerance_,kind=r64), &
              &    L_INFINITY_NORM )
              end if
            case default
              print *,'select-error-2'
         end select

!         OK = &
!         &  isWithinTolerance( &
!         &    delta1, &
!         &    real(tolerance_,kind=r64), &
!         &    L_INFINITY_NORM )
         
!         OK = .not. ( """+expected_i+""" /= """+found_i+""" )
    end if

    if( .not. OK )then

    ! Save the FirstBad...
    expected_ = """+expected_i+"""
    found_    = """+found_i+"""

!    if( m > 0 )then
    if( size(fshape) > 0 ) then

    i = i - 1
    do ir = 1,size(fShape)
      iLocation(ir) = mod(i,fShape(ir)) + 1
      i = i / fShape(ir)
    end do

!    print *,'0998 ',m
!    print *,'0999 ',size(fShape)
!    print *,'1000 ',iLocation
    write(locationInArray,locationFormat(iLocation)) iLocation

    else

    write(locationInArray,*) '[1]'

    end if

! Scalar
! Note use of abs

    select case (comparison)
    case (EQP)
       call throw( &
       & appendWithSpace(message, &
       & trim(valuesReport(expected_,found_)) // &
       & '; '//trim(differenceReport(abs(found_ - expected_), tolerance_)) // &
       & unlessScalar(fShape,';  first difference at element '//trim(locationInArray))//'.'), &
       & location = location &
       )
    case (NEQP)
       call throw( &
       & appendWithSpace(message, &
       & 'NOT '//trim(valuesReport(expected_,found_)) // &
       & '; '//trim(differenceReport(abs(found_ - expected_), tolerance_)) // &
       & unlessScalar(fShape,';  first difference at element '//trim(locationInArray))//'.'), &
       & location = location &
       ) """ + \
ifElseString(fType != 'complex', \
"""
    case (GTP)
       call throw( &
       & appendWithSpace(message, &
       & trim(valuesReport(expected_,found_, &
       &   ePrefix='expected', &
       &   fPrefix='to be greater than:')) // &       
       & unlessScalar(fShape,';  first difference at element '//trim(locationInArray))//'.'), &
       & location = location &
       )
    case (GEP)
       call throw( &
       & appendWithSpace(message, &
       & trim(valuesReport(expected_,found_, &
       &   ePrefix='expected', &
       &   fPrefix='to be greater than or equal to:')) // &       
       & unlessScalar(fShape,';  first difference at element '//trim(locationInArray))//'.'), &
       & location = location &
       )
    case (LTP)
       call throw( &
       & appendWithSpace(message, &
       & trim(valuesReport(expected_,found_, &
       &   ePrefix='expected', &
       &   fPrefix='to be less than:')) // &       
       & unlessScalar(fShape,';  first difference at element '//trim(locationInArray))//'.'), &
       & location = location &
       )
    case (LEP)
       call throw( &
       & appendWithSpace(message, &
       & trim(valuesReport(expected_,found_, &
       &   ePrefix='expected', &
       &   fPrefix='to be less than or equal to:')) // &       
       & unlessScalar(fShape,';  first difference at element '//trim(locationInArray))//'.'), &
       & location = location &
       ) """,'') + \
"""
    case (RELEQP)    
       call throw( &
       & appendWithSpace(message, &
       & unlessScalar(fShape,';  first difference at element '//trim(locationInArray))//'.'), &
       & location = location &
       )
    case default
       print *,appendWithSpace(message,'select-error-3')
    end select

    end if

    end subroutine """+subroutineName+"""

"""

    runit = routineUnit(subroutineName,retStr)
    
    
    return runit
    

class constraintASSERTEQUAL(routineUnit):
    "Defines the comparison code as a routineUnit so that it can be \
    used by the module generation code.  These declarations are used \
    to construct interface blocks as well as the routines themeselves."
    def __init__(self, expectedDescr, foundDescr, tolerance):
        self.expectedDescr = expectedDescr
        self.foundDescr = foundDescr
        self.name = makeSubroutineName( \
                                        expectedDescr.NAME(), \
                                        foundDescr.NAME(), \
                                        str(tolerance) )
        #! bad... too simple...
        ## Add in the extra module procedures... If needed...
        self.setDeclaration(declaration(self.name,self.name))
        ## Kluge.  Need to make makeSubroutineNames and load the extra interface entries there.
        self.name1 = self.name+'_WithoutTolerance'
        self.addDeclaration(declaration(self.name1,self.name1))

        ## If you need another kind of code generator, perhaps
        ## conditioned on eDesc., fDesc., or tol, then that logic
        ## would go here... E.g. to implement assertEqual(Logical(...))
        ##
        ## This next line actually generates the text of the code.
        self.setImplementation(generateASSERTEQUAL(expectedDescr, \
                                                   foundDescr, \
                                                   tolerance ))
        self.tolerance = tolerance
        return

def constructAssertEqualInterfaceBlock(foundFTypes=['real']):
    AssertEqualInterfaceBlock = interfaceBlock('assertEqual')
    # Construct asserts generates the combinations based on what is passed in here.
    [AssertEqualInterfaceBlock.addRoutineUnit(r) for r in constructASSERTS(foundFTypes=foundFTypes)]
    return AssertEqualInterfaceBlock

def VECTOR_NORM_NAME(rank,fType='real',precision=64):
    return """vectorNorm_"""+str(rank)+"D"+"_"+fType+str(precision)

def vnSQRT(x,fType,precision):
    retStr = ''
    if fType == 'integer' :
        retStr = 'sqrt(real('+x+',kind=r64))'
    else :
        retStr = 'sqrt('+x+')'
    return retStr

def generateVECTOR_NORM(rank,fType='real',precision=64):
    subroutineName = VECTOR_NORM_NAME(rank,fType=fType,precision=precision)
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
    """+DECLARE('x',fType,precision,rank,opts=', intent(in)')+"""
    integer :: norm
! mlr 2013-0908 Maybe we change the range of VECTOR_NORM to include integer & logical.
    real (kind=r64) :: y
""" + ifElseString(rank == 0, \
"""
    y = abs(x) ! independent of norm for rank=0 (scalar) case.
""", \
"""
! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
!       y = sqrt(sum(x*"""+CONJG('x',fType)+"""))
       y = """ + vnSQRT("""sum(x*"""+CONJG('x',fType)+""")""",fType,precision) + """
    end select
""") + \
"""
  end function """ + subroutineName + """
"""
    return retstr

class VECTOR_NORM(routineUnit):
    def __init__(self,rank,fType='real',precision=32):
        self.rank = rank
        self.fType = fType
        self.precision = precision
        self.name = VECTOR_NORM_NAME(rank,fType=self.fType,precision=self.precision)
        self.declaration = declaration(self.name,self.name)
        self.declarations = [self.declaration]
        self.implementation \
            = implementation( \
                self.name, \
                generateVECTOR_NORM(rank,fType=self.fType,precision=self.precision))
        return

def constructVectorNormInterfaceBlock():
    VectorNormInterface = interfaceBlock('vectorNorm')
    list(map(VectorNormInterface.addRoutineUnit,
             flattened( [[VECTOR_NORM(i,fType=t,precision=p) for i in range(6) \
                          for p in allowedPrecisions(t) ] \
                         for t in ['real','complex','integer']
                     ])))
    return VectorNormInterface

def constructDifferenceReportInterfaceBlock():
    DifferenceReportInterface = interfaceBlock('differenceReport')
    list(map(DifferenceReportInterface.addRoutineUnit,
             flattened( [[[makeDifferenceReport_type(t=t,p=p,tol=tol) \
                           for tol in dr_TolAllowedPrecisions(t) ]
                          for p in allowedPrecisions(t) ]
                         for t in ['integer','real','complex'] \
                     ])))
    return DifferenceReportInterface

def constructValuesReportInterfaceBlock():
    ValuesReportInterface = interfaceBlock('valuesReport')
    list(map(ValuesReportInterface.addRoutineUnit,
             flattened( [[[[makeValuesReport_type(te=te,tf=tf,pe=pe,pf=pf) \
                            for pe in allowedPrecisions(te,pFound=pf) ] \
                           for pf in allowedPrecisions(tf) ] \
                          for te in allowedExpected(tf) ] \
                         for tf in ['integer','real','complex'] \
                     ])))
    return ValuesReportInterface

# Scalar args?
def constructAssertEqualInternalInterfaceBlock():
    AssertEqualInternalInterface = interfaceBlock('assertEqual_internal')
    list(map(AssertEqualInternalInterface.addRoutineUnit,
             [makeAssertEqualInternal_type(a.getExpectedDescription(),
                                           a.getFoundDescription(),
                                           a.getTolerance()
                                       )
              for a in
              flattened(
                  [[[[[[[[AssertRealArrayArgument(te,pe,re,tf,pf,rf,tol)
                          for re in [0,1] ]
                         for rf in [0,1] ]
                        for tol in makeTolerances(pe,pf) ]
                       for pe in allowedPrecisions(te,pFound=pf) ]
                      for pf in allowedPrecisions(tf) ]
                     for te in allowedExpected(tf) ]
                    for tf in ['integer','real','complex']
                ]])]))
    return AssertEqualInternalInterface

def isWithinToleranceName(rank,fType='real',precision=64):
    return """isWithinTolerance_"""+str(rank)+"""D"""+"_"+fType+str(precision)

def generateIsWithinTolerance(rank,fType='real',precision=64):
    "Generate the code for the comparison function. Calls \
    vectorNorm..."
    subroutineName = isWithinToleranceName(rank,fType=fType,precision=precision)
    dimStr = DIMS(rank)

    declareKind = ''
    if fType == 'integer' :
        declareKind = ''
    elif fType == 'real' :
        declareKind = '(kind=r'+str(precision)+')'
    elif fType == 'complex' :
        declareKind = '(kind=c'+str(precision)+')'
    else:
        print('isWithinToleranceTypeError')
        
    retstr = \
"""
   logical function """+subroutineName+"""(x, tolerance, norm)
!     """+fType+""" (kind=r"""+str(precision)+"""), intent(in) :: x"""+dimStr+"""
     """+fType+declareKind+""", intent(in) :: x"""+dimStr+"""
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     """+subroutineName+""" = ( vectorNorm(x, norm) <= tolerance )

   end function """+subroutineName+"""
"""
    return retstr

class IsWithinTolerance(routineUnit):
    "A routineUnit specialized to the isWithinTolerance comparison function."
    def __init__(self,rank,fType='real',precision=64):
        self.rank = rank
        self.precision = precision
        self.name = isWithinToleranceName(rank,fType=fType,precision=precision)
        self.fType = fType
        self.declaration = declaration(self.name, self.name)
        self.declarations = [self.declaration]
        self.implementation \
            = implementation(self.name, \
                                           generateIsWithinTolerance(self.rank, \
                                                                     fType=self.fType, \
                                                                     precision=self.precision))
        return

def constructIsWithinToleranceInterfaceBlock():
    "For the comparison function, make an interface block and \
    implementation for inclusion into a module."
    iwt_InterfaceBlock = interfaceBlock('isWithinTolerance')
    list(map(iwt_InterfaceBlock.addRoutineUnit,
             flattened(
                 [[IsWithinTolerance(i,fType=t,precision=p)
                   for i in range(6)
                   for p in allowedPrecisions(t) ]
                  for t in ['real','complex','integer']
              ])))
    return iwt_InterfaceBlock


### Currently Active ###
def makeValuesReport_type(te='real',tf='real',pe='64',pf='64'):
    expectedKind = reportKind(te,pe)
    foundKind =    reportKind(tf,pf)
    mxType = maxType(te,tf) 
    mxPrec = maxPrecision(pe,pf)
    coercedExpected = coerceKind('expected',t=mxType)
    coercedFound    = coerceKind('found',t=mxType)
    runit = routineUnit('valuesReport_'+te+tf+pe+pf, \
"""
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_"""+te+tf+pe+pf+""" &
      & (expected,found,ePrefix,ePostfix,fPrefix,fPostfix) &
      & result(valuesReport)
        """+te+expectedKind+""", intent(in) :: expected
        """+tf+foundKind+""", intent(in) :: found
        character(len=*), optional, intent(in) :: &
      &   ePrefix, ePostfix, fPrefix, fPostfix
        character(len=MAXLEN_MESSAGE) :: &
      &   ePrefix_, ePostfix_, fPrefix_, fPostfix_

      if( .not.present(ePrefix) ) then
         ePrefix_ = 'expected:'
      else
         ePrefix_ = ePrefix
      end if
      if( .not.present(ePostfix) ) then
         ePostfix_ = ''
      else
         ePostfix_ = ePostfix
      end if
      if( .not.present(fPrefix) ) then
         fPrefix_ = 'but found:'
      else
         fPrefix_ = fPrefix
      end if
      if( .not.present(fPostfix) ) then
         fPostfix_ = ''
      else
         fPostfix_ = fPostfix
      end if

! Note: removed '<.>'
        valuesReport = &
      & trim(ePrefix_)//' '// trim(toString("""+coercedExpected+""")) // &
      & trim(ePostfix_)//' '// &
      & trim(fPrefix_)//' '//trim(toString("""+coercedFound+""")) // &
      & trim(fPostfix_)// &
      & ''
      
      end function
""")
    # runit.setDeclaration(declaration(runit.getName(),'public '+runit.getName()))
    return runit

### Currently Active ###
def makeDifferenceReport_type(t='real',p='64',tol='64'):
    if t == 'integer' :
        expectedDeclaration = " integer, intent(in) :: difference"
    else :
        expectedDeclaration = t+"""(kind=r"""+p+"""), intent(in) :: difference"""
    coercedDifference = coerceKind('difference',t=t)
    #    coercedTolerance =  coerceKind('tolerance',t=t)
    
    runit = routineUnit('differenceReport_'+t+p+tol,\
"""
    character(len=MAXLEN_MESSAGE) &
    & function differenceReport_"""+t+p+tol+"""(difference, tolerance) result(differenceReport)
     """+expectedDeclaration+"""
     real(kind=r"""+tol+"""), intent(in) :: tolerance
!     real(kind=r"""+tol+"""), optional, intent(in) :: tolerance
      differenceReport = '    difference: |' // trim(toString("""+coercedDifference+""")) // &
      & '| > tolerance:' // trim(toString("""+'tolerance'+"""))
    end function 
""")

# Don't need the following because we'll add to an interface block.
#    runit.setDeclaration(declaration(runit.getName(),'public '+runit.getName()))
    return runit

def declareUSES():
    return \
"""
   use Params_mod
   use AssertBasic_mod
   use Exception_mod
   use SourceLocation_mod
   use StringConversionUtilities_mod
"""

def declareDISCIPLINE():
    return \
"""
   implicit none
   private
"""

def declareEXPORTS(basename='AssertReal'):
    retPublic = \
"""
   public :: assertEqual
"""
    if basename == 'AssertReal' :
        retPublic = retPublic + """
   public :: vectorNorm
   public :: isWithinTolerance
 
   public :: L_INFINITY_NORM
   public :: L1_NORM
   public :: L2_NORM

   public :: valuesReport
   public :: differenceReport
"""
    return retPublic

def declareEXPORTS_PARAMETERS():
    return \
"""
   integer, parameter :: L_INFINITY_NORM = 0
   integer, parameter :: L1_NORM         = 1
   integer, parameter :: L2_NORM         = 2

   integer, parameter :: MAXLEN_SHAPE = 80
"""

#### Helper functions for constructASSERTS -- a main workhorse for generating the specific routines.

def makeExpectedFTypes(expectedPrecision,foundFType,foundFTypes=['real']):
    """A very application-specific mapping to construct an fType list
    for expected.  Make sure that if we're looking at complex that we
    do not replicate real-real comparisons."""
    retTypes = ['makeExpectedFType::ERROR']
    if 'logical' in foundFTypes :
        if foundFType == 'logical' :
            retTypes=['logical']        
    elif expectedPrecision == 'default':
        if not 'complex' in foundFTypes :
            retTypes=['integer']
        else :
            # If we're in AssertComplex and we're not duplicating reals...
            if foundFType == 'real' :
                retTypes=[]
            else :
                retTypes=['integer']
    elif expectedPrecision == 32 or expectedPrecision == 64:
        if not 'complex' in foundFTypes :
            retTypes=['real']
        else :
            if foundFType == 'real' :
                # Tom asserts that finding a real when expecting complex should be an error.
                # retTypes=['complex']
                retTypes=[]
            else :
                retTypes=['real','complex']
    return retTypes

def makeExpectedRanks(foundRank):
    ranks = [0]
    if foundRank != 0:
        ranks.append(foundRank)
    return ranks

def makeTolerances(expectedP, foundP) :
    "unless default (int) is found, collect all of the tolerances \
    found in eP and fP and return the maximum"
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
        # 2013-1022 MLR Fix this!!!
        # print('tolerance error! setting lp to 64.')
        lp = [64]
    tol = max(lp)
    return [tol]

class AssertRealArrayArgument:
    def __init__(self,eft,ep,er,fft,fp,fr,tol):
        # print(' ',eft,ep,er,fft,fp,fr,tol)
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

def makeExpectedPrecisions(foundPrecision,foundFType='real'):
    if foundFType == 'logical' :
        expectedPrecisions = ['']
    elif foundFType == 'integer' :
        expectedPrecisions = ['default']
    else :
        expectedPrecisions = ['default',32]
        if foundPrecision > 32 :
            expectedPrecisions.append(foundPrecision)
    return expectedPrecisions

def ca_MakeAllowedPrecisions(foundFType) :
    precs = []
    ap = allowedPrecisions(foundFType,pFound='64')
    for i in ap :
        if i == 'default' :
            precs = precs + [i]
        else :
            precs = precs + [int(i)]
    return precs

def constructASSERTS(foundFTypes=['real','complex'],foundRanks = [0,1,2,3,4,5]):

    AssertList = []

    # Note:  expectedPrecision <= foundPrecision
    # Note:  Need to eliminate redundancy of real asserts that can arise in AssertComplex.
    #        I.e. remove real-real comparisons when complex is available.
 
    # expectedFTypes -> 'integer' if expectedPrecision 'default' else 'real'
    # was tolerances = [32,64], but replaced by the following:
    # tolerances = max expectedPrecisions & foundPrecisions
    # expectedPrecisions = ['default',32,64] that are < foundPrecision
    # expectedRanks(foundRank) -> [0,foundRank]
    # + passed in foundFTypes = ['real','complex']
    # + passed in foundFTypes = ['real']
    # foundPrecisions = [32,64] replaced with ca_MakeAllowedPrecisions
    # + passed in foundRanks = [0,1,2,3,4,5]

# -> foundFTypes --> adding 'complex'

# THE MAIN LOOP.
# May need a special case if we don't want to construct a real-real in AssertComplex...
# Many type-kind-rank combinations are not allowed.  The allowed combinations result from 
# some options depending on others.  These are implemented in the "make..." functions listed below.
# The argument foundFType (found Fortran Type) is a key independent variable, which drives the types
# chosen for other arguments.
#
# The variable a contains the arguments for the specialized AssertEqual being generated.
# The constraintASSERTEQUAL object is the specialized routine, which is then used to
# construct the module.
#
# To change the list of asserts constructed, one can either change the logic implemented in the
# network of "make..." functions below, or one could add other routineUnits to AssertList, as long
# as it make sense to include them in the list (and interface block).
#
    AssertList = \
    [ \
      #test      a \
      constraintASSERTEQUAL(a.getExpectedDescription(),\
                            a.getFoundDescription(),\
                            a.getTolerance()\
                            ) \
    for a in \
    flattened( \
               [[[[[[[ \
                       AssertRealArrayArgument(eft,ep,er,fft,fp,fr,tol) \
                       for eft in makeExpectedFTypes(ep,fft,foundFTypes=foundFTypes) ]  \
                       for tol in makeTolerances(ep,fp) ] \
                       for ep in makeExpectedPrecisions(fp,foundFType=fft)  ] \
                       for er in makeExpectedRanks(fr) ] \
                       for fp in ca_MakeAllowedPrecisions(fft) ] \
                       for fft in foundFTypes ] \
                       for fr in foundRanks ] \
                       )]

    ## To insert by hand, one might try the following (sketch...)...
    ## a = AssertRealArrayArgument('integer','default','1','integer','default','1',0)
    ## AssertList += [MyConstraintAssertEqual(a.getExpectedDescription(),a.getFoundDescription())]
    ## Any specialization of routineUnit should work here...
    ## The code is generated when the routineUnit is instantiated, so that support code would
    ## need to be available.
    
    
    return AssertList

def constructDeclarations(basename=''):
    "Construct declarations to be used at the beginning of the Module."
    declarations = \
        [ \
          declaration('uses',declareUSES()), \
          declaration('discipline',declareDISCIPLINE()), \
          declaration('exports',declareEXPORTS(basename)), \
          declaration('exportsParameters',declareEXPORTS_PARAMETERS()) \
          ]
    return declarations

def constructModule(baseName='AssertReal',foundFTypes=['real']):
    "A main test of how to construct the module."
    m1 = module(baseName+'_mod')
    m1.setFileName(baseName+'.F90')
    [m1.addDeclaration(d) for d in constructDeclarations(basename=baseName)]
    # add interface blocks (and the implementations)
    m1.addInterfaceBlock(constructVectorNormInterfaceBlock())
    m1.addInterfaceBlock(constructIsWithinToleranceInterfaceBlock())
    m1.addInterfaceBlock(constructAssertEqualInterfaceBlock(foundFTypes=foundFTypes))
    m1.addInterfaceBlock(constructDifferenceReportInterfaceBlock())
    m1.addInterfaceBlock(constructValuesReportInterfaceBlock())
    # *in test*
    # This is where the "list of asserts" is generated.
    m1.addInterfaceBlock(constructAssertEqualInternalInterfaceBlock())
    # add individual routine units
    #    m1.addRoutineUnit(makeThrowDifferentValues()) # arg. provides a routine unit.
    return m1

def filePreamble(filename):
    return """

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  This file '"""+filename+"""' is automatically generated by
!  'GenerateRealArrayNewSignature.py'.  Changes made here will be
!  overwritten the next time that script is run.
!
!  2013-0722 MLR Michael.L.Rilee-1@nasa.gov
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

"""

def makeModuleReal():
    mod = constructModule()
    # print('\n'.join(mod.generate()))
    # print(mod)
    # print('makeModuleReal: opening    '+mod.getFileName())
    with open(mod.getFileName(),'w') as f:
        # print('makeModuleReal: writing to '+mod.getFileName())
        f.write(filePreamble(mod.getFileName()))
        f.write('\n'.join(mod.generate()))
    print('makeModuleReal: done')
    return

def makeModuleComplex():
    #
    # mod = constructModule(baseName='AssertComplex',foundFTypes=['complex'])
    #
    mod = constructModule(baseName='AssertComplex',foundFTypes=['real','complex'])
    # -- mod = constructModule(baseName='AssertComplex',foundFTypes=['complex'])
    # print('makeModuleComplex: opening    '+mod.getFileName())
    with open(mod.getFileName(),'w') as f:
        # print('makeModuleComplex: writing to '+mod.getFileName())
        f.write(filePreamble(mod.getFileName()))
        f.write('\n'.join(mod.generate()))
    print('makeModuleComplex: done')
    return

def makeModuleInteger():
    mod = constructModule(baseName='AssertInteger1',foundFTypes=['integer'])
    with open(mod.getFileName(),'w') as f:
        f.write(filePreamble(mod.getFileName()))
        f.write('\n'.join(mod.generate()))
    print('makeModuleInteger: done')
    return

# def makeModuleLogical():
#     mod = constructModule(baseName='AssertLogical1',foundFTypes=['logical'])
#     with open(mod.getFileName(),'w') as f:
#         f.write(filePreamble(mod.getFileName()))
#         f.write('\n'.join(mod.generate()))
#     print('makeModuleInteger: done')
#     return

def main():
    # Make the modules for the different types...
    #++
    makeModuleReal()
    #++
    makeModuleComplex()
    #? The following requires testing.
    makeModuleInteger()
    #? Just started...
    #- makeModuleLogical()
    return

if __name__ == "__main__":
    main()

