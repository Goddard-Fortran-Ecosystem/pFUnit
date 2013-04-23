#!/usr/bin/env python

class module:
    def __init__(self, name):
        # what do we need?
        self.name = ''
        self.declarations = []
        self.implementations = []
        self.generation = []
        #
        self.name = name
        self.fileName = name+'.F90'
        return 
    def generate(self):
        generation = [ 'module '+self.name]
        generation.extend( [ i.generate() for i in self.declarations ] )
        generation.extend([ 'contains' ])
        generation.extend( [ i.generate() for i in self.implementations ] )
        generation.extend([ 'end module '+self.name])
        return generation
    def addDeclaration(self,declaration):
        # print 'adding declaration: ',declaration
        if type(declaration) is list :
            self.declarations.extend(declaration)
        else:
            self.declarations.append(declaration)
        return self
    def addImplementation(self,implementation):
        self.implementations.append(implementation)
        return self
    def addRoutineUnit(self, rUnit):
        # Might need to add more than one decl.
        self.addDeclaration(rUnit.getDeclarations())
        self.addImplementation(rUnit.getImplementation())
        return self
    def addInterfaceBlock(self, interface):
        self.addDeclaration(interface.getDeclaration())
        self.addImplementation(interface.getImplementation())
        return self
    def getName(self):
        return self.name
    def setFileName(self,fName):
        self.fileName = fName
        return self
    def getFileName(self):
        return self.fileName

class declaration:
    def __init__(self,name,simpleDeclaration):
        self.simpleDeclaration = simpleDeclaration
        self.fullDeclaration = ''
        self.name = name
        return
    def generate(self):
        return self.simpleDeclaration

class implementation:
    def __init__(self,name,source):
        self.name = name
        self.source = source
    def generate(self):
        return self.source

class routineUnit:
    def __init__(self,name,implementSource):
        self.name = name
        self.declaration = declaration(self.name, self.name) # get better later
        self.declarations = []
        self.declarations.append(self.declaration)
        self.implementation = implementation(self.name,implementSource)
        return
    def setName(self,name):
        self.name = name
        return
    def getName(self):
        return self.name
    def setDeclaration(self,declaration):
        self.declaration = declaration
        self.declarations = [self.declaration]
        return
    def addDeclaration(self,declaration):
        self.declarations.append(declaration)
        return
    def setImplementation(self,implementationSource):
        self.implementation = implementation(self.name, implementationSource)
        return
    def getDeclaration(self):
        return self.declaration
    def getDeclarations(self):
        return self.declarations
    def getImplementation(self):
        return self.implementation
    def clearDeclarations(self):
        self.declarations = []
        self.declaration = ''
        return self

class interfaceBlock:
#    name = ''
#    moduleProcedureAlternatives = []
#    moduleProcedureImplementations = []
    def __init__(self,name):
        self.name = name
        self.moduleProcedureAlternatives = []
        self.moduleProcedureImplementations = []
    def generateDeclaration(self):
        retStr = '\ninterface ' + self.name + '\n'
        if self.moduleProcedureAlternatives != [] :
            # Note that we need to treat the first line as a special case.
            retStr += \
                '\n   module procedure ' + \
                '\n   module procedure '.join(self.moduleProcedureAlternatives)
        retStr += '\n\nend interface ' + self.name + '\n'
        return declaration(self.name,retStr)
    def generateImplementation(self):
        retStr = '! interface ' + self.name + ' implementations\n'
        if self.moduleProcedureImplementations != [] :
            retStr += \
                '\n   '.join(self.moduleProcedureImplementations)
        retStr += '\n! end interface ' + self.name + ' implementations'
        return implementation(self.name,retStr)
    def addModuleProcedureAlternative(self,newName):
        self.moduleProcedureAlternatives.append(newName)
        return self
    def addRoutineUnit(self,routineUnit):
        for d in routineUnit.getDeclarations():
            self.addModuleProcedureAlternative(d.generate())
        # self.addModuleProcedureAlternative(routineUnit.getDeclaration().generate())
        self.moduleProcedureImplementations.append(routineUnit.getImplementation().generate())
        return self
    def getDeclaration(self):
        return self.generateDeclaration()
    def getImplementation(self):
        return self.generateImplementation()
    
class fortranSubroutineSignature:
    def __init__(self,name):
        self.name = name
        self.ArgumentToFType = {}
        self.ReturnFType = ''
        self.SubroutineType = 'subroutine'
    def setReturnFType(self,ReturnFType):
        self.ReturnFType = ReturnFType
        if not ReturnFType :
            self.SubroutineType = 'function'
        return self
    def addArg(self,arg,fType):
        self.ArgumentToFType[arg] = fType
        return self
    def generateInterfaceEntry(self):
        print 'generateInterfaceEntryNotImplemented'
        return
    def generateImplementationSignature(self):
        print 'generateImplementationSignatureNotImplemented'
        return
    def generateImplementationClose(self):
        print 'generateImplementationCloseNotImplemented'
        return
    

    
