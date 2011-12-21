#!/usr/bin/env python
#
#------------------------------------------------------------------------------
# NASA/GSFC, Software Integration & Visualization Office, Code 610.3
#------------------------------------------------------------------------------
#
# MODULE:       DoxygenManager
#
# AUTHORS:      Joey Gurganus
# AFFLIATION:   NASA/GSFC
#
# DESCRIPTION:  This purpose is to handle the configuration file and then
#               execute the Doxygen tool to generate the Application 
#               Programming Interface (API) documentation via the HTML  
#               and PDF files.
#
# REVISION HISTORY:
#              25 Jun 2010 - Initial version
#
#------------------------------------------------------------------------------
#
import os
import sys

class DoxygenException(Exception):
    """ Implemented the pFUnit's exception for its message. """
    def _get_message(self, message):
        return self._message
    def _set_message(self, message):
        self._message = message
    message = property(_get_message, _set_message)


class DoxygenManager(object):
    """ Generates the Doxygen for pFUnit's API documentation including
        html and latex.
    """

    DOXYGEN     = 'doxygen'
    TEMP_FILE   = DOXYGEN + '.tmpl'
    CONF_FILE  =  DOXYGEN + '.conf'
    PFUNIT_HOME = '<PFUNIT_HOME>'
    DOT_PATH    = '<DOT_PATH>'
    PFUNIT      = '/pFUnit/'
    NO_FOUND    = -1
    MAC_DOXYGEN = '/Applications/Doxygen.app/Contents/Resources'
    MAC_DOT     = '/usr/local/graphviz-2.14/bin'
    DEBUG       = 0               # DEBUG indicator (0 - off and 1 - on)


    def __init__(self,tempFile=TEMP_FILE):
        """ Intializes its location and dot path for its path environment. 

            Args:
                tempFile : template file that is used for being converted to 
                           the configuration file.
        """
        self.__setLocation(tempFile)
        self.__setDotPath()


    def __setLocation(self,tempFile):
        """ Sets up the path environment including its current working 
            directory and configuration filename.

            Args:
                tempFile : template file that is used for being converted to 
                           the configuration file.

            Raises:
                DoxygenException:  Current directory is not found
        """ 
        cwd = os.getcwd()
        self.tempFile = os.path.join(cwd, tempFile)
        self.configFile = os.path.join(cwd,self.CONF_FILE)
        rightFound = cwd.rfind(self.PFUNIT)
        if rightFound == self.NO_FOUND:
            message = "'pFUnit' is not found in the current directory"
            raise DoxygenException(message)
        length = rightFound + len(self.PFUNIT) - 1
        self.pFUnitHome = cwd[:length]


    def __setDotPath(self):
        """ Sets up the DOT PATH environment for Doxygen's configuration file. 
        """
        self.dotPath = ''
        if self.isMacOSX():
            self.dotPath = self.MAC_DOT 


    def handleFile(self):
        """ Handles the path enviromment and template/configuration file. """
        self.__checkPathAndFile()
        self.__performFile()


    def __checkPathAndFile(self):
        """ Check validity on the template file and its pFUnit's home directory.

            Raises:
                DoxygenException:   Non-existing file and directory.
        """ 
        if not os.path.exists(self.tempFile):
            message = "The file '" + self.tempFile + "' doesn't exist!"
            raise DoxygenException(message)
        if not os.path.isdir(self.pFUnitHome):
            message = "The path '" + self.pFUnitHome+ "' doesn't exist!"
            raise DoxygenException(message)


    def __performFile(self):
        """ Performs to convert the template file to the configuration file.
        """ 
        lines = file(self.tempFile).readlines()
        newLines = []
        for line in lines:
            if line.find(self.PFUNIT_HOME) != self.NO_FOUND:
                newLines.append(line.replace(self.PFUNIT_HOME, self.pFUnitHome))
            elif line.find(self.DOT_PATH) != self.NO_FOUND:
                newLines.append(line.replace(self.DOT_PATH, self.dotPath))
            else: 
                newLines.append(line)
        self.__writeFile(newLines) 


    def __writeFile(self,lines):
        """ Writes the configuration file. 

            Args:
                lines = collection of lines from reading the template file.

            Raises:
                IOError:           Not able to write file
                DoxygenException:  Failure of writing the file
        """
        try:
            newFile = open(self.configFile,'w')
            newFile.writelines(lines)
            newFile.close()
        except IOError, ioe:
            message = "Failure of writing the file: %s" % self.configFile
            raise DoxygenException(message)
 

    def executeCommand(self,command,output=False):
        """ Executes the system command by the given command.

            Args:
                command: input string of system command
                output:  display the output from the system command
                         (default: no output)

            Returns:
               Returns the status of the system command.
        """
        if not output:
            from commands import getstatusoutput 
            status = getstatusoutput(command)[0]
        else:
           status = os.system(command)
        if self.DEBUG:
            print "status of the command '%s': %d" % (command,status)
        return (status == 0)


    def setPath(self):
        """ Set the path environment for MacOS X. """
        newPath = ''
        if self.isMacOSX():
            newPath = self.MAC_DOXYGEN
            oldPath = os.environ['PATH']
            newPath += os.path.pathsep + oldPath
            os.environ['PATH'] = newPath


    def isMacOSX(self):
        """ Determines if it is MacOS X. """ 
        return (os.uname()[0].lower() == "darwin")

 
    def run(self):
        """ Determines the Doxygen available and then generate the 
            pFunit API documentation including html and latex

            Raises:
                DoxygenException:   Failure of executing the Doxygen 
        """ 
        sysCommand = "which " + self.DOXYGEN
        if not self.executeCommand(sysCommand): 
            self.setPath()
            if not self.executeCommand(sysCommand): 
                message = "Doxygen is not available in this system"
                raise DoxygenException(message)
            # end-if
        # end-if
        sysCommand = "%s %s" % (self.DOXYGEN, self.configFile)
        if not self.executeCommand(sysCommand,output=True): 
            message = "Generating Doxygen is not successful!"
            raise DoxygenException(message)

 
##################   Main program ##################
if __name__ == "__main__":
    STARS = '*'*80 
    try:
        dManager = DoxygenManager()
        dManager.handleFile()
        dManager.run()
    except DoxygenException, de:
        print "%s\nERROR: %s\n%s" % (STARS, de, STARS)
    except Exception, e:
        print "%s\nUNKNOWN ERROR: %s\n%s" % (STARS, e, STARS)
