#!/usr/bin/env python
# For python 2.6-2.7
from __future__ import print_function
# For python2.5
# from __future__ import with_statement

import re
import sys

includeFile="generated.inc"
cmakeIncludeFile="cmakeGenerated.inc"

generatedFiles = []
with open(includeFile,'r') as f:
   l0 = f.readline()
   lines = f.readlines()
   generatedFiles = [(re.match(r'(.*)\+\= (.*)',line)).group(2) for line in lines]

for i in generatedFiles:
   sys.stdout.write(i + ';')
       






    
    
