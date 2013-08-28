#!/usr/bin/env python

import argparse
from pre_If import *
from pre_Repeat import *

parser = argparse.ArgumentParser(description='A preproccessor for pfunit research')

parser.add_argument('--inFile')
parser.add_argument('--outFile')
args = parser.parse_args()

if __name__ == '__main__' :
    result = pre(inFile=args.inFile, outFile=args.outFile)
    if args.inFile :
        if not args.outFile :
            print result


