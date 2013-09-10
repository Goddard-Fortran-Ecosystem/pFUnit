#!/usr/bin/env python

import argparse

# from mods.pre import pre_If
# from mods.pre import pre_Repeat

import mods.pre
from mods.pre.pre_If import *
from mods.pre.pre_Repeat import *

parser = \
  argparse.ArgumentParser( \
                           description='A set of code macros to aid preproccessing and code generation for pfunit research', \
                           usage='%(prog)s -inFile INFILE [options]' \
      )

parser.add_argument('--inFile', help='The input file.')
parser.add_argument('--outFile', help='The output file. (Standard out is the default.)')
args = parser.parse_args()

if __name__ == '__main__' :
    result = pre(inFile=args.inFile, outFile=args.outFile)
    if args.inFile :
        if not args.outFile :
            print result


