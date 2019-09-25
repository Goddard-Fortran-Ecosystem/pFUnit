import os
import sys
import argparse
import re


parser = argparse.ArgumentParser(prog=os.path.basename(sys.argv[0]))
parser.add_argument("-i","--infile",help="the input file containing template to be expanded")
parser.add_argument("-o","--outfile",help="the output file containing generatef Fortran code")
parser.add_argument("-r","--rank",type=int,help="rank of 'actual' parameter for template generation")

args, unknown = parser.parse_known_args()

with open (args.infile, 'r') as ifile:
    text = ifile.readlines()

if args.outfile:
    ofile = open(args.outfile, 'w')
    ofile.write("!***********************************\n")
    ofile.write("!      Generated source code        \n")
    ofile.write("!           DO NOT EDIT             \n")
    ofile.write("!***********************************\n")
    ofile.write("\n")
else:
    ofile = sys.stdout

mydict = {}
mydict['rank'] = args.rank
mydict['dims'] = '('+','.join(':'*args.rank)+')'

for line in text:
    ofile.write(line.format(**mydict))
    
