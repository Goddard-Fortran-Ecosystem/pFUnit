import os
import sys
import argparse
import re


parser = argparse.ArgumentParser(prog=os.path.basename(sys.argv[0]))
parser.add_argument("-i","--infile",help="the input file containing template to be expanded")
parser.add_argument("-o","--outfile",help="the output file containing generatef Fortran code")
parser.add_argument("-r","--rank",help="rank of 'found' paremeter for template generation")
args=parser.parse_args()

if args.infile:
    f = open(args.infile, 'r')
else:
    f = sys.stdin

    
if args.outfile: 
    ofile = open(args.outfile, 'w')
    ofile.write("!***********************************\n")
    ofile.write("!      Generated source code        \n")
    ofile.write("!           DO NOT EDIT             \n")
    ofile.write("!***********************************\n")
    ofile.write("\n")
else:
    ofile = sys.stdout



def tkr_found_list(tkr):
    if tkr['type'] == 'integer':
        types = ['integer']
    elif tkr['type'] == 'real':
        types = ['integer','real']
    else: # complex
        types = ['integer','real','complex']


class TKR:
    """Type Kind and Rank"""
    type = None
    kind = None
    rank = None
    dims = None

    def __init__(self, type, kind_value, rank):
        self.type = type.strip().lower()
        self.kind_value = kind_value.strip()

        if self.type == 'integer':
            if self.kind_value == 'default':
                self.kind = 'kind(1)'
                self.type_kind = 'Int'
            else:
                self.kind = 'INT' + self.kind_value
                self.type_kind = self.kind.capitalize()
        elif self.type == 'real':
            if self.kind_value == 'default':
                self.kind = 'kind(1.)'
                self.type_kind = 'Real'
            else:
                self.kind = 'REAL' + self.kind_value
                self.type_kind = self.kind.capitalize()

        if (rank.strip() == "rank"):
            self.rank = int(args.rank)
        else:
            self.rank = int(rank)
        if self.rank == 0:
            self.dims = ''
        else:
            self.dims = '(' + ','.join([':']*self.rank) + ')'

        self.mangle = self.type_kind + '_' + str(self.rank) + 'd'

class State:
    def __init__(self):
        self.tkr_dictionaries = {}
        self.current_tkr = ''
        self.templates = {}
        self.current_template = ''
        self.ofile = ofile

class Action:
    def match(self, line, state):
        return re.match(self.regexp, line)
    def apply(self, line, state):
        m = self.match(line, state)
        if m:
            self.action(m, line, state)
        return m

class TKR_begin(Action):
    regexp = re.compile("\s*@tkr_parameters\s*(\w*)")
    def action(self, m, line, state):
        state.current_tkr = m.group(1)
        state.tkr_dictionaries[state.current_tkr] = []

class TKR_inside(Action):
    def match(self, line, state):
        return state.current_tkr # empty string?
    def action(self, m, line, state):
        rexp = re.compile("\s*\!.*")
        if re.match(rexp, line):
            return
        inside = re.compile("\s*\[(.*)\]\s*").match(line).groups(0)[0]
        params = re.split(r',\s*(?![^()]*\))', inside)
        tkrs = [x for x in params if re.compile("\s*\(.*\)\s*").match(x)]
        condition = [x for x in params if not re.compile("\s*\(.*\)\s*").match(x)]
        elements = [(re.compile("\((.*)\)").match(x.strip()).groups(0)[0]).split(',') for x in tkrs]

        d = {}
        d['expected'] = TKR(*elements[0])
        d['found'] = TKR(*elements[1])
        if len(elements) > 2:
            d['tolerance'] = TKR(*elements[2])
        if condition:
            d['condition'] = condition[0]
        state.tkr_dictionaries[state.current_tkr].append(d)

    
class TKR_end(Action):
    regexp = re.compile("\s*@end tkr_parameters")
    def action(self, m, line, state):
        state.current_tkr = ''

def name_mangle(base, d):
    mangle = base + '_' + d['expected'].mangle + '_' + d['found'].mangle
    if 'tolerance' in d:
        mangle += d['tolerance'].mangle
    return mangle
        
class Overload(Action):
    regexp = re.compile("(\s*)@overload\((\w+),\s*(\w+)\s*\)")
    def action(self, m, line, state):
        generic_name =  m.group(2)
        indent = m.group(1)
        state.ofile.write(indent + 'interface ' + generic_name + '\n')
        for instance in state.tkr_dictionaries[m.group(3)]:
            if 'condition' in instance:
                state.ofile.write("#  " + indent + "if " + instance['condition'] + '\n')
            state.ofile.write(indent + '   module procedure ' + name_mangle(generic_name,instance) + '\n')
            if 'condition' in instance:
                state.ofile.write('#  ' + indent + 'endif\n')
        state.ofile.write(indent + 'end interface \n')


class Module(Action):
    regexp = re.compile("\s*(end)?\s*module\s+\w*\s*")
    def action(self, m, line, state):
        state.ofile.write(line.format(rank=args.rank))

class Template_begin(Action):        
    regexp = re.compile("\s*@template\s+(\w*)")
    def action(self, m, line, state):
        state.current_template = m.group(1)
        state.templates[state.current_template] = ''
        
class Template_inside(Action):        
    def match(self, line, state):
        return state.current_template
    def action(self, m, line, state):
        state.templates[state.current_template] += line

class Template_end(Action):        
    regexp = re.compile("\s*@end template")
    def action(self, m, line, state):
        state.current_template = ''
        
class Instantiate(Action):
    regexp = re.compile("\s*@instantiate\(\s*(\w*)\s*,\s*(\w*)\s*\)")
    def action(self, m, line, state):
        template_name = m.group(1)
        template =  state.templates[template_name]
        instances = state.tkr_dictionaries[m.group(2)]
        for instance in instances:
            if 'condition' in instance:
                state.ofile.write('#     if ' + instance['condition'] + '\n')
            d = dict(instance)
            d['name'] = name_mangle(template_name, d)
            state.ofile.write(template.format(**d))
            if 'condition' in d:
                state.ofile.write('#endif\n')
            state.ofile.write('\n\n')                              
        


# Note that it is important for "end" actions to be after "inside" actions    
actions = [TKR_begin(),
           TKR_end(),
           TKR_inside(),
           Overload(),
           Module(),
           Template_begin(),
           Template_end(),
           Template_inside(),
           Instantiate()
          ]
state = State()


accumulate_tkr = False
current_tkr_set = ""
for line in f:

    for a in actions:
        if a.apply(line, state):
            break
    else:
        # nothing applies => just emit the line
        state.ofile.write(line)
    

f.close()
ofile.close()

