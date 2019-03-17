import os
import sys
import argparse
import re


parser = argparse.ArgumentParser(prog=os.path.basename(sys.argv[0]))
parser.add_argument("-i","--infile",help="the input file containing template to be expanded")
parser.add_argument("-o","--outfile",help="the output file containing generatef Fortran code")
parser.add_argument("-r","--rank",help="rank of 'actual' parameter for template generation")

# Fortran supported kinds.  Note the value might be "-1" if a compiler does not support a given kind,
# but the constant is required to exist in ISO_FORTRAN_ENV.
parser.add_argument("-_ISO_INT8", help="value of INT8 in ISO_FORTRAN_ENV")
parser.add_argument("-_ISO_INT16", help="value of INT16 in ISO_FORTRAN_ENV")
parser.add_argument("-_ISO_INT32", help="value of INT32 in ISO_FORTRAN_ENV")
parser.add_argument("-_ISO_INT64", help="value of INT64 in ISO_FORTRAN_ENV")
parser.add_argument("-_ISO_REAL16", help="value of REAL16 in ISO_FORTRAN_ENV")
parser.add_argument("-_ISO_REAL32", help="value of REAL32 in ISO_FORTRAN_ENV")
parser.add_argument("-_ISO_REAL64", help="value of REAL64 in ISO_FORTRAN_ENV")
parser.add_argument("-_ISO_REAL80", help="value of REAL80 in ISO_FORTRAN_ENV")
parser.add_argument("-_ISO_REAL128", help="value of REAL128 in ISO_FORTRAN_ENV")
# To avoid duplicate interfaces, we need to know the kind value for default integer and real as well.
parser.add_argument("-_LOGICAL_DEFAULT_KIND", help="kind of default logical")
parser.add_argument("-_INT_DEFAULT_KIND", help="kind of default integer")
parser.add_argument("-_REAL_DEFAULT_KIND", help="kind of default real")
parser.add_argument("-_DOUBLE_DEFAULT_KIND", help="kind of default double")


args, unknown = parser.parse_known_args()

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


class TKR:
    """Type Kind and Rank"""
    type = None
    kind = None
    rank = None
    dims = None

    def __init__(self, type, kind_value, rank):
        self.type = type.strip().lower()
        self.kind_label = kind_value.strip()
        if self.type == 'logical':
            if self.kind_label == 'default':
                self.kind_value = '{_LOGICAL_DEFAULT_KIND}'.format(**vars(args))
                self.kind = 'kind(.true.)' # appears in the generated source
                self.type_kind = 'Logical' # for name mangling
        elif self.type == 'integer':
            if self.kind_label == 'default':
                self.kind_value = '{_INT_DEFAULT_KIND}'.format(**vars(args))
                self.kind = 'kind(1)' # appears in the generated source
                self.type_kind = 'Int' # for name mangling
            else:
                self.kind_value = ('{_ISO_INT' + self.kind_label +'}').format(**vars(args))
                self.kind = 'INT' + self.kind_label
                self.type_kind = self.kind.capitalize()
        elif self.type == 'real':
            if self.kind_label == 'default':
                self.kind_value = '{_REAL_DEFAULT_KIND}'.format(**vars(args))
                self.kind = 'kind(1.)' # appears in the generated source
                self.type_kind = 'Real' # for name mangling
            elif self.kind_label == 'double':
                self.kind_value = '{_DOUBLE_DEFAULT_KIND}'.format(**vars(args))
                self.kind = 'kind(1.d0)' # appears in the generated source
                self.type_kind = 'DblReal' # for name mangling
            else:
                self.kind_value = ('{_ISO_REAL' + self.kind_label +'}').format(**vars(args))
                self.kind = 'REAL' + self.kind_label
                self.type_kind = self.kind.capitalize()
        elif self.type == 'complex':
            if self.kind_label == 'default':
                self.kind_value = '{_REAL_DEFAULT_KIND}'.format(**vars(args))
                self.kind = 'kind(1.)' # appears in the generated source
                self.type_kind = 'Complex' # for name mangling
            elif self.kind_label == 'double':
                self.kind_value = '{_DOUBLE_DEFAULT_KIND}'.format(**vars(args))
                self.kind = 'kind(1.d0)' # appears in the generated source
                self.type_kind = 'DblComplex' # for name mangling
            else:
                self.kind_value = ('{_ISO_REAL' + self.kind_label +'}').format(**vars(args))
                self.kind = 'REAL' + self.kind_label
                self.type_kind = 'COMPLEX' + self.kind_label
        elif self.type == '-1':
            self.kind_value = '0'
            self.kind = '0'
            self.type_kind = 'absent'

        if (rank.strip() == "rank"):
            self.rank = int(args.rank)
        else:
            self.rank = int(rank)
        if self.rank == 0:
            self.dims = ''
        else:
            self.dims = '(' + ','.join([':']*self.rank) + ')'

        self.mangle = self.type_kind + '_' + str(self.rank) + 'd'
        if self.kind_value == 'None' or int(self.kind_value) == -1:  # unsupported kind
            self.hash = ''
        else:
            self.hash = self.type + self.kind_value + str(self.rank)
        # Need to be able access Fortran array with multidimensional index 'i'
        # in a generic manner.
        self.multi_index = ','.join(['i('+str(j)+')' for j in range(1,self.rank+1)])


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
        elements = [(re.compile("\((.*)\)").match(x.strip()).groups(0)[0]).split(',') for x in tkrs]

        hash = ''
        d = {}
        d['items'] = [TKR(*x) for x in elements]
        for tkr in d['items']:
            if tkr.hash:
                hash += tkr.hash
            else:
                return
        if hash:
            if not any([(x['hash'] == hash) for x in state.tkr_dictionaries[state.current_tkr]]):
                d['hash'] = hash
                state.tkr_dictionaries[state.current_tkr].append(d)

    
class TKR_end(Action):
    regexp = re.compile("\s*@end tkr_parameters")
    def action(self, m, line, state):
        state.current_tkr = ''

def name_mangle(base, items):
    mangle = base
    for item in items:
        mangle +=  '_' + item.mangle
    return mangle
        
class Overload(Action):
    regexp = re.compile("(\s*)@overload\((\w+),\s*(\w+)\s*\)")
    def action(self, m, line, state):
        generic_name =  m.group(2)
        indent = m.group(1)
        state.ofile.write(indent + 'interface ' + generic_name + '\n')
        for instance in state.tkr_dictionaries[m.group(3)]:
            state.ofile.write(indent + '   module procedure ' + name_mangle(generic_name,instance['items']) + '\n')
        state.ofile.write(indent + 'end interface \n')


class Module(Action):
    regexp = re.compile("\s*(end)?\s*module\s+\w*\s*")
    def action(self, m, line, state):
        if state.current_template:
            state.templates[state.current_template]['text'] += line
        else:
            state.ofile.write(line.format(rank=args.rank))


class Template_begin(Action):        
    regexp = re.compile("\s*@template\s*\(\s*(\w+)\s*,\s*\[(.*)\]\s*")
    def action(self, m, line, state):
        state.current_template = m.group(1)
        parameters = [x.strip() for x in m.group(2).split(',')]
        state.templates[state.current_template] = {'text':'', 'parameters':parameters}
        
class Template_inside(Action):        
    def match(self, line, state):
        return state.current_template
    def action(self, m, line, state):
        state.templates[state.current_template]['text'] += line

class Template_end(Action):        
    regexp = re.compile("\s*@end template")
    def action(self, m, line, state):
        state.current_template = ''
        
class Instantiate(Action):
    regexp = re.compile("\s*@instantiate\(\s*(\w*)\s*,\s*(\w*)\s*\)")
    def action(self, m, line, state):
        template_name = m.group(1)
        template =  state.templates[template_name]['text']
        instances = state.tkr_dictionaries[m.group(2)]
        for instance in instances:
            d = {}
            d['name'] = name_mangle(template_name, instance['items'])
            d['rank'] = args.rank
            d['mangle'] = name_mangle('',instance['items'])
            parameters = state.templates[template_name]['parameters']
            for item,p in zip(instance['items'],parameters):
                d[p] = item
            state.ofile.write(template.format(**d))
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

