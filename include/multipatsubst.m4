# Composes multiple patsubsts
# 2nd argument is a list of (<pattern>,<replacement>).
define(`first',`$1')
define(`FIRST',`first$1')
define(`REST',`(shift$1)')
define(`multipatsubst',`ifelse(`$2',(),`$1',
    `multipatsubst(patsubst(`$1',FIRST(FIRST(`$2')),FIRST(REST(FIRST(`$2')))),REST(`$2'))')')

