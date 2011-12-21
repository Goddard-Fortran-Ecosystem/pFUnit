divert(-1)

include(assert.m4)
include(multipatsubst.m4)

define(`lowerCase',`translit($1,`ABCDEFGHIJKLMNOPQRSTUVWXYZ',`abcdefghijklmnopqrstuvwxyz')')
define(`upperCase',`translit($1,`abcdefghijklmnopqrstuvwxyz',`ABCDEFGHIJKLMNOPQRSTUVWXYZ')')

# IBM XLF compiler specifications
define(`ibmCase',`lowerCase($1)')
define(`ibmPrefix',`')
define(`ibmSuffix',`')
define(`ibmConvert',`multipatsubst($1,((`real',`float*'),(`integer',`int*'),(`proc \(\w*\)',`void (**\1)')))')

# NAG f95 compiler specifications
define(`nagCase',`lowerCase($1)')
define(`nagPrefix',`')
define(`nagSuffix',`_')
define(`nagConvert',`multipatsubst($1,((`real',`float*'),(`integer',`int*'),(`proc \(\w*\)',`void (**\1)')))')

# GNU g95 compiler specifications
define(`g95Case',`lowerCase($1)')
define(`g95Prefix',`')
define(`g95Suffix',`_')
define(`g95Convert',`multipatsubst($1,((`real',`float*'),(`integer',`int*'),(`proc \(\w*\)',`void (**\1)')))')

# Intel ifort compiler specifications
define(`intelCase',`lowerCase($1)')
define(`intelPrefix',`')
define(`intelSuffix',`_')
define(`intelConvert',`multipatsubst($1,((`real',`float*'),(`integer',`int*'),(`proc \(\w*\)',`void (**\1)')))')

# Generic conversion
define(`case',`$2Case($1)')
define(`prefix',`$1Prefix()')
define(`suffix',`$1Suffix()')
define(`convert',`$2Convert($1)')

define(`mangle',`prefix($2)`''`case($1,$2)`''`suffix($2)')
define(`Mangle',`mangle($1,fortranCompiler)')

define(`name',`patsubst($1,`\(\w*\)(.*)',`\1')')
define(`tail',`patsubst($1,`\w*\((.*)\)',`\1')')

define(`signature',`mangle(name($1),$2)`'convert(tail($1),$2)')
define(`Signature',`signature($1,fortranCompiler)')

divert
