define(`assertEqual',
       `ifelse(`$1',$2,,
       `errprint(`assertion failed in file:' __file__ at line: __line__
     expected: `
<$1>' 
    but found: `
<$2>'
)')')

