#!/usr/bin/env python

def flattened(l):
    "http://caolanmcmahon.com/posts/flatten_for_python/"
    result = _flatten(l, lambda x: x)
    while type(result) == list and len(result) and callable(result[0]):
        if result[1] != []:
            yield result[1]
        result = result[0]([])
    yield result

def _flatten(l, fn, val=[]):
    "http://caolanmcmahon.com/posts/flatten_for_python/"
    if type(l) != list:
        return fn(l)
    if len(l) == 0:
        return fn(val)
    return [lambda x: _flatten(l[0], \
                               lambda y: _flatten(l[1:],fn,y), x), val]

 
def main():
    a = [1,2,3,4,[5,[6,7]]]
    print 'a ',a
    print 'f(a)', flattened(a)
    print '[f(a)]', list(flattened(a))
    return

if __name__ == "__main__":
    main()
 
