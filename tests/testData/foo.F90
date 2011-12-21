subroutine foo()
 use pfunit
 call throw(Exception('foo was called'))
end subroutine foo
