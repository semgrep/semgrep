# https://github.com/returntocorp/semgrep-rules/issues/660

def decorator_factory( foo ):
    def decorator( function ):
        # ok:reproducer-660
        def function_wrapper( *args, **kwargs ):
            # Do something with 'foo'.
            return function( *args, **kwargs )
        return function_wrapper
    return decorator

@decorator_factory( 'bar' )
def test( ): ''' Simple reproducer. '''
