from django.db.models.expressions import Func

# This used to not match when we improved AST_generic.expr_to_type
#ERROR: match
class Position(Func):
    function = 'POSITION'
    template = "%(function)s('%(substring)s' in %(expressions)s)"
