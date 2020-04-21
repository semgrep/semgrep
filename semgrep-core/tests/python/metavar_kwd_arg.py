from django.db.models.expressions import Func

class Position(Func):
    function = 'POSITION'
    template = "%(function)s('%(substring)s' in %(expressions)s)"

    #ERROR: match
    def __init__(self, expression, substring):
        # substring=substring is a SQL injection vulnerability!
        super().__init__(expression, substring=substring)

