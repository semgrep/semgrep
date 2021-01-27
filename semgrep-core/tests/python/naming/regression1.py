from django.db.models.expressions import Func

class Position(Func):
    function = 'POSITION'
    template = "%(function)s('%(substring)s' in %(expressions)s)"

    def __init__(self, expression, substring):
        super().__init__(expression, substring=substring)
