import django
from django.db.models.aggregates import Aggregate as DjangoAggregate
from django.db.models.sql.aggregates import Aggregate as DjangoSqlAggregate

def query_as_sql(query, connection):
    # ruleid: custom-expression-as-sql
    return query.get_compiler(connection=connection).as_sql()

class SqlAggregate(DjangoSqlAggregate):
    conditional_template = '%(function)s(CASE WHEN %(condition)s THEN %(field)s ELSE null END)'

    def __init__(self, col, source=None, is_summary=False, condition=None, **extra):
        super(SqlAggregate, self).__init__(col, source, is_summary, **extra)
        self.condition = condition

    def relabel_aliases(self, change_map):
        if VERSION < (1, 7):
            super(SqlAggregate, self).relabel_aliases(change_map)
        if self.has_condition:
            condition_change_map = dict((k, v) for k, v in \
                change_map.items() if k in self.condition.query.alias_map
            )
            self.condition.query.change_aliases(condition_change_map)

    def relabeled_clone(self, change_map):
        self.relabel_aliases(change_map)
        return super(SqlAggregate, self).relabeled_clone(change_map)

    def as_sql(self, qn, connection):
        if self.has_condition:
            self.sql_template = self.conditional_template
            self.extra['condition'] = self._condition_as_sql(qn, connection)

        # ruleid: custom-expression-as-sql
        return super(SqlAggregate, self).as_sql(qn, connection)

    @property
    def has_condition(self):
        # Warning: bool(QuerySet) will hit the database
        return self.condition is not None

    def _condition_as_sql(self, qn, connection):
        '''
        Return sql for condition.
        '''
        def escape(value):
            if isinstance(value, bool):
                value = str(int(value))
            if isinstance(value, six.string_types):
                # Escape params used with LIKE
                if '%' in value:
                    value = value.replace('%', '%%')
                # Escape single quotes
                if "'" in value:
                    value = value.replace("'", "''")
                # Add single quote to text values
                value = "'" + value + "'"
            return value

        # ruleid: custom-expression-as-sql
        sql, param = self.condition.query.where.as_sql(qn, connection)
        param = map(escape, param)

        return sql % tuple(param)