import sqlalchemy
from sqlalchemy.orm import sessionmaker, scoped_session

source = source()

engine = sqlalchemy.create_engine('my connection string')
Session = scoped_session(sessionmaker(bind=engine))

with Session(engine) as s:
    #ruleid: test
    result = s.execute('SELECT * FROM my_table WHERE my_column = ' + source)
