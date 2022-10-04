require 'pg'

def bad1()
    con = PG.connect :dbname => 'testdb', :user => 'janbodnar'
    query = "SELECT name FROM users WHERE age=" + params['age']
    # ruleid: ruby-pg-sqli
    con.exec query
end

def bad2(user_input)
    age = params[user_input]
    con = PG.connect :dbname => 'testdb', :user => 'janbodnar'
    query = "SELECT name FROM users WHERE age="
    query += age
    # ruleid: ruby-pg-sqli
    con.exec query
end

def bad3(userinput)
    con = PG.connect :dbname => 'testdb', :user => 'janbodnar'
    query = "SELECT name FROM users WHERE age="
    query.concat(cookies[userinput])
    # ruleid: ruby-pg-sqli
    con.exec query
end

def bad4(userinput)
    con = PG.connect :dbname => 'testdb', :user => 'janbodnar'
    query = "SELECT name FROM users WHERE age="
    query << params[userinput]
    # ruleid: ruby-pg-sqli
    con.exec(query)
end

def bad5()
    con = PG.connect :dbname => 'testdb', :user => 'janbodnar'
    # ruleid: ruby-pg-sqli
    con.exec_params("SELECT name FROM users WHERE age=" + params['age'])
end

def bad6()
    con = PG.connect :dbname => 'testdb', :user => 'janbodnar'
    # ruleid: ruby-pg-sqli
    con.exec_params("SELECT name FROM users WHERE age=".concat(params['age']))
end

def bad7(userinput)
    con = PG.connect :dbname => 'testdb', :user => 'janbodnar'
    # ruleid: ruby-pg-sqli
    con.exec_params("SELECT name FROM users WHERE age=" << params[userinput])
end

def ok1()
    conn = PG.connect(:dbname => 'db1')
    conn.prepare('statement1', 'insert into table1 (id, name, profile) values ($1, $2, $3)')
    # ok: ruby-pg-sqli
    conn.exec_prepared('statement1', [ 11, 'J.R. "Bob" Dobbs', 'Too much is always better than not enough.' ])
end

def ok2()
    con = PG.connect :dbname => 'testdb', :user => 'janbodnar'
    query = "SELECT name FROM users WHERE age=" + "3"
    # ok: ruby-pg-sqli
    con.exec query
end

def ok3()
    con = PG.connect :dbname => 'testdb', :user => 'janbodnar'
    query = "SELECT name FROM users WHERE age="
    query += "3"
    # ok: ruby-pg-sqli
    con.exec query
end

def ok4(userinput)
    con = PG.connect :dbname => 'testdb', :user => 'janbodnar'
    query = "SELECT name FROM users WHERE age="
    query.concat("hello")
    # ok: ruby-pg-sqli
    con.exec query
end

def ok5(userinput)
    con = PG.connect :dbname => 'testdb', :user => 'janbodnar'
    query = "SELECT name FROM users WHERE age="
    query << "hello"
    # ok: ruby-pg-sqli
    con.exec query
end

def ok6()
    con = PG.connect :dbname => 'testdb', :user => 'janbodnar'
    stm = "SELECT $1::int AS a, $2::int AS b, $3::int AS c"
    # ok: ruby-pg-sqli
    con.exec_params(stm, [1, 2, 3])
end

def ok7()
    con = PG.connect :dbname => 'testdb', :user => 'janbodnar'
    # ok: ruby-pg-sqli
    con.exec("SELECT name FROM users WHERE age=" + "3")
end

def ok8()
    con = PG.connect :dbname => 'testdb', :user => 'janbodnar'
    # ok: ruby-pg-sqli
    con.exec("SELECT * FROM users WHERE email=hello;".concat("hello"))
end

def ok9()
    con = PG.connect :dbname => 'testdb', :user => 'janbodnar'
    # ok: ruby-pg-sqli
    con.exec("SELECT * FROM users WHERE email=hello;" << "hello")
end

