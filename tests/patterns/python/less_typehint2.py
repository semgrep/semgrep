#ERROR: match
engine = create_engine('postgresql://user@localhost/database')
query: str = f"INSERT INTO person (name) VALUES ('{name}')"
engine.execute(query)
