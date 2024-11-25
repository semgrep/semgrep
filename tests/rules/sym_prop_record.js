let bar = {
  client: "mysql",
  connection: {
    host: SOLA_DB_HOST,
    port: SOLA_DB_PORT,
    user: SOLA_DB_USER,
    password: 'SOLA_DB_PWD',
  },
}
// ruleid: test
foo(bar)
