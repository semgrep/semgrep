int main(int argc, char** argv) {
  char *userName = argv[1];
  char query[1000] = {0}
  strcat(query, "SELECT UID FROM USERS where name = \"");
  strcat(query, userName);
  strcat(query, "\"");
  // ruleid: test
  mysql_query(0, query);
}
