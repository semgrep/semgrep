async function getUserById(id: number) {
  // Solution 1: unsafe, allows SQL injection
  await prisma.$executeRaw("SELECT * FROM users WHERE id = " + id);
  // Solution 2: safe: properly escapes the value
  await prisma.$executeRaw`SELECT * FROM users WHERE id = ${id}`;
    // Solution 3: unsafe, allows SQL injection
    //ERROR: match
  await prisma.$executeRaw(`SELECT * FROM users WHERE id = ${id}`);
}
