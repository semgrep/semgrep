module.exports = {
  local: {
    username: "AppUser",
    database: "AppDb",
    dialect: "postgres",
    host: "127.0.0.1",
    dialectOptions: {
    // ruleid: sequelize-weak-tls-version
      ssl: {
        minVersion: 'TLSv1'
      }
    }
  }
};
