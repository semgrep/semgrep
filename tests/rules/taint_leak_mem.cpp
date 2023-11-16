void f() {
  if (g()) {
  } else {
    // ruleid: test
    CipherDialog *cipherDialog = new CipherDialog(nullptr, false); 
    if(cipherDialog->exec())
    {
      *cipherSettings = cipherDialog->getCipherSettings();
    } else {
      sqlite3_close_v2(dbHandle);
      *encrypted = false;
      return false;
    }
  }
}
