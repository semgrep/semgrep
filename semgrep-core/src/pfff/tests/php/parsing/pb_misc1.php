<?php
class DatabaseMssql extends Database {

  function open($server,$user,$password,$dbName) {
    $success = @/**/mssql_select_db($dbName, $this->mConn);
  }

  /**
   * Close an MSSQL database
   */
  function close() {
    $this->mOpened = false;
    if ($this->mConn) {
      if ($this->trxLevel()) $this->immediateCommit();
      return mssql_close($this->mConn);
    } else return true;
  }

}
