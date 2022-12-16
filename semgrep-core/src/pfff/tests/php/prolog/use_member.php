<?php

class TestSimpleMember {

  public function getShortName() {
    return $this->data['short_name'];
  }

  public function getAllLocalizedNames() {
    return idx($this->data, 'localized_names', array());
  }

}
