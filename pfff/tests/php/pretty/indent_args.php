<?php

class Foo {
  public function prepareCanViewerEdit() {
    if ($this->canEdit === MUST_PREPARE) {
      $this->canEdit = new CanEditProfile(
        $this->viewerContext,
        $this->getOwnerID()
      );
    }

    return $this->canEdit;
  }

}
