<?php

class LiteTermsController extends LiteController {
  public function isVisibleWithoutLogin() {
    return true;
  }

  public function process() {
    return <LTermsPage />;
  }

  public function isVisibleWithoutAccountConfirmation() {
    return true;
  }

}
