<?php

class :fb:plans:feed-attachment extends :ui:attachment {

  attribute
    ViewerContext viewercontext @required,
    EntPlan plan @required,
    array taggees = array()
  ;
}
