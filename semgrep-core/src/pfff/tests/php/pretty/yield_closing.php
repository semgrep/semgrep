<?php

function foo() {
  yield wait_for_result(
    new GdpAggregationFilter($this->getViewerContext(), $this)
  );
}
