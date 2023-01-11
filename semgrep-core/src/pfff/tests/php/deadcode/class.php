<?php

class :fb:stream:debugrank extends :fb:stream:base {

  protected function getExtraDebugInfo(BaseStoryData $story) {

    foreach ($debug_arr['edges_summary'] as $edge_type => $edge_arr) {
      foreach ($edge_arr['participants'] as $i => $participant) {
        $name = user_get_name($participant);
        $time = render_time_elapsed(time() - $edge_arr['timestamps'][$i]);
        $debug_edges[$edge_type . " " . $i . " " .$edge_arr['weights'][$i]] =
          array(
          'type' => $edge_type,
          'name' => $name,
          'time' => $time,
          'weight' => $edge_arr['weights'][$i],
          'coeff' => idx($object_weights, $participant, 'na'));
      }
    }

  }
}

function render_time_elapsed($seconds, $include_seconds = false) {
  if ($seconds <= 0) {
    return 0;
  }
}
