val record_time_limit : name:string -> duration:float -> exceeded:bool -> unit
(** [record_time_limit ~name:"some_func" ~duration:1.0 ~exceeded] records
      metrics on if a time limit was exceeded, how long we spent, and which
      function set it *)
