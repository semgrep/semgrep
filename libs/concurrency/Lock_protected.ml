type 'a t = { data : 'a; lock : Mutex.t }

let with_lock f x = Concurrency.with_lock (fun () -> f x.data) x.lock
let protect data = { data; lock = Mutex.create () }
