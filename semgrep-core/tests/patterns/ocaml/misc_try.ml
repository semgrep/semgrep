let test2 xs =
 (* ERROR: match *)
 try
   if List.find 1 xs
   then 1
   else 2
 with Not_found -> 3
