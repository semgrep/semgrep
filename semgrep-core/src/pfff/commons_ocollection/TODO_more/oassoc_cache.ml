open Common

open Oassoc

open Oassocb
open Osetb

(* todo: gather stat of use per key, so when flush, try keep
 * entries that are used above a certain threshold, and if after
 * this step, there is still too much, then erase also those keys.
 *
 * todo: limit number of entries, and erase all (then better do a ltu)
 *
 * todo: another cache that behave as in lfs1,
 * every 100 operation do a flush
 *
 * todo: choose between oassocb and oassoch ?
 *
 * Also take care that must often redefine all function in the original
 * oassoc.ml because if some methods are not redefined, for instance
 * #clear, then if do wrapper over a oassocdbm, then even if oassocdbm
 * redefine #clear, it will not be called, but instead the default
 * method will be called that internally will call another method.
 * So better delegate all the methods and override even the method
 * with a default definition.
 *
 * In the same way sometimes an exn can occur at weird time. When
 * we add an element, sometimes this may raise an exn such as Out_of_memory,
 * but as we dont add directly but only at flush time, the exn
 * may happen far later the user added something in this oassoc.
 * Also in the case of Out_of_memory, even if the entry is not
 * added in the wrapped, it will still be present in the cache
 * and so the next flush will still generate an exn that again
 * may not be cached. So for the moment if Out_of_memory then
 * do something special and erase the entry in the cache.
*)

(* !!take care!!: this class has side effect, not a pure oassoc *)
(* can not make it pure, cos the assoc have side effect on the cache *)
class ['a,'b] oassoc_buffer   max cached =
  object(o)
    inherit ['a,'b] oassoc

    val counter = ref 0
    val cache = ref (new oassocb [])
    val dirty = ref (new osetb Set_poly.empty)
    val wrapped = ref cached

    method private myflush =

      let has_a_raised = ref false in

      !dirty#iter (fun k ->
        try
          wrapped := !wrapped#add (k, !cache#assoc k)
        with Out_of_memory ->
          pr2 "PBBBBBB: Out_of_memory in oassoc_buffer, but still empty cache";
          has_a_raised := true;
      );
      dirty := (new osetb Set_poly.empty);
      cache := (new oassocb []);
      counter := 0;
      if !has_a_raised then raise Out_of_memory


    method misc_op_hook2 = o#myflush

    method empty =
      raise Todo

    (* what happens in k is already present ? or if add multiple times
     * the same k ? cache is a oassocb and so the previous binding is
     * still there, but dirty is a set, and in myflush we iter based
     * on dirty so we will flush only the last 'k' in the cache.
    *)
    method add (k,v) =
      cache := !cache#add (k,v);
      dirty := !dirty#add k;
      incr counter;
      if !counter > max then o#myflush;
      o

    method iter f =
      o#myflush; (* bugfix: have to flush !!! *)
      !wrapped#iter f


    method keys =
      o#myflush; (* bugfix: have to flush !!! *)
      !wrapped#keys

    method clear =
      o#myflush; (* bugfix: have to flush !!! *)
      !wrapped#clear


    method length =
      o#myflush;
      !wrapped#length

    method view =
      raise Todo

    method del (k,v) =
      cache := !cache#del (k,v);
      (* TODO as for delkey, do a try over wrapped *)
      wrapped := !wrapped#del (k,v);
      dirty := !dirty#del k;
      o
    method mem e = raise Todo
    method null = raise Todo

    method assoc k =
      try !cache#assoc k
      with Not_found ->
        (* may launch Not_found, but this time, dont catch it *)
        let v = !wrapped#assoc k in
        begin
          cache := !cache#add (k,v);
          (* otherwise can use too much mem *)
          incr counter;
          if !counter > max then o#myflush;
          v
        end

    method delkey k =
      cache := !cache#delkey k;
      (* sometimes have not yet flushed, so may not be yet in, (could
       * also flush in place of doing try).
       *
       * TODO would be better to see if was in cache (in case mean that
       * perhaps not flushed and do try and in other case just cos del
       * (without try) cos forcement flushed ou was an error *)
      begin
        try wrapped := !wrapped#delkey k
        with Not_found -> ()
      end;
      dirty := !dirty#del k;
      o

  end


(*
class ['a,'b] oassoc_cache cache cached max =
  object(o)
    inherit ['a,'b] oassoc

    val full = ref 0
    val max = max
    val cache = cache
    val cached = cached
    val lru = TODO

    val data = Hashtbl.create 100

    method empty = raise Todo
    method add (k,v) = (Hashtbl.add data k v; o)
    method iter f = cached#iter f
    method view = raise Todo

    method del (k,v) = (cache#del (k,v); cached#del (k,v); o)
    method mem e = raise Todo
    method null = raise Todo

    method assoc k = Hashtbl.find data k
    method delkey k = (cache#delkey (k,v); cached#del (k,v); o)
end
*)
