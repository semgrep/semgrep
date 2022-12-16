(* !!take care!!: this classe have side effect, not a pure oassoc *)
class ['a, 'b] oassoc_buffer :
  int ->
  (< add : 'a * 'b -> 'd; assoc : 'a -> 'b; del : 'a * 'b -> 'd;
     delkey : 'a -> 'd; iter : ('a * 'b -> unit) -> unit; length : int;
     keys: 'a list; clear: unit;
     .. >
   as 'd) ->
  object ('o)
    inherit ['a,'b] Oassoc.oassoc

    (* ocollection concrete instantiation of virtual methods *)
    method empty : 'o
    method add : 'a * 'b -> 'o

    method iter : ('a * 'b -> unit) -> unit
    method view : ('a * 'b, 'o) Ocollection.view

    method del : 'a * 'b -> 'o
    method mem : 'a * 'b -> bool
    method null : bool

    (* oassoc concrete instantiation of virtual methods *)
    method assoc : 'a -> 'b
    method delkey : 'a -> 'o

    method keys: 'a list

    (* ugly, from objet class, extension trick *)
    method private myflush : unit
    method misc_op_hook2 : unit


  end
