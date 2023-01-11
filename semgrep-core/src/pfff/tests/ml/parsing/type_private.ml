type 'a t = private
  | Atom of 'a
  | List of op * 'a t list
