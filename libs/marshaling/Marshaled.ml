open Fpath_.Operators

module InMemory = struct
  type 'a t = string

  let marshal x = Marshal.to_string x []
  let unmarshal x = Marshal.from_string x 0
end

module OnDisk = struct
  type 'a t = Fpath.t

  let marshal path x =
    Common2.write_value x !!path;
    path

  let unmarshal x = Common2.get_value (Fpath.to_string x)
  let get_path x = x
end
