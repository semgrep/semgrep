let \#type = 1
let use = \#type
let café = 2
module Étage = struct let naïve = 3 end
let total = café + Étage.naïve
type \#record = { \#field : int }
let raw_field r = r.\#field
let raw_type (x : \#record) = x
