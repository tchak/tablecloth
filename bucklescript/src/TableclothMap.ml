type ('key, 'value, 'cmp) t = ('key, 'value, 'cmp) Belt.Map.t

module Of (M : Comparator.S) = struct
  type nonrec 'value t = (M.t, 'value, M.identity) t
end

let ofArray (comparator : ('key, 'id) Comparator.s) (values : ('key * 'v) array)
    : ('key, 'value, 'id) t =
  Belt.Map.fromArray values ~id:(Internal.toBeltComparator comparator)


let empty comparator = ofArray comparator [||]

let ofList comparator l = ofArray comparator (Array.of_list l)

let singleton comparator ~key ~value = ofArray comparator [| (key, value) |]

let isEmpty = Belt.Map.isEmpty

let includes = Belt.Map.has

let length = Belt.Map.size

let add m ~key ~value = Belt.Map.set m key value

let ( .?{}<- ) (map : ('key, 'value, 'id) t) (key : 'key) (value : 'value) :
    ('key, 'value, 'id) t =
  add map ~key ~value


let remove = Belt.Map.remove

let get = Belt.Map.get

let ( .?{} ) (map : ('key, 'value, _) t) (key : 'key) : 'value option =
  get map key


let update m ~key ~f = Belt.Map.update m key f

let merge m1 m2 ~f = Belt.Map.merge m1 m2 f

let map m ~f = Belt.Map.map m (fun value -> f value)

let mapI t ~f = Belt.Map.mapWithKey t f

let filter m ~f = Belt.Map.keep m (fun _ value -> f value)

let partition m ~f = Belt.Map.partition m (fun key value -> f ~key ~value)

let find m ~f = Belt.Map.findFirstBy m (fun key value -> f ~key ~value)

let any m ~f = Belt.Map.some m (fun _ value -> f value)

let all m ~f = Belt.Map.every m (fun _ value -> f value)

let forEach m ~f = Belt.Map.forEach m (fun _ value -> f value)

let forEachI m ~f = Belt.Map.forEach m (fun key value -> f ~key ~value)

let fold m ~initial ~f =
  Belt.Map.reduce m initial (fun acc key data -> f acc ~key ~value:data)


let keys m = Belt.Map.keysToArray m |. Array.to_list

let values m = Belt.Map.valuesToArray m |. Array.to_list

let maximum = Belt.Map.maxKey

let minimum = Belt.Map.minKey

let extent t = Option.both (minimum t) (maximum t)

let toArray = Belt.Map.toArray

let toList = Belt.Map.toList

module Poly = struct
  type identity

  type nonrec ('k, 'v) t = ('k, 'v, identity) t

  let ofArray (type k v) (a : (k * v) array) =
    ( Belt.Map.fromArray
        a
        ~id:
          ( module struct
            type t = k

            type nonrec identity = identity

            let cmp = Pervasives.compare |. Obj.magic
          end )
      : (k, v) t )


  let empty () = ofArray [||]

  let ofList l = ofArray (Array.of_list l)

  let singleton ~key ~value = ofArray [| (key, value) |]
end

module Int = struct
  type nonrec 'v t = (Int.t, 'v, Int.identity) t

  let ofArray a = Poly.ofArray a |. Obj.magic

  let empty = ofArray [||]

  let singleton ~key ~value = ofArray [| (key, value) |]

  let ofList l = ofArray (Array.of_list l)
end

module String = struct
  type nonrec 'v t = (String.t, 'v, String.identity) t

  let ofArray a = Poly.ofArray a |. Obj.magic

  let empty = ofArray [||]

  let singleton ~key ~value = ofArray [| (key, value) |]

  let ofList l = ofArray (Array.of_list l)
end
