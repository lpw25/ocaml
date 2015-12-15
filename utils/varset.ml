
type id = int

type 'a t =
  | Empty
  | Leaf of int * 'a
  | Branch of int * int * 'a t * 'a t

let empty = Empty

let is_empty = function
  | Empty -> true
  | Leaf _ -> false
  | Branch _ -> false

let singleton i d = Leaf(i, d)

let zero_bit i bit =
  i land bit = 0

let lowest_bit x =
  x land (-x)

let branching_bit prefix0 prefix1 =
  lowest_bit (prefix0 lxor prefix1)

let mask i bit =
  i land (bit - 1)

let match_prefix i prefix bit =
  mask i bit = prefix

let equal_prefix prefix0 bit0 prefix1 bit1 =
  bit0 = bit1 && prefix0 = prefix1

let includes_prefix prefix0 bit0 prefix1 bit1 =
  bit0 < bit1 && match_prefix prefix1 prefix0 bit0

let compare_prefix prefix0 bit0 prefix1 bit1 =
  let c = compare bit0 bit1 in
  if c = 0 then compare prefix0 prefix1
  else c

let rec mem i = function
  | Empty -> false
  | Leaf(j, _) -> j = i
  | Branch(prefix, bit, t0, t1) ->
      if not (match_prefix i prefix bit) then false
      else if zero_bit i bit then mem i t0
      else mem i t1

let branch prefix bit t0 t1 =
  match t0, t1 with
  | Empty, _ -> t1
  | _, Empty -> t0
  | t0, t1 -> Branch(prefix, bit, t0, t1)

let join prefix0 t0 prefix1 t1 =
  let bit = branching_bit prefix0 prefix1 in
    if zero_bit prefix0 bit then
      Branch(mask prefix0 bit, bit, t0, t1)
    else
      Branch(mask prefix0 bit, bit, t1, t0)

let rec add i d = function
  | Empty -> Leaf(i, d)
  | Leaf(j, _) as t ->
    if i = j then t
    else join i (Leaf(i, d)) j t
  | Branch(prefix, bit, t0, t1) as t ->
    if match_prefix i prefix bit then
      if zero_bit i bit then
        Branch(prefix, bit, add i d t0, t1)
      else
        Branch(prefix, bit, t0, add i d t1)
    else
      join i (Leaf(i, d)) prefix t

let rec remove i = function
  | Empty -> Empty
  | Leaf(j, _) as t ->
    if i = j then Empty
    else t
  | Branch (prefix, bit, t0, t1) as t ->
    if match_prefix i prefix bit then
      if zero_bit i bit then
        branch prefix bit (remove i t0) t1
      else
        branch prefix bit t0 (remove i t1)
    else
      t

let rec union t0 t1 =
  match t0, t1 with
  | Empty, _ -> t1
  | _, Empty -> t0
  | Leaf(i, d), _ -> add i d t1
  | _, Leaf(i, d) -> add i d t0
  | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
    if equal_prefix prefix0 bit0 prefix1 bit1 then
      Branch(prefix0, bit0, union t00 t10, union t01 t11)
    else if includes_prefix prefix0 bit0 prefix1 bit1 then
      if zero_bit prefix1 bit0 then
        Branch(prefix0, bit0, union t00 t1, t01)
      else
        Branch(prefix0, bit0, t00, union t01 t1)
    else if includes_prefix prefix1 bit1 prefix0 bit0 then
      if zero_bit prefix0 bit1 then
        Branch(prefix1, bit1, union t0 t10, t11)
      else
        Branch(prefix1, bit1, t10, union t0 t11)
    else
      join prefix0 t0 prefix1 t1

let rec subset t0 t1 =
  match t0, t1 with
  | Empty, _ -> true
  | _, Empty -> false
  | Branch _, Leaf _ -> false
  | Leaf(i, _), _ -> mem i t1
  | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
      if equal_prefix prefix0 bit0 prefix1 bit1 then
        subset t00 t10 && subset t01 t11
      else if includes_prefix prefix1 bit1 prefix0 bit0 then
        if zero_bit prefix0 bit1 then
          subset t0 t10
        else
          subset t0 t11
      else
        false

let rec inter t0 t1 =
  match t0, t1 with
  | Empty, _ -> Empty
  | _, Empty -> Empty
  | Leaf(i, _), _ -> if mem i t1 then t0 else Empty
  | _, Leaf(i, _) -> if mem i t0 then t1 else Empty
  | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
      if equal_prefix prefix0 bit0 prefix1 bit1 then
        branch prefix0 bit0 (inter t00 t10) (inter t01 t11)
      else if includes_prefix prefix0 bit0 prefix1 bit1 then
        if zero_bit prefix1 bit0 then
          inter t00 t1
        else
          inter t01 t1
      else if includes_prefix prefix1 bit1 prefix0 bit0 then
        if zero_bit prefix0 bit1 then
          inter t0 t10
        else
          inter t0 t11
      else
        Empty

let rec diff t0 t1 =
  match t0, t1 with
  | Empty, _ -> Empty
  | _, Empty -> t0
  | Leaf(i, _), _ -> if mem i t1 then Empty else t0
  | _, Leaf(i, _) -> remove i t0
  | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
      if equal_prefix prefix0 bit0 prefix1 bit1 then
        branch prefix0 bit0 (diff t00 t10) (diff t01 t11)
      else if includes_prefix prefix0 bit0 prefix1 bit1 then
        if zero_bit prefix1 bit0 then
          branch prefix0 bit0 (diff t00 t1) t01
        else
          branch prefix0 bit0 t00 (diff t01 t1)
      else if includes_prefix prefix1 bit1 prefix0 bit0 then
        if zero_bit prefix0 bit1 then
          diff t0 t10
        else
          diff t0 t11
      else
        t0

let rec cardinal = function
  | Empty -> 0
  | Leaf _ -> 1
  | Branch(_, _, t0, t1) -> cardinal t0 + cardinal t1

let rec iter f = function
  | Empty -> ()
  | Leaf(_, d) -> f d
  | Branch(_, _, t0, t1) -> iter f t0; iter f t1

let rec fold f t acc =
  match t with
  | Empty -> acc
  | Leaf(_, d) -> f d acc
  | Branch(_, _, t0, t1) -> fold f t0 (fold f t1 acc)

let rec for_all p = function
  | Empty -> true
  | Leaf(_, d) -> p d
  | Branch(_, _, t0, t1) -> for_all p t0 && for_all p t1

let rec exists p = function
  | Empty -> false
  | Leaf(_, d) -> p d
  | Branch (_,_,t0,t1) -> exists p t0 || exists p t1

let filter p t =
  let rec loop acc = function
    | Empty -> acc
    | Leaf(i, d) -> if p d then add i d acc else acc
    | Branch(_, _, t0, t1) -> loop (loop acc t0) t1
  in
  loop Empty t

let partition p t =
  let rec loop ((true_, false_) as acc) = function
    | Empty -> acc
    | Leaf(i, d) ->
      if p d then (add i d true_, false_)
      else (true_, add i d false_)
    | Branch(_, _, t0, t1) -> loop (loop acc t0) t1
  in
  loop (Empty, Empty) t

let rec choose = function
  | Empty -> raise Not_found
  | Leaf(_, d) -> d
  | Branch(_, _, t0, _) -> choose t0

let elements t =
  let rec loop acc = function
    | Empty -> acc
    | Leaf(_, d) -> d :: acc
    | Branch(_, _, t0, t1) -> loop (loop acc t0) t1
  in
  loop [] t

let min_elt t =
  let rec loop = function
    | Empty -> raise Not_found
    | Leaf(i, d) -> i, d
    | Branch(_, _, t0, t1) ->
      let i0, d0 = loop t0 in
      let i1, d1 = loop t1 in
        if i0 < i1 then i0, d0
        else i1, d1
  in
  let _, d = loop t in
  d

let max_elt t =
  let rec loop = function
    | Empty -> raise Not_found
    | Leaf(i, d) -> i, d
    | Branch(_, _, t0, t1) ->
      let i0, d0 = loop t0 in
      let i1, d1 = loop t1 in
        if i0 > i1 then i0, d0
        else i1, d1
  in
  let _, d = loop t in
  d

let rec equal t0 t1 =
  match t0, t1 with
  | Empty, Empty -> true
  | Leaf(i, _), Leaf(j, _) -> i = j
  | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
    if equal_prefix prefix0 bit0 prefix1 bit1 then
      equal t00 t10 && equal t01 t11
    else false
  | _, _ -> false

let rec compare t0 t1 =
  match t0, t1 with
  | Empty, Empty -> 0
  | Leaf(i, _), Leaf(j, _) -> Pervasives.compare i j
  | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
    let c = compare_prefix prefix0 bit0 prefix1 bit1 in
    if c = 0 then
      let c = compare t00 t10 in
      if c = 0 then compare t01 t11
      else c
    else c
  | Empty, Leaf _ -> 1
  | Empty, Branch _ -> 1
  | Leaf _, Branch _ -> 1
  | Leaf _, Empty -> -1
  | Branch _, Empty -> -1
  | Branch _, Leaf _ -> -1

let split i t =
  let rec loop ((lt, mem, gt) as acc) = function
    | Empty -> acc
    | Leaf(j, d) ->
        if i = j then (lt, true, gt)
        else if j < i then (add j d lt, mem, gt)
        else (lt, mem, add j d gt)
    | Branch(_, _, t0, t1) -> loop (loop acc t0) t1
  in
  loop (Empty, false, Empty) t

let rec find i = function
  | Empty -> raise Not_found
  | Leaf(j, d) -> if j = i then d else raise Not_found
  | Branch(prefix, bit, t0, t1) ->
      if not (match_prefix i prefix bit) then raise Not_found
      else if zero_bit i bit then find i t0
      else find i t1
