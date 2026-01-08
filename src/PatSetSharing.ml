type t = {
  k: int;
  v: int;
  mutable l: t;
  mutable r: t;
}

let rec empty = {k = 0; v = 0; l = empty; r = empty}

(* Extract the most significant bit of integer `x` *)
let extract_bit = Bit_lib.extract_msb

let older a b =
  Int.compare (Obj.magic a) (Obj.magic b) < 0

let share_oldest_l t1 t2 =
  if older t1.l t2.l
  then t2.l <- t1.l
  else t1.l <- t2.l

let share_oldest_r t1 t2 =
  if older t1.r t2.r
  then t2.r <- t1.r
  else t1.r <- t2.r

let rec equal t1 t2 =
  t1.k = t2.k && t1.v = t2.v &&
  (t1.l == t2.l || (equal t1.l t2.l && (share_oldest_l t1 t2; true))) &&
  (t1.r == t2.r || (equal t1.r t2.r && (share_oldest_r t1 t2; true)))

let equal t1 t2 = t1 == t2 || equal t1 t2

let rec compare t1 t2 =
  if t1 == t2 then 0 else
  let c = Int.compare t1.k t2.k in
  if c <> 0 then c else
    let c = Int.compare t1.v t2.v in
    if c <> 0 then c else
      let c = compare t1.l t2.l in
      if c <> 0 then c else (
        if t1.l != t2.l then share_oldest_l t1 t2;
        let c = compare t1.r t2.r in
        if c <> 0 then c else (
          if t1.r != t2.r then share_oldest_r t1 t2;
          0
        )
      )

let rec lookup k t =
  if k >= t.k then
    if k = t.k
    then t.v
    else 0
  else
    let k' = k land lnot (extract_bit t.k) in
    lookup k' (if k = k' then t.r else t.l)

let rec insert k v t =
  if k >= t.k then
    if k = t.k then
      match v lor t.v with
      | v when v = t.v -> t
      | v -> {t with v}
    else
      let m = extract_bit k in
      if m land t.k = m then
        {k; v; l = insert (t.k land lnot m) t.v t.l; r = t.r}
      else
        {k; v; l = empty; r = t}
  else
    let m = extract_bit k in
    if m > t.k lsr 1 then
      match insert (k land lnot m) v t.l with
      | l when t.l == l -> t
      | l -> {t with l}
    else
      match insert k v t.r with
      | r when t.r == r -> t
      | r -> {t with r}

let low_level_insert_mask = insert

let rec join k l r =
  if l.v = 0 then
    r
  else
    let l' = join l.k l.l l.r in
    {k = (extract_bit k) lor l.k; v = l.v; l = l'; r}

let rec remove k v t =
  if k >= t.k then
    if k = t.k then
      match t.v land lnot v with
      | 0 -> join t.k t.l t.r
      | v -> if v = t.v then t else {t with v}
    else
      t
  else
    let m = extract_bit k in
    if m > t.k lsr 1 then
      match remove (k land lnot m) v t.l with
      | l when t.l == l -> t
      | l -> {t with l}
    else
      match remove k v t.r with
      | r when t.r == r -> t
      | r -> {t with r}

let rec union a b =
  let a, b = if a.k > b.k then b, a else a, b in
  (* b.k >= a.k *)
  if a.v = 0 then b else
  if a.k = b.k then
    let l = union a.l b.l in
    let r = union a.r b.r in
    match a.v lor b.v with
    | v when v = b.v && l == b.l && r == b.r -> b
    | v -> {b with v; l; r}
  else
    let m = extract_bit a.k in
    if m > b.k lsr 1 then
      match
        (* TODO: insert_and_union? *)
        union_fringe (lnot m) a a.l b.l,
        union a.r b.r
      with
      | l, r when l == b.l && r == b.r -> b
      | l, r -> {b with l; r}
    else
      match union a b.r with
      | r when r == b.r -> b
      | r -> {b with r}

and union_fringe mask a0 a b =
  if a0 == a then union a b
  else
    let k = a0.k land mask in
    if k = b.k then
      (* b.k > a.k *)
      let m = extract_bit a.k in
      let v = a0.v lor b.v in
      let l, r =
        let mask = mask land lnot m in
        if m > b.k lsr 1 then
          union_fringe mask a0.l a.l b.l,
          union a.r b.r
        else
          insert_fringe mask a0.l a b.l,
          union a b.r
      in
      if v = b.v && l == b.l && r == b.r then
        b
      else
        {k; v; l; r}
    else if k > b.k then
      union_fringe_safe mask a0 a b
      (*let m = extract_bit k in
        let v = a0.v in
        if b.k land m = m (* same bit *) then
        {k; v; l = union_fringe_safe (mask land lnot m) a0.l a b.l; r = b.r}
        else
        {k; v; l = empty; r = union_fringe mask a0.l a b}*)
    else
      let m = extract_bit b.k in
      if a.k land m = m then
        (*same bit*)
        match
          union_fringe_safe (mask land lnot m) a0 a.l b.l,
          union a.r b.r
        with
        | l, r when l == b.l && r == b.r -> b
        | l, r -> {b with l; r}
      else
        let mask = mask land lnot m in
        let rec extract_fringe a0 a l =
          if a0.k land m = m then
            extract_fringe a0.l a (insert (a0.k land mask) a0.v l)
          else
            let r = union_fringe mask a0 a b.r in
            if l == b.l && r == b.r
            then b
            else {b with l; r}
        in
        extract_fringe a0 a b.l

and union_fringe_safe mask a0 a b =
  union a (insert_fringe mask a0 a b)

and insert_fringe mask a0 a b =
  if a0 == a then
    b
  else
    insert_fringe mask a0.l a (insert (a0.k land mask) a0.v b)

let diff_update a v l r =
  if v = 0 then
    join a.k l r
  else if v = a.v && a.l == l && a.r == r then
    a
  else
    {a with v; l; r}

let rec diff a b =
  if b.v = 0 then a else
  if a.k >= b.k then
    if a.k = b.k then
      diff_update a
        (a.v land lnot b.v)
        (diff a.l b.l)
        (diff a.r b.r)
    else
      let m = extract_bit a.k in
      if b.k land m = m then
        diff_update a a.v
          (* TODO: remove_and_diff?
             skip to b.k land m -1, remove b.l *)
          (diff (remove (b.k land lnot m) b.v a.l) b.l)
          (diff a.r b.r)
      else
        diff_update a a.v a.l (diff a.r b)
  else
    let m = extract_bit a.k in
    if m > b.k lsr 1 then
      diff_update a
        (a.v land lnot (lookup (a.k land lnot m) b.l))
        (diff a.l b.l)
        (diff a.r b.r)
    else
      diff a b.r

(*let rec inter a b =
  if a.k > b.k then inter_right b a else inter_right a b

and inter_right a b =
  if b.v = 0 then empty else
  if a.k >= b.k then
    if a.k = b.k then
      diff_update a
        (a.v land lnot b.v)
        (diff a.l b.l)
        (diff a.r b.r)
    else
      let m = extract_bit a.k in
      if m = extract_bit b.k then
        diff_update a a.v
          (* TODO: remove_and_diff?
             skip to b.k land m -1, remove b.l *)
          (diff (remove (b.k land lnot m) b.v a.l) b.l)
          (diff a.r b.r)
      else
        diff_update a a.v a.l (diff a.r b)
  else
    let m = extract_bit a.k in
    if m = extract_bit b.k then
      diff_update a
        (a.v land lnot (lookup (a.k land lnot m) b.l))
        (diff a.l b.l)
        (diff a.r b.r)
    else
      diff a b.r*)

let mask_size = Sys.word_size - 1

let encode x = (x / mask_size, 1 lsl (x mod mask_size))

let decode_base x = x * mask_size

let add x t =
  let k, v = encode x in
  insert k v t

let mem x t =
  let k, v = encode x in
  lookup k t land v <> 0

let remove x t =
  let k, v = encode x in
  remove k v t

let singleton x = add x empty

let rec iter f mask t =
  if t.v <> 0 then (
    iter f mask t.r;
    iter f (mask lor extract_bit t.k) t.l;
    let base = decode_base (mask lor t.k) in
    let v = ref t.v in
    while !v <> 0 do
      let index = Bit_lib.lsb_index !v in
      v := !v lxor (1 lsl index);
      f (base + index);
    done
  )

let iter f t = iter f 0 t

(*let bits x =
  let size = Sys.word_size - 1 in
  let b = Bytes.make size '0' in
  for i = 0 to size - 1 do
    if x land (1 lsl (size - i)) <> 0 then
      Bytes.set b i '1'
  done;
  Bytes.unsafe_to_string b*)

let rec rev_iter f mask t =
  if t.v <> 0 then (
    let base = decode_base (mask lor t.k) in
    let v = ref t.v in
    while !v <> 0 do
      let index = Bit_lib.msb_index !v in
      v := !v lxor (1 lsl index);
      f (base + index);
    done;
    rev_iter f (mask lor extract_bit t.k) t.l;
    rev_iter f mask t.r;
  )

let rev_iter f t = rev_iter f 0 t

let validate _ = true

let is_empty t = t.v = 0

let is_singleton t =
  t.v <> 0 &&
  (t.v land (t.v - 1)) lor t.l.v lor t.r.v = 0
