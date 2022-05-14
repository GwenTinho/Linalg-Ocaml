(* Real numbers for now *)
type vector = float list

let zeroV n =
  let rec aux c l = match c with
    | 0 -> l
    | c -> aux (c-1) (0.0::l) in
  aux n []

let addV = List.map2 (+.)
let subV = List.map2 (-.)
let scaleV k = List.map (Float.mul k)

let dotV = List.fold_left2 (fun acc a b -> acc +. a *. b) 0.0

(*matrix as entry list is smarter! TODO*)

type matrix = vector list
type entry = float * int * int
type matrixC = entry list

let addM = List.map2 addV
let subM = List.map2 subV

let scaleM k = List.map scaleV

let mulMV m v= List.fold_left addV (zeroV (List.length m)) (List.map2 scaleV m v)

(* A[b1,b2,..,bn] = [Ab1, Ab2, ..., Abn]    *)
let mul a b = List.map (mulMV a) b

(* Transpose of a matrix a *)
(* think of algorithm that doesnt suck*)
let convertLC m =
  let rec aux acc cols indR indC = match cols with
    | [] -> acc
    | h::t -> match h with
      | [] -> aux acc t 0 (indC+1)
      | h::t -> aux ((h,indR,indC)::acc) cols (indR+1) indC in
  aux [] m 0 0

let trC (m:matrixC) = List.map (fun (v,r,c) : entry -> (v,c,r)) m

(*Idea: sort by row, then by column*)
