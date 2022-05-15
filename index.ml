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

let mulMV v m= List.fold_left addV (zeroV (List.length m)) (List.map2 scaleV m v)

(* A[b1,b2,..,bn] = [Ab1, Ab2, ..., Abn]    *)
let mul a b = List.map (mulMV a) b

(* Transpose of a matrix a *)
(* think of algorithm that doesnt suck*)
let convertLC m =
  let rec aux acc cols indC = match cols with
    | [] -> acc
    | workingCol::remainingColumns ->
      let rec aux2 acc col indR = match col with
      | [] -> acc
      | h::t ->
        aux2 ((h,indR,indC)::acc) t (indR+1) in
      aux ((aux2 [] workingCol 0)@acc) remainingColumns (indC+1) in
  aux [] (List.rev m) 0

let trC (m:matrixC) = List.map (fun (v,r,c) : entry -> (v,c,r)) m

let convertCL m =
  let rec aux acc remaining indC =
    let pos,neg = List.partition (fun (v,r,c) -> indC == c) remaining in
    match neg with
    | [] -> acc
    | neg -> aux ((List.map (fun (v,r,c) -> v) pos)::acc) neg (indC + 1) in
    List.rev (aux [] m 0)

let tr m = convertCL (trC (convertLC m))

let printC m =
    let rec aux remaining indR =
    let pos,neg = List.partition (fun (v,r,c) -> indR == r) remaining in
    let _ = List.iter (fun (v,r,c) -> Printf.printf "%F " v) pos in
    let _ = Printf.printf "\n" in
    match neg with
    | [] -> ()
    | neg ->
      aux neg (indR + 1) in
    aux m 0

let printM m = printC (convertLC m)


let _ =
  let d = [[3.0;0.0;0.0];[0.0;3.0;0.0];[0.0;0.0;3.0]] in
  let m = [[1.0;2.0;3.0];[4.0;5.0;6.0];[7.0;8.0;9.0]] in printM (mul m d)

  (* next up: algorithm for Gauss elimination*)
