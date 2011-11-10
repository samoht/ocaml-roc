type op = PLUS | TIMES

let string_of_op = function
  | PLUS -> "PLUS"
  | TIMES -> "TIMES"

type t =
  | Var of int
  | Cos of t
  | Sin of t
  | Op of t * op * t

let rec string_of_t = function
  | Var v -> Printf.sprintf "Var %s" (string_of_int v)
  | Cos v -> Printf.sprintf "Cos %s" (string_of_t v)
  | Sin v -> Printf.sprintf "Sin %s" (string_of_t v)
  | Op (v0, v1, v2) ->
    let ll = [string_of_t v0; string_of_op v1; string_of_t v2] in
    Printf.sprintf "Op (%s)" (String.concat ", " ll)

let random_var vars =
  Random.int vars

let random_op () =
  match Random.int 2 with
  | 0 -> PLUS
  | _ -> TIMES

let rec random nodes vars =
  if nodes <= 1 then
    Var (random_var vars)
  else begin
    match Random.int 3 with
    | 0 -> Cos (random (nodes-1) vars)
    | 1 -> Sin (random (nodes-1) vars)
    | _ ->
      let left = nodes / 2 in
      let right = nodes - left in
      Op (random left vars, random_op(), random right vars)
  end

let eval_op = function
  | PLUS  -> (+.)
  | TIMES -> ( *.)

let rec eval env = function
  | Var i       -> env.(i)
  | Cos t       -> cos (eval env t)
  | Sin t       -> sin (eval env t)
  | Op(t1,o,t2) -> (eval_op o) (eval env t1) (eval env t2)

let () =
  let n = (try int_of_string Sys.argv.(1) with _ -> 1000) in

  let a = Array.create n (Var 0) in
  for i = 0 to n-1 do
    a.(i) <- random (100*i) 10
  done;

  let vars = Array.make 10 0. in
  for i = 0 to 9 do
    vars.(i) <- Random.float 1.
  done;

  let result = Array.create n 0. in
  for i = 0 to n-1 do
    let t1 = Unix.gettimeofday () in
    let _ = eval vars a.(i) in
    let t2 = Unix.gettimeofday () in
    result.(i) <- t2 -. t1;
  done;

  for i = 0 to n-1 do
    Printf.printf "%d %.3f\n%!" (i * 100) (1000. *. result.(i));
  done
