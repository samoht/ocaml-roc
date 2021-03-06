type op = PLUS | TIMES

let string_of_op = function
  | PLUS  -> "+"
  | TIMES -> "*"

type expr =
  | Var of int
  | Cos of t
  | Sin of t
  | Op of t * op * t

and t = {
  mutable valid: bool;
  mutable data : float;
  expr : expr;
}

let dummy = {
  valid = false;
  data  = 0.;
  expr  = Var 0;
}

let rec string_of_expr = function
  | Var v -> Printf.sprintf "v%s" (string_of_int v)
  | Cos v -> Printf.sprintf "cos(%s)" (string_of_t v)
  | Sin v -> Printf.sprintf "sin(%s)" (string_of_t v)
  | Op (v0, v1, v2) ->
    let ll = [string_of_t v0; string_of_op v1; string_of_t v2] in
    String.concat " " ll

and string_of_t t =
  string_of_expr t.expr

let random_var vars =
  Random.int vars

let random_op () =
  match Random.int 2 with
  | 0 -> PLUS
  | _ -> TIMES

let rec random_expr nodes vars =
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

and random nodes vars = {
  valid = false;
  data  = 0.;
  expr  = random_expr nodes vars;
}

module T = Hashtbl.Make(struct
  type node = t
  type t = node
  let compare = compare
  let equal = (=)
  let hash = Hashtbl.hash
end)

let hashcons t =
  let cons = T.create 1024 in
  let make expr = { expr; valid = true; data = 0. } in
  let rec aux t =
    if T.mem cons t then
      T.find cons t
    else
      let t2 = match t.expr with
        | Var i       -> t
        | Cos t       -> make (Cos (aux t))
        | Sin t       -> make (Sin (aux t))
        | Op(t1,o,t2) -> make (Op(aux t1, o, aux t2)) in
      if T.mem cons t2 then
        T.find cons t2
      else begin
        T.add cons t2 t2;
        t2
      end in
  aux t

let eval_op = function
  | PLUS  -> (+.)
  | TIMES -> ( *.)

let rec eval_expr env = function
  | Var i       -> env.(i)
  | Cos t       -> cos (eval env t)
  | Sin t       -> sin (eval env t)
  | Op(t1,o,t2) -> (eval_op o) (eval env t1) (eval env t2)

and eval env t =
  if t.valid then
    t.data
  else
    let data = eval_expr env t.expr in
    t.data  <- data;
    t.valid <- true;
    data

let () =
  let n = (try int_of_string Sys.argv.(1) with _ -> 1000) in

  let a = Array.create n dummy in
  for i = 0 to n-1 do
    a.(i) <- random (100*i) 10
  done;

  let vars = Array.make 10 0. in
  for i = 0 to 9 do
    vars.(i) <- Random.float 1.
  done;

  let result = Array.create n 0. in
  Gc.compact ();
  for i = 0 to n-1 do
    let t1 = Unix.gettimeofday () in
    let _ = eval vars a.(i) in
    let t2 = Unix.gettimeofday () in
    result.(i) <- t2 -. t1;
  done;

  for i = 0 to n-1 do
    Printf.printf "%d, %.3f\n%!" (i * 100) (1000. *. result.(i));
  done
