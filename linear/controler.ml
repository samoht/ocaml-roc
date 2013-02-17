module Vector: sig
  type t = float array
  val add: t -> t -> t
  val sub: t -> t -> t
  val scalar: float -> t -> t
  val print : t -> unit
end = struct

  type t = float array

  let add x y =
    assert (Array.length x > 0);
    assert (Array.length x = Array.length y);
    let n = Array.length x in
    let r = Array.create n 0. in
    for i=0 to Array.length x - 1 do
      r.(i) <- x.(i) +. y.(i)
    done;
    r

  let scalar a v =
    Array.map (fun v -> a *. v) v

  let sub x y =
    add x (scalar (-1.) y)

  let print v =
    Printf.printf "| %s |\n"
      (Array.fold_left (fun accu r -> accu ^ (Printf.sprintf "%s " (string_of_float r))) "" v)

end

module Matrix: sig
  type t = float array array
  val mul   : t -> t -> t
  val apply : t -> Vector.t-> Vector.t
end = struct

  type t = float array array

  let row_to_string m i =
    Printf.sprintf "| %s |\n"
      (Array.fold_left (fun accu r -> accu ^ (Printf.sprintf "%s " (string_of_float r))) "" m.(i))

  let print_matrix m =
    for i=0 to Array.length m - 1 do
      print_string (row_to_string m i);
    done

  let assert_matrix u =
    assert (Array.length u > 0);
    assert (Array.length u.(0) > 0)

  let mul u v =
    assert_matrix u;
    assert_matrix v;
    assert (Array.length u.(0) = Array.length v);
    let res = Array.make_matrix (Array.length u) (Array.length v.(0)) 0. in
    for i=0 to Array.length u-1 do
      for j=0 to Array.length v.(0)-1 do
	for k=0 to Array.length u.(0)-1 do
	  res.(i).(j) <- res.(i).(j) +. (u.(i).(k) *. v.(k).(j))
	done
      done
    done;
    res

  let apply u v =
    assert_matrix u;
    assert (Array.length u.(0) = Array.length v);
    let res = Array.make (Array.length v) 0. in
    for i=0 to Array.length u-1 do
      for k=0 to Array.length u.(0)-1 do
        res.(i) <- res.(i) +. (u.(i).(k) *. v.(k))
      done;
    done;
    res

end

let ( + ) = Vector.add
let ( - ) = Vector.sub
let inv   = Vector.scalar (-1.)
let ( * ) = Vector.scalar

let (|*|) = Matrix.mul
let ( ** ) = Matrix.apply

type model = {

  (* Model of the underlying system physics (only valid for locally linear
     systems). *)
  a: Matrix.t;

  (* Model of the actuators *)
  b: Matrix.t;

  (* Model of the sensors *)
  c: Matrix.t;

}

type gains = {

  (* Gains for the controler *)
  k: Matrix.t;

  (* Gains for the observer *)
  l: Matrix.t;

}

module Observer = struct

  type t = {
    (* Estimated state. Could we wrong, but we hope that it will
     eventually converge towards the real value. *)
    state: Vector.t;

    (* last time the observer routine has been called.
       Used to estimate dt in the update rules. *)
    time : float;
  }

  let state t = t.state

  let create ~input =
    let time = Sys.time () in
    let state = input () in
    { state; time }

  let update model gains input t =
    let y = input () in
    let u = inv (gains.k ** t.state) in
    let time = Sys.time () in
    let dt = time -. t.time in
    let state =
      t.state
      +  dt * (
          model.a ** t.state
          + model.b ** u
          + gains.l ** (y - (model.c ** t.state))
        ) in
    { state; time }

end

module System = struct

  type t = {
    model: model;
    gains: gains;
    input: unit -> Vector.t;
  }

  let compute t observer =
    let observer = Observer.update t.model t.gains t.input observer in
    inv (t.gains.k ** (Observer.state observer))

end
