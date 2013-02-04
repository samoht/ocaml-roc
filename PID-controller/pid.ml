type model = {
  kp: float;
  ki: float;
  kd: float;
}

type state = {
  last_time : float;
  last_error: float;
  sum_errors: float;
}

let control model state e =
  let new_time = Sys.time () in
  let dt = new_time -. state.last_time in
  let ie = state.sum_errors +. e *. dt in
  let de = (e -. state.last_error) /. dt in
  let new_state = {
    last_time  = new_time;
    last_error = e;
    sum_errors = ie;
  } in
  let u = model.kp *. e +. model.ki *. ie +. model.kd *. de in
  new_state, u

