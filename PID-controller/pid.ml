(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

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

