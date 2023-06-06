open Byoppl

type 'a distribution = 'a Distribution.t

let gaussian mu sigma = Distribution.gaussian ~mu ~sigma
let bernoulli p = Distribution.bernoulli ~p
let uniform a b = Distribution.uniform ~a ~b
let draw = Distribution.draw
let mean = Distribution.mean

open Ztypes

type prob = { id : int; scores : float array } (* TODO *)

let sample (_prob, d) = Distribution.draw d (* TODO *)

let factor (prob, s) =
  prob.scores.(prob.id) <- prob.scores.(prob.id) +. s (* TODO *)

let observe (prob, d, x) = factor (prob, Distribution.logpdf d x) (* TODO *)

type 'a infer_state = { mutable particles : 'a array; scores : float array }

let infer_importance n (Cnode { alloc; reset; step; copy }) =
  let infer_alloc () =
    {
      (* particles = Array.init n (fun _ -> alloc ());  *)
      particles = Array.make n (alloc ());
      scores = Array.make n 0.;
    }
    (* TODO *)
  in
  let infer_reset state =
    Array.iter reset state.particles;
    Array.fill state.scores 0 n 0. (* TODO *)
  in
  let infer_step state data =
    let values =
      Array.mapi
        (fun i s -> step s ({ id = i; scores = state.scores }, data))
        state.particles
    in
    Distribution.support ~values ~logits:state.scores (* TODO *)
  in
  let infer_copy _ _ = () in
  Cnode
    {
      alloc = infer_alloc;
      reset = infer_reset;
      step = infer_step;
      copy = infer_copy;
    }

let resample alloc copy n state =
  let dist =
    Distribution.support ~values:state.particles ~logits:state.scores
  in
  Array.init n (fun i ->
      let p = alloc () in
      let s = Distribution.draw dist in
      copy s p;
      p)

let infer_pf n (Cnode { alloc; reset; step; copy }) =
  let infer_alloc () =
    { particles = Array.init n (fun _ -> alloc ()); scores = Array.make n 0. }
    (* TODO *)
  in
  let infer_reset state =
    Array.iter reset state.particles;
    Array.fill state.scores 0 n 0. (* TODO *)
  in
  let infer_step state data =
    let values =
      Array.mapi
        (fun i s -> step s ({ id = i; scores = state.scores }, data))
        state.particles
    in
    let v_dist = Distribution.support ~values ~logits:state.scores in
    state.particles <- resample alloc copy n state;
    Array.fill state.scores 0 n 0.;
    v_dist (* TODO *)
  in
  let infer_copy _ _ = () in
  Cnode
    {
      alloc = infer_alloc;
      reset = infer_reset;
      step = infer_step;
      copy = infer_copy;
    }
