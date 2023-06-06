module Rejection_sampling = struct

  type prob = Prob

  exception Reject
  let sample _prob d = Distribution.draw d
  let factor _prob _ = assert false

  let assume _prob p = if not p then raise Reject

  let observe prob d v  = 
    let y = sample prob d in
    assume prob (y = v)
  
  let infer ?(n=1000) model data = 
    let rec exec i = try model Prob data with Reject -> exec i in
    let values = Array.init n exec in
    Distribution.uniform_support ~values
end

module Importance_sampling = struct
  type prob = {id: int; scores: float Array.t}

  let sample _prob d = Distribution.draw d
  let factor prob s = prob.scores.(prob.id) <- prob.scores.(prob.id) +. s
  let assume prob p = factor prob (if p then 0. else -. infinity)
  let observe prob d v = factor prob (Distribution.logpdf d v)
  let infer ?(n=1000) model data = 
    let scores = Array.make n 0. in
    let values = Array.mapi (fun i _ -> model { id=i; scores } data) scores in
    Distribution.support ~values ~logits:scores
end

module Enumeration = struct

  exception Reject 
  exception Error


  type prob = { score: float }


  (* Using continuation c, it returns a list with values and their scores for a single run*)
  let single_sample v logpdf c prob =
    let prob = { 
      score = prob.score +. logpdf 
                } 
      in
    c v prob
  
  let sample d c prob =
    let support =
      Distribution.get_support ~shrink:true d (*handling the duplicated values*)
    in
    (*compute the result and its prob for each distribution issue, optimize by merging the issue which has same value*)
    let _, scores = Array.fold_left
        (fun (i, x1) v ->
           try
             let logpdf = support.logits.(i) in
             let x2 = single_sample v logpdf c prob in
             (i + 1, List.rev_append x1 x2)
           with 
              Reject -> (i + 1, x1)
        )
        (0, [])
        support.values
    in
    scores

  let assume p c prob = 
    if not p 
      then raise Reject 
    else c () prob


  let observe d v c prob =
    let score = prob.score +. (Distribution.logpdf d v) in
    c () { score }

  let infer model data =
    let init_prob = { score= 0.0 } in
    let c = (fun v prob -> [v, prob.score]) in
    let scores = model data c init_prob in
    let scores = Array.of_list scores in
    (*the of_list defined in utils to compute the list values*)
    let values, logits = Array.split scores in
    Distribution.support ~values ~logits

end