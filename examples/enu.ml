open Byoppl
open Distribution
open Cps_operators
open Basic.Enumeration
open Owl_plplot
open Owl


(*setting up testing enviroment*)
let test_bias str model data =
  Format.printf "@.-- %s, CPS Enumeration --@." str;
  let dist = infer model data in
  let m, s = Distribution.stats dist in
  Format.printf "%s bias, mean: %f std:%f@." str m s

let test_dist str model data =
  Format.printf "@.-- %s, CPS Enumeration --@." str;
  let dist = infer model data in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  let length = Array.length values in
  if length <= 1 then ()
  else
    let h = Plot.create ("enumeration_" ^ str ^ ".png") in
    let x = Mat.of_array (Array.map Float.of_int values) 1 length in
    let y = Mat.of_array probs 1 length in
    Plot.set_yrange h 0. 1.;
    Plot.set_ylabel h "Probabilities";
    Plot.set_xlabel h "Values";
    Plot.set_title h ("Probability distribution for " ^ str);
    Plot.stem ~h ~spec:[ Marker "#[0x2295]"; MarkerSize 5.; LineStyle 1 ] x y;
    Plot.output h


(*a simple example*)
let simple_example () =
  let* x = sample (dirac ~v:7) in
  let* y = sample (bernoulli ~p:0.8) in
  if (x + y) = 10 then
    let* z = sample (dirac ~v:3) in
    return (x + y + z)
  else
    return (x - y)

let _ =
  test_dist "Simple Example" simple_example ()


(*on examples*)

let coin_dist () =
  sample (bernoulli ~p:0.5)

let _ =
  test_dist "One coin" coin_dist()

let coin2_ber_dist () =
  let* n1 = sample (bernoulli ~p:0.5) in
  let* n2 = sample (bernoulli ~p:0.5) in
  return (n1 + n2)

let _ = 
  test_dist "Two coins bernoulli" coin2_ber_dist ()

let coin2_bi_dist () =
  sample (binomial ~p:0.5 ~n:2)

let _ =
  test_dist "Two coins binomial" coin2_bi_dist ()

let coin_bias data =
  let* z = sample (uniform_support ~values:(Array.init 101 (fun x -> x))) in
  let z = Float.of_int z in
  let z = z /. 100. in
  let* () = Cps_list.iter (observe (bernoulli ~p:z)) data in
  return z

let _ =
  test_bias "Coin" coin_bias [ 0; 1; 1; 0; 0; 0; 0; 0; 0; 0 ]

let coin_rec_bias data =
  let* z = sample (uniform_support ~values:(Array.init 101 (fun x -> x))) in
  let z = Float.of_int z in
  let z = z /. 100. in
  let rec aux data =
    match data with
    | [] -> return ()
    | h :: t ->
      let* () = observe (bernoulli ~p:z) h in aux t
  in
  let* () = aux data in
  return z

let _ =
  test_bias "Coin rec" coin_rec_bias [ 0; 1; 1; 0; 0; 0; 0; 0; 0; 0 ]

let coin_funny_bias data =
  let* z = sample (uniform_support ~values:(Array.init 101 (fun x -> x))) in
  let z = Float.of_int z in
  let z = z /. 100. in
  let* direction = sample (bernoulli ~p:0.5) in
  let rec aux data =
    match data with
    | [] -> return ()
    | h :: t ->
      let* () = observe (bernoulli ~p:z) h in aux t
  in
  let data = if direction = 1 then data else List.rev data in
  let* () = aux data in
  return z

let _ =
  test_bias "Coin funny" coin_funny_bias [ 0; 1; 1; 0; 0; 0; 0; 0; 0; 0 ]



let funny_bernoulli () =
  let* a = sample (bernoulli ~p:0.5) in
  let* b = sample (bernoulli ~p:0.5) in
  let* c = sample (bernoulli ~p:0.5) in
  let* () = assume (a = 1 || b = 1) in
  return (a + b + c)

let _ =
  test_dist "Funny Bernoulli" funny_bernoulli ()


let sum_dice_one_odd () =
  let* d1 = sample (uniform_support ~values:(Array.init 6 (fun i -> i))) in
  let* d2 = sample (uniform_support ~values:(Array.init 6 (fun i -> i))) in
  let* () = assume (d1 mod 2 = 1 || d2 mod 2 = 1) in
  return (d1 + d2)

let _ =
  test_dist "Sum dice one odd" sum_dice_one_odd ()

let sum_dice_all_odd () =
  let* d1 = sample (uniform_support ~values:(Array.init 6 (fun i -> i))) in
  let* d2 = sample (uniform_support ~values:(Array.init 6 (fun i -> i))) in
  let* () = assume (d1 mod 2 = 1 && d2 mod 2 = 1) in
  return (d1 + d2)

let _ =
  test_dist "Sum dice all odd" sum_dice_all_odd ()

let dice () =
  let rec gen x =
    let* c = sample (bernoulli ~p:0.5) in
    match x with
    | 0 -> gen (if c = 1 then 1 else 2)
    | 1 -> gen (if c = 1 then 3 else 4)
    | 2 -> gen (if c = 1 then 5 else 6)
    | 3 -> gen (if c = 1 then 1 else 11)
    | 4 -> gen (if c = 1 then 12 else 13)
    | 5 -> gen (if c = 1 then 14 else 15)
    | 6 -> gen (if c = 1 then 16 else 2)
    | _ -> return x
  in
  gen 0

let _ =
  let dist = infer dice () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %d %f@." i x probs.(i)) values

  (*aknowledgement : Many thanks for Charles for helping me out of this project, telling me some technical details of Ocaml e.g. what plotting method I should use.*)


