(* this is a module for a generalized likelihood vector, which is
 * actually an across-site collection of across-rate collections of likelihood
 * vectors.
 * this abstraction layer means that we should be able to treat any glv like it
 * was a simple likelihood vector.
 *
 * i wanted originally to strictly conform to a very general module signature
 * which would be universal across all types of likelihood type vectors.
 * however, it is necessary to pass some rate information in functions like
 * evolve_into. i think i could have done so by specifying some
 * extra_information type in the signature, but it didn't seem worth it given
 * that i don't know what other sorts of data i would want to support.
 *
 * this is designed to avoid underflows in most cases when doing phylogenetics
 * calculation, although such behavior is not guaranteed.
 *
 * the key detail is that a base-two exponent is stored in the e field.
 * "pulling" the exponent refers to finding a suitable exponent x and then
 * dividing all of the entries of the site likelihood vectors by 2^x to bring
 * them back in line, then storing x in field e.
 *
 * The first index in the tensor is the gamma rate, the second is the site
 * index, and the third is the state index.
 *)

open Ppatteries

module BA = Bigarray
module BA1 = BA.Array1
module BA2 = BA.Array2
module BA3 = BA.Array3

let log_of_2 = log 2.

(* integer big arrays *)
let iba1_create = BA1.create BA.int BA.c_layout
let iba1_mimic a = iba1_create (BA1.dim a)
let iba1_copy a = let b = iba1_mimic a in BA1.blit a b; b
let iba1_to_array a =
  let arr = Array.make (BA1.dim a) 0 in
  for i=0 to (BA1.dim a)-1 do arr.(i) <- a.{i} done;
  arr
let iba1_ppr ff a = Ppr.ppr_int_array ff (iba1_to_array a)
let iba1_pairwise_sum dest x y =
  let n = BA1.dim x in
  assert(n = BA1.dim y && n = BA1.dim dest);
  for i=0 to n-1 do
    BA1.unsafe_set
      dest i ((BA1.unsafe_get x i) + (BA1.unsafe_get y i))
  done

(* glvs *)
type glv = { e : (int, BA.int_elt, BA.c_layout) BA1.t;
             a : Tensor.tensor; }

let get_n_rates g = Tensor.dim1 g.a
let get_n_sites g =
  let n = Tensor.dim2 g.a in
  assert(n = BA1.dim g.e);
  n
let get_n_states g = Tensor.dim3 g.a

let dims g = (get_n_rates g, get_n_sites g, get_n_states g)

let ppr ff g =
  Format.fprintf ff "@[{ e = %a; @,a = %a }@]"
    iba1_ppr g.e
    Tensor.ppr g.a

let make ~n_rates ~n_sites ~n_states =
  { e = iba1_create n_sites;
    a = Tensor.create n_rates n_sites n_states; }


(* make a glv of the same dimensions *)
let mimic x =
  { e = iba1_mimic x.e;
    a = Tensor.mimic x.a; }

(* deep copy *)
let copy x =
  { e = iba1_copy x.e;
    a = Tensor.copy x.a; }

let memcpy ~dst ~src =
  BA1.blit src.e dst.e;
  BA3.blit src.a dst.a

(* set all of the entries of the glv to some float *)
let set_exp_and_all_entries g e x =
  BA1.fill g.e e;
  BA3.fill g.a x

let set_all g ve va =
  BA1.fill g.e ve;
  Tensor.set_all g.a va

(* Find the "worst" fpclass of the floats in g. *)
let fp_classify g =
  Tensor.fp_classify g.a

(* set g according to function fe for exponenent and fa for entries *)
let seti g fe fa =
  let n_sites = get_n_sites g
  and n_rates = get_n_rates g
  and n_states = get_n_states g in
  for site=0 to n_sites-1 do
    for rate=0 to n_rates-1 do
      for state=0 to n_states-1 do
        g.a.{rate,site,state} <- fa ~rate ~site ~state
      done
    done;
    g.e.{site} <- fe site
  done

(* copy the site information from src to dst. _i is which site to copy. *)
let copy_site ~src_i ~src ~dst_i ~dst =
  (dst.e).{dst_i} <- (src.e).{src_i};
  for rate=0 to (get_n_rates src)-1 do
    BA1.blit (BA3.slice_left_1 src.a rate src_i)
             (BA3.slice_left_1 dst.a rate dst_i)
  done

(* copy the sites marked with true in site_mask_arr from src to dst. the number
 * of trues in site_mask_arr should be equal to the number of sites in dst. *)
let mask_into site_mask_arr ~src ~dst =
  let dst_n_sites = get_n_sites dst in
  let dst_i = ref 0 in
  Array.iteri
    (fun src_i b ->
      if b then begin
        assert(!dst_i < dst_n_sites);
        copy_site ~src ~src_i ~dst_i:(!dst_i) ~dst;
        incr dst_i;
      end)
    site_mask_arr;
  assert(!dst_i = dst_n_sites)

(* this is used when we have a pre-allocated GLV and want to fill it with a
 * same-length lv array. zero pulled exponents as well. *)
let prep_constant_rate_glv_from_lv_arr g lv_arr =
  assert(lv_arr <> [||]);
  assert(get_n_sites g = Array.length lv_arr);
  assert(get_n_states g = Gsl_vector.length lv_arr.(0));
  seti g
       (fun _ -> 0)
       (fun ~rate:_ ~site ~state ->
         lv_arr.(site).{state})

(* this is used when we want to make a glv out of a list of likelihood vectors.
 * differs from below because we want to make a new one.
 * *)
let lv_arr_to_constant_rate_glv n_rates lv_arr =
  assert(lv_arr <> [||]);
  let g = make ~n_rates
               ~n_sites:(Array.length lv_arr)
               ~n_states:(Gsl_vector.length lv_arr.(0)) in
  prep_constant_rate_glv_from_lv_arr g lv_arr;
  g


(* *** pulling exponent *** *)

(* gets the base two exponent *)
let get_twoexp x = snd (frexp x)

(* makes a float given a base two exponent. we use 0.5 because:
# frexp (ldexp 1. 3);;
- : float * int = (0.5, 4)
so that's how ocaml interprets 2^i anyway.
*)
let of_twoexp i = ldexp 0.5 (i+1)

(* pull out the exponent if it's below min_allowed_twoexp and return it. this
 * process is a bit complicated by the fact that we are partitioned by rate, as
 * can be seen below. *)
let perhaps_pull_exponent min_allowed_twoexp g =
  let n_rates = get_n_rates g
  and n_sites = get_n_sites g in
  let max_twoexp = ref (-max_int) in
  (* cycle through sites *)
  for site=0 to n_sites-1 do
    max_twoexp := (-max_int);
    (* first find the max twoexp *)
    for rate=0 to n_rates-1 do
      let s = BA3.slice_left_1 g.a rate site in
      let (_, twoexp) = frexp (Gsl_vector.max s) in
      if twoexp > !max_twoexp then max_twoexp := twoexp
    done;
    (* now scale if it's needed *)
    if !max_twoexp < min_allowed_twoexp then begin
      for rate=0 to n_rates-1 do
        (* take the negative so that we "divide" by 2^our_twoexp *)
        Gsl_vector.scale
          (BA3.slice_left_1 g.a rate site)
          (of_twoexp (-(!max_twoexp)));
      done;
      (* bring the exponent out *)
      g.e.{site} <- g.e.{site} + !max_twoexp;
    end
  done


(* *** likelihood calculations *** *)

(* total all of the stored exponents. we use a float to avoid overflow. *)
let total_twoexp g =
  let tot = ref 0. in
  for i=0 to (get_n_sites g)-1 do
    tot := !tot +. float_of_int (BA1.unsafe_get g.e i)
  done;
  !tot

(* total all of the stored exponents in a specified range. *)
let bounded_total_twoexp g start last =
  let tot = ref 0. in
  for i=start to last do
    tot := !tot +. float_of_int (BA1.unsafe_get g.e i)
  done;
  !tot

(* take the log like of the product of three things then dot with the stationary
 * distribution. *)
let log_like3 utilv_nsites model x y z =
  assert(dims x = dims y && dims y = dims z);
  (Linear.log_like3 (Model.statd model)
                    x.a
                    y.a
                    z.a
                    utilv_nsites)
    +. (log_of_2 *.
        ((total_twoexp x) +. (total_twoexp y) +. (total_twoexp z)))

(* the log "dot" of the likelihood vectors in the 0-indexed interval
 * [start,last] *)
let bounded_logdot utilv_nsites x y start last =
  assert(dims x = dims y);
  assert(start >= 0 && start <= last && last < get_n_sites x);
  (Linear.bounded_logdot
    x.a y.a start last utilv_nsites)
    +. (log_of_2 *. ((bounded_total_twoexp x start last) +.
                     (bounded_total_twoexp y start last)))

(* just take the log "dot" of the likelihood vectors *)
let logdot utilv_nsites x y =
  bounded_logdot utilv_nsites x y 0 ((get_n_sites x)-1)

(* multiply by a tensor *)
let tensor_mul tensor ~dst ~src =
  (* iter over rates *)
  for i=0 to (Tensor.dim1 src.a)-1 do
    let src_mat = BA3.slice_left_2 src.a i
    and evo_mat = BA3.slice_left_2 tensor i
    and dst_mat = BA3.slice_left_2 dst.a i
    in
    Linear.gemmish dst_mat evo_mat src_mat
  done

(* evolve_into:
 * evolve src according to model for branch length bl, then store the
 * results in dst.
 *)
let evolve_into model ~dst ~src bl =
  (* copy over the exponents *)
  BA1.blit src.e dst.e;
  (* prepare the matrices in our matrix cache *)
  Model.prep_tensor_for_bl model bl;
  (* iter over rates *)
  tensor_mul (Model.tensor model) ~dst ~src

(* take the pairwise product of glvs g1 and g2, then store in dest. *)
let pairwise_prod ~dst g1 g2 =
  assert(dims g1 = dims g2);
  iba1_pairwise_sum dst.e g1.e g2.e;
  Linear.pairwise_prod dst.a g1.a g2.a

(* take the pairwise product of glvs g1 and g2, incorporating the stationary
 * distribution, then store in dest. *)
let statd_pairwise_prod model ~dst g1 g2 =
  assert(dims g1 = dims g2);
  iba1_pairwise_sum dst.e g1.e g2.e;
  Linear.statd_pairwise_prod (Model.statd model) dst.a g1.a g2.a

(* take the product of all of the GLV's in the list, then store in dst.
 * could probably be implemented more quickly, but typically we are only taking
 * pairwise products anyway. we pull out the x::y below to optimize for that
 * case. *)
let listwise_prod dst = function
  | x::y::rest ->
      (* first product of first two *)
      pairwise_prod ~dst x y;
      (* now take product with each of the rest *)
      List.iter (pairwise_prod ~dst dst) rest
  | [src] ->
      (* just copy over *)
      memcpy ~dst ~src
  | [] -> assert(false)


(* For verification purposes. *)

let get_a g ~rate ~site ~state = BA3.get g.a rate site state

let slow_log_like3 model x y z =
  let f_n_rates = float_of_int (Model.n_rates model)
  and ll_tot = ref 0.
  and statd = Model.statd model
  in
  for site=0 to (get_n_sites x)-1 do
    let site_like = ref 0. in
    for rate=0 to (get_n_rates x)-1 do
      for state=0 to (get_n_states x)-1 do
        site_like := !site_like +.
          statd.{state}
            *. (get_a x ~rate ~site ~state)
            *. (get_a y ~rate ~site ~state)
            *. (get_a z ~rate ~site ~state)
      done;
    done;
    if 0. >= !site_like then
      failwith (Printf.sprintf "Site %d has zero likelihood." site);
    ll_tot := !ll_tot
      +. log(!site_like /. f_n_rates)
      +. log_of_2 *.
           (float_of_int (x.e.{site} + y.e.{site} + z.e.{site}))
  done;
  !ll_tot
