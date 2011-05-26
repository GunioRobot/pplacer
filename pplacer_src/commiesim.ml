open Fam_batteries
open MapsSets
open Splits

let size_transform x = x ** 10.

(* Return the counts from throwing n_items into n_bins. *)
let balls_in_boxes rng ~n_bins ~n_items =
  let counts = Array.create n_bins 0 in
  for i=1 to n_items do
    let which_bin = Gsl_rng.uniform_int rng n_bins in
    counts.(which_bin) <- counts.(which_bin) + 1
  done;
  Array.to_list counts

(* Return the counts from throwing n_items into n_bins such that each bin is not
 * empty. *)
let nonempty_balls_in_boxes rng ~n_bins ~n_items =
  assert(n_items >= n_bins);
  let counts = Array.create n_bins 1 in
  for i=1 to n_items - n_bins do
    let which_bin = Gsl_rng.uniform_int rng n_bins in
    counts.(which_bin) <- counts.(which_bin) + 1
  done;
  Array.to_list counts

let lsetset_to_array lss =
  let a = Array.make (Lsetset.cardinal lss) (Lsetset.choose lss) in
  let _ = Lsetset.fold (fun x i -> a.(i) <- x; i+1) lss 0 in
  a

(* Throw the elements of lss into n_bins boxes, ensuring that each box has at
 * least one element.
 * The implementation below could be made more functional and list-y but I don't
 * think that it would be shorter.
 * *)
let uniform_nonempty_partition rng n_bins lss =
  let a = lsetset_to_array lss in
  Base.shuffle rng a;
  let pos = ref 0 in
  List.map
    (fun n_samples ->
      let new_lss =
        Lsetset.of_list (Array.to_list (Array.sub a !pos n_samples)) in
      pos := !pos + n_samples;
      new_lss)
    (nonempty_balls_in_boxes rng ~n_bins ~n_items:(Lsetset.cardinal lss))

exception Invalid_sample of string

type weighting =
  | Array of float array
  | Function of (int -> float)
  | Uniform

(* Sampling with and without replacement.
 * Note that because of the implementation of GSL's discrete distributions, the
 * sum of the elements of an Array weighting need not total to one; it's scaled
 * to make a probability distribution. They do need to be positive. *)
let sample ~replacement rng ?(weighting = Uniform) n k =
  if k > n then raise (Invalid_sample "k > n");
  if k < 0 then raise (Invalid_sample "k < 0");
  let distribution = match weighting with
    | Array a -> a
    | Function f -> Array.init n f
    | Uniform -> Array.make n 1.0
  in
  if k = 0 then []
  else begin
    let discrete_distn = Gsl_randist.discrete_preproc distribution in
    let rec aux_repl accum = function
      | 0 -> accum
      | k ->
        aux_repl ((Gsl_randist.discrete rng discrete_distn) :: accum) (k - 1)
    and aux_no_repl accum = function
      | 0 -> accum
      | k ->
        let i =
          Gsl_randist.discrete
            rng
            (Gsl_randist.discrete_preproc distribution)
        in
        (* clear out i from the distribution (w/o replacement *)
        distribution.(i) <- 0.0;
        aux_no_repl (i :: accum) (k - 1)
    in
    (if replacement then aux_repl else aux_no_repl) [] k
  end

(* We take the weight on a split to be proportional to weight_transform applied
 * to the size of the smaller element of the set. *)
let sample_sset_weighted rng =
  Sset.weighted_sample
    (fun arr -> sample ~replacement:true rng ~weighting:(Array arr))
    (fun (k, _) -> size_transform (float_of_int (Lset.cardinal k)))

let repeat f n =
  let rec aux accum = function
    | 0 -> accum
    | n -> aux ((f n) :: accum) (n - 1)
  in
  aux [] n

let rev_enumerate ?(start = 0) l =
  let _, l' = List.fold_left
    (fun (e, accum) x -> e + 1, (e, x) :: accum)
    (start, [])
    l
  in
  l'

let generate_yule rng count =
  let rec aux next_id trees =
    match List.length trees with
      | 1 -> List.hd trees
      | n ->
        let nodes = sample ~replacement:false rng n 2 in
        let _, trees', nodes' = List.fold_left
          (fun (e, l1, l2) x ->
            if List.mem e nodes then
              e + 1, l1, x :: l2
            else
              e + 1, x :: l1, l2)
          (0, [], [])
          trees
        in
        aux
          (next_id + 1)
          (Stree.node next_id nodes' :: trees')
  in
  aux (count + 1) (repeat Stree.leaf count)


let newick_bark_of_prefixed_int prefix n =
  Newick_bark.map_set_name n (Printf.sprintf "%s%d" prefix n) IntMap.empty

let rec bark_of_stree_numbers bark_fn = function
  | Stree.Leaf n -> bark_fn n
  | Stree.Node (_, subtree) ->
    List.fold_left
      (fun map node ->
        IntMap.union map (bark_of_stree_numbers bark_fn node))
      IntMap.empty
      subtree

let gtree_of_stree_numbers bark_fn stree =
  let bark = bark_of_stree_numbers bark_fn stree in
  Gtree.gtree stree bark

let subselect rng n_select n_bins lss =
  ListFuns.init
    n_bins
    (fun _ ->
      Lsetset.plain_sample
        (sample ~replacement:true rng ~weighting:Uniform)
        lss
        n_select)

let distribute_lsetset_on_tree rng n_select splits leafss gt =
  let bl = Gtree.get_bl gt in
  let name = Gtree.get_name gt in
  let rec aux splits leafss = function
    | Stree.Leaf n ->
      StringMap.add (name n) (int_of_float (bl n), leafss) StringMap.empty
    | Stree.Node (n, subtrees) ->
      (* the splits that actually cut leafss *)
      let cutting_splits = select_sset_cutting_lsetset splits leafss in
      (* sample some number from them *)
      let n_splits = Gsl_randist.poisson rng (bl n) in
      Printf.printf "using %d splits\n" n_splits;
      let chosen_splits = sample_sset_weighted rng cutting_splits n_splits in
      (* apply these splits *)
      let cut_leafss = sset_lsetset chosen_splits leafss in
      (* throw the balls (leafs) into boxes (subtrees) *)
      let distributed =
        subselect rng n_select (List.length subtrees) cut_leafss
      in
      List.fold_left2
        (fun map leafss t -> StringMap.union map (aux cutting_splits leafss t))
        StringMap.empty
        distributed
        subtrees
  in
  aux splits leafss gt.Gtree.stree

let pquery_of_leaf_and_seq leaf seq =
  Pquery.make_ml_sorted
    [Printf.sprintf "%d_%d" leaf seq]
    ""
    [
      Placement.make_ml
        leaf
        ~ml_ratio:1.0
        ~dist_bl:0.0
        ~pend_bl:1.0
        ~log_like:0.0
    ]

let write_random_pr rng tree leafl name n_pqueries =
  let distribute_pqueries = Gsl_randist.multinomial rng ~n:n_pqueries in
  let pqueries =
    List.map2
      (fun leaf -> repeat (pquery_of_leaf_and_seq leaf))
      leafl
      (Array.to_list
        (distribute_pqueries (Array.make (List.length leafl) 1.0)))
  in
  Placerun_io.to_json_file
    ""
    (name^".json")
    (Placerun.make tree name (List.flatten pqueries))

let main
    rng
    ?(retries = 100)
    ~n_select
    ~cluster_tree
    ~n_pqueries
    ~tree
    name_prefix =

  let stree = Gtree.get_stree tree
  and cluster_stree = Gtree.get_stree cluster_tree in
  let splits = sset_of_tree stree
  and leafs = lset_of_tree stree
  in

  let rec retry = function
    | 0 -> failwith "failed too many resamplings"
    | n ->
      try
        distribute_lsetset_on_tree
          rng
          n_select
          splits
          (Lsetset.singleton leafs)
          cluster_tree
      with
        | Invalid_sample _ -> retry (n - 1)
  in
  let leaf_map = retry retries in

  StringMap.iter
    (fun name (multiplier, leafss) ->
      List.iter
        (fun i ->
          let leafs = Lsetset.fold Lset.union leafss Lset.empty in
          write_random_pr
            rng
            tree
            (IntSet.elements leafs)
            (Printf.sprintf "%s%s_%d" name_prefix name i)
            n_pqueries)
        (Base.range multiplier))
    leaf_map;

  (* okay it finally got to me *)
  let next_id = ref (Gtree.top_id cluster_tree)
  and new_bark = ref (Gtree.get_bark_map cluster_tree) in
  let rec aux = function
    | Stree.Node (i, subtrees) -> Stree.node i (List.map aux subtrees)
    | Stree.Leaf i as original ->
      let mult = int_of_float (Gtree.get_bl cluster_tree i) in
      assert(mult >= 1);
      if mult = 1 then original
      else begin
        let subtree =
          Stree.node i
            (List.map
               (fun e ->
                 let old_name = Gtree.get_name cluster_tree i in
                 incr next_id;
                 new_bark :=
                   Newick_bark.map_set_name
                   (!next_id)
                   (Printf.sprintf "%s_%d" old_name e)
                   (!new_bark);
                 Stree.leaf (!next_id))
               (Base.range mult))
        in
        new_bark := IntMap.remove i !new_bark;
        subtree
      end
  in
  let cluster_stree' = aux cluster_stree in
  let new_bark' = !new_bark in
  Gtree.gtree cluster_stree' new_bark'

let random_colored_tree rng size n_colors =
  let st = generate_yule rng size in
  let colors =
    StringSet.of_list
      (List.map
         (fun i -> String.make 1 (char_of_int (i + 65)))
         (Base.range n_colors))
  in
  let choose_color =
    StringSet.plain_sample (sample ~replacement:false rng) colors
  in
  let rec aux accum = function
    | Stree.Leaf i :: rest ->
      let accum' =
        Newick_bark.map_set_name
          i
          (StringSet.choose (choose_color 1)) accum
      in
      aux accum' rest
    | Stree.Node (_, subtrees) :: rest ->
      let rest' = List.rev_append subtrees rest in
      aux accum rest'
    | [] -> accum
  in
  let bark = aux IntMap.empty [st] in
  Gtree.gtree st bark
