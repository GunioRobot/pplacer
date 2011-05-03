open Subcommand
open Guppy_cmdobjs
open MapsSets

let escape = Base.sqlite_escape
module TIAMR = AlgMap.AlgMapR (Tax_id.OrderedTaxId)

(* if rank is less than the tax rank of ti, then move up the taxonomy until
 * the first time that the tax rank is less than or equal to rank *)
let classify_at_rank td rank ti =
  let rec aux curr_ti =
    if rank >= Tax_taxonomy.get_tax_rank td curr_ti then curr_ti
    else
      aux
        (try Tax_taxonomy.get_ancestor td curr_ti with
        | Tax_taxonomy.NoAncestor _ -> assert(false))
  in
  aux ti

(* apply f to all of the keys and add the results together *)
let keymap_add_by f m =
  List.fold_right
    (fun (k,v) -> (TIAMR.add_by (f k) v))
    (Tax_id.TaxIdMapFuns.to_pairs m)
    TIAMR.M.empty

(* m is a taxid_algmap and this outputs a list of string_arrays, one for each
 * placement *)
let classif_stral td pq rank_map =
  List.fold_left
    (fun accum name ->
      List.rev_append
        (IntMap.fold
           (fun desired_rank rankl accum ->
             List.rev_append
               (List.fold_left
                  (fun accum (ti, p) ->
                    [|
                      name;
                      Tax_taxonomy.get_rank_name td desired_rank;
                      Tax_taxonomy.rank_name_of_tax_id td ti;
                      Tax_id.to_string ti;
                      Printf.sprintf "%g" p;
                    |] :: accum)
                  []
                  rankl)
               accum)
           rank_map
           [])
        accum)
    []
    (Pquery.namel pq)

let classify how criterion n_ranks td pr f =
  try
    List.iter
      (fun pq ->
        let outmap = ref IntMap.empty in
        let m = ref
          (List.fold_right
             (fun p ->
               TIAMR.add_by
                 (how p)
                 (criterion p))
             (Pquery.place_list pq)
             (TIAMR.M.empty))
        in
        for desired_rank=(n_ranks-1) downto 0 do
          m := keymap_add_by (classify_at_rank td desired_rank) !m;
          outmap := IntMap.add
            desired_rank
            (Tax_id.TaxIdMapFuns.to_pairs (!m))
            !outmap
        done;
        f pq (!outmap))
      (Placerun.get_pqueries pr)
  with
    | Placement.No_classif ->
      invalid_arg
        ((Placerun.get_name pr)^" contains unclassified queries!")


(* UI-related *)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit placefile_cmd () as super_placefile

  val use_pp = flag "--pp"
    (Plain (false, "Use posterior probability for our criteria."))
  val csv_out = flag "--csv"
    (Plain (false, "Write .class.csv files containing CSV data."))
  val sqlite_out = flag "--sqlite"
    (Plain (false, "Write .class.sqlite files containing sqlite insert statements."))

  method specl =
    super_refpkg#specl
  @ [
    toggle_flag use_pp;
    toggle_flag csv_out;
    toggle_flag sqlite_out;
  ]

  method desc =
    "outputs classification information in a tabular or SQLite format"
  method usage = "usage: classify [options] placefile[s]"

  method private placefile_action prl =
    let rp = self#get_rp in
    let criterion = if (fv use_pp) then Placement.post_prob else Placement.ml_ratio in
    let td = Refpkg.get_taxonomy rp in
    let n_ranks = Tax_taxonomy.get_n_ranks td in
    let out_func pr =
      if fv csv_out then
        let prn = Placerun.get_name pr in
        let ch = open_out (prn ^ ".class.csv") in
        let close () = close_out ch in
        output_string ch "name,desired_rank,rank,tax_id,likelihood,origin\n";
        close, (fun pq rank_map ->
          let outl = classif_stral td pq rank_map in
          List.iter
            (fun arr -> Printf.fprintf ch "%s,%s\n" (String.concat "," (Array.to_list arr)) prn)
            outl)

      else if fv sqlite_out then
        let prn = Placerun.get_name pr in
        let ch = open_out (prn ^ ".class.sqlite") in
        let close () =
          output_string ch "COMMIT;\n";
          close_out ch
        in
        output_string ch "BEGIN TRANSACTION;\n";
        let place_id = ref 0 in
        close, (fun pq rank_map ->
          incr place_id;
          Printf.fprintf ch
            "INSERT INTO placements VALUES (%d, %s);\n"
            (!place_id)
            (escape prn);
          List.iter
            (fun name -> Printf.fprintf ch
              "INSERT INTO placement_names VALUES (%d, %s);\n"
              (!place_id)
              (escape name))
            (Pquery.namel pq);
          IntMap.iter
            (fun desired_rank rankl ->
              List.iter
                (fun (tax_id, prob) -> Printf.fprintf ch
                  "INSERT INTO placement_probabilities VALUES (%d, %s, %s, %s, %g);\n"
                  (!place_id)
                  (escape (Tax_taxonomy.get_rank_name td desired_rank))
                  (escape (Tax_taxonomy.rank_name_of_tax_id td tax_id))
                  (escape (Tax_id.to_string tax_id))
                  prob)
                rankl)
            rank_map)

      else
        let ch = open_out ((Placerun.get_name pr)^".class.tab") in
        let close () = close_out ch in
        close, (fun pq rank_map ->
          String_matrix.write_padded ch (Array.of_list (classif_stral td pq rank_map)))

    in
    List.iter
      (fun pr ->
        let close, out_func = out_func pr in
        classify Placement.classif criterion n_ranks td pr out_func;
        close ())
      prl

end

