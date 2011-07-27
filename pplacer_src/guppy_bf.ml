open Subcommand
open Guppy_cmdobjs
open MapsSets
open Tax_id

let compose f g a = f (g a)

let stringify = Printf.sprintf "%0.5g"
let compare_fst_classif td (x, _) (y, _) = Tax_taxonomy.compare_classif td x y

let classif_stral td (pq, mpem) =
  let name = List.hd (Pquery.namel pq) in
  let stral = TaxIdMap.fold
    (fun _ mpel accum ->
      List.fold_left
        (fun accum (factor, child, n1, d1, n2, d2) ->
          (child, [|
            name;
            Tax_taxonomy.rank_name_of_tax_id td child;
            Tax_id.to_string child;
            stringify factor;
            stringify n1;
            stringify d1;
            stringify n2;
            stringify d2;
          |]) :: accum)
        accum
        mpel)
    mpem
    []
  in
  let stral' = List.sort (compare_fst_classif td) stral in
  List.map snd stral'

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit placefile_cmd () as super_placefile
  inherit sqlite_cmd () as super_sqlite
  inherit output_cmd () as super_output

  val use_pp = flag "--no-pp"
    (Plain (true, "Use ML ratio instead of posterior probability for our criteria."))
  val all_mpes = flag "--all-classifications"
    (Plain (false, "Show all classifications instead of only the best."))

  method specl =
    super_refpkg#specl
  @ super_sqlite#specl
  @ super_output#specl
  @ [
    toggle_flag use_pp;
    toggle_flag all_mpes;
  ]

  method desc = "perform bayes factor classification"
  method usage = "usage: bf [options] placefiles"

  method private placefile_action prl =
    let rp = self#get_rp in
    let td = Refpkg.get_taxonomy rp in
    let mpeml = Base.map_and_flatten
      (Bayes_factor.mpeml_of_refpkg_and_placerun
         ~all_mpes:(fv all_mpes)
         ~use_pp:(fv use_pp)
         rp)
      prl
    in
    let ch = self#out_channel in
    let stral = Base.map_and_flatten (classif_stral td) mpeml in
    String_matrix.write_padded ch (Array.of_list stral)

end
