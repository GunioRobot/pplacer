open Subcommand
open Guppy_cmdobjs

open MapsSets
open Fam_batteries

let tolerance = 1e-3

let map_of_arr a =
  let m = ref IntMap.empty in
  Array.iteri (fun i x -> m := IntMap.add i x (!m)) a;
  !m

class cmd () =
object (self)
  inherit subcommand () as super
  inherit out_prefix_cmd () as super_out_prefix
  inherit mass_cmd () as super_mass
  inherit heat_cmd () as super_heat
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit placefile_cmd () as super_placefile

  val write_n = flag "--write-n"
    (Plain (5, "The number of principal coordinates to write out (default is 5)."))
  val scale = flag "--scale"
    (Plain (false, "Scale variances to one before performing principal components."))
  val multiplier = flag "--multiplier"
    (Formatted (50., "The factor by which we multiply the principal component eigenvectors to get branch thickness. Default: %g."))

  method specl =
    super_out_prefix#specl
    @ super_mass#specl
    @ super_refpkg#specl
    @ super_heat#specl
    @ [
      int_flag write_n;
      toggle_flag scale;
      float_flag multiplier;
    ]

  method desc =
"performs edge principal components"
  method usage = "usage: pca [options] placefiles"

  method private placefile_action prl =
    self#check_placerunl prl;
    let transform, weighting, criterion = self#mass_opts
    and scale = fv scale
    and write_n = fv write_n
    and refpkgo = self#get_rpo
    and out_prefix = fv out_prefix
    in
    let prt = Mokaphy_common.list_get_same_tree prl in
    let t = match refpkgo with
    | None -> Decor_gtree.of_newick_gtree prt
    | Some rp -> Refpkg.get_tax_ref_tree rp
    in
    let data = List.map (Guppy_splitify.splitify_placerun transform weighting criterion) prl
    in
    let (eval, evect) = Pca.gen_pca ~scale ~n_keep:write_n (Array.of_list data)
    in
    let combol = (List.combine (Array.to_list eval) (Array.to_list evect))
    and names = (List.map Placerun.get_name prl)
    in
    Phyloxml.named_gtrees_to_file
      (out_prefix^".xml")
      (List.map
        (fun (eval, evect) ->
          (Some (string_of_float eval),
          super_heat#heat_tree_of_floatim t (map_of_arr evect)))
        combol);
    Guppy_splitify.save_named_fal
      (out_prefix^".rot")
      (List.map (fun (eval, evect) -> (string_of_float eval, evect)) combol);
    Guppy_splitify.save_named_fal
      (out_prefix^".trans")
      (List.combine
        names
        (List.map (fun d -> Array.map (Pca.dot d) evect) data));
    Guppy_splitify.save_named_fal
      (out_prefix^".edgediff")
      (List.combine names data);
    ()





end
