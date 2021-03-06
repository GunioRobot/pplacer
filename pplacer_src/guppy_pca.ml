open Ppatteries
open Subcommand
open Guppy_cmdobjs

let save_named_fal fname fal =
  Csv.save fname (Guppy_splitify.fal_to_strll fal)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd ~show_fname:false ~prefix_required:true () as super_output
  inherit mass_cmd () as super_mass
  inherit heat_cmd () as super_heat
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit placefile_cmd () as super_placefile

  val write_n = flag "--write-n"
    (Formatted (5, "The number of principal coordinates to write out (default is %d)."))
  val scale = flag "--scale"
    (Plain (false, "Scale variances to one before performing principal components."))
  val symmv = flag "--symmv"
    (Plain (false, "Use a complete eigendecomposition rather than power iteration."))
  val raw_eval = flag "--raw-eval"
    (Plain (false, "Output the raw eigenvalue rather than the fraction of variance."))

  method specl =
    super_output#specl
    @ super_mass#specl
    @ super_refpkg#specl
    @ super_heat#specl
    @ [
      int_flag write_n;
      toggle_flag scale;
      toggle_flag symmv;
    ]

  method desc =
"performs edge principal components"
  method usage = "usage: pca [options] placefiles"

  method private placefile_action prl =
    self#check_placerunl prl;
    let weighting, criterion = self#mass_opts
    and scale = fv scale
    and write_n = fv write_n
    and refpkgo = self#get_rpo
    and prefix = self#single_prefix ~requires_user_prefix:true ()
    in
    let prt = Mokaphy_common.list_get_same_tree prl in
    let t = match refpkgo with
    | None -> Decor_gtree.of_newick_gtree prt
    | Some rp -> Refpkg.get_tax_ref_tree rp
    in
    let data =
      List.map
        (Guppy_splitify.splitify_placerun weighting criterion)
        prl
    in
    let (eval, evect) =
      Pca.gen_pca ~use_raw_eval:(fv raw_eval)
                  ~scale ~symmv:(fv symmv) write_n (Array.of_list data)
    in
    let combol = (List.combine (Array.to_list eval) (Array.to_list evect))
    and names = (List.map Placerun.get_name prl)
    in
    Phyloxml.named_gtrees_to_file
      (prefix^".xml")
      (List.map
        (fun (eval, evect) ->
          (Some (string_of_float eval),
          super_heat#heat_tree_of_float_arr t evect))
        combol);
    save_named_fal
      (prefix^".rot")
      (List.map (fun (eval, evect) -> (string_of_float eval, evect)) combol);
    save_named_fal
      (prefix^".trans")
      (List.combine
        names
        (List.map (fun d -> Array.map (Pca.dot d) evect) data));
    save_named_fal
      (prefix^".edgediff")
      (List.combine names data);
    ()

end
