open Subcommand
open Guppy_cmdobjs
open MapsSets
open Tax_id

let stringify = Printf.sprintf "%0.5g"
let compare_fst_classif td (x, _) (y, _) = Tax_taxonomy.compare_classif td x y

let classif_stral td pr (pq, mpem) =
  let name = List.hd (Pquery.namel pq) in
  let stral = TaxIdMap.fold
    (fun _ mpel accum ->
      List.fold_left
        (fun accum (factor, child, n1, d1, n2, d2) ->
          (child, [|
            pr.Placerun.name;
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
  val csv_out = flag "--csv"
    (Plain (false, "Write .class.csv files containing CSV data."))

  method specl =
    super_refpkg#specl
  @ super_sqlite#specl
  @ super_output#specl
  @ [
    toggle_flag use_pp;
    toggle_flag all_mpes;
    toggle_flag csv_out;
  ]

  method desc = "perform bayes factor classification"
  method usage = "usage: bf [options] placefiles"

  method private placefile_action prl =
    let rp = self#get_rp in
    let td = Refpkg.get_taxonomy rp in
    let to_mpeml = Bayes_factor.mpeml_of_refpkg_and_placerun
      ~all_mpes:(fv all_mpes)
      ~use_pp:(fv use_pp)
      rp
    in

    match fvo sqlite_fname with
      | Some _ ->
        let db = self#get_db in
        Sql.check_exec db "BEGIN TRANSACTION";
        let pn_st = Sqlite3.prepare db
          "INSERT INTO placement_names VALUES (?, ?, ?);"
        and pc_st = Sqlite3.prepare db
          "INSERT INTO placement_classifications VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        and pp_st = Sqlite3.prepare db
          "INSERT INTO placement_positions VALUES (?, ?, ?, ?, ?, ?, ?)"
        in
        let insert_mpem pr (pq, mpem) =
          Sql.check_exec db "INSERT INTO placements VALUES (NULL)";
          let place_id = Sqlite3.last_insert_rowid db in
          List.iter
            (fun name -> Sql.bind_step_reset db pn_st [|
                Sql.D.INT place_id;
                Sql.D.TEXT name;
                Sql.D.TEXT pr.Placerun.name;
              |])
            (Pquery.namel pq);
          TaxIdMap.iter
            (fun _ mpel ->
              List.iter
                (fun (factor, child, n1, d1, n2, d2) -> Sql.bind_step_reset db pc_st [|
                  Sql.D.INT place_id;
                  Sql.D.TEXT (Tax_taxonomy.rank_name_of_tax_id td child);
                  Sql.D.TEXT (Tax_id.to_string child);
                  Sql.D.FLOAT factor;
                  Sql.D.FLOAT n1;
                  Sql.D.FLOAT d1;
                  Sql.D.FLOAT n2;
                  Sql.D.FLOAT d2;
                |])
                mpel)
            mpem;
          List.iter
            (fun p -> Sql.bind_step_reset db pp_st [|
              Sql.D.INT place_id;
              Sql.D.INT (Int64.of_int (Placement.location p));
              Sql.D.FLOAT (Placement.ml_ratio p);
              Sql.D.FLOAT (Placement.log_like p);
              Sql.D.FLOAT (Placement.distal_bl p);
              Sql.D.FLOAT (Placement.pendant_bl p);
              Sql.D.TEXT (Tax_id.to_string (Placement.classif p));
            |])
            (Pquery.place_list pq);
        in
        List.iter (fun pr -> List.iter (insert_mpem pr) (to_mpeml pr)) prl;
        Sql.check_exec db "COMMIT";
        Sql.close db

      | None ->
        let to_stral = classif_stral td in
        let stral = Base.map_and_flatten
          (fun pr -> Base.map_and_flatten (to_stral pr) (to_mpeml pr))
          prl
        and ch = self#out_channel in
        if fv csv_out then begin
          Csv.save_out
            ch
            [["origin"; "name"; "rank"; "tax_id"; "bayes_factor";
              "this_posterior"; "this_n_edges";
              "other_posterior"; "other_n_edges"]];
          Csv.save_out ch (List.map Array.to_list stral)
        end else
          String_matrix.write_padded ch (Array.of_list stral)

end
