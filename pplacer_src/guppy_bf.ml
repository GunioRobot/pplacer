open Subcommand
open Guppy_cmdobjs
open MapsSets

let assert_is_prob x = assert(0. <= x && x <= 1.)
let int_div x y = (float_of_int x) /. (float_of_int y)

let bayes_factor posterior prior =
  assert_is_prob posterior;
  assert_is_prob prior;
  log ((posterior /. prior) /. ((1. -. posterior) /. (1. -. prior)))

let edge_bf posteriorl n_possible_edges =
  bayes_factor
    (List.fold_left (+.) 0. posteriorl)
    (int_div (List.length posteriorl) n_possible_edges)

let de_optionize = function
  | Some x -> x
  | None -> assert(false)

class cmd () =
object
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile

  method specl = []

  method desc = "calculates Bayes factors"
  method usage = "usage: bf [options] placefiles"

  method private placefile_action = function
    | [] -> ()
    | prl ->
      List.iter
        (fun pr ->
          let ch = open_out (pr.Placerun.name^".bf") in
          List.iter
            (fun pq ->
              let name = List.hd pq.Pquery.namel in
              Printf.fprintf ch ">%s\n" name;
              let classification_weights =
                List.fold_left
                  (fun m p ->
                    StringMap.add_listly
                      (Tax_id.to_string (de_optionize p.Placement.classif))
                      (de_optionize p.Placement.post_prob)
                      m)
                  StringMap.empty
                  pq.Pquery.place_list
              in
              let n_places = List.length pq.Pquery.place_list in
              StringMap.iter
                (fun tax_name pprobs ->
                  Printf.fprintf ch "%s\t" tax_name;
                  List.iter
                    (fun x ->
                      Printf.fprintf ch "%g\t" x)
                    pprobs;
                  Printf.fprintf ch "\n";
                  Printf.fprintf ch "%g\n" (edge_bf pprobs n_places)
                    )
                classification_weights
            )
            pr.Placerun.pqueries
        )
        prl
end
