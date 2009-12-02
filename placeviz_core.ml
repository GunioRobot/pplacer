(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * the actual functionality of placeviz
*)

type tree_fmt = Newick | Phyloxml

open MapsSets
open Fam_batteries

let min_width = 0.5

let width_of_mass mass_width mass = 
  Decor.width (min_width +. mass_width *. mass)

(* writing the .loc.fasta file *)
let write_loc_file fname_base unplaced_seqs placed_map =
  let out_ch = open_out (fname_base^".loc.fasta") in
  let print_pquery_seq pq =
    Printf.fprintf out_ch ">%s\n%s\n" 
      (Pquery.name pq) 
      (Pquery.seq pq)
  in
  if unplaced_seqs <> [] then
    Printf.fprintf out_ch ">unplaced_sequences\n\n";
  List.iter print_pquery_seq unplaced_seqs;
  IntMap.iter
    (fun loc pquery_list ->
      Printf.fprintf out_ch ">placed_at_%d\n\n" loc;
      List.iter print_pquery_seq pquery_list)
    placed_map;
  close_out out_ch


(* writing various tree formats *)
let trees_to_file tree_fmt prefix trees = 
  match tree_fmt with
  | Newick -> Newick.tree_list_to_file trees (prefix^".tre") 
  | Phyloxml -> Phyloxml.tree_list_to_file trees (prefix^".xml") 

let make_zero_leaf decor_list bl name = 
  Gtree.Subtree 
    (Gtree.gtree 
      (Stree.leaf 0)
      (IntMap.add 
        0 
        (new Decor_bark.decor_bark 
          (`Of_bl_name_boot_dlist 
            (Some bl, Some name, None, decor_list)))
        IntMap.empty))

let decor_bark_of_bl bl = 
  new Decor_bark.decor_bark 
    (`Of_bl_name_boot_dlist (Some bl, None, None, []))

(* given a function that takes a location and a list of somethings and returns a
 * (where, tree) list for that location, make a tree containing those extra
 * subtrees given a something map
 *)
let tree_by_map f ref_tree placed_map = 
  Gtree.add_subtrees_by_map
    ref_tree
    (IntMap.mapi f placed_map)

(* tog tree *)
let tog_tree ref_tree placed_map = 
  tree_by_map
    (fun _ ->
      List.map
        (fun pquery ->
          let best = Pquery.best_place Placement.ml_ratio pquery in
          (Placement.distal_bl best,
          make_zero_leaf 
            [ Decor.red ]
            (Placement.pendant_bl best)
            (Pquery.name pquery),
         decor_bark_of_bl)))
    ref_tree
    placed_map

let write_tog_file tree_fmt fname_base ref_tree placed_map = 
  trees_to_file 
    tree_fmt
    (fname_base^".tog") 
    [tog_tree ref_tree placed_map]
        
(* num tree *)
let num_tree bogus_bl ref_tree placed_map = 
  tree_by_map
    (fun loc pqueries ->
      [((Gtree.get_bl ref_tree loc) /. 2.,
      make_zero_leaf 
        [ Decor.red ]
        bogus_bl
        (Printf.sprintf "%d_at_%d" (List.length pqueries) loc),
      decor_bark_of_bl)])
    ref_tree
    placed_map

let write_num_file bogus_bl tree_fmt fname_base ref_tree 
                                                   placed_map = 
  trees_to_file 
    tree_fmt
    (fname_base^".num") 
    [num_tree bogus_bl ref_tree placed_map]

(* sing trees *)
let sing_tree criterion mass_width ref_tree pquery = 
  let pqname = Pquery.name pquery in
  Gtree.add_subtrees_by_map
    ref_tree
    (IntMapFuns.of_pairlist_listly 
      (ListFuns.mapi
        (fun num p -> 
          let mass = criterion p in
          (Placement.location p,
            (Placement.distal_bl p,
            make_zero_leaf 
              [ Decor.red; 
                width_of_mass mass_width mass; ]
              (Placement.pendant_bl p)
              (Printf.sprintf 
                "%s_#%d_M=%g" 
                pqname 
                num
                mass),
            decor_bark_of_bl)))
        (Pquery.place_list pquery)))

let write_sing_file criterion mass_width tree_fmt fname_base ref_tree 
                                           placed_pquery_list = 
  trees_to_file 
    tree_fmt 
    (fname_base^".sing") 
    (List.map 
      (sing_tree criterion mass_width ref_tree) 
      placed_pquery_list)


(* fat trees *)
let fat_tree weighting criterion mass_width decor_ref_tree pr =
  Decor_gtree.add_decor_by_map
    decor_ref_tree
    (IntMap.map
      (fun mass -> 
        [ width_of_mass mass_width mass; ])
      (Mass_map.By_edge.of_placerun weighting criterion pr))

let write_fat_tree weighting criterion fat_width fname_base decor_ref_tree placerun = 
  Phyloxml.named_tree_to_file
    (fname_base^".fat")
    (fat_tree weighting criterion fat_width decor_ref_tree placerun)
    (fname_base^".fat.xml") 


(* bounce trees *)

let bounce_tree white_bg
      weighting criterion mass_width max_bounce decor_ref_tree pr = 
  let gray = if white_bg then Decor.black else Decor.white in
  Decor_gtree.add_decor_by_map
    decor_ref_tree
    (IntMap.map
      (fun (mass, bounce) -> 
        [ width_of_mass mass_width mass;
          if bounce <= max_bounce then
            Decor.color_avg (bounce /. max_bounce) Decor.red gray 
          else
            Decor.orange ])
      (Bounce.weighted_bounce_map_of_pr weighting criterion pr))

let write_bounce_tree white_bg weighting criterion mass_width max_bounce fname_base decor_ref_tree placerun = 
  Phyloxml.named_tree_to_file
    (fname_base^".bounce")
    (bounce_tree white_bg weighting criterion mass_width max_bounce decor_ref_tree placerun)
    (fname_base^".bounce.xml") 
