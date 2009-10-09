(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * what if we just have placements in a big list, and split the list?
 * then we could read all of the loc.fasta files into a map.
 *)

open Fam_batteries
open MapsSets

let version_str = "v0.3"
let verbose = false

let out_prefix = ref ""
let verbose = ref false
let ml_cutoff = ref 0.
let re_sep_fname = ref ""
let warn_multiple = ref true

let parse_args () =
  let files  = ref [] in
  let out_prefix_opt = "-o", Arg.Set_string out_prefix,
    "Set the output prefix. Required if there are two or more input files."
  and verbose_opt = "-v", Arg.Set verbose,
    "Verbose output."
  and ml_cutoff_opt = "-l", Arg.Set_float ml_cutoff,
    "ML separation cutoff."
  and re_sep_fname_opt = "--reSepFile", Arg.Set_string re_sep_fname,
    "File name for the regular expression separation file."
  and warn_multiple_opt = "--noWarnMultipleRe", Arg.Clear warn_multiple,
    "Warn if a read name matches several regular expressions."
  in
  let usage =
    "placeutil "^version_str
      ^"\nplaceutil ex1.place ex2.place ... combines place files, filters, then splits them back up again if you want.\n"
  and anon_arg arg =
    files := arg :: !files in
  let args = [out_prefix_opt; verbose_opt; ml_cutoff_opt; re_sep_fname_opt; warn_multiple_opt] in
  Arg.parse args anon_arg usage;
  List.rev !files

    (* note return code of 0 is OK *)
let () =
  if not !Sys.interactive then begin
    let fnames = parse_args () in
    if fnames = [] then exit 0;
    (* parse the placements *)
    let parsed = 
      List.map 
        (fun fname -> Pquery_io.parse_place_file version_str fname)
        fnames
    in
    if !verbose then begin
      print_endline "combining placements...";
      List.iter2
        (fun (_, places) fname ->
          Printf.printf 
            "found %d placements in %s\n" 
            (List.length places)
            fname)
        parsed
        fnames
    end;
    if parsed = [] then exit 0;
    let ref_tree = 
      Base.complete_fold_left
        (fun prev_ref a_ref ->
          if prev_ref <> a_ref then
            failwith "Reference trees not all the same!";
          prev_ref)
        (List.map fst parsed)
    in
    let combined = List.flatten (List.map snd parsed) in
    (* make the out prefix *)
    let out_prefix_complete = 
      (if !out_prefix <> "" then !out_prefix
      else if List.length fnames > 1 then 
        failwith "Please supply an out prefix with the -o option. This is required when there are two or more input files."
      else 
        (* hd: have already checked that fnames isn't [] *)
        Pquery_io.chop_place_extension (List.hd fnames))
    in
    (* write pqueries. infix is a string to put before .place *)
    let write_pqueries prefix pqueries = 
      let placement_out_ch = open_out (prefix^".place") in
      Placeutil_core.warn_about_duplicate_names pqueries;
      Placeutil_core.write_placeutil_preamble 
        placement_out_ch
        version_str
        Sys.argv
        ref_tree;
      Pquery_io.write_by_best_loc
        Placement.ml_ratio
        placement_out_ch 
        pqueries;
      close_out placement_out_ch
    in
    (* function to split up the queries by likelihood ratio and then write them *)
    let process_pqueries prefix pqueries = 
      if !ml_cutoff <> 0. then begin
        (* split them up by ml cutoff *)
        let (below, above) = 
          Placeutil_core.partition_by_cutoff 
            Placement.ml_ratio 
            (!ml_cutoff) 
            pqueries 
        in
        let cutoff_str = 
          Printf.sprintf "%02d" (int_of_float (100. *. !ml_cutoff)) in
        List.iter 
          (fun (which_str, pqs) ->
            write_pqueries (prefix^".L"^which_str^cutoff_str) pqs)
          ["lt",below; "ge",above]
      end
      else
        write_pqueries prefix pqueries
    in 
    (* "main" *)
    if !re_sep_fname = "" then
      if List.length fnames > 1 || !ml_cutoff <> 0. then
        process_pqueries out_prefix_complete combined
      else 
        print_endline "hmm... I don't have to split up by the ML ratio cutoff or regular expressions, and I am not combining any files. so i'm not doing anything."
    else begin
      let re_split_list = 
        Placeutil_core.read_re_split_file (!re_sep_fname) in
      if List.length re_split_list <= 1 then
        failwith "I only found one regular expression split. If you don't want to split by regular expression, just don't use the option."
      else
        List.iter
          (fun (prefix, pqueries) ->
            process_pqueries prefix pqueries)
          (Placeutil_core.separate_pqueries_by_regex 
            re_split_list 
            combined)
    end
  end
