let command_list () =
  [
    "rppr", [
      "prep_db", (fun () -> new Rppr_prep_db.cmd ());
      "check_refpkg", (fun () -> new Rppr_check_refpkg.cmd ());
      "convexify", (fun () -> new Rppr_convexify.cmd ());
      "ref_tree", (fun () -> new Rppr_ref_tree.cmd ());
      "voronoi", (fun () -> new Rppr_voronoi.cmd ());
      "pdprune", (fun () -> new Rppr_pdprune.cmd ());
      "info", (fun () -> new Rppr_info.cmd ());
      "reroot", (fun () -> new Rppr_reroot.cmd ());
    ];
  ]
