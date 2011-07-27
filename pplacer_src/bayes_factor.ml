open MapsSets
open Tax_id

let flip f x y = f y x
let compose f g a = f (g a)

let last l =
  let rec aux accum = function
    | [] -> accum
    | h :: t -> aux h t
  in
  aux (List.hd l) (List.tl l)

let complete_map_of_refpkg rp =
  let mrcam = Refpkg.get_mrcam rp
  and tree = (Refpkg.get_ref_tree rp).Gtree.stree
  and td = Refpkg.get_taxonomy rp in
  let rec aux complete_acc = function
    | [] -> complete_acc
    | (mrcas, tree) :: rest ->
      let i = Stree.top_id tree in
      let complete_acc' = List.fold_left
        (flip ((flip TaxIdMap.add_listly) i))
        complete_acc
        mrcas
      and mrcas' =
        if IntMap.mem i mrcam then
          Tax_taxonomy.get_lineage td (IntMap.find i mrcam)
        else mrcas
      in
      let rest' = match tree with
        | Stree.Node (_, subtrees) ->
          List.fold_left
            (fun accum subtree -> (mrcas', subtree) :: accum)
            rest
            subtrees
        | Stree.Leaf _ -> rest
      in
      aux complete_acc' rest'
  in
  aux TaxIdMap.empty [[], tree]

let post_prob_or_ml ~use_pp =
  if use_pp then Placement.post_prob
  else Placement.ml_ratio

let posterior_of_placements ~use_pp =
  List.fold_left (flip (compose (+.) (post_prob_or_ml ~use_pp))) 0.

let is_direct_parent_of td parent child =
  try Tax_taxonomy.get_ancestor td child = parent
  with Tax_taxonomy.NoAncestor _ -> false

let mpem_of_pquery ?(all_mpes = false) ?(use_pp = true) td complete_map pq =
  let pl = Pquery.place_list pq in
  let complete_posterior tid = posterior_of_placements
    ~use_pp
    (List.filter
       (compose
          (List.mem tid)
          (compose (Tax_taxonomy.get_lineage td) Placement.classif))
       pl)
  and complete_cardinality tid =
    float_of_int (List.length (TaxIdMap.get tid [] complete_map))
  and tax_ids = List.fold_left
    (flip
       (compose
          (compose TaxIdSet.union TaxIdSet.of_list)
          (compose (Tax_taxonomy.get_lineage td) Placement.classif)))
    TaxIdSet.empty
    pl
  in
  let tax_ids' = List.sort
    (Tax_taxonomy.compare_classif td)
    (TaxIdSet.elements tax_ids)
  in
  let mpem, _ = List.fold_left
    (fun ((mpem, accepted_tids) as accum) tid ->
      if not (TaxIdSet.mem tid accepted_tids) then accum else
        let parent_post = complete_posterior tid
        and parent_card = complete_cardinality tid
        and children = TaxIdSet.filter (is_direct_parent_of td tid) tax_ids in
        let mpel = List.map
          (fun child ->
            let this_post = complete_posterior child
            and this_card = complete_cardinality child in
            let other_post = parent_post -. this_post
            and other_card = parent_card -. this_card in
            let ratio = (this_post /. this_card) /. (other_post /. other_card) in
            (* x <> x implies x is nan. *)
            let ratio' = if ratio <> ratio then infinity else ratio in
            (log ratio') /. (log 2.), child,
            this_post, this_card, other_post, other_card)
          (TaxIdSet.elements children)
        in
        if TaxIdSet.is_empty children then accum else
          let mpel' =
            if all_mpes then mpel else
              [List.fold_left max (List.hd mpel) (List.tl mpel)]
          in
          let accepted_tids' = List.fold_left
            (fun accum (_, tid, _, _, _, _) -> TaxIdSet.add tid accum)
            accepted_tids
            mpel'
          in
          TaxIdMap.add tid mpel' mpem, accepted_tids')
    (TaxIdMap.empty, TaxIdSet.singleton (List.hd tax_ids'))
    tax_ids'
  in
  pq, mpem

let mpeml_of_refpkg_and_placerun ?all_mpes ?use_pp rp pr =
  let complete_map = complete_map_of_refpkg rp
  and td = Refpkg.get_taxonomy rp in
  List.map
    (mpem_of_pquery ?all_mpes ?use_pp td complete_map)
    (Placerun.get_pqueries pr)
