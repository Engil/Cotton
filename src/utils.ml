let find l key =
  match l with
  | `O list -> List.assoc key list
  | _ -> assert false

let get_all_qp' uri = List.map (Uri.get_query_param uri)

let get_all_qp uri keys = List.fold_right (fun key -> function
    | None -> None
    | Some l -> match Uri.get_query_param uri key with None -> None | Some v -> Some (v :: l)
  ) keys (Some [])
