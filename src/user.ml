type t =
  { name : string;
    email : string;
    password : string }

let user name email password =
  {name; email; password }

let of_json json =
  let name = Utils.find json "name" |> Ezjsonm.get_string in
  let email = Utils.find json "email" |> Ezjsonm.get_string in
  let password  = Utils.find json "password" |> Ezjsonm.get_string in
  {name; email; password}

let to_json_response user =
  let open Ezjsonm in
  dict [
    "name", (string user.name);
    "email", (string user.email);
  ]

let to_json_content user =
  let open Ezjsonm in
  dict [
    "name", (string user.name);
    "email", (string user.email);
    "password", (string user.password);
  ]
