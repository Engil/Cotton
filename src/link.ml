type t = { url : string;
           author : string;
           uuid : string }

let link url author uuid =
  {url; author; uuid}

let of_json uuid json =
  let author = Utils.find json "author" |> Ezjsonm.get_string in
  let url = Utils.find json "url" |> Ezjsonm.get_string in
  {author; url; uuid}

let to_json_response link =
  let open Ezjsonm in
  dict [
    "author", (string link.author);
    "url", (string link.url);
    "uuid", (string link.uuid)
  ]

let to_json_content link =
  let open Ezjsonm in
  dict [
    "author", (string link.author);
    "url", (string link.url);
  ]
