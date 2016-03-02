open Lwt
open Irmin_unix
open Opium.Std
open Utils

module Store =

  Irmin_git.FS (Irmin.Contents.Json)(Irmin.Ref.String)(Irmin.Hash.SHA1)

let add_link t author url =
  let uuid = Uuidm.create `V4 |> Uuidm.to_string in
  let msg = Printf.sprintf "Adding URL %s" uuid in
  let json = Link.link url author uuid |> Link.to_json_content in
  Store.update (t msg) ["links"; uuid] json >>= fun () ->
  Lwt.return uuid

let get_key_uuid = function
  | [s; uuid] when s = "links" -> uuid
  | _ -> assert false

let get_links t =
  Store.list (t "Reading links") ["links"] >>=
  fun keys -> Lwt_list.map_s (fun key ->
      Store.read_exn (t "Reading link") key >>= fun json ->
      Link.of_json (get_key_uuid key) json |> Lwt.return) keys

let get_link t uuid =
  Store.read_exn (t "Reading link") ["links"; uuid] >>= fun link ->
  Link.of_json uuid link |> Lwt.return

let config = Irmin_git.config ~root:"/tmp/irmin/test" ~bare:false ()

let new_task _ = Store.Repo.create config >>= Store.master task

let format_error error_name error_content =
  let open Ezjsonm in
  `Json (dict [error_name, (string error_content)])

let add_url = post "/url/"
    begin fun req ->
      new_task () >>= fun t ->
      match get_all_qp (Request.uri req) ["author";"url"] with
      | Some [author; url] ->
        add_link t author url >>= fun uuid ->
        let link = Link.link url author uuid |> Link.to_json_response in
        `Json link |> respond'
      | _ ->
        format_error "param" "missing parameter" |> respond' ~code:`Bad_request
    end

let get_url = get "/url/:uuid"
    begin fun req ->
      new_task () >>= fun t ->
      let uuid = param req "uuid" in
      get_link t uuid >>= fun link ->
      `Json (Link.to_json_response link) |> respond'
    end

let get_urls = get "/url/"
    begin fun req ->
      new_task () >>= fun t ->
      get_links t >>= fun links ->
      `Json (Ezjsonm.list Link.to_json_response links) |> respond'
    end

let () =
  App.empty
  |> add_url
  |> get_url
  |> get_urls
  |> App.run_command
