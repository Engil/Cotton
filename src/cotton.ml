open Lwt
open Irmin_unix
open Opium.Std
open Utils
open Core_kernel.Std

module LinkStore =
  Irmin_git.FS (Irmin.Contents.Json)(Irmin.Ref.String)(Irmin.Hash.SHA1)

module UserStore =
  Irmin_git.FS (Irmin.Contents.Json)(Irmin.Ref.String)(Irmin.Hash.SHA1)

let config root = Irmin_git.config ~root ~bare:true ()

let config_links = config "/tmp/irmin/links"

let config_users = config "/tmp/irmin/users"

let link_task _ = LinkStore.Repo.create config_links >>= LinkStore.master task

let user_task _ = UserStore.Repo.create config_users >>= UserStore.master task

let add_user t name email password =
  let hashed_password = Bcrypt.hash password |> Bcrypt.string_of_hash in
  let msg = Printf.sprintf "Adding user %s" name in
  let user = User.user name email hashed_password  in
  let json = User.to_json_content user in
  UserStore.update (t msg) ["users";name] json >>= fun () ->
  Lwt.return user

let get_user t name =
  UserStore.read_exn (t "Reading user") ["users"; name] >>= fun user ->
  User.of_json user |> Lwt.return

let get_token t token =
  UserStore.read (t "Reading token") ["token"; token] >>= function
  | None -> Lwt.return_none
  | Some json -> find json "name" |> Ezjsonm.get_string |> Lwt.return_some

let check_password t name password =
  get_user t name >>= fun user ->
  Bcrypt.hash_of_string user.password |> Bcrypt.verify password |> Lwt.return

let key = Univ_map.Key.create "user" String.sexp_of_t

let auth_middleware =
  let filter handler req =
    match req |> Request.headers |> Cohttp.Header.get_authorization with
    | Some `Other s ->
      user_task () >>= fun t ->
      get_token t s >>= begin function
      | Some name ->
        let env = Univ_map.add_exn (Request.env req) key name in
        let req = Field.fset Request.Fields.env req env in
        handler req
      | None ->
        handler req
      end
    | _ -> handler req in
  Rock.Middleware.create ~name:(Info.of_string "auth for cotton") ~filter

let add_token t name =
  let open Ezjsonm in
  let token = Cryptokit.Random.string Cryptokit.Random.secure_rng 16 |> Digest.to_hex in
  let json = dict ["name", (string name)] in
  UserStore.update (t "Creating new token") ["token";token] json >>= fun () ->
  Lwt.return token

let add_link t author url =
  let uuid = Uuidm.create `V4 |> Uuidm.to_string in
  let msg = Printf.sprintf "Adding URL %s" uuid in
  let json = Link.link url author uuid |> Link.to_json_content in
  LinkStore.update (t msg) ["links"; uuid] json >>= fun () ->
  Lwt.return uuid

let get_key_uuid = function
  | [s; uuid] when s = "links" -> uuid
  | _ -> assert false

let get_links t =
  LinkStore.list (t "Reading links") ["links"] >>=
  fun keys -> Lwt_list.map_s (fun key ->
      LinkStore.read_exn (t "Reading link") key >>= fun json ->
      Link.of_json (get_key_uuid key) json |> Lwt.return) keys

let get_link t uuid =
  LinkStore.read_exn (t "Reading link") ["links"; uuid] >>= fun link ->
  Link.of_json uuid link |> Lwt.return

let format_error error_name error_content =
  let open Ezjsonm in
  `Json (dict [error_name, (string error_content)])

let authentified f req =
  match Univ_map.find (Request.env req) key with
  | None -> format_error "auth" "You need to be authentified" |> respond' ~code:`Bad_request
  | Some name -> f name req

let add_url_handler = post "/url/" @@ authentified
    begin fun name req ->
      link_task () >>= fun t ->
      match get_all_qp (Request.uri req) ["url"] with
      | Some [url] ->
        add_link t name url >>= fun uuid ->
        let link = Link.link url name uuid |> Link.to_json_response in
        `Json link |> respond'
      | _ ->
        format_error "param" "missing parameter" |> respond' ~code:`Bad_request
    end

let add_user_handler = post "/user/"
    begin fun req ->
      user_task () >>= fun t ->
      match get_all_qp (Request.uri req) ["name";"email";"password"] with
      | Some [name; email; password] ->
        add_user t name email password >>= fun user ->
        let user = User.to_json_response user in
        `Json user |> respond'
      | _ ->
        format_error "param" "missing parameter" |> respond' ~code:`Bad_request
    end

let add_token_handler = post "/token/"
    begin fun req ->
      user_task () >>= fun t ->
      match get_all_qp (Request.uri req) ["name";"password"] with
      | Some [name; password] ->
        check_password t name password >>= fun is_auth ->
        if is_auth then
          add_token t name >>= fun token ->
          `Json (Ezjsonm.string token |> Ezjsonm.wrap) |> respond'
        else
          format_error "auth" "auth error" |> respond' ~code:`Bad_request
      | _ ->
        format_error "param" "missing parameter" |> respond' ~code:`Bad_request
    end

let get_url_handler = get "/url/:uuid"
    begin fun req ->
      link_task () >>= fun t ->
      let uuid = param req "uuid" in
      get_link t uuid >>= fun link ->
      `Json (Link.to_json_response link) |> respond'
    end

let get_url_handlers_handler = get "/url/"
    begin fun req ->
      link_task () >>= fun t ->
      get_links t >>= fun links ->
      `Json (Ezjsonm.list Link.to_json_response links) |> respond'
    end

let () =
  let open Opium_middleware_extra in
  App.empty
  |> middleware auth_middleware
  |> middleware Cors.cors_middleware
  |> add_url_handler
  |> add_token_handler
  |> add_user_handler
  |> get_url_handler
  |> get_url_handlers_handler
  |> App.run_command
