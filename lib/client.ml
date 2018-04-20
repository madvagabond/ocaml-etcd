open Protocol
open Lwt.Infix
       

exception Neither
            
module Client = Cohttp_lwt_unix.Client
module Option = Batteries.Option
                  
          
type result = (response, error) Result.result
type key = string list
                  

let path_string path =
  String.concat "/" path

let make_path key =
  ["v2";"keys"] @ key
  |> path_string

       
let ttl_param ttl =
  match ttl with
  | Some x ->
     [( "ttl", [(string_of_int x)] )  ]

  | _ -> []


let decode_response body =
  Cohttp_lwt.Body.to_string body >>= fun s ->
  let f () =
    let x = response_of_string s in
    Result.Ok x |> Lwt.return 
  in

  Lwt.catch (fun () -> f ()) (fun exn ->
      Result.Error(error_of_string s )
      |> Lwt.return
           
    )
            

            
let put host key ?ttl v =

  let path = make_path key in 
  let uri = Uri.make ~host ~path () in
  let value = Uri.pct_encode v in

  let ttl_p = ttl_param ttl in 
  
  let params = [("value", [value])] @ ttl_p in

  Client.post_form ~params uri >>= fun (_, body) ->
  decode_response body



let get host key =
  let path = make_path key in
  let uri = Uri.make ~host ~path () in
  Client.get uri >>= fun (_, body) ->
  decode_response body

                  

let delete host key =
  let path = make_path key in
  let uri = Uri.make ~host ~path ()  in
  Client.delete uri >>= fun (_, body) ->
  decode_response body


                  
let create_dir host key ?ttl () =
  let path = make_path key in 
  let uri = Uri.make ~host ~path () in

  let ttl_p = ttl_param ttl in
  let params =
    [
      ("dir", ["true"]) 
    ] @ ttl_p
  in

  Client.post_form uri ~params >>= fun (_, body) ->
  decode_response body




                  
let refresh host key ~ttl () =
  let path = make_path key in
  let uri = Uri.make ~host ~path () in

  let ttl_p = string_of_int ttl in
  let params =
    [
      ("refresh", ["true"]);
      ("ttl", [ttl_p]);
      ("prevExist", ["true"])
    ]
  in

  Client.post_form uri ~params >>= fun (_, body) ->
  decode_response body


                  


let list host key ?r:(r=false) () =
  let path = make_path key in
  let query =
    [ ("recursive", [ (string_of_bool r) ] ) ]
  in

  let uri = Uri.make ~host ~path ~query () in

  Client.get uri >>= fun (_, body) ->
  decode_response body 
  
  

let rm_dir host key =
  let path = make_path key in
  let query = [("dir", ["true"])] in
  let uri = Uri.make ~host ~path ~query () in

  Client.delete uri >>= fun (_, body) ->
  decode_response body


              

let watch host key cb ?times:(ct = 1) () =

  let make_uri i () = 
    let path = make_path key in
    let common_q = [  ("wait", ["true"])  ] in  

    let query =
      match i with
      | Some x ->
         let index_q =
           [
             ("waitIndex", [   (string_of_int x) ]  )
           ]
         in 
         common_q @ index_q

      | None -> common_q 
    in 


    Uri.make ~host ~path ~query ()

  in



  let rec handle_watch ctr ?i () =
    
    if ctr > 1 then
      let url = make_uri i () in
      Client.get url >>= fun (_, body) ->
      decode_response body >>= fun res ->

      match res with
      | Result.Ok rep ->
         let i0 = rep.node.modifiedIndex in
         let (i1, ctr0) = (i0 + 1), (ctr - 1) in
         cb rep >>= fun _ ->
         handle_watch ctr0 ~i:i1 ()


      | Result.Error e ->
         let emsg = Fmt.strf "Error %s the cause was %s " e.message e.cause in 
         Lwt.fail_with emsg



    else
      let url = make_uri i () in
      Client.get url >>= fun (_, body) ->
      decode_response body >>= fun res ->

      match res with
      | Result.Ok rep -> cb rep
      | Result.Error e ->
         let emsg = Fmt.strf "Error %s the cause was %s " e.message e.cause in 
         Lwt.fail_with emsg


  in
  
  handle_watch ct ()
    
                          
                
