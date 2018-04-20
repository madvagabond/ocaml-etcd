open OUnit2
open Lwt.Infix
       
       
module Client = Etcd.Client
module R = Rresult.R
             
open Etcd.Protocol

let to_write =
  [
    ( ["testing";"foo"] , "bar");
    ( ["testing"; "tomb"], "home");
    ( ["testing"; "condition"], "alive");
    ( ["testing"; "temp"], "temp")
    
  ]
               
let assert_ok res =
  
  (match res with
  | Result.Ok rep ->
     print_endline (string_of_response rep);
  | Result.Error e ->
     print_endline (string_of_error e)

  );
  

  assert_bool "got error" (R.is_ok res);
  (* print_endline (string_of_error (R.get_error res) ); *)
  R.get_ok res


let assert_opt o =
  assert_bool "Option was None" (Batteries.Option.is_some o);
  Batteries.Option.get o
                       
let pp_string x = x
                    
let path_of_string x =
  Str.split (Str.regexp "/") x


let pp_key key =
  String.concat "/" key

let pp_index index =
  let l = List.map (fun k -> pp_key k) index in
  "["  ^ (String.concat "," l) ^ "]"
    
  
let host = "http://127.0.0.1:2379"

let assert_op exp res =
  let rep = assert_ok res in
  let got = rep.action in
  assert_equal ~printer:pp_string exp got

               
             
let writes ctx =
  Client.create_dir host ["testing"] () >>= fun dir_res ->
  assert_ok dir_res;
  print_endline "testing"; 
  Client.put host ["testing"; "foo"] "bar" >>= fun res ->

  let got =
    assert_ok res
    |> (fun r -> assert_opt r.node.value)
  in
  
  assert_equal ~printer:pp_string "bar" got;

  Lwt_list.iter_s (
      fun (key, v) -> Client.put host key v >>= fun _ -> Lwt.return_unit
    ) to_write >>= fun () ->

  Client.delete host ["testing"; "temp"] >>= fun del_r ->
  (*assert_op "delete" del_r;*)
  assert_ok del_r;

  Client.list host ["testing"] () >>= fun res1 ->
  assert_op "get" res1;
  Lwt.return_unit
  

  
  

  
let run f ctx =
  Lwt_main.run (f ctx)
               
let suite =
  "Etcd Client Suite" >::: ["Write Test" >:: run writes ]
                             
                             
let _ =
  run_test_tt_main suite
