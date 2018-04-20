open OUnit2
open Lwt.Infix
       
       
module Client = Etcd.Client
module R = Rresult.R
             

type node = {
  key: string;
  createIndex: int;
  modifiedIndex: int;
  expiration: string option;
  value: string option;
  dir: bool option;
  nodes: node list option
}

type response = { action: string; node: node; prevNode: node option }

type error = { errorCode: int; message: string; cause: string; index: int }

let to_write =
  [
    ( ["testing";"foo"] , "bar");
    ( ["testing"; "tomb"], "home");
    ( ["testing"; "condition"], "alive");
    ( ["testing"; "tmp"], "temp")
    
  ]
               
let assert_ok res =
  assert_bool (R.is_ok res);
  R.get_ok res 
 
let writes ctx =
  Client.create_dir ["testing"] () >>= fun dir_res ->
  assert_ok res;

  
  Client.put ["testing"; "foo"] "bar" >>= fun res ->
  
