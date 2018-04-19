module Node = struct 
  type t =
    {
      key: string;
      createIndex: int;
      modifiedIndex: int;

      expiration: string option;
      value: string option;
      
      dir: bool option;
      nodes: t list option; 
    } [@@deriving yojson, fields]

    
end


module Response = struct
  type t =
    {
      action: string;
      node: Node.t;
      prevNode: Node.t option;
    } [@@deriving yojson, fields]
end 


module Error = struct
  type t =
    {
      errorCode: int;
      message: string;
      cause: string;
      index: int 
    } [@@deriving yojson, fields]
end 
