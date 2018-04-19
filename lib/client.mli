open Protocol

exception Neither
            
type result = (Response.t, Error.t) Result.result
                                    
val put: string -> string -> ?ttl:int -> () -> result Lwt.t 
val get: string -> string -> result Lwt.t
                                    
val delete: string -> string -> result Lwt.t
val create_dir: string -> string -> ?ttl:int -> unit -> result Lwt.t

val refresh: string -> string -> result Lwt.t                                        
val list: string -> string -> ?r:bool -> unit -> result Lwt.t
val rm_dir: string -> string -> result Lwt.t                                   
                                   
val watch: string -> string -> (fun result -> 'a) -> ?times:int -> unit -> 'a Lwt.t
val refresh: string -> string -> result Lwt.t  
