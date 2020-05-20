open Tablecloth
open AlcoJest
module AT = Alcotest

let t_Option () =
  AT.check AT.int "getExn Some(1)" (Option.getExn (Some 1)) 1 ;

  AT.check_raises "getExn None" (Invalid_argument "option is None") (fun () ->
      Option.getExn None) ;

  ()


let t_Result () =
  AT.check
    (AT.result AT.int AT.string)
    "fromOption - maps None into Error"
    Result.(fromOption ~error:"error message" (None : int option))
    (Error "error message") ;
  AT.check
    (AT.result AT.int AT.string)
    "fromOption - maps Some into Ok"
    Result.(fromOption ~error:"error message" (Some 10))
    (Ok 10) ;
  ()


let suite = [ ("Option", `Quick, t_Option); ("Result", `Quick, t_Result) ]
