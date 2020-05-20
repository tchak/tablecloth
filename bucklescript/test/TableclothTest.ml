open Tablecloth
open Jest
open Expect

let () =
  describe "Result" (fun () ->
      describe "fromOption" (fun () ->
          test "maps None into Error" (fun () ->
              expect Result.(fromOption ~error:"error message" None)
              |> toEqual (Belt.Result.Error "error message")) ;
          test "maps Some into Ok" (fun () ->
              expect Result.(fromOption ~error:"error message" (Some 10))
              |> toEqual (Belt.Result.Ok 10))) ;
      describe "map" (fun () ->
          test "maps value on success" (fun () ->
              expect Result.(map ~f:String.reverse (succeed "blah"))
              |> toEqual (Belt.Result.Ok "halb")))) ;
  describe "Option" (fun () ->
      test "getExn Some(1)" (fun () ->
          expect (Option.getExn (Some 1)) |> toEqual 1) ;
      test "getExn None" (fun () ->
          expect (fun () -> Option.getExn None)
          |> toThrowException (Invalid_argument "option is None")))
