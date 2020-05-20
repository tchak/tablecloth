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

  describe "Fun" (fun () ->
      test "identity" (fun () -> expect (Fun.identity 1) |> toEqual 1) ;
      test "ignore" (fun () -> expect (Fun.ignore 1) |> toEqual ()) ;
      test "constant" (fun () -> expect (Fun.constant 1 2) |> toEqual 1) ;
      test "sequence" (fun () -> expect (Fun.sequence 1 2) |> toEqual 2) ;
      test "flip" (fun () -> expect (Fun.flip Int.( / ) 2 4) |> toEqual 2) ;
      test "apply" (fun () ->
          expect (Fun.apply (fun a -> a + 1) 1) |> toEqual 2) ;
      test "compose" (fun () ->
          let increment x = x + 1 in
          let double x = x * 2 in
          expect (Fun.compose increment double 1) |> toEqual 3) ;
      test "composeRight" (fun () ->
          let increment x = x + 1 in

          let double x = x * 2 in
          expect (Fun.composeRight increment double 1) |> toEqual 4) ;
      test "tap" (fun () ->
          expect
            ( Array.filter [| 1; 3; 2; 5; 4 |] ~f:Int.isEven
            |> Fun.tap ~f:(fun numbers -> ignore (Belt.Array.set numbers 1 0))
            |> Fun.tap ~f:Belt.Array.reverseInPlace )
          |> toEqual [| 0; 2 |])) ;

  describe "Tuple2" (fun () ->
      test "create" (fun () -> expect (Tuple2.create 3 4) |> toEqual (3, 4)) ;
      test "first" (fun () -> expect (Tuple2.first (3, 4)) |> toEqual 3) ;
      test "second" (fun () -> expect (Tuple2.second (3, 4)) |> toEqual 4) ;
      test "mapFirst" (fun () ->
          expect (Tuple2.mapFirst ~f:String.reverse ("stressed", 16))
          |> toEqual ("desserts", 16)) ;
      test "mapSecond" (fun () ->
          expect (Tuple2.mapSecond ~f:sqrt ("stressed", 16.))
          |> toEqual ("stressed", 4.)) ;
      test "mapEach" (fun () ->
          expect (Tuple2.mapEach ~f:String.reverse ~g:sqrt ("stressed", 16.))
          |> toEqual ("desserts", 4.)) ;
      test "mapAll" (fun () ->
          expect (Tuple2.mapAll ~f:String.reverse ("was", "stressed"))
          |> toEqual ("saw", "desserts")) ;
      test "swap" (fun () -> expect (Tuple2.swap (3, 4)) |> toEqual (4, 3)) ;
      test "toList" (fun () ->
          expect (Tuple2.toList (3, 4)) |> toEqual [ 3; 4 ])) ;
  describe "Tuple3" (fun () ->
      test "create" (fun () ->
          expect (Tuple3.create 3 4 5) |> toEqual (3, 4, 5)) ;
      test "first" (fun () -> expect (Tuple3.first (3, 4, 5)) |> toEqual 3) ;
      test "second" (fun () -> expect (Tuple3.second (3, 4, 5)) |> toEqual 4) ;
      test "third" (fun () -> expect (Tuple3.third (3, 4, 5)) |> toEqual 5) ;
      test "init" (fun () -> expect (Tuple3.init (3, 4, 5)) |> toEqual (3, 4)) ;
      test "tail" (fun () -> expect (Tuple3.tail (3, 4, 5)) |> toEqual (4, 5)) ;
      test "mapFirst" (fun () ->
          expect (Tuple3.mapFirst ~f:String.reverse ("stressed", 16, false))
          |> toEqual ("desserts", 16, false)) ;
      test "mapSecond" (fun () ->
          expect (Tuple3.mapSecond ~f:sqrt ("stressed", 16., false))
          |> toEqual ("stressed", 4., false)) ;
      test "mapThird" (fun () ->
          expect (Tuple3.mapThird ~f:not ("stressed", 16, false))
          |> toEqual ("stressed", 16, true)) ;
      test "mapEach" (fun () ->
          expect
            (Tuple3.mapEach
               ~f:String.reverse
               ~g:sqrt
               ~h:not
               ("stressed", 16., false))
          |> toEqual ("desserts", 4., true)) ;
      test "mapAll" (fun () ->
          expect (Tuple3.mapAll ~f:String.reverse ("was", "stressed", "now"))
          |> toEqual ("saw", "desserts", "won")) ;
      test "rotateLeft" (fun () ->
          expect (Tuple3.rotateLeft (3, 4, 5)) |> toEqual (4, 5, 3)) ;
      test "rotateRight" (fun () ->
          expect (Tuple3.rotateRight (3, 4, 5)) |> toEqual (5, 3, 4)) ;
      test "toList" (fun () ->
          expect (Tuple3.toList (3, 4, 5)) |> toEqual [ 3; 4; 5 ])) ;

  describe "Option" (fun () ->
      test "getExn Some(1)" (fun () ->
          expect (Option.getExn (Some 1)) |> toEqual 1) ;
      test "getExn None" (fun () ->
          expect (fun () -> Option.getExn None)
          |> toThrowException (Invalid_argument "option is None")))
