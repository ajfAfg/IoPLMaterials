open Miniml

let program = Syntax.show_program |> Fmt.of_to_string |> Alcotest.of_pp

let program_of_string str =
  Parser.toplevel_input Lexer.main (Lexing.from_string str)

let () =
  let open Alcotest in
  run "MyList"
    [
      ( "remove",
        let check = check (list int) "" in
        [
          test_case
            "If there is no element to be removed, return the given list" `Quick
            (fun () ->
              check [] @@ MyList.remove 1 [];
              check [ 2 ] @@ MyList.remove 1 [ 2 ];
              check [ 2; 3 ] @@ MyList.remove 1 [ 2; 3 ]);
          test_case "Delete only one applicable element" `Quick (fun () ->
              check [] @@ MyList.remove 1 [ 1 ];
              check [ 2 ] @@ MyList.remove 1 [ 2; 1 ];
              check [ 2; 1 ] @@ MyList.remove 1 [ 1; 2; 1 ]);
        ] );
      ( "subtract",
        let check = check (list int) "" in
        [
          test_case
            "If there are no duplicates in the two given lists, return the \
             first argument"
            `Quick (fun () ->
              check [] @@ MyList.subtract [] [];
              check [] @@ MyList.subtract [] [ 1; 2 ];
              check [ 1; 2 ] @@ MyList.subtract [ 1; 2 ] []);
          test_case
            "remove elements from the first argument lsit that are included in \
             the second argument list"
            `Quick (fun () ->
              check [] @@ MyList.subtract [ 1; 2 ] [ 1; 2 ];
              check [ 1; 2 ] @@ MyList.subtract [ 1; 2; 3; 4 ] [ 3; 4 ];
              check [ 1; 2 ] @@ MyList.subtract [ 1; 1; 2; 2 ] [ 1; 2 ]);
        ] );
    ]
