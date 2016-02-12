  
let () =
  Alcotest.run "ppx_polyprint" @@ List.concat [
    Noppx.tests;
    Ppx.tests;
  ];

