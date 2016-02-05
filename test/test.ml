  
let () =
  Alcotest.run "ppx_polyprint" @@ List.concat [
    Print.tests;
    Transform.tests;
  ];

