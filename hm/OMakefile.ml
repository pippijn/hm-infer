install Program ".DEFAULT" [
  (* Target *)
  Name		"hm";

  (* Sources *)
  Modules [
    "Hm";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "hm";
  ];
]
