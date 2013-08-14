install Program ".DEFAULT" [
  (* Target *)
  Name		"hm_gtk";

  (* Sources *)
  Modules [
    "Hm_ide";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "hm";
    "gtk_intf";
  ];
]
