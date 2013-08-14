build Program ".DEFAULT" [
  (* Target *)
  Name		"hm_js";

  (* Sources *)
  Modules [
    "Hm_js";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "hm";
    "js_intf";
  ];

  (* Only byte-code *)
  Var ("OCAML_BYTE", "true");
  Var ("OCAML_NATIVE", "false");

  Rule ("upload", "$(Name).js", [
    "chmod 644 $^";
    "scp $^ $'ra:public_html/files/up/parser/'";
  ]);
]
