install Program ".DEFAULT" [
  (* Target *)
  Name		"hm";

  (* Sources *)
  Modules [
    "Hm";
    "Hm_ast";
    "Hm_dynparse";
    "Hm_env";
    "Hm_errors";
    "Hm_infer";
    "Hm_lexer";
    "Hm_parser";
    "Hm_token";
    "Hm_tokens";
    "Hm_type";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "batteries";
    "libmerr";
    "sexplib.syntax";
  ];

  (* Camlp4 *)
  Flags [
    "a_ast.ml",		"-syntax camlp4o";
    "e_ast.ml",		"-syntax camlp4o";
    "makeErr.ml",	"-syntax camlp4o";
  ];

  Var ("RUNMERR", "hm.native -merr");
]
