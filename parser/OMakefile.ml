install Library ".DEFAULT" [
  (* Target *)
  Name		"hm";
  Description	"Hindley-Milner type inference";
  Version	"0.1";

  (* Sources *)
  Modules [
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
  ];

  Var ("RUNMERR", "$(bindir)/hm.native -merr");
]
