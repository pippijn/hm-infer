type env = {
  types : (string * Hm_type.ty) list;
  insts : (Hm_type.var * Hm_type.ty) list;
  lastvar : Hm_type.var;
}


let empty = {
  types = [];
  insts = [];
  lastvar = Hm_type.zero_var;
}


let combine env1 env2 = {
  env1 with
  insts = env2.insts;
  lastvar = env2.lastvar;
}


let add name ty env = {
  env with
  types = (name, ty) :: env.types;
}

let find env name =
  snd (List.find (fun (nm, _) -> nm = name) env.types)

let mem env name =
  List.exists (fun (nm, _) -> nm = name) env.types

let iter fn env =
  List.iter fn env.types

let fold_left fn a env =
  List.fold_left fn a env.types


let add_instance env name ty = {
  env with
  insts = (name, ty) :: env.insts;
}

let find_instance env name =
  snd (List.find (fun (nm, _) -> nm = name) env.insts)

let mem_instance env name =
  List.exists (fun (nm, _) -> nm = name) env.insts

let iter_instance fn env =
  List.iter fn env.insts


let new_var env =
  let var = Hm_type.Var env.lastvar in
  { env with lastvar = Hm_type.succ_var env.lastvar }, var
