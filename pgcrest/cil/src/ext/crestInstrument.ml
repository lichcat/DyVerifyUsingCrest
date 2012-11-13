(* Copyright (c) 2008, Jacob Burnim (jburnim@cs.berkeley.edu)
 *
 * This file is part of CREST, which is distributed under the revised
 * BSD license.  A copy of this license can be found in the file LICENSE.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See LICENSE
 * for details.
 *)

open Cil

(*open String
 * Utilities that should be in the O'Caml standard libraries.
 *)

let isSome o =
  match o with
    | Some _ -> true
    | None   -> false

let rec mapOptional f ls =
  match ls with
    | [] -> []
    | (x::xs) -> (match (f x) with
                    | None -> mapOptional f xs
                    | Some x' -> x' :: mapOptional f xs)

let concatMap f ls =
  let rec doIt res ls =
    match ls with
      | [] -> List.rev res
      | (x::xs) -> doIt (List.rev_append (f x) res) xs
  in
    doIt [] ls

let open_append fname =
  open_out_gen [Open_append; Open_creat; Open_text] 0o700 fname


(*
 * We maintain several bits of state while instrumenting a program:
 *  - the last id assigned to an instrumentation call
 *    (equal to the number of such inserted calls)
 *  - the last id assigned to a statement in the program
 *    (equal to the number of CFG-transformed statements)
 *  - the last id assigned to a function
 *  - the set of all branches seen so far (stored as pairs of branch
 *    id's -- with paired true and false branches stored together),
 *    annotating branches with the funcion they are in
 *  - a per-function control-flow graph (CFG), along with all calls
 *    between functions
 *  - a map from function names to the first statement ID in the function
 *    (to build the complete CFG once all files have been processed)
 *
 * Because the CIL executable will be run once per source file in the
 * instrumented program, we must save/restore this state in files
 * between CIL executions.  (These last two bits of state are
 * write-only -- at the end of each run we just append updates.)
 *)

let idCount = ref 0
let stmtCount = Cfg.start_id
let funCount = ref 0
let branches = ref []
let curBranches = ref []
let warningId = ref 0 
let warningPath = ref []
let pathStmtId = ref 0
let matchWarning = ref false
(*when a __StaticPathMark(id,pathId,pathStmtId) instru-ed,
 put into storePathMarkIdList, because when writting
 cfg,need to write the name^pahtId^pathStmtId
 *)
let storePathMarkIdList = ref [] 

(* Control-flow graph is stored inside the CIL AST. *)

let getNewId () = ((idCount := !idCount + 1); !idCount)
let addBranchPair bp = (curBranches := bp :: !curBranches)
let addFunction () = (branches := (!funCount, !curBranches) :: !branches;
		      curBranches := [];
		      funCount := !funCount + 1)

let readCounter fname =
  try
    let f = open_in fname in
      Scanf.fscanf f "%d" (fun x -> x)
  with x -> 0

let writeCounter fname (cnt : int) =
  try
    let f = open_out fname in
      Printf.fprintf f "%d\n" cnt ;
      close_out f
  with x ->
    failwith ("Failed to write counter to: " ^ fname ^ "\n")
(*
let mySubStr fname ?indexHead:(indexHead=0) indexEnd =
  try
	let len = indexEnd-indexHead in
    String.sub fname indexHead len
  with x ->
    failwith ("Invalid_argument of string and indexHead/indexEnd:" ^ fname ^ (string_of_int indexHead) ^(string_of_int indexEnd) ^"\n")
  *)  
 
let readIdCount () = (idCount := readCounter "idcount")
let readStmtCount () = (stmtCount := readCounter "stmtcount")
let readFunCount () = (funCount := readCounter "funcount")

let readCurrentCheckFile () =
  let f =open_in "currentCheck" in
	let rec iter_lines chan =
	try
		let words = Str.split (Str.regexp "[ \t]+") (input_line chan) in
		if (List.length words)=1 then 
		(let oneword= List.nth words 0 in
		 if oneword<>"END_PATH" then
			warningId:=int_of_string oneword
		 )else(
			 let alreadyIn =
				List.mem words !warningPath
			 in
			 let notPP =
				let st = List.nth words 2 in
				if st="PP" then false
				else true
			 in
			 if(notPP && not(alreadyIn)) then
				warningPath:= words::!warningPath
			 );
		iter_lines chan
	with End_of_file -> () in
	iter_lines f;
	close_in f;
  warningPath:=List.rev !warningPath(*;
  let printPathMark word =
	List.iter (fun x-> Printf.printf "%s " x) word;
	Printf.printf "\n"
  in
  List.iter printPathMark !warningPath*)

let instruPathMark stype location =
	pathStmtId:= 0;
	let count = ref 0 in 
 let iterWarningPath warningline =
	 let tmp_file = List.nth warningline 0 in
	 let tmp_line = List.nth warningline 1 in
	 let tmp_type = List.nth warningline 2 in
	 (count:=!count+1;
	  if(tmp_file=location.file && (int_of_string tmp_line)=location.line && tmp_type=stype) then
		(matchWarning:=true;
		pathStmtId:= !count);
	 )
 in
  List.iter iterWarningPath !warningPath 
  
let writeIdCount () = writeCounter "idcount" !idCount
let writeStmtCount () = writeCounter "stmtcount" !stmtCount
let writeFunCount () = writeCounter "funcount" !funCount

let writeBranches () =
  let writeFunBranches out (fid, bs) =
    if (fid > 0) then
      (let sorted = List.sort compare bs in
         Printf.fprintf out "%d %d\n" fid (List.length bs) ;
         List.iter (fun (s,d) -> Printf.fprintf out "%d %d\n" s d) sorted)
  in
    try
      let f = open_append "branches" in
      let allBranches = (!funCount, !curBranches) :: !branches in
        List.iter (writeFunBranches f) (List.tl (List.rev allBranches));
        close_out f
    with x ->
      prerr_string "Failed to write branches.\n"

(* Visitor which walks the CIL AST, printing the (already computed) CFG. *)
class writeCfgVisitor out firstStmtIdMap =
object (self)
  inherit nopCilVisitor
  val out = out
  val firstStmtIdMap = firstStmtIdMap

  method writeCfgCall f =
    (*let isInstruFunc fname =
	  let regStr = Str.regexp "^[__Crest|__DyVerify].+" in
		Str.string_match regStr fname 0
	in*)
    if List.mem_assq f firstStmtIdMap then
      Printf.fprintf out " %d" (List.assq f firstStmtIdMap).sid
	else if (f.vname="__StaticPathMark")then
	  (let (curpathId,curpathStmtId) = List.hd !storePathMarkIdList in
		Printf.fprintf out " %s" (f.vname^"_P"^(string_of_int curpathId)^"_S"^(string_of_int curpathStmtId));
		storePathMarkIdList:=List.tl !storePathMarkIdList
	   )	
    else (*if not (isInstruFunc f.vname)then*)
      Printf.fprintf out " %s" f.vname 

  method writeCfgInst i =
     match i with
         Call(_, Lval(Var f, _), _, _) -> self#writeCfgCall f 
       | _ -> ()

  method vstmt(s) =
    Printf.fprintf out "%d" s.sid ;
	List.iter (fun dst -> Printf.fprintf out " %d" dst.sid ) s.succs;
    (match s.skind with
         Instr is ->List.iter self#writeCfgInst is 
	  | _	->());
	output_string out "\n";
    DoChildren

end

let writeCfg cilFile firstStmtIdMap =
  try
    let out = open_append "cfg" in
    let wcfgv = new writeCfgVisitor out firstStmtIdMap in
    visitCilFileSameGlobals (wcfgv :> cilVisitor) cilFile ;
    close_out out
  with x ->
    prerr_string "Failed to write CFG.\n"

let buildFirstStmtIdMap cilFile =
  let getFirstFuncStmtId glob =
    match glob with
      | GFun(f, _) -> Some (f.svar, List.hd f.sbody.bstmts)
      | _ -> None
  in
    mapOptional getFirstFuncStmtId cilFile.globals

let writeFirstStmtIdMap firstStmtIdMap =
  let writeEntry out (f,s) =
    (* To help avoid "collisions", skip static functions. *)
    (*if not (f.vstorage = Static) then*)
      Printf.fprintf out "%s %d\n" f.vname s.sid
  in
  try
    let out = open_append "cfg_func_map" in
    List.iter (writeEntry out) firstStmtIdMap ;
    close_out out
  with x ->
    prerr_string "Failed to write (function, first statement ID) map.\n"

let handleCallEdgesAndWriteCfg cilFile =
  let stmtMap = buildFirstStmtIdMap cilFile in
   writeCfg cilFile stmtMap ;
   writeFirstStmtIdMap stmtMap

(* Utilities *)

let noAddr = zero

let shouldSkipFunction f = hasAttribute "crest_skip" f.vattr

let prependToBlock (is : instr list) (b : block) =
  b.bstmts <- mkStmt (Instr is) :: b.bstmts

let isSymbolicType ty = isIntegralType (unrollType ty)
let isPointerType ty = isPointerType (unrollType ty)

(* These definitions must match those in "libcrest/crest.h". *)
let idType   = intType
let bidType  = intType
let fidType  = uintType
let valType  = TInt (ILongLong, [])
let addrType = TInt (IULong, [])
let boolType = TInt (IUChar, [])
let opType   = intType  (* enum *)
let argIntType = TPtr (TInt (IInt, []),[])
let argUIntType = TPtr (TInt (IUInt, []), [])
let argUShortType = TPtr (TInt (IUShort, []), [])
let argShortType = TPtr (TInt (IShort, []), [])
let argCharType = TPtr (TInt (IChar, []), [])
let argUCharType = TPtr (TInt (IUChar, []), [])
(*Visitor which walks the AST and Instrument the PathMark
  so that when compute CFG it can be write into cfg file
  and this is before the crestInstrument *)

class instruPathMarkVisitor f =
	let idArg   = ("id",   idType,   []) in
	let pathIdArg =("pathIdArg",idType, []) in
	let pathStmtIdArg = ("pathStmtIdArg",idType,[]) in

	let mkPathMarkInstFunc name args =
		let ty = TFun (voidType, Some (idArg :: args), false, []) in
		let func = findOrCreateFunc f ("__Static" ^ name) ty in
		func.vstorage <- Extern ;
		func.vattr <- [Attr ("crest_skip", [])] ;
		func
	in
	let mkInstCall func args =
	 let args' = integer (getNewId ()) :: args in
	   Call (None, Lval (var func), args', locUnknown)
	in
	let mkPathMark pathId pathStmtId = 
	  let csvPathMark = mkPathMarkInstFunc ("PathMark") [pathIdArg;pathStmtIdArg] in
		mkInstCall csvPathMark [integer pathId; integer pathStmtId]
	in
	let pushtoSPMIL pid psid =
	  (*let alreadyInList =
		List.mem (pid,psid) !storePathMarkIdList
	  in
		if (not alreadyInList) then*)
		storePathMarkIdList := (pid,psid)::!storePathMarkIdList
	in
	let instruMatchedPathMark self str location =
	matchWarning:=false;
	instruPathMark str location;
	if (!matchWarning) then
	(self#queueInstr [mkPathMark !warningId !pathStmtId];
	 pushtoSPMIL !warningId !pathStmtId
	);
matchWarning:=false
	in
object (self)
	inherit nopCilVisitor

	method vstmt(s) =
	  match s.skind with
      | If (e, b1, b2, location) ->
		(*matchWarning:=false;
		instruPathMark "LK" location;
		if (!matchWarning) then
		  (prependToBlock [mkPathMark !warningId !pathStmtId] b1;
		   pushtoSPMIL !warningId !pathStmtId
		   );*)
		matchWarning:=false;
		instruPathMark "BT" location;
		if (!matchWarning) then
		  (prependToBlock [mkPathMark !warningId !pathStmtId] b1;
		   pushtoSPMIL !warningId !pathStmtId
		   );
		matchWarning:=false;	
		instruPathMark "BF" location;
		if (!matchWarning) then
		  (prependToBlock [mkPathMark !warningId !pathStmtId] b2;
		   pushtoSPMIL !warningId !pathStmtId
		   );
		matchWarning:=false;
		DoChildren
      | Return (_,location) ->
		instruMatchedPathMark self "LK" location;
        SkipChildren
	  | _ ->
		DoChildren

	method vinst(i) =
		match i with 
		| Call (_, Lval (Var f, _),args,location) ->
		(match f.vname with
		 | "malloc" -> instruMatchedPathMark self "MA" location
		 | "calloc" -> instruMatchedPathMark self "CA" location
		 | "realloc" -> instruMatchedPathMark self "RA" location
		 | "xmalloc" -> instruMatchedPathMark self "XMA" location
		 | "xcalloc" -> instruMatchedPathMark self "XCA" location
		 | "xstrdup" -> instruMatchedPathMark self "XSD" location
		 | _ ->()
		 );
		  DoChildren
	    | Set (_,_,location) -> instruMatchedPathMark self "LK" location;
		  DoChildren
		| _ ->
		  DoChildren
end






(*
 * normalizeConditionalsVisitor ensures that every if block has an
 * accompanying else block (by adding empty "else { }" blocks where
 * necessary).  It also attempts to convert conditional expressions
 * into predicates (i.e. binary expressions with one of the comparison
 * operators ==, !=, >, <, >=, <=.)
 *)
class normalizeConditionalsVisitor =

  let isCompareOp op =
    match op with
      | Eq -> true  | Ne -> true  | Lt -> true
      | Gt -> true  | Le -> true  | Ge -> true
      | _ -> false
  in

  let negateCompareOp op =
    match op with
      | Eq -> Ne  | Ne -> Eq
      | Lt -> Ge  | Ge -> Lt
      | Le -> Gt  | Gt -> Le
      | _ ->
          invalid_arg "negateCompareOp"
  in

  (* TODO(jburnim): We ignore casts here because downcasting can
   * convert a non-zero value into a zero -- e.g. from a larger to a
   * smaller integral type.  However, we could safely handle casting
   * from smaller to larger integral types. *)
  let rec mkPredicate e negated =
    match e with
      | UnOp (LNot, e, _) -> mkPredicate e (not negated)

      | BinOp (op, e1, e2, ty) when isCompareOp op ->
          if negated then
            BinOp (negateCompareOp op, e1, e2, ty)
          else
            e

      | _ ->
          let op = if negated then Eq else Ne in
            BinOp (op, e, zero, intType)
  in

object (self)
  inherit nopCilVisitor

  method vstmt(s) =
    match s.skind with
      | If (e, b1, b2, loc) ->
          (* Ensure neither branch is empty. *)
          if (b1.bstmts == []) then b1.bstmts <- [mkEmptyStmt ()] ;
          if (b2.bstmts == []) then b2.bstmts <- [mkEmptyStmt ()] ;
          (* Ensure the conditional is actually a predicate. *)
          s.skind <- If (mkPredicate e false, b1, b2, loc) ;
          DoChildren

      | _ -> DoChildren

end


let addressOf : lval -> exp = mkAddrOrStartOf


let hasAddress (_, off) =
  let rec containsBitField off =
    match off with
      | NoOffset         -> false
      | Field (fi, off) -> (isSome fi.fbitfield) || (containsBitField off)
      | Index (_, off)  -> containsBitField off
  in
    not (containsBitField off)


class crestInstrumentVisitor f =
  (*
   * Get handles to the instrumentation functions.
   *
   * NOTE: If the file we are instrumenting includes "crest.h", this
   * code will grab the varinfo's from the included declarations.
   * Otherwise, it will create declarations for these functions.
   *)
  let idArg   = ("id",   idType,   []) in
  let bidArg  = ("bid",  bidType,  []) in
  let fidArg  = ("fid",  fidType,  []) in
  let valArg  = ("val",  valType,  []) in
  let addrArg = ("addr", addrType, []) in
  let opArg   = ("op",   opType,   []) in
  let boolArg = ("b",    boolType, []) in
  (*
      add by lichcat
      ptrAddrArg and memAddrArg for alloc/free related functioin argument
      retPtrAddrArg and retPtrValueArg for return pointer related function argument
   *)
  let numArg =("num",idType,[]) in
  let ptrAddrArg =("ptrAddr", addrType, []) in
  let memAddrArg =("memAddr", addrType, []) in
  let retPtrAddrArg =("retPtrAddr", addrType,[]) in
  let retPtrValueArg =("retPtrValue", addrType, []) in
  let inputIntArg =("x", argIntType,[]) in
  let inputUIntArg =("x", argUIntType,[]) in
  let inputUShortArg =("x", argUShortType,[]) in
  let inputShortArg =("x",argShortType,[]) in
  let inputCharArg =("x",argCharType,[]) in
  let inputUCharArg =("x",argUCharType,[]) in
  let newAddrArg=("newAddr",addrType,[]) in
  let oldAddrArg=("oldAddr",addrType,[]) in

  let mkInstFunc name args =
    let ty = TFun (voidType, Some (idArg :: args), false, []) in
    let func = findOrCreateFunc f ("__Crest" ^ name) ty in
      func.vstorage <- Extern ;
      func.vattr <- [Attr ("crest_skip", [])] ;
      func
  in

  let mkInputInstFunc name args =
    let ty = TFun (voidType, Some (args), false, []) in
    let func = findOrCreateFunc f ("__Crest" ^ name) ty in
      func.vstorage <- Extern ;
      func.vattr <- [Attr ("crest_skip", [])] ;
      func
  in
  let loadFunc         = mkInstFunc "Load"  [addrArg; valArg] in
  let storeFunc        = mkInstFunc "Store" [addrArg] in
  let clearStackFunc   = mkInstFunc "ClearStack" [] in
  let apply1Func       = mkInstFunc "Apply1" [opArg; valArg] in
  let apply2Func       = mkInstFunc "Apply2" [opArg; valArg] in
  let branchFunc       = mkInstFunc "Branch" [bidArg; boolArg] in
  let callFunc         = mkInstFunc "Call" [fidArg] in
  let returnFunc       = mkInstFunc "Return" [] in
  let handleReturnFunc = mkInstFunc "HandleReturn" [valArg] in

  let inputCharFunc	 = mkInputInstFunc "Char" [inputCharArg] in
  let inputUCharFunc	 = mkInputInstFunc "UChar" [inputUCharArg] in
  let inputShortFunc	 = mkInputInstFunc "Short" [inputShortArg] in
  let inputUShortFunc	 = mkInputInstFunc "UShort" [inputUShortArg] in
  let inputUIntFunc	 = mkInputInstFunc "UInt" [inputUIntArg] in
  let inputIntFunc	 = mkInputInstFunc "Int" [inputIntArg] in
  let inputStringFunc = mkInputInstFunc "String" [inputCharArg] in

  let mkCsvInstFunc name args =
    let ty = TFun (voidType, Some (idArg :: args), false, []) in
    let func = findOrCreateFunc f ("__DyVerify" ^ name) ty in
      func.vstorage <- Extern ;
      func.vattr <- [Attr ("crest_skip", [])] ;
      func
  in
  let csvMallocFunc  = mkCsvInstFunc "Malloc" [memAddrArg;valArg] in
  let csvCallocFunc = mkCsvInstFunc "Calloc" [memAddrArg;numArg;valArg] in
  let csvReallocFunc = mkCsvInstFunc "Realloc" [newAddrArg;oldAddrArg;valArg] in
  let csvStrDup = mkCsvInstFunc "StrDup" [newAddrArg;oldAddrArg] in
  let csvFreeFunc  =mkCsvInstFunc "Free" [memAddrArg] in

  let csvLiveMemory = mkCsvInstFunc "LiveMemory" [memAddrArg] in
  let csvStaticPathEnd = mkCsvInstFunc "StaticPathEnd" [] in

  let csvIsWarningMem = mkCsvInstFunc "IsWarningMem" [memAddrArg] in
  (*
   * Functions to create calls to the above instrumentation functions.
   *)
  let mkInstCall func args =
    let args' = integer (getNewId ()) :: args in
      Call (None, Lval (var func), args', locUnknown)
  in
  let mkInputInstCall func args =
      Call (None, Lval (var func), args, locUnknown)
  in

  let unaryOpCode op =
    let c =
      match op with
        | Neg -> 19  | BNot -> 20  |  LNot -> 21
    in
      integer c
  in

  let binaryOpCode op =
    let c =
      match op with
        | PlusA   ->  0  | MinusA  ->  1  | Mult  ->  2  | Div   ->  3
        | Mod     ->  4  | BAnd    ->  5  | BOr   ->  6  | BXor  ->  7
        | Shiftlt ->  8  | Shiftrt ->  9  | LAnd  -> 10  | LOr   -> 11
        | Eq      -> 12  | Ne      -> 13  | Gt    -> 14  | Le    -> 15
        | Lt      -> 16  | Ge      -> 17
            (* Other/unhandled operators discarded and treated concretely. *)
        | _ -> 18
    in
      integer c
  in

  let toAddr e = CastE (addrType, e) in

  let toValue e =
      if isPointerType (typeOf e) then
        CastE (valType, CastE (addrType, e))
      else
        CastE (valType, e)
  in

  let mkLoad addr value    = mkInstCall loadFunc [toAddr addr; toValue value] in
  let mkStore addr         = mkInstCall storeFunc [toAddr addr] in
  let mkClearStack ()      = mkInstCall clearStackFunc [] in
  let mkApply1 op value    = mkInstCall apply1Func [unaryOpCode op; toValue value] in
  let mkApply2 op value    = mkInstCall apply2Func [binaryOpCode op; toValue value] in
  let mkBranch bid b       = mkInstCall branchFunc [integer bid; integer b] in
  let mkCall fid           = mkInstCall callFunc [integer fid] in
  let mkReturn ()          = mkInstCall returnFunc [] in
  let mkHandleReturn value = mkInstCall handleReturnFunc [toValue value] in
  
  (*
   *)
  let mkCsvMalloc memAddr size = mkInstCall csvMallocFunc [toAddr memAddr;toValue size] in 

  let mkCsvCalloc memAddr num size = mkInstCall csvCallocFunc [toAddr memAddr;toValue num;toValue size] in

  let mkCsvRealloc newAddr oldAddr size = mkInstCall csvReallocFunc [toAddr newAddr;toAddr oldAddr;toValue size] in

  let mkCsvStrDup retAddr oldAddr = mkInstCall csvStrDup [toAddr retAddr;toAddr oldAddr] in

  let mkCsvFree memAddr = mkInstCall csvFreeFunc [toAddr memAddr] in
  
  let mkCsvLiveMemory addr = mkInstCall csvLiveMemory [toAddr addr] in
  
  let mkStaticPathEnd = mkInstCall csvStaticPathEnd [] in
  
  let mkIsWarningMem memAddr = mkInstCall csvIsWarningMem [toAddr memAddr] in
  
  (*
   * Instrument an expression.
   *)
  let rec instrumentExpr e =
    if isConstant e then
      [mkLoad noAddr e]
    else
      match e with
        | Lval lv when hasAddress lv ->
          (match lv with
            | (Mem memExp,_) ->[mkCsvLiveMemory (addressOf lv);mkLoad (addressOf lv) e]
            | _ -> [mkLoad (addressOf lv) e]
          )

        | UnOp (op, e, _) ->
            (* Should skip this if we don't currently handle 'op'. *)
            (instrumentExpr e) @ [mkApply1 op e]

        | BinOp (op, e1, e2, _) ->
            (* Should skip this if we don't currently handle 'op'. *)
            (instrumentExpr e1) @ (instrumentExpr e2) @ [mkApply2 op e]

        | CastE (_, e) ->
            (* We currently treat cast's as no-ops, which is not precise. *)
            instrumentExpr e

        (* Default case: We cannot instrument, so generate a concrete load
         * and stop recursing. *)
        | _ -> [mkLoad noAddr e]
  in
	
  (* lichcat add
   * instrumentPointerExpr for   
   *
  let rec instrumentPointerExpr e =
    if isConstant e then
      [mkCsvLoadPointer noAddr e]
    else
      match e with
        | Lval lv when hasAddress lv ->
            [mkCsvLoadPointer (addressOf lv) e]

        | UnOp (op, e, _) ->
            (* Should skip this if we don't currently handle 'op'. *)
            (instrumentPointerExpr e) @ [mkCsvPointerApply1 op e]

        | BinOp (op, e1, e2, _) ->
            (* Should skip this if we don't currently handle 'op'. *)
            (instrumentPointerExpr e1) @ (instrumentPointerExpr e2) @ [mkCsvPointerApply2 op e]

        | CastE (_, e) ->
            (* We currently treat cast's as no-ops, which is not precise. *)
            instrumentPointerExpr e

        (* Default case: We cannot instrument, so generate a concrete load
         * and stop recursing. *)
        | _ -> [mkCsvLoadPointer noAddr e]
  in
    *)
  
object (self)
  inherit nopCilVisitor


  (*
   * Instrument a statement (branch or function return).
   *)
  method vstmt(s) =
    match s.skind with
      | If (e, b1, b2, location) ->
			
          let getFirstStmtId blk = (List.hd blk.bstmts).sid in
          let b1_sid = getFirstStmtId b1 in
          let b2_sid = getFirstStmtId b2 in
		   (*(*instru PathEnd*)
			matchWarning:=false;
			instruPathMark "LK" location;
			if (!matchWarning) then
				prependToBlock [mkStaticPathEnd] b1;*)
		  (self#queueInstr (instrumentExpr e) ;
		   prependToBlock [mkBranch b1_sid 1] b1 ;
		   prependToBlock [mkBranch b2_sid 0] b2 ;
		   addBranchPair (b1_sid, b2_sid)
		  ) ;
            DoChildren

      | Return (Some e, location) ->
          if isSymbolicType (typeOf e) then
            self#queueInstr (instrumentExpr e) ;
          self#queueInstr [mkReturn ()] ;
		(*instru PathEnd*)
			(matchWarning:=false;
			instruPathMark "LK" location;
			if (!matchWarning) then
				self#queueInstr [mkStaticPathEnd] ;
			matchWarning:=false);
          SkipChildren

      | Return (None, location) ->
          self#queueInstr [mkReturn ()] ;
		  (*instru PathEnd*)
			(matchWarning:=false;
			instruPathMark "LK" location;
			if (!matchWarning) then
				self#queueInstr [mkStaticPathEnd] ;
			matchWarning:=false);
          SkipChildren

      | _ -> DoChildren


  (*
   * Instrument assignment and call statements.
   *)
  method vinst(i) =
    match i with
      | Set (lv, e, location) ->
        (match lv with
          | (Mem memExp,offset) ->
			let isbitfield f =
				match f with
				|None -> false
				|Some i -> true
			in
			let hasField off =
				match off with
				| Field (_,_)->true
				| _ ->false
			in
			let rec isBitField parent =
				match parent with
				|Field (pinfo,child) ->(
					if (hasField child) then (isBitField child)
					else (isbitfield  pinfo.fbitfield)
					)
				| _ -> false
			in

			if (not (isBitField offset)) then
				(if (isSymbolicType (typeOf e)) && (hasAddress lv) then
						(self#queueInstr (instrumentExpr e) ;
						self#queueInstr [mkStore (addressOf lv)]);
					 self#queueInstr [mkCsvLiveMemory (addressOf lv)]
				)
					
			
          | _ ->if (isSymbolicType (typeOf e)) && (hasAddress lv) then
				  (self#queueInstr (instrumentExpr e) ;
				   self#queueInstr [mkStore (addressOf lv)])
				  
        );
		(*instru PathEnd*)
		(matchWarning:=false;
		 instruPathMark "LK" location;
		 if (!matchWarning) then
		 self#queueInstr [mkStaticPathEnd] ;
		 matchWarning:=false);
          
         (* else if (isPointerType (typeOf e)) && (hasAddress lv) then  
            ((*self#queueInstr (instrumentExpr e) ;
             self#queueInstr [mkStore (addressOf lv)] ; *)
             self#queueInstr (instrumentPointerExpr e) ;
             (*self#queueInstr [mkCsvLoadPointer (addressOf lv) e] ; *)
             self#queueInstr [mkCsvStorePointer (addressOf lv) (Lval lv)]) ;
            *)
        (* SkipChildren*)DoChildren
          
      (* Don't instrument calls to functions marked as uninstrumented. *)
      | Call (_, Lval (Var f, NoOffset), _, _)
          when shouldSkipFunction f -> 
        SkipChildren
       
      | Call (ret, Lval (Var f, NoOffset),args,location)
          when ((f.vname = "malloc") or (f.vname="xmalloc"))->
         (*when (f.vname = "malloc") ->*)
        	let sizeArg = List.hd args in
          let isSymbolicExp e = isSymbolicType (typeOf e) in
			let isSymbolicLval lv = isSymbolicType (typeOfLval lv) in
			let argsToInst = List.filter isSymbolicExp args in

			self#queueInstr (concatMap instrumentExpr argsToInst) ;
            (match ret with
              |  Some lv when (hasAddress lv) ->
			  (matchWarning:=false;
			   if f.vname="malloc" then instruPathMark "MA" location
			   else if f.vname="xmalloc" then instruPathMark "XMA" location;
			   if (!matchWarning) then ChangeTo [i ; mkCsvMalloc (Lval lv) sizeArg ; mkIsWarningMem (Lval lv) ; mkClearStack()]
			   else ChangeTo [i ; mkCsvMalloc (Lval lv) sizeArg; mkClearStack()]
			  )
              |  _ -> ChangeTo [i ; mkClearStack()]
            )
              
      | Call (ret, Lval (Var f, NoOffset),args,location)
          (*when ((f.vname = "calloc") or (f.vname="xcalloc")) ->*)
          when (f.vname = "calloc")  ->
        	let numArg = List.nth args 0 in
			let sizeArg = List.nth args 1 in
          let isSymbolicExp e = isSymbolicType (typeOf e) in
			let isSymbolicLval lv = isSymbolicType (typeOfLval lv) in
			let argsToInst = List.filter isSymbolicExp args in

			self#queueInstr (concatMap instrumentExpr argsToInst) ;
            (match ret with
              |  Some lv when (hasAddress lv) ->
			  (matchWarning:=false;
			   if f.vname="calloc" then instruPathMark "CA" location
			   else if f.vname="xcalloc" then instruPathMark "XCA" location;
			   if (!matchWarning) then ChangeTo [i ; mkCsvCalloc (Lval lv) numArg sizeArg ; mkIsWarningMem (Lval lv); mkClearStack() ]
			   else ChangeTo [i ; mkCsvCalloc (Lval lv) numArg sizeArg; mkClearStack()]
			  )
              |  _ -> ChangeTo [i ; mkClearStack() ]
            )
	  | Call (ret, Lval (Var f, NoOffset),args,location)
		  when f.vname = "realloc" ->
        	let oldPtrArg = List.nth args 0 in
			let sizeArg = List.nth args 1 in
          let isSymbolicExp e = isSymbolicType (typeOf e) in
			let isSymbolicLval lv = isSymbolicType (typeOfLval lv) in
			let argsToInst = List.filter isSymbolicExp args in

			self#queueInstr (concatMap instrumentExpr argsToInst) ;
            (match ret with
              |  Some lv when (hasAddress lv) ->
			  (matchWarning:=false;
			   instruPathMark "RA" location;
			   if (!matchWarning) then ChangeTo [i ; mkCsvRealloc (Lval lv) oldPtrArg sizeArg ; mkIsWarningMem (Lval lv); mkClearStack()]
			   else ChangeTo [i ; mkCsvRealloc (Lval lv) oldPtrArg sizeArg; mkClearStack()]
			  )
              |  _ -> ChangeTo [i ; mkClearStack()]
            )
			
	  | Call (ret, Lval (Var f, NoOffset),args,location)
          when f.vname = "xstrdup" ->
        	let strAddrArg = List.hd args in
          let isSymbolicExp e = isSymbolicType (typeOf e) in
			let isSymbolicLval lv = isSymbolicType (typeOfLval lv) in
			let argsToInst = List.filter isSymbolicExp args in

			self#queueInstr (concatMap instrumentExpr argsToInst) ;
            (match ret with
              |  Some lv when (hasAddress lv) ->
			  (matchWarning:=false;
			   instruPathMark "XSD" location;
			   if (!matchWarning) then ChangeTo [i ; mkCsvStrDup (Lval lv) strAddrArg ; mkIsWarningMem (Lval lv); mkClearStack()]
			   else ChangeTo [i ; mkCsvStrDup (Lval lv) strAddrArg;mkClearStack()]
			  )
              |  _ -> ChangeTo [i ; mkClearStack()]
            )

      | Call(None, Lval(Var f, NoOffset),args,_)
          when f.vname = "free" ->	
            let frPtr = List.hd args in
          let isSymbolicExp e = isSymbolicType (typeOf e) in
			let isSymbolicLval lv = isSymbolicType (typeOfLval lv) in
			let argsToInst = List.filter isSymbolicExp args in

			self#queueInstr (concatMap instrumentExpr argsToInst) ;
        	(match frPtr with
              |  CastE (_,Lval castExp)->
                   		ChangeTo [i ;  mkCsvFree (Lval castExp);mkClearStack()]
                (* ChangeTo [i ;  mkCsvFree (addressOf castExp) (Lval castExp)] *)
              |  _ -> ChangeTo [i ; mkClearStack()]
         	)
		(*	
      | Call(ret, Lval(Var f, NoOffset),args,_)		(* int scanf( const char* format, ...); *)
		  when ((f.vname ="read") or (f.vname = "fread")) ->
			let argBuf = 
				match f.vname with
				| "read"-> List.nth args 1
				| "fread"-> List.nth args 0
			in
          let isSymbolicExp e = isSymbolicType (typeOf e) in
			let isSymbolicLval lv = isSymbolicType (typeOfLval lv) in
			let argsToInst = List.filter isSymbolicExp args in

			self#queueInstr (concatMap instrumentExpr argsToInst) ;
			(*let argCount = List.nth args 2 in*)
			(match ret with
			| Some lv when (hasAddress lv) ->
				ChangeTo  [i;mkInputInstCall inputStringFunc [argBuf];mkClearStack()]
			|  _ -> ChangeTo [i ; mkClearStack()]
			)

	  | Call(ret, Lval(Var f, NoOffset),args,_)		(* int scanf( const char* format, ...); *)
		  when ((f.vname ="fgets") or (f.vname = "gets") or (f.vname = "fgetc") or (f.vname = "getchar") or (f.vname = "getc")) ->
          let isSymbolicExp e = isSymbolicType (typeOf e) in
			let isSymbolicLval lv = isSymbolicType (typeOfLval lv) in
			let argsToInst = List.filter isSymbolicExp args in

			self#queueInstr (concatMap instrumentExpr argsToInst) ;
			(match ret with
			| Some lv  ->
				(match f.vname with
				 | "fgets"  -> ChangeTo  [i;mkInputInstCall inputStringFunc [Lval lv];mkClearStack()]
				 | "gets"   -> ChangeTo  [i;mkInputInstCall inputStringFunc [Lval lv];mkClearStack()]
				 | "fgetc" when (hasAddress lv) -> ChangeTo [i;mkInputInstCall inputCharFunc [addressOf lv];mkClearStack()]
				 | "getchar" when (hasAddress lv) -> ChangeTo [i;mkInputInstCall inputCharFunc [addressOf lv];mkClearStack()]
				 | "getc" when (hasAddress lv) -> ChangeTo [i;mkInputInstCall inputCharFunc [addressOf lv];mkClearStack()]
				 | _ ->ChangeTo[i]
				)
			| _ -> ChangeTo [i ; mkClearStack()]
			)

      | Call(_, Lval(Var f, NoOffset),args,_)		(* int scanf( const char* format, ...); *)
          when ((f.vname = "scanf") or (f.vname = "fscanf")) -> (*| f.vname = "sscanf"*)
		  let instruList = ref [] in
		  let instruINPUTs = ref [] in
		  let handleInput  arg =
			let instrumatchedINPUTs funcname =
				(match funcname with
				 | "UChar" -> instruINPUTs := (mkInputInstCall inputUCharFunc [arg])::!instruINPUTs 
				 | "Char" ->instruINPUTs := (mkInputInstCall inputCharFunc [arg])::!instruINPUTs
				 | "UShort" ->instruINPUTs := (mkInputInstCall inputUShortFunc [arg])::!instruINPUTs
				 | "Short" ->instruINPUTs := (mkInputInstCall inputShortFunc [arg])::!instruINPUTs
				 | "UInt" ->instruINPUTs := (mkInputInstCall inputUIntFunc [arg])::!instruINPUTs
				 | "Int" ->instruINPUTs := (mkInputInstCall inputIntFunc [arg])::!instruINPUTs
				 | "Str" ->instruINPUTs := (mkInputInstCall inputStringFunc [arg;])::!instruINPUTs
				 | _ ->()
				)
			in
          let isSymbolicExp e = isSymbolicType (typeOf e) in
			let isSymbolicLval lv = isSymbolicType (typeOfLval lv) in
			let argsToInst = List.filter isSymbolicExp args in

			self#queueInstr (concatMap instrumentExpr argsToInst) ;
		    (match arg with
			 |	CastE (_,Const CStr cstr) ->
				let remove_blank = Str.global_replace (Str.regexp "[ \t]") "" in
				let inputTypes = Str.split (Str.regexp "%") (remove_blank cstr) in
				let addToInstruList typStr =
					let match_format fm_exp = Str.string_match (Str.regexp fm_exp) typStr 0 in
					if (match_format ".*\\*.*") then ()
					else if(match_format "^.*hhu$") then instruList:="UChar"::!instruList
					else if(match_format "^.*hu$") then instruList:="UShort"::!instruList
					else if(match_format "^.*u$") then instruList:="UInt"::!instruList
					else if(match_format "^.*h[dioxX]$") then instruList:="Short"::!instruList
					else if(match_format "^.*[dioxX]$") then instruList:="Int"::!instruList
					else if(match_format "^.*c$") then instruList:="Char"::!instruList 
					else if(match_format "^.*s$") then instruList:="Str"::!instruList 
					(*else if(match_format "fFeEgG") then()*)
				in
				(List.iter addToInstruList  inputTypes;
				instruList:= List.rev !instruList;
				)
			 |	AddrOf lval -> 
				if  (List.length !instruList)>0 then(
				let funcname = List.hd !instruList in
				instruList:= List.tl !instruList;
				instrumatchedINPUTs funcname)

			 |  Lval lv ->
				if  (List.length !instruList)>0 then(
					let funcname = List.hd !instruList in
					 instruList:= List.tl !instruList;
				     instrumatchedINPUTs funcname
					 )
			 |  StartOf lv ->
				if  (List.length !instruList)>0 then(
					let funcname = List.hd !instruList in
					 instruList:= List.tl !instruList;
				     instrumatchedINPUTs funcname)
			 |  _ -> (Printf.printf "-------------Others-Input-Exp-Type-----------")
			)
		  in
		  if(f.vname = "fscanf") then
			List.iter handleInput (List.tl args)
		  else
		  List.iter handleInput args;
		  instruINPUTs := (mkClearStack())::!instruINPUTs;
		  instruINPUTs := List.rev !instruINPUTs;
		  (*instruINPUTs := i::!instruINPUTs;*)
		  ChangeTo !instruINPUTs
*)
      | Call (ret, Lval(Var f,NoOffset), args, _)
        when f.vname <> "exit" ->
          let isSymbolicExp e = isSymbolicType (typeOf e) in
		  let liveMemArg e =
			let isPointerExp e = isPointerType (typeOf e) in
			let notbitfield e =
				(match e with
				 |CastE (_,Const c)->false	(*althrough not bitfield but not instru, so false*)
				 |Lval lv ->
					(match lv with
					 | (Mem memExp,offset) ->
						let isbitfield f =
							match f with
							|None -> false
							|Some i ->true
							in
						let hasField off =
							match off with
							| Field (_,_)->true
							| _ ->false
							in
						let rec isBitField parent =
							match parent with
							|Field (pinfo,child) ->(
								if (hasField child) then (isBitField child)
								else (isbitfield  pinfo.fbitfield)
								)
							| _ -> false
							in
						isBitField offset
					| _ ->true
					)
				| _ -> true
				)
			in
			(isPointerExp e) && (notbitfield e)
		  in
          let isSymbolicLval lv = isSymbolicType (typeOfLval lv) in
         (* let isPointerLval lv = isPointerType (typeOfLval lv) in  *)
          let argsToInst = List.filter isSymbolicExp args in
          let pointerArgsToCheck = List.filter liveMemArg args in	

          self#queueInstr (concatMap instrumentExpr argsToInst) ;
		  (*check pointer type argument for live memory use*)
		  List.iter (fun x->  self#queueInstr [mkCsvLiveMemory x]) pointerArgsToCheck ;

         (*	self#queueInstr (concatMap instrumentPointerExpr pointerArgsToInst) ;	*)
            (match ret with
               | Some lv when ((isSymbolicLval lv) && (hasAddress lv)) ->
                   ChangeTo [i ;
                             mkHandleReturn (Lval lv) ;
                             mkStore (addressOf lv)]
               (*  
               | Some lv when ((isPointerLval lv) && (hasAddress lv))  ->
                   ChangeTo [i ;
                             mkCsvHandleReturnPointer (addressOf lv) (Lval lv) ;
                             mkCsvStorePointer (addressOf lv) (Lval lv)]
                *) 
               | _ ->
        		 ChangeTo [i ; 
        					 mkClearStack ()])
      | _ -> DoChildren


  (*
   * Instrument function entry.
   * lichcat add instParamPointer and isPointerType
   * using by pointerParamsToInst
   *)
  method vfunc(f) =
    if shouldSkipFunction f.svar then
      SkipChildren
    else
      let instParam v = mkStore (addressOf (var v)) in
      (*let instPointerParam v = mkCsvStorePointer (addressOf (var v)) (Lval (var v)) in	*)
      let isSymbolic v = isSymbolicType v.vtype in
      (*let isPointer v = isPointerType v.vtype in	*)
      let (_, _, isVarArgs, _) = splitFunctionType f.svar.vtype in
      let paramsToInst = List.filter isSymbolic f.sformals in
      (*let pointerParamsToInst = List.filter isPointer f.sformals in	*)
        addFunction () ;
        if (not isVarArgs) then 
          prependToBlock (List.rev_map instParam paramsToInst) f.sbody ;
          (*(prependToBlock (List.rev_map instParam paramsToInst) f.sbody ;
          prependToBlock (List.rev_map instPointerParam pointerParamsToInst) f.sbody) ;	*)
        prependToBlock [mkCall !funCount] f.sbody ;
        DoChildren

end


let addCrestInitializer f =
  let crestInitTy = TFun (voidType, Some [], false, []) in
  let crestInitFunc = findOrCreateFunc f "__CrestInit" crestInitTy in
  let globalInit = getGlobInit f in
    crestInitFunc.vstorage <- Extern ;
    crestInitFunc.vattr <- [Attr ("crest_skip", [])] ;
    prependToBlock [Call (None, Lval (var crestInitFunc), [], locUnknown)]
                   globalInit.sbody



let prepareGlobalForCFG glob =
  match glob with
    GFun(func, _) -> prepareCFG func
  | _ -> ()


let feature : featureDescr =
  { fd_name = "CrestInstrument";
    fd_enabled = ref false;
    fd_description = "instrument a program for use with CREST";
    fd_extraopt = [];
    fd_post_check = true;
    fd_doit =
      function (f: file) ->
        ((* Simplify the code:
          *  - simplifying expressions with complex memory references
          *  - converting loops and switches into goto's and if's
          *  - transforming functions to have exactly one return *)
          Simplemem.feature.fd_doit f ;
          iterGlobals f prepareGlobalForCFG ;
          (*Oneret.feature.fd_doit f ;*) 
          (* To simplify later processing:
           *  - ensure that every 'if' has a non-empty else block
           *  - try to transform conditional expressions into predicates
           *    (e.g. "if (!x) {}" to "if (x == 0) {}") *)
          (let ncVisitor = new normalizeConditionalsVisitor in
             visitCilFileSameGlobals (ncVisitor :> cilVisitor) f) ;
          (* Clear out any existing CFG information. *)
          Cfg.clearFileCFG f ;

    	  readCurrentCheckFile();
		  (* debug
		  Printf.printf "%d\n" !warningId;
		  (let printWarningPath lines =
		     let printlines s =
				Printf.printf "%s " s in
			 List.iter printlines lines;
			 Printf.printf "\n" in
		  List.iter printWarningPath !warningPath ) ;
		  *)

          (* Read the ID and statement counts from files.  (This must
           * occur after clearFileCFG, because clearFileCfg clobbers
           * the statement counter.) *)
          readIdCount () ;
          readStmtCount () ;
          readFunCount () ;
          (* Compute the control-flow graph. *)
		  (*instrument static warning path mark before compute CFG *)
		(let ipmv = new instruPathMarkVisitor f in
			visitCilFileSameGlobals (ipmv :> cilVisitor) f) ; 
		  (*when writting to cfg we've changed the __StaticPathMark to
		   __StaticPathMark_P[digit]+_S[digit]+, by record the 
		   pathId and pathStmtId into a list(storePathMarkIdList)
		   but add to its front, so reverse the storePathMarkIdList here!
		   *)
		  storePathMarkIdList := List.rev !storePathMarkIdList;
          Cfg.computeFileCFG f ;
          (* Adds function calls to the CFG, by building a map from
           * function names to the first statements in those functions
           * and by explicitly adding edges for calls to functions
           * defined in this file. *)
          handleCallEdgesAndWriteCfg f ;
          (* Finally instrument the program. *)
	  (let instVisitor = new crestInstrumentVisitor f in
             visitCilFileSameGlobals (instVisitor :> cilVisitor) f) ;
          (* Add a function to initialize the instrumentation library. *)
          addCrestInitializer f ;

          (* Write the ID and statement counts, the branches. *)
          writeIdCount () ;
          writeStmtCount () ;
          writeFunCount () ;
          writeBranches ());
  }
