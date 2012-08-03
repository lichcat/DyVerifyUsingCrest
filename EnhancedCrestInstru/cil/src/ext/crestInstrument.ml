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
		warningPath:= words::!warningPath;
		iter_lines chan
	with End_of_file -> () in
	iter_lines f;
	close_in f;
  warningPath:=List.tl !warningPath;
  warningPath:=List.rev !warningPath;
  let lines=List.hd !warningPath in
  let hdline=List.hd lines in
  warningId:=int_of_string hdline;
  warningPath:=List.tl !warningPath

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
(* 
let file_pathEnd location =
  !currentCheckFile=location.file && (!pathEndLineNumber)==location.line
let file_targetMem location =
  !currentCheckFile=location.file && (!warningMemLine)==location.line  	
 *) 
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
    if not (f.vstorage = Static) then
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
  let ptrAddrArg =("ptrAddr", addrType, []) in
  let memAddrArg =("memAddr", addrType, []) in
  let retPtrAddrArg =("retPtrAddr", addrType,[]) in 
  let retPtrValueArg =("retPtrValue", addrType, []) in
  let pathIdArg =("pathIdArg",idType, []) in
  let pathStmtIdArg = ("pathStmtIdArg",idType,[]) in

  let mkInstFunc name args =
    let ty = TFun (voidType, Some (idArg :: args), false, []) in
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
  (*
     add by lichcat
     csvMallocFunc for __csvMalloc
     csvFreeFunc for __csvFree
     csvLoadPointerFunc for __csvLoadPointer
     csvStorePointerFunc for __csvStorePointer
     csvPointerApply1/2 for __csvPointerApply1/2
     csvHandleReturnPointer for __csvHandleReturnPointer
     csvClearPointerStack fro __csvClearPointerStack
   *)
  let mkCsvInstFunc name args =
    let ty = TFun (voidType, Some (idArg :: args), false, []) in
    let func = findOrCreateFunc f ("__DyVerify" ^ name) ty in
      func.vstorage <- Extern ;
      func.vattr <- [Attr ("crest_skip", [])] ;
      func
  in
  let csvMallocFunc  = mkCsvInstFunc "Malloc" [memAddrArg;valArg] in
  let csvFreeFunc  =mkCsvInstFunc "Free" [memAddrArg] in
  (*
  let csvMallocFunc  = mkCsvInstFunc "Malloc" [ptrAddrArg;memAddrArg;valArg] in
  let csvFreeFunc  =mkCsvInstFunc "Free" [ptrAddrArg;memAddrArg] in
  let csvLoadPointerFunc  = mkCsvInstFunc "LoadPointer" [ptrAddrArg;memAddrArg] in
  let csvStorePointerFunc  = mkCsvInstFunc "StorePointer" [ptrAddrArg;memAddrArg] in
  let csvPointerApply1 = mkCsvInstFunc "PointerApply1" [opArg; valArg] in
  let csvPointerApply2 = mkCsvInstFunc "PointerApply2" [opArg; valArg] in
  let csvHandleReturnPointerFunc = mkCsvInstFunc "HandleReturnPointer" [retPtrAddrArg; retPtrValueArg] in
  let csvClearPointerStackFunc = mkCsvInstFunc "ClearPointerStack" [] in
  *)
  let csvLiveMemory = mkCsvInstFunc "LiveMemory" [memAddrArg;valArg] in
  let csvStaticPathEnd = mkCsvInstFunc "StaticPathEnd" [] in

  let csvPathMark = mkCsvInstFunc "PathMark" [pathIdArg;pathStmtIdArg] in
  (*let csvIsWarningMem = mkCsvInstFunc "IsWarningMem" [memAddrArg] in*)
  (*
   * Functions to create calls to the above instrumentation functions.
   *)
  let mkInstCall func args =
    let args' = integer (getNewId ()) :: args in
      Call (None, Lval (var func), args', locUnknown)
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
     Instrument csv functions 
     __csvMalloc,__csvRealloc,__csvCalloc,__csvFree
     __csvLoadPointer,__csvStorePointer
     __csvPointerApply1,__csvPointerApply2
     __csvHandleReturnPointer
     __csvClearPointerStack
   *)
  let mkCsvMalloc memAddr size = mkInstCall csvMallocFunc [toAddr memAddr;toValue size] in 
  let mkCsvFree memAddr = mkInstCall csvFreeFunc [toAddr memAddr] in
  
  (* reference count
  let mkCsvMalloc ptrAddr memAddr size = mkInstCall csvMallocFunc [toAddr ptrAddr; toAddr memAddr;toValue size] in 
  let mkCsvFree ptrAddr memAddr = mkInstCall csvFreeFunc [toAddr ptrAddr; toAddr memAddr] in
 
  let mkCsvLoadPointer ptrAddr memAddr  = mkInstCall csvLoadPointerFunc [toAddr ptrAddr; toAddr memAddr] in
  let mkCsvStorePointer ptrAddr memAddr   = mkInstCall csvStorePointerFunc [toAddr ptrAddr; toAddr memAddr] in
  let mkCsvPointerApply1 op value = mkInstCall csvPointerApply1 [unaryOpCode op; toValue value] in
  let mkCsvPointerApply2 op value = mkInstCall csvPointerApply2 [binaryOpCode op; toValue value] in
  let mkCsvHandleReturnPointer retPtrAddr retPtrValue = mkInstCall csvHandleReturnPointerFunc [toAddr retPtrAddr; toValue retPtrValue] in
  let mkCsvClearPointerStack ()  = mkInstCall csvClearPointerStackFunc [] in
  *)
  let mkCsvLiveMemory addr value = mkInstCall csvLiveMemory [toAddr addr; toValue value] in
  
  let mkStaticPathEnd = mkInstCall csvStaticPathEnd [] in
  let mkPathMark pathId pathStmtId = mkInstCall csvPathMark [integer pathId; integer pathStmtId] in
  (*
  let mkIsWarningMem memAddr = mkInstCall csvIsWarningMem [toAddr memAddr] in
  *)
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
            | (Mem memExp,_) ->[mkCsvLiveMemory (addressOf lv) e;mkLoad (addressOf lv) e]
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
	    (self#queueInstr (instrumentExpr e) ;
	     prependToBlock [mkBranch b1_sid 1] b1 ;
	     prependToBlock [mkBranch b2_sid 0] b2 ;
             addBranchPair (b1_sid, b2_sid);
			  matchWarning:=false;
			   instruPathMark "BT" location;
			   if (!matchWarning) then
				  prependToBlock [mkPathMark !warningId !pathStmtId] b1 ;
			   matchWarning:=false;	
			   instruPathMark "BF" location;
			   if (!matchWarning) then
				  prependToBlock [mkPathMark !warningId !pathStmtId] b2 ;
			   matchWarning:=false;
			) ;
            DoChildren

      | Return (Some e, location) ->
	  (*
          if(file_pathEnd location) then
            self#queueInstr [mkStaticPathEnd];
			*)
          if isSymbolicType (typeOf e) then
            self#queueInstr (instrumentExpr e) ;
          self#queueInstr [mkReturn ()] ;
          SkipChildren

      | Return (None, location) ->
          (*if(file_pathEnd location) then
            self#queueInstr [mkStaticPathEnd];
			*)
          self#queueInstr [mkReturn ()] ;
          SkipChildren

      | _ -> DoChildren


  (*
   * Instrument assignment and call statements.
   *)
        (*
         * lichcat
         * add isPointerType :handle pointer assignment
         * and add insert __csvLoadPointer and __csvStorePointer 
         self#queueInstr [mkCsvLiveMemory (addressOf lv) e];
           
           *)
  method vinst(i) =
    match i with
      | Set (lv, e, location) ->
	  (*
          if(file_pathEnd location) then
            self#queueInstr [mkStaticPathEnd];
			*)
        (match lv with
          | (Mem memExp,_) ->
            if (isSymbolicType (typeOf e)) && (hasAddress lv) then
            (self#queueInstr (instrumentExpr e) ;
             self#queueInstr [mkStore (addressOf lv)]);
            self#queueInstr [mkCsvLiveMemory (addressOf lv) (Lval lv)];
          | _ -> 
            if (isSymbolicType (typeOf e)) && (hasAddress lv) then
            (self#queueInstr (instrumentExpr e) ;
             self#queueInstr [mkStore (addressOf lv)])
        );
          
         (* else if (isPointerType (typeOf e)) && (hasAddress lv) then  
            ((*self#queueInstr (instrumentExpr e) ;
             self#queueInstr [mkStore (addressOf lv)] ; *)
             self#queueInstr (instrumentPointerExpr e) ;
             (*self#queueInstr [mkCsvLoadPointer (addressOf lv) e] ; *)
             self#queueInstr [mkCsvStorePointer (addressOf lv) (Lval lv)]) ;
            *)
         SkipChildren
          
      (* Don't instrument calls to functions marked as uninstrumented. *)
      | Call (_, Lval (Var f, NoOffset), _, location)
          when shouldSkipFunction f -> 
		  (*
          	if(file_pathEnd location) then
            	self#queueInstr [mkStaticPathEnd];
				*)
        SkipChildren
        
      | Call (ret, Lval (Var f, NoOffset),args,location)
          when f.vname = "malloc" ->
        	let sizeArg = List.hd args in
            (match ret with
              |  Some lv when (hasAddress lv) ->
			  (*
          		if(file_pathEnd location) then
                   		ChangeTo [i ;  mkCsvMalloc (Lval lv) sizeArg; mkStaticPathEnd]
	  		else if(file_targetMem location ) then
	    			ChangeTo [i ;  mkCsvMalloc (Lval lv) sizeArg; mkIsWarningMem (Lval lv)]
                   	else *) 
              	   		ChangeTo [i ;  mkCsvMalloc (Lval lv) sizeArg]
                (* ChangeTo [i ;  mkCsvMalloc (addressOf lv) (Lval lv) sizeArg]	*)
              |  _ -> DoChildren
            )
              
      | Call(None, Lval(Var f, NoOffset),args,location)
          when f.vname = "free" ->	
            let frPtr = List.hd args in
        	(match frPtr with
              |  CastE (_,Lval castExp)->
			  (*
          		if(file_pathEnd location) then
                		ChangeTo [i ;  mkCsvFree (Lval castExp); mkStaticPathEnd]
						
                 	else *)
                   		ChangeTo [i ;  mkCsvFree (Lval castExp)]
                (* ChangeTo [i ;  mkCsvFree (addressOf castExp) (Lval castExp)] *)
              |  _ -> DoChildren
         	)
      (*  
       *  lichcat add 
       *  isPointerExp for pointerArgsToInst
       *  to instrument pointer args
       *)

      | Call (ret, _, args, location) ->
          let isSymbolicExp e = isSymbolicType (typeOf e) in
         (* let isPointerExp e = isPointerType (typeOf e) in	*)
          let isSymbolicLval lv = isSymbolicType (typeOfLval lv) in
         (* let isPointerLval lv = isPointerType (typeOfLval lv) in  *)
          let argsToInst = List.filter isSymbolicExp args in
         (* let pointerArgsToInst = List.filter isPointerExp args in	*) 
            self#queueInstr (concatMap instrumentExpr argsToInst) ;
        	
         (*	self#queueInstr (concatMap instrumentPointerExpr pointerArgsToInst) ;	*)
            (match ret with
               | Some lv when ((isSymbolicLval lv) && (hasAddress lv)) ->
			   (*
          	  if(file_pathEnd location) then
                   ChangeTo [i ;
                             mkHandleReturn (Lval lv) ;
                             mkStore (addressOf lv) ;
                             mkStaticPathEnd]
                   else *)ChangeTo [i ;
                             mkHandleReturn (Lval lv) ;
                             mkStore (addressOf lv)]
               (*  
               | Some lv when ((isPointerLval lv) && (hasAddress lv))  ->
                   ChangeTo [i ;
                             mkCsvHandleReturnPointer (addressOf lv) (Lval lv) ;
                             mkCsvStorePointer (addressOf lv) (Lval lv)]
                *) 
               | _ ->
			   (*
          	  if(file_pathEnd location) then
                   ChangeTo [i ; 
                     		 (*mkCsvClearPointerStack () ; *)
                     		 mkClearStack ();
                             mkStaticPathEnd]
        		else*) ChangeTo [i ; 
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
