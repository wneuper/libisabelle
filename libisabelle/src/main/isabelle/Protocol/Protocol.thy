theory Protocol
imports 
  "/home/wneuper/proto4/libisabelle/libisabelle/src/main/isabelle/Protocol/Codec"
  (* isabelle build runs in /usr/local/isabisac/ and thus requires this ^^^ path *)
  "~~/src/Tools/isac/Knowledge/Build_Thydata"
keywords "operation_setup" :: thy_decl % "ML"
begin

ML\<open>
signature LIBISABELLE = sig
  type name = string
  type ('i, 'o) operation =
    {from_lib : 'i codec,
     to_lib : 'o codec,
     action : 'i -> 'o}

  val add_operation : name -> ('i, 'o) operation -> unit
  val operation_setup : bstring -> Symbol_Pos.source -> theory -> unit
end

structure Libisabelle : LIBISABELLE = struct

type name = string
type ('i, 'o) operation =
  {from_lib : 'i codec,
   to_lib : 'o codec,
   action : 'i -> 'o}

type raw_operation = XML.tree -> XML.tree

exception GENERIC of string

val operations =
  Synchronized.var "libisabelle.operations" (Symtab.empty: raw_operation Symtab.table)

fun add_operation name {from_lib, to_lib, action} =
  let
    fun raw tree =
      case Codec.decode from_lib tree of
        Codec.Success i => Codec.encode to_lib (action i)
      | Codec.Failure (msg, _) => raise Fail ("decoding input failed " ^ msg)
  in
    Synchronized.change operations (Symtab.update (name, raw))
  end

val _ = Isabelle_Process.protocol_command "libisabelle"
  (fn id :: name :: [arg] =>
    let
      val id = Markup.parse_int id
      val response =
        [(Markup.functionN, "libisabelle_response"),
         ("id", Markup.print_int id)]
      val args = YXML.parse arg
      fun exec f =
        (Future.fork (fn () =>
          let
            val res = Exn.interruptible_capture f args
            val yxml = YXML.string_of (Codec.encode (Codec.exn_result Codec.id) res)
          in
            Output.protocol_message response [yxml]
          end);
        ())
    in
      (case Symtab.lookup (Synchronized.value operations) name of
        SOME operation => exec operation
      | NONE => exec (fn _ => raise Fail "libisabelle: unknown command"))
    end)

fun operation_setup name source thy =
  ML_Context.eval_in (SOME (Proof_Context.init_global thy)) ML_Compiler.flags (#pos source)
    (ML_Lex.read Position.none ("Libisabelle.add_operation " ^ ML_Syntax.print_string name ^ "(") @
      ML_Lex.read_source false source @
      ML_Lex.read Position.none ")")

end

val _ = Outer_Syntax.command @{command_spec "operation_setup"} "define protocol operation in ML"
  (Parse.name -- Parse.!!! (@{keyword "="} |-- Parse.ML_source)
    >> (fn (name, txt) => Toplevel.theory (tap (Libisabelle.operation_setup name txt))))
\<close>

section \<open>Keep Hello_PIDE.java alive\<close>

operation_setup hello = \<open>
  {from_lib = Codec.string,
   to_lib = Codec.string,
   action = (fn data => "Hello " ^ data)}\<close>

operation_setup teststr = \<open>
  {from_lib = Codec.string,
   to_lib = Codec.string,
   action = (fn data => "teststr returns " ^ data)}\<close>

operation_setup testit = \<open>
  {from_lib = Codec.int,
   to_lib = Codec.tree,
   action = (fn calcid => 
	 let 
	   val result =
	     XML.Elem (("ADDUSER", []),
         [XML.Elem (("CALCID", []), [XML.Text (string_of_int 1)]),
         XML.Elem (("USERID", []), [XML.Text (string_of_int 1)])])
	 in result end)}\<close>

section \<open>Operation setup for Math_Engine : MATH_ENGINE\<close>
ML {*
*} ML {*
*}
text \<open>The sequence below follows structure Math_Engine : MATH_ENGINE\<close>
(* val appendFormula : calcID -> cterm' -> XML.tree ------------------------ *)

(* val autoCalculate : calcID -> auto -> XML.tree -------------------------- *)
operation_setup autocalculate = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("AUTOCALC", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]), a]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
	 in result end)}\<close>

(* val applyTactic : calcID -> pos' -> tac -> XML.tree --------------------- *)

(* val CalcTree : fmz list -> XML.tree ------------------------------------- *)
operation_setup calctree = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let
	   val fmz = case intree of
	       tree as XML.Elem (("FORMALIZATION", []), vars) => xml_to_fmz tree
       | tree => error ("calctree: intree =" ^ xmlstr 0 tree)
	   val result = Math_Engine.CalcTree fmz
	 in result end)} \<close>

(* val checkContext : calcID -> pos' -> guh -> XML.tree -------------------- *)

(* val DEconstrCalcTree : calcID -> XML.tree ------------------------------- *)
operation_setup deconstrcalctree = \<open>
  {from_lib = Codec.int,
   to_lib = Codec.tree,
   action = (fn calcid => 
	 let 
	   val result = Math_Engine.DEconstrCalcTree calcid
	 in result end)}\<close>

(* val fetchApplicableTactics : calcID -> int -> pos' -> XML.tree ---------- *)
(* val fetchProposedTactic : calcID -> XML.tree ---------------------------- *)
(* val findFillpatterns : calcID -> errpatID -> XML.tree ------------------- *)
(* val getAccumulatedAsms : calcID -> pos' -> XML.tree --------------------- *)
(* val getActiveFormula : calcID -> XML.tree ------------------------------- *)
(* val getAssumptions : calcID -> pos' -> XML.tree ------------------------- *)

(* val getFormulaeFromTo : calcID -> pos' -> pos' -> int -> bool -> XML.tree *)
operation_setup getformulaefromto = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let
	   val (calcid, from, to, level, rules) = case intree of
       XML.Elem (("GETFORMULAEFROMTO", []), [
         XML.Elem (("CALCID", []), [XML.Text calcid]),
         from as XML.Elem (("POSITION", []), [
           XML.Elem (("INTLIST", []), _),
           XML.Elem (("POS", []), [XML.Text _])]),
         to as XML.Elem (("POSITION", []), [
           XML.Elem (("INTLIST", []), _),
           XML.Elem (("POS", []), [XML.Text _])]),
         XML.Elem (("INT", []), [XML.Text level]),
         XML.Elem (("BOOL", []), [XML.Text rules])]) 
       => (str2int calcid, xml_to_pos from, xml_to_pos to, str2int level, string_to_bool rules)
     | tree => error ("getformulaefromto: WRONG intree = " ^ xmlstr 0 tree)
     val result = Math_Engine.getFormulaeFromTo calcid from to level rules
	 in result end)}\<close>

(* val getTactic : calcID -> pos' -> XML.tree ------------------------------ *)
(* val initContext : calcID -> ketype -> pos' -> XML.tree ------------------ *)
(* val inputFillFormula: calcID -> string -> XML.tree ---------------------- *)
(* val interSteps : calcID -> pos' -> XML.tree ----------------------------- *)

(* val Iterator : calcID -> XML.tree --------------------------------------- *)
operation_setup iterator = \<open>
  {from_lib = Codec.int,
   to_lib = Codec.tree,
   action = (fn calcid => 
	 let
     val result = Math_Engine.Iterator calcid
	 in result end)}\<close>

(* val IteratorTEST : calcID -> iterID ------------------------------------- *)
(* val modelProblem : calcID -> XML.tree ----------------------------------- *)
(* val modifyCalcHead : calcID -> icalhd -> XML.tree ----------------------- *)
(* val moveActiveCalcHead : calcID -> XML.tree ----------------------------- *)
(* val moveActiveDown : calcID -> XML.tree --------------------------------- *)
(* val moveActiveFormula : calcID -> pos' -> XML.tree ---------------------- *)
(* val moveActiveLevelDown : calcID -> XML.tree ---------------------------- *)
(* val moveActiveLevelUp : calcID -> XML.tree ------------------------------ *)

(* val moveActiveRoot : calcID -> XML.tree --------------------------------- *)
operation_setup moveactiveroot = \<open>
  {from_lib = Codec.int,
   to_lib = Codec.tree,
   action = (fn calcid => 
	 let
	   val result = Math_Engine.moveActiveRoot calcid
	 in result end)}\<close>

(* val moveActiveRootTEST : calcID -> XML.tree ----------------------------- *)
(* val moveActiveUp : calcID -> XML.tree ----------------------------------- *)
(* val moveCalcHead : calcID -> pos' -> XML.tree --------------------------- *)
(* val moveDown : calcID -> pos' -> XML.tree ------------------------------- *)
(* val moveLevelDown : calcID -> pos' -> XML.tree -------------------------- *)
(* val moveLevelUp : calcID -> pos' -> XML.tree ---------------------------- *)
(* val moveRoot : calcID -> XML.tree --------------------------------------- *)
(* val moveUp : calcID -> pos' -> XML.tree --------------------------------- *)

(* val refFormula : calcID -> pos' -> XML.tree ----------------------------- *)
operation_setup refformula = \<open> (* ATTENTION: 2nd call in step 10 WITH DIFFERENT result *)
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, p) = case intree of
       XML.Elem (("REFFORMULA", []), [
           XML.Elem (("CALCID", []), [XML.Text ci]), 
           p]) => (ci, p)
     val SOME calcid = int_of_str ci
     val pos = xml_to_pos p
     val result = Math_Engine.refFormula calcid pos
	 in result end)}\<close>

(* val refineProblem : calcID -> pos' -> guh -> XML.tree ------------------- *)
(* val replaceFormula : calcID -> cterm' -> XML.tree ----------------------- *)
(* val requestFillformula : calcID -> errpatID * fillpatID -> XML.tree ----- *)
(* val resetCalcHead : calcID -> XML.tree ---------------------------------- *)
(* val setContext : calcID -> pos' -> guh -> XML.tree ---------------------- *)
(* val setMethod : calcID -> metID -> XML.tree ----------------------------- *)
(* val setNextTactic : calcID -> tac -> XML.tree --------------------------- *)
(* val setProblem : calcID -> pblID -> XML.tree ---------------------------- *)
(* val setTheory : calcID -> thyID -> XML.tree ----------------------------- *)

section \<open>Native libisabelle: operation_setup use_thys\<close>

operation_setup use_thys = \<open>
  {from_lib = Codec.list Codec.string,
   to_lib = Codec.unit,
   action = Thy_Info.use_thys o map (rpair Position.none)}\<close>

end
