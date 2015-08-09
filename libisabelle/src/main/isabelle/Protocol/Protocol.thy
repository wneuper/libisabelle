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
subsection \<open>Survey on input and output\<close>
text \<open>
  The sequence of code below follows the sequence of the signature;
  The structure of the resulting XML.tree is given by the respective fun:
signature MATH_ENGINE =
  sig
    val appendFormula : calcID -> cterm' -> XML.tree
          appendformulaOK2xml
          appendformulaERROR2xml
          sysERROR2xml
    val autoCalculate : calcID -> auto -> XML.tree
          autocalculateOK2xml
          autocalculateERROR2xml
          sysERROR2xml
    val applyTactic : calcID -> pos' -> tac -> XML.tree
          autocalculateOK2xml
          autocalculateERROR2xml
    val CalcTree : fmz list -> XML.tree
          calctreeOK2xml
          sysERROR2xml
    val checkContext : calcID -> pos' -> guh -> XML.tree
          message2xml
          contextthyOK2xml
          sysERROR2xml
    val DEconstrCalcTree : calcID -> XML.tree
          deconstructcalctreeOK2xml
    val fetchApplicableTactics : calcID -> int -> pos' -> XML.tree
          applicabletacticsOK
          sysERROR2xml
    val fetchProposedTactic : calcID -> XML.tree
          fetchproposedtacticOK2xml
          fetchproposedtacticERROR2xml
          sysERROR2xml
    val findFillpatterns : calcID -> errpatID -> XML.tree
          findFillpatterns2xml
    val getAccumulatedAsms : calcID -> pos' -> XML.tree
          getasmsOK2xml
          sysERROR2xml
    val getActiveFormula : calcID -> XML.tree
          iteratorOK2xml
    val getAssumptions : calcID -> pos' -> XML.tree
          getasmsOK2xml
          sysERROR2xml
    val getFormulaeFromTo : calcID -> pos' -> pos' -> int -> bool -> XML.tree
          getintervalOK
          sysERROR2xml
    val getTactic : calcID -> pos' -> XML.tree
          gettacticOK2xml
          gettacticERROR2xml
          sysERROR2xml
    val initContext : calcID -> ketype -> pos' -> XML.tree
          message2xml
          contextthyOK2xml
          sysERROR2xml
    val inputFillFormula: calcID -> string -> XML.tree
          autocalculateOK2xml
          autocalculateERROR2xml
          message2xml
    val interSteps : calcID -> pos' -> XML.tree
          interStepsOK
          interStepsERROR
          sysERROR2xml
    val Iterator : calcID -> XML.tree
          adduserOK2xml
          sysERROR2xml
    val IteratorTEST : calcID -> iterID
    val modelProblem : calcID -> XML.tree
          modifycalcheadOK2xml
          sysERROR2xml
    val modifyCalcHead : calcID -> icalhd -> XML.tree
          modifycalcheadOK2xml
          sysERROR2xml
    val moveActiveCalcHead : calcID -> XML.tree
          iteratorOK2xml
          sysERROR2xml
    val moveActiveDown : calcID -> XML.tree
          iteratorOK2xml
          iteratorERROR2xml
          sysERROR2xml
    val moveActiveFormula : calcID -> pos' -> XML.tree
          iteratorOK2xml
          sysERROR2xml
    val moveActiveLevelDown : calcID -> XML.tree
          iteratorOK2xml
          iteratorERROR2xml
          sysERROR2xml
    val moveActiveLevelUp : calcID -> XML.tree
          iteratorOK2xml
          iteratorERROR2xml
          sysERROR2xml
    val moveActiveRoot : calcID -> XML.tree
          iteratorOK2xml
          sysERROR2xml
    val moveActiveRootTEST : calcID -> XML.tree
    val moveActiveUp : calcID -> XML.tree
          iteratorOK2xml
          iteratorERROR2xml
          sysERROR2xml
    val moveCalcHead : calcID -> pos' -> XML.tree
          iteratorOK2xml
          iteratorERROR2xml
          sysERROR2xml
    val moveDown : calcID -> pos' -> XML.tree
          iteratorOK2xml
          iteratorERROR2xml
          sysERROR2xml
    val moveLevelDown : calcID -> pos' -> XML.tree
          iteratorOK2xml
          iteratorERROR2xml
          sysERROR2xml
    val moveLevelUp : calcID -> pos' -> XML.tree
          iteratorOK2xml
          iteratorERROR2xml
          sysERROR2xml
    val moveRoot : calcID -> XML.tree
          iteratorOK2xml
          sysERROR2xml
    val moveUp : calcID -> pos' -> XML.tree
          iteratorOK2xml
          iteratorERROR2xml
          sysERROR2xml
    val refFormula : calcID -> pos' -> XML.tree
          refformulaOK2xml
          sysERROR2xml
    val refineProblem : calcID -> pos' -> guh -> XML.tree
          xml_of_matchpbl
          sysERROR2xml
    val replaceFormula : calcID -> cterm' -> XML.tree
          replaceformulaOK2xml
          replaceformulaERROR2xml
          sysERROR2xml
    val requestFillformula : calcID -> errpatID * fillpatID -> XML.tree
          autocalculateOK2xml
          autocalculateERROR2xml
    val resetCalcHead : calcID -> XML.tree
          modifycalcheadOK2xml
          sysERROR2xml
    val setContext : calcID -> pos' -> guh -> XML.tree
          message2xml
          autocalculateOK2xml
          sysERROR2xml
    val setMethod : calcID -> metID -> XML.tree
          modifycalcheadOK2xml
          sysERROR2xml
    val setNextTactic : calcID -> tac -> XML.tree
          setnexttactic2xml
          sysERROR2xml
    val setProblem : calcID -> pblID -> XML.tree
          modifycalcheadOK2xml
          sysERROR2xml
    val setTheory : calcID -> thyID -> XML.tree
          modifycalcheadOK2xml
          sysERROR2xml
  end\<close>
subsection \<open>Implementation\<close>
(* val appendFormula : calcID -> cterm' -> XML.tree ------------------------
         appendformulaOK2xml
         appendformulaERROR2xml
         sysERROR2xml *)
ML {*
*} ML {*
*} ML {*

*} ML {*
*} ML {*
xml_to_formula
*} ML {*
*} ML {*
val t = parse @{theory} "111" |> the |> term_of;
*} ML {*
*}
operation_setup append_form = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 (let 
	   val (calcid, cterm') = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         form]) => (ci |> int_of_str', form |> xml_to_formula |> term2str)
     | x => raise ERROR ("append_form: intree = " ^ xmlstr 0 x)
     val result = Math_Engine.appendFormula calcid cterm'
	 in result end)
	 handle ERROR msg => appendformulaERROR2xml 4711 msg)}\<close>

(* val autoCalculate : calcID -> auto -> XML.tree --------------------------
         autocalculateOK2xml
         autocalculateERROR2xml
         sysERROR2xml *)
operation_setup autocalculate = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 (let 
	   val (ci, a) = case intree of
       XML.Elem (("AUTOCALC", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]), a]) => (ci, a)
     | tree => raise ERROR ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
	 in result end)
	 handle ERROR msg => autocalculateERROR2xml 4711 msg)}\<close>

(* val applyTactic : calcID -> pos' -> tac -> XML.tree ---------------------
         autocalculateOK2xml
         autocalculateERROR2xml *)
operation_setup apply_tac = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, pos, tac) = case intree of
       XML.Elem (("AUTOCALC", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         pos, tac]) => (str2int ci, xml_to_pos pos, xml_to_tac tac)
       | tree => raise ERROR ("apply_tac: intree = " ^ xmlstr 0 tree)
     val result = Math_Engine.applyTactic ci pos tac
	 in result end)}\<close>

(* val CalcTree : fmz list -> XML.tree -------------------------------------
         calctreeOK2xml
         sysERROR2xml *)
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

(* val checkContext : calcID -> pos' -> guh -> XML.tree --------------------
         message2xml
         contextthyOK2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup check_ctxt = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val DEconstrCalcTree : calcID -> XML.tree -------------------------------
         deconstructcalctreeOK2xml *)
operation_setup deconstrcalctree = \<open>
  {from_lib = Codec.int,
   to_lib = Codec.tree,
   action = (fn calcid => 
	 let 
	   val result = Math_Engine.DEconstrCalcTree calcid
	 in result end)}\<close>

(* val fetchApplicableTactics : calcID -> int -> pos' -> XML.tree ----------
   applicabletacticsOK
   sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup fetch_applicable_tacs = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val fetchProposedTactic : calcID -> XML.tree ----------------------------
         fetchproposedtacticOK2xml
         fetchproposedtacticERROR2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup fetch_proposed_tac = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val findFillpatterns : calcID -> errpatID -> XML.tree -------------------
         findFillpatterns2xml *)
ML {*
*} ML {*
*}
operation_setup find_fill_patts = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val getAccumulatedAsms : calcID -> pos' -> XML.tree ---------------------
         getasmsOK2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup get_accumulated_asms = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val getActiveFormula : calcID -> XML.tree -------------------------------
         iteratorOK2xml *)
ML {*
*} ML {*
*}
operation_setup get_active_form = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val getAssumptions : calcID -> pos' -> XML.tree -------------------------
         getasmsOK2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup get_asms = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val getFormulaeFromTo : calcID -> pos' -> pos' -> int -> bool -> XML.tree
         getintervalOK
         sysERROR2xml *)
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

(* val getTactic : calcID -> pos' -> XML.tree ------------------------------
         gettacticOK2xml
         gettacticERROR2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup get_tac = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val initContext : calcID -> ketype -> pos' -> XML.tree ------------------
         message2xml
         contextthyOK2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup init_ctxt = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val inputFillFormula: calcID -> string -> XML.tree ----------------------
         autocalculateOK2xml
         autocalculateERROR2xml
         message2xml *)
ML {*
*} ML {*
*}
operation_setup input_fill_from = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val interSteps : calcID -> pos' -> XML.tree -----------------------------
         interStepsOK
         interStepsERROR
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup inter_steps = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val Iterator : calcID -> XML.tree ---------------------------------------
         adduserOK2xml
         sysERROR2xml *)
operation_setup iterator = \<open>
  {from_lib = Codec.int,
   to_lib = Codec.tree,
   action = (fn calcid => 
	 let
     val result = Math_Engine.Iterator calcid
	 in result end)}\<close>

(* val IteratorTEST : calcID -> iterID ------------------------------------- *)
(* val modelProblem : calcID -> XML.tree -----------------------------------
         modifycalcheadOK2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup model_pbl = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val modifyCalcHead : calcID -> icalhd -> XML.tree -----------------------
         modifycalcheadOK2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup modify_calchead = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val moveActiveCalcHead : calcID -> XML.tree -----------------------------
         iteratorOK2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup move_active_calchead = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val moveActiveDown : calcID -> XML.tree ---------------------------------
         iteratorOK2xml
         iteratorERROR2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup move_active_down = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val moveActiveFormula : calcID -> pos' -> XML.tree ----------------------
         iteratorOK2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup move_active_form = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val moveActiveLevelDown : calcID -> XML.tree ----------------------------
         iteratorOK2xml
         iteratorERROR2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup move_active_levdown = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val moveActiveLevelUp : calcID -> XML.tree ------------------------------
         iteratorOK2xml
         iteratorERROR2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup move_active_levup = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val moveActiveRoot : calcID -> XML.tree ---------------------------------
         iteratorOK2xml
         sysERROR2xml *)
operation_setup moveactiveroot = \<open>
  {from_lib = Codec.int,
   to_lib = Codec.tree,
   action = (fn calcid => 
	 let
	   val result = Math_Engine.moveActiveRoot calcid
	 in result end)}\<close>

(* val moveActiveRootTEST : calcID -> XML.tree ----------------------------- *)
(* val moveActiveUp : calcID -> XML.tree -----------------------------------
         iteratorOK2xml
         iteratorERROR2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup move_active_up = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val moveCalcHead : calcID -> pos' -> XML.tree ---------------------------
         iteratorOK2xml
         iteratorERROR2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup move_active_calchead = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val moveDown : calcID -> pos' -> XML.tree -------------------------------
         iteratorOK2xml
         iteratorERROR2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup move_down = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val moveLevelDown : calcID -> pos' -> XML.tree --------------------------
         iteratorOK2xml
         iteratorERROR2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup move_levdn = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val moveLevelUp : calcID -> pos' -> XML.tree ----------------------------
         iteratorOK2xml
         iteratorERROR2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup move_levup = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val moveRoot : calcID -> XML.tree ---------------------------------------
         iteratorOK2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup move_root = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val moveUp : calcID -> pos' -> XML.tree ---------------------------------
         iteratorOK2xml
         iteratorERROR2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup move_up = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val refFormula : calcID -> pos' -> XML.tree -----------------------------
         refformulaOK2xml
         sysERROR2xml *)
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

(* val refineProblem : calcID -> pos' -> guh -> XML.tree -------------------
         xml_of_matchpbl
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup refine_pbl = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val replaceFormula : calcID -> cterm' -> XML.tree -----------------------
         replaceformulaOK2xml
         replaceformulaERROR2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup replace_form = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val requestFillformula : calcID -> errpatID * fillpatID -> XML.tree -----
         autocalculateOK2xml
         autocalculateERROR2xml *)
ML {*
*} ML {*
*}
operation_setup request_fill_form = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val resetCalcHead : calcID -> XML.tree ----------------------------------
         modifycalcheadOK2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup reset_calchead = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val setContext : calcID -> pos' -> guh -> XML.tree ----------------------
         message2xml
         autocalculateOK2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup set_ctxt = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val setMethod : calcID -> metID -> XML.tree -----------------------------
         modifycalcheadOK2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup set_met = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val setNextTactic : calcID -> tac -> XML.tree ---------------------------
         setnexttactic2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup set_next_tac = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val setProblem : calcID -> pblID -> XML.tree ----------------------------
         modifycalcheadOK2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup set_pbl = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

(* val setTheory : calcID -> thyID -> XML.tree -----------------------------
         modifycalcheadOK2xml
         sysERROR2xml *)
ML {*
*} ML {*
*}
operation_setup set_thy = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, a) = case intree of
       XML.Elem (("APPENDFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text ci]),
         XML.Elem (("CALCID", []), [XML.Text _])
]) => (ci, a)
       | tree => error ("autocalculate: intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str ci
(*     val auto = xml_to_auto a
     val result = Math_Engine.autoCalculate calcid auto
*)
	 in XML.Text("TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO") end)}\<close>

section \<open>Native libisabelle: operation_setup use_thys\<close>

operation_setup use_thys = \<open>
  {from_lib = Codec.list Codec.string,
   to_lib = Codec.unit,
   action = Thy_Info.use_thys o map (rpair Position.none)}\<close>

end
