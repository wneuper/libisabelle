theory Protocol
imports 
  "/home/wneuper/proto4/libisabelle/libisabelle/src/main/isabelle/Protocol/Codec"
  (* isabelle build runs in /usr/local/isabisac/ and thus requires this ^^^ path *)
  "~~/src/Tools/isac/Frontend/Frontend"
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

section \<open>Implement mini-test from ~~/doc/test--isac-Java--isac-kernel.txt\<close>

subsection \<open>New code to \<longrightarrow> isabisac/test/...\<close>
(* \<longrightarrow> test/../xmlsrc/datatypes.sml *)
ML {*
val (is, kind) = ([], Pbl)
;
writeln (xmlstr 0 (xml_of_pos "POSITION" (is, kind)))
*}

subsection \<open>operation_setup for mini-test\<close>
(*setup follows ~~/doc/test--isac-Java--isac-kernel.txt
# #I = from_lib: CHECKED AND DECOMPOSED TO SML!
# #O = to_lib:   COPIED FROM isabisac/test/Pure/PIDE/xml.ML 
*)
subsubsection \<open>step 1\<close>
ML {*
(* ad --- step 1 -----------------------------------------------------
#I: Formalization
    isac-java/src/java/isac/util/Formalization.java
#O: int
ML {* CalcTree [(["equality (x+1=(2::real))", "solveFor x", "solutions L"],
        ("Test",["sqroot-test","univariate","equation","test"],["Test","squ-equ-test-subpbl1"]))]; 
* }*)
val items = ["equality (x+1=(2::real))", "solveFor x", "solutions L"]
val spec = ("Test", ["sqroot-test","univariate","equation","test"], ["Test","squ-equ-test-subpbl1"])
val intree = (* CREATE THIS IN Mini_Test.java *)
  XML.Elem (("FORMALIZATION", []), [
    xml_of_strs items,
    xml_of_spec spec])
*}
(*------- step 1 -----------------------------------------------------*)
operation_setup calctree = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (its, spc) = case intree of
	       XML.Elem (("FORMALIZATION", []), [its, spc]) => (its, spc)
       | tree => error ("calctree: intree =" ^ xmlstr 0 tree)
	   val items = xml_to_strs its
	   val spec = xml_to_spec spc
	   val calcid = 1 (* ------------------------------- work done in Isabelle/Isac *)
	   val result =   (* see doc/test--isac-java--isac-kernel.txt *)
	     XML.Elem (("CALCTREE", []),
  	       [XML.Elem (("CALCID", []), 
  	         [XML.Text (string_of_int calcid)])])
	 in result (* Math_Engine.CalcTree [(items, spec) : fmz] *) end)} \<close>

subsubsection \<open>step 2\<close>
(*
#I: int
#O: (int, int)
ML {* Iterator 1; * }*)
(*------- step 2 -----------------------------------------------------*)
operation_setup iterator = \<open>
  {from_lib = Codec.int,
   to_lib = Codec.tree,
   action = (fn calcid => 
	 let 
	   val (calcid, userid) = (1, 1) (* ------------------------------- work done in Isabelle/Isac *)
	   val result =   (* see doc/test--isac-java--isac-kernel.txt *)
	     XML.Elem (("ADDUSER", []),
         [XML.Elem (("CALCID", []), [XML.Text (string_of_int calcid)]),
         XML.Elem (("USERID", []), [XML.Text (string_of_int userid)])])
	 in result (* Math_Engine.Iterator (str2int calcid) *) end)}\<close>

subsubsection \<open>step 3\<close>
(*
#I: int
#O: (int, ICalcIterator)
    isac-java/src/java/isac/interfaces/ICalcIterator.java
ML {* moveActiveRoot 1; * }*)
(*------- step 3 -----------------------------------------------------*)
operation_setup moveactiveroot = \<open>
  {from_lib = Codec.int,
   to_lib = Codec.tree,
   action = (fn calcid => 
	 let 
	   val (is, kind) = ([], Pbl) (* ---------------------------------- work done in Isabelle/Isac *)
	   val result =   (* see doc/test--isac-java--isac-kernel.txt *)
	     XML.Elem (("CALCITERATOR", []), [
         XML.Elem (("CALCID", []), [XML.Text (string_of_int calcid)]),
         XML.Elem (("POSITION", []), [
           XML.Elem (("INTLIST", []), is),
           XML.Elem (("POS", []), [XML.Text (pos_2str kind)])])])
	 in result (* Math_Engine.moveActiveRoot (str2int calcid) *) end)}\<close>

subsubsection \<open>step 4\<close>
ML {*
(* ad --- step 4 -----------------------------------------------------
#I: (int, ICalcIterator, ICalcIterator, int, string) ...SIMPLIFIED TO:
    (int, pos',          pos',          int, string)                #O: (int, FormHeadsContainer)
    isac-java/src/java/isac/util/formulae/FormHeadsContainer.java, is a list of ...
    isac-java/src/java/isac/util/formulae/CalcFormula.java         and ...
    isac-java/src/java/isac/util/formulae/CalcHead.java
ML {* getFormulaeFromTo 1 ([],Pbl) ([],Pbl) 0 false; * }*)
val calcid = 1
val pos as (is, kind) = ([], Pbl)
val calcformula as (pos, formula) = (pos, "solve (x + 1 = 2, x)")
val formheads = [calcformula (*, calchead .. see below*)]
val intree = (* CREATE THIS IN Mini_Test.java *)
  XML.Elem (("GETFORMULAEFROMTO", []), [
    XML.Elem (("CALCID", []), 
      [XML.Text  (string_of_int calcid)]),
      xml_of_pos "POSITION" (is, kind),
      xml_of_pos "POSITION" (is, kind),
      xml_of_int 0,
      xml_of_bool false]);
*}
(*------- step 4 -----------------------------------------------------*)
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
         XML.Elem (("BOOL", []), [XML.Text rules])]) => (calcid, from, to, level, rules)
     | tree => error ("getformulaefromto: WRONG intree = " ^ xmlstr 0 tree)
     val SOME calcid = int_of_str calcid
     val from = xml_to_pos from
     val to = xml_to_pos to
     val SOME level = (*xml_to_int*) int_of_str level
     val rules = (*xml_to_bool*) string_to_bool rules
	   (* ------------------------------------------------------------ work done in Isabelle/Isac *)
	   val calcid = 1
	   val pos as (is, kind) = ([], Pbl)
	   val formula = "solve (x + 1 = 2, x)"
	   val result =   (* see doc/test--isac-java--isac-kernel.txt *)
	     XML.Elem (("GETELEMENTSFROMTO", []), [
         XML.Elem (("CALCID", []), [XML.Text (string_of_int calcid)]),
         XML.Elem (("FORMHEADS", []), [
           XML.Elem (("CALCFORMULA", []),  [
             XML.Elem (("POSITION", []), [
               XML.Elem (("INTLIST", []), is),
                 XML.Elem (("POS", []), [XML.Text (pos_2str kind)])]),
             XML.Elem (("FORMULA", []), [
               XML.Elem (("MATHML", []), [
                 XML.Elem (("ISA", []), [XML.Text formula])])])])])])
	 in result (* Math_Engine.getFormulaeFromTo calcid from to level rules *) end)}\<close>

subsubsection \<open>step 6\<close>
(*------- step 5 -----------------------------------------------------
ML {* refFormula 1 ([],Pbl); * } *)
ML {*
(* ad --- step 6 -----------------------------------------------------
#I: (int, ICalcIterator) ...SIMPLIFIED TO:
    (int, pos')
#O: (int, CalcHead)
    isac-java/src/java/isac/util/formulae/CalcHead.java
ML {* refFormula 1 ([],Pbl); * }
*)
val calcid = 1
val pos as (is, kind) = ([], Pbl)
val intree = (* CREATE THIS IN Mini_Test.java *)
  XML.Elem (("REFFORMULA", []), [
    XML.Elem (("CALCID", []), [XML.Text (string_of_int calcid)]),
    xml_of_pos "POSITION" (is, kind)])

(* for result cp from ~~/test/Pure/PIDE/xml.ML --- step 6 --- *)
val head = "solve (x + 1 = 2, x)"
val given = [] : ((string * string) * string) list
val item as (attr, form) = (("status", "false"), "precond_rootpbl v_v")
val where_ as items = [item]
val find = [] : ((string * string) * string) list
val relate = [] : ((string * string) * string) list
val model = (given, where_, find, relate)
val belongsto = "Pbl"
val specification as (theoryid, problemid, methodid) = (["e_domID"], ["e_pblID"], ["e_metID"])
val calchead = (("status", "incorrect"), (pos, head, model, belongsto, specification))
*}
(*------- step 6 + 10 ------------------------------------------------*)
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
	   (* ------------------------------------------------------------ work done in Isabelle/Isac *)
	   val result = case pos of
	     ([], Pbl) => (* see doc/test--isac-java--isac-kernel.txt --- step 6 --- *)
	     XML.Elem (("REFFORMULA", []), [
         XML.Elem (("CALCID", []), [XML.Text (string_of_int calcid)]),
         XML.Elem (("CALCHEAD", [("status", "\"incorrect\"")]), [
           XML.Elem (("POSITION", []), [
             XML.Elem (("INTLIST", []), is),
             XML.Elem (("POS", []), [XML.Text (pos_2str kind)])]),
           XML.Elem (("HEAD", []), [
             XML.Elem (("MATHML", []), [
               XML.Elem (("ISA", []), [XML.Text formula])])]),
           XML.Elem (("MODEL", []), [
             XML.Elem (("GIVEN", []), []),
             XML.Elem (("WHERE", []), [
               XML.Elem (("ITEM", [("status", "\"false\"")]), [
                 XML.Elem (("MATHML", []), [
                   XML.Elem (("ISA", []), [XML.Text "precond_rootpbl v_v"])])])]),
             XML.Elem (("FIND", []), []),
             XML.Elem (("RELATE", []), [])]),
           XML.Elem (("BELONGSTO", []), [XML.Text "Pbl"]),
           XML.Elem (("SPECIFICATION", []), [
             XML.Elem (("THEORYID", []), [XML.Text "e_domID"]),
             XML.Elem (("PROBLEMID", []), [
               XML.Elem (("STRINGLIST", []), [
                 XML.Elem (("STRING", []), [XML.Text "e_pblID"])])]),
             XML.Elem (("METHODID", []), [
               XML.Elem (("STRINGLIST", []), [
                 XML.Elem (("STRING", []), [XML.Text "e_metID"])])])])])])
	   | ([], Res) => (* see doc/test--isac-java--isac-kernel.txt --- step 10 --- *)
	       let 
	         val pos as (is, kind) = ([], Res)
           val calcformula as (pos, formula) = (pos, "[x = 1]")
           val formheads = [calcformula (*, calchead .. see below*)]
	       in 
           XML.Elem (("REFFORMULA", []), [
             XML.Elem (("CALCID", []), [XML.Text (string_of_int calcid)]),
             XML.Elem (("CALCFORMULA", []),  [
               XML.Elem (("POSITION", []), [
                 XML.Elem (("INTLIST", []), is),
                 XML.Elem (("POS", []), [XML.Text (pos_2str kind)])]),
               XML.Elem (("FORMULA", []), [
                 XML.Elem (("MATHML", []), [
                   XML.Elem (("ISA", []), [XML.Text formula])])])])])
	       end
	   | _ => error ("refformula called with " ^ pos'2str pos)
	 in result end)}\<close>

subsubsection \<open>step 7\<close>
ML {*
(* ad --- step 7 -----------------------------------------------------
#I: (int, string) ...CHANGED TO:
    (int, auto)
#O: (int, CalcChanged)
	isac-java/src/java/isac/util/CalcChanged.java
 ML {* autoCalculate 1 CompleteCalc; * }
*)
val calcid = 1
val auto = CompleteCalc
val intree = (* CREATE THIS IN Mini_Test.java *)
  XML.Elem (("AUTOCALC", []), [
    XML.Elem (("CALCID", []), [XML.Text (string_of_int calcid)]),
    xml_of_auto CompleteCalc])

(* for result cp from ~~/test/Pure/PIDE/xml.ML *)
val result =
  XML.Elem (("AUTOCALC", []), [
    XML.Elem (("CALCID", []), [XML.Text (string_of_int calcid)]),
    XML.Elem (("CALCCHANGED", []),  [
      xml_of_pos "UNCHANGED" ([], Pbl),
      xml_of_pos "DELETED" ([], Pbl),
      xml_of_pos "GENERATED" ([], Res)])]);
*}
(*------- step 7 -----------------------------------------------------*)
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
	   (* ------------------------------------------------------------ work done in Isabelle/Isac *)
     val result =   (* see doc/test--isac-java--isac-kernel.txt 1st example *)
	     XML.Elem (("AUTOCALC", []), [
    XML.Elem (("CALCID", []), [XML.Text (string_of_int calcid)]),
    XML.Elem (("CALCCHANGED", []),  [
      XML.Elem (("UNCHANGED", []), [
        XML.Elem (("INTLIST", []), is),
        XML.Elem (("POS", []), [XML.Text (pos_2str kind)])]),
      XML.Elem (("DELETED", []), [
        XML.Elem (("INTLIST", []), is),
        XML.Elem (("POS", []), [XML.Text (pos_2str kind)])]),
     XML.Elem (("GENERATED", []), [
        XML.Elem (("INTLIST", []), is),
        XML.Elem (("POS", []), [XML.Text "Res"])])])])
	 in result end)}\<close>

subsubsection \<open>step 10 covered by step 6\<close>
(*------- step 8 -----------------------------------------------------
ML {* getFormulaeFromTo 1 ([],Pbl) ([],Res) 0 false; * }
  ------- step 9 -----------------------------------------------------
ML {* getFormulaeFromTo 1 ([],Pbl) ([],Res) 0 false; * }
  ------- step 10 -----------------------------------------------------
#I: (int, ICalcIterator)
#O: (int, CalcFormula)
ML {* refFormula 1 ([],Res); * }
*)

subsubsection \<open>step 13\<close>
(*------- step 11 -----------------------------------------------------
ML {* refFormula 1 ([],Res); * }
  ------- step 12 -----------------------------------------------------
ML {* refFormula 1 ([],Res); * }
  ------- step 13 -----------------------------------------------------
#I: int
#O: int
ML {* DEconstrCalcTree 1; * }
*)
operation_setup deconstrcalctree = \<open>
  {from_lib = Codec.int,
   to_lib = Codec.tree,
   action = (fn calcid => 
	 let 
	   val _ = 1 (* ------------------------------- work done in Isabelle/Isac *)
	   val result =   (* see doc/test--isac-java--isac-kernel.txt *)
	     XML.Elem (("DELCALC", []), [
	       XML.Elem (("CALCID", []), [XML.Text (string_of_int calcid)])])
	 in result end)}\<close>


operation_setup use_thys = \<open>
  {from_lib = Codec.list Codec.string,
   to_lib = Codec.unit,
   action = Thy_Info.use_thys o map (rpair Position.none)}\<close>

end
