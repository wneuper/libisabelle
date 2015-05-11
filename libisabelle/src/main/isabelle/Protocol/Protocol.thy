theory Protocol
imports Codec
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

(* see mini-test in ~~/doc/test--isac-Java--isac-kernel.txt
POLICY FOR THIS TEST PHASE:
# WE KEEP THE CODE FOR MINI-TEST BELOW INDEPENDENT FROM isac-kernel 
# #I = from_lib: DECOMPOSED AND CHECKED !
# #O = to_lib: COPIED FROM isabisac/test/Pure/PIDE/xml.ML 
*)
(*------- step 1 -----------------------------------------------------*)
(* TODO *)

(*------- step 2 -----------------------------------------------------*)
operation_setup iterator = \<open>
  {from_lib = Codec.int,
   to_lib = Codec.tree,
   action = (fn calcid => 
	 let 
	   val (calcid, userid) = (1, 1) (* ------------------------------- work done in Isabelle/Isac *)
	   val result =   (* see doc/test--isac-java--isac-kernel.txt 1st example *)
	     XML.Elem (("ADDUSER", []),
  	       [XML.Elem (("CALCID", []), 
  	         [XML.Text (string_of_int calcid)]),
  	       XML.Elem (("USERID", []), 
  	         [XML.Text (string_of_int userid)])
           ])
	 in result end)}\<close>

ML {*
(* ad --- step 3 -----------------------------------------------------*)
*} ML {*
(* DONT CHANGE THESE IDENTIFIERS from library.sml, Interpret/ctree.sml, ProgLang/termC.sml, etc*)
fun drop_last l = ((rev o tl o rev) l);
fun int_of_str str =
    let val ss = Symbol.explode str
	val str' = case ss of
	   "("::s => drop_last s | _ => ss
    in (SOME (Thy_Output.integer (implode str'))) handle _ => NONE end;
fun bool2str true = "true"
  | bool2str false = "false";
datatype pos_ = Pbl | Met | Frm | Res | Und   
fun pos_2str Pbl = "Pbl"
  | pos_2str Met = "Met"
  | pos_2str Frm = "Frm"
  | pos_2str Res = "Res"
  | pos_2str Und = "Und";
*} ML {*
(* THIS IS MISSING IN Interpret/ctree.sml *)
fun str2pos_ "Pbl" = Pbl
  | str2pos_ "Met" = Met
  | str2pos_ "Frm" = Frm
  | str2pos_ "Res" = Res
  | str2pos_ "Und" = Und
*} ML {*
val (is, kind) = ([], Pbl)
*} ML {*
(* ATTENTION AT INTEGRATION INTO isabisac: 
# use|change "fun indt" 
# rename funs, e.g. xml/datatypes.sml: fun ints2xml
# 
*)
fun indent i = fold (curry op ^) (replicate i "  ") "" (*cp from isabisac/test/Pure/PIDE/xml.ML*)
fun xmlstr i (XML.Text str) = indent i ^ str ^ "\n"
  | xmlstr i (XML.Elem ((str, []), trees)) = 
    indent i ^ "<" ^ str ^ ">" ^ "\n" ^
      List.foldr op ^ "" (map (xmlstr (i + 1)) trees) ^
    indent i ^ "</" ^ str ^ ">" ^ "\n"
  | xmlstr i (XML.Elem ((str, [("status", a)]), trees)) = 
    indent i ^ "<" ^ str ^ " status " ^ a  ^ ">" ^ "\n" ^
      List.foldr op ^ "" (map (xmlstr (i + 1)) trees) ^
    indent i ^ "</" ^ str ^ ">" ^ "\n"
  | xmlstr _ (XML.Elem ((_, (_ :: _)), _)) = 
    error "xmlstr: TODO review attribute \"status\" etc";
*} ML {*
fun xml_of_int i = XML.Elem (("INT", []), [XML.Text (string_of_int i)])
fun xml_of_ints is = (*xml/datatypes.sml: fun ints2xml*)
  XML.Elem (("INTLIST", []), map xml_of_int is)
fun xml_of_pos tag (is, pp) = (*xml/datatypes.sml: fun pos'2xml*)
  XML.Elem ((tag, []), [
    xml_of_ints is,
    XML.Elem (("POS", []), [XML.Text (pos_2str pp)])
    ])
;
writeln (xmlstr 0 (xml_of_pos "POSITION" (is, kind)))
*}
(*------- step 3 -----------------------------------------------------*)
operation_setup moveactiveroot = \<open>
  {from_lib = Codec.int,
   to_lib = Codec.tree,
   action = (fn calcid => 
	 let 
	   val (is, kind) = ([], Pbl) (* ---------------------------------- work done in Isabelle/Isac *)
	   val result =   (* see doc/test--isac-java--isac-kernel.txt 1st example *)
	     XML.Elem (("CALCITERATOR", []),
  	       [XML.Elem (("CALCID", []), 
  	         [XML.Text (string_of_int calcid)]),
  	         xml_of_pos "POSITION" (is, kind)])
	 in result end)}\<close>

ML {*
(* ad --- step 4 -----------------------------------------------------*)
(* from isabisac/test/Pure/PIDE/xml.ML
  #I: (int, ICalcIterator, ICalcIterator, int, string)
      (int, pos,           pos,           int, string)  ...SIMPLIFIED BELOW *)
val calcid = 1
val pos as (is, kind) = ([], Pbl)
val calcformula as (pos, formula) = (pos, "solve (x + 1 = 2, x)")
val formheads = [calcformula (*, calchead .. see below*)]
*} ML {*
val intree = (* CREATE THIS IN Mini_Test.java *)
  XML.Elem (("GETFORMULAEFROMTO", []), [
    XML.Elem (("CALCID", []), 
      [XML.Text (string_of_int calcid)]),
      xml_of_pos "POSITION" (is, kind),
      xml_of_pos "POSITION" (is, kind),
      XML.Text (string_of_int 0),
      XML.Text (bool2str false)])
*} ML {*
*} ML {*
fun xml_to_int (XML.Elem (("INT", []), [XML.Text i])) = 
      (case int_of_str i of SOME i => i | _ => error "xml_to_int: int_of_str \<Rightarrow> NONE")
  | xml_to_int tree = error ("xml_to_int: wrong XML.tree " ^ xmlstr 0 tree)
fun xml_to_ints (XML.Elem (("INTLIST", []), is)) = map xml_to_int is
  | xml_to_ints tree = error ("xml_to_ints: wrong XML.tree " ^ xmlstr 0 tree)
fun xml_to_pos_ (XML.Elem (("POS", []), [XML.Text pp])) = str2pos_ pp
  | xml_to_pos_ tree = error ("xml_to_pos_: wrong XML.tree " ^ xmlstr 0 tree)
*} ML {*

*} ML {*
fun xml_to_pos (XML.Elem (("POSITION", []), [is, pp])) = (xml_to_ints is, xml_to_pos_ pp) (*: pos'*)
*} ML {*
*}
(*------- step 4 -----------------------------------------------------*)
operation_setup getformulaefromto = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn intree => 
	 let 
	   val (ci, p1, p2, ii, bb) = case intree of
         XML.Elem (("GETFORMULAEFROMTO", []), [
           XML.Elem (("CALCID", []), [XML.Text ci]),
           p1, p2, XML.Text i, XML.Text b]) => (ci, p1, p2, i, b)
       | _ => error "getformulaefromto: intree wrong"
     val calcid = int_of_str ci
     val (pos1, pos2) = (xml_to_pos p1, xml_to_pos p2)
     val i = int_of_str ii
     (*val b = bool_of_str bb ...MISSING *)
	   (* ------------------------------------------------------------ work done in Isabelle/Isac *)
	   val (calcid, kind, formula) = (1, "Pbl", "solve (x + 1 = 2, x)")
	   val result =   (* see doc/test--isac-java--isac-kernel.txt 1st example *)
	     XML.Elem (("GETELEMENTSFROMTO", []), [
         XML.Elem (("CALCID", []), [XML.Text (string_of_int calcid)]),
         XML.Elem (("FORMHEADS", []), [
           XML.Elem (("CALCFORMULA", []),  [
             XML.Elem (("POSITION", []), [
               XML.Elem (("INTLIST", []), is),
                 XML.Elem (("POS", []), [XML.Text kind])]),
             XML.Elem (("FORMULA", []), [
               XML.Elem (("MATHML", []), [
                 XML.Elem (("ISA", []), [XML.Text formula])])])])])])
	 in result end)}\<close>

ML {*
(* ad --- step 6 -----------------------------------------------------*)
*} ML {*
*}
(*------- step 6 -----------------------------------------------------*)
operation_setup refformula = \<open> (* ATTENTION: 2nd call in step 10 WITH DIFFERENT result *)
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn tree => 
	 let 
	   val xxx = tree
	   val result =   (* see doc/test--isac-java--isac-kernel.txt 1st example *)
	     XML.Elem (("CALCITERATOR", []), [])
	 in result end)}\<close>

ML {*
(* ad --- step 7 -----------------------------------------------------*)
*} ML {*
*}
(*------- step 7 -----------------------------------------------------*)
operation_setup autocalculate = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn tree => 
	 let 
	   val xxx = tree
	   val result =   (* see doc/test--isac-java--isac-kernel.txt 1st example *)
	     XML.Elem (("CALCITERATOR", []), [])
	 in result end)}\<close>

ML {*
(* ad --- step 10 -----------------------------------------------------*)
*} ML {*
*}
(*------- step 10 -----------------------------------------------------*)
operation_setup refformula = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn tree => 
	 let 
	   val xxx = tree
	   val result =   (* see doc/test--isac-java--isac-kernel.txt 1st example *)
	     XML.Elem (("CALCITERATOR", []), [])
	 in result end)}\<close>

ML {*
(* ad --- step 13 -----------------------------------------------------*)
*} ML {*
*}
(*------- step 13 -----------------------------------------------------*)
operation_setup deconstrcalctree = \<open>
  {from_lib = Codec.tree,
   to_lib = Codec.tree,
   action = (fn tree => 
	 let 
	   val xxx = tree
	   val result =   (* see doc/test--isac-java--isac-kernel.txt 1st example *)
	     XML.Elem (("CALCITERATOR", []), [])
	 in result end)}\<close>


operation_setup use_thys = \<open>
  {from_lib = Codec.list Codec.string,
   to_lib = Codec.unit,
   action = Thy_Info.use_thys o map (rpair Position.none)}\<close>

end
