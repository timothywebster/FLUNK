(* Compile command to show more characters *)
Control.Print.printLength := 100;

(* Create two lists, splitting on the the equal sign *)
fun makeOrdList(s:string) = map ord (String.explode(s));
fun makeChrList ordList = map chr ordList;

fun firsthalf([]) = []
  | firsthalf(start::rest) = if (start<>ord #"=") then [start]@firsthalf(rest) else [];

fun lasthalf([]) = []
  | lasthalf(start::rest) = if (start<>ord #"=") then lasthalf(rest) else rest;

(*----------------------------------------------------------------------------*)
datatype token = PlusSign | EqualSign | UCase of char | 
                 LCase of char | LiteralNumber of int | Element of string |
                 Coef of int | SubScript of int
;

fun tokenize nil = nil
  | tokenize (#"+" :: restOfList) = (PlusSign :: tokenize restOfList)
  | tokenize (#"=" :: restOfList) = (EqualSign :: tokenize restOfList)
  | tokenize (x :: restOfList) = if (ord x <= ord #"9") 
                                 then (LiteralNumber (ord x - ord #"0"):: tokenize restOfList)
                                 else (if (ord x <= ord #"Z")
                                       then (UCase x :: tokenize restOfList) 
                                       else (LCase x :: tokenize restOfList)
                                      )
;
(*----------------------------------------------------------------------------*)

(* Assumes base 10 numbers *)
fun createNumbers nil = nil
  | createNumbers (LiteralNumber a :: LiteralNumber b :: restOfList) 
                                    = createNumbers (LiteralNumber (a*10+b)::restOfList) 
  | createNumbers (x :: restOfList) = x::createNumbers(restOfList)
;

fun getElements nil = nil
  | getElements (UCase a :: LCase b :: restOfList)    (* Two Letter Elements *)
                         = getElements (Element (String.implode([a,b]))::restOfList)
  | getElements (Element a :: LCase b :: restOfList)  (* Three Letter Elements *)
                         = getElements (Element (String.implode(String.explode(a)@([b])))::restOfList)
  | getElements (UCase a :: restOfList)               (* One Letter Elements *)
                         = getElements (Element (String.implode([a]))::restOfList)
  | getElements (x :: restOfList)                     (* non-element *)
                         = x::getElements(restOfList)
;

fun determinNumberType nil = nil
  | determinNumberType (LiteralNumber a :: Element b :: restOfList)
                        = determinNumberType ( Coef a :: Element b :: restOfList )
  | determinNumberType (Element a :: LiteralNumber b :: restOfList)
                        = determinNumberType ( Element a :: SubScript b :: restOfList )
  | determinNumberType (x :: restOfList)
                        = x::determinNumberType(restOfList)
;

fun processSubscripts nil = nil
  | processSubscripts (Element a :: SubScript b :: restOfList)
                        = if (b>1) 
                          then (processSubscripts(Element a :: Element a :: SubScript (b-1) :: restOfList))
                          else (processSubscripts(Element a :: restOfList))
  | processSubscripts (x :: restOfList)
                        = x::processSubscripts(restOfList)
;

fun addCoefs nil = nil
  | addCoefs (Coef a :: Element b :: Element c :: restOfList) 
                        = Coef a::Element b::addCoefs(Coef a::Element c:: restOfList)
  | addCoefs (x :: restOfList)
                        = x::addCoefs(restOfList)
;

fun processCoefs nil = nil
  | processCoefs(Coef a :: Element b :: restOfList)
                     = if (a>1)
                       then (processCoefs(Coef (a-1) :: Element b :: Element b :: restOfList))
                       else (processCoefs(Element b :: restOfList))
  | processCoefs (x :: restOfList)
                     = x::processCoefs(restOfList)
;

fun removePlus nil = nil
  | removePlus (PlusSign :: restOfList) = removePlus(restOfList)
  | removePlus (x :: restOfList) = x::removePlus(restOfList)
;

fun convertToStrings nil = nil
  | convertToStrings (Element a::restOfList) = a::convertToStrings(restOfList)
;

(* -- begin stolen code -- *)
fun insertSort R [] = []
  | insertSort R (h::t) =
        let fun insert R [] x = [x]
            | insert R (h::t) x = if R (x, h) then x::(h::t)
                                  else h::(insert R t x)
          val rest = insertSort R t
      in  insert R rest h
      end;

val i = insertSort;
fun lexlt (s, t) =
    let val Ls = explode (s);
        val Lt = explode (t);
        fun lstlexlt (_, []) = false
        |   lstlexlt ([], (b:char)::M) = true
        |   lstlexlt (a::L, b::M) = 
                   if (a < b) then true 
                   else if (a = b) then lstlexlt (L, M)
                        else false
        ;
     in lstlexlt (Ls, Lt)
     end
;

fun lexleq (s, t) = (s = t) orelse lexlt (s, t);

fun lexgt (s, t) = lexlt(t, s);

fun lexgeq (s, t) = (s = t) orelse lexgt (s, t);
(* -- end stolen code -- *)


fun runThis myEq =
i lexlt (
   convertToStrings(
      removePlus(
         processCoefs(
            addCoefs(
               processSubscripts(
                  determinNumberType(
                     getElements(
                        createNumbers(
                           tokenize(
                              makeChrList(firsthalf(makeOrdList myEq))
                           )
                        )
                     )
                  )
               )
            )
         )
      )
   )
)
=
i lexlt (
   convertToStrings(
      removePlus(
         processCoefs(
            addCoefs(
               processSubscripts(
                  determinNumberType(
                     getElements(
                        createNumbers(
                           tokenize(
                              makeChrList(lasthalf(makeOrdList myEq))
                           )
                        )
                     )
                  )
               )
            )
         )
      )
   )
)
;

val eq = "2Unn+3H4O+O2+3NaCl=H12O5+3NaCl+Unn+Unn"; 
runThis eq;
val eq = "H2O+O2=H2O2"; 
runThis eq;
val eq = "6H2O+6CO2=6O2+C6H12O6"; 
runThis eq;
val eq = "2Na+2H2O=2NaOH+H2"; 
runThis eq;
val eq = "C6H12O6=3C2H2+3O2"; 
runThis eq;
val eq = "20H2O+10O2=20H2O2"; 
runThis eq;


