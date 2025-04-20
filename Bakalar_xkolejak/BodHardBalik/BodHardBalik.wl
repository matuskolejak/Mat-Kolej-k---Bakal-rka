(* Content-type: application/vnd.wolfram.mathematica *)

(*** Wolfram Notebook File ***)
(* http://www.wolfram.com/nb *)

(* CreatedBy='Wolfram 14.1' *)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[       154,          7]
NotebookDataLength[     92625,       2196]
NotebookOptionsPosition[     92273,       2182]
NotebookOutlinePosition[     92664,       2198]
CellTagsIndexPosition[     92621,       2195]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{
Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{"::", "Package", "::"}], "*)"}], 
  RowBox[{
   RowBox[{
    RowBox[{"BeginPackage", "[", "\"\<BodHardBalik`\>\"", "]"}], ";"}], "\n", 
   "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{"Export", " ", "verejn\[YAcute]ch", " ", "symbolov"}], "*)"}], 
   "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"BodTrojitaTransformacia", "::", "usage"}], "=", 
     "\"\<BodTrojitaTransformacia[] umo\[ZHacek]n\[IAcute] postupn\[YAcute] v\
\[YAcute]ber a aplik\[AAcute]ciu troch transform\[AAcute]ci\[IAcute] na \
bod.\>\""}], ";"}], "\[IndentingNewLine]", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodTrojitaTransformaciaSVysledkom", "::", "usage"}], "=", 
     "\"\<BodTrojitaTransformaciaSVysledkom[] umo\[ZHacek]n\[IAcute] postupn\
\[YAcute] v\[YAcute]ber a aplik\[AAcute]ciu troch transform\[AAcute]ci\
\[IAcute] na bod a zobraz\[IAcute] s\[UAcute]hrnn\[YAcute] \
v\[YAcute]sledok.\>\""}], ";"}], "\[IndentingNewLine]", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodGeneruj", "::", "usage"}], "=", 
     "\"\<BodGeneruj[] generuje n\[AAcute]hodn\[YAcute] bod s \
vhodn\[YAcute]mi vlastnos\[THacek]ami.\>\""}], ";"}], "\n", 
   "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Spolo\[CHacek]n\[EAcute]", " ", "funkcie", " ", "pre", " ", 
     "v\[SHacek]etky", " ", "transform\[AAcute]cie"}], "*)"}], 
   "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"validPointQ", "::", "usage"}], "=", 
     "\"\<validPointQ[p] over\[IAcute], \[CHacek]i bod sp\:013a\[NHacek]a \
krit\[EAcute]ri\[AAcute].\>\""}], ";"}], "\[IndentingNewLine]", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"pointDistance", "::", "usage"}], "=", 
     "\"\<pointDistance[p1, p2] vypo\[CHacek]\[IAcute]ta vzdialenos\[THacek] \
medzi dvoma bodmi.\>\""}], ";"}], "\n", "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Defin\[IAcute]cia", " ", "chybov\[YAcute]ch", " ", "spr\[AAcute]v"}], 
    "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"BodTrojitaTransformacia", "::", "infinityerr"}], "=", 
     "\"\<Vyskytla sa chyba: v\[YAcute]po\[CHacek]et viedol k \
nekone\[CHacek]n\[EAcute]mu alebo neur\[CHacek]it\[EAcute]mu \
v\[YAcute]sledku. Pou\[ZHacek]ije sa preddefinovan\[YAcute] bod.\>\""}], 
    ";"}], "\[IndentingNewLine]", "\n", 
   RowBox[{
    RowBox[{"Begin", "[", "\"\<`Private`\>\"", "]"}], ";"}], "\n", 
   "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Potla\[CHacek]enie", " ", "varovn\[YAcute]ch", " ", "spr\[AAcute]v", " ",
      "o", " ", "nekone\[CHacek]n\[YAcute]ch", " ", "v\[YAcute]sledkoch"}], 
    "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{"Off", "[", 
     RowBox[{"Power", "::", "infy"}], "]"}], ";"}], "\n", 
   RowBox[{
    RowBox[{"Off", "[", 
     RowBox[{"Infinity", "::", "indet"}], "]"}], ";"}], "\n", 
   "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Na\[CHacek]\[IAcute]tanie", " ", "v\[SHacek]etk\[YAcute]ch", " ", 
     "transforma\[CHacek]n\[YAcute]ch", " ", "modulov", " ", "na", " ", 
     "za\[CHacek]iatku"}], "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{"Needs", "[", "\"\<BodHardBalik`Transforms`Posun`\>\"", "]"}], 
    ";"}], "\n", 
   RowBox[{
    RowBox[{"Needs", "[", "\"\<BodHardBalik`Transforms`Rotacia`\>\"", "]"}], 
    ";"}], "\n", 
   RowBox[{
    RowBox[{
    "Needs", "[", "\"\<BodHardBalik`Transforms`ZvacsenieZmensenie`\>\"", 
     "]"}], ";"}], "\n", 
   RowBox[{
    RowBox[{"Needs", "[", "\"\<BodHardBalik`Transforms`Skosenie`\>\"", "]"}], 
    ";"}], "\n", 
   RowBox[{
    RowBox[{"Needs", "[", "\"\<BodHardBalik`Transforms`Symetria`\>\"", "]"}], 
    ";"}], "\n", "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Defin\[IAcute]cia", " ", "miernej\[SHacek]ej", " ", "farby", " ", 
     "zelenej"}], "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{"mildGreen", "=", 
     RowBox[{"RGBColor", "[", 
      RowBox[{"0.2", ",", "0.6", ",", "0.2"}], "]"}]}], ";"}], "\n", 
   "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Funkcia", " ", "na", " ", "v\[YAcute]po\[CHacek]et", " ", "vzdialenosti",
      " ", "medzi", " ", "dvoma", " ", "bodmi", " ", "s", " ", "kontrolou", 
     " ", "ch\[YAcute]b"}], "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"pointDistance", "[", 
      RowBox[{"p1_", ",", "p2_"}], "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "dist", "}"}], ",", 
       RowBox[{
        RowBox[{"dist", "=", 
         RowBox[{"Check", "[", 
          RowBox[{
           RowBox[{"EuclideanDistance", "[", 
            RowBox[{"p1", ",", "p2"}], "]"}], ",", "$Failed"}], "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"dist", "===", "$Failed"}], "||", 
           RowBox[{"dist", "===", "ComplexInfinity"}], "||", 
           RowBox[{"dist", "===", "Indeterminate"}], "||", 
           RowBox[{"!", 
            RowBox[{"NumberQ", "[", "dist", "]"}]}]}], ",", 
          RowBox[{"(*", 
           RowBox[{
           "Vr\[AAcute]ti\[THacek]", " ", "predvolen\[UAcute]", " ", 
            "hodnotu", " ", "v", " ", "pr\[IAcute]pade", " ", "chyby"}], 
           "*)"}], "10", ",", 
          RowBox[{"(*", 
           RowBox[{
           "Inak", " ", "vr\[AAcute]ti\[THacek]", " ", 
            "skuto\[CHacek]n\[UAcute]", " ", "vzdialenos\[THacek]"}], "*)"}], 
          "dist"}], "]"}]}]}], "]"}]}], ";"}], "\n", "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Funkcia", " ", "na", " ", "valid\[AAcute]ciu", " ", "bodu", " ", "s", 
     " ", "o\[SHacek]etren\[IAcute]m", " ", "ch\[YAcute]b"}], "*)"}], 
   "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"validPointQ", "[", "p_", "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "isValid", "}"}], ",", 
       RowBox[{"(*", 
        RowBox[{
        "Kontrola", " ", "\[CHacek]i", " ", "je", " ", "vstupn\[YAcute]", " ",
          "bod", " ", "validn\[YAcute]"}], "*)"}], 
       RowBox[{
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"!", 
            RowBox[{"VectorQ", "[", 
             RowBox[{"p", ",", "NumberQ"}], "]"}]}], "||", 
           RowBox[{
            RowBox[{"Length", "[", "p", "]"}], "!=", "2"}]}], ",", 
          RowBox[{"Return", "[", "False", "]"}]}], "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Kontrola", " ", "\[CHacek]i", " ", "s\[UAcute]", " ", 
          "s\[UAcute]radnice", " ", "v", " ", "bezpe\[CHacek]nom", " ", 
          "rozsahu"}], "*)"}], 
        RowBox[{"isValid", "=", 
         RowBox[{"Check", "[", 
          RowBox[{
           RowBox[{
            RowBox[{"Max", "[", 
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"p", "[", 
                RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", 
              RowBox[{"Abs", "[", 
               RowBox[{"p", "[", 
                RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "]"}], "<=", "10"}], 
           ",", "False"}], "]"}]}], ";", "\[IndentingNewLine]", "isValid"}]}],
       "]"}]}], ";"}], "\n", "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Funkcia", " ", "na", " ", "generovanie", " ", 
     "n\[AAcute]hodn\[EAcute]ho", " ", "bodu", " ", "s", " ", 
     "vhodn\[YAcute]mi", " ", "vlastnos\[THacek]ami"}], "*)"}], 
   "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"GenerateInitialPoint", "[", "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"point", ",", 
         RowBox[{"count", "=", "0"}], ",", 
         RowBox[{"defaultPoint", "=", 
          RowBox[{"{", 
           RowBox[{"2", ",", "3"}], "}"}]}], ",", "result", ",", "randomSeed",
          ",", "generationType"}], "}"}], ",", 
       RowBox[{"(*", 
        RowBox[{
        "Pou\[ZHacek]i\[THacek]", " ", "kombin\[AAcute]ciu", " ", 
         "r\[OHat]znych", " ", "zdrojov", " ", "n\[AAcute]hodnosti"}], "*)"}], 
       RowBox[{
        RowBox[{"randomSeed", "=", 
         RowBox[{"Hash", "[", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{"AbsoluteTime", "[", "]"}], ",", "$TimeZone", ",", 
            "$ProcessID", ",", 
            RowBox[{"RandomInteger", "[", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"-", 
                RowBox[{"10", "^", "7"}]}], ",", 
               RowBox[{"10", "^", "7"}]}], "}"}], "]"}], ",", 
            RowBox[{"StringJoin", "@@", 
             RowBox[{"ToString", "/@", 
              RowBox[{"RandomInteger", "[", 
               RowBox[{
                RowBox[{"{", 
                 RowBox[{"0", ",", "9"}], "}"}], ",", "10"}], "]"}]}]}]}], 
           "}"}], "]"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"SeedRandom", "[", "randomSeed", "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Pretrasenie", " ", "gener\[AAcute]tora", " ", 
          "n\[AAcute]hodn\[YAcute]ch", " ", "\[CHacek]\[IAcute]sel"}], "*)"}], 
        RowBox[{"Do", "[", 
         RowBox[{
          RowBox[{"RandomInteger", "[", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"-", "1000"}], ",", "1000"}], "}"}], "]"}], ",", 
          RowBox[{"{", 
           RowBox[{"RandomInteger", "[", 
            RowBox[{"{", 
             RowBox[{"10", ",", "30"}], "}"}], "]"}], "}"}]}], "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{"Rozhodnutie", ",", 
          RowBox[{
           RowBox[{
           "ak\[YAcute]", " ", "typ", " ", "hodn\[OHat]t", " ", 
            "pou\[ZHacek]i\[THacek]"}], "-", 
           RowBox[{
           "cel\[EAcute]", " ", "\[CHacek]\[IAcute]sla", " ", "alebo", " ", 
            "jednoduch\[EAcute]", " ", "zlomky"}]}]}], "*)"}], 
        RowBox[{"generationType", "=", 
         RowBox[{"RandomChoice", "[", 
          RowBox[{
           RowBox[{"{", 
            RowBox[{"0.8", ",", "0.2"}], "}"}], "->", 
           RowBox[{"{", 
            RowBox[{"1", ",", "2"}], "}"}]}], "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
          RowBox[{"Pou\[ZHacek]itie", " ", "Try"}], "-", 
          RowBox[{"Catch", " ", "\[SHacek]trukt\[UAcute]ry"}]}], "*)"}], 
        RowBox[{"result", "=", 
         RowBox[{"Check", "[", 
          RowBox[{
           RowBox[{"Block", "[", 
            RowBox[{
             RowBox[{"{", 
              RowBox[{"$MessagePreprint", "=", 
               RowBox[{
                RowBox[{"(", 
                 RowBox[{
                  RowBox[{"Message", "[", 
                   RowBox[{"BodTrojitaTransformacia", "::", "infinityerr"}], 
                   "]"}], ";", "#"}], ")"}], "&"}]}], "}"}], ",", 
             RowBox[{"While", "[", 
              RowBox[{"True", ",", 
               RowBox[{
                RowBox[{"Switch", "[", 
                 RowBox[{"generationType", ",", 
                  RowBox[{"(*", 
                   RowBox[{
                    RowBox[{"Cel\[EAcute]", " ", "\[CHacek]\[IAcute]sla"}], 
                    "-", "PREFEROVAN\[CapitalEAcute]"}], "*)"}], "1", ",", 
                  RowBox[{"Module", "[", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{"range", ",", "nonZeroRandom"}], "}"}], ",", 
                    RowBox[{"(*", 
                    RowBox[{
                    "Funkcia", " ", "pre", " ", "generovanie", " ", 
                    "nenulov\[YAcute]ch", " ", "hodn\[OHat]t"}], "*)"}], 
                    RowBox[{
                    RowBox[{
                    RowBox[{"nonZeroRandom", "[", 
                    RowBox[{"min_", ",", "max_"}], "]"}], ":=", 
                    RowBox[{"Module", "[", 
                    RowBox[{
                    RowBox[{"{", "val", "}"}], ",", 
                    RowBox[{
                    RowBox[{"val", "=", 
                    RowBox[{"RandomInteger", "[", 
                    RowBox[{"{", 
                    RowBox[{"min", ",", "max"}], "}"}], "]"}]}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{"val", "==", "0"}], ",", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"RandomReal", "[", "]"}], "<", "0.5"}], ",", 
                    RowBox[{"-", "1"}], ",", "1"}], "]"}], ",", "val"}], 
                    "]"}]}]}], "]"}]}], ";", "\[IndentingNewLine]", 
                    RowBox[{"(*", 
                    RowBox[{
                    "Men\[SHacek]\[IAcute]", " ", "rozsah", " ", "pre", " ", 
                    "jednoduch\[SHacek]ie", " ", "v\[YAcute]po\[CHacek]ty"}], 
                    "*)"}], 
                    RowBox[{"range", "=", 
                    RowBox[{"RandomInteger", "[", 
                    RowBox[{"{", 
                    RowBox[{"5", ",", "7"}], "}"}], "]"}]}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{"(*", 
                    RowBox[{
                    "Generovanie", " ", "s", " ", "vyh\[YAcute]ban\[IAcute]m",
                     " ", "sa", " ", "nul\[AAcute]m"}], "*)"}], 
                    RowBox[{"point", "=", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"nonZeroRandom", "[", 
                    RowBox[{
                    RowBox[{"-", "range"}], ",", "range"}], "]"}], ",", 
                    RowBox[{"nonZeroRandom", "[", 
                    RowBox[{
                    RowBox[{"-", "range"}], ",", "range"}], "]"}]}], "}"}]}], 
                    ";", "\[IndentingNewLine]", 
                    RowBox[{"(*", 
                    RowBox[{"Zabezpe\[CHacek]\[IAcute]me", ",", 
                    RowBox[{
                    "\[ZHacek]e", " ", "aspo\[NHacek]", " ", "jedna", " ", 
                    "s\[UAcute]radnica", " ", "nie", " ", "je", " ", 
                    "nulov\[AAcute]"}]}], "*)"}], 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{
                    RowBox[{"point", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], "==", "0"}], "&&", 
                    RowBox[{
                    RowBox[{"point", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], "==", "0"}]}], ",", 
                    RowBox[{"point", "=", 
                    RowBox[{"{", 
                    RowBox[{"1", ",", 
                    RowBox[{"nonZeroRandom", "[", 
                    RowBox[{
                    RowBox[{"-", "range"}], ",", "range"}], "]"}]}], 
                    "}"}]}]}], "]"}], ";"}]}], "]"}], ",", 
                  RowBox[{"(*", 
                   RowBox[{
                    RowBox[{
                    RowBox[{"Jednoduch\[EAcute]", " ", "zlomky"}], "-", 
                    RowBox[{"len", " ", "menovatele", " ", "2"}]}], ",", "3", 
                    ",", "4"}], "*)"}], "2", ",", 
                  RowBox[{"Module", "[", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{
                    "numX", ",", "numY", ",", "denX", ",", "denY", ",", 
                    "nonZeroNum"}], "}"}], ",", 
                    RowBox[{"(*", 
                    RowBox[{
                    "Funkcia", " ", "pre", " ", "generovanie", " ", 
                    "nenulov\[EAcute]ho", " ", "\[CHacek]itate\:013ea"}], 
                    "*)"}], 
                    RowBox[{
                    RowBox[{
                    RowBox[{"nonZeroNum", "[", 
                    RowBox[{"min_", ",", "max_"}], "]"}], ":=", 
                    RowBox[{"Module", "[", 
                    RowBox[{
                    RowBox[{"{", "val", "}"}], ",", 
                    RowBox[{
                    RowBox[{"val", "=", 
                    RowBox[{"RandomInteger", "[", 
                    RowBox[{"{", 
                    RowBox[{"min", ",", "max"}], "}"}], "]"}]}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{"val", "==", "0"}], ",", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"RandomReal", "[", "]"}], "<", "0.5"}], ",", "1", 
                    ",", 
                    RowBox[{"-", "1"}]}], "]"}], ",", "val"}], "]"}]}]}], 
                    "]"}]}], ";", "\[IndentingNewLine]", 
                    RowBox[{"(*", 
                    RowBox[{
                    RowBox[{
                    "Jednoduch\[EAcute]", " ", "\[CHacek]itatele", " ", "a", 
                    " ", "menovatele", " ", "pre", " ", "\:013eahk\[EAcute]", 
                    " ", "po\[CHacek]\[IAcute]tanie"}], "-", 
                    RowBox[{
                    "vyh\[YAcute]bame", " ", "sa", " ", "nul\[AAcute]m"}]}], 
                    "*)"}], 
                    RowBox[{"numX", "=", 
                    RowBox[{"nonZeroNum", "[", 
                    RowBox[{
                    RowBox[{"-", "8"}], ",", "8"}], "]"}]}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{"numY", "=", 
                    RowBox[{"nonZeroNum", "[", 
                    RowBox[{
                    RowBox[{"-", "8"}], ",", "8"}], "]"}]}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{"(*", 
                    RowBox[{
                    RowBox[{
                    "Len", " ", "jednoduch\[EAcute]", " ", "menovatele"}], 
                    "-", 
                    RowBox[{
                    "aby", " ", "\[SHacek]tudenti", " ", "mohli", " ", 
                    "\:013eahko", " ", "po\[CHacek]\[IAcute]ta\[THacek]"}]}], 
                    "*)"}], 
                    RowBox[{"denX", "=", 
                    RowBox[{"RandomChoice", "[", 
                    RowBox[{"{", 
                    RowBox[{"2", ",", "3", ",", "4"}], "}"}], "]"}]}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{"denY", "=", 
                    RowBox[{"RandomChoice", "[", 
                    RowBox[{"{", 
                    RowBox[{"2", ",", "3", ",", "4"}], "}"}], "]"}]}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{"(*", 
                    RowBox[{
                    "Zjednodu\[SHacek]enie", " ", "zlomkov", " ", "pre", " ", 
                    "lep\[SHacek]iu", " ", 
                    "\[CHacek]itate\:013enos\[THacek]"}], "*)"}], 
                    RowBox[{"point", "=", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"Rationalize", "[", 
                    RowBox[{"numX", "/", "denX"}], "]"}], ",", 
                    RowBox[{"Rationalize", "[", 
                    RowBox[{"numY", "/", "denY"}], "]"}]}], "}"}]}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{"(*", 
                    RowBox[{"Zabezpe\[CHacek]\[IAcute]me", ",", 
                    RowBox[{
                    "\[ZHacek]e", " ", "aspo\[NHacek]", " ", "jedna", " ", 
                    "s\[UAcute]radnica", " ", "nie", " ", "je", " ", 
                    "nulov\[AAcute]"}]}], "*)"}], 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"point", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], "==", "0"}], ",", 
                    RowBox[{
                    RowBox[{"point", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], "=", 
                    RowBox[{"Rationalize", "[", 
                    RowBox[{"1", "/", "denX"}], "]"}]}]}], "]"}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"point", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], "==", "0"}], ",", 
                    RowBox[{
                    RowBox[{"point", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], "=", 
                    RowBox[{"Rationalize", "[", 
                    RowBox[{"1", "/", "denY"}], "]"}]}]}], "]"}], ";"}]}], 
                   "]"}]}], "]"}], ";", "\[IndentingNewLine]", 
                RowBox[{"(*", 
                 RowBox[{
                 "Kontrola", " ", "validity", " ", "a", " ", "\[CHacek]i", 
                  " ", "nevyjde", " ", "z", " ", "okna"}], "*)"}], 
                RowBox[{"If", "[", 
                 RowBox[{
                  RowBox[{
                   RowBox[{"validPointQ", "[", "point", "]"}], "&&", 
                   RowBox[{
                    RowBox[{"Max", "[", 
                    RowBox[{
                    RowBox[{"Abs", "[", 
                    RowBox[{"point", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", 
                    RowBox[{"Abs", "[", 
                    RowBox[{"point", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "]"}], "<=", 
                    "10"}]}], ",", 
                  RowBox[{"(*", 
                   RowBox[{"V\[YAcute]sledn\[YAcute]", " ", "bod"}], "*)"}], 
                  RowBox[{"Return", "[", "point", "]"}]}], "]"}], ";", 
                "\[IndentingNewLine]", 
                RowBox[{"(*", 
                 RowBox[{
                 "Zmena", " ", "typu", " ", "ka\[ZHacek]d\[YAcute]ch", " ", 
                  "nieko\:013eko", " ", "pokusov"}], "*)"}], 
                RowBox[{"If", "[", 
                 RowBox[{
                  RowBox[{
                   RowBox[{
                    RowBox[{"Mod", "[", 
                    RowBox[{"count", ",", "5"}], "]"}], "==", "0"}], "&&", 
                   RowBox[{"count", ">", "0"}]}], ",", 
                  RowBox[{"generationType", "=", 
                   RowBox[{"RandomChoice", "[", 
                    RowBox[{
                    RowBox[{"{", 
                    RowBox[{"0.8", ",", "0.2"}], "}"}], "->", 
                    RowBox[{"{", 
                    RowBox[{"1", ",", "2"}], "}"}]}], "]"}]}]}], "]"}], ";", 
                "\[IndentingNewLine]", 
                RowBox[{"(*", 
                 RowBox[{
                  RowBox[{"Z\[AAcute]chrann\[YAcute]", " ", "mechanizmus"}], 
                  "-", 
                  RowBox[{"jednoduch\[EAcute]", " ", "body"}]}], "*)"}], 
                RowBox[{"If", "[", 
                 RowBox[{
                  RowBox[{"count", ">", "50"}], ",", 
                  RowBox[{"Module", "[", 
                   RowBox[{
                    RowBox[{"{", "simplePoints", "}"}], ",", 
                    RowBox[{
                    RowBox[{"simplePoints", "=", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"{", 
                    RowBox[{"3", ",", "4"}], "}"}], ",", 
                    RowBox[{"(*", 
                    RowBox[{
                    "Jednoduch\[YAcute]", " ", "bod", " ", "bez", " ", 
                    "n\[UAcute]l"}], "*)"}], 
                    RowBox[{"{", 
                    RowBox[{"5", ",", "2"}], "}"}], ",", 
                    RowBox[{"(*", 
                    RowBox[{
                    "Jednoduch\[YAcute]", " ", "bod", " ", "bez", " ", 
                    "n\[UAcute]l"}], "*)"}], 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"-", "2"}], ",", "3"}], "}"}], ",", 
                    RowBox[{"(*", 
                    RowBox[{
                    "Jednoduch\[YAcute]", " ", "bod", " ", "bez", " ", 
                    "n\[UAcute]l"}], "*)"}], 
                    RowBox[{"{", 
                    RowBox[{"4", ",", 
                    RowBox[{"-", "3"}]}], "}"}], ",", 
                    RowBox[{"(*", 
                    RowBox[{
                    "Jednoduch\[YAcute]", " ", "bod", " ", "bez", " ", 
                    "n\[UAcute]l"}], "*)"}], 
                    RowBox[{"{", 
                    RowBox[{"1", ",", "5"}], "}"}], ",", 
                    RowBox[{"(*", 
                    RowBox[{
                    "Jednoduch\[YAcute]", " ", "bod", " ", "bez", " ", 
                    "n\[UAcute]l"}], "*)"}], 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"-", "3"}], ",", 
                    RowBox[{"-", "4"}]}], "}"}], ",", 
                    RowBox[{"(*", 
                    RowBox[{
                    "Jednoduch\[YAcute]", " ", "bod", " ", "bez", " ", 
                    "n\[UAcute]l"}], "*)"}], 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"Rationalize", "[", 
                    RowBox[{"1", "/", "2"}], "]"}], ",", "3"}], "}"}], ",", 
                    RowBox[{"(*", 
                    RowBox[{"Bod", " ", "so", " ", "zlomkom"}], "*)"}], 
                    RowBox[{"{", 
                    RowBox[{"2", ",", 
                    RowBox[{"Rationalize", "[", 
                    RowBox[{"1", "/", "3"}], "]"}]}], "}"}], ",", 
                    RowBox[{"(*", 
                    RowBox[{"Bod", " ", "so", " ", "zlomkom"}], "*)"}], 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"-", "2"}], ",", 
                    RowBox[{"Rationalize", "[", 
                    RowBox[{"2", "/", "3"}], "]"}]}], "}"}], ",", 
                    RowBox[{"(*", 
                    RowBox[{"Bod", " ", "so", " ", "zlomkom"}], "*)"}], 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"Rationalize", "[", 
                    RowBox[{"3", "/", "4"}], "]"}], ",", 
                    RowBox[{"-", "2"}]}], "}"}]}], "  ", 
                    RowBox[{"(*", 
                    RowBox[{"Bod", " ", "so", " ", "zlomkom"}], "*)"}], 
                    "}"}]}], ";", "\[IndentingNewLine]", 
                    RowBox[{"(*", 
                    RowBox[{
                    "Vyberieme", " ", "n\[AAcute]hodn\[YAcute]", " ", "bod", 
                    " ", "z", " ", "pripraven\[YAcute]ch"}], "*)"}], 
                    RowBox[{"Return", "[", 
                    RowBox[{"RandomChoice", "[", "simplePoints", "]"}], 
                    "]"}]}]}], "]"}]}], "]"}], ";", "\[IndentingNewLine]", 
                RowBox[{"count", "++"}]}]}], "]"}]}], "]"}], ",", 
           RowBox[{"(*", 
            RowBox[{
            "Predvolen\[YAcute]", " ", "bod", " ", "v", " ", 
             "pr\[IAcute]pade", " ", "chyby"}], "*)"}], 
           RowBox[{"{", 
            RowBox[{"2", ",", "3"}], "}"}]}], "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Vr\[AAcute]ti\[THacek]", " ", "v\[YAcute]sledok", " ", "alebo", " ",
           "predvolen\[YAcute]", " ", "bod"}], "*)"}], 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"VectorQ", "[", 
            RowBox[{"result", ",", "NumberQ"}], "]"}], "&&", 
           RowBox[{
            RowBox[{"Length", "[", "result", "]"}], "==", "2"}]}], ",", 
          "result", ",", 
          RowBox[{"{", 
           RowBox[{"2", ",", "3"}], "}"}]}], "  ", 
         RowBox[{"(*", 
          RowBox[{
          "Jednoduch\[YAcute]", " ", "bod", " ", "bez", " ", "n\[UAcute]l"}], 
          "*)"}], "]"}]}]}], "]"}]}], ";"}], "\n", "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Verejn\[AAcute]", " ", "funkcia", " ", "na", " ", "generovanie", " ", 
     "bodu"}], "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"BodGeneruj", "[", "]"}], ":=", 
     RowBox[{"GenerateInitialPoint", "[", "]"}]}], ";"}], "\n", 
   "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Funkcia", " ", "na", " ", "form\[AAcute]tovan\[EAcute]", " ", 
     "zobrazenie", " ", "vlastnost\[IAcute]", " ", "bodu", " ", "s", " ", 
     "o\[SHacek]etren\[IAcute]m", " ", "ch\[YAcute]b"}], "*)"}], 
   "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"DisplayPointProperties", "[", 
      RowBox[{"point_", ",", "style_"}], "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "dist", "}"}], ",", 
       RowBox[{"(*", 
        RowBox[{
        "V\[YAcute]po\[CHacek]et", " ", "s", " ", "o\[SHacek]etren\[IAcute]m",
          " ", "pre", " ", "pr\[IAcute]pad", " ", "chyby"}], "*)"}], 
       RowBox[{
        RowBox[{"dist", "=", 
         RowBox[{"Check", "[", 
          RowBox[{
           RowBox[{"Round", "[", 
            RowBox[{"pointDistance", "[", 
             RowBox[{"point", ",", 
              RowBox[{"{", 
               RowBox[{"0", ",", "0"}], "}"}]}], "]"}], "]"}], ",", "5"}], 
          "]"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "O\[SHacek]etrenie", " ", "pre", " ", "pr\[IAcute]pad", " ", 
          "neplatn\[YAcute]ch", " ", "hodn\[OHat]t"}], "*)"}], 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"!", 
            RowBox[{"NumberQ", "[", "dist", "]"}]}], "||", 
           RowBox[{"dist", "===", "ComplexInfinity"}], "||", 
           RowBox[{"dist", "===", "Indeterminate"}]}], ",", 
          RowBox[{"dist", "=", "5"}]}], "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{"\"\<Vlastnosti bodu:\>\"", ",", "Bold"}], "]"}], "]"}], 
        ";", "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<Vzdialenos\[THacek] od po\[CHacek]iatku: \>\"", ",", 
          RowBox[{"Style", "[", 
           RowBox[{"dist", ",", "style"}], "]"}]}], "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<Kvadrant: \>\"", ",", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"Which", "[", 
             RowBox[{
              RowBox[{
               RowBox[{
                RowBox[{"point", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], ">", "0"}], "&&", 
               RowBox[{
                RowBox[{"point", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], ">", "0"}]}], ",", 
              "\"\<I\>\"", ",", 
              RowBox[{
               RowBox[{
                RowBox[{"point", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "<", "0"}], "&&", 
               RowBox[{
                RowBox[{"point", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], ">", "0"}]}], ",", 
              "\"\<II\>\"", ",", 
              RowBox[{
               RowBox[{
                RowBox[{"point", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "<", "0"}], "&&", 
               RowBox[{
                RowBox[{"point", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "<", "0"}]}], ",", 
              "\"\<III\>\"", ",", 
              RowBox[{
               RowBox[{
                RowBox[{"point", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], ">", "0"}], "&&", 
               RowBox[{
                RowBox[{"point", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "<", "0"}]}], ",", 
              "\"\<IV\>\"", ",", 
              RowBox[{
               RowBox[{
                RowBox[{"point", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "==", "0"}], "&&", 
               RowBox[{
                RowBox[{"point", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], ">", "0"}]}], ",", 
              "\"\<os y (kladn\[AAcute] \[CHacek]as\[THacek])\>\"", ",", 
              RowBox[{
               RowBox[{
                RowBox[{"point", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "==", "0"}], "&&", 
               RowBox[{
                RowBox[{"point", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "<", "0"}]}], ",", 
              "\"\<os y (z\[AAcute]porn\[AAcute] \[CHacek]as\[THacek])\>\"", 
              ",", 
              RowBox[{
               RowBox[{
                RowBox[{"point", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], ">", "0"}], "&&", 
               RowBox[{
                RowBox[{"point", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "==", "0"}]}], ",", 
              "\"\<os x (kladn\[AAcute] \[CHacek]as\[THacek])\>\"", ",", 
              RowBox[{
               RowBox[{
                RowBox[{"point", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "<", "0"}], "&&", 
               RowBox[{
                RowBox[{"point", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "==", "0"}]}], ",", 
              "\"\<os x (z\[AAcute]porn\[AAcute] \[CHacek]as\[THacek])\>\"", 
              ",", "True", ",", "\"\<po\[CHacek]iatok\>\""}], "]"}], ",", 
            "style"}], "]"}]}], "]"}], ";"}]}], "]"}]}], ";"}], "\n", 
   "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Pokro\[CHacek]il\[EAcute]", " ", "form\[AAcute]tovanie", " ", 
     "v\[YAcute]razov", " ", "pre", " ", "lep\[SHacek]ie", " ", "zobrazenie", 
     " ", "bodov"}], "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"FormatPointExpression", "[", "expr_", "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"expandedExpr", ",", "simplifiedExpr"}], "}"}], ",", 
       RowBox[{"(*", 
        RowBox[{"Rozvinutie", " ", "v\[YAcute]razu"}], "*)"}], 
       RowBox[{
        RowBox[{"expandedExpr", "=", 
         RowBox[{"Expand", "[", "expr", "]"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{"Z\[AAcute]kladn\[EAcute]", " ", "zjednodu\[SHacek]enie"}], 
         "*)"}], 
        RowBox[{"simplifiedExpr", "=", 
         RowBox[{"Simplify", "[", "expandedExpr", "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Vr\[AAcute]ti\[THacek]", " ", "v\[YAcute]sledok", " ", "v", " ", 
          "preferovanej", " ", "forme"}], "*)"}], "simplifiedExpr"}]}], 
      "]"}]}], ";"}], "\n", "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Funkcia", " ", "na", " ", "spracovanie", " ", "bodu", " ", "pre", " ", 
     "lep\[SHacek]ie", " ", "zobrazenie"}], "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"ProcessPoint", "[", "point_", "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "}"}], ",", 
       RowBox[{"(*", 
        RowBox[{
        "Aplikovanie", " ", "spracovania", " ", "na", " ", 
         "ka\[ZHacek]d\[UAcute]", " ", "s\[UAcute]radnicu"}], "*)"}], 
       RowBox[{"Map", "[", 
        RowBox[{"FormatPointExpression", ",", "point"}], "]"}]}], "]"}]}], 
    ";"}], "\n", "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Komplexnej\[SHacek]ia", " ", "funkcia", " ", "pre", " ", "spracovanie", 
     " ", "vnoren\[YAcute]ch", " ", "v\[YAcute]razov"}], "*)"}], 
   "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"ExpandNestedExpressions", "[", "expr_", "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"result", "=", "expr"}], "}"}], ",", 
       RowBox[{"(*", 
        RowBox[{
        "Detekcia", " ", "a", " ", "spracovanie", " ", "v\[YAcute]razov", " ",
          "ako", " ", 
         RowBox[{"1", "/", "4"}], " ", 
         RowBox[{"(", 
          RowBox[{
           RowBox[{"-", "7"}], "-", 
           RowBox[{"2", "*", 
            RowBox[{"Sqrt", "[", "5", "]"}]}]}], ")"}]}], "*)"}], 
       RowBox[{
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{
            RowBox[{"Head", "[", "expr", "]"}], "===", "Times"}], "&&", 
           RowBox[{
            RowBox[{"Length", "[", "expr", "]"}], ">=", "2"}], "&&", 
           RowBox[{"(", 
            RowBox[{
             RowBox[{"MatchQ", "[", 
              RowBox[{
               RowBox[{"expr", "[", 
                RowBox[{"[", "1", "]"}], "]"}], ",", "_Rational"}], "]"}], "||", 
             RowBox[{"MatchQ", "[", 
              RowBox[{
               RowBox[{"expr", "[", 
                RowBox[{"[", "1", "]"}], "]"}], ",", "_Integer"}], "]"}]}], 
            ")"}], "&&", 
           RowBox[{"MatchQ", "[", 
            RowBox[{
             RowBox[{"expr", "[", 
              RowBox[{"[", "2", "]"}], "]"}], ",", "_Plus"}], "]"}]}], ",", 
          RowBox[{"(*", 
           RowBox[{"Rozpis", " ", "zlomku", " ", "do", " ", 
            RowBox[{"s\[UAcute]\[CHacek]tu", "/", "rozdielu"}]}], "*)"}], 
          RowBox[{
           RowBox[{"result", "=", 
            RowBox[{"Expand", "[", "expr", "]"}]}], ";"}]}], "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{"Spracovanie", " ", "vnoren\[YAcute]ch", " ", "prvkov"}], 
         "*)"}], 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"Head", "[", "result", "]"}], "===", "Plus"}], ",", 
          RowBox[{"result", "=", 
           RowBox[{"Plus", "@@", 
            RowBox[{"Map", "[", 
             RowBox[{"ExpandNestedExpressions", ",", 
              RowBox[{"List", "@@", "result"}]}], "]"}]}]}], ",", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"Head", "[", "result", "]"}], "===", "Times"}], ",", 
            RowBox[{"result", "=", 
             RowBox[{"Times", "@@", 
              RowBox[{"Map", "[", 
               RowBox[{"ExpandNestedExpressions", ",", 
                RowBox[{"List", "@@", "result"}]}], "]"}]}]}]}], "]"}]}], 
         "]"}], ";", "\[IndentingNewLine]", "result"}]}], "]"}]}], ";"}], 
   "\n", "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Funkcia", " ", "na", " ", "pln\[UAcute]", " ", "\[UAcute]pravu", " ", 
     "v\[YAcute]razu", " ", "pre", " ", "zobrazenie"}], "*)"}], 
   "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"FullExpressionProcessor", "[", "expr_", "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"step1", ",", "step2"}], "}"}], ",", 
       RowBox[{"(*", 
        RowBox[{
         RowBox[{"Krok", " ", "1"}], ":", 
         RowBox[{"Expandova\[THacek]", " ", "v\[YAcute]raz"}]}], "*)"}], 
       RowBox[{
        RowBox[{"step1", "=", 
         RowBox[{"Expand", "[", "expr", "]"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
          RowBox[{"Krok", " ", "2"}], ":", 
          RowBox[{
          "Spracova\[THacek]", " ", "vnoren\[EAcute]", " ", 
           "v\[YAcute]razy"}]}], "*)"}], 
        RowBox[{"step2", "=", 
         RowBox[{"ExpandNestedExpressions", "[", "step1", "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{"Vr\[AAcute]ti\[THacek]", " ", "v\[YAcute]sledok"}], "*)"}], 
        "step2"}]}], "]"}]}], ";"}], "\n", "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Funkcia", " ", "na", " ", "spracovanie", " ", "bodu", " ", "s", " ", 
     "pln\[YAcute]m", " ", "spracovan\[IAcute]m", " ", "v\[YAcute]razov"}], 
    "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"ProcessPointComplete", "[", "point_", "]"}], ":=", 
     RowBox[{"Map", "[", 
      RowBox[{"FullExpressionProcessor", ",", "point"}], "]"}]}], ";"}], "\n",
    "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Vylep\[SHacek]en\[AAcute]", " ", "funkcia", " ", "na", " ", "zobrazenie",
      " ", "postupn\[EAcute]ho", " ", "v\[YAcute]po\[CHacek]tu", " ", 
     "transform\[AAcute]ci\[IAcute]"}], "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"DisplayTransformationSequence", "[", 
      RowBox[{
      "pociatocny_", ",", "druhy_", ",", "treti_", ",", "finalny_", ",", 
       "prva_", ",", "druha_", ",", "tretia_"}], "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "}"}], ",", 
       RowBox[{
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nPOSTUPN\[CapitalYAcute] \
V\[CapitalYAcute]PO\[CapitalCHacek]ET:\>\"", ",", "Bold", ",", "16"}], "]"}], 
         "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{"Vlastnosti", " ", "p\[OHat]vodn\[EAcute]ho", " ", "bodu"}], 
         "*)"}], 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nP\[OHat]vodn\[YAcute] bod P:\>\"", ",", "Bold", ",", "14"}],
           "]"}], "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"Print", "[", "\"\<S\[UAcute]radnice:\>\"", "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{"MatrixForm", "[", 
          RowBox[{"Map", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"Style", "[", 
              RowBox[{"#", ",", "Blue"}], "]"}], "&"}], ",", "pociatocny", 
            ",", 
            RowBox[{"{", "0", "}"}]}], "]"}], "]"}], "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"DisplayPointProperties", "[", 
         RowBox[{"pociatocny", ",", "Blue"}], "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
          RowBox[{
          "Vlastnosti", " ", "po", " ", "prvej", " ", 
           "transform\[AAcute]cii"}], "-", 
          RowBox[{
          "S", " ", "MAXIM\[CapitalAAcute]LNE", " ", 
           "VYLEP\[CapitalSHacek]EN\[CapitalYAcute]M", " ", 
           "ZOBRAZEN\[CapitalIAcute]M"}]}], "*)"}], 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
           RowBox[{
           "\"\<\\nPo prvej transform\[AAcute]cii (\>\"", "<>", "prva", "<>", 
            "\"\<):\>\""}], ",", "Bold", ",", "14"}], "]"}], "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"Print", "[", "\"\<S\[UAcute]radnice bodu P':\>\"", "]"}], 
        ";", "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Spracovanie", " ", "bodu", " ", "pre", " ", "lep\[SHacek]ie", " ", 
          "zobrazenie"}], "*)"}], 
        RowBox[{"Module", "[", 
         RowBox[{
          RowBox[{"{", "processedPoint", "}"}], ",", 
          RowBox[{
           RowBox[{"processedPoint", "=", 
            RowBox[{"ProcessPointComplete", "[", "druhy", "]"}]}], ";", 
           "\[IndentingNewLine]", 
           RowBox[{"(*", 
            RowBox[{
            "Zobrazenie", " ", "upraven\[YAcute]ch", " ", "v\[YAcute]razov"}],
             "*)"}], 
           RowBox[{"Print", "[", 
            RowBox[{"MatrixForm", "[", 
             RowBox[{"Map", "[", 
              RowBox[{
               RowBox[{
                RowBox[{"Style", "[", 
                 RowBox[{"#", ",", "mildGreen"}], "]"}], "&"}], ",", 
               "processedPoint", ",", 
               RowBox[{"{", "0", "}"}]}], "]"}], "]"}], "]"}], ";"}]}], "]"}],
         ";", "\[IndentingNewLine]", 
        RowBox[{"DisplayPointProperties", "[", 
         RowBox[{"druhy", ",", "mildGreen"}], "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
          RowBox[{
          "Vlastnosti", " ", "po", " ", "druhej", " ", 
           "transform\[AAcute]cii"}], "-", 
          RowBox[{
          "S", " ", "MAXIM\[CapitalAAcute]LNE", " ", 
           "VYLEP\[CapitalSHacek]EN\[CapitalYAcute]M", " ", 
           "ZOBRAZEN\[CapitalIAcute]M"}]}], "*)"}], 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
           RowBox[{
           "\"\<\\nPo druhej transform\[AAcute]cii (\>\"", "<>", "druha", 
            "<>", "\"\<):\>\""}], ",", "Bold", ",", "14"}], "]"}], "]"}], ";",
         "\[IndentingNewLine]", 
        RowBox[{"Print", "[", "\"\<S\[UAcute]radnice bodu P'':\>\"", "]"}], 
        ";", "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Spracovanie", " ", "bodu", " ", "pre", " ", "lep\[SHacek]ie", " ", 
          "zobrazenie"}], "*)"}], 
        RowBox[{"Module", "[", 
         RowBox[{
          RowBox[{"{", "processedPoint", "}"}], ",", 
          RowBox[{
           RowBox[{"processedPoint", "=", 
            RowBox[{"ProcessPointComplete", "[", "treti", "]"}]}], ";", 
           "\[IndentingNewLine]", 
           RowBox[{"(*", 
            RowBox[{
            "Zobrazenie", " ", "upraven\[YAcute]ch", " ", "v\[YAcute]razov"}],
             "*)"}], 
           RowBox[{"Print", "[", 
            RowBox[{"MatrixForm", "[", 
             RowBox[{"Map", "[", 
              RowBox[{
               RowBox[{
                RowBox[{"Style", "[", 
                 RowBox[{"#", ",", "Orange"}], "]"}], "&"}], ",", 
               "processedPoint", ",", 
               RowBox[{"{", "0", "}"}]}], "]"}], "]"}], "]"}], ";"}]}], "]"}],
         ";", "\[IndentingNewLine]", 
        RowBox[{"DisplayPointProperties", "[", 
         RowBox[{"treti", ",", "Orange"}], "]"}], ";", "\[IndentingNewLine]", 
        
        RowBox[{"(*", 
         RowBox[{
          RowBox[{
          "Vlastnosti", " ", "po", " ", "tretej", " ", 
           "transform\[AAcute]cii"}], "-", 
          RowBox[{
          "S", " ", "MAXIM\[CapitalAAcute]LNE", " ", 
           "VYLEP\[CapitalSHacek]EN\[CapitalYAcute]M", " ", 
           "ZOBRAZEN\[CapitalIAcute]M"}]}], "*)"}], 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
           RowBox[{
           "\"\<\\nPo tretej transform\[AAcute]cii (\>\"", "<>", "tretia", 
            "<>", "\"\<):\>\""}], ",", "Bold", ",", "14"}], "]"}], "]"}], ";",
         "\[IndentingNewLine]", 
        RowBox[{"Print", "[", "\"\<S\[UAcute]radnice bodu P''':\>\"", "]"}], 
        ";", "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Spracovanie", " ", "bodu", " ", "pre", " ", "lep\[SHacek]ie", " ", 
          "zobrazenie"}], "*)"}], 
        RowBox[{"Module", "[", 
         RowBox[{
          RowBox[{"{", "processedPoint", "}"}], ",", 
          RowBox[{
           RowBox[{"processedPoint", "=", 
            RowBox[{"ProcessPointComplete", "[", "finalny", "]"}]}], ";", 
           "\[IndentingNewLine]", 
           RowBox[{"(*", 
            RowBox[{
            "Zobrazenie", " ", "upraven\[YAcute]ch", " ", "v\[YAcute]razov"}],
             "*)"}], 
           RowBox[{"Print", "[", 
            RowBox[{"MatrixForm", "[", 
             RowBox[{"Map", "[", 
              RowBox[{
               RowBox[{
                RowBox[{"Style", "[", 
                 RowBox[{"#", ",", "Red"}], "]"}], "&"}], ",", 
               "processedPoint", ",", 
               RowBox[{"{", "0", "}"}]}], "]"}], "]"}], "]"}], ";"}]}], "]"}],
         ";", "\[IndentingNewLine]", 
        RowBox[{"DisplayPointProperties", "[", 
         RowBox[{"finalny", ",", "Red"}], "]"}], ";"}]}], "]"}]}], ";"}], 
   "\n", "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Funkcia", " ", "na", " ", "vytvorenie", " ", "vizualiz\[AAcute]cie", " ",
      "transform\[AAcute]ci\[IAcute]"}], "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"CreatePointVisualization", "[", 
      RowBox[{
      "pociatocny_", ",", "druhy_", ",", "treti_", ",", "finalny_", ",", 
       "prva_", ",", "druha_", ",", "tretia_"}], "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
        "allPoints", ",", "xMin", ",", "xMax", ",", "yMin", ",", "yMax", ",", 
         "xRange", ",", "yRange", ",", 
         RowBox[{"padding", "=", "1"}], ",", 
         RowBox[{"brightBlue", "=", 
          RowBox[{"RGBColor", "[", 
           RowBox[{"0", ",", "0.4", ",", "0.8"}], "]"}]}], ",", 
         RowBox[{"brightGreen", "=", 
          RowBox[{"RGBColor", "[", 
           RowBox[{"0.2", ",", "0.7", ",", "0.3"}], "]"}]}], ",", 
         RowBox[{"brightOrange", "=", 
          RowBox[{"RGBColor", "[", 
           RowBox[{"1", ",", "0.6", ",", "0"}], "]"}]}], ",", 
         RowBox[{"brightRed", "=", 
          RowBox[{"RGBColor", "[", 
           RowBox[{"0.9", ",", "0.1", ",", "0.1"}], "]"}]}], ",", 
         RowBox[{"lightGray", "=", 
          RowBox[{"RGBColor", "[", 
           RowBox[{"0.9", ",", "0.9", ",", "0.9"}], "]"}]}], ",", 
         "labelPositions", ",", "maxRange", ",", "xMid", ",", "yMid"}], "}"}],
        ",", 
       RowBox[{
        RowBox[{"allPoints", "=", 
         RowBox[{"{", 
          RowBox[{"pociatocny", ",", "druhy", ",", "treti", ",", "finalny"}], 
          "}"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"xMin", "=", 
         RowBox[{"Min", "[", 
          RowBox[{"N", "[", 
           RowBox[{"allPoints", "[", 
            RowBox[{"[", 
             RowBox[{"All", ",", "1"}], "]"}], "]"}], "]"}], "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"xMax", "=", 
         RowBox[{"Max", "[", 
          RowBox[{"N", "[", 
           RowBox[{"allPoints", "[", 
            RowBox[{"[", 
             RowBox[{"All", ",", "1"}], "]"}], "]"}], "]"}], "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"yMin", "=", 
         RowBox[{"Min", "[", 
          RowBox[{"N", "[", 
           RowBox[{"allPoints", "[", 
            RowBox[{"[", 
             RowBox[{"All", ",", "2"}], "]"}], "]"}], "]"}], "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"yMax", "=", 
         RowBox[{"Max", "[", 
          RowBox[{"N", "[", 
           RowBox[{"allPoints", "[", 
            RowBox[{"[", 
             RowBox[{"All", ",", "2"}], "]"}], "]"}], "]"}], "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Add", " ", "padding", " ", "to", " ", "ensure", " ", "points", " ", 
          
          RowBox[{"don", "'"}], "t", " ", "crowd", " ", "the", " ", "edges"}],
          "*)"}], 
        RowBox[{"xRange", "=", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"xMin", "-", "padding"}], ",", 
           RowBox[{"xMax", "+", "padding"}]}], "}"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"yRange", "=", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"yMin", "-", "padding"}], ",", 
           RowBox[{"yMax", "+", "padding"}]}], "}"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"maxRange", "=", 
         RowBox[{"Max", "[", 
          RowBox[{
           RowBox[{
            RowBox[{"xRange", "[", 
             RowBox[{"[", "2", "]"}], "]"}], "-", 
            RowBox[{"xRange", "[", 
             RowBox[{"[", "1", "]"}], "]"}]}], ",", 
           RowBox[{
            RowBox[{"yRange", "[", 
             RowBox[{"[", "2", "]"}], "]"}], "-", 
            RowBox[{"yRange", "[", 
             RowBox[{"[", "1", "]"}], "]"}]}]}], "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"xMid", "=", 
         RowBox[{"Mean", "[", "xRange", "]"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"yMid", "=", 
         RowBox[{"Mean", "[", "yRange", "]"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"xRange", "=", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"xMid", "-", 
            RowBox[{"maxRange", "/", "2"}]}], ",", 
           RowBox[{"xMid", "+", 
            RowBox[{"maxRange", "/", "2"}]}]}], "}"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"yRange", "=", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"yMid", "-", 
            RowBox[{"maxRange", "/", "2"}]}], ",", 
           RowBox[{"yMid", "+", 
            RowBox[{"maxRange", "/", "2"}]}]}], "}"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Create", " ", "improved", " ", "label", " ", "positions", " ", 
          "with", " ", "balanced", " ", "distance", " ", "from", " ", 
          "points"}], "*)"}], 
        RowBox[{"labelPositions", "=", 
         RowBox[{"Table", "[", 
          RowBox[{
           RowBox[{"Module", "[", 
            RowBox[{
             RowBox[{"{", 
              RowBox[{
               RowBox[{"point", "=", 
                RowBox[{"allPoints", "[", 
                 RowBox[{"[", "i", "]"}], "]"}]}], ",", 
               RowBox[{"(*", 
                RowBox[{
                 RowBox[{
                  RowBox[{"Better", " ", "balanced", " ", "offset"}], "-", 
                  RowBox[{"not", " ", "too", " ", "close"}]}], ",", 
                 RowBox[{"not", " ", "too", " ", "far"}]}], "*)"}], 
               RowBox[{"offset", "=", 
                RowBox[{"{", 
                 RowBox[{"0.4", ",", "0.4"}], "}"}]}], ",", 
               RowBox[{"nearby", "=", 
                RowBox[{"{", "}"}]}]}], "}"}], ",", 
             RowBox[{"(*", 
              RowBox[{
              "Find", " ", "nearby", " ", "points", " ", "to", " ", "avoid", 
               " ", "overlaps"}], "*)"}], 
             RowBox[{
              RowBox[{"nearby", "=", 
               RowBox[{"DeleteCases", "[", 
                RowBox[{"allPoints", ",", "point"}], "]"}]}], ";", 
              "\[IndentingNewLine]", 
              RowBox[{"nearby", "=", 
               RowBox[{"Select", "[", 
                RowBox[{"nearby", ",", 
                 RowBox[{
                  RowBox[{
                   RowBox[{"EuclideanDistance", "[", 
                    RowBox[{"#", ",", "point"}], "]"}], "<", "0.7"}], "&"}]}],
                 "]"}]}], ";", "\[IndentingNewLine]", 
              RowBox[{"If", "[", 
               RowBox[{
                RowBox[{
                 RowBox[{"Length", "[", "nearby", "]"}], ">", "0"}], ",", 
                RowBox[{"(*", 
                 RowBox[{
                 "Calculate", " ", "a", " ", "balanced", " ", "offset", " ", 
                  "based", " ", "on", " ", "nearby", " ", "points"}], "*)"}], 
                
                RowBox[{
                 RowBox[{"offset", "=", 
                  RowBox[{"Mean", "[", 
                   RowBox[{"Table", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"Normalize", "[", 
                    RowBox[{"point", "-", "nearbyPoint"}], "]"}], "*", 
                    "0.5"}], ",", 
                    RowBox[{"{", 
                    RowBox[{"nearbyPoint", ",", "nearby"}], "}"}]}], "]"}], 
                   "]"}]}], ";", "\[IndentingNewLine]", 
                 RowBox[{"(*", 
                  RowBox[{
                  "Adjust", " ", "for", " ", "single", " ", "nearby", " ", 
                   "point"}], "*)"}], 
                 RowBox[{"If", "[", 
                  RowBox[{
                   RowBox[{
                    RowBox[{"Length", "[", "nearby", "]"}], "==", "1"}], ",", 
                   
                   RowBox[{"offset", "=", 
                    RowBox[{"offset", "*", "1.5"}]}]}], "]"}], ";"}], ",", 
                RowBox[{"(*", 
                 RowBox[{
                  RowBox[{"Default", " ", "offsets"}], "-", 
                  RowBox[{"balanced", " ", "distance"}]}], "*)"}], 
                RowBox[{
                 RowBox[{"If", "[", 
                  RowBox[{
                   RowBox[{
                    RowBox[{"N", "[", 
                    RowBox[{"point", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], "]"}], ">", "0"}], ",", 
                   RowBox[{
                    RowBox[{"offset", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], "=", "0.4"}], ",", 
                   RowBox[{
                    RowBox[{"offset", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], "=", 
                    RowBox[{"-", "0.4"}]}]}], "]"}], ";", 
                 "\[IndentingNewLine]", 
                 RowBox[{"If", "[", 
                  RowBox[{
                   RowBox[{
                    RowBox[{"N", "[", 
                    RowBox[{"point", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], "]"}], ">", "0"}], ",", 
                   RowBox[{
                    RowBox[{"offset", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], "=", "0.4"}], ",", 
                   RowBox[{
                    RowBox[{"offset", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], "=", 
                    RowBox[{"-", "0.4"}]}]}], "]"}], ";"}]}], "]"}], ";", 
              "\[IndentingNewLine]", 
              RowBox[{"(*", 
               RowBox[{
               "Scale", " ", "the", " ", "offset", " ", "based", " ", "on", 
                " ", "distance", " ", "from", " ", "origin"}], "*)"}], 
              RowBox[{"offset", "=", 
               RowBox[{"offset", "*", 
                RowBox[{"(", 
                 RowBox[{"0.8", "+", 
                  RowBox[{"0.1", "*", 
                   RowBox[{"EuclideanDistance", "[", 
                    RowBox[{"point", ",", 
                    RowBox[{"{", 
                    RowBox[{"0", ",", "0"}], "}"}]}], "]"}]}]}], ")"}]}]}], 
              ";", "\[IndentingNewLine]", 
              RowBox[{"point", "+", "offset"}]}]}], "]"}], ",", 
           RowBox[{"{", 
            RowBox[{"i", ",", "4"}], "}"}]}], "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"Graphics", "[", 
         RowBox[{
          RowBox[{"{", 
           RowBox[{"(*", 
            RowBox[{"Grid", " ", "lines"}], "*)"}], 
           RowBox[{"lightGray", ",", "Thin", ",", 
            RowBox[{"Table", "[", 
             RowBox[{
              RowBox[{"Line", "[", 
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"xRange", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], ",", "y"}], "}"}], ",", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"xRange", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], ",", "y"}], "}"}]}], 
                "}"}], "]"}], ",", 
              RowBox[{"{", 
               RowBox[{"y", ",", 
                RowBox[{"Ceiling", "[", 
                 RowBox[{"yRange", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", 
                RowBox[{"Floor", "[", 
                 RowBox[{"yRange", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "}"}]}], "]"}], 
            ",", 
            RowBox[{"Table", "[", 
             RowBox[{
              RowBox[{"Line", "[", 
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"{", 
                  RowBox[{"x", ",", 
                   RowBox[{"yRange", "[", 
                    RowBox[{"[", "1", "]"}], "]"}]}], "}"}], ",", 
                 RowBox[{"{", 
                  RowBox[{"x", ",", 
                   RowBox[{"yRange", "[", 
                    RowBox[{"[", "2", "]"}], "]"}]}], "}"}]}], "}"}], "]"}], 
              ",", 
              RowBox[{"{", 
               RowBox[{"x", ",", 
                RowBox[{"Ceiling", "[", 
                 RowBox[{"xRange", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", 
                RowBox[{"Floor", "[", 
                 RowBox[{"xRange", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "}"}]}], "]"}], 
            ",", 
            RowBox[{"(*", "Axes", "*)"}], "Black", ",", 
            RowBox[{"Thickness", "[", "0.003", "]"}], ",", 
            RowBox[{"Arrowheads", "[", "0.02", "]"}], ",", 
            RowBox[{"Arrow", "[", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"xRange", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], ",", "0"}], "}"}], ",", 
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"xRange", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], ",", "0"}], "}"}]}], "}"}], 
             "]"}], ",", 
            RowBox[{"Arrow", "[", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"{", 
                RowBox[{"0", ",", 
                 RowBox[{"yRange", "[", 
                  RowBox[{"[", "1", "]"}], "]"}]}], "}"}], ",", 
               RowBox[{"{", 
                RowBox[{"0", ",", 
                 RowBox[{"yRange", "[", 
                  RowBox[{"[", "2", "]"}], "]"}]}], "}"}]}], "}"}], "]"}], 
            ",", 
            RowBox[{"Text", "[", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"\"\<x\>\"", ",", "Bold", ",", "14"}], "]"}], ",", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{
                 RowBox[{"xRange", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "-", "0.3"}], ",", 
                RowBox[{"-", "0.3"}]}], "}"}]}], "]"}], ",", 
            RowBox[{"Text", "[", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"\"\<y\>\"", ",", "Bold", ",", "14"}], "]"}], ",", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{"-", "0.3"}], ",", 
                RowBox[{
                 RowBox[{"yRange", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "-", "0.3"}]}], "}"}]}], 
             "]"}], ",", 
            RowBox[{"(*", 
             RowBox[{"Tick", " ", "marks"}], "*)"}], 
            RowBox[{"Table", "[", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{
                RowBox[{"Line", "[", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"{", 
                    RowBox[{"i", ",", 
                    RowBox[{"-", "0.1"}]}], "}"}], ",", 
                   RowBox[{"{", 
                    RowBox[{"i", ",", "0.1"}], "}"}]}], "}"}], "]"}], ",", 
                RowBox[{"Text", "[", 
                 RowBox[{
                  RowBox[{"Style", "[", 
                   RowBox[{"i", ",", "10"}], "]"}], ",", 
                  RowBox[{"{", 
                   RowBox[{"i", ",", 
                    RowBox[{"-", "0.3"}]}], "}"}]}], "]"}]}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{"i", ",", 
                RowBox[{"Ceiling", "[", 
                 RowBox[{"xRange", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", 
                RowBox[{"Floor", "[", 
                 RowBox[{"xRange", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "}"}]}], "]"}], 
            ",", 
            RowBox[{"Table", "[", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{
                RowBox[{"Line", "[", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"{", 
                    RowBox[{
                    RowBox[{"-", "0.1"}], ",", "i"}], "}"}], ",", 
                   RowBox[{"{", 
                    RowBox[{"0.1", ",", "i"}], "}"}]}], "}"}], "]"}], ",", 
                RowBox[{"Text", "[", 
                 RowBox[{
                  RowBox[{"Style", "[", 
                   RowBox[{"i", ",", "10"}], "]"}], ",", 
                  RowBox[{"{", 
                   RowBox[{
                    RowBox[{"-", "0.3"}], ",", "i"}], "}"}]}], "]"}]}], "}"}],
               ",", 
              RowBox[{"{", 
               RowBox[{"i", ",", 
                RowBox[{"Ceiling", "[", 
                 RowBox[{"yRange", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", 
                RowBox[{"Floor", "[", 
                 RowBox[{"yRange", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "}"}]}], "]"}], 
            ",", 
            RowBox[{"(*", 
             RowBox[{
              RowBox[{"Transformation", " ", "paths"}], "-", 
              RowBox[{
              "dashed", " ", "lines", " ", "between", " ", "corresponding", 
               " ", "points"}]}], "*)"}], 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Opacity", "[", "0.5", "]"}], ",", "Dashed", ",", 
              RowBox[{"Thickness", "[", "0.002", "]"}], ",", "brightGreen", 
              ",", 
              RowBox[{"Line", "[", 
               RowBox[{"{", 
                RowBox[{"pociatocny", ",", "druhy"}], "}"}], "]"}]}], "}"}], 
            ",", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Opacity", "[", "0.5", "]"}], ",", "Dashed", ",", 
              RowBox[{"Thickness", "[", "0.002", "]"}], ",", "brightOrange", 
              ",", 
              RowBox[{"Line", "[", 
               RowBox[{"{", 
                RowBox[{"druhy", ",", "treti"}], "}"}], "]"}]}], "}"}], ",", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Opacity", "[", "0.5", "]"}], ",", "Dashed", ",", 
              RowBox[{"Thickness", "[", "0.002", "]"}], ",", "brightRed", ",", 
              RowBox[{"Line", "[", 
               RowBox[{"{", 
                RowBox[{"treti", ",", "finalny"}], "}"}], "]"}]}], "}"}], ",", 
            RowBox[{"(*", 
             RowBox[{
             "Points", " ", "with", " ", "white", " ", "background", " ", 
              "circles"}], "*)"}], 
            RowBox[{"(*", 
             RowBox[{
              RowBox[{"Original", " ", "point"}], "-", "blue"}], "*)"}], 
            RowBox[{"{", 
             RowBox[{"White", ",", 
              RowBox[{"Disk", "[", 
               RowBox[{"pociatocny", ",", "0.2"}], "]"}], ",", "brightBlue", 
              ",", 
              RowBox[{"Disk", "[", 
               RowBox[{"pociatocny", ",", "0.15"}], "]"}]}], "}"}], ",", 
            RowBox[{"(*", 
             RowBox[{
              RowBox[{"Second", " ", "point"}], "-", "green"}], "*)"}], 
            RowBox[{"{", 
             RowBox[{"White", ",", 
              RowBox[{"Disk", "[", 
               RowBox[{"druhy", ",", "0.2"}], "]"}], ",", "brightGreen", ",", 
              
              RowBox[{"Disk", "[", 
               RowBox[{"druhy", ",", "0.15"}], "]"}]}], "}"}], ",", 
            RowBox[{"(*", 
             RowBox[{
              RowBox[{"Third", " ", "point"}], "-", "orange"}], "*)"}], 
            RowBox[{"{", 
             RowBox[{"White", ",", 
              RowBox[{"Disk", "[", 
               RowBox[{"treti", ",", "0.2"}], "]"}], ",", "brightOrange", ",", 
              RowBox[{"Disk", "[", 
               RowBox[{"treti", ",", "0.15"}], "]"}]}], "}"}], ",", 
            RowBox[{"(*", 
             RowBox[{
              RowBox[{"Final", " ", "point"}], "-", "red"}], "*)"}], 
            RowBox[{"{", 
             RowBox[{"White", ",", 
              RowBox[{"Disk", "[", 
               RowBox[{"finalny", ",", "0.2"}], "]"}], ",", "brightRed", ",", 
              
              RowBox[{"Disk", "[", 
               RowBox[{"finalny", ",", "0.15"}], "]"}]}], "}"}], ",", 
            RowBox[{"(*", 
             RowBox[{
             "Point", " ", "labels", " ", "with", " ", "white", " ", 
              "background", " ", "circles"}], "*)"}], 
            RowBox[{"Table", "[", 
             RowBox[{
              RowBox[{"With", "[", 
               RowBox[{
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"label", "=", 
                   RowBox[{"Which", "[", 
                    RowBox[{
                    RowBox[{"i", "==", "1"}], ",", "\"\<P\>\"", ",", 
                    RowBox[{"i", "==", "2"}], ",", "\"\<P'\>\"", ",", 
                    RowBox[{"i", "==", "3"}], ",", "\"\<P''\>\"", ",", 
                    RowBox[{"i", "==", "4"}], ",", "\"\<P'''\>\""}], "]"}]}], 
                  ",", 
                  RowBox[{"color", "=", 
                   RowBox[{"Which", "[", 
                    RowBox[{
                    RowBox[{"i", "==", "1"}], ",", "brightBlue", ",", 
                    RowBox[{"i", "==", "2"}], ",", "brightGreen", ",", 
                    RowBox[{"i", "==", "3"}], ",", "brightOrange", ",", 
                    RowBox[{"i", "==", "4"}], ",", "brightRed"}], "]"}]}], 
                  ",", 
                  RowBox[{"pos", "=", 
                   RowBox[{"labelPositions", "[", 
                    RowBox[{"[", "i", "]"}], "]"}]}]}], "}"}], ",", 
                RowBox[{"{", 
                 RowBox[{"(*", 
                  RowBox[{
                  "White", " ", "background", " ", "circle", " ", "for", " ", 
                   "label"}], "*)"}], 
                 RowBox[{"White", ",", 
                  RowBox[{"Disk", "[", 
                   RowBox[{"pos", ",", "0.25"}], "]"}], ",", 
                  RowBox[{"(*", 
                   RowBox[{
                   "Label", " ", "with", " ", "colored", " ", "text"}], 
                   "*)"}], "color", ",", 
                  RowBox[{"Text", "[", 
                   RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{"label", ",", "Bold", ",", "14"}], "]"}], ",", 
                    "pos"}], "]"}]}], "}"}]}], "]"}], ",", 
              RowBox[{"{", 
               RowBox[{"i", ",", "4"}], "}"}]}], "]"}]}], "}"}], ",", 
          RowBox[{"PlotRange", "->", 
           RowBox[{"{", 
            RowBox[{"xRange", ",", "yRange"}], "}"}]}], ",", 
          RowBox[{"AspectRatio", "->", "1"}], ",", 
          RowBox[{"(*", 
           RowBox[{
           "This", " ", "ensures", " ", "equal", " ", "scaling", " ", "for", 
            " ", "x", " ", "and", " ", "y", " ", "axes"}], "*)"}], 
          RowBox[{"ImageSize", "->", "650"}], ",", 
          RowBox[{"PlotLabel", "->", 
           RowBox[{"Style", "[", 
            RowBox[{
            "\"\<Postupn\[AAcute] aplik\[AAcute]cia troch \
transform\[AAcute]ci\[IAcute] bodu\>\"", ",", "Bold", ",", "16"}], "]"}]}], 
          ",", 
          RowBox[{"ImagePadding", "->", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"{", 
              RowBox[{"40", ",", "40"}], "}"}], ",", 
             RowBox[{"{", 
              RowBox[{"40", ",", "40"}], "}"}]}], "}"}]}], ",", 
          RowBox[{"Background", "->", "White"}], ",", 
          RowBox[{"Method", "->", 
           RowBox[{"{", 
            RowBox[{"\"\<ShrinkWrap\>\"", "->", "True"}], "}"}]}]}], 
         "]"}]}]}], "]"}]}], ";"}], "\n", "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Funkcia", " ", "na", " ", "v\[YAcute]ber", " ", "transform\[AAcute]cie", 
     " ", "s", " ", "popisom"}], "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"SelectTransformation", "[", 
      RowBox[{"message_", ",", 
       RowBox[{"exclude1_", ":", "\"\<\>\""}], ",", 
       RowBox[{"exclude2_", ":", "\"\<\>\""}]}], "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
        "options", ",", "fullOptions", ",", "formattedChoices", ",", 
         "result"}], "}"}], ",", 
       RowBox[{
        RowBox[{"options", "=", 
         RowBox[{"DeleteCases", "[", 
          RowBox[{
           RowBox[{"{", 
            RowBox[{
             RowBox[{"\"\<Posun\>\"", "->", 
              RowBox[{"{", 
               RowBox[{
               "\"\<Posun\>\"", ",", "\"\<Posun bodu vo smere vektora\>\""}], 
               "}"}]}], ",", 
             RowBox[{"\"\<Rot\[AAcute]cia\>\"", "->", 
              RowBox[{"{", 
               RowBox[{
               "\"\<Rot\[AAcute]cia\>\"", ",", 
                "\"\<Rot\[AAcute]cia bodu okolo po\[CHacek]iatku \
s\[UAcute]radnicovej s\[UAcute]stavy\>\""}], "}"}]}], ",", 
             RowBox[{
             "\"\<Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie\>\"\
", "->", 
              RowBox[{"{", 
               RowBox[{
               "\"\<Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie\>\
\"", ",", 
                "\"\<Zv\[ADoubleDot]\[CHacek]\[SHacek]enie alebo \
zmen\[SHacek]enie vzdialenosti bodu od po\[CHacek]iatku\>\""}], "}"}]}], ",", 
             
             RowBox[{"\"\<Skosenie\>\"", "->", 
              RowBox[{"{", 
               RowBox[{
               "\"\<Skosenie\>\"", ",", 
                "\"\<Skosenie bodu v smere os\[IAcute]\>\""}], "}"}]}], ",", 
             RowBox[{"\"\<Symetria\>\"", "->", 
              RowBox[{"{", 
               RowBox[{
               "\"\<Symetria\>\"", ",", 
                "\"\<Zrkadlenie bodu pod\:013ea zvolenej osi\>\""}], 
               "}"}]}]}], "}"}], ",", 
           RowBox[{"x_", "/;", 
            RowBox[{"MemberQ", "[", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{"exclude1", ",", "exclude2"}], "}"}], ",", 
              RowBox[{"First", "[", "x", "]"}]}], "]"}]}]}], "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
          RowBox[{
          "Pou\[ZHacek]ijeme", " ", "jednoduch\[SHacek]\[IAcute]", " ", 
           "pr\[IAcute]stup", " ", "s", " ", "ChoiceDialog"}], ",", 
          RowBox[{
          "ale", " ", "s", " ", "vertik\[AAcute]lnym", " ", "zoznamom"}]}], 
         "*)"}], 
        RowBox[{"ChoiceDialog", "[", 
         RowBox[{"message", ",", "options", ",", 
          RowBox[{
          "WindowTitle", "->", 
           "\"\<V\[YAcute]ber transfform\[AAcute]cie\>\""}], ",", 
          RowBox[{"WindowSize", "->", 
           RowBox[{"{", 
            RowBox[{"500", ",", "All"}], "}"}]}]}], "]"}]}]}], "]"}]}], ";"}],
    "\n", "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Hlavn\[AAcute]", " ", "funkcia", " ", "pre", " ", "trojit\[UAcute]", " ",
      "transform\[AAcute]ciu", " ", "s", " ", "vylep\[SHacek]en\[YAcute]m", 
     " ", "pou\[ZHacek]\[IAcute]vate\:013esk\[YAcute]m", " ", 
     "rozhran\[IAcute]m", " ", "a", " ", "o\[SHacek]etren\[IAcute]m", " ", 
     "ch\[YAcute]b"}], "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"BodTrojitaTransformacia", "[", "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
        "pociatocnyBod", ",", "prvaTransformacia", ",", "druhyBod", ",", 
         "druhaTransformacia", ",", "tretiBod", ",", "tretiaTransformacia", 
         ",", "finalnyBod", ",", "prvaPopis", ",", "druhaPopis", ",", 
         "tretiaPopis"}], "}"}], ",", 
       RowBox[{"(*", 
        RowBox[{
        "Potla\[CHacek]enie", " ", "chybov\[YAcute]ch", " ", 
         "spr\[AAcute]v"}], "*)"}], 
       RowBox[{"Quiet", "[", 
        RowBox[{"(*", 
         RowBox[{"\[CapitalUAcute]vodn\[AAcute]", " ", "spr\[AAcute]va"}], 
         "*)"}], 
        RowBox[{
         RowBox[{"Print", "[", 
          RowBox[{"Style", "[", 
           RowBox[{
           "\"\<TROJIT\[CapitalAAcute] GEOMETRICK\[CapitalAAcute] TRANSFORM\
\[CapitalAAcute]CIA BODU\>\"", ",", "Bold", ",", "24"}], "]"}], "]"}], ";", 
         "\[IndentingNewLine]", 
         RowBox[{"Print", "[", 
          RowBox[{"Style", "[", 
           RowBox[{
           "\"\<==========================================\>\"", ",", 
            "Bold"}], "]"}], "]"}], ";", "\[IndentingNewLine]", 
         RowBox[{"(*", 
          RowBox[{
          "Generovanie", " ", "a", " ", "zobrazenie", " ", 
           "po\[CHacek]iato\[CHacek]n\[EAcute]ho", " ", "bodu"}], "*)"}], 
         RowBox[{"pociatocnyBod", "=", 
          RowBox[{"GenerateInitialPoint", "[", "]"}]}], ";", 
         "\[IndentingNewLine]", 
         RowBox[{"(*", 
          RowBox[{
          "Upraven\[AAcute]", " ", "\[CHacek]as\[THacek]", " ", "pre", " ", 
           "zobrazenie", " ", "po\[CHacek]iato\[CHacek]n\[EAcute]ho", " ", 
           "bodu", " ", "v", " ", "maticovom", " ", "form\[AAcute]te"}], 
          "*)"}], 
         RowBox[{"Print", "[", 
          RowBox[{"Style", "[", 
           RowBox[{
           "\"\<\\nPO\[CapitalCHacek]IATO\[CapitalCHacek]N\[CapitalYAcute] \
BOD:\>\"", ",", "Bold", ",", "16"}], "]"}], "]"}], ";", "\n", 
         RowBox[{"Print", "[", "\"\<S\[UAcute]radnice bodu P:\>\"", "]"}], 
         ";", "\[IndentingNewLine]", "\n", 
         RowBox[{"(*", 
          RowBox[{
          "Zobrazenie", " ", "s\[UAcute]radn\[IAcute]c", " ", "v", " ", 
           "maticovom", " ", "tvare"}], "*)"}], "\[IndentingNewLine]", 
         RowBox[{"Print", "[", 
          RowBox[{"Row", "[", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"Style", "[", 
              RowBox[{"\"\<P = \>\"", ",", "Blue", ",", "Bold"}], "]"}], ",", 
             
             RowBox[{"MatrixForm", "[", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{"{", 
                 RowBox[{"Style", "[", 
                  RowBox[{
                   RowBox[{"pociatocnyBod", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], ",", "Blue"}], "]"}], 
                 "}"}], ",", 
                RowBox[{"{", 
                 RowBox[{"Style", "[", 
                  RowBox[{
                   RowBox[{"pociatocnyBod", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], ",", "Blue"}], "]"}], 
                 "}"}]}], "}"}], "]"}]}], "}"}], "]"}], "]"}], ";", "\n", 
         "\[IndentingNewLine]", 
         RowBox[{"DisplayPointProperties", "[", 
          RowBox[{"pociatocnyBod", ",", "Blue"}], "]"}], ";", 
         "\[IndentingNewLine]", 
         RowBox[{"(*", 
          RowBox[{
          "V\[YAcute]ber", " ", "a", " ", "aplik\[AAcute]cia", " ", "prvej", 
           " ", "transform\[AAcute]cie"}], "*)"}], 
         RowBox[{"prvaPopis", "=", 
          RowBox[{
          "SelectTransformation", "[", 
           "\"\<Vyberte prv\[UAcute] transform\[AAcute]ciu:\>\"", "]"}]}], 
         ";", "\[IndentingNewLine]", 
         RowBox[{"If", "[", 
          RowBox[{
           RowBox[{"prvaPopis", "===", "$Canceled"}], ",", 
           RowBox[{"Return", "[", "]"}]}], "]"}], ";", "\[IndentingNewLine]", 
         
         RowBox[{"prvaTransformacia", "=", 
          RowBox[{"First", "[", "prvaPopis", "]"}]}], ";", 
         "\[IndentingNewLine]", 
         RowBox[{"Print", "[", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{
            "\"\<\\nPRV\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA: \>\"", "<>",
              "prvaTransformacia"}], ",", "Bold", ",", "16"}], "]"}], "]"}], 
         ";", "\[IndentingNewLine]", 
         RowBox[{"druhyBod", "=", 
          RowBox[{"Switch", "[", 
           RowBox[{"prvaTransformacia", ",", "\"\<Posun\>\"", ",", 
            RowBox[{"BodPosun", "[", "pociatocnyBod", "]"}], ",", 
            "\"\<Rot\[AAcute]cia\>\"", ",", 
            RowBox[{"BodRotacia", "[", "pociatocnyBod", "]"}], ",", 
            "\"\<Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie\>\"",
             ",", 
            RowBox[{"BodZvacsenieZmensenie", "[", "pociatocnyBod", "]"}], 
            ",", "\"\<Skosenie\>\"", ",", 
            RowBox[{"BodSkosenie", "[", "pociatocnyBod", "]"}], ",", 
            "\"\<Symetria\>\"", ",", 
            RowBox[{"BodSymetria", "[", "pociatocnyBod", "]"}]}], "]"}]}], 
         ";", "\[IndentingNewLine]", 
         RowBox[{"(*", 
          RowBox[{
          "V\[YAcute]ber", " ", "a", " ", "aplik\[AAcute]cia", " ", "druhej", 
           " ", "transform\[AAcute]cie"}], "*)"}], 
         RowBox[{"druhaPopis", "=", 
          RowBox[{"SelectTransformation", "[", 
           RowBox[{
           "\"\<Vyberte druh\[UAcute] transform\[AAcute]ciu:\>\"", ",", 
            "prvaTransformacia", ",", "\"\<\>\""}], "]"}]}], ";", 
         "\[IndentingNewLine]", 
         RowBox[{"If", "[", 
          RowBox[{
           RowBox[{"druhaPopis", "===", "$Canceled"}], ",", 
           RowBox[{"Return", "[", "]"}]}], "]"}], ";", "\[IndentingNewLine]", 
         
         RowBox[{"druhaTransformacia", "=", 
          RowBox[{"First", "[", "druhaPopis", "]"}]}], ";", 
         "\[IndentingNewLine]", 
         RowBox[{"Print", "[", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{
            "\"\<\\nDRUH\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA: \>\"", 
             "<>", "druhaTransformacia"}], ",", "Bold", ",", "16"}], "]"}], 
          "]"}], ";", "\[IndentingNewLine]", 
         RowBox[{"tretiBod", "=", 
          RowBox[{"Switch", "[", 
           RowBox[{"druhaTransformacia", ",", "\"\<Posun\>\"", ",", 
            RowBox[{"BodPosun", "[", "druhyBod", "]"}], ",", 
            "\"\<Rot\[AAcute]cia\>\"", ",", 
            RowBox[{"BodRotacia", "[", "druhyBod", "]"}], ",", 
            "\"\<Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie\>\"",
             ",", 
            RowBox[{"BodZvacsenieZmensenie", "[", "druhyBod", "]"}], ",", 
            "\"\<Skosenie\>\"", ",", 
            RowBox[{"BodSkosenie", "[", "druhyBod", "]"}], ",", 
            "\"\<Symetria\>\"", ",", 
            RowBox[{"BodSymetria", "[", "druhyBod", "]"}]}], "]"}]}], ";", 
         "\[IndentingNewLine]", 
         RowBox[{"(*", 
          RowBox[{
          "V\[YAcute]ber", " ", "a", " ", "aplik\[AAcute]cia", " ", "tretej", 
           " ", "transform\[AAcute]cie"}], "*)"}], 
         RowBox[{"tretiaPopis", "=", 
          RowBox[{"SelectTransformation", "[", 
           RowBox[{
           "\"\<Vyberte tretiu transform\[AAcute]ciu:\>\"", ",", 
            "prvaTransformacia", ",", "druhaTransformacia"}], "]"}]}], ";", 
         "\[IndentingNewLine]", 
         RowBox[{"If", "[", 
          RowBox[{
           RowBox[{"tretiaPopis", "===", "$Canceled"}], ",", 
           RowBox[{"Return", "[", "]"}]}], "]"}], ";", "\[IndentingNewLine]", 
         
         RowBox[{"tretiaTransformacia", "=", 
          RowBox[{"First", "[", "tretiaPopis", "]"}]}], ";", 
         "\[IndentingNewLine]", 
         RowBox[{"Print", "[", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{
            "\"\<\\nTRETIA TRANSFORM\[CapitalAAcute]CIA: \>\"", "<>", 
             "tretiaTransformacia"}], ",", "Bold", ",", "16"}], "]"}], "]"}], 
         ";", "\[IndentingNewLine]", 
         RowBox[{"finalnyBod", "=", 
          RowBox[{"Switch", "[", 
           RowBox[{"tretiaTransformacia", ",", "\"\<Posun\>\"", ",", 
            RowBox[{"BodPosun", "[", "tretiBod", "]"}], ",", 
            "\"\<Rot\[AAcute]cia\>\"", ",", 
            RowBox[{"BodRotacia", "[", "tretiBod", "]"}], ",", 
            "\"\<Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie\>\"",
             ",", 
            RowBox[{"BodZvacsenieZmensenie", "[", "tretiBod", "]"}], ",", 
            "\"\<Skosenie\>\"", ",", 
            RowBox[{"BodSkosenie", "[", "tretiBod", "]"}], ",", 
            "\"\<Symetria\>\"", ",", 
            RowBox[{"BodSymetria", "[", "tretiBod", "]"}]}], "]"}]}], ";", 
         "\[IndentingNewLine]", 
         RowBox[{"(*", 
          RowBox[{
          "Zobrazenie", " ", "v\[YAcute]po\[CHacek]tu", " ", "a", " ", 
           "v\[YAcute]sledkov"}], "*)"}], 
         RowBox[{"DisplayTransformationSequence", "[", 
          RowBox[{
          "pociatocnyBod", ",", "druhyBod", ",", "tretiBod", ",", 
           "finalnyBod", ",", "prvaTransformacia", ",", "druhaTransformacia", 
           ",", "tretiaTransformacia"}], "]"}], ";", "\[IndentingNewLine]", 
         RowBox[{"(*", 
          RowBox[{
          "Zobrazenie", " ", "s\[UAcute]hrnnej", " ", "vizualiz\[AAcute]cie", 
           " ", "a", " ", "presunutie", " ", "legendy"}], "*)"}], 
         RowBox[{"Print", "[", 
          RowBox[{"Style", "[", 
           RowBox[{
           "\"\<\\nS\[CapitalUAcute]HRNN\[CapitalAAcute] VIZUALIZ\
\[CapitalAAcute]CIA:\>\"", ",", "Bold", ",", "16"}], "]"}], "]"}], ";", 
         "\[IndentingNewLine]", 
         RowBox[{"Print", "[", 
          RowBox[{"CreatePointVisualization", "[", 
           RowBox[{
           "pociatocnyBod", ",", "druhyBod", ",", "tretiBod", ",", 
            "finalnyBod", ",", "prvaTransformacia", ",", "druhaTransformacia",
             ",", "tretiaTransformacia"}], "]"}], "]"}], ";", 
         "\[IndentingNewLine]", 
         RowBox[{"Print", "[", 
          RowBox[{"Style", "[", 
           RowBox[{"\"\<\\nLEGENDA:\>\"", ",", "Bold", ",", "16"}], "]"}], 
          "]"}], ";", "\[IndentingNewLine]", 
         RowBox[{"Print", "[", 
          RowBox[{
           RowBox[{"Style", "[", 
            RowBox[{"\"\<\[Bullet] Modr\[EAcute]\>\"", ",", 
             RowBox[{"RGBColor", "[", 
              RowBox[{"0", ",", "0.4", ",", "0.8"}], "]"}], ",", "Bold"}], 
            "]"}], ",", "\"\< body: P\[OHat]vodn\[YAcute] bod P\>\""}], "]"}],
          ";", "\[IndentingNewLine]", 
         RowBox[{"Print", "[", 
          RowBox[{
           RowBox[{"Style", "[", 
            RowBox[{"\"\<\[Bullet] Tmavozelen\[EAcute]\>\"", ",", 
             RowBox[{"RGBColor", "[", 
              RowBox[{"0.2", ",", "0.7", ",", "0.3"}], "]"}], ",", "Bold"}], 
            "]"}], ",", 
           "\"\< body: Bod P' po prvej transform\[AAcute]cii (\>\"", ",", 
           RowBox[{"Style", "[", 
            RowBox[{"prvaTransformacia", ",", "Bold"}], "]"}], ",", 
           "\"\<)\>\""}], "]"}], ";", "\[IndentingNewLine]", 
         RowBox[{"Print", "[", 
          RowBox[{
           RowBox[{"Style", "[", 
            RowBox[{"\"\<\[Bullet] Oran\[ZHacek]ov\[EAcute]\>\"", ",", 
             RowBox[{"RGBColor", "[", 
              RowBox[{"1", ",", "0.6", ",", "0"}], "]"}], ",", "Bold"}], 
            "]"}], ",", 
           "\"\< body: Bod P'' po druhej transform\[AAcute]cii (\>\"", ",", 
           RowBox[{"Style", "[", 
            RowBox[{"druhaTransformacia", ",", "Bold"}], "]"}], ",", 
           "\"\<)\>\""}], "]"}], ";", "\[IndentingNewLine]", 
         RowBox[{"Print", "[", 
          RowBox[{
           RowBox[{"Style", "[", 
            RowBox[{"\"\<\[Bullet] \[CapitalCHacek]erven\[EAcute]\>\"", ",", 
             RowBox[{"RGBColor", "[", 
              RowBox[{"0.9", ",", "0.1", ",", "0.1"}], "]"}], ",", "Bold"}], 
            "]"}], ",", 
           "\"\< body: Bod P''' po tretej transform\[AAcute]cii (\>\"", ",", 
           RowBox[{"Style", "[", 
            RowBox[{"tretiaTransformacia", ",", "Bold"}], "]"}], ",", 
           "\"\<)\>\""}], "]"}], ";", "\[IndentingNewLine]", 
         RowBox[{"(*", 
          RowBox[{
          "Inform\[AAcute]cia", " ", "o", " ", "postupnosti", " ", 
           "transform\[AAcute]ci\[IAcute]"}], "*)"}], 
         RowBox[{"Print", "[", 
          RowBox[{"Style", "[", 
           RowBox[{
           "\"\<\\nPostupnos\[THacek] transform\[AAcute]ci\[IAcute]:\>\"", 
            ",", "Bold", ",", "16"}], "]"}], "]"}], ";", 
         "\[IndentingNewLine]", 
         RowBox[{"Print", "[", 
          RowBox[{"\"\<1. \>\"", ",", "prvaTransformacia"}], "]"}], ";", 
         "\[IndentingNewLine]", 
         RowBox[{"Print", "[", 
          RowBox[{"\"\<2. \>\"", ",", "druhaTransformacia"}], "]"}], ";", 
         "\[IndentingNewLine]", 
         RowBox[{"Print", "[", 
          RowBox[{"\"\<3. \>\"", ",", "tretiaTransformacia"}], "]"}], ";", 
         "\[IndentingNewLine]", 
         RowBox[{"(*", 
          RowBox[{
          "Vr\[AAcute]ti\[THacek]", " ", "inform\[AAcute]cie", " ", "o", " ", 
           "transform\[AAcute]ci\[AAcute]ch", " ", "pre", " ", 
           "pou\[ZHacek]itie", " ", "v", " ", "in\[YAcute]ch", " ", 
           "funkci\[AAcute]ch"}], "*)"}], 
         RowBox[{"{", 
          RowBox[{
          "pociatocnyBod", ",", "druhyBod", ",", "tretiBod", ",", 
           "finalnyBod", ",", "prvaTransformacia", ",", "druhaTransformacia", 
           ",", "tretiaTransformacia"}], "}"}]}], "]"}]}], "  ", 
      RowBox[{"(*", 
       RowBox[{"Ukon\[CHacek]enie", " ", "Quiet", " ", "bloku"}], "*)"}], 
      "]"}]}], ";"}], "\[IndentingNewLine]", "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Funkcia", " ", "pre", " ", "s\[UAcute]hrnn\[UAcute]", " ", 
     "vizualiz\[AAcute]ciu", " ", 
     RowBox[{"(", 
      RowBox[{"bez", " ", "dial\[OAcute]gu"}], ")"}], " ", "s", " ", 
     "o\[SHacek]etren\[IAcute]m", " ", "ch\[YAcute]b"}], "*)"}], "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodTrojitaTransformaciaSVysledkom", "[", "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "result", "}"}], ",", 
       RowBox[{"(*", 
        RowBox[{
        "Pou\[ZHacek]itie", " ", "Quiet", " ", "na", " ", 
         "potla\[CHacek]enie", " ", "chybov\[YAcute]ch", " ", 
         "spr\[AAcute]v"}], "*)"}], 
       RowBox[{
        RowBox[{"result", "=", 
         RowBox[{"Quiet", "[", 
          RowBox[{
           RowBox[{"BodTrojitaTransformacia", "[", "]"}], ",", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"Power", "::", "infy"}], ",", 
             RowBox[{"Infinity", "::", "indet"}]}], "}"}]}], "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Ak", " ", "funkcia", " ", "nebola", " ", 
          "zru\[SHacek]en\[AAcute]"}], "*)"}], 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{"result", "=!=", "Null"}], ",", 
          RowBox[{"Check", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{"Style", "[", 
               RowBox[{
               "\"\<\\nZ\[CapitalAAcute]VERE\[CapitalCHacek]N\[CapitalYAcute] \
PREH\:013dAD:\>\"", ",", "Bold", ",", "16"}], "]"}], "]"}], ";", 
             "\[IndentingNewLine]", 
             RowBox[{
             "Print", "[", 
              "\"\<Vykonali ste postupne tieto transform\[AAcute]cie:\>\"", 
              "]"}], ";", "\[IndentingNewLine]", 
             RowBox[{"Print", "[", 
              RowBox[{"\"\<1. \>\"", ",", 
               RowBox[{"result", "[", 
                RowBox[{"[", "5", "]"}], "]"}]}], "]"}], ";", 
             "\[IndentingNewLine]", 
             RowBox[{"Print", "[", 
              RowBox[{"\"\<2. \>\"", ",", 
               RowBox[{"result", "[", 
                RowBox[{"[", "6", "]"}], "]"}]}], "]"}], ";", 
             "\[IndentingNewLine]", 
             RowBox[{"Print", "[", 
              RowBox[{"\"\<3. \>\"", ",", 
               RowBox[{"result", "[", 
                RowBox[{"[", "7", "]"}], "]"}]}], "]"}], ";", 
             "\[IndentingNewLine]", 
             RowBox[{
             "Print", "[", "\"\<\\nV\[YAcute]sledn\[YAcute] bod P''':\>\"", 
              "]"}], ";", "\[IndentingNewLine]", 
             RowBox[{"Print", "[", 
              RowBox[{"MatrixForm", "[", 
               RowBox[{"result", "[", 
                RowBox[{"[", "4", "]"}], "]"}], "]"}], "]"}]}], ",", 
            RowBox[{"(*", 
             RowBox[{
             "Z\[AAcute]chytn\[YAcute]", " ", "blok", " ", "pre", " ", 
              "pr\[IAcute]pad", " ", "chyby", " ", "pri", " ", 
              "zobrazovan\[IAcute]"}], "*)"}], 
            RowBox[{"Print", "[", 
             RowBox[{"Style", "[", 
              RowBox[{
              "\"\<Nastala chyba pri zobrazovan\[IAcute] v\[YAcute]sledkov.\>\
\"", ",", "Red", ",", "Bold"}], "]"}], "]"}]}], "]"}]}], "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Vr\[AAcute]ti\[THacek]", " ", "Null", " ", "aby", " ", "sa", " ", 
          "nezobrazil", " ", 
          RowBox[{"Out", "[", "]"}]}], "*)"}], "Null"}]}], "]"}]}], ";"}], 
   "\[IndentingNewLine]", "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Na", " ", "konci", " ", "package", " ", "obnovi\[THacek]", " ", 
     "norm\[AAcute]lne", " ", "spr\[AAcute]vanie", " ", "varovn\[YAcute]ch", 
     " ", "spr\[AAcute]v"}], "*)"}], "\n", 
   RowBox[{
    RowBox[{"On", "[", 
     RowBox[{"Power", "::", "infy"}], "]"}], ";"}], "\n", 
   RowBox[{
    RowBox[{"On", "[", 
     RowBox[{"Infinity", "::", "indet"}], "]"}], ";"}], "\n", 
   "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{"End", "[", "]"}], ";"}], " ", 
   RowBox[{"(*", 
    RowBox[{"Uzavretie", " ", "Private", " ", "kontextu"}], "*)"}], "\n", 
   RowBox[{
    RowBox[{"EndPackage", "[", "]"}], ";"}], " ", 
   RowBox[{"(*", 
    RowBox[{"Uzavretie", " ", "BodHardBalik", " ", "package"}], 
    "*)"}]}]}]], "Input",
 CellChangeTimes->{{3.9527926019094543`*^9, 3.952792601911684*^9}, 
   3.95287781282687*^9},
 CellLabel->
  "In[2173]:=",ExpressionUUID->"e586ecac-2bb2-4e4a-8f52-3230ebac1ad0"]
},
WindowSize->{808, 731},
WindowMargins->{{4, Automatic}, {Automatic, 4}},
FrontEndVersion->"14.1 for Mac OS X ARM (64-bit) (July 16, 2024)",
StyleDefinitions->"Default.nb",
ExpressionUUID->"cfda3039-547f-4ab2-b6bc-252a6d11c2f2"
]
(* End of Notebook Content *)

(* Internal cache information *)
(*CellTagsOutline
CellTagsIndex->{}
*)
(*CellTagsIndex
CellTagsIndex->{}
*)
(*NotebookFileOutline
Notebook[{
Cell[554, 20, 91715, 2160, 7609, "Input",ExpressionUUID->"e586ecac-2bb2-4e4a-8f52-3230ebac1ad0"]
}
]
*)

