(* Content-type: application/vnd.wolfram.mathematica *)

(*** Wolfram Notebook File ***)
(* http://www.wolfram.com/nb *)

(* CreatedBy='Wolfram 14.1' *)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[       154,          7]
NotebookDataLength[    255679,       5407]
NotebookOptionsPosition[    255326,       5393]
NotebookOutlinePosition[    255717,       5409]
CellTagsIndexPosition[    255674,       5406]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{
Cell[BoxData[
 RowBox[{"\n", "\n", 
  RowBox[{
   RowBox[{
    RowBox[{"BeginPackage", "[", "\"\<BodHardBalik`\>\"", "]"}], ";"}], "\n", 
   "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodTrojitaTransformacia", "::", "usage"}], " ", "=", " ", "\n", 
     "    ", "\"\<BodTrojitaTransformacia[] umo\[ZHacek]n\[IAcute] postupn\
\[YAcute] v\[YAcute]ber a aplik\[AAcute]ciu troch transform\[AAcute]ci\
\[IAcute].\>\""}], ";"}], "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodTrojitaTransformaciaSVysledkom", "::", "usage"}], " ", "=", 
     " ", "\n", "    ", 
     "\"\<BodTrojitaTransformaciaSVysledkom[] umo\[ZHacek]n\[IAcute] postupn\
\[YAcute] v\[YAcute]ber a aplik\[AAcute]ciu troch transform\[AAcute]ci\
\[IAcute] a zobraz\[IAcute] s\[UAcute]hrnn\[YAcute] v\[YAcute]sledok.\>\""}], 
    ";"}], "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodGeneruj", "::", "usage"}], " ", "=", " ", "\n", "    ", 
     "\"\<BodGeneruj[] generuje n\[AAcute]hodn\[YAcute] bod s \
vhodn\[YAcute]mi vlastnos\[THacek]ami.\>\""}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodTrojitaTransformacia", "::", "infinityerr"}], " ", "=", " ", 
     "\n", "    ", 
     "\"\<Vyskytla sa chyba: v\[YAcute]po\[CHacek]et viedol k \
nekone\[CHacek]n\[EAcute]mu alebo neur\[CHacek]it\[EAcute]mu \
v\[YAcute]sledku. Pou\[ZHacek]ije sa preddefinovan\[YAcute] bod.\>\""}], 
    ";"}], "\n", "\n", 
   RowBox[{
    RowBox[{"Begin", "[", "\"\<`Private`\>\"", "]"}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{"Off", "[", 
     RowBox[{"Power", "::", "infy"}], "]"}], ";"}], "\n", 
   RowBox[{
    RowBox[{"Off", "[", 
     RowBox[{"Infinity", "::", "indet"}], "]"}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{"Needs", "[", "\"\<BodHardBalik`Transforms`Posun`\>\"", "]"}], 
    ";"}], "\n", 
   RowBox[{
    RowBox[{"Needs", "[", "\"\<BodHardBalik`Transforms`Rotacia`\>\"", "]"}], 
    ";"}], "\n", 
   RowBox[{
    RowBox[{"Needs", "[", "\"\<BodHardBalik`Transforms`Symetria`\>\"", "]"}], 
    ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{"mildGreen", " ", "=", " ", 
     RowBox[{"RGBColor", "[", 
      RowBox[{"0.2", ",", " ", "0.6", ",", " ", "0.2"}], "]"}]}], ";"}], "\n",
    "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"GenerateInitialPoint", "[", "]"}], " ", ":=", " ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "\n", "    ", 
        RowBox[{"point", ",", " ", "\n", "    ", 
         RowBox[{"count", " ", "=", " ", "0"}], ",", " ", "\n", "    ", 
         RowBox[{"defaultPoint", " ", "=", " ", 
          RowBox[{"{", 
           RowBox[{"2", ",", " ", "1"}], "}"}]}], ",", " ", "\n", "    ", 
         "result", ",", "\n", "    ", 
         RowBox[{"previousPoints", " ", "=", " ", 
          RowBox[{"{", "}"}]}], ",", "\n", "    ", "randomSeed", ",", "\n", 
         "    ", "generationType", ",", "\n", "    ", 
         RowBox[{"minDistance", " ", "=", " ", "2.5"}]}], "\n", "  ", "}"}], 
       ",", "\n", "  ", "\n", "  ", "\n", "  ", 
       RowBox[{
        RowBox[{"randomSeed", " ", "=", " ", 
         RowBox[{"Hash", "[", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{"AbsoluteTime", "[", "]"}], ",", " ", "$TimeZone", ",", 
            " ", "$ProcessID", ",", " ", "\n", "                     ", 
            RowBox[{"RandomInteger", "[", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"-", 
                RowBox[{"10", "^", "7"}]}], ",", " ", 
               RowBox[{"10", "^", "7"}]}], "}"}], "]"}], ",", " ", 
            RowBox[{
            "StringJoin", " ", "@@", " ", "\n", "                     ", 
             RowBox[{"ToString", " ", "/@", " ", 
              RowBox[{"RandomInteger", "[", 
               RowBox[{
                RowBox[{"{", 
                 RowBox[{"0", ",", " ", "9"}], "}"}], ",", " ", "10"}], 
               "]"}]}]}]}], "}"}], "]"}]}], ";", "\n", "  ", 
        RowBox[{"SeedRandom", "[", "randomSeed", "]"}], ";", "\n", "  ", "\n",
         "  ", "\n", "  ", 
        RowBox[{"Do", "[", 
         RowBox[{
          RowBox[{"RandomInteger", "[", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"-", "1000"}], ",", " ", "1000"}], "}"}], "]"}], ",", 
          " ", 
          RowBox[{"{", 
           RowBox[{"RandomInteger", "[", 
            RowBox[{"{", 
             RowBox[{"10", ",", " ", "30"}], "}"}], "]"}], "}"}]}], "]"}], 
        ";", "\n", "  ", "\n", "  ", "\n", "  ", 
        RowBox[{"generationType", " ", "=", " ", 
         RowBox[{"RandomChoice", "[", 
          RowBox[{
           RowBox[{"{", 
            RowBox[{"0.82", ",", " ", "0.18"}], "}"}], " ", "->", " ", 
           RowBox[{"{", 
            RowBox[{"1", ",", " ", "2"}], "}"}]}], "]"}]}], ";", "\n", "  ", 
        "\n", "  ", "\n", "  ", 
        RowBox[{"result", " ", "=", " ", 
         RowBox[{"Check", "[", "\n", "    ", 
          RowBox[{
           RowBox[{"Block", "[", 
            RowBox[{
             RowBox[{"{", 
              RowBox[{"$MessagePreprint", " ", "=", " ", 
               RowBox[{
                RowBox[{"(", 
                 RowBox[{
                  RowBox[{"Message", "[", 
                   RowBox[{"BodTrojitaTransformacia", "::", "infinityerr"}], 
                   "]"}], ";", " ", "#"}], ")"}], "&"}]}], "}"}], ",", "\n", 
             "      ", 
             RowBox[{"While", "[", 
              RowBox[{"True", ",", "\n", "        ", 
               RowBox[{
                RowBox[{"Switch", "[", 
                 RowBox[{
                 "generationType", ",", "\n", "          ", "\n", 
                  "          ", "1", ",", " ", "\n", "          ", 
                  RowBox[{"Module", "[", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{"range", ",", " ", "nonZeroRandom"}], "}"}], ",", 
                    "\n", "            ", "\n", "            ", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"nonZeroRandom", "[", 
                    RowBox[{"min_", ",", " ", "max_"}], "]"}], " ", ":=", " ", 
                    RowBox[{"Module", "[", 
                    RowBox[{
                    RowBox[{"{", "val", "}"}], ",", "\n", "              ", 
                    RowBox[{
                    RowBox[{"val", " ", "=", " ", 
                    RowBox[{"RandomInteger", "[", 
                    RowBox[{"{", 
                    RowBox[{"min", ",", " ", "max"}], "}"}], "]"}]}], ";", 
                    "\n", "              ", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{"val", " ", "==", " ", "0"}], ",", " ", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"RandomReal", "[", "]"}], " ", "<", " ", "0.5"}], 
                    ",", " ", 
                    RowBox[{"-", "1"}], ",", " ", "1"}], "]"}], ",", " ", 
                    "val"}], "]"}]}]}], "\n", "            ", "]"}]}], ";", 
                    "\n", "            ", "\n", "            ", "\n", 
                    "            ", 
                    RowBox[{"range", " ", "=", " ", 
                    RowBox[{"RandomInteger", "[", 
                    RowBox[{"{", 
                    RowBox[{"5", ",", " ", "8"}], "}"}], "]"}]}], ";", "\n", 
                    "            ", "\n", "            ", "\n", 
                    "            ", 
                    RowBox[{"point", " ", "=", " ", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"nonZeroRandom", "[", 
                    RowBox[{
                    RowBox[{"-", "range"}], ",", " ", "range"}], "]"}], ",", 
                    " ", 
                    RowBox[{"nonZeroRandom", "[", 
                    RowBox[{
                    RowBox[{"-", "range"}], ",", " ", "range"}], "]"}]}], 
                    "}"}]}], ";", "\n", "            ", "\n", "            ", 
                    "\n", "            ", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{
                    RowBox[{"point", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], " ", "==", " ", "0"}], 
                    " ", "&&", " ", 
                    RowBox[{
                    RowBox[{"point", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], " ", "==", " ", "0"}]}], 
                    ",", " ", 
                    RowBox[{"point", " ", "=", " ", 
                    RowBox[{"{", 
                    RowBox[{"1", ",", " ", 
                    RowBox[{"nonZeroRandom", "[", 
                    RowBox[{
                    RowBox[{"-", "range"}], ",", " ", "range"}], "]"}]}], 
                    "}"}]}]}], "]"}], ";"}]}], "\n", "          ", "]"}], ",",
                   "\n", "          ", "\n", "          ", "\n", "          ",
                   "2", ",", "\n", "          ", 
                  RowBox[{"Module", "[", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{
                    "numX", ",", " ", "numY", ",", " ", "denX", ",", " ", 
                    "denY", ",", " ", "nonZeroNum"}], "}"}], ",", "\n", 
                    "            ", "\n", "            ", "\n", 
                    "            ", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"nonZeroNum", "[", 
                    RowBox[{"min_", ",", " ", "max_"}], "]"}], " ", ":=", " ", 
                    RowBox[{"Module", "[", 
                    RowBox[{
                    RowBox[{"{", "val", "}"}], ",", "\n", "              ", 
                    RowBox[{
                    RowBox[{"val", " ", "=", " ", 
                    RowBox[{"RandomInteger", "[", 
                    RowBox[{"{", 
                    RowBox[{"min", ",", " ", "max"}], "}"}], "]"}]}], ";", 
                    "\n", "              ", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{"val", " ", "==", " ", "0"}], ",", " ", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"RandomReal", "[", "]"}], " ", "<", " ", "0.5"}], 
                    ",", " ", "1", ",", " ", 
                    RowBox[{"-", "1"}]}], "]"}], ",", " ", "val"}], "]"}]}]}],
                     "\n", "            ", "]"}]}], ";", "\n", "            ",
                     "\n", "            ", "\n", "            ", 
                    RowBox[{"numX", " ", "=", " ", 
                    RowBox[{"nonZeroNum", "[", 
                    RowBox[{
                    RowBox[{"-", "8"}], ",", " ", "8"}], "]"}]}], ";", "\n", 
                    "            ", 
                    RowBox[{"numY", " ", "=", " ", 
                    RowBox[{"nonZeroNum", "[", 
                    RowBox[{
                    RowBox[{"-", "8"}], ",", " ", "8"}], "]"}]}], ";", "\n", 
                    "            ", "\n", "            ", "\n", 
                    "            ", 
                    RowBox[{"denX", " ", "=", " ", 
                    RowBox[{"RandomChoice", "[", 
                    RowBox[{"{", 
                    RowBox[{"2", ",", " ", "3", ",", " ", "4"}], "}"}], 
                    "]"}]}], ";", "\n", "            ", 
                    RowBox[{"denY", " ", "=", " ", 
                    RowBox[{"RandomChoice", "[", 
                    RowBox[{"{", 
                    RowBox[{"2", ",", " ", "3", ",", " ", "4"}], "}"}], 
                    "]"}]}], ";", "\n", "            ", "\n", "            ", 
                    "\n", "            ", 
                    RowBox[{"point", " ", "=", " ", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"Rationalize", "[", 
                    RowBox[{"numX", "/", "denX"}], "]"}], ",", " ", 
                    RowBox[{"Rationalize", "[", 
                    RowBox[{"numY", "/", "denY"}], "]"}]}], "}"}]}], ";", 
                    "\n", "            ", "\n", "            ", "\n", 
                    "            ", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"point", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], " ", "==", " ", "0"}], 
                    ",", " ", 
                    RowBox[{
                    RowBox[{"point", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], " ", "=", " ", 
                    RowBox[{"Rationalize", "[", 
                    RowBox[{"1", "/", "denX"}], "]"}]}]}], "]"}], ";", "\n", 
                    "            ", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"point", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], " ", "==", " ", "0"}], 
                    ",", " ", 
                    RowBox[{
                    RowBox[{"point", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], " ", "=", " ", 
                    RowBox[{"Rationalize", "[", 
                    RowBox[{"1", "/", "denY"}], "]"}]}]}], "]"}], ";"}]}], 
                   "\n", "          ", "]"}]}], "\n", "        ", "]"}], ";", 
                "\n", "        ", "\n", "        ", 
                RowBox[{"If", "[", 
                 RowBox[{
                  RowBox[{"ContainsAny", "[", 
                   RowBox[{
                    RowBox[{"{", "point", "}"}], ",", " ", "previousPoints"}],
                    "]"}], ",", "\n", "          ", "\n", "          ", 
                  RowBox[{
                   RowBox[{"count", "++"}], ";", "\n", "          ", 
                   RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"Mod", "[", 
                    RowBox[{"count", ",", " ", "5"}], "]"}], " ", "==", " ", 
                    "0"}], ",", " ", "\n", "            ", 
                    RowBox[{"generationType", " ", "=", " ", 
                    RowBox[{"RandomChoice", "[", 
                    RowBox[{
                    RowBox[{"{", 
                    RowBox[{"0.8", ",", " ", "0.18", ",", " ", "0.02"}], 
                    "}"}], " ", "->", " ", 
                    RowBox[{"{", 
                    RowBox[{"1", ",", " ", "2", ",", " ", "3"}], "}"}]}], 
                    "]"}]}]}], "\n", "          ", "]"}], ";", "\n", 
                   "          ", 
                   RowBox[{"Continue", "[", "]"}]}]}], "\n", "        ", 
                 "]"}], ";", "\n", "        ", "\n", "        ", "\n", 
                "        ", 
                RowBox[{"If", "[", 
                 RowBox[{
                  RowBox[{
                   RowBox[{"Max", "[", 
                    RowBox[{"Abs", "[", 
                    RowBox[{"Flatten", "[", 
                    RowBox[{"{", "point", "}"}], "]"}], "]"}], "]"}], " ", "<=",
                    " ", "10"}], ",", "\n", "          ", "\n", "          ", 
                  
                  RowBox[{
                   RowBox[{"AppendTo", "[", 
                    RowBox[{"previousPoints", ",", " ", "point"}], "]"}], ";",
                    "\n", "          ", 
                   RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"Length", "[", "previousPoints", "]"}], " ", ">", 
                    " ", "20"}], ",", " ", 
                    RowBox[{"previousPoints", " ", "=", " ", 
                    RowBox[{"previousPoints", "[", 
                    RowBox[{"[", 
                    RowBox[{
                    RowBox[{"-", "20"}], ";;"}], "]"}], "]"}]}]}], "]"}], ";",
                    "\n", "          ", "\n", "          ", "\n", 
                   "          ", 
                   RowBox[{"Return", "[", "point", "]"}]}]}], "\n", 
                 "        ", "]"}], ";", "\n", "        ", "\n", "        ", 
                "\n", "        ", 
                RowBox[{"If", "[", 
                 RowBox[{
                  RowBox[{
                   RowBox[{
                    RowBox[{"Mod", "[", 
                    RowBox[{"count", ",", " ", "5"}], "]"}], " ", "==", " ", 
                    "0"}], " ", "&&", " ", 
                   RowBox[{"count", " ", ">", " ", "0"}]}], ",", " ", "\n", 
                  "          ", 
                  RowBox[{"generationType", " ", "=", " ", 
                   RowBox[{"RandomChoice", "[", 
                    RowBox[{
                    RowBox[{"{", 
                    RowBox[{"0.82", ",", " ", "0.18"}], "}"}], " ", "->", " ", 
                    RowBox[{"{", 
                    RowBox[{"1", ",", " ", "2"}], "}"}]}], "]"}]}]}], "\n", 
                 "        ", "]"}], ";", "\n", "        ", "\n", "        ", 
                "\n", "        ", 
                RowBox[{"If", "[", 
                 RowBox[{
                  RowBox[{"count", " ", ">", " ", "50"}], ",", " ", "\n", 
                  "          ", 
                  RowBox[{"Module", "[", 
                   RowBox[{
                    RowBox[{"{", "simplePoints", "}"}], ",", "\n", 
                    "            ", 
                    RowBox[{
                    RowBox[{"simplePoints", " ", "=", " ", 
                    RowBox[{"{", "\n", "              ", 
                    RowBox[{
                    RowBox[{"{", 
                    RowBox[{"3", ",", " ", "1"}], "}"}], ",", "            ", 
                    "\n", "              ", 
                    RowBox[{"{", 
                    RowBox[{"5", ",", " ", "1"}], "}"}], ",", "            ", 
                    "\n", "              ", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"-", "3"}], ",", " ", 
                    RowBox[{"-", "2"}]}], "}"}], ",", "          ", "\n", 
                    "              ", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"-", "4"}], ",", " ", "1"}], "}"}], ",", 
                    "           ", "\n", "              ", 
                    RowBox[{"{", 
                    RowBox[{"1", ",", " ", "1"}], "}"}], ",", "            ", 
                    "\n", "              ", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"-", "3"}], ",", " ", "1"}], "}"}], ",", 
                    "           ", "\n", "              ", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"-", "2"}], ",", " ", 
                    RowBox[{"-", "2"}]}], "}"}], ",", "          ", "\n", 
                    "              ", 
                    RowBox[{"{", 
                    RowBox[{"1", ",", " ", "2"}], "}"}], ",", "            ", 
                    "\n", "              ", 
                    RowBox[{"{", 
                    RowBox[{"1", ",", " ", "1"}], "}"}], ",", "            ", 
                    "\n", "              ", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"-", "3"}], ",", " ", 
                    RowBox[{"-", "3"}]}], "}"}], ",", "          ", "\n", 
                    "              ", "\n", "              ", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"-", "2"}], ",", " ", "1"}], "}"}], ",", 
                    "            ", "\n", "              ", 
                    RowBox[{"{", 
                    RowBox[{"1", ",", " ", 
                    RowBox[{"Rationalize", "[", 
                    RowBox[{"1", "/", "2"}], "]"}]}], "}"}], ",", " ", "\n", 
                    "              ", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"-", "1"}], ",", " ", 
                    RowBox[{"-", "1"}]}], "}"}], ",", "          ", "\n", 
                    "              ", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"-", "2"}], ",", " ", "1"}], "}"}]}], 
                    "            ", "\n", "            ", "}"}]}], ";", "\n", 
                    "            ", "\n", "            ", "\n", 
                    "            ", 
                    RowBox[{"Return", "[", 
                    RowBox[{"RandomChoice", "[", "simplePoints", "]"}], 
                    "]"}]}]}], "\n", "          ", "]"}]}], "\n", "        ", 
                 "]"}], ";", "\n", "        ", 
                RowBox[{"count", "++"}]}]}], "\n", "      ", "]"}]}], "\n", 
            "    ", "]"}], ",", "\n", "    ", "\n", "    ", 
           RowBox[{"{", 
            RowBox[{"3", ",", " ", "0"}], "}"}]}], "\n", "  ", "]"}]}], ";", 
        "\n", "  ", "\n", "  ", "\n", "  ", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{
            RowBox[{"Head", "[", "result", "]"}], " ", "===", " ", "List"}], 
           " ", "&&", " ", 
           RowBox[{
            RowBox[{"Length", "[", "result", "]"}], " ", "===", " ", "2"}], 
           " ", "&&", " ", 
           RowBox[{"AllTrue", "[", 
            RowBox[{"result", ",", " ", "NumberQ"}], "]"}]}], ",", " ", "\n", 
          "    ", "result", ",", " ", "\n", "    ", 
          RowBox[{"{", 
           RowBox[{"3", ",", " ", "1"}], "}"}]}], "  ", "\n", "  ", "]"}]}]}],
       "\n", "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodGeneruj", "[", "]"}], " ", ":=", " ", 
     RowBox[{"GenerateInitialPoint", "[", "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"DisplayPointProperties", "[", 
      RowBox[{"point_", ",", " ", "style_"}], "]"}], " ", ":=", " ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "}"}], ",", "\n", "    ", 
       RowBox[{
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{"\"\<Vlastnosti bodu:\>\"", ",", " ", "Bold"}], "]"}], 
         "]"}], ";", "\n", "    ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<S\[UAcute]radnice: \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{"point", ",", " ", "style"}], "]"}]}], "]"}], ";"}]}], 
      "\n", "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"FormatPointExpression", "[", "expr_", "]"}], " ", ":=", "\n", 
     "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"expandedExpr", ",", " ", "simplifiedExpr"}], "}"}], ",", 
       "\n", "        ", "\n", "        ", 
       RowBox[{
        RowBox[{"expandedExpr", " ", "=", " ", 
         RowBox[{"Expand", "[", "expr", "]"}]}], ";", "\n", "        ", "\n", 
        "        ", "\n", "        ", 
        RowBox[{"simplifiedExpr", " ", "=", " ", 
         RowBox[{"Simplify", "[", "expandedExpr", "]"}]}], ";", "\n", 
        "        ", "\n", "        ", "\n", "        ", "simplifiedExpr"}]}], 
      "\n", "    ", "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"ProcessPoint", "[", "point_", "]"}], " ", ":=", " ", "\n", 
     "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "}"}], ",", "\n", "        ", "\n", "        ", 
       RowBox[{"Map", "[", 
        RowBox[{"FormatPointExpression", ",", " ", "point"}], "]"}]}], "\n", 
      "    ", "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"ExpandNestedExpressions", "[", "expr_", "]"}], " ", ":=", " ", 
     "\n", "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"result", " ", "=", " ", "expr"}], "}"}], ",", "\n", 
       "        ", "\n", "        ", 
       RowBox[{
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{
            RowBox[{"Head", "[", "expr", "]"}], " ", "===", " ", "Times"}], 
           " ", "&&", " ", 
           RowBox[{
            RowBox[{"Length", "[", "expr", "]"}], " ", ">=", " ", "2"}], " ", 
           "&&", " ", "\n", "           ", 
           RowBox[{"(", 
            RowBox[{
             RowBox[{"MatchQ", "[", 
              RowBox[{
               RowBox[{"expr", "[", 
                RowBox[{"[", "1", "]"}], "]"}], ",", " ", "_Rational"}], 
              "]"}], " ", "||", " ", 
             RowBox[{"MatchQ", "[", 
              RowBox[{
               RowBox[{"expr", "[", 
                RowBox[{"[", "1", "]"}], "]"}], ",", " ", "_Integer"}], 
              "]"}]}], ")"}], " ", "&&", "\n", "           ", 
           RowBox[{"MatchQ", "[", 
            RowBox[{
             RowBox[{"expr", "[", 
              RowBox[{"[", "2", "]"}], "]"}], ",", " ", "_Plus"}], "]"}]}], 
          ",", "\n", "           ", "\n", "           ", 
          RowBox[{
           RowBox[{"result", " ", "=", " ", 
            RowBox[{"Expand", "[", "expr", "]"}]}], ";"}]}], "\n", "        ",
          "]"}], ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"Head", "[", "result", "]"}], " ", "===", " ", "Plus"}], 
          ",", " ", "\n", "            ", 
          RowBox[{"result", " ", "=", " ", 
           RowBox[{"Plus", " ", "@@", " ", 
            RowBox[{"Map", "[", 
             RowBox[{"ExpandNestedExpressions", ",", " ", 
              RowBox[{"List", " ", "@@", " ", "result"}]}], "]"}]}]}], ",", 
          "\n", "            ", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"Head", "[", "result", "]"}], " ", "===", " ", "Times"}],
             ",", "\n", "                ", 
            RowBox[{"result", " ", "=", " ", 
             RowBox[{"Times", " ", "@@", " ", 
              RowBox[{"Map", "[", 
               RowBox[{"ExpandNestedExpressions", ",", " ", 
                RowBox[{"List", " ", "@@", " ", "result"}]}], "]"}]}]}]}], 
           "\n", "            ", "]"}]}], "\n", "        ", "]"}], ";", "\n", 
        "        ", "\n", "        ", "result"}]}], "\n", "    ", "]"}]}], 
    ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"FullExpressionProcessor", "[", "expr_", "]"}], " ", ":=", "\n", 
     "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"step1", ",", " ", "step2"}], "}"}], ",", "\n", "        ", 
       "\n", "        ", 
       RowBox[{
        RowBox[{"step1", " ", "=", " ", 
         RowBox[{"Expand", "[", "expr", "]"}]}], ";", "\n", "        ", "\n", 
        "        ", "\n", "        ", 
        RowBox[{"step2", " ", "=", " ", 
         RowBox[{"ExpandNestedExpressions", "[", "step1", "]"}]}], ";", "\n", 
        "        ", "\n", "        ", "\n", "        ", "step2"}]}], "\n", 
      "    ", "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"ProcessPointComplete", "[", "point_", "]"}], " ", ":=", "\n", 
     "    ", 
     RowBox[{"Map", "[", 
      RowBox[{"FullExpressionProcessor", ",", " ", "point"}], "]"}]}], ";"}], 
   "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"GetTransformationMatrix", "[", 
      RowBox[{"transformType_", ",", " ", "params_"}], "]"}], " ", ":=", " ", 
     "\n", "    ", 
     RowBox[{"Switch", "[", 
      RowBox[{
      "transformType", ",", "\n", "        ", "\"\<Posun\>\"", ",", " ", "\n",
        "            ", 
       RowBox[{"Module", "[", 
        RowBox[{
         RowBox[{"{", 
          RowBox[{
           RowBox[{"dx", " ", "=", " ", 
            RowBox[{"params", "[", 
             RowBox[{"[", "1", "]"}], "]"}]}], ",", " ", 
           RowBox[{"dy", " ", "=", " ", 
            RowBox[{"params", "[", 
             RowBox[{"[", "2", "]"}], "]"}]}]}], "}"}], ",", "\n", 
         "                ", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"{", 
            RowBox[{"1", ",", " ", "0", ",", " ", "dx"}], "}"}], ",", " ", 
           RowBox[{"{", 
            RowBox[{"0", ",", " ", "1", ",", " ", "dy"}], "}"}], ",", " ", 
           RowBox[{"{", 
            RowBox[{"0", ",", " ", "0", ",", " ", "1"}], "}"}]}], "}"}]}], 
        "\n", "            ", "]"}], ",", "\n", "        ", "\n", "        ", 
       "\"\<Rot\[AAcute]cia\>\"", ",", " ", "\n", "            ", 
       RowBox[{"Module", "[", 
        RowBox[{
         RowBox[{"{", 
          RowBox[{
           RowBox[{"angle", " ", "=", " ", 
            RowBox[{"params", " ", "*", " ", "Degree"}]}], ",", " ", 
           RowBox[{"c", " ", "=", " ", 
            RowBox[{"Cos", "[", 
             RowBox[{"params", " ", "*", " ", "Degree"}], "]"}]}], ",", " ", 
           RowBox[{"s", " ", "=", " ", 
            RowBox[{"Sin", "[", 
             RowBox[{"params", " ", "*", " ", "Degree"}], "]"}]}]}], "}"}], 
         ",", "\n", "                ", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"{", 
            RowBox[{"c", ",", " ", 
             RowBox[{"-", "s"}], ",", " ", "0"}], "}"}], ",", " ", 
           RowBox[{"{", 
            RowBox[{"s", ",", " ", "c", ",", " ", "0"}], "}"}], ",", " ", 
           RowBox[{"{", 
            RowBox[{"0", ",", " ", "0", ",", " ", "1"}], "}"}]}], "}"}]}], 
        "\n", "            ", "]"}], ",", "\n", "        ", "\n", "        ", 
       "\"\<Symetria\>\"", ",", " ", "\n", "            ", "\n", 
       "            ", 
       RowBox[{"Switch", "[", 
        RowBox[{
        "params", ",", "\n", "                ", "\"\<os x\>\"", ",", " ", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"{", 
            RowBox[{"1", ",", " ", "0", ",", " ", "0"}], "}"}], ",", " ", 
           RowBox[{"{", 
            RowBox[{"0", ",", " ", 
             RowBox[{"-", "1"}], ",", " ", "0"}], "}"}], ",", " ", 
           RowBox[{"{", 
            RowBox[{"0", ",", " ", "0", ",", " ", "1"}], "}"}]}], "}"}], ",", 
         "\n", "                ", "\"\<os y\>\"", ",", " ", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"{", 
            RowBox[{
             RowBox[{"-", "1"}], ",", " ", "0", ",", " ", "0"}], "}"}], ",", 
           " ", 
           RowBox[{"{", 
            RowBox[{"0", ",", " ", "1", ",", " ", "0"}], "}"}], ",", " ", 
           RowBox[{"{", 
            RowBox[{"0", ",", " ", "0", ",", " ", "1"}], "}"}]}], "}"}], ",", 
         "\n", "                ", "\"\<priamka y=x\>\"", ",", " ", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"{", 
            RowBox[{"0", ",", " ", "1", ",", " ", "0"}], "}"}], ",", " ", 
           RowBox[{"{", 
            RowBox[{"1", ",", " ", "0", ",", " ", "0"}], "}"}], ",", " ", 
           RowBox[{"{", 
            RowBox[{"0", ",", " ", "0", ",", " ", "1"}], "}"}]}], "}"}], ",", 
         "\n", "                ", "\"\<priamka y=-x\>\"", ",", " ", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"{", 
            RowBox[{"0", ",", " ", 
             RowBox[{"-", "1"}], ",", " ", "0"}], "}"}], ",", " ", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"-", "1"}], ",", " ", "0", ",", " ", "0"}], "}"}], ",", 
           " ", 
           RowBox[{"{", 
            RowBox[{"0", ",", " ", "0", ",", " ", "1"}], "}"}]}], "}"}], ",", 
         "\n", "                ", "_", ",", " ", "\n", "                ", 
         RowBox[{"If", "[", 
          RowBox[{
           RowBox[{
            RowBox[{"ListQ", "[", "params", "]"}], " ", "&&", " ", 
            RowBox[{
             RowBox[{"Length", "[", "params", "]"}], " ", "==", " ", "3"}]}], 
           ",", "\n", "                    ", "\n", "                    ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
             "ValueQ", "[", "BodHardBalik`Transforms`Symetria`SymetriaMatrix",
               "]"}], ",", "\n", "                        ", "\n", 
             "                        ", 
             "BodHardBalik`Transforms`Symetria`SymetriaMatrix", ",", "\n", 
             "                        ", "\n", "                        ", 
             "\n", "                        ", 
             RowBox[{"Module", "[", 
              RowBox[{
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"a", " ", "=", " ", 
                  RowBox[{"params", "[", 
                   RowBox[{"[", "1", "]"}], "]"}]}], ",", " ", 
                 RowBox[{"b", " ", "=", " ", 
                  RowBox[{"params", "[", 
                   RowBox[{"[", "2", "]"}], "]"}]}], ",", " ", 
                 RowBox[{"c", " ", "=", " ", 
                  RowBox[{"params", "[", 
                   RowBox[{"[", "3", "]"}], "]"}]}], ",", " ", "factor"}], 
                "}"}], ",", "\n", "                            ", 
               RowBox[{
                RowBox[{"factor", " ", "=", " ", 
                 RowBox[{
                  RowBox[{"a", "^", "2"}], " ", "+", " ", 
                  RowBox[{"b", "^", "2"}]}]}], ";", "\n", 
                "                            ", 
                RowBox[{"If", "[", 
                 RowBox[{
                  RowBox[{"factor", " ", "==", " ", "0"}], ",", " ", "\n", 
                  "                                ", "\n", 
                  "                                ", 
                  RowBox[{"{", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{"1", ",", " ", "0", ",", " ", "0"}], "}"}], ",", 
                    " ", 
                    RowBox[{"{", 
                    RowBox[{"0", ",", " ", 
                    RowBox[{"-", "1"}], ",", " ", "0"}], "}"}], ",", " ", 
                    RowBox[{"{", 
                    RowBox[{"0", ",", " ", "0", ",", " ", "1"}], "}"}]}], 
                   "}"}], ",", "\n", "                                ", "\n",
                   "                                ", 
                  RowBox[{"{", "\n", "                                    ", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"(", 
                    RowBox[{
                    RowBox[{"b", "^", "2"}], " ", "-", " ", 
                    RowBox[{"a", "^", "2"}]}], ")"}], "/", "factor"}], ",", 
                    " ", 
                    RowBox[{
                    RowBox[{"-", "2"}], "*", "a", "*", 
                    RowBox[{"b", "/", "factor"}]}], ",", " ", 
                    RowBox[{
                    RowBox[{"-", "2"}], "*", "a", "*", 
                    RowBox[{"c", "/", "factor"}]}]}], "}"}], ",", "\n", 
                    "                                    ", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"-", "2"}], "*", "a", "*", 
                    RowBox[{"b", "/", "factor"}]}], ",", " ", 
                    RowBox[{
                    RowBox[{"(", 
                    RowBox[{
                    RowBox[{"a", "^", "2"}], " ", "-", " ", 
                    RowBox[{"b", "^", "2"}]}], ")"}], "/", "factor"}], ",", 
                    " ", 
                    RowBox[{
                    RowBox[{"-", "2"}], "*", "b", "*", 
                    RowBox[{"c", "/", "factor"}]}]}], "}"}], ",", "\n", 
                    "                                    ", 
                    RowBox[{"{", 
                    RowBox[{"0", ",", " ", "0", ",", " ", "1"}], "}"}]}], 
                   "\n", "                                ", "}"}]}], "\n", 
                 "                            ", "]"}]}]}], "\n", 
              "                        ", "]"}]}], "\n", 
            "                    ", "]"}], ",", "\n", "                    ", 
           "\n", "                    ", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"{", 
              RowBox[{"1", ",", " ", "0", ",", " ", "0"}], "}"}], ",", " ", 
             RowBox[{"{", 
              RowBox[{"0", ",", " ", "1", ",", " ", "0"}], "}"}], ",", " ", 
             RowBox[{"{", 
              RowBox[{"0", ",", " ", "0", ",", " ", "1"}], "}"}]}], "}"}]}], 
          "\n", "                ", "]"}]}], "\n", "            ", "]"}]}], 
      "\n", "    ", "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"ApplyTransformationMatrix", "[", 
      RowBox[{"matrix_", ",", " ", "point_"}], "]"}], " ", ":=", "\n", "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"homogeneousPoint", ",", " ", "result"}], "}"}], ",", "\n", 
       "        ", "\n", "        ", 
       RowBox[{
        RowBox[{"homogeneousPoint", " ", "=", " ", 
         RowBox[{"Append", "[", 
          RowBox[{"point", ",", " ", "1"}], "]"}]}], ";", "\n", "        ", 
        "\n", "        ", "\n", "        ", 
        RowBox[{"result", " ", "=", " ", 
         RowBox[{"matrix", " ", ".", " ", "homogeneousPoint"}]}], ";", "\n", 
        "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Take", "[", 
         RowBox[{"result", ",", " ", "2"}], "]"}]}]}], "\n", "    ", "]"}]}], 
    ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"CompositeTransformationMatrix", "[", "matrices_", "]"}], " ", ":=",
      "\n", "    ", 
     RowBox[{"Fold", "[", 
      RowBox[{"Dot", ",", " ", 
       RowBox[{"IdentityMatrix", "[", "3", "]"}], ",", " ", "matrices"}], 
      "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"DisplayCompositeMatrixCalculation", "[", 
      RowBox[{"matrices_", ",", " ", "transformNames_"}], "]"}], " ", ":=", 
     "\n", "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
         RowBox[{"result", " ", "=", " ", 
          RowBox[{"IdentityMatrix", "[", "3", "]"}]}], ",", " ", "step", ",", 
         " ", 
         RowBox[{"compositeSteps", " ", "=", " ", 
          RowBox[{"{", "}"}]}], ",", " ", 
         RowBox[{"labels", " ", "=", " ", 
          RowBox[{"{", "}"}]}], ",", "\n", "            ", 
         RowBox[{"matrixColors", " ", "=", " ", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{"RGBColor", "[", 
             RowBox[{"0", ",", " ", "0.4", ",", " ", "0.8"}], "]"}], ",", " ", 
            RowBox[{"RGBColor", "[", 
             RowBox[{"0.2", ",", " ", "0.7", ",", " ", "0.3"}], "]"}], ",", 
            " ", 
            RowBox[{"RGBColor", "[", 
             RowBox[{"1", ",", " ", "0.6", ",", " ", "0"}], "]"}]}], "}"}]}], 
         ",", "\n", "            ", 
         RowBox[{"operationColor", " ", "=", " ", 
          RowBox[{"RGBColor", "[", 
           RowBox[{"0.7", ",", " ", "0", ",", " ", "0.7"}], "]"}]}], ",", " ",
          "\n", "            ", 
         RowBox[{"resultColor", " ", "=", " ", 
          RowBox[{"RGBColor", "[", 
           RowBox[{"0.9", ",", " ", "0.1", ",", " ", "0.1"}], "]"}]}], ",", 
         "\n", "            ", "exactMatrices", ",", " ", 
         RowBox[{"intermediateResults", " ", "=", " ", 
          RowBox[{"{", "}"}]}]}], "\n", "            ", "}"}], ",", "\n", 
       "        ", "\n", "        ", "\n", "        ", 
       RowBox[{
        RowBox[{"exactMatrices", " ", "=", " ", 
         RowBox[{"Map", "[", 
          RowBox[{
          "ConvertMatrixToExactValues", ",", " ", "matrices", ",", " ", 
           RowBox[{"{", "1", "}"}]}], "]"}]}], ";", "\n", "        ", "\n", 
        "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<V\[CapitalYAcute]PO\[CapitalCHacek]ET S\[CapitalUAcute]HRNNEJ \
TRANSFORMA\[CapitalCHacek]NEJ MATICE:\>\"", ",", " ", "Bold", ",", " ", 
           "16"}], "]"}], "]"}], ";", "\n", "        ", "\n", "        ", 
        "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nTransforma\[CHacek]n\[EAcute] matice pre jednotliv\[EAcute] \
transform\[AAcute]cie:\>\"", ",", " ", "Bold", ",", " ", "14"}], "]"}], "]"}],
         ";", "\n", "        ", 
        RowBox[{"For", "[", 
         RowBox[{
          RowBox[{"step", " ", "=", " ", "1"}], ",", " ", 
          RowBox[{"step", " ", "<=", " ", 
           RowBox[{"Length", "[", "exactMatrices", "]"}]}], ",", " ", 
          RowBox[{"step", "++"}], ",", "\n", "            ", 
          RowBox[{
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
              RowBox[{"\"\<Matica pre \>\"", " ", "<>", " ", 
               RowBox[{"transformNames", "[", 
                RowBox[{"[", "step", "]"}], "]"}], " ", "<>", " ", 
               "\"\< (M\>\"", " ", "<>", " ", 
               RowBox[{"ToString", "[", "step", "]"}], " ", "<>", " ", 
               "\"\<):\>\""}], ",", " ", "Bold", ",", " ", 
              RowBox[{"matrixColors", "[", 
               RowBox[{"[", "step", "]"}], "]"}]}], "]"}], "]"}], ";", "\n", 
           "            ", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"MatrixForm", "[", 
             RowBox[{"Map", "[", 
              RowBox[{
               RowBox[{
                RowBox[{"Style", "[", 
                 RowBox[{
                  RowBox[{"FormatRationalElement", "[", "#", "]"}], ",", " ", 
                  
                  RowBox[{"matrixColors", "[", 
                   RowBox[{"[", "step", "]"}], "]"}]}], "]"}], " ", "&"}], 
               ",", " ", 
               RowBox[{"exactMatrices", "[", 
                RowBox[{"[", "step", "]"}], "]"}], ",", " ", 
               RowBox[{"{", "2", "}"}]}], "]"}], "]"}], "]"}], ";"}]}], "\n", 
         "        ", "]"}], ";", "\n", "        ", "\n", "        ", "\n", 
        "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nPostupn\[YAcute] v\[YAcute]po\[CHacek]et s\[UAcute]hrnnej \
matice:\>\"", ",", " ", "Bold", ",", " ", "14"}], "]"}], "]"}], ";", "\n", 
        "        ", 
        RowBox[{"compositeSteps", " ", "=", " ", 
         RowBox[{"{", 
          RowBox[{"exactMatrices", "[", 
           RowBox[{"[", "1", "]"}], "]"}], "}"}]}], ";", "\n", "        ", 
        "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<Krok 1: Za\[CHacek]\[IAcute]name s maticou prvej transform\
\[AAcute]cie:\>\"", ",", " ", "Bold"}], "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
           RowBox[{"\"\<M\:2081 = Matica pre \>\"", " ", "<>", " ", 
            RowBox[{"transformNames", "[", 
             RowBox[{"[", "1", "]"}], "]"}]}], ",", " ", "Bold", ",", " ", 
           RowBox[{"matrixColors", "[", 
            RowBox[{"[", "1", "]"}], "]"}]}], "]"}], "]"}], ";", "\n", 
        "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"MatrixForm", "[", 
          RowBox[{"Map", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"Style", "[", 
              RowBox[{
               RowBox[{"FormatRationalElement", "[", "#", "]"}], ",", " ", 
               RowBox[{"matrixColors", "[", 
                RowBox[{"[", "1", "]"}], "]"}]}], "]"}], " ", "&"}], ",", " ", 
            RowBox[{"exactMatrices", "[", 
             RowBox[{"[", "1", "]"}], "]"}], ",", " ", 
            RowBox[{"{", "2", "}"}]}], "]"}], "]"}], "]"}], ";", "\n", 
        "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"For", "[", 
         RowBox[{
          RowBox[{"step", " ", "=", " ", "2"}], ",", " ", 
          RowBox[{"step", " ", "<=", " ", 
           RowBox[{"Length", "[", "exactMatrices", "]"}]}], ",", " ", 
          RowBox[{"step", "++"}], ",", "\n", "            ", 
          RowBox[{
           RowBox[{"result", " ", "=", " ", 
            RowBox[{"compositeSteps", "[", 
             RowBox[{"[", 
              RowBox[{"-", "1"}], "]"}], "]"}]}], ";", "\n", "            ", 
           "\n", "            ", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
              RowBox[{"\"\<\\nKrok \>\"", " ", "<>", " ", 
               RowBox[{"ToString", "[", "step", "]"}], " ", "<>", " ", 
               "\"\<: N\[AAcute]sobenie mat\[IAcute]c\>\""}], ",", " ", 
              "Bold"}], "]"}], "]"}], ";", "\n", "            ", "\n", 
           "            ", "\n", "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{"step", " ", "==", " ", "2"}], ",", "\n", 
             "                ", 
             RowBox[{
              RowBox[{"Print", "[", 
               RowBox[{"Style", "[", 
                RowBox[{
                "\"\<Pri kompoz\[IAcute]cii transform\[AAcute]ci\[IAcute] n\
\[AAcute]sob\[IAcute]me matice v opa\[CHacek]nom porad\[IAcute], ako sa \
aplikuj\[UAcute].\>\"", ",", " ", "operationColor"}], "]"}], "]"}], ";", "\n",
               "                ", 
              RowBox[{"Print", "[", 
               RowBox[{"Style", "[", 
                RowBox[{
                "\"\<Matica pre aktu\[AAcute]lnu transform\[AAcute]ciu sa n\
\[AAcute]sob\[IAcute] s v\[YAcute]sledkom predch\[AAcute]dzaj\[UAcute]cich \
transform\[AAcute]ci\[IAcute].\>\"", ",", " ", "operationColor"}], "]"}], 
               "]"}], ";"}]}], "\n", "            ", "]"}], ";", "\n", 
           "            ", "\n", "            ", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
              RowBox[{"\"\<M\>\"", " ", "<>", " ", 
               RowBox[{"ToString", "[", "step", "]"}], " ", "<>", " ", 
               "\"\< \[CenterDot] \>\"", " ", "<>", " ", "\n", 
               "                       ", 
               RowBox[{"If", "[", 
                RowBox[{
                 RowBox[{"step", " ", "==", " ", "2"}], ",", " ", 
                 "\"\<M\:2081\>\"", ",", " ", 
                 RowBox[{"\"\<(M\>\"", " ", "<>", " ", 
                  RowBox[{"ToString", "[", 
                   RowBox[{"step", "-", "1"}], "]"}], " ", "<>", " ", 
                  "\"\< \[CenterDot] ... \[CenterDot] M\:2081)\>\""}]}], 
                "]"}], " ", "<>", " ", "\n", "                       ", 
               "\"\< = Matica pre \>\"", " ", "<>", " ", 
               RowBox[{"transformNames", "[", 
                RowBox[{"[", "step", "]"}], "]"}], " ", "<>", " ", 
               "\"\< \[CenterDot] \>\"", " ", "<>", " ", "\n", 
               "                       ", 
               RowBox[{"If", "[", 
                RowBox[{
                 RowBox[{"step", " ", "==", " ", "2"}], ",", " ", 
                 RowBox[{"\"\<Matica pre \>\"", " ", "<>", " ", 
                  RowBox[{"transformNames", "[", 
                   RowBox[{"[", "1", "]"}], "]"}]}], ",", " ", "\n", 
                 "                           ", 
                 RowBox[{
                 "\"\<V\[YAcute]sledok predch\[AAcute]dzaj\[UAcute]cich \
transform\[AAcute]ci\[IAcute] (M\>\"", " ", "<>", " ", 
                  RowBox[{"ToString", "[", 
                   RowBox[{"step", "-", "1"}], "]"}], " ", "<>", " ", 
                  "\"\< \[CenterDot] ... \[CenterDot] M\:2081)\>\""}]}], 
                "]"}]}], ",", " ", "\n", "                 ", "Bold", ",", 
              " ", "operationColor"}], "]"}], "]"}], ";", "\n", 
           "            ", "\n", "            ", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
              RowBox[{"\"\<Matica pre \>\"", " ", "<>", " ", 
               RowBox[{"transformNames", "[", 
                RowBox[{"[", "step", "]"}], "]"}], " ", "<>", " ", 
               "\"\< (M\>\"", " ", "<>", " ", 
               RowBox[{"ToString", "[", "step", "]"}], " ", "<>", " ", 
               "\"\<):\>\""}], ",", " ", 
              RowBox[{"matrixColors", "[", 
               RowBox[{"[", "step", "]"}], "]"}]}], "]"}], "]"}], ";", "\n", 
           "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"MatrixForm", "[", 
             RowBox[{"Map", "[", 
              RowBox[{
               RowBox[{
                RowBox[{"Style", "[", 
                 RowBox[{
                  RowBox[{"FormatRationalElement", "[", "#", "]"}], ",", " ", 
                  
                  RowBox[{"matrixColors", "[", 
                   RowBox[{"[", "step", "]"}], "]"}]}], "]"}], " ", "&"}], 
               ",", " ", 
               RowBox[{"exactMatrices", "[", 
                RowBox[{"[", "step", "]"}], "]"}], ",", " ", 
               RowBox[{"{", "2", "}"}]}], "]"}], "]"}], "]"}], ";", "\n", 
           "            ", "\n", "            ", "\n", "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{"step", " ", "==", " ", "2"}], ",", "\n", 
             "                ", "\n", "                ", 
             RowBox[{
              RowBox[{"Print", "[", 
               RowBox[{"Style", "[", 
                RowBox[{
                "\"\<Matica prvej transform\[AAcute]cie (M\:2081):\>\"", ",", 
                 " ", 
                 RowBox[{"matrixColors", "[", 
                  RowBox[{"[", 
                   RowBox[{"step", "-", "1"}], "]"}], "]"}]}], "]"}], "]"}], 
              ";", "\n", "                ", 
              RowBox[{"Print", "[", 
               RowBox[{"MatrixForm", "[", 
                RowBox[{"Map", "[", 
                 RowBox[{
                  RowBox[{
                   RowBox[{"Style", "[", 
                    RowBox[{
                    RowBox[{"FormatRationalElement", "[", "#", "]"}], ",", 
                    " ", 
                    RowBox[{"matrixColors", "[", 
                    RowBox[{"[", 
                    RowBox[{"step", "-", "1"}], "]"}], "]"}]}], "]"}], " ", 
                   "&"}], ",", " ", "result", ",", " ", 
                  RowBox[{"{", "2", "}"}]}], "]"}], "]"}], "]"}], ";"}], "\n",
              "            ", ",", "\n", "                ", "\n", 
             "                ", 
             RowBox[{
              RowBox[{"Print", "[", 
               RowBox[{"Style", "[", 
                RowBox[{
                 RowBox[{
                 "\"\<V\[YAcute]sledok predch\[AAcute]dzaj\[UAcute]cich \
transform\[AAcute]ci\[IAcute] (M\>\"", " ", "<>", " ", 
                  RowBox[{"ToString", "[", 
                   RowBox[{"step", "-", "1"}], "]"}], " ", "<>", " ", 
                  "\"\< \[CenterDot] ... \[CenterDot] M\:2081):\>\""}], ",", 
                 " ", 
                 RowBox[{"matrixColors", "[", 
                  RowBox[{"[", 
                   RowBox[{"step", "-", "1"}], "]"}], "]"}]}], "]"}], "]"}], 
              ";", "\n", "                ", 
              RowBox[{"Print", "[", 
               RowBox[{"MatrixForm", "[", 
                RowBox[{"Map", "[", 
                 RowBox[{
                  RowBox[{
                   RowBox[{"Style", "[", 
                    RowBox[{
                    RowBox[{"FormatRationalElement", "[", "#", "]"}], ",", 
                    " ", 
                    RowBox[{"matrixColors", "[", 
                    RowBox[{"[", 
                    RowBox[{"step", "-", "1"}], "]"}], "]"}]}], "]"}], " ", 
                   "&"}], ",", " ", "result", ",", " ", 
                  RowBox[{"{", "2", "}"}]}], "]"}], "]"}], "]"}], ";"}]}], 
            "\n", "            ", "]"}], ";", "\n", "            ", "\n", 
           "            ", "\n", "            ", 
           RowBox[{"Module", "[", 
            RowBox[{
             RowBox[{"{", 
              RowBox[{"newMatrix", ",", " ", 
               RowBox[{"errorOccurred", " ", "=", " ", "False"}]}], "}"}], 
             ",", "\n", "                ", 
             RowBox[{
              RowBox[{"newMatrix", " ", "=", " ", 
               RowBox[{
               "Check", "[", "\n", "                    ", "\n", 
                "                    ", 
                RowBox[{
                 RowBox[{"Block", "[", 
                  RowBox[{
                   RowBox[{"{", 
                    RowBox[{"$MaxExtraPrecision", " ", "=", " ", "100"}], 
                    "}"}], ",", "\n", "                        ", 
                   RowBox[{"Simplify", "[", 
                    RowBox[{
                    RowBox[{"exactMatrices", "[", 
                    RowBox[{"[", "step", "]"}], "]"}], " ", ".", " ", 
                    "result"}], "]"}]}], "\n", "                    ", "]"}], 
                 ",", "\n", "                    ", "\n", 
                 "                    ", 
                 RowBox[{"(", 
                  RowBox[{
                   RowBox[{"errorOccurred", " ", "=", " ", "True"}], ";", " ", 
                   RowBox[{"ConstantArray", "[", 
                    RowBox[{"Indeterminate", ",", " ", 
                    RowBox[{"{", 
                    RowBox[{"3", ",", " ", "3"}], "}"}]}], "]"}]}], ")"}], 
                 ",", "\n", "                    ", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"Power", "::", "infy"}], ",", " ", 
                   RowBox[{"Infinity", "::", "indet"}], ",", " ", 
                   RowBox[{"General", "::", "munfl"}], ",", " ", 
                   RowBox[{"General", "::", "ovfl"}]}], "}"}]}], "\n", 
                "                ", "]"}]}], ";", "\n", "                ", 
              "\n", "                ", "\n", "                ", 
              RowBox[{"If", "[", 
               RowBox[{
                RowBox[{"errorOccurred", " ", "||", " ", 
                 RowBox[{"MemberQ", "[", 
                  RowBox[{
                   RowBox[{"Flatten", "[", "newMatrix", "]"}], ",", " ", 
                   "Indeterminate"}], "]"}], " ", "||", " ", 
                 RowBox[{"MemberQ", "[", 
                  RowBox[{
                   RowBox[{"Flatten", "[", "newMatrix", "]"}], ",", " ", 
                   "ComplexInfinity"}], "]"}]}], ",", "\n", 
                "                    ", 
                RowBox[{
                 RowBox[{"newMatrix", " ", "=", " ", 
                  RowBox[{"Table", "[", "\n", "                        ", 
                   RowBox[{
                    RowBox[{"Module", "[", 
                    RowBox[{
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"sum", " ", "=", " ", "0"}], ",", " ", 
                    RowBox[{"i", " ", "=", " ", "row"}], ",", " ", 
                    RowBox[{"j", " ", "=", " ", "col"}]}], "}"}], ",", "\n", 
                    "                            ", 
                    RowBox[{
                    RowBox[{"sum", " ", "=", " ", 
                    RowBox[{
                    "Sum", "[", "\n", "                                ", 
                    RowBox[{
                    RowBox[{"Block", "[", 
                    RowBox[{
                    RowBox[{"{", "term", "}"}], ",", "\n", 
                    "                                    ", 
                    RowBox[{
                    RowBox[{"term", " ", "=", " ", 
                    RowBox[{"Simplify", "[", 
                    RowBox[{
                    RowBox[{"exactMatrices", "[", 
                    RowBox[{"[", 
                    RowBox[{"step", ",", " ", "i", ",", " ", "k"}], "]"}], 
                    "]"}], " ", "*", " ", 
                    RowBox[{"result", "[", 
                    RowBox[{"[", 
                    RowBox[{"k", ",", " ", "j"}], "]"}], "]"}]}], "]"}]}], 
                    ";", "\n", "                                    ", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"term", " ", "===", " ", "Indeterminate"}], " ", "||",
                     " ", 
                    RowBox[{"term", " ", "===", " ", "ComplexInfinity"}]}], 
                    ",", "\n", "                                        ", 
                    "\n", "                                        ", 
                    RowBox[{
                    RowBox[{"term", " ", "=", " ", 
                    RowBox[{
                    RowBox[{"N", "[", 
                    RowBox[{
                    RowBox[{"exactMatrices", "[", 
                    RowBox[{"[", 
                    RowBox[{"step", ",", " ", "i", ",", " ", "k"}], "]"}], 
                    "]"}], ",", " ", "20"}], "]"}], " ", "*", " ", 
                    RowBox[{"N", "[", 
                    RowBox[{
                    RowBox[{"result", "[", 
                    RowBox[{"[", 
                    RowBox[{"k", ",", " ", "j"}], "]"}], "]"}], ",", " ", 
                    "20"}], "]"}]}]}], ";", "\n", 
                    "                                        ", "\n", 
                    "                                        ", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"term", " ", "===", " ", "Indeterminate"}], " ", "||",
                     " ", 
                    RowBox[{"term", " ", "===", " ", "ComplexInfinity"}]}], 
                    ",", " ", "0", ",", " ", "term"}], "]"}]}], "\n", 
                    "                                    ", ",", "\n", 
                    "                                        ", "term"}], 
                    "\n", "                                    ", "]"}]}]}], 
                    "\n", "                                ", "]"}], ",", 
                    "\n", "                                ", 
                    RowBox[{"{", 
                    RowBox[{"k", ",", " ", "1", ",", " ", "3"}], "}"}]}], 
                    "\n", "                            ", "]"}]}], ";", "\n", 
                    "                            ", 
                    RowBox[{"Simplify", "[", "sum", "]"}]}]}], "\n", 
                    "                        ", "]"}], ",", "\n", 
                    "                        ", 
                    RowBox[{"{", 
                    RowBox[{"row", ",", " ", "1", ",", " ", "3"}], "}"}], ",",
                     " ", 
                    RowBox[{"{", 
                    RowBox[{"col", ",", " ", "1", ",", " ", "3"}], "}"}]}], 
                   "\n", "                    ", "]"}]}], ";"}]}], "\n", 
               "                ", "]"}], ";", "\n", "                ", "\n",
               "                ", "\n", "                ", 
              RowBox[{"Print", "[", 
               RowBox[{"Style", "[", 
                RowBox[{
                "\"\<\\nV\[YAcute]po\[CHacek]et n\[AAcute]sobenia \
mat\[IAcute]c:\>\"", ",", " ", "Bold"}], "]"}], "]"}], ";", "\n", 
              "                ", 
              RowBox[{"For", "[", 
               RowBox[{
                RowBox[{"i", " ", "=", " ", "1"}], ",", " ", 
                RowBox[{"i", " ", "<=", " ", "3"}], ",", " ", 
                RowBox[{"i", "++"}], ",", "\n", "                    ", 
                RowBox[{
                 RowBox[{"For", "[", 
                  RowBox[{
                   RowBox[{"j", " ", "=", " ", "1"}], ",", " ", 
                   RowBox[{"j", " ", "<=", " ", "3"}], ",", " ", 
                   RowBox[{"j", "++"}], ",", "\n", "                        ", 
                   RowBox[{
                    RowBox[{"Print", "[", 
                    RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{
                    RowBox[{"\"\<C[\>\"", " ", "<>", " ", 
                    RowBox[{"ToString", "[", "i", "]"}], " ", "<>", " ", 
                    "\"\<,\>\"", " ", "<>", " ", 
                    RowBox[{"ToString", "[", "j", "]"}], " ", "<>", " ", 
                    "\"\<] = \>\""}], ",", " ", "Bold"}], "]"}], ",", " ", 
                    "\n", "                              ", 
                    RowBox[{
                    "Table", "[", "\n", "                                  ", 
                    
                    RowBox[{
                    RowBox[{"With", "[", 
                    RowBox[{
                    RowBox[{"{", 
                    RowBox[{"term", " ", "=", " ", 
                    RowBox[{"Simplify", "[", 
                    RowBox[{
                    RowBox[{"exactMatrices", "[", 
                    RowBox[{"[", 
                    RowBox[{"step", ",", " ", "i", ",", " ", "m"}], "]"}], 
                    "]"}], " ", "*", " ", 
                    RowBox[{"result", "[", 
                    RowBox[{"[", 
                    RowBox[{"m", ",", " ", "j"}], "]"}], "]"}]}], "]"}]}], 
                    "}"}], ",", "\n", 
                    "                                      ", 
                    RowBox[{"Row", "[", 
                    RowBox[{
                    "{", "\n", "                                          ", 
                    RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{"\"\<(\>\"", ",", " ", "operationColor"}], "]"}], 
                    ",", " ", "\n", 
                    "                                          ", 
                    RowBox[{"Style", "[", 
                    RowBox[{
                    RowBox[{"FormatRationalElement", "[", 
                    RowBox[{"exactMatrices", "[", 
                    RowBox[{"[", 
                    RowBox[{"step", ",", " ", "i", ",", " ", "m"}], "]"}], 
                    "]"}], "]"}], ",", " ", 
                    RowBox[{"matrixColors", "[", 
                    RowBox[{"[", "step", "]"}], "]"}]}], "]"}], ",", " ", 
                    "\n", "                                          ", 
                    RowBox[{"Style", "[", 
                    RowBox[{
                    "\"\< \[Times] \>\"", ",", " ", "operationColor"}], "]"}],
                     ",", " ", "\n", 
                    "                                          ", 
                    RowBox[{"Style", "[", 
                    RowBox[{
                    RowBox[{"FormatRationalElement", "[", 
                    RowBox[{"result", "[", 
                    RowBox[{"[", 
                    RowBox[{"m", ",", " ", "j"}], "]"}], "]"}], "]"}], ",", 
                    " ", 
                    RowBox[{"matrixColors", "[", 
                    RowBox[{"[", 
                    RowBox[{"step", "-", "1"}], "]"}], "]"}]}], "]"}], ",", 
                    " ", "\n", "                                          ", 
                    RowBox[{"Style", "[", 
                    RowBox[{"\"\<)\>\"", ",", " ", "operationColor"}], "]"}], 
                    ",", " ", "\n", 
                    "                                          ", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{"m", " ", "<", " ", "3"}], ",", " ", 
                    RowBox[{"Style", "[", 
                    RowBox[{"\"\< + \>\"", ",", " ", "operationColor"}], 
                    "]"}], ",", " ", "\"\<\>\""}], "]"}]}], "\n", 
                    "                                      ", "}"}], "]"}]}], 
                    "\n", "                                  ", "]"}], ",", 
                    "\n", "                                  ", 
                    RowBox[{"{", 
                    RowBox[{"m", ",", " ", "1", ",", " ", "3"}], "}"}]}], 
                    "\n", "                              ", "]"}], ",", " ", 
                    "\n", "                              ", 
                    RowBox[{"Style", "[", 
                    RowBox[{"\"\< = \>\"", ",", " ", "operationColor"}], 
                    "]"}], ",", " ", "\n", "                              ", 
                    RowBox[{"Style", "[", 
                    RowBox[{
                    RowBox[{"FormatRationalElement", "[", 
                    RowBox[{"newMatrix", "[", 
                    RowBox[{"[", 
                    RowBox[{"i", ",", " ", "j"}], "]"}], "]"}], "]"}], ",", 
                    " ", "resultColor"}], "]"}]}], "]"}], ";"}]}], "\n", 
                  "                    ", "]"}], ";"}]}], "\n", 
               "                ", "]"}], ";", "\n", "                ", "\n",
               "                ", "\n", "                ", 
              RowBox[{"AppendTo", "[", 
               RowBox[{"compositeSteps", ",", " ", "newMatrix"}], "]"}], ";", 
              "\n", "                ", "\n", "                ", "\n", 
              "                ", 
              RowBox[{"AppendTo", "[", 
               RowBox[{"intermediateResults", ",", " ", "newMatrix"}], "]"}], 
              ";", "\n", "                ", "\n", "                ", "\n", 
              "                ", 
              RowBox[{"Print", "[", 
               RowBox[{"Style", "[", 
                RowBox[{
                "\"\<\\nV\[YAcute]sledok n\[AAcute]sobenia:\>\"", ",", " ", 
                 "Bold", ",", " ", "resultColor"}], "]"}], "]"}], ";", "\n", 
              "                ", 
              RowBox[{"Print", "[", 
               RowBox[{"MatrixForm", "[", 
                RowBox[{"Map", "[", 
                 RowBox[{
                  RowBox[{
                   RowBox[{"Style", "[", 
                    RowBox[{
                    RowBox[{"FormatRationalElement", "[", "#", "]"}], ",", 
                    " ", "resultColor"}], "]"}], " ", "&"}], ",", " ", 
                  "newMatrix", ",", " ", 
                  RowBox[{"{", "2", "}"}]}], "]"}], "]"}], "]"}], ";"}]}], 
            "\n", "            ", "]"}], ";"}]}], "\n", "        ", "]"}], 
        ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nV\[CapitalYAcute]SLEDN\[CapitalAAcute] \
S\[CapitalUAcute]HRNN\[CapitalAAcute] TRANSFORMA\[CapitalCHacek]N\
\[CapitalAAcute] MATICA:\>\"", ",", " ", "Bold", ",", " ", "16", ",", " ", 
           "resultColor"}], "]"}], "]"}], ";", "\n", "        ", "\n", 
        "        ", "\n", "        ", 
        RowBox[{"Module", "[", 
         RowBox[{
          RowBox[{"{", 
           RowBox[{"finalMatrix", " ", "=", " ", 
            RowBox[{"compositeSteps", "[", 
             RowBox[{"[", 
              RowBox[{"-", "1"}], "]"}], "]"}]}], "}"}], ",", "\n", 
          "            ", 
          RowBox[{
           RowBox[{"Print", "[", 
            RowBox[{"MatrixForm", "[", 
             RowBox[{"Map", "[", 
              RowBox[{
               RowBox[{
                RowBox[{"Style", "[", 
                 RowBox[{
                  RowBox[{"FormatRationalElement", "[", "#", "]"}], ",", " ", 
                  "resultColor"}], "]"}], " ", "&"}], ",", " ", "finalMatrix",
                ",", " ", 
               RowBox[{"{", "2", "}"}]}], "]"}], "]"}], "]"}], ";", "\n", 
           "            ", "\n", "            ", "\n", "            ", 
           RowBox[{"{", 
            RowBox[{"finalMatrix", ",", " ", "compositeSteps"}], "}"}]}]}], 
         "\n", "        ", "]"}]}]}], "\n", "    ", "]"}]}], ";"}], "\n", 
   "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"SafeSimplifyMatrix", "[", "matrix_", "]"}], " ", ":=", " ", 
     "\n", "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"result", ",", " ", "i", ",", " ", "j"}], "}"}], ",", "\n", 
       "        ", 
       RowBox[{
        RowBox[{"result", " ", "=", " ", 
         RowBox[{"Table", "[", "\n", "            ", 
          RowBox[{
           RowBox[{"Check", "[", "\n", "                ", 
            RowBox[{
             RowBox[{"Simplify", "[", 
              RowBox[{"matrix", "[", 
               RowBox[{"[", 
                RowBox[{"i", ",", " ", "j"}], "]"}], "]"}], "]"}], ",", "\n", 
             "                ", 
             RowBox[{"matrix", "[", 
              RowBox[{"[", 
               RowBox[{"i", ",", " ", "j"}], "]"}], "]"}]}], "  ", "\n", 
            "            ", "]"}], ",", "\n", "            ", 
           RowBox[{"{", 
            RowBox[{"i", ",", " ", "1", ",", " ", 
             RowBox[{"Length", "[", "matrix", "]"}]}], "}"}], ",", "\n", 
           "            ", 
           RowBox[{"{", 
            RowBox[{"j", ",", " ", "1", ",", " ", 
             RowBox[{"Length", "[", 
              RowBox[{"matrix", "[", 
               RowBox[{"[", "1", "]"}], "]"}], "]"}]}], "}"}]}], "\n", 
          "        ", "]"}]}], ";", "\n", "        ", "result"}]}], "\n", 
      "    ", "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"ConvertMatrixToExactValues", "[", "matrix_", "]"}], " ", ":=", 
     " ", "\n", "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
         RowBox[{"exactMatrix", " ", "=", " ", "matrix"}], ",", " ", "i", ",",
          " ", "j"}], "}"}], ",", "\n", "        ", 
       RowBox[{
        RowBox[{"For", "[", 
         RowBox[{
          RowBox[{"i", " ", "=", " ", "1"}], ",", " ", 
          RowBox[{"i", " ", "<=", " ", 
           RowBox[{"Length", "[", "matrix", "]"}]}], ",", " ", 
          RowBox[{"i", "++"}], ",", "\n", "            ", 
          RowBox[{
           RowBox[{"For", "[", 
            RowBox[{
             RowBox[{"j", " ", "=", " ", "1"}], ",", " ", 
             RowBox[{"j", " ", "<=", " ", 
              RowBox[{"Length", "[", 
               RowBox[{"matrix", "[", 
                RowBox[{"[", "i", "]"}], "]"}], "]"}]}], ",", " ", 
             RowBox[{"j", "++"}], ",", "\n", "                ", "\n", 
             "                ", 
             RowBox[{
              RowBox[{
               RowBox[{"exactMatrix", "[", 
                RowBox[{"[", 
                 RowBox[{"i", ",", " ", "j"}], "]"}], "]"}], " ", "=", " ", 
               RowBox[{"Check", "[", "\n", "                    ", 
                RowBox[{
                 RowBox[{"ConvertToExactValue", "[", 
                  RowBox[{"matrix", "[", 
                   RowBox[{"[", 
                    RowBox[{"i", ",", " ", "j"}], "]"}], "]"}], "]"}], ",", 
                 "\n", "                    ", 
                 RowBox[{"matrix", "[", 
                  RowBox[{"[", 
                   RowBox[{"i", ",", " ", "j"}], "]"}], "]"}], ",", "  ", 
                 "\n", "                    ", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"Power", "::", "infy"}], ",", " ", 
                   RowBox[{"Infinity", "::", "indet"}]}], "}"}]}], "\n", 
                "                ", "]"}]}], ";"}]}], "\n", "            ", 
            "]"}], ";"}]}], "\n", "        ", "]"}], ";", "\n", "        ", 
        "exactMatrix"}]}], "\n", "    ", "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"ConvertToExactValue", "[", "value_", "]"}], " ", ":=", " ", 
     "\n", "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
        "trigValues", ",", " ", "specialValues", ",", " ", "exactValue", ",", 
         " ", "i", ",", " ", 
         RowBox[{"found", " ", "=", " ", "False"}], ",", " ", "n"}], "}"}], 
       ",", "\n", "        ", "\n", "        ", 
       RowBox[{
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"MatchQ", "[", 
            RowBox[{"value", ",", " ", 
             RowBox[{
             "_Integer", " ", "|", " ", "_Rational", " ", "|", " ", 
              "_Sqrt"}]}], "]"}], " ", "||", " ", "\n", "           ", 
           RowBox[{
            RowBox[{
             RowBox[{"Head", "[", "value", "]"}], " ", "===", " ", "Times"}], 
            " ", "&&", " ", 
            RowBox[{"(", 
             RowBox[{
              RowBox[{"MatchQ", "[", 
               RowBox[{"value", ",", " ", 
                RowBox[{"_", "*", 
                 RowBox[{"Sqrt", "[", "_", "]"}]}]}], "]"}], " ", "||", " ", 
              RowBox[{"MatchQ", "[", 
               RowBox[{"value", ",", " ", 
                RowBox[{"_", "*", 
                 RowBox[{"Power", "[", 
                  RowBox[{"_", ",", " ", "_Rational"}], "]"}]}]}], "]"}]}], 
             ")"}]}]}], ",", "\n", "            ", 
          RowBox[{"Return", "[", "value", "]"}]}], "\n", "        ", "]"}], 
        ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{
            RowBox[{"Head", "[", "value", "]"}], " ", "===", " ", "Symbol"}], 
           " ", "||", " ", 
           RowBox[{
            RowBox[{
             RowBox[{"Head", "[", "value", "]"}], " ", "===", " ", "Power"}], 
            " ", "&&", " ", 
            RowBox[{"(", 
             RowBox[{
              RowBox[{"MatchQ", "[", 
               RowBox[{"value", ",", " ", 
                RowBox[{"Sqrt", "[", "_", "]"}]}], "]"}], " ", "||", " ", 
              RowBox[{"MatchQ", "[", 
               RowBox[{"value", ",", " ", 
                RowBox[{"Power", "[", 
                 RowBox[{"_", ",", " ", "_Rational"}], "]"}]}], "]"}]}], 
             ")"}]}]}], ",", "\n", "            ", 
          RowBox[{"Return", "[", "value", "]"}]}], "\n", "        ", "]"}], 
        ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{"NumericQ", "[", "value", "]"}], ",", "\n", "            ", 
          
          RowBox[{
           RowBox[{"n", " ", "=", " ", 
            RowBox[{"N", "[", "value", "]"}]}], ";", "\n", "            ", 
           "\n", "            ", "\n", "            ", "\n", "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"n", " ", "-", " ", "0"}], "]"}], " ", "<", " ", 
              RowBox[{"10", "^", 
               RowBox[{"-", "10"}]}]}], ",", " ", 
             RowBox[{"Return", "[", "0", "]"}]}], "]"}], ";", "\n", 
           "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"n", " ", "-", " ", "1"}], "]"}], " ", "<", " ", 
              RowBox[{"10", "^", 
               RowBox[{"-", "10"}]}]}], ",", " ", 
             RowBox[{"Return", "[", "1", "]"}]}], "]"}], ";", "\n", 
           "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"n", " ", "-", " ", 
                RowBox[{"1", "/", "2"}]}], "]"}], " ", "<", " ", 
              RowBox[{"10", "^", 
               RowBox[{"-", "10"}]}]}], ",", " ", 
             RowBox[{"Return", "[", 
              RowBox[{"1", "/", "2"}], "]"}]}], "]"}], ";", "\n", 
           "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"n", " ", "-", " ", 
                RowBox[{
                 RowBox[{"Sqrt", "[", "3", "]"}], "/", "2"}]}], "]"}], " ", 
              "<", " ", 
              RowBox[{"10", "^", 
               RowBox[{"-", "10"}]}]}], ",", " ", 
             RowBox[{"Return", "[", 
              RowBox[{
               RowBox[{"Sqrt", "[", "3", "]"}], "/", "2"}], "]"}]}], "]"}], 
           ";", "\n", "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"n", " ", "-", " ", 
                RowBox[{
                 RowBox[{"Sqrt", "[", "2", "]"}], "/", "2"}]}], "]"}], " ", 
              "<", " ", 
              RowBox[{"10", "^", 
               RowBox[{"-", "10"}]}]}], ",", " ", 
             RowBox[{"Return", "[", 
              RowBox[{
               RowBox[{"Sqrt", "[", "2", "]"}], "/", "2"}], "]"}]}], "]"}], 
           ";", "\n", "            ", "\n", "            ", "\n", 
           "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"n", " ", "+", " ", "1"}], "]"}], " ", "<", " ", 
              RowBox[{"10", "^", 
               RowBox[{"-", "10"}]}]}], ",", " ", 
             RowBox[{"Return", "[", 
              RowBox[{"-", "1"}], "]"}]}], "]"}], ";", "            ", "\n", 
           "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"n", " ", "+", " ", 
                RowBox[{"1", "/", "2"}]}], "]"}], " ", "<", " ", 
              RowBox[{"10", "^", 
               RowBox[{"-", "10"}]}]}], ",", " ", 
             RowBox[{"Return", "[", 
              RowBox[{
               RowBox[{"-", "1"}], "/", "2"}], "]"}]}], "]"}], ";", "\n", 
           "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"n", " ", "+", " ", 
                RowBox[{
                 RowBox[{"Sqrt", "[", "3", "]"}], "/", "2"}]}], "]"}], " ", 
              "<", " ", 
              RowBox[{"10", "^", 
               RowBox[{"-", "10"}]}]}], ",", " ", 
             RowBox[{"Return", "[", 
              RowBox[{
               RowBox[{"-", 
                RowBox[{"Sqrt", "[", "3", "]"}]}], "/", "2"}], "]"}]}], "]"}],
            ";", "\n", "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"n", " ", "+", " ", 
                RowBox[{
                 RowBox[{"Sqrt", "[", "2", "]"}], "/", "2"}]}], "]"}], " ", 
              "<", " ", 
              RowBox[{"10", "^", 
               RowBox[{"-", "10"}]}]}], ",", " ", 
             RowBox[{"Return", "[", 
              RowBox[{
               RowBox[{"-", 
                RowBox[{"Sqrt", "[", "2", "]"}]}], "/", "2"}], "]"}]}], "]"}],
            ";", "\n", "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"n", " ", "-", " ", 
                RowBox[{"1", "/", 
                 RowBox[{"Sqrt", "[", "2", "]"}]}]}], "]"}], " ", "<", " ", 
              RowBox[{"10", "^", 
               RowBox[{"-", "10"}]}]}], ",", " ", 
             RowBox[{"Return", "[", 
              RowBox[{"1", "/", 
               RowBox[{"Sqrt", "[", "2", "]"}]}], "]"}]}], "]"}], ";", "\n", 
           "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"n", " ", "+", " ", 
                RowBox[{"1", "/", 
                 RowBox[{"Sqrt", "[", "2", "]"}]}]}], "]"}], " ", "<", " ", 
              RowBox[{"10", "^", 
               RowBox[{"-", "10"}]}]}], ",", " ", 
             RowBox[{"Return", "[", 
              RowBox[{
               RowBox[{"-", "1"}], "/", 
               RowBox[{"Sqrt", "[", "2", "]"}]}], "]"}]}], "]"}], ";", "\n", 
           "            ", "\n", "            ", "\n", "            ", 
           RowBox[{"exactValue", " ", "=", " ", 
            RowBox[{"Check", "[", "\n", "                ", 
             RowBox[{
              RowBox[{"Rationalize", "[", 
               RowBox[{"n", ",", " ", 
                RowBox[{"10", "^", 
                 RowBox[{"-", "10"}]}]}], "]"}], ",", "\n", 
              "                ", "\n", "                ", 
              RowBox[{"Check", "[", "\n", "                    ", 
               RowBox[{
                RowBox[{"Rationalize", "[", 
                 RowBox[{"n", ",", " ", 
                  RowBox[{"10", "^", 
                   RowBox[{"-", "5"}]}]}], "]"}], ",", "\n", 
                "                    ", "\n", "                    ", 
                RowBox[{"Check", "[", "\n", "                        ", 
                 RowBox[{
                  RowBox[{"Rationalize", "[", 
                   RowBox[{"n", ",", " ", 
                    RowBox[{"10", "^", 
                    RowBox[{"-", "3"}]}]}], "]"}], ",", "\n", 
                  "                        ", "\n", 
                  "                        ", "n"}], "\n", 
                 "                    ", "]"}]}], "\n", "                ", 
               "]"}]}], "\n", "            ", "]"}]}], ";", "\n", 
           "            ", "\n", "            ", "\n", "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"exactValue", " ", "-", " ", 
                RowBox[{"Sqrt", "[", "2", "]"}]}], "]"}], " ", "<", " ", 
              RowBox[{"10", "^", 
               RowBox[{"-", "3"}]}]}], ",", " ", 
             RowBox[{"exactValue", " ", "=", " ", 
              RowBox[{"Sqrt", "[", "2", "]"}]}]}], "]"}], ";", "\n", 
           "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"exactValue", " ", "+", " ", 
                RowBox[{"Sqrt", "[", "2", "]"}]}], "]"}], " ", "<", " ", 
              RowBox[{"10", "^", 
               RowBox[{"-", "3"}]}]}], ",", " ", 
             RowBox[{"exactValue", " ", "=", " ", 
              RowBox[{"-", 
               RowBox[{"Sqrt", "[", "2", "]"}]}]}]}], "]"}], ";", "\n", 
           "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"exactValue", " ", "-", " ", 
                RowBox[{"Sqrt", "[", "3", "]"}]}], "]"}], " ", "<", " ", 
              RowBox[{"10", "^", 
               RowBox[{"-", "3"}]}]}], ",", " ", 
             RowBox[{"exactValue", " ", "=", " ", 
              RowBox[{"Sqrt", "[", "3", "]"}]}]}], "]"}], ";", "\n", 
           "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"exactValue", " ", "+", " ", 
                RowBox[{"Sqrt", "[", "3", "]"}]}], "]"}], " ", "<", " ", 
              RowBox[{"10", "^", 
               RowBox[{"-", "3"}]}]}], ",", " ", 
             RowBox[{"exactValue", " ", "=", " ", 
              RowBox[{"-", 
               RowBox[{"Sqrt", "[", "3", "]"}]}]}]}], "]"}], ";", "\n", 
           "            ", "\n", "            ", 
           RowBox[{"Return", "[", "exactValue", "]"}], ";"}]}], "\n", 
         "        ", "]"}], ";", "\n", "        ", "\n", "        ", "\n", 
        "        ", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"Head", "[", "value", "]"}], " ", "===", " ", "Complex"}], 
          ",", "\n", "            ", 
          RowBox[{
           RowBox[{"Return", "[", 
            RowBox[{"Complex", "[", "\n", "                ", 
             RowBox[{
              RowBox[{"ConvertToExactValue", "[", 
               RowBox[{"Re", "[", "value", "]"}], "]"}], ",", " ", "\n", 
              "                ", 
              RowBox[{"ConvertToExactValue", "[", 
               RowBox[{"Im", "[", "value", "]"}], "]"}]}], "\n", 
             "            ", "]"}], "]"}], ";"}]}], "\n", "        ", "]"}], 
        ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        "value"}]}], "\n", "    ", "]"}]}], ";"}], "\n", "    ", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"FormatRationalElement", "[", "element_", "]"}], " ", ":=", " ", 
     
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "formattedValue", "}"}], ",", "\n", "    ", 
       RowBox[{
        RowBox[{"formattedValue", " ", "=", " ", 
         RowBox[{"Check", "[", "\n", "        ", "\n", "        ", 
          RowBox[{
           RowBox[{"Simplify", "[", "element", "]"}], ",", "\n", "        ", 
           "\n", "        ", "element", ",", "\n", "        ", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"Power", "::", "infy"}], ",", " ", 
             RowBox[{"Infinity", "::", "indet"}]}], "}"}]}], "\n", "    ", 
          "]"}]}], ";", "\n", "    ", "\n", "    ", "\n", "    ", 
        RowBox[{"Check", "[", "\n", "        ", 
         RowBox[{
          RowBox[{"TraditionalForm", "[", "formattedValue", "]"}], ",", "\n", 
          "        ", "\n", "        ", "formattedValue", ",", "\n", 
          "        ", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{"Power", "::", "infy"}], ",", " ", 
            RowBox[{"Infinity", "::", "indet"}]}], "}"}]}], "\n", "    ", 
         "]"}]}]}], "\n", "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"SafeMatrixMultiply", "[", 
      RowBox[{"matrixA_", ",", " ", "matrixB_"}], "]"}], " ", ":=", " ", "\n",
      "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
        "result", ",", " ", "i", ",", " ", "j", ",", " ", "k", ",", " ", 
         "sum"}], "}"}], ",", "\n", "        ", "\n", "        ", 
       RowBox[{
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"Length", "[", 
            RowBox[{"matrixA", "[", 
             RowBox[{"[", "1", "]"}], "]"}], "]"}], " ", "!=", " ", 
           RowBox[{"Length", "[", "matrixB", "]"}]}], ",", "\n", 
          "            ", "\n", "            ", 
          RowBox[{
           RowBox[{"Message", "[", 
            RowBox[{"MatrixMultiplication", "::", "dims"}], "]"}], ";", "\n", 
           "            ", 
           RowBox[{"Return", "[", "$Failed", "]"}], ";"}]}], "\n", "        ",
          "]"}], ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"result", " ", "=", " ", 
         RowBox[{"ConstantArray", "[", 
          RowBox[{"0", ",", " ", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"Length", "[", "matrixA", "]"}], ",", " ", 
             RowBox[{"Length", "[", 
              RowBox[{"matrixB", "[", 
               RowBox[{"[", "1", "]"}], "]"}], "]"}]}], "}"}]}], "]"}]}], ";",
         "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"For", "[", 
         RowBox[{
          RowBox[{"i", " ", "=", " ", "1"}], ",", " ", 
          RowBox[{"i", " ", "<=", " ", 
           RowBox[{"Length", "[", "matrixA", "]"}]}], ",", " ", 
          RowBox[{"i", "++"}], ",", "\n", "            ", 
          RowBox[{
           RowBox[{"For", "[", 
            RowBox[{
             RowBox[{"j", " ", "=", " ", "1"}], ",", " ", 
             RowBox[{"j", " ", "<=", " ", 
              RowBox[{"Length", "[", 
               RowBox[{"matrixB", "[", 
                RowBox[{"[", "1", "]"}], "]"}], "]"}]}], ",", " ", 
             RowBox[{"j", "++"}], ",", "\n", "                ", 
             RowBox[{
              RowBox[{"sum", " ", "=", " ", "0"}], ";", "\n", 
              "                ", 
              RowBox[{"For", "[", 
               RowBox[{
                RowBox[{"k", " ", "=", " ", "1"}], ",", " ", 
                RowBox[{"k", " ", "<=", " ", 
                 RowBox[{"Length", "[", "matrixB", "]"}]}], ",", " ", 
                RowBox[{"k", "++"}], ",", "\n", "                    ", "\n", 
                "                    ", 
                RowBox[{
                 RowBox[{"sum", " ", "=", " ", 
                  RowBox[{"Check", "[", "\n", "                        ", 
                   RowBox[{
                    RowBox[{"sum", " ", "+", " ", 
                    RowBox[{
                    RowBox[{"matrixA", "[", 
                    RowBox[{"[", 
                    RowBox[{"i", ",", " ", "k"}], "]"}], "]"}], " ", "*", " ", 
                    RowBox[{"matrixB", "[", 
                    RowBox[{"[", 
                    RowBox[{"k", ",", " ", "j"}], "]"}], "]"}]}]}], ",", "\n",
                     "                        ", "\n", 
                    "                        ", 
                    RowBox[{"sum", " ", "+", " ", 
                    RowBox[{
                    RowBox[{"N", "[", 
                    RowBox[{
                    RowBox[{"matrixA", "[", 
                    RowBox[{"[", 
                    RowBox[{"i", ",", " ", "k"}], "]"}], "]"}], ",", " ", 
                    "20"}], "]"}], " ", "*", " ", 
                    RowBox[{"N", "[", 
                    RowBox[{
                    RowBox[{"matrixB", "[", 
                    RowBox[{"[", 
                    RowBox[{"k", ",", " ", "j"}], "]"}], "]"}], ",", " ", 
                    "20"}], "]"}]}]}], ",", "\n", "                        ", 
                    
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"Power", "::", "infy"}], ",", " ", 
                    RowBox[{"Infinity", "::", "indet"}]}], "}"}]}], "\n", 
                   "                    ", "]"}]}], ";"}]}], "\n", 
               "                ", "]"}], ";", "\n", "                ", "\n",
               "                ", 
              RowBox[{
               RowBox[{"result", "[", 
                RowBox[{"[", 
                 RowBox[{"i", ",", " ", "j"}], "]"}], "]"}], " ", "=", " ", 
               RowBox[{"Check", "[", "\n", "                    ", 
                RowBox[{
                 RowBox[{"Simplify", "[", "sum", "]"}], ",", "\n", 
                 "                    ", "sum", ",", "  ", "\n", 
                 "                    ", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"Power", "::", "infy"}], ",", " ", 
                   RowBox[{"Infinity", "::", "indet"}]}], "}"}]}], "\n", 
                "                ", "]"}]}], ";"}]}], "\n", "            ", 
            "]"}], ";"}]}], "\n", "        ", "]"}], ";", "\n", "        ", 
        "\n", "        ", "result"}]}], "\n", "    ", "]"}]}], ";"}], "\n", 
   "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"VerifyCompositeTransformation", "[", 
      RowBox[{
      "originalPoint_", ",", " ", "compositeMatrix_", ",", " ", "finalPoint_",
        ",", " ", 
       RowBox[{"compositeSteps_", ":", 
        RowBox[{"{", "}"}]}]}], "]"}], " ", ":=", "\n", "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
        "calculatedPoint", ",", " ", "exactCompositeMatrix", ",", " ", 
         "exactOriginalPoint", ",", " ", "exactFinalPoint"}], "}"}], ",", 
       "\n", "        ", "\n", "        ", 
       RowBox[{
        RowBox[{"exactCompositeMatrix", " ", "=", " ", "compositeMatrix"}], 
        ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"exactOriginalPoint", " ", "=", " ", "originalPoint"}], ";", 
        "\n", "        ", 
        RowBox[{"exactFinalPoint", " ", "=", " ", "finalPoint"}], ";", "\n", 
        "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nAPLIK\[CapitalAAcute]CIA S\[CapitalUAcute]HRNNEJ MATICE NA \
P\[CapitalOHat]VODN\[CapitalYAcute] BOD:\>\"", ",", " ", "Bold", ",", " ", 
           "16"}], "]"}], "]"}], ";", "\n", "        ", "\n", "        ", 
        "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nS\[UAcute]hrnn\[AAcute] transforma\[CHacek]n\[AAcute] \
matica:\>\"", ",", " ", "Bold", ",", " ", "14", ",", " ", 
           RowBox[{"RGBColor", "[", 
            RowBox[{"0.9", ",", " ", "0.1", ",", " ", "0.1"}], "]"}]}], "]"}],
          "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"MatrixForm", "[", 
          RowBox[{"Map", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"Style", "[", 
              RowBox[{"#", ",", " ", 
               RowBox[{"RGBColor", "[", 
                RowBox[{"0.9", ",", " ", "0.1", ",", " ", "0.1"}], "]"}]}], 
              "]"}], " ", "&"}], ",", " ", "exactCompositeMatrix", ",", " ", 
            RowBox[{"{", "2", "}"}]}], "]"}], "]"}], "]"}], ";", "\n", 
        "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nP\[OHat]vodn\[YAcute] bod P:\>\"", ",", " ", "Bold", ",", 
           " ", "Blue", ",", " ", "14"}], "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<S\[UAcute]radnice: \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{"exactOriginalPoint", ",", " ", "Blue"}], "]"}]}], "]"}], 
        ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"calculatedPoint", " ", "=", " ", 
         RowBox[{"Module", "[", 
          RowBox[{
           RowBox[{"{", 
            RowBox[{
             RowBox[{"vertex", " ", "=", " ", "exactOriginalPoint"}], ",", 
             " ", "homogeneousVertex", ",", " ", "resultVector"}], "}"}], ",",
            "\n", "            ", 
           RowBox[{
            RowBox[{"homogeneousVertex", " ", "=", " ", 
             RowBox[{"Append", "[", 
              RowBox[{"vertex", ",", " ", "1"}], "]"}]}], ";", "\n", 
            "            ", "\n", "            ", "\n", "            ", 
            RowBox[{"resultVector", " ", "=", " ", 
             RowBox[{
             "exactCompositeMatrix", " ", ".", " ", "homogeneousVertex"}]}], 
            ";", "\n", "            ", "\n", "            ", "\n", 
            "            ", 
            RowBox[{"Take", "[", 
             RowBox[{"resultVector", ",", " ", "2"}], "]"}]}]}], "\n", 
          "        ", "]"}]}], ";", "\n", "        ", "\n", "        ", "\n", 
        "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nPostup pri aplikovan\[IAcute] s\[UAcute]hrnnej transforma\
\[CHacek]nej matice na bod:\>\"", ",", " ", "Bold", ",", " ", "14"}], "]"}], 
         "]"}], ";", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<1. Bod (x, y) sa roz\[SHacek]\[IAcute]ri na homog\[EAcute]nne s\
\[UAcute]radnice (x, y, 1)\>\"", "]"}], ";", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<2. Vyn\[AAcute]sob\[IAcute] sa s\[UAcute]hrnnou transforma\
\[CHacek]nou maticou 3\[Times]3\>\"", "]"}], ";", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<3. Z v\[YAcute]sledn\[EAcute]ho vektora vezmeme prv\[EAcute] \
dve s\[UAcute]radnice, \[CHacek]\[IAcute]m dostaneme transformovan\[YAcute] \
bod\>\"", "]"}], ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nPODROBN\[CapitalYAcute] \
V\[CapitalYAcute]PO\[CapitalCHacek]ET:\>\"", ",", " ", "Bold", ",", " ", 
           "16"}], "]"}], "]"}], ";", "\n", "        ", "\n", "        ", 
        RowBox[{"Module", "[", 
         RowBox[{
          RowBox[{"{", 
           RowBox[{
            RowBox[{"vertex", " ", "=", " ", "exactOriginalPoint"}], ",", " ",
             "homogeneousVertex", ",", " ", "resultVector"}], "}"}], ",", 
          "\n", "            ", 
          RowBox[{
           RowBox[{"homogeneousVertex", " ", "=", " ", 
            RowBox[{"Append", "[", 
             RowBox[{"vertex", ",", " ", "1"}], "]"}]}], ";", "\n", 
           "            ", 
           RowBox[{"resultVector", " ", "=", " ", 
            RowBox[{
            "exactCompositeMatrix", " ", ".", " ", "homogeneousVertex"}]}], 
           ";", "\n", "            ", "\n", "            ", "\n", 
           "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
             "\"\<\\nBod P:\>\"", ",", " ", "Bold", ",", " ", "Blue", ",", 
              " ", "14"}], "]"}], "]"}], ";", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{
            "\"\<P\[OHat]vodn\[EAcute] s\[UAcute]radnice: \>\"", ",", " ", 
             RowBox[{"Style", "[", 
              RowBox[{"vertex", ",", " ", "Blue"}], "]"}]}], "]"}], ";", "\n",
            "            ", 
           RowBox[{"Print", "[", 
            RowBox[{
            "\"\<V homog\[EAcute]nnych s\[UAcute]radniciach: \>\"", ",", " ", 
             
             RowBox[{"Style", "[", 
              RowBox[{"homogeneousVertex", ",", " ", "Blue"}], "]"}]}], "]"}],
            ";", "\n", "            ", "\n", "            ", "\n", 
           "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
             "\"\<\\nMaticov\[EAcute] n\[AAcute]sobenie:\>\"", ",", " ", 
              "Bold"}], "]"}], "]"}], ";", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"Row", "[", 
             RowBox[{"{", "\n", "                ", 
              RowBox[{
               RowBox[{"Style", "[", 
                RowBox[{"\"\<M \[CenterDot] \>\"", ",", " ", 
                 RowBox[{"RGBColor", "[", 
                  RowBox[{"0.5", ",", " ", "0", ",", " ", "0.5"}], "]"}]}], 
                "]"}], ",", " ", "\n", "                ", 
               RowBox[{"Style", "[", 
                RowBox[{"\"\<P\>\"", ",", " ", "Blue"}], "]"}], ",", " ", 
               "\n", "                ", 
               RowBox[{"Style", "[", 
                RowBox[{"\"\< = \>\"", ",", " ", 
                 RowBox[{"RGBColor", "[", 
                  RowBox[{"0.5", ",", " ", "0", ",", " ", "0.5"}], "]"}]}], 
                "]"}]}], "\n", "            ", "}"}], "]"}], "]"}], ";", "\n",
            "            ", "\n", "            ", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"Row", "[", 
             RowBox[{"{", "\n", "                ", 
              RowBox[{
               RowBox[{"Style", "[", 
                RowBox[{
                 RowBox[{"MatrixForm", "[", "exactCompositeMatrix", "]"}], 
                 ",", " ", 
                 RowBox[{"RGBColor", "[", 
                  RowBox[{"0.9", ",", " ", "0.1", ",", " ", "0.1"}], "]"}]}], 
                "]"}], ",", " ", "\n", "                ", 
               RowBox[{"Style", "[", 
                RowBox[{"\"\< \[CenterDot] \>\"", ",", " ", 
                 RowBox[{"RGBColor", "[", 
                  RowBox[{"0.5", ",", " ", "0", ",", " ", "0.5"}], "]"}]}], 
                "]"}], ",", " ", "\n", "                ", 
               RowBox[{"Style", "[", 
                RowBox[{
                 RowBox[{"MatrixForm", "[", "homogeneousVertex", "]"}], ",", 
                 " ", "Blue"}], "]"}]}], "\n", "            ", "}"}], "]"}], 
            "]"}], ";", "\n", "            ", "\n", "            ", "\n", 
           "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
             "\"\<\\nPodrobn\[YAcute] v\[YAcute]po\[CHacek]et:\>\"", ",", " ",
               "Bold"}], "]"}], "]"}], ";", "\n", "            ", "\n", 
           "            ", 
           RowBox[{"Module", "[", 
            RowBox[{
             RowBox[{"{", 
              RowBox[{"j", ",", " ", "row", ",", " ", "rowResult"}], "}"}], 
             ",", "\n", "                ", 
             RowBox[{
              RowBox[{"For", "[", 
               RowBox[{
                RowBox[{"j", " ", "=", " ", "1"}], ",", " ", 
                RowBox[{"j", " ", "<=", " ", "3"}], ",", " ", 
                RowBox[{"j", "++"}], ",", "\n", "                    ", 
                RowBox[{
                 RowBox[{"row", " ", "=", " ", 
                  RowBox[{"exactCompositeMatrix", "[", 
                   RowBox[{"[", "j", "]"}], "]"}]}], ";", "\n", 
                 "                    ", 
                 RowBox[{"rowResult", " ", "=", " ", 
                  RowBox[{"Sum", "[", 
                   RowBox[{
                    RowBox[{
                    RowBox[{"row", "[", 
                    RowBox[{"[", "k", "]"}], "]"}], " ", "*", " ", 
                    RowBox[{"homogeneousVertex", "[", 
                    RowBox[{"[", "k", "]"}], "]"}]}], ",", " ", 
                    RowBox[{"{", 
                    RowBox[{"k", ",", " ", "1", ",", " ", "3"}], "}"}]}], 
                   "]"}]}], ";", "\n", "                    ", "\n", 
                 "                    ", "\n", "                    ", 
                 RowBox[{"Print", "[", 
                  RowBox[{"Style", "[", 
                   RowBox[{
                    RowBox[{"\"\<Riadok \>\"", " ", "<>", " ", 
                    RowBox[{"ToString", "[", "j", "]"}], " ", "<>", " ", 
                    "\"\<:\>\""}], ",", " ", "Bold"}], "]"}], "]"}], ";", 
                 "\n", "                    ", 
                 RowBox[{"Print", "[", 
                  RowBox[{"Row", "[", 
                   RowBox[{"{", "\n", "                        ", 
                    RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{
                    RowBox[{"row", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], ",", " ", 
                    RowBox[{"RGBColor", "[", 
                    RowBox[{"0.9", ",", " ", "0.1", ",", " ", "0.1"}], 
                    "]"}]}], "]"}], ",", " ", "\n", 
                    "                        ", 
                    RowBox[{"Style", "[", 
                    RowBox[{"\"\< \[CenterDot] \>\"", ",", " ", 
                    RowBox[{"RGBColor", "[", 
                    RowBox[{"0.5", ",", " ", "0", ",", " ", "0.5"}], "]"}]}], 
                    "]"}], ",", " ", "\n", "                        ", 
                    RowBox[{"Style", "[", 
                    RowBox[{
                    RowBox[{"homogeneousVertex", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], ",", " ", "Blue"}], "]"}],
                     ",", " ", "\n", "                        ", 
                    RowBox[{"Style", "[", 
                    RowBox[{"\"\< + \>\"", ",", " ", 
                    RowBox[{"RGBColor", "[", 
                    RowBox[{"0.5", ",", " ", "0", ",", " ", "0.5"}], "]"}]}], 
                    "]"}], ",", "\n", "                        ", "\n", 
                    "                        ", 
                    RowBox[{"Style", "[", 
                    RowBox[{
                    RowBox[{"row", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], ",", " ", 
                    RowBox[{"RGBColor", "[", 
                    RowBox[{"0.9", ",", " ", "0.1", ",", " ", "0.1"}], 
                    "]"}]}], "]"}], ",", " ", "\n", 
                    "                        ", 
                    RowBox[{"Style", "[", 
                    RowBox[{"\"\< \[CenterDot] \>\"", ",", " ", 
                    RowBox[{"RGBColor", "[", 
                    RowBox[{"0.5", ",", " ", "0", ",", " ", "0.5"}], "]"}]}], 
                    "]"}], ",", " ", "\n", "                        ", 
                    RowBox[{"Style", "[", 
                    RowBox[{
                    RowBox[{"homogeneousVertex", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], ",", " ", "Blue"}], "]"}],
                     ",", " ", "\n", "                        ", 
                    RowBox[{"Style", "[", 
                    RowBox[{"\"\< + \>\"", ",", " ", 
                    RowBox[{"RGBColor", "[", 
                    RowBox[{"0.5", ",", " ", "0", ",", " ", "0.5"}], "]"}]}], 
                    "]"}], ",", "\n", "                        ", "\n", 
                    "                        ", 
                    RowBox[{"Style", "[", 
                    RowBox[{
                    RowBox[{"row", "[", 
                    RowBox[{"[", "3", "]"}], "]"}], ",", " ", 
                    RowBox[{"RGBColor", "[", 
                    RowBox[{"0.9", ",", " ", "0.1", ",", " ", "0.1"}], 
                    "]"}]}], "]"}], ",", " ", "\n", 
                    "                        ", 
                    RowBox[{"Style", "[", 
                    RowBox[{"\"\< \[CenterDot] \>\"", ",", " ", 
                    RowBox[{"RGBColor", "[", 
                    RowBox[{"0.5", ",", " ", "0", ",", " ", "0.5"}], "]"}]}], 
                    "]"}], ",", " ", "\n", "                        ", 
                    RowBox[{"Style", "[", 
                    RowBox[{
                    RowBox[{"homogeneousVertex", "[", 
                    RowBox[{"[", "3", "]"}], "]"}], ",", " ", "Blue"}], "]"}],
                     ",", " ", "\n", "                        ", 
                    RowBox[{"Style", "[", 
                    RowBox[{"\"\< = \>\"", ",", " ", 
                    RowBox[{"RGBColor", "[", 
                    RowBox[{"0.5", ",", " ", "0", ",", " ", "0.5"}], "]"}]}], 
                    "]"}], ",", "\n", "                        ", "\n", 
                    "                        ", 
                    RowBox[{"Style", "[", 
                    RowBox[{"rowResult", ",", " ", 
                    RowBox[{"RGBColor", "[", 
                    RowBox[{"0", ",", " ", "0.6", ",", " ", "0"}], "]"}]}], 
                    "]"}]}], "\n", "                    ", "}"}], "]"}], 
                  "]"}], ";"}]}], "\n", "                ", "]"}], ";"}]}], 
            "\n", "            ", "]"}], ";", "\n", "            ", "\n", 
           "            ", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
             "\"\<\\nV\[YAcute]sledok pre bod P:\>\"", ",", " ", "Bold"}], 
             "]"}], "]"}], ";", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{
            "\"\<V homog\[EAcute]nnych s\[UAcute]radniciach: \>\"", ",", " ", 
             
             RowBox[{"Style", "[", 
              RowBox[{"resultVector", ",", " ", 
               RowBox[{"RGBColor", "[", 
                RowBox[{"0", ",", " ", "0.6", ",", " ", "0"}], "]"}]}], 
              "]"}]}], "]"}], ";", "\n", "            ", "\n", "            ", 
           RowBox[{"Module", "[", 
            RowBox[{
             RowBox[{"{", "transformedPoint", "}"}], ",", "\n", 
             "                ", 
             RowBox[{
              RowBox[{"transformedPoint", " ", "=", " ", 
               RowBox[{"Take", "[", 
                RowBox[{"resultVector", ",", " ", "2"}], "]"}]}], ";", "\n", 
              "                ", "\n", "                ", 
              RowBox[{"Print", "[", 
               RowBox[{
               "\"\<Transformovan\[EAcute] s\[UAcute]radnice (P'): \>\"", ",",
                 " ", "\n", "                     ", 
                RowBox[{"Style", "[", 
                 RowBox[{"transformedPoint", ",", " ", 
                  RowBox[{"RGBColor", "[", 
                   RowBox[{"0", ",", " ", "0.6", ",", " ", "0"}], "]"}]}], 
                 "]"}]}], "]"}], ";"}]}], "\n", "            ", "]"}], ";", 
           "\n", "            ", "\n", "            ", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{
            "\"\<O\[CHacek]ak\[AAcute]van\[YAcute] v\[YAcute]sledok (P'): \
\>\"", ",", " ", "\n", "                 ", 
             RowBox[{"Style", "[", 
              RowBox[{"exactFinalPoint", ",", " ", 
               RowBox[{"RGBColor", "[", 
                RowBox[{"0.9", ",", " ", "0.1", ",", " ", "0.1"}], "]"}]}], 
              "]"}]}], "]"}], ";", "\n", "            ", "\n", "            ",
            "\n", "            ", 
           RowBox[{"Module", "[", 
            RowBox[{
             RowBox[{"{", 
              RowBox[{"difference", ",", " ", 
               RowBox[{"tolerance", " ", "=", " ", 
                RowBox[{"10", "^", 
                 RowBox[{"-", "4"}]}]}]}], "}"}], ",", "\n", 
             "                ", 
             RowBox[{
              RowBox[{"difference", " ", "=", " ", 
               RowBox[{"Norm", "[", 
                RowBox[{
                 RowBox[{"Take", "[", 
                  RowBox[{"resultVector", ",", " ", "2"}], "]"}], " ", "-", 
                 " ", "exactFinalPoint"}], "]"}]}], ";", "\n", 
              "                ", "\n", "                ", 
              RowBox[{"If", "[", 
               RowBox[{
                RowBox[{"difference", " ", "<", " ", "tolerance"}], ",", "\n",
                 "                    ", 
                RowBox[{"Print", "[", 
                 RowBox[{"Style", "[", 
                  RowBox[{
                  "\"\<\[Checkmark] V\[YAcute]sledok sa zhoduje s \
o\[CHacek]ak\[AAcute]van\[YAcute]m bodom.\>\"", ",", " ", 
                   RowBox[{"RGBColor", "[", 
                    RowBox[{"0.2", ",", " ", "0.6", ",", " ", "0.2"}], "]"}], 
                   ",", " ", "Bold"}], "]"}], "]"}], ",", "\n", 
                "                    ", 
                RowBox[{"Print", "[", 
                 RowBox[{"Style", "[", 
                  RowBox[{
                  "\"\<\:2717 V\[YAcute]sledok sa nezhoduje s o\[CHacek]ak\
\[AAcute]van\[YAcute]m bodom!\>\"", ",", " ", 
                   RowBox[{"RGBColor", "[", 
                    RowBox[{"0.9", ",", " ", "0", ",", " ", "0"}], "]"}], ",",
                    " ", "Bold"}], "]"}], "]"}]}], "\n", "                ", 
               "]"}], ";"}]}], "\n", "            ", "]"}], ";"}]}], "\n", 
         "        ", "]"}], ";", "\n", "        ", "\n", "        ", "\n", 
        "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nV\[CapitalYAcute]SLEDN\[CapitalYAcute] TRANSFORMOVAN\
\[CapitalYAcute] BOD P':\>\"", ",", " ", "Bold", ",", " ", "16", ",", " ", 
           RowBox[{"RGBColor", "[", 
            RowBox[{"0", ",", " ", "0.6", ",", " ", "0"}], "]"}]}], "]"}], 
         "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<S\[UAcute]radnice: \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{"calculatedPoint", ",", " ", 
            RowBox[{"RGBColor", "[", 
             RowBox[{"0", ",", " ", "0.6", ",", " ", "0"}], "]"}]}], "]"}]}], 
         "]"}], ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nO\[CHacek]ak\[AAcute]van\[YAcute] v\[YAcute]sledn\[YAcute] \
bod:\>\"", ",", " ", "Bold", ",", " ", "14", ",", " ", 
           RowBox[{"RGBColor", "[", 
            RowBox[{"0.9", ",", " ", "0.1", ",", " ", "0.1"}], "]"}]}], "]"}],
          "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<S\[UAcute]radnice: \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{"exactFinalPoint", ",", " ", 
            RowBox[{"RGBColor", "[", 
             RowBox[{"0.9", ",", " ", "0.1", ",", " ", "0.1"}], "]"}]}], 
           "]"}]}], "]"}], ";", "\n", "        ", "\n", "        ", "\n", 
        "        ", 
        RowBox[{"Module", "[", 
         RowBox[{
          RowBox[{"{", 
           RowBox[{"difference", ",", " ", 
            RowBox[{"tolerance", " ", "=", " ", 
             RowBox[{"10", "^", 
              RowBox[{"-", "4"}]}]}]}], "}"}], ",", "\n", "            ", 
          RowBox[{
           RowBox[{"difference", " ", "=", " ", 
            RowBox[{"Norm", "[", 
             RowBox[{"calculatedPoint", " ", "-", " ", "exactFinalPoint"}], 
             "]"}]}], ";", "\n", "            ", "\n", "            ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{"difference", " ", "<", " ", "tolerance"}], ",", "\n", 
             "                ", 
             RowBox[{"Print", "[", 
              RowBox[{"Style", "[", 
               RowBox[{
               "\"\<\\n\[Checkmark] OVERENIE \[CapitalUAcute]SPE\
\[CapitalSHacek]N\[CapitalEAcute]: Transform\[AAcute]cia pomocou \
s\[UAcute]hrnnej matice d\[AAcute]va spr\[AAcute]vny v\[YAcute]sledok.\>\"", 
                ",", " ", "\n", "                      ", 
                RowBox[{"RGBColor", "[", 
                 RowBox[{"0.2", ",", " ", "0.6", ",", " ", "0.2"}], "]"}], 
                ",", " ", "Bold", ",", " ", "16"}], "]"}], "]"}], ",", "\n", 
             "                ", 
             RowBox[{"Print", "[", 
              RowBox[{"Style", "[", 
               RowBox[{
               "\"\<\\n\:2717 OVERENIE NE\[CapitalUAcute]SPE\[CapitalSHacek]N\
\[CapitalEAcute]: Transform\[AAcute]cia pomocou s\[UAcute]hrnnej matice ned\
\[AAcute]va o\[CHacek]ak\[AAcute]van\[YAcute] v\[YAcute]sledok!\>\"", ",", 
                " ", "\n", "                      ", 
                RowBox[{"RGBColor", "[", 
                 RowBox[{"0.9", ",", " ", "0", ",", " ", "0"}], "]"}], ",", 
                " ", "Bold", ",", " ", "16"}], "]"}], "]"}]}], "\n", 
            "            ", "]"}], ";"}]}], "\n", "        ", "]"}], ";", 
        "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nZ\[CapitalAAcute]VER:\>\"", ",", " ", "Bold", ",", " ", 
           "16"}], "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<Pou\[ZHacek]it\[IAcute]m jedinej transform\[AAcute]cie \
definovanej s\[UAcute]hrnnou maticou sme dosiahli rovnak\[YAcute] \
v\[YAcute]sledok,\>\"", "]"}], ";", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<ako postupn\[YAcute]m aplikovan\[IAcute]m viacer\[YAcute]ch \
transform\[AAcute]ci\[IAcute]. Tento princ\[IAcute]p je k\:013e\[UAcute]\
\[CHacek]ov\[YAcute]\>\"", "]"}], ";", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<v po\[CHacek]\[IAcute]ta\[CHacek]ovej grafike a umo\[ZHacek]\
\[NHacek]uje efekt\[IAcute]vne vykres\:013eovanie zlo\[ZHacek]it\[YAcute]ch \
transform\[AAcute]ci\[IAcute].\>\"", "]"}], ";"}]}], "\n", "    ", "]"}]}], 
    ";"}], "\n", "    ", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"DisplayTransformationSequence", "[", 
      RowBox[{
      "pociatocny_", ",", " ", "druhy_", ",", " ", "treti_", ",", " ", 
       "finalny_", ",", " ", "prva_", ",", " ", "druha_", ",", " ", "tretia_",
        ",", " ", "difficultyLevel_", ",", " ", "transformParams_"}], "]"}], 
     " ", ":=", " ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "}"}], ",", "\n", "    ", 
       RowBox[{
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nPOSTUPN\[CapitalYAcute] \
V\[CapitalYAcute]PO\[CapitalCHacek]ET:\>\"", ",", " ", "Bold", ",", " ", 
           "16"}], "]"}], "]"}], ";", "\n", "    ", "\n", "    ", "\n", 
        "    ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nP\[OHat]vodn\[YAcute] bod P:\>\"", ",", " ", "Bold", ",", 
           " ", "14"}], "]"}], "]"}], ";", "\n", "    ", 
        RowBox[{"Print", "[", "\"\<S\[UAcute]radnice:\>\"", "]"}], ";", "\n", 
        "    ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{"pociatocny", ",", " ", "Blue"}], "]"}], "]"}], ";", "\n", 
        "    ", "\n", "    ", "\n", "    ", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{"prva", " ", "==", " ", "\"\<Posun\>\""}], ",", "\n", 
          "        ", "\n", "        ", 
          RowBox[{"Module", "[", 
           RowBox[{
            RowBox[{"{", 
             RowBox[{"dx", ",", " ", "dy"}], "}"}], ",", "\n", "            ", 
            RowBox[{
             RowBox[{"dx", " ", "=", " ", 
              RowBox[{
               RowBox[{"druhy", "[", 
                RowBox[{"[", "1", "]"}], "]"}], " ", "-", " ", 
               RowBox[{"pociatocny", "[", 
                RowBox[{"[", "1", "]"}], "]"}]}]}], ";", "\n", "            ", 
             RowBox[{"dy", " ", "=", " ", 
              RowBox[{
               RowBox[{"druhy", "[", 
                RowBox[{"[", "2", "]"}], "]"}], " ", "-", " ", 
               RowBox[{"pociatocny", "[", 
                RowBox[{"[", "2", "]"}], "]"}]}]}], ";", "\n", "            ", 
             RowBox[{"Print", "[", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{
                "\"\<\\nVektor posunu: \[CapitalDelta] = \>\"", " ", "<>", 
                 " ", 
                 RowBox[{"ToString", "[", 
                  RowBox[{"InputForm", "[", 
                   RowBox[{"{", 
                    RowBox[{"dx", ",", " ", "dy"}], "}"}], "]"}], "]"}]}], 
                ",", " ", "Bold", ",", " ", "Blue"}], "]"}], "]"}], ";"}]}], 
           "\n", "        ", "]"}]}], "\n", "    ", "]"}], ";", "\n", "    ", 
        "\n", "    ", "\n", "    ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
           RowBox[{
           "\"\<\\nPo prvej transform\[AAcute]cii (\>\"", " ", "<>", " ", 
            "prva", " ", "<>", " ", "\"\<):\>\""}], ",", " ", "Bold", ",", 
           " ", "14"}], "]"}], "]"}], ";", "\n", "    ", 
        RowBox[{"Print", "[", "\"\<S\[UAcute]radnice bodu P':\>\"", "]"}], 
        ";", "\n", "    ", "\n", "    ", "\n", "    ", 
        RowBox[{"Module", "[", 
         RowBox[{
          RowBox[{"{", "processedPoint", "}"}], ",", "\n", "        ", 
          RowBox[{
           RowBox[{"processedPoint", " ", "=", " ", 
            RowBox[{"ProcessPointComplete", "[", "druhy", "]"}]}], ";", "\n", 
           "        ", "\n", "        ", 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{"processedPoint", ",", " ", "mildGreen"}], "]"}], "]"}], 
           ";"}]}], "\n", "    ", "]"}], ";", "\n", "    ", "\n", "    ", 
        "\n", "    ", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"difficultyLevel", " ", "==", " ", "\"\<Medium\>\""}], " ",
            "||", " ", 
           RowBox[{"difficultyLevel", " ", "==", " ", "\"\<Hard\>\""}]}], ",",
           "\n", "        ", "\n", "        ", 
          RowBox[{
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
              RowBox[{
              "\"\<\\nPo druhej transform\[AAcute]cii (\>\"", " ", "<>", " ", 
               "druha", " ", "<>", " ", "\"\<):\>\""}], ",", " ", "Bold", ",",
               " ", "14"}], "]"}], "]"}], ";", "\n", "        ", 
           RowBox[{"Print", "[", "\"\<S\[UAcute]radnice bodu P'':\>\"", "]"}],
            ";", "\n", "        ", "\n", "        ", "\n", "        ", 
           RowBox[{"Module", "[", 
            RowBox[{
             RowBox[{"{", "processedPoint", "}"}], ",", "\n", "            ", 
             
             RowBox[{
              RowBox[{"processedPoint", " ", "=", " ", 
               RowBox[{"ProcessPointComplete", "[", "treti", "]"}]}], ";", 
              "\n", "            ", "\n", "            ", 
              RowBox[{"Print", "[", 
               RowBox[{"Style", "[", 
                RowBox[{"processedPoint", ",", " ", "Orange"}], "]"}], "]"}], 
              ";"}]}], "\n", "        ", "]"}], ";"}]}], "\n", "    ", "]"}], 
        ";", "\n", "    ", "\n", "    ", "\n", "    ", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{"difficultyLevel", " ", "==", " ", "\"\<Hard\>\""}], ",", 
          "\n", "        ", "\n", "        ", 
          RowBox[{
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
              RowBox[{
              "\"\<\\nPo tretej transform\[AAcute]cii (\>\"", " ", "<>", " ", 
               "tretia", " ", "<>", " ", "\"\<):\>\""}], ",", " ", "Bold", 
              ",", " ", "14"}], "]"}], "]"}], ";", "\n", "        ", 
           RowBox[{
           "Print", "[", "\"\<S\[UAcute]radnice bodu P''':\>\"", "]"}], ";", 
           "\n", "        ", "\n", "        ", "\n", "        ", 
           RowBox[{"Module", "[", 
            RowBox[{
             RowBox[{"{", "processedPoint", "}"}], ",", "\n", "            ", 
             
             RowBox[{
              RowBox[{"processedPoint", " ", "=", " ", 
               RowBox[{"ProcessPointComplete", "[", "finalny", "]"}]}], ";", 
              "\n", "            ", "\n", "            ", 
              RowBox[{"Print", "[", 
               RowBox[{"Style", "[", 
                RowBox[{"processedPoint", ",", " ", "Red"}], "]"}], "]"}], 
              ";"}]}], "\n", "        ", "]"}], ";"}]}], "\n", "    ", "]"}], 
        ";", "\n", "    ", "\n", "    ", "\n", "    ", "\n", "    ", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"difficultyLevel", " ", "==", " ", "\"\<Medium\>\""}], " ",
            "||", " ", 
           RowBox[{"difficultyLevel", " ", "==", " ", "\"\<Hard\>\""}]}], ",",
           "\n", "        ", 
          RowBox[{"Module", "[", 
           RowBox[{
            RowBox[{"{", 
             RowBox[{
              RowBox[{"transformMatrices", " ", "=", " ", 
               RowBox[{"{", "}"}]}], ",", " ", 
              RowBox[{"transformNames", " ", "=", " ", 
               RowBox[{"{", "}"}]}], ",", " ", "compositeMatrixResult"}], 
             "}"}], ",", "\n", "            ", "\n", "            ", 
            RowBox[{
             RowBox[{"AppendTo", "[", 
              RowBox[{"transformMatrices", ",", " ", "\n", "                ", 
               RowBox[{"GetTransformationMatrix", "[", 
                RowBox[{"prva", ",", " ", 
                 RowBox[{"transformParams", "[", 
                  RowBox[{"[", "1", "]"}], "]"}]}], "]"}]}], "]"}], ";", "\n",
              "            ", 
             RowBox[{"AppendTo", "[", 
              RowBox[{"transformNames", ",", " ", "prva"}], "]"}], ";", "\n", 
             "            ", "\n", "            ", 
             RowBox[{"If", "[", 
              RowBox[{
               RowBox[{
                RowBox[{"difficultyLevel", " ", "==", " ", "\"\<Medium\>\""}],
                 " ", "||", " ", 
                RowBox[{"difficultyLevel", " ", "==", " ", "\"\<Hard\>\""}]}],
                ",", "\n", "                ", 
               RowBox[{
                RowBox[{"AppendTo", "[", 
                 RowBox[{
                 "transformMatrices", ",", " ", "\n", "                    ", 
                  
                  RowBox[{"GetTransformationMatrix", "[", 
                   RowBox[{"druha", ",", " ", 
                    RowBox[{"transformParams", "[", 
                    RowBox[{"[", "2", "]"}], "]"}]}], "]"}]}], "]"}], ";", 
                "\n", "                ", 
                RowBox[{"AppendTo", "[", 
                 RowBox[{"transformNames", ",", " ", "druha"}], "]"}], 
                ";"}]}], "\n", "            ", "]"}], ";", "\n", 
             "            ", "\n", "            ", 
             RowBox[{"If", "[", 
              RowBox[{
               RowBox[{"difficultyLevel", " ", "==", " ", "\"\<Hard\>\""}], 
               ",", "\n", "                ", 
               RowBox[{
                RowBox[{"AppendTo", "[", 
                 RowBox[{
                 "transformMatrices", ",", " ", "\n", "                    ", 
                  
                  RowBox[{"GetTransformationMatrix", "[", 
                   RowBox[{"tretia", ",", " ", 
                    RowBox[{"transformParams", "[", 
                    RowBox[{"[", "3", "]"}], "]"}]}], "]"}]}], "]"}], ";", 
                "\n", "                ", 
                RowBox[{"AppendTo", "[", 
                 RowBox[{"transformNames", ",", " ", "tretia"}], "]"}], 
                ";"}]}], "\n", "            ", "]"}], ";", "\n", 
             "            ", "\n", "            ", "\n", "            ", 
             RowBox[{"compositeMatrixResult", " ", "=", " ", 
              RowBox[{"DisplayCompositeMatrixCalculation", "[", 
               RowBox[{"transformMatrices", ",", " ", "transformNames"}], 
               "]"}]}], ";", "\n", "            ", "\n", "            ", "\n",
              "            ", 
             RowBox[{"VerifyCompositeTransformation", "[", 
              RowBox[{"pociatocny", ",", " ", 
               RowBox[{"compositeMatrixResult", "[", 
                RowBox[{"[", "1", "]"}], "]"}], ",", " ", "finalny", ",", " ", 
               RowBox[{"compositeMatrixResult", "[", 
                RowBox[{"[", "2", "]"}], "]"}]}], "]"}], ";"}]}], "\n", 
           "        ", "]"}]}], "\n", "    ", "]"}], ";"}]}], "\n", "]"}]}], 
    ";"}], "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"FormatTransformationDetailedDescription", "[", 
      RowBox[{"transformType_", ",", " ", "params_"}], "]"}], " ", ":=", " ", 
     "\n", "    ", 
     RowBox[{"Switch", "[", 
      RowBox[{
      "transformType", ",", "\n", "        ", "\"\<Posun\>\"", ",", " ", "\n",
        "            ", 
       RowBox[{"Row", "[", 
        RowBox[{"{", 
         RowBox[{
         "\"\<Vykonajte posun bodu v 2D priestore pomocou vektora posunu \
\>\"", ",", " ", "\n", "                 ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"ToString", "[", 
             RowBox[{"InputForm", "[", "params", "]"}], "]"}], ",", " ", 
            "Blue", ",", " ", "Bold"}], "]"}]}], "}"}], "]"}], ",", "\n", 
       "                 ", "\n", "        ", "\"\<Rot\[AAcute]cia\>\"", ",", 
       " ", "\n", "            ", 
       RowBox[{"Row", "[", 
        RowBox[{"{", 
         RowBox[{
         "\"\<Vykonajte rot\[AAcute]ciu bodu v 2D priestore okolo \
po\[CHacek]iatku s\[UAcute]radnicovej s\[UAcute]stavy [0,0] o uhol \>\"", ",",
           " ", "\n", "                 ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"ToString", "[", "params", "]"}], " ", "<>", " ", 
             "\"\<\[Degree]\>\""}], ",", " ", "Blue", ",", " ", "Bold"}], 
           "]"}]}], "}"}], "]"}], ",", "\n", "            ", "\n", 
       "        ", "\"\<Symetria\>\"", ",", " ", "\n", "            ", 
       RowBox[{"Switch", "[", 
        RowBox[{
        "params", ",", "\n", "                ", "\"\<os x\>\"", ",", " ", 
         "\"\<Vykonajte symetriu bodu v 2D priestore pod\:013ea osi x \
(priamky y = 0)\>\"", ",", "\n", "                ", "\"\<os y\>\"", ",", 
         " ", "\"\<Vykonajte symetriu bodu v 2D priestore pod\:013ea osi y \
(priamky x = 0)\>\"", ",", "\n", "                ", "\"\<priamka y=x\>\"", 
         ",", " ", 
         "\"\<Vykonajte symetriu bodu v 2D priestore pod\:013ea priamky y = x\
\>\"", ",", "\n", "                ", "\"\<priamka y=-x\>\"", ",", " ", 
         "\"\<Vykonajte symetriu bodu v 2D priestore pod\:013ea priamky y = \
-x\>\"", ",", "\n", "                ", "\"\<complex\>\"", ",", " ", "\n", 
         "                    ", 
         "\"\<Vykonajte symetriu bodu v 2D priestore pod\:013ea \
v\[SHacek]eobecnej priamky\>\"", ",", "\n", "                ", "_", ",", 
         "\n", "                ", 
         RowBox[{"If", "[", 
          RowBox[{
           RowBox[{
            RowBox[{"ListQ", "[", "params", "]"}], " ", "&&", " ", 
            RowBox[{
             RowBox[{"Length", "[", "params", "]"}], " ", "==", " ", "3"}]}], 
           ",", "\n", "                    ", 
           RowBox[{"Module", "[", 
            RowBox[{
             RowBox[{"{", 
              RowBox[{
              "a", ",", " ", "b", ",", " ", "c", ",", " ", "standardForm"}], 
              "}"}], ",", "\n", "                        ", 
             RowBox[{
              RowBox[{"a", " ", "=", " ", 
               RowBox[{"params", "[", 
                RowBox[{"[", "1", "]"}], "]"}]}], ";", "\n", 
              "                        ", 
              RowBox[{"b", " ", "=", " ", 
               RowBox[{"params", "[", 
                RowBox[{"[", "2", "]"}], "]"}]}], ";", "\n", 
              "                        ", 
              RowBox[{"c", " ", "=", " ", 
               RowBox[{"params", "[", 
                RowBox[{"[", "3", "]"}], "]"}]}], ";", "\n", 
              "                        ", "\n", "                        ", 
              "\n", "                        ", 
              RowBox[{"standardForm", " ", "=", " ", 
               RowBox[{
                RowBox[{"ToString", "[", "a", "]"}], " ", "<>", " ", 
                "\"\<x \>\""}]}], ";", "\n", "                        ", 
              RowBox[{"standardForm", " ", "=", " ", 
               RowBox[{"standardForm", " ", "<>", " ", 
                RowBox[{"If", "[", 
                 RowBox[{
                  RowBox[{"b", " ", ">=", " ", "0"}], ",", " ", "\"\<+ \>\"", 
                  ",", " ", "\"\<\>\""}], "]"}], " ", "<>", " ", 
                RowBox[{"ToString", "[", "b", "]"}], " ", "<>", " ", 
                "\"\<y \>\""}]}], ";", "\n", "                        ", 
              RowBox[{"If", "[", 
               RowBox[{
                RowBox[{"c", " ", "!=", " ", "0"}], ",", " ", 
                RowBox[{"standardForm", " ", "=", " ", 
                 RowBox[{"standardForm", " ", "<>", " ", 
                  RowBox[{"If", "[", 
                   RowBox[{
                    RowBox[{"c", " ", ">=", " ", "0"}], ",", " ", 
                    "\"\<+ \>\"", ",", " ", "\"\<\>\""}], "]"}], " ", "<>", 
                  " ", 
                  RowBox[{"ToString", "[", "c", "]"}]}]}]}], "]"}], ";", "\n",
               "                        ", 
              RowBox[{"standardForm", " ", "=", " ", 
               RowBox[{"standardForm", " ", "<>", " ", "\"\< = 0\>\""}]}], 
              ";", "\n", "                        ", "\n", 
              "                        ", "\n", "                        ", 
              RowBox[{"Row", "[", 
               RowBox[{"{", "\n", "                            ", 
                RowBox[{
                "\"\<Vykonajte symetriu bodu v 2D priestore pod\:013ea \
priamky \>\"", ",", "\n", "                            ", 
                 RowBox[{"Style", "[", 
                  RowBox[{
                  "standardForm", ",", " ", "Blue", ",", " ", "Bold"}], 
                  "]"}]}], "\n", "                        ", "}"}], "]"}]}]}],
             "\n", "                    ", "]"}], ",", "\n", 
           "                    ", "\n", "                    ", 
           "\"\<Vykonajte symetriu bodu v 2D priestore pod\:013ea danej \
priamky\>\""}], "\n", "                ", "]"}]}], "\n", "            ", 
        "]"}], ",", "\n", "            ", "\n", "        ", "_", ",", "\n", 
       "            ", 
       RowBox[{
       "\"\<Vykonajte \>\"", " ", "<>", " ", "transformType", " ", "<>", " ", 
        "\"\< bodu\>\""}]}], "\n", "    ", "]"}]}], ";"}], "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"FormatTransformationParameters", "[", 
      RowBox[{"transformType_", ",", " ", "params_"}], "]"}], " ", ":=", " ", 
     
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "formattedStr", "}"}], ",", "\n", "    ", 
       RowBox[{"Switch", "[", 
        RowBox[{
        "transformType", ",", "            ", "\n", "        ", 
         "\"\<Posun\>\"", ",", " ", "\n", "            ", "\n", 
         "            ", 
         RowBox[{"\"\<o vektor \>\"", " ", "<>", " ", 
          RowBox[{"ToString", "[", 
           RowBox[{"InputForm", "[", "params", "]"}], "]"}]}], ",", "\n", 
         "            ", "\n", "        ", "\"\<Rot\[AAcute]cia\>\"", ",", 
         " ", "\n", "            ", "\n", "            ", 
         RowBox[{"\"\<o uhol \>\"", " ", "<>", " ", 
          RowBox[{"ToString", "[", "params", "]"}], " ", "<>", " ", 
          "\"\<\[Degree]\>\""}], ",", "\n", "            ", "\n", "        ", 
         "\"\<Symetria\>\"", ",", " ", "\n", "            ", "\n", 
         "            ", 
         RowBox[{"Switch", "[", 
          RowBox[{
          "params", ",", "\n", "                ", "\"\<os x\>\"", ",", " ", 
           "\"\<pod\:013ea osi x\>\"", ",", "\n", "                ", 
           "\"\<os y\>\"", ",", " ", "\"\<pod\:013ea osi y\>\"", ",", "\n", 
           "                ", "\"\<priamka y=x\>\"", ",", " ", 
           "\"\<pod\:013ea priamky y = x\>\"", ",", "\n", "                ", 
           "\"\<priamka y=-x\>\"", ",", " ", 
           "\"\<pod\:013ea priamky y = -x\>\"", ",", "\n", "                ",
            "_", ",", "\n", "                ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"ListQ", "[", "params", "]"}], " ", "&&", " ", 
              RowBox[{
               RowBox[{"Length", "[", "params", "]"}], " ", "==", " ", 
               "3"}]}], ",", "\n", "                    ", 
             RowBox[{"Module", "[", 
              RowBox[{
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"a", " ", "=", " ", 
                  RowBox[{"params", "[", 
                   RowBox[{"[", "1", "]"}], "]"}]}], ",", " ", 
                 RowBox[{"b", " ", "=", " ", 
                  RowBox[{"params", "[", 
                   RowBox[{"[", "2", "]"}], "]"}]}], ",", " ", 
                 RowBox[{"c", " ", "=", " ", 
                  RowBox[{"params", "[", 
                   RowBox[{"[", "3", "]"}], "]"}]}], ",", " ", "eqStr"}], 
                "}"}], ",", "\n", "                        ", "\n", 
               "                        ", 
               RowBox[{
                RowBox[{"eqStr", " ", "=", " ", 
                 RowBox[{
                  RowBox[{"ToString", "[", 
                   RowBox[{"TraditionalForm", "[", "a", "]"}], "]"}], " ", "<>",
                   " ", "\"\<x \>\"", " ", "<>", " ", "\n", 
                  "                                ", 
                  RowBox[{"If", "[", 
                   RowBox[{
                    RowBox[{"b", " ", ">=", " ", "0"}], ",", " ", 
                    "\"\<+ \>\"", ",", " ", "\"\<\>\""}], "]"}], " ", "<>", 
                  "\n", "                                ", 
                  RowBox[{"ToString", "[", 
                   RowBox[{"TraditionalForm", "[", "b", "]"}], "]"}], " ", "<>",
                   " ", "\"\<y \>\"", " ", "<>", " ", "\n", 
                  "                                ", 
                  RowBox[{"If", "[", 
                   RowBox[{
                    RowBox[{"c", " ", ">=", " ", "0"}], ",", " ", 
                    "\"\<+ \>\"", ",", " ", "\"\<\>\""}], "]"}], " ", "<>", 
                  "\n", "                                ", 
                  RowBox[{"ToString", "[", 
                   RowBox[{"TraditionalForm", "[", "c", "]"}], "]"}], " ", "<>",
                   " ", "\"\< = 0\>\""}]}], ";", "\n", 
                "                        ", 
                RowBox[{
                "\"\<pod\:013ea priamky \>\"", " ", "<>", " ", "eqStr"}]}]}], 
              "\n", "                    ", "]"}], ",", "\n", 
             "                    ", 
             RowBox[{"\"\<pod\:013ea \>\"", " ", "<>", " ", 
              RowBox[{"ToString", "[", "params", "]"}]}]}], "\n", 
            "                ", "]"}]}], "\n", "            ", "]"}], ",", 
         "\n", "            ", "\n", "        ", "_", ",", " ", "\n", 
         "            ", "\"\<\>\""}], "\n", "    ", "]"}]}], "\n", "]"}]}], 
    ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"CreatePointVisualization", "[", 
      RowBox[{
      "pociatocny_", ",", " ", "druhy_", ",", " ", "treti_", ",", " ", 
       "finalny_", ",", " ", "prva_", ",", " ", "druha_", ",", " ", 
       "tretia_"}], "]"}], " ", ":=", " ", "\n", "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "\n", "            ", 
        RowBox[{
        "allPoints", ",", " ", "xMin", ",", " ", "xMax", ",", " ", "yMin", 
         ",", " ", "yMax", ",", " ", "\n", "            ", "xRange", ",", " ",
          "yRange", ",", " ", 
         RowBox[{"padding", " ", "=", " ", "1"}], ",", "\n", "            ", 
         RowBox[{"brightBlue", " ", "=", " ", 
          RowBox[{"RGBColor", "[", 
           RowBox[{"0", ",", " ", "0.4", ",", " ", "0.8"}], "]"}]}], ",", 
         "\n", "            ", 
         RowBox[{"brightGreen", " ", "=", " ", 
          RowBox[{"RGBColor", "[", 
           RowBox[{"0.2", ",", " ", "0.7", ",", " ", "0.3"}], "]"}]}], ",", 
         "\n", "            ", 
         RowBox[{"brightOrange", " ", "=", " ", 
          RowBox[{"RGBColor", "[", 
           RowBox[{"1", ",", " ", "0.6", ",", " ", "0"}], "]"}]}], ",", "\n", 
         "            ", 
         RowBox[{"brightRed", " ", "=", " ", 
          RowBox[{"RGBColor", "[", 
           RowBox[{"0.9", ",", " ", "0.1", ",", " ", "0.1"}], "]"}]}], ",", 
         "\n", "            ", 
         RowBox[{"lightGray", " ", "=", " ", 
          RowBox[{"RGBColor", "[", 
           RowBox[{"0.9", ",", " ", "0.9", ",", " ", "0.9"}], "]"}]}], ",", 
         "\n", "            ", "labelPositions", ",", "\n", "            ", 
         "maxRange", ",", " ", "xMid", ",", " ", "yMid", ",", "\n", 
         "            ", 
         RowBox[{"showSecond", " ", "=", " ", 
          RowBox[{
          "druha", " ", "!=", " ", "\"\<\[CapitalZHacek]iadna\>\""}]}], ",", 
         "\n", "            ", 
         RowBox[{"showThird", " ", "=", " ", 
          RowBox[{
          "tretia", " ", "!=", " ", "\"\<\[CapitalZHacek]iadna\>\""}]}], ",", 
         "\n", "            ", 
         RowBox[{"numTransformations", " ", "=", " ", 
          RowBox[{"1", " ", "+", " ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
             "druha", " ", "!=", " ", "\"\<\[CapitalZHacek]iadna\>\""}], ",", 
             " ", "1", ",", " ", "0"}], "]"}], " ", "+", " ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
             "tretia", " ", "!=", " ", "\"\<\[CapitalZHacek]iadna\>\""}], ",",
              " ", "1", ",", " ", "0"}], "]"}]}]}]}], "\n", "        ", "}"}],
        ",", "\n", "            ", "\n", "            ", "\n", "            ", 
       RowBox[{
        RowBox[{"allPoints", " ", "=", " ", 
         RowBox[{"{", 
          RowBox[{"pociatocny", ",", " ", "druhy"}], "}"}]}], ";", "\n", 
        "            ", 
        RowBox[{"If", "[", 
         RowBox[{"showSecond", ",", " ", 
          RowBox[{"AppendTo", "[", 
           RowBox[{"allPoints", ",", " ", "treti"}], "]"}]}], "]"}], ";", 
        "\n", "            ", 
        RowBox[{"If", "[", 
         RowBox[{"showThird", ",", " ", 
          RowBox[{"AppendTo", "[", 
           RowBox[{"allPoints", ",", " ", "finalny"}], "]"}]}], "]"}], ";", 
        "\n", "            ", "\n", "            ", 
        RowBox[{"xMin", " ", "=", " ", 
         RowBox[{"Min", "[", 
          RowBox[{"N", "[", 
           RowBox[{"allPoints", "[", 
            RowBox[{"[", 
             RowBox[{"All", ",", " ", "1"}], "]"}], "]"}], "]"}], "]"}]}], 
        ";", "\n", "            ", 
        RowBox[{"xMax", " ", "=", " ", 
         RowBox[{"Max", "[", 
          RowBox[{"N", "[", 
           RowBox[{"allPoints", "[", 
            RowBox[{"[", 
             RowBox[{"All", ",", " ", "1"}], "]"}], "]"}], "]"}], "]"}]}], 
        ";", "\n", "            ", 
        RowBox[{"yMin", " ", "=", " ", 
         RowBox[{"Min", "[", 
          RowBox[{"N", "[", 
           RowBox[{"allPoints", "[", 
            RowBox[{"[", 
             RowBox[{"All", ",", " ", "2"}], "]"}], "]"}], "]"}], "]"}]}], 
        ";", "\n", "            ", 
        RowBox[{"yMax", " ", "=", " ", 
         RowBox[{"Max", "[", 
          RowBox[{"N", "[", 
           RowBox[{"allPoints", "[", 
            RowBox[{"[", 
             RowBox[{"All", ",", " ", "2"}], "]"}], "]"}], "]"}], "]"}]}], 
        ";", "\n", "            ", "\n", "            ", "\n", "            ", 
        RowBox[{"xRange", " ", "=", " ", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"xMin", " ", "-", " ", "padding"}], ",", " ", 
           RowBox[{"xMax", " ", "+", " ", "padding"}]}], "}"}]}], ";", "\n", 
        "            ", 
        RowBox[{"yRange", " ", "=", " ", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"yMin", " ", "-", " ", "padding"}], ",", " ", 
           RowBox[{"yMax", " ", "+", " ", "padding"}]}], "}"}]}], ";", "\n", 
        "            ", "\n", "            ", 
        RowBox[{"maxRange", " ", "=", " ", 
         RowBox[{"Max", "[", 
          RowBox[{
           RowBox[{
            RowBox[{"xRange", "[", 
             RowBox[{"[", "2", "]"}], "]"}], " ", "-", " ", 
            RowBox[{"xRange", "[", 
             RowBox[{"[", "1", "]"}], "]"}]}], ",", " ", 
           RowBox[{
            RowBox[{"yRange", "[", 
             RowBox[{"[", "2", "]"}], "]"}], " ", "-", " ", 
            RowBox[{"yRange", "[", 
             RowBox[{"[", "1", "]"}], "]"}]}]}], "]"}]}], ";", "\n", 
        "            ", 
        RowBox[{"xMid", " ", "=", " ", 
         RowBox[{"Mean", "[", "xRange", "]"}]}], ";", "\n", "            ", 
        RowBox[{"yMid", " ", "=", " ", 
         RowBox[{"Mean", "[", "yRange", "]"}]}], ";", "\n", "            ", 
        "\n", "            ", 
        RowBox[{"xRange", " ", "=", " ", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"xMid", " ", "-", " ", 
            RowBox[{"maxRange", "/", "2"}]}], ",", " ", 
           RowBox[{"xMid", " ", "+", " ", 
            RowBox[{"maxRange", "/", "2"}]}]}], "}"}]}], ";", "\n", 
        "            ", 
        RowBox[{"yRange", " ", "=", " ", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"yMid", " ", "-", " ", 
            RowBox[{"maxRange", "/", "2"}]}], ",", " ", 
           RowBox[{"yMid", " ", "+", " ", 
            RowBox[{"maxRange", "/", "2"}]}]}], "}"}]}], ";", "\n", 
        "            ", "\n", "            ", "\n", "            ", 
        RowBox[{"labelPositions", " ", "=", " ", 
         RowBox[{"{", "}"}]}], ";", "\n", "            ", "\n", 
        "            ", "\n", "            ", 
        RowBox[{"Do", "[", "\n", "                ", 
         RowBox[{
          RowBox[{"Module", "[", 
           RowBox[{
            RowBox[{"{", 
             RowBox[{
              RowBox[{"point", " ", "=", " ", 
               RowBox[{"allPoints", "[", 
                RowBox[{"[", "i", "]"}], "]"}]}], ",", " ", 
              RowBox[{"offset", " ", "=", " ", 
               RowBox[{"{", 
                RowBox[{"0.35", ",", " ", "0.35"}], "}"}]}]}], "}"}], ",", 
            "\n", "                    ", 
            RowBox[{
             RowBox[{"If", "[", 
              RowBox[{
               RowBox[{
                RowBox[{"N", "[", 
                 RowBox[{"point", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], "]"}], " ", ">", " ", "0"}],
                ",", " ", 
               RowBox[{
                RowBox[{"offset", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], " ", "=", " ", "0.35"}], ",",
                " ", 
               RowBox[{
                RowBox[{"offset", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], " ", "=", " ", 
                RowBox[{"-", "0.35"}]}]}], "]"}], ";", "\n", 
             "                    ", 
             RowBox[{"If", "[", 
              RowBox[{
               RowBox[{
                RowBox[{"N", "[", 
                 RowBox[{"point", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "]"}], " ", ">", " ", "0"}],
                ",", " ", 
               RowBox[{
                RowBox[{"offset", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], " ", "=", " ", "0.35"}], ",",
                " ", 
               RowBox[{
                RowBox[{"offset", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], " ", "=", " ", 
                RowBox[{"-", "0.35"}]}]}], "]"}], ";", "\n", 
             "                    ", 
             RowBox[{"offset", " ", "=", " ", 
              RowBox[{"offset", " ", "*", " ", 
               RowBox[{"(", 
                RowBox[{"0.8", " ", "+", " ", 
                 RowBox[{"0.07", " ", "*", " ", 
                  RowBox[{"EuclideanDistance", "[", 
                   RowBox[{"point", ",", " ", 
                    RowBox[{"{", 
                    RowBox[{"0", ",", " ", "0"}], "}"}]}], "]"}]}]}], 
                ")"}]}]}], ";", "\n", "                    ", 
             RowBox[{"AppendTo", "[", 
              RowBox[{"labelPositions", ",", " ", 
               RowBox[{"point", " ", "+", " ", "offset"}]}], "]"}], ";"}]}], 
           "\n", "                ", "]"}], ",", "\n", "                ", 
          RowBox[{"{", 
           RowBox[{"i", ",", " ", 
            RowBox[{"Length", "[", "allPoints", "]"}]}], "}"}]}], "\n", 
         "            ", "]"}], ";", "\n", "            ", "\n", 
        "            ", 
        RowBox[{"graphicsElements", " ", "=", " ", 
         RowBox[{"{", "\n", "                ", "\n", "                ", 
          RowBox[{
          "lightGray", ",", " ", "Thin", ",", "\n", "                ", 
           RowBox[{"Table", "[", 
            RowBox[{
             RowBox[{"Line", "[", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"xRange", "[", 
                   RowBox[{"[", "1", "]"}], "]"}], ",", " ", "y"}], "}"}], 
                ",", " ", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"xRange", "[", 
                   RowBox[{"[", "2", "]"}], "]"}], ",", " ", "y"}], "}"}]}], 
               "}"}], "]"}], ",", " ", "\n", "                        ", 
             RowBox[{"{", 
              RowBox[{"y", ",", " ", 
               RowBox[{"Ceiling", "[", 
                RowBox[{"yRange", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", " ", 
               RowBox[{"Floor", "[", 
                RowBox[{"yRange", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "}"}]}], "]"}], ",",
            "\n", "                ", 
           RowBox[{"Table", "[", 
            RowBox[{
             RowBox[{"Line", "[", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{"{", 
                 RowBox[{"x", ",", " ", 
                  RowBox[{"yRange", "[", 
                   RowBox[{"[", "1", "]"}], "]"}]}], "}"}], ",", " ", 
                RowBox[{"{", 
                 RowBox[{"x", ",", " ", 
                  RowBox[{"yRange", "[", 
                   RowBox[{"[", "2", "]"}], "]"}]}], "}"}]}], "}"}], "]"}], 
             ",", " ", "\n", "                        ", 
             RowBox[{"{", 
              RowBox[{"x", ",", " ", 
               RowBox[{"Ceiling", "[", 
                RowBox[{"xRange", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", " ", 
               RowBox[{"Floor", "[", 
                RowBox[{"xRange", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "}"}]}], "]"}], ",",
            "\n", "                ", "\n", "                ", "\n", 
           "                ", "Black", ",", " ", 
           RowBox[{"Thickness", "[", "0.003", "]"}], ",", " ", 
           RowBox[{"Arrowheads", "[", "0.02", "]"}], ",", "\n", 
           "                ", 
           RowBox[{"Arrow", "[", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{
                RowBox[{"xRange", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], ",", " ", "0"}], "}"}], ",", 
              " ", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{"xRange", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], ",", " ", "0"}], "}"}]}], 
             "}"}], "]"}], ",", "\n", "                ", 
           RowBox[{"Arrow", "[", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{"0", ",", " ", 
                RowBox[{"yRange", "[", 
                 RowBox[{"[", "1", "]"}], "]"}]}], "}"}], ",", " ", 
              RowBox[{"{", 
               RowBox[{"0", ",", " ", 
                RowBox[{"yRange", "[", 
                 RowBox[{"[", "2", "]"}], "]"}]}], "}"}]}], "}"}], "]"}], ",",
            "\n", "                ", 
           RowBox[{"Text", "[", 
            RowBox[{
             RowBox[{"Style", "[", 
              RowBox[{"\"\<x\>\"", ",", " ", "Bold", ",", " ", "14"}], "]"}], 
             ",", " ", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{
                RowBox[{"xRange", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], " ", "-", " ", "0.3"}], ",", 
               " ", 
               RowBox[{"-", "0.3"}]}], "}"}]}], "]"}], ",", "\n", 
           "                ", 
           RowBox[{"Text", "[", 
            RowBox[{
             RowBox[{"Style", "[", 
              RowBox[{"\"\<y\>\"", ",", " ", "Bold", ",", " ", "14"}], "]"}], 
             ",", " ", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"-", "0.3"}], ",", " ", 
               RowBox[{
                RowBox[{"yRange", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], " ", "-", " ", "0.3"}]}], 
              "}"}]}], "]"}], ",", "\n", "                ", "\n", 
           "                ", "\n", "                ", 
           RowBox[{"Table", "[", "\n", "                    ", 
            RowBox[{
             RowBox[{"{", "\n", "                        ", 
              RowBox[{
               RowBox[{"Line", "[", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"{", 
                   RowBox[{"i", ",", " ", 
                    RowBox[{"-", "0.1"}]}], "}"}], ",", " ", 
                  RowBox[{"{", 
                   RowBox[{"i", ",", " ", "0.1"}], "}"}]}], "}"}], "]"}], ",",
                "\n", "                        ", 
               RowBox[{"Text", "[", 
                RowBox[{
                 RowBox[{"Style", "[", 
                  RowBox[{"i", ",", " ", "10"}], "]"}], ",", " ", 
                 RowBox[{"{", 
                  RowBox[{"i", ",", " ", 
                   RowBox[{"-", "0.3"}]}], "}"}]}], "]"}]}], "\n", 
              "                    ", "}"}], ",", "\n", 
             "                    ", 
             RowBox[{"{", 
              RowBox[{"i", ",", " ", 
               RowBox[{"Ceiling", "[", 
                RowBox[{"xRange", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", " ", 
               RowBox[{"Floor", "[", 
                RowBox[{"xRange", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "}"}]}], "\n", 
            "                ", "]"}], ",", "\n", "                ", 
           RowBox[{"Table", "[", "\n", "                    ", 
            RowBox[{
             RowBox[{"{", "\n", "                        ", 
              RowBox[{
               RowBox[{"Line", "[", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"{", 
                   RowBox[{
                    RowBox[{"-", "0.1"}], ",", " ", "i"}], "}"}], ",", " ", 
                  RowBox[{"{", 
                   RowBox[{"0.1", ",", " ", "i"}], "}"}]}], "}"}], "]"}], ",",
                "\n", "                        ", 
               RowBox[{"Text", "[", 
                RowBox[{
                 RowBox[{"Style", "[", 
                  RowBox[{"i", ",", " ", "10"}], "]"}], ",", " ", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"-", "0.3"}], ",", " ", "i"}], "}"}]}], "]"}]}], 
              "\n", "                    ", "}"}], ",", "\n", 
             "                    ", 
             RowBox[{"{", 
              RowBox[{"i", ",", " ", 
               RowBox[{"Ceiling", "[", 
                RowBox[{"yRange", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", " ", 
               RowBox[{"Floor", "[", 
                RowBox[{"yRange", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "}"}]}], "\n", 
            "                ", "]"}]}], "\n", "            ", "}"}]}], ";", 
        "\n", "            ", "\n", "            ", "\n", "            ", 
        "\n", "            ", 
        RowBox[{"AppendTo", "[", 
         RowBox[{"graphicsElements", ",", " ", "\n", "                ", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{"Opacity", "[", "0.3", "]"}], ",", " ", "Dashed", ",", 
            " ", 
            RowBox[{"Thickness", "[", "0.002", "]"}], ",", " ", "brightGreen",
             ",", "\n", "                    ", 
            RowBox[{"Line", "[", 
             RowBox[{"{", 
              RowBox[{"pociatocny", ",", " ", "druhy"}], "}"}], "]"}]}], "\n",
            "                ", "}"}]}], "\n", "            ", "]"}], ";", 
        "\n", "            ", "\n", "            ", "\n", "            ", 
        RowBox[{"If", "[", 
         RowBox[{"showSecond", ",", "\n", "                ", 
          RowBox[{
           RowBox[{"AppendTo", "[", 
            RowBox[{
            "graphicsElements", ",", " ", "\n", "                    ", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"Opacity", "[", "0.3", "]"}], ",", " ", "Dashed", ",", 
               " ", 
               RowBox[{"Thickness", "[", "0.002", "]"}], ",", " ", 
               "brightOrange", ",", "\n", "                        ", 
               RowBox[{"Line", "[", 
                RowBox[{"{", 
                 RowBox[{"druhy", ",", " ", "treti"}], "}"}], "]"}]}], "\n", 
              "                    ", "}"}]}], "\n", "                ", 
            "]"}], ";"}]}], "\n", "            ", "]"}], ";", "\n", 
        "            ", "\n", "            ", "\n", "            ", 
        RowBox[{"If", "[", 
         RowBox[{"showThird", ",", "\n", "                ", 
          RowBox[{
           RowBox[{"AppendTo", "[", 
            RowBox[{
            "graphicsElements", ",", " ", "\n", "                    ", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"Opacity", "[", "0.3", "]"}], ",", " ", "Dashed", ",", 
               " ", 
               RowBox[{"Thickness", "[", "0.002", "]"}], ",", " ", 
               "brightRed", ",", "\n", "                        ", 
               RowBox[{"Line", "[", 
                RowBox[{"{", 
                 RowBox[{"treti", ",", " ", "finalny"}], "}"}], "]"}]}], "\n",
               "                    ", "}"}]}], "\n", "                ", 
            "]"}], ";"}]}], "\n", "            ", "]"}], ";", "\n", 
        "            ", "\n", "            ", "\n", "            ", "\n", 
        "            ", 
        RowBox[{"AppendTo", "[", 
         RowBox[{"graphicsElements", ",", " ", "\n", "                ", 
          RowBox[{"{", 
           RowBox[{"White", ",", " ", 
            RowBox[{"Disk", "[", 
             RowBox[{"pociatocny", ",", " ", "0.15"}], "]"}], ",", "\n", 
            "                 ", "brightBlue", ",", " ", 
            RowBox[{"Disk", "[", 
             RowBox[{"pociatocny", ",", " ", "0.12"}], "]"}]}], "}"}]}], "\n",
          "            ", "]"}], ";", "\n", "            ", "\n", 
        "            ", "\n", "            ", 
        RowBox[{"AppendTo", "[", 
         RowBox[{"graphicsElements", ",", " ", "\n", "                ", 
          RowBox[{"{", 
           RowBox[{"White", ",", " ", 
            RowBox[{"Disk", "[", 
             RowBox[{"druhy", ",", " ", "0.15"}], "]"}], ",", "\n", 
            "                 ", "brightGreen", ",", " ", 
            RowBox[{"Disk", "[", 
             RowBox[{"druhy", ",", " ", "0.12"}], "]"}]}], "}"}]}], "\n", 
         "            ", "]"}], ";", "\n", "            ", "\n", 
        "            ", "\n", "            ", 
        RowBox[{"If", "[", 
         RowBox[{"showSecond", ",", "\n", "                ", 
          RowBox[{
           RowBox[{"AppendTo", "[", 
            RowBox[{
            "graphicsElements", ",", " ", "\n", "                    ", 
             RowBox[{"{", 
              RowBox[{"White", ",", " ", 
               RowBox[{"Disk", "[", 
                RowBox[{"treti", ",", " ", "0.15"}], "]"}], ",", "\n", 
               "                     ", "brightOrange", ",", " ", 
               RowBox[{"Disk", "[", 
                RowBox[{"treti", ",", " ", "0.12"}], "]"}]}], "}"}]}], "\n", 
            "                ", "]"}], ";"}]}], "\n", "            ", "]"}], 
        ";", "\n", "            ", "\n", "            ", "\n", "            ", 
        RowBox[{"If", "[", 
         RowBox[{"showThird", ",", "\n", "                ", 
          RowBox[{
           RowBox[{"AppendTo", "[", 
            RowBox[{
            "graphicsElements", ",", " ", "\n", "                    ", 
             RowBox[{"{", 
              RowBox[{"White", ",", " ", 
               RowBox[{"Disk", "[", 
                RowBox[{"finalny", ",", " ", "0.15"}], "]"}], ",", "\n", 
               "                     ", "brightRed", ",", " ", 
               RowBox[{"Disk", "[", 
                RowBox[{"finalny", ",", " ", "0.12"}], "]"}]}], "}"}]}], "\n",
             "                ", "]"}], ";"}]}], "\n", "            ", "]"}], 
        ";", "\n", "            ", "\n", "            ", "\n", "            ", 
        RowBox[{"pointLabels", " ", "=", " ", 
         RowBox[{"{", "}"}]}], ";", "\n", "            ", "\n", 
        "            ", "\n", "            ", 
        RowBox[{"With", "[", 
         RowBox[{
          RowBox[{"{", "\n", "                ", 
           RowBox[{
            RowBox[{"label", " ", "=", " ", "\"\<P\>\""}], ",", "\n", 
            "                ", 
            RowBox[{"color", " ", "=", " ", "brightBlue"}], ",", "\n", 
            "                ", 
            RowBox[{"pos", " ", "=", " ", 
             RowBox[{"labelPositions", "[", 
              RowBox[{"[", "1", "]"}], "]"}]}]}], "\n", "            ", "}"}],
           ",", "\n", "                ", 
          RowBox[{
           RowBox[{"AppendTo", "[", 
            RowBox[{"pointLabels", ",", " ", "\n", "                    ", 
             RowBox[{"{", "\n", "                        ", 
              RowBox[{"White", ",", " ", 
               RowBox[{"Disk", "[", 
                RowBox[{"pos", ",", " ", "0.2"}], "]"}], ",", "\n", 
               "                        ", "color", ",", " ", "\n", 
               "                        ", 
               RowBox[{"Text", "[", 
                RowBox[{
                 RowBox[{"Style", "[", 
                  RowBox[{"label", ",", " ", "Bold", ",", " ", "14"}], "]"}], 
                 ",", " ", "pos"}], "]"}]}], "\n", "                    ", 
              "}"}]}], "\n", "                ", "]"}], ";"}]}], "\n", 
         "            ", "]"}], ";", "\n", "            ", "\n", 
        "            ", "\n", "            ", 
        RowBox[{"With", "[", 
         RowBox[{
          RowBox[{"{", "\n", "                ", 
           RowBox[{
            RowBox[{"label", " ", "=", " ", "\"\<P'\>\""}], ",", "\n", 
            "                ", 
            RowBox[{"color", " ", "=", " ", "brightGreen"}], ",", "\n", 
            "                ", 
            RowBox[{"pos", " ", "=", " ", 
             RowBox[{"labelPositions", "[", 
              RowBox[{"[", "2", "]"}], "]"}]}]}], "\n", "            ", "}"}],
           ",", "\n", "                ", 
          RowBox[{
           RowBox[{"AppendTo", "[", 
            RowBox[{"pointLabels", ",", " ", "\n", "                    ", 
             RowBox[{"{", "\n", "                        ", 
              RowBox[{"White", ",", " ", 
               RowBox[{"Disk", "[", 
                RowBox[{"pos", ",", " ", "0.2"}], "]"}], ",", "\n", 
               "                        ", "color", ",", " ", "\n", 
               "                        ", 
               RowBox[{"Text", "[", 
                RowBox[{
                 RowBox[{"Style", "[", 
                  RowBox[{"label", ",", " ", "Bold", ",", " ", "14"}], "]"}], 
                 ",", " ", "pos"}], "]"}]}], "\n", "                    ", 
              "}"}]}], "\n", "                ", "]"}], ";"}]}], "\n", 
         "            ", "]"}], ";", "\n", "            ", "\n", 
        "            ", "\n", "            ", 
        RowBox[{"If", "[", 
         RowBox[{"showSecond", ",", "\n", "                ", 
          RowBox[{
           RowBox[{"With", "[", 
            RowBox[{
             RowBox[{"{", "\n", "                    ", 
              RowBox[{
               RowBox[{"label", " ", "=", " ", "\"\<P''\>\""}], ",", "\n", 
               "                    ", 
               RowBox[{"color", " ", "=", " ", "brightOrange"}], ",", "\n", 
               "                    ", 
               RowBox[{"pos", " ", "=", " ", 
                RowBox[{"labelPositions", "[", 
                 RowBox[{"[", "3", "]"}], "]"}]}]}], "\n", "                ",
               "}"}], ",", "\n", "                    ", 
             RowBox[{
              RowBox[{"AppendTo", "[", 
               RowBox[{
               "pointLabels", ",", " ", "\n", "                        ", 
                RowBox[{"{", "\n", "                            ", 
                 RowBox[{"White", ",", " ", 
                  RowBox[{"Disk", "[", 
                   RowBox[{"pos", ",", " ", "0.2"}], "]"}], ",", "\n", 
                  "                            ", "color", ",", " ", "\n", 
                  "                            ", 
                  RowBox[{"Text", "[", 
                   RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{"label", ",", " ", "Bold", ",", " ", "14"}], 
                    "]"}], ",", " ", "pos"}], "]"}]}], "\n", 
                 "                        ", "}"}]}], "\n", 
               "                    ", "]"}], ";"}]}], "\n", 
            "                ", "]"}], ";"}]}], "\n", "            ", "]"}], 
        ";", "\n", "            ", "\n", "            ", "\n", "            ", 
        RowBox[{"If", "[", 
         RowBox[{"showThird", ",", "\n", "                ", 
          RowBox[{
           RowBox[{"With", "[", 
            RowBox[{
             RowBox[{"{", "\n", "                    ", 
              RowBox[{
               RowBox[{"label", " ", "=", " ", "\"\<P'''\>\""}], ",", "\n", 
               "                    ", 
               RowBox[{"color", " ", "=", " ", "brightRed"}], ",", "\n", 
               "                    ", 
               RowBox[{"pos", " ", "=", " ", 
                RowBox[{"labelPositions", "[", 
                 RowBox[{"[", "4", "]"}], "]"}]}]}], "\n", "                ",
               "}"}], ",", "\n", "                    ", 
             RowBox[{
              RowBox[{"AppendTo", "[", 
               RowBox[{
               "pointLabels", ",", " ", "\n", "                        ", 
                RowBox[{"{", "\n", "                            ", 
                 RowBox[{"White", ",", " ", 
                  RowBox[{"Disk", "[", 
                   RowBox[{"pos", ",", " ", "0.2"}], "]"}], ",", "\n", 
                  "                            ", "color", ",", " ", "\n", 
                  "                            ", 
                  RowBox[{"Text", "[", 
                   RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{"label", ",", " ", "Bold", ",", " ", "14"}], 
                    "]"}], ",", " ", "pos"}], "]"}]}], "\n", 
                 "                        ", "}"}]}], "\n", 
               "                    ", "]"}], ";"}]}], "\n", 
            "                ", "]"}], ";"}]}], "\n", "            ", "]"}], 
        ";", "\n", "            ", "\n", "            ", 
        RowBox[{"AppendTo", "[", 
         RowBox[{"graphicsElements", ",", " ", "pointLabels"}], "]"}], ";", 
        "\n", "            ", "\n", "            ", "\n", "            ", 
        RowBox[{"Graphics", "[", 
         RowBox[{"graphicsElements", ",", "\n", "              ", 
          RowBox[{"PlotRange", " ", "->", " ", 
           RowBox[{"{", 
            RowBox[{"xRange", ",", " ", "yRange"}], "}"}]}], ",", "\n", 
          "              ", 
          RowBox[{"AspectRatio", " ", "->", " ", "1"}], ",", "\n", 
          "              ", 
          RowBox[{"ImageSize", " ", "->", " ", "650"}], ",", "\n", 
          "              ", 
          RowBox[{"PlotLabel", " ", "->", " ", 
           RowBox[{"Style", "[", 
            RowBox[{
             RowBox[{
             "\"\<Postupn\[AAcute] aplik\[AAcute]cia \>\"", " ", "<>", " ", 
              RowBox[{"ToString", "[", "numTransformations", "]"}], " ", "<>",
               " ", "\n", "                           ", 
              RowBox[{"Switch", "[", 
               RowBox[{
               "numTransformations", ",", " ", "\n", 
                "                               ", "1", ",", " ", 
                "\"\< transform\[AAcute]cie\>\"", ",", " ", "\n", 
                "                               ", "_", ",", " ", 
                "\"\< transform\[AAcute]ci\[IAcute]\>\""}], "]"}], " ", "<>", 
              " ", "\"\< bodu\>\""}], ",", " ", "Bold", ",", " ", "16"}], 
            "]"}]}], ",", "\n", "              ", 
          RowBox[{"ImagePadding", " ", "->", " ", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"{", 
              RowBox[{"40", ",", " ", "40"}], "}"}], ",", " ", 
             RowBox[{"{", 
              RowBox[{"40", ",", " ", "40"}], "}"}]}], "}"}]}], ",", "\n", 
          "              ", 
          RowBox[{"Background", " ", "->", " ", "White"}], ",", "\n", 
          "              ", 
          RowBox[{"Method", " ", "->", " ", 
           RowBox[{"{", 
            RowBox[{"\"\<ShrinkWrap\>\"", " ", "->", " ", "True"}], "}"}]}]}],
          "]"}]}]}], "\n", "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodTrojitaTransformaciaSVysledkom", "[", "]"}], " ", ":=", " ", 
     
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "result", "}"}], ",", "\n", "   ", "\n", "   ", 
       RowBox[{
        RowBox[{"result", " ", "=", " ", 
         RowBox[{"Quiet", "[", 
          RowBox[{
           RowBox[{"BodTrojitaTransformacia", "[", "]"}], ",", " ", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"Power", "::", "infy"}], ",", " ", 
             RowBox[{"Infinity", "::", "indet"}]}], "}"}]}], "]"}]}], ";", 
        "\n", "   ", "\n", "   ", "\n", "   ", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{"result", " ", "=!=", " ", "Null"}], ",", "\n", "       ", 
          RowBox[{"Check", "[", "\n", "           ", 
           RowBox[{
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{"Style", "[", 
               RowBox[{
               "\"\<\\nZ\[CapitalAAcute]VERE\[CapitalCHacek]N\[CapitalYAcute] \
PREH\:013dAD:\>\"", ",", " ", "Bold", ",", " ", "16"}], "]"}], "]"}], ";", 
             "\n", "           ", 
             RowBox[{"Print", "[", 
              RowBox[{
              "\"\<Vybrat\[AAcute] obtia\[ZHacek]nos\[THacek]: \>\"", ",", 
               " ", 
               RowBox[{"Style", "[", "\n", "               ", 
                RowBox[{
                 RowBox[{"Switch", "[", 
                  RowBox[{
                   RowBox[{"result", "[", 
                    RowBox[{"[", "4", "]"}], "]"}], ",", "\n", 
                   "                   ", "\"\<Easy\>\"", ",", " ", 
                   "\"\<Jednoduch\[AAcute] (1 transform\[AAcute]cia)\>\"", 
                   ",", "\n", "                   ", "\"\<Medium\>\"", ",", 
                   " ", "\"\<Stredn\[AAcute] (2 transform\[AAcute]cie)\>\"", 
                   ",", "\n", "                   ", "\"\<Hard\>\"", ",", 
                   " ", "\"\<Zlo\[ZHacek]it\[AAcute] (3 \
transform\[AAcute]cie)\>\""}], "\n", "               ", "]"}], ",", " ", 
                 "Bold"}], "]"}]}], "]"}], ";", "\n", "           ", 
             RowBox[{
             "Print", "[", 
              "\"\<Vykonali ste tieto transform\[AAcute]cie:\>\"", "]"}], ";",
              "\n", "           ", "\n", "           ", 
             RowBox[{"Do", "[", "\n", "               ", 
              RowBox[{
               RowBox[{"Print", "[", 
                RowBox[{"i", ",", " ", "\"\<. \>\"", ",", " ", 
                 RowBox[{"result", "[", 
                  RowBox[{"[", 
                   RowBox[{"3", ",", " ", "i"}], "]"}], "]"}]}], "]"}], ",", 
               "\n", "               ", 
               RowBox[{"{", 
                RowBox[{"i", ",", " ", "1", ",", " ", 
                 RowBox[{"Length", "[", 
                  RowBox[{"result", "[", 
                   RowBox[{"[", "3", "]"}], "]"}], "]"}]}], "}"}]}], "\n", 
              "           ", "]"}], ";", "\n", "           ", "\n", 
             "           ", 
             RowBox[{
             "Print", "[", "\"\<\\nV\[YAcute]sledn\[YAcute] bod:\>\"", "]"}], 
             ";", "\n", "           ", 
             RowBox[{"Print", "[", 
              RowBox[{"result", "[", 
               RowBox[{"[", 
                RowBox[{"2", ",", " ", 
                 RowBox[{"-", "1"}]}], "]"}], "]"}], "]"}]}], ",", "\n", 
            "           ", "\n", "           ", "\n", "           ", 
            RowBox[{"Print", "[", 
             RowBox[{"Style", "[", 
              RowBox[{
              "\"\<Nastala chyba pri zobrazovan\[IAcute] v\[YAcute]sledkov.\>\
\"", ",", " ", "Red", ",", " ", "Bold"}], "]"}], "]"}]}], "\n", "       ", 
           "]"}]}], "\n", "   ", "]"}], ";", "\n", "   ", "\n", "   ", 
        "Null"}]}], "\n", "]"}]}], ";"}], "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"DisplayComplexAssignment", "[", 
      RowBox[{
      "pociatocny_", ",", " ", "prva_", ",", " ", "druha_", ",", " ", 
       "tretia_"}], "]"}], " ", ":=", " ", "\n", "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "}"}], ",", "\n", "        ", 
       RowBox[{
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<KOMPLEXN\[CapitalEAcute] ZADANIE PRE \[CapitalZHacek]IAKA\>\"",
            ",", " ", "Bold", ",", " ", "24"}], "]"}], "]"}], ";", "\n", 
        "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<==========================================\>\"", ",", " ", 
           "Bold"}], "]"}], "]"}], ";", "\n", "        ", "\n", "        ", 
        "\n", "        ", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{"prva", " ", "==", " ", "\"\<Posun\>\""}], ",", "\n", 
          "            ", "\n", "            ", 
          RowBox[{"Module", "[", 
           RowBox[{
            RowBox[{"{", 
             RowBox[{
             "posunVector", " ", "=", " ", 
              "BodHardBalik`Transforms`Posun`BodPosunVector"}], "}"}], ",", 
            "\n", "                ", 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{"Style", "[", 
               RowBox[{
               "\"\<\\nVykonajte postupne tieto tri \
transform\[AAcute]cie:\>\"", ",", " ", "Bold", ",", " ", "16"}], "]"}], "]"}],
              ";", "\n", "                ", 
             RowBox[{"Print", "[", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{
                "\"\<1. \>\"", " ", "<>", " ", "prva", " ", "<>", " ", 
                 "\"\< o \>\"", " ", "<>", " ", 
                 RowBox[{"ToString", "[", "posunVector", "]"}]}], ",", " ", 
                "Bold", ",", " ", "Blue"}], "]"}], "]"}], ";", "\n", 
             "                ", 
             RowBox[{"Print", "[", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"\"\<2. \>\"", " ", "<>", " ", "druha"}], ",", " ", 
                "Bold", ",", " ", "Blue"}], "]"}], "]"}], ";", "\n", 
             "                ", 
             RowBox[{"Print", "[", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"\"\<3. \>\"", " ", "<>", " ", "tretia"}], ",", " ", 
                "Bold", ",", " ", "Blue"}], "]"}], "]"}], ";"}]}], "\n", 
           "            ", "]"}], "\n", "        ", ",", "\n", "            ",
           "\n", "            ", 
          RowBox[{
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
             "\"\<\\nVykonajte postupne tieto tri transform\[AAcute]cie:\>\"",
               ",", " ", "Bold", ",", " ", "16"}], "]"}], "]"}], ";", "\n", 
           "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
              RowBox[{"\"\<1. \>\"", " ", "<>", " ", "prva"}], ",", " ", 
              "Bold", ",", " ", "Blue"}], "]"}], "]"}], ";", "\n", 
           "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
              RowBox[{"\"\<2. \>\"", " ", "<>", " ", "druha"}], ",", " ", 
              "Bold", ",", " ", "Blue"}], "]"}], "]"}], ";", "\n", 
           "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
              RowBox[{"\"\<3. \>\"", " ", "<>", " ", "tretia"}], ",", " ", 
              "Bold", ",", " ", "Blue"}], "]"}], "]"}], ";"}]}], "\n", 
         "        ", "]"}], ";", "\n", "        ", "\n", "        ", "\n", 
        "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nDAN\[CapitalEAcute]:\>\"", ",", " ", "Bold", ",", " ", 
           "16"}], "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<M\[AAcute]te bod P so s\[UAcute]radnicami:\>\"", ",", " ", 
           "Bold", ",", " ", "14"}], "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{"pociatocny", ",", " ", "Blue"}], "]"}], "]"}], ";", "\n", 
        "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{"\"\<\\nPOSTUP:\>\"", ",", " ", "Bold", ",", " ", "16"}], 
          "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<1. Aplikujte prv\[UAcute] transform\[AAcute]ciu na p\[OHat]vodn\
\[YAcute] bod P, \[CHacek]\[IAcute]m z\[IAcute]skate bod P'\>\"", "]"}], ";", 
        "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<2. Aplikujte druh\[UAcute] transform\[AAcute]ciu na bod P', \
\[CHacek]\[IAcute]m z\[IAcute]skate bod P''\>\"", "]"}], ";", "\n", 
        "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<3. Aplikujte tretiu transform\[AAcute]ciu na bod P'', \[CHacek]\
\[IAcute]m z\[IAcute]skate fin\[AAcute]lny bod P'''\>\"", "]"}], ";", "\n", 
        "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nPOZN\[CapitalAAcute]MKA:\>\"", ",", " ", "Bold", ",", " ", 
           "14"}], "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<\[Bullet] Pre ka\[ZHacek]d\[UAcute] transform\[AAcute]ciu si \
zap\[IAcute]\[SHacek]te pr\[IAcute]slu\[SHacek]n\[UAcute] \
transforma\[CHacek]n\[UAcute] maticu\>\"", "]"}], ";", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<\[Bullet] Vypo\[CHacek]\[IAcute]tajte nov\[EAcute] \
s\[UAcute]radnice bodu po ka\[ZHacek]dej transform\[AAcute]cii\>\"", "]"}], 
        ";", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<\[Bullet] Nezabudnite na spr\[AAcute]vne pou\[ZHacek]itie homog\
\[EAcute]nnych s\[UAcute]radn\[IAcute]c pre v\[SHacek]etky v\[YAcute]po\
\[CHacek]ty\>\"", "]"}], ";", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nPo stla\[CHacek]en\[IAcute] tla\[CHacek]idla \
pokra\[CHacek]ova\[THacek] sa zobraz\[IAcute] podrobn\[YAcute] postup v\
\[YAcute]po\[CHacek]tu.\>\"", ",", " ", "Bold", ",", " ", "12"}], "]"}], 
         "]"}], ";"}]}], "\n", "    ", "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"SelectTransformation", "[", 
      RowBox[{"message_", ",", " ", 
       RowBox[{"exclude1_", ":", "\"\<\>\""}], ",", " ", 
       RowBox[{"exclude2_", ":", "\"\<\>\""}]}], "]"}], " ", ":=", " ", "\n", 
     "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
        "options", ",", " ", "fullOptions", ",", " ", "formattedChoices", ",",
          " ", "result"}], "}"}], ",", "\n", "        ", 
       RowBox[{
        RowBox[{"options", " ", "=", " ", 
         RowBox[{"DeleteCases", "[", "\n", "            ", 
          RowBox[{
           RowBox[{"{", "\n", "                ", 
            RowBox[{
             RowBox[{"\"\<Posun\>\"", " ", "->", " ", 
              RowBox[{"{", 
               RowBox[{
               "\"\<Posun\>\"", ",", " ", 
                "\"\<Posun bodu vo smere vektora\>\""}], "}"}]}], ",", "\n", 
             "                ", 
             RowBox[{"\"\<Rot\[AAcute]cia\>\"", " ", "->", " ", 
              RowBox[{"{", 
               RowBox[{
               "\"\<Rot\[AAcute]cia\>\"", ",", " ", 
                "\"\<Rot\[AAcute]cia bodu okolo po\[CHacek]iatku \
s\[UAcute]radnicovej s\[UAcute]stavy\>\""}], "}"}]}], ",", "\n", 
             "                ", 
             RowBox[{"\"\<Symetria\>\"", " ", "->", " ", 
              RowBox[{"{", 
               RowBox[{
               "\"\<Symetria\>\"", ",", " ", 
                "\"\<Zrkadlenie bodu pod\:013ea zvolenej osi\>\""}], 
               "}"}]}]}], "\n", "            ", "}"}], ",", "\n", 
           "            ", 
           RowBox[{"x_", " ", "/;", " ", 
            RowBox[{"MemberQ", "[", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{"exclude1", ",", " ", "exclude2"}], "}"}], ",", " ", 
              RowBox[{"First", "[", "x", "]"}]}], "]"}]}]}], "\n", "        ",
           "]"}]}], ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"ChoiceDialog", "[", "\n", "            ", 
         RowBox[{
         "message", ",", "\n", "            ", "options", ",", "\n", 
          "            ", 
          RowBox[{
          "WindowTitle", " ", "->", " ", 
           "\"\<V\[YAcute]ber transform\[AAcute]cie\>\""}], ",", "\n", 
          "            ", 
          RowBox[{"WindowSize", " ", "->", " ", 
           RowBox[{"{", 
            RowBox[{"500", ",", " ", "All"}], "}"}]}]}], "\n", "        ", 
         "]"}]}]}], "\n", "    ", "]"}]}], ";"}], "\n", "    ", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"SelectDifficultyLevel", "[", "]"}], " ", ":=", " ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "}"}], ",", "\n", "    ", 
       RowBox[{"ChoiceDialog", "[", "\n", "        ", 
        RowBox[{
        "\"\<Vyberte \[UAcute]rove\[NHacek] n\[AAcute]ro\[CHacek]nosti \
zadania:\>\"", ",", "\n", "        ", 
         RowBox[{"{", "\n", "            ", 
          RowBox[{
           RowBox[{
           "\"\<Jednoduch\[AAcute] (1 transform\[AAcute]cia)\>\"", " ", "->", 
            " ", "\"\<Easy\>\""}], ",", "\n", "            ", 
           RowBox[{
           "\"\<Stredn\[AAcute] (2 transform\[AAcute]cie)\>\"", " ", "->", 
            " ", "\"\<Medium\>\""}], ",", "\n", "            ", 
           RowBox[{
           "\"\<Zlo\[ZHacek]it\[AAcute] (3 transform\[AAcute]cie)\>\"", " ", "->",
             " ", "\"\<Hard\>\""}]}], "\n", "        ", "}"}], ",", "\n", 
         "        ", 
         RowBox[{
         "WindowTitle", " ", "->", " ", 
          "\"\<V\[YAcute]ber \[UAcute]rovne n\[AAcute]ro\[CHacek]nosti\>\""}],
          ",", "\n", "        ", 
         RowBox[{"WindowSize", " ", "->", " ", 
          RowBox[{"{", 
           RowBox[{"600", ",", " ", "All"}], "}"}]}]}], "\n", "    ", "]"}]}],
       "\n", "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodRotaciaNoDisplayWithParams", "[", "point_", "]"}], " ", ":=",
      " ", "\n", "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"angle", ",", " ", "result"}], "}"}], ",", "\n", "        ", 
       "\n", "        ", 
       RowBox[{
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
          "BodHardBalik`Transforms`Rotacia`RotaciaUhol", " ", "!=", " ", 
           "0"}], ",", "\n", "            ", 
          RowBox[{
          "angle", " ", "=", " ", 
           "BodHardBalik`Transforms`Rotacia`RotaciaUhol"}], ",", "\n", 
          "            ", 
          RowBox[{
           RowBox[{"angle", " ", "=", " ", 
            RowBox[{"RandomChoice", "[", 
             RowBox[{"{", 
              RowBox[{
              "30", ",", " ", "45", ",", " ", "60", ",", " ", "90", ",", " ", 
               "120", ",", " ", "135", ",", " ", "180", ",", " ", "270"}], 
              "}"}], "]"}]}], ";", "\n", "            ", "\n", "            ", 
           RowBox[{
           "BodHardBalik`Transforms`Rotacia`RotaciaUhol", " ", "=", " ", 
            "angle"}], ";"}]}], "\n", "        ", "]"}], ";", "\n", 
        "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"result", " ", "=", " ", 
         RowBox[{"{", "\n", "            ", 
          RowBox[{
           RowBox[{
            RowBox[{
             RowBox[{"point", "[", 
              RowBox[{"[", "1", "]"}], "]"}], "*", 
             RowBox[{"Cos", "[", 
              RowBox[{"angle", "*", "Degree"}], "]"}]}], " ", "-", " ", 
            RowBox[{
             RowBox[{"point", "[", 
              RowBox[{"[", "2", "]"}], "]"}], "*", 
             RowBox[{"Sin", "[", 
              RowBox[{"angle", "*", "Degree"}], "]"}]}]}], ",", " ", "\n", 
           "            ", 
           RowBox[{
            RowBox[{
             RowBox[{"point", "[", 
              RowBox[{"[", "1", "]"}], "]"}], "*", 
             RowBox[{"Sin", "[", 
              RowBox[{"angle", "*", "Degree"}], "]"}]}], " ", "+", " ", 
            RowBox[{
             RowBox[{"point", "[", 
              RowBox[{"[", "2", "]"}], "]"}], "*", 
             RowBox[{"Cos", "[", 
              RowBox[{"angle", "*", "Degree"}], "]"}]}]}]}], "\n", "        ",
           "}"}]}], ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"{", 
         RowBox[{"angle", ",", " ", "result"}], "}"}]}]}], "\n", "    ", 
      "]"}]}], ";"}], "\n", "    ", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"GetTranslationParametersNoDisplay", "[", "point_", "]"}], " ", ":=",
      " ", "\n", "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"dx", ",", " ", "dy", ",", " ", 
         RowBox[{"valid", " ", "=", " ", "False"}], ",", " ", "newPoint", ",",
          " ", "\n", "            ", "xMin", ",", " ", "xMax", ",", " ", 
         "yMin", ",", " ", "yMax", ",", " ", "\n", "            ", "invDx", 
         ",", " ", "invDy", ",", " ", "invNewPoint", ",", " ", "invPoint", 
         ",", "\n", "            ", "narocnost"}], "}"}], ",", "\n", 
       "        ", "\n", "        ", "\n", "        ", 
       RowBox[{
        RowBox[{"narocnost", " ", "=", " ", 
         RowBox[{"RandomReal", "[", "]"}]}], ";", " ", "\n", "        ", "\n",
         "        ", 
        RowBox[{"While", "[", 
         RowBox[{
          RowBox[{"!", "valid"}], ",", "\n", "            ", "\n", 
          "            ", 
          RowBox[{
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{"narocnost", " ", "<", " ", "0.7"}], ",", " ", "\n", 
             "                ", "\n", "                ", 
             RowBox[{
              RowBox[{"dx", " ", "=", " ", 
               RowBox[{"RandomChoice", "[", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"-", "7"}], ",", " ", 
                  RowBox[{"-", "6"}], ",", " ", 
                  RowBox[{"-", "5"}], ",", " ", 
                  RowBox[{"-", "4"}], ",", " ", 
                  RowBox[{"-", "3"}], ",", " ", "3", ",", " ", "4", ",", " ", 
                  "5", ",", " ", "6", ",", " ", "7"}], "}"}], "]"}]}], ";", 
              "\n", "                ", 
              RowBox[{"dy", " ", "=", " ", 
               RowBox[{"RandomChoice", "[", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"-", "7"}], ",", " ", 
                  RowBox[{"-", "6"}], ",", " ", 
                  RowBox[{"-", "5"}], ",", " ", 
                  RowBox[{"-", "4"}], ",", " ", 
                  RowBox[{"-", "3"}], ",", " ", "3", ",", " ", "4", ",", " ", 
                  "5", ",", " ", "6", ",", " ", "7"}], "}"}], "]"}]}]}], ",", 
             "\n", "                ", "\n", "                ", "\n", 
             "                ", 
             RowBox[{
              RowBox[{"dx", " ", "=", " ", 
               RowBox[{"RandomChoice", "[", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"-", "5"}], ",", " ", 
                  RowBox[{"-", "4"}], ",", " ", 
                  RowBox[{"-", "3"}], ",", " ", 
                  RowBox[{"-", "2"}], ",", " ", "2", ",", " ", "3", ",", " ", 
                  "4", ",", " ", "5"}], "}"}], "]"}]}], ";", "\n", 
              "                ", 
              RowBox[{"dy", " ", "=", " ", 
               RowBox[{"RandomChoice", "[", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"-", "5"}], ",", " ", 
                  RowBox[{"-", "4"}], ",", " ", 
                  RowBox[{"-", "3"}], ",", " ", 
                  RowBox[{"-", "2"}], ",", " ", "2", ",", " ", "3", ",", " ", 
                  "4", ",", " ", "5"}], "}"}], "]"}]}]}]}], "\n", 
            "            ", "]"}], ";", "\n", "            ", "\n", 
           "            ", "\n", "            ", 
           RowBox[{"newPoint", " ", "=", " ", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{
               RowBox[{"point", "[", 
                RowBox[{"[", "1", "]"}], "]"}], " ", "+", " ", "dx"}], ",", 
              " ", 
              RowBox[{
               RowBox[{"point", "[", 
                RowBox[{"[", "2", "]"}], "]"}], " ", "+", " ", "dy"}]}], 
             "}"}]}], ";", "\n", "            ", "\n", "            ", "\n", 
           "            ", 
           RowBox[{"invDx", " ", "=", " ", 
            RowBox[{"-", "dx"}]}], ";", "\n", "            ", 
           RowBox[{"invDy", " ", "=", " ", 
            RowBox[{"-", "dy"}]}], ";", "\n", "            ", "\n", 
           "            ", 
           RowBox[{
           "valid", " ", "=", " ", "\n", "                ", "\n", 
            "                ", 
            RowBox[{
             RowBox[{"(", 
              RowBox[{
               RowBox[{
                RowBox[{"Abs", "[", 
                 RowBox[{"newPoint", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], "]"}], " ", "<=", " ", 
                "14"}], " ", "&&", " ", 
               RowBox[{
                RowBox[{"Abs", "[", 
                 RowBox[{"newPoint", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "]"}], " ", "<=", " ", 
                "14"}]}], ")"}], " ", "&&", "\n", "                ", "\n", 
             "                ", 
             RowBox[{
              RowBox[{"Sqrt", "[", 
               RowBox[{
                RowBox[{"dx", "^", "2"}], " ", "+", " ", 
                RowBox[{"dy", "^", "2"}]}], "]"}], " ", ">=", " ", "3"}]}]}], 
           ";"}]}], "\n", "        ", "]"}], ";", "\n", "        ", "\n", 
        "        ", "\n", "        ", 
        RowBox[{"{", 
         RowBox[{"dx", ",", " ", "dy", ",", " ", 
          RowBox[{"\"\<Posun o [\>\"", " ", "<>", " ", 
           RowBox[{"ToString", "[", "dx", "]"}], " ", "<>", " ", "\"\<, \>\"",
            " ", "<>", " ", 
           RowBox[{"ToString", "[", "dy", "]"}], " ", "<>", " ", 
           "\"\<]\>\""}], ",", " ", "newPoint"}], "}"}]}]}], "\n", "    ", 
      "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodSymetriaNoDisplayWithParams", "[", "point_", "]"}], " ", ":=",
      " ", "\n", "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
        "axis", ",", " ", "result", ",", " ", "a", ",", " ", "b", ",", " ", 
         "c", ",", " ", "lineParams", ",", " ", "specialAxes", ",", " ", 
         "randomAxis"}], "}"}], ",", "\n", "        ", "\n", "        ", 
       RowBox[{
        RowBox[{"specialAxes", " ", "=", " ", 
         RowBox[{"{", 
          RowBox[{
          "\"\<os x\>\"", ",", " ", "\"\<os y\>\"", ",", " ", 
           "\"\<priamka y=x\>\"", ",", " ", "\"\<priamka y=-x\>\""}], "}"}]}],
         ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"TrueMemberQ", "[", 
            RowBox[{
             RowBox[{"{", 
              RowBox[{"Null", ",", " ", "\"\<complex\>\""}], "}"}], ",", " ", 
             "BodHardBalik`Transforms`Symetria`SymetriaOs"}], "]"}], " ", "||",
            " ", "\n", "           ", 
           RowBox[{"!", 
            RowBox[{"MemberQ", "[", 
             RowBox[{
              RowBox[{"Join", "[", 
               RowBox[{"specialAxes", ",", " ", "_List"}], "]"}], ",", " ", 
              "BodHardBalik`Transforms`Symetria`SymetriaOs"}], "]"}]}]}], ",",
           "\n", "           ", "\n", "           ", "\n", "           ", 
          RowBox[{
           RowBox[{"randomAxis", " ", "=", " ", 
            RowBox[{"RandomReal", "[", "1.0", "]"}]}], ";", "\n", 
           "           ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{"randomAxis", " ", "<", " ", "0.7"}], ",", " ", "\n", 
             "               ", "\n", "               ", 
             RowBox[{
              RowBox[{"a", " ", "=", " ", 
               RowBox[{"RandomChoice", "[", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"-", "5"}], ",", " ", 
                  RowBox[{"-", "4"}], ",", " ", 
                  RowBox[{"-", "3"}], ",", " ", 
                  RowBox[{"-", "2"}], ",", " ", 
                  RowBox[{"-", "1"}], ",", " ", "1", ",", " ", "2", ",", " ", 
                  "3", ",", " ", "4", ",", " ", "5"}], "}"}], "]"}]}], ";", 
              "\n", "               ", 
              RowBox[{"b", " ", "=", " ", 
               RowBox[{"RandomChoice", "[", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"-", "5"}], ",", " ", 
                  RowBox[{"-", "4"}], ",", " ", 
                  RowBox[{"-", "3"}], ",", " ", 
                  RowBox[{"-", "2"}], ",", " ", 
                  RowBox[{"-", "1"}], ",", " ", "1", ",", " ", "2", ",", " ", 
                  "3", ",", " ", "4", ",", " ", "5"}], "}"}], "]"}]}], ";", 
              "\n", "               ", 
              RowBox[{"c", " ", "=", " ", 
               RowBox[{"RandomChoice", "[", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"-", "6"}], ",", " ", 
                  RowBox[{"-", "5"}], ",", " ", 
                  RowBox[{"-", "4"}], ",", " ", 
                  RowBox[{"-", "3"}], ",", " ", 
                  RowBox[{"-", "2"}], ",", " ", 
                  RowBox[{"-", "1"}], ",", " ", "1", ",", " ", "2", ",", " ", 
                  "3", ",", " ", "4", ",", " ", "5", ",", " ", "6"}], "}"}], 
                "]"}]}], ";", "\n", "               ", "\n", 
              "               ", "\n", "               ", 
              RowBox[{"While", "[", 
               RowBox[{
                RowBox[{
                 RowBox[{"a", " ", "==", " ", "0"}], " ", "&&", " ", 
                 RowBox[{"b", " ", "==", " ", "0"}]}], ",", "\n", 
                "                   ", 
                RowBox[{
                 RowBox[{"a", " ", "=", " ", 
                  RowBox[{"RandomChoice", "[", 
                   RowBox[{"{", 
                    RowBox[{
                    RowBox[{"-", "5"}], ",", " ", 
                    RowBox[{"-", "4"}], ",", " ", 
                    RowBox[{"-", "3"}], ",", " ", 
                    RowBox[{"-", "2"}], ",", " ", 
                    RowBox[{"-", "1"}], ",", " ", "1", ",", " ", "2", ",", 
                    " ", "3", ",", " ", "4", ",", " ", "5"}], "}"}], "]"}]}], 
                 ";"}]}], "\n", "               ", "]"}], ";", "\n", 
              "               ", "\n", "               ", "\n", 
              "               ", 
              RowBox[{"If", "[", 
               RowBox[{
                RowBox[{
                 RowBox[{"GCD", "[", 
                  RowBox[{
                   RowBox[{"Abs", "[", "a", "]"}], ",", " ", 
                   RowBox[{"Abs", "[", "b", "]"}], ",", " ", 
                   RowBox[{"Abs", "[", "c", "]"}]}], "]"}], " ", ">", " ", 
                 "1"}], ",", "\n", "                   ", 
                RowBox[{
                 RowBox[{"{", 
                  RowBox[{"a", ",", " ", "b", ",", " ", "c"}], "}"}], " ", 
                 "=", " ", 
                 RowBox[{
                  RowBox[{"{", 
                   RowBox[{"a", ",", " ", "b", ",", " ", "c"}], "}"}], "/", 
                  RowBox[{"GCD", "[", 
                   RowBox[{
                    RowBox[{"Abs", "[", "a", "]"}], ",", " ", 
                    RowBox[{"Abs", "[", "b", "]"}], ",", " ", 
                    RowBox[{"Abs", "[", "c", "]"}]}], "]"}]}]}]}], "\n", 
               "               ", "]"}], ";", "\n", "               ", "\n", 
              "               ", "\n", "               ", 
              RowBox[{"axis", " ", "=", " ", 
               RowBox[{"{", 
                RowBox[{"a", ",", " ", "b", ",", " ", "c"}], "}"}]}], ";"}], 
             "\n", "           ", ",", "\n", "               ", "\n", 
             "               ", 
             RowBox[{
              RowBox[{"axis", " ", "=", " ", 
               RowBox[{"RandomChoice", "[", "specialAxes", "]"}]}], ";"}]}], 
            "\n", "           ", "]"}]}], ",", "\n", "           ", "\n", 
          "           ", 
          RowBox[{
           RowBox[{
           "axis", " ", "=", " ", 
            "BodHardBalik`Transforms`Symetria`SymetriaOs"}], ";"}]}], "\n", 
         "        ", "]"}], ";", "\n", "        ", "\n", "        ", "\n", 
        "        ", 
        RowBox[{"result", " ", "=", " ", 
         RowBox[{"Switch", "[", 
          RowBox[{
          "axis", ",", "\n", "            ", "\"\<os x\>\"", ",", " ", "\n", 
           "                ", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"point", "[", 
              RowBox[{"[", "1", "]"}], "]"}], ",", " ", 
             RowBox[{"-", 
              RowBox[{"point", "[", 
               RowBox[{"[", "2", "]"}], "]"}]}]}], "}"}], ",", "\n", 
           "            ", "\"\<os y\>\"", ",", " ", "\n", "                ", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"-", 
              RowBox[{"point", "[", 
               RowBox[{"[", "1", "]"}], "]"}]}], ",", " ", 
             RowBox[{"point", "[", 
              RowBox[{"[", "2", "]"}], "]"}]}], "}"}], ",", "\n", 
           "            ", "\"\<priamka y=x\>\"", ",", " ", "\n", 
           "                ", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"point", "[", 
              RowBox[{"[", "2", "]"}], "]"}], ",", " ", 
             RowBox[{"point", "[", 
              RowBox[{"[", "1", "]"}], "]"}]}], "}"}], ",", "\n", 
           "            ", "\"\<priamka y=-x\>\"", ",", " ", "\n", 
           "                ", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"-", 
              RowBox[{"point", "[", 
               RowBox[{"[", "2", "]"}], "]"}]}], ",", " ", 
             RowBox[{"-", 
              RowBox[{"point", "[", 
               RowBox[{"[", "1", "]"}], "]"}]}]}], "}"}], ",", "\n", 
           "            ", "_", ",", " ", "\n", "                ", "\n", 
           "                ", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"ListQ", "[", "axis", "]"}], " ", "&&", " ", 
              RowBox[{
               RowBox[{"Length", "[", "axis", "]"}], " ", "==", " ", "3"}]}], 
             ",", "\n", "                    ", "\n", "                    ", 
             
             RowBox[{"Module", "[", 
              RowBox[{
               RowBox[{"{", "reflectFunc", "}"}], ",", "\n", 
               "                        ", "\n", "                        ", 
               RowBox[{
                RowBox[{"reflectFunc", " ", "=", " ", 
                 RowBox[{"Function", "[", 
                  RowBox[{
                   RowBox[{"{", 
                    RowBox[{
                    "point", ",", " ", "a", ",", " ", "b", ",", " ", "c"}], 
                    "}"}], ",", "\n", "                            ", 
                   RowBox[{"Module", "[", 
                    RowBox[{
                    RowBox[{"{", 
                    RowBox[{
                    "x", ",", " ", "y", ",", " ", "d", ",", " ", "factor"}], 
                    "}"}], ",", "\n", "                                ", 
                    RowBox[{
                    RowBox[{"x", " ", "=", " ", 
                    RowBox[{"point", "[", 
                    RowBox[{"[", "1", "]"}], "]"}]}], ";", "\n", 
                    "                                ", 
                    RowBox[{"y", " ", "=", " ", 
                    RowBox[{"point", "[", 
                    RowBox[{"[", "2", "]"}], "]"}]}], ";", "\n", 
                    "                                ", 
                    RowBox[{"factor", " ", "=", " ", 
                    RowBox[{
                    RowBox[{"a", "^", "2"}], " ", "+", " ", 
                    RowBox[{"b", "^", "2"}]}]}], ";", "\n", 
                    "                                ", 
                    RowBox[{"d", " ", "=", " ", 
                    RowBox[{"(", 
                    RowBox[{
                    RowBox[{"a", "*", "x"}], " ", "+", " ", 
                    RowBox[{"b", "*", "y"}], " ", "+", " ", "c"}], ")"}]}], 
                    ";", "\n", "                                ", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"x", " ", "-", " ", 
                    RowBox[{"2", "*", "a", "*", 
                    RowBox[{"d", "/", "factor"}]}]}], ",", " ", 
                    RowBox[{"y", " ", "-", " ", 
                    RowBox[{"2", "*", "b", "*", 
                    RowBox[{"d", "/", "factor"}]}]}]}], "}"}]}]}], "\n", 
                    "                            ", "]"}]}], "\n", 
                  "                        ", "]"}]}], ";", "\n", 
                "                        ", "\n", "                        ", 
                "\n", "                        ", 
                RowBox[{"reflectFunc", "[", 
                 RowBox[{"point", ",", " ", 
                  RowBox[{"axis", "[", 
                   RowBox[{"[", "1", "]"}], "]"}], ",", " ", 
                  RowBox[{"axis", "[", 
                   RowBox[{"[", "2", "]"}], "]"}], ",", " ", 
                  RowBox[{"axis", "[", 
                   RowBox[{"[", "3", "]"}], "]"}]}], "]"}]}]}], "\n", 
              "                    ", "]"}], ",", "\n", 
             "                    ", "\n", "                    ", 
             RowBox[{"Module", "[", 
              RowBox[{
               RowBox[{"{", 
                RowBox[{"defaultAxis", " ", "=", " ", 
                 RowBox[{"{", 
                  RowBox[{"3", ",", " ", "4", ",", " ", "2"}], "}"}]}], "}"}],
                ",", "  ", "\n", "                        ", 
               RowBox[{
                RowBox[{"axis", " ", "=", " ", "defaultAxis"}], ";", "\n", 
                "                        ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", 
                   RowBox[{
                   "x", ",", " ", "y", ",", " ", "d", ",", " ", "factor", ",",
                     " ", "a", ",", " ", "b", ",", " ", "c"}], "}"}], ",", 
                  "\n", "                            ", 
                  RowBox[{
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{"a", ",", " ", "b", ",", " ", "c"}], "}"}], " ", 
                    "=", " ", "defaultAxis"}], ";", "\n", 
                   "                            ", 
                   RowBox[{"x", " ", "=", " ", 
                    RowBox[{"point", "[", 
                    RowBox[{"[", "1", "]"}], "]"}]}], ";", " ", 
                   RowBox[{"y", " ", "=", " ", 
                    RowBox[{"point", "[", 
                    RowBox[{"[", "2", "]"}], "]"}]}], ";", "\n", 
                   "                            ", 
                   RowBox[{"factor", " ", "=", " ", 
                    RowBox[{
                    RowBox[{"a", "^", "2"}], " ", "+", " ", 
                    RowBox[{"b", "^", "2"}]}]}], ";", "\n", 
                   "                            ", 
                   RowBox[{"d", " ", "=", " ", 
                    RowBox[{"(", 
                    RowBox[{
                    RowBox[{"a", "*", "x"}], " ", "+", " ", 
                    RowBox[{"b", "*", "y"}], " ", "+", " ", "c"}], ")"}]}], 
                   ";", "\n", "                            ", 
                   RowBox[{"{", 
                    RowBox[{
                    RowBox[{"x", " ", "-", " ", 
                    RowBox[{"2", "*", "a", "*", 
                    RowBox[{"d", "/", "factor"}]}]}], ",", " ", 
                    RowBox[{"y", " ", "-", " ", 
                    RowBox[{"2", "*", "b", "*", 
                    RowBox[{"d", "/", "factor"}]}]}]}], "}"}]}]}], "\n", 
                 "                        ", "]"}]}]}], "\n", 
              "                    ", "]"}]}], "\n", "                ", 
            "]"}]}], "\n", "        ", "]"}]}], ";", "\n", "        ", "\n", 
        "        ", "\n", "        ", 
        RowBox[{
        "BodHardBalik`Transforms`Symetria`SymetriaOs", " ", "=", " ", 
         "axis"}], ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        
        RowBox[{"{", 
         RowBox[{"axis", ",", " ", "result"}], "}"}]}]}], "\n", "    ", 
      "]"}]}], ";"}], "\n", "    ", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"EnsureExactValues", "[", "point_", "]"}], " ", ":=", " ", "\n", 
     "    ", 
     RowBox[{"Map", "[", 
      RowBox[{
       RowBox[{
        RowBox[{"Rationalize", "[", 
         RowBox[{"#", ",", " ", "0"}], "]"}], " ", "&"}], ",", " ", "point"}],
       "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodTrojitaTransformacia", "[", "]"}], " ", ":=", " ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "\n", "    ", 
        RowBox[{
        "difficultyLevel", ",", "\n", "    ", "numTransformations", ",", "\n",
          "    ", "pociatocnyBod", ",", "\n", "    ", "prvaTransformacia", 
         ",", "\n", "    ", "druhyBod", ",", "\n", "    ", 
         RowBox[{
         "druhaTransformacia", " ", "=", " ", 
          "\"\<\[CapitalZHacek]iadna\>\""}], ",", "\n", "    ", "tretiBod", 
         ",", "\n", "    ", 
         RowBox[{
         "tretiaTransformacia", " ", "=", " ", 
          "\"\<\[CapitalZHacek]iadna\>\""}], ",", "\n", "    ", "finalnyBod", 
         ",", "\n", "    ", 
         RowBox[{"transformacie", " ", "=", " ", 
          RowBox[{"{", "}"}]}], ",", "\n", "    ", 
         RowBox[{"transformovane", " ", "=", " ", 
          RowBox[{"{", "}"}]}], ",", "\n", "    ", "prvaPopis", ",", "\n", 
         "    ", "druhaPopis", ",", "\n", "    ", "tretiaPopis", ",", "\n", 
         "    ", "\n", "    ", "\n", "    ", "posunVektor1", ",", " ", 
         "posunVektor2", ",", " ", "posunVektor3", ",", "\n", "    ", 
         RowBox[{"rotaciaUhol1", " ", "=", " ", "0"}], ",", " ", 
         RowBox[{"rotaciaUhol2", " ", "=", " ", "0"}], ",", " ", 
         RowBox[{"rotaciaUhol3", " ", "=", " ", "0"}], ",", "\n", "    ", 
         "symetriaOs1", ",", " ", "symetriaOs2", ",", " ", "symetriaOs3", ",",
          "\n", "    ", "\n", "    ", "\n", "    ", 
         RowBox[{"transformParams", " ", "=", " ", 
          RowBox[{"{", "}"}]}]}], "\n", "    ", "}"}], ",", "\n", "    ", 
       "\n", "    ", "\n", "    ", 
       RowBox[{
        RowBox[{
        "BodHardBalik`Transforms`Symetria`SymetriaOs", " ", "=", " ", 
         "Null"}], ";", "\n", "    ", 
        RowBox[{
        "BodHardBalik`Transforms`Rotacia`RotaciaUhol", " ", "=", " ", "0"}], 
        ";", "\n", "    ", 
        RowBox[{"BodHardBalik`Transforms`Posun`BodPosunVector", " ", "=", " ", 
         RowBox[{"{", 
          RowBox[{"0", ",", " ", "0"}], "}"}]}], ";", "\n", "\n", "    ", 
        "\n", "    ", 
        RowBox[{"difficultyLevel", " ", "=", " ", 
         RowBox[{"SelectDifficultyLevel", "[", "]"}]}], ";", "\n", "    ", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{"difficultyLevel", " ", "===", " ", "$Canceled"}], ",", " ", 
          RowBox[{"Return", "[", "]"}]}], "]"}], ";", "\n", "    ", "\n", 
        "    ", "\n", "    ", 
        RowBox[{"numTransformations", " ", "=", " ", 
         RowBox[{"Switch", "[", 
          RowBox[{
          "difficultyLevel", ",", "\n", "        ", "\"\<Easy\>\"", ",", " ", 
           "1", ",", "\n", "        ", "\"\<Medium\>\"", ",", " ", "2", ",", 
           "\n", "        ", "\"\<Hard\>\"", ",", " ", "3", ",", "\n", 
           "        ", "_", ",", " ", "3"}], " ", "\n", "    ", "]"}]}], ";", 
        "\n", "    ", "\n", "    ", "\n", "    ", 
        RowBox[{"Quiet", "[", "\n", "    ", "\n", "    ", "\n", "    ", 
         RowBox[{
          RowBox[{"pociatocnyBod", " ", "=", " ", 
           RowBox[{"GenerateInitialPoint", "[", "]"}]}], ";", "\n", "    ", 
          RowBox[{"transformovane", " ", "=", " ", 
           RowBox[{"{", "pociatocnyBod", "}"}]}], ";", "\n", "    ", "\n", 
          "    ", "\n", "    ", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"numTransformations", " ", ">=", " ", "1"}], ",", " ", 
            "\n", "        ", "\n", "        ", 
            RowBox[{
             RowBox[{"prvaPopis", " ", "=", " ", 
              RowBox[{
              "SelectTransformation", "[", 
               "\"\<Vyberte prv\[UAcute] transform\[AAcute]ciu:\>\"", "]"}]}],
              ";", "\n", "        ", 
             RowBox[{"If", "[", 
              RowBox[{
               RowBox[{"prvaPopis", " ", "===", " ", "$Canceled"}], ",", " ", 
               
               RowBox[{"Return", "[", "]"}]}], "]"}], ";", "\n", "        ", 
             RowBox[{"prvaTransformacia", " ", "=", " ", 
              RowBox[{"First", "[", "prvaPopis", "]"}]}], ";", "\n", 
             "        ", 
             RowBox[{"AppendTo", "[", 
              RowBox[{"transformacie", ",", " ", "prvaTransformacia"}], "]"}],
              ";", "\n", "        ", "\n", "        ", "\n", "        ", 
             RowBox[{"druhyBod", " ", "=", " ", 
              RowBox[{"Switch", "[", 
               RowBox[{
               "prvaTransformacia", ",", "\n", "            ", 
                "\"\<Posun\>\"", ",", " ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", 
                   RowBox[{
                   "result", ",", " ", "dx", ",", " ", "dy", ",", " ", 
                    "translationParams"}], "}"}], ",", " ", "\n", 
                  "                ", "\n", "                ", 
                  RowBox[{
                   RowBox[{"translationParams", " ", "=", " ", 
                    RowBox[{
                    "GetTranslationParametersNoDisplay", "[", "pociatocnyBod",
                     "]"}]}], ";", "\n", "                ", "\n", 
                   "                ", "\n", "                ", 
                   RowBox[{"posunVektor1", " ", "=", " ", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"translationParams", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], ",", " ", 
                    RowBox[{"translationParams", "[", 
                    RowBox[{"[", "2", "]"}], "]"}]}], "}"}]}], ";", "\n", 
                   "                ", 
                   RowBox[{"AppendTo", "[", 
                    RowBox[{"transformParams", ",", " ", "posunVektor1"}], 
                    "]"}], ";", "\n", "                ", "\n", 
                   "                ", "\n", "                ", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{
                    "dx", ",", " ", "dy", ",", " ", "_", ",", " ", "result"}],
                     "}"}], " ", "=", " ", "translationParams"}], ";", "\n", 
                   "                ", "result"}]}], "]"}], ",", "\n", 
                "                ", "\n", "            ", 
                "\"\<Rot\[AAcute]cia\>\"", ",", " ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", 
                   RowBox[{"result", ",", " ", "angle"}], "}"}], ",", "\n", 
                  "                ", "\n", "                ", 
                  RowBox[{
                   RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{"rotaciaUhol1", " ", "!=", " ", "0"}], ",", "\n", 
                    "                    ", 
                    RowBox[{
                    RowBox[{
                    "BodHardBalik`Transforms`Rotacia`RotaciaUhol", " ", "=", 
                    " ", "rotaciaUhol1"}], ";"}]}], "\n", "                ", 
                    "]"}], ";", "\n", "                ", "\n", 
                   "                ", "\n", "                ", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{"angle", ",", " ", "result"}], "}"}], " ", "=", 
                    " ", 
                    RowBox[{
                    "BodRotaciaNoDisplayWithParams", "[", "pociatocnyBod", 
                    "]"}]}], ";", "\n", "                ", "\n", 
                   "                ", "\n", "                ", 
                   RowBox[{"rotaciaUhol1", " ", "=", " ", "angle"}], ";", 
                   "\n", "                ", 
                   RowBox[{"AppendTo", "[", 
                    RowBox[{"transformParams", ",", " ", "rotaciaUhol1"}], 
                    "]"}], ";", "\n", "                ", "\n", 
                   "                ", "result"}]}], "]"}], ",", "\n", 
                "                ", "\n", "            ", "\"\<Symetria\>\"", 
                ",", " ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", 
                   RowBox[{"result", ",", " ", "axis"}], "}"}], ",", "\n", 
                  "                ", "\n", "                ", 
                  RowBox[{
                   RowBox[{
                   "BodHardBalik`Transforms`Symetria`SymetriaOs", " ", "=", 
                    " ", "Null"}], ";", "\n", "                ", "\n", 
                   "                ", "\n", "                ", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{"axis", ",", " ", "result"}], "}"}], " ", "=", 
                    " ", 
                    RowBox[{
                    "BodSymetriaNoDisplayWithParams", "[", "pociatocnyBod", 
                    "]"}]}], ";", "\n", "                ", "\n", 
                   "                ", "\n", "                ", 
                   RowBox[{"symetriaOs1", " ", "=", " ", "axis"}], ";", "\n", 
                   "                ", 
                   RowBox[{"AppendTo", "[", 
                    RowBox[{"transformParams", ",", " ", "symetriaOs1"}], 
                    "]"}], ";", "\n", "                ", "\n", 
                   "                ", "result"}]}], "]"}]}], "\n", 
               "        ", "]"}]}], ";", "\n", "        ", 
             RowBox[{"AppendTo", "[", 
              RowBox[{"transformovane", ",", " ", "druhyBod"}], "]"}], ";", 
             "\n", "        ", 
             RowBox[{"finalnyBod", " ", "=", " ", "druhyBod"}], ";"}]}], "\n",
            "    ", "]"}], ";", "\n", "    ", "\n", "    ", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"numTransformations", " ", ">=", " ", "2"}], ",", " ", 
            "\n", "        ", "\n", "        ", 
            RowBox[{
             RowBox[{"druhaPopis", " ", "=", " ", 
              RowBox[{"SelectTransformation", "[", "\n", "            ", 
               RowBox[{
               "\"\<Vyberte druh\[UAcute] transform\[AAcute]ciu:\>\"", ",", 
                " ", "\n", "            ", "prvaTransformacia", ",", "\n", 
                "            ", "\"\<\>\""}], "\n", "        ", "]"}]}], ";", 
             "\n", "        ", 
             RowBox[{"If", "[", 
              RowBox[{
               RowBox[{"druhaPopis", " ", "===", " ", "$Canceled"}], ",", " ", 
               RowBox[{"Return", "[", "]"}]}], "]"}], ";", "\n", "        ", 
             RowBox[{"druhaTransformacia", " ", "=", " ", 
              RowBox[{"First", "[", "druhaPopis", "]"}]}], ";", "\n", 
             "        ", 
             RowBox[{"AppendTo", "[", 
              RowBox[{"transformacie", ",", " ", "druhaTransformacia"}], 
              "]"}], ";", "\n", "        ", "\n", "        ", "\n", 
             "        ", 
             RowBox[{"tretiBod", " ", "=", " ", 
              RowBox[{"Switch", "[", 
               RowBox[{
               "druhaTransformacia", ",", "\n", "            ", 
                "\"\<Posun\>\"", ",", " ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", 
                   RowBox[{
                   "result", ",", " ", "dx", ",", " ", "dy", ",", " ", 
                    "translationParams"}], "}"}], ",", " ", "\n", 
                  "                ", 
                  RowBox[{
                   RowBox[{"translationParams", " ", "=", " ", 
                    RowBox[{"GetTranslationParametersNoDisplay", "[", 
                    RowBox[{"EnsureExactValues", "[", "druhyBod", "]"}], 
                    "]"}]}], ";", "\n", "                ", 
                   RowBox[{"posunVektor2", " ", "=", " ", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"translationParams", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], ",", " ", 
                    RowBox[{"translationParams", "[", 
                    RowBox[{"[", "2", "]"}], "]"}]}], "}"}]}], ";", "\n", 
                   "                ", 
                   RowBox[{"AppendTo", "[", 
                    RowBox[{"transformParams", ",", " ", "posunVektor2"}], 
                    "]"}], ";", "\n", "                ", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{
                    "dx", ",", " ", "dy", ",", " ", "_", ",", " ", "result"}],
                     "}"}], " ", "=", " ", "translationParams"}], ";", "\n", 
                   "                ", "result"}]}], "]"}], ",", "\n", 
                "                ", "\n", "            ", 
                "\"\<Rot\[AAcute]cia\>\"", ",", " ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", 
                   RowBox[{"result", ",", " ", "angle"}], "}"}], ",", "\n", 
                  "                ", 
                  RowBox[{
                   RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{"rotaciaUhol2", " ", "!=", " ", "0"}], ",", "\n", 
                    "                    ", 
                    RowBox[{
                    RowBox[{
                    "BodHardBalik`Transforms`Rotacia`RotaciaUhol", " ", "=", 
                    " ", "rotaciaUhol2"}], ";"}]}], "\n", "                ", 
                    "]"}], ";", "\n", "                ", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{"angle", ",", " ", "result"}], "}"}], " ", "=", 
                    " ", 
                    RowBox[{"BodRotaciaNoDisplayWithParams", "[", 
                    RowBox[{"EnsureExactValues", "[", "druhyBod", "]"}], 
                    "]"}]}], ";", "\n", "                ", 
                   RowBox[{"rotaciaUhol2", " ", "=", " ", "angle"}], ";", 
                   "\n", "                ", 
                   RowBox[{"AppendTo", "[", 
                    RowBox[{"transformParams", ",", " ", "rotaciaUhol2"}], 
                    "]"}], ";", "\n", "                ", "result"}]}], "]"}],
                 ",", "\n", "                ", "\n", "            ", 
                "\"\<Symetria\>\"", ",", " ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", 
                   RowBox[{"result", ",", " ", "axis"}], "}"}], ",", "\n", 
                  "                ", "\n", "                ", 
                  RowBox[{
                   RowBox[{
                   "BodHardBalik`Transforms`Symetria`SymetriaOs", " ", "=", 
                    " ", "Null"}], ";", "\n", "                ", "\n", 
                   "                ", "\n", "                ", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{"axis", ",", " ", "result"}], "}"}], " ", "=", 
                    " ", 
                    RowBox[{"BodSymetriaNoDisplayWithParams", "[", 
                    RowBox[{"EnsureExactValues", "[", "druhyBod", "]"}], 
                    "]"}]}], ";", "\n", "                ", "\n", 
                   "                ", "\n", "                ", 
                   RowBox[{"symetriaOs2", " ", "=", " ", "axis"}], ";", "\n", 
                   "                ", 
                   RowBox[{"AppendTo", "[", 
                    RowBox[{"transformParams", ",", " ", "symetriaOs2"}], 
                    "]"}], ";", "\n", "                ", "\n", 
                   "                ", "result"}]}], "]"}]}], "\n", 
               "        ", "]"}]}], ";", "\n", "        ", 
             RowBox[{"AppendTo", "[", 
              RowBox[{"transformovane", ",", " ", "tretiBod"}], "]"}], ";", 
             "\n", "        ", 
             RowBox[{"finalnyBod", " ", "=", " ", "tretiBod"}], ";"}]}], "\n",
            "    ", "]"}], ";", "\n", "    ", "\n", "    ", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"numTransformations", " ", ">=", " ", "3"}], ",", " ", 
            "\n", "        ", "\n", "        ", 
            RowBox[{
             RowBox[{"tretiaPopis", " ", "=", " ", 
              RowBox[{"SelectTransformation", "[", "\n", "            ", 
               RowBox[{
               "\"\<Vyberte tretiu transform\[AAcute]ciu:\>\"", ",", " ", 
                "\n", "            ", "prvaTransformacia", ",", "\n", 
                "            ", "druhaTransformacia"}], "\n", "        ", 
               "]"}]}], ";", "\n", "        ", 
             RowBox[{"If", "[", 
              RowBox[{
               RowBox[{"tretiaPopis", " ", "===", " ", "$Canceled"}], ",", 
               " ", 
               RowBox[{"Return", "[", "]"}]}], "]"}], ";", "\n", "        ", 
             RowBox[{"tretiaTransformacia", " ", "=", " ", 
              RowBox[{"First", "[", "tretiaPopis", "]"}]}], ";", "\n", 
             "        ", 
             RowBox[{"AppendTo", "[", 
              RowBox[{"transformacie", ",", " ", "tretiaTransformacia"}], 
              "]"}], ";", "\n", "        ", "\n", "        ", "\n", 
             "        ", 
             RowBox[{"finalnyBod", " ", "=", " ", 
              RowBox[{"Switch", "[", 
               RowBox[{
               "tretiaTransformacia", ",", "\n", "            ", 
                "\"\<Posun\>\"", ",", " ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", 
                   RowBox[{
                   "result", ",", " ", "dx", ",", " ", "dy", ",", " ", 
                    "translationParams"}], "}"}], ",", " ", "\n", 
                  "                ", 
                  RowBox[{
                   RowBox[{"translationParams", " ", "=", " ", 
                    RowBox[{"GetTranslationParametersNoDisplay", "[", 
                    RowBox[{"EnsureExactValues", "[", "tretiBod", "]"}], 
                    "]"}]}], ";", "\n", "                ", 
                   RowBox[{"posunVektor3", " ", "=", " ", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"translationParams", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], ",", " ", 
                    RowBox[{"translationParams", "[", 
                    RowBox[{"[", "2", "]"}], "]"}]}], "}"}]}], ";", "\n", 
                   "                ", 
                   RowBox[{"AppendTo", "[", 
                    RowBox[{"transformParams", ",", " ", "posunVektor3"}], 
                    "]"}], ";", "\n", "                ", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{
                    "dx", ",", " ", "dy", ",", " ", "_", ",", " ", "result"}],
                     "}"}], " ", "=", " ", "translationParams"}], ";", "\n", 
                   "                ", "result"}]}], "]"}], ",", "\n", 
                "                ", "\n", "            ", 
                "\"\<Rot\[AAcute]cia\>\"", ",", " ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", 
                   RowBox[{"result", ",", " ", "angle"}], "}"}], ",", "\n", 
                  "                ", 
                  RowBox[{
                   RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{"rotaciaUhol3", " ", "!=", " ", "0"}], ",", "\n", 
                    "                    ", 
                    RowBox[{
                    RowBox[{
                    "BodHardBalik`Transforms`Rotacia`RotaciaUhol", " ", "=", 
                    " ", "rotaciaUhol3"}], ";"}]}], "\n", "                ", 
                    "]"}], ";", "\n", "                ", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{"angle", ",", " ", "result"}], "}"}], " ", "=", 
                    " ", 
                    RowBox[{"BodRotaciaNoDisplayWithParams", "[", 
                    RowBox[{"EnsureExactValues", "[", "tretiBod", "]"}], 
                    "]"}]}], ";", "\n", "                ", 
                   RowBox[{"rotaciaUhol3", " ", "=", " ", "angle"}], ";", 
                   "\n", "                ", 
                   RowBox[{"AppendTo", "[", 
                    RowBox[{"transformParams", ",", " ", "rotaciaUhol3"}], 
                    "]"}], ";", "\n", "                ", "result"}]}], "]"}],
                 ",", "\n", "                ", "\n", "            ", 
                "\"\<Symetria\>\"", ",", " ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", 
                   RowBox[{"result", ",", " ", "axis"}], "}"}], ",", "\n", 
                  "                ", "\n", "                ", 
                  RowBox[{
                   RowBox[{
                   "BodHardBalik`Transforms`Symetria`SymetriaOs", " ", "=", 
                    " ", "Null"}], ";", "\n", "                ", "\n", 
                   "                ", "\n", "                ", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{"axis", ",", " ", "result"}], "}"}], " ", "=", 
                    " ", 
                    RowBox[{"BodSymetriaNoDisplayWithParams", "[", 
                    RowBox[{"EnsureExactValues", "[", "tretiBod", "]"}], 
                    "]"}]}], ";", "\n", "                ", "\n", 
                   "                ", "\n", "                ", 
                   RowBox[{"symetriaOs3", " ", "=", " ", "axis"}], ";", "\n", 
                   "                ", 
                   RowBox[{"AppendTo", "[", 
                    RowBox[{"transformParams", ",", " ", "symetriaOs3"}], 
                    "]"}], ";", "\n", "                ", "\n", 
                   "                ", "result"}]}], "]"}]}], "\n", 
               "        ", "]"}]}], ";", "\n", "        ", 
             RowBox[{"AppendTo", "[", 
              RowBox[{"transformovane", ",", " ", "finalnyBod"}], "]"}], 
             ";"}]}], "\n", "    ", "]"}], ";", "\n", "    ", "\n", "    ", 
          "\n", "    ", 
          RowBox[{"Print", "[", 
           RowBox[{"Style", "[", 
            RowBox[{
             RowBox[{
             "\"\<GEOMETRICK\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA \
BODU - \>\"", " ", "<>", " ", "\n", "           ", 
              RowBox[{"Switch", "[", 
               RowBox[{
               "difficultyLevel", ",", " ", "\"\<Easy\>\"", ",", " ", 
                "\"\<JEDNODUCH\[CapitalAAcute] \[CapitalUAcute]LOHA\>\"", ",",
                 " ", "\n", "                              ", 
                "\"\<Medium\>\"", ",", " ", 
                "\"\<STREDNE \[CapitalTHacek]A\[CapitalZHacek]K\
\[CapitalAAcute] \[CapitalUAcute]LOHA\>\"", ",", "\n", 
                "                              ", "\"\<Hard\>\"", ",", " ", 
                "\"\<ZLO\[CapitalZHacek]IT\[CapitalAAcute] \
\[CapitalUAcute]LOHA\>\""}], "]"}]}], ",", " ", "Bold", ",", " ", "24"}], 
            "]"}], "]"}], ";", "\n", "    ", 
          RowBox[{"Print", "[", 
           RowBox[{"Style", "[", 
            RowBox[{
            "\"\<==========================================\>\"", ",", " ", 
             "Bold"}], "]"}], "]"}], ";", "\n", "    ", "\n", "    ", 
          RowBox[{"Print", "[", 
           RowBox[{"Style", "[", 
            RowBox[{
            "\"\<\\nZADANIE PRE \[CapitalSHacek]TUDENTA:\>\"", ",", " ", 
             "Bold", ",", " ", "18"}], "]"}], "]"}], ";", "\n", "    ", 
          RowBox[{
          "Print", "[", "\"\<Majme bod P so s\[UAcute]radnicami:\>\"", "]"}], 
          ";", "\n", "    ", 
          RowBox[{"Print", "[", 
           RowBox[{"Style", "[", 
            RowBox[{"pociatocnyBod", ",", " ", "Blue"}], "]"}], "]"}], ";", 
          "\n", "    ", "\n", "    ", 
          RowBox[{
          "Print", "[", 
           "\"\<\\nVykonajte postupne tieto transform\[AAcute]cie:\>\"", 
           "]"}], ";", "\n", "    ", "\n", "    ", "\n", "    ", "\n", "    ",
           "\n", "    ", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"numTransformations", " ", ">=", " ", "1"}], ",", "\n", 
            "        ", 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{"\"\<1. \>\"", ",", " ", 
               RowBox[{"FormatTransformationDetailedDescription", "[", 
                RowBox[{
                "prvaTransformacia", ",", " ", "\n", 
                 "                         ", 
                 RowBox[{"Switch", "[", 
                  RowBox[{
                  "prvaTransformacia", ",", "\n", 
                   "                             ", "\"\<Posun\>\"", ",", " ",
                    "posunVektor1", ",", "\n", 
                   "                             ", "\"\<Rot\[AAcute]cia\>\"",
                    ",", " ", "rotaciaUhol1", ",", "\n", 
                   "                             ", "\"\<Symetria\>\"", ",", 
                   " ", "symetriaOs1"}], "\n", "                         ", 
                  "]"}]}], "]"}]}], "]"}], ";"}]}], "\n", "    ", "]"}], ";", 
          "\n", "    ", "\n", "    ", "\n", "    ", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"numTransformations", " ", ">=", " ", "2"}], ",", "\n", 
            "        ", 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{"\"\<2. \>\"", ",", " ", 
               RowBox[{"FormatTransformationDetailedDescription", "[", 
                RowBox[{
                "druhaTransformacia", ",", " ", "\n", 
                 "                         ", 
                 RowBox[{"Switch", "[", 
                  RowBox[{
                  "druhaTransformacia", ",", "\n", 
                   "                             ", "\"\<Posun\>\"", ",", " ",
                    "posunVektor2", ",", "\n", 
                   "                             ", "\"\<Rot\[AAcute]cia\>\"",
                    ",", " ", "rotaciaUhol2", ",", "\n", 
                   "                             ", "\"\<Symetria\>\"", ",", 
                   " ", "symetriaOs2"}], "\n", "                         ", 
                  "]"}]}], "]"}]}], "]"}], ";"}]}], "\n", "    ", "]"}], ";", 
          "\n", "    ", "\n", "    ", "\n", "    ", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"numTransformations", " ", ">=", " ", "3"}], ",", "\n", 
            "        ", 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{"\"\<3. \>\"", ",", " ", 
               RowBox[{"FormatTransformationDetailedDescription", "[", 
                RowBox[{
                "tretiaTransformacia", ",", " ", "\n", 
                 "                         ", 
                 RowBox[{"Switch", "[", 
                  RowBox[{
                  "tretiaTransformacia", ",", "\n", 
                   "                             ", "\"\<Posun\>\"", ",", " ",
                    "posunVektor3", ",", "\n", 
                   "                             ", "\"\<Rot\[AAcute]cia\>\"",
                    ",", " ", "rotaciaUhol3", ",", "\n", 
                   "                             ", "\"\<Symetria\>\"", ",", 
                   " ", "symetriaOs3"}], "\n", "                         ", 
                  "]"}]}], "]"}]}], "]"}], ";"}]}], "\n", "    ", "]"}], ";", 
          "\n", "    ", "\n", "    ", 
          RowBox[{"Print", "[", 
           RowBox[{"Style", "[", 
            RowBox[{
            "\"\<\\nRIE\[CapitalSHacek]ENIE:\>\"", ",", " ", "Bold", ",", " ",
              "18"}], "]"}], "]"}], ";", "\n", "\n", "    ", "\n", "    ", 
          "\n", "    ", "\n", "    ", 
          RowBox[{"Print", "[", 
           RowBox[{"Style", "[", 
            RowBox[{
             RowBox[{
             "\"\<GEOMETRICK\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA \
BODU - \>\"", " ", "<>", " ", "\n", "           ", 
              RowBox[{"Switch", "[", 
               RowBox[{
               "difficultyLevel", ",", " ", "\"\<Easy\>\"", ",", " ", 
                "\"\<JEDNODUCH\[CapitalAAcute] \[CapitalUAcute]LOHA\>\"", ",",
                 " ", "\n", "                              ", 
                "\"\<Medium\>\"", ",", " ", 
                "\"\<STREDNE \[CapitalTHacek]A\[CapitalZHacek]K\
\[CapitalAAcute] \[CapitalUAcute]LOHA\>\"", ",", "\n", 
                "                              ", "\"\<Hard\>\"", ",", " ", 
                "\"\<ZLO\[CapitalZHacek]IT\[CapitalAAcute] \
\[CapitalUAcute]LOHA\>\""}], "]"}]}], ",", " ", "Bold", ",", " ", "24"}], 
            "]"}], "]"}], ";", "\n", "    ", 
          RowBox[{"Print", "[", 
           RowBox[{"Style", "[", 
            RowBox[{
            "\"\<==========================================\>\"", ",", " ", 
             "Bold"}], "]"}], "]"}], ";", "\n", "    ", "\n", "    ", "\n", 
          "    ", 
          RowBox[{"Print", "[", 
           RowBox[{"Style", "[", 
            RowBox[{
            "\"\<\\nPO\[CapitalCHacek]IATO\[CapitalCHacek]N\[CapitalYAcute] \
BOD:\>\"", ",", " ", "Bold", ",", " ", "16"}], "]"}], "]"}], ";", "\n", 
          "    ", 
          RowBox[{"Print", "[", "\"\<S\[UAcute]radnice bodu P:\>\"", "]"}], 
          ";", "\n", "    ", 
          RowBox[{"Print", "[", 
           RowBox[{"Style", "[", 
            RowBox[{"pociatocnyBod", ",", " ", "Blue"}], "]"}], "]"}], ";", 
          "\n", "    ", "\n", "    ", "\n", "    ", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"numTransformations", " ", ">=", " ", "1"}], ",", "\n", 
            "        ", 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{
                "\"\<\\nPRV\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA: \
\>\"", " ", "<>", " ", "prvaTransformacia"}], ",", " ", "Bold", ",", " ", 
                "16"}], "]"}], "]"}], ";", "\n", "        ", "\n", "        ",
              "\n", "        ", 
             RowBox[{"druhyBod", " ", "=", " ", 
              RowBox[{"Switch", "[", 
               RowBox[{
               "prvaTransformacia", ",", "\n", "            ", 
                "\"\<Posun\>\"", ",", " ", "\n", "                ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", "}"}], ",", " ", "\n", "                    ", 
                  
                  RowBox[{
                   RowBox[{
                   "BodHardBalik`Transforms`Posun`BodPosunVector", " ", "=", 
                    " ", "posunVektor1"}], ";", "\n", "                    ", 
                   
                   RowBox[{"BodPosun", "[", "pociatocnyBod", "]"}]}]}], "\n", 
                 "                ", "]"}], ",", "\n", "            ", 
                "\"\<Rot\[AAcute]cia\>\"", ",", " ", "\n", "                ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", "}"}], ",", " ", "\n", "                    ", 
                  
                  RowBox[{
                   RowBox[{
                   "BodHardBalik`Transforms`Rotacia`RotaciaUhol", " ", "=", 
                    " ", "rotaciaUhol1"}], ";", "\n", "                    ", 
                   
                   RowBox[{"BodRotacia", "[", "pociatocnyBod", "]"}]}]}], 
                 "\n", "                ", "]"}], ",", "\n", "            ", 
                "\"\<Symetria\>\"", ",", " ", "\n", "                ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", "}"}], ",", " ", "\n", "                    ", 
                  "\n", "                    ", 
                  RowBox[{
                   RowBox[{
                   "BodHardBalik`Transforms`Symetria`SymetriaOs", " ", "=", 
                    " ", "symetriaOs1"}], ";", "\n", "                    ", 
                   RowBox[{"BodSymetria", "[", "pociatocnyBod", "]"}]}]}], 
                 "\n", "                ", "]"}]}], "\n", "        ", "]"}]}],
              ";"}]}], "\n", "    ", "]"}], ";", "\n", "    ", "\n", "    ", 
          "\n", "    ", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"numTransformations", " ", ">=", " ", "2"}], ",", "\n", 
            "        ", 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{
                "\"\<\\nDRUH\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA: \>\
\"", " ", "<>", " ", "druhaTransformacia"}], ",", " ", "Bold", ",", " ", 
                "16"}], "]"}], "]"}], ";", "\n", "        ", "\n", "        ",
              "\n", "        ", 
             RowBox[{"tretiBod", " ", "=", " ", 
              RowBox[{"Switch", "[", 
               RowBox[{
               "druhaTransformacia", ",", "\n", "            ", 
                "\"\<Posun\>\"", ",", " ", "\n", "                ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", "}"}], ",", " ", "\n", "                    ", 
                  
                  RowBox[{
                   RowBox[{
                   "BodHardBalik`Transforms`Posun`BodPosunVector", " ", "=", 
                    " ", "posunVektor2"}], ";", "\n", "                    ", 
                   
                   RowBox[{"BodPosun", "[", 
                    RowBox[{"EnsureExactValues", "[", "druhyBod", "]"}], 
                    "]"}]}]}], "\n", "                ", "]"}], ",", "\n", 
                "            ", "\"\<Rot\[AAcute]cia\>\"", ",", " ", "\n", 
                "                ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", "}"}], ",", " ", "\n", "                    ", 
                  
                  RowBox[{
                   RowBox[{
                   "BodHardBalik`Transforms`Rotacia`RotaciaUhol", " ", "=", 
                    " ", "rotaciaUhol2"}], ";", "\n", "                    ", 
                   
                   RowBox[{"BodRotacia", "[", 
                    RowBox[{"EnsureExactValues", "[", "druhyBod", "]"}], 
                    "]"}]}]}], "\n", "                ", "]"}], ",", "\n", 
                "            ", "\"\<Symetria\>\"", ",", " ", "\n", 
                "                ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", "}"}], ",", " ", "\n", "                    ", 
                  "\n", "                    ", 
                  RowBox[{
                   RowBox[{
                   "BodHardBalik`Transforms`Symetria`SymetriaOs", " ", "=", 
                    " ", "symetriaOs2"}], ";", "\n", "                    ", 
                   RowBox[{"BodSymetria", "[", 
                    RowBox[{"EnsureExactValues", "[", "druhyBod", "]"}], 
                    "]"}]}]}], "\n", "                ", "]"}]}], "\n", 
               "        ", "]"}]}], ";"}]}], "\n", "    ", "]"}], ";", "\n", 
          "    ", "\n", "    ", "\n", "    ", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"numTransformations", " ", ">=", " ", "3"}], ",", "\n", 
            "        ", 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{
                "\"\<\\nTRETIA TRANSFORM\[CapitalAAcute]CIA: \>\"", " ", "<>",
                  " ", "tretiaTransformacia"}], ",", " ", "Bold", ",", " ", 
                "16"}], "]"}], "]"}], ";", "\n", "        ", "\n", "        ",
              "\n", "        ", 
             RowBox[{"finalnyBod", " ", "=", " ", 
              RowBox[{"Switch", "[", 
               RowBox[{
               "tretiaTransformacia", ",", "\n", "            ", 
                "\"\<Posun\>\"", ",", " ", "\n", "                ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", "}"}], ",", " ", "\n", "                    ", 
                  
                  RowBox[{
                   RowBox[{
                   "BodHardBalik`Transforms`Posun`BodPosunVector", " ", "=", 
                    " ", "posunVektor3"}], ";", "\n", "                    ", 
                   
                   RowBox[{"BodPosun", "[", 
                    RowBox[{"EnsureExactValues", "[", "tretiBod", "]"}], 
                    "]"}]}]}], "\n", "                ", "]"}], ",", "\n", 
                "            ", "\"\<Rot\[AAcute]cia\>\"", ",", " ", "\n", 
                "                ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", "}"}], ",", " ", "\n", "                    ", 
                  
                  RowBox[{
                   RowBox[{
                   "BodHardBalik`Transforms`Rotacia`RotaciaUhol", " ", "=", 
                    " ", "rotaciaUhol3"}], ";", "\n", "                    ", 
                   
                   RowBox[{"BodRotacia", "[", 
                    RowBox[{"EnsureExactValues", "[", "tretiBod", "]"}], 
                    "]"}]}]}], "\n", "                ", "]"}], ",", "\n", 
                "            ", "\"\<Symetria\>\"", ",", " ", "\n", 
                "                ", 
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", "}"}], ",", " ", "\n", "                    ", 
                  "\n", "                    ", 
                  RowBox[{
                   RowBox[{
                   "BodHardBalik`Transforms`Symetria`SymetriaOs", " ", "=", 
                    " ", "symetriaOs3"}], ";", "\n", "                    ", 
                   RowBox[{"BodSymetria", "[", 
                    RowBox[{"EnsureExactValues", "[", "tretiBod", "]"}], 
                    "]"}]}]}], "\n", "                ", "]"}]}], "\n", 
               "        ", "]"}]}], ";"}]}], "\n", "    ", "]"}], ";", "\n", 
          "    ", "\n", "    ", "\n", "    ", 
          RowBox[{"DisplayTransformationSequence", "[", "\n", "        ", 
           RowBox[{
           "pociatocnyBod", ",", "\n", "        ", "druhyBod", ",", "\n", 
            "        ", 
            RowBox[{"If", "[", 
             RowBox[{
              RowBox[{"numTransformations", " ", ">=", " ", "2"}], ",", " ", 
              "tretiBod", ",", " ", "druhyBod"}], "]"}], ",", "  ", "\n", 
            "        ", "finalnyBod", ",", "\n", "        ", 
            "prvaTransformacia", ",", "\n", "        ", 
            RowBox[{"If", "[", 
             RowBox[{
              RowBox[{"numTransformations", " ", ">=", " ", "2"}], ",", " ", 
              "druhaTransformacia", ",", " ", 
              "\"\<\[CapitalZHacek]iadna\>\""}], "]"}], ",", "\n", "        ", 
            RowBox[{"If", "[", 
             RowBox[{
              RowBox[{"numTransformations", " ", ">=", " ", "3"}], ",", " ", 
              "tretiaTransformacia", ",", " ", 
              "\"\<\[CapitalZHacek]iadna\>\""}], "]"}], ",", "\n", "        ",
             "difficultyLevel", ",", "  ", "\n", "        ", 
            "transformParams"}], "   ", "\n", "    ", "]"}], ";", "\n", 
          "    ", "\n", "    ", "\n", "    ", 
          RowBox[{"Print", "[", 
           RowBox[{"Style", "[", 
            RowBox[{
            "\"\<\\nS\[CapitalUAcute]HRNN\[CapitalAAcute] VIZUALIZ\
\[CapitalAAcute]CIA:\>\"", ",", " ", "Bold", ",", " ", "16"}], "]"}], "]"}], 
          ";", "\n", "    ", 
          RowBox[{"Switch", "[", 
           RowBox[{
           "numTransformations", ",", "\n", "        ", "1", ",", "\n", 
            "            ", 
            RowBox[{"Print", "[", 
             RowBox[{
             "CreatePointVisualization", "[", "\n", "                ", 
              RowBox[{
              "pociatocnyBod", ",", "\n", "                ", "finalnyBod", 
               ",", "\n", "                ", "finalnyBod", ",", "  ", "\n", 
               "                ", "finalnyBod", ",", "  ", "\n", 
               "                ", "prvaTransformacia", ",", "\n", 
               "                ", "\"\<\[CapitalZHacek]iadna\>\"", ",", "\n",
                "                ", "\"\<\[CapitalZHacek]iadna\>\""}], "\n", 
              "            ", "]"}], "]"}], ",", "\n", "        ", "2", ",", 
            "\n", "            ", 
            RowBox[{"Print", "[", 
             RowBox[{
             "CreatePointVisualization", "[", "\n", "                ", 
              RowBox[{
              "pociatocnyBod", ",", "\n", "                ", "druhyBod", ",",
                "\n", "                ", "finalnyBod", ",", "\n", 
               "                ", "finalnyBod", ",", "  ", "\n", 
               "                ", "prvaTransformacia", ",", "\n", 
               "                ", "druhaTransformacia", ",", "\n", 
               "                ", "\"\<\[CapitalZHacek]iadna\>\""}], "\n", 
              "            ", "]"}], "]"}], ",", "\n", "        ", "3", ",", 
            "\n", "            ", 
            RowBox[{"Print", "[", 
             RowBox[{
             "CreatePointVisualization", "[", "\n", "                ", 
              RowBox[{
              "pociatocnyBod", ",", "\n", "                ", "druhyBod", ",",
                "\n", "                ", "tretiBod", ",", "\n", 
               "                ", "finalnyBod", ",", "\n", 
               "                ", "prvaTransformacia", ",", "\n", 
               "                ", "druhaTransformacia", ",", "\n", 
               "                ", "tretiaTransformacia"}], "\n", 
              "            ", "]"}], "]"}]}], "\n", "    ", "]"}], ";", "\n", 
          "    ", "\n", "    ", "\n", "    ", 
          RowBox[{"Print", "[", 
           RowBox[{"Style", "[", 
            RowBox[{"\"\<\\nLEGENDA:\>\"", ",", " ", "Bold", ",", " ", "16"}],
             "]"}], "]"}], ";", "\n", "    ", 
          RowBox[{"Print", "[", 
           RowBox[{
            RowBox[{"Style", "[", 
             RowBox[{"\"\<\[Bullet] Modr\[EAcute]\>\"", ",", " ", 
              RowBox[{"RGBColor", "[", 
               RowBox[{"0", ",", " ", "0.4", ",", " ", "0.8"}], "]"}], ",", 
              " ", "Bold"}], "]"}], ",", " ", 
            "\"\< body a \[CHacek]iary: P\[OHat]vodn\[YAcute] bod P\>\""}], 
           "]"}], ";", "\n", "    ", "\n", "    ", "\n", "    ", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"numTransformations", " ", ">=", " ", "1"}], ",", "\n", 
            "        ", 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{
               RowBox[{"Style", "[", 
                RowBox[{"\"\<\[Bullet] Tmavozelen\[EAcute]\>\"", ",", " ", 
                 RowBox[{"RGBColor", "[", 
                  RowBox[{"0", ",", " ", "0.8", ",", " ", "0.2"}], "]"}], ",",
                  " ", "Bold"}], "]"}], ",", " ", 
               "\"\< body a \[CHacek]iary: Bod po prvej transform\[AAcute]cii \
(\>\"", ",", " ", "\n", "              ", 
               RowBox[{"Style", "[", 
                RowBox[{"prvaTransformacia", ",", " ", "Bold"}], "]"}], ",", 
               " ", "\"\<)\>\""}], "]"}], ";"}]}], "\n", "    ", "]"}], ";", 
          "\n", "    ", "\n", "    ", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"numTransformations", " ", ">=", " ", "2"}], ",", "\n", 
            "        ", 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{
               RowBox[{"Style", "[", 
                RowBox[{
                "\"\<\[Bullet] Oran\[ZHacek]ov\[EAcute]\>\"", ",", " ", 
                 RowBox[{"RGBColor", "[", 
                  RowBox[{"1", ",", " ", "0.6", ",", " ", "0"}], "]"}], ",", 
                 " ", "Bold"}], "]"}], ",", " ", 
               "\"\< body a \[CHacek]iary: Bod po druhej \
transform\[AAcute]cii (\>\"", ",", " ", "\n", "              ", 
               RowBox[{"Style", "[", 
                RowBox[{"druhaTransformacia", ",", " ", "Bold"}], "]"}], ",", 
               " ", "\"\<)\>\""}], "]"}], ";"}]}], "\n", "    ", "]"}], ";", 
          "\n", "    ", "\n", "    ", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"numTransformations", " ", ">=", " ", "3"}], ",", "\n", 
            "        ", 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{
               RowBox[{"Style", "[", 
                RowBox[{
                "\"\<\[Bullet] \[CapitalCHacek]erven\[EAcute]\>\"", ",", " ", 
                 
                 RowBox[{"RGBColor", "[", 
                  RowBox[{"0.9", ",", " ", "0.1", ",", " ", "0.1"}], "]"}], 
                 ",", " ", "Bold"}], "]"}], ",", " ", 
               "\"\< body a \[CHacek]iary: Bod po tretej \
transform\[AAcute]cii (\>\"", ",", " ", "\n", "              ", 
               RowBox[{"Style", "[", 
                RowBox[{"tretiaTransformacia", ",", " ", "Bold"}], "]"}], ",",
                " ", "\"\<)\>\""}], "]"}], ";"}]}], "\n", "    ", "]"}], ";", 
          "\n", "    ", "\n", "    ", "\n", "    ", 
          RowBox[{"Print", "[", 
           RowBox[{"Style", "[", 
            RowBox[{
            "\"\<\\nPostupnos\[THacek] transform\[AAcute]ci\[IAcute]:\>\"", 
             ",", " ", "Bold", ",", " ", "16"}], "]"}], "]"}], ";", "\n", 
          "    ", "\n", "    ", "\n", "    ", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"numTransformations", " ", ">=", " ", "1"}], ",", "\n", 
            "        ", 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{
              "\"\<1. \>\"", ",", " ", "prvaTransformacia", ",", " ", 
               "\"\< \>\"", ",", " ", "\n", "              ", 
               RowBox[{"FormatTransformationParameters", "[", 
                RowBox[{
                "prvaTransformacia", ",", " ", "\n", "                  ", 
                 RowBox[{"Switch", "[", 
                  RowBox[{
                  "prvaTransformacia", ",", "\n", "                      ", 
                   "\"\<Posun\>\"", ",", " ", "posunVektor1", ",", "\n", 
                   "                      ", "\"\<Rot\[AAcute]cia\>\"", ",", 
                   " ", "rotaciaUhol1", ",", "\n", "                      ", 
                   "\"\<Symetria\>\"", ",", " ", "symetriaOs1"}], "\n", 
                  "                  ", "]"}]}], "]"}]}], "]"}], ";"}]}], 
           "\n", "    ", "]"}], ";", "\n", "    ", "\n", "    ", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"numTransformations", " ", ">=", " ", "2"}], ",", "\n", 
            "        ", 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{
              "\"\<2. \>\"", ",", " ", "druhaTransformacia", ",", " ", 
               "\"\< \>\"", ",", " ", "\n", "              ", 
               RowBox[{"FormatTransformationParameters", "[", 
                RowBox[{
                "druhaTransformacia", ",", " ", "\n", "                  ", 
                 RowBox[{"Switch", "[", 
                  RowBox[{
                  "druhaTransformacia", ",", "\n", "                      ", 
                   "\"\<Posun\>\"", ",", " ", "posunVektor2", ",", "\n", 
                   "                      ", "\"\<Rot\[AAcute]cia\>\"", ",", 
                   " ", "rotaciaUhol2", ",", "\n", "                      ", 
                   "\"\<Symetria\>\"", ",", " ", "symetriaOs2"}], "\n", 
                  "                  ", "]"}]}], "]"}]}], "]"}], ";"}]}], 
           "\n", "    ", "]"}], ";", "\n", "    ", "\n", "    ", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"numTransformations", " ", ">=", " ", "3"}], ",", "\n", 
            "        ", 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{
              "\"\<3. \>\"", ",", " ", "tretiaTransformacia", ",", " ", 
               "\"\< \>\"", ",", " ", "\n", "              ", 
               RowBox[{"FormatTransformationParameters", "[", 
                RowBox[{
                "tretiaTransformacia", ",", " ", "\n", "                  ", 
                 RowBox[{"Switch", "[", 
                  RowBox[{
                  "tretiaTransformacia", ",", "\n", "                      ", 
                   "\"\<Posun\>\"", ",", " ", "posunVektor3", ",", "\n", 
                   "                      ", "\"\<Rot\[AAcute]cia\>\"", ",", 
                   " ", "rotaciaUhol3", ",", "\n", "                      ", 
                   "\"\<Symetria\>\"", ",", " ", "symetriaOs3"}], "\n", 
                  "                  ", "]"}]}], "]"}]}], "]"}], ";"}]}], 
           "\n", "    ", "]"}], ";", "\n", "    ", "\n", "    ", "\n", "    ", 
          RowBox[{"{", 
           RowBox[{
           "pociatocnyBod", ",", " ", "transformovane", ",", " ", 
            "transformacie", ",", " ", "difficultyLevel", ",", " ", 
            "transformParams"}], "}"}]}], "\n", "    ", "\n", "    ", 
         "]"}]}]}], " ", "\n", "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodRotaciaNoDisplay", "[", "point_", "]"}], " ", ":=", " ", 
     "\n", "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"angle", ",", " ", "result"}], "}"}], ",", "\n", "        ", 
       RowBox[{
        RowBox[{"angle", " ", "=", " ", 
         RowBox[{"RandomChoice", "[", 
          RowBox[{"{", 
           RowBox[{
           "30", ",", " ", "45", ",", " ", "60", ",", " ", "90", ",", " ", 
            "120", ",", " ", "135", ",", " ", "180", ",", " ", "270"}], "}"}],
           "]"}]}], ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"result", " ", "=", " ", 
         RowBox[{"{", "\n", "            ", 
          RowBox[{
           RowBox[{
            RowBox[{
             RowBox[{"point", "[", 
              RowBox[{"[", "1", "]"}], "]"}], "*", 
             RowBox[{"Cos", "[", 
              RowBox[{"angle", "*", "Degree"}], "]"}]}], " ", "-", " ", 
            RowBox[{
             RowBox[{"point", "[", 
              RowBox[{"[", "2", "]"}], "]"}], "*", 
             RowBox[{"Sin", "[", 
              RowBox[{"angle", "*", "Degree"}], "]"}]}]}], ",", " ", "\n", 
           "            ", 
           RowBox[{
            RowBox[{
             RowBox[{"point", "[", 
              RowBox[{"[", "1", "]"}], "]"}], "*", 
             RowBox[{"Sin", "[", 
              RowBox[{"angle", "*", "Degree"}], "]"}]}], " ", "+", " ", 
            RowBox[{
             RowBox[{"point", "[", 
              RowBox[{"[", "2", "]"}], "]"}], "*", 
             RowBox[{"Cos", "[", 
              RowBox[{"angle", "*", "Degree"}], "]"}]}]}]}], "\n", "        ",
           "}"}]}], ";", "\n", "        ", "\n", "        ", "result"}]}], 
      "\n", "    ", "]"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{"On", "[", 
     RowBox[{"Power", "::", "infy"}], "]"}], ";"}], "\n", 
   RowBox[{
    RowBox[{"On", "[", 
     RowBox[{"Infinity", "::", "indet"}], "]"}], ";"}], "\n", "\n", 
   RowBox[{
    RowBox[{"End", "[", "]"}], ";"}], " ", "\n", 
   RowBox[{
    RowBox[{"EndPackage", "[", "]"}], ";", " "}]}]}]], "Code",
 CellChangeTimes->{{3.9527926019094543`*^9, 3.952792601911684*^9}, 
   3.95287781282687*^9, {3.9550285218059998`*^9, 3.955028523018416*^9}, {
   3.955705563769134*^9, 3.9557056156175623`*^9}, 3.9557057914001102`*^9, {
   3.955766883919005*^9, 3.9557669176287394`*^9}, {3.955768013579053*^9, 
   3.955768014707519*^9}},
 CellLabel->
  "In[19572]:=",ExpressionUUID->"e586ecac-2bb2-4e4a-8f52-3230ebac1ad0"]
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
Cell[554, 20, 254768, 5371, 35484, "Code",ExpressionUUID->"e586ecac-2bb2-4e4a-8f52-3230ebac1ad0"]
}
]
*)

