(* Content-type: application/vnd.wolfram.mathematica *)

(*** Wolfram Notebook File ***)
(* http://www.wolfram.com/nb *)

(* CreatedBy='Wolfram 14.1' *)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[       154,          7]
NotebookDataLength[     87950,       2100]
NotebookOptionsPosition[     87598,       2086]
NotebookOutlinePosition[     87989,       2102]
CellTagsIndexPosition[     87946,       2099]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{
Cell[BoxData[{
 RowBox[{
  RowBox[{
   RowBox[{"BeginPackage", "[", 
    RowBox[{"\"\<BodHardBalik`Transforms`Skosenie`\>\"", ",", 
     RowBox[{"{", "\"\<BodHardBalik`\>\"", "}"}]}], "]"}], ";"}], 
  "\[IndentingNewLine]", "\n", 
  RowBox[{"(*", 
   RowBox[{"Export", " ", "public", " ", "symbols"}], 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{
    RowBox[{"BodSkosenie", "::", "usage"}], "=", 
    "\"\<BodSkosenie[coordinates_] zobraz\[IAcute] interakt\[IAcute]vny \
tutori\[AAcute]l pre skosenie bodu a vr\[AAcute]ti nov\[EAcute] \
s\[UAcute]radnice.\>\""}], ";"}], "\n"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"Begin", "[", "\"\<`Private`\>\"", "]"}], ";"}], 
  "\[IndentingNewLine]", "\n", 
  RowBox[{"(*", 
   RowBox[{
   "Funkcia", " ", "na", " ", "form\[AAcute]tovanie", " ", 
    "exaktn\[YAcute]ch", " ", "hodn\[OHat]t"}], 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{
    RowBox[{"FormatExactValue", "[", "expr_", "]"}], ":=", 
    RowBox[{"Module", "[", 
     RowBox[{
      RowBox[{"{", "strExpr", "}"}], ",", 
      RowBox[{"(*", 
       RowBox[{"Konvertovanie", " ", "na", " ", "string"}], "*)"}], 
      RowBox[{
       RowBox[{"strExpr", "=", 
        RowBox[{"ToString", "[", 
         RowBox[{"expr", ",", "InputForm"}], "]"}]}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
        "Vykonanie", " ", "v\[SHacek]etk\[YAcute]ch", " ", 
         "nahraden\[IAcute]"}], "*)"}], 
       RowBox[{"strExpr", "=", 
        RowBox[{"StringReplace", "[", 
         RowBox[{"strExpr", ",", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{
             RowBox[{"\"\<Sqrt[\>\"", "~~", "n__", "~~", "\"\<]\>\""}], ":>", 
             
             RowBox[{"\"\<\[Sqrt]\>\"", "<>", "n"}]}], ",", 
            RowBox[{"\"\<*\>\"", "->", "\"\< \[CenterDot] \>\""}], ",", 
            RowBox[{"\"\<[\>\"", "->", "\"\<\>\""}], ",", 
            RowBox[{"\"\<]\>\"", "->", "\"\<\>\""}]}], "}"}]}], "]"}]}], ";", 
       "\[IndentingNewLine]", "strExpr"}]}], "]"}]}], ";"}], 
  "\[IndentingNewLine]", "\n", 
  RowBox[{"(*", 
   RowBox[{
   "Funkcia", " ", "na", " ", "rozvinutie", " ", "a", " ", 
    "zjednodu\[SHacek]enie", " ", "v\[YAcute]razov", " ", "pre", " ", 
    "v\[YAcute]po\[CHacek]ty"}], "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{
    RowBox[{"ExpandAndFormatExpression", "[", "expr_", "]"}], ":=", 
    RowBox[{"Module", "[", 
     RowBox[{
      RowBox[{"{", 
       RowBox[{"expandedExpr", ",", "formattedExpr"}], "}"}], ",", 
      RowBox[{"(*", 
       RowBox[{
       "Plne", " ", "rozvinieme", " ", "v\[YAcute]raz", " ", "pre", " ", 
        "lep\[SHacek]ie", " ", "zobrazenie", " ", "krokov"}], "*)"}], 
      RowBox[{
       RowBox[{"expandedExpr", "=", 
        RowBox[{"Expand", "[", "expr", "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
        "Form\[AAcute]tujeme", " ", "rozvinut\[YAcute]", " ", 
         "v\[YAcute]raz"}], "*)"}], 
       RowBox[{"formattedExpr", "=", 
        RowBox[{"FormatExactValue", "[", "expandedExpr", "]"}]}], ";", 
       "\[IndentingNewLine]", "formattedExpr"}]}], "]"}]}], ";"}], 
  "\[IndentingNewLine]", "\n", 
  RowBox[{"(*", 
   RowBox[{
   "Pomocn\[AAcute]", " ", "funkcia", " ", "na", " ", "normaliz\[AAcute]ciu", 
    " ", "vstupn\[YAcute]ch", " ", "s\[UAcute]radn\[IAcute]c"}], 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{
    RowBox[{"NormalizeCoordinates", "[", "coordinates_", "]"}], ":=", 
    RowBox[{"Module", "[", 
     RowBox[{
      RowBox[{"{", "processedCoordinates", "}"}], ",", 
      RowBox[{"(*", 
       RowBox[{
       "Spracovanie", " ", "r\[OHat]znych", " ", "vstupn\[YAcute]ch", " ", 
        "form\[AAcute]tov"}], "*)"}], 
      RowBox[{
       RowBox[{"processedCoordinates", "=", 
        RowBox[{"Which", "[", 
         RowBox[{"(*", 
          RowBox[{
          "Ak", " ", "ide", " ", "o", " ", "vektor", " ", 
           "d\[IAcute]\[ZHacek]ky", " ", "2"}], "*)"}], 
         RowBox[{
          RowBox[{
           RowBox[{"VectorQ", "[", "coordinates", "]"}], "&&", 
           RowBox[{
            RowBox[{"Length", "[", "coordinates", "]"}], "==", "2"}]}], ",", 
          "coordinates", ",", 
          RowBox[{"(*", 
           RowBox[{
            RowBox[{
            "Ak", " ", "ide", " ", "o", " ", "zoznam", " ", "s", " ", 
             "jednou", " ", "polo\[ZHacek]kou"}], ",", 
            RowBox[{
            "ktor\[AAcute]", " ", "je", " ", "vektor", " ", 
             "d\[IAcute]\[ZHacek]ky", " ", "2"}]}], "*)"}], 
          RowBox[{
           RowBox[{"ListQ", "[", "coordinates", "]"}], "&&", 
           RowBox[{
            RowBox[{"Length", "[", "coordinates", "]"}], "==", "1"}], "&&", 
           RowBox[{"VectorQ", "[", 
            RowBox[{"coordinates", "[", 
             RowBox[{"[", "1", "]"}], "]"}], "]"}], "&&", 
           RowBox[{
            RowBox[{"Length", "[", 
             RowBox[{"coordinates", "[", 
              RowBox[{"[", "1", "]"}], "]"}], "]"}], "==", "2"}]}], ",", 
          RowBox[{"coordinates", "[", 
           RowBox[{"[", "1", "]"}], "]"}], ",", 
          RowBox[{"(*", 
           RowBox[{
           "Ak", " ", "ide", " ", "o", " ", "zoznam", " ", "s", " ", "dvoma", 
            " ", "\[CHacek]\[IAcute]slami"}], "*)"}], 
          RowBox[{
           RowBox[{"ListQ", "[", "coordinates", "]"}], "&&", 
           RowBox[{
            RowBox[{"Length", "[", "coordinates", "]"}], "==", "2"}], "&&", 
           RowBox[{"(", 
            RowBox[{
             RowBox[{"NumberQ", "[", 
              RowBox[{"coordinates", "[", 
               RowBox[{"[", "1", "]"}], "]"}], "]"}], "||", 
             RowBox[{
              RowBox[{"Head", "[", 
               RowBox[{"coordinates", "[", 
                RowBox[{"[", "1", "]"}], "]"}], "]"}], "===", "Rational"}]}], 
            ")"}], "&&", 
           RowBox[{"(", 
            RowBox[{
             RowBox[{"NumberQ", "[", 
              RowBox[{"coordinates", "[", 
               RowBox[{"[", "2", "]"}], "]"}], "]"}], "||", 
             RowBox[{
              RowBox[{"Head", "[", 
               RowBox[{"coordinates", "[", 
                RowBox[{"[", "2", "]"}], "]"}], "]"}], "===", "Rational"}]}], 
            ")"}]}], ",", "coordinates", ",", 
          RowBox[{"(*", "Inak", "*)"}], "True", ",", 
          RowBox[{
           RowBox[{"Message", "[", 
            RowBox[{"BodSkosenie", "::", "invalidInput"}], "]"}], ";", 
           "\[IndentingNewLine]", 
           RowBox[{"Abort", "[", "]"}]}]}], "]"}]}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
         RowBox[{
         "ZACHOV\[CapitalAAcute]VAME", " ", "PRESN\[CapitalEAcute]", " ", 
          "HODNOTY"}], "-", 
         RowBox[{
         "BEZ", " ", "KONVERZIE", " ", "NA", " ", "NUMERICK\[CapitalEAcute]", 
          " ", "HODNOTY"}]}], "*)"}], 
       RowBox[{"(*", 
        RowBox[{"Kontrola", ",", 
         RowBox[{
         "\[CHacek]i", " ", "s\[UAcute]radnice", " ", "zostan\[UAcute]", " ", 
          "v", " ", "bezpe\[CHacek]nej", " ", "oblasti"}], ",", 
         RowBox[{
         "ale", " ", "zachov\[AAcute]vame", " ", "presn\[EAcute]", " ", 
          "hodnoty"}]}], "*)"}], 
       RowBox[{"If", "[", 
        RowBox[{
         RowBox[{
          RowBox[{"Max", "[", 
           RowBox[{"Abs", "[", 
            RowBox[{"N", "[", "processedCoordinates", "]"}], "]"}], "]"}], 
          ">", "10"}], ",", 
         RowBox[{"(*", 
          RowBox[{
           RowBox[{
           "\[CapitalSHacek]k\[AAcute]lovanie", " ", 
            "s\[UAcute]radn\[IAcute]c"}], ",", 
           RowBox[{
           "ak", " ", "s\[UAcute]", " ", "pr\[IAcute]li\[SHacek]", " ", 
            "\[DHacek]aleko"}], ",", 
           RowBox[{
           "ale", " ", "zachov\[AAcute]me", " ", "presn\[YAcute]", " ", 
            "v\[YAcute]po\[CHacek]et"}]}], "*)"}], 
         RowBox[{
          RowBox[{"processedCoordinates", "=", 
           RowBox[{"processedCoordinates", "/", 
            RowBox[{"(", 
             RowBox[{
              RowBox[{"Max", "[", 
               RowBox[{"Abs", "[", 
                RowBox[{"N", "[", "processedCoordinates", "]"}], "]"}], "]"}],
               "/", "6"}], ")"}]}]}], ";"}]}], "]"}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{"N\[AAcute]vrat", " ", "s\[UAcute]radn\[IAcute]c"}], "*)"}], 
       "processedCoordinates"}]}], "]"}]}], ";"}], "\[IndentingNewLine]", 
  "\n"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{
    RowBox[{"DetectShearingParameters", "[", 
     RowBox[{"originalCoordinates_", ",", "finalCoordinates_"}], "]"}], ":=", 
    
    RowBox[{"Module", "[", 
     RowBox[{
      RowBox[{"{", 
       RowBox[{
        RowBox[{"kx", "=", "0"}], ",", 
        RowBox[{"ky", "=", "0"}], ",", 
        RowBox[{"foundKx", "=", "False"}], ",", 
        RowBox[{"foundKy", "=", "False"}], ",", 
        RowBox[{"smer", "=", "\"\<\>\""}]}], "}"}], ",", 
      RowBox[{"(*", 
       RowBox[{
       "H\:013ead\[AAcute]me", " ", "koeficienty", " ", "skosenia", " ", 
        "porovn\[AAcute]van\[IAcute]m", " ", "s\[UAcute]radn\[IAcute]c"}], 
       "*)"}], 
      RowBox[{"(*", 
       RowBox[{
       "Detekcia", " ", "skosenia", " ", "v", " ", "smere", " ", "osi", " ", 
        "x"}], "*)"}], 
      RowBox[{
       RowBox[{"If", "[", 
        RowBox[{
         RowBox[{
          RowBox[{"originalCoordinates", "[", 
           RowBox[{"[", "2", "]"}], "]"}], "!=", "0"}], ",", 
         RowBox[{
          RowBox[{"kx", "=", 
           RowBox[{
            RowBox[{"(", 
             RowBox[{
              RowBox[{"finalCoordinates", "[", 
               RowBox[{"[", "1", "]"}], "]"}], "-", 
              RowBox[{"originalCoordinates", "[", 
               RowBox[{"[", "1", "]"}], "]"}]}], ")"}], "/", 
            RowBox[{"originalCoordinates", "[", 
             RowBox[{"[", "2", "]"}], "]"}]}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{"Kontrola", ",", 
            RowBox[{
            "\[CHacek]i", " ", "ide", " ", "skuto\[CHacek]ne", " ", "o", " ", 
             "skosenie", " ", "x"}]}], "*)"}], 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{
             RowBox[{
              RowBox[{"Simplify", "[", 
               RowBox[{
                RowBox[{"finalCoordinates", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "-", 
                RowBox[{"originalCoordinates", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "-", 
                RowBox[{"kx", "*", 
                 RowBox[{"originalCoordinates", "[", 
                  RowBox[{"[", "2", "]"}], "]"}]}]}], "]"}], "==", "0"}], "&&", 
             RowBox[{
              RowBox[{"finalCoordinates", "[", 
               RowBox[{"[", "2", "]"}], "]"}], "==", 
              RowBox[{"originalCoordinates", "[", 
               RowBox[{"[", "2", "]"}], "]"}]}]}], ",", 
            RowBox[{
             RowBox[{"foundKx", "=", "True"}], ";", "\[IndentingNewLine]", 
             RowBox[{"smer", "=", "\"\<v smere osi x\>\""}], ";"}]}], 
           "]"}]}]}], "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
        "Detekcia", " ", "skosenia", " ", "v", " ", "smere", " ", "osi", " ", 
         "y"}], "*)"}], 
       RowBox[{"If", "[", 
        RowBox[{
         RowBox[{
          RowBox[{
           RowBox[{"originalCoordinates", "[", 
            RowBox[{"[", "1", "]"}], "]"}], "!=", "0"}], "&&", 
          RowBox[{"!", "foundKx"}]}], ",", 
         RowBox[{
          RowBox[{"ky", "=", 
           RowBox[{
            RowBox[{"(", 
             RowBox[{
              RowBox[{"finalCoordinates", "[", 
               RowBox[{"[", "2", "]"}], "]"}], "-", 
              RowBox[{"originalCoordinates", "[", 
               RowBox[{"[", "2", "]"}], "]"}]}], ")"}], "/", 
            RowBox[{"originalCoordinates", "[", 
             RowBox[{"[", "1", "]"}], "]"}]}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{"Kontrola", ",", 
            RowBox[{
            "\[CHacek]i", " ", "ide", " ", "skuto\[CHacek]ne", " ", "o", " ", 
             "skosenie", " ", "y"}]}], "*)"}], 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{
             RowBox[{
              RowBox[{"Simplify", "[", 
               RowBox[{
                RowBox[{"finalCoordinates", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "-", 
                RowBox[{"originalCoordinates", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "-", 
                RowBox[{"ky", "*", 
                 RowBox[{"originalCoordinates", "[", 
                  RowBox[{"[", "1", "]"}], "]"}]}]}], "]"}], "==", "0"}], "&&", 
             RowBox[{
              RowBox[{"finalCoordinates", "[", 
               RowBox[{"[", "1", "]"}], "]"}], "==", 
              RowBox[{"originalCoordinates", "[", 
               RowBox[{"[", "1", "]"}], "]"}]}]}], ",", 
            RowBox[{
             RowBox[{"foundKy", "=", "True"}], ";", "\[IndentingNewLine]", 
             RowBox[{"smer", "=", "\"\<v smere osi y\>\""}], ";"}]}], 
           "]"}]}]}], "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
        "Kontrola", " ", "\[CHacek]i", " ", "sme", " ", "na\[SHacek]li", " ", 
         "nejak\[EAcute]", " ", "skosenie"}], "*)"}], 
       RowBox[{"If", "[", 
        RowBox[{"foundKx", ",", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"Simplify", "[", "kx", "]"}], ",", "0", ",", "smer", ",", 
           RowBox[{"\"\<Skosenie v smere osi x s koeficientom k = \>\"", "<>", 
            RowBox[{"FormatExactValue", "[", "kx", "]"}]}]}], "}"}], ",", 
         RowBox[{"If", "[", 
          RowBox[{"foundKy", ",", 
           RowBox[{"{", 
            RowBox[{"0", ",", 
             RowBox[{"Simplify", "[", "ky", "]"}], ",", "smer", ",", 
             RowBox[{"\"\<Skosenie v smere osi y s koeficientom k = \>\"", "<>", 
              RowBox[{"FormatExactValue", "[", "ky", "]"}]}]}], "}"}], ",", 
           "None"}], "  ", 
          RowBox[{"(*", 
           RowBox[{
           "\[CapitalZHacek]iadne", " ", "skosenie", " ", 
            "nen\[AAcute]jden\[EAcute]"}], "*)"}], "]"}]}], "]"}]}]}], 
     "]"}]}], ";"}], "\[IndentingNewLine]", "\n", 
  RowBox[{"(*", 
   RowBox[{
   "Funkcia", " ", "na", " ", "generovanie", " ", "parametrov", " ", 
    "skosenia"}], "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{
    RowBox[{"GetShearingParameters", "[", 
     RowBox[{"coordinates_", ",", 
      RowBox[{"finalCoordinates_", ":", "None"}]}], "]"}], ":=", 
    RowBox[{"Module", "[", 
     RowBox[{
      RowBox[{"{", 
       RowBox[{"kx", ",", "ky", ",", "smer", ",", 
        RowBox[{"valid", "=", "False"}], ",", "newCoordinates", ",", "xMin", 
        ",", "xMax", ",", "yMin", ",", "yMax", ",", "invKx", ",", "invKy", 
        ",", "invNewCoordinates", ",", "invCoordinates", ",", "narocnost", 
        ",", "popis", ",", "detectedParameters"}], "}"}], ",", 
      RowBox[{"(*", 
       RowBox[{
        RowBox[{
        "Ak", " ", "s\[UAcute]", " ", "zadan\[EAcute]", " ", 
         "finalCoordinates"}], ",", 
        RowBox[{
        "pok\[UAcute]sime", " ", "sa", " ", "detekova\[THacek]", " ", 
         "parametre", " ", "skosenia"}]}], "*)"}], 
      RowBox[{
       RowBox[{"If", "[", 
        RowBox[{
         RowBox[{"finalCoordinates", "=!=", "None"}], ",", 
         RowBox[{
          RowBox[{"detectedParameters", "=", 
           RowBox[{"DetectShearingParameters", "[", 
            RowBox[{"coordinates", ",", "finalCoordinates"}], "]"}]}], ";", 
          "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{
            RowBox[{
            "Ak", " ", "sme", " ", "na\[SHacek]li", " ", "parametre"}], ",", 
            RowBox[{"pou\[ZHacek]ijeme", " ", "ich"}]}], "*)"}], 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"detectedParameters", "=!=", "None"}], ",", 
            RowBox[{
             RowBox[{"kx", "=", 
              RowBox[{"detectedParameters", "[", 
               RowBox[{"[", "1", "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
             RowBox[{"ky", "=", 
              RowBox[{"detectedParameters", "[", 
               RowBox[{"[", "2", "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
             RowBox[{"smer", "=", 
              RowBox[{"detectedParameters", "[", 
               RowBox[{"[", "3", "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
             RowBox[{"popis", "=", 
              RowBox[{"detectedParameters", "[", 
               RowBox[{"[", "4", "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
             RowBox[{"Return", "[", 
              RowBox[{"{", 
               RowBox[{
               "kx", ",", "ky", ",", "smer", ",", "popis", ",", 
                "finalCoordinates"}], "}"}], "]"}], ";"}]}], "]"}]}]}], "]"}],
        ";", "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
        "Anal\[YAcute]za", " ", "p\[OHat]vodn\[YAcute]ch", " ", 
         "s\[UAcute]radn\[IAcute]c"}], "*)"}], 
       RowBox[{"xMin", "=", 
        RowBox[{"N", "[", 
         RowBox[{"coordinates", "[", 
          RowBox[{"[", "1", "]"}], "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"xMax", "=", 
        RowBox[{"N", "[", 
         RowBox[{"coordinates", "[", 
          RowBox[{"[", "1", "]"}], "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"yMin", "=", 
        RowBox[{"N", "[", 
         RowBox[{"coordinates", "[", 
          RowBox[{"[", "2", "]"}], "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"yMax", "=", 
        RowBox[{"N", "[", 
         RowBox[{"coordinates", "[", 
          RowBox[{"[", "2", "]"}], "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
        "N\[AAcute]ro\[CHacek]nej\[SHacek]ie", " ", "hodnoty", " ", 
         "skosenia", " ", "pre", " ", "Hard", " ", "bal\[IAcute]k"}], "*)"}], 
       
       RowBox[{"narocnost", "=", 
        RowBox[{"RandomReal", "[", "]"}]}], ";", 
       RowBox[{"(*", 
        RowBox[{
        "N\[AAcute]hodn\[EAcute]", " ", "\[CHacek]\[IAcute]slo", " ", "pre", 
         " ", "ur\[CHacek]enie", " ", "n\[AAcute]ro\[CHacek]nosti"}], "*)"}], 
       
       RowBox[{"While", "[", 
        RowBox[{
         RowBox[{"!", "valid"}], ",", 
         RowBox[{"(*", 
          RowBox[{
           RowBox[{"Generovanie", " ", "parametrov", " ", "skosenia"}], "-", 
           RowBox[{
           "n\[AAcute]ro\[CHacek]nej\[SHacek]ie", " ", "pre", " ", "Hard", 
            " ", "bal\[IAcute]k"}]}], "*)"}], 
         RowBox[{
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"narocnost", "<", "0.7"}], ",", 
            RowBox[{"(*", 
             RowBox[{
              RowBox[{
              "N\[AAcute]ro\[CHacek]nej\[SHacek]ie", " ", "koeficienty"}], 
              "-", 
              RowBox[{
              "zlomky", " ", "a", " ", "v\[ADoubleDot]\[CHacek]\[SHacek]ie", 
               " ", "hodnoty"}]}], "*)"}], 
            RowBox[{"If", "[", 
             RowBox[{
              RowBox[{
               RowBox[{"RandomReal", "[", "]"}], "<", "0.5"}], ",", 
              RowBox[{"(*", 
               RowBox[{
               "Skosenie", " ", "v", " ", "smere", " ", "osi", " ", "x"}], 
               "*)"}], 
              RowBox[{
               RowBox[{"kx", "=", 
                RowBox[{"RandomChoice", "[", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"-", "2"}], ",", 
                   RowBox[{
                    RowBox[{"-", "3"}], "/", "2"}], ",", 
                   RowBox[{"-", "1"}], ",", 
                   RowBox[{
                    RowBox[{"-", "1"}], "/", "2"}], ",", 
                   RowBox[{"1", "/", "2"}], ",", "1", ",", 
                   RowBox[{"3", "/", "2"}], ",", "2"}], "}"}], "]"}]}], ";", 
               "\[IndentingNewLine]", 
               RowBox[{"ky", "=", "0"}], ";", "\[IndentingNewLine]", 
               RowBox[{"smer", "=", "\"\<v smere osi x\>\""}], ";", 
               "\[IndentingNewLine]", 
               RowBox[{"popis", "=", 
                RowBox[{
                "\"\<Skosenie v smere osi x s koeficientom k = \>\"", "<>", 
                 RowBox[{"FormatExactValue", "[", "kx", "]"}]}]}], ";"}], ",", 
              RowBox[{"(*", 
               RowBox[{
               "Skosenie", " ", "v", " ", "smere", " ", "osi", " ", "y"}], 
               "*)"}], 
              RowBox[{
               RowBox[{"kx", "=", "0"}], ";", "\[IndentingNewLine]", 
               RowBox[{"ky", "=", 
                RowBox[{"RandomChoice", "[", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"-", "2"}], ",", 
                   RowBox[{
                    RowBox[{"-", "3"}], "/", "2"}], ",", 
                   RowBox[{"-", "1"}], ",", 
                   RowBox[{
                    RowBox[{"-", "1"}], "/", "2"}], ",", 
                   RowBox[{"1", "/", "2"}], ",", "1", ",", 
                   RowBox[{"3", "/", "2"}], ",", "2"}], "}"}], "]"}]}], ";", 
               "\[IndentingNewLine]", 
               RowBox[{"smer", "=", "\"\<v smere osi y\>\""}], ";", 
               "\[IndentingNewLine]", 
               RowBox[{"popis", "=", 
                RowBox[{
                "\"\<Skosenie v smere osi y s koeficientom k = \>\"", "<>", 
                 RowBox[{"FormatExactValue", "[", "ky", "]"}]}]}], ";"}]}], 
             "]"}], ",", 
            RowBox[{"(*", 
             RowBox[{"Jednoduch\[SHacek]ie", " ", "koeficienty"}], "*)"}], 
            RowBox[{"If", "[", 
             RowBox[{
              RowBox[{
               RowBox[{"RandomReal", "[", "]"}], "<", "0.5"}], ",", 
              RowBox[{"(*", 
               RowBox[{
               "Skosenie", " ", "v", " ", "smere", " ", "osi", " ", "x"}], 
               "*)"}], 
              RowBox[{
               RowBox[{"kx", "=", 
                RowBox[{"RandomChoice", "[", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"-", "1"}], ",", 
                   RowBox[{"1", "/", "2"}], ",", "1"}], "}"}], "]"}]}], ";", 
               "\[IndentingNewLine]", 
               RowBox[{"ky", "=", "0"}], ";", "\[IndentingNewLine]", 
               RowBox[{"smer", "=", "\"\<v smere osi x\>\""}], ";", 
               "\[IndentingNewLine]", 
               RowBox[{"popis", "=", 
                RowBox[{
                "\"\<Skosenie v smere osi x s koeficientom k = \>\"", "<>", 
                 RowBox[{"FormatExactValue", "[", "kx", "]"}]}]}], ";"}], ",", 
              RowBox[{"(*", 
               RowBox[{
               "Skosenie", " ", "v", " ", "smere", " ", "osi", " ", "y"}], 
               "*)"}], 
              RowBox[{
               RowBox[{"kx", "=", "0"}], ";", "\[IndentingNewLine]", 
               RowBox[{"ky", "=", 
                RowBox[{"RandomChoice", "[", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"-", "1"}], ",", 
                   RowBox[{"1", "/", "2"}], ",", "1"}], "}"}], "]"}]}], ";", 
               "\[IndentingNewLine]", 
               RowBox[{"smer", "=", "\"\<v smere osi y\>\""}], ";", 
               "\[IndentingNewLine]", 
               RowBox[{"popis", "=", 
                RowBox[{
                "\"\<Skosenie v smere osi y s koeficientom k = \>\"", "<>", 
                 RowBox[{"FormatExactValue", "[", "ky", "]"}]}]}], ";"}]}], 
             "]"}]}], "]"}], ";", "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{
            RowBox[{
            "V\[YAcute]po\[CHacek]et", " ", "nov\[YAcute]ch", " ", 
             "s\[UAcute]radn\[IAcute]c", " ", "po", " ", "skosen\[IAcute]"}], 
            "-", 
            RowBox[{
            "PRESN\[CapitalYAcute]", " ", 
             "V\[CapitalYAcute]PO\[CapitalCHacek]ET"}]}], "*)"}], 
          RowBox[{"newCoordinates", "=", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"Simplify", "[", 
              RowBox[{
               RowBox[{"coordinates", "[", 
                RowBox[{"[", "1", "]"}], "]"}], "+", 
               RowBox[{"kx", "*", 
                RowBox[{"coordinates", "[", 
                 RowBox[{"[", "2", "]"}], "]"}]}]}], "]"}], ",", 
             RowBox[{"Simplify", "[", 
              RowBox[{
               RowBox[{"coordinates", "[", 
                RowBox[{"[", "2", "]"}], "]"}], "+", 
               RowBox[{"ky", "*", 
                RowBox[{"coordinates", "[", 
                 RowBox[{"[", "1", "]"}], "]"}]}]}], "]"}]}], "}"}]}], ";", 
          "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{
           "V\[YAcute]po\[CHacek]et", " ", "parametrov", " ", 
            "inverzn\[EAcute]ho", " ", "skosenia"}], "*)"}], 
          RowBox[{"invKx", "=", 
           RowBox[{"-", "kx"}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"invKy", "=", 
           RowBox[{"-", "ky"}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{
            RowBox[{"Test", " ", "inverznej", " ", "transform\[AAcute]cie"}], 
            "-", 
            RowBox[{
            "PRESN\[CapitalYAcute]", " ", 
             "V\[CapitalYAcute]PO\[CapitalCHacek]ET"}]}], "*)"}], 
          RowBox[{"invNewCoordinates", "=", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"Simplify", "[", 
              RowBox[{
               RowBox[{"newCoordinates", "[", 
                RowBox[{"[", "1", "]"}], "]"}], "+", 
               RowBox[{"invKx", "*", 
                RowBox[{"newCoordinates", "[", 
                 RowBox[{"[", "2", "]"}], "]"}]}]}], "]"}], ",", 
             RowBox[{"Simplify", "[", 
              RowBox[{
               RowBox[{"newCoordinates", "[", 
                RowBox[{"[", "2", "]"}], "]"}], "+", 
               RowBox[{"invKy", "*", 
                RowBox[{"newCoordinates", "[", 
                 RowBox[{"[", "1", "]"}], "]"}]}]}], "]"}]}], "}"}]}], ";", 
          "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{
            RowBox[{"Kontroly", "-", 
             RowBox[{"PRESN\[CapitalEAcute]", " ", "POROVNANIA"}]}], ",", 
            RowBox[{"len", " ", 
             RowBox[{"N", "[", "]"}], " ", "pre", " ", 
             "bezpe\[CHacek]n\[UAcute]", " ", "oblas\[THacek]"}]}], "*)"}], 
          RowBox[{"valid", "=", 
           RowBox[{"(*", 
            RowBox[{"Kontrola", ",", 
             RowBox[{
             "\[CHacek]i", " ", "v\[YAcute]sledn\[EAcute]", " ", 
              "s\[UAcute]radnice", " ", "s\[UAcute]", " ", "v", " ", 
              "bezpe\[CHacek]nej", " ", "oblasti"}]}], "*)"}], 
           RowBox[{
            RowBox[{"(", 
             RowBox[{
              RowBox[{
               RowBox[{"Abs", "[", 
                RowBox[{"N", "[", 
                 RowBox[{"newCoordinates", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], "]"}], "]"}], "<=", "14"}], 
              "&&", 
              RowBox[{
               RowBox[{"Abs", "[", 
                RowBox[{"N", "[", 
                 RowBox[{"newCoordinates", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "]"}], "]"}], "<=", 
               "14"}]}], ")"}], "&&", 
            RowBox[{"(*", 
             RowBox[{"Kontrola", ",", 
              RowBox[{
               RowBox[{
               "\[CHacek]i", " ", "inverzn\[EAcute]", " ", 
                "s\[UAcute]radnice", " ", "sa", " ", "rovnaj\[UAcute]", " ", 
                "p\[OHat]vodn\[YAcute]m"}], "-", 
               RowBox[{"PRESN\[CapitalEAcute]", " ", "POROVNANIE"}]}]}], 
             "*)"}], 
            RowBox[{"(", 
             RowBox[{
              RowBox[{
               RowBox[{"Simplify", "[", 
                RowBox[{
                 RowBox[{"coordinates", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], "-", 
                 RowBox[{"invNewCoordinates", "[", 
                  RowBox[{"[", "1", "]"}], "]"}]}], "]"}], "==", "0"}], "&&", 
              
              RowBox[{
               RowBox[{"Simplify", "[", 
                RowBox[{
                 RowBox[{"coordinates", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "-", 
                 RowBox[{"invNewCoordinates", "[", 
                  RowBox[{"[", "2", "]"}], "]"}]}], "]"}], "==", "0"}]}], 
             ")"}], "&&", 
            RowBox[{"(*", 
             RowBox[{
             "Zabr\[AAcute]nenie", " ", "trivi\[AAcute]lnemu", " ", 
              "pr\[IAcute]padu", " ", "kde", " ", "ni\[CHacek]", " ", 
              "nemen\[IAcute]"}], "*)"}], 
            RowBox[{"(", 
             RowBox[{
              RowBox[{"kx", "!=", "0"}], "||", 
              RowBox[{"ky", "!=", "0"}]}], ")"}]}]}], ";"}]}], "]"}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{"Vytvorenie", " ", "popisu"}], "*)"}], 
       RowBox[{"{", 
        RowBox[{
        "kx", ",", "ky", ",", "smer", ",", "popis", ",", "newCoordinates"}], 
        "}"}]}]}], "]"}]}], ";"}], "\[IndentingNewLine]", "\n", 
  RowBox[{"(*", 
   RowBox[{
   "Funkcia", " ", "na", " ", "vizualiz\[AAcute]ciu", " ", "skosenia"}], 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{
    RowBox[{"CreateVisualization", "[", 
     RowBox[{
     "originalCoordinates_", ",", "finalCoordinates_", ",", "k_", ",", 
      "smer_"}], "]"}], ":=", 
    RowBox[{"Module", "[", 
     RowBox[{
      RowBox[{"{", 
       RowBox[{
       "allCoordinates", ",", "xMin", ",", "xMax", ",", "yMin", ",", "yMax", 
        ",", "rangeBuffer", ",", "xRange", ",", "yRange", ",", "labelOffsets",
         ",", "kx", ",", "ky", ",", "brightGreen"}], "}"}], ",", 
      RowBox[{"(*", 
       RowBox[{
       "Defin\[IAcute]cia", " ", "jasnej\[SHacek]ej", " ", "zelenej", " ", 
        "farby"}], "*)"}], 
      RowBox[{
       RowBox[{"brightGreen", "=", 
        RowBox[{"RGBColor", "[", 
         RowBox[{"0", ",", "0.8", ",", "0.2"}], "]"}]}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{"Nastavenie", " ", "parametrov", " ", "skosenia"}], "*)"}], 
       RowBox[{"kx", "=", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{"smer", "==", "\"\<v smere osi x\>\""}], ",", "k", ",", 
          "0"}], "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"ky", "=", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{"smer", "==", "\"\<v smere osi y\>\""}], ",", "k", ",", 
          "0"}], "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
         RowBox[{
         "Spojenie", " ", "v\[SHacek]etk\[YAcute]ch", " ", 
          "s\[UAcute]radn\[IAcute]c", " ", "pre", " ", 
          "v\[YAcute]po\[CHacek]et", " ", "rozsahu"}], "-", 
         RowBox[{
          RowBox[{"N", "[", "]"}], " ", "len", " ", "pre", " ", 
          "zobrazenie"}]}], "*)"}], 
       RowBox[{"allCoordinates", "=", 
        RowBox[{"{", 
         RowBox[{"originalCoordinates", ",", "finalCoordinates"}], "}"}]}], 
       ";", "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
         RowBox[{
         "V\[YAcute]po\[CHacek]et", " ", "minim\[AAcute]lnych", " ", "a", " ",
           "maxim\[AAcute]lnych", " ", "hodn\[OHat]t"}], "-", 
         RowBox[{
          RowBox[{"N", "[", "]"}], " ", "pre", " ", 
          "vizualiz\[AAcute]ciu"}]}], "*)"}], 
       RowBox[{"xMin", "=", 
        RowBox[{"Min", "[", 
         RowBox[{"N", "[", 
          RowBox[{"allCoordinates", "[", 
           RowBox[{"[", 
            RowBox[{"All", ",", "1"}], "]"}], "]"}], "]"}], "]"}]}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"xMax", "=", 
        RowBox[{"Max", "[", 
         RowBox[{"N", "[", 
          RowBox[{"allCoordinates", "[", 
           RowBox[{"[", 
            RowBox[{"All", ",", "1"}], "]"}], "]"}], "]"}], "]"}]}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"yMin", "=", 
        RowBox[{"Min", "[", 
         RowBox[{"N", "[", 
          RowBox[{"allCoordinates", "[", 
           RowBox[{"[", 
            RowBox[{"All", ",", "2"}], "]"}], "]"}], "]"}], "]"}]}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"yMax", "=", 
        RowBox[{"Max", "[", 
         RowBox[{"N", "[", 
          RowBox[{"allCoordinates", "[", 
           RowBox[{"[", 
            RowBox[{"All", ",", "2"}], "]"}], "]"}], "]"}], "]"}]}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
        "Buffer", " ", "pre", " ", "estetick\[YAcute]", " ", "vzh\:013ead"}], 
        "*)"}], 
       RowBox[{"rangeBuffer", "=", "3"}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{"Nastavenie", " ", "rozsahu", " ", "os\[IAcute]"}], "*)"}], 
       RowBox[{"xRange", "=", 
        RowBox[{"{", 
         RowBox[{
          RowBox[{"Min", "[", 
           RowBox[{
            RowBox[{"-", "8"}], ",", 
            RowBox[{"xMin", "-", "rangeBuffer"}]}], "]"}], ",", 
          RowBox[{"Max", "[", 
           RowBox[{"8", ",", 
            RowBox[{"xMax", "+", "rangeBuffer"}]}], "]"}]}], "}"}]}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"yRange", "=", 
        RowBox[{"{", 
         RowBox[{
          RowBox[{"Min", "[", 
           RowBox[{
            RowBox[{"-", "8"}], ",", 
            RowBox[{"yMin", "-", "rangeBuffer"}]}], "]"}], ",", 
          RowBox[{"Max", "[", 
           RowBox[{"8", ",", 
            RowBox[{"yMax", "+", "rangeBuffer"}]}], "]"}]}], "}"}]}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
         RowBox[{
         "Vypo\[CHacek]\[IAcute]ta\[THacek]", " ", "offset", " ", "pre", " ", 
          "ozna\[CHacek]enie"}], ",", 
         RowBox[{
         "aby", " ", "sa", " ", "neprekr\[YAcute]val", " ", "s", " ", 
          "\[CHacek]iarou"}]}], "*)"}], 
       RowBox[{"labelOffsets", "=", 
        RowBox[{"Module", "[", 
         RowBox[{
          RowBox[{"{", 
           RowBox[{
            RowBox[{"originalCoord", "=", "originalCoordinates"}], ",", 
            RowBox[{"finalCoord", "=", "finalCoordinates"}], ",", 
            RowBox[{"offset", "=", 
             RowBox[{"{", 
              RowBox[{"0.5", ",", "0.5"}], "}"}]}]}], "}"}], ",", 
          RowBox[{"(*", 
           RowBox[{
            RowBox[{
            "Upravi\[THacek]", " ", "offset", " ", "pod\:013eea", " ", 
             "polohy", " ", "s\[UAcute]radn\[IAcute]c"}], "-", 
            RowBox[{
             RowBox[{"N", "[", "]"}], " ", "len", " ", "pre", " ", 
             "vizualiz\[AAcute]ciu"}]}], "*)"}], 
          RowBox[{
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"N", "[", 
               RowBox[{"originalCoord", "[", 
                RowBox[{"[", "1", "]"}], "]"}], "]"}], ">", "8"}], ",", 
             RowBox[{
              RowBox[{"offset", "[", 
               RowBox[{"[", "1", "]"}], "]"}], "=", 
              RowBox[{"-", "0.8"}]}]}], "]"}], ";", "\[IndentingNewLine]", 
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"N", "[", 
               RowBox[{"originalCoord", "[", 
                RowBox[{"[", "2", "]"}], "]"}], "]"}], ">", "8"}], ",", 
             RowBox[{
              RowBox[{"offset", "[", 
               RowBox[{"[", "2", "]"}], "]"}], "=", 
              RowBox[{"-", "0.8"}]}]}], "]"}], ";", "\[IndentingNewLine]", 
           RowBox[{"{", 
            RowBox[{"offset", ",", "offset"}], "}"}]}]}], "]"}]}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"Graphics", "[", 
        RowBox[{
         RowBox[{"{", 
          RowBox[{"(*", "Grid", "*)"}], 
          RowBox[{"LightGray", ",", "Thin", ",", 
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
                   RowBox[{"[", "2", "]"}], "]"}], ",", "y"}], "}"}]}], "}"}],
               "]"}], ",", 
             RowBox[{"{", 
              RowBox[{"y", ",", 
               RowBox[{"Ceiling", "[", 
                RowBox[{"yRange", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", 
               RowBox[{"Floor", "[", 
                RowBox[{"yRange", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "]"}], ",", "1"}], "}"}]}], 
            "]"}], ",", 
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
                 RowBox[{"[", "2", "]"}], "]"}], "]"}], ",", "1"}], "}"}]}], 
            "]"}], ",", 
           RowBox[{"(*", 
            RowBox[{
             RowBox[{"Pomocn\[EAcute]", " ", "\[CHacek]iary"}], "-", 
             RowBox[{"zelen\[EAcute]", " ", "preru\[SHacek]ovan\[EAcute]"}]}],
             "*)"}], "brightGreen", ",", "Dashed", ",", 
           RowBox[{"Line", "[", 
            RowBox[{"{", 
             RowBox[{"originalCoordinates", ",", "finalCoordinates"}], "}"}], 
            "]"}], ",", 
           RowBox[{"(*", 
            RowBox[{
             RowBox[{
             "Body", " ", "s", " ", "bielym", " ", "pozad\[IAcute]m"}], "-", 
             "zv\[ADoubleDot]\[CHacek]\[SHacek]en\[EAcute]"}], "*)"}], 
           "White", ",", 
           RowBox[{"Disk", "[", 
            RowBox[{"originalCoordinates", ",", "0.35"}], "]"}], ",", 
           RowBox[{"Disk", "[", 
            RowBox[{"finalCoordinates", ",", "0.35"}], "]"}], ",", 
           RowBox[{"(*", 
            RowBox[{
            "Body", "-", "zv\[ADoubleDot]\[CHacek]\[SHacek]en\[EAcute]"}], 
            "*)"}], "Blue", ",", 
           RowBox[{"PointSize", "[", "0.025", "]"}], ",", 
           RowBox[{"Point", "[", "originalCoordinates", "]"}], ",", "Red", 
           ",", 
           RowBox[{"PointSize", "[", "0.025", "]"}], ",", 
           RowBox[{"Point", "[", "finalCoordinates", "]"}], ",", 
           RowBox[{"(*", 
            RowBox[{
            "Labels", " ", "s", " ", "optimalizovan\[YAcute]mi", " ", 
             "poz\[IAcute]ciami"}], "*)"}], 
           RowBox[{"Text", "[", 
            RowBox[{
             RowBox[{"Style", "[", 
              RowBox[{"\"\<P\>\"", ",", "Blue", ",", "Bold", ",", "16"}], 
              "]"}], ",", 
             RowBox[{"originalCoordinates", "+", 
              RowBox[{"labelOffsets", "[", 
               RowBox[{"[", "1", "]"}], "]"}]}]}], "]"}], ",", 
           RowBox[{"Text", "[", 
            RowBox[{
             RowBox[{"Style", "[", 
              RowBox[{"\"\<P'\>\"", ",", "Red", ",", "Bold", ",", "16"}], 
              "]"}], ",", 
             RowBox[{"finalCoordinates", "+", 
              RowBox[{"labelOffsets", "[", 
               RowBox[{"[", "2", "]"}], "]"}]}]}], "]"}], ",", 
           RowBox[{"(*", "Osi", "*)"}], "Black", ",", "Thick", ",", 
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
                 RowBox[{"[", "2", "]"}], "]"}]}], "}"}]}], "}"}], "]"}], ",", 
           RowBox[{"Text", "[", 
            RowBox[{
             RowBox[{"Style", "[", 
              RowBox[{"\"\<x\>\"", ",", "Black", ",", "Bold"}], "]"}], ",", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{
                RowBox[{"xRange", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "-", "0.5"}], ",", 
               RowBox[{"-", "0.5"}]}], "}"}]}], "]"}], ",", 
           RowBox[{"Text", "[", 
            RowBox[{
             RowBox[{"Style", "[", 
              RowBox[{"\"\<y\>\"", ",", "Black", ",", "Bold"}], "]"}], ",", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"-", "0.5"}], ",", 
               RowBox[{
                RowBox[{"yRange", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "-", "0.5"}]}], "}"}]}], 
            "]"}], ",", 
           RowBox[{"(*", 
            RowBox[{
            "\[CapitalCHacek]\[IAcute]sla", " ", "na", " ", "osiach"}], 
            "*)"}], 
           RowBox[{"Table", "[", 
            RowBox[{
             RowBox[{"Text", "[", 
              RowBox[{
               RowBox[{"Style", "[", 
                RowBox[{"i", ",", "Gray", ",", "10"}], "]"}], ",", 
               RowBox[{"{", 
                RowBox[{"i", ",", 
                 RowBox[{"-", "0.3"}]}], "}"}]}], "]"}], ",", 
             RowBox[{"{", 
              RowBox[{"i", ",", 
               RowBox[{"Ceiling", "[", 
                RowBox[{"xRange", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", 
               RowBox[{"Floor", "[", 
                RowBox[{"xRange", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "}"}]}], "]"}], ",", 
           RowBox[{"Table", "[", 
            RowBox[{
             RowBox[{"Text", "[", 
              RowBox[{
               RowBox[{"Style", "[", 
                RowBox[{"i", ",", "Gray", ",", "10"}], "]"}], ",", 
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"-", "0.3"}], ",", "i"}], "}"}]}], "]"}], ",", 
             RowBox[{"{", 
              RowBox[{"i", ",", 
               RowBox[{"Ceiling", "[", 
                RowBox[{"yRange", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", 
               RowBox[{"Floor", "[", 
                RowBox[{"yRange", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "}"}]}], "]"}]}], 
          "}"}], ",", 
         RowBox[{"PlotRange", "->", 
          RowBox[{"{", 
           RowBox[{"xRange", ",", "yRange"}], "}"}]}], ",", 
         RowBox[{"AspectRatio", "->", "1"}], ",", 
         RowBox[{"ImageSize", "->", "500"}], ",", 
         RowBox[{"ImagePadding", "->", "20"}]}], "]"}]}]}], "]"}]}], ";"}], 
  "\[IndentingNewLine]", "\n", 
  RowBox[{"(*", 
   RowBox[{"Hlavn\[AAcute]", " ", "funkcia"}], 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{
    RowBox[{"BodSkosenie", "[", "inputCoordinates_", "]"}], ":=", 
    RowBox[{"Module", "[", 
     RowBox[{
      RowBox[{"{", 
       RowBox[{
       "normalizedCoordinates", ",", "shearParams", ",", "outputCoordinates", 
        ",", "kx", ",", "ky", ",", "smer", ",", "popis", ",", "invKx", ",", 
        "invKy", ",", "k", ",", "koefX", ",", "koefY", ",", 
        RowBox[{"detectInputOutput", "=", "False"}], ",", 
        RowBox[{"finalCoordinates", "=", "None"}]}], "}"}], ",", 
      RowBox[{"(*", 
       RowBox[{"Skontrolujeme", ",", 
        RowBox[{
         RowBox[{
         "\[CHacek]i", " ", "vstup", " ", "m\[AAcute]", " ", "viac", " ", 
          "ako", " ", "2", " ", "hodnoty"}], "-", 
         RowBox[{
         "mo\[ZHacek]no", " ", "obsahuje", " ", "aj", " ", "v\[YAcute]sledok",
           " ", "predch\[AAcute]dzaj\[UAcute]cej", " ", 
          "transform\[AAcute]cie"}]}]}], "*)"}], 
      RowBox[{
       RowBox[{"If", "[", 
        RowBox[{
         RowBox[{
          RowBox[{
           RowBox[{"Length", "[", 
            RowBox[{"Flatten", "[", "inputCoordinates", "]"}], "]"}], ">", 
           "2"}], "&&", 
          RowBox[{
           RowBox[{"Mod", "[", 
            RowBox[{
             RowBox[{"Length", "[", 
              RowBox[{"Flatten", "[", "inputCoordinates", "]"}], "]"}], ",", 
             "2"}], "]"}], "==", "0"}]}], ",", 
         RowBox[{"(*", 
          RowBox[{
          "Rozdel\[IAcute]me", " ", "vstup", " ", "na", " ", 
           "vstupn\[EAcute]", " ", "a", " ", 
           "o\[CHacek]ak\[AAcute]van\[EAcute]", " ", 
           "v\[YAcute]stupn\[EAcute]", " ", "s\[UAcute]radnice"}], "*)"}], 
         RowBox[{
          RowBox[{"normalizedCoordinates", "=", 
           RowBox[{"Take", "[", 
            RowBox[{
             RowBox[{"Flatten", "[", "inputCoordinates", "]"}], ",", "2"}], 
            "]"}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{
           "Berieme", " ", "posledn\[EAcute]", " ", "dve", " ", "hodnoty", 
            " ", "ako", " ", "o\[CHacek]ak\[AAcute]van\[YAcute]", " ", 
            "v\[YAcute]stup"}], "*)"}], 
          RowBox[{"finalCoordinates", "=", 
           RowBox[{"Take", "[", 
            RowBox[{
             RowBox[{"Flatten", "[", "inputCoordinates", "]"}], ",", 
             RowBox[{"-", "2"}]}], "]"}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"detectInputOutput", "=", "True"}]}], ",", 
         RowBox[{"(*", 
          RowBox[{
          "Inak", " ", "normalizujeme", " ", "vstupn\[EAcute]", " ", 
           "s\[UAcute]radnice", " ", "a", " ", "\[ZHacek]iadne", " ", 
           "v\[YAcute]stupn\[EAcute]", " ", "ne\[CHacek]ak\[AAcute]me"}], 
          "*)"}], 
         RowBox[{
          RowBox[{"normalizedCoordinates", "=", 
           RowBox[{"NormalizeCoordinates", "[", "inputCoordinates", "]"}]}], 
          ";"}]}], "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
         RowBox[{"Z\[IAcute]skanie", " ", "parametrov", " ", "skosenia"}], 
         "-", 
         RowBox[{
         "bu\[DHacek]", " ", "detekovan\[EAcute]", " ", "alebo", " ", 
          "generovan\[EAcute]"}]}], "*)"}], 
       RowBox[{"shearParams", "=", 
        RowBox[{"If", "[", 
         RowBox[{"detectInputOutput", ",", 
          RowBox[{"GetShearingParameters", "[", 
           RowBox[{"normalizedCoordinates", ",", "finalCoordinates"}], "]"}], 
          ",", 
          RowBox[{
          "GetShearingParameters", "[", "normalizedCoordinates", "]"}]}], 
         "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"kx", "=", 
        RowBox[{"shearParams", "[", 
         RowBox[{"[", "1", "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"ky", "=", 
        RowBox[{"shearParams", "[", 
         RowBox[{"[", "2", "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"smer", "=", 
        RowBox[{"shearParams", "[", 
         RowBox[{"[", "3", "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"popis", "=", 
        RowBox[{"shearParams", "[", 
         RowBox[{"[", "4", "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"outputCoordinates", "=", 
        RowBox[{"shearParams", "[", 
         RowBox[{"[", "5", "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
        "Nastavenie", " ", "koeficientu", " ", "pre", " ", "zobrazenie", " ", 
         "v", " ", "postupe"}], "*)"}], 
       RowBox[{"k", "=", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{"smer", "==", "\"\<v smere osi x\>\""}], ",", "kx", ",", 
          "ky"}], "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{"Inverzn\[EAcute]", " ", "parametre"}], "*)"}], 
       RowBox[{"invKx", "=", 
        RowBox[{"-", "kx"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"invKy", "=", 
        RowBox[{"-", "ky"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
        "Koeficienty", " ", "pre", " ", "transforma\[CHacek]n\[UAcute]", " ", 
         "maticu"}], "*)"}], 
       RowBox[{"koefX", "=", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{"smer", "==", "\"\<v smere osi x\>\""}], ",", "k", ",", 
          "0"}], "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"koefY", "=", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{"smer", "==", "\"\<v smere osi y\>\""}], ",", "k", ",", 
          "0"}], "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", "Nadpis", "*)"}], 
       RowBox[{"Print", "[", 
        RowBox[{"Style", "[", 
         RowBox[{
         "\"\<GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE BODU - \
SKOSENIE\>\"", ",", "Bold", ",", "16"}], "]"}], "]"}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"Print", "[", 
        RowBox[{"Style", "[", 
         RowBox[{
         "\"\<========================================\>\"", ",", "Bold"}], 
         "]"}], "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", "ZADANIE", "*)"}], 
       RowBox[{"Print", "[", 
        RowBox[{"Style", "[", 
         RowBox[{"\"\<\\nZADANIE:\>\"", ",", "Bold", ",", "14"}], "]"}], 
        "]"}], ";", "\n", 
       RowBox[{
       "Print", "[", "\"\<Majme bod P so s\[UAcute]radnicami:\>\"", "]"}], 
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
                 RowBox[{"normalizedCoordinates", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], ",", "Blue"}], "]"}], "}"}],
               ",", 
              RowBox[{"{", 
               RowBox[{"Style", "[", 
                RowBox[{
                 RowBox[{"normalizedCoordinates", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], ",", "Blue"}], "]"}], 
               "}"}]}], "}"}], "]"}]}], "}"}], "]"}], "]"}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"Print", "[", 
        RowBox[{
         RowBox[{
         "\"\<\\nVykonajte skosenie bodu v 2D priestore \>\"", "<>", "smer", 
          "<>", "\"\< s koeficientom k = \>\""}], ",", 
         RowBox[{"Style", "[", 
          RowBox[{
           RowBox[{"FormatExactValue", "[", "k", "]"}], ",", "Red"}], "]"}], 
         ",", "\"\<.\>\""}], "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{
       "Print", "[", 
        "\"\<\\nTransforma\[CHacek]n\[AAcute] matica skosenia:\>\"", "]"}], 
       ";", "\[IndentingNewLine]", 
       RowBox[{"If", "[", 
        RowBox[{
         RowBox[{"smer", "==", "\"\<v smere osi x\>\""}], ",", 
         RowBox[{"Print", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"{", 
              RowBox[{
               RowBox[{"Style", "[", 
                RowBox[{"1", ",", "Red"}], "]"}], ",", 
               RowBox[{"Style", "[", 
                RowBox[{"k", ",", "Red"}], "]"}]}], "}"}], ",", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"Style", "[", 
                RowBox[{"0", ",", "Red"}], "]"}], ",", 
               RowBox[{"Style", "[", 
                RowBox[{"1", ",", "Red"}], "]"}]}], "}"}]}], "}"}], "]"}], 
          "]"}], ",", 
         RowBox[{"Print", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"{", 
              RowBox[{
               RowBox[{"Style", "[", 
                RowBox[{"1", ",", "Red"}], "]"}], ",", 
               RowBox[{"Style", "[", 
                RowBox[{"0", ",", "Red"}], "]"}]}], "}"}], ",", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"Style", "[", 
                RowBox[{"k", ",", "Red"}], "]"}], ",", 
               RowBox[{"Style", "[", 
                RowBox[{"1", ",", "Red"}], "]"}]}], "}"}]}], "}"}], "]"}], 
          "]"}]}], "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", "POSTUP", "*)"}], 
       RowBox[{"Print", "[", 
        RowBox[{"Style", "[", 
         RowBox[{"\"\<\\nPOSTUP:\>\"", ",", "Bold", ",", "14"}], "]"}], "]"}],
        ";", "\[IndentingNewLine]", 
       RowBox[{
       "Print", "[", 
        "\"\<Pre v\[YAcute]po\[CHacek]et pou\[ZHacek]ijeme maticu skosenia:\>\
\"", "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"Print", "[", 
        RowBox[{"Style", "[", 
         RowBox[{
          RowBox[{
          "\"\<Transforma\[CHacek]n\[AAcute] matica pre skosenie \>\"", "<>", 
           "smer", "<>", "\"\<:\>\""}], ",", "Bold"}], "]"}], "]"}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"If", "[", 
        RowBox[{
         RowBox[{"smer", "==", "\"\<v smere osi x\>\""}], ",", 
         RowBox[{
          RowBox[{"Print", "[", 
           RowBox[{"MatrixForm", "[", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{
                RowBox[{"Style", "[", 
                 RowBox[{"1", ",", "Red"}], "]"}], ",", 
                RowBox[{"Style", "[", 
                 RowBox[{"\"\<k\>\"", ",", "Red"}], "]"}]}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{"Style", "[", 
                 RowBox[{"0", ",", "Red"}], "]"}], ",", 
                RowBox[{"Style", "[", 
                 RowBox[{"1", ",", "Red"}], "]"}]}], "}"}]}], "}"}], "]"}], 
           "]"}], ";", "\[IndentingNewLine]", 
          RowBox[{
          "Print", "[", 
           "\"\<\\nVzorec pre v\[YAcute]po\[CHacek]et nov\[YAcute]ch s\
\[UAcute]radn\[IAcute]c pri skosen\[IAcute] v smere osi x:\>\"", "]"}], ";", 
          "\[IndentingNewLine]", 
          RowBox[{"Print", "[", "\"\<x' = x + k\[CenterDot]y\>\"", "]"}], ";",
           "\[IndentingNewLine]", 
          RowBox[{"Print", "[", "\"\<y' = y\>\"", "]"}]}], ",", 
         RowBox[{
          RowBox[{"Print", "[", 
           RowBox[{"MatrixForm", "[", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{
                RowBox[{"Style", "[", 
                 RowBox[{"1", ",", "Red"}], "]"}], ",", 
                RowBox[{"Style", "[", 
                 RowBox[{"0", ",", "Red"}], "]"}]}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{"Style", "[", 
                 RowBox[{"\"\<k\>\"", ",", "Red"}], "]"}], ",", 
                RowBox[{"Style", "[", 
                 RowBox[{"1", ",", "Red"}], "]"}]}], "}"}]}], "}"}], "]"}], 
           "]"}], ";", "\[IndentingNewLine]", 
          RowBox[{
          "Print", "[", 
           "\"\<\\nVzorec pre v\[YAcute]po\[CHacek]et nov\[YAcute]ch s\
\[UAcute]radn\[IAcute]c pri skosen\[IAcute] v smere osi y:\>\"", "]"}], ";", 
          "\[IndentingNewLine]", 
          RowBox[{"Print", "[", "\"\<x' = x\>\"", "]"}], ";", 
          "\[IndentingNewLine]", 
          RowBox[{"Print", "[", "\"\<y' = k\[CenterDot]x + y\>\"", "]"}]}]}], 
        "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"Print", "[", 
        RowBox[{"\"\<\\nNa\[SHacek]a hodnota k = \>\"", ",", 
         RowBox[{"FormatExactValue", "[", "k", "]"}]}], "]"}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{
       "Print", "[", "\"\<\\nV\[YAcute]po\[CHacek]et pre bod P:\>\"", "]"}], 
       ";", "\[IndentingNewLine]", 
       RowBox[{"Module", "[", 
        RowBox[{
         RowBox[{"{", 
          RowBox[{
           RowBox[{"x", "=", 
            RowBox[{"normalizedCoordinates", "[", 
             RowBox[{"[", "1", "]"}], "]"}]}], ",", 
           RowBox[{"y", "=", 
            RowBox[{"normalizedCoordinates", "[", 
             RowBox[{"[", "2", "]"}], "]"}]}], ",", "newX", ",", "newY", ",", 
           "expandedX", ",", "expandedY", ",", "simplifiedX", ",", 
           "simplifiedY"}], "}"}], ",", 
         RowBox[{"(*", 
          RowBox[{
          "V\[YAcute]po\[CHacek]et", " ", "nov\[YAcute]ch", " ", 
           "s\[UAcute]radn\[IAcute]c", " ", "s", " ", "presn\[YAcute]m", " ", 
           "v\[YAcute]sledkom"}], "*)"}], 
         RowBox[{
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"smer", "==", "\"\<v smere osi x\>\""}], ",", 
            RowBox[{
             RowBox[{"newX", "=", 
              RowBox[{"x", "+", 
               RowBox[{"k", "*", "y"}]}]}], ";", "\[IndentingNewLine]", 
             RowBox[{"newY", "=", "y"}]}], ",", 
            RowBox[{
             RowBox[{"newX", "=", "x"}], ";", "\[IndentingNewLine]", 
             RowBox[{"newY", "=", 
              RowBox[{
               RowBox[{"k", "*", "x"}], "+", "y"}]}]}]}], "]"}], ";", 
          "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{
           "Rozvinut\[EAcute]", " ", "a", " ", "zjednodu\[SHacek]en\[EAcute]",
             " ", "v\[YAcute]razy"}], "*)"}], 
          RowBox[{"expandedX", "=", 
           RowBox[{"Expand", "[", "newX", "]"}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"expandedY", "=", 
           RowBox[{"Expand", "[", "newY", "]"}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"simplifiedX", "=", 
           RowBox[{"Simplify", "[", "newX", "]"}]}], ";", 
          "\[IndentingNewLine]", 
          RowBox[{"simplifiedY", "=", 
           RowBox[{"Simplify", "[", "newY", "]"}]}], ";", 
          "\[IndentingNewLine]", 
          RowBox[{"Print", "[", 
           RowBox[{
            RowBox[{"Style", "[", 
             RowBox[{
             "\"\<P\[OHat]vodn\[EAcute] s\[UAcute]radnice:\>\"", ",", 
              "Bold"}], "]"}], ",", "\"\< [\>\"", ",", 
            RowBox[{"FormatExactValue", "[", "x", "]"}], ",", "\"\<, \>\"", 
            ",", 
            RowBox[{"FormatExactValue", "[", "y", "]"}], ",", "\"\<]\>\""}], 
           "]"}], ";", "\[IndentingNewLine]", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"smer", "==", "\"\<v smere osi x\>\""}], ",", 
            RowBox[{"(*", 
             RowBox[{
              RowBox[{
              "Detailn\[YAcute]", " ", "postup", " ", "pre", " ", "x"}], "-", 
              
              RowBox[{
              "ov\[UAcute]", " ", "s\[UAcute]radnicu", " ", "pri", " ", 
               "skosen\[IAcute]", " ", "v", " ", "smere", " ", "x"}]}], 
             "*)"}], 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{"Style", "[", 
               RowBox[{
               "\"\<V\[YAcute]po\[CHacek]et x-ovej s\[UAcute]radnice:\>\"", 
                ",", "Bold"}], "]"}], "]"}], ";", "\[IndentingNewLine]", 
             RowBox[{"Print", "[", 
              RowBox[{"\"\<x' = x + k\[CenterDot]y = \>\"", ",", 
               RowBox[{"FormatExactValue", "[", "x", "]"}], ",", 
               "\"\< + \>\"", ",", 
               RowBox[{"FormatExactValue", "[", "k", "]"}], ",", 
               "\"\<\[CenterDot]\>\"", ",", 
               RowBox[{"FormatExactValue", "[", "y", "]"}]}], "]"}], ";", 
             "\[IndentingNewLine]", 
             RowBox[{"(*", 
              RowBox[{
               RowBox[{
               "Ak", " ", "je", " ", "v\[YAcute]raz", " ", 
                "komplexn\[YAcute]"}], ",", 
               RowBox[{
               "zobraz\[IAcute]me", " ", "rozvinut\[YAcute]", " ", "tvar"}]}],
               "*)"}], 
             RowBox[{"If", "[", 
              RowBox[{
               RowBox[{
                RowBox[{"y", "!=", "0"}], "&&", 
                RowBox[{"k", "!=", "0"}]}], ",", 
               RowBox[{"Print", "[", 
                RowBox[{"\"\<x' = \>\"", ",", 
                 RowBox[{"FormatExactValue", "[", "x", "]"}], ",", 
                 "\"\< + \>\"", ",", 
                 RowBox[{"FormatExactValue", "[", 
                  RowBox[{"k", "*", "y"}], "]"}]}], "]"}]}], "]"}], ";", 
             "\[IndentingNewLine]", 
             RowBox[{"Print", "[", 
              RowBox[{"\"\<x' = \>\"", ",", 
               RowBox[{"FormatExactValue", "[", "simplifiedX", "]"}]}], "]"}],
              ";", "\[IndentingNewLine]", 
             RowBox[{"(*", 
              RowBox[{
               RowBox[{
               "Detailn\[YAcute]", " ", "postup", " ", "pre", " ", "y"}], "-", 
               RowBox[{
               "ov\[UAcute]", " ", "s\[UAcute]radnicu", " ", "pri", " ", 
                "skosen\[IAcute]", " ", "v", " ", "smere", " ", "x"}]}], 
              "*)"}], 
             RowBox[{"Print", "[", 
              RowBox[{"Style", "[", 
               RowBox[{
               "\"\<V\[YAcute]po\[CHacek]et y-ovej s\[UAcute]radnice:\>\"", 
                ",", "Bold"}], "]"}], "]"}], ";", "\[IndentingNewLine]", 
             RowBox[{"Print", "[", 
              RowBox[{"\"\<y' = y = \>\"", ",", 
               RowBox[{"FormatExactValue", "[", "y", "]"}]}], "]"}]}], ",", 
            RowBox[{"(*", 
             RowBox[{
              RowBox[{
              "Detailn\[YAcute]", " ", "postup", " ", "pre", " ", "x"}], "-", 
              
              RowBox[{
              "ov\[UAcute]", " ", "s\[UAcute]radnicu", " ", "pri", " ", 
               "skosen\[IAcute]", " ", "v", " ", "smere", " ", "y"}]}], 
             "*)"}], 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{"Style", "[", 
               RowBox[{
               "\"\<V\[YAcute]po\[CHacek]et x-ovej s\[UAcute]radnice:\>\"", 
                ",", "Bold"}], "]"}], "]"}], ";", "\[IndentingNewLine]", 
             RowBox[{"Print", "[", 
              RowBox[{"\"\<x' = x = \>\"", ",", 
               RowBox[{"FormatExactValue", "[", "x", "]"}]}], "]"}], ";", 
             "\[IndentingNewLine]", 
             RowBox[{"(*", 
              RowBox[{
               RowBox[{
               "Detailn\[YAcute]", " ", "postup", " ", "pre", " ", "y"}], "-", 
               RowBox[{
               "ov\[UAcute]", " ", "s\[UAcute]radnicu", " ", "pri", " ", 
                "skosen\[IAcute]", " ", "v", " ", "smere", " ", "y"}]}], 
              "*)"}], 
             RowBox[{"Print", "[", 
              RowBox[{"Style", "[", 
               RowBox[{
               "\"\<V\[YAcute]po\[CHacek]et y-ovej s\[UAcute]radnice:\>\"", 
                ",", "Bold"}], "]"}], "]"}], ";", "\[IndentingNewLine]", 
             RowBox[{"Print", "[", 
              RowBox[{"\"\<y' = k\[CenterDot]x + y = \>\"", ",", 
               RowBox[{"FormatExactValue", "[", "k", "]"}], ",", 
               "\"\<\[CenterDot]\>\"", ",", 
               RowBox[{"FormatExactValue", "[", "x", "]"}], ",", 
               "\"\< + \>\"", ",", 
               RowBox[{"FormatExactValue", "[", "y", "]"}]}], "]"}], ";", 
             "\[IndentingNewLine]", 
             RowBox[{"(*", 
              RowBox[{
               RowBox[{
               "Ak", " ", "je", " ", "v\[YAcute]raz", " ", 
                "komplexn\[YAcute]"}], ",", 
               RowBox[{
               "zobraz\[IAcute]me", " ", "rozvinut\[YAcute]", " ", "tvar"}]}],
               "*)"}], 
             RowBox[{"If", "[", 
              RowBox[{
               RowBox[{
                RowBox[{"x", "!=", "0"}], "&&", 
                RowBox[{"k", "!=", "0"}]}], ",", 
               RowBox[{"Print", "[", 
                RowBox[{"\"\<y' = \>\"", ",", 
                 RowBox[{"FormatExactValue", "[", 
                  RowBox[{"k", "*", "x"}], "]"}], ",", "\"\< + \>\"", ",", 
                 RowBox[{"FormatExactValue", "[", "y", "]"}]}], "]"}]}], 
              "]"}], ";", "\[IndentingNewLine]", 
             RowBox[{"Print", "[", 
              RowBox[{"\"\<y' = \>\"", ",", 
               RowBox[{"FormatExactValue", "[", "simplifiedY", "]"}]}], 
              "]"}]}]}], "]"}], ";", "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{
            RowBox[{"V\[YAcute]sledn\[EAcute]", " ", "s\[UAcute]radnice"}], 
            "-", 
            RowBox[{
            "plne", " ", "rozvinut\[EAcute]", " ", "a", " ", 
             "zjednodu\[SHacek]en\[EAcute]"}]}], "*)"}], 
          RowBox[{"Print", "[", 
           RowBox[{
            RowBox[{"Style", "[", 
             RowBox[{
             "\"\<V\[YAcute]sledn\[EAcute] s\[UAcute]radnice:\>\"", ",", 
              "Bold"}], "]"}], ",", "\"\< [\>\"", ",", 
            RowBox[{"FormatExactValue", "[", "simplifiedX", "]"}], ",", 
            "\"\<, \>\"", ",", 
            RowBox[{"FormatExactValue", "[", "simplifiedY", "]"}], ",", 
            "\"\<]\>\""}], "]"}], ";", "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{
           "Overenie", " ", "inverznej", " ", "transform\[AAcute]cie"}], 
           "*)"}], 
          RowBox[{"Print", "[", 
           RowBox[{"Style", "[", 
            RowBox[{
            "\"\<\\nOverenie pomocou inverznej transform\[AAcute]cie:\>\"", 
             ",", "Bold"}], "]"}], "]"}], ";", "\[IndentingNewLine]", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"smer", "==", "\"\<v smere osi x\>\""}], ",", 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{
              "\"\<Pre inverzn\[EAcute] skosenie pou\[ZHacek]ijeme k' = -k = \
\>\"", ",", 
               RowBox[{"FormatExactValue", "[", 
                RowBox[{"-", "k"}], "]"}]}], "]"}], ";", 
             "\[IndentingNewLine]", 
             RowBox[{"Print", "[", 
              RowBox[{"\"\<x = x' + k'\[CenterDot]y' = \>\"", ",", 
               RowBox[{"FormatExactValue", "[", 
                RowBox[{"outputCoordinates", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", "\"\< + (\>\"", 
               ",", 
               RowBox[{"FormatExactValue", "[", 
                RowBox[{"-", "k"}], "]"}], ",", "\"\<)\[CenterDot]\>\"", ",", 
               
               RowBox[{"FormatExactValue", "[", 
                RowBox[{"outputCoordinates", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "]"}], ";", 
             "\[IndentingNewLine]", 
             RowBox[{"(*", 
              RowBox[{
              "Rozvineme", " ", "v\[YAcute]po\[CHacek]et", " ", "pre", " ", 
               "lep\[SHacek]iu", " ", "\[CHacek]itate\:013enos\[THacek]"}], 
              "*)"}], 
             RowBox[{"If", "[", 
              RowBox[{
               RowBox[{
                RowBox[{
                 RowBox[{"outputCoordinates", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "!=", "0"}], "&&", 
                RowBox[{"k", "!=", "0"}]}], ",", 
               RowBox[{"Print", "[", 
                RowBox[{"\"\<x = \>\"", ",", 
                 RowBox[{"FormatExactValue", "[", 
                  RowBox[{"outputCoordinates", "[", 
                   RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", "\"\< + (\>\"",
                  ",", 
                 RowBox[{"FormatExactValue", "[", 
                  RowBox[{
                   RowBox[{"-", "k"}], "*", 
                   RowBox[{"outputCoordinates", "[", 
                    RowBox[{"[", "2", "]"}], "]"}]}], "]"}], ",", 
                 "\"\<)\>\""}], "]"}]}], "]"}], ";", "\[IndentingNewLine]", 
             RowBox[{"Print", "[", 
              RowBox[{"\"\<x = \>\"", ",", 
               RowBox[{"FormatExactValue", "[", 
                RowBox[{"Simplify", "[", 
                 RowBox[{
                  RowBox[{"outputCoordinates", "[", 
                   RowBox[{"[", "1", "]"}], "]"}], "-", 
                  RowBox[{"k", "*", 
                   RowBox[{"outputCoordinates", "[", 
                    RowBox[{"[", "2", "]"}], "]"}]}]}], "]"}], "]"}], ",", 
               "\"\< = \>\"", ",", 
               RowBox[{"FormatExactValue", "[", "x", "]"}]}], "]"}], ";", 
             "\[IndentingNewLine]", 
             RowBox[{"Print", "[", 
              RowBox[{"\"\<y = y' = \>\"", ",", 
               RowBox[{"FormatExactValue", "[", 
                RowBox[{"outputCoordinates", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "]"}], ",", "\"\< = \>\"", 
               ",", 
               RowBox[{"FormatExactValue", "[", "y", "]"}]}], "]"}]}], ",", 
            RowBox[{
             RowBox[{"Print", "[", 
              RowBox[{
              "\"\<Pre inverzn\[EAcute] skosenie pou\[ZHacek]ijeme k' = -k = \
\>\"", ",", 
               RowBox[{"FormatExactValue", "[", 
                RowBox[{"-", "k"}], "]"}]}], "]"}], ";", 
             "\[IndentingNewLine]", 
             RowBox[{"Print", "[", 
              RowBox[{"\"\<x = x' = \>\"", ",", 
               RowBox[{"FormatExactValue", "[", 
                RowBox[{"outputCoordinates", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", "\"\< = \>\"", 
               ",", 
               RowBox[{"FormatExactValue", "[", "x", "]"}]}], "]"}], ";", 
             "\[IndentingNewLine]", 
             RowBox[{"Print", "[", 
              RowBox[{"\"\<y = k'\[CenterDot]x' + y' = (\>\"", ",", 
               RowBox[{"FormatExactValue", "[", 
                RowBox[{"-", "k"}], "]"}], ",", "\"\<)\[CenterDot]\>\"", ",", 
               
               RowBox[{"FormatExactValue", "[", 
                RowBox[{"outputCoordinates", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", "\"\< + \>\"", 
               ",", 
               RowBox[{"FormatExactValue", "[", 
                RowBox[{"outputCoordinates", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "]"}], ";", 
             "\[IndentingNewLine]", 
             RowBox[{"(*", 
              RowBox[{
              "Rozvineme", " ", "v\[YAcute]po\[CHacek]et", " ", "pre", " ", 
               "lep\[SHacek]iu", " ", "\[CHacek]itate\:013enos\[THacek]"}], 
              "*)"}], 
             RowBox[{"If", "[", 
              RowBox[{
               RowBox[{
                RowBox[{
                 RowBox[{"outputCoordinates", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], "!=", "0"}], "&&", 
                RowBox[{"k", "!=", "0"}]}], ",", 
               RowBox[{"Print", "[", 
                RowBox[{"\"\<y = \>\"", ",", 
                 RowBox[{"FormatExactValue", "[", 
                  RowBox[{
                   RowBox[{"-", "k"}], "*", 
                   RowBox[{"outputCoordinates", "[", 
                    RowBox[{"[", "1", "]"}], "]"}]}], "]"}], ",", 
                 "\"\< + \>\"", ",", 
                 RowBox[{"FormatExactValue", "[", 
                  RowBox[{"outputCoordinates", "[", 
                   RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "]"}]}], "]"}], 
             ";", "\[IndentingNewLine]", 
             RowBox[{"Print", "[", 
              RowBox[{"\"\<y = \>\"", ",", 
               RowBox[{"FormatExactValue", "[", 
                RowBox[{"Simplify", "[", 
                 RowBox[{
                  RowBox[{
                   RowBox[{"-", "k"}], "*", 
                   RowBox[{"outputCoordinates", "[", 
                    RowBox[{"[", "1", "]"}], "]"}]}], "+", 
                  RowBox[{"outputCoordinates", "[", 
                   RowBox[{"[", "2", "]"}], "]"}]}], "]"}], "]"}], ",", 
               "\"\< = \>\"", ",", 
               RowBox[{"FormatExactValue", "[", "y", "]"}]}], "]"}]}]}], 
           "]"}], ";", "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{
            RowBox[{"Maticov\[YAcute]", " ", "z\[AAcute]pis"}], "-", 
            RowBox[{
            "s", " ", "plne", " ", "rozvinut\[YAcute]mi", " ", 
             "v\[YAcute]razmi"}]}], "*)"}], 
          RowBox[{"Print", "[", 
           RowBox[{"Style", "[", 
            RowBox[{
            "\"\<\\nMaticov\[YAcute] z\[AAcute]pis:\>\"", ",", "Bold"}], 
            "]"}], "]"}], ";", "\[IndentingNewLine]", 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"smer", "==", "\"\<v smere osi x\>\""}], ",", 
            RowBox[{"Print", "[", 
             RowBox[{"Row", "[", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{"MatrixForm", "[", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"{", 
                    RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{"1", ",", "Red"}], "]"}], ",", 
                    RowBox[{"Style", "[", 
                    RowBox[{"k", ",", "Red"}], "]"}]}], "}"}], ",", 
                   RowBox[{"{", 
                    RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{"0", ",", "Red"}], "]"}], ",", 
                    RowBox[{"Style", "[", 
                    RowBox[{"1", ",", "Red"}], "]"}]}], "}"}]}], "}"}], "]"}],
                 ",", "\"\< \[CenterDot] \>\"", ",", 
                RowBox[{"MatrixForm", "[", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"{", "x", "}"}], ",", 
                   RowBox[{"{", "y", "}"}]}], "}"}], "]"}], ",", 
                "\"\< = \>\"", ",", 
                RowBox[{"MatrixForm", "[", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"{", 
                    RowBox[{"Style", "[", 
                    RowBox[{"simplifiedX", ",", "Red"}], "]"}], "}"}], ",", 
                   RowBox[{"{", 
                    RowBox[{"Style", "[", 
                    RowBox[{"simplifiedY", ",", "Red"}], "]"}], "}"}]}], 
                  "}"}], "]"}]}], "}"}], "]"}], "]"}], ",", 
            RowBox[{"Print", "[", 
             RowBox[{"Row", "[", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{"MatrixForm", "[", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"{", 
                    RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{"1", ",", "Red"}], "]"}], ",", 
                    RowBox[{"Style", "[", 
                    RowBox[{"0", ",", "Red"}], "]"}]}], "}"}], ",", 
                   RowBox[{"{", 
                    RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{"k", ",", "Red"}], "]"}], ",", 
                    RowBox[{"Style", "[", 
                    RowBox[{"1", ",", "Red"}], "]"}]}], "}"}]}], "}"}], "]"}],
                 ",", "\"\< \[CenterDot] \>\"", ",", 
                RowBox[{"MatrixForm", "[", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"{", "x", "}"}], ",", 
                   RowBox[{"{", "y", "}"}]}], "}"}], "]"}], ",", 
                "\"\< = \>\"", ",", 
                RowBox[{"MatrixForm", "[", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"{", 
                    RowBox[{"Style", "[", 
                    RowBox[{"simplifiedX", ",", "Red"}], "]"}], "}"}], ",", 
                   RowBox[{"{", 
                    RowBox[{"Style", "[", 
                    RowBox[{"simplifiedY", ",", "Red"}], "]"}], "}"}]}], 
                  "}"}], "]"}]}], "}"}], "]"}], "]"}]}], "]"}], ";"}]}], 
        "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"Print", "[", 
        RowBox[{"Style", "[", 
         RowBox[{
         "\"\<\\nV\[CapitalYAcute]SLEDOK:\>\"", ",", "Bold", ",", "14"}], 
         "]"}], "]"}], ";", "\n", 
       RowBox[{
       "Print", "[", "\"\<S\[UAcute]radnice bodu po skosen\[IAcute]:\>\"", 
        "]"}], ";", "\[IndentingNewLine]", "\n", 
       RowBox[{"(*", 
        RowBox[{
        "Plne", " ", "rozvinut\[AAcute]", " ", "a", " ", 
         "zjednodu\[SHacek]en\[AAcute]", " ", "matica", " ", 
         "v\[YAcute]sledku"}], "*)"}], "\[IndentingNewLine]", 
       RowBox[{"Module", "[", 
        RowBox[{
         RowBox[{"{", "expandedOutputCoordinates", "}"}], ",", 
         RowBox[{"(*", 
          RowBox[{
          "Rozvinieme", " ", "a", " ", "zjednodu\[SHacek]\[IAcute]me", " ", 
           "ka\[ZHacek]d\[YAcute]", " ", "prvok", " ", 
           "v\[YAcute]sledn\[YAcute]ch", " ", "s\[UAcute]radn\[IAcute]c"}], 
          "*)"}], 
         RowBox[{
          RowBox[{"expandedOutputCoordinates", "=", 
           RowBox[{"Map", "[", 
            RowBox[{"Expand", ",", "outputCoordinates"}], "]"}]}], ";", 
          "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{
           "Zobraz\[IAcute]me", " ", "upraven\[EAcute]", " ", 
            "s\[UAcute]radnice", " ", "v", " ", "maticovom", " ", "tvare"}], 
           "*)"}], 
          RowBox[{"Print", "[", 
           RowBox[{"Row", "[", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"\"\<P' = \>\"", ",", "Red", ",", "Bold"}], "]"}], ",", 
              RowBox[{"MatrixForm", "[", 
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"{", 
                  RowBox[{"Style", "[", 
                   RowBox[{
                    RowBox[{"expandedOutputCoordinates", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], ",", "Red"}], "]"}], 
                  "}"}], ",", 
                 RowBox[{"{", 
                  RowBox[{"Style", "[", 
                   RowBox[{
                    RowBox[{"expandedOutputCoordinates", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], ",", "Red"}], "]"}], 
                  "}"}]}], "}"}], "]"}]}], "}"}], "]"}], "]"}], ";"}]}], 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Pre", " ", "jednoduch\[SHacek]iu", " ", 
          "\[CHacek]itate\:013enos\[THacek]", " ", "prid\[AAcute]me", " ", 
          "aj", " ", "klasick\[YAcute]", " ", "z\[AAcute]pis"}], "*)"}], 
        "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", "VIZUALIZ\[CapitalAAcute]CIA", "*)"}], 
       RowBox[{"Print", "[", 
        RowBox[{"Style", "[", 
         RowBox[{"\"\<\\nVIZUALIZ\[CapitalAAcute]CIA:\>\"", ",", "Bold"}], 
         "]"}], "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"Print", "[", 
        RowBox[{"CreateVisualization", "[", 
         RowBox[{
         "normalizedCoordinates", ",", "outputCoordinates", ",", "k", ",", 
          "smer"}], "]"}], "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", "LEGENDA", "*)"}], 
       RowBox[{"Print", "[", 
        RowBox[{"Style", "[", 
         RowBox[{"\"\<\\nLEGENDA:\>\"", ",", "Bold", ",", "14"}], "]"}], 
        "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{
       "Print", "[", 
        "\"\<\[Bullet] Modr\[YAcute] bod: P\[OHat]vodn\[YAcute] bod P\>\"", 
        "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{
       "Print", "[", 
        "\"\<\[Bullet] \[CapitalCHacek]erven\[YAcute] bod: Skosen\[YAcute] \
bod P'\>\"", "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{
       "Print", "[", 
        "\"\<\[Bullet] Zelen\[AAcute] preru\[SHacek]ovan\[AAcute] \
\[CHacek]iara: Dr\[AAcute]ha pohybu bodu pri skosen\[IAcute]\>\"", "]"}], ";",
        "\[IndentingNewLine]", 
       RowBox[{
       "Print", "[", 
        "\"\<\[Bullet] Mrie\[ZHacek]ka: S\[UAcute]radnicov\[YAcute] syst\
\[EAcute]m\>\"", "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"Print", "[", 
        RowBox[{"Style", "[", 
         RowBox[{
         "\"\<\\nP\[OHat]vodn\[EAcute] s\[UAcute]radnice \
(modr\[AAcute]):\>\"", ",", "Bold"}], "]"}], "]"}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"Print", "[", 
        RowBox[{"Row", "[", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"Style", "[", 
            RowBox[{"\"\<Bod P: \>\"", ",", 
             RowBox[{"RGBColor", "[", 
              RowBox[{"0.1", ",", "0.1", ",", "1"}], "]"}]}], "]"}], ",", 
           "normalizedCoordinates"}], "}"}], "]"}], "]"}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"Print", "[", 
        RowBox[{"Style", "[", 
         RowBox[{
         "\"\<\\nV\[YAcute]sledn\[EAcute] s\[UAcute]radnice (\[CHacek]erven\
\[AAcute]):\>\"", ",", "Bold"}], "]"}], "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"Module", "[", 
        RowBox[{
         RowBox[{"{", "expandedOutput", "}"}], ",", 
         RowBox[{
          RowBox[{"expandedOutput", "=", 
           RowBox[{"Expand", "[", "outputCoordinates", "]"}]}], ";", 
          "\[IndentingNewLine]", 
          RowBox[{"Print", "[", 
           RowBox[{"Row", "[", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"\"\<Bod P': \>\"", ",", 
                RowBox[{"RGBColor", "[", 
                 RowBox[{"1", ",", "0.1", ",", "0.1"}], "]"}]}], "]"}], ",", 
              "expandedOutput"}], "}"}], "]"}], "]"}], ";"}]}], "]"}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"Print", "[", 
        RowBox[{"Style", "[", 
         RowBox[{
         "\"\<\\nInverzn\[AAcute] transform\[AAcute]cia:\>\"", ",", "Bold"}], 
         "]"}], "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{
       "Print", "[", 
        "\"\<Pre vr\[AAcute]tenie do p\[OHat]vodn\[EAcute]ho stavu pou\
\[ZHacek]ite koeficient skosenia:\>\"", "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"If", "[", 
        RowBox[{
         RowBox[{"smer", "==", "\"\<v smere osi x\>\""}], ",", 
         RowBox[{"Print", "[", 
          RowBox[{"\"\<k' = -k = \>\"", ",", 
           RowBox[{"FormatExactValue", "[", 
            RowBox[{"-", "k"}], "]"}], ",", "\"\< v smere osi x\>\""}], "]"}],
          ",", 
         RowBox[{"Print", "[", 
          RowBox[{"\"\<k' = -k = \>\"", ",", 
           RowBox[{"FormatExactValue", "[", 
            RowBox[{"-", "k"}], "]"}], ",", "\"\< v smere osi y\>\""}], 
          "]"}]}], "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
        "MATEMATICK\[CapitalEAcute]", " ", "VLASTNOSTI", " ", 
         "TRANSFORM\[CapitalAAcute]CIE"}], "*)"}], 
       RowBox[{"Print", "[", 
        RowBox[{"Style", "[", 
         RowBox[{
         "\"\<\\nMATEMATICK\[CapitalEAcute] VLASTNOSTI:\>\"", ",", "Bold", 
          ",", "14"}], "]"}], "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{
       "Print", "[", 
        "\"\<\[Bullet] Skosenie je line\[AAcute]rna transform\[AAcute]cia, \
ktor\[AAcute] zachov\[AAcute]va rovnobe\[ZHacek]nos\[THacek] priamok s jednou \
z os\[IAcute]\>\"", "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"If", "[", 
        RowBox[{
         RowBox[{"smer", "==", "\"\<v smere osi x\>\""}], ",", 
         RowBox[{
          RowBox[{
          "Print", "[", 
           "\"\<\[Bullet] Pri skosen\[IAcute] v smere osi x zost\[AAcute]vaj\
\[UAcute] zachovan\[EAcute] y-ov\[EAcute] s\[UAcute]radnice v\[SHacek]etk\
\[YAcute]ch bodov\>\"", "]"}], ";", "\[IndentingNewLine]", 
          RowBox[{
          "Print", "[", 
           "\"\<\[Bullet] Zvisl\[EAcute] priamky (rovnobe\[ZHacek]n\[EAcute] \
s osou y) zost\[AAcute]vaj\[UAcute] zvisl\[EAcute]\>\"", "]"}]}], ",", 
         RowBox[{
          RowBox[{
          "Print", "[", 
           "\"\<\[Bullet] Pri skosen\[IAcute] v smere osi y zost\[AAcute]vaj\
\[UAcute] zachovan\[EAcute] x-ov\[EAcute] s\[UAcute]radnice v\[SHacek]etk\
\[YAcute]ch bodov\>\"", "]"}], ";", "\[IndentingNewLine]", 
          RowBox[{
          "Print", "[", 
           "\"\<\[Bullet] Vodorovn\[EAcute] priamky (rovnobe\[ZHacek]n\
\[EAcute] s osou x) zost\[AAcute]vaj\[UAcute] vodorovn\[EAcute]\>\"", 
           "]"}]}]}], "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{
       "Print", "[", 
        "\"\<\[Bullet] Skosenie men\[IAcute] uhly a vzdialenosti medzi bodmi, \
no zachov\[AAcute]va kolinearnos\[THacek] bodov\>\"", "]"}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{
       "Print", "[", 
        "\"\<\[Bullet] Body le\[ZHacek]iace na osi, ktor\[AAcute] je rovnobe\
\[ZHacek]n\[AAcute] so smerom skosenia, sa nemenia\>\"", "]"}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{
       "Print", "[", 
        "\"\<\[Bullet] Skosenie je af\[IAcute]nna transform\[AAcute]cia, ktor\
\[AAcute] nezachov\[AAcute]va vzdialenosti ani uhly\>\"", "]"}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{"\[CapitalSHacek]peci\[AAcute]lne", " ", "pr\[IAcute]pady"}], 
        "*)"}], 
       RowBox[{"Print", "[", 
        RowBox[{"Style", "[", 
         RowBox[{
         "\"\<\\n\[CapitalSHacek]PECI\[CapitalAAcute]LNE \
PR\[CapitalIAcute]PADY:\>\"", ",", "Bold", ",", "14"}], "]"}], "]"}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{
       "Print", "[", 
        "\"\<\[Bullet] Pre k = 0 je skosenie identickou \
transform\[AAcute]ciou (bod sa nemen\[IAcute])\>\"", "]"}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{
       "Print", "[", 
        "\"\<\[Bullet] Pre ve\:013ek\[EAcute] hodnoty |k| sa bod \
v\[YAcute]razne presunie v pr\[IAcute]slu\[SHacek]nom smere\>\"", "]"}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{
       "Print", "[", 
        "\"\<\[Bullet] Ak le\[ZHacek]\[IAcute] p\[OHat]vodn\[YAcute] bod na \
osi x, pri skosen\[IAcute] v smere osi x sa jeho poloha nemen\[IAcute]\>\"", 
        "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{
       "Print", "[", 
        "\"\<\[Bullet] Ak le\[ZHacek]\[IAcute] p\[OHat]vodn\[YAcute] bod na \
osi y, pri skosen\[IAcute] v smere osi y sa jeho poloha nemen\[IAcute]\>\"", 
        "]"}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
        "Vr\[AAcute]tenie", " ", "nov\[YAcute]ch", " ", 
         "s\[UAcute]radn\[IAcute]c", " ", "pre", " ", 
         "pr\[IAcute]padn\[EAcute]", " ", "\[DHacek]al\[SHacek]ie", " ", 
         "transform\[AAcute]cie"}], "*)"}], "outputCoordinates"}]}], "]"}]}], 
   ";"}], "\[IndentingNewLine]", "\n", 
  RowBox[{"(*", 
   RowBox[{
   "Defin\[IAcute]cia", " ", "chybov\[YAcute]ch", " ", "spr\[AAcute]v"}], 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{
    RowBox[{"BodSkosenie", "::", "invalidInput"}], "=", 
    "\"\<Neplatn\[YAcute] vstup. O\[CHacek]ak\[AAcute]vaj\[UAcute] sa s\
\[UAcute]radnice bodu v 2D priestore.\>\""}], ";"}], 
  "\[IndentingNewLine]"}], "\n", 
 RowBox[{
  RowBox[{"End", "[", "]"}], ";"}], "\n", 
 RowBox[{
  RowBox[{"EndPackage", "[", "]"}], ";"}]}], "Input",
 CellChangeTimes->{{3.952874877615095*^9, 3.952874877617379*^9}, 
   3.952875083158778*^9, 3.952875177816028*^9, 3.95287524742797*^9},
 CellLabel->
  "In[1234]:=",ExpressionUUID->"3b975b38-4d31-4b2a-b55c-ca09303e4d28"]
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
Cell[554, 20, 87040, 2064, 7602, "Input",ExpressionUUID->"3b975b38-4d31-4b2a-b55c-ca09303e4d28"]
}
]
*)

