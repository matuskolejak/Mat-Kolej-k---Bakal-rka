(* Content-type: application/vnd.wolfram.mathematica *)

(*** Wolfram Notebook File ***)
(* http://www.wolfram.com/nb *)

(* CreatedBy='Wolfram 14.1' *)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[       154,          7]
NotebookDataLength[     53138,       1280]
NotebookOptionsPosition[     52472,       1261]
NotebookOutlinePosition[     52863,       1277]
CellTagsIndexPosition[     52820,       1274]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{
Cell["", "Text",
 CellChangeTimes->{{3.952792901413539*^9, 
  3.952792981615295*^9}},ExpressionUUID->"5c8a06ec-6e7d-40b0-b49f-\
cfd4a27be3c4"],

Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{"::", "Package", "::"}], "*)"}], 
  RowBox[{"(*", " ", 
   RowBox[{"::", "Package", "::"}], "*)"}], 
  RowBox[{"(*", "*)"}], 
  RowBox[{"(*", " ", 
   RowBox[{"::", "Package", "::"}], "*)"}], 
  RowBox[{"(*", "*)"}], 
  RowBox[{"(*", " ", 
   RowBox[{"::", "Package", "::"}], "*)"}], 
  RowBox[{"(*", "*)"}], 
  RowBox[{
   RowBox[{
    RowBox[{"BeginPackage", "[", 
     RowBox[{"\"\<BodHardBalik`Transforms`Posun`\>\"", ",", 
      RowBox[{"{", "\"\<BodHardBalik`\>\"", "}"}]}], "]"}], ";"}], "\n", 
   "\[IndentingNewLine]", "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"BodPosun", "::", "usage"}], "=", 
     "\"\<BodPosun[point_] zobraz\[IAcute] interakt\[IAcute]vny tutori\
\[AAcute]l pre posun bodu a vr\[AAcute]ti nov\[EAcute] \
s\[UAcute]radnice.\>\""}], ";"}], "\[IndentingNewLine]", "\n", 
   RowBox[{
    RowBox[{"Begin", "[", "\"\<`Private`\>\"", "]"}], ";"}], "\n", 
   "\[IndentingNewLine]", "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"NormalizePoint", "[", "point_", "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", "processedPoint", "}"}], ",", 
       RowBox[{"(*", 
        RowBox[{
        "Spracovanie", " ", "r\[OHat]znych", " ", "vstupn\[YAcute]ch", " ", 
         "form\[AAcute]tov"}], "*)"}], 
       RowBox[{
        RowBox[{"processedPoint", "=", 
         RowBox[{"Which", "[", 
          RowBox[{"(*", 
           RowBox[{"Ak", " ", "je", " ", "to", " ", "vektor"}], "*)"}], 
          RowBox[{
           RowBox[{"VectorQ", "[", 
            RowBox[{"point", ",", "NumberQ"}], "]"}], ",", "point", ",", 
           RowBox[{"(*", 
            RowBox[{
            "Ak", " ", "je", " ", "to", " ", "zoznam", " ", "s", " ", "dvoma",
              " ", "prvkami"}], "*)"}], 
           RowBox[{
            RowBox[{"ListQ", "[", "point", "]"}], "&&", 
            RowBox[{
             RowBox[{"Length", "[", "point", "]"}], "==", "2"}], "&&", 
            RowBox[{"AllTrue", "[", 
             RowBox[{"point", ",", "NumberQ"}], "]"}]}], ",", "point", ",", 
           RowBox[{"(*", "Inak", "*)"}], "True", ",", 
           RowBox[{
            RowBox[{"Message", "[", 
             RowBox[{"BodPosun", "::", "invalidInput"}], "]"}], ";", 
            "\[IndentingNewLine]", 
            RowBox[{"Abort", "[", "]"}]}]}], "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Kontrola", " ", "\[CHacek]i", " ", "s\[UAcute]", " ", 
          "s\[UAcute]radnice", " ", "v", " ", "bezpe\[CHacek]nom", " ", 
          "rozsahu"}], "*)"}], 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"Max", "[", 
            RowBox[{
             RowBox[{"Abs", "[", 
              RowBox[{"processedPoint", "[", 
               RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", 
             RowBox[{"Abs", "[", 
              RowBox[{"processedPoint", "[", 
               RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "]"}], ">", "10"}], 
          ",", 
          RowBox[{"(*", 
           RowBox[{
            RowBox[{"\[CapitalSHacek]k\[AAcute]lovanie", " ", "bodu"}], ",", 
            RowBox[{
            "ak", " ", "je", " ", "pr\[IAcute]li\[SHacek]", " ", 
             "\[DHacek]aleko"}]}], "*)"}], 
          RowBox[{
           RowBox[{"processedPoint", "=", 
            RowBox[{"processedPoint", "/", 
             RowBox[{"(", 
              RowBox[{
               RowBox[{"Max", "[", 
                RowBox[{"Abs", "[", 
                 RowBox[{"Flatten", "[", "processedPoint", "]"}], "]"}], 
                "]"}], "/", "6"}], ")"}]}]}], ";"}]}], "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Vrac\[IAcute]ame", " ", "presn\[EAcute]", " ", "hodnoty", " ", 
          "bez", " ", "zaokr\[UAcute]h\:013eovania"}], "*)"}], 
        "processedPoint"}]}], "]"}]}], ";"}], "\[IndentingNewLine]", "\n", 
   RowBox[{"(*", 
    RowBox[{
     RowBox[{
     "Funkcia", " ", "na", " ", "generovanie", " ", "parametrov", " ", 
      "posunu"}], "-", 
     RowBox[{
     "UPRAVEN\[CapitalAAcute]", " ", "PRE", " ", "ZACHOVANIE", " ", 
      "PRESN\[CapitalYAcute]CH", " ", "HODN\[CapitalOHat]T"}]}], "*)"}], 
   "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"GetTranslationParameters", "[", "point_", "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"dx", ",", "dy", ",", 
         RowBox[{"valid", "=", "False"}], ",", "newPoint", ",", "allPoints", 
         ",", "xMin", ",", "xMax", ",", "yMin", ",", "yMax", ",", "invDx", 
         ",", "invDy", ",", "invNewPoint", ",", "invPoint", ",", 
         "narocnost"}], "}"}], ",", 
       RowBox[{"(*", 
        RowBox[{
        "Anal\[YAcute]za", " ", "p\[OHat]vodn\[YAcute]ch", " ", 
         "s\[UAcute]radn\[IAcute]c"}], "*)"}], 
       RowBox[{
        RowBox[{"xMin", "=", 
         RowBox[{"point", "[", 
          RowBox[{"[", "1", "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"xMax", "=", 
         RowBox[{"point", "[", 
          RowBox[{"[", "1", "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"yMin", "=", 
         RowBox[{"point", "[", 
          RowBox[{"[", "2", "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"yMax", "=", 
         RowBox[{"point", "[", 
          RowBox[{"[", "2", "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "N\[AAcute]ro\[CHacek]nej\[SHacek]ie", " ", "hodnoty", " ", "posunu",
           " ", "pre", " ", "Hard", " ", "bal\[IAcute]k"}], "*)"}], 
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
            RowBox[{
            "Generovanie", " ", "konkr\[EAcute]tnych", " ", "posunov"}], "-", 
            
            RowBox[{
            "n\[AAcute]ro\[CHacek]nej\[SHacek]ie", " ", "pre", " ", "Hard", 
             " ", "bal\[IAcute]k"}]}], "*)"}], 
          RowBox[{
           RowBox[{"If", "[", 
            RowBox[{
             RowBox[{"narocnost", "<", "0.7"}], ",", 
             RowBox[{"(*", 
              RowBox[{
               RowBox[{"N\[AAcute]ro\[CHacek]nej\[SHacek]ie", " ", "posuny"}],
                "-", 
               RowBox[{
               "v\[ADoubleDot]\[CHacek]\[SHacek]ie", " ", "hodnoty", " ", 
                RowBox[{"a", "/", "alebo"}], " ", "aj", " ", 
                "z\[AAcute]porn\[EAcute]", " ", "hodnoty"}]}], "*)"}], 
             RowBox[{
              RowBox[{"dx", "=", 
               RowBox[{"RandomChoice", "[", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"-", "7"}], ",", 
                  RowBox[{"-", "6"}], ",", 
                  RowBox[{"-", "5"}], ",", 
                  RowBox[{"-", "4"}], ",", 
                  RowBox[{"-", "3"}], ",", "3", ",", "4", ",", "5", ",", "6", 
                  ",", "7"}], "}"}], "]"}]}], ";", "\[IndentingNewLine]", 
              RowBox[{"dy", "=", 
               RowBox[{"RandomChoice", "[", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"-", "7"}], ",", 
                  RowBox[{"-", "6"}], ",", 
                  RowBox[{"-", "5"}], ",", 
                  RowBox[{"-", "4"}], ",", 
                  RowBox[{"-", "3"}], ",", "3", ",", "4", ",", "5", ",", "6", 
                  ",", "7"}], "}"}], "]"}]}]}], ",", 
             RowBox[{"(*", 
              RowBox[{
              "Klasick\[EAcute]", " ", "posuny", " ", "ako", " ", "v", " ", 
               "Medium", " ", "bal\[IAcute]ku"}], "*)"}], 
             RowBox[{
              RowBox[{"dx", "=", 
               RowBox[{"RandomChoice", "[", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"-", "5"}], ",", 
                  RowBox[{"-", "4"}], ",", 
                  RowBox[{"-", "3"}], ",", 
                  RowBox[{"-", "2"}], ",", "2", ",", "3", ",", "4", ",", 
                  "5"}], "}"}], "]"}]}], ";", "\[IndentingNewLine]", 
              RowBox[{"dy", "=", 
               RowBox[{"RandomChoice", "[", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"-", "5"}], ",", 
                  RowBox[{"-", "4"}], ",", 
                  RowBox[{"-", "3"}], ",", 
                  RowBox[{"-", "2"}], ",", "2", ",", "3", ",", "4", ",", 
                  "5"}], "}"}], "]"}]}]}]}], "]"}], ";", 
           "\[IndentingNewLine]", 
           RowBox[{"(*", 
            RowBox[{
             RowBox[{
             "V\[YAcute]po\[CHacek]et", " ", "nov\[EAcute]ho", " ", "bodu", 
              " ", "po", " ", "posune"}], "-", 
             RowBox[{
             "PRESN\[CapitalEAcute]", " ", "HODNOTY", " ", "BEZ", " ", 
              "ZAOKR\[CapitalUAcute]H\:013dOVANIA"}]}], "*)"}], 
           RowBox[{"newPoint", "=", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{
               RowBox[{"point", "[", 
                RowBox[{"[", "1", "]"}], "]"}], "+", "dx"}], ",", 
              RowBox[{
               RowBox[{"point", "[", 
                RowBox[{"[", "2", "]"}], "]"}], "+", "dy"}]}], "}"}]}], ";", 
           "\[IndentingNewLine]", 
           RowBox[{"(*", 
            RowBox[{
            "V\[YAcute]po\[CHacek]et", " ", "parametrov", " ", 
             "inverzn\[EAcute]ho", " ", "posunu"}], "*)"}], 
           RowBox[{"invDx", "=", 
            RowBox[{"-", "dx"}]}], ";", "\[IndentingNewLine]", 
           RowBox[{"invDy", "=", 
            RowBox[{"-", "dy"}]}], ";", "\[IndentingNewLine]", 
           RowBox[{"(*", 
            RowBox[{
             RowBox[{"Test", " ", "inverznej", " ", "transform\[AAcute]cie"}],
              "-", 
             RowBox[{"PRESN\[CapitalEAcute]", " ", "POROVNANIE"}]}], "*)"}], 
           RowBox[{"invNewPoint", "=", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{
               RowBox[{"newPoint", "[", 
                RowBox[{"[", "1", "]"}], "]"}], "+", "invDx"}], ",", 
              RowBox[{
               RowBox[{"newPoint", "[", 
                RowBox[{"[", "2", "]"}], "]"}], "+", "invDy"}]}], "}"}]}], 
           ";", "\[IndentingNewLine]", 
           RowBox[{"valid", "=", 
            RowBox[{"(*", 
             RowBox[{"Kontrola", ",", 
              RowBox[{
              "\[CHacek]i", " ", "v\[YAcute]sledn\[YAcute]", " ", "bod", " ", 
               "je", " ", "v", " ", "bezpe\[CHacek]nej", " ", "oblasti"}]}], 
             "*)"}], 
            RowBox[{
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"newPoint", "[", 
                RowBox[{"[", "1", "]"}], "]"}], "]"}], "<=", "14"}], "&&", 
             RowBox[{
              RowBox[{"Abs", "[", 
               RowBox[{"newPoint", "[", 
                RowBox[{"[", "2", "]"}], "]"}], "]"}], "<=", "14"}], "&&", 
             RowBox[{"(*", 
              RowBox[{
               RowBox[{
               "Kontrola", " ", "minim\[AAcute]lnej", " ", "vzdialenosti", 
                " ", "posunu"}], "-", 
               RowBox[{
               "aspo\[NHacek]", " ", "o", " ", "3", " ", "jednotky"}]}], 
              "*)"}], 
             RowBox[{
              RowBox[{"Sqrt", "[", 
               RowBox[{
                RowBox[{"dx", "^", "2"}], "+", 
                RowBox[{"dy", "^", "2"}]}], "]"}], ">=", "3"}], "&&", 
             RowBox[{"(*", 
              RowBox[{"Kontrola", ",", 
               RowBox[{
                RowBox[{
                "\[CHacek]i", " ", "inverzn\[EAcute]", " ", 
                 "s\[UAcute]radnice", " ", "sa", " ", "rovnaj\[UAcute]", " ", 
                 "p\[OHat]vodn\[YAcute]m"}], "-", 
                RowBox[{"PRESN\[CapitalEAcute]", " ", "POROVNANIE"}]}]}], 
              "*)"}], 
             RowBox[{
              RowBox[{"Total", "[", 
               RowBox[{"Flatten", "[", 
                RowBox[{"Abs", "[", 
                 RowBox[{"point", "-", "invNewPoint"}], "]"}], "]"}], "]"}], 
              "<", "0.00001"}]}]}], ";"}]}], "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{"Vytvorenie", " ", "popisu"}], "*)"}], 
        RowBox[{"{", 
         RowBox[{"dx", ",", "dy", ",", 
          RowBox[{"\"\<Posun o [\>\"", "<>", 
           RowBox[{"ToString", "[", "dx", "]"}], "<>", "\"\<, \>\"", "<>", 
           RowBox[{"ToString", "[", "dy", "]"}], "<>", "\"\<]\>\""}], ",", 
          "newPoint"}], "}"}]}]}], "]"}]}], ";"}], "\[IndentingNewLine]", 
   "\[IndentingNewLine]", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"CreateVisualization", "[", 
      RowBox[{
      "originalPoint_", ",", "finalPoint_", ",", "translationVector_"}], 
      "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
        "allPoints", ",", "xMin", ",", "xMax", ",", "yMin", ",", "yMax", ",", 
         "rangeBuffer", ",", "xRange", ",", "yRange", ",", 
         RowBox[{"dx", "=", 
          RowBox[{"translationVector", "[", 
           RowBox[{"[", "1", "]"}], "]"}]}], ",", 
         RowBox[{"dy", "=", 
          RowBox[{"translationVector", "[", 
           RowBox[{"[", "2", "]"}], "]"}]}]}], "}"}], ",", 
       RowBox[{"(*", 
        RowBox[{
        "Spojenie", " ", "v\[SHacek]etk\[YAcute]ch", " ", "bodov", " ", "pre",
          " ", "v\[YAcute]po\[CHacek]et", " ", "rozsahu"}], "*)"}], 
       RowBox[{
        RowBox[{"allPoints", "=", 
         RowBox[{"{", 
          RowBox[{"originalPoint", ",", "finalPoint"}], "}"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "V\[YAcute]po\[CHacek]et", " ", "minim\[AAcute]lnych", " ", "a", " ",
           "maxim\[AAcute]lnych", " ", "hodn\[OHat]t"}], "*)"}], 
        RowBox[{"xMin", "=", 
         RowBox[{"Min", "[", 
          RowBox[{"allPoints", "[", 
           RowBox[{"[", 
            RowBox[{"All", ",", "1"}], "]"}], "]"}], "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"xMax", "=", 
         RowBox[{"Max", "[", 
          RowBox[{"allPoints", "[", 
           RowBox[{"[", 
            RowBox[{"All", ",", "1"}], "]"}], "]"}], "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"yMin", "=", 
         RowBox[{"Min", "[", 
          RowBox[{"allPoints", "[", 
           RowBox[{"[", 
            RowBox[{"All", ",", "2"}], "]"}], "]"}], "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"yMax", "=", 
         RowBox[{"Max", "[", 
          RowBox[{"allPoints", "[", 
           RowBox[{"[", 
            RowBox[{"All", ",", "2"}], "]"}], "]"}], "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Buffer", " ", "pre", " ", "estetick\[YAcute]", " ", "vzh\:013ead"}],
          "*)"}], 
        RowBox[{"rangeBuffer", "=", "2"}], ";", "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{"Nastavenie", " ", "rozsahu", " ", "os\[IAcute]"}], "*)"}], 
        RowBox[{"xRange", "=", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"Min", "[", 
            RowBox[{
             RowBox[{"-", "11"}], ",", 
             RowBox[{"xMin", "-", "rangeBuffer"}]}], "]"}], ",", 
           RowBox[{"Max", "[", 
            RowBox[{"11", ",", 
             RowBox[{"xMax", "+", "rangeBuffer"}]}], "]"}]}], "}"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"yRange", "=", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"Min", "[", 
            RowBox[{
             RowBox[{"-", "11"}], ",", 
             RowBox[{"yMin", "-", "rangeBuffer"}]}], "]"}], ",", 
           RowBox[{"Max", "[", 
            RowBox[{"11", ",", 
             RowBox[{"yMax", "+", "rangeBuffer"}]}], "]"}]}], "}"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Definujeme", " ", "offsety", " ", "pre", " ", "ozna\[CHacek]enia", 
          " ", "bodov", " ", "s", " ", "lep\[SHacek]\[IAcute]m", " ", 
          "odstupom"}], "*)"}], 
        RowBox[{"labelOffset1", "=", 
         RowBox[{"{", 
          RowBox[{"1.0", ",", "0.8"}], "}"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"labelOffset2", "=", 
         RowBox[{"{", 
          RowBox[{"1.0", ",", "0.8"}], "}"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Upravi\[THacek]", " ", "offset", " ", "pod\:013ea", " ", "polohy", 
          " ", "bodov"}], "*)"}], 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"originalPoint", "[", 
            RowBox[{"[", "1", "]"}], "]"}], ">", "8"}], ",", 
          RowBox[{
           RowBox[{"labelOffset1", "[", 
            RowBox[{"[", "1", "]"}], "]"}], "=", 
           RowBox[{"-", "1.5"}]}]}], "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"originalPoint", "[", 
            RowBox[{"[", "2", "]"}], "]"}], ">", "8"}], ",", 
          RowBox[{
           RowBox[{"labelOffset1", "[", 
            RowBox[{"[", "2", "]"}], "]"}], "=", 
           RowBox[{"-", "1.0"}]}]}], "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"finalPoint", "[", 
            RowBox[{"[", "1", "]"}], "]"}], ">", "8"}], ",", 
          RowBox[{
           RowBox[{"labelOffset2", "[", 
            RowBox[{"[", "1", "]"}], "]"}], "=", 
           RowBox[{"-", "1.5"}]}]}], "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"finalPoint", "[", 
            RowBox[{"[", "2", "]"}], "]"}], ">", "8"}], ",", 
          RowBox[{
           RowBox[{"labelOffset2", "[", 
            RowBox[{"[", "2", "]"}], "]"}], "=", 
           RowBox[{"-", "1.0"}]}]}], "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Pou\[ZHacek]itie", " ", "tmav\[SHacek]ej", " ", "zelenej", " ", 
          "farby", " ", "pre", " ", "lep\[SHacek]iu", " ", 
          "vidite\:013enos\[THacek]"}], "*)"}], 
        RowBox[{"darkGreen", "=", 
         RowBox[{"RGBColor", "[", 
          RowBox[{"0", ",", "0.6", ",", "0"}], "]"}]}], ";", 
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
                    RowBox[{"[", "2", "]"}], "]"}], ",", "y"}], "}"}]}], 
                "}"}], "]"}], ",", 
              RowBox[{"{", 
               RowBox[{"y", ",", 
                RowBox[{"Ceiling", "[", 
                 RowBox[{"yRange", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", 
                RowBox[{"Floor", "[", 
                 RowBox[{"yRange", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "]"}], ",", "2"}], "}"}]}], 
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
                  RowBox[{"[", "2", "]"}], "]"}], "]"}], ",", "2"}], "}"}]}], 
             "]"}], ",", 
            RowBox[{"(*", 
             RowBox[{
              RowBox[{"Vektor", " ", "posunu"}], "-", 
              RowBox[{
              "tmav\[SHacek]ia", " ", "zelen\[AAcute]", " ", 
               "\[SHacek]\[IAcute]pka"}]}], "*)"}], "darkGreen", ",", "Thick",
             ",", 
            RowBox[{"Arrowheads", "[", "0.03", "]"}], ",", 
            RowBox[{"Arrow", "[", 
             RowBox[{"{", 
              RowBox[{"originalPoint", ",", "finalPoint"}], "}"}], "]"}], ",", 
            RowBox[{"(*", 
             RowBox[{
              RowBox[{"Pomocn\[EAcute]", " ", "\[CHacek]iary"}], "-", 
              RowBox[{
              "tmav\[SHacek]ie", " ", "zelen\[EAcute]", " ", 
               "preru\[SHacek]ovan\[EAcute]"}]}], "*)"}], "darkGreen", ",", 
            "Dashed", ",", 
            RowBox[{"Line", "[", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"originalPoint", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], ",", 
                 RowBox[{"originalPoint", "[", 
                  RowBox[{"[", "2", "]"}], "]"}]}], "}"}], ",", 
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"finalPoint", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], ",", 
                 RowBox[{"originalPoint", "[", 
                  RowBox[{"[", "2", "]"}], "]"}]}], "}"}]}], "}"}], "]"}], 
            ",", 
            RowBox[{"Line", "[", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"finalPoint", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], ",", 
                 RowBox[{"originalPoint", "[", 
                  RowBox[{"[", "2", "]"}], "]"}]}], "}"}], ",", 
               "finalPoint"}], "}"}], "]"}], ",", 
            RowBox[{"(*", 
             RowBox[{
             "Biely", " ", "podklad", " ", "pod", " ", "ozna\[CHacek]eniami", 
              " ", "dx", " ", "a", " ", "dy", " ", "s", " ", "bielym", " ", 
              "pozad\[IAcute]m"}], "*)"}], "White", ",", 
            RowBox[{"Opacity", "[", "0.8", "]"}], ",", 
            RowBox[{"Disk", "[", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{
                RowBox[{"Mean", "[", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"originalPoint", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], ",", 
                   RowBox[{"finalPoint", "[", 
                    RowBox[{"[", "1", "]"}], "]"}]}], "}"}], "]"}], ",", 
                RowBox[{
                 RowBox[{"originalPoint", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "-", "0.5"}]}], "}"}], ",", 
              "0.7"}], "]"}], ",", 
            RowBox[{"Disk", "[", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{
                RowBox[{
                 RowBox[{"finalPoint", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], "-", "1.0"}], ",", 
                RowBox[{"Mean", "[", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"originalPoint", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], ",", 
                   RowBox[{"finalPoint", "[", 
                    RowBox[{"[", "2", "]"}], "]"}]}], "}"}], "]"}]}], "}"}], 
              ",", "0.7"}], "]"}], ",", 
            RowBox[{"(*", 
             RowBox[{
             "Vyraznejsie", " ", "body", " ", "s", " ", "bielym", " ", 
              "pozadim", " ", "a", " ", "vacsim", " ", "polomerom"}], "*)"}], 
            "White", ",", 
            RowBox[{"Opacity", "[", "1.0", "]"}], ",", 
            RowBox[{"Disk", "[", 
             RowBox[{"originalPoint", ",", "0.5"}], "]"}], ",", 
            RowBox[{"Disk", "[", 
             RowBox[{"finalPoint", ",", "0.5"}], "]"}], ",", 
            RowBox[{"(*", 
             RowBox[{
             "Biely", " ", "podklad", " ", "pod", " ", 
              "p\[IAcute]smen\[AAcute]", " ", "P", " ", "a", " ", 
              RowBox[{"P", "'"}]}], "*)"}], 
            RowBox[{"Disk", "[", 
             RowBox[{
              RowBox[{"originalPoint", "+", "labelOffset1"}], ",", "0.5"}], 
             "]"}], ",", 
            RowBox[{"Disk", "[", 
             RowBox[{
              RowBox[{"finalPoint", "+", "labelOffset2"}], ",", "0.5"}], 
             "]"}], ",", 
            RowBox[{"(*", 
             RowBox[{"Body", "-", 
              RowBox[{
              "v\[ADoubleDot]\[CHacek]\[SHacek]ie", " ", "a", " ", 
               "v\[YAcute]raznej\[SHacek]ie"}]}], "*)"}], "Blue", ",", 
            RowBox[{"PointSize", "[", "0.03", "]"}], ",", 
            RowBox[{"Point", "[", "originalPoint", "]"}], ",", "Red", ",", 
            RowBox[{"PointSize", "[", "0.03", "]"}], ",", 
            RowBox[{"Point", "[", "finalPoint", "]"}], ",", 
            RowBox[{"(*", 
             RowBox[{
             "Labels", " ", "s", " ", "optimalizovan\[YAcute]mi", " ", 
              "poz\[IAcute]ciami"}], "*)"}], 
            RowBox[{"Text", "[", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"\"\<P\>\"", ",", "Blue", ",", "Bold", ",", "18"}], 
               "]"}], ",", 
              RowBox[{"originalPoint", "+", "labelOffset1"}]}], "]"}], ",", 
            RowBox[{"Text", "[", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"\"\<P'\>\"", ",", "Red", ",", "Bold", ",", "18"}], 
               "]"}], ",", 
              RowBox[{"finalPoint", "+", "labelOffset2"}]}], "]"}], ",", 
            RowBox[{"(*", 
             RowBox[{"Ozna\[CHacek]enie", " ", "dx", " ", "a", " ", "dy"}], 
             "*)"}], 
            RowBox[{"Text", "[", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"\"\<dx = \>\"", "<>", 
                 RowBox[{"ToString", "[", "dx", "]"}]}], ",", "darkGreen", 
                ",", "Bold"}], "]"}], ",", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{"Mean", "[", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"originalPoint", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], ",", 
                   RowBox[{"finalPoint", "[", 
                    RowBox[{"[", "1", "]"}], "]"}]}], "}"}], "]"}], ",", 
                RowBox[{
                 RowBox[{"originalPoint", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "-", "0.5"}]}], "}"}]}], 
             "]"}], ",", 
            RowBox[{"Text", "[", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"\"\<dy = \>\"", "<>", 
                 RowBox[{"ToString", "[", "dy", "]"}]}], ",", "darkGreen", 
                ",", "Bold"}], "]"}], ",", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{
                 RowBox[{"finalPoint", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], "-", "1.0"}], ",", 
                RowBox[{"Mean", "[", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"originalPoint", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], ",", 
                   RowBox[{"finalPoint", "[", 
                    RowBox[{"[", "2", "]"}], "]"}]}], "}"}], "]"}]}], "}"}]}],
              "]"}], ",", 
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
                  RowBox[{"[", "2", "]"}], "]"}]}], "}"}]}], "}"}], "]"}], 
            ",", 
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
              RowBox[{
              "\[CapitalCHacek]\[IAcute]sla", " ", "na", " ", "osiach"}], "-", 
              RowBox[{"PRESN\[CapitalEAcute]", " ", "ZOBRAZENIE"}]}], "*)"}], 
            
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
                  RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "}"}]}], "]"}], 
            ",", 
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
   "\[IndentingNewLine]", "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"BodPosun", "[", "inputPoint_", "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
        "normalizedPoint", ",", "translationParams", ",", "outputPoint", ",", 
         "dx", ",", "dy", ",", "invDx", ",", "invDy"}], "}"}], ",", 
       RowBox[{"(*", 
        RowBox[{
         RowBox[{
         "Normaliz\[AAcute]cia", " ", "vstupn\[EAcute]ho", " ", "bodu"}], "-", 
         RowBox[{
         "ZACHOV\[CapitalAAcute]VA", " ", "PRESN\[CapitalEAcute]", " ", 
          "HODNOTY"}]}], "*)"}], 
       RowBox[{
        RowBox[{"normalizedPoint", "=", 
         RowBox[{"NormalizePoint", "[", "inputPoint", "]"}]}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{"Z\[IAcute]skanie", " ", "parametrov", " ", "posunu"}], 
         "*)"}], 
        RowBox[{"translationParams", "=", 
         RowBox[{"GetTranslationParameters", "[", "normalizedPoint", "]"}]}], 
        ";", "\[IndentingNewLine]", 
        RowBox[{"dx", "=", 
         RowBox[{"translationParams", "[", 
          RowBox[{"[", "1", "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"dy", "=", 
         RowBox[{"translationParams", "[", 
          RowBox[{"[", "2", "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"outputPoint", "=", 
         RowBox[{"translationParams", "[", 
          RowBox[{"[", "4", "]"}], "]"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{"Inverzn\[EAcute]", " ", "parametre"}], "*)"}], 
        RowBox[{"invDx", "=", 
         RowBox[{"-", "dx"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"invDy", "=", 
         RowBox[{"-", "dy"}]}], ";", "\[IndentingNewLine]", 
        RowBox[{"(*", "Nadpis", "*)"}], 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE BODU - \
POSUN\>\"", ",", "Bold", ",", "16"}], "]"}], "]"}], ";", 
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
         "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{
        "Print", "[", "\"\<Majme bod P so s\[UAcute]radnicami:\>\"", "]"}], 
        ";", "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{"MatrixForm", "[", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{"{", 
             RowBox[{"normalizedPoint", "[", 
              RowBox[{"[", "1", "]"}], "]"}], "}"}], ",", 
            RowBox[{"{", 
             RowBox[{"normalizedPoint", "[", 
              RowBox[{"[", "2", "]"}], "]"}], "}"}]}], "}"}], "]"}], "]"}], 
        ";", "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{
         "\"\<\\nVykonajte posun bodu v 2D priestore pomocou vektora posunu: \
\[CapitalDelta] = [\>\"", ",", 
          RowBox[{"Style", "[", 
           RowBox[{"dx", ",", "Red"}], "]"}], ",", "\"\<, \>\"", ",", 
          RowBox[{"Style", "[", 
           RowBox[{"dy", ",", "Red"}], "]"}], ",", "\"\<]\>\""}], "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{
        "Print", "[", 
         "\"\<\\nTransforma\[CHacek]n\[AAcute] matica posunu v \
homog\[EAcute]nnych s\[UAcute]radniciach:\>\"", "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{"MatrixForm", "[", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"1", ",", "Red"}], "]"}], ",", 
              RowBox[{"Style", "[", 
               RowBox[{"0", ",", "Red"}], "]"}], ",", 
              RowBox[{"Style", "[", 
               RowBox[{"dx", ",", "Red"}], "]"}]}], "}"}], ",", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"0", ",", "Red"}], "]"}], ",", 
              RowBox[{"Style", "[", 
               RowBox[{"1", ",", "Red"}], "]"}], ",", 
              RowBox[{"Style", "[", 
               RowBox[{"dy", ",", "Red"}], "]"}]}], "}"}], ",", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"0", ",", "Red"}], "]"}], ",", 
              RowBox[{"Style", "[", 
               RowBox[{"0", ",", "Red"}], "]"}], ",", 
              RowBox[{"Style", "[", 
               RowBox[{"1", ",", "Red"}], "]"}]}], "}"}]}], "}"}], "]"}], 
         "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"(*", "POSTUP", "*)"}], 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{"\"\<\\nPOSTUP:\>\"", ",", "Bold", ",", "14"}], "]"}], 
         "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{
        "Print", "[", 
         "\"\<Pre v\[YAcute]po\[CHacek]et pou\[ZHacek]ijeme maticu posunu v \
homog\[EAcute]nnych s\[UAcute]radniciach:\>\"", "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<Transforma\[CHacek]n\[AAcute] matica:\>\"", ",", "Bold"}], 
          "]"}], "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{"MatrixForm", "[", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"1", ",", "Red"}], "]"}], ",", 
              RowBox[{"Style", "[", 
               RowBox[{"0", ",", "Red"}], "]"}], ",", 
              RowBox[{"Style", "[", 
               RowBox[{"\"\<dx\>\"", ",", "Red"}], "]"}]}], "}"}], ",", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"0", ",", "Red"}], "]"}], ",", 
              RowBox[{"Style", "[", 
               RowBox[{"1", ",", "Red"}], "]"}], ",", 
              RowBox[{"Style", "[", 
               RowBox[{"\"\<dy\>\"", ",", "Red"}], "]"}]}], "}"}], ",", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"0", ",", "Red"}], "]"}], ",", 
              RowBox[{"Style", "[", 
               RowBox[{"0", ",", "Red"}], "]"}], ",", 
              RowBox[{"Style", "[", 
               RowBox[{"1", ",", "Red"}], "]"}]}], "}"}]}], "}"}], "]"}], 
         "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"Print", "[", "\"\<\\nPre na\[SHacek]e hodnoty:\>\"", "]"}], 
        ";", "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<dx = \>\"", ",", "dx"}], "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<dy = \>\"", ",", "dy"}], "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{
        "Print", "[", "\"\<\\nPosun vykon\[AAcute]me pomocou vzorcov:\>\"", 
         "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"Print", "[", "\"\<x' = x + dx\>\"", "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"Print", "[", "\"\<y' = y + dy\>\"", "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{
        "Print", "[", "\"\<\\nV\[YAcute]po\[CHacek]et pre bod P:\>\"", "]"}], 
        ";", "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
          RowBox[{"V\[YAcute]po\[CHacek]et", " ", "pre", " ", "bod"}], "-", 
          RowBox[{"PRESN\[CapitalEAcute]", " ", "HODNOTY"}]}], "*)"}], 
        RowBox[{"Module", "[", 
         RowBox[{
          RowBox[{"{", 
           RowBox[{
            RowBox[{"x", "=", 
             RowBox[{"normalizedPoint", "[", 
              RowBox[{"[", "1", "]"}], "]"}]}], ",", 
            RowBox[{"y", "=", 
             RowBox[{"normalizedPoint", "[", 
              RowBox[{"[", "2", "]"}], "]"}]}]}], "}"}], ",", 
          RowBox[{
           RowBox[{"Print", "[", 
            RowBox[{
             RowBox[{"Style", "[", 
              RowBox[{
              "\"\<P\[OHat]vodn\[EAcute] s\[UAcute]radnice:\>\"", ",", 
               "Bold"}], "]"}], ",", "\"\< [\>\"", ",", "x", ",", 
             "\"\<, \>\"", ",", "y", ",", "\"\<]\>\""}], "]"}], ";", 
           "\[IndentingNewLine]", 
           RowBox[{"(*", 
            RowBox[{
             RowBox[{
             "Detailn\[YAcute]", " ", "postup", " ", "pre", " ", "x"}], "-", 
             RowBox[{"ov\[UAcute]", " ", "s\[UAcute]radnicu"}]}], "*)"}], 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
             "\"\<V\[YAcute]po\[CHacek]et x-ovej s\[UAcute]radnice:\>\"", ",",
               "Bold"}], "]"}], "]"}], ";", "\[IndentingNewLine]", 
           RowBox[{"Print", "[", 
            RowBox[{
            "\"\<x' = x + dx = \>\"", ",", "x", ",", "\"\< + \>\"", ",", 
             "dx"}], "]"}], ";", "\[IndentingNewLine]", 
           RowBox[{"Print", "[", 
            RowBox[{"\"\<x' = \>\"", ",", 
             RowBox[{"outputPoint", "[", 
              RowBox[{"[", "1", "]"}], "]"}]}], "]"}], ";", 
           "\[IndentingNewLine]", 
           RowBox[{"(*", 
            RowBox[{
             RowBox[{
             "Detailn\[YAcute]", " ", "postup", " ", "pre", " ", "y"}], "-", 
             RowBox[{"ov\[UAcute]", " ", "s\[UAcute]radnicu"}]}], "*)"}], 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
             "\"\<V\[YAcute]po\[CHacek]et y-ovej s\[UAcute]radnice:\>\"", ",",
               "Bold"}], "]"}], "]"}], ";", "\[IndentingNewLine]", 
           RowBox[{"Print", "[", 
            RowBox[{
            "\"\<y' = y + dy = \>\"", ",", "y", ",", "\"\< + \>\"", ",", 
             "dy"}], "]"}], ";", "\[IndentingNewLine]", 
           RowBox[{"Print", "[", 
            RowBox[{"\"\<y' = \>\"", ",", 
             RowBox[{"outputPoint", "[", 
              RowBox[{"[", "2", "]"}], "]"}]}], "]"}], ";", 
           "\[IndentingNewLine]", 
           RowBox[{"(*", 
            RowBox[{
            "Overenie", " ", "inverznej", " ", "transform\[AAcute]cie"}], 
            "*)"}], 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
             "\"\<\\nOverenie pomocou inverznej transform\[AAcute]cie:\>\"", 
              ",", "Bold"}], "]"}], "]"}], ";", "\[IndentingNewLine]", 
           RowBox[{"Print", "[", 
            RowBox[{"\"\<x = x' + (-dx) = \>\"", ",", 
             RowBox[{"outputPoint", "[", 
              RowBox[{"[", "1", "]"}], "]"}], ",", "\"\< + (\>\"", ",", 
             "invDx", ",", "\"\<) = \>\"", ",", "x"}], "]"}], ";", 
           "\[IndentingNewLine]", 
           RowBox[{"Print", "[", 
            RowBox[{"\"\<y = y' + (-dy) = \>\"", ",", 
             RowBox[{"outputPoint", "[", 
              RowBox[{"[", "2", "]"}], "]"}], ",", "\"\< + (\>\"", ",", 
             "invDy", ",", "\"\<) = \>\"", ",", "y"}], "]"}], ";", 
           "\[IndentingNewLine]", 
           RowBox[{"(*", 
            RowBox[{"Maticov\[YAcute]", " ", "z\[AAcute]pis"}], "*)"}], 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
             "\"\<\\nMaticov\[YAcute] z\[AAcute]pis:\>\"", ",", "Bold"}], 
             "]"}], "]"}], ";", "\[IndentingNewLine]", 
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
                    RowBox[{"0", ",", "Red"}], "]"}], ",", 
                    RowBox[{"Style", "[", 
                    RowBox[{"dx", ",", "Red"}], "]"}]}], "}"}], ",", 
                  RowBox[{"{", 
                   RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{"0", ",", "Red"}], "]"}], ",", 
                    RowBox[{"Style", "[", 
                    RowBox[{"1", ",", "Red"}], "]"}], ",", 
                    RowBox[{"Style", "[", 
                    RowBox[{"dy", ",", "Red"}], "]"}]}], "}"}], ",", 
                  RowBox[{"{", 
                   RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{"0", ",", "Red"}], "]"}], ",", 
                    RowBox[{"Style", "[", 
                    RowBox[{"0", ",", "Red"}], "]"}], ",", 
                    RowBox[{"Style", "[", 
                    RowBox[{"1", ",", "Red"}], "]"}]}], "}"}]}], "}"}], "]"}],
                ",", "\"\< \[CenterDot] \>\"", ",", 
               RowBox[{"MatrixForm", "[", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"{", "x", "}"}], ",", 
                  RowBox[{"{", "y", "}"}], ",", 
                  RowBox[{"{", "1", "}"}]}], "}"}], "]"}], ",", "\"\< = \>\"",
                ",", 
               RowBox[{"MatrixForm", "[", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"{", 
                   RowBox[{"outputPoint", "[", 
                    RowBox[{"[", "1", "]"}], "]"}], "}"}], ",", 
                  RowBox[{"{", 
                   RowBox[{"outputPoint", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], "}"}], ",", 
                  RowBox[{"{", "1", "}"}]}], "}"}], "]"}]}], "}"}], "]"}], 
            "]"}], ";"}]}], "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"(*", "V\[CapitalYAcute]SLEDOK", "*)"}], 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nV\[CapitalYAcute]SLEDOK:\>\"", ",", "Bold", ",", "14"}], 
          "]"}], "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{
        "Print", "[", "\"\<S\[UAcute]radnice bodu po posune:\>\"", "]"}], ";",
         "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
           RowBox[{"MatrixForm", "[", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{"outputPoint", "[", 
                RowBox[{"[", "1", "]"}], "]"}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{"outputPoint", "[", 
                RowBox[{"[", "2", "]"}], "]"}], "}"}]}], "}"}], "]"}], ",", 
           "Red"}], "]"}], "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"(*", "VIZUALIZ\[CapitalAAcute]CIA", "*)"}], 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{"\"\<\\nVIZUALIZ\[CapitalAAcute]CIA:\>\"", ",", "Bold"}], 
          "]"}], "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{"CreateVisualization", "[", 
          RowBox[{"normalizedPoint", ",", "outputPoint", ",", 
           RowBox[{"{", 
            RowBox[{"dx", ",", "dy"}], "}"}]}], "]"}], "]"}], ";", 
        "\[IndentingNewLine]", 
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
         "\"\<\[Bullet] \[CapitalCHacek]erven\[YAcute] bod: Posunut\[YAcute] \
bod P'\>\"", "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{
        "Print", "[", 
         "\"\<\[Bullet] Zelen\[AAcute] \[SHacek]\[IAcute]pka: Vektor posunu\>\
\"", "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{
        "Print", "[", 
         "\"\<\[Bullet] Zelen\[EAcute] preru\[SHacek]ovan\[EAcute] \
\[CHacek]iary: Posun po s\[UAcute]radniciach\>\"", "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{
        "Print", "[", 
         "\"\<\[Bullet] Mrie\[ZHacek]ka: S\[UAcute]radnicov\[YAcute] syst\
\[EAcute]m\>\"", "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nP\[OHat]vodn\[YAcute] bod (modr\[AAcute]):\>\"", ",", 
           "Bold"}], "]"}], "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{"Row", "[", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{"Style", "[", 
             RowBox[{"\"\<Bod P: \>\"", ",", 
              RowBox[{"RGBColor", "[", 
               RowBox[{"0.1", ",", "0.1", ",", "1"}], "]"}]}], "]"}], ",", 
            "normalizedPoint"}], "}"}], "]"}], "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nV\[YAcute]sledn\[YAcute] bod \
(\[CHacek]erven\[AAcute]):\>\"", ",", "Bold"}], "]"}], "]"}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{"Row", "[", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{"Style", "[", 
             RowBox[{"\"\<Bod P': \>\"", ",", 
              RowBox[{"RGBColor", "[", 
               RowBox[{"1", ",", "0.1", ",", "0.1"}], "]"}]}], "]"}], ",", 
            "outputPoint"}], "}"}], "]"}], "]"}], ";", "\[IndentingNewLine]", 
        
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nInverzn\[AAcute] transform\[AAcute]cia:\>\"", ",", "Bold"}],
           "]"}], "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{
        "Print", "[", 
         "\"\<Pre vr\[AAcute]tenie do p\[OHat]vodn\[EAcute]ho stavu pou\
\[ZHacek]ite vektor posunu:\>\"", "]"}], ";", "\[IndentingNewLine]", 
        RowBox[{"Print", "[", 
         RowBox[{
         "\"\<\[CapitalDelta]' = [\>\"", ",", "invDx", ",", "\"\<, \>\"", ",",
           "invDy", ",", "\"\<] (opa\[CHacek]n\[YAcute] vektor)\>\""}], "]"}],
         ";", "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
          RowBox[{
          "Vr\[AAcute]tenie", " ", "nov\[YAcute]ch", " ", 
           "s\[UAcute]radn\[IAcute]c", " ", "pre", " ", 
           "pr\[IAcute]padn\[EAcute]", " ", "\[DHacek]al\[SHacek]ie", " ", 
           "transform\[AAcute]cie"}], "-", 
          RowBox[{"PRESN\[CapitalEAcute]", " ", "HODNOTY"}]}], "*)"}], 
        "outputPoint"}]}], "]"}]}], ";"}], "\[IndentingNewLine]", 
   "\[IndentingNewLine]", "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{
    "Defin\[IAcute]cia", " ", "chybov\[YAcute]ch", " ", "spr\[AAcute]v"}], 
    "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"BodPosun", "::", "invalidInput"}], "=", 
     "\"\<Neplatn\[YAcute] vstup. O\[CHacek]ak\[AAcute]va sa bod so \
s\[UAcute]radnicami [x, y].\>\""}], ";"}], "\[IndentingNewLine]", "\n", 
   RowBox[{
    RowBox[{"End", "[", "]"}], ";"}], "\n", 
   RowBox[{
    RowBox[{"EndPackage", "[", "]"}], ";"}]}]}]], "Input",
 CellChangeTimes->{{3.952792993374786*^9, 3.9527929933772173`*^9}, {
   3.952793192486888*^9, 3.952793193889473*^9}, {3.952793504347988*^9, 
   3.952793506450665*^9}, 3.952793654999399*^9, 3.952793844807601*^9, {
   3.9527940129666357`*^9, 3.952794079061771*^9}, {3.952794119304493*^9, 
   3.952794120655641*^9}, 3.952794445326734*^9},
 CellLabel->
  "In[1032]:=",ExpressionUUID->"ce9b335b-365b-44ae-9e82-eef667b9c6c3"],

Cell[CellGroupData[{

Cell[BoxData["\"\<BodHardBalik`Transforms`Posun`\>\""], "Input",
 NumberMarks->False,
 CellLabel->
  "In[865]:=",ExpressionUUID->"bf8eba9a-d91a-4084-aacd-bc1af1141e49"],

Cell[BoxData["\<\"BodHardBalik`Transforms`Posun`\"\>"], "Output",
 CellChangeTimes->{3.952793208503615*^9},
 CellLabel->
  "Out[865]=",ExpressionUUID->"091d9c8d-370c-4860-8e70-da34108f1adf"]
}, Open  ]]
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
Cell[554, 20, 142, 3, 33, "Text",ExpressionUUID->"5c8a06ec-6e7d-40b0-b49f-cfd4a27be3c4"],
Cell[699, 25, 51371, 1221, 4084, "Input",ExpressionUUID->"ce9b335b-365b-44ae-9e82-eef667b9c6c3"],
Cell[CellGroupData[{
Cell[52095, 1250, 168, 3, 29, "Input",ExpressionUUID->"bf8eba9a-d91a-4084-aacd-bc1af1141e49"],
Cell[52266, 1255, 190, 3, 33, "Output",ExpressionUUID->"091d9c8d-370c-4860-8e70-da34108f1adf"]
}, Open  ]]
}
]
*)

