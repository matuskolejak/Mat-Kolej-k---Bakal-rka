(* Content-type: application/vnd.wolfram.mathematica *)

(*** Wolfram Notebook File ***)
(* http://www.wolfram.com/nb *)

(* CreatedBy='Wolfram 14.1' *)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[       154,          7]
NotebookDataLength[     73840,       1647]
NotebookOptionsPosition[     73394,       1632]
NotebookOutlinePosition[     73785,       1648]
CellTagsIndexPosition[     73742,       1645]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{
Cell[BoxData[
 RowBox[{"\n", "\n", 
  RowBox[{
   RowBox[{
    RowBox[{"BeginPackage", "[", 
     RowBox[{"\"\<BodHardBalik`Transforms`Posun`\>\"", ",", " ", 
      RowBox[{"{", "\"\<BodHardBalik`\>\"", "}"}]}], "]"}], ";"}], "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodPosunVector", "::", "usage"}], " ", "=", " ", 
     "\"\<BodPosunVector je globalny vektor posunu [dx, dy], ktory sa pouziva \
v BodPosun funkcii.\>\""}], ";"}], "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodPosun", "::", "usage"}], " ", "=", " ", "\n", "    ", 
     "\"\<BodPosun[point_] zobraz\[IAcute] interakt\[IAcute]vny tutori\
\[AAcute]l pre posun bodu a vr\[AAcute]ti nov\[EAcute] \
s\[UAcute]radnice.\>\""}], ";"}], "\n", "\n", 
   RowBox[{
    RowBox[{"Begin", "[", "\"\<`Private`\>\"", "]"}], ";"}], "\n", "\n", 
   RowBox[{
    RowBox[{"BodPosunVector", " ", "=", " ", 
     RowBox[{"{", 
      RowBox[{"0", ",", " ", "0"}], "}"}]}], ";"}], "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"GetTranslationParameters", "[", "point_", "]"}], " ", ":=", " ",
      "\n", "    ", 
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
      "]"}]}], ";"}], "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"CreateVisualization", "[", 
      RowBox[{
      "originalPoint_", ",", " ", "finalPoint_", ",", " ", 
       "translationVector_"}], "]"}], " ", ":=", " ", "\n", "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
        "allPoints", ",", " ", "xMin", ",", " ", "xMax", ",", " ", "yMin", 
         ",", " ", "yMax", ",", " ", "rangeBuffer", ",", " ", "xRange", ",", 
         " ", "yRange", ",", " ", "\n", "            ", "labelOffsets", ",", 
         " ", 
         RowBox[{"dx", " ", "=", " ", 
          RowBox[{"translationVector", "[", 
           RowBox[{"[", "1", "]"}], "]"}]}], ",", " ", 
         RowBox[{"dy", " ", "=", " ", 
          RowBox[{"translationVector", "[", 
           RowBox[{"[", "2", "]"}], "]"}]}]}], "}"}], ",", "\n", "        ", 
       "\n", "        ", 
       RowBox[{
        RowBox[{"allPoints", " ", "=", " ", 
         RowBox[{"{", 
          RowBox[{"originalPoint", ",", " ", "finalPoint"}], "}"}]}], ";", 
        "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"xMin", " ", "=", " ", 
         RowBox[{"Min", "[", 
          RowBox[{"allPoints", "[", 
           RowBox[{"[", 
            RowBox[{"All", ",", " ", "1"}], "]"}], "]"}], "]"}]}], ";", "\n", 
        "        ", 
        RowBox[{"xMax", " ", "=", " ", 
         RowBox[{"Max", "[", 
          RowBox[{"allPoints", "[", 
           RowBox[{"[", 
            RowBox[{"All", ",", " ", "1"}], "]"}], "]"}], "]"}]}], ";", "\n", 
        "        ", 
        RowBox[{"yMin", " ", "=", " ", 
         RowBox[{"Min", "[", 
          RowBox[{"allPoints", "[", 
           RowBox[{"[", 
            RowBox[{"All", ",", " ", "2"}], "]"}], "]"}], "]"}]}], ";", "\n", 
        "        ", 
        RowBox[{"yMax", " ", "=", " ", 
         RowBox[{"Max", "[", 
          RowBox[{"allPoints", "[", 
           RowBox[{"[", 
            RowBox[{"All", ",", " ", "2"}], "]"}], "]"}], "]"}]}], ";", "\n", 
        "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"rangeBuffer", " ", "=", " ", "2"}], ";", "\n", "        ", 
        "\n", "        ", "\n", "        ", 
        RowBox[{"xRange", " ", "=", " ", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"Min", "[", 
            RowBox[{
             RowBox[{"-", "11"}], ",", " ", 
             RowBox[{"xMin", " ", "-", " ", "rangeBuffer"}]}], "]"}], ",", 
           " ", 
           RowBox[{"Max", "[", 
            RowBox[{"11", ",", " ", 
             RowBox[{"xMax", " ", "+", " ", "rangeBuffer"}]}], "]"}]}], 
          "}"}]}], ";", "\n", "        ", 
        RowBox[{"yRange", " ", "=", " ", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"Min", "[", 
            RowBox[{
             RowBox[{"-", "11"}], ",", " ", 
             RowBox[{"yMin", " ", "-", " ", "rangeBuffer"}]}], "]"}], ",", 
           " ", 
           RowBox[{"Max", "[", 
            RowBox[{"11", ",", " ", 
             RowBox[{"yMax", " ", "+", " ", "rangeBuffer"}]}], "]"}]}], 
          "}"}]}], ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        
        RowBox[{"labelOffsets", " ", "=", " ", 
         RowBox[{"{", 
          RowBox[{"0.7", ",", " ", "0.7"}], "}"}]}], ";", "\n", "        ", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"originalPoint", "[", 
            RowBox[{"[", "1", "]"}], "]"}], " ", ">", " ", "8"}], ",", " ", 
          RowBox[{
           RowBox[{"labelOffsets", "[", 
            RowBox[{"[", "1", "]"}], "]"}], " ", "=", " ", 
           RowBox[{"-", "1.0"}]}]}], "]"}], ";", "\n", "        ", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"originalPoint", "[", 
            RowBox[{"[", "2", "]"}], "]"}], " ", ">", " ", "8"}], ",", " ", 
          RowBox[{
           RowBox[{"labelOffsets", "[", 
            RowBox[{"[", "2", "]"}], "]"}], " ", "=", " ", 
           RowBox[{"-", "1.0"}]}]}], "]"}], ";", "\n", "        ", "\n", 
        "        ", 
        RowBox[{"Graphics", "[", 
         RowBox[{
          RowBox[{"{", "\n", "            ", "\n", "            ", 
           RowBox[{"LightGray", ",", " ", "Thin", ",", "\n", "            ", 
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
                "}"}], "]"}], ",", " ", "\n", "                ", 
              RowBox[{"{", 
               RowBox[{"y", ",", " ", 
                RowBox[{"Ceiling", "[", 
                 RowBox[{"yRange", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", " ", 
                RowBox[{"Floor", "[", 
                 RowBox[{"yRange", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "]"}], ",", " ", "2"}], 
               "}"}]}], "]"}], ",", "\n", "            ", 
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
              ",", " ", "\n", "                ", 
              RowBox[{"{", 
               RowBox[{"x", ",", " ", 
                RowBox[{"Ceiling", "[", 
                 RowBox[{"xRange", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", " ", 
                RowBox[{"Floor", "[", 
                 RowBox[{"xRange", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "]"}], ",", " ", "2"}], 
               "}"}]}], "]"}], ",", "\n", "            ", "\n", 
            "            ", "\n", "            ", "Blue", ",", " ", "Thick", 
            ",", " ", 
            RowBox[{"Opacity", "[", "0.7", "]"}], ",", "\n", "            ", 
            RowBox[{"Point", "[", "originalPoint", "]"}], ",", "\n", 
            "            ", "\n", "            ", "\n", "            ", "Red",
             ",", " ", "Thick", ",", " ", 
            RowBox[{"Opacity", "[", "0.7", "]"}], ",", "\n", "            ", 
            RowBox[{"Point", "[", "finalPoint", "]"}], ",", "\n", 
            "            ", "\n", "            ", "\n", "            ", 
            "Green", ",", " ", "Dashed", ",", "\n", "            ", 
            RowBox[{"Line", "[", 
             RowBox[{"{", 
              RowBox[{"originalPoint", ",", " ", "finalPoint"}], "}"}], "]"}],
             ",", "\n", "            ", "\n", "            ", "\n", 
            "            ", "White", ",", " ", "\n", "            ", 
            RowBox[{"Disk", "[", 
             RowBox[{"originalPoint", ",", " ", "0.45"}], "]"}], ",", "\n", 
            "            ", 
            RowBox[{"Disk", "[", 
             RowBox[{"finalPoint", ",", " ", "0.45"}], "]"}], ",", "\n", 
            "            ", "\n", "            ", "\n", "            ", 
            "Blue", ",", " ", 
            RowBox[{"PointSize", "[", "0.025", "]"}], ",", " ", 
            RowBox[{"Point", "[", "originalPoint", "]"}], ",", "\n", 
            "            ", "Red", ",", " ", 
            RowBox[{"PointSize", "[", "0.025", "]"}], ",", " ", 
            RowBox[{"Point", "[", "finalPoint", "]"}], ",", "\n", 
            "            ", "\n", "            ", "\n", "            ", 
            RowBox[{"Text", "[", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{
               "\"\<P\>\"", ",", " ", "Blue", ",", " ", "Bold", ",", " ", 
                "16"}], "]"}], ",", " ", "\n", "                ", 
              RowBox[{"originalPoint", " ", "+", " ", "labelOffsets"}]}], 
             "]"}], ",", "\n", "            ", 
            RowBox[{"Text", "[", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{
               "\"\<P'\>\"", ",", " ", "Red", ",", " ", "Bold", ",", " ", 
                "16"}], "]"}], ",", " ", "\n", "                ", 
              RowBox[{"finalPoint", " ", "+", " ", "labelOffsets"}]}], "]"}], 
            ",", "\n", "            ", "\n", "            ", "\n", 
            "            ", "Black", ",", " ", "Thick", ",", "\n", 
            "            ", 
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
              "}"}], "]"}], ",", "\n", "            ", 
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
                  RowBox[{"[", "2", "]"}], "]"}]}], "}"}]}], "}"}], "]"}], 
            ",", "\n", "            ", 
            RowBox[{"Text", "[", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"\"\<x\>\"", ",", " ", "Black", ",", " ", "Bold"}], 
               "]"}], ",", " ", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{
                 RowBox[{"xRange", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], " ", "-", " ", "0.5"}], ",",
                 " ", 
                RowBox[{"-", "0.5"}]}], "}"}]}], "]"}], ",", "\n", 
            "            ", 
            RowBox[{"Text", "[", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"\"\<y\>\"", ",", " ", "Black", ",", " ", "Bold"}], 
               "]"}], ",", " ", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{"-", "0.5"}], ",", " ", 
                RowBox[{
                 RowBox[{"yRange", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], " ", "-", " ", "0.5"}]}], 
               "}"}]}], "]"}], ",", "\n", "            ", "\n", 
            "            ", "\n", "            ", 
            RowBox[{"Table", "[", 
             RowBox[{
              RowBox[{"Text", "[", 
               RowBox[{
                RowBox[{"Style", "[", 
                 RowBox[{"i", ",", " ", "Gray", ",", " ", "10"}], "]"}], ",", 
                " ", 
                RowBox[{"{", 
                 RowBox[{"i", ",", " ", 
                  RowBox[{"-", "0.3"}]}], "}"}]}], "]"}], ",", " ", "\n", 
              "                ", 
              RowBox[{"{", 
               RowBox[{"i", ",", " ", 
                RowBox[{"Ceiling", "[", 
                 RowBox[{"xRange", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", " ", 
                RowBox[{"Floor", "[", 
                 RowBox[{"xRange", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "}"}]}], "]"}], 
            ",", "\n", "            ", 
            RowBox[{"Table", "[", 
             RowBox[{
              RowBox[{"Text", "[", 
               RowBox[{
                RowBox[{"Style", "[", 
                 RowBox[{"i", ",", " ", "Gray", ",", " ", "10"}], "]"}], ",", 
                " ", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"-", "0.3"}], ",", " ", "i"}], "}"}]}], "]"}], ",", 
              " ", "\n", "                ", 
              RowBox[{"{", 
               RowBox[{"i", ",", " ", 
                RowBox[{"Ceiling", "[", 
                 RowBox[{"yRange", "[", 
                  RowBox[{"[", "1", "]"}], "]"}], "]"}], ",", " ", 
                RowBox[{"Floor", "[", 
                 RowBox[{"yRange", "[", 
                  RowBox[{"[", "2", "]"}], "]"}], "]"}]}], "}"}]}], "]"}]}], 
           "\n", "        ", "}"}], ",", "\n", "        ", 
          RowBox[{"PlotRange", " ", "->", " ", 
           RowBox[{"{", 
            RowBox[{"xRange", ",", " ", "yRange"}], "}"}]}], ",", "\n", 
          "        ", 
          RowBox[{"AspectRatio", " ", "->", " ", "1"}], ",", "\n", "        ", 
          RowBox[{"ImageSize", " ", "->", " ", "500"}], ",", "\n", "        ", 
          RowBox[{"ImagePadding", " ", "->", " ", "20"}]}], "\n", "        ", 
         "]"}]}]}], "\n", "    ", "]"}]}], ";"}], "\n", "    ", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"DisplayIntuitiveMathMatrixMultiplication", "[", 
      RowBox[{
      "matrix_", ",", " ", "vector_", ",", " ", "result_", ",", " ", 
       "pointName_"}], "]"}], " ", ":=", " ", "\n", "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"darkGreen", " ", "=", " ", 
         RowBox[{"RGBColor", "[", 
          RowBox[{"0", ",", " ", "0.5", ",", " ", "0"}], "]"}]}], "}"}], ",", 
       "\n", "        ", "\n", "        ", 
       RowBox[{
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
           RowBox[{
           "\"\<Maticov\[YAcute] z\[AAcute]pis transform\[AAcute]cie bodu \
\>\"", " ", "<>", " ", "pointName", " ", "<>", " ", "\"\<:\>\""}], ",", " ", 
           "Bold"}], "]"}], "]"}], ";", "\n", "        ", "\n", "        ", 
        "\n", "        ", 
        RowBox[{"Print", "[", "\n", "            ", 
         RowBox[{"Grid", "[", 
          RowBox[{"{", "\n", "                ", 
           RowBox[{"{", "\n", "                    ", 
            RowBox[{
             RowBox[{"MatrixForm", "[", 
              RowBox[{"Map", "[", 
               RowBox[{
                RowBox[{
                 RowBox[{"Style", "[", 
                  RowBox[{"#", ",", " ", "Red"}], "]"}], " ", "&"}], ",", " ",
                 "matrix", ",", " ", 
                RowBox[{"{", "2", "}"}]}], "]"}], "]"}], ",", " ", "\n", 
             "                    ", "\"\< \[CenterDot] \>\"", ",", " ", "\n",
              "                    ", 
             RowBox[{"MatrixForm", "[", 
              RowBox[{"Map", "[", 
               RowBox[{
                RowBox[{
                 RowBox[{"Style", "[", 
                  RowBox[{"#", ",", " ", "Blue"}], "]"}], " ", "&"}], ",", 
                " ", "vector"}], "]"}], "]"}], ",", "\n", 
             "                    ", "\"\< = \>\"", ",", "\n", 
             "                    ", 
             RowBox[{"MatrixForm", "[", 
              RowBox[{"Map", "[", 
               RowBox[{
                RowBox[{
                 RowBox[{"Style", "[", 
                  RowBox[{"#", ",", " ", "darkGreen"}], "]"}], " ", "&"}], 
                ",", " ", "result"}], "]"}], "]"}]}], "\n", 
            "                ", "}"}], "\n", "            ", "}"}], "]"}], 
         "\n", "        ", "]"}], ";", "\n", "        ", "\n", "        ", 
        "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<Podrobn\[YAcute] v\[YAcute]po\[CHacek]et jednotliv\[YAcute]ch \
s\[UAcute]radn\[IAcute]c:\>\"", ",", " ", "Bold"}], "]"}], "]"}], ";", "\n", 
        "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\[Bullet] V\[YAcute]po\[CHacek]et novej x-ovej \
s\[UAcute]radnice (1. riadok matice):\>\"", ",", " ", "Brown"}], "]"}], "]"}],
         ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", "\n", "            ", 
         RowBox[{"Grid", "[", 
          RowBox[{
           RowBox[{"{", "\n", "                ", 
            RowBox[{"{", "\n", "                    ", 
             RowBox[{"\"\<   [\>\"", ",", " ", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"matrix", "[", 
                 RowBox[{"[", 
                  RowBox[{"1", ",", " ", "1"}], "]"}], "]"}], ",", " ", 
                "Red"}], "]"}], ",", " ", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"matrix", "[", 
                 RowBox[{"[", 
                  RowBox[{"1", ",", " ", "2"}], "]"}], "]"}], ",", " ", 
                "Red"}], "]"}], ",", " ", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"matrix", "[", 
                 RowBox[{"[", 
                  RowBox[{"1", ",", " ", "3"}], "]"}], "]"}], ",", " ", 
                "Red"}], "]"}], ",", " ", "\n", "                    ", 
              "\"\<] \[CenterDot] [\>\"", ",", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"vector", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], ",", " ", "Blue"}], "]"}], 
              ",", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"vector", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], ",", " ", "Blue"}], "]"}], 
              ",", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"vector", "[", 
                 RowBox[{"[", "3", "]"}], "]"}], ",", " ", "Blue"}], "]"}], 
              ",", "\n", "                    ", "\"\<]^T =\>\"", ",", "\n", 
              "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"result", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], ",", " ", "darkGreen"}], 
               "]"}]}], "\n", "                ", "}"}], "\n", "            ",
             "}"}], ",", " ", 
           RowBox[{"Alignment", " ", "->", " ", "Left"}]}], "]"}], "\n", 
         "        ", "]"}], ";", "\n", "        ", "\n", "        ", "\n", 
        "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<   = (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"1", ",", " ", "1"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\< \[CenterDot] \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"vector", "[", 
             RowBox[{"[", "1", "]"}], "]"}], ",", " ", "Blue"}], "]"}], ",", 
          " ", "\n", "              ", "\"\<) + (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"1", ",", " ", "2"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\< \[CenterDot] \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"vector", "[", 
             RowBox[{"[", "2", "]"}], "]"}], ",", " ", "Blue"}], "]"}], ",", 
          " ", "\n", "              ", "\"\<) + (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"1", ",", " ", "3"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\< \[CenterDot] \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"vector", "[", 
             RowBox[{"[", "3", "]"}], "]"}], ",", " ", "Blue"}], "]"}], ",", 
          " ", "\"\<)\>\""}], "]"}], ";", "\n", "        ", "\n", "        ", 
        "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<   = (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"1", ",", " ", "1"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\< \[CenterDot] \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"vector", "[", 
             RowBox[{"[", "1", "]"}], "]"}], ",", " ", "Blue"}], "]"}], ",", 
          " ", "\n", "              ", "\"\<) + (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"1", ",", " ", "2"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\< \[CenterDot] \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"vector", "[", 
             RowBox[{"[", "2", "]"}], "]"}], ",", " ", "Blue"}], "]"}], ",", 
          " ", "\n", "              ", "\"\<) + (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"1", ",", " ", "3"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\<)\>\""}], "]"}], ";", "\n", "        ", "\n",
         "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<   = \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"matrix", "[", 
              RowBox[{"[", 
               RowBox[{"1", ",", " ", "1"}], "]"}], "]"}], "*", 
             RowBox[{"vector", "[", 
              RowBox[{"[", "1", "]"}], "]"}]}], ",", " ", "Purple"}], "]"}], 
          ",", " ", "\n", "              ", "\"\< + \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"matrix", "[", 
              RowBox[{"[", 
               RowBox[{"1", ",", " ", "2"}], "]"}], "]"}], "*", 
             RowBox[{"vector", "[", 
              RowBox[{"[", "2", "]"}], "]"}]}], ",", " ", "Purple"}], "]"}], 
          ",", "\n", "              ", "\"\< + \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"matrix", "[", 
              RowBox[{"[", 
               RowBox[{"1", ",", " ", "3"}], "]"}], "]"}], "*", 
             RowBox[{"vector", "[", 
              RowBox[{"[", "3", "]"}], "]"}]}], ",", " ", "Purple"}], "]"}]}],
          "]"}], ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<   = \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"result", "[", 
             RowBox[{"[", "1", "]"}], "]"}], ",", " ", "darkGreen"}], "]"}]}],
          "]"}], ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\n\[Bullet] V\[YAcute]po\[CHacek]et novej y-ovej \
s\[UAcute]radnice (2. riadok matice):\>\"", ",", " ", "Brown"}], "]"}], "]"}],
         ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", "\n", "            ", 
         RowBox[{"Grid", "[", 
          RowBox[{
           RowBox[{"{", "\n", "                ", 
            RowBox[{"{", "\n", "                    ", 
             RowBox[{"\"\<   [\>\"", ",", " ", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"matrix", "[", 
                 RowBox[{"[", 
                  RowBox[{"2", ",", " ", "1"}], "]"}], "]"}], ",", " ", 
                "Red"}], "]"}], ",", " ", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"matrix", "[", 
                 RowBox[{"[", 
                  RowBox[{"2", ",", " ", "2"}], "]"}], "]"}], ",", " ", 
                "Red"}], "]"}], ",", " ", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"matrix", "[", 
                 RowBox[{"[", 
                  RowBox[{"2", ",", " ", "3"}], "]"}], "]"}], ",", " ", 
                "Red"}], "]"}], ",", " ", "\n", "                    ", 
              "\"\<] \[CenterDot] [\>\"", ",", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"vector", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], ",", " ", "Blue"}], "]"}], 
              ",", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"vector", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], ",", " ", "Blue"}], "]"}], 
              ",", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"vector", "[", 
                 RowBox[{"[", "3", "]"}], "]"}], ",", " ", "Blue"}], "]"}], 
              ",", "\n", "                    ", "\"\<]^T =\>\"", ",", "\n", 
              "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"result", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], ",", " ", "darkGreen"}], 
               "]"}]}], "\n", "                ", "}"}], "\n", "            ",
             "}"}], ",", " ", 
           RowBox[{"Alignment", " ", "->", " ", "Left"}]}], "]"}], "\n", 
         "        ", "]"}], ";", "\n", "        ", "\n", "        ", "\n", 
        "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<   = (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"2", ",", " ", "1"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\< \[CenterDot] \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"vector", "[", 
             RowBox[{"[", "1", "]"}], "]"}], ",", " ", "Blue"}], "]"}], ",", 
          " ", "\n", "              ", "\"\<) + (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"2", ",", " ", "2"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\< \[CenterDot] \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"vector", "[", 
             RowBox[{"[", "2", "]"}], "]"}], ",", " ", "Blue"}], "]"}], ",", 
          " ", "\n", "              ", "\"\<) + (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"2", ",", " ", "3"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\< \[CenterDot] \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"vector", "[", 
             RowBox[{"[", "3", "]"}], "]"}], ",", " ", "Blue"}], "]"}], ",", 
          " ", "\"\<)\>\""}], "]"}], ";", "\n", "        ", "\n", "        ", 
        "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<   = (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"2", ",", " ", "1"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\< \[CenterDot] \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"vector", "[", 
             RowBox[{"[", "1", "]"}], "]"}], ",", " ", "Blue"}], "]"}], ",", 
          " ", "\n", "              ", "\"\<) + (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"2", ",", " ", "2"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\< \[CenterDot] \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"vector", "[", 
             RowBox[{"[", "2", "]"}], "]"}], ",", " ", "Blue"}], "]"}], ",", 
          " ", "\n", "              ", "\"\<) + (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"2", ",", " ", "3"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\<)\>\""}], "]"}], ";", "\n", "        ", "\n",
         "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<   = \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"matrix", "[", 
              RowBox[{"[", 
               RowBox[{"2", ",", " ", "1"}], "]"}], "]"}], "*", 
             RowBox[{"vector", "[", 
              RowBox[{"[", "1", "]"}], "]"}]}], ",", " ", "Purple"}], "]"}], 
          ",", " ", "\n", "              ", "\"\< + \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"matrix", "[", 
              RowBox[{"[", 
               RowBox[{"2", ",", " ", "2"}], "]"}], "]"}], "*", 
             RowBox[{"vector", "[", 
              RowBox[{"[", "2", "]"}], "]"}]}], ",", " ", "Purple"}], "]"}], 
          ",", "\n", "              ", "\"\< + \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"matrix", "[", 
              RowBox[{"[", 
               RowBox[{"2", ",", " ", "3"}], "]"}], "]"}], "*", 
             RowBox[{"vector", "[", 
              RowBox[{"[", "3", "]"}], "]"}]}], ",", " ", "Purple"}], "]"}]}],
          "]"}], ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<   = \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"result", "[", 
             RowBox[{"[", "2", "]"}], "]"}], ",", " ", "darkGreen"}], "]"}]}],
          "]"}], ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\n\[Bullet] V\[YAcute]po\[CHacek]et homog\[EAcute]nnej s\
\[UAcute]radnice (3. riadok matice):\>\"", ",", " ", "Brown"}], "]"}], "]"}], 
        ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", "\n", "            ", 
         RowBox[{"Grid", "[", 
          RowBox[{
           RowBox[{"{", "\n", "                ", 
            RowBox[{"{", "\n", "                    ", 
             RowBox[{"\"\<   [\>\"", ",", " ", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"matrix", "[", 
                 RowBox[{"[", 
                  RowBox[{"3", ",", " ", "1"}], "]"}], "]"}], ",", " ", 
                "Red"}], "]"}], ",", " ", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"matrix", "[", 
                 RowBox[{"[", 
                  RowBox[{"3", ",", " ", "2"}], "]"}], "]"}], ",", " ", 
                "Red"}], "]"}], ",", " ", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"matrix", "[", 
                 RowBox[{"[", 
                  RowBox[{"3", ",", " ", "3"}], "]"}], "]"}], ",", " ", 
                "Red"}], "]"}], ",", " ", "\n", "                    ", 
              "\"\<] \[CenterDot] [\>\"", ",", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"vector", "[", 
                 RowBox[{"[", "1", "]"}], "]"}], ",", " ", "Blue"}], "]"}], 
              ",", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"vector", "[", 
                 RowBox[{"[", "2", "]"}], "]"}], ",", " ", "Blue"}], "]"}], 
              ",", "\n", "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"vector", "[", 
                 RowBox[{"[", "3", "]"}], "]"}], ",", " ", "Blue"}], "]"}], 
              ",", "\n", "                    ", "\"\<]^T =\>\"", ",", "\n", 
              "                    ", 
              RowBox[{"Style", "[", 
               RowBox[{
                RowBox[{"result", "[", 
                 RowBox[{"[", "3", "]"}], "]"}], ",", " ", "darkGreen"}], 
               "]"}]}], "\n", "                ", "}"}], "\n", "            ",
             "}"}], ",", " ", 
           RowBox[{"Alignment", " ", "->", " ", "Left"}]}], "]"}], "\n", 
         "        ", "]"}], ";", "\n", "        ", "\n", "        ", "\n", 
        "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<   = (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"3", ",", " ", "1"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\< \[CenterDot] \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"vector", "[", 
             RowBox[{"[", "1", "]"}], "]"}], ",", " ", "Blue"}], "]"}], ",", 
          " ", "\n", "              ", "\"\<) + (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"3", ",", " ", "2"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\< \[CenterDot] \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"vector", "[", 
             RowBox[{"[", "2", "]"}], "]"}], ",", " ", "Blue"}], "]"}], ",", 
          " ", "\n", "              ", "\"\<) + (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"3", ",", " ", "3"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\< \[CenterDot] \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"vector", "[", 
             RowBox[{"[", "3", "]"}], "]"}], ",", " ", "Blue"}], "]"}], ",", 
          " ", "\"\<)\>\""}], "]"}], ";", "\n", "        ", "\n", "        ", 
        "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<   = (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"3", ",", " ", "1"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\< \[CenterDot] \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"vector", "[", 
             RowBox[{"[", "1", "]"}], "]"}], ",", " ", "Blue"}], "]"}], ",", 
          " ", "\n", "              ", "\"\<) + (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"3", ",", " ", "2"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\< \[CenterDot] \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"vector", "[", 
             RowBox[{"[", "2", "]"}], "]"}], ",", " ", "Blue"}], "]"}], ",", 
          " ", "\n", "              ", "\"\<) + (\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"matrix", "[", 
             RowBox[{"[", 
              RowBox[{"3", ",", " ", "3"}], "]"}], "]"}], ",", " ", "Red"}], 
           "]"}], ",", " ", "\"\<)\>\""}], "]"}], ";", "\n", "        ", "\n",
         "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<   = \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"matrix", "[", 
              RowBox[{"[", 
               RowBox[{"3", ",", " ", "1"}], "]"}], "]"}], "*", 
             RowBox[{"vector", "[", 
              RowBox[{"[", "1", "]"}], "]"}]}], ",", " ", "Purple"}], "]"}], 
          ",", " ", "\n", "              ", "\"\< + \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"matrix", "[", 
              RowBox[{"[", 
               RowBox[{"3", ",", " ", "2"}], "]"}], "]"}], "*", 
             RowBox[{"vector", "[", 
              RowBox[{"[", "2", "]"}], "]"}]}], ",", " ", "Purple"}], "]"}], 
          ",", "\n", "              ", "\"\< + \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"matrix", "[", 
              RowBox[{"[", 
               RowBox[{"3", ",", " ", "3"}], "]"}], "]"}], "*", 
             RowBox[{"vector", "[", 
              RowBox[{"[", "3", "]"}], "]"}]}], ",", " ", "Purple"}], "]"}]}],
          "]"}], ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<   = \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"result", "[", 
             RowBox[{"[", "3", "]"}], "]"}], ",", " ", "darkGreen"}], "]"}]}],
          "]"}], ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
           RowBox[{
           "\"\<\\nV\[YAcute]sledn\[AAcute] homog\[EAcute]nna reprezent\
\[AAcute]cia bodu \>\"", " ", "<>", " ", "pointName", " ", "<>", " ", 
            "\"\<':\>\""}], ",", " ", "Bold"}], "]"}], "]"}], ";", "\n", 
        "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<[\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"result", "[", 
             RowBox[{"[", "1", "]"}], "]"}], ",", " ", "darkGreen"}], "]"}], 
          ",", " ", "\"\<, \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"result", "[", 
             RowBox[{"[", "2", "]"}], "]"}], ",", " ", "darkGreen"}], "]"}], 
          ",", " ", "\"\<, \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"result", "[", 
             RowBox[{"[", "3", "]"}], "]"}], ",", " ", "darkGreen"}], "]"}], 
          ",", " ", "\"\<]\>\""}], "]"}], ";", "\n", "        ", "\n", 
        "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
           RowBox[{
           "\"\<\\nKart\[EAcute]zske s\[UAcute]radnice bodu \>\"", " ", "<>", 
            " ", "pointName", " ", "<>", " ", "\"\<':\>\""}], ",", " ", 
           "Bold"}], "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<[\>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"result", "[", 
             RowBox[{"[", "1", "]"}], "]"}], ",", " ", "darkGreen"}], "]"}], 
          ",", " ", "\"\<, \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{
            RowBox[{"result", "[", 
             RowBox[{"[", "2", "]"}], "]"}], ",", " ", "darkGreen"}], "]"}], 
          ",", " ", "\"\<]\>\""}], "]"}], ";"}]}], "\n", "    ", "]"}]}], 
    ";"}], "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodPosun", "[", "inputPoint_", "]"}], " ", ":=", " ", "\n", 
     "    ", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
        "outputPoint", ",", " ", "dx", ",", " ", "dy", ",", " ", "invDx", ",",
          " ", "invDy", ",", "\n", "            ", "transformMatrix", ",", 
         " ", "homogeneousPoint", ",", " ", "resultVector", ",", " ", 
         RowBox[{"darkGreen", " ", "=", " ", 
          RowBox[{"RGBColor", "[", 
           RowBox[{"0", ",", " ", "0.5", ",", " ", "0"}], "]"}]}]}], "}"}], 
       ",", "\n", "            ", "\n", "        ", "\n", "        ", 
       RowBox[{
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{"BodPosunVector", " ", "=!=", " ", 
           RowBox[{"{", 
            RowBox[{"0", ",", " ", "0"}], "}"}]}], ",", "\n", "            ", 
          "\n", "            ", 
          RowBox[{
           RowBox[{
            RowBox[{"{", 
             RowBox[{"dx", ",", " ", "dy"}], "}"}], " ", "=", " ", 
            "BodPosunVector"}], ";", "\n", "            ", "\n", 
           "            ", 
           RowBox[{"BodPosunVector", " ", "=", " ", 
            RowBox[{"{", 
             RowBox[{"0", ",", " ", "0"}], "}"}]}], ";"}], "\n", "        ", 
          ",", "\n", "            ", "\n", "            ", 
          RowBox[{
           RowBox[{
            RowBox[{"{", 
             RowBox[{
             "dx", ",", " ", "dy", ",", " ", "_", ",", " ", "outputPoint"}], 
             "}"}], " ", "=", " ", 
            RowBox[{"GetTranslationParameters", "[", "inputPoint", "]"}]}], 
           ";"}]}], "\n", "        ", "]"}], ";", "\n", "        ", "\n", 
        "        ", "\n", "        ", 
        RowBox[{"outputPoint", " ", "=", " ", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{
            RowBox[{"inputPoint", "[", 
             RowBox[{"[", "1", "]"}], "]"}], " ", "+", " ", "dx"}], ",", " ", 
           
           RowBox[{
            RowBox[{"inputPoint", "[", 
             RowBox[{"[", "2", "]"}], "]"}], " ", "+", " ", "dy"}]}], "}"}]}],
         ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"invDx", " ", "=", " ", 
         RowBox[{"-", "dx"}]}], ";", "\n", "        ", 
        RowBox[{"invDy", " ", "=", " ", 
         RowBox[{"-", "dy"}]}], ";", "\n", "        ", "\n", "        ", "\n",
         "        ", 
        RowBox[{"transformMatrix", " ", "=", " ", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"{", 
            RowBox[{"1", ",", " ", "0", ",", " ", "dx"}], "}"}], ",", " ", 
           RowBox[{"{", 
            RowBox[{"0", ",", " ", "1", ",", " ", "dy"}], "}"}], ",", " ", 
           RowBox[{"{", 
            RowBox[{"0", ",", " ", "0", ",", " ", "1"}], "}"}]}], "}"}]}], 
        ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", "\n", "            ", 
          RowBox[{
          "\"\<GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE BODU - \
POSUN\>\"", ",", " ", "\n", "            ", "Bold", ",", " ", "16"}], "]"}], 
         "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<========================================\>\"", ",", " ", 
           "Bold"}], "]"}], "]"}], ";", "\n", "        ", "\n", "        ", 
        "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{"\"\<\\nZADANIE:\>\"", ",", " ", "Bold", ",", " ", "14"}], 
          "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{
        "Print", "[", "\"\<Majme bod P so s\[UAcute]radnicami:\>\"", "]"}], 
        ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{"inputPoint", ",", " ", "Blue"}], "]"}], "]"}], ";", "\n", 
        "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{
         "\"\<\\nVykonajte posun bodu v 2D priestore pomocou vektora posunu: \
\[CapitalDelta] = [\>\"", ",", " ", "\n", "            ", 
          RowBox[{"Style", "[", 
           RowBox[{"dx", ",", " ", "Red"}], "]"}], ",", " ", "\"\<, \>\"", 
          ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{"dy", ",", " ", "Red"}], "]"}], ",", " ", "\"\<]\>\""}], 
         "]"}], ";", "\n", "            ", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<\\nTransforma\[CHacek]n\[AAcute] matica posunu v \
homog\[EAcute]nnych s\[UAcute]radniciach:\>\"", "]"}], ";", "\n", "        ", 
        
        RowBox[{"Print", "[", 
         RowBox[{"MatrixForm", "[", 
          RowBox[{"{", "\n", "            ", 
           RowBox[{
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"1", ",", " ", "Red"}], "]"}], ",", " ", 
              RowBox[{"Style", "[", 
               RowBox[{"0", ",", " ", "Red"}], "]"}], ",", " ", 
              RowBox[{"Style", "[", 
               RowBox[{"dx", ",", " ", "Red"}], "]"}]}], "}"}], ",", " ", 
            "\n", "            ", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"0", ",", " ", "Red"}], "]"}], ",", " ", 
              RowBox[{"Style", "[", 
               RowBox[{"1", ",", " ", "Red"}], "]"}], ",", " ", 
              RowBox[{"Style", "[", 
               RowBox[{"dy", ",", " ", "Red"}], "]"}]}], "}"}], ",", "\n", 
            "            ", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"0", ",", " ", "Red"}], "]"}], ",", " ", 
              RowBox[{"Style", "[", 
               RowBox[{"0", ",", " ", "Red"}], "]"}], ",", " ", 
              RowBox[{"Style", "[", 
               RowBox[{"1", ",", " ", "Red"}], "]"}]}], "}"}]}], "\n", 
           "        ", "}"}], "]"}], "]"}], ";", "\n", "        ", "\n", 
        "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nTE\[CapitalOAcute]RIA MATICOV\[CapitalEAcute]HO ZOBRAZENIA \
POSUNU:\>\"", ",", " ", "Bold", ",", " ", "14"}], "]"}], "]"}], ";", "\n", 
        "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<Pri posune v 2D priestore pou\[ZHacek]\[IAcute]vame homog\
\[EAcute]nne s\[UAcute]radnice, ktor\[EAcute] \
umo\[ZHacek]\[NHacek]uj\[UAcute] vyjadri\[THacek] posun ako maticov\[EAcute] \
n\[AAcute]sobenie:\>\"", "]"}], ";", "\n", "        ", "\n", "        ", "\n",
         "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<\\nTransforma\[CHacek]n\[AAcute] matica posunu:\>\"", "]"}], 
        ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"MatrixForm", "[", 
          RowBox[{"{", "\n", "            ", 
           RowBox[{
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"1", ",", " ", "Red"}], "]"}], ",", " ", 
              RowBox[{"Style", "[", 
               RowBox[{"0", ",", " ", "Red"}], "]"}], ",", " ", 
              RowBox[{"Style", "[", 
               RowBox[{"\"\<dx\>\"", ",", " ", "Red"}], "]"}]}], "}"}], ",", 
            " ", "\n", "            ", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"0", ",", " ", "Red"}], "]"}], ",", " ", 
              RowBox[{"Style", "[", 
               RowBox[{"1", ",", " ", "Red"}], "]"}], ",", " ", 
              RowBox[{"Style", "[", 
               RowBox[{"\"\<dy\>\"", ",", " ", "Red"}], "]"}]}], "}"}], ",", 
            "\n", "            ", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Style", "[", 
               RowBox[{"0", ",", " ", "Red"}], "]"}], ",", " ", 
              RowBox[{"Style", "[", 
               RowBox[{"0", ",", " ", "Red"}], "]"}], ",", " ", 
              RowBox[{"Style", "[", 
               RowBox[{"1", ",", " ", "Red"}], "]"}]}], "}"}]}], "\n", 
           "        ", "}"}], "]"}], "]"}], ";", "\n", "        ", "\n", 
        "        ", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<\\nAk je bod P = [x, y] v z\[AAcute]kladn\[YAcute]ch \
s\[UAcute]radniciach, v homog\[EAcute]nnych s\[UAcute]radniciach ho zap\
\[IAcute]\[SHacek]eme ako P = [x, y, 1].\\nPosun potom m\[OHat]\[ZHacek]eme \
zap\[IAcute]sa\[THacek] ako maticov\[EAcute] n\[AAcute]sobenie:\>\"", "]"}], 
        ";", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Row", "[", 
          RowBox[{"{", "\n", "            ", 
           RowBox[{
            RowBox[{"MatrixForm", "[", 
             RowBox[{"{", "\n", "                ", 
              RowBox[{
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"Style", "[", 
                  RowBox[{"1", ",", " ", "Red"}], "]"}], ",", " ", 
                 RowBox[{"Style", "[", 
                  RowBox[{"0", ",", " ", "Red"}], "]"}], ",", " ", 
                 RowBox[{"Style", "[", 
                  RowBox[{"\"\<dx\>\"", ",", " ", "Red"}], "]"}]}], "}"}], 
               ",", " ", "\n", "                ", 
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"Style", "[", 
                  RowBox[{"0", ",", " ", "Red"}], "]"}], ",", " ", 
                 RowBox[{"Style", "[", 
                  RowBox[{"1", ",", " ", "Red"}], "]"}], ",", " ", 
                 RowBox[{"Style", "[", 
                  RowBox[{"\"\<dy\>\"", ",", " ", "Red"}], "]"}]}], "}"}], 
               ",", "\n", "                ", 
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"Style", "[", 
                  RowBox[{"0", ",", " ", "Red"}], "]"}], ",", " ", 
                 RowBox[{"Style", "[", 
                  RowBox[{"0", ",", " ", "Red"}], "]"}], ",", " ", 
                 RowBox[{"Style", "[", 
                  RowBox[{"1", ",", " ", "Red"}], "]"}]}], "}"}]}], "\n", 
              "            ", "}"}], "]"}], ",", "\n", "            ", 
            "\"\< \[CenterDot] \>\"", ",", "\n", "            ", 
            RowBox[{"MatrixForm", "[", 
             RowBox[{"{", "\n", "                ", 
              RowBox[{
               RowBox[{"{", 
                RowBox[{"Style", "[", 
                 RowBox[{"\"\<x\>\"", ",", " ", "Blue"}], "]"}], "}"}], ",", 
               " ", "\n", "                ", 
               RowBox[{"{", 
                RowBox[{"Style", "[", 
                 RowBox[{"\"\<y\>\"", ",", " ", "Blue"}], "]"}], "}"}], ",", 
               "\n", "                ", 
               RowBox[{"{", 
                RowBox[{"Style", "[", 
                 RowBox[{"1", ",", " ", "Blue"}], "]"}], "}"}]}], "\n", 
              "            ", "}"}], "]"}], ",", "\n", "            ", 
            "\"\< = \>\"", ",", "\n", "            ", 
            RowBox[{"MatrixForm", "[", 
             RowBox[{"{", "\n", "                ", 
              RowBox[{
               RowBox[{"{", 
                RowBox[{"Style", "[", 
                 RowBox[{"\"\<x + dx\>\"", ",", " ", "darkGreen"}], "]"}], 
                "}"}], ",", " ", "\n", "                ", 
               RowBox[{"{", 
                RowBox[{"Style", "[", 
                 RowBox[{"\"\<y + dy\>\"", ",", " ", "darkGreen"}], "]"}], 
                "}"}], ",", "\n", "                ", 
               RowBox[{"{", 
                RowBox[{"Style", "[", 
                 RowBox[{"1", ",", " ", "darkGreen"}], "]"}], "}"}]}], "\n", 
              "            ", "}"}], "]"}]}], "\n", "        ", "}"}], "]"}], 
         "]"}], ";", "\n", "        ", "\n", "        ", 
        RowBox[{
        "Print", "[", "\"\<\\nPre na\[SHacek]e hodnoty posunu:\>\"", "]"}], 
        ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<dx = \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{"dx", ",", " ", "Red"}], "]"}], ",", " ", 
          "\"\< (posun v smere osi x)\>\""}], "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"\"\<dy = \>\"", ",", " ", 
          RowBox[{"Style", "[", 
           RowBox[{"dy", ",", " ", "Red"}], "]"}], ",", " ", 
          "\"\< (posun v smere osi y)\>\""}], "]"}], ";", "\n", "        ", 
        "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nV\[CapitalYAcute]PO\[CapitalCHacek]ET POSUNU BODU:\>\"", 
           ",", " ", "Bold", ",", " ", "14"}], "]"}], "]"}], ";", "\n", 
        "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Module", "[", 
         RowBox[{
          RowBox[{"{", 
           RowBox[{
            RowBox[{"x", " ", "=", " ", 
             RowBox[{"inputPoint", "[", 
              RowBox[{"[", "1", "]"}], "]"}]}], ",", " ", "\n", 
            "               ", 
            RowBox[{"y", " ", "=", " ", 
             RowBox[{"inputPoint", "[", 
              RowBox[{"[", "2", "]"}], "]"}]}], ",", "\n", "               ", 
            "homogeneousVector", ",", " ", "resultVector"}], "}"}], ",", "\n",
           "               ", "\n", "            ", 
          RowBox[{
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{"\"\<\\nBOD P:\>\"", ",", " ", "Bold", ",", " ", "14"}], 
             "]"}], "]"}], ";", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{
            "\"\<P\[OHat]vodn\[EAcute] s\[UAcute]radnice: [\>\"", ",", " ", 
             RowBox[{"Style", "[", 
              RowBox[{"x", ",", " ", "Blue"}], "]"}], ",", " ", "\"\<, \>\"", 
             ",", " ", 
             RowBox[{"Style", "[", 
              RowBox[{"y", ",", " ", "Blue"}], "]"}], ",", " ", "\"\<]\>\""}],
             "]"}], ";", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{
            "\"\<Homog\[EAcute]nne s\[UAcute]radnice: [\>\"", ",", " ", 
             RowBox[{"Style", "[", 
              RowBox[{"x", ",", " ", "Blue"}], "]"}], ",", " ", "\"\<, \>\"", 
             ",", " ", 
             RowBox[{"Style", "[", 
              RowBox[{"y", ",", " ", "Blue"}], "]"}], ",", " ", "\"\<, \>\"", 
             ",", " ", 
             RowBox[{"Style", "[", 
              RowBox[{"1", ",", " ", "Blue"}], "]"}], ",", " ", "\"\<]\>\""}],
             "]"}], ";", "\n", "            ", "\n", "            ", "\n", 
           "            ", 
           RowBox[{"homogeneousVector", " ", "=", " ", 
            RowBox[{"{", 
             RowBox[{"x", ",", " ", "y", ",", " ", "1"}], "}"}]}], ";", "\n", 
           "            ", 
           RowBox[{"resultVector", " ", "=", " ", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"x", " ", "+", " ", "dx"}], ",", " ", 
              RowBox[{"y", " ", "+", " ", "dy"}], ",", " ", "1"}], "}"}]}], 
           ";", "\n", "            ", "\n", "            ", "\n", 
           "            ", 
           RowBox[{
           "DisplayIntuitiveMathMatrixMultiplication", "[", "\n", 
            "                ", 
            RowBox[{
            "transformMatrix", ",", " ", "\n", "                ", 
             "homogeneousVector", ",", " ", "\n", "                ", 
             "resultVector", ",", "\n", "                ", "\"\<P\>\""}], 
            "\n", "            ", "]"}], ";", "\n", "            ", "\n", 
           "            ", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
             "\"\<\\nAlternat\[IAcute]vny v\[YAcute]po\[CHacek]et pomocou z\
\[AAcute]kladn\[YAcute]ch vzorcov posunu:\>\"", ",", " ", "Bold"}], "]"}], 
            "]"}], ";", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"\"\<x' = x + dx  =>  \>\"", ",", " ", 
             RowBox[{"Style", "[", 
              RowBox[{"x", ",", " ", "Blue"}], "]"}], ",", " ", "\"\< + \>\"",
              ",", " ", 
             RowBox[{"Style", "[", 
              RowBox[{"dx", ",", " ", "Red"}], "]"}], ",", " ", "\"\< = \>\"",
              ",", " ", 
             RowBox[{"Style", "[", 
              RowBox[{
               RowBox[{"x", " ", "+", " ", "dx"}], ",", " ", "darkGreen"}], 
              "]"}]}], "]"}], ";", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"\"\<y' = y + dy  =>  \>\"", ",", " ", 
             RowBox[{"Style", "[", 
              RowBox[{"y", ",", " ", "Blue"}], "]"}], ",", " ", "\"\< + \>\"",
              ",", " ", 
             RowBox[{"Style", "[", 
              RowBox[{"dy", ",", " ", "Red"}], "]"}], ",", " ", "\"\< = \>\"",
              ",", " ", 
             RowBox[{"Style", "[", 
              RowBox[{
               RowBox[{"y", " ", "+", " ", "dy"}], ",", " ", "darkGreen"}], 
              "]"}]}], "]"}], ";", "\n", "            ", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
             "\"\<\\nV\[YAcute]sledn\[EAcute] s\[UAcute]radnice bodu P':\>\"",
               ",", " ", "Bold"}], "]"}], "]"}], ";", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"\"\<[\>\"", ",", " ", 
             RowBox[{"Style", "[", 
              RowBox[{
               RowBox[{"outputPoint", "[", 
                RowBox[{"[", "1", "]"}], "]"}], ",", " ", "darkGreen"}], 
              "]"}], ",", " ", "\"\<, \>\"", ",", " ", 
             RowBox[{"Style", "[", 
              RowBox[{
               RowBox[{"outputPoint", "[", 
                RowBox[{"[", "2", "]"}], "]"}], ",", " ", "darkGreen"}], 
              "]"}], ",", " ", "\"\<]\>\""}], "]"}], ";", "\n", 
           "            ", "\n", "            ", 
           RowBox[{"Print", "[", 
            RowBox[{"Style", "[", 
             RowBox[{
             "\"\<--------------------------------------------------\>\"", 
              ",", " ", "Bold"}], "]"}], "]"}], ";"}]}], "\n", "        ", 
         "]"}], ";", "\n", "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nV\[CapitalYAcute]SLEDOK TRANSFORM\[CapitalAAcute]CIE \
BODU:\>\"", ",", " ", "Bold", ",", " ", "14"}], "]"}], "]"}], ";", "\n", 
        "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<P\[OHat]vodn\[YAcute] bod P:\>\"", ",", " ", "Blue", ",", " ", 
           "Bold"}], "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{"inputPoint", ",", " ", "Blue"}], "]"}], "]"}], ";", "\n", 
        "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<Posunut\[YAcute] bod P':\>\"", ",", " ", "Red", ",", " ", 
           "Bold"}], "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{"outputPoint", ",", " ", "Red"}], "]"}], "]"}], ";", "\n", 
        "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nVIZUALIZ\[CapitalAAcute]CIA TRANSFORM\[CapitalAAcute]CIE:\>\
\"", ",", " ", "Bold"}], "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"CreateVisualization", "[", 
          RowBox[{"inputPoint", ",", " ", "outputPoint", ",", " ", 
           RowBox[{"{", 
            RowBox[{"dx", ",", " ", "dy"}], "}"}]}], "]"}], "]"}], ";", "\n", 
        "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{"\"\<\\nLEGENDA:\>\"", ",", " ", "Bold", ",", " ", "14"}], 
          "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{
          RowBox[{"Style", "[", 
           RowBox[{"\"\<\[Bullet] Modr\[EAcute]\>\"", ",", " ", "Blue"}], 
           "]"}], ",", " ", 
          "\"\< body a \[CHacek]iary: P\[OHat]vodn\[YAcute] bod\>\""}], "]"}],
         ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{
          RowBox[{"Style", "[", 
           RowBox[{
           "\"\<\[Bullet] \[CapitalCHacek]erven\[EAcute]\>\"", ",", " ", 
            "Red"}], "]"}], ",", " ", 
          "\"\< body a \[CHacek]iary: Posunut\[YAcute] bod\>\""}], "]"}], ";",
         "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{
          RowBox[{"Style", "[", 
           RowBox[{
           "\"\<\[Bullet] Zelen\[EAcute]\>\"", ",", " ", "darkGreen"}], "]"}],
           ",", " ", 
          "\"\< preru\[SHacek]ovan\[EAcute] \[CHacek]iary: Posun bodu\>\""}], 
         "]"}], ";", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nFAREBN\[CapitalEAcute] OZNA\[CapitalCHacek]ENIA V MATICOV\
\[CapitalYAcute]CH V\[CapitalYAcute]PO\[CapitalCHacek]TOCH:\>\"", ",", " ", 
           "Bold"}], "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{
          RowBox[{"Style", "[", 
           RowBox[{
           "\"\<\[Bullet] \[CapitalCHacek]erven\[AAcute]:\>\"", ",", " ", 
            "Red"}], "]"}], ",", " ", 
          "\"\< Transforma\[CHacek]n\[AAcute] matica\>\""}], "]"}], ";", "\n",
         "        ", 
        RowBox[{"Print", "[", 
         RowBox[{
          RowBox[{"Style", "[", 
           RowBox[{"\"\<\[Bullet] Modr\[AAcute]:\>\"", ",", " ", "Blue"}], 
           "]"}], ",", " ", "\"\< Vstupn\[EAcute] s\[UAcute]radnice\>\""}], 
         "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{
          RowBox[{"Style", "[", 
           RowBox[{"\"\<\[Bullet] Fialov\[AAcute]:\>\"", ",", " ", "Purple"}],
            "]"}], ",", " ", "\"\< Medziv\[YAcute]po\[CHacek]ty\>\""}], "]"}],
         ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{
          RowBox[{"Style", "[", 
           RowBox[{
           "\"\<\[Bullet] Zelen\[AAcute]:\>\"", ",", " ", "darkGreen"}], 
           "]"}], ",", " ", 
          "\"\< V\[YAcute]sledn\[EAcute] s\[UAcute]radnice\>\""}], "]"}], ";",
         "\n", "                ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nP\[OHat]vodn\[YAcute] bod (modr\[AAcute]):\>\"", ",", " ", 
           "Bold"}], "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Row", "[", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{"Style", "[", 
             RowBox[{"\"\<Bod P: \>\"", ",", " ", 
              RowBox[{"RGBColor", "[", 
               RowBox[{"0.1", ",", " ", "0.1", ",", " ", "1"}], "]"}]}], 
             "]"}], ",", " ", "inputPoint"}], "}"}], "]"}], "]"}], ";", "\n", 
        "                ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nV\[YAcute]sledn\[YAcute] bod \
(\[CHacek]erven\[AAcute]):\>\"", ",", " ", "Bold"}], "]"}], "]"}], ";", "\n", 
        "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Row", "[", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{"Style", "[", 
             RowBox[{"\"\<Bod P': \>\"", ",", " ", 
              RowBox[{"RGBColor", "[", 
               RowBox[{"1", ",", " ", "0.1", ",", " ", "0.1"}], "]"}]}], 
             "]"}], ",", " ", "outputPoint"}], "}"}], "]"}], "]"}], ";", "\n",
         "                ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nInverzn\[AAcute] transform\[AAcute]cia:\>\"", ",", " ", 
           "Bold"}], "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<Pre vr\[AAcute]tenie do p\[OHat]vodn\[EAcute]ho stavu pou\
\[ZHacek]ite vektor posunu:\>\"", "]"}], ";", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{
         "\"\<\[CapitalDelta]' = [\>\"", ",", " ", "invDx", ",", " ", 
          "\"\<, \>\"", ",", " ", "invDy", ",", " ", 
          "\"\<] (opa\[CHacek]n\[YAcute] vektor)\>\""}], "]"}], ";", "\n", 
        "        ", "\n", "        ", "\n", "        ", 
        RowBox[{"Print", "[", 
         RowBox[{"Style", "[", 
          RowBox[{
          "\"\<\\nMATEMATICK\[CapitalEAcute] VLASTNOSTI:\>\"", ",", " ", 
           "Bold", ",", " ", "14"}], "]"}], "]"}], ";", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<\[Bullet] Posun zachov\[AAcute]va tvar a ve\:013ekos\[THacek] \
geometrick\[YAcute]ch \[UAcute]tvarov\>\"", "]"}], ";", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<\[Bullet] Posun zachov\[AAcute]va vzdialenosti medzi bodmi - je \
to izometria\>\"", "]"}], ";", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<\[Bullet] Posun zachov\[AAcute]va rovnobe\[ZHacek]nos\[THacek] \
priamok\>\"", "]"}], ";", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<\[Bullet] Posun zachov\[AAcute]va uhly medzi priamkami\>\"", 
         "]"}], ";", "\n", "        ", 
        RowBox[{
        "Print", "[", 
         "\"\<\[Bullet] Na rozdiel od rot\[AAcute]cie, posun nem\[AAcute] \
\[ZHacek]iadny fixn\[YAcute] bod (bod, ktor\[YAcute] zost\[AAcute]va na \
rovnakom mieste)\>\"", "]"}], ";", "\n", "                ", "\n", "        ",
         "\n", "        ", "outputPoint"}]}], "\n", "    ", "]"}]}], ";"}], 
   "\n", "\n", "\n", 
   RowBox[{
    RowBox[{
     RowBox[{"BodPosun", "::", "invalidInput"}], " ", "=", " ", 
     "\"\<Neplatn\[YAcute] vstup. O\[CHacek]ak\[AAcute]va sa bod so \
s\[UAcute]radnicami [x, y].\>\""}], ";"}], "\n", "\n", 
   RowBox[{
    RowBox[{"End", "[", "]"}], ";"}], "\n", 
   RowBox[{
    RowBox[{"EndPackage", "[", "]"}], ";"}]}]}]], "Code",
 CellChangeTimes->{{3.952792901413539*^9, 3.952792981615295*^9}, {
  3.955024551057796*^9, 3.955024573313505*^9}, {3.9557064356691227`*^9, 
  3.9557064478317432`*^9}, {3.9557671926321898`*^9, 3.955767227820746*^9}, {
  3.955767323223488*^9, 3.955767380225047*^9}, {3.9557677391123457`*^9, 
  3.9557677401200314`*^9}, {3.955767848520174*^9, 3.955767865157909*^9}},
 CellLabel->
  "In[19440]:=",ExpressionUUID->"5c8a06ec-6e7d-40b0-b49f-cfd4a27be3c4"],

Cell[BoxData[""], "Input",
 CellChangeTimes->{{3.955766825868209*^9, 
  3.955766825873898*^9}},ExpressionUUID->"091d9c8d-370c-4860-8e70-\
da34108f1adf"]
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
Cell[554, 20, 72681, 1605, 8556, "Code",ExpressionUUID->"5c8a06ec-6e7d-40b0-b49f-cfd4a27be3c4"],
Cell[73238, 1627, 152, 3, 29, "Input",ExpressionUUID->"091d9c8d-370c-4860-8e70-da34108f1adf"]
}
]
*)

