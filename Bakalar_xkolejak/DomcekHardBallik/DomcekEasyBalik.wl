(* ::Package:: *)

(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)

(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


BeginPackage["DomcekEasyBalik`"];

(* Export verejn\[YAcute]ch symbolov *)
DomcekJednoduchaTransformacia::usage = 
    "DomcekJednoduchaTransformacia[] umo\[ZHacek]n\[IAcute] v\[YAcute]ber a aplik\[AAcute]ciu jednej transform\[AAcute]cie.";

DomcekJednoduchaTransformaciaSVysledkom::usage = 
    "DomcekJednoduchaTransformaciaSVysledkom[] umo\[ZHacek]n\[IAcute] v\[YAcute]ber a aplik\[AAcute]ciu jednej transform\[AAcute]cie a zobraz\[IAcute] s\[UAcute]hrnn\[YAcute] v\[YAcute]sledok.";

DomcekGeneruj::usage = 
    "DomcekGeneruj[] generuje n\[AAcute]hodn\[YAcute] dom\[CHacek]ek s vhodn\[YAcute]mi vlastnos\[THacek]ami.";

(* Spolo\[CHacek]n\[EAcute] funkcie pre v\[SHacek]etky transform\[AAcute]cie *)
domcekArea::usage = 
    "domcekArea[vertices] vypo\[CHacek]\[IAcute]ta obsah dom\[CHacek]eka s vrcholmi vertices.";

validDomcekQ::usage = 
    "validDomcekQ[vertices] over\[IAcute], \[CHacek]i dom\[CHacek]ek sp\:013a\[NHacek]a krit\[EAcute]ri\[AAcute].";

domcekPerimeter::usage = 
    "domcekPerimeter[vertices] vypo\[CHacek]\[IAcute]ta obvod dom\[CHacek]eka s vrcholmi vertices.";

(* Defin\[IAcute]cia chybov\[YAcute]ch spr\[AAcute]v *)
DomcekJednoduchaTransformacia::infinityerr = 
    "Vyskytla sa chyba: v\[YAcute]po\[CHacek]et viedol k nekone\[CHacek]n\[EAcute]mu alebo neur\[CHacek]it\[EAcute]mu v\[YAcute]sledku. Pou\[ZHacek]ije sa preddefinovan\[YAcute] dom\[CHacek]ek.";

Begin["`Private`"];

(* Potla\[CHacek]enie varovn\[YAcute]ch spr\[AAcute]v o nekone\[CHacek]n\[YAcute]ch v\[YAcute]sledkoch *)
Off[Power::infy];
Off[Infinity::indet];

(* Na\[CHacek]\[IAcute]tanie v\[SHacek]etk\[YAcute]ch transforma\[CHacek]n\[YAcute]ch modulov na za\[CHacek]iatku *)
Needs["DomcekHardBalik`Transforms`Posun`"];
Needs["DomcekHardBalik`Transforms`Rotacia`"];
Needs["DomcekHardBalik`Transforms`ZvacsenieZmensenie`"];
Needs["DomcekHardBalik`Transforms`Skosenie`"];
Needs["DomcekHardBalik`Transforms`Symetria`"];

(* Toto zabezpe\[CHacek]\[IAcute], \[ZHacek]e bud\[UAcute] dostupn\[EAcute] v Easy kontexte *)
DomcekPosun = DomcekHardBalik`Transforms`Posun`DomcekPosun;
DomcekRotacia = DomcekHardBalik`Transforms`Rotacia`DomcekRotacia;
DomcekZvacsenieZmensenie = DomcekHardBalik`Transforms`ZvacsenieZmensenie`DomcekZvacsenieZmensenie;
DomcekSkosenie = DomcekHardBalik`Transforms`Skosenie`DomcekSkosenie;
DomcekSymetria = DomcekHardBalik`Transforms`Symetria`DomcekSymetria;

(* Defin\[IAcute]cia miernej\[SHacek]ej farby zelenej *)
mildGreen = RGBColor[0.2, 0.6, 0.2];

(* Funkcia na v\[YAcute]po\[CHacek]et obsahu dom\[CHacek]eka - po\[CHacek]\[IAcute]tan\[EAcute]ho ako s\[UAcute]\[CHacek]et obsahu obd\:013a\[ZHacek]nika a trojuholn\[IAcute]ka *)
domcekArea[vertices_] := Module[{rectangleArea, triangleArea, area},
    (* Dom\[CHacek]ek m\[AAcute] 5 vrcholov - prv\[EAcute] 4 tvoria obd\:013a\[ZHacek]nik (z\[AAcute]klad\[NHacek]u), piaty je vrchol strechy *)
    (* V\[YAcute]po\[CHacek]et obsahu obd\:013a\[ZHacek]nika ako absol\[UAcute]tna hodnota determinantu *)
    rectangleArea = Check[
        Abs[(vertices[[3, 1]] - vertices[[1, 1]]) * (vertices[[3, 2]] - vertices[[1, 2]])],
        $Failed
    ];
    
    (* V\[YAcute]po\[CHacek]et obsahu trojuholn\[IAcute]ka strechy *)
    triangleArea = Check[
        Abs[(vertices[[4, 1]]*(vertices[[1, 2]] - vertices[[5, 2]]) + 
             vertices[[1, 1]]*(vertices[[5, 2]] - vertices[[4, 2]]) + 
             vertices[[5, 1]]*(vertices[[4, 2]] - vertices[[1, 2]]))/2],
        $Failed
    ];
    
    area = If[rectangleArea === $Failed || triangleArea === $Failed ||
              rectangleArea === ComplexInfinity || triangleArea === ComplexInfinity ||
              rectangleArea === Indeterminate || triangleArea === Indeterminate || 
              !NumberQ[rectangleArea] || !NumberQ[triangleArea],
        (* Vr\[AAcute]ti\[THacek] predvolen\[UAcute] hodnotu v pr\[IAcute]pade chyby *)
        25,
        (* Inak vr\[AAcute]ti\[THacek] skuto\[CHacek]n\[YAcute] obsah *)
        rectangleArea + triangleArea
    ];
    
    area
];

(* Funkcia na v\[YAcute]po\[CHacek]et obvodu dom\[CHacek]eka s kontrolou ch\[YAcute]b *)
domcekPerimeter[vertices_] := Module[{perimeter, sides},
    sides = Table[
        Check[EuclideanDistance[vertices[[i]], vertices[[Mod[i, Length[vertices]] + 1]]], $Failed],
        {i, Length[vertices]}
    ];
    
    If[MemberQ[sides, $Failed],
        (* Vr\[AAcute]ti\[THacek] predvolen\[UAcute] hodnotu v pr\[IAcute]pade chyby *)
        30,
        (* Inak vr\[AAcute]ti\[THacek] skuto\[CHacek]n\[YAcute] obvod *)
        Total[sides]
    ]
];

(* Funkcia na valid\[AAcute]ciu dom\[CHacek]eka s o\[SHacek]etren\[IAcute]m ch\[YAcute]b *)
validDomcekQ[vertices_] := 
    Module[{isValid, area, perimeter, minArea = 10, maxArea = 100, minPerimeter = 15}, 
        (* Kontrola \[CHacek]i s\[UAcute] vstupn\[EAcute] body validn\[EAcute] *)
        If[!MatchQ[vertices, {_?VectorQ, _?VectorQ, _?VectorQ, _?VectorQ, _?VectorQ}] ||
           !AllTrue[vertices, (Length[#] == 2 && AllTrue[#, NumberQ]) &],
            Return[False]
        ];
        
        (* Bezpe\[CHacek]n\[YAcute] v\[YAcute]po\[CHacek]et obsahu a obvodu *)
        area = domcekArea[vertices];
        perimeter = domcekPerimeter[vertices];
        
        (* Zachytenie Indeterminate alebo ComplexInfinity v\[YAcute]sledkov *)
        If[area === ComplexInfinity || area === Indeterminate || 
           perimeter === ComplexInfinity || perimeter === Indeterminate, 
            Return[False]
        ];
        
        (* Hlavn\[AAcute] valid\[AAcute]cia dom\[CHacek]eka *)
        isValid = Check[
            (* Kontrola ve\:013ekosti plochy a obvodu *)
            minArea <= area <= maxArea && 
            perimeter >= minPerimeter &&
            (* Kontrola \[CHacek]i vrchol strechy je nad z\[AAcute]klad\[NHacek]ou *)
            vertices[[5, 2]] > Max[vertices[[1, 2]], vertices[[2, 2]], vertices[[3, 2]], vertices[[4, 2]]] &&
            (* Kontrola tvaru obd\:013a\[ZHacek]nika (z\[AAcute]kladne) *)
            Abs[vertices[[1, 1]] - vertices[[4, 1]]] < 0.001 &&
            Abs[vertices[[2, 1]] - vertices[[3, 1]]] < 0.001 &&
            Abs[vertices[[1, 2]] - vertices[[2, 2]]] < 0.001 &&
            Abs[vertices[[3, 2]] - vertices[[4, 2]]] < 0.001,
            False
        ];
        
        isValid
    ];

(* Vylep\[SHacek]en\[AAcute] funkcia na generovanie n\[AAcute]hodn\[EAcute]ho dom\[CHacek]eka *)
DomcekGeneruj[] := Module[{
    vertices, 
    width, height, roofHeight,
    xOffset, yOffset,
    numberType, 
    simpleDomceky,
    useSpecialNumbers,
    makeSpecialNumber,
    attempts = 0
  },
  
  (* N\[AAcute]hodn\[YAcute] seed pre lep\[SHacek]iu variabilitu *)
  SeedRandom[Hash[{AbsoluteTime[], $ProcessID, RandomInteger[{-10^7, 10^7}]}]];
  
  (* Funkcia na generovanie \[SHacek]peci\[AAcute]lnych \[CHacek]\[IAcute]sel (odmocniny, zlomky) *)
  makeSpecialNumber[base_, variation_] := Module[{type, num, denom},
    type = RandomChoice[{0.5, 0.3, 0.2} -> {"whole", "sqrt", "fraction"}];
    
    Switch[type,
      "whole", 
        base + RandomInteger[{-variation, variation}],
      
      "sqrt", 
        Module[{sqrtBase},
          sqrtBase = RandomChoice[{5, 2, 3, 7, 11}];
          If[RandomReal[] < 0.7,
            base + Sign[RandomReal[]-0.5] * Sqrt[sqrtBase],
            base + RandomInteger[{-variation, variation}]/Sqrt[sqrtBase]
          ]
        ],
      
      "fraction", 
        Module[{num, denom},
          num = RandomInteger[{1, 3}];
          denom = RandomChoice[{2, 3, 4}];
          base + num/denom * Sign[RandomReal[]-0.5]
        ]
    ]
  ];
  
  (* Generovanie s opakovan\[YAcute]mi pokusmi pre zabezpe\[CHacek]enie validity *)
  While[attempts < 30,
    attempts++;
    
    (* Rozhodnutie o type \[CHacek]\[IAcute]sel *)
    useSpecialNumbers = RandomReal[] < 0.4;
    
    (* Generovanie z\[AAcute]kladn\[YAcute]ch rozmerov *)
    If[useSpecialNumbers,
      (* Generovanie s odmocninami alebo zlomkami *)
      width = makeSpecialNumber[RandomInteger[{3, 5}], 1];
      height = makeSpecialNumber[RandomInteger[{2, 4}], 1];
      roofHeight = makeSpecialNumber[RandomInteger[{2, 3}], 1];
      xOffset = makeSpecialNumber[RandomInteger[{-2, 2}], 1];
      yOffset = makeSpecialNumber[RandomInteger[{0, 2}], 1];
    ,
      (* Generovanie len s cel\[YAcute]mi \[CHacek]\[IAcute]slami - preferovan\[YAcute] sp\[OHat]sob *)
      width = RandomInteger[{3, 6}];
      height = RandomInteger[{2, 4}];
      roofHeight = RandomInteger[{2, 3}];
      xOffset = RandomInteger[{-2, 2}];
      yOffset = RandomInteger[{0, 2}];
    ];
    
    (* Definovanie vrcholov dom\[CHacek]eka *)
    vertices = {
      {-width, 0},         (* \:013dav\[YAcute] doln\[YAcute] roh *)
      {width, 0},          (* Prav\[YAcute] doln\[YAcute] roh *)
      {width, height},     (* Prav\[YAcute] horn\[YAcute] roh *)
      {-width, height},    (* \:013dav\[YAcute] horn\[YAcute] roh *)
      {0, height + roofHeight} (* Vrchol strechy *)
    };
    
    (* Aplikovanie posunu *)
    vertices = vertices + {xOffset, yOffset};
    
    (* Ak je dom\[CHacek]ek validn\[YAcute], vysko\[CHacek]\[IAcute]me z cyklu *)
    If[validDomcekQ[vertices] && Max[Abs[Flatten[vertices]]] <= 10, Break[]];
  ];
  
  (* Ak sa nepodarilo vygenerova\[THacek] validn\[YAcute] dom\[CHacek]ek, vr\[AAcute]time jeden z predpripraven\[YAcute]ch *)
  If[!validDomcekQ[vertices] || attempts >= 30,
    simpleDomceky = {
      {{-3, 0}, {3, 0}, {3, 4}, {-3, 4}, {0, 7}},
      {{-4, 1}, {4, 1}, {4, 4}, {-4, 4}, {0, 7}},
      {{-3, -1}, {3, -1}, {3, 2}, {-3, 2}, {0, 5}},
      {{-3, 1}, {3, 1}, {3, 3}, {-3, 3}, {0, 6}},
      {{-5, 0}, {5, 0}, {5, 3}, {-5, 3}, {0, 6}},
      {{-2, 0}, {2, 0}, {2, 4}, {-2, 4}, {0, 6}},
      (* Dom\[CHacek]eky so \[SHacek]peci\[AAcute]lnymi hodnotami *)
      {{-2, 0}, {2, 0}, {2, 3}, {-2, 3}, {0, 3 + Sqrt[5]}},
      {{-5/2, 1/2}, {5/2, 1/2}, {5/2, 7/2}, {-5/2, 7/2}, {0, 6}},
      {{-3, 0}, {3, 0}, {3, Sqrt[7]}, {-3, Sqrt[7]}, {0, 2*Sqrt[7]}},
      {{-8/3, 1}, {8/3, 1}, {8/3, 3}, {-8/3, 3}, {0, 11/2}}
    };
    vertices = RandomChoice[simpleDomceky];
  ];
  
  (* Vr\[AAcute]time vygenerovan\[YAcute] dom\[CHacek]ek *)
  vertices
];

(* Funkcia na form\[AAcute]tovan\[EAcute] zobrazenie vlastnost\[IAcute] dom\[CHacek]eka s o\[SHacek]etren\[IAcute]m ch\[YAcute]b *)
DisplayDomcekProperties[vertices_, style_] := Module[{area, perimeter},
    (* V\[YAcute]po\[CHacek]et s o\[SHacek]etren\[IAcute]m pre pr\[IAcute]pad chyby *)
    area = Check[Round[domcekArea[vertices]], 25];
    perimeter = Check[Round[domcekPerimeter[vertices]], 30];
    
    (* O\[SHacek]etrenie pre pr\[IAcute]pad neplatn\[YAcute]ch hodn\[OHat]t *)
    If[!NumberQ[area] || area === ComplexInfinity || area === Indeterminate, area = 25];
    If[!NumberQ[perimeter] || perimeter === ComplexInfinity || perimeter === Indeterminate, perimeter = 30];
    
    Print[Style["Vlastnosti dom\[CHacek]eka:", Bold]];
    Print["Obsah: ", Style[area, style]];
    Print["Obvod: ", Style[perimeter, style]];
];

(* Pokro\[CHacek]il\[EAcute] form\[AAcute]tovanie v\[YAcute]razov pre lep\[SHacek]ie zobrazenie dom\[CHacek]ekov *)
FormatDomcekExpression[expr_] :=
    Module[{expandedExpr, simplifiedExpr},
        (* Rozvinutie v\[YAcute]razu *)
        expandedExpr = Expand[expr];
        
        (* Z\[AAcute]kladn\[EAcute] zjednodu\[SHacek]enie *)
        simplifiedExpr = Simplify[expandedExpr];
        
        (* Vr\[AAcute]ti\[THacek] v\[YAcute]sledok v preferovanej forme *)
        simplifiedExpr
    ];

(* Funkcia na spracovanie jedn\[EAcute]ho vrcholu dom\[CHacek]eka pre lep\[SHacek]ie zobrazenie *)
ProcessDomcekVertex[vertex_] := 
    Module[{},
        (* Aplikovanie spracovania na ka\[ZHacek]d\[UAcute] s\[UAcute]radnicu *)
        Map[FormatDomcekExpression, vertex]
    ];

(* Komplexnej\[SHacek]ia funkcia pre spracovanie vnoren\[YAcute]ch v\[YAcute]razov *)
ExpandNestedExpressions[expr_] := 
    Module[{result = expr},
        (* Detekcia a spracovanie v\[YAcute]razov ako 1/4 (-7 - 2*Sqrt[5]) *)
        If[Head[expr] === Times && Length[expr] >= 2 && 
           (MatchQ[expr[[1]], _Rational] || MatchQ[expr[[1]], _Integer]) &&
           MatchQ[expr[[2]], _Plus],
           (* Rozpis zlomku do s\[UAcute]\[CHacek]tu/rozdielu *)
           result = Expand[expr];
        ];
        
        (* Spracovanie vnoren\[YAcute]ch prvkov *)
        If[Head[result] === Plus, 
            result = Plus @@ Map[ExpandNestedExpressions, List @@ result],
            If[Head[result] === Times,
                result = Times @@ Map[ExpandNestedExpressions, List @@ result]
            ]
        ];
        
        result
    ];

(* Funkcia na pln\[UAcute] \[UAcute]pravu v\[YAcute]razu pre zobrazenie *)
FullExpressionProcessor[expr_] :=
    Module[{step1, step2},
        (* Krok 1: Expandova\[THacek] v\[YAcute]raz *)
        step1 = Expand[expr];
        
        (* Krok 2: Spracova\[THacek] vnoren\[EAcute] v\[YAcute]razy *)
        step2 = ExpandNestedExpressions[step1];
        
        (* Vr\[AAcute]ti\[THacek] v\[YAcute]sledok *)
        step2
    ];

(* Funkcia na spracovanie v\[SHacek]etk\[YAcute]ch vrcholov dom\[CHacek]eka s pln\[YAcute]m spracovan\[IAcute]m v\[YAcute]razov *)
ProcessDomcekVerticesComplete[vertices_] :=
    Map[Function[vertex, Map[FullExpressionProcessor, vertex]], vertices];

(* Vylep\[SHacek]en\[AAcute] funkcia na zobrazenie postupn\[EAcute]ho v\[YAcute]po\[CHacek]tu transform\[AAcute]ci\[IAcute] - PRE JEDNU TRANSFORM\[CapitalAAcute]CIU *)
DisplayTransformationSequence[pociatocny_, finalny_, transformacia_] := Module[{},
    Print[Style["\nPOSTUPN\[CapitalYAcute] V\[CapitalYAcute]PO\[CapitalCHacek]ET:", Bold, 16]];
    
    (* Vlastnosti p\[OHat]vodn\[EAcute]ho dom\[CHacek]eka *)
    Print[Style["\nP\[OHat]vodn\[YAcute] dom\[CHacek]ek:", Bold, 14]];
    Print["Vrcholy:"];
    Print[MatrixForm[Map[Style[#, Blue] &, pociatocny, {2}]]];
    DisplayDomcekProperties[pociatocny, Blue];
    
    (* Vlastnosti po transform\[AAcute]cii - S MAXIM\[CapitalAAcute]LNE VYLEP\[CapitalSHacek]EN\[CapitalYAcute]M ZOBRAZEN\[CapitalIAcute]M *)
    Print[Style["\nPo transform\[AAcute]cii (" <> transformacia <> "):", Bold, 14]];
    Print["Vrcholy dom\[CHacek]eka po transform\[AAcute]cii:"];
    
    (* Spracovanie vrcholov pre lep\[SHacek]ie zobrazenie *)
    Module[{processedVertices},
        processedVertices = ProcessDomcekVerticesComplete[finalny];
        (* Zobrazenie upraven\[YAcute]ch v\[YAcute]razov *)
        Print[MatrixForm[Map[Style[#, mildGreen] &, processedVertices, {2}]]];
    ];
    
    DisplayDomcekProperties[finalny, mildGreen];
];

(* Funkcia pre vizualiz\[AAcute]ciu jednej transform\[AAcute]cie *)
CreateDomcekVisualization[pociatocny_, finalny_, transformacia_] := 
    Module[{
        allVertices, xMin, xMax, yMin, yMax, 
        xRange, yRange, padding = 1,
        brightBlue = RGBColor[0, 0.4, 0.8],
        brightGreen = RGBColor[0.2, 0.7, 0.3],
        lightGray = RGBColor[0.9, 0.9, 0.9],
        labelPositions,
        maxRange, xMid, yMid,
        labels1 = {"A", "B", "C", "D", "E"},
        labels2 = {"A'", "B'", "C'", "D'", "E'"},
        originalHouseLines, finalHouseLines
    },
        
        (* Gather all vertices to determine plot range *)
        allVertices = Join[pociatocny, finalny];
        
        xMin = Min[N[allVertices[[All, 1]]]];
        xMax = Max[N[allVertices[[All, 1]]]];
        yMin = Min[N[allVertices[[All, 2]]]];
        yMax = Max[N[allVertices[[All, 2]]]];
        
        (* Add padding to ensure points don't crowd the edges *)
        xRange = {xMin - padding, xMax + padding};
        yRange = {yMin - padding, yMax + padding};
        
        (* Ensure the plot is square for better visualization *)
        maxRange = Max[xRange[[2]] - xRange[[1]], yRange[[2]] - yRange[[1]]];
        xMid = Mean[xRange];
        yMid = Mean[yRange];
        
        xRange = {xMid - maxRange/2, xMid + maxRange/2};
        yRange = {yMid - maxRange/2, yMid + maxRange/2};
        
        (* Define house lines for both houses *)
        (* Modified vertices meaning: 
           A(1)=left bottom corner
           B(2)=right bottom corner
           C(3)=right top corner
           D(4)=left top corner
           E(5)=roof peak
        *)
        
        (* Original house (blue) *)
        originalHouseLines = {
            {pociatocny[[1]], pociatocny[[2]]},  (* A-B: bottom line *)
            {pociatocny[[2]], pociatocny[[3]]},  (* B-C: right side *)
            {pociatocny[[3]], pociatocny[[4]]},  (* C-D: top line *)
            {pociatocny[[4]], pociatocny[[1]]},  (* D-A: left side *)
            {pociatocny[[3]], pociatocny[[5]]},  (* C-E: right roof *)
            {pociatocny[[4]], pociatocny[[5]]}   (* D-E: left roof *)
        };
        
        (* Final house (green) *)
        finalHouseLines = {
            {finalny[[1]], finalny[[2]]},  (* A'-B': bottom line *)
            {finalny[[2]], finalny[[3]]},  (* B'-C': right side *)
            {finalny[[3]], finalny[[4]]},  (* C'-D': top line *)
            {finalny[[4]], finalny[[1]]},  (* D'-A': left side *)
            {finalny[[3]], finalny[[5]]},  (* C'-E': right roof *)
            {finalny[[4]], finalny[[5]]}   (* D'-E': left roof *)
        };
        
        (* Create improved label positions with balanced distance from vertices *)
        labelPositions = Table[
            Module[{
                vertex = Which[
                    i <= 5, pociatocny[[i]],
                    True, finalny[[i - 5]]
                ],
                (* Better balanced offset - not too close, not too far *)
                offset = {0.35, 0.35},
                nearby = {}
            },
                
                (* Find nearby points to avoid overlaps *)
                nearby = DeleteCases[allVertices, vertex];
                nearby = Select[nearby, EuclideanDistance[#, vertex] < 0.7 &]; 
                
                If[Length[nearby] > 0,
                    (* Calculate a balanced offset based on nearby points *)
                    offset = Mean[Table[
                        Normalize[vertex - point] * 0.4,
                        {point, nearby}
                    ]];
                    
                    (* Adjust for single nearby point *)
                    If[Length[nearby] == 1, offset = offset * 1.3];
                ,
                    (* Default offsets - balanced distance *)
                    If[N[vertex[[1]]] > 0, offset[[1]] = 0.35, offset[[1]] = -0.35];
                    If[N[vertex[[2]]] > 0, offset[[2]] = 0.35, offset[[2]] = -0.35];
                ];
                
                (* Scale the offset based on distance from origin *)
                offset = offset * (0.8 + 0.07 * EuclideanDistance[vertex, {0, 0}]);
                
                vertex + offset
            ],
            {i, 10}  (* 5 vertices per domcek, 2 domceky *)
        ];
        
        Graphics[{
            (* Grid lines *)
            lightGray, Thin,
            Table[Line[{{xRange[[1]], y}, {xRange[[2]], y}}], 
                 {y,Ceiling[yRange[[1]]], Floor[yRange[[2]]]}],
            Table[Line[{{x, yRange[[1]]}, {x, yRange[[2]]}}], 
                  {x, Ceiling[xRange[[1]]], Floor[xRange[[2]]]}],
            
            (* Axes *)
            Black, Thickness[0.003], Arrowheads[0.02],
            Arrow[{{xRange[[1]], 0}, {xRange[[2]], 0}}],
            Arrow[{{0, yRange[[1]]}, {0, yRange[[2]]}}],
            Text[Style["x", Bold, 14], {xRange[[2]] - 0.3, -0.3}],
            Text[Style["y", Bold, 14], {-0.3, yRange[[2]] - 0.3}],
            
            (* Tick marks *)
            Table[
                {
                    Line[{{i, -0.1}, {i, 0.1}}],
                    Text[Style[i, 10], {i, -0.3}]
                },
                {i, Ceiling[xRange[[1]]], Floor[xRange[[2]]]}
            ],
            Table[
                {
                    Line[{{-0.1, i}, {0.1, i}}],
                    Text[Style[i, 10], {-0.3, i}]
                },
                {i, Ceiling[yRange[[1]]], Floor[yRange[[2]]]}
            ],
            
            (* Transformation paths - dashed lines between corresponding vertices *)
            {Opacity[0.3], Dashed, Thickness[0.002], brightGreen,
                Table[
                    Line[{pociatocny[[i]], finalny[[i]]}],
                    {i, 5}
                ]
            },
            
            (* Original domcek - blue - proper house shape *)
            {brightBlue, Thickness[0.005], Opacity[0.9], 
                Line /@ originalHouseLines
            },
            
            (* Final domcek - green - proper house shape *)
            {brightGreen, Thickness[0.005], Opacity[0.9], 
                Line /@ finalHouseLines
            },
            
            (* Vertex points - original domcek *)
            Table[{
                White, Disk[pociatocny[[i]], 0.20],
                brightBlue, Disk[pociatocny[[i]], 0.15]
            }, {i, 5}],
            
            (* Vertex points - final domcek *)
            Table[{
                White, Disk[finalny[[i]], 0.20],
                brightGreen, Disk[finalny[[i]], 0.15]
            }, {i, 5}],
            
            (* Vertex labels with white background circles *)
            Table[
                With[{
                    label = Which[
                        i <= 5, labels1[[i]],      (* Original domcek - A to E *)
                        True, labels2[[i - 5]]     (* Final domcek - A' to E' *)
                    ],
                    color = Which[
                        i <= 5, brightBlue,
                        True, brightGreen
                    ],
                    pos = labelPositions[[i]]
                },
                    {
                        (* White background circle for label *)
                        White, Disk[pos, 0.2],
                        (* Label with colored text *)
                        color, 
                        Text[Style[label, Bold, 12], pos]
                    }
                ],
                {i, 10}  (* 5 vertices per domcek, 2 domceky *)
            ]
        },
        PlotRange -> {xRange, yRange},
        AspectRatio -> 1,  (* This ensures equal scaling for x and y axes *)
        ImageSize -> 650,
        PlotLabel -> Style["Aplik\[AAcute]cia transform\[AAcute]cie dom\[CHacek]eka", Bold, 16],
        ImagePadding -> {{40, 40}, {40, 40}},
        Background -> White,
        Method -> {"ShrinkWrap" -> True}]
    ];

(* Funkcia na v\[YAcute]ber transform\[AAcute]cie s popisom *)
SelectTransformation[message_] := 
    Module[{options, fullOptions, formattedChoices, result},
        options = {
            "Posun" -> {"Posun", "Posun dom\[CHacek]eka vo smere vektora"},
            "Rot\[AAcute]cia" -> {"Rot\[AAcute]cia", "Rot\[AAcute]cia dom\[CHacek]eka okolo po\[CHacek]iatku s\[UAcute]radnicovej s\[UAcute]stavy"},
            "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie" -> {"Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie alebo zmen\[SHacek]enie dom\[CHacek]eka v r\[OHat]znych smeroch"},
            "Skosenie" -> {"Skosenie", "Skosenie dom\[CHacek]eka v smere os\[IAcute]"},
            "Symetria" -> {"Symetria", "Zrkadlenie dom\[CHacek]eka pod\:013ea zvolenej osi"}
        };
        
        (* Pou\[ZHacek]ijeme jednoduch\[SHacek]\[IAcute] pr\[IAcute]stup s ChoiceDialog, ale s vertik\[AAcute]lnym zoznamom *)
        ChoiceDialog[
            message,
            options,
            WindowTitle -> "V\[YAcute]ber transform\[AAcute]cie",
            WindowSize -> {500, All}
        ]
    ];

(* Hlavn\[AAcute] funkcia pre jednoduch\[UAcute] transform\[AAcute]ciu s vylep\[SHacek]en\[YAcute]m pou\[ZHacek]\[IAcute]vate\:013esk\[YAcute]m rozhran\[IAcute]m a o\[SHacek]etren\[IAcute]m ch\[YAcute]b *)
DomcekJednoduchaTransformacia[] := Module[{
    pociatocnyDomcek,
    transformacia,
    finalnyDomcek,
    transformaciaPopis
    },
    
    (* Potla\[CHacek]enie chybov\[YAcute]ch spr\[AAcute]v *)
    Quiet[
    (* \[CapitalUAcute]vodn\[AAcute] spr\[AAcute]va *)
    Print[Style["JEDNODUCH\[CapitalAAcute] GEOMETRICK\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA DOM\[CapitalCHacek]EKA", Bold, 24]];
    Print[Style["==========================================", Bold]];
    
    (* Generovanie a zobrazenie po\[CHacek]iato\[CHacek]n\[EAcute]ho dom\[CHacek]eka *)
    pociatocnyDomcek = DomcekGeneruj[];
    Print[Style["\nPO\[CapitalCHacek]IATO\[CapitalCHacek]N\[CapitalYAcute] DOM\[CapitalCHacek]EK:", Bold, 16]];
    Print["Vrcholy dom\[CHacek]eka:"];
    Print[MatrixForm[Map[Style[#, Blue] &, pociatocnyDomcek, {2}]]];
    DisplayDomcekProperties[pociatocnyDomcek, Blue];
    
    (* V\[YAcute]ber a aplik\[AAcute]cia transform\[AAcute]cie *)
    transformaciaPopis = SelectTransformation["Vyberte transform\[AAcute]ciu:"];
    
    If[transformaciaPopis === $Canceled, Return[]];
    transformacia = First[transformaciaPopis];
    
    Print[Style["\nTRANSFORM\[CapitalAAcute]CIA: " <> transformacia, Bold, 16]];
    
    finalnyDomcek = Switch[transformacia,
        "Posun", DomcekPosun[pociatocnyDomcek],
        "Rot\[AAcute]cia", DomcekRotacia[pociatocnyDomcek],
        "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", DomcekZvacsenieZmensenie[pociatocnyDomcek],
        "Skosenie", DomcekSkosenie[pociatocnyDomcek],
        "Symetria", DomcekSymetria[pociatocnyDomcek]
    ];
    
    (* Zobrazenie v\[YAcute]po\[CHacek]tu a v\[YAcute]sledkov *)
    DisplayTransformationSequence[
        pociatocnyDomcek,
        finalnyDomcek,
        transformacia
    ];
    
    (* Zobrazenie s\[UAcute]hrnnej vizualiz\[AAcute]cie a presunutej legendy *)
    Print[Style["\nS\[CapitalUAcute]HRNN\[CapitalAAcute] VIZUALIZ\[CapitalAAcute]CIA:", Bold, 16]];
    Print[CreateDomcekVisualization[
        pociatocnyDomcek,
        finalnyDomcek,
        transformacia
    ]];
    
    Print[Style["\nLEGENDA:", Bold, 16]];
    Print[Style["\[Bullet] Modr\[EAcute]", RGBColor[0, 0.4, 0.8], Bold], " body a \[CHacek]iary: P\[OHat]vodn\[YAcute] dom\[CHacek]ek"];
    Print[Style["\[Bullet] Zelen\[EAcute]", RGBColor[0.2, 0.7, 0.3], Bold], " body a \[CHacek]iary: Dom\[CHacek]ek po transform\[AAcute]cii (", 
          Style[transformacia, Bold], ")"];
    
    (* Inform\[AAcute]cia o transform\[AAcute]cii *)
    Print[Style["\nVykonan\[AAcute] transform\[AAcute]cia:", Bold, 16]];
    Print[transformacia];
    
    (* Vr\[AAcute]ti\[THacek] inform\[AAcute]cie o transform\[AAcute]cii pre pou\[ZHacek]itie v in\[YAcute]ch funkci\[AAcute]ch *)
    {pociatocnyDomcek, finalnyDomcek, transformacia}
    ]  (* Ukon\[CHacek]enie Quiet bloku *)
];

(* Funkcia pre s\[UAcute]hrnn\[UAcute] vizualiz\[AAcute]ciu (bez dial\[OAcute]gu) s o\[SHacek]etren\[IAcute]m ch\[YAcute]b *)
DomcekJednoduchaTransformaciaSVysledkom[] := Module[{result},
   (* Pou\[ZHacek]itie Quiet na potla\[CHacek]enie chybov\[YAcute]ch spr\[AAcute]v *)
   result = Quiet[DomcekJednoduchaTransformacia[], {Power::infy, Infinity::indet}];
   
   (* Ak funkcia nebola zru\[SHacek]en\[AAcute] *)
   If[result =!= Null,
       Check[
           Print[Style["\nZ\[CapitalAAcute]VERE\[CapitalCHacek]N\[CapitalYAcute] PREH\:013dAD:", Bold, 16]];
           Print["Vykonali ste transform\[AAcute]ciu:"];
           Print[result[[3]]];
           Print["\nV\[YAcute]sledn\[YAcute] dom\[CHacek]ek:"];
           Print[MatrixForm[result[[2]]]],
           (* Z\[AAcute]chytn\[YAcute] blok pre pr\[IAcute]pad chyby pri zobrazovan\[IAcute] *)
           Print[Style["Nastala chyba pri zobrazovan\[IAcute] v\[YAcute]sledkov.", Red, Bold]]
       ]
   ];
   (* Vr\[AAcute]ti\[THacek] Null aby sa nezobrazil Out[] *)
   Null
];

(* Na konci package obnovi\[THacek] norm\[AAcute]lne spr\[AAcute]vanie varovn\[YAcute]ch spr\[AAcute]v *)
On[Power::infy];
On[Infinity::indet];

End[]; (* Uzavretie Private kontextu *)
EndPackage[]; (* Uzavretie DomcekEasyBalik package *)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)
