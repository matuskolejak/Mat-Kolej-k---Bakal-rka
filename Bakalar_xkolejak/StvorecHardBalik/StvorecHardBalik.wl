(* ::Package:: *)

(* ::Package:: *)
(**)


BeginPackage["StvorecHardBalik`"];

(* Export verejn\[YAcute]ch symbolov *)
StvorecTrojitaTransformacia::usage = 
    "StvorecTrojitaTransformacia[] umo\[ZHacek]n\[IAcute] postupn\[YAcute] v\[YAcute]ber a aplik\[AAcute]ciu troch transform\[AAcute]ci\[IAcute].";

StvorecTrojitaTransformaciaSVysledkom::usage = 
    "StvorecTrojitaTransformaciaSVysledkom[] umo\[ZHacek]n\[IAcute] postupn\[YAcute] v\[YAcute]ber a aplik\[AAcute]ciu troch transform\[AAcute]ci\[IAcute] a zobraz\[IAcute] s\[UAcute]hrnn\[YAcute] v\[YAcute]sledok.";

StvorecGeneruj::usage = 
    "StvorecGeneruj[] generuje n\[AAcute]hodn\[YAcute] \[SHacek]tvorec s vhodn\[YAcute]mi vlastnos\[THacek]ami.";

(* Spolo\[CHacek]n\[EAcute] funkcie pre v\[SHacek]etky transform\[AAcute]cie *)
squareArea::usage = 
    "squareArea[p1, p2, p3, p4] vypo\[CHacek]\[IAcute]ta obsah \[SHacek]tvorca s vrcholmi p1, p2, p3 a p4.";

validSquareQ::usage = 
    "validSquareQ[p1, p2, p3, p4] over\[IAcute], \[CHacek]i \[SHacek]tvorec sp\:013a\[NHacek]a krit\[EAcute]ri\[AAcute].";

squarePerimeter::usage = 
    "squarePerimeter[p1, p2, p3, p4] vypo\[CHacek]\[IAcute]ta obvod \[SHacek]tvorca s vrcholmi p1, p2, p3 a p4.";

(* Defin\[IAcute]cia chybov\[YAcute]ch spr\[AAcute]v *)
StvorecTrojitaTransformacia::infinityerr = 
    "Vyskytla sa chyba: v\[YAcute]po\[CHacek]et viedol k nekone\[CHacek]n\[EAcute]mu alebo neur\[CHacek]it\[EAcute]mu v\[YAcute]sledku. Pou\[ZHacek]ije sa preddefinovan\[YAcute] \[SHacek]tvorec.";

Begin["`Private`"];

(* Potla\[CHacek]enie varovn\[YAcute]ch spr\[AAcute]v o nekone\[CHacek]n\[YAcute]ch v\[YAcute]sledkoch *)
Off[Power::infy];
Off[Infinity::indet];

(* Na\[CHacek]\[IAcute]tanie v\[SHacek]etk\[YAcute]ch transforma\[CHacek]n\[YAcute]ch modulov na za\[CHacek]iatku *)
Needs["StvorecHardBalik`Transforms`Posun`"];
Needs["StvorecHardBalik`Transforms`Rotacia`"];
Needs["StvorecHardBalik`Transforms`ZvacsenieZmensenie`"];
Needs["StvorecHardBalik`Transforms`Skosenie`"];
Needs["StvorecHardBalik`Transforms`Symetria`"];

(* Defin\[IAcute]cia miernej\[SHacek]ej farby zelenej *)
mildGreen = RGBColor[0.2, 0.6, 0.2];

(* Funkcia na v\[YAcute]po\[CHacek]et obsahu \[SHacek]tvorca s kontrolou ch\[YAcute]b *)
squareArea[p1_, p2_, p3_, p4_] := Module[{area},
    area = Check[
        EuclideanDistance[p1, p2] * EuclideanDistance[p2, p3],
        $Failed
    ];
    
    If[area === $Failed || area === ComplexInfinity || area === Indeterminate || !NumberQ[area],
        (* Vr\[AAcute]ti\[THacek] predvolen\[UAcute] hodnotu v pr\[IAcute]pade chyby *)
        16,
        (* Inak vr\[AAcute]ti\[THacek] skuto\[CHacek]n\[YAcute] obsah *)
        area
    ]
];

(* Funkcia na v\[YAcute]po\[CHacek]et obvodu \[SHacek]tvorca s kontrolou ch\[YAcute]b *)
squarePerimeter[p1_, p2_, p3_, p4_] := Module[{dist1, dist2, dist3, dist4},
    dist1 = Check[EuclideanDistance[p1, p2], $Failed];
    dist2 = Check[EuclideanDistance[p2, p3], $Failed];
    dist3 = Check[EuclideanDistance[p3, p4], $Failed];
    dist4 = Check[EuclideanDistance[p4, p1], $Failed];
    
    If[MemberQ[{dist1, dist2, dist3, dist4}, $Failed],
        (* Vr\[AAcute]ti\[THacek] predvolen\[UAcute] hodnotu v pr\[IAcute]pade chyby *)
        16,
        (* Inak vr\[AAcute]ti\[THacek] skuto\[CHacek]n\[YAcute] obvod *)
        dist1 + dist2 + dist3 + dist4
    ]
];

(* Funkcia na v\[YAcute]po\[CHacek]et d\:013a\[ZHacek]ok str\[AAcute]n \[SHacek]tvorca s kontrolou ch\[YAcute]b *)
squareSides[p1_, p2_, p3_, p4_] := Module[{sides},
    sides = Check[{
        EuclideanDistance[p1, p2],
        EuclideanDistance[p2, p3],
        EuclideanDistance[p3, p4],
        EuclideanDistance[p4, p1]
    }, $Failed];
    
    If[sides === $Failed,
        (* Vr\[AAcute]ti\[THacek] predvolen\[EAcute] hodnoty v pr\[IAcute]pade chyby *)
        {4, 4, 4, 4},
        (* Inak vr\[AAcute]ti\[THacek] skuto\[CHacek]n\[EAcute] d\:013a\[ZHacek]ky *)
        sides
    ]
];

(* Funkcia na v\[YAcute]po\[CHacek]et uhlov \[SHacek]tvorca v stup\[NHacek]och s kontrolou ch\[YAcute]b *)
squareAngles[p1_, p2_, p3_, p4_] := Module[{v1, v2, v3, v4, v5, v6, v7, v8, angles},
    v1 = Normalize[p2 - p1];
    v2 = Normalize[p4 - p1];
    v3 = Normalize[p3 - p2];
    v4 = Normalize[p1 - p2];
    v5 = Normalize[p4 - p3];
    v6 = Normalize[p2 - p3];
    v7 = Normalize[p1 - p4];
    v8 = Normalize[p3 - p4];
    
    (* V\[YAcute]po\[CHacek]et uhlov s kontrolou ch\[YAcute]b *)
    angles = Check[
        {
            ArcCos[Clip[v1 . v2, {-1, 1}]]/Degree,
            ArcCos[Clip[v3 . v4, {-1, 1}]]/Degree,
            ArcCos[Clip[v5 . v6, {-1, 1}]]/Degree,
            ArcCos[Clip[v7 . v8, {-1, 1}]]/Degree
        },
        $Failed
    ];
    
    If[angles === $Failed || MemberQ[angles, ComplexInfinity] || MemberQ[angles, Indeterminate],
        (* Vr\[AAcute]ti\[THacek] predvolen\[EAcute] hodnoty v pr\[IAcute]pade chyby *)
        {90, 90, 90, 90},
        (* Inak vr\[AAcute]ti\[THacek] skuto\[CHacek]n\[EAcute] uhly *)
        angles
    ]
];

(* Funkcia na kontrolu \[CHacek]i s\[UAcute] body usporiadan\[EAcute] v smere hodinov\[YAcute]ch ru\[CHacek]i\[CHacek]iek *)
clockwiseOrderQ[p1_, p2_, p3_, p4_] := Module[{center, angles},
    center = Mean[{p1, p2, p3, p4}];
    angles = ArcTan[#[[1]] - center[[1]], #[[2]] - center[[2]]] & /@ {p1, p2, p3, p4};
    
    (* Kontrola, \[CHacek]i s\[UAcute] uhly v spr\[AAcute]vnom porad\[IAcute] *)
    And @@ Table[
        angles[[i]] <= angles[[Mod[i, 4] + 1]] || 
        (angles[[i]] > 3 && angles[[Mod[i, 4] + 1]] < 0),
        {i, 1, 4}
    ]
];

(* Funkcia na valid\[AAcute]ciu \[SHacek]tvorca s o\[SHacek]etren\[IAcute]m ch\[YAcute]b *)
validSquareQ[p1_, p2_, p3_, p4_] := 
    Module[{sides, angles, area, minSide = 3, minArea = 9, maxArea = 100, isValid, diag1, diag2}, 
        (* Kontrola \[CHacek]i s\[UAcute] vstupn\[EAcute] body validn\[EAcute] *)
        If[!VectorQ[p1, NumberQ] || !VectorQ[p2, NumberQ] || !VectorQ[p3, NumberQ] || !VectorQ[p4, NumberQ] || 
           Length[p1] != 2 || Length[p2] != 2 || Length[p3] != 2 || Length[p4] != 2,
            Return[False]
        ];
        
        (* Kontrola konvexnosti \[SHacek]tvorca *)
        area = squareArea[p1, p2, p3, p4];
        If[area < 0.1, Return[False]];
        
        (* Bezpe\[CHacek]n\[YAcute] v\[YAcute]po\[CHacek]et str\[AAcute]n a uhlov *)
        sides = squareSides[p1, p2, p3, p4];
        If[!VectorQ[sides, NumberQ], Return[False]];
        
        (* V\[YAcute]po\[CHacek]et uhlov s kontrolou ch\[YAcute]b *)
        angles = Check[squareAngles[p1, p2, p3, p4] * Degree, {$Failed}];
        If[MemberQ[angles, $Failed], Return[False]];
        
        (* Zachytenie Indeterminate alebo ComplexInfinity v\[YAcute]sledkov *)
        If[MemberQ[angles, ComplexInfinity] || MemberQ[angles, Indeterminate] || 
           area === ComplexInfinity || area === Indeterminate, 
            Return[False]
        ];
        
        (* Kontrola \[CHacek]i s\[UAcute] diagon\[AAcute]ly \[SHacek]tvorca rovnak\[EAcute] a kolm\[EAcute] na seba *)
        diag1 = p3 - p1;
        diag2 = p4 - p2;
        
        (* Hlavn\[AAcute] valid\[AAcute]cia \[SHacek]tvorca s kontrolou ch\[YAcute]b *)
        isValid = Check[
            (* Kontrola rovnosti str\[AAcute]n - pribli\[ZHacek]ne rovnak\[EAcute] *)
            And @@ Table[
                Abs[sides[[i]] - sides[[Mod[i, 4] + 1]]] < 0.1,
                {i, 1, 4}
            ] &&
            (* Kontrola uhlov - pribli\[ZHacek]ne 90 stup\[NHacek]ov *)
            And @@ Table[
                Abs[angles[[i]] - 90] < 5,
                {i, 1, 4}
            ] &&
            (* Kontrola ve\:013ekosti plochy *)
            minArea <= area <= maxArea && 
            (* Kontrola minim\[AAcute]lnej d\:013a\[ZHacek]ky strany *)
            Min[sides] >= minSide &&
            (* Kontrola diagon\[AAcute]l - pribli\[ZHacek]ne rovnako dlh\[EAcute] *)
            Abs[Norm[diag1] - Norm[diag2]] < 0.1 &&
            (* Kontrola kolmosti diagon\[AAcute]l *)
            Abs[diag1 . diag2] < 0.1 * Norm[diag1] * Norm[diag2],
            False
        ];
        
        isValid
    ];


(* Improved GenerateInitialSquare function *)
GenerateInitialSquare[] := Module[{
    p1, p2, p3, p4, 
    count = 0, 
    defaultSquare = {{-2, -2}, {2, -2}, {2, 2}, {-2, 2}}, 
    result, previousSquares = {}, randomSeed,
    generationType, size, center, rotation, xRange, yRange
  },
  
  (* Improve randomness by using multiple sources *)
  randomSeed = Hash[{AbsoluteTime[], $TimeZone, $ProcessID, 
                     RandomInteger[{-10^7, 10^7}], StringJoin @@ 
                     ToString /@ RandomInteger[{0, 9}, 20]}];
  SeedRandom[randomSeed];
  
  (* Shuffle the random generator more thoroughly *)
  Do[RandomInteger[{-1000, 1000}], {RandomInteger[{50, 100}]}];
  
  (* Expanded ranges for more variety *)
  xRange = {-8, 8};
  yRange = {-8, 8};
  
  (* Choose generation type - with better balance *)
  generationType = RandomChoice[{0.7, 0.3} -> {1, 2}];
  
  result = Check[
    Block[{$MessagePreprint = (Message[StvorecTrojitaTransformacia::infinityerr]; #)&},
      While[True,
        Switch[generationType,
          (* Integer coordinates - PRIMARY METHOD *)
          1, 
          Module[{},
            (* Generate a square with more variability *)
            center = {RandomInteger[xRange], RandomInteger[yRange]};
            size = RandomInteger[{2, 6}]; (* Larger range of sizes *)
            rotation = RandomReal[{0, 2*Pi}]; (* Allow any rotation *)
            
            (* Create the square *)
            p1 = center + RotationMatrix[rotation] . {-size, -size};
            p2 = center + RotationMatrix[rotation] . {size, -size};
            p3 = center + RotationMatrix[rotation] . {size, size};
            p4 = center + RotationMatrix[rotation] . {-size, size};
            
            (* Round to integers *)
            p1 = Round[p1];
            p2 = Round[p2];
            p3 = Round[p3];
            p4 = Round[p4];
            
            (* Ensure we have a proper square after rounding *)
            If[!validSquareQ[p1, p2, p3, p4],
              (* If not a valid square after rounding, try a different approach *)
              (* Start with a random point *)
              p1 = {RandomInteger[xRange], RandomInteger[yRange]};
              
              (* Choose a random side length *)
              size = RandomInteger[{2, 6}];
              
              (* Pick a random angle in multiples of \[Pi]/4 for cleaner squares *)
              rotation = RandomChoice[Range[0, 7]] * (Pi/4);
              
              (* Create remaining points with exact integer coordinates *)
              p2 = p1 + Round[RotationMatrix[rotation] . {size, 0}];
              p3 = p2 + Round[RotationMatrix[rotation] . {0, size}];
              p4 = p1 + Round[RotationMatrix[rotation] . {0, size}];
            ];
          ],
          
          (* Fractional coordinates - for more variety *)
          2,
          Module[{},
            center = {RandomInteger[xRange], RandomInteger[yRange]};
            size = RandomChoice[{2, 5/2, 3, 7/2, 4, 9/2, 5}]; (* More size options *)
            rotation = RandomReal[{0, 2*Pi}]; (* Any rotation *)
            
            (* Create square *)
            p1 = center + RotationMatrix[rotation] . {-size, -size};
            p2 = center + RotationMatrix[rotation] . {size, -size};
            p3 = center + RotationMatrix[rotation] . {size, size};
            p4 = center + RotationMatrix[rotation] . {-size, size};
            
            (* Convert to nice fractions *)
            p1 = Rationalize[p1, 0.1];
            p2 = Rationalize[p2, 0.1];
            p3 = Rationalize[p3, 0.1];
            p4 = Rationalize[p4, 0.1];
          ]
        ];
        
        (* Check for duplicates *)
        If[ContainsAny[{Sort[{p1, p2, p3, p4}]}, previousSquares],
          count++;
          (* Try a different generation type after a few attempts *)
          If[Mod[count, 3] == 0, 
            generationType = RandomChoice[{0.7, 0.3} -> {1, 2}]
          ];
          Continue[]
        ];
        
        (* Validate the square *)
        If[validSquareQ[p1, p2, p3, p4] && Max[Abs[Flatten[{p1, p2, p3, p4}]]] <= 15,
          (* Add to history *)
          AppendTo[previousSquares, Sort[{p1, p2, p3, p4}]];
          If[Length[previousSquares] > 20, previousSquares = previousSquares[[-20;;]]];
          
          (* Return result *)
          Return[{p1, p2, p3, p4}]
        ];
        
        (* Change type occasionally *)
        If[Mod[count, 3] == 0 && count > 0, 
          generationType = RandomChoice[{0.7, 0.3} -> {1, 2}]
        ];
        
        (* Fallback mechanism - generate simple squares but with more variety *)
        If[count > 30, 
          Module[{simpleSquares, offsets, sizes, idx},
            (* Generate a variety of different squares *)
            offsets = Table[{RandomInteger[{-5, 5}], RandomInteger[{-5, 5}]}, {10}];
            sizes = RandomInteger[{2, 6}, 10];
            
            simpleSquares = Table[
              Module[{center = offsets[[i]], size = sizes[[i]]},
                {
                  center + {-size, -size},
                  center + {size, -size},
                  center + {size, size},
                  center + {-size, size}
                }
              ],
              {i, 1, 10}
            ];
            
            (* Pick a random one *)
            idx = RandomInteger[{1, Length[simpleSquares]}];
            Return[simpleSquares[[idx]]]
          ]
        ];
        count++
      ]
    ],
    (* Default square - only as a last resort *)
    {{-2, -2}, {2, -2}, {2, 2}, {-2, 2}}
  ];
  
  (* Return result or default square *)
  If[Head[result] === List && Length[result] === 4 && AllTrue[result, VectorQ[#, NumberQ]&], 
    result, 
    {{-2, -2}, {2, -2}, {2, 2}, {-2, 2}}
  ]
];

(* Consider also updating the validSquareQ function to be more flexible *)
(* This allows for slightly more varied squares while still maintaining the square properties *)
validSquareQ[p1_, p2_, p3_, p4_] := 
    Module[{sides, angles, area, minSide = 1, minArea = 4, maxArea = 144, isValid, diag1, diag2}, 
        (* Check if input points are valid *)
        If[!VectorQ[p1, NumberQ] || !VectorQ[p2, NumberQ] || !VectorQ[p3, NumberQ] || !VectorQ[p4, NumberQ] || 
           Length[p1] != 2 || Length[p2] != 2 || Length[p3] != 2 || Length[p4] != 2,
            Return[False]
        ];
        
        (* Check square convexity *)
        area = squareArea[p1, p2, p3, p4];
        If[area < 0.1, Return[False]];
        
        (* Calculate sides and angles safely *)
        sides = squareSides[p1, p2, p3, p4];
        If[!VectorQ[sides, NumberQ], Return[False]];
        
        (* Calculate angles with error handling *)
        angles = Check[squareAngles[p1, p2, p3, p4] * Degree, {$Failed}];
        If[MemberQ[angles, $Failed], Return[False]];
        
        (* Catch Indeterminate or ComplexInfinity results *)
        If[MemberQ[angles, ComplexInfinity] || MemberQ[angles, Indeterminate] || 
           area === ComplexInfinity || area === Indeterminate, 
            Return[False]
        ];
        
        (* Check if diagonals are equal and perpendicular *)
        diag1 = p3 - p1;
        diag2 = p4 - p2;
        
        (* Main square validation with error handling *)
        isValid = Check[
            (* Check sides are approximately equal *)
            And @@ Table[
                Abs[sides[[i]] - sides[[Mod[i, 4] + 1]]] < 0.2 * sides[[i]],
                {i, 1, 4}
            ] &&
            (* Check angles are approximately 90 degrees *)
            And @@ Table[
                Abs[angles[[i]] - 90] < 10,
                {i, 1, 4}
            ] &&
            (* Check area size *)
            minArea <= area <= maxArea && 
            (* Check minimum side length *)
            Min[sides] >= minSide &&
            (* Check diagonals are approximately equal *)
            Abs[Norm[diag1] - Norm[diag2]] < 0.2 * Norm[diag1] &&
            (* Check diagonals are perpendicular *)
            Abs[diag1 . diag2] < 0.2 * Norm[diag1] * Norm[diag2],
            False
        ];
        
        isValid
    ];

(* Verejn\[AAcute] funkcia na generovanie \[SHacek]tvorca *)
StvorecGeneruj[] := GenerateInitialSquare[];

(* Funkcia na form\[AAcute]tovan\[EAcute] zobrazenie vlastnost\[IAcute] \[SHacek]tvorca s o\[SHacek]etren\[IAcute]m ch\[YAcute]b *)
DisplaySquareProperties[vertices_, style_] := Module[{area, perimeter},
    (* V\[YAcute]po\[CHacek]et s o\[SHacek]etren\[IAcute]m pre pr\[IAcute]pad chyby *)
    area = Check[Round[squareArea @@ vertices], 16];
    perimeter = Check[Round[squarePerimeter @@ vertices], 16];
    
    (* O\[SHacek]etrenie pre pr\[IAcute]pad neplatn\[YAcute]ch hodn\[OHat]t *)
    If[!NumberQ[area] || area === ComplexInfinity || area === Indeterminate, area = 16];
    If[!NumberQ[perimeter] || perimeter === ComplexInfinity || perimeter === Indeterminate, perimeter = 16];
    
    Print[Style["Vlastnosti \[SHacek]tvorca:", Bold]];
    Print["Obsah: ", Style[area, style]];
    Print["Obvod: ", Style[perimeter, style]];
];

(* Pokro\[CHacek]il\[EAcute] form\[AAcute]tovanie v\[YAcute]razov pre lep\[SHacek]ie zobrazenie \[SHacek]tvorcov *)
FormatSquareExpression[expr_] :=
    Module[{expandedExpr, simplifiedExpr},
        (* Rozvinutie v\[YAcute]razu *)
        expandedExpr = Expand[expr];
        
        (* Z\[AAcute]kladn\[EAcute] zjednodu\[SHacek]enie *)
        simplifiedExpr = Simplify[expandedExpr];
        
        (* Vr\[AAcute]ti\[THacek] v\[YAcute]sledok v preferovanej forme *)
        simplifiedExpr
    ];

(* Funkcia na spracovanie jedn\[EAcute]ho vrcholu \[SHacek]tvorca pre lep\[SHacek]ie zobrazenie *)
ProcessSquareVertex[vertex_] := 
    Module[{},
        (* Aplikovanie spracovania na ka\[ZHacek]d\[UAcute] s\[UAcute]radnicu *)
        Map[FormatSquareExpression, vertex]
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

(* Funkcia na spracovanie v\[SHacek]etk\[YAcute]ch vrcholov \[SHacek]tvorca s pln\[YAcute]m spracovan\[IAcute]m v\[YAcute]razov *)
ProcessSquareVerticesComplete[vertices_] :=
    Map[Function[vertex, Map[FullExpressionProcessor, vertex]], vertices];

(* Vylep\[SHacek]en\[AAcute] funkcia na zobrazenie postupn\[EAcute]ho v\[YAcute]po\[CHacek]tu transform\[AAcute]ci\[IAcute] *)
DisplayTransformationSequence[pociatocny_, druhy_, treti_, finalny_, prva_, druha_, tretia_] := Module[{},
    Print[Style["\nPOSTUPN\[CapitalYAcute] V\[CapitalYAcute]PO\[CapitalCHacek]ET:", Bold, 16]];
    
    (* Vlastnosti p\[OHat]vodn\[EAcute]ho \[SHacek]tvorca *)
    Print[Style["\nP\[OHat]vodn\[YAcute] \[SHacek]tvorec ABCD:", Bold, 14]];
    Print["Vrcholy:"];
    Print[MatrixForm[Map[Style[#, Blue] &, pociatocny, {2}]]];
    DisplaySquareProperties[pociatocny, Blue];
    
    (* Vlastnosti po prvej transform\[AAcute]cii - S MAXIM\[CapitalAAcute]LNE VYLEP\[CapitalSHacek]EN\[CapitalYAcute]M ZOBRAZEN\[CapitalIAcute]M *)
    Print[Style["\nPo prvej transform\[AAcute]cii (" <> prva <> "):", Bold, 14]];
    Print["Vrcholy \[SHacek]tvorca A'B'C'D':"];
    
    (* Spracovanie vrcholov pre lep\[SHacek]ie zobrazenie *)
    Module[{processedVertices},
        processedVertices = ProcessSquareVerticesComplete[druhy];
        (* Zobrazenie upraven\[YAcute]ch v\[YAcute]razov *)
        Print[MatrixForm[Map[Style[#, mildGreen] &, processedVertices, {2}]]];
    ];
    
    DisplaySquareProperties[druhy, mildGreen];
    
    (* Vlastnosti po druhej transform\[AAcute]cii - S MAXIM\[CapitalAAcute]LNE VYLEP\[CapitalSHacek]EN\[CapitalYAcute]M ZOBRAZEN\[CapitalIAcute]M *)
    Print[Style["\nPo druhej transform\[AAcute]cii (" <> druha <> "):", Bold, 14]];
    Print["Vrcholy \[SHacek]tvorca A''B''C''D'':"];
    
    (* Spracovanie vrcholov pre lep\[SHacek]ie zobrazenie *)
    Module[{processedVertices},
        processedVertices = ProcessSquareVerticesComplete[treti];
        (* Zobrazenie upraven\[YAcute]ch v\[YAcute]razov *)
        Print[MatrixForm[Map[Style[#, Orange] &, processedVertices, {2}]]];
    ];
    
    DisplaySquareProperties[treti, Orange];
    
    (* Vlastnosti po tretej transform\[AAcute]cii - S MAXIM\[CapitalAAcute]LNE VYLEP\[CapitalSHacek]EN\[CapitalYAcute]M ZOBRAZEN\[CapitalIAcute]M *)
    Print[Style["\nPo tretej transform\[AAcute]cii (" <> tretia <> "):", Bold, 14]];
    Print["Vrcholy \[SHacek]tvorca A'''B'''C'''D''':"];
    
    (* Spracovanie vrcholov pre lep\[SHacek]ie zobrazenie *)
    Module[{processedVertices},
        processedVertices = ProcessSquareVerticesComplete[finalny];
        (* Zobrazenie upraven\[YAcute]ch v\[YAcute]razov *)
        Print[MatrixForm[Map[Style[#, Red] &, processedVertices, {2}]]];
    ];
    
    DisplaySquareProperties[finalny, Red];
];

CreateSquareVisualization[pociatocny_, druhy_, treti_, finalny_, prva_, druha_, tretia_] := 
    Module[{
        allVertices, xMin, xMax, yMin, yMax, 
        xRange, yRange, padding = 1,
        brightBlue = RGBColor[0, 0.4, 0.8],
        brightGreen = RGBColor[0.2, 0.7, 0.3],
        brightOrange = RGBColor[1, 0.6, 0],
        brightRed = RGBColor[0.9, 0.1, 0.1],
        lightGray = RGBColor[0.9, 0.9, 0.9],
        labelPositions,
        maxRange, xMid, yMid
    },
        
        allVertices = Join[pociatocny, druhy, treti, finalny];
        
        xMin = Min[N[allVertices[[All, 1]]]];
        xMax = Max[N[allVertices[[All, 1]]]];
        yMin = Min[N[allVertices[[All, 2]]]];
        yMax = Max[N[allVertices[[All, 2]]]];
        
        (* Add padding to ensure points don't crowd the edges *)
        xRange = {xMin - padding, xMax + padding};
        yRange = {yMin - padding, yMax + padding};
        
        maxRange = Max[xRange[[2]] - xRange[[1]], yRange[[2]] - yRange[[1]]];
        xMid = Mean[xRange];
        yMid = Mean[yRange];
        
        xRange = {xMid - maxRange/2, xMid + maxRange/2};
        yRange = {yMid - maxRange/2, yMid + maxRange/2};
        
        (* Create improved label positions with balanced distance from vertices *)
        labelPositions = Table[
            Module[{
                vertex = Which[
                    i <= 4, pociatocny[[i]],
                    i <= 8, druhy[[i - 4]],
                    i <= 12, treti[[i - 8]],
                    True, finalny[[i - 12]]
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
            {i, 16}  (* 4 vertices per square, 4 squares *)
        ];
        
        Graphics[{
            (* Grid lines *)
            lightGray, Thin,
            Table[Line[{{xRange[[1]], y}, {xRange[[2]], y}}], 
                  {y, Ceiling[yRange[[1]]], Floor[yRange[[2]]]}],
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
                    Line[{pociatocny[[i]], druhy[[i]]}],
                    {i, 4}
                ]
            },
            {Opacity[0.3], Dashed, Thickness[0.002], brightOrange,
                Table[
                    Line[{druhy[[i]], treti[[i]]}],
                    {i, 4}
                ]
            },
            {Opacity[0.3], Dashed, Thickness[0.002], brightRed,
                Table[
                    Line[{treti[[i]], finalny[[i]]}],
                    {i, 4}
                ]
            },
            
            (* Original square - blue *)
            {brightBlue, Thickness[0.005], Opacity[0.9],
                Line[Append[pociatocny, First[pociatocny]]]
            },
            
            (* Second square - green *)
            {brightGreen, Thickness[0.005], Opacity[0.9],
                Line[Append[druhy, First[druhy]]]
            },
            
            (* Third square - orange *)
            {brightOrange, Thickness[0.005], Opacity[0.9],
                Line[Append[treti, First[treti]]]
            },
            
            (* Final square - red *)
            {brightRed, Thickness[0.005], Opacity[0.9],
                Line[Append[finalny, First[finalny]]]
            },
            
            (* Vertex points - original square - removed black circles *)
            Table[
                {
                    White, Disk[pociatocny[[i]], 0.15],
                    brightBlue, Disk[pociatocny[[i]], 0.12]
                    (* Removed black circles *)
                },
                {i, 4}
            ],
            
            (* Vertex points - second square - removed black circles *)
            Table[
                {
                    White, Disk[druhy[[i]], 0.15],
                    brightGreen, Disk[druhy[[i]], 0.12]
                    (* Removed black circles *)
                },
                {i, 4}
            ],
            
            (* Vertex points - third square - removed black circles *)
            Table[
                {
                    White, Disk[treti[[i]], 0.15],
                    brightOrange, Disk[treti[[i]], 0.12]
                    (* Removed black circles *)
                },
                {i, 4}
            ],
            
            (* Vertex points - final square - removed black circles *)
            Table[
                {
                    White, Disk[finalny[[i]], 0.15],
                    brightRed, Disk[finalny[[i]], 0.12]
                    (* Removed black circles *)
                },
                {i, 4}
            ],
            
            (* Vertex labels with white background circles *)
            Table[
                With[{
                    label = Which[
                        i <= 4, FromCharacterCode[64 + i],
                        i <= 8, FromCharacterCode[64 + (i - 4)] <> "'",
                        i <= 12, FromCharacterCode[64 + (i - 8)] <> "''",
                        True, FromCharacterCode[64 + (i - 12)] <> "'''"
                    ],
                    color = Which[
                        i <= 4, brightBlue,
                        i <= 8, brightGreen,
                        i <= 12, brightOrange,
                        True, brightRed
                    ],
                    pos = labelPositions[[i]]
                },
                    {
                        (* White background circle for label *)
                        White, Disk[pos, 0.2],
                        (* Label with colored text *)
                        color, 
                        Text[Style[label, Bold, 14], pos]
                    }
                ],
                {i, 16}
            ]
        },
        PlotRange -> {xRange, yRange},
        AspectRatio -> 1, (* This ensures equal scaling for x and y axes *)
        ImageSize -> 650,
        PlotLabel -> Style["Postupn\[AAcute] aplik\[AAcute]cia troch transform\[AAcute]ci\[IAcute] \[SHacek]tvorca", Bold, 16],
        ImagePadding -> {{40, 40}, {40, 40}},
        Background -> White,
        Method -> {"ShrinkWrap" -> True}]
    ];


    
(* Funkcia na v\[YAcute]ber transform\[AAcute]cie s popisom  *)
SelectTransformation[message_, exclude1_:"", exclude2_:""] := 
    Module[{options, fullOptions, formattedChoices, result},
        options = DeleteCases[
            {
                "Posun" -> {"Posun", "Posun \[SHacek]tvorca vo smere vektora"},
                "Rot\[AAcute]cia" -> {"Rot\[AAcute]cia", "Rot\[AAcute]cia \[SHacek]tvorca okolo po\[CHacek]iatku s\[UAcute]radnicovej s\[UAcute]stavy"},
                "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie" -> {"Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie alebo zmen\[SHacek]enie \[SHacek]tvorca v r\[OHat]znych smeroch"},
                "Skosenie" -> {"Skosenie", "Skosenie \[SHacek]tvorca v smere os\[IAcute]"},
                "Symetria" -> {"Symetria", "Zrkadlenie \[SHacek]tvorca pod\:013ea zvolenej osi"}
            },
            x_ /; MemberQ[{exclude1, exclude2}, First[x]]
        ];
        
        (* Pou\[ZHacek]ijeme jednoduch\[SHacek]\[IAcute] pr\[IAcute]stup s ChoiceDialog, ale s vertik\[AAcute]lnym zoznamom *)
        ChoiceDialog[
            message,
            options,
            WindowTitle -> "V\[YAcute]ber transfform\[AAcute]cie",
            WindowSize -> {500, All}
        ]
    ];


(* Hlavn\[AAcute] funkcia pre trojit\[UAcute] transform\[AAcute]ciu s vylep\[SHacek]en\[YAcute]m pou\[ZHacek]\[IAcute]vate\:013esk\[YAcute]m rozhran\[IAcute]m a o\[SHacek]etren\[IAcute]m ch\[YAcute]b *)
StvorecTrojitaTransformacia[] := Module[{
    pociatocnyStvorec,
    prvaTransformacia,
    druhyStvorec,
    druhaTransformacia,
    tretiStvorec,
    tretiaTransformacia,
    finalnyStvorec,
    prvaPopis,
    druhaPopis,
    tretiaPopis
    },
    
    (* Potla\[CHacek]enie chybov\[YAcute]ch spr\[AAcute]v *)
    Quiet[
    (* \[CapitalUAcute]vodn\[AAcute] spr\[AAcute]va *)
    Print[Style["TROJIT\[CapitalAAcute] GEOMETRICK\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA \[CapitalSHacek]TVORCA", Bold, 24]];
    Print[Style["==========================================", Bold]];
    
    (* Generovanie a zobrazenie po\[CHacek]iato\[CHacek]n\[EAcute]ho \[SHacek]tvorca *)
    pociatocnyStvorec = GenerateInitialSquare[];
    Print[Style["\nPO\[CapitalCHacek]IATO\[CapitalCHacek]N\[CapitalYAcute] \[CapitalSHacek]TVOREC:", Bold, 16]];
    Print["Vrcholy \[SHacek]tvorca ABCD:"];
    Print[MatrixForm[Map[Style[#, Blue] &, pociatocnyStvorec, {2}]]];
    DisplaySquareProperties[pociatocnyStvorec, Blue];
    
    (* V\[YAcute]ber a aplik\[AAcute]cia prvej transform\[AAcute]cie *)
    prvaPopis = SelectTransformation["Vyberte prv\[UAcute] transform\[AAcute]ciu:"];
    
    If[prvaPopis === $Canceled, Return[]];
    prvaTransformacia = First[prvaPopis];
    
    Print[Style["\nPRV\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA: " <> prvaTransformacia, Bold, 16]];
    
    druhyStvorec = Switch[prvaTransformacia,
        "Posun", StvorecPosun[pociatocnyStvorec],
        "Rot\[AAcute]cia", StvorecRotacia[pociatocnyStvorec],
        "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", StvorecZvacsenieZmensenie[pociatocnyStvorec],
        "Skosenie", StvorecSkosenie[pociatocnyStvorec],
        "Symetria", StvorecSymetria[pociatocnyStvorec]
    ];
    
    (* V\[YAcute]ber a aplik\[AAcute]cia druhej transform\[AAcute]cie *)
    druhaPopis = SelectTransformation[
        "Vyberte druh\[UAcute] transform\[AAcute]ciu:", 
        prvaTransformacia,
        ""
    ];
    
    If[druhaPopis === $Canceled, Return[]];
    druhaTransformacia = First[druhaPopis];
    
    Print[Style["\nDRUH\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA: " <> druhaTransformacia, Bold, 16]];
    
    tretiStvorec = Switch[druhaTransformacia,
        "Posun", StvorecPosun[druhyStvorec],
        "Rot\[AAcute]cia", StvorecRotacia[druhyStvorec],
        "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", StvorecZvacsenieZmensenie[druhyStvorec],
        "Skosenie", StvorecSkosenie[druhyStvorec],
        "Symetria", StvorecSymetria[druhyStvorec]
    ];
    
    (* V\[YAcute]ber a aplik\[AAcute]cia tretej transform\[AAcute]cie *)
    tretiaPopis = SelectTransformation[
        "Vyberte tretiu transform\[AAcute]ciu:", 
        prvaTransformacia,
        druhaTransformacia
    ];
    
    If[tretiaPopis === $Canceled, Return[]];
    tretiaTransformacia = First[tretiaPopis];
    
    Print[Style["\nTRETIA TRANSFORM\[CapitalAAcute]CIA: " <> tretiaTransformacia, Bold, 16]];
    
    finalnyStvorec = Switch[tretiaTransformacia,
        "Posun", StvorecPosun[tretiStvorec],
        "Rot\[AAcute]cia", StvorecRotacia[tretiStvorec],
        "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", StvorecZvacsenieZmensenie[tretiStvorec],
        "Skosenie", StvorecSkosenie[tretiStvorec],
        "Symetria", StvorecSymetria[tretiStvorec]
    ];
    
    (* Zobrazenie v\[YAcute]po\[CHacek]tu a v\[YAcute]sledkov *)
    DisplayTransformationSequence[
        pociatocnyStvorec,
        druhyStvorec,
        tretiStvorec,
        finalnyStvorec,
        prvaTransformacia,
        druhaTransformacia,
        tretiaTransformacia
    ];
    
    (* Zobrazenie s\[UAcute]hrnnej vizualiz\[AAcute]cie a presunutej legendy *)
    Print[Style["\nS\[CapitalUAcute]HRNN\[CapitalAAcute] VIZUALIZ\[CapitalAAcute]CIA:", Bold, 16]];
    Print[CreateSquareVisualization[
        pociatocnyStvorec,
        druhyStvorec,
        tretiStvorec,
        finalnyStvorec,
        prvaTransformacia,
        druhaTransformacia,
        tretiaTransformacia
    ]];
    
    
	Print[Style["\nLEGENDA:", Bold, 16]];
	Print[Style["\nLEGENDA:", Bold, 16]];
		Print[Style["\[Bullet] Modr\[EAcute]", RGBColor[0, 0.4, 0.8], Bold], " body a \[CHacek]iary: P\[OHat]vodn\[YAcute] \[SHacek]tvorec ABCD"];
		Print[Style["\[Bullet] Tmavozelen\[EAcute]", RGBColor[0, 0.8, 0.2], Bold], " body a \[CHacek]iary: \[CapitalSHacek]tvorec A'B'C'D' po prvej transform\[AAcute]cii (", 
      Style[prvaTransformacia, Bold], ")"];
		Print[Style["\[Bullet] Oran\[ZHacek]ov\[EAcute]", RGBColor[1, 0.6, 0], Bold], " body a \[CHacek]iary: \[CapitalSHacek]tvorec A''B''C''D'' po druhej transform\[AAcute]cii (", 
      Style[druhaTransformacia, Bold], ")"];
		Print[Style["\[Bullet] \[CapitalCHacek]erven\[EAcute]", RGBColor[0.9, 0.1, 0.1], Bold], " body a \[CHacek]iary: \[CapitalSHacek]tvorec A'''B'''C'''D''' po tretej transform\[AAcute]cii (", 
      Style[tretiaTransformacia, Bold], ")"];

    
    (* Inform\[AAcute]cia o postupnosti transform\[AAcute]ci\[IAcute] *)
    Print[Style["\nPostupnos\[THacek] transform\[AAcute]ci\[IAcute]:", Bold, 16]];
    Print["1. ", prvaTransformacia];
    Print["2. ", druhaTransformacia];
    Print["3. ", tretiaTransformacia];
    
    (* Vr\[AAcute]ti\[THacek] inform\[AAcute]cie o transform\[AAcute]ci\[AAcute]ch pre pou\[ZHacek]itie v in\[YAcute]ch funkci\[AAcute]ch *)
    {pociatocnyStvorec, druhyStvorec, tretiStvorec, finalnyStvorec, 
     prvaTransformacia, druhaTransformacia, tretiaTransformacia}
    ]  (* Ukon\[CHacek]enie Quiet bloku *)
];

(* Funkcia pre s\[UAcute]hrnn\[UAcute] vizualiz\[AAcute]ciu (bez dial\[OAcute]gu) s o\[SHacek]etren\[IAcute]m ch\[YAcute]b *)
StvorecTrojitaTransformaciaSVysledkom[] := Module[{result},
   (* Pou\[ZHacek]itie Quiet na potla\[CHacek]enie chybov\[YAcute]ch spr\[AAcute]v *)
   result = Quiet[StvorecTrojitaTransformacia[], {Power::infy, Infinity::indet}];
   
   (* Ak funkcia nebola zru\[SHacek]en\[AAcute] *)
   If[result =!= Null,
       Check[
           Print[Style["\nZ\[CapitalAAcute]VERE\[CapitalCHacek]N\[CapitalYAcute] PREH\:013dAD:", Bold, 16]];
           Print["Vykonali ste postupne tieto transform\[AAcute]cie:"];
           Print["1. ", result[[5]]];
           Print["2. ", result[[6]]];
           Print["3. ", result[[7]]];
           Print["\nV\[YAcute]sledn\[YAcute] \[SHacek]tvorec A'''B'''C'''D''':"];
           Print[MatrixForm[result[[4]]]],
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
EndPackage[]; (* Uzavretie StvorecHardBalik package *)
