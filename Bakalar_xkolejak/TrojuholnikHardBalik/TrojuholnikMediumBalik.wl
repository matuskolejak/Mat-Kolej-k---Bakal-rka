(* ::Package:: *)

(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


BeginPackage["TrojuholnikMediumBalik`"];

(* Export verejn\[YAcute]ch symbolov *)
TrojuholnikDvojitaTransformacia::usage = 
    "TrojuholnikDvojitaTransformacia[] umo\[ZHacek]n\[IAcute] postupn\[YAcute] v\[YAcute]ber a aplik\[AAcute]ciu dvoch transform\[AAcute]ci\[IAcute].";

TrojuholnikDvojitaTransformaciaSVysledkom::usage = 
    "TrojuholnikDvojitaTransformaciaSVysledkom[] umo\[ZHacek]n\[IAcute] postupn\[YAcute] v\[YAcute]ber a aplik\[AAcute]ciu dvoch transform\[AAcute]ci\[IAcute] a zobraz\[IAcute] s\[UAcute]hrnn\[YAcute] v\[YAcute]sledok.";

TrojuholnikGeneruj::usage = 
    "TrojuholnikGeneruj[] generuje n\[AAcute]hodn\[YAcute] trojuholn\[IAcute]k s vhodn\[YAcute]mi vlastnos\[THacek]ami.";

(* Spolo\[CHacek]n\[EAcute] funkcie pre v\[SHacek]etky transform\[AAcute]cie *)
triangleArea::usage = 
    "triangleArea[p1, p2, p3] vypo\[CHacek]\[IAcute]ta obsah trojuholn\[IAcute]ka s vrcholmi p1, p2 a p3.";

validTriangleQ::usage = 
    "validTriangleQ[p1, p2, p3] over\[IAcute], \[CHacek]i trojuholn\[IAcute]k sp\:013a\[NHacek]a krit\[EAcute]ri\[AAcute].";

trianglePerimeter::usage = 
    "trianglePerimeter[p1, p2, p3] vypo\[CHacek]\[IAcute]ta obvod trojuholn\[IAcute]ka s vrcholmi p1, p2 a p3.";

(* Defin\[IAcute]cia chybov\[YAcute]ch spr\[AAcute]v *)
TrojuholnikDvojitaTransformacia::infinityerr = 
    "Vyskytla sa chyba: v\[YAcute]po\[CHacek]et viedol k nekone\[CHacek]n\[EAcute]mu alebo neur\[CHacek]it\[EAcute]mu v\[YAcute]sledku. Pou\[ZHacek]ije sa preddefinovan\[YAcute] trojuholn\[IAcute]k.";

Begin["`Private`"];

(* Potla\[CHacek]enie varovn\[YAcute]ch spr\[AAcute]v o nekone\[CHacek]n\[YAcute]ch v\[YAcute]sledkoch *)
Off[Power::infy];
Off[Infinity::indet];

(* Na\[CHacek]\[IAcute]tanie transforma\[CHacek]n\[YAcute]ch modulov z Hard bal\[IAcute]ka *)
Needs["TrojuholnikHardBalik`Transforms`Posun`"];
Needs["TrojuholnikHardBalik`Transforms`Rotacia`"];
Needs["TrojuholnikHardBalik`Transforms`ZvacsenieZmensenie`"];
Needs["TrojuholnikHardBalik`Transforms`Skosenie`"];
Needs["TrojuholnikHardBalik`Transforms`Symetria`"];

(* Toto zabezpe\[CHacek]\[IAcute], \[ZHacek]e bud\[UAcute] dostupn\[EAcute] v Medium kontexte *)
TrojuholnikPosun = TrojuholnikHardBalik`Transforms`Posun`TrojuholnikPosun;
TrojuholnikRotacia = TrojuholnikHardBalik`Transforms`Rotacia`TrojuholnikRotacia;
TrojuholnikZvacsenieZmensenie = TrojuholnikHardBalik`Transforms`ZvacsenieZmensenie`TrojuholnikZvacsenieZmensenie;
TrojuholnikSkosenie = TrojuholnikHardBalik`Transforms`Skosenie`TrojuholnikSkosenie;
TrojuholnikSymetria = TrojuholnikHardBalik`Transforms`Symetria`TrojuholnikSymetria;

(* Defin\[IAcute]cia miernej\[SHacek]ej farby zelenej *)
mildGreen = RGBColor[0.2, 0.6, 0.2];

(* Funkcia na v\[YAcute]po\[CHacek]et obsahu trojuholn\[IAcute]ka s kontrolou ch\[YAcute]b *)
triangleArea[p1_, p2_, p3_] := Module[{area},
    area = Check[
        Abs[(p1[[1]]*(p2[[2]] - p3[[2]]) + 
             p2[[1]]*(p3[[2]] - p1[[2]]) + 
             p3[[1]]*(p1[[2]] - p2[[2]]))/2],
        $Failed
    ];
    
    If[area === $Failed || area === ComplexInfinity || area === Indeterminate || !NumberQ[area],
        (* Vr\[AAcute]ti\[THacek] predvolen\[UAcute] hodnotu v pr\[IAcute]pade chyby *)
        10,
        (* Inak vr\[AAcute]ti\[THacek] skuto\[CHacek]n\[YAcute] obsah *)
        area
    ]
];

(* Funkcia na v\[YAcute]po\[CHacek]et obvodu trojuholn\[IAcute]ka s kontrolou ch\[YAcute]b *)
trianglePerimeter[p1_, p2_, p3_] := Module[{dist1, dist2, dist3},
    dist1 = Check[EuclideanDistance[p1, p2], $Failed];
    dist2 = Check[EuclideanDistance[p2, p3], $Failed];
    dist3 = Check[EuclideanDistance[p3, p1], $Failed];
    
    If[MemberQ[{dist1, dist2, dist3}, $Failed],
        (* Vr\[AAcute]ti\[THacek] predvolen\[UAcute] hodnotu v pr\[IAcute]pade chyby *)
        20,
        (* Inak vr\[AAcute]ti\[THacek] skuto\[CHacek]n\[YAcute] obvod *)
        dist1 + dist2 + dist3
    ]
];

(* Funkcia na v\[YAcute]po\[CHacek]et d\:013a\[ZHacek]ok str\[AAcute]n trojuholn\[IAcute]ka s kontrolou ch\[YAcute]b *)
triangleSides[p1_, p2_, p3_] := Module[{sides},
    sides = Check[{
        EuclideanDistance[p2, p3],
        EuclideanDistance[p1, p3],
        EuclideanDistance[p1, p2]
    }, $Failed];
    
    If[sides === $Failed,
        (* Vr\[AAcute]ti\[THacek] predvolen\[EAcute] hodnoty v pr\[IAcute]pade chyby *)
        {5, 5, 5},
        (* Inak vr\[AAcute]ti\[THacek] skuto\[CHacek]n\[EAcute] d\:013a\[ZHacek]ky *)
        sides
    ]
];

(* Funkcia na v\[YAcute]po\[CHacek]et uhlov trojuholn\[IAcute]ka v stup\[NHacek]och s kontrolou ch\[YAcute]b *)
triangleAngles[p1_, p2_, p3_] := Module[{a, b, c, angles},
    {a, b, c} = triangleSides[p1, p2, p3];
    
    (* V\[YAcute]po\[CHacek]et uhlov s kontrolou ch\[YAcute]b *)
    angles = Check[
        {
            ArcCos[Clip[(b^2 + c^2 - a^2)/(2 b c), {-1, 1}]]/Degree,
            ArcCos[Clip[(a^2 + c^2 - b^2)/(2 a c), {-1, 1}]]/Degree,
            ArcCos[Clip[(a^2 + b^2 - c^2)/(2 a b), {-1, 1}]]/Degree
        },
        $Failed
    ];
    
    If[angles === $Failed || MemberQ[angles, ComplexInfinity] || MemberQ[angles, Indeterminate],
        (* Vr\[AAcute]ti\[THacek] predvolen\[EAcute] hodnoty v pr\[IAcute]pade chyby *)
        {60, 60, 60},
        (* Inak vr\[AAcute]ti\[THacek] skuto\[CHacek]n\[EAcute] uhly *)
        angles
    ]
];

(* Funkcia na valid\[AAcute]ciu trojuholn\[IAcute]ka s o\[SHacek]etren\[IAcute]m ch\[YAcute]b *)
validTriangleQ[p1_, p2_, p3_] := 
    Module[{sides, angles, area, minSide = 3, minArea = 5, maxArea = 70, isValid}, 
        (* Kontrola \[CHacek]i s\[UAcute] vstupn\[EAcute] body validn\[EAcute] *)
        If[!VectorQ[p1, NumberQ] || !VectorQ[p2, NumberQ] || !VectorQ[p3, NumberQ] || 
           Length[p1] != 2 || Length[p2] != 2 || Length[p3] != 2,
            Return[False]
        ];
        
        (* Kontrola kolinearity bodov *)
        area = triangleArea[p1, p2, p3];
        If[area < 0.1, Return[False]];
        
        (* Bezpe\[CHacek]n\[YAcute] v\[YAcute]po\[CHacek]et str\[AAcute]n a uhlov *)
        sides = triangleSides[p1, p2, p3];
        If[!VectorQ[sides, NumberQ], Return[False]];
        
        (* V\[YAcute]po\[CHacek]et uhlov s kontrolou ch\[YAcute]b *)
        angles = Check[triangleAngles[p1, p2, p3] * Degree, {$Failed}];
        If[MemberQ[angles, $Failed], Return[False]];
        
        (* Zachytenie Indeterminate alebo ComplexInfinity v\[YAcute]sledkov *)
        If[MemberQ[angles, ComplexInfinity] || MemberQ[angles, Indeterminate] || 
           area === ComplexInfinity || area === Indeterminate, 
            Return[False]
        ];
        
        (* Hlavn\[AAcute] valid\[AAcute]cia trojuholn\[IAcute]ka s kontrolou ch\[YAcute]b *)
        isValid = Check[
            (* Kontrola rohov (minim\[AAcute]lne 30\[Degree], maxim\[AAcute]lne 120\[Degree]) *)
            AllTrue[angles, 30 Degree <= # <= 120 Degree &] && 
            (* Kontrola ve\:013ekosti plochy *)
            minArea <= area <= maxArea && 
            (* Kontrola minim\[AAcute]lnej d\:013a\[ZHacek]ky strany *)
            Min[sides] >= minSide,
            False
        ];
        
        isValid
    ];


GenerateInitialTriangle[] := Module[{
    p1, p2, p3, 
    count = 0, 
    defaultTriangle = {{2, 1}, {-4, 2}, {1, -4}}, 
    result,
    previousTriangles = {},
    randomSeed,
    generationType,
    minDistance = 2.5,
    minArea = 3.5
  },
  
  (* Pou\[ZHacek]i\[THacek] kombin\[AAcute]ciu r\[OHat]znych zdrojov n\[AAcute]hodnosti *)
  randomSeed = Hash[{AbsoluteTime[], $TimeZone, $ProcessID, 
                     RandomInteger[{-10^7, 10^7}], StringJoin @@ 
                     ToString /@ RandomInteger[{0, 9}, 10]}];
  SeedRandom[randomSeed];
  
  (* Pretrasenie gener\[AAcute]tora n\[AAcute]hodn\[YAcute]ch \[CHacek]\[IAcute]sel *)
  Do[RandomInteger[{-1000, 1000}], {RandomInteger[{10, 30}]}];
  
  (* Rozhodnutie, ak\[YAcute] typ hodn\[OHat]t pou\[ZHacek]i\[THacek] - len cel\[EAcute] \[CHacek]\[IAcute]sla a jednoduch\[EAcute] zlomky *)
  generationType = RandomChoice[{0.82, 0.18} -> {1, 2}];
  
  (* Pou\[ZHacek]itie Try-Catch \[SHacek]trukt\[UAcute]ry *)
  result = Check[
    Block[{$MessagePreprint = (Message[TrojuholnikDvojitaTransformacia::infinityerr]; #)&},
      While[True,
        Switch[generationType,
          (* Cel\[EAcute] \[CHacek]\[IAcute]sla - PREFEROVAN\[CapitalEAcute] *)
          1, 
          Module[{range, nonZeroRandom},
            (* Funkcia pre generovanie nenulov\[YAcute]ch hodn\[OHat]t *)
            nonZeroRandom[min_, max_] := Module[{val},
              val = RandomInteger[{min, max}];
              If[val == 0, If[RandomReal[] < 0.5, -1, 1], val]
            ];
            
            (* Men\[SHacek]\[IAcute] rozsah pre jednoduch\[SHacek]ie v\[YAcute]po\[CHacek]ty *)
            range = RandomInteger[{5, 8}];
            
            (* Generovanie s vyh\[YAcute]ban\[IAcute]m sa nul\[AAcute]m *)
            p1 = {nonZeroRandom[-range, range], nonZeroRandom[-range, range]};
            p2 = {nonZeroRandom[-range, range], nonZeroRandom[-range, range]};
            p3 = {nonZeroRandom[-range, range], nonZeroRandom[-range, range]};
            
            (* Zabezpe\[CHacek]\[IAcute]me, \[ZHacek]e aspo\[NHacek] jedna s\[UAcute]radnica nie je nulov\[AAcute] *)
            If[p1[[1]] == 0 && p1[[2]] == 0, p1 = {1, nonZeroRandom[-range, range]}];
            If[p2[[1]] == 0 && p2[[2]] == 0, p2 = {nonZeroRandom[-range, range], 1}];
            If[p3[[1]] == 0 && p3[[2]] == 0, p3 = {nonZeroRandom[-range, range], nonZeroRandom[-range, range]}];
          ],
          
          (* Jednoduch\[EAcute] zlomky - len menovatele 2,3,4 *)
          2,
          Module[{num1x, num1y, num2x, num2y, num3x, num3y, 
                  den1x, den1y, den2x, den2y, den3x, den3y, nonZeroNum},
            
            (* Funkcia pre generovanie nenulov\[EAcute]ho \[CHacek]itate\:013ea *)
            nonZeroNum[min_, max_] := Module[{val},
              val = RandomInteger[{min, max}];
              If[val == 0, If[RandomReal[] < 0.5, 1, -1], val]
            ];
            
            (* Jednoduch\[EAcute] \[CHacek]itatele a menovatele pre \:013eahk\[EAcute] po\[CHacek]\[IAcute]tanie - vyh\[YAcute]bame sa nul\[AAcute]m *)
            num1x = nonZeroNum[-8, 8];
            num1y = nonZeroNum[-8, 8];
            num2x = nonZeroNum[-8, 8];
            num2y = nonZeroNum[-8, 8];
            num3x = nonZeroNum[-8, 8];
            num3y = nonZeroNum[-8, 8];
            
            (* Len jednoduch\[EAcute] menovatele - aby \[SHacek]tudenti mohli \:013eahko po\[CHacek]\[IAcute]ta\[THacek] *)
            den1x = RandomChoice[{2, 3, 4}];
            den1y = RandomChoice[{2, 3, 4}];
            den2x = RandomChoice[{2, 3, 4}];
            den2y = RandomChoice[{2, 3, 4}];
            den3x = RandomChoice[{2, 3, 4}];
            den3y = RandomChoice[{2, 3, 4}];
            
            (* Zjednodu\[SHacek]enie zlomkov pre lep\[SHacek]iu \[CHacek]itate\:013enos\[THacek] *)
            p1 = {Rationalize[num1x/den1x], Rationalize[num1y/den1y]};
            p2 = {Rationalize[num2x/den2x], Rationalize[num2y/den2y]};
            p3 = {Rationalize[num3x/den3x], Rationalize[num3y/den3y]};
            
            (* Zabezpe\[CHacek]\[IAcute]me, \[ZHacek]e zlomok nie je nulov\[YAcute] - ak predsa, nahrad\[IAcute]me ho *)
            If[p1[[1]] == 0, p1[[1]] = Rationalize[1/den1x]];
            If[p1[[2]] == 0, p1[[2]] = Rationalize[1/den1y]];
            If[p2[[1]] == 0, p2[[1]] = Rationalize[2/den2x]];
            If[p2[[2]] == 0, p2[[2]] = Rationalize[2/den2y]];
            If[p3[[1]] == 0, p3[[1]] = Rationalize[-1/den3x]];
            If[p3[[2]] == 0, p3[[2]] = Rationalize[-1/den3y]];
          ]
        ];
        
        
        If[ContainsAny[{Sort[{p1, p2, p3}]}, previousTriangles],
          (* Sk\[UAcute]sime znova *)
          count++;
          If[Mod[count, 5] == 0, 
            generationType = RandomChoice[{0.8, 0.18, 0.02} -> {1, 2, 3}]
          ];
          Continue[]
        ];
        
        (* Kontrola vzdialenost\[IAcute] a ve\:013ekosti *)
        If[Min[EuclideanDistance[p1, p2], EuclideanDistance[p2, p3], EuclideanDistance[p3, p1]] > minDistance &&
           triangleArea[p1, p2, p3] > minArea,
          
          (* Kontrola validity a \[CHacek]i nevyjde z okna *)
          If[validTriangleQ[p1, p2, p3] && Max[Abs[Flatten[{p1, p2, p3}]]] <= 10,
            (* Prida\[THacek] do hist\[OAcute]rie *)
            AppendTo[previousTriangles, Sort[{p1, p2, p3}]];
            If[Length[previousTriangles] > 20, previousTriangles = previousTriangles[[-20;;]]];
            
            (* V\[YAcute]sledn\[YAcute] trojuholn\[IAcute]k *)
            Return[{p1, p2, p3}]
          ]
        ];
        
        (* Zmena typu ka\[ZHacek]d\[YAcute]ch nieko\:013eko pokusov *)
        If[Mod[count, 5] == 0 && count > 0, 
          generationType = RandomChoice[{0.82, 0.18} -> {1, 2}]
        ];
        
        (* Z\[AAcute]chrann\[YAcute] mechanizmus - jednoduch\[EAcute] geometrick\[EAcute] trojuholn\[IAcute]ky *)
        If[count > 50, 
          Module[{simpleTriangles},
            simpleTriangles = {
              
              {{3, 1}, {1, 4}, {-1, 2}},            (* Pekn\[YAcute] trojuholn\[IAcute]k bez n\[UAcute]l *)
              {{5, 1}, {2, 4}, {-1, 3}},            (* R\[OHat]znorod\[YAcute] bez n\[UAcute]l *)
              {{-3, -2}, {3, -2}, {1, 4}},          (* Obsah podobn\[YAcute] 18, bez n\[UAcute]l *)
              {{-4, 1}, {4, 2}, {1, 3}},            (* Pekn\[YAcute] bez n\[UAcute]l *)
              {{1, 1}, {6, 2}, {3, 5}},             (* Bez n\[UAcute]l *)
              {{-3, 1}, {3, 2}, {1, -4}},           (* Symetrick\[YAcute] bez n\[UAcute]l *)
              {{-2, -2}, {4, -1}, {1, 3}},          (* Obsah podobn\[YAcute] 15, bez n\[UAcute]l *)
              {{1, 2}, {4, 1}, {2, 5}},             (* Bez n\[UAcute]l *)
              {{1, 1}, {5, 2}, {2, 6}},             (* R\[OHat]znorod\[YAcute] bez n\[UAcute]l *)
              {{-3, -3}, {3, -2}, {1, 3}},          (* Bez n\[UAcute]l *)
              (* Trojuholn\[IAcute]ky s jednoduch\[YAcute]mi zlomkami, bez n\[UAcute]l *)
              {{-2, 1}, {2, 3}, {1, 3}},            (* Obsah podobn\[YAcute] 6, bez n\[UAcute]l *)
              {{1, Rationalize[1/2]}, {3, 2}, {2, 4}}, (* Obsahuje zlomok, bez n\[UAcute]l *)
              {{-1, -1}, {3, -1}, {1, 3}},          (* Obsah 8, be\[ZHacek]n\[YAcute], bez n\[UAcute]l *)
              {{-2, 1}, {2, 2}, {1, 3}}            (* Bez n\[UAcute]l *)
            };
            
            (* Vyberieme n\[AAcute]hodn\[YAcute] trojuholn\[IAcute]k z pripraven\[YAcute]ch *)
            Return[RandomChoice[simpleTriangles]]
          ]
        ];
        count++
      ]
    ],
    (* Predvolen\[YAcute] trojuholn\[IAcute]k s pekn\[YAcute]m obsahom *)
    {{3, 0}, {0, 4}, {0, 0}}
  ];
  
  (* Vr\[AAcute]ti\[THacek] v\[YAcute]sledok alebo predvolen\[YAcute] trojuholn\[IAcute]k *)
  If[Head[result] === List && Length[result] === 3 && AllTrue[result, VectorQ[#, NumberQ]&], 
    result, 
    {{3, 1}, {1, 4}, {-1, 2}}  (* Jednoduch\[YAcute] trojuholn\[IAcute]k bez n\[UAcute]l *)
  ]
];

(* Verejn\[AAcute] funkcia na generovanie trojuholn\[IAcute]ka *)
TrojuholnikGeneruj[] := GenerateInitialTriangle[];

(* Funkcia na form\[AAcute]tovan\[EAcute] zobrazenie vlastnost\[IAcute] trojuholn\[IAcute]ka s o\[SHacek]etren\[IAcute]m ch\[YAcute]b *)
DisplayTriangleProperties[vertices_, style_] := Module[{area, perimeter},
    (* V\[YAcute]po\[CHacek]et s o\[SHacek]etren\[IAcute]m pre pr\[IAcute]pad chyby *)
    area = Check[Round[triangleArea @@ vertices], 10];
    perimeter = Check[Round[trianglePerimeter @@ vertices], 20];
    
    (* O\[SHacek]etrenie pre pr\[IAcute]pad neplatn\[YAcute]ch hodn\[OHat]t *)
    If[!NumberQ[area] || area === ComplexInfinity || area === Indeterminate, area = 10];
    If[!NumberQ[perimeter] || perimeter === ComplexInfinity || perimeter === Indeterminate, perimeter = 20];
    
    Print[Style["Vlastnosti trojuholn\[IAcute]ka:", Bold]];
    Print["Obsah: ", Style[area, style]];
    Print["Obvod: ", Style[perimeter, style]];
];

(* Pokro\[CHacek]il\[EAcute] form\[AAcute]tovanie v\[YAcute]razov pre lep\[SHacek]ie zobrazenie trojuholn\[IAcute]kov *)
FormatTriangleExpression[expr_] :=
    Module[{expandedExpr, simplifiedExpr},
        (* Rozvinutie v\[YAcute]razu *)
        expandedExpr = Expand[expr];
        
        (* Z\[AAcute]kladn\[EAcute] zjednodu\[SHacek]enie *)
        simplifiedExpr = Simplify[expandedExpr];
        
        (* Vr\[AAcute]ti\[THacek] v\[YAcute]sledok v preferovanej forme *)
        simplifiedExpr
    ];

(* Funkcia na spracovanie jedn\[EAcute]ho vrcholu trojuholn\[IAcute]ka pre lep\[SHacek]ie zobrazenie *)
ProcessTriangleVertex[vertex_] := 
    Module[{},
        (* Aplikovanie spracovania na ka\[ZHacek]d\[UAcute] s\[UAcute]radnicu *)
        Map[FormatTriangleExpression, vertex]
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

(* Funkcia na spracovanie v\[SHacek]etk\[YAcute]ch vrcholov trojuholn\[IAcute]ka s pln\[YAcute]m spracovan\[IAcute]m v\[YAcute]razov *)
ProcessTriangleVerticesComplete[vertices_] :=
    Map[Function[vertex, Map[FullExpressionProcessor, vertex]], vertices];

(* Vylep\[SHacek]en\[AAcute] funkcia na zobrazenie postupn\[EAcute]ho v\[YAcute]po\[CHacek]tu transform\[AAcute]ci\[IAcute] - AKTUALIZOVAN\[CapitalAAcute] PRE DVE TRANSFORM\[CapitalAAcute]CIE *)
DisplayTransformationSequence[pociatocny_, druhy_, finalny_, prva_, druha_] := Module[{},
    Print[Style["\nPOSTUPN\[CapitalYAcute] V\[CapitalYAcute]PO\[CapitalCHacek]ET:", Bold, 16]];
    
    (* Vlastnosti p\[OHat]vodn\[EAcute]ho trojuholn\[IAcute]ka *)
    Print[Style["\nP\[OHat]vodn\[YAcute] trojuholn\[IAcute]k ABC:", Bold, 14]];
    Print["Vrcholy:"];
    Print[MatrixForm[Map[Style[#, Blue] &, pociatocny, {2}]]];
    DisplayTriangleProperties[pociatocny, Blue];
    
    (* Vlastnosti po prvej transform\[AAcute]cii - S MAXIM\[CapitalAAcute]LNE VYLEP\[CapitalSHacek]EN\[CapitalYAcute]M ZOBRAZEN\[CapitalIAcute]M *)
    Print[Style["\nPo prvej transform\[AAcute]cii (" <> prva <> "):", Bold, 14]];
    Print["Vrcholy trojuholn\[IAcute]ka A'B'C':"];
    
    (* Spracovanie vrcholov pre lep\[SHacek]ie zobrazenie *)
    Module[{processedVertices},
        processedVertices = ProcessTriangleVerticesComplete[druhy];
        (* Zobrazenie upraven\[YAcute]ch v\[YAcute]razov *)
        Print[MatrixForm[Map[Style[#, mildGreen] &, processedVertices, {2}]]];
    ];
    
    DisplayTriangleProperties[druhy, mildGreen];
    
    (* Vlastnosti po druhej transform\[AAcute]cii - S MAXIM\[CapitalAAcute]LNE VYLEP\[CapitalSHacek]EN\[CapitalYAcute]M ZOBRAZEN\[CapitalIAcute]M *)
    Print[Style["\nPo druhej transform\[AAcute]cii (" <> druha <> "):", Bold, 14]];
    Print["Vrcholy trojuholn\[IAcute]ka A''B''C'':"];
    
    (* Spracovanie vrcholov pre lep\[SHacek]ie zobrazenie *)
    Module[{processedVertices},
        processedVertices = ProcessTriangleVerticesComplete[finalny];
        (* Zobrazenie upraven\[YAcute]ch v\[YAcute]razov *)
        Print[MatrixForm[Map[Style[#, Orange] &, processedVertices, {2}]]];
    ];
    
    DisplayTriangleProperties[finalny, Orange];
];

(* Bezpe\[CHacek]n\[AAcute] funkcia pre vizualiz\[AAcute]ciu dvoch transform\[AAcute]ci\[IAcute] - \[CapitalUAcute]PLNE NOV\[CapitalAAcute] IMPLEMENT\[CapitalAAcute]CIA *)
CreateTriangleVisualization[pociatocny_, druhy_, finalny_, prva_, druha_] := 
    Module[{
        allVertices, xMin, xMax, yMin, yMax, 
        xRange, yRange, padding = 1,
        brightBlue = RGBColor[0, 0.4, 0.8],
        brightGreen = RGBColor[0.2, 0.7, 0.3],
        brightOrange = RGBColor[1, 0.6, 0],
        lightGray = RGBColor[0.9, 0.9, 0.9],
        labelPositions,
        maxRange, xMid, yMid
    },
        
        (* Spojenie v\[SHacek]etk\[YAcute]ch vrcholov pre v\[YAcute]po\[CHacek]et rozsahu *)
        allVertices = Join[pociatocny, druhy, finalny];
        
        (* V\[YAcute]po\[CHacek]et minim\[AAcute]lnych a maxim\[AAcute]lnych hodn\[OHat]t *)
        xMin = Min[N[allVertices[[All, 1]]]];
        xMax = Max[N[allVertices[[All, 1]]]];
        yMin = Min[N[allVertices[[All, 2]]]];
        yMax = Max[N[allVertices[[All, 2]]]];
        
        (* Prida\[THacek] odsadenie aby body neboli pr\[IAcute]li\[SHacek] bl\[IAcute]zko okrajom *)
        xRange = {xMin - padding, xMax + padding};
        yRange = {yMin - padding, yMax + padding};
        
        (* Zabezpe\[CHacek]\[IAcute] rovnak\[YAcute] rozsah os\[IAcute] pre zachovanie pomeru str\[AAcute]n *)
        maxRange = Max[xRange[[2]] - xRange[[1]], yRange[[2]] - yRange[[1]]];
        xMid = Mean[xRange];
        yMid = Mean[yRange];
        
        xRange = {xMid - maxRange/2, xMid + maxRange/2};
        yRange = {yMid - maxRange/2, yMid + maxRange/2};
        
        (* Vytvorenie lep\[SHacek]\[IAcute]ch poz\[IAcute]ci\[IAcute] \[SHacek]t\[IAcute]tkov s vyv\[AAcute]\[ZHacek]enou vzdialenos\[THacek]ou od vrcholov *)
        labelPositions = Table[
            Module[{
                vertex = Which[
                    i <= 3, pociatocny[[i]],
                    i <= 6, druhy[[i - 3]],
                    True, finalny[[i - 6]]
                ],
                (* Lep\[SHacek]ie vyv\[AAcute]\[ZHacek]en\[YAcute] posun - nie pr\[IAcute]li\[SHacek] bl\[IAcute]zko, nie pr\[IAcute]li\[SHacek] \[DHacek]aleko *)
                offset = {0.35, 0.35},
                nearby = {}
            },
                
                (* N\[AAcute]js\[THacek] bl\[IAcute]zke body, aby sa zabr\[AAcute]nilo prekr\[YAcute]vaniu *)
                nearby = DeleteCases[allVertices, vertex];
                nearby = Select[nearby, EuclideanDistance[#, vertex] < 0.7 &]; 
                
                If[Length[nearby] > 0,
                    (* Vypo\[CHacek]\[IAcute]ta\[THacek] vyv\[AAcute]\[ZHacek]en\[YAcute] posun na z\[AAcute]klade bl\[IAcute]zkych bodov *)
                    offset = Mean[Table[
                        Normalize[vertex - point] * 0.4,
                        {point, nearby}
                    ]];
                    
                    (* Upravi\[THacek] pre jeden bl\[IAcute]zky bod *)
                    If[Length[nearby] == 1, offset = offset * 1.3];
                ,
                    (* Predvolen\[EAcute] posunutia - vyv\[AAcute]\[ZHacek]en\[AAcute] vzdialenos\[THacek] *)
                    If[N[vertex[[1]]] > 0, offset[[1]] = 0.35, offset[[1]] = -0.35];
                    If[N[vertex[[2]]] > 0, offset[[2]] = 0.35, offset[[2]] = -0.35];
                ];
                
                (* Prisp\[OHat]sobi\[THacek] posun pod\:013ea vzdialenosti od po\[CHacek]iatku *)
                offset = offset * (0.8 + 0.07 * EuclideanDistance[vertex, {0, 0}]);
                
                vertex + offset
            ],
            {i, 9}  (* 3 vrcholy pre trojuholn\[IAcute]k, 3 trojuholn\[IAcute]ky *)
        ];
        
        Graphics[{
            (* Mrie\[ZHacek]ka *)
            lightGray, Thin,
            Table[Line[{{xRange[[1]], y}, {xRange[[2]], y}}], 
                  {y, Ceiling[yRange[[1]]], Floor[yRange[[2]]]}],
            Table[Line[{{x, yRange[[1]]}, {x, yRange[[2]]}}], 
                  {x, Ceiling[xRange[[1]]], Floor[xRange[[2]]]}],
            
            (* Osi *)
            Black, Thickness[0.003], Arrowheads[0.02],
            Arrow[{{xRange[[1]], 0}, {xRange[[2]], 0}}],
            Arrow[{{0, yRange[[1]]}, {0, yRange[[2]]}}],
            Text[Style["x", Bold, 14], {xRange[[2]] - 0.3, -0.3}],
            Text[Style["y", Bold, 14], {-0.3, yRange[[2]] - 0.3}],
            
            (* Zna\[CHacek]ky na osiach *)
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
            
            (* Cesty transform\[AAcute]cie - preru\[SHacek]ovan\[EAcute] \[CHacek]iary medzi zodpovedaj\[UAcute]cimi vrcholmi *)
            {Opacity[0.3], Dashed, Thickness[0.002], brightGreen,
                Table[
                    Line[{pociatocny[[i]], druhy[[i]]}],
                    {i, 3}
                ]
            },
            {Opacity[0.3], Dashed, Thickness[0.002], brightOrange,
                Table[
                    Line[{druhy[[i]], finalny[[i]]}],
                    {i, 3}
                ]
            },
            
            (* P\[OHat]vodn\[YAcute] trojuholn\[IAcute]k - modr\[YAcute] *)
            {brightBlue, Thickness[0.005], Opacity[0.9],
                Line[Append[pociatocny, First[pociatocny]]]
            },
            
            (* Druh\[YAcute] trojuholn\[IAcute]k - zelen\[YAcute] *)
            {brightGreen, Thickness[0.005], Opacity[0.9],
                Line[Append[druhy, First[druhy]]]
            },
            
            (* V\[YAcute]sledn\[YAcute] trojuholn\[IAcute]k - oran\[ZHacek]ov\[YAcute] *)
            {brightOrange, Thickness[0.005], Opacity[0.9],
                Line[Append[finalny, First[finalny]]]
            },
            
            (* Body vrcholov - p\[OHat]vodn\[YAcute] trojuholn\[IAcute]k *)
            Table[
                {
                    White, Disk[pociatocny[[i]], 0.15],
                    brightBlue, Disk[pociatocny[[i]], 0.12]
                },
                {i, 3}
            ],
            
            (* Body vrcholov - druh\[YAcute] trojuholn\[IAcute]k *)
            Table[
                {
                    White, Disk[druhy[[i]], 0.15],
                    brightGreen, Disk[druhy[[i]], 0.12]
                },
                {i, 3}
            ],
            
            (* Body vrcholov - v\[YAcute]sledn\[YAcute] trojuholn\[IAcute]k *)
            Table[
                {
                    White, Disk[finalny[[i]], 0.15],
                    brightOrange, Disk[finalny[[i]], 0.12]
                },
                {i, 3}
            ],
            
            (* \[CapitalSHacek]t\[IAcute]tky vrcholov s bielym pozad\[IAcute]m *)
            Table[
                With[{
                    label = Which[
                        i <= 3, FromCharacterCode[64 + i],
                        i <= 6, FromCharacterCode[64 + (i - 3)] <> "'",
                        True, FromCharacterCode[64 + (i - 6)] <> "''"
                    ],
                    color = Which[
                        i <= 3, brightBlue,
                        i <= 6, brightGreen,
                        True, brightOrange
                    ],
                    pos = labelPositions[[i]]
                },
                    {
                        (* Biele pozadie pre \[SHacek]t\[IAcute]tok *)
                        White, Disk[pos, 0.2],
                        (* \[CapitalSHacek]t\[IAcute]tok s farebn\[YAcute]m textom *)
                        color, 
                        Text[Style[label, Bold, 14], pos]
                    }
                ],
                {i, 9}
            ]
        },
        PlotRange -> {xRange, yRange},
        AspectRatio -> 1, (* Zabezpe\[CHacek]uje rovnak\[EAcute] mierky pre osi x a y *)
        ImageSize -> 650,
        PlotLabel -> Style["Postupn\[AAcute] aplik\[AAcute]cia dvoch transform\[AAcute]ci\[IAcute] trojuholn\[IAcute]ka", Bold, 16],
        ImagePadding -> {{40, 40}, {40, 40}},
        Background -> White,
        Method -> {"ShrinkWrap" -> True}]
    ];

(* Funkcia na v\[YAcute]ber transform\[AAcute]cie s popisom *)
SelectTransformation[message_, exclude1_:""] := 
    Module[{options, fullOptions, formattedChoices, result},
        options = DeleteCases[
            {
                "Posun" -> {"Posun", "Posun trojuholn\[IAcute]ka vo smere vektora"},
                "Rot\[AAcute]cia" -> {"Rot\[AAcute]cia", "Rot\[AAcute]cia trojuholn\[IAcute]ka okolo po\[CHacek]iatku s\[UAcute]radnicovej s\[UAcute]stavy"},
                "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie" -> {"Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie alebo zmen\[SHacek]enie trojuholn\[IAcute]ka v r\[OHat]znych smeroch"},
                "Skosenie" -> {"Skosenie", "Skosenie trojuholn\[IAcute]ka v smere os\[IAcute]"},
                "Symetria" -> {"Symetria", "Zrkadlenie trojuholn\[IAcute]ka pod\:013ea zvolenej osi"}
            },
            x_ /; MemberQ[{exclude1}, First[x]]
        ];
        
        (* Pou\[ZHacek]ijeme jednoduch\[SHacek]\[IAcute] pr\[IAcute]stup s ChoiceDialog, ale s vertik\[AAcute]lnym zoznamom *)
        ChoiceDialog[
            message,
            options,
            WindowTitle -> "V\[YAcute]ber transform\[AAcute]cie",
            WindowSize -> {500, All}
        ]
    ];

(* Hlavn\[AAcute] funkcia pre dvojit\[UAcute] transform\[AAcute]ciu s vylep\[SHacek]en\[YAcute]m pou\[ZHacek]\[IAcute]vate\:013esk\[YAcute]m rozhran\[IAcute]m a o\[SHacek]etren\[IAcute]m ch\[YAcute]b *)
TrojuholnikDvojitaTransformacia[] := Module[{
    pociatocnyTrojuholnik,
    prvaTransformacia,
    druhyTrojuholnik,
    druhaTransformacia,
    finalnyTrojuholnik,
    prvaPopis,
    druhaPopis
    },
    
    (* Potla\[CHacek]enie chybov\[YAcute]ch spr\[AAcute]v *)
    Quiet[
    (* \[CapitalUAcute]vodn\[AAcute] spr\[AAcute]va *)
    Print[Style["DVOJIT\[CapitalAAcute] GEOMETRICK\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA TROJUHOLN\[CapitalIAcute]KA", Bold, 24]];
    Print[Style["==========================================", Bold]];
    
    (* Generovanie a zobrazenie po\[CHacek]iato\[CHacek]n\[EAcute]ho trojuholn\[IAcute]ka *)
    pociatocnyTrojuholnik = GenerateInitialTriangle[];
    Print[Style["\nPO\[CapitalCHacek]IATO\[CapitalCHacek]N\[CapitalYAcute] TROJUHOLN\[CapitalIAcute]K:", Bold, 16]];
    Print["Vrcholy trojuholn\[IAcute]ka ABC:"];
    Print[MatrixForm[Map[Style[#, Blue] &, pociatocnyTrojuholnik, {2}]]];
    DisplayTriangleProperties[pociatocnyTrojuholnik, Blue];
    
    (* V\[YAcute]ber a aplik\[AAcute]cia prvej transform\[AAcute]cie *)
    prvaPopis = SelectTransformation["Vyberte prv\[UAcute] transform\[AAcute]ciu:"];
    
    If[prvaPopis === $Canceled, Return[]];
    prvaTransformacia = First[prvaPopis];
    
    Print[Style["\nPRV\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA: " <> prvaTransformacia, Bold, 16]];
    
    druhyTrojuholnik = Switch[prvaTransformacia,
        "Posun", TrojuholnikPosun[pociatocnyTrojuholnik],
        "Rot\[AAcute]cia", TrojuholnikRotacia[pociatocnyTrojuholnik],
        "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", TrojuholnikZvacsenieZmensenie[pociatocnyTrojuholnik],
        "Skosenie", TrojuholnikSkosenie[pociatocnyTrojuholnik],
        "Symetria", TrojuholnikSymetria[pociatocnyTrojuholnik]
    ];
    
    (* V\[YAcute]ber a aplik\[AAcute]cia druhej transform\[AAcute]cie *)
    druhaPopis = SelectTransformation[
        "Vyberte druh\[UAcute] transform\[AAcute]ciu:", 
        prvaTransformacia
    ];
    
    If[druhaPopis === $Canceled, Return[]];
    druhaTransformacia = First[druhaPopis];
    
    Print[Style["\nDRUH\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA: " <> druhaTransformacia, Bold, 16]];
    
    finalnyTrojuholnik = Switch[druhaTransformacia,
        "Posun", TrojuholnikPosun[druhyTrojuholnik],
        "Rot\[AAcute]cia", TrojuholnikRotacia[druhyTrojuholnik],
        "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", TrojuholnikZvacsenieZmensenie[druhyTrojuholnik],
        "Skosenie", TrojuholnikSkosenie[druhyTrojuholnik],
        "Symetria", TrojuholnikSymetria[druhyTrojuholnik]
    ];
    
    (* Zobrazenie v\[YAcute]po\[CHacek]tu a v\[YAcute]sledkov *)
    DisplayTransformationSequence[
        pociatocnyTrojuholnik,
        druhyTrojuholnik,
        finalnyTrojuholnik,
        prvaTransformacia,
        druhaTransformacia
    ];
    
    (* Zobrazenie s\[UAcute]hrnnej vizualiz\[AAcute]cie a presunut\[EAcute] legendy *)
    Print[Style["\nS\[CapitalUAcute]HRNN\[CapitalAAcute] VIZUALIZ\[CapitalAAcute]CIA:", Bold, 16]];
    Print[CreateTriangleVisualization[
        pociatocnyTrojuholnik,
        druhyTrojuholnik,
        finalnyTrojuholnik,
        prvaTransformacia,
        druhaTransformacia
    ]];
    
    Print[Style["\nLEGENDA:", Bold, 16]];
    Print[Style["\[Bullet] Modr\[EAcute]", RGBColor[0, 0.4, 0.8], Bold], " body a \[CHacek]iary: P\[OHat]vodn\[YAcute] trojuholn\[IAcute]k ABC"];
    Print[Style["\[Bullet] Tmavozelen\[EAcute]", RGBColor[0.2, 0.7, 0.3], Bold], " body a \[CHacek]iary: Trojuholn\[IAcute]k A'B'C' po prvej transform\[AAcute]cii (", 
          Style[prvaTransformacia, Bold], ")"];
    Print[Style["\[Bullet] Oran\[ZHacek]ov\[EAcute]", RGBColor[1, 0.6, 0], Bold], " body a \[CHacek]iary: Trojuholn\[IAcute]k A''B''C'' po druhej transform\[AAcute]cii (", 
          Style[druhaTransformacia, Bold], ")"];
    
    (* Inform\[AAcute]cia o postupnosti transform\[AAcute]ci\[IAcute] *)
    Print[Style["\nPostupnos\[THacek] transform\[AAcute]ci\[IAcute]:", Bold, 16]];
    Print["1. ", prvaTransformacia];
    Print["2. ", druhaTransformacia];
    
    (* Vr\[AAcute]ti\[THacek] inform\[AAcute]cie o transform\[AAcute]ci\[AAcute]ch pre pou\[ZHacek]itie v in\[YAcute]ch funkci\[AAcute]ch *)
    {pociatocnyTrojuholnik, druhyTrojuholnik, finalnyTrojuholnik, 
     prvaTransformacia, druhaTransformacia}
    ]  (* Ukon\[CHacek]enie Quiet bloku *)
];

(* Funkcia pre s\[UAcute]hrnn\[UAcute] vizualiz\[AAcute]ciu (bez dial\[OAcute]gu) s o\[SHacek]etren\[IAcute]m ch\[YAcute]b *)
TrojuholnikDvojitaTransformaciaSVysledkom[] := Module[{result},
   (* Pou\[ZHacek]itie Quiet na potla\[CHacek]enie chybov\[YAcute]ch spr\[AAcute]v *)
   result = Quiet[TrojuholnikDvojitaTransformacia[], {Power::infy, Infinity::indet}];
   
   (* Ak funkcia nebola zru\[SHacek]en\[AAcute] *)
   If[result =!= Null,
       Check[
           Print[Style["\nZ\[CapitalAAcute]VERE\[CapitalCHacek]N\[CapitalYAcute] PREH\:013dAD:", Bold, 16]];
           Print["Vykonali ste postupne tieto transform\[AAcute]cie:"];
           Print["1. ", result[[4]]];
           Print["2. ", result[[5]]];
           Print["\nV\[YAcute]sledn\[YAcute] trojuholn\[IAcute]k A''B''C'':"];
           Print[MatrixForm[result[[3]]]],
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
EndPackage[]; (* Uzavretie TrojuholnikMediumBalik package *)


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


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)
