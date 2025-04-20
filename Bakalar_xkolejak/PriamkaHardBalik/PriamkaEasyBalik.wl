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


BeginPackage["PriamkaEasyBalik`"];

(* Export verejn\[YAcute]ch symbolov *)
PriamkaJednaTransformacia::usage = 
    "PriamkaJednaTransformacia[] umo\[ZHacek]n\[IAcute] v\[YAcute]ber a aplik\[AAcute]ciu jednej transform\[AAcute]cie.";

PriamkaJednaTransformaciaSVysledkom::usage = 
    "PriamkaJednaTransformaciaSVysledkom[] umo\[ZHacek]n\[IAcute] v\[YAcute]ber a aplik\[AAcute]ciu jednej transform\[AAcute]cie a zobraz\[IAcute] s\[UAcute]hrnn\[YAcute] v\[YAcute]sledok.";

PriamkaGeneruj::usage = 
    "PriamkaGeneruj[] generuje n\[AAcute]hodn\[UAcute] priamku s vhodn\[YAcute]mi vlastnos\[THacek]ami.";

(* Spolo\[CHacek]n\[EAcute] funkcie pre v\[SHacek]etky transform\[AAcute]cie *)
priamkaLength::usage = 
    "priamkaLength[p1, p2] vypo\[CHacek]\[IAcute]ta d\:013a\[ZHacek]ku priamky medzi bodmi p1 a p2.";

validPriamkaQ::usage = 
    "validPriamkaQ[p1, p2] over\[IAcute], \[CHacek]i priamka sp\:013a\[NHacek]a krit\[EAcute]ri\[AAcute].";

priamkaSlope::usage = 
    "priamkaSlope[p1, p2] vypo\[CHacek]\[IAcute]ta smernicu priamky prechadzaj\[UAcute]cej bodmi p1 a p2.";

priamkaYIntercept::usage = 
    "priamkaYIntercept[p1, p2] vypo\[CHacek]\[IAcute]ta priese\[CHacek]n\[IAcute]k priamky s osou y.";

(* Defin\[IAcute]cia chybov\[YAcute]ch spr\[AAcute]v *)
PriamkaJednaTransformacia::infinityerr = 
    "Vyskytla sa chyba: v\[YAcute]po\[CHacek]et viedol k nekone\[CHacek]n\[EAcute]mu alebo neur\[CHacek]it\[EAcute]mu v\[YAcute]sledku. Pou\[ZHacek]ije sa preddefinovan\[AAcute] priamka.";

Begin["`Private`"];

(* Potla\[CHacek]enie varovn\[YAcute]ch spr\[AAcute]v o nekone\[CHacek]n\[YAcute]ch v\[YAcute]sledkoch *)
Off[Power::infy];
Off[Infinity::indet];

(* Na\[CHacek]\[IAcute]tanie v\[SHacek]etk\[YAcute]ch transforma\[CHacek]n\[YAcute]ch modulov na za\[CHacek]iatku *)
Needs["PriamkaHardBalik`Transforms`Posun`"];
Needs["PriamkaHardBalik`Transforms`Rotacia`"];
Needs["PriamkaHardBalik`Transforms`ZvacsenieZmensenie`"];
Needs["PriamkaHardBalik`Transforms`Skosenie`"];
Needs["PriamkaHardBalik`Transforms`Symetria`"];

(* Toto zabezpe\[CHacek]\[IAcute], \[ZHacek]e bud\[UAcute] dostupn\[EAcute] v Easy kontexte *)
PriamkaPosun = PriamkaHardBalik`Transforms`Posun`PriamkaPosun;
PriamkaRotacia = PriamkaHardBalik`Transforms`Rotacia`PriamkaRotacia;
PriamkaZvacsenieZmensenie = PriamkaHardBalik`Transforms`ZvacsenieZmensenie`PriamkaZvacsenieZmensenie;
PriamkaSkosenie = PriamkaHardBalik`Transforms`Skosenie`PriamkaSkosenie;
PriamkaSymetria = PriamkaHardBalik`Transforms`Symetria`PriamkaSymetria;

(* Defin\[IAcute]cia farby modrej *)
blue = RGBColor[0, 0.4, 0.8];
green = RGBColor[0.2, 0.8, 0.2];

(* Funkcia na v\[YAcute]po\[CHacek]et smernice priamky s kontrolou ch\[YAcute]b a exaktn\[YAcute]mi hodnotami *)
priamkaSlope[p1_, p2_] := Module[{slope},
    slope = Check[
        If[Abs[p2[[1]] - p1[[1]]] < 0.000001,
            ComplexInfinity,  (* Zvisl\[AAcute] priamka *)
            Simplify[(p2[[2]] - p1[[2]])/(p2[[1]] - p1[[1]])]  (* Pou\[ZHacek]itie Simplify pre zlomky *)
        ],
        $Failed
    ];
    
    If[slope === $Failed || slope === Indeterminate || !(NumberQ[slope] || Head[slope] === Rational || RadicalBox),
        (* Vr\[AAcute]ti\[THacek] predvolen\[UAcute] hodnotu v pr\[IAcute]pade chyby *)
        1,
        (* Inak vr\[AAcute]ti\[THacek] skuto\[CHacek]n\[UAcute] smernicu v exaktnej forme *)
        slope
    ]
];

(* Funkcia na v\[YAcute]po\[CHacek]et priese\[CHacek]n\[IAcute]ka priamky s osou y s kontrolou ch\[YAcute]b a exaktn\[YAcute]mi hodnotami *)
priamkaYIntercept[p1_, p2_] := Module[{yInt, slope},
    slope = priamkaSlope[p1, p2];
    
    yInt = Check[
        If[slope === ComplexInfinity,
            Indeterminate,  (* Pre zvisl\[UAcute] priamku *)
            Simplify[p1[[2]] - slope * p1[[1]]]  (* Pou\[ZHacek]itie Simplify pre zlomky/iracion\[AAcute]lne \[CHacek]\[IAcute]sla *)
        ],
        $Failed
    ];
    
    (* Roz\[SHacek]\[IAcute]ren\[AAcute] kontrola typov na detekciu v\[SHacek]etk\[YAcute]ch mo\[ZHacek]n\[YAcute]ch typov \[CHacek]\[IAcute]sel *)
    If[yInt === $Failed || yInt === Indeterminate || 
       !(NumberQ[yInt] || Head[yInt] === Rational || 
         (Head[yInt] === Power && Length[yInt] >= 2 && yInt[[2]] === 1/2) || 
         Head[yInt] === Complex || Head[yInt] === Symbol),
        (* Vr\[AAcute]ti\[THacek] predvolen\[UAcute] hodnotu v pr\[IAcute]pade chyby *)
        0,
        (* Inak vr\[AAcute]ti\[THacek] skuto\[CHacek]n\[YAcute] priese\[CHacek]n\[IAcute]k s osou y v exaktnej forme *)
        yInt
    ]
];

(* Funkcia na v\[YAcute]po\[CHacek]et priese\[CHacek]n\[IAcute]ka priamky s osou x s kontrolou ch\[YAcute]b a exaktn\[YAcute]mi hodnotami *)
priamkaXIntercept[p1_, p2_] := Module[{xInt, slope, yInt},
    slope = priamkaSlope[p1, p2];
    yInt = priamkaYIntercept[p1, p2];
    
    xInt = Check[
        If[Abs[slope] < 0.000001,
            Indeterminate,  (* Pre vodorovn\[UAcute] priamku *)
            If[slope === ComplexInfinity,
                p1[[1]],  (* Pre zvisl\[UAcute] priamku *)
                Simplify[-yInt/slope]  (* Pou\[ZHacek]itie Simplify pre zlomky/odmocniny *)
            ]
        ],
        $Failed
    ];
    
    (* Roz\[SHacek]\[IAcute]ren\[AAcute] kontrola typov na detekciu v\[SHacek]etk\[YAcute]ch mo\[ZHacek]n\[YAcute]ch typov \[CHacek]\[IAcute]sel *)
    If[xInt === $Failed || xInt === Indeterminate || 
       !(NumberQ[xInt] || Head[xInt] === Rational || 
         (Head[xInt] === Power && Length[xInt] >= 2 && xInt[[2]] === 1/2) || 
         Head[xInt] === Complex || Head[xInt] === Symbol),
        (* Vr\[AAcute]ti\[THacek] predvolen\[UAcute] hodnotu v pr\[IAcute]pade chyby *)
        0,
        (* Inak vr\[AAcute]ti\[THacek] skuto\[CHacek]n\[YAcute] priese\[CHacek]n\[IAcute]k s osou x v exaktnej forme *)
        xInt
    ]
];

(* Funkcia na valid\[AAcute]ciu priamky s o\[SHacek]etren\[IAcute]m ch\[YAcute]b *)
validPriamkaQ[p1_, p2_] := 
    Module[{minLength = 2, maxLength = 15, isValid}, 
        (* Kontrola \[CHacek]i s\[UAcute] vstupn\[EAcute] body validn\[EAcute] *)
        If[!VectorQ[p1, NumberQ] || !VectorQ[p2, NumberQ] || 
           Length[p1] != 2 || Length[p2] != 2,
            Return[False]
        ];
        
        (* Kontrola \[CHacek]i body nie s\[UAcute] toto\[ZHacek]n\[EAcute] alebo pr\[IAcute]li\[SHacek] bl\[IAcute]zko *)
        If[EuclideanDistance[p1, p2] < 0.1, Return[False]];
        
        (* Hlavn\[AAcute] valid\[AAcute]cia priamky s kontrolou ch\[YAcute]b *)
        isValid = Check[
            (* Kontrola d\:013a\[ZHacek]ky priamky *)
            minLength <= priamkaLength[p1, p2] <= maxLength &&
            (* Kontrola, \[CHacek]i body nie s\[UAcute] mimo definovan\[EAcute]ho rozsahu *)
            Max[Abs[p1[[1]]], Abs[p1[[2]]], Abs[p2[[1]]], Abs[p2[[2]]]] <= 10,
            False
        ];
        
        isValid
    ];

(* Funkcia na generovanie n\[AAcute]hodnej priamky *)
GenerateInitialLine[] := Module[{
    p1, p2, 
    count = 0, 
    defaultLine = {{-2, -1}, {2, 3}}, 
    result,
    previousLines = {},
    randomSeed,
    generationType,
    minDistance = 2
  },
  
  (* Pou\[ZHacek]i\[THacek] kombin\[AAcute]ciu r\[OHat]znych zdrojov n\[AAcute]hodnosti *)
  randomSeed = Hash[{AbsoluteTime[], $TimeZone, $ProcessID, 
                     RandomInteger[{-10^7, 10^7}], StringJoin @@ 
                     ToString /@ RandomInteger[{0, 9}, 10]}];
  SeedRandom[randomSeed];
  
  (* Pretrasenie gener\[AAcute]tora n\[AAcute]hodn\[YAcute]ch \[CHacek]\[IAcute]sel *)
  Do[RandomInteger[{-1000, 1000}], {RandomInteger[{10, 30}]}];
  
  (* Rozhodnutie, ak\[YAcute] typ hodn\[OHat]t pou\[ZHacek]i\[THacek] - cel\[EAcute] \[CHacek]\[IAcute]sla vs. zlomky/odmocniny *)
  generationType = RandomChoice[{0.85, 0.15} -> {1, 2}];
  
  (* Pou\[ZHacek]itie Try-Catch \[SHacek]trukt\[UAcute]ry *)
  result = Check[
    Block[{$MessagePreprint = (Message[PriamkaJednaTransformacia::infinityerr]; #)&},
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
            range = RandomInteger[{6, 8}];
            
            (* Generovanie bodov priamky *)
            p1 = {nonZeroRandom[-range, range], nonZeroRandom[-range, range]};
            p2 = {nonZeroRandom[-range, range], nonZeroRandom[-range, range]};
            
            (* Zabezpe\[CHacek]\[IAcute]me, \[ZHacek]e body s\[UAcute] dostato\[CHacek]ne vzdialen\[EAcute] *)
            While[EuclideanDistance[p1, p2] < minDistance, 
              p2 = {nonZeroRandom[-range, range], nonZeroRandom[-range, range]};
            ];
          ],
          
          (* Jednoduch\[EAcute] zlomky/odmocniny *)
          2,
          Module[{makeSpecialNumber, p1x, p1y, p2x, p2y},
            (* Funkcia pre generovanie \[SHacek]peci\[AAcute]lnych \[CHacek]\[IAcute]sel *)
            makeSpecialNumber[min_, max_] := Module[{choice, base, num, denom, sqrtBase},
              choice = RandomChoice[{0.6, 0.2, 0.2} -> {"whole", "fraction", "sqrt"}];
              
              Switch[choice,
                "whole", 
                  RandomInteger[{min, max}],
                
                "fraction", 
                  Module[{num, denom},
                    num = RandomInteger[{1, 5}];
                    denom = RandomChoice[{2, 3, 4}];
                    Rationalize[num/denom]
                  ],
                
                "sqrt", 
                  Module[{sqrtBase, sign},
                    sqrtBase = RandomChoice[{2, 3, 5}];
                    sign = If[RandomReal[] < 0.5, 1, -1];
                    sign * Sqrt[sqrtBase]
                  ]
              ]
            ];
            
            (* Generovanie bodov s r\[OHat]znymi typmi \[CHacek]\[IAcute]sel *)
            p1x = makeSpecialNumber[-7, 7];
            p1y = makeSpecialNumber[-7, 7];
            p2x = makeSpecialNumber[-7, 7];
            p2y = makeSpecialNumber[-7, 7];
            
            p1 = {p1x, p1y};
            p2 = {p2x, p2y};
            
            (* Zabezpe\[CHacek]\[IAcute]me, \[ZHacek]e body nie s\[UAcute] pr\[IAcute]li\[SHacek] bl\[IAcute]zko *)
            While[EuclideanDistance[p1, p2] < minDistance, 
              p2x = makeSpecialNumber[-7, 7];
              p2y = makeSpecialNumber[-7, 7];
              p2 = {p2x, p2y};
            ];
          ]
        ];
        
        (* Kontrola \[CHacek]i sme t\[UAcute]to priamku u\[ZHacek] niekedy nesk\[OHat]r nevygenerovali *)
        If[MemberQ[previousLines, Sort[{p1, p2}]],
          count++;
          If[Mod[count, 5] == 0, 
            generationType = RandomChoice[{0.85, 0.15} -> {1, 2}]
          ];
          Continue[];
        ];
        
        (* Kontrola validity *)
        If[validPriamkaQ[p1, p2],
          (* Prida\[THacek] do hist\[OAcute]rie *)
          AppendTo[previousLines, Sort[{p1, p2}]];
          If[Length[previousLines] > 20, previousLines = previousLines[[-20;;]]];
          
          (* Vr\[AAcute]ti\[THacek] v\[YAcute]sledok *)
          Return[{p1, p2}]
        ];
        
        (* Zmena typu po nieko\:013ek\[YAcute]ch pokusoch *)
        If[Mod[count, 5] == 0 && count > 0, 
          generationType = RandomChoice[{0.85, 0.15} -> {1, 2}]
        ];
        
        (* Z\[AAcute]chrann\[YAcute] mechanizmus - predpripraven\[EAcute] priamky *)
        If[count > 30, 
          Module[{simpleLines},
            simpleLines = {
              {{-4, -3}, {4, 5}},      (* Diagon\[AAcute]lna priamka 1 *)
              {{-5, 2}, {5, -2}},      (* Diagon\[AAcute]lna priamka 2 *)
              {{-6, 0}, {6, 0}},       (* Vodorovn\[AAcute] priamka *)
              {{0, -6}, {0, 6}},       (* Zvisl\[AAcute] priamka *)
              {{-3, -3}, {3, 3}},      (* Priamka y = x *)
              {{-3, 3}, {3, -3}},      (* Priamka y = -x *)
              {{-4, 1}, {4, 3}},       (* Priamka s mal\[YAcute]m sklonom *)
              {{2, -5}, {8, 5}},       (* Priamka v 1. kvadrante *)
              {{-8, -3}, {-2, 3}},     (* Priamka v 2. kvadrante *)
              {{-5, -2}, {-2, -5}},    (* Priamka v 3. kvadrante *)
              {{2, -4}, {6, -2}},      (* Priamka v 4. kvadrante *)
              (* Priamky so \[SHacek]peci\[AAcute]lnymi hodnotami *)
              {{-2, Sqrt[3]}, {2, -Sqrt[3]}},
              {{-3, 1/2}, {3, 5/2}},
              {{-Sqrt[5], -1}, {Sqrt[5], 2}}
            };
            
            (* N\[AAcute]hodne vyberieme z predpripraven\[YAcute]ch *)
            Return[RandomChoice[simpleLines]]
          ]
        ];
        count++
      ]
    ],
    (* Predvolen\[AAcute] priamka v pr\[IAcute]pade chyby *)
    {{-3, -2}, {3, 4}}
  ];
  
  (* Vr\[AAcute]ti\[THacek] v\[YAcute]sledok alebo predvolen\[UAcute] priamku *)
  If[Head[result] === List && Length[result] === 2 && 
     AllTrue[result, (VectorQ[#, NumberQ] && Length[#] == 2) &], 
    result, 
    {{-3, -2}, {3, 4}}
  ]
];

(* Verejn\[AAcute] funkcia na generovanie priamky *)
PriamkaGeneruj[] := GenerateInitialLine[];

(* Funkcia na v\[YAcute]po\[CHacek]et d\:013a\[ZHacek]ky priamky s presnou aritmetikou *)
priamkaLength[p1_, p2_] := Module[{dx, dy},
    dx = p2[[1]] - p1[[1]];
    dy = p2[[2]] - p1[[2]];
    
    (* Presn\[YAcute] v\[YAcute]po\[CHacek]et pomocou Pytagorovej vety *)
    Simplify[Sqrt[dx^2 + dy^2]]
];

(* Funkcia na form\[AAcute]tovanie \[CHacek]\[IAcute]sel pre kompletn\[EAcute] Unicode zna\[CHacek]enie *)
formatExpressionToUnicode[expr_] := 
    Module[{result},
        result = Which[
            (* Cel\[EAcute] \[CHacek]\[IAcute]sla *)
            IntegerQ[expr], 
                ToString[expr],
            
            (* Zlomky *)
            Head[expr] === Rational, 
                ToString[Numerator[expr]] <> "/" <> ToString[Denominator[expr]],
            
            (* \[CapitalCHacek]ist\[AAcute] odmocnina \[Sqrt]n *)
            Head[expr] === Power && Length[expr] >= 2 && expr[[2]] == 1/2 && IntegerQ[expr[[1]]],
                "\[Sqrt]" <> ToString[expr[[1]]],
            
            (* N\[AAcute]sobok odmocniny k\[Sqrt]n *)
            Head[expr] === Times && Length[expr] == 2 && 
                IntegerQ[expr[[1]]] && Head[expr[[2]]] === Power && 
                Length[expr[[2]]] >= 2 && expr[[2, 2]] == 1/2 && IntegerQ[expr[[2, 1]]],
                ToString[expr[[1]]] <> "\[Sqrt]" <> ToString[expr[[2, 1]]],
            
            (* Alternat\[IAcute]vna forma pre k*Sqrt[n] *)
            Head[expr] === Times && Length[expr] == 2 && 
                IntegerQ[expr[[1]]] && Head[expr[[2]]] === Sqrt && IntegerQ[expr[[2, 1]]],
                ToString[expr[[1]]] <> "\[Sqrt]" <> ToString[expr[[2, 1]]],
            
            (* In\[EAcute] pr\[IAcute]pady - zame\[NHacek]te "Sqrt" za unicode znak \[Sqrt] *)
            True,
                ToString[Simplify[expr], InputForm] /. {"Sqrt[" ~~ n__ ~~ "]" :> "\[Sqrt]" <> n}
        ];
        result
    ];

(* Funkcia na form\[AAcute]tovan\[EAcute] zobrazenie vlastnost\[IAcute] priamky s presnou aritmetikou *)
DisplayPriamkaProperties[points_, style_] := Module[{length, slope, yIntercept, xIntercept, equation, slopeStr, yInterceptStr},
    (* V\[YAcute]po\[CHacek]et d\:013a\[ZHacek]ky \[UAcute]se\[CHacek]ky *)
    length = priamkaLength @@ points;
    slope = priamkaSlope @@ points;
    yIntercept = priamkaYIntercept @@ points;
    
    (* Vytvorenie rovnice priamky *)
    equation = If[slope === ComplexInfinity,
        "x = " <> formatExpressionToUnicode[points[[1, 1]]],  (* Zvisl\[AAcute] priamka *)
        If[Abs[N[slope]] < 0.000001,
            "y = " <> formatExpressionToUnicode[points[[1, 2]]],  (* Vodorovn\[AAcute] priamka *)
            Block[{eqStr = "y = "},
                (* Smernica *)
                slopeStr = formatExpressionToUnicode[slope];
                If[slope == 1, 
                   eqStr = eqStr <> "x", 
                   If[slope == -1, 
                      eqStr = eqStr <> "-x", 
                      eqStr = eqStr <> slopeStr <> "x"
                   ]
                ];
                
                (* Abs. \[CHacek]len *)
                If[yIntercept != 0,
                    yInterceptStr = formatExpressionToUnicode[yIntercept];
                    If[yIntercept > 0, 
                       eqStr = eqStr <> " + " <> yInterceptStr,
                       eqStr = eqStr <> " - " <> formatExpressionToUnicode[Abs[yIntercept]]
                    ]
                ];
                
                eqStr
            ]
        ]
    ];
    
    Print[Style["Vlastnosti priamky:", Bold]];
    Print["D\:013a\[ZHacek]ka \[UAcute]se\[CHacek]ky: ", Style[formatExpressionToUnicode[length], style]];
    Print["Rovnica priamky: ", Style[equation, style]];
    
    (* Zobrazenie priese\[CHacek]n\[IAcute]kov s osami *)
    If[slope =!= ComplexInfinity && Abs[N[slope]] >= 0.000001,
        xIntercept = priamkaXIntercept @@ points;
        If[xIntercept =!= Indeterminate, 
            Print["Priese\[CHacek]n\[IAcute]k s osou x: ", Style["[" <> formatExpressionToUnicode[xIntercept] <> ", 0]", style]]
        ]
    ];
    
    If[slope =!= 0,
        If[yIntercept =!= Indeterminate, 
            Print["Priese\[CHacek]n\[IAcute]k s osou y: ", Style["[0, " <> formatExpressionToUnicode[yIntercept] <> "]", style]]
        ]
    ];
];

(* Pokro\[CHacek]il\[EAcute] form\[AAcute]tovanie v\[YAcute]razov pre lep\[SHacek]ie zobrazenie priamok *)
FormatPriamkaExpression[expr_] :=
    Module[{expandedExpr, simplifiedExpr},
        (* Rozvinutie v\[YAcute]razu *)
        expandedExpr = Expand[expr];
        
        (* Z\[AAcute]kladn\[EAcute] zjednodu\[SHacek]enie *)
        simplifiedExpr = Simplify[expandedExpr];
        
        (* Vr\[AAcute]ti\[THacek] v\[YAcute]sledok v preferovanej forme *)
        simplifiedExpr
    ];

(* Funkcia na spracovanie jedn\[EAcute]ho bodu priamky pre lep\[SHacek]ie zobrazenie *)
ProcessPriamkaPoint[point_] := 
    Module[{},
        (* Aplikovanie spracovania na ka\[ZHacek]d\[UAcute] s\[UAcute]radnicu *)
        Map[FormatPriamkaExpression, point]
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

(* Funkcia na spracovanie v\[SHacek]etk\[YAcute]ch bodov priamky s pln\[YAcute]m spracovan\[IAcute]m v\[YAcute]razov *)
ProcessPriamkaPointsComplete[points_] :=
    Map[Function[point, Map[FullExpressionProcessor, point]], points];

(* Vylep\[SHacek]en\[AAcute] funkcia na zobrazenie postupn\[EAcute]ho v\[YAcute]po\[CHacek]tu transform\[AAcute]ci\[IAcute] - PRE JEDNU TRANSFORM\[CapitalAAcute]CIU *)
DisplayTransformationSequence[pociatocna_, finalna_, prva_] := Module[{},
    Print[Style["POSTUPN\[CapitalYAcute] V\[CapitalYAcute]PO\[CapitalCHacek]ET:", Bold, 16]];
    
    (* Vlastnosti p\[OHat]vodnej priamky *)
    Print[Style["\nP\[OHat]vodn\[AAcute] priamka AB:", Bold, 14]];
    Print["Body priamky:"];
    Print[Style[MatrixForm[pociatocna], Blue]];
    DisplayPriamkaProperties[pociatocna, Blue];
    
    (* Vlastnosti po transform\[AAcute]cii *)
    Print[Style["\nPo transform\[AAcute]cii (" <> prva <> "):", Bold, 14]];
    Print["Body priamky A'B':"];
    Print[Style[MatrixForm[finalna], Green]];
    DisplayPriamkaProperties[finalna, Green];
];

(* Vytvorenie s\[UAcute]hrnnej vizualiz\[AAcute]cie jednej transform\[AAcute]cie *)
CreateSummaryVisualization[pociatocna_, finalna_] := 
    Module[{
        allVertices, xMin, xMax, yMin, yMax, rangeBuffer, xRange, yRange,
        blue = RGBColor[0, 0.4, 0.8],
        green = RGBColor[0.2, 0.8, 0.2],
        gridStep = 2, (* krok mrie\[ZHacek]ky a zna\[CHacek]iek na osiach *)
        labelOffset = 0.5 (* men\[SHacek]ia vzdialenos\[THacek] popisov bodov od samotn\[YAcute]ch bodov *)
    },
        
        (* Spracovanie v\[SHacek]etk\[YAcute]ch bodov pre vizualiz\[AAcute]ciu *)
        allVertices = Join[N[pociatocna], N[finalna]];
        
        (* V\[YAcute]po\[CHacek]et min a max hodn\[OHat]t pre stanovenie rozsahu *)
        xMin = Min[allVertices[[All, 1]]];
        xMax = Max[allVertices[[All, 1]]];
        yMin = Min[allVertices[[All, 2]]];
        yMax = Max[allVertices[[All, 2]]];
        
        (* Men\[SHacek]\[IAcute] buffer pre tesnej\[SHacek]\[IAcute] zoom na \[UAcute]se\[CHacek]ky *)
        rangeBuffer = 1;
        
        (* Nastavenie rozsahu os\[IAcute] - presnej\[SHacek]ie zoomovanie na \[UAcute]se\[CHacek]ky *)
        xRange = {Floor[xMin - rangeBuffer], Ceiling[xMax + rangeBuffer]};
        yRange = {Floor[yMin - rangeBuffer], Ceiling[yMax + rangeBuffer]};
        
        (* Vylep\[SHacek]en\[AAcute] funkcia pre v\[YAcute]po\[CHacek]et poz\[IAcute]cie popisku bodu - inteligentnej\[SHacek]ie ur\[CHacek]enie smeru *)
        getLabelPosition[point_, otherPoint_, color_] := Module[{
            dir, norm, offset, angle, dirToCenter, proposedPos, 
            allLines, intersects = False, i, dists
        },
            (* Smer od bodu k druh\[EAcute]mu bodu priamky *)
            dir = otherPoint - point;
            
            (* Normaliz\[AAcute]cia vektora *)
            norm = If[Norm[dir] > 0.001, Normalize[dir], {1, 0}];
            
            (* Uhol medzi priamkou a osou x *)
            angle = ArcTan[norm[[1]], norm[[2]]];
            
            (* Vytvor\[IAcute]me 4 mo\[ZHacek]n\[EAcute] poz\[IAcute]cie pre umiestnenie textu (vpravo, hore, v\:013eavo, dole) *)
            dists = {
                {0.5, 0}, {0, 0.5}, {-0.5, 0}, {0, -0.5}
            };
            
            (* Vyberieme poz\[IAcute]ciu, ktor\[AAcute] je najviac kolm\[AAcute] na smer priamky *)
            offset = dists[[Mod[Round[(angle + Pi)/(Pi/2)], 4] + 1]];
            
            (* Vr\[AAcute]time fin\[AAcute]lnu poz\[IAcute]ciu *)
            point + offset
        ];
        
        Graphics[{
            (* Mrie\[ZHacek]ka - prisp\[OHat]soben\[AAcute] rozsahu \[UAcute]dajov *)
            LightGray, Thin,
            Table[Line[{{xRange[[1]], y}, {xRange[[2]], y}}], 
                {y, Ceiling[yRange[[1]]/gridStep]*gridStep, Floor[yRange[[2]]/gridStep]*gridStep, gridStep}],
            Table[Line[{{x, yRange[[1]]}, {x, yRange[[2]]}}], 
                {x, Ceiling[xRange[[1]]/gridStep]*gridStep, Floor[xRange[[2]]/gridStep]*gridStep, gridStep}],
            
            (* Zna\[CHacek]ky na osiach - zobrazuj\[UAcute] sa cel\[EAcute] \[CHacek]\[IAcute]sla pre lep\[SHacek]iu \[CHacek]itate\:013enos\[THacek] *)
            Table[Text[Style[ToString[y], 9, LightGray], {0.3, y}, {-1, 0}], 
                {y, Ceiling[yRange[[1]]/gridStep]*gridStep, Floor[yRange[[2]]/gridStep]*gridStep, gridStep}],
            Table[Text[Style[ToString[x], 9, LightGray], {x, 0.3}, {0, -1}], 
                {x, Ceiling[xRange[[1]]/gridStep]*gridStep, Floor[xRange[[2]]/gridStep]*gridStep, gridStep}],
            
            (* \[CapitalCHacek]iary priamok - plnou \[CHacek]iarou *)
            {blue, Thick, Line[N[pociatocna]]},
            {green, Thick, Line[N[finalna]]},
            
            (* Body s bielym pozad\[IAcute]m pre lep\[SHacek]iu vidite\:013enos\[THacek] *)
            White, 
            Disk[N[pociatocna[[1]]], 0.3],
            Disk[N[pociatocna[[2]]], 0.3],
            Disk[N[finalna[[1]]], 0.3],
            Disk[N[finalna[[2]]], 0.3],
            
            (* Body ozna\[CHacek]en\[EAcute] pod\:013ea farby priamky *)
            {blue, PointSize[0.02], Point[N[pociatocna]]},
            {green, PointSize[0.02], Point[N[finalna]]},
            
            (* Popisky bodov - bli\[ZHacek]\[SHacek]ie k bodom *)
            {blue, Text[Style["A", Bold, 12], 
                getLabelPosition[N[pociatocna[[1]]], N[pociatocna[[2]]], blue], {0, 0}]},
            {blue, Text[Style["B", Bold, 12], 
                getLabelPosition[N[pociatocna[[2]]], N[pociatocna[[1]]], blue], {0, 0}]},
            
            {green, Text[Style["A'", Bold, 12], 
                getLabelPosition[N[finalna[[1]]], N[finalna[[2]]], green], {0, 0}]},
            {green, Text[Style["B'", Bold, 12], 
                getLabelPosition[N[finalna[[2]]], N[finalna[[1]]], green], {0, 0}]},
            
            (* Osi - prech\[AAcute]dzaj\[UAcute] cez za\[CHacek]iatok s\[UAcute]radnicovej s\[UAcute]stavy *)
            Black, Thick,
            Arrow[{{xRange[[1]], 0}, {xRange[[2]], 0}}],
            Arrow[{{0, yRange[[1]]}, {0, yRange[[2]]}}],
            Text[Style["x", Black, Bold], {xRange[[2]] - 0.5, -0.5}],
            Text[Style["y", Black, Bold], {-0.5, yRange[[2]] - 0.5}]
        },
        PlotRange -> {xRange, yRange},
        AspectRatio -> 1,
        ImageSize -> 600,
        ImagePadding -> 30,
        PlotLabel -> Style["Transform\[AAcute]cia priamky", Bold, 14]
        ]
    ];
    
(* Vytvorenie textovej legendy pre s\[UAcute]hrnn\[UAcute] vizualiz\[AAcute]ciu *)
CreateSummaryLegend[prva_] := 
    Module[{},
        Print[Style["LEGENDA:", Bold, 14]];
        Print[Row[{Style["\[Bullet] Modr\[EAcute]", Blue], " body a \[CHacek]iary: P\[OHat]vodn\[AAcute] priamka AB"}]];
        Print[Row[{Style["\[Bullet] Zelen\[EAcute]", Green], " body a \[CHacek]iary: Priamka A'B' po transform\[AAcute]cii (" <> prva <> ")"}]];
    ];
    
(* Funkcia na v\[YAcute]ber transform\[AAcute]cie s popisom *)
SelectTransformation[message_] := 
    Module[{options, fullOptions, formattedChoices, result},
        options = {
            "Posun" -> {"Posun", "Posun priamky vo smere vektora"},
            "Rot\[AAcute]cia" -> {"Rot\[AAcute]cia", "Rot\[AAcute]cia priamky okolo po\[CHacek]iatku s\[UAcute]radnicovej s\[UAcute]stavy"},
            "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie" -> {"Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie alebo zmen\[SHacek]enie priamky v r\[OHat]znych smeroch"},
            "Skosenie" -> {"Skosenie", "Skosenie priamky v smere os\[IAcute]"},
            "Symetria" -> {"Symetria", "Zrkadlenie priamky pod\:013ea zvolenej osi"}
        };
        
        (* Pou\[ZHacek]ijeme jednoduch\[SHacek]\[IAcute] pr\[IAcute]stup s ChoiceDialog, ale s vertik\[AAcute]lnym zoznamom *)
        ChoiceDialog[
            message,
            options,
            WindowTitle -> "V\[YAcute]ber transform\[AAcute]cie",
            WindowSize -> {500, All}
        ]
    ];

(* Funkcia na zobrazenie s\[UAcute]hrnn\[YAcute]ch inform\[AAcute]ci\[IAcute] o transform\[AAcute]cii s farebn\[YAcute]mi \[SHacek]t\[YAcute]lmi *)
DisplayTransformationSummary[pociatocnaPriamka_, finalnaPriamka_, prva_] := 
  Module[{
      blue = RGBColor[0, 0.4, 0.8],
      green = RGBColor[0.2, 0.8, 0.2],
      simplifiedFinalPriamka
    },
    
    (* Aplikovanie spracovania pre v\[YAcute]sledn\[EAcute] body *)
    simplifiedFinalPriamka = Map[
      Function[point, 
        Map[
          Function[coord, 
            Module[{expanded, simplified},
              expanded = Expand[coord];
              simplified = Simplify[expanded];
              simplified
            ]
          ],
          point
        ]
      ],
      finalnaPriamka
    ];
    
    Print[Style["S\[CapitalUAcute]HRN TRANSFORM\[CapitalAAcute]CIE:", Bold, 16]];
    Print[Row[{Style["P\[OHat]vodn\[AAcute] priamka AB: ", Bold], Style[MatrixForm[pociatocnaPriamka], blue, Bold]}]];
    Print[Row[{Style["Po transform\[AAcute]cii (", Bold], Style[prva, Bold, green], Style["): ", Bold], Style[MatrixForm[simplifiedFinalPriamka], green, Bold]}]];
  ];
  
(* Hlavn\[AAcute] funkcia pre jednu transform\[AAcute]ciu *)
PriamkaJednaTransformacia[] := Module[{
    pociatocnaPriamka,
    prvaTransformacia,
    finalnaPriamka,
    prvaPopis
    },
    
    (* Potla\[CHacek]enie chybov\[YAcute]ch spr\[AAcute]v *)
    Quiet[
    (* \[CapitalUAcute]vodn\[AAcute] spr\[AAcute]va - teraz \[CHacek]iernou farbou *)
    Print[Style["GEOMETRICK\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA PRIAMKY", Bold, 18]];
    Print[Style["==========================================", Bold]];
    
    (* Generovanie a zobrazenie po\[CHacek]iato\[CHacek]nej priamky *)
    pociatocnaPriamka = PriamkaGeneruj[];
    Print[Style["\nPO\[CapitalCHacek]IATO\[CapitalCHacek]N\[CapitalAAcute] PRIAMKA:", Bold, 16]];
    Print["Body priamky AB:"];
    Print[Style[MatrixForm[pociatocnaPriamka], Blue]];
    DisplayPriamkaProperties[pociatocnaPriamka, Blue];
    
    (* V\[YAcute]ber a aplik\[AAcute]cia transform\[AAcute]cie *)
    prvaPopis = SelectTransformation["Vyberte transform\[AAcute]ciu:"];
    
    If[prvaPopis === $Canceled, Return[]];
    prvaTransformacia = First[prvaPopis];
    
    Print[Style["\nTRANSFORM\[CapitalAAcute]CIA: " <> prvaTransformacia, Bold, 16]];
    
    finalnaPriamka = Switch[prvaTransformacia,
        "Posun", PriamkaPosun[pociatocnaPriamka],
        "Rot\[AAcute]cia", PriamkaRotacia[pociatocnaPriamka],
        "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", PriamkaZvacsenieZmensenie[pociatocnaPriamka],
        "Skosenie", PriamkaSkosenie[pociatocnaPriamka],
        "Symetria", PriamkaSymetria[pociatocnaPriamka]
    ];
    
    (* Zobrazenie postupn\[EAcute]ho v\[YAcute]po\[CHacek]tu transform\[AAcute]cie *)
    DisplayTransformationSequence[
        pociatocnaPriamka, 
        finalnaPriamka, 
        prvaTransformacia
    ];
    
    (* Zobrazenie s\[UAcute]hrnnej inform\[AAcute]cie *)
    DisplayTransformationSummary[
        pociatocnaPriamka, 
        finalnaPriamka, 
        prvaTransformacia
    ];
    
    (* S\[UAcute]hrnn\[AAcute] vizualiz\[AAcute]cia *)
    Print[Style["\nS\[CapitalUAcute]HRNN\[CapitalAAcute] VIZUALIZ\[CapitalAAcute]CIA:", Bold, 16]];
    Print[CreateSummaryVisualization[pociatocnaPriamka, finalnaPriamka]];
    
    (* Zobrazenie legendy *)
    CreateSummaryLegend[prvaTransformacia];
    
    (* Vr\[AAcute]ti\[THacek] inform\[AAcute]cie o transform\[AAcute]cii pre pou\[ZHacek]itie v in\[YAcute]ch funkci\[AAcute]ch *)
    {pociatocnaPriamka, finalnaPriamka, prvaTransformacia}
    ]  (* Ukon\[CHacek]enie Quiet bloku *)
];

(* Funkcia pre s\[UAcute]hrnn\[UAcute] vizualiz\[AAcute]ciu (bez dial\[OAcute]gu) s o\[SHacek]etren\[IAcute]m ch\[YAcute]b *)
PriamkaJednaTransformaciaSVysledkom[] := Module[{result},
   (* Pou\[ZHacek]itie Quiet na potla\[CHacek]enie chybov\[YAcute]ch spr\[AAcute]v *)
   result = Quiet[PriamkaJednaTransformacia[], {Power::infy, Infinity::indet}];
   
   (* Vr\[AAcute]ti\[THacek] Null aby sa nezobrazil Out[] *)
   Null
];

(* Na konci package obnovi\[THacek] norm\[AAcute]lne spr\[AAcute]vanie varovn\[YAcute]ch spr\[AAcute]v *)
On[Power::infy];
On[Infinity::indet];

End[]; (* Uzavretie Private kontextu *)
EndPackage[]; (* Uzavretie PriamkaEasyBalik package *)


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
