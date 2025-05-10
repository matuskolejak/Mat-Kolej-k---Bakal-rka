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


(* ::Package:: *)
(**)


BeginPackage["DomcekHardBalik`"];


DomcekTrojitaTransformacia::usage = 
    "DomcekTrojitaTransformacia[] umo\[ZHacek]n\[IAcute] postupn\[YAcute] v\[YAcute]ber a aplik\[AAcute]ciu troch transform\[AAcute]ci\[IAcute].";

DomcekTrojitaTransformaciaSVysledkom::usage = 
    "DomcekTrojitaTransformaciaSVysledkom[] umo\[ZHacek]n\[IAcute] postupn\[YAcute] v\[YAcute]ber a aplik\[AAcute]ciu troch transform\[AAcute]ci\[IAcute] a zobraz\[IAcute] s\[UAcute]hrnn\[YAcute] v\[YAcute]sledok.";

DomcekGeneruj::usage = 
    "DomcekGeneruj[] generuje n\[AAcute]hodn\[YAcute] dom\[CHacek]ek s vhodn\[YAcute]mi vlastnos\[THacek]ami.";


domcekArea::usage = 
    "domcekArea[p1, p2, p3, p4, p5] vypo\[CHacek]\[IAcute]ta obsah dom\[CHacek]eka s vrcholmi p1, p2, p3, p4 a p5.";

validDomcekQ::usage = 
    "validDomcekQ[p1, p2, p3, p4, p5] over\[IAcute], \[CHacek]i dom\[CHacek]ek sp\:013a\[NHacek]a krit\[EAcute]ri\[AAcute].";

domcekPerimeter::usage = 
    "domcekPerimeter[p1, p2, p3, p4, p5] vypo\[CHacek]\[IAcute]ta obvod dom\[CHacek]eka s vrcholmi p1, p2, p3, p4 a p5.";


DomcekTrojitaTransformacia::infinityerr = 
    "Vyskytla sa chyba: v\[YAcute]po\[CHacek]et viedol k nekone\[CHacek]n\[EAcute]mu alebo neur\[CHacek]it\[EAcute]mu v\[YAcute]sledku. Pou\[ZHacek]ije sa preddefinovan\[YAcute] dom\[CHacek]ek.";

Begin["`Private`"];


Off[Power::infy];
Off[Infinity::indet];


Needs["DomcekHardBalik`Transforms`Posun`"];
Needs["DomcekHardBalik`Transforms`Rotacia`"];
Needs["DomcekHardBalik`Transforms`ZvacsenieZmensenie`"];
Needs["DomcekHardBalik`Transforms`Skosenie`"];
Needs["DomcekHardBalik`Transforms`Symetria`"];


mildGreen = RGBColor[0.2, 0.6, 0.2];


domcekArea[p1_, p2_, p3_, p4_, p5_] := Module[{areaRect, areaTri, area},
    
    areaRect = Check[
        Abs[(p2[[1]] - p1[[1]]) * (p4[[2]] - p1[[2]])],
        $Failed
    ];
    
    
    areaTri = Check[
        Abs[(p3[[1]]*(p2[[2]] - p4[[2]]) + 
             p2[[1]]*(p4[[2]] - p3[[2]]) + 
             p4[[1]]*(p3[[2]] - p2[[2]]))/2],
        $Failed
    ];
    
    
    area = Check[areaRect + areaTri, $Failed];
    
    If[area === $Failed || area === ComplexInfinity || area === Indeterminate || !NumberQ[area],
        
        20,
        
        area
    ]
];


domcekPerimeter[p1_, p2_, p3_, p4_, p5_] := Module[{dist1, dist2, dist3, dist4, dist5},
    dist1 = Check[EuclideanDistance[p1, p2], $Failed];
    dist2 = Check[EuclideanDistance[p2, p3], $Failed];
    dist3 = Check[EuclideanDistance[p3, p4], $Failed];
    dist4 = Check[EuclideanDistance[p4, p5], $Failed];
    dist5 = Check[EuclideanDistance[p5, p1], $Failed];
    
    If[MemberQ[{dist1, dist2, dist3, dist4, dist5}, $Failed],
        
        30,
        
        dist1 + dist2 + dist3 + dist4 + dist5
    ]
];


domcekSides[p1_, p2_, p3_, p4_, p5_] := Module[{sides},
    sides = Check[{
        EuclideanDistance[p1, p2],
        EuclideanDistance[p2, p3],
        EuclideanDistance[p3, p4],
        EuclideanDistance[p4, p5],
        EuclideanDistance[p5, p1]
    }, $Failed];
    
    If[sides === $Failed,
        
        {5, 5, 5, 5, 5},
        
        sides
    ]
];


validDomcekQ[p1_, p2_, p3_, p4_, p5_] := 
    Module[{sides, area, minSide = 3, minArea = 15, maxArea = 120, isValid}, 
        
        If[!VectorQ[p1, NumberQ] || !VectorQ[p2, NumberQ] || !VectorQ[p3, NumberQ] || 
           !VectorQ[p4, NumberQ] || !VectorQ[p5, NumberQ] || 
           Length[p1] != 2 || Length[p2] != 2 || Length[p3] != 2 || 
           Length[p4] != 2 || Length[p5] != 2,
            Return[False]
        ];
        
        
        
        
        
        area = domcekArea[p1, p2, p3, p4, p5];
        If[area < 1.0, Return[False]]; 
        
        
        sides = domcekSides[p1, p2, p3, p4, p5];
        If[!VectorQ[sides, NumberQ], Return[False]];
        
        
        If[area === ComplexInfinity || area === Indeterminate, 
            Return[False]
        ];
        
        
        isValid = Check[
            
            minArea <= area <= maxArea && 
            
            Min[sides] >= minSide &&
            
            p3[[2]] > Max[p2[[2]], p4[[2]]] &&
            
            Abs[p1[[2]] - p5[[2]]] < 0.5 &&
            
            Abs[p2[[2]] - p4[[2]]] < 0.5,
            False
        ];
        
        isValid
    ];


GenerateInitialDomcek[] := Module[{
    p1, p2, p3, p4, p5, 
    count = 0, 
    defaultDomcek = {{-3, 0}, {-3, 3}, {0, 5}, {3, 3}, {3, 0}}, 
    result,
    previousDomceky = {},
    randomSeed,
    generationType,
    minDistance = 2.5,
    minArea = 15
  },
  
  
  randomSeed = Hash[{AbsoluteTime[], $TimeZone, $ProcessID, 
                     RandomInteger[{-10^7, 10^7}], StringJoin @@ 
                     ToString /@ RandomInteger[{0, 9}, 10]}];
  SeedRandom[randomSeed];
  
  
  Do[RandomInteger[{-1000, 1000}], {RandomInteger[{10, 30}]}];
  
  
  generationType = RandomChoice[{0.82, 0.18} -> {1, 2}];
  
  
  result = Check[
    Block[{$MessagePreprint = (Message[DomcekTrojitaTransformacia::infinityerr]; #)&},
      While[True,
        Switch[generationType,
          
          1, 
          Module[{range, nonZeroRandom, width, height, roofHeight},
            
            nonZeroRandom[min_, max_] := Module[{val},
              val = RandomInteger[{min, max}];
              If[val == 0, If[RandomReal[] < 0.5, -1, 1], val]
            ];
            
            
            range = RandomInteger[{5, 8}];
            
            
            width = RandomInteger[{4, 6}];
            height = RandomInteger[{3, 5}];
            roofHeight = RandomInteger[{2, 3}];
            
            
            xShift = RandomInteger[{-4, 4}];
            yShift = RandomInteger[{-2, 2}];
            
            
            p1 = {xShift - width/2, yShift};
            p5 = {xShift + width/2, yShift};
            p2 = {xShift - width/2, yShift + height};
            p4 = {xShift + width/2, yShift + height};
            p3 = {xShift, yShift + height + roofHeight};
          ],
          
          
          2,
          Module[{denominator, width, height, roofHeight, xShift, yShift},
            
            denominator = RandomChoice[{2, 3, 4}];
            
            
            width = Rationalize[RandomInteger[{8, 12}]/denominator];
            height = Rationalize[RandomInteger[{6, 10}]/denominator];
            roofHeight = Rationalize[RandomInteger[{4, 6}]/denominator];
            
            
            xShift = Rationalize[RandomInteger[{-8, 8}]/denominator];
            yShift = Rationalize[RandomInteger[{-4, 4}]/denominator];
            
            
            p1 = {xShift - width/2, yShift};
            p5 = {xShift + width/2, yShift};
            p2 = {xShift - width/2, yShift + height};
            p4 = {xShift + width/2, yShift + height};
            p3 = {xShift, yShift + height + roofHeight};
          ]
        ];
        
        domcekVrcholy = {p1, p2, p3, p4, p5};
        
        If[ContainsAny[{Sort[domcekVrcholy]}, previousDomceky],
          
          count++;
          If[Mod[count, 5] == 0, 
            generationType = RandomChoice[{0.8, 0.18, 0.02} -> {1, 2, 3}]
          ];
          Continue[]
        ];
        
        
        allDistances = {
          EuclideanDistance[p1, p2], EuclideanDistance[p2, p3], 
          EuclideanDistance[p3, p4], EuclideanDistance[p4, p5], 
          EuclideanDistance[p5, p1]
        };
        
        If[Min[allDistances] > minDistance &&
           domcekArea[p1, p2, p3, p4, p5] > minArea,
          
          
          If[validDomcekQ[p1, p2, p3, p4, p5] && Max[Abs[Flatten[domcekVrcholy]]] <= 10,
            
            AppendTo[previousDomceky, Sort[domcekVrcholy]];
            If[Length[previousDomceky] > 20, previousDomceky = previousDomceky[[-20;;]]];
            
            
            Return[domcekVrcholy]
          ]
        ];
        
        
        If[Mod[count, 5] == 0 && count > 0, 
          generationType = RandomChoice[{0.82, 0.18} -> {1, 2}]
        ];
        
        
        If[count > 50, 
          Module[{simpleDomceky},
            simpleDomceky = {
              
              {{-3, 0}, {-3, 3}, {0, 5}, {3, 3}, {3, 0}},             
              {{-4, -1}, {-4, 2}, {0, 4}, {4, 2}, {4, -1}},           
              {{-2, -2}, {-2, 1}, {0, 3}, {2, 1}, {2, -2}},           
              {{-3, -1}, {-3, 2}, {0, 4}, {3, 2}, {3, -1}},           
              {{-3, 0}, {-3, 2}, {0, 4}, {3, 2}, {3, 0}},             
              {{-4, 0}, {-4, 3}, {0, 6}, {4, 3}, {4, 0}}              
            };
            
            
            Return[RandomChoice[simpleDomceky]]
          ]
        ];
        count++
      ]
    ],
    
    {{-3, 0}, {-3, 3}, {0, 5}, {3, 3}, {3, 0}}
  ];
  
  
  If[Head[result] === List && Length[result] === 5 && AllTrue[result, VectorQ[#, NumberQ]&], 
    result, 
    {{-3, 0}, {-3, 3}, {0, 5}, {3, 3}, {3, 0}}  
  ]
];


DomcekGeneruj[] := GenerateInitialDomcek[];


DisplayDomcekProperties[vertices_, style_] := Module[{area, perimeter},
    
    area = Check[Round[domcekArea @@ vertices], 20];
    perimeter = Check[Round[domcekPerimeter @@ vertices], 30];
    
    
    If[!NumberQ[area] || area === ComplexInfinity || area === Indeterminate, area = 20];
    If[!NumberQ[perimeter] || perimeter === ComplexInfinity || perimeter === Indeterminate, perimeter = 30];
    
    Print[Style["Vlastnosti dom\[CHacek]eka:", Bold]];
    Print["Obsah: ", Style[area, style]];
    Print["Obvod: ", Style[perimeter, style]];
];


FormatDomcekExpression[expr_] :=
    Module[{expandedExpr, simplifiedExpr},
        
        expandedExpr = Expand[expr];
        
        
        simplifiedExpr = Simplify[expandedExpr];
        
        
        simplifiedExpr
    ];


ProcessDomcekVertex[vertex_] := 
    Module[{},
        
        Map[FormatDomcekExpression, vertex]
    ];


ExpandNestedExpressions[expr_] := 
    Module[{result = expr},
        
        If[Head[expr] === Times && Length[expr] >= 2 && 
           (MatchQ[expr[[1]], _Rational] || MatchQ[expr[[1]], _Integer]) &&
           MatchQ[expr[[2]], _Plus],
           
           result = Expand[expr];
        ];
        
        
        If[Head[result] === Plus, 
            result = Plus @@ Map[ExpandNestedExpressions, List @@ result],
            If[Head[result] === Times,
                result = Times @@ Map[ExpandNestedExpressions, List @@ result]
            ]
        ];
        
        result
    ];


FullExpressionProcessor[expr_] :=
    Module[{step1, step2},
        
        step1 = Expand[expr];
        
        
        step2 = ExpandNestedExpressions[step1];
        
        
        step2
    ];


ProcessDomcekVerticesComplete[vertices_] :=
    Map[Function[vertex, Map[FullExpressionProcessor, vertex]], vertices];


GetTransformationMatrix[transformType_, params_] := 
    Switch[transformType,
        "Posun", 
            Module[{dx = params[[1]], dy = params[[2]]},
                {{1, 0, dx}, {0, 1, dy}, {0, 0, 1}}
            ],
        
        "Rot\[AAcute]cia", 
            Module[{angle = params * Degree, c = Cos[params * Degree], s = Sin[params * Degree]},
                {{c, -s, 0}, {s, c, 0}, {0, 0, 1}}
            ],
        
        "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", 
            Module[{sx = params[[1]], sy = params[[2]]},
                {{sx, 0, 0}, {0, sy, 0}, {0, 0, 1}}
            ],
        
        "Skosenie", 
            Module[{kx = params[[1]], ky = params[[2]]},
                {{1, kx, 0}, {ky, 1, 0}, {0, 0, 1}}
            ],
        
        "Symetria", 
            
            Switch[params,
                "os x", {{1, 0, 0}, {0, -1, 0}, {0, 0, 1}},
                "os y", {{-1, 0, 0}, {0, 1, 0}, {0, 0, 1}},
                "priamka y=x", {{0, 1, 0}, {1, 0, 0}, {0, 0, 1}},
                "priamka y=-x", {{0, -1, 0}, {-1, 0, 0}, {0, 0, 1}},
                _, 
                If[ListQ[params] && Length[params] == 3,
                    
                    If[ValueQ[DomcekHardBalik`Transforms`Symetria`SymetriaMatrix],
                        
                        DomcekHardBalik`Transforms`Symetria`SymetriaMatrix,
                        
                        
                        Module[{a = params[[1]], b = params[[2]], c = params[[3]], factor},
                            factor = a^2 + b^2;
                            If[factor == 0, 
                                
                                {{1, 0, 0}, {0, -1, 0}, {0, 0, 1}},
                                
                                {
                                    {(b^2 - a^2)/factor, -2*a*b/factor, -2*a*c/factor},
                                    {-2*a*b/factor, (a^2 - b^2)/factor, -2*b*c/factor},
                                    {0, 0, 1}
                                }
                            ]
                        ]
                    ],
                    
                    {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}
                ]
            ]
    ];


ApplyTransformationMatrix[matrix_, point_] :=
    Module[{homogeneousPoint, result},
        
        homogeneousPoint = Append[point, 1];
        
        
        result = matrix . homogeneousPoint;
        
        
        Take[result, 2]
    ];


ApplyTransformationMatrixToDomcek[matrix_, domcek_] :=
    Map[ApplyTransformationMatrix[matrix, #] &, domcek];


CompositeTransformationMatrix[matrices_] :=
    Fold[Dot, IdentityMatrix[3], matrices];



DisplayCompositeMatrixCalculation[matrices_, transformNames_] :=
    Module[{result = IdentityMatrix[3], step, compositeSteps = {}, labels = {},
            matrixColors = {RGBColor[0, 0.4, 0.8], RGBColor[0.2, 0.7, 0.3], RGBColor[1, 0.6, 0]},
            operationColor = RGBColor[0.7, 0, 0.7], 
            resultColor = RGBColor[0.9, 0.1, 0.1],
            exactMatrices, intermediateResults = {}
            },
        
        
        exactMatrices = Map[ConvertMatrixToExactValues, matrices, {1}];
        
        Print[Style["V\[CapitalYAcute]PO\[CapitalCHacek]ET S\[CapitalUAcute]HRNNEJ TRANSFORMA\[CapitalCHacek]NEJ MATICE:", Bold, 16]];
        
        
        Print[Style["\nTransforma\[CHacek]n\[EAcute] matice pre jednotliv\[EAcute] transform\[AAcute]cie:", Bold, 14]];
        For[step = 1, step <= Length[exactMatrices], step++,
            Print[Style["Matica pre " <> transformNames[[step]] <> " (M" <> ToString[step] <> "):", Bold, matrixColors[[step]]]];
            
            Print[MatrixForm[Map[Style[FormatRationalElement[#], matrixColors[[step]]] &, exactMatrices[[step]], {2}]]];
        ];
        
        
        Print[Style["\nPostupn\[YAcute] v\[YAcute]po\[CHacek]et s\[UAcute]hrnnej matice:", Bold, 14]];
        compositeSteps = {exactMatrices[[1]]};
        
        
        Print[Style["Krok 1: Za\[CHacek]\[IAcute]name s maticou prvej transform\[AAcute]cie:", Bold]];
        Print[Style["M\:2081 = Matica pre " <> transformNames[[1]], Bold, matrixColors[[1]]]];
        Print[MatrixForm[Map[Style[FormatRationalElement[#], matrixColors[[1]]] &, exactMatrices[[1]], {2}]]];
        
        
        For[step = 2, step <= Length[exactMatrices], step++,
            result = compositeSteps[[-1]];
            
            
            Print[Style["\nKrok " <> ToString[step] <> ": N\[AAcute]sobenie mat\[IAcute]c", Bold]];
            
            
            If[step == 2,
                Print[Style["Pri kompoz\[IAcute]cii transform\[AAcute]ci\[IAcute] n\[AAcute]sob\[IAcute]me matice v opa\[CHacek]nom porad\[IAcute], ako sa aplikuj\[UAcute].", operationColor]];
                Print[Style["Matica pre aktu\[AAcute]lnu transform\[AAcute]ciu sa n\[AAcute]sob\[IAcute] s v\[YAcute]sledkom predch\[AAcute]dzaj\[UAcute]cich transform\[AAcute]ci\[IAcute].", operationColor]];
            ];
            
            
            Print[Style["M" <> ToString[step] <> " \[CenterDot] " <> 
                       If[step == 2, "M\:2081", "(M" <> ToString[step-1] <> " \[CenterDot] ... \[CenterDot] M\:2081)"] <> 
                       " = Matica pre " <> transformNames[[step]] <> " \[CenterDot] " <> 
                       If[step == 2, "Matica pre " <> transformNames[[1]], 
                           "V\[YAcute]sledok predch\[AAcute]dzaj\[UAcute]cich transform\[AAcute]ci\[IAcute] (M" <> ToString[step-1] <> " \[CenterDot] ... \[CenterDot] M\:2081)"], 
                 Bold, operationColor]];
            
            
            Print[Style["Matica pre " <> transformNames[[step]] <> " (M" <> ToString[step] <> "):", matrixColors[[step]]]];
            Print[MatrixForm[Map[Style[FormatRationalElement[#], matrixColors[[step]]] &, exactMatrices[[step]], {2}]]];
            
            
            If[step == 2,
                
                Print[Style["Matica prvej transform\[AAcute]cie (M\:2081):", matrixColors[[step-1]]]];
                Print[MatrixForm[Map[Style[FormatRationalElement[#], matrixColors[[step-1]]] &, result, {2}]]];
            ,
                
                Print[Style["V\[YAcute]sledok predch\[AAcute]dzaj\[UAcute]cich transform\[AAcute]ci\[IAcute] (M" <> ToString[step-1] <> " \[CenterDot] ... \[CenterDot] M\:2081):", matrixColors[[step-1]]]];
                Print[MatrixForm[Map[Style[FormatRationalElement[#], matrixColors[[step-1]]] &, result, {2}]]];
            ];
            
            
            Module[{newMatrix, errorOccurred = False},
                newMatrix = Check[
                    
                    Block[{$MaxExtraPrecision = 100},
                        Simplify[exactMatrices[[step]] . result]
                    ],
                    
                    (errorOccurred = True; ConstantArray[Indeterminate, {3, 3}]),
                    {Power::infy, Infinity::indet, General::munfl, General::ovfl}
                ];
                
                
                If[errorOccurred || MemberQ[Flatten[newMatrix], Indeterminate] || MemberQ[Flatten[newMatrix], ComplexInfinity],
                    newMatrix = Table[
                        Module[{sum = 0, i = row, j = col},
                            sum = Sum[
                                Block[{term},
                                    term = Simplify[exactMatrices[[step, i, k]] * result[[k, j]]];
                                    If[term === Indeterminate || term === ComplexInfinity,
                                        
                                        term = N[exactMatrices[[step, i, k]], 20] * N[result[[k, j]], 20];
                                        
                                        If[term === Indeterminate || term === ComplexInfinity, 0, term]
                                    ,
                                        term
                                    ]
                                ],
                                {k, 1, 3}
                            ];
                            Simplify[sum]
                        ],
                        {row, 1, 3}, {col, 1, 3}
                    ];
                ];
                
                
                Print[Style["\nV\[YAcute]po\[CHacek]et n\[AAcute]sobenia mat\[IAcute]c:", Bold]];
                For[i = 1, i <= 3, i++,
                    For[j = 1, j <= 3, j++,
                        Print[Style["C[" <> ToString[i] <> "," <> ToString[j] <> "] = ", Bold], 
                              Table[
                                  With[{term = Simplify[exactMatrices[[step, i, m]] * result[[m, j]]]},
                                      Row[{
                                          Style["(", operationColor], 
                                          Style[FormatRationalElement[exactMatrices[[step, i, m]]], matrixColors[[step]]], 
                                          Style[" \[Times] ", operationColor], 
                                          Style[FormatRationalElement[result[[m, j]]], matrixColors[[step-1]]], 
                                          Style[")", operationColor], 
                                          If[m < 3, Style[" + ", operationColor], ""]
                                      }]
                                  ],
                                  {m, 1, 3}
                              ], 
                              Style[" = ", operationColor], 
                              Style[FormatRationalElement[newMatrix[[i, j]]], resultColor]];
                    ];
                ];
                
                
                AppendTo[compositeSteps, newMatrix];
                
                
                AppendTo[intermediateResults, newMatrix];
                
                
                Print[Style["\nV\[YAcute]sledok n\[AAcute]sobenia:", Bold, resultColor]];
                Print[MatrixForm[Map[Style[FormatRationalElement[#], resultColor] &, newMatrix, {2}]]];
            ];
        ];
        
        
        Print[Style["\nV\[CapitalYAcute]SLEDN\[CapitalAAcute] S\[CapitalUAcute]HRNN\[CapitalAAcute] TRANSFORMA\[CapitalCHacek]N\[CapitalAAcute] MATICA:", Bold, 16, resultColor]];
        
        
        Module[{finalMatrix = compositeSteps[[-1]]},
            Print[MatrixForm[Map[Style[FormatRationalElement[#], resultColor] &, finalMatrix, {2}]]];
            
            
            {finalMatrix, compositeSteps}
        ]
    ];


SafeSimplifyMatrix[matrix_] := 
    Module[{result, i, j},
        result = Table[
            Check[
                Simplify[matrix[[i, j]]],
                matrix[[i, j]]  
            ],
            {i, 1, Length[matrix]},
            {j, 1, Length[matrix[[1]]]}
        ];
        result
    ];


ConvertMatrixToExactValues[matrix_] := 
    Module[{exactMatrix = matrix, i, j},
        For[i = 1, i <= Length[matrix], i++,
            For[j = 1, j <= Length[matrix[[i]]], j++,
                
                exactMatrix[[i, j]] = Check[
                    ConvertToExactValue[matrix[[i, j]]],
                    matrix[[i, j]],  
                    {Power::infy, Infinity::indet}
                ];
            ];
        ];
        exactMatrix
    ];


ConvertToExactValue[value_] := 
    Module[{trigValues, specialValues, exactValue, i, found = False, n},
        
        If[MatchQ[value, _Integer | _Rational | _Sqrt] || 
           Head[value] === Times && (MatchQ[value, _*Sqrt[_]] || MatchQ[value, _*Power[_, _Rational]]),
            Return[value]
        ];
        
        
        If[Head[value] === Symbol || Head[value] === Power && (MatchQ[value, Sqrt[_]] || MatchQ[value, Power[_, _Rational]]),
            Return[value]
        ];
        
        
        If[NumericQ[value],
            n = N[value];
            
            
            
            If[Abs[n - 0] < 10^-10, Return[0]];
            If[Abs[n - 1] < 10^-10, Return[1]];
            If[Abs[n - 1/2] < 10^-10, Return[1/2]];
            If[Abs[n - Sqrt[3]/2] < 10^-10, Return[Sqrt[3]/2]];
            If[Abs[n - Sqrt[2]/2] < 10^-10, Return[Sqrt[2]/2]];
            
            
            If[Abs[n + 1] < 10^-10, Return[-1]];            
            If[Abs[n + 1/2] < 10^-10, Return[-1/2]];
            If[Abs[n + Sqrt[3]/2] < 10^-10, Return[-Sqrt[3]/2]];
            If[Abs[n + Sqrt[2]/2] < 10^-10, Return[-Sqrt[2]/2]];
            If[Abs[n - 1/Sqrt[2]] < 10^-10, Return[1/Sqrt[2]]];
            If[Abs[n + 1/Sqrt[2]] < 10^-10, Return[-1/Sqrt[2]]];
            
            
            exactValue = Check[
                Rationalize[n, 10^-10],
                
                Check[
                    Rationalize[n, 10^-5],
                    
                    Check[
                        Rationalize[n, 10^-3],
                        
                        n
                    ]
                ]
            ];
            
            
            If[Abs[exactValue - Sqrt[2]] < 10^-3, exactValue = Sqrt[2]];
            If[Abs[exactValue + Sqrt[2]] < 10^-3, exactValue = -Sqrt[2]];
            If[Abs[exactValue - Sqrt[3]] < 10^-3, exactValue = Sqrt[3]];
            If[Abs[exactValue + Sqrt[3]] < 10^-3, exactValue = -Sqrt[3]];
            
            Return[exactValue];
        ];
        
        
        If[Head[value] === Complex,
            Return[Complex[
                ConvertToExactValue[Re[value]], 
                ConvertToExactValue[Im[value]]
            ]];
        ];
        
        
        value
    ];
    

FormatRationalElement[element_] := Module[{formattedValue},
    formattedValue = Check[
        
        Simplify[element],
        
        element,
        {Power::infy, Infinity::indet}
    ];
    
    
    Check[
        TraditionalForm[formattedValue],
        
        formattedValue,
        {Power::infy, Infinity::indet}
    ]
];


SafeMatrixMultiply[matrixA_, matrixB_] := 
    Module[{result, i, j, k, sum},
        
        If[Length[matrixA[[1]]] != Length[matrixB],
            
            Message[MatrixMultiplication::dims];
            Return[$Failed];
        ];
        
        
        result = ConstantArray[0, {Length[matrixA], Length[matrixB[[1]]]}];
        
        
        For[i = 1, i <= Length[matrixA], i++,
            For[j = 1, j <= Length[matrixB[[1]]], j++,
                sum = 0;
                For[k = 1, k <= Length[matrixB], k++,
                    
                    sum = Check[
                        sum + matrixA[[i, k]] * matrixB[[k, j]],
                        
                        sum + N[matrixA[[i, k]], 20] * N[matrixB[[k, j]], 20],
                        {Power::infy, Infinity::indet}
                    ];
                ];
                
                result[[i, j]] = Check[
                    Simplify[sum],
                    sum,  
                    {Power::infy, Infinity::indet}
                ];
            ];
        ];
        
        result
    ];


VerifyCompositeTransformation[originalDomcek_, compositeMatrix_, finalDomcek_, compositeSteps_:{}] :=
    Module[{calculatedDomcek, i, exactCompositeMatrix, exactOriginalDomcek, exactFinalDomcek},
        
        exactCompositeMatrix = compositeMatrix;
        
        
        exactOriginalDomcek = originalDomcek;
        exactFinalDomcek = finalDomcek;
        
        Print[Style["\nAPLIK\[CapitalAAcute]CIA S\[CapitalUAcute]HRNNEJ MATICE NA P\[CapitalOHat]VODN\[CapitalYAcute] DOM\[CapitalCHacek]EK:", Bold, 16]];
        
        
        Print[Style["\nS\[UAcute]hrnn\[AAcute] transforma\[CHacek]n\[AAcute] matica:", Bold, 14, RGBColor[0.9, 0.1, 0.1]]];
        Print[MatrixForm[Map[Style[#, RGBColor[0.9, 0.1, 0.1]] &, exactCompositeMatrix, {2}]]];
        
        
        Print[Style["\nP\[OHat]vodn\[YAcute] dom\[CHacek]ek ABCDE:", Bold, Blue, 14]];
        Print[TableForm[
            exactOriginalDomcek,
            TableHeadings -> {{"A", "B", "C", "D", "E"}, {"x", "y"}},
            TableAlignments -> Center
        ]];
        
        
        calculatedDomcek = Table[
            Module[{vertex = exactOriginalDomcek[[i]], homogeneousVertex, resultVector},
                homogeneousVertex = Append[vertex, 1];
                
                
                resultVector = exactCompositeMatrix . homogeneousVertex;
                
                
                Take[resultVector, 2]
            ],
            {i, Length[exactOriginalDomcek]}
        ];
        
        
        Print[Style["\nPostup pri aplikovan\[IAcute] s\[UAcute]hrnnej transforma\[CHacek]nej matice na dom\[CHacek]ek:", Bold, 14]];
        Print["1. Ka\[ZHacek]d\[YAcute] vrchol (x, y) sa roz\[SHacek]\[IAcute]ri na homog\[EAcute]nne s\[UAcute]radnice (x, y, 1)"];
        Print["2. Vyn\[AAcute]sob\[IAcute] sa s\[UAcute]hrnnou transforma\[CHacek]nou maticou 3\[Times]3"];
        Print["3. Z v\[YAcute]sledn\[EAcute]ho vektora vezmeme prv\[EAcute] dve s\[UAcute]radnice, \[CHacek]\[IAcute]m dostaneme transformovan\[YAcute] vrchol"];
        
        
        Print[Style["\nPODROBN\[CapitalYAcute] V\[CapitalYAcute]PO\[CapitalCHacek]ET PRE KA\[CapitalZHacek]D\[CapitalYAcute] VRCHOL:", Bold, 16]];
        
        For[i = 1, i <= Length[exactOriginalDomcek], i++,
            Module[{vertex = exactOriginalDomcek[[i]], homogeneousVertex, resultVector, label},
                homogeneousVertex = Append[vertex, 1];
                resultVector = exactCompositeMatrix . homogeneousVertex;
                
                
                label = Switch[i, 1, "A", 2, "B", 3, "C", 4, "D", 5, "E"];
                
                
                Print[Style["\nVrchol " <> label <> ":", Bold, Blue, 14]];
                Print["P\[OHat]vodn\[EAcute] s\[UAcute]radnice: ", Style[vertex, Blue]];
                Print["V homog\[EAcute]nnych s\[UAcute]radniciach: ", Style[homogeneousVertex, Blue]];
                
                
                Print[Style["\nMaticov\[EAcute] n\[AAcute]sobenie:", Bold]];
                Print[Row[{
                    Style["M \[CenterDot] ", RGBColor[0.5, 0, 0.5]], 
                    Style[label, Blue], 
                    Style[" = ", RGBColor[0.5, 0, 0.5]]
                }]];
                
                
                Print[Row[{
                    Style[MatrixForm[exactCompositeMatrix], RGBColor[0.9, 0.1, 0.1]], 
                    Style[" \[CenterDot] ", RGBColor[0.5, 0, 0.5]], 
                    Style[MatrixForm[homogeneousVertex], Blue]
                }]];
                
                
                Print[Style["\nPodrobn\[YAcute] v\[YAcute]po\[CHacek]et:", Bold]];
                
                Module[{j, row, rowResult},
                    For[j = 1, j <= 3, j++,
                        row = exactCompositeMatrix[[j]];
                        rowResult = Sum[row[[k]] * homogeneousVertex[[k]], {k, 1, 3}];
                        
                        
                        Print[Style["Riadok " <> ToString[j] <> ":", Bold]];
                        Print[Row[{
                            Style[row[[1]], RGBColor[0.9, 0.1, 0.1]], 
                            Style[" \[CenterDot] ", RGBColor[0.5, 0, 0.5]], 
                            Style[homogeneousVertex[[1]], Blue], 
                            Style[" + ", RGBColor[0.5, 0, 0.5]],
                            
                            Style[row[[2]], RGBColor[0.9, 0.1, 0.1]], 
                            Style[" \[CenterDot] ", RGBColor[0.5, 0, 0.5]], 
                            Style[homogeneousVertex[[2]], Blue], 
                            Style[" + ", RGBColor[0.5, 0, 0.5]],
                            
                            Style[row[[3]], RGBColor[0.9, 0.1, 0.1]], 
                            Style[" \[CenterDot] ", RGBColor[0.5, 0, 0.5]], 
                            Style[homogeneousVertex[[3]], Blue], 
                            Style[" = ", RGBColor[0.5, 0, 0.5]],
                            
                            Style[rowResult, RGBColor[0, 0.6, 0]]
                        }]];
                    ];
                ];
                
                
                Print[Style["\nV\[YAcute]sledok pre vrchol " <> label <> ":", Bold]];
                Print["V homog\[EAcute]nnych s\[UAcute]radniciach: ", Style[resultVector, RGBColor[0, 0.6, 0]]];
                
                Module[{transformedPoint},
                    transformedPoint = Take[resultVector, 2];
                    
                    Print["Transformovan\[EAcute] s\[UAcute]radnice (", label, "'): ", 
                         Style[transformedPoint, RGBColor[0, 0.6, 0]]];
                ];
                
                
                Print["O\[CHacek]ak\[AAcute]van\[YAcute] v\[YAcute]sledok (", label, "'): ", 
                     Style[exactFinalDomcek[[i]], RGBColor[0.9, 0.1, 0.1]]];
                
                
                Module[{difference, tolerance = 10^-4},
                    difference = Norm[Take[resultVector, 2] - exactFinalDomcek[[i]]];
                    
                    If[difference < tolerance,
                        Print[Style["\[Checkmark] V\[YAcute]sledok sa zhoduje s o\[CHacek]ak\[AAcute]van\[YAcute]m bodom.", RGBColor[0.2, 0.6, 0.2], Bold]],
                        Print[Style["\:2717 V\[YAcute]sledok sa nezhoduje s o\[CHacek]ak\[AAcute]van\[YAcute]m bodom!", RGBColor[0.9, 0, 0], Bold]]
                    ];
                ];
            ];
        ];
        
        
        Print[Style["\nV\[CapitalYAcute]SLEDN\[CapitalYAcute] TRANSFORMOVAN\[CapitalYAcute] DOM\[CapitalCHacek]EK A'B'C'D'E':", Bold, 16, RGBColor[0, 0.6, 0]]];
        Print[TableForm[
            calculatedDomcek,
            TableHeadings -> {{"A'", "B'", "C'", "D'", "E'"}, {"x", "y"}},
            TableAlignments -> Center
        ]];
        
        
        Print[Style["\nO\[CHacek]ak\[AAcute]van\[YAcute] v\[YAcute]sledn\[YAcute] dom\[CHacek]ek:", Bold, 14, RGBColor[0.9, 0.1, 0.1]]];
        Print[TableForm[
            exactFinalDomcek,
            TableHeadings -> {{"A'", "B'", "C'", "D'", "E'"}, {"x", "y"}},
            TableAlignments -> Center
        ]];
        
        
        Module[{difference, tolerance = 10^-4},
            difference = Norm[Flatten[calculatedDomcek] - Flatten[exactFinalDomcek]];
            
            If[difference < tolerance,
                Print[Style["\n\[Checkmark] OVERENIE \[CapitalUAcute]SPE\[CapitalSHacek]N\[CapitalEAcute]: Transform\[AAcute]cia pomocou s\[UAcute]hrnnej matice d\[AAcute]va spr\[AAcute]vny v\[YAcute]sledok.", 
                      RGBColor[0.2, 0.6, 0.2], Bold, 16]],
                Print[Style["\n\:2717 OVERENIE NE\[CapitalUAcute]SPE\[CapitalSHacek]N\[CapitalEAcute]: Transform\[AAcute]cia pomocou s\[UAcute]hrnnej matice ned\[AAcute]va o\[CHacek]ak\[AAcute]van\[YAcute] v\[YAcute]sledok!", 
                      RGBColor[0.9, 0, 0], Bold, 16]]
            ];
        ];
        
        
        Print[Style["\nZ\[CapitalAAcute]VER:", Bold, 16]];
        Print["Pou\[ZHacek]it\[IAcute]m jedinej transform\[AAcute]cie definovanej s\[UAcute]hrnnou maticou sme dosiahli rovnak\[YAcute] v\[YAcute]sledok,"];
        Print["ako postupn\[YAcute]m aplikovan\[IAcute]m viacer\[YAcute]ch transform\[AAcute]ci\[IAcute]. Tento princ\[IAcute]p je k\:013e\[UAcute]\[CHacek]ov\[YAcute]"];
        Print["v po\[CHacek]\[IAcute]ta\[CHacek]ovej grafike a umo\[ZHacek]\[NHacek]uje efekt\[IAcute]vne vykres\:013eovanie zlo\[ZHacek]it\[YAcute]ch transform\[AAcute]ci\[IAcute]."];
    ];
    

DisplayTransformationSequence[pociatocny_, druhy_, treti_, finalny_, prva_, druha_, tretia_, difficultyLevel_, transformParams_] := Module[{},
    Print[Style["\nPOSTUPN\[CapitalYAcute] V\[CapitalYAcute]PO\[CapitalCHacek]ET:", Bold, 16]];
    
    
    Print[Style["\nP\[OHat]vodn\[YAcute] dom\[CHacek]ek ABCDE:", Bold, 14]];
    Print["Vrcholy:"];
    Print[MatrixForm[Map[Style[#, Blue] &, pociatocny, {2}]]];
    DisplayDomcekProperties[pociatocny, Blue];
    
    
    If[prva == "Posun",
        
        Module[{dx, dy},
            dx = druhy[[1, 1]] - pociatocny[[1, 1]];
            dy = druhy[[1, 2]] - pociatocny[[1, 2]];
            Print[Style["\nVektor posunu: \[CapitalDelta] = " <> ToString[InputForm[{dx, dy}]], Bold, Blue]];
        ]
    ];
    
    
    Print[Style["\nPo prvej transform\[AAcute]cii (" <> prva <> "):", Bold, 14]];
    Print["Vrcholy dom\[CHacek]eka A'B'C'D'E':"];
    
    
    Module[{processedVertices},
        processedVertices = ProcessDomcekVerticesComplete[druhy];
        
        Print[MatrixForm[Map[Style[#, mildGreen] &, processedVertices, {2}]]];
    ];
    
    DisplayDomcekProperties[druhy, mildGreen];
    
    
    If[difficultyLevel == "Medium" || difficultyLevel == "Hard",
        
        Print[Style["\nPo druhej transform\[AAcute]cii (" <> druha <> "):", Bold, 14]];
        Print["Vrcholy dom\[CHacek]eka A''B''C''D''E'':"];
        
        
        Module[{processedVertices},
            processedVertices = ProcessDomcekVerticesComplete[treti];
            
            Print[MatrixForm[Map[Style[#, Orange] &, processedVertices, {2}]]];
        ];
        
        DisplayDomcekProperties[treti, Orange];
    ];
    
    
    If[difficultyLevel == "Hard",
        
        Print[Style["\nPo tretej transform\[AAcute]cii (" <> tretia <> "):", Bold, 14]];
        Print["Vrcholy dom\[CHacek]eka A'''B'''C'''D'''E''':"];
        
        
        Module[{processedVertices},
            processedVertices = ProcessDomcekVerticesComplete[finalny];
            
            Print[MatrixForm[Map[Style[#, Red] &, processedVertices, {2}]]];
        ];
        
        DisplayDomcekProperties[finalny, Red];
    ];
    
    
    
    If[difficultyLevel == "Medium" || difficultyLevel == "Hard",
        Module[{transformMatrices = {}, transformNames = {}, compositeMatrixResult},
            
            AppendTo[transformMatrices, 
                GetTransformationMatrix[prva, transformParams[[1]]]];
            AppendTo[transformNames, prva];
            
            If[difficultyLevel == "Medium" || difficultyLevel == "Hard",
                AppendTo[transformMatrices, 
                    GetTransformationMatrix[druha, transformParams[[2]]]];
                AppendTo[transformNames, druha];
            ];
            
            If[difficultyLevel == "Hard",
                AppendTo[transformMatrices, 
                    GetTransformationMatrix[tretia, transformParams[[3]]]];
                AppendTo[transformNames, tretia];
            ];
            
            
            compositeMatrixResult = DisplayCompositeMatrixCalculation[transformMatrices, transformNames];
            
            
            VerifyCompositeTransformation[pociatocny, compositeMatrixResult[[1]], finalny, compositeMatrixResult[[2]]];
        ]
    ];
];


FormatTransformationDetailedDescription[transformType_, params_] := 
    Switch[transformType,
        "Posun", 
            
            Module[{formattedVector},
                formattedVector = "{" <> ToString[TraditionalForm[params[[1]]]] <> ", " <> 
                               ToString[TraditionalForm[params[[2]]]] <> "}";
                Row[{"Vykonajte posun dom\[CHacek]eka v 2D priestore pomocou vektora posunu ", 
                     Style[formattedVector, Blue, Bold]}]
            ],
                 
        "Rot\[AAcute]cia", 
            Row[{"Vykonajte rot\[AAcute]ciu dom\[CHacek]eka v 2D priestore okolo po\[CHacek]iatku s\[UAcute]radnicovej s\[UAcute]stavy [0,0] o uhol ", 
                 Style[ToString[TraditionalForm[params]] <> "\[Degree]", Blue, Bold]}],
                 
        "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie",
            Module[{sx = params[[1]], sy = params[[2]]},
                Row[{
                    "Vykonajte ",
                    If[sx < 1, 
                        Row[{"zmen\[SHacek]enie v smere osi x s koeficientom ", 
                             Style[FormatRationalNumber[sx], Blue, Bold]}],
                        Row[{"zv\[ADoubleDot]\[CHacek]\[SHacek]enie v smere osi x s koeficientom ", 
                             Style[FormatRationalNumber[sx], Blue, Bold]}]
                    ],
                    " a ",
                    If[sy < 1, 
                        Row[{"zmen\[SHacek]enie v smere osi y s koeficientom ", 
                             Style[FormatRationalNumber[sy], Blue, Bold]}],
                        Row[{"zv\[ADoubleDot]\[CHacek]\[SHacek]enie v smere osi y s koeficientom ", 
                             Style[FormatRationalNumber[sy], Blue, Bold]}]
                    ],
                    " dom\[CHacek]eka"
                }]
            ],
            
        "Skosenie",
            With[{
                kx = params[[1]],
                ky = params[[2]]
            },
                If[kx != 0 && ky == 0,
                    Row[{"Vykonajte skosenie dom\[CHacek]eka v 2D priestore v smere osi x s koeficientom ", 
                         Style[FormatRationalNumber[kx], Blue, Bold]}],
                    If[kx == 0 && ky != 0,
                        Row[{"Vykonajte skosenie dom\[CHacek]eka v 2D priestore v smere osi y s koeficientom ", 
                             Style[FormatRationalNumber[ky], Blue, Bold]}],
                        Row[{"Vykonajte skosenie dom\[CHacek]eka v 2D priestore v smere osi x s koeficientom ", 
                             Style[FormatRationalNumber[kx], Blue, Bold], 
                             " a v smere osi y s koeficientom ", 
                             Style[FormatRationalNumber[ky], Blue, Bold]}]
                    ]
                ]
            ],
            
        "Symetria",
            Switch[params,
                "os x", "Vykonajte symetriu dom\[CHacek]eka v 2D priestore pod\:013ea osi x (priamky y = 0)",
                "os y", "Vykonajte symetriu dom\[CHacek]eka v 2D priestore pod\:013ea osi y (priamky x = 0)",
                "priamka y=x", "Vykonajte symetriu dom\[CHacek]eka v 2D priestore pod\:013ea priamky y = x",
                "priamka y=-x", "Vykonajte symetriu dom\[CHacek]eka v 2D priestore pod\:013ea priamky y = -x",
                "complex", 
                    "Vykonajte symetriu dom\[CHacek]eka v 2D priestore pod\:013ea v\[SHacek]eobecnej priamky",
                _,
                If[ListQ[params] && Length[params] == 3,
                    Module[{a, b, c, standardForm},
                        a = params[[1]];
                        b = params[[2]];
                        c = params[[3]];
                        
                        
                        standardForm = ToString[TraditionalForm[a]] <> "x ";
                        standardForm = standardForm <> If[b >= 0, "+ ", ""] <> ToString[TraditionalForm[b]] <> "y ";
                        If[c != 0, standardForm = standardForm <> If[c >= 0, "+ ", ""] <> ToString[TraditionalForm[c]]];
                        standardForm = standardForm <> " = 0";
                        
                        
                        Row[{
                            "Vykonajte symetriu dom\[CHacek]eka v 2D priestore pod\:013ea priamky ",
                            Style[standardForm, Blue, Bold]
                        }]
                    ],
                    
                    "Vykonajte symetriu dom\[CHacek]eka v 2D priestore pod\:013ea danej priamky"
                ]
            ],
            
        _,
            "Vykonajte " <> transformType <> " dom\[CHacek]eka"
    ];



FormatTransformationParameters[transformType_, params_] := Module[{formattedStr},
    Switch[transformType,
        "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", 
            
            formattedStr = ToString[TraditionalForm[params[[1]]]] <> ", " <> 
                           ToString[TraditionalForm[params[[2]]]];
            "s parametrami {" <> formattedStr <> "}",
            
        "Skosenie", 
            
            formattedStr = ToString[TraditionalForm[params[[1]]]] <> ", " <> 
                           ToString[TraditionalForm[params[[2]]]];
            "s parametrami {" <> formattedStr <> "}",
            
        "Posun", 
            
            "o vektor " <> ToString[InputForm[params]],
            
        "Rot\[AAcute]cia", 
            
            "o uhol " <> ToString[params] <> "\[Degree]",
            
        "Symetria", 
            
            Switch[params,
                "os x", "pod\:013ea osi x",
                "os y", "pod\:013ea osi y",
                "priamka y=x", "pod\:013ea priamky y = x",
                "priamka y=-x", "pod\:013ea priamky y = -x",
                _,
                If[ListQ[params] && Length[params] == 3,
                    Module[{a = params[[1]], b = params[[2]], c = params[[3]], eqStr},
                        
                        eqStr = ToString[TraditionalForm[a]] <> "x " <> 
                                If[b >= 0, "+ ", ""] <>
                                ToString[TraditionalForm[b]] <> "y " <> 
                                If[c >= 0, "+ ", ""] <>
                                ToString[TraditionalForm[c]] <> " = 0";
                        "pod\:013ea priamky " <> eqStr
                    ],
                    "pod\:013ea " <> ToString[params]
                ]
            ],
            
        _, 
            ""
    ]
];


CreateDomcekVisualization[pociatocny_, druhy_, treti_, finalny_, prva_, druha_, tretia_] := 
    Module[{
            allVertices, xMin, xMax, yMin, yMax, 
            xRange, yRange, padding = 1,
            brightBlue = RGBColor[0, 0.4, 0.8],
            brightGreen = RGBColor[0.2, 0.7, 0.3],
            brightOrange = RGBColor[1, 0.6, 0],
            brightRed = RGBColor[0.9, 0.1, 0.1],
            lightGray = RGBColor[0.9, 0.9, 0.9],
            labelPositions,
            maxRange, xMid, yMid,
            showSecond = druha != "\[CapitalZHacek]iadna",
            showThird = tretia != "\[CapitalZHacek]iadna",
            numTransformations = 1 + If[druha != "\[CapitalZHacek]iadna", 1, 0] + If[tretia != "\[CapitalZHacek]iadna", 1, 0]
        },
            
            
            allVertices = Join[pociatocny, druhy];
            If[showSecond, allVertices = Join[allVertices, treti]];
            If[showThird, allVertices = Join[allVertices, finalny]];
            
            xMin = Min[N[allVertices[[All, 1]]]];
            xMax = Max[N[allVertices[[All, 1]]]];
            yMin = Min[N[allVertices[[All, 2]]]];
            yMax = Max[N[allVertices[[All, 2]]]];
            
            
            xRange = {xMin - padding, xMax + padding};
            yRange = {yMin - padding, yMax + padding};
            
            maxRange = Max[xRange[[2]] - xRange[[1]], yRange[[2]] - yRange[[1]]];
            xMid = Mean[xRange];
            yMid = Mean[yRange];
            
            xRange = {xMid - maxRange/2, xMid + maxRange/2};
            yRange = {yMid - maxRange/2, yMid + maxRange/2};
            
            
            
            labelPositions = {};
            
            
            AppendTo[labelPositions, Table[
                Module[{vertex = pociatocny[[i]], offset = {0.35, 0.35}, nearby = {}},
                    If[N[vertex[[1]]] > 0, offset[[1]] = 0.35, offset[[1]] = -0.35];
                    If[N[vertex[[2]]] > 0, offset[[2]] = 0.35, offset[[2]] = -0.35];
                    offset = offset * (0.8 + 0.07 * EuclideanDistance[vertex, {0, 0}]);
                    vertex + offset
                ],
                {i, 5}
            ]];
            
            
            AppendTo[labelPositions, Table[
                Module[{vertex = druhy[[i]], offset = {0.35, 0.35}, nearby = {}},
                    If[N[vertex[[1]]] > 0, offset[[1]] = 0.35, offset[[1]] = -0.35];
                    If[N[vertex[[2]]] > 0, offset[[2]] = 0.35, offset[[2]] = -0.35];
                    offset = offset * (0.8 + 0.07 * EuclideanDistance[vertex, {0, 0}]);
                    vertex + offset
                ],
                {i, 5}
            ]];
            
            
            If[showSecond,
                AppendTo[labelPositions, Table[
                    Module[{vertex = treti[[i]], offset = {0.35, 0.35}, nearby = {}},
                        If[N[vertex[[1]]] > 0, offset[[1]] = 0.35, offset[[1]] = -0.35];
                        If[N[vertex[[2]]] > 0, offset[[2]] = 0.35, offset[[2]] = -0.35];
                        offset = offset * (0.8 + 0.07 * EuclideanDistance[vertex, {0, 0}]);
                        vertex + offset
                    ],
                    {i, 5}
                ]];
            ];
            
            
            If[showThird,
                AppendTo[labelPositions, Table[
                    Module[{vertex = finalny[[i]], offset = {0.35, 0.35}, nearby = {}},
                        If[N[vertex[[1]]] > 0, offset[[1]] = 0.35, offset[[1]] = -0.35];
                        If[N[vertex[[2]]] > 0, offset[[2]] = 0.35, offset[[2]] = -0.35];
                        offset = offset * (0.8 + 0.07 * EuclideanDistance[vertex, {0, 0}]);
                        vertex + offset
                    ],
                    {i, 5}
                ]];
            ];
            
            
            labelPositions = Flatten[labelPositions, 1];
            
            graphicsElements = {
                
                lightGray, Thin,
                Table[Line[{{xRange[[1]], y}, {xRange[[2]], y}}], 
                        {y, Ceiling[yRange[[1]]], Floor[yRange[[2]]]}],
                Table[Line[{{x, yRange[[1]]}, {x, yRange[[2]]}}], 
                        {x, Ceiling[xRange[[1]]], Floor[xRange[[2]]]}],
                
                
                Black, Thickness[0.003], Arrowheads[0.02],
                Arrow[{{xRange[[1]], 0}, {xRange[[2]], 0}}],
                Arrow[{{0, yRange[[1]]}, {0, yRange[[2]]}}],
                Text[Style["x", Bold, 14], {xRange[[2]] - 0.3, -0.3}],
                Text[Style["y", Bold, 14], {-0.3, yRange[[2]] - 0.3}],
                
                
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
                ]
            };
            
            
            
            AppendTo[graphicsElements, 
                {Opacity[0.3], Dashed, Thickness[0.002], brightGreen,
                      Table[
                            Line[{pociatocny[[i]], druhy[[i]]}],
                            {i, 5}
                        ]
                  }
            ];
            
            
            If[showSecond,
                AppendTo[graphicsElements, 
                    {Opacity[0.3], Dashed, Thickness[0.002], brightOrange,
                          Table[
                                Line[{druhy[[i]], treti[[i]]}],
                                {i, 5}
                            ]
                      }
                ];
            ];
            
            
            If[showThird,
                AppendTo[graphicsElements, 
                    {Opacity[0.3], Dashed, Thickness[0.002], brightRed,
                          Table[
                                Line[{treti[[i]], finalny[[i]]}],
                                {i, 5}
                            ]
                      }
                ];
            ];
            
            
            drawDomcek[vertices_, color_] := {
                color, Thickness[0.005], Opacity[0.9],
                Line[Append[vertices, First[vertices]]]
            };
            
            
            AppendTo[graphicsElements, drawDomcek[pociatocny, brightBlue]];
            
            
            AppendTo[graphicsElements, drawDomcek[druhy, brightGreen]];
            
            
            If[showSecond,
                AppendTo[graphicsElements, drawDomcek[treti, brightOrange]];
            ];
            
            
            If[showThird,
                AppendTo[graphicsElements, drawDomcek[finalny, brightRed]];
            ];
            
            
            
            AppendTo[graphicsElements, 
                Table[
                      {
                            White, Disk[pociatocny[[i]], 0.15],
                            brightBlue, Disk[pociatocny[[i]], 0.12]
                        },
                      {i, 5}
                  ]
            ];
            
            
            AppendTo[graphicsElements, 
                Table[
                      {
                            White, Disk[druhy[[i]], 0.15],
                            brightGreen, Disk[druhy[[i]], 0.12]
                        },
                      {i, 5}
                  ]
            ];
            
            
            If[showSecond,
                AppendTo[graphicsElements, 
                    Table[
                          {
                                White, Disk[treti[[i]], 0.15],
                                brightOrange, Disk[treti[[i]], 0.12]
                            },
                          {i, 5}
                      ]
                ];
            ];
            
            
            If[showThird,
                AppendTo[graphicsElements, 
                    Table[
                          {
                                White, Disk[finalny[[i]], 0.15],
                                brightRed, Disk[finalny[[i]], 0.12]
                            },
                          {i, 5}
                      ]
                ];
            ];
            
            
            totalVertices = 5 * (1 + 1 + If[showSecond, 1, 0] + If[showThird, 1, 0]);
            
            
            vertexLabels = {};
            
            
            Do[
                With[{
                        label = FromCharacterCode[64 + i],
                        color = brightBlue,
                        pos = labelPositions[[i]]
                    },
                    AppendTo[vertexLabels, 
                        {
                            White, Disk[pos, 0.2],
                            color, 
                            Text[Style[label, Bold, 14], pos]
                        }
                    ];
                ],
                {i, 5}
            ];
            
            
            Do[
                With[{
                        label = FromCharacterCode[64 + (i - 5)] <> "'",
                        color = brightGreen,
                        pos = labelPositions[[i]]
                    },
                    AppendTo[vertexLabels, 
                        {
                            White, Disk[pos, 0.2],
                            color, 
                            Text[Style[label, Bold, 14], pos]
                        }
                    ];
                ],
                {i, 6, 10}
            ];
            
            
            If[showSecond,
                Do[
                    With[{
                            label = FromCharacterCode[64 + (i - 10)] <> "''",
                            color = brightOrange,
                            pos = labelPositions[[i]]
                        },
                        AppendTo[vertexLabels, 
                            {
                                White, Disk[pos, 0.2],
                                color, 
                                Text[Style[label, Bold, 14], pos]
                            }
                        ];
                    ],
                    {i, 11, 15}
                ];
            ];
            
            
            If[showThird,
                Do[
                    With[{
                            label = FromCharacterCode[64 + (i - 15)] <> "'''",
                            color = brightRed,
                            pos = labelPositions[[i]]
                        },
                        AppendTo[vertexLabels, 
                            {
                                White, Disk[pos, 0.2],
                                color, 
                                Text[Style[label, Bold, 14], pos]
                            }
                        ];
                    ],
                    {i, 16, 20}
                ];
            ];
            
            AppendTo[graphicsElements, vertexLabels];
            
            
            Graphics[graphicsElements,
              PlotRange -> {xRange, yRange},
              AspectRatio -> 1,
              ImageSize -> 650,
              PlotLabel -> Style["Postupn\[AAcute] aplik\[AAcute]cia " <> ToString[numTransformations] <> 
                           Switch[numTransformations, 
                               1, " transform\[AAcute]cie", 
                               _, " transform\[AAcute]ci\[IAcute]"] <> " dom\[CHacek]eka", Bold, 16],
              ImagePadding -> {{40, 40}, {40, 40}},
              Background -> White,
              Method -> {"ShrinkWrap" -> True}]
];


DomcekTrojitaTransformaciaSVysledkom[] := Module[{result},
   
   result = Quiet[DomcekTrojitaTransformacia[], {Power::infy, Infinity::indet}];
   
   
   If[result =!= Null,
       Check[
           Print[Style["\nZ\[CapitalAAcute]VERE\[CapitalCHacek]N\[CapitalYAcute] PREH\:013dAD:", Bold, 16]];
           Print["Vybrat\[AAcute] obtia\[ZHacek]nos\[THacek]: ", Style[
               Switch[result[[4]],
                   "Easy", "Jednoduch\[AAcute] (1 transform\[AAcute]cia)",
                   "Medium", "Stredn\[AAcute] (2 transform\[AAcute]cie)",
                   "Hard", "Zlo\[ZHacek]it\[AAcute] (3 transform\[AAcute]cie)"
               ], Bold]];
           Print["Vykonali ste tieto transform\[AAcute]cie:"];
           
           Do[
               Print[i, ". ", result[[3, i]]],
               {i, 1, Length[result[[3]]]}
           ];
           
           Print["\nV\[YAcute]sledn\[YAcute] dom\[CHacek]ek:"];
           Print[MatrixForm[result[[2, -1]]]],
           
           
           Print[Style["Nastala chyba pri zobrazovan\[IAcute] v\[YAcute]sledkov.", Red, Bold]]
       ]
   ];
   
   Null
];


DisplayComplexAssignment[pociatocny_, prva_, druha_, tretia_] := 
    Module[{},
        Print[Style["KOMPLEXN\[CapitalEAcute] ZADANIE PRE \[CapitalZHacek]IAKA", Bold, 24]];
        Print[Style["==========================================", Bold]];
        
        
        If[prva == "Posun",
            
            Module[{posunVector = DomcekHardBalik`Transforms`Posun`DomcekPosunVector,
                   formattedVector},
                
                
                formattedVector = "{" <> ToString[TraditionalForm[posunVector[[1]]]] <> ", " <> 
                               ToString[TraditionalForm[posunVector[[2]]]] <> "}";
                
                Print[Style["\nVykonajte postupne tieto tri transform\[AAcute]cie:", Bold, 16]];
                Print[Style["1. " <> prva <> " o " <> formattedVector, Bold, Blue]];
                Print[Style["2. " <> druha, Bold, Blue]];
                Print[Style["3. " <> tretia, Bold, Blue]];
            ]
        ,
            
            Print[Style["\nVykonajte postupne tieto tri transform\[AAcute]cie:", Bold, 16]];
            Print[Style["1. " <> prva, Bold, Blue]];
            Print[Style["2. " <> druha, Bold, Blue]];
            Print[Style["3. " <> tretia, Bold, Blue]];
        ];
        
        
        Print[Style["\nDAN\[CapitalEAcute]:", Bold, 16]];
        Print[Style["M\[AAcute]te dom\[CHacek]ek ABCDE s vrcholmi:", Bold, 14]];
        Print[MatrixForm[Map[Style[#, Blue] &, pociatocny, {2}]]];
        
        
        Print[Style["\nPOSTUP:", Bold, 16]];
        Print["1. Aplikujte prv\[UAcute] transform\[AAcute]ciu na p\[OHat]vodn\[YAcute] dom\[CHacek]ek ABCDE, \[CHacek]\[IAcute]m z\[IAcute]skate dom\[CHacek]ek A'B'C'D'E'"];
        Print["2. Aplikujte druh\[UAcute] transform\[AAcute]ciu na dom\[CHacek]ek A'B'C'D'E', \[CHacek]\[IAcute]m z\[IAcute]skate dom\[CHacek]ek A''B''C''D''E''"];
        Print["3. Aplikujte tretiu transform\[AAcute]ciu na dom\[CHacek]ek A''B''C''D''E'', \[CHacek]\[IAcute]m z\[IAcute]skate fin\[AAcute]lny dom\[CHacek]ek A'''B'''C'''D'''E'''"];
        
        Print[Style["\nPOZN\[CapitalAAcute]MKA:", Bold, 14]];
        Print["\[Bullet] Pre ka\[ZHacek]d\[UAcute] transform\[AAcute]ciu si zap\[IAcute]\[SHacek]te pr\[IAcute]slu\[SHacek]n\[UAcute] transforma\[CHacek]n\[UAcute] maticu"];
        Print["\[Bullet] Vypo\[CHacek]\[IAcute]tajte nov\[EAcute] s\[UAcute]radnice v\[SHacek]etk\[YAcute]ch vrcholov po ka\[ZHacek]dej transform\[AAcute]cii"];
        Print["\[Bullet] Nezabudnite na spr\[AAcute]vne pou\[ZHacek]itie homog\[EAcute]nnych s\[UAcute]radn\[IAcute]c pre v\[SHacek]etky v\[YAcute]po\[CHacek]ty"];
        
        Print[Style["\nPo stla\[CHacek]en\[IAcute] tla\[CHacek]idla pokra\[CHacek]ova\[THacek] sa zobraz\[IAcute] podrobn\[YAcute] postup v\[YAcute]po\[CHacek]tu.", Bold, 12]];
    ];


FormatRationalNumber[num_] := Module[{rationalized},
  
  rationalized = Rationalize[num, 10^-10];
  
  
  If[Denominator[rationalized] == 1,
    ToString[Numerator[rationalized]],
    ToString[Numerator[rationalized]] <> "/" <> ToString[Denominator[rationalized]]
  ]
];


SelectTransformation[message_, exclude1_:"", exclude2_:""] := 
    Module[{options, fullOptions, formattedChoices, result},
        options = DeleteCases[
            {
                "Posun" -> {"Posun", "Posun dom\[CHacek]eka vo smere vektora"},
                "Rot\[AAcute]cia" -> {"Rot\[AAcute]cia", "Rot\[AAcute]cia dom\[CHacek]eka okolo po\[CHacek]iatku s\[UAcute]radnicovej s\[UAcute]stavy"},
                "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie" -> {"Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie alebo zmen\[SHacek]enie dom\[CHacek]eka v r\[OHat]znych smeroch"},
                "Skosenie" -> {"Skosenie", "Skosenie dom\[CHacek]eka v smere os\[IAcute]"},
                "Symetria" -> {"Symetria", "Zrkadlenie dom\[CHacek]eka pod\:013ea zvolenej osi"}
            },
            x_ /; MemberQ[{exclude1, exclude2}, First[x]]
        ];
        
        
        ChoiceDialog[
            message,
            options,
            WindowTitle -> "V\[YAcute]ber transform\[AAcute]cie",
            WindowSize -> {500, All}
        ]
    ];



DomcekZvacsenieZmensenieNoDisplayWithParams[inputVertices_] := 
    Module[{sx, sy, finalVertices, normalizedVertices, detectedScaling = None, detectedFinalVertices = None},
            
        
        If[Length[inputVertices] > 5 && Mod[Length[inputVertices], 5] == 0,
            normalizedVertices = inputVertices[[1;;5]];
            detectedFinalVertices = inputVertices[[-(5;;-1)]];
            
            detectedScaling = DetectScalingParameters[normalizedVertices, detectedFinalVertices],
            
            normalizedVertices = inputVertices
        ];
            
        
        If[detectedScaling =!= None,
            sx = detectedScaling[[1]];
            sy = detectedScaling[[2]];
            finalVertices = detectedFinalVertices,
            
            
            If[DomcekHardBalik`Transforms`ZvacsenieZmensenie`ZvacsenieParams =!= {0, 0},
                
                {sx, sy} = DomcekHardBalik`Transforms`ZvacsenieZmensenie`ZvacsenieParams;
                
                DomcekHardBalik`Transforms`ZvacsenieZmensenie`ZvacsenieParams = {0, 0},
                
                
                Module[{result},
                    
                    sx = RandomChoice[{0.5, 0.75, 1.25, 1.5, 2}];
                    sy = RandomChoice[{0.5, 0.75, 1.25, 1.5, 2}];
                ]
            ];
            
            
            finalVertices = Map[
                Function[point,
                    {Simplify[point[[1]]*sx], Simplify[point[[2]]*sy]}
                ],
                normalizedVertices
            ]
        ];
        
        
        DomcekHardBalik`Transforms`ZvacsenieZmensenie`ZvacsenieParams = {sx, sy};
        
        
        {{sx, sy}, finalVertices}
    ];
    
SelectDifficultyLevel[] := Module[{},
    ChoiceDialog[
        "Vyberte \[UAcute]rove\[NHacek] n\[AAcute]ro\[CHacek]nosti zadania:",
        {
            "Jednoduch\[AAcute] (1 transform\[AAcute]cia)" -> "Easy",
            "Stredn\[AAcute] (2 transform\[AAcute]cie)" -> "Medium",
            "Zlo\[ZHacek]it\[AAcute] (3 transform\[AAcute]cie)" -> "Hard"
        },
        WindowTitle -> "V\[YAcute]ber \[UAcute]rovne n\[AAcute]ro\[CHacek]nosti",
        WindowSize -> {600, All}
    ]
];



DetectScalingParameters[originalVertices_, finalVertices_] := 
    Module[{sx, sy, foundScaling = False, detectedSx, detectedSy, i = 1, point, scaledPoint},
        While[i <= 5 && !foundScaling,
            point = originalVertices[[i]];
            scaledPoint = finalVertices[[i]];
            
            
            If[point[[1]] != 0 && scaledPoint[[1]] != 0,
                detectedSx = scaledPoint[[1]]/point[[1]];
                foundScaling = True;
            ];
            
            If[point[[2]] != 0 && scaledPoint[[2]] != 0,
                detectedSy = scaledPoint[[2]]/point[[2]];
                foundScaling = True;
            ];
            
            i++;
        ];
        
        
        If[foundScaling, 
            {Simplify[detectedSx], Simplify[detectedSy]}, 
            None
        ]
    ];


DomcekRotaciaNoDisplayWithParams[vertices_] := 
    Module[{angle, result},
        
        If[DomcekHardBalik`Transforms`Rotacia`RotaciaUhol != 0,
            angle = DomcekHardBalik`Transforms`Rotacia`RotaciaUhol,
            angle = RandomChoice[{30, 45, 60, 90, 120, 135, 180, 270}];
            
            DomcekHardBalik`Transforms`Rotacia`RotaciaUhol = angle;
        ];
        
        
        result = Map[
            {#[[1]]*Cos[angle*Degree] - #[[2]]*Sin[angle*Degree], 
             #[[1]]*Sin[angle*Degree] + #[[2]]*Cos[angle*Degree]} &,
            vertices
        ];
        
        
        {angle, result}
    ];
    

GetTranslationParametersNoDisplay[vertices_] := 
    Module[{dx, dy, valid = False, newVertices, 
            allVertices, xMin, xMax, yMin, yMax, 
            invDx, invDy, invNewVertices, invVertices,
            narocnost},
        
        
        allVertices = vertices;
        xMin = Min[allVertices[[All, 1]]];
        xMax = Max[allVertices[[All, 1]]];
        yMin = Min[allVertices[[All, 2]]];
        yMax = Max[allVertices[[All, 2]]];
        
        
        narocnost = RandomReal[]; 
        
        While[!valid,
            
            If[narocnost < 0.7, 
                
                dx = RandomChoice[{-7, -6, -5, -4, -3, 3, 4, 5, 6, 7}];
                dy = RandomChoice[{-7, -6, -5, -4, -3, 3, 4, 5, 6, 7}],
                
                
                dx = RandomChoice[{-5, -4, -3, -2, 2, 3, 4, 5}];
                dy = RandomChoice[{-5, -4, -3, -2, 2, 3, 4, 5}]
            ];
            
            
            newVertices = Map[{#[[1]] + dx, #[[2]] + dy} &, allVertices];
            
            
            invDx = -dx;
            invDy = -dy;
            
            
            invNewVertices = Map[{#[[1]] + invDx, #[[2]] + invDy} &, newVertices];
            
            valid = 
                
                AllTrue[newVertices, (Abs[#[[1]]] <= 14 && Abs[#[[2]]] <= 14) &] &&
                
                Sqrt[dx^2 + dy^2] >= 3 &&
                
                Total[Flatten[Abs[allVertices - invNewVertices]]] < 0.00001 &&
                
                Min[
                    Table[
                        EuclideanDistance[allVertices[[i]], newVertices[[i]]],
                        {i, Length[allVertices]}
                    ]
                ] >= 3;
        ];
        
        
        {dx, dy, "Posun o [" <> ToString[dx] <> ", " <> ToString[dy] <> "]", newVertices}
    ];


DomcekSkosenieNoDisplayWithParams[vertices_] := 
    Module[{kx, ky, result},
        
        If[DomcekHardBalik`Transforms`Skosenie`SkosenieParams =!= {0, 0},
            {kx, ky} = DomcekHardBalik`Transforms`Skosenie`SkosenieParams;
            
            DomcekHardBalik`Transforms`Skosenie`SkosenieParams = {0, 0},
            
            kx = RandomChoice[{-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2}];
            ky = RandomChoice[{-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2}];
            
            
            If[kx == 0 && ky == 0, kx = RandomChoice[{-1, 1}]];
        ];
        
        
        result = Map[
            Function[point,
                {point[[1]] + kx*point[[2]], ky*point[[1]] + point[[2]]}
            ],
            vertices
        ];
        
        
        DomcekHardBalik`Transforms`Skosenie`SkosenieParams = {kx, ky};
        
        
        {{kx, ky}, result}
    ];


DomcekSymetriaNoDisplayWithParams[vertices_] := 
    Module[{axis, result, a, b, c, lineParams, specialAxes, randomAxis},
        
        specialAxes = {"os x", "os y", "priamka y=x", "priamka y=-x"};
        
        
        If[TrueMemberQ[{Null, "complex"}, DomcekHardBalik`Transforms`Symetria`SymetriaOs] || 
           !MemberQ[Join[specialAxes, _List], DomcekHardBalik`Transforms`Symetria`SymetriaOs],
           
           
           randomAxis = RandomReal[1.0];
           If[randomAxis < 0.7, 
               
               a = RandomChoice[{-5, -4, -3, -2, -1, 1, 2, 3, 4, 5}];
               b = RandomChoice[{-5, -4, -3, -2, -1, 1, 2, 3, 4, 5}];
               c = RandomChoice[{-6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6}];
               
               
               While[a == 0 && b == 0,
                   a = RandomChoice[{-5, -4, -3, -2, -1, 1, 2, 3, 4, 5}];
               ];
               
               
               If[GCD[Abs[a], Abs[b], Abs[c]] > 1,
                   {a, b, c} = {a, b, c}/GCD[Abs[a], Abs[b], Abs[c]]
               ];
               
               
               axis = {a, b, c};
           ,
               
               axis = RandomChoice[specialAxes];
           ],
           
           axis = DomcekHardBalik`Transforms`Symetria`SymetriaOs;
        ];
        
        
        result = Switch[axis,
            "os x", 
                Map[{#[[1]], -#[[2]]} &, vertices],
            "os y", 
                Map[{-#[[1]], #[[2]]} &, vertices],
            "priamka y=x", 
                Map[{#[[2]], #[[1]]} &, vertices],
            "priamka y=-x", 
                Map[{-#[[2]], -#[[1]]} &, vertices],
            _, 
                
                If[ListQ[axis] && Length[axis] == 3,
                    
                    Module[{reflectFunc},
                        
                        reflectFunc = Function[{point, a, b, c},
                            Module[{x, y, d, factor},
                                x = point[[1]];
                                y = point[[2]];
                                factor = a^2 + b^2;
                                d = (a*x + b*y + c);
                                {x - 2*a*d/factor, y - 2*b*d/factor}
                            ]
                        ];
                        
                        
                        Map[reflectFunc[#, axis[[1]], axis[[2]], axis[[3]]] &, vertices]
                    ],
                    
                    Module[{defaultAxis = {3, 4, 2}},  
                        axis = defaultAxis;
                        Map[
                            Function[point,
                                Module[{x, y, d, factor, a, b, c},
                                    {a, b, c} = defaultAxis;
                                    x = point[[1]]; y = point[[2]];
                                    factor = a^2 + b^2;
                                    d = (a*x + b*y + c);
                                    {x - 2*a*d/factor, y - 2*b*d/factor}
                                ]
                            ],
                            vertices
                        ]
                    ]
                ]
        ];
        
        
        DomcekHardBalik`Transforms`Symetria`SymetriaOs = axis;
        
        
        {axis, result}
    ];
    

EnsureExactValues[vertices_] := 
    Module[{result = vertices},
        
        If[!MatrixQ[vertices], Return[vertices]];
        
        
        result = Map[
            Function[vertex,
                Map[
                    Function[coord,
                        
                        Which[
                            
                            Head[coord] === Integer || Head[coord] === Rational,
                                coord,
                            
                            
                            NumericQ[coord] && Abs[Round[coord] - coord] < 10^-12,
                                Round[coord],
                                
                            
                            !NumericQ[coord],
                                Simplify[coord],
                            
                            
                            True,
                                coord
                        ]
                    ],
                    vertex
                ]
            ],
            vertices,
            {1}  
        ];
        
        
        
        Map[Simplify, result, {2}]
    ];


DomcekTrojitaTransformacia[] := Module[{
    difficultyLevel,
    numTransformations,
    pociatocnyDomcek,
    prvaTransformacia,
    druhyDomcek,
    druhaTransformacia = "\[CapitalZHacek]iadna",
    tretiDomcek,
    tretiaTransformacia = "\[CapitalZHacek]iadna",
    finalnyDomcek,
    transformacie = {},
    transformovane = {},
    prvaPopis,
    druhaPopis,
    tretiaPopis,
    
    
    posunVektor1, posunVektor2, posunVektor3,
    rotaciaUhol1 = 0, rotaciaUhol2 = 0, rotaciaUhol3 = 0,
    zvacsenieParams1, zvacsenieParams2, zvacsenieParams3,
    skosenieParams1, skosenieParams2, skosenieParams3,
    symetriaOs1, symetriaOs2, symetriaOs3,
    
    
    transformParams = {}
    },
    
    
    DomcekHardBalik`Transforms`Symetria`SymetriaOs = Null;
    DomcekHardBalik`Transforms`Rotacia`RotaciaUhol = 0;
    DomcekHardBalik`Transforms`Posun`DomcekPosunVector = {0, 0};
    DomcekHardBalik`Transforms`ZvacsenieZmensenie`ZvacsenieParams = {0, 0};
    DomcekHardBalik`Transforms`Skosenie`SkosenieParams = {0, 0};

    
    difficultyLevel = SelectDifficultyLevel[];
    If[difficultyLevel === $Canceled, Return[]];
    
    
    numTransformations = Switch[difficultyLevel,
        "Easy", 1,
        "Medium", 2,
        "Hard", 3,
        _, 3 
    ];
    
    
    Quiet[
    
    
    pociatocnyDomcek = GenerateInitialDomcek[];
    transformovane = {pociatocnyDomcek};
    
    
    If[numTransformations >= 1, 
        
        prvaPopis = SelectTransformation["Vyberte prv\[UAcute] transform\[AAcute]ciu:"];
        If[prvaPopis === $Canceled, Return[]];
        prvaTransformacia = First[prvaPopis];
        AppendTo[transformacie, prvaTransformacia];
        
        
        druhyDomcek = Switch[prvaTransformacia,
            "Posun", Module[{result, dx, dy, translationParams}, 
                
                translationParams = GetTranslationParametersNoDisplay[pociatocnyDomcek];
                
                
                posunVektor1 = {translationParams[[1]], translationParams[[2]]};
                AppendTo[transformParams, posunVektor1];
                
                
                {dx, dy, _, result} = translationParams;
                result],
                
            "Rot\[AAcute]cia", Module[{result, angle},
                
                If[rotaciaUhol1 != 0,
                    DomcekHardBalik`Transforms`Rotacia`RotaciaUhol = rotaciaUhol1;
                ];
                
                
                {angle, result} = DomcekRotaciaNoDisplayWithParams[pociatocnyDomcek];
                
                
                rotaciaUhol1 = angle;
                AppendTo[transformParams, rotaciaUhol1];
                
                result],
                
            "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", Module[{result, params},
                
                {params, result} = DomcekZvacsenieZmensenieNoDisplayWithParams[pociatocnyDomcek];
                
                
                zvacsenieParams1 = params;
                AppendTo[transformParams, zvacsenieParams1];
                
                result],
                
            "Skosenie", Module[{result, params},
                
                {params, result} = DomcekSkosenieNoDisplayWithParams[pociatocnyDomcek];
                
                
                skosenieParams1 = params;
                AppendTo[transformParams, skosenieParams1];
                
                result],
                
            "Symetria", Module[{result, axis},
                
                DomcekHardBalik`Transforms`Symetria`SymetriaOs = Null;
                
                
                {axis, result} = DomcekSymetriaNoDisplayWithParams[pociatocnyDomcek];
                
                
                symetriaOs1 = axis;
                AppendTo[transformParams, symetriaOs1];
                
                result]
        ];
        AppendTo[transformovane, druhyDomcek];
        finalnyDomcek = druhyDomcek;
    ];
    
    If[numTransformations >= 2, 
        
        druhaPopis = SelectTransformation[
            "Vyberte druh\[UAcute] transform\[AAcute]ciu:", 
            prvaTransformacia,
            ""
        ];
        If[druhaPopis === $Canceled, Return[]];
        druhaTransformacia = First[druhaPopis];
        AppendTo[transformacie, druhaTransformacia];
        
        
        tretiDomcek = Switch[druhaTransformacia,
            "Posun", Module[{result, dx, dy, translationParams}, 
                translationParams = GetTranslationParametersNoDisplay[EnsureExactValues[druhyDomcek]];
                posunVektor2 = {translationParams[[1]], translationParams[[2]]};
                AppendTo[transformParams, posunVektor2];
                {dx, dy, _, result} = translationParams;
                result],
                
            "Rot\[AAcute]cia", Module[{result, angle},
                If[rotaciaUhol2 != 0,
                    DomcekHardBalik`Transforms`Rotacia`RotaciaUhol = rotaciaUhol2;
                ];
                {angle, result} = DomcekRotaciaNoDisplayWithParams[EnsureExactValues[druhyDomcek]];
                rotaciaUhol2 = angle;
                AppendTo[transformParams, rotaciaUhol2];
                result],
                
            "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", Module[{result, params},
                {params, result} = DomcekZvacsenieZmensenieNoDisplayWithParams[EnsureExactValues[druhyDomcek]];
                zvacsenieParams2 = params;
                AppendTo[transformParams, zvacsenieParams2];
                result],
                
            "Skosenie", Module[{result, params},
                {params, result} = DomcekSkosenieNoDisplayWithParams[EnsureExactValues[druhyDomcek]];
                skosenieParams2 = params;
                AppendTo[transformParams, skosenieParams2];
                result],
                
            "Symetria", Module[{result, axis},
                
                DomcekHardBalik`Transforms`Symetria`SymetriaOs = Null;
                
                
                {axis, result} = DomcekSymetriaNoDisplayWithParams[EnsureExactValues[druhyDomcek]];
                
                
                symetriaOs2 = axis;
                AppendTo[transformParams, symetriaOs2];
                
                result]
        ];
        AppendTo[transformovane, tretiDomcek];
        finalnyDomcek = tretiDomcek;
    ];
    
    If[numTransformations >= 3, 
        
        tretiaPopis = SelectTransformation[
            "Vyberte tretiu transform\[AAcute]ciu:", 
            prvaTransformacia,
            druhaTransformacia
        ];
        If[tretiaPopis === $Canceled, Return[]];
        tretiaTransformacia = First[tretiaPopis];
        AppendTo[transformacie, tretiaTransformacia];
        
        
        finalnyDomcek = Switch[tretiaTransformacia,
            "Posun", Module[{result, dx, dy, translationParams}, 
                translationParams = GetTranslationParametersNoDisplay[EnsureExactValues[tretiDomcek]];
                posunVektor3 = {translationParams[[1]], translationParams[[2]]};
                AppendTo[transformParams, posunVektor3];
                {dx, dy, _, result} = translationParams;
                result],
                
            "Rot\[AAcute]cia", Module[{result, angle},
                If[rotaciaUhol3 != 0,
                    DomcekHardBalik`Transforms`Rotacia`RotaciaUhol = rotaciaUhol3;
                ];
                {angle, result} = DomcekRotaciaNoDisplayWithParams[EnsureExactValues[tretiDomcek]];
                rotaciaUhol3 = angle;
                AppendTo[transformParams, rotaciaUhol3];
                result],
                
            "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", Module[{result, params},
                {params, result} = DomcekZvacsenieZmensenieNoDisplayWithParams[EnsureExactValues[tretiDomcek]];
                zvacsenieParams3 = params;
                AppendTo[transformParams, zvacsenieParams3];
                result],
                
            "Skosenie", Module[{result, params},
                {params, result} = DomcekSkosenieNoDisplayWithParams[EnsureExactValues[tretiDomcek]];
                skosenieParams3 = params;
                AppendTo[transformParams, skosenieParams3];
                result],
                
            "Symetria", Module[{result, axis},
                
                DomcekHardBalik`Transforms`Symetria`SymetriaOs = Null;
                
                
                {axis, result} = DomcekSymetriaNoDisplayWithParams[EnsureExactValues[tretiDomcek]];
                
                
                symetriaOs3 = axis;
                AppendTo[transformParams, symetriaOs3];
                
                result]
        ];
        AppendTo[transformovane, finalnyDomcek];
    ];
    
    
    Print[Style["GEOMETRICK\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA DOM\[CapitalCHacek]EKA - " <> 
           Switch[difficultyLevel, "Easy", "JEDNODUCH\[CapitalAAcute] \[CapitalUAcute]LOHA", 
                              "Medium", "STREDNE \[CapitalTHacek]A\[CapitalZHacek]K\[CapitalAAcute] \[CapitalUAcute]LOHA",
                              "Hard", "ZLO\[CapitalZHacek]IT\[CapitalAAcute] \[CapitalUAcute]LOHA"], Bold, 24]];
    Print[Style["==========================================", Bold]];
    
    Print[Style["\nZADANIE PRE \[CapitalSHacek]TUDENTA:", Bold, 18]];
    Print["Majme dom\[CHacek]ek ABCDE s vrcholmi:"];
    Print[MatrixForm[Map[Style[#, Blue] &, pociatocnyDomcek, {2}]]];
    
    Print["\nVykonajte postupne tieto transform\[AAcute]cie:"];
    
    
    
    
    If[numTransformations >= 1,
        Print["1. ", FormatTransformationDetailedDescription[prvaTransformacia, 
                         Switch[prvaTransformacia,
                             "Posun", posunVektor1,
                             "Rot\[AAcute]cia", rotaciaUhol1,
                             "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", zvacsenieParams1,
                             "Skosenie", skosenieParams1,
                             "Symetria", symetriaOs1
                         ]]];
    ];
    
    
    If[numTransformations >= 2,
        Print["2. ", FormatTransformationDetailedDescription[druhaTransformacia, 
                         Switch[druhaTransformacia,
                             "Posun", posunVektor2,
                             "Rot\[AAcute]cia", rotaciaUhol2,
                             "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", zvacsenieParams2,
                             "Skosenie", skosenieParams2,
                             "Symetria", symetriaOs2
                         ]]];
    ];
    
    
    If[numTransformations >= 3,
        Print["3. ", FormatTransformationDetailedDescription[tretiaTransformacia, 
                         Switch[tretiaTransformacia,
                             "Posun", posunVektor3,
                             "Rot\[AAcute]cia", rotaciaUhol3,
                             "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", zvacsenieParams3,
                             "Skosenie", skosenieParams3,
                             "Symetria", symetriaOs3
                         ]]];
    ];
    
    Print[Style["\nRIE\[CapitalSHacek]ENIE:", Bold, 18]];

    
    
    
    Print[Style["GEOMETRICK\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA DOM\[CapitalCHacek]EKA - " <> 
           Switch[difficultyLevel, "Easy", "JEDNODUCH\[CapitalAAcute] \[CapitalUAcute]LOHA", 
                              "Medium", "STREDNE \[CapitalTHacek]A\[CapitalZHacek]K\[CapitalAAcute] \[CapitalUAcute]LOHA",
                              "Hard", "ZLO\[CapitalZHacek]IT\[CapitalAAcute] \[CapitalUAcute]LOHA"], Bold, 24]];
    Print[Style["==========================================", Bold]];
    
    
    Print[Style["\nPO\[CapitalCHacek]IATO\[CapitalCHacek]N\[CapitalYAcute] DOM\[CapitalCHacek]EK:", Bold, 16]];
    Print["Vrcholy dom\[CHacek]eka ABCDE:"];
    Print[MatrixForm[Map[Style[#, Blue] &, pociatocnyDomcek, {2}]]];
    DisplayDomcekProperties[pociatocnyDomcek, Blue];
    
    
    If[numTransformations >= 1,
        Print[Style["\nPRV\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA: " <> prvaTransformacia, Bold, 16]];
        
        
        druhyDomcek = Switch[prvaTransformacia,
            "Posun", 
                Module[{}, 
                    DomcekHardBalik`Transforms`Posun`DomcekPosunVector = posunVektor1;
                    DomcekPosun[pociatocnyDomcek]
                ],
            "Rot\[AAcute]cia", 
                Module[{}, 
                    DomcekHardBalik`Transforms`Rotacia`RotaciaUhol = rotaciaUhol1;
                    DomcekRotacia[pociatocnyDomcek]
                ],
            "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", 
                Module[{}, 
                    DomcekHardBalik`Transforms`ZvacsenieZmensenie`ZvacsenieParams = zvacsenieParams1;
                    DomcekZvacsenieZmensenie[pociatocnyDomcek]
                ],
            "Skosenie", 
                Module[{}, 
                    DomcekHardBalik`Transforms`Skosenie`SkosenieParams = skosenieParams1;
                    DomcekSkosenie[pociatocnyDomcek]
                ],
            "Symetria", 
                Module[{}, 
                    
                    DomcekHardBalik`Transforms`Symetria`SymetriaOs = symetriaOs1;
                    DomcekSymetria[pociatocnyDomcek]
                ]
        ];
    ];
    
    
    If[numTransformations >= 2,
        Print[Style["\nDRUH\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA: " <> druhaTransformacia, Bold, 16]];
        
        
        tretiDomcek = Switch[druhaTransformacia,
            "Posun", 
                Module[{}, 
                    DomcekHardBalik`Transforms`Posun`DomcekPosunVector = posunVektor2;
                    DomcekPosun[EnsureExactValues[druhyDomcek]]
                ],
            "Rot\[AAcute]cia", 
                Module[{}, 
                    DomcekHardBalik`Transforms`Rotacia`RotaciaUhol = rotaciaUhol2;
                    DomcekRotacia[EnsureExactValues[druhyDomcek]]
                ],
            "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", 
                Module[{}, 
                    DomcekHardBalik`Transforms`ZvacsenieZmensenie`ZvacsenieParams = zvacsenieParams2;
                    DomcekZvacsenieZmensenie[EnsureExactValues[druhyDomcek]]
                ],
            "Skosenie", 
                Module[{}, 
                    DomcekHardBalik`Transforms`Skosenie`SkosenieParams = skosenieParams2;
                    DomcekSkosenie[EnsureExactValues[druhyDomcek]]
                ],
            "Symetria", 
                Module[{}, 
                    
                    DomcekHardBalik`Transforms`Symetria`SymetriaOs = symetriaOs2;
                    DomcekSymetria[EnsureExactValues[druhyDomcek]]
                ]
        ];
        AppendTo[transformovane, tretiDomcek];
        finalnyDomcek = tretiDomcek;
    ];
    
    
    If[numTransformations >= 3,
        Print[Style["\nTRETIA TRANSFORM\[CapitalAAcute]CIA: " <> tretiaTransformacia, Bold, 16]];
        
        
        finalnyDomcek = Switch[tretiaTransformacia,
            "Posun", 
                Module[{}, 
                    DomcekHardBalik`Transforms`Posun`DomcekPosunVector = posunVektor3;
                    DomcekPosun[EnsureExactValues[tretiDomcek]]
                ],
            "Rot\[AAcute]cia", 
                Module[{}, 
                    DomcekHardBalik`Transforms`Rotacia`RotaciaUhol = rotaciaUhol3;
                    DomcekRotacia[EnsureExactValues[tretiDomcek]]
                ],
            "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", 
                Module[{}, 
                    DomcekHardBalik`Transforms`ZvacsenieZmensenie`ZvacsenieParams = zvacsenieParams3;
                    DomcekZvacsenieZmensenie[EnsureExactValues[tretiDomcek]]
                ],
            "Skosenie", 
                Module[{}, 
                    DomcekHardBalik`Transforms`Skosenie`SkosenieParams = skosenieParams3;
                    DomcekSkosenie[EnsureExactValues[tretiDomcek]]
                ],
            "Symetria", 
                Module[{}, 
                    
                    DomcekHardBalik`Transforms`Symetria`SymetriaOs = symetriaOs3;
                    DomcekSymetria[EnsureExactValues[tretiDomcek]]
                ]
        ];
    ];
    
    
    DisplayTransformationSequence[
        pociatocnyDomcek,
        druhyDomcek,
        If[numTransformations >= 2, tretiDomcek, druhyDomcek],  
        finalnyDomcek,
        prvaTransformacia,
        If[numTransformations >= 2, druhaTransformacia, "\[CapitalZHacek]iadna"],
        If[numTransformations >= 3, tretiaTransformacia, "\[CapitalZHacek]iadna"],
        difficultyLevel,  
        transformParams   
    ];
    
    
    Print[Style["\nS\[CapitalUAcute]HRNN\[CapitalAAcute] VIZUALIZ\[CapitalAAcute]CIA:", Bold, 16]];
    Switch[numTransformations,
        1,
            Print[CreateDomcekVisualization[
                pociatocnyDomcek,
                finalnyDomcek,
                finalnyDomcek,  
                finalnyDomcek,  
                prvaTransformacia,
                "\[CapitalZHacek]iadna",
                "\[CapitalZHacek]iadna"
            ]],
        2,
            Print[CreateDomcekVisualization[
                pociatocnyDomcek,
                druhyDomcek,
                finalnyDomcek,
                finalnyDomcek,  
                prvaTransformacia,
                druhaTransformacia,
                "\[CapitalZHacek]iadna"
            ]],
        3,
            Print[CreateDomcekVisualization[
                pociatocnyDomcek,
                druhyDomcek,
                tretiDomcek,
                finalnyDomcek,
                prvaTransformacia,
                druhaTransformacia,
                tretiaTransformacia
            ]]
    ];
    
    
    Print[Style["\nLEGENDA:", Bold, 16]];
    Print[Style["\[Bullet] Modr\[EAcute]", RGBColor[0, 0.4, 0.8], Bold], " body a \[CHacek]iary: P\[OHat]vodn\[YAcute] dom\[CHacek]ek ABCDE"];
    
    
    If[numTransformations >= 1,
        Print[Style["\[Bullet] Tmavozelen\[EAcute]", RGBColor[0, 0.8, 0.2], Bold], " body a \[CHacek]iary: Dom\[CHacek]ek po prvej transform\[AAcute]cii (", 
              Style[prvaTransformacia, Bold], ")"];
    ];
    
    If[numTransformations >= 2,
        Print[Style["\[Bullet] Oran\[ZHacek]ov\[EAcute]", RGBColor[1, 0.6, 0], Bold], " body a \[CHacek]iary: Dom\[CHacek]ek po druhej transform\[AAcute]cii (", 
              Style[druhaTransformacia, Bold], ")"];
    ];
    
    If[numTransformations >= 3,
        Print[Style["\[Bullet] \[CapitalCHacek]erven\[EAcute]", RGBColor[0.9, 0.1, 0.1], Bold], " body a \[CHacek]iary: Dom\[CHacek]ek po tretej transform\[AAcute]cii (", 
              Style[tretiaTransformacia, Bold], ")"];
    ];
    
    
    Print[Style["\nPostupnos\[THacek] transform\[AAcute]ci\[IAcute]:", Bold, 16]];
    
    
    If[numTransformations >= 1,
        Print["1. ", prvaTransformacia, " ", 
              FormatTransformationParameters[prvaTransformacia, 
                  Switch[prvaTransformacia,
                      "Posun", posunVektor1,
                      "Rot\[AAcute]cia", rotaciaUhol1,
                      "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", zvacsenieParams1,
                      "Skosenie", skosenieParams1,
                      "Symetria", symetriaOs1
                  ]]];
    ];
    
    If[numTransformations >= 2,
        Print["2. ", druhaTransformacia, " ", 
              FormatTransformationParameters[druhaTransformacia, 
                  Switch[druhaTransformacia,
                      "Posun", posunVektor2,
                      "Rot\[AAcute]cia", rotaciaUhol2,
                      "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", zvacsenieParams2,
                      "Skosenie", skosenieParams2,
                      "Symetria", symetriaOs2
                  ]]];
    ];
    
    If[numTransformations >= 3,
        Print["3. ", tretiaTransformacia, " ", 
              FormatTransformationParameters[tretiaTransformacia, 
                  Switch[tretiaTransformacia,
                      "Posun", posunVektor3,
                      "Rot\[AAcute]cia", rotaciaUhol3,
                      "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", zvacsenieParams3,
                      "Skosenie", skosenieParams3,
                      "Symetria", symetriaOs3
                  ]]];
    ];
    
    
    {pociatocnyDomcek, transformovane, transformacie, difficultyLevel, transformParams}
    
    ] 
];


DomcekRotaciaNoDisplay[vertices_] := 
    Module[{angle, result},
        angle = RandomChoice[{30, 45, 60, 90, 120, 135, 180, 270}];
        
        
        result = Map[
            {#[[1]]*Cos[angle*Degree] - #[[2]]*Sin[angle*Degree], 
             #[[1]]*Sin[angle*Degree] + #[[2]]*Cos[angle*Degree]} &,
            vertices
        ];
        
        result
    ];


On[Power::infy];
On[Infinity::indet];

End[]; 
EndPackage[]; 















