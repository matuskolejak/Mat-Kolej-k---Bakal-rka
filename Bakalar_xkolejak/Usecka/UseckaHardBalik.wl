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




























BeginPackage["UseckaHardBalik`"];


UseckaTrojitaTransformacia::usage = 
    "UseckaTrojitaTransformacia[] umo\[ZHacek]n\[IAcute] postupn\[YAcute] v\[YAcute]ber a aplik\[AAcute]ciu troch transform\[AAcute]ci\[IAcute].";

UseckaTrojitaTransformaciaSVysledkom::usage = 
    "UseckaTrojitaTransformaciaSVysledkom[] umo\[ZHacek]n\[IAcute] postupn\[YAcute] v\[YAcute]ber a aplik\[AAcute]ciu troch transform\[AAcute]ci\[IAcute] a zobraz\[IAcute] s\[UAcute]hrnn\[YAcute] v\[YAcute]sledok.";

UseckaGeneruj::usage = 
    "UseckaGeneruj[] generuje n\[AAcute]hodn\[UAcute] \[UAcute]se\[CHacek]ku s vhodn\[YAcute]mi vlastnos\[THacek]ami.";


segmentLength::usage = 
    "segmentLength[p1, p2] vypo\[CHacek]\[IAcute]ta d\:013a\[ZHacek]ku \[UAcute]se\[CHacek]ky s koncov\[YAcute]mi bodmi p1 a p2.";

validSegmentQ::usage = 
    "validSegmentQ[p1, p2] over\[IAcute], \[CHacek]i \[UAcute]se\[CHacek]ka sp\:013a\[NHacek]a krit\[EAcute]ri\[AAcute].";


UseckaTrojitaTransformacia::infinityerr = 
    "Vyskytla sa chyba: v\[YAcute]po\[CHacek]et viedol k nekone\[CHacek]n\[EAcute]mu alebo neur\[CHacek]it\[EAcute]mu v\[YAcute]sledku. Pou\[ZHacek]ije sa preddefinovan\[AAcute] \[UAcute]se\[CHacek]ka.";

Begin["`Private`"];


Off[Power::infy];
Off[Infinity::indet];


Needs["UseckaHardBalik`Transforms`Posun`"];
Needs["UseckaHardBalik`Transforms`Rotacia`"];
Needs["UseckaHardBalik`Transforms`ZvacsenieZmensenie`"];
Needs["UseckaHardBalik`Transforms`Skosenie`"];
Needs["UseckaHardBalik`Transforms`Symetria`"];


mildGreen = RGBColor[0.2, 0.6, 0.2];


segmentLength[p1_, p2_] := Module[{length},
    length = Check[
        EuclideanDistance[p1, p2],
        $Failed
    ];
    
    If[length === $Failed || length === ComplexInfinity || length === Indeterminate || !NumberQ[length],
        
        5,
        
        length
    ]
];


validSegmentQ[p1_, p2_] := 
    Module[{length, minLength = 3, maxLength = 20, isValid}, 
        
        If[!VectorQ[p1, NumberQ] || !VectorQ[p2, NumberQ] || 
           Length[p1] != 2 || Length[p2] != 2,
            Return[False]
        ];
        
        
        length = segmentLength[p1, p2];
        If[!NumberQ[length], Return[False]];
        
        
        If[length === ComplexInfinity || length === Indeterminate, 
            Return[False]
        ];
        
        
        isValid = Check[
            
            minLength <= length <= maxLength && 
            
            p1 != p2,
            False
        ];
        
        isValid
    ];


GenerateInitialSegment[] := Module[{
    p1, p2, 
    count = 0, 
    defaultSegment = {{0, 0}, {5, 5}}, 
    result,
    previousSegments = {},
    randomSeed,
    generationType,
    minDistance = 2.5
  },
  
  
  randomSeed = Hash[{AbsoluteTime[], $TimeZone, $ProcessID, 
                     RandomInteger[{-10^7, 10^7}], StringJoin @@ 
                     ToString /@ RandomInteger[{0, 9}, 10]}];
  SeedRandom[randomSeed];
  
  
  Do[RandomInteger[{-1000, 1000}], {RandomInteger[{10, 30}]}];
  
  
  generationType = RandomChoice[{0.82, 0.18} -> {1, 2}];
  
  
  result = Check[
    Block[{$MessagePreprint = (Message[UseckaTrojitaTransformacia::infinityerr]; #)&},
      While[True,
        Switch[generationType,
          
          1, 
          Module[{range, nonZeroRandom},
            
            nonZeroRandom[min_, max_] := Module[{val},
              val = RandomInteger[{min, max}];
              If[val == 0, If[RandomReal[] < 0.5, -1, 1], val]
            ];
            
            
            range = RandomInteger[{5, 8}];
            
            
            p1 = {nonZeroRandom[-range, range], nonZeroRandom[-range, range]};
            p2 = {nonZeroRandom[-range, range], nonZeroRandom[-range, range]};
            
            
            If[p1[[1]] == 0 && p1[[2]] == 0, p1 = {1, nonZeroRandom[-range, range]}];
            If[p2[[1]] == 0 && p2[[2]] == 0, p2 = {nonZeroRandom[-range, range], 1}];
          ],
          
          
          2,
          Module[{num1x, num1y, num2x, num2y, 
                  den1x, den1y, den2x, den2y, nonZeroNum},
            
            
            nonZeroNum[min_, max_] := Module[{val},
              val = RandomInteger[{min, max}];
              If[val == 0, If[RandomReal[] < 0.5, 1, -1], val]
            ];
            
            
            num1x = nonZeroNum[-8, 8];
            num1y = nonZeroNum[-8, 8];
            num2x = nonZeroNum[-8, 8];
            num2y = nonZeroNum[-8, 8];
            
            
            den1x = RandomChoice[{2, 3, 4}];
            den1y = RandomChoice[{2, 3, 4}];
            den2x = RandomChoice[{2, 3, 4}];
            den2y = RandomChoice[{2, 3, 4}];
            
            
            p1 = {Rationalize[num1x/den1x], Rationalize[num1y/den1y]};
            p2 = {Rationalize[num2x/den2x], Rationalize[num2y/den2y]};
            
            
            If[p1[[1]] == 0, p1[[1]] = Rationalize[1/den1x]];
            If[p1[[2]] == 0, p1[[2]] = Rationalize[1/den1y]];
            If[p2[[1]] == 0, p2[[1]] = Rationalize[2/den2x]];
            If[p2[[2]] == 0, p2[[2]] = Rationalize[2/den2y]];
          ]
        ];
        
        If[ContainsAny[{Sort[{p1, p2}]}, previousSegments],
          
          count++;
          If[Mod[count, 5] == 0, 
            generationType = RandomChoice[{0.8, 0.18, 0.02} -> {1, 2, 3}]
          ];
          Continue[]
        ];
        
        
        If[EuclideanDistance[p1, p2] > minDistance,
          
          
          If[validSegmentQ[p1, p2] && Max[Abs[Flatten[{p1, p2}]]] <= 10,
            
            AppendTo[previousSegments, Sort[{p1, p2}]];
            If[Length[previousSegments] > 20, previousSegments = previousSegments[[-20;;]]];
            
            
            Return[{p1, p2}]
          ]
        ];
        
        
        If[Mod[count, 5] == 0 && count > 0, 
          generationType = RandomChoice[{0.82, 0.18} -> {1, 2}]
        ];
        
        
        If[count > 50, 
          Module[{simpleSegments},
            simpleSegments = {
              {{3, 1}, {-1, 4}},           
              {{5, 1}, {-1, 3}},           
              {{-3, -2}, {3, 4}},          
              {{-4, 1}, {4, 3}},           
              {{1, 1}, {6, 5}},            
              {{-3, 1}, {3, -4}},          
              {{-2, -2}, {4, 3}},          
              {{1, 2}, {4, 5}},            
              {{1, 1}, {5, 6}},            
              {{-3, -3}, {3, 3}},          
              
              {{-2, 1}, {2, 3}},           
              {{1, Rationalize[1/2]}, {3, 4}},  
              {{-1, -1}, {3, 3}},          
              {{-2, 1}, {2, 3}}            
            };
            
            
            Return[RandomChoice[simpleSegments]]
          ]
        ];
        count++
      ]
    ],
    
    {{0, 0}, {4, 4}}
  ];
  
  
  If[Head[result] === List && Length[result] === 2 && AllTrue[result, VectorQ[#, NumberQ]&], 
    result, 
    {{3, 1}, {-1, 4}}  
  ]
];


UseckaGeneruj[] := GenerateInitialSegment[];


DisplaySegmentProperties[vertices_, style_] := Module[{length},
    
    length = Check[Round[segmentLength @@ vertices], 5];
    
    
    If[!NumberQ[length] || length === ComplexInfinity || length === Indeterminate, length = 5];
    
    Print[Style["Vlastnosti \[UAcute]se\[CHacek]ky:", Bold]];
    Print["D\:013a\[ZHacek]ka: ", Style[length, style]];
];


FormatSegmentExpression[expr_] :=
    Module[{expandedExpr, simplifiedExpr},
        
        expandedExpr = Expand[expr];
        
        
        simplifiedExpr = Simplify[expandedExpr];
        
        
        simplifiedExpr
    ];


ProcessSegmentVertex[vertex_] := 
    Module[{},
        
        Map[FormatSegmentExpression, vertex]
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


ProcessSegmentVerticesComplete[vertices_] :=
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
                    
                    If[ValueQ[UseckaHardBalik`Transforms`Symetria`SymetriaMatrix],
                        
                        UseckaHardBalik`Transforms`Symetria`SymetriaMatrix,
                        
                        
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


ApplyTransformationMatrixToSegment[matrix_, segment_] :=
    Map[ApplyTransformationMatrix[matrix, #] &, segment];


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




VerifyCompositeTransformation[originalSegment_, compositeMatrix_, finalSegment_, compositeSteps_:{}] :=
    Module[{calculatedSegment, i, exactCompositeMatrix, exactOriginalSegment, exactFinalSegment},
        
        exactCompositeMatrix = compositeMatrix;
        
        
        exactOriginalSegment = originalSegment;
        exactFinalSegment = finalSegment;
        
        Print[Style["\nAPLIK\[CapitalAAcute]CIA S\[CapitalUAcute]HRNNEJ MATICE NA P\[CapitalOHat]VODN\[CapitalUAcute] \[CapitalUAcute]SE\[CapitalCHacek]KU:", Bold, 16]];
        
        
        Print[Style["\nS\[UAcute]hrnn\[AAcute] transforma\[CHacek]n\[AAcute] matica:", Bold, 14, RGBColor[0.9, 0.1, 0.1]]];
        Print[MatrixForm[Map[Style[#, RGBColor[0.9, 0.1, 0.1]] &, exactCompositeMatrix, {2}]]];
        
        
        Print[Style["\nP\[OHat]vodn\[AAcute] \[UAcute]se\[CHacek]ka AB:", Bold, Blue, 14]];
        Print[TableForm[
            exactOriginalSegment,
            TableHeadings -> {{"A", "B"}, {"x", "y"}},
            TableAlignments -> Center
        ]];
        
        
        calculatedSegment = Table[
            Module[{vertex = exactOriginalSegment[[i]], homogeneousVertex, resultVector},
                homogeneousVertex = Append[vertex, 1];
                
                
                resultVector = exactCompositeMatrix . homogeneousVertex;
                
                
                Take[resultVector, 2]
            ],
            {i, Length[exactOriginalSegment]}
        ];
        
        
        Print[Style["\nPostup pri aplikovan\[IAcute] s\[UAcute]hrnnej transforma\[CHacek]nej matice na \[UAcute]se\[CHacek]ku:", Bold, 14]];
        Print["1. Ka\[ZHacek]d\[YAcute] bod (x, y) sa roz\[SHacek]\[IAcute]ri na homog\[EAcute]nne s\[UAcute]radnice (x, y, 1)"];
        Print["2. Vyn\[AAcute]sob\[IAcute] sa s\[UAcute]hrnnou transforma\[CHacek]nou maticou 3\[Times]3"];
        Print["3. Z v\[YAcute]sledn\[EAcute]ho vektora vezmeme prv\[EAcute] dve s\[UAcute]radnice, \[CHacek]\[IAcute]m dostaneme transformovan\[YAcute] bod"];
        
        
        Print[Style["\nPODROBN\[CapitalYAcute] V\[CapitalYAcute]PO\[CapitalCHacek]ET PRE KA\[CapitalZHacek]D\[CapitalYAcute] BOD:", Bold, 16]];
        
        For[i = 1, i <= Length[exactOriginalSegment], i++,
            Module[{vertex = exactOriginalSegment[[i]], homogeneousVertex, resultVector, label},
                homogeneousVertex = Append[vertex, 1];
                resultVector = exactCompositeMatrix . homogeneousVertex;
                
                
                label = Switch[i, 1, "A", 2, "B"];
                
                
                Print[Style["\nBod " <> label <> ":", Bold, Blue, 14]];
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
                
                
                Print[Style["\nV\[YAcute]sledok pre bod " <> label <> ":", Bold]];
                Print["V homog\[EAcute]nnych s\[UAcute]radniciach: ", Style[resultVector, RGBColor[0, 0.6, 0]]];
                
                Module[{transformedPoint},
                    transformedPoint = Take[resultVector, 2];
                    
                    Print["Transformovan\[EAcute] s\[UAcute]radnice (", label, "'): ", 
                         Style[transformedPoint, RGBColor[0, 0.6, 0]]];
                ];
                
                
                Print["O\[CHacek]ak\[AAcute]van\[YAcute] v\[YAcute]sledok (", label, "'): ", 
                     Style[exactFinalSegment[[i]], RGBColor[0.9, 0.1, 0.1]]];
                
                
                Module[{difference, tolerance = 10^-4},
                    difference = Norm[Take[resultVector, 2] - exactFinalSegment[[i]]];
                    
                    If[difference < tolerance,
                        Print[Style["\[Checkmark] V\[YAcute]sledok sa zhoduje s o\[CHacek]ak\[AAcute]van\[YAcute]m bodom.", RGBColor[0.2, 0.6, 0.2], Bold]],
                        Print[Style["\:2717 V\[YAcute]sledok sa nezhoduje s o\[CHacek]ak\[AAcute]van\[YAcute]m bodom!", RGBColor[0.9, 0, 0], Bold]]
                    ];
                ];
            ];
        ];
        
        
        Print[Style["\nV\[CapitalYAcute]SLEDN\[CapitalAAcute] TRANSFORMOVAN\[CapitalAAcute] \[CapitalUAcute]SE\[CapitalCHacek]KA A'B':", Bold, 16, RGBColor[0, 0.6, 0]]];
        Print[TableForm[
            calculatedSegment,
            TableHeadings -> {{"A'", "B'"}, {"x", "y"}},
            TableAlignments -> Center
        ]];
        
        
        Print[Style["\nO\[CHacek]ak\[AAcute]van\[YAcute] v\[YAcute]sledn\[YAcute] \[UAcute]se\[CHacek]ka:", Bold, 14, RGBColor[0.9, 0.1, 0.1]]];
        Print[TableForm[
            exactFinalSegment,
            TableHeadings -> {{"A'", "B'"}, {"x", "y"}},
            TableAlignments -> Center
        ]];
        
        
        Module[{difference, tolerance = 10^-4},
            difference = Norm[Flatten[calculatedSegment] - Flatten[exactFinalSegment]];
            
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
    
    
    Print[Style["\nP\[OHat]vodn\[AAcute] \[UAcute]se\[CHacek]ka AB:", Bold, 14]];
    Print["Body:"];
    Print[MatrixForm[Map[Style[#, Blue] &, pociatocny, {2}]]];
    DisplaySegmentProperties[pociatocny, Blue];
    
    
    If[prva == "Posun",
        
        Module[{dx, dy},
            dx = druhy[[1, 1]] - pociatocny[[1, 1]];
            dy = druhy[[1, 2]] - pociatocny[[1, 2]];
            Print[Style["\nVektor posunu: \[CapitalDelta] = " <> ToString[InputForm[{dx, dy}]], Bold, Blue]];
        ]
    ];
    
    
    Print[Style["\nPo prvej transform\[AAcute]cii (" <> prva <> "):", Bold, 14]];
    Print["Body \[UAcute]se\[CHacek]ky A'B':"];
    
    
    Module[{processedVertices},
        processedVertices = ProcessSegmentVerticesComplete[druhy];
        
        Print[MatrixForm[Map[Style[#, mildGreen] &, processedVertices, {2}]]];
    ];
    
    DisplaySegmentProperties[druhy, mildGreen];
    
    
    If[difficultyLevel == "Medium" || difficultyLevel == "Hard",
        
        Print[Style["\nPo druhej transform\[AAcute]cii (" <> druha <> "):", Bold, 14]];
        Print["Body \[UAcute]se\[CHacek]ky A''B'':"];
        
        
        Module[{processedVertices},
            processedVertices = ProcessSegmentVerticesComplete[treti];
            
            Print[MatrixForm[Map[Style[#, Orange] &, processedVertices, {2}]]];
        ];
        
        DisplaySegmentProperties[treti, Orange];
    ];
    
    
    If[difficultyLevel == "Hard",
        
        Print[Style["\nPo tretej transform\[AAcute]cii (" <> tretia <> "):", Bold, 14]];
        Print["Body \[UAcute]se\[CHacek]ky A'''B''':"];
        
        
        Module[{processedVertices},
            processedVertices = ProcessSegmentVerticesComplete[finalny];
            
            Print[MatrixForm[Map[Style[#, Red] &, processedVertices, {2}]]];
        ];
        
        DisplaySegmentProperties[finalny, Red];
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
            Row[{"Vykonajte posun \[UAcute]se\[CHacek]ky v 2D priestore pomocou vektora posunu ", 
                 Style[ToString[InputForm[params]], Blue, Bold]}],
                 
        "Rot\[AAcute]cia", 
            Row[{"Vykonajte rot\[AAcute]ciu \[UAcute]se\[CHacek]ky v 2D priestore okolo po\[CHacek]iatku s\[UAcute]radnicovej s\[UAcute]stavy [0,0] o uhol ", 
                 Style[ToString[params] <> "\[Degree]", Blue, Bold]}],
                 
        "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie",
            Module[{sx = params[[1]], sy = params[[2]]},
                Row[{
                    "Vykonajte ",
                    If[sx < 1, 
                        Row[{"zmen\[SHacek]enie v smere osi x s koeficientom ", 
                             Style[ToString[InputForm[sx]], Blue, Bold]}],
                        Row[{"zv\[ADoubleDot]\[CHacek]\[SHacek]enie v smere osi x s koeficientom ", 
                             Style[ToString[InputForm[sx]], Blue, Bold]}]
                    ],
                    " a ",
                    If[sy < 1, 
                        Row[{"zmen\[SHacek]enie v smere osi y s koeficientom ", 
                             Style[ToString[InputForm[sy]], Blue, Bold]}],
                        Row[{"zv\[ADoubleDot]\[CHacek]\[SHacek]enie v smere osi y s koeficientom ", 
                             Style[ToString[InputForm[sy]], Blue, Bold]}]
                    ],
                    " \[UAcute]se\[CHacek]ky"
                }]
            ],
            
        "Skosenie",
            With[{
                kx = If[NumberQ[params[[1]]] && !IntegerQ[params[[1]]], Rationalize[params[[1]], 0], params[[1]]],
                ky = If[NumberQ[params[[2]]] && !IntegerQ[params[[2]]], Rationalize[params[[2]], 0], params[[2]]]
            },
                If[kx != 0 && ky == 0,
                    Row[{"Vykonajte skosenie \[UAcute]se\[CHacek]ky v 2D priestore v smere osi x s koeficientom ", 
                         Style[ToString[TraditionalForm[kx]], Blue, Bold]}],
                    If[kx == 0 && ky != 0,
                        Row[{"Vykonajte skosenie \[UAcute]se\[CHacek]ky v 2D priestore v smere osi y s koeficientom ", 
                             Style[ToString[TraditionalForm[ky]], Blue, Bold]}],
                        Row[{"Vykonajte skosenie \[UAcute]se\[CHacek]ky v 2D priestore v smere osi x s koeficientom ", 
                             Style[ToString[TraditionalForm[kx]], Blue, Bold], 
                             " a v smere osi y s koeficientom ", 
                             Style[ToString[TraditionalForm[ky]], Blue, Bold]}]
                    ]
                ]
            ],
            
        "Symetria",
            Switch[params,
                "os x", "Vykonajte symetriu \[UAcute]se\[CHacek]ky v 2D priestore pod\:013ea osi x (priamky y = 0)",
                "os y", "Vykonajte symetriu \[UAcute]se\[CHacek]ky v 2D priestore pod\:013ea osi y (priamky x = 0)",
                "priamka y=x", "Vykonajte symetriu \[UAcute]se\[CHacek]ky v 2D priestore pod\:013ea priamky y = x",
                "priamka y=-x", "Vykonajte symetriu \[UAcute]se\[CHacek]ky v 2D priestore pod\:013ea priamky y = -x",
                "complex", 
                    "Vykonajte symetriu \[UAcute]se\[CHacek]ky v 2D priestore pod\:013ea v\[SHacek]eobecnej priamky",
                _,
                If[ListQ[params] && Length[params] == 3,
                    Module[{a, b, c, standardForm},
                        a = params[[1]];
                        b = params[[2]];
                        c = params[[3]];
                        
                        
                        standardForm = ToString[a] <> "x ";
                        standardForm = standardForm <> If[b >= 0, "+ ", ""] <> ToString[b] <> "y ";
                        If[c != 0, standardForm = standardForm <> If[c >= 0, "+ ", ""] <> ToString[c]];
                        standardForm = standardForm <> " = 0";
                        
                        
                        Row[{
                            "Vykonajte symetriu \[UAcute]se\[CHacek]ky v 2D priestore pod\:013ea priamky ",
                            Style[standardForm, Blue, Bold]
                        }]
                    ],
                    
                    "Vykonajte symetriu \[UAcute]se\[CHacek]ky v 2D priestore pod\:013ea danej priamky"
                ]
            ],
            
        _,
            "Vykonajte " <> transformType <> " \[UAcute]se\[CHacek]ky"
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


CreateSegmentVisualization[pociatocny_, druhy_, treti_, finalny_, prva_, druha_, tretia_] := 
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
                {i, 2}
            ]];
            
            
            AppendTo[labelPositions, Table[
                Module[{vertex = druhy[[i]], offset = {0.35, 0.35}, nearby = {}},
                    If[N[vertex[[1]]] > 0, offset[[1]] = 0.35, offset[[1]] = -0.35];
                    If[N[vertex[[2]]] > 0, offset[[2]] = 0.35, offset[[2]] = -0.35];
                    offset = offset * (0.8 + 0.07 * EuclideanDistance[vertex, {0, 0}]);
                    vertex + offset
                ],
                {i, 2}
            ]];
            
            
            If[showSecond,
                AppendTo[labelPositions, Table[
                    Module[{vertex = treti[[i]], offset = {0.35, 0.35}, nearby = {}},
                        If[N[vertex[[1]]] > 0, offset[[1]] = 0.35, offset[[1]] = -0.35];
                        If[N[vertex[[2]]] > 0, offset[[2]] = 0.35, offset[[2]] = -0.35];
                        offset = offset * (0.8 + 0.07 * EuclideanDistance[vertex, {0, 0}]);
                        vertex + offset
                    ],
                    {i, 2}
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
                    {i, 2}
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
                            {i, 2}
                        ]
                  }
            ];
            
            
            If[showSecond,
                AppendTo[graphicsElements, 
                    {Opacity[0.3], Dashed, Thickness[0.002], brightOrange,
                          Table[
                                Line[{druhy[[i]], treti[[i]]}],
                                {i, 2}
                            ]
                      }
                ];
            ];
            
            
            If[showThird,
                AppendTo[graphicsElements, 
                    {Opacity[0.3], Dashed, Thickness[0.002], brightRed,
                          Table[
                                Line[{treti[[i]], finalny[[i]]}],
                                {i, 2}
                            ]
                      }
                ];
            ];
            
            
            AppendTo[graphicsElements, 
                {brightBlue, Thickness[0.005], Opacity[0.9],
                      Line[pociatocny]
                  }
            ];
            
            
            AppendTo[graphicsElements, 
                {brightGreen, Thickness[0.005], Opacity[0.9],
                      Line[druhy]
                  }
            ];
            
            
            If[showSecond,
                AppendTo[graphicsElements, 
                    {brightOrange, Thickness[0.005], Opacity[0.9],
                          Line[treti]
                      }
                ];
            ];
            
            
            If[showThird,
                AppendTo[graphicsElements, 
                    {brightRed, Thickness[0.005], Opacity[0.9],
                          Line[finalny]
                      }
                ];
            ];
            
            
            
            AppendTo[graphicsElements, 
                Table[
                      {
                            White, Disk[pociatocny[[i]], 0.15],
                            brightBlue, Disk[pociatocny[[i]], 0.12]
                        },
                      {i, 2}
                  ]
            ];
            
            
            AppendTo[graphicsElements, 
                Table[
                      {
                            White, Disk[druhy[[i]], 0.15],
                            brightGreen, Disk[druhy[[i]], 0.12]
                        },
                      {i, 2}
                  ]
            ];
            
            
            If[showSecond,
                AppendTo[graphicsElements, 
                    Table[
                          {
                                White, Disk[treti[[i]], 0.15],
                                brightOrange, Disk[treti[[i]], 0.12]
                            },
                          {i, 2}
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
                          {i, 2}
                      ]
                ];
            ];
            
            
            totalVertices = 2 * (1 + 1 + If[showSecond, 1, 0] + If[showThird, 1, 0]);
            
            
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
                {i, 2}
            ];
            
            
            Do[
                With[{
                        label = FromCharacterCode[64 + (i - 2)] <> "'",
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
                {i, 3, 4}
            ];
            
            
            If[showSecond,
                Do[
                    With[{
                            label = FromCharacterCode[64 + (i - 4)] <> "''",
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
                    {i, 5, 6}
                ];
            ];
            
            
            If[showThird,
                Do[
                    With[{
                            label = FromCharacterCode[64 + (i - 6)] <> "'''",
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
                    {i, 7, 8}
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
                               _, " transform\[AAcute]ci\[IAcute]"] <> " \[UAcute]se\[CHacek]ky", Bold, 16],
              ImagePadding -> {{40, 40}, {40, 40}},
              Background -> White,
              Method -> {"ShrinkWrap" -> True}]
];


UseckaTrojitaTransformaciaSVysledkom[] := Module[{result},
   
   result = Quiet[UseckaTrojitaTransformacia[], {Power::infy, Infinity::indet}];
   
   
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
           
           Print["\nV\[YAcute]sledn\[AAcute] \[UAcute]se\[CHacek]ka:"];
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
            
            Module[{posunVector = UseckaHardBalik`Transforms`Posun`UseckaPosunVector},
                Print[Style["\nVykonajte postupne tieto tri transform\[AAcute]cie:", Bold, 16]];
                Print[Style["1. " <> prva <> " o " <> ToString[posunVector], Bold, Blue]];
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
        Print[Style["M\[AAcute]te \[UAcute]se\[CHacek]ku AB s bodmi:", Bold, 14]];
        Print[MatrixForm[Map[Style[#, Blue] &, pociatocny, {2}]]];
        
        
        Print[Style["\nPOSTUP:", Bold, 16]];
        Print["1. Aplikujte prv\[UAcute] transform\[AAcute]ciu na p\[OHat]vodn\[UAcute] \[UAcute]se\[CHacek]ku AB, \[CHacek]\[IAcute]m z\[IAcute]skate \[UAcute]se\[CHacek]ku A'B'"];
        Print["2. Aplikujte druh\[UAcute] transform\[AAcute]ciu na \[UAcute]se\[CHacek]ku A'B', \[CHacek]\[IAcute]m z\[IAcute]skate \[UAcute]se\[CHacek]ku A''B''"];
        Print["3. Aplikujte tretiu transform\[AAcute]ciu na \[UAcute]se\[CHacek]ku A''B'', \[CHacek]\[IAcute]m z\[IAcute]skate fin\[AAcute]lnu \[UAcute]se\[CHacek]ku A'''B'''"];
        
        Print[Style["\nPOZN\[CapitalAAcute]MKA:", Bold, 14]];
        Print["\[Bullet] Pre ka\[ZHacek]d\[UAcute] transform\[AAcute]ciu si zap\[IAcute]\[SHacek]te pr\[IAcute]slu\[SHacek]n\[UAcute] transforma\[CHacek]n\[UAcute] maticu"];
        Print["\[Bullet] Vypo\[CHacek]\[IAcute]tajte nov\[EAcute] s\[UAcute]radnice v\[SHacek]etk\[YAcute]ch bodov po ka\[ZHacek]dej transform\[AAcute]cii"];
        Print["\[Bullet] Nezabudnite na spr\[AAcute]vne pou\[ZHacek]itie homog\[EAcute]nnych s\[UAcute]radn\[IAcute]c pre v\[SHacek]etky v\[YAcute]po\[CHacek]ty"];
        
        Print[Style["\nPo stla\[CHacek]en\[IAcute] tla\[CHacek]idla pokra\[CHacek]ova\[THacek] sa zobraz\[IAcute] podrobn\[YAcute] postup v\[YAcute]po\[CHacek]tu.", Bold, 12]];
    ];


SelectTransformation[message_, exclude1_:"", exclude2_:""] := 
    Module[{options, fullOptions, formattedChoices, result},
        options = DeleteCases[
            {
                "Posun" -> {"Posun", "Posun \[UAcute]se\[CHacek]ky vo smere vektora"},
                "Rot\[AAcute]cia" -> {"Rot\[AAcute]cia", "Rot\[AAcute]cia \[UAcute]se\[CHacek]ky okolo po\[CHacek]iatku s\[UAcute]radnicovej s\[UAcute]stavy"},
                "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie" -> {"Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie alebo zmen\[SHacek]enie \[UAcute]se\[CHacek]ky v r\[OHat]znych smeroch"},
                "Skosenie" -> {"Skosenie", "Skosenie \[UAcute]se\[CHacek]ky v smere os\[IAcute]"},
                "Symetria" -> {"Symetria", "Zrkadlenie \[UAcute]se\[CHacek]ky pod\:013ea zvolenej osi"}
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



UseckaZvacsenieZmensenieNoDisplayWithParams[inputVertices_] := 
    Module[{sx, sy, finalVertices, normalizedVertices, detectedScaling = None, detectedFinalVertices = None},
            
        
        If[Length[inputVertices] > 2 && Mod[Length[inputVertices], 2] == 0,
            normalizedVertices = inputVertices[[1;;2]];
            detectedFinalVertices = inputVertices[[-(2;;-1)]];
            
            detectedScaling = DetectScalingParameters[normalizedVertices, detectedFinalVertices],
            
            normalizedVertices = inputVertices
        ];
            
        
        If[detectedScaling =!= None,
            sx = detectedScaling[[1]];
            sy = detectedScaling[[2]];
            finalVertices = detectedFinalVertices,
            
            
            If[UseckaHardBalik`Transforms`ZvacsenieZmensenie`ZvacsenieParams =!= {0, 0},
                
                {sx, sy} = UseckaHardBalik`Transforms`ZvacsenieZmensenie`ZvacsenieParams;
                
                UseckaHardBalik`Transforms`ZvacsenieZmensenie`ZvacsenieParams = {0, 0},
                
                
                Module[{result},
                    result = UseckaHardBalik`Transforms`ZvacsenieZmensenie`GenerateScalingParameters[];
                    sx = result[[1]];
                    sy = result[[2]];
                ]
            ];
            
            
            finalVertices = Map[
                Function[point,
                    {Simplify[point[[1]]*sx], Simplify[point[[2]]*sy]}
                ],
                normalizedVertices
            ]
        ];
        
        
        UseckaHardBalik`Transforms`ZvacsenieZmensenie`ZvacsenieParams = {sx, sy};
        
        
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
        While[i <= 2 && !foundScaling,
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


UseckaRotaciaNoDisplayWithParams[vertices_] := 
    Module[{angle, result},
        
        If[UseckaHardBalik`Transforms`Rotacia`RotaciaUhol != 0,
            angle = UseckaHardBalik`Transforms`Rotacia`RotaciaUhol,
            angle = RandomChoice[{30, 45, 60, 90, 120, 135, 180, 270}];
            
            UseckaHardBalik`Transforms`Rotacia`RotaciaUhol = angle;
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


UseckaSkosenieNoDisplayWithParams[vertices_] := 
    Module[{kx, ky, result},
        
        If[UseckaHardBalik`Transforms`Skosenie`SkosenieParams =!= {0, 0},
            {kx, ky} = UseckaHardBalik`Transforms`Skosenie`SkosenieParams;
            
            UseckaHardBalik`Transforms`Skosenie`SkosenieParams = {0, 0},
            
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
        
        
        UseckaHardBalik`Transforms`Skosenie`SkosenieParams = {kx, ky};
        
        
        {{kx, ky}, result}
    ];


UseckaSymetriaNoDisplayWithParams[vertices_] := 
    Module[{axis, result, a, b, c, lineParams, specialAxes, randomAxis},
        
        specialAxes = {"os x", "os y", "priamka y=x", "priamka y=-x"};
        
        
        If[TrueMemberQ[{Null, "complex"}, UseckaHardBalik`Transforms`Symetria`SymetriaOs] || 
           !MemberQ[Join[specialAxes, _List], UseckaHardBalik`Transforms`Symetria`SymetriaOs],
           
           
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
           
           axis = UseckaHardBalik`Transforms`Symetria`SymetriaOs;
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
        
        
        UseckaHardBalik`Transforms`Symetria`SymetriaOs = axis;
        
        
        {axis, result}
    ];
    

EnsureExactValues[vertices_] := 
    Map[Function[vertex, Map[Rationalize[#, 0] &, vertex]], vertices];


UseckaTrojitaTransformacia[] := Module[{
    difficultyLevel,
    numTransformations,
    pociatocnyUsecka,
    prvaTransformacia,
    druhyUsecka,
    druhaTransformacia = "\[CapitalZHacek]iadna",
    tretiUsecka,
    tretiaTransformacia = "\[CapitalZHacek]iadna",
    finalnyUsecka,
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
    
    
    UseckaHardBalik`Transforms`Symetria`SymetriaOs = Null;
    UseckaHardBalik`Transforms`Rotacia`RotaciaUhol = 0;
    UseckaHardBalik`Transforms`Posun`UseckaPosunVector = {0, 0};
    UseckaHardBalik`Transforms`ZvacsenieZmensenie`ZvacsenieParams = {0, 0};
    UseckaHardBalik`Transforms`Skosenie`SkosenieParams = {0, 0};

    
    difficultyLevel = SelectDifficultyLevel[];
    If[difficultyLevel === $Canceled, Return[]];
    
    
    numTransformations = Switch[difficultyLevel,
        "Easy", 1,
        "Medium", 2,
        "Hard", 3,
        _, 3 
    ];
    
    
    Quiet[
    
    
    pociatocnyUsecka = GenerateInitialSegment[];
    transformovane = {pociatocnyUsecka};
    
    
    If[numTransformations >= 1, 
        
        prvaPopis = SelectTransformation["Vyberte prv\[UAcute] transform\[AAcute]ciu:"];
        If[prvaPopis === $Canceled, Return[]];
        prvaTransformacia = First[prvaPopis];
        AppendTo[transformacie, prvaTransformacia];
        
        
        druhyUsecka = Switch[prvaTransformacia,
            "Posun", Module[{result, dx, dy, translationParams}, 
                
                translationParams = GetTranslationParametersNoDisplay[pociatocnyUsecka];
                
                
                posunVektor1 = {translationParams[[1]], translationParams[[2]]};
                AppendTo[transformParams, posunVektor1];
                
                
                {dx, dy, _, result} = translationParams;
                result],
                
            "Rot\[AAcute]cia", Module[{result, angle},
                
                If[rotaciaUhol1 != 0,
                    UseckaHardBalik`Transforms`Rotacia`RotaciaUhol = rotaciaUhol1;
                ];
                
                
                {angle, result} = UseckaRotaciaNoDisplayWithParams[pociatocnyUsecka];
                
                
                rotaciaUhol1 = angle;
                AppendTo[transformParams, rotaciaUhol1];
                
                result],
                
            "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", Module[{result, params},
                
                {params, result} = UseckaZvacsenieZmensenieNoDisplayWithParams[pociatocnyUsecka];
                
                
                zvacsenieParams1 = params;
                AppendTo[transformParams, zvacsenieParams1];
                
                result],
                
            "Skosenie", Module[{result, params},
                
                {params, result} = UseckaSkosenieNoDisplayWithParams[pociatocnyUsecka];
                
                
                skosenieParams1 = params;
                AppendTo[transformParams, skosenieParams1];
                
                result],
                
            "Symetria", Module[{result, axis},
                
                UseckaHardBalik`Transforms`Symetria`SymetriaOs = Null;
                
                
                {axis, result} = UseckaSymetriaNoDisplayWithParams[pociatocnyUsecka];
                
                
                symetriaOs1 = axis;
                AppendTo[transformParams, symetriaOs1];
                
                result]
        ];
        AppendTo[transformovane, druhyUsecka];
        finalnyUsecka = druhyUsecka;
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
        
        
        tretiUsecka = Switch[druhaTransformacia,
            "Posun", Module[{result, dx, dy, translationParams}, 
                translationParams = GetTranslationParametersNoDisplay[EnsureExactValues[druhyUsecka]];
                posunVektor2 = {translationParams[[1]], translationParams[[2]]};
                AppendTo[transformParams, posunVektor2];
                {dx, dy, _, result} = translationParams;
                result],
                
            "Rot\[AAcute]cia", Module[{result, angle},
                If[rotaciaUhol2 != 0,
                    UseckaHardBalik`Transforms`Rotacia`RotaciaUhol = rotaciaUhol2;
                ];
                {angle, result} = UseckaRotaciaNoDisplayWithParams[EnsureExactValues[druhyUsecka]];
                rotaciaUhol2 = angle;
                AppendTo[transformParams, rotaciaUhol2];
                result],
                
            "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", Module[{result, params},
                {params, result} = UseckaZvacsenieZmensenieNoDisplayWithParams[EnsureExactValues[druhyUsecka]];
                zvacsenieParams2 = params;
                AppendTo[transformParams, zvacsenieParams2];
                result],
                
            "Skosenie", Module[{result, params},
                {params, result} = UseckaSkosenieNoDisplayWithParams[EnsureExactValues[druhyUsecka]];
                skosenieParams2 = params;
                AppendTo[transformParams, skosenieParams2];
                result],
                
            "Symetria", Module[{result, axis},
                
                UseckaHardBalik`Transforms`Symetria`SymetriaOs = Null;
                
                
                {axis, result} = UseckaSymetriaNoDisplayWithParams[EnsureExactValues[druhyUsecka]];
                
                
                symetriaOs2 = axis;
                AppendTo[transformParams, symetriaOs2];
                
                result]
        ];
        AppendTo[transformovane, tretiUsecka];
        finalnyUsecka = tretiUsecka;
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
        
        
        finalnyUsecka = Switch[tretiaTransformacia,
            "Posun", Module[{result, dx, dy, translationParams}, 
                translationParams = GetTranslationParametersNoDisplay[EnsureExactValues[tretiUsecka]];
                posunVektor3 = {translationParams[[1]], translationParams[[2]]};
                AppendTo[transformParams, posunVektor3];
                {dx, dy, _, result} = translationParams;
                result],
                
            "Rot\[AAcute]cia", Module[{result, angle},
                If[rotaciaUhol3 != 0,
                    UseckaHardBalik`Transforms`Rotacia`RotaciaUhol = rotaciaUhol3;
                ];
                {angle, result} = UseckaRotaciaNoDisplayWithParams[EnsureExactValues[tretiUsecka]];
                rotaciaUhol3 = angle;
                AppendTo[transformParams, rotaciaUhol3];
                result],
                
            "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", Module[{result, params},
                {params, result} = UseckaZvacsenieZmensenieNoDisplayWithParams[EnsureExactValues[tretiUsecka]];
                zvacsenieParams3 = params;
                AppendTo[transformParams, zvacsenieParams3];
                result],
                
            "Skosenie", Module[{result, params},
                {params, result} = UseckaSkosenieNoDisplayWithParams[EnsureExactValues[tretiUsecka]];
                skosenieParams3 = params;
                AppendTo[transformParams, skosenieParams3];
                result],
                
            "Symetria", Module[{result, axis},
                
                UseckaHardBalik`Transforms`Symetria`SymetriaOs = Null;
                
                
                {axis, result} = UseckaSymetriaNoDisplayWithParams[EnsureExactValues[tretiUsecka]];
                
                
                symetriaOs3 = axis;
                AppendTo[transformParams, symetriaOs3];
                
                result]
        ];
        AppendTo[transformovane, finalnyUsecka];
    ];
    
    
    Print[Style["GEOMETRICK\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA \[CapitalUAcute]SE\[CapitalCHacek]KY - " <> 
           Switch[difficultyLevel, "Easy", "JEDNODUCH\[CapitalAAcute] \[CapitalUAcute]LOHA", 
                              "Medium", "STREDNE \[CapitalTHacek]A\[CapitalZHacek]K\[CapitalAAcute] \[CapitalUAcute]LOHA",
                              "Hard", "ZLO\[CapitalZHacek]IT\[CapitalAAcute] \[CapitalUAcute]LOHA"], Bold, 24]];
    Print[Style["==========================================", Bold]];
    
    Print[Style["\nZADANIE PRE \[CapitalSHacek]TUDENTA:", Bold, 18]];
    Print["Majme \[UAcute]se\[CHacek]ku AB s bodmi:"];
    Print[MatrixForm[Map[Style[#, Blue] &, pociatocnyUsecka, {2}]]];
    
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

    
    
    
    Print[Style["GEOMETRICK\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA \[CapitalUAcute]SE\[CapitalCHacek]KY - " <> 
           Switch[difficultyLevel, "Easy", "JEDNODUCH\[CapitalAAcute] \[CapitalUAcute]LOHA", 
                              "Medium", "STREDNE \[CapitalTHacek]A\[CapitalZHacek]K\[CapitalAAcute] \[CapitalUAcute]LOHA",
                              "Hard", "ZLO\[CapitalZHacek]IT\[CapitalAAcute] \[CapitalUAcute]LOHA"], Bold, 24]];
    Print[Style["==========================================", Bold]];
    
    
    Print[Style["\nPO\[CapitalCHacek]IATO\[CapitalCHacek]N\[CapitalAAcute] \[CapitalUAcute]SE\[CapitalCHacek]KA:", Bold, 16]];
    Print["Body \[UAcute]se\[CHacek]ky AB:"];
    Print[MatrixForm[Map[Style[#, Blue] &, pociatocnyUsecka, {2}]]];
    DisplaySegmentProperties[pociatocnyUsecka, Blue];
    
    
    If[numTransformations >= 1,
        Print[Style["\nPRV\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA: " <> prvaTransformacia, Bold, 16]];
        
        
        druhyUsecka = Switch[prvaTransformacia,
            "Posun", 
                Module[{}, 
                    UseckaHardBalik`Transforms`Posun`UseckaPosunVector = posunVektor1;
                    UseckaPosun[pociatocnyUsecka]
                ],
            "Rot\[AAcute]cia", 
                Module[{}, 
                    UseckaHardBalik`Transforms`Rotacia`RotaciaUhol = rotaciaUhol1;
                    UseckaRotacia[pociatocnyUsecka]
                ],
            "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", 
                Module[{}, 
                    UseckaHardBalik`Transforms`ZvacsenieZmensenie`ZvacsenieParams = zvacsenieParams1;
                    UseckaZvacsenieZmensenie[pociatocnyUsecka]
                ],
            "Skosenie", 
                Module[{}, 
                    UseckaHardBalik`Transforms`Skosenie`SkosenieParams = skosenieParams1;
                    UseckaSkosenie[pociatocnyUsecka]
                ],
            "Symetria", 
                Module[{}, 
                    
                    UseckaHardBalik`Transforms`Symetria`SymetriaOs = symetriaOs1;
                    UseckaSymetria[pociatocnyUsecka]
                ]
        ];
    ];
    
    
    If[numTransformations >= 2,
        Print[Style["\nDRUH\[CapitalAAcute] TRANSFORM\[CapitalAAcute]CIA: " <> druhaTransformacia, Bold, 16]];
        
        
        tretiUsecka = Switch[druhaTransformacia,
            "Posun", 
                Module[{}, 
                    UseckaHardBalik`Transforms`Posun`UseckaPosunVector = posunVektor2;
                    UseckaPosun[EnsureExactValues[druhyUsecka]]
                ],
            "Rot\[AAcute]cia", 
                Module[{}, 
                    UseckaHardBalik`Transforms`Rotacia`RotaciaUhol = rotaciaUhol2;
                    UseckaRotacia[EnsureExactValues[druhyUsecka]]
                ],
            "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", 
                Module[{}, 
                    UseckaHardBalik`Transforms`ZvacsenieZmensenie`ZvacsenieParams = zvacsenieParams2;
                    UseckaZvacsenieZmensenie[EnsureExactValues[druhyUsecka]]
                ],
            "Skosenie", 
                Module[{}, 
                    UseckaHardBalik`Transforms`Skosenie`SkosenieParams = skosenieParams2;
                    UseckaSkosenie[EnsureExactValues[druhyUsecka]]
                ],
            "Symetria", 
                Module[{}, 
                    
                    UseckaHardBalik`Transforms`Symetria`SymetriaOs = symetriaOs2;
                    UseckaSymetria[EnsureExactValues[druhyUsecka]]
                ]
        ];
    ];
    
    
    If[numTransformations >= 3,
        Print[Style["\nTRETIA TRANSFORM\[CapitalAAcute]CIA: " <> tretiaTransformacia, Bold, 16]];
        
        
        finalnyUsecka = Switch[tretiaTransformacia,
            "Posun", 
                Module[{}, 
                    UseckaHardBalik`Transforms`Posun`UseckaPosunVector = posunVektor3;
                    UseckaPosun[EnsureExactValues[tretiUsecka]]
                ],
            "Rot\[AAcute]cia", 
                Module[{}, 
                    UseckaHardBalik`Transforms`Rotacia`RotaciaUhol = rotaciaUhol3;
                    UseckaRotacia[EnsureExactValues[tretiUsecka]]
                ],
            "Zv\[ADoubleDot]\[CHacek]\[SHacek]enie/Zmen\[SHacek]enie", 
                Module[{}, 
                    UseckaHardBalik`Transforms`ZvacsenieZmensenie`ZvacsenieParams = zvacsenieParams3;
                    UseckaZvacsenieZmensenie[EnsureExactValues[tretiUsecka]]
                ],
            "Skosenie", 
                Module[{}, 
                    UseckaHardBalik`Transforms`Skosenie`SkosenieParams = skosenieParams3;
                    UseckaSkosenie[EnsureExactValues[tretiUsecka]]
                ],
            "Symetria", 
                Module[{}, 
                    
                    UseckaHardBalik`Transforms`Symetria`SymetriaOs = symetriaOs3;
                    UseckaSymetria[EnsureExactValues[tretiUsecka]]
                ]
        ];
    ];
    
    
    DisplayTransformationSequence[
        pociatocnyUsecka,
        druhyUsecka,
        If[numTransformations >= 2, tretiUsecka, druhyUsecka],  
        finalnyUsecka,
        prvaTransformacia,
        If[numTransformations >= 2, druhaTransformacia, "\[CapitalZHacek]iadna"],
        If[numTransformations >= 3, tretiaTransformacia, "\[CapitalZHacek]iadna"],
        difficultyLevel,  
        transformParams   
    ];
    
    
    Print[Style["\nS\[CapitalUAcute]HRNN\[CapitalAAcute] VIZUALIZ\[CapitalAAcute]CIA:", Bold, 16]];
    Switch[numTransformations,
        1,
            Print[CreateSegmentVisualization[
                pociatocnyUsecka,
                finalnyUsecka,
                finalnyUsecka,  
                finalnyUsecka,  
                prvaTransformacia,
                "\[CapitalZHacek]iadna",
                "\[CapitalZHacek]iadna"
            ]],
        2,
            Print[CreateSegmentVisualization[
                pociatocnyUsecka,
                druhyUsecka,
                finalnyUsecka,
                finalnyUsecka,  
                prvaTransformacia,
                druhaTransformacia,
                "\[CapitalZHacek]iadna"
            ]],
        3,
            Print[CreateSegmentVisualization[
                pociatocnyUsecka,
                druhyUsecka,
                tretiUsecka,
                finalnyUsecka,
                prvaTransformacia,
                druhaTransformacia,
                tretiaTransformacia
            ]]
    ];
    
    
    Print[Style["\nLEGENDA:", Bold, 16]];
    Print[Style["\[Bullet] Modr\[EAcute]", RGBColor[0, 0.4, 0.8], Bold], " body a \[CHacek]iary: P\[OHat]vodn\[AAcute] \[UAcute]se\[CHacek]ka AB"];
    
    
    If[numTransformations >= 1,
        Print[Style["\[Bullet] Tmavozelen\[EAcute]", RGBColor[0, 0.8, 0.2], Bold], " body a \[CHacek]iary: \[CapitalUAcute]se\[CHacek]ka po prvej transform\[AAcute]cii (", 
              Style[prvaTransformacia, Bold], ")"];
    ];
    
    If[numTransformations >= 2,
        Print[Style["\[Bullet] Oran\[ZHacek]ov\[EAcute]", RGBColor[1, 0.6, 0], Bold], " body a \[CHacek]iary: \[CapitalUAcute]se\[CHacek]ka po druhej transform\[AAcute]cii (", 
              Style[druhaTransformacia, Bold], ")"];
    ];
    
    If[numTransformations >= 3,
        Print[Style["\[Bullet] \[CapitalCHacek]erven\[EAcute]", RGBColor[0.9, 0.1, 0.1], Bold], " body a \[CHacek]iary: \[CapitalUAcute]se\[CHacek]ka po tretej transform\[AAcute]cii (", 
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
    
    
    {pociatocnyUsecka, transformovane, transformacie, difficultyLevel, transformParams}
    
    ] 
];



UseckaRotaciaNoDisplay[vertices_] := 
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















