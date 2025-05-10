(* ::Package:: *)

(* ::Package:: *)
(**)








BeginPackage["DomcekHardBalik`Transforms`Symetria`", {"DomcekHardBalik`"}];

DomcekSymetriaMatrix::usage = 
    "DomcekSymetriaMatrix[a_, b_, c_] returns the transformation matrix for symmetry with respect to line ax + by + c = 0.";


DomcekSymetriaMatrix[a_, b_, c_] := 
    CalculateSymmetryMatrix[a, b, c];
    

SymetriaOs::usage = "SymetriaOs je glob\[AAcute]lna premenn\[AAcute] pre os symetrie {a, b, c}, ktor\[AAcute] sa pou\[ZHacek]\[IAcute]va v DomcekSymetria funkcii.";

DomcekSymetria::usage = 
    "DomcekSymetria[vertices_] zobraz\[IAcute] interakt\[IAcute]vny tutorial pre symetriu dom\[CHacek]eka pod\:013ea priamky a vr\[AAcute]ti nov\[EAcute] vrcholy.";

DomcekSymetriaNoDisplayWithParams::usage = 
    "DomcekSymetriaNoDisplayWithParams[vertices_] z\[IAcute]ska parametre symetrie a aplikuje ich bez zobrazovania.";

Begin["`Private`"];


SymetriaOs = {1, 1, 0};  


DisplayIntuitiveMathMatrixMultiplication[matrix_, vector_, result_, pointName_] := 
    Module[{darkGreen = RGBColor[0, 0.5, 0]},
        
        Print[Style["Maticov\[YAcute] z\[AAcute]pis transform\[AAcute]cie bodu " <> pointName <> ":", Bold]];
        
        
        Print[
            Grid[{
                {
                    MatrixForm[Map[Style[#, Red] &, matrix, {2}]], 
                    " \[CenterDot] ", 
                    MatrixForm[Map[Style[#, Blue] &, vector]],
                    " = ",
                    MatrixForm[Map[Style[#, darkGreen] &, result]]
                }
            }]
        ];
        
        
        Print[Style["Podrobn\[YAcute] v\[YAcute]po\[CHacek]et jednotliv\[YAcute]ch s\[UAcute]radn\[IAcute]c:", Bold]];
        
        
        Print[Style["\[Bullet] V\[YAcute]po\[CHacek]et novej x-ovej s\[UAcute]radnice (1. riadok matice):", Brown]];
        
        
        Print[
            Grid[{
                {
                    "   [", 
                    Style[matrix[[1, 1]], Red], 
                    Style[matrix[[1, 2]], Red], 
                    "] \[CenterDot] [",
                    Style[vector[[1]], Blue],
                    Style[vector[[2]], Blue],
                    "]^T =",
                    Style[result[[1]], darkGreen]
                }
            }, Alignment -> Left]
        ];
        
        
        Print["   = (", Style[matrix[[1, 1]], Red], " \[CenterDot] ", Style[vector[[1]], Blue], 
              ") + (", Style[matrix[[1, 2]], Red], " \[CenterDot] ", Style[vector[[2]], Blue], ")"];
        
        
        Print["   = ", Style[matrix[[1, 1]]*vector[[1]], Purple], 
              " + ", Style[matrix[[1, 2]]*vector[[2]], Purple]];
        
        
        Print["   = ", Style[result[[1]], darkGreen]];
        
        
        Print[Style["\n\[Bullet] V\[YAcute]po\[CHacek]et novej y-ovej s\[UAcute]radnice (2. riadok matice):", Brown]];
        
        
        Print[
            Grid[{
                {
                    "   [", 
                    Style[matrix[[2, 1]], Red], 
                    Style[matrix[[2, 2]], Red], 
                    "] \[CenterDot] [",
                    Style[vector[[1]], Blue],
                    Style[vector[[2]], Blue],
                    "]^T =",
                    Style[result[[2]], darkGreen]
                }
            }, Alignment -> Left]
        ];
        
        
        Print["   = (", Style[matrix[[2, 1]], Red], " \[CenterDot] ", Style[vector[[1]], Blue], 
              ") + (", Style[matrix[[2, 2]], Red], " \[CenterDot] ", Style[vector[[2]], Blue], ")"];
        
        
        Print["   = ", Style[matrix[[2, 1]]*vector[[1]], Purple], 
              " + ", Style[matrix[[2, 2]]*vector[[2]], Purple]];
        
        
        Print["   = ", Style[result[[2]], darkGreen]];
        
        
        Print[Style["\nV\[YAcute]sledn\[AAcute] reprezent\[AAcute]cia bodu " <> pointName <> "':", Bold]];
        Print["[", Style[result[[1]], darkGreen], ", ", Style[result[[2]], darkGreen], "]"];
    ];


NormalizeVertices[vertices_] := 
    Module[{normalizedVertices, processedVertices},
        
        processedVertices = Which[
            
            MatrixQ[vertices], vertices,
            
            
            ListQ[vertices] && Length[Dimensions[vertices]] == 2, 
                If[Length[DeleteDuplicates[Length /@ vertices]] > 1,
                    
                    Block[{cleanedVertices = Select[vertices, VectorQ]},
                        If[Length[cleanedVertices] >= 5,
                            cleanedVertices[[1;;5]],
                            Message[DomcekSymetria::invalidInput]; 
                            Abort[]
                        ]
                    ],
                    
                    vertices
                ],
            
            
            ListQ[vertices] && Length[Dimensions[vertices]] == 1 && EvenQ[Length[vertices]], 
                Partition[vertices, 2],
                
            
            True, 
                Message[DomcekSymetria::invalidInput]; 
                Abort[]
        ];
        
        
        normalizedVertices = processedVertices;
        
        
        If[Length[normalizedVertices] < 5,
            Message[DomcekSymetria::insufficientVertices];
            Abort[]
        ];
        
        
        If[Max[Abs[N[normalizedVertices[[All, 1]]]]] > 10 || Max[Abs[N[normalizedVertices[[All, 2]]]]] > 10,
            (* \[CapitalSHacek]k\[AAcute]lovanie vrcholov, ak s\[UAcute] pr\[IAcute]li\[SHacek] \[DHacek]aleko, 
               ale zachov\[AAcute]me presn\[UAacute] aritmetiku *)
            normalizedVertices = normalizedVertices / (Max[Abs[N[Flatten[normalizedVertices]]]] / 6);
        ];
        
        
        normalizedVertices[[1;;5]]
    ];

ReflectPointOverLine[point_, a_, b_, c_] := 
    Module[{x, y, d, xNew, yNew, factor},
        x = point[[1]];
        y = point[[2]];
        
        
        factor = a^2 + b^2;  
        d = (a*x + b*y + c);
        
        
        xNew = x - 2*a*d/factor;
        yNew = y - 2*b*d/factor;
        
        
        xNew = Simplify[xNew];
        yNew = Simplify[yNew];
        
        
        {xNew, yNew}
    ];


ConvertAxisToCoefficients[axis_] := 
    Module[{a = 0, b = 0, c = 0, result},
        Which[
            
            ListQ[axis] && Length[axis] == 3,
                result = axis,
                
            
            ListQ[axis] && Length[axis] == 2,
                result = Append[axis, 0],
                
            
            axis === "os x" || axis === "x" || axis === "os x",
                result = {0, 1, 0},  
                
            axis === "os y" || axis === "y" || axis === "os y",
                result = {1, 0, 0},  
                
            axis === "priamka y=x" || axis === "y = x" || axis === "x = y", 
                result = {1, -1, 0},  
                
            axis === "priamka y=-x" || axis === "y = -x" || axis === "x = -y", 
                result = {1, 1, 0},  
            
            
            StringQ[axis] && StringMatchQ[axis, "y = "*_~~ "x" ~~ ___],
                Module[{m, n},
                    
                    If[StringMatchQ[axis, "y = " ~~ _ ~~ "x"],
                        
                        m = ToExpression[StringReplace[axis, "y = " ~~ m_ ~~ "x" :> m]];
                        n = 0,
                        
                        m = If[StringMatchQ[axis, "y = " ~~ _ ~~ "x + " ~~ _],
                                ToExpression[StringReplace[axis, "y = " ~~ m_ ~~ "x + " ~~ _ :> m]],
                                ToExpression[StringReplace[axis, "y = " ~~ m_ ~~ "x - " ~~ _ :> m]]
                             ];
                        n = If[StringMatchQ[axis, "y = " ~~ _ ~~ "x + " ~~ _],
                                ToExpression[StringReplace[axis, "y = " ~~ _ ~~ "x + " ~~ n_ :> n]],
                                -ToExpression[StringReplace[axis, "y = " ~~ _ ~~ "x - " ~~ n_ :> n]]
                             ];
                    ];
                    
                    If[n != 0,
                        result = {-m, 1, -n},  
                        result = {-m, 1, 0}    
                    ]
                ],
                
            
            True,
                result = {1, 1, 0}  
        ];
        
        
        If[result[[1]] != 0 || result[[2]] != 0,
            Module[{gcd = GCD @@ (If[IntegerQ[#], #, Numerator[#]] & /@ DeleteCases[result, 0])},
                If[gcd > 1, result = result/gcd]
            ]
        ];
        
        
        If[Length[result] < 3, 
            result = PadRight[result, 3, 0]
        ];
        
        
        If[result[[1]] < 0,
            result = -result
        ];
        
        result
    ];


GenerateSymmetryLine[vertices_] := 
    Module[{a, b, c, validLine = False, allVertices, 
            lineEquation, lineType, popis, xIntercept, yIntercept, 
            testVertices, specialLines, invNewVertices, 
            attempts = 0, maxAttempts = 100, 
            simpleCoefficients, simpleIntercepts, 
            initialVertices, targetVertices, invTransform, complexLines = {}},
        
        
        SeedRandom[Hash[{ToString[Date[]], ToString[vertices], RandomReal[]}]];
        
        
        allVertices = vertices;
        initialVertices = vertices;
        
        
        invTransform[points_, a_, b_, c_] := 
            Map[ReflectPointOverLine[#, a, b, c] &, points];
        
        
        simpleCoefficients = {-5, -4, -3, -2, -1, 1, 2, 3, 4, 5};
        
        
        simpleIntercepts = {-6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6};
        
        
        specialLines = {
            
            {0, 1, 0, "os x", "symetria pod\:013ea osi x"},
            
            
            {1, 0, 0, "os y", "symetria pod\:013ea osi y"},
            
            
            {1, -1, 0, "priamka y = x", "symetria pod\:013ea priamky y = x"},
            
            
            {1, 1, 0, "priamka y = -x", "symetria pod\:013ea priamky y = -x"},
            
            
            {0, 1, -1, "priamka y = 1", "symetria pod\:013ea priamky y = 1"},
            {0, 1, -2, "priamka y = 2", "symetria pod\:013ea priamky y = 2"},
            {0, 1, 1, "priamka y = -1", "symetria pod\:013ea priamky y = -1"},
            {0, 1, 2, "priamka y = -2", "symetria pod\:013ea priamky y = -2"},
            
            
            {1, 0, -1, "priamka x = 1", "symetria pod\:013ea priamky x = 1"},
            {1, 0, -2, "priamka x = 2", "symetria pod\:013ea priamky x = 2"},
            {1, 0, 1, "priamka x = -1", "symetria pod\:013ea priamky x = -1"},
            {1, 0, 2, "priamka x = -2", "symetria pod\:013ea priamky x = -2"}
        };
        
        
        complexLines = {
            
            {2, -3, 0, "priamka 2x - 3y = 0", "symetria pod\:013ea priamky y = 2/3 x"},
            {3, -2, 0, "priamka 3x - 2y = 0", "symetria pod\:013ea priamky y = 3/2 x"},
            {2, -1, 0, "priamka 2x - y = 0", "symetria pod\:013ea priamky y = 2x"},
            {1, -2, 0, "priamka x - 2y = 0", "symetria pod\:013ea priamky y = 1/2 x"},
            {3, -1, 0, "priamka 3x - y = 0", "symetria pod\:013ea priamky y = 3x"},
            {1, -3, 0, "priamka x - 3y = 0", "symetria pod\:013ea priamky y = 1/3 x"},
            
            
            {1, -1, 2, "priamka x - y + 2 = 0", "symetria pod\:013ea priamky y = x + 2"},
            {1, -1, -2, "priamka x - y - 2 = 0", "symetria pod\:013ea priamky y = x - 2"},
            {1, -2, 3, "priamka x - 2y + 3 = 0", "symetria pod\:013ea priamky y = 1/2 x + 3/2"},
            {2, -1, 3, "priamka 2x - y + 3 = 0", "symetria pod\:013ea priamky y = 2x + 3"},
            {3, -2, 4, "priamka 3x - 2y + 4 = 0", "symetria pod\:013ea priamky y = 3/2 x + 2"},
            {2, -3, 5, "priamka 2x - 3y + 5 = 0", "symetria pod\:013ea priamky y = 2/3 x + 5/3"},
            
            
            {2, -5, 3, "priamka 2x - 5y + 3 = 0", "symetria pod\:013ea priamky y = 2/5 x + 3/5"},
            {3, -4, 2, "priamka 3x - 4y + 2 = 0", "symetria pod\:013ea priamky y = 3/4 x + 1/2"},
            {4, -3, -2, "priamka 4x - 3y - 2 = 0", "symetria pod\:013ea priamky y = 4/3 x - 2/3"},
            {5, -2, -4, "priamka 5x - 2y - 4 = 0", "symetria pod\:013ea priamky y = 5/2 x - 2"},
            {4, -5, 6, "priamka 4x - 5y + 6 = 0", "symetria pod\:013ea priamky y = 4/5 x + 6/5"},
            {5, -3, -1, "priamka 5x - 3y - 1 = 0", "symetria pod\:013ea priamky y = 5/3 x - 1/3"}
        };
        
        
        specialLines = Join[specialLines, complexLines];
        
        
        
        
        
        
        While[!validLine && attempts < maxAttempts,
            attempts++;
            
            
            If[RandomReal[] < 0.25,  
                
                {a, b, c, lineType, popis} = RandomChoice[specialLines];
            ,
                
                a = RandomChoice[simpleCoefficients];
                b = RandomChoice[simpleCoefficients];
                c = RandomChoice[simpleIntercepts];
                
                
                While[a == 0 && b == 0,
                    a = RandomChoice[simpleCoefficients];
                ];
                
                
                If[GCD[Abs[a], Abs[b], Abs[c]] > 1,
                    {a, b, c} = {a, b, c}/GCD[Abs[a], Abs[b], Abs[c]];
                ];
                
                
                If[b != 0,
                    
                    Module[{slope = -a/b, intercept = -c/b},
                        If[Denominator[slope] == 1, 
                            lineEquation = "y = " <> ToString[slope] <> "x", 
                            lineEquation = "y = " <> ToString[Numerator[slope]] <> 
                                          "/" <> ToString[Denominator[slope]] <> "x"
                        ];
                        
                        If[intercept != 0,
                            If[intercept > 0,
                                If[Denominator[intercept] == 1, 
                                    lineEquation = lineEquation <> " + " <> ToString[intercept],
                                    lineEquation = lineEquation <> " + " <> ToString[Numerator[intercept]] <> 
                                                  "/" <> ToString[Denominator[intercept]]
                                ],
                                If[Denominator[Abs[intercept]] == 1, 
                                    lineEquation = lineEquation <> " - " <> ToString[Abs[intercept]],
                                    lineEquation = lineEquation <> " - " <> ToString[Numerator[Abs[intercept]]] <> 
                                                  "/" <> ToString[Denominator[Abs[intercept]]]
                                ]
                            ];
                        ];
                    ];
                    
                    lineType = "v\[SHacek]eobecn\[AAcute] priamka";
                ,
                    
                    lineEquation = "x = " <> ToString[-c/a];
                    lineType = "priamka rovnobe\[ZHacek]n\[AAcute] s osou y";
                ];
                
                popis = "symetria pod\:013ea priamky " <> lineEquation;
            ];
            
            
            targetVertices = Map[ReflectPointOverLine[#, a, b, c] &, initialVertices];
            
            
            validLine = True;
            
            (* Kontrola rozsahu s\[UAcute]radn\[IAcute]c - Pou\[ZHacek]\[IAcute]vame N[] len pre kontrolu,
               nie pre modifik\[AAcute]ciu hodn\[OHat]t *)
            validLine = validLine && AllTrue[targetVertices, Function[vertex,
                Abs[N[vertex[[1]]]] <= 14 && Abs[N[vertex[[2]]]] <= 14
            ]];
            
            
            If[validLine, 
                validLine = Min[Flatten[Map[Function[{orig, target}, 
                    N[EuclideanDistance[orig, target]]], 
                    Transpose[{initialVertices, targetVertices}]]]] >= 1;
            ];
            
            
            If[validLine,
                invNewVertices = invTransform[targetVertices, a, b, c];
                
                
                
                validLine = Total[Flatten[Abs[N[initialVertices] - N[invNewVertices]]]] < 0.1;
            ];
        ];
        
        
        If[!validLine,
            a = 1; b = 1; c = 0;  
            lineEquation = "y = -x";
            lineType = "priamka y = -x";
            popis = "symetria pod\:013ea priamky y = -x";
            targetVertices = Map[ReflectPointOverLine[#, a, b, c] &, initialVertices];
        ];
        
        
        xIntercept = If[a != 0, -c/a, None];
        yIntercept = If[b != 0, -c/b, None];
        
        
        {a, b, c, lineType, popis, targetVertices, xIntercept, yIntercept}
    ];




CalculateSymmetryMatrix[a_, b_, c_] := 
    Module[{matrix, factor},
        factor = a^2 + b^2;
        
        
        If[factor == 0,
            
            Return[{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}]
        ];
        
        
        matrix = {
            {(b^2 - a^2)/factor, -2*a*b/factor, -2*a*c/factor},
            {-2*a*b/factor, (a^2 - b^2)/factor, -2*b*c/factor},
            {0, 0, 1}
        };
        
        
        matrix = Map[Simplify, matrix, {2}];
        
        
        matrix
    ];
    


CalculateSymmetryTranslation[a_, b_, c_] := 
    Module[{vector, factor},
        factor = a^2 + b^2;
        
        
        vector = {-2*a*c/factor, -2*b*c/factor};
        
        
        vector = Map[Simplify, vector];
        
        vector
    ];


CreateVisualization[originalVertices_, finalVertices_, a_, b_, c_, lineType_, xIntercept_, yIntercept_] := 
    Module[{allVertices, xMin, xMax, yMin, yMax, rangeBuffer, xRange, yRange, 
            labelOffsets, linePoints, slope, intercept, brightGreen, labelPoint},
            
        
        brightGreen = RGBColor[0, 0.8, 0.2];
        
        
        allVertices = Join[originalVertices, finalVertices];
        
        
        xMin = Min[N[allVertices[[All, 1]]]];
        xMax = Max[N[allVertices[[All, 1]]]];
        yMin = Min[N[allVertices[[All, 2]]]];
        yMax = Max[N[allVertices[[All, 2]]]];
        
        
        rangeBuffer = 2;
        
        
        xRange = {Min[-11, xMin - rangeBuffer], Max[11, xMax + rangeBuffer]};
        yRange = {Min[-11, yMin - rangeBuffer], Max[11, yMax + rangeBuffer]};
        
        
        labelOffsets = Table[
            Module[{originalVertex = originalVertices[[i]], 
                    finalVertex = finalVertices[[i]], 
                    offset = {0.7, 0.7}},
                
                
                If[N[originalVertex[[1]]] > 8, offset[[1]] = -1.0];
                If[N[originalVertex[[2]]] > 8, offset[[2]] = -1.0];
                
                {offset, offset}
            ],
            {i, Length[originalVertices]}
        ];
        
        
        linePoints = If[b != 0,
            
            slope = -a/b;
            intercept = -c/b;
            {{xRange[[1]], slope*xRange[[1]] + intercept}, 
             {xRange[[2]], slope*xRange[[2]] + intercept}},
             
            
            {{-c/a, yRange[[1]]}, {-c/a, yRange[[2]]}}
        ];
        
        
        labelPoint = If[b != 0,
            
            Module[{edgeX, edgeY, offset},
                
                edgeX = xRange[[2]] - 1.5;
                edgeY = slope*edgeX + intercept;
                
                
                offset = {0, 1.0};
                
                
                If[slope > 0, offset = {0, -1.0}];
                
                
                If[Abs[slope] < 0.3, offset = {1.2, 0}];
                
                {edgeX, edgeY} + offset
            ],
            
            {-c/a + 0.7, yRange[[2]] - 1.5}
        ];
        
        Graphics[{
            
            LightGray, Thin,
            Table[Line[{{xRange[[1]], y}, {xRange[[2]], y}}], 
                {y, Ceiling[yRange[[1]]], Floor[yRange[[2]]], 2}],
            Table[Line[{{x, yRange[[1]]}, {x, yRange[[2]]}}], 
                {x, Ceiling[xRange[[1]]], Floor[xRange[[2]]], 2}],
            
            
            Blue, Thick, Opacity[0.7],
            Line[Append[originalVertices, First[originalVertices]]],
            
            
            Red, Thick, Opacity[0.7],
            Line[Append[finalVertices, First[finalVertices]]],
            
            
            brightGreen, Thickness[0.005],
            Line[linePoints],
            
            
            
            White, Opacity[0.7], Rectangle[labelPoint - {0.4, 0.4}, labelPoint + {0.4, 0.4}],
            
            brightGreen, Opacity[1.0],
            Text[Style["p", Bold, 18, brightGreen], labelPoint],
            
            
            brightGreen, Dashed,
            Table[
                Line[{originalVertices[[i]], finalVertices[[i]]}],
                {i, Length[originalVertices]}
            ],
            
            
            White, 
            Table[
                Disk[originalVertices[[i]], 0.45],
                {i, 5}
            ],
            Table[
                Disk[finalVertices[[i]], 0.45],
                {i, 5}
            ],
            
            
            
            
            Blue, PointSize[0.025], Point[originalVertices],
            Red, PointSize[0.025], Point[finalVertices],
            
            
            Table[
                {
                    Text[Style[FromCharacterCode[64 + i], Blue, Bold, 16], 
                        originalVertices[[i]] + labelOffsets[[i, 1]]],
                    Text[Style[FromCharacterCode[64 + i] <> "'", Red, Bold, 16], 
                        finalVertices[[i]] + labelOffsets[[i, 2]]]
                },
                {i, 5}
            ],
            
            
            Black, Thick,
            Arrow[{{xRange[[1]], 0}, {xRange[[2]], 0}}],
            Arrow[{{0, yRange[[1]]}, {0, yRange[[2]]}}],
            Text[Style["x", Black, Bold], {xRange[[2]] - 0.5, -0.5}],
            Text[Style["y", Black, Bold], {-0.5, yRange[[2]] - 0.5}],
            
            
            Table[Text[Style[i, Gray, 10], {i, -0.3}], 
                {i, Ceiling[xRange[[1]]], Floor[xRange[[2]]]}],
            Table[Text[Style[i, Gray, 10], {-0.3, i}], 
                {i, Ceiling[yRange[[1]]], Floor[yRange[[2]]]}]
        },
        PlotRange -> {xRange, yRange},
        AspectRatio -> 1,
        ImageSize -> 500,
        ImagePadding -> 20
        ]
    ];


FormatLineEquation[a_, b_, c_] := 
    Module[{eqn = ""},
        
        If[a == 0 && b == 0,
            
            Return["Neplatn\[AAcute] rovnica priamky"];
        ];
        
        
        If[a != 0,
            eqn = If[a == 1, "x", If[a == -1, "-x", ToString[a] <> "x"]];
        ];
        
        
        If[b != 0,
            If[a != 0, 
                eqn = eqn <> If[b > 0, " + ", " - "];
                eqn = eqn <> If[Abs[b] == 1, "y", ToString[Abs[b]] <> "y"],
                eqn = If[b == 1, "y", If[b == -1, "-y", ToString[b] <> "y"]]
            ];
        ];
        
        
        If[c != 0,
            eqn = eqn <> If[c > 0, " + ", " - "] <> ToString[Abs[c]];
        ];
        
        
        eqn <> " = 0"
    ];


CalculateIntersection[point_, a_, b_, c_] := 
    Module[{x0, y0, x1, y1, t},
        x0 = point[[1]];
        y0 = point[[2]];
        
        
        t = -(a*x0 + b*y0 + c)/(a^2 + b^2);
        
        
        x1 = x0 + a*t;
        y1 = y0 + b*t;
        
        
        {Simplify[x1], Simplify[y1]}
    ];


DomcekSymetriaNoDisplayWithParams[vertices_] := 
    Module[{axis, result, lineParams, a, b, c},
        
        If[DomcekHardBalik`Transforms`Symetria`SymetriaOs =!= Null,
            axis = DomcekHardBalik`Transforms`Symetria`SymetriaOs;
        ,
            
            axis = RandomChoice[{
                0.15 -> "os x", 
                0.15 -> "os y", 
                0.15 -> "priamka y=x", 
                0.15 -> "priamka y=-x",
                0.4 -> "complex" 
            }];
            
            
            If[axis == "complex" || !MemberQ[{"os x", "os y", "priamka y=x", "priamka y=-x"}, axis],
                
                a = RandomChoice[{-5, -4, -3, -2, -1, 1, 2, 3, 4, 5}];
                b = RandomChoice[{-5, -4, -3, -2, -1, 1, 2, 3, 4, 5}];
                c = RandomChoice[{-6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6}];
                
                
                While[a == 0 && b == 0,
                    a = RandomChoice[{-5, -4, -3, -2, -1, 1, 2, 3, 4, 5}];
                ];
                
                
                If[GCD[Abs[a], Abs[b], Abs[c]] > 1,
                    {a, b, c} = {a, b, c}/GCD[Abs[a], Abs[b], Abs[c]];
                ];
                
                
                axis = {a, b, c};
            ];
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
                    
                    lineParams = axis;
                    Module[{reflectFunc},
                        
                        reflectFunc = If[
                            MemberQ[Names["DomcekHardBalik`Transforms`Symetria`Private`*"], 
                                  "ReflectPointOverLine"],
                            DomcekHardBalik`Transforms`Symetria`Private`ReflectPointOverLine,
                            
                            Function[{point, a, b, c},
                                Module[{x, y, d, factor},
                                    x = point[[1]];
                                    y = point[[2]];
                                    factor = a^2 + b^2;
                                    d = (a*x + b*y + c);
                                    {x - 2*a*d/factor, y - 2*b*d/factor}
                                ]
                            ]
                        ];
                        
                        
                        Map[reflectFunc[#, lineParams[[1]], lineParams[[2]], lineParams[[3]]] &, 
                            vertices]
                    ],
                    
                    
                    Module[{defaultAxis = {1, 1, 0}},  
                        
                        axis = defaultAxis;
                        
                        
                        Map[DomcekHardBalik`Transforms`Symetria`Private`ReflectPointOverLine[
                            #, defaultAxis[[1]], defaultAxis[[2]], defaultAxis[[3]]] &, 
                            vertices]
                    ]
                ]
        ];
        
        
        If[axis === "complex", axis = {1, 1, 0}];  
        DomcekHardBalik`Transforms`Symetria`SymetriaOs = axis;
        
        
        {axis, result}
    ];


GenerateSymmetryLineFromGlobal[vertices_] := 
    Module[{a, b, c, lineType, popis, targetVertices, xIntercept, yIntercept},
        
        If[DomcekHardBalik`Transforms`Symetria`SymetriaOs =!= Null,
            
            Switch[DomcekHardBalik`Transforms`Symetria`SymetriaOs,
                "os x", 
                    a = 0; b = 1; c = 0;
                    lineType = "os x";
                    popis = "symetria pod\:013ea osi x";
                    xIntercept = None;
                    yIntercept = 0;,
                    
                "os y", 
                    a = 1; b = 0; c = 0;
                    lineType = "os y";
                    popis = "symetria pod\:013ea osi y";
                    xIntercept = 0;
                    yIntercept = None;,
                    
                "priamka y=x", 
                    a = 1; b = -1; c = 0;
                    lineType = "priamka y = x";
                    popis = "symetria pod\:013ea priamky y = x";
                    xIntercept = 0;
                    yIntercept = 0;,
                    
                "priamka y=-x", 
                    a = 1; b = 1; c = 0;
                    lineType = "priamka y = -x";
                    popis = "symetria pod\:013ea priamky y = -x";
                    xIntercept = 0;
                    yIntercept = 0;,
                    
                _,
                    
                    If[ListQ[DomcekHardBalik`Transforms`Symetria`SymetriaOs] && 
                       Length[DomcekHardBalik`Transforms`Symetria`SymetriaOs] == 3,
                        {a, b, c} = DomcekHardBalik`Transforms`Symetria`SymetriaOs;
                        lineType = "v\[SHacek]eobecn\[AAcute] priamka";
                        popis = "symetria pod\:013ea priamky " <> FormatLineEquation[a, b, c];
                        
                        
                        xIntercept = If[a != 0, -c/a, None];
                        yIntercept = If[b != 0, -c/b, None];,
                        
                        
                        a = 0; b = 1; c = 0;
                        lineType = "os x";
                        popis = "symetria pod\:013ea osi x";
                        xIntercept = None;
                        yIntercept = 0;
                    ]
            ],
            
            
            {a, b, c, lineType, popis, targetVertices, xIntercept, yIntercept} = GenerateSymmetryLine[vertices];
        ];
        
        
        targetVertices = Map[ReflectPointOverLine[#, a, b, c] &, vertices];
        
        
        {a, b, c, lineType, popis, targetVertices, xIntercept, yIntercept}
    ];
    

DomcekSymetria[inputVertices_] := 
    Module[{normalizedVertices, symmetryParams, outputVertices, 
            a, b, c, lineType, popis, xIntercept, yIntercept, 
            intersections, distances, lineEqn, 
            transformMatrix, darkGreen = RGBColor[0, 0.5, 0]},
            
        
        normalizedVertices = NormalizeVertices[inputVertices];
        
        
        symmetryParams = GenerateSymmetryLineFromGlobal[normalizedVertices];
        
        a = symmetryParams[[1]];
        b = symmetryParams[[2]];
        c = symmetryParams[[3]];
        lineType = symmetryParams[[4]];
        popis = symmetryParams[[5]];
        outputVertices = symmetryParams[[6]];
        xIntercept = symmetryParams[[7]];
        yIntercept = symmetryParams[[8]];
        
        
        transformMatrix = CalculateSymmetryMatrix[a, b, c];
        
        
        DomcekHardBalik`Transforms`Symetria`SymetriaMatrix = transformMatrix;
        
        
        SymetriaOs = Switch[lineType,
            "os x", "os x",
            "os y", "os y",
            "priamka y = x", "priamka y=x",
            "priamka y = -x", "priamka y=-x",
            _, 
            {a, b, c}
        ];
        
        
        Print[Style[
            "GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE DOM\[CapitalCHacek]EKA - SYMETRIA", 
            Bold, 16]];
        Print[Style["========================================", Bold]];
        
        
        Print[Style["\nZADANIE:", Bold, 14]];
        Print["Majme dom\[CHacek]ek ABCDE s vrcholmi:"];
        Print[MatrixForm[Map[Style[#, Blue] &, normalizedVertices, {2}]]];
        
        Print["\nVykonajte symetriu v 2D priestore pod\:013ea priamky ", Style[FormatLineEquation[a, b, c], RGBColor[0, 0.8, 0.2], Bold], "."];
        
        
        Print[Style["\nTE\[CapitalOAcute]RIA MATICOV\[CapitalEAcute]HO ZOBRAZENIA SYMETRIE:", Bold, 14]];
        Print["Pri symetrii pod\:013ea priamky ax + by + c = 0 v 2D priestore m\[OHat]\[ZHacek]eme pou\[ZHacek]i\[THacek] homog\[EAcute]nnu transforma\[CHacek]n\[UAcute] maticu 3\[Times]3:"];
        
        
        Print["\nHomog\[EAcute]nna transforma\[CHacek]n\[AAcute] matica pre symetriu m\[AAcute] tvar:"];
        Print[
            MatrixForm[{
                {Style["(b^2 - a^2)/(a^2 + b^2)", Red], Style["-2ab/(a^2 + b^2)", Red], Style["-2ac/(a^2 + b^2)", Red]}, 
                {Style["-2ab/(a^2 + b^2)", Red], Style["(a^2 - b^2)/(a^2 + b^2)", Red], Style["-2bc/(a^2 + b^2)", Red]},
                {Style["0", Red], Style["0", Red], Style["1", Red]}
            }]
        ];
        
        Print["\nV\[YAcute]hoda pou\[ZHacek]itia homog\[EAcute]nnej matice je, \[ZHacek]e zah\:0155\[NHacek]a line\[AAcute]rnu transform\[AAcute]ciu aj posun v jednej matici, \[CHacek]o umo\[ZHacek]\[NHacek]uje"];
        Print["jednoduch\[EAcute] re\[THacek]azenie viacer\[YAcute]ch transform\[AAcute]ci\[IAcute] pomocou n\[AAcute]sobenia mat\[IAcute]c."];
        
        
        
        Print[Style["\nV\[CapitalYAcute]PO\[CapitalCHacek]ET TRANSFORMA\[CapitalCHacek]NEJ MATICE PRE SYMETRIU:", Bold, 12]];
        Print["Pre na\[SHacek]u priamku ", Style[ToString[a] <> "x + " <> ToString[b] <> "y + " <> ToString[c] <> " = 0", RGBColor[0.1, 0.7, 0.1]], ":"];
        Print["Vypo\[CHacek]\[IAcute]tame jednotliv\[EAcute] prvky matice pou\[ZHacek]it\[IAcute]m vzorca:"];
        
        
        Print["\nMenovate\:013e: a\.b2 + b\.b2 = ", a, "\.b2 + ", b, "\.b2 = ", a^2 + b^2];
        
        
        Print[Style["\nPrv\[YAcute] riadok matice:", Bold]];
        Print["M[1,1] = (b\.b2 - a\.b2)/(a\.b2 + b\.b2) = (", b^2, " - ", a^2, ")/(", a^2 + b^2, ") = ", (b^2 - a^2)/(a^2 + b^2)];
        Print["M[1,2] = -2ab/(a\.b2 + b\.b2) = -2\[CenterDot]", a, "\[CenterDot]", b, "/(", a^2 + b^2, ") = ", -2*a*b/(a^2 + b^2)];
        Print["M[1,3] = -2ac/(a\.b2 + b\.b2) = -2\[CenterDot]", a, "\[CenterDot]", c, "/(", a^2 + b^2, ") = ", -2*a*c/(a^2 + b^2)];
        
        
        Print[Style["\nDruh\[YAcute] riadok matice:", Bold]];
        Print["M[2,1] = -2ab/(a\.b2 + b\.b2) = -2\[CenterDot]", a, "\[CenterDot]", b, "/(", a^2 + b^2, ") = ", -2*a*b/(a^2 + b^2)];
        Print["M[2,2] = (a\.b2 - b\.b2)/(a\.b2 + b\.b2) = (", a^2, " - ", b^2, ")/(", a^2 + b^2, ") = ", (a^2 - b^2)/(a^2 + b^2)];
        Print["M[2,3] = -2bc/(a\.b2 + b\.b2) = -2\[CenterDot]", b, "\[CenterDot]", c, "/(", a^2 + b^2, ") = ", -2*b*c/(a^2 + b^2)];
        
        
        Print[Style["\nTret\[IAcute] riadok matice:", Bold]];
        Print["M[3,1] = 0, M[3,2] = 0, M[3,3] = 1 (\[SHacek]tandardn\[EAcute] hodnoty pre homog\[EAcute]nnu transform\[AAcute]ciu)"];
        
        
        Print[Style["\nV\[CapitalYAcute]SLEDN\[CapitalAAcute] TRANSFORMA\[CapitalCHacek]N\[CapitalAAcute] MATICA:", Bold]];
        
        
        transformMatrix = CalculateSymmetryMatrix[a, b, c];
        Print[MatrixForm[Map[Style[#, Red] &, transformMatrix, {2}]]];
        
        
        Print["\nPre na\[SHacek]u priamku ", Style[FormatLineEquation[a, b, c], RGBColor[0, 0.8, 0.2], Bold], ":"];
        Print["a = ", Style[a, Red]];
        Print["b = ", Style[b, Red]];
        Print["c = ", Style[c, Red]];
        
        
        intersections = Map[CalculateIntersection[#, a, b, c] &, normalizedVertices];
        
        
        distances = MapThread[
            EuclideanDistance[#1, #2] &, 
            {normalizedVertices, intersections}
        ];
        
        
        lineEqn = FormatLineEquation[a, b, c];
        
        
        Print[Style["\nV\[CapitalYAcute]PO\[CapitalCHacek]ET SYMETRIE JEDNOTLIV\[CapitalYAcute]CH VRCHOLOV:", Bold, 14]];
        
        
        Table[
            Module[{x = normalizedVertices[[i, 1]], 
                   y = normalizedVertices[[i, 2]],
                   homogeneousVector, resultVector},
                   
                Print[Style["\nVRCHOL " <> FromCharacterCode[64 + i] <> ":", Bold, 14]];
                Print["P\[OHat]vodn\[EAcute] s\[UAcute]radnice: [", Style[x, Blue], ", ", Style[y, Blue], "]"];
                
                
                homogeneousVector = {x, y, 1};
                
                
                resultVector = transformMatrix . homogeneousVector;
                
                
                Print[Style["V\[YAcute]po\[CHacek]et pomocou homog\[EAcute]nnej matice:", Bold]];
                Print[
                    Grid[{
                        {
                            MatrixForm[Map[Style[#, Red] &, transformMatrix, {2}]], 
                            " \[CenterDot] ", 
                            MatrixForm[Map[Style[#, Blue] &, homogeneousVector]],
                            " = ",
                            MatrixForm[Map[Style[#, darkGreen] &, resultVector]]
                        }
                    }]
                ];
                
                
                Print[Style["Podrobn\[YAcute] v\[YAcute]po\[CHacek]et s\[UAcute]radn\[IAcute]c:", Bold]];
                
                
                Print[Style["\[Bullet] V\[YAcute]po\[CHacek]et novej x-ovej s\[UAcute]radnice:", Brown]];
                Print["   = (", Style[transformMatrix[[1, 1]], Red], " \[CenterDot] ", Style[homogeneousVector[[1]], Blue], 
                      ") + (", Style[transformMatrix[[1, 2]], Red], " \[CenterDot] ", Style[homogeneousVector[[2]], Blue], 
                      ") + (", Style[transformMatrix[[1, 3]], Red], " \[CenterDot] ", Style[homogeneousVector[[3]], Blue], ")"];
                
                Print["   = ", Style[transformMatrix[[1, 1]]*homogeneousVector[[1]], Purple], 
                      " + ", Style[transformMatrix[[1, 2]]*homogeneousVector[[2]], Purple],
                      " + ", Style[transformMatrix[[1, 3]]*homogeneousVector[[3]], Purple]];
                
                Print["   = ", Style[resultVector[[1]], darkGreen]];
                
                
                Print[Style["\n\[Bullet] V\[YAcute]po\[CHacek]et novej y-ovej s\[UAcute]radnice:", Brown]];
                Print["   = (", Style[transformMatrix[[2, 1]], Red], " \[CenterDot] ", Style[homogeneousVector[[1]], Blue], 
                      ") + (", Style[transformMatrix[[2, 2]], Red], " \[CenterDot] ", Style[homogeneousVector[[2]], Blue], 
                      ") + (", Style[transformMatrix[[2, 3]], Red], " \[CenterDot] ", Style[homogeneousVector[[3]], Blue], ")"];
                
                Print["   = ", Style[transformMatrix[[2, 1]]*homogeneousVector[[1]], Purple], 
                      " + ", Style[transformMatrix[[2, 2]]*homogeneousVector[[2]], Purple],
                      " + ", Style[transformMatrix[[2, 3]]*homogeneousVector[[3]], Purple]];
                
                Print["   = ", Style[resultVector[[2]], darkGreen]];
                
                
                Print[Style["\nV\[YAcute]sledn\[AAcute] reprezent\[AAcute]cia bodu " <> FromCharacterCode[64 + i] <> "':", Bold]];
                Print["[", Style[resultVector[[1]], darkGreen], ", ", Style[resultVector[[2]], darkGreen], "]"];
                
                Print[Style["--------------------------------------------------", Bold]];
            ],
            {i, 5}
        ];
        
        
        Module[{M},
            
            Print[Style["\n\[CapitalSHacek]PECI\[CapitalAAcute]LNE MATICE PRE NIEKTOR\[CapitalEAcute] TYPY SYMETRI\[CapitalIAcute]:", Bold, 14]];
            
            If[a == 0 && b == 1, 
                Print["\nPre os x (y = 0) je homog\[EAcute]nna transforma\[CHacek]n\[AAcute] matica:"];
                Print[MatrixForm[{
                    {Style[1, Red], Style[0, Red], Style[0, Red]}, 
                    {Style[0, Red], Style[-1, Red], Style[0, Red]},
                    {Style[0, Red], Style[0, Red], Style[1, Red]}
                }]];
            ];
            
            If[a == 1 && b == 0, 
                Print["\nPre os y (x = 0) je homog\[EAcute]nna transforma\[CHacek]n\[AAcute] matica:"];
                Print[MatrixForm[{
                    {Style[-1, Red], Style[0, Red], Style[0, Red]}, 
                    {Style[0, Red], Style[1, Red], Style[0, Red]},
                    {Style[0, Red], Style[0, Red], Style[1, Red]}
                }]];
            ];
            
            If[a == 1 && b == -1 && c == 0, 
                Print["\nPre priamku y = x je homog\[EAcute]nna transforma\[CHacek]n\[AAcute] matica:"];
                Print[MatrixForm[{
                    {Style[0, Red], Style[1, Red], Style[0, Red]}, 
                    {Style[1, Red], Style[0, Red], Style[0, Red]},
                    {Style[0, Red], Style[0, Red], Style[1, Red]}
                }]];
            ];
            
            If[a == 1 && b == 1 && c == 0, 
                Print["\nPre priamku y = -x je homog\[EAcute]nna transforma\[CHacek]n\[AAcute] matica:"];
                Print[MatrixForm[{
                    {Style[0, Red], Style[-1, Red], Style[0, Red]}, 
                    {Style[-1, Red], Style[0, Red], Style[0, Red]},
                    {Style[0, Red], Style[0, Red], Style[1, Red]}
                }]];
            ];
        ];
        
        
        Print[Style["\nPOSTUP:", Bold, 14]];
        Print["Pre v\[YAcute]po\[CHacek]et osovej symetrie pod\:013ea priamky ", Style[lineEqn, RGBColor[0, 0.8, 0.2], Bold], " pou\[ZHacek]\[IAcute]vame vzorce:"];
        
        Print["\nV\[SHacek]eobecn\[YAcute] vzorec pre v\[YAcute]po\[CHacek]et symetrie bodu [x, y] pod\:013ea priamky ax + by + c = 0:"];
        Print["x' = x - 2a(ax + by + c)/(a\.b2 + b\.b2)"];
        Print["y' = y - 2b(ax + by + c)/(a\.b2 + b\.b2)"];
        
        Print["\nTento vzorec m\[OHat]\[ZHacek]eme zap\[IAcute]sa\[THacek] v maticovej forme pomocou homog\[EAcute]nnych s\[UAcute]radn\[IAcute]c [x, y, 1]"];
        Print["ako n\[AAcute]sobenie s 3\[Times]3 maticou, \[CHacek]o umo\[ZHacek]\[NHacek]uje jednoduch\[EAcute] zre\[THacek]azenie transform\[AAcute]ci\[IAcute]."];
        
        Print["\nNa\[SHacek]e hodnoty koeficientov:"];
        Print["a = ", a];
        Print["b = ", b];
        Print["c = ", c];
        
        Print["\nRovnica osi symetrie: ", lineEqn];
        Print["Norm\[AAcute]lov\[YAcute] vektor osi symetrie: (", a, ", ", b, ")"];
        
        
        Print[Style["\nV\[CapitalYAcute]SLEDOK TRANSFORM\[CapitalAAcute]CIE CEL\[CapitalEAcute]HO DOM\[CapitalCHacek]EKA:", Bold, 14]];
        Print[Style["P\[OHat]vodn\[YAcute] dom\[CHacek]ek ABCDE:", Blue, Bold]];
        Print[MatrixForm[normalizedVertices]];
        
        Print[Style["Dom\[CHacek]ek po symetrii A'B'C'D'E':", Red, Bold]];
        Print[MatrixForm[outputVertices]];
        
        
        Print[Style["\nVIZUALIZ\[CapitalAAcute]CIA TRANSFORM\[CapitalAAcute]CIE:", Bold]];
        Print[CreateVisualization[normalizedVertices, outputVertices, a, b, c, lineType, xIntercept, yIntercept]];
        
        
        Print[Style["\nLEGENDA:", Bold, 14]];
        Print[Style["\[Bullet] Modr\[EAcute]", Blue], " body a \[CHacek]iary: P\[OHat]vodn\[YAcute] dom\[CHacek]ek"];
        Print[Style["\[Bullet] \[CapitalCHacek]erven\[EAcute]", Red], " body a \[CHacek]iary: Dom\[CHacek]ek po symetrii"];
        Print[Style["\[Bullet] Zelen\[EAcute]", darkGreen], " \[CHacek]iara: Os symetrie"];
        Print[Style["\[Bullet] Zelen\[EAcute]", darkGreen], " preru\[SHacek]ovan\[EAcute] \[CHacek]iary: Spojenia zodpovedaj\[UAcute]cich si vrcholov"];
        
        
            Print[Style["\[Bullet] \[CapitalCHacek]erven\[AAcute]:", Red], " Transforma\[CHacek]n\[AAcute] matica"];
            Print[Style["\[Bullet] Modr\[AAcute]:", Blue], " Vstupn\[EAcute] s\[UAcute]radnice"];
            Print[Style["\[Bullet] Fialov\[AAcute]:", Purple], " Medziv\[YAcute]po\[CHacek]ty"];
            Print[Style["\[Bullet] Zelen\[AAcute]:", darkGreen], " V\[YAcute]sledn\[EAcute] s\[UAcute]radnice"];
                
        Print[Style["\nP\[OHat]vodn\[EAcute] vrcholy (modr\[AAcute]):", Bold]];
        Table[
            Print[Row[{Style["Vrchol " <> FromCharacterCode[64 + i] <> ": ", 
                RGBColor[0.1, 0.1, 1]], normalizedVertices[[i]]}]], {i, 5}];
                
        Print[Style["\nV\[YAcute]sledn\[EAcute] vrcholy (\[CHacek]erven\[AAcute]):", Bold]];
        Table[
            Print[Row[{Style["Vrchol " <> FromCharacterCode[64 + i] <> "': ", 
                RGBColor[1, 0.1, 0.1]], outputVertices[[i]]}]], {i, 5}];
                
        
        Print[Style["\nMATEMATICK\[CapitalEAcute] VLASTNOSTI:", Bold, 14]];
        Print["\[Bullet] Symetria pod\:013ea priamky je izometrick\[AAcute] transform\[AAcute]cia - zachov\[AAcute]va vzdialenosti medzi bodmi"];
        Print["\[Bullet] Symetria zachov\[AAcute]va ve\:013ekos\[THacek] a tvar geometrick\[YAcute]ch \[UAcute]tvarov"];
        Print["\[Bullet] Symetria zachov\[AAcute]va uhly medzi \[UAcute]se\[CHacek]kami a ich d\:013a\[ZHacek]ky"];
        Print["\[Bullet] Symetria zachov\[AAcute]va obsahy a obvody"];
        Print["\[Bullet] Body le\[ZHacek]iace na osi symetrie zost\[AAcute]vaj\[UAcute] po symetrii nezmenen\[EAcute]"];
        Print["\[Bullet] Aplikovan\[IAcute]m symetrie dvakr\[AAcute]t z\[IAcute]skame p\[OHat]vodn\[YAcute] obraz - je to transform\[AAcute]cia r\[AAcute]du 2"];
        Print["\[Bullet] Spojnica bodu a jeho obrazu je v\[ZHacek]dy kolm\[AAcute] na os symetrie"];
        Print["\[Bullet] Stred spojnice bodu a jeho obrazu v\[ZHacek]dy le\[ZHacek]\[IAcute] na osi symetrie"];
        
        
        Print[Style["\nV\[CapitalYAcute]ZNAMN\[CapitalEAcute] OSI SYMETRIE:", Bold, 14]];
        Print["\[Bullet] Os x (y = 0): bod [x, y] sa zobraz\[IAcute] na [x, -y]"];
        Print["\[Bullet] Os y (x = 0): bod [x, y] sa zobraz\[IAcute] na [-x, y]"];
        Print["\[Bullet] Priamka y = x: bod [x, y] sa zobraz\[IAcute] na [y, x]"];
        Print["\[Bullet] Priamka y = -x: bod [x, y] sa zobraz\[IAcute] na [-y, -x]"];
                
        
        outputVertices
    ];
    

DomcekSymetria::invalidInput = "Neplatn\[YAcute] vstup. O\[CHacek]ak\[AAcute]va sa matica alebo zoznam s\[UAcute]radn\[IAcute]c dom\[CHacek]eka.";
DomcekSymetria::insufficientVertices = "Nedostato\[CHacek]n\[YAcute] po\[CHacek]et vrcholov. Pre dom\[CHacek]ek s\[UAcute] potrebn\[EAcute] aspo\[NHacek] 5 vrcholov.";

End[];
EndPackage[];







