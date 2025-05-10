(* ::Package:: *)

(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)




BeginPackage["StvorecHardBalik`Transforms`Skosenie`", {"StvorecHardBalik`"}];


StvorecSkosenie::usage = 
    "StvorecSkosenie[vertices_] zobraz\[IAcute] interakt\[IAcute]vny tutori\[AAcute]l pre skosenie \[SHacek]tvorca a vr\[AAcute]ti nov\[EAcute] vrcholy.";


SkosenieParams::usage = "SkosenieParams je globalny parameter skosenia {kx, ky}, ktory sa pouziva v StvorecSkosenie funkcii.";


StvorecSkosenieNoDisplayWithParams::usage = 
    "StvorecSkosenieNoDisplayWithParams[vertices_] vrati parametry skosenia a nove vrcholy bez zobrazovania.";

Begin["`Private`"];


SkosenieParams = {0, 0};


FormatExactValue[expr_] :=
    Module[{strExpr, rationalExpr},
        
        rationalExpr = If[NumberQ[expr] && !IntegerQ[expr],
                          Rationalize[expr, 0],
                          expr];
        
        
        strExpr = ToString[rationalExpr, InputForm];
        
        
        strExpr = StringReplace[strExpr, {
            "Sqrt[" ~~ n__ ~~ "]" :> "\[Sqrt]" <> n,
            "*" -> " \[CenterDot] ",
            "[" -> "",
            "]" -> ""
        }];
        
        strExpr
    ];


ExpandAndFormatExpression[expr_] :=
    Module[{expandedExpr, formattedExpr},
        
        expandedExpr = Expand[expr];
        
        
        formattedExpr = FormatExactValue[expandedExpr];
        
        formattedExpr
    ];


NormalizeVertices[vertices_] := 
    Module[{processedVertices},
        
        processedVertices = Which[
            
            MatrixQ[vertices], vertices,
            
            
            ListQ[vertices] && Length[Dimensions[vertices]] == 2, 
                If[Length[DeleteDuplicates[Length /@ vertices]] > 1,
                    
                    Block[{cleanedVertices = Select[vertices, VectorQ]},
                        If[Length[cleanedVertices] >= 4,
                            cleanedVertices[[1;;4]],
                            Message[StvorecSkosenie::invalidInput]; 
                            Abort[]
                        ]
                    ],
                    
                    vertices
                ],
            
            
            ListQ[vertices] && Length[Dimensions[vertices]] == 1 && EvenQ[Length[vertices]], 
                Partition[vertices, 2],
                
            
            True, 
                Message[StvorecSkosenie::invalidInput]; 
                Abort[]
        ];
        
        
        
        
        If[Length[processedVertices] < 4,
            Message[StvorecSkosenie::insufficientVertices];
            Abort[]
        ];
        
        
        If[Max[Abs[N[processedVertices[[All, 1]]]]] > 10 || Max[Abs[N[processedVertices[[All, 2]]]]] > 10,
            
            processedVertices = processedVertices / (Max[Abs[N[Flatten[processedVertices]]]] / 6);
        ];
        
        
        processedVertices[[1;;4]]
    ];


DetectShearingParameters[originalVertices_, finalVertices_] := 
    Module[{kx = 0, ky = 0, foundKx = False, foundKy = False, i = 1, 
            originalPoint, finalPoint, smer = ""},
        
        
        While[i <= 4 && (!foundKx || !foundKy),
            originalPoint = originalVertices[[i]];
            finalPoint = finalVertices[[i]];
            
            
            If[originalPoint[[2]] != 0 && !foundKx,
                kx = (finalPoint[[1]] - originalPoint[[1]])/originalPoint[[2]];
                
                If[AllTrue[Range[4], Function[j,
                    Simplify[finalVertices[[j, 1]] - originalVertices[[j, 1]] - 
                             kx*originalVertices[[j, 2]]] == 0
                ]],
                    foundKx = True;
                ]
            ];
            
            
            If[originalPoint[[1]] != 0 && !foundKy,
                ky = (finalPoint[[2]] - originalPoint[[2]])/originalPoint[[1]];
                
                If[AllTrue[Range[4], Function[j,
                    Simplify[finalVertices[[j, 2]] - originalVertices[[j, 2]] - 
                             ky*originalVertices[[j, 1]]] == 0
                ]],
                    foundKy = True;
                ]
            ];
            
            i++;
        ];
        
        
        If[foundKx && foundKy,
            smer = "v smere os\[IAcute] x a y";
            {Simplify[kx], Simplify[ky], smer, 
             "Skosenie v smere osi x s koeficientom k = " <> FormatExactValue[kx] <>
             " a v smere osi y s koeficientom k = " <> FormatExactValue[ky]}
        ,
            If[foundKx,
                smer = "v smere osi x";
                {Simplify[kx], 0, smer, "Skosenie v smere osi x s koeficientom k = " <> FormatExactValue[kx]}
            ,
                If[foundKy,
                    smer = "v smere osi y";
                    {0, Simplify[ky], smer, "Skosenie v smere osi y s koeficientom k = " <> FormatExactValue[ky]}
                ,
                    None  
                ]
            ]
        ]
    ];


GetShearingParameters[vertices_, finalVertices_: None] := 
    Module[{kx, ky, smer, valid = False, newVertices, 
            allVertices, xMin, xMax, yMin, yMax, 
            invKx, invKy, invNewVertices, invVertices,
            narocnost, popis, detectedParameters},
            
        
        If[finalVertices =!= None,
            detectedParameters = DetectShearingParameters[vertices, finalVertices];
            
            
            If[detectedParameters =!= None,
                kx = detectedParameters[[1]];
                ky = detectedParameters[[2]];
                smer = detectedParameters[[3]];
                popis = detectedParameters[[4]];
                
                Return[{kx, ky, smer, popis, finalVertices}];
            ]
        ];
        
        
        If[SkosenieParams =!= {0, 0},
            
            kx = SkosenieParams[[1]];
            ky = SkosenieParams[[2]];
            
            
            kx = If[NumberQ[kx] && !IntegerQ[kx], Rationalize[kx, 0], kx];
            ky = If[NumberQ[ky] && !IntegerQ[ky], Rationalize[ky, 0], ky];
            
            
            smer = If[kx != 0 && ky != 0, 
                     "v smere os\[IAcute] x a y", 
                     If[kx != 0, "v smere osi x", "v smere osi y"]];
            
            
            popis = If[kx != 0 && ky != 0,
                      "Skosenie v smere osi x s koeficientom k = " <> FormatExactValue[kx] <>
                      " a v smere osi y s koeficientom k = " <> FormatExactValue[ky],
                      If[kx != 0, 
                        "Skosenie v smere osi x s koeficientom k = " <> FormatExactValue[kx],
                        "Skosenie v smere osi y s koeficientom k = " <> FormatExactValue[ky]
                      ]
                   ];
            
            
            newVertices = Map[
                Function[p,
                    {Simplify[p[[1]] + kx*p[[2]]], Simplify[p[[2]] + ky*p[[1]]]}
                ], 
                vertices
            ];
            
            
            SkosenieParams = {0, 0};
            
            
            Return[{kx, ky, smer, popis, newVertices}];
        ];
        
        
        allVertices = vertices;
        xMin = Min[N[allVertices[[All, 1]]]];
        xMax = Max[N[allVertices[[All, 1]]]];
        yMin = Min[N[allVertices[[All, 2]]]];
        yMax = Max[N[allVertices[[All, 2]]]];
        
        
        narocnost = RandomReal[]; 
        
        While[!valid,
            
            If[narocnost < 0.3,
                
                kx = RandomChoice[{-2, -3/2, -1, -1/2, 1/2, 1, 3/2, 2}];
                ky = RandomChoice[{-2, -3/2, -1, -1/2, 1/2, 1, 3/2, 2}];
                smer = "v smere os\[IAcute] x a y";
                popis = "Skosenie v smere osi x s koeficientom k = " <> FormatExactValue[kx] <>
                        " a v smere osi y s koeficientom k = " <> FormatExactValue[ky];
            ,
                If[narocnost < 0.7, 
                    
                    If[RandomReal[] < 0.5,
                        
                        kx = RandomChoice[{-2, -3/2, -1, -1/2, 1/2, 1, 3/2, 2}];
                        ky = 0;
                        smer = "v smere osi x";
                        popis = "Skosenie v smere osi x s koeficientom k = " <> FormatExactValue[kx];
                    ,
                        
                        kx = 0;
                        ky = RandomChoice[{-2, -3/2, -1, -1/2, 1/2, 1, 3/2, 2}];
                        smer = "v smere osi y";
                        popis = "Skosenie v smere osi y s koeficientom k = " <> FormatExactValue[ky];
                    ]
                ,
                    
                    If[RandomReal[] < 0.5,
                        
                        kx = RandomChoice[{-1, 1/2, 1}];
                        ky = 0;
                        smer = "v smere osi x";
                        popis = "Skosenie v smere osi x s koeficientom k = " <> FormatExactValue[kx];
                    ,
                        
                        kx = 0;
                        ky = RandomChoice[{-1, 1/2, 1}];
                        smer = "v smere osi y";
                        popis = "Skosenie v smere osi y s koeficientom k = " <> FormatExactValue[ky];
                    ]
                ]
            ];
            
            
            newVertices = Map[
                Function[p,
                    {Simplify[p[[1]] + kx*p[[2]]], Simplify[p[[2]] + ky*p[[1]]]}
                ], 
                allVertices
            ];
            
            
            invKx = -kx;
            invKy = -ky;
            
            
            invNewVertices = Map[
                Function[p,
                    {Simplify[p[[1]] + invKx*p[[2]]], Simplify[p[[2]] + invKy*p[[1]]]}
                ], 
                newVertices
            ];
            
            
            valid = 
                
                AllTrue[newVertices, (Abs[N[#[[1]]]] <= 14 && Abs[N[#[[2]]]] <= 14) &] &&
                
                AllTrue[Range[Length[allVertices]], Function[i, 
                    Simplify[allVertices[[i, 1]] - invNewVertices[[i, 1]]] == 0 && 
                    Simplify[allVertices[[i, 2]] - invNewVertices[[i, 2]]] == 0]] &&
                
                (kx != 0 || ky != 0);
        ];
        
        
        {kx, ky, smer, popis, newVertices}
    ];


StvorecSkosenieNoDisplayWithParams[vertices_] := 
    Module[{normalizedVertices, shearParams, outputVertices, kx, ky, smer, koef},
        
        normalizedVertices = NormalizeVertices[vertices];
        
        
        shearParams = GetShearingParameters[normalizedVertices];
        kx = shearParams[[1]];
        ky = shearParams[[2]];
        smer = shearParams[[3]];
        outputVertices = shearParams[[5]];
        
        
        kx = If[NumberQ[kx] && !IntegerQ[kx], Rationalize[kx, 0], kx];
        ky = If[NumberQ[ky] && !IntegerQ[ky], Rationalize[ky, 0], ky];
        
        
        koef = {kx, ky};
        
        
        {koef, outputVertices}
    ];


CreateVisualization[originalVertices_, finalVertices_, k_, smer_] := 
    Module[{allVertices, xMin, xMax, yMin, yMax, rangeBuffer, xRange, yRange, 
            labelOffsets, kx, ky, brightGreen},
            
        
        brightGreen = RGBColor[0, 0.8, 0.2];
            
        
        If[smer == "v smere os\[IAcute] x a y",
            
            If[VectorQ[k],
                kx = If[NumberQ[k[[1]]] && !IntegerQ[k[[1]]], Rationalize[k[[1]], 0], k[[1]]];
                ky = If[NumberQ[k[[2]]] && !IntegerQ[k[[2]]], Rationalize[k[[2]], 0], k[[2]]];
                ,
                
                kx = If[NumberQ[k] && !IntegerQ[k], Rationalize[k, 0], k];
                ky = kx
            ]
        ,
            
            If[smer == "v smere osi x",
                kx = If[NumberQ[k] && !IntegerQ[k], Rationalize[k, 0], k];
                ky = 0;
            ,
                kx = 0;
                ky = If[NumberQ[k] && !IntegerQ[k], Rationalize[k, 0], k];
            ]
        ];
        
        
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
        
        Graphics[{
            
            LightGray, Thin,
            Table[Line[{{xRange[[1]], y}, {xRange[[2]], y}}], 
                {y, Ceiling[yRange[[1]]], Floor[yRange[[2]]], 1}],
            Table[Line[{{x, yRange[[1]]}, {x, yRange[[2]]}}], 
                {x, Ceiling[xRange[[1]]], Floor[xRange[[2]]], 1}],
            
            
            Blue, Thick, Opacity[0.7],
            Line[Append[originalVertices, First[originalVertices]]],
            
            
            Red, Thick, Opacity[0.7],
            Line[Append[finalVertices, First[finalVertices]]],
            
            
            brightGreen, Dashed,
            Table[
                Line[{originalVertices[[i]], finalVertices[[i]]}],
                {i, Length[originalVertices]}
            ],
            
            
            White, 
            Table[
                Disk[originalVertices[[i]], 0.45],
                {i, 4}
            ],
            Table[
                Disk[finalVertices[[i]], 0.45],
                {i, 4}
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
                {i, 4}
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


DisplayIntuitiveMathMatrixMultiplication[matrix_, vector_, result_, pointName_, smer_] := 
    Module[{darkGreen = RGBColor[0, 0.5, 0], rationalMatrix, rationalVector, rationalResult},
        
        rationalMatrix = Map[If[NumberQ[#] && !IntegerQ[#], Rationalize[#, 0], #] &, matrix, {2}];
        rationalVector = Map[If[NumberQ[#] && !IntegerQ[#], Rationalize[#, 0], #] &, vector];
        rationalResult = Map[If[NumberQ[#] && !IntegerQ[#], Rationalize[#, 0], #] &, result];
        
        
        Print[Style["Maticov\[YAcute] z\[AAcute]pis transform\[AAcute]cie bodu " <> pointName <> ":", Bold]];
        
        
        Print[
            Grid[{
                {
                    MatrixForm[Map[Style[#, Red] &, rationalMatrix, {2}]], 
                    " \[CenterDot] ", 
                    MatrixForm[Map[Style[#, Blue] &, rationalVector]],
                    " = ",
                    MatrixForm[Map[Style[#, darkGreen] &, rationalResult]]
                }
            }]
        ];
        
        
        Print[Style["Podrobn\[YAcute] v\[YAcute]po\[CHacek]et jednotliv\[YAcute]ch s\[UAcute]radn\[IAcute]c:", Bold]];
        
        
        Print[Style["\[Bullet] V\[YAcute]po\[CHacek]et novej x-ovej s\[UAcute]radnice (1. riadok matice):", Brown]];
        
        
        Print[
            Grid[{
                {
                    "   [", 
                    Style[rationalMatrix[[1, 1]], Red], 
                    Style[rationalMatrix[[1, 2]], Red], 
                    "] \[CenterDot] [",
                    Style[rationalVector[[1]], Blue],
                    Style[rationalVector[[2]], Blue],
                    "]^T =",
                    Style[rationalResult[[1]], darkGreen]
                }
            }, Alignment -> Left]
        ];
        
        
        Print["   = (", Style[rationalMatrix[[1, 1]], Red], " \[CenterDot] ", Style[rationalVector[[1]], Blue], 
              ") + (", Style[rationalMatrix[[1, 2]], Red], " \[CenterDot] ", Style[rationalVector[[2]], Blue], ")"];
        
        
        Print["   = (", Style[rationalMatrix[[1, 1]], Red], " \[CenterDot] ", Style[rationalVector[[1]], Blue], 
              ") + (", Style[rationalMatrix[[1, 2]], Red], " \[CenterDot] ", Style[rationalVector[[2]], Blue], ")"];
        
        
        Print["   = ", Style[Rationalize[rationalMatrix[[1, 1]]*rationalVector[[1]], 0], Purple], 
              " + ", Style[Rationalize[rationalMatrix[[1, 2]]*rationalVector[[2]], 0], Purple]];
        
        
        Print["   = ", Style[rationalResult[[1]], darkGreen]];
        
        
        Print[Style["\n\[Bullet] V\[YAcute]po\[CHacek]et novej y-ovej s\[UAcute]radnice (2. riadok matice):", Brown]];
        
        
        Print[
            Grid[{
                {
                    "   [", 
                    Style[rationalMatrix[[2, 1]], Red], 
                    Style[rationalMatrix[[2, 2]], Red], 
                    "] \[CenterDot] [",
                    Style[rationalVector[[1]], Blue],
                    Style[rationalVector[[2]], Blue],
                    "]^T =",
                    Style[rationalResult[[2]], darkGreen]
                }
            }, Alignment -> Left]
        ];
        
        
        Print["   = (", Style[rationalMatrix[[2, 1]], Red], " \[CenterDot] ", Style[rationalVector[[1]], Blue], 
              ") + (", Style[rationalMatrix[[2, 2]], Red], " \[CenterDot] ", Style[rationalVector[[2]], Blue], ")"];
        
        
        Print["   = (", Style[rationalMatrix[[2, 1]], Red], " \[CenterDot] ", Style[rationalVector[[1]], Blue], 
              ") + (", Style[rationalMatrix[[2, 2]], Red], " \[CenterDot] ", Style[rationalVector[[2]], Blue], ")"];
        
        
        Print["   = ", Style[Rationalize[rationalMatrix[[2, 1]]*rationalVector[[1]], 0], Purple], 
              " + ", Style[Rationalize[rationalMatrix[[2, 2]]*rationalVector[[2]], 0], Purple]];
        
        
        Print["   = ", Style[rationalResult[[2]], darkGreen]];
        
        
        If[smer == "v smere os\[IAcute] x a y",
            Print[Style["\nPre skosenie v smere os\[IAcute] x a y:", Bold]];
            Print["- X-ov\[AAcute] s\[UAcute]radnica sa men\[IAcute] pod\:013ea hodnoty Y: x' = x + kx\[CenterDot]y"];
            Print["- Y-ov\[AAcute] s\[UAcute]radnica sa men\[IAcute] pod\:013ea hodnoty X: y' = ky\[CenterDot]x + y"];
            Print["V\[YAcute]sledn\[EAcute] s\[UAcute]radnice bodu ", pointName, "' = [", Style[rationalResult[[1]], darkGreen], ", ", Style[rationalResult[[2]], darkGreen], "]"]
        ,
            If[smer == "v smere osi x",
                Print[Style["\nPre skosenie v smere osi x:", Bold]];
                Print["- X-ov\[AAcute] s\[UAcute]radnica sa men\[IAcute] pod\:013ea hodnoty Y: x' = x + k\[CenterDot]y"];
                Print["- Y-ov\[AAcute] s\[UAcute]radnica zost\[AAcute]va nezmenen\[AAcute]: y' = y"];
                Print["V\[YAcute]sledn\[EAcute] s\[UAcute]radnice bodu ", pointName, "' = [", Style[rationalResult[[1]], darkGreen], ", ", Style[rationalResult[[2]], darkGreen], "]"]
            ,
                Print[Style["\nPre skosenie v smere osi y:", Bold]];
                Print["- X-ov\[AAcute] s\[UAcute]radnica zost\[AAcute]va nezmenen\[AAcute]: x' = x"];
                Print["- Y-ov\[AAcute] s\[UAcute]radnica sa men\[IAcute] pod\:013ea hodnoty X: y' = y + k\[CenterDot]x"];
                Print["V\[YAcute]sledn\[EAcute] s\[UAcute]radnice bodu ", pointName, "' = [", Style[rationalResult[[1]], darkGreen], ", ", Style[rationalResult[[2]], darkGreen], "]"]
            ]
        ];
    ];


DisplayTransformMatrix[matrix_] := 
    Module[{rationalMatrix},
        
        rationalMatrix = Map[If[NumberQ[#] && !IntegerQ[#], Rationalize[#, 0], #] &, matrix, {2}];
        
        
        MatrixForm[Map[Style[#, Red] &, rationalMatrix, {2}]]
    ];

StvorecSkosenie[inputVertices_] := 
    Module[{normalizedVertices, shearParams, outputVertices, kx, ky, smer, popis, 
            invKx, invKy, kVektor, 
            detectInputOutput = False, finalVertices = None,
            transformMatrix, homogeneousVertices, resultVertices, darkGreen = RGBColor[0, 0.5, 0]},
            
        
        normalizedVertices = If[MatrixQ[inputVertices] && Length[inputVertices] == 4 && 
                             Length[First[inputVertices]] == 2,
            
            inputVertices,
            
            NormalizeVertices[inputVertices]
        ];
        
        
        If[Length[inputVertices] > 4 && Mod[Length[inputVertices], 4] == 0,
            
            normalizedVertices = inputVertices[[1;;4]];
            
            finalVertices = inputVertices[[-(4;;-1)]];
            detectInputOutput = True
        ];
        
        
        shearParams = If[detectInputOutput,
            GetShearingParameters[normalizedVertices, finalVertices],
            GetShearingParameters[normalizedVertices]
        ];
        
        kx = shearParams[[1]];
        ky = shearParams[[2]];
        smer = shearParams[[3]];
        popis = shearParams[[4]];
        outputVertices = shearParams[[5]];
        
        
        kx = If[NumberQ[kx] && !IntegerQ[kx], Rationalize[kx, 0], kx];
        ky = If[NumberQ[ky] && !IntegerQ[ky], Rationalize[ky, 0], ky];
        
        
        invKx = -kx;
        invKy = -ky;
        
        
        If[smer == "v smere os\[IAcute] x a y",
            
            kVektor = {kx, ky};
            
            transformMatrix = {{1, kx}, {ky, 1}};
        ,
            
            kVektor = If[smer == "v smere osi x", kx, ky];
            
            If[smer == "v smere osi x",
                transformMatrix = {{1, kx}, {0, 1}},
                transformMatrix = {{1, 0}, {ky, 1}}
            ];
        ];
        
        
        Print[Style[
            "GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE \[CapitalSHacek]TVORCA - SKOSENIE", 
            Bold, 16]];
        Print[Style["========================================", Bold]];
        
        
        Print[Style["\nZADANIE:", Bold, 14]];
        Print["Majme \[SHacek]tvorec ABCD s vrcholmi:"];
        Print[MatrixForm[Map[Style[#, Blue] &, normalizedVertices, {2}]]];
        
        
        If[smer == "v smere os\[IAcute] x a y",
            Print["\nVykonajte skosenie \[SHacek]tvorca v 2D priestore v smere osi x s koeficientom k = ", 
                Style[FormatExactValue[kx], Red], " a v smere osi y s koeficientom k = ", 
                Style[FormatExactValue[ky], Red], "."];
        ,
            Print["\nVykonajte skosenie \[SHacek]tvorca v 2D priestore " <> smer <> " s koeficientom k = ", 
                Style[FormatExactValue[kVektor], Red], "."];
        ];
        
        
        Print["\nTransforma\[CHacek]n\[AAcute] matica skosenia:"];
        Print[DisplayTransformMatrix[transformMatrix]];
        
        
        Print[Style["\nPOSTUP:", Bold, 14]];
        Print["Pre v\[YAcute]po\[CHacek]et pou\[ZHacek]ijeme maticu skosenia:"];
        
        
        If[smer == "v smere os\[IAcute] x a y",
            Print[Style["Transforma\[CHacek]n\[AAcute] matica pre skosenie v smere os\[IAcute] x a y:", Bold]];
            Print[MatrixForm[{
                {Style[1, Red], Style["kx", Red]}, 
                {Style["ky", Red], Style[1, Red]}
            }]];
            Print["\nVzorec pre v\[YAcute]po\[CHacek]et nov\[YAcute]ch s\[UAcute]radn\[IAcute]c pri skosen\[IAcute] v smere os\[IAcute] x a y:"];
            Print["x' = x + kx\[CenterDot]y"];
            Print["y' = ky\[CenterDot]x + y"];
            Print["\nNa\[SHacek]e hodnoty: kx = ", FormatExactValue[kx], ", ky = ", FormatExactValue[ky]];
        ,
            Print[Style["Transforma\[CHacek]n\[AAcute] matica pre skosenie " <> smer <> ":", Bold]];
            If[smer == "v smere osi x",
                Print[MatrixForm[{
                    {Style[1, Red], Style["k", Red]}, 
                    {Style[0, Red], Style[1, Red]}
                }]];
                Print["\nVzorec pre v\[YAcute]po\[CHacek]et nov\[YAcute]ch s\[UAcute]radn\[IAcute]c pri skosen\[IAcute] v smere osi x:"];
                Print["x' = x + k\[CenterDot]y"];
                Print["y' = y"]
            ,
                Print[MatrixForm[{
                    {Style[1, Red], Style[0, Red]}, 
                    {Style["k", Red], Style[1, Red]}
                }]];
                Print["\nVzorec pre v\[YAcute]po\[CHacek]et nov\[YAcute]ch s\[UAcute]radn\[IAcute]c pri skosen\[IAcute] v smere osi y:"];
                Print["x' = x"];
                Print["y' = k\[CenterDot]x + y"]
            ];
            Print["\nNa\[SHacek]a hodnota k = ", FormatExactValue[kVektor]];
        ];
        
        Print[Style["\nTE\[CapitalOAcute]RIA MATICOV\[CapitalEAcute]HO ZOBRAZENIA SKOSENIA:", Bold, 14]];
        Print["Pri skosen\[IAcute] v 2D priestore pou\[ZHacek]\[IAcute]vame transforma\[CHacek]n\[EAcute] matice, ktor\[EAcute] umo\[ZHacek]\[NHacek]uj\[UAcute] vyjadri\[THacek] skosenie ako maticov\[EAcute] n\[AAcute]sobenie:"];
        
        
        Print["\nObecn\[AAcute] transforma\[CHacek]n\[AAcute] matica skosenia:"];
        If[smer == "v smere os\[IAcute] x a y",
            Print[MatrixForm[{
                {Style[1, Red], Style["kx", Red]}, 
                {Style["ky", Red], Style[1, Red]}
            }]]
        ,
            If[smer == "v smere osi x",
                Print[MatrixForm[{
                    {Style[1, Red], Style["k", Red]}, 
                    {Style[0, Red], Style[1, Red]}
                }]]
            ,
                Print[MatrixForm[{
                    {Style[1, Red], Style[0, Red]}, 
                    {Style["k", Red], Style[1, Red]}
                }]]
            ]
        ];
        
        
        Print["\nAk je bod P = [x, y] v 2D priestore, skosenie m\[OHat]\[ZHacek]eme zap\[IAcute]sa\[THacek] ako maticov\[EAcute] n\[AAcute]sobenie:"];
        
        
        If[smer == "v smere os\[IAcute] x a y",
            Print[Row[{
                MatrixForm[{
                    {Style[1, Red], Style["kx", Red]}, 
                    {Style["ky", Red], Style[1, Red]}
                }],
                " \[CenterDot] ",
                MatrixForm[{
                    {Style["x", Blue]}, 
                    {Style["y", Blue]}
                }],
                " = ",
                MatrixForm[{
                    {Style["x + kx\[CenterDot]y", darkGreen]}, 
                    {Style["ky\[CenterDot]x + y", darkGreen]}
                }]
            }]]
        ,
            If[smer == "v smere osi x",
                Print[Row[{
                    MatrixForm[{
                        {Style[1, Red], Style["k", Red]}, 
                        {Style[0, Red], Style[1, Red]}
                    }],
                    " \[CenterDot] ",
                    MatrixForm[{
                        {Style["x", Blue]}, 
                        {Style["y", Blue]}
                    }],
                    " = ",
                    MatrixForm[{
                        {Style["x + k\[CenterDot]y", darkGreen]}, 
                        {Style["y", darkGreen]}
                    }]
                }]]
            ,
                Print[Row[{
                    MatrixForm[{
                        {Style[1, Red], Style[0, Red]}, 
                        {Style["k", Red], Style[1, Red]}
                    }],
                    " \[CenterDot] ",
                    MatrixForm[{
                        {Style["x", Blue]}, 
                        {Style["y", Blue]}
                    }],
                    " = ",
                    MatrixForm[{
                        {Style["x", darkGreen]}, 
                        {Style["k\[CenterDot]x + y", darkGreen]}
                    }]
                }]]
            ]
        ];
        
        
        If[smer == "v smere os\[IAcute] x a y",
            Print["\nPre na\[SHacek]e hodnoty skosenia:"];
            Print["kx = ", Style[kx, Red], " (koeficient skosenia v smere osi x)"];
            Print["ky = ", Style[ky, Red], " (koeficient skosenia v smere osi y)"];
        ,
            Print["\nPre na\[SHacek]e hodnoty skosenia:"];
            Print["k = ", Style[kVektor, Red], " (koeficient skosenia ", smer, ")"];
        ];
        
        Print[Style["\nV\[CapitalYAcute]PO\[CapitalCHacek]ET SKOSENIA JEDNOTLIV\[CapitalYAcute]CH VRCHOLOV:", Bold, 14]];
        
        
        Table[
            Module[{x = normalizedVertices[[i, 1]], 
                   y = normalizedVertices[[i, 2]],
                   vector, resultVector},
                   
                Print[Style["\nVRCHOL " <> FromCharacterCode[64 + i] <> ":", Bold, 14]];
                Print["P\[OHat]vodn\[EAcute] s\[UAcute]radnice: [", Style[x, Blue], ", ", Style[y, Blue], "]"];
                
                
                vector = {x, y};
                
                
                If[smer == "v smere os\[IAcute] x a y",
                    resultVector = {x + kx*y, y + ky*x}
                ,
                    If[smer == "v smere osi x",
                        resultVector = {x + kVektor*y, y}
                    ,
                        resultVector = {x, y + kVektor*x}
                    ]
                ];
                
                
                DisplayIntuitiveMathMatrixMultiplication[
                    transformMatrix,
                    vector,
                    resultVector,
                    FromCharacterCode[64 + i],
                    smer
                ];
                
                
                Print[Style["\nAlternat\[IAcute]vny v\[YAcute]po\[CHacek]et pomocou z\[AAcute]kladn\[YAcute]ch vzorcov skosenia:", Bold]];
                
                
                If[smer == "v smere os\[IAcute] x a y",
                    Print["x' = x + kx\[CenterDot]y  =>  ", Style[x, Blue], " + ", Style[kx, Red], 
                          "\[CenterDot]", Style[y, Blue], " = ", Style[x + kx*y, darkGreen]];
                    Print["y' = ky\[CenterDot]x + y  =>  ", Style[ky, Red], "\[CenterDot]", Style[x, Blue], 
                          " + ", Style[y, Blue], " = ", Style[ky*x + y, darkGreen]];
                ,
                    If[smer == "v smere osi x",
                        Print["x' = x + k\[CenterDot]y  =>  ", Style[x, Blue], " + ", Style[kVektor, Red], 
                              "\[CenterDot]", Style[y, Blue], " = ", Style[x + kVektor*y, darkGreen]];
                        Print["y' = y  =>  ", Style[y, Blue], " = ", Style[y, darkGreen]]
                    ,
                        Print["x' = x  =>  ", Style[x, Blue], " = ", Style[x, darkGreen]];
                        Print["y' = k\[CenterDot]x + y  =>  ", Style[kVektor, Red], "\[CenterDot]", 
                              Style[x, Blue], " + ", Style[y, Blue], " = ", Style[kVektor*x + y, darkGreen]]
                    ]
                ];
                
                Print[Style["\nV\[YAcute]sledn\[EAcute] s\[UAcute]radnice vrcholu " <> FromCharacterCode[64 + i] <> "':", Bold]];
                Print["[", Style[outputVertices[[i, 1]], darkGreen], ", ", Style[outputVertices[[i, 2]], darkGreen], "]"];
                
                Print[Style["--------------------------------------------------", Bold]];
            ],
            {i, 4}
        ];
        
        
        Print[Style["\nV\[CapitalYAcute]SLEDOK TRANSFORM\[CapitalAAcute]CIE CEL\[CapitalEAcute]HO \[CapitalSHacek]TVORCA:", Bold, 14]];
        Print[Style["P\[OHat]vodn\[YAcute] \[SHacek]tvorec ABCD:", Blue, Bold]];
        Print[MatrixForm[normalizedVertices]];
        
        Print[Style["Skosen\[YAcute] \[SHacek]tvorec A'B'C'D':", Red, Bold]];
        Print[MatrixForm[outputVertices]];
        
        
        Print[Style["\nVIZUALIZ\[CapitalAAcute]CIA TRANSFORM\[CapitalAAcute]CIE:", Bold]];
        Print[CreateVisualization[normalizedVertices, outputVertices, kVektor, smer]];
        
        
        Print[Style["\nLEGENDA:", Bold, 14]];
        Print[Style["\[Bullet] Modr\[EAcute]", Blue], " body a \[CHacek]iary: P\[OHat]vodn\[YAcute] \[SHacek]tvorec"];
        Print[Style["\[Bullet] \[CapitalCHacek]erven\[EAcute]", Red], " body a \[CHacek]iary: Skosen\[YAcute] \[SHacek]tvorec"];
        Print[Style["\[Bullet] Zelen\[EAcute]", darkGreen], " preru\[SHacek]ovan\[EAcute] \[CHacek]iary: Dr\[AAcute]ha pohybu vrcholov pri skosen\[IAcute]"];
        
        Print[Style["\nFAREBN\[CapitalEAcute] OZNA\[CapitalCHacek]ENIA V MATICOV\[CapitalYAcute]CH V\[CapitalYAcute]PO\[CapitalCHacek]TOCH:", Bold]];
        Print[Style["\[Bullet] \[CapitalCHacek]erven\[AAcute]:", Red], " Transforma\[CHacek]n\[AAcute] matica"];
        Print[Style["\[Bullet] Modr\[AAcute]:", Blue], " Vstupn\[EAcute] s\[UAcute]radnice"];
        Print[Style["\[Bullet] Fialov\[AAcute]:", Purple], " Medziv\[YAcute]po\[CHacek]ty"];
        Print[Style["\[Bullet] Zelen\[AAcute]:", darkGreen], " V\[YAcute]sledn\[EAcute] s\[UAcute]radnice"];
                
        Print[Style["\nP\[OHat]vodn\[EAcute] vrcholy (modr\[AAcute]):", Bold]];
        Table[
            Print[Row[{Style["Vrchol " <> FromCharacterCode[64 + i] <> ": ", 
                RGBColor[0.1, 0.1, 1]], normalizedVertices[[i]]}]], {i, 4}];
                
        Print[Style["\nV\[YAcute]sledn\[EAcute] vrcholy (\[CHacek]erven\[AAcute]):", Bold]];
        Module[{expandedOutput},
            Table[
                expandedOutput = Expand[outputVertices[[i]]];
                Print[Row[{Style["Vrchol " <> FromCharacterCode[64 + i] <> "': ", 
                    RGBColor[1, 0.1, 0.1]], expandedOutput}]], {i, 4}];
        ];
                
        Print[Style["\nInverzn\[AAcute] transform\[AAcute]cia:", Bold]];
        Print["Pre vr\[AAcute]tenie do p\[OHat]vodn\[EAcute]ho stavu pou\[ZHacek]ite koeficient skosenia:"];
        
        
        If[smer == "v smere os\[IAcute] x a y",
            Print["kx' = -kx = ", FormatExactValue[-kx], " v smere osi x"];
            Print["ky' = -ky = ", FormatExactValue[-ky], " v smere osi y"];
        ,
            If[smer == "v smere osi x",
                Print["k' = -k = ", FormatExactValue[-kVektor], " v smere osi x"]
            ,
                Print["k' = -k = ", FormatExactValue[-kVektor], " v smere osi y"]
            ]
        ];
        
        
        Print[Style["\nMATEMATICK\[CapitalEAcute] VLASTNOSTI:", Bold, 14]];
        Print["\[Bullet] Skosenie je line\[AAcute]rna transform\[AAcute]cia, ktor\[AAcute] zachov\[AAcute]va rovnobe\[ZHacek]nos\[THacek] priamok s jednou z os\[IAcute]"];
        
        
        If[smer == "v smere os\[IAcute] x a y",
            Print["\[Bullet] Pri skosen\[IAcute] v smere os\[IAcute] x a y sa menia obe s\[UAcute]radnice bodov"];
            Print["\[Bullet] Skosen\[IAcute]m v smere os\[IAcute] x a y sa priamky rovnobe\[ZHacek]n\[EAcute] s oboma osami deformuj\[UAcute]"];
        ,
            If[smer == "v smere osi x",
                Print["\[Bullet] Pri skosen\[IAcute] v smere osi x zost\[AAcute]vaj\[UAcute] zachovan\[EAcute] y-ov\[EAcute] s\[UAcute]radnice v\[SHacek]etk\[YAcute]ch bodov"];
                Print["\[Bullet] Zvisl\[EAcute] priamky (rovnobe\[ZHacek]n\[EAcute] s osou y) zost\[AAcute]vaj\[UAcute] zvisl\[EAcute]"]
            ,
                Print["\[Bullet] Pri skosen\[IAcute] v smere osi y zost\[AAcute]vaj\[UAcute] zachovan\[EAcute] x-ov\[EAcute] s\[UAcute]radnice v\[SHacek]etk\[YAcute]ch bodov"];
                Print["\[Bullet] Vodorovn\[EAcute] priamky (rovnobe\[ZHacek]n\[EAcute] s osou x) zost\[AAcute]vaj\[UAcute] vodorovn\[EAcute]"]
            ]
        ];
        
        Print["\[Bullet] Skosenie men\[IAcute] uhly a tvary geometrick\[YAcute]ch \[UAcute]tvarov, ale zachov\[AAcute]va ich obsah"];
        Print["\[Bullet] Body le\[ZHacek]iace na osi, ktor\[AAcute] je rovnobe\[ZHacek]n\[AAcute] so smerom skosenia, sa nemenia"];
        Print["\[Bullet] Skosenie je af\[IAcute]nna transform\[AAcute]cia, ktor\[AAcute] nezachov\[AAcute]va vzdialenosti ani uhly"];
        
        
        Print[Style["\n\[CapitalSHacek]PECI\[CapitalAAcute]LNE PR\[CapitalIAcute]PADY:", Bold, 14]];
        Print["\[Bullet] Pre k = 0 je skosenie identickou transform\[AAcute]ciou (bod sa nemen\[IAcute])"];
        Print["\[Bullet] Pre ve\:013ek\[EAcute] hodnoty |k| sa \[UAcute]tvar v\[YAcute]razne skos\[IAcute] a m\[OHat]\[ZHacek]e by\[THacek] \[THacek]a\[ZHacek]\[SHacek]ie rozpoznate\:013en\[YAcute]"];
        Print["\[Bullet] Pre k = 1 sa vytvor\[IAcute] rovnoramenn\[YAcute] pravouhl\[YAcute] trojuholn\[IAcute]k z p\[OHat]vodn\[EAcute]ho \[SHacek]tvorca"];
        Print["\[Bullet] Pre k = -1 sa vytvor\[IAcute] rovnoramenn\[YAcute] pravouhl\[YAcute] trojuholn\[IAcute]k z p\[OHat]vodn\[EAcute]ho \[SHacek]tvorca (v opa\[CHacek]nom smere)"];
        Print["\[Bullet] Skosenie \[SHacek]tvorca v 2D priestore je jedn\[YAcute]m zo z\[AAcute]kladn\[YAcute]ch geometrick\[YAcute]ch transform\[AAcute]ci\[IAcute]"];
        
        
        outputVertices
    ];
    

StvorecSkosenie::invalidInput = "Neplatn\[YAcute] vstup. O\[CHacek]ak\[AAcute]va sa matica alebo zoznam s\[UAcute]radn\[IAcute]c \[SHacek]tvorca.";
StvorecSkosenie::insufficientVertices = "Nedostato\[CHacek]n\[YAcute] po\[CHacek]et vrcholov. S\[UAcute] potrebn\[EAcute] aspo\[NHacek] 4 vrcholy.";

End[];
EndPackage[];



