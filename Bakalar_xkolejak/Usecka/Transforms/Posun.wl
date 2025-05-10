(* ::Package:: *)

(* ::Package:: *)
(**)


(* ::Package:: *)
(**)



BeginPackage["UseckaHardBalik`Transforms`Posun`", {"UseckaHardBalik`"}];

UseckaPosunVector::usage = "UseckaPosunVector je globalny vektor posunu [dx, dy], ktory sa pouziva v UseckaPosun funkcii.";

UseckaPosun::usage = 
    "UseckaPosun[vertices_] zobraz\[IAcute] interakt\[IAcute]vny tutori\[AAcute]l pre posun \[UAcute]se\[CHacek]ky a vr\[AAcute]ti nov\[EAcute] vrcholy.";

Begin["`Private`"];

UseckaPosunVector = {0, 0};

NormalizeVertices[vertices_] := 
    Module[{processedVertices},
        
        processedVertices = Which[
            
            MatrixQ[vertices], vertices,
            
            
            ListQ[vertices] && Length[Dimensions[vertices]] == 2, 
                If[Length[DeleteDuplicates[Length /@ vertices]] > 1,
                    
                    Block[{cleanedVertices = Select[vertices, VectorQ]},
                        If[Length[cleanedVertices] >= 2,
                            cleanedVertices[[1;;2]],
                            Message[UseckaPosun::invalidInput]; 
                            Abort[]
                        ]
                    ],
                    
                    vertices
                ],
            
            
            ListQ[vertices] && Length[Dimensions[vertices]] == 1 && EvenQ[Length[vertices]], 
                Partition[vertices, 2],
                
            
            True, 
                Message[UseckaPosun::invalidInput]; 
                Abort[]
        ];
        
        If[Length[processedVertices] < 2,
            Message[UseckaPosun::insufficientVertices];
            Abort[]
        ];
        
        If[Max[Abs[processedVertices[[All, 1]]]] > 10 || Max[Abs[processedVertices[[All, 2]]]] > 10,
            
            processedVertices = processedVertices / (Max[Abs[Flatten[processedVertices]]] / 6);
        ];
        
        
        processedVertices[[1;;2]]
    ];

GetTranslationParameters[vertices_] := 
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
        
        
        UseckaPosunVector = {dx, dy};
        
        
        {dx, dy, "Posun o [" <> ToString[dx] <> ", " <> ToString[dy] <> "]", newVertices}
    ];

CreateVisualization[originalVertices_, finalVertices_, translationVector_] := 
    Module[{allVertices, xMin, xMax, yMin, yMax, rangeBuffer, xRange, yRange, 
            labelOffsets, dx = translationVector[[1]], dy = translationVector[[2]]},
        
        allVertices = Join[originalVertices, finalVertices];
        
        
        xMin = Min[allVertices[[All, 1]]];
        xMax = Max[allVertices[[All, 1]]];
        yMin = Min[allVertices[[All, 2]]];
        yMax = Max[allVertices[[All, 2]]];
        
        
        rangeBuffer = 2;
        
        
        xRange = {Min[-11, xMin - rangeBuffer], Max[11, xMax + rangeBuffer]};
        yRange = {Min[-11, yMin - rangeBuffer], Max[11, yMax + rangeBuffer]};
        
        
        labelOffsets = Table[
            Module[{originalVertex = originalVertices[[i]], 
                    finalVertex = finalVertices[[i]], 
                    offset = {0.7, 0.7}},
                
                
                If[originalVertex[[1]] > 8, offset[[1]] = -1.0];
                If[originalVertex[[2]] > 8, offset[[2]] = -1.0];
                
                {offset, offset}
            ],
            {i, Length[originalVertices]}
        ];
        
        Graphics[{
            
            LightGray, Thin,
            Table[Line[{{xRange[[1]], y}, {xRange[[2]], y}}], 
                {y, Ceiling[yRange[[1]]], Floor[yRange[[2]]], 2}],
            Table[Line[{{x, yRange[[1]]}, {x, yRange[[2]]}}], 
                {x, Ceiling[xRange[[1]]], Floor[xRange[[2]]], 2}],
            
            
            Blue, Thick, Opacity[0.7],
            Line[originalVertices],
            
            
            Red, Thick, Opacity[0.7],
            Line[finalVertices],
            
            
            Green, Dashed,
            Table[
                Line[{originalVertices[[i]], finalVertices[[i]]}],
                {i, Length[originalVertices]}
            ],
            
            
            White, 
            Table[
                Disk[originalVertices[[i]], 0.45],
                {i, 2}
            ],
            Table[
                Disk[finalVertices[[i]], 0.45],
                {i, 2}
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
                {i, 2}
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
                    Style[matrix[[1, 3]], Red], 
                    "] \[CenterDot] [",
                    Style[vector[[1]], Blue],
                    Style[vector[[2]], Blue],
                    Style[vector[[3]], Blue],
                    "]^T =",
                    Style[result[[1]], darkGreen]
                }
            }, Alignment -> Left]
        ];
        
        
        Print["   = (", Style[matrix[[1, 1]], Red], " \[CenterDot] ", Style[vector[[1]], Blue], 
              ") + (", Style[matrix[[1, 2]], Red], " \[CenterDot] ", Style[vector[[2]], Blue], 
              ") + (", Style[matrix[[1, 3]], Red], " \[CenterDot] ", Style[vector[[3]], Blue], ")"];
        
        
        Print["   = (", Style[matrix[[1, 1]], Red], " \[CenterDot] ", Style[vector[[1]], Blue], 
              ") + (", Style[matrix[[1, 2]], Red], " \[CenterDot] ", Style[vector[[2]], Blue], 
              ") + (", Style[matrix[[1, 3]], Red], ")"];
        
        
        Print["   = ", Style[matrix[[1, 1]]*vector[[1]], Purple], 
              " + ", Style[matrix[[1, 2]]*vector[[2]], Purple],
              " + ", Style[matrix[[1, 3]]*vector[[3]], Purple]];
        
        
        Print["   = ", Style[result[[1]], darkGreen]];
        
        
        Print[Style["\n\[Bullet] V\[YAcute]po\[CHacek]et novej y-ovej s\[UAcute]radnice (2. riadok matice):", Brown]];
        
        
        Print[
            Grid[{
                {
                    "   [", 
                    Style[matrix[[2, 1]], Red], 
                    Style[matrix[[2, 2]], Red], 
                    Style[matrix[[2, 3]], Red], 
                    "] \[CenterDot] [",
                    Style[vector[[1]], Blue],
                    Style[vector[[2]], Blue],
                    Style[vector[[3]], Blue],
                    "]^T =",
                    Style[result[[2]], darkGreen]
                }
            }, Alignment -> Left]
        ];
        
        
        Print["   = (", Style[matrix[[2, 1]], Red], " \[CenterDot] ", Style[vector[[1]], Blue], 
              ") + (", Style[matrix[[2, 2]], Red], " \[CenterDot] ", Style[vector[[2]], Blue], 
              ") + (", Style[matrix[[2, 3]], Red], " \[CenterDot] ", Style[vector[[3]], Blue], ")"];
        
        
        Print["   = (", Style[matrix[[2, 1]], Red], " \[CenterDot] ", Style[vector[[1]], Blue], 
              ") + (", Style[matrix[[2, 2]], Red], " \[CenterDot] ", Style[vector[[2]], Blue], 
              ") + (", Style[matrix[[2, 3]], Red], ")"];
        
        
        Print["   = ", Style[matrix[[2, 1]]*vector[[1]], Purple], 
              " + ", Style[matrix[[2, 2]]*vector[[2]], Purple],
              " + ", Style[matrix[[2, 3]]*vector[[3]], Purple]];
        
        
        Print["   = ", Style[result[[2]], darkGreen]];
        
        
        Print[Style["\n\[Bullet] V\[YAcute]po\[CHacek]et homog\[EAcute]nnej s\[UAcute]radnice (3. riadok matice):", Brown]];
        
        
        Print[
            Grid[{
                {
                    "   [", 
                    Style[matrix[[3, 1]], Red], 
                    Style[matrix[[3, 2]], Red], 
                    Style[matrix[[3, 3]], Red], 
                    "] \[CenterDot] [",
                    Style[vector[[1]], Blue],
                    Style[vector[[2]], Blue],
                    Style[vector[[3]], Blue],
                    "]^T =",
                    Style[result[[3]], darkGreen]
                }
            }, Alignment -> Left]
        ];
        
        
        Print["   = (", Style[matrix[[3, 1]], Red], " \[CenterDot] ", Style[vector[[1]], Blue], 
              ") + (", Style[matrix[[3, 2]], Red], " \[CenterDot] ", Style[vector[[2]], Blue], 
              ") + (", Style[matrix[[3, 3]], Red], " \[CenterDot] ", Style[vector[[3]], Blue], ")"];
        
        
        Print["   = (", Style[matrix[[3, 1]], Red], " \[CenterDot] ", Style[vector[[1]], Blue], 
              ") + (", Style[matrix[[3, 2]], Red], " \[CenterDot] ", Style[vector[[2]], Blue], 
              ") + (", Style[matrix[[3, 3]], Red], ")"];
        
        
        Print["   = ", Style[matrix[[3, 1]]*vector[[1]], Purple], 
              " + ", Style[matrix[[3, 2]]*vector[[2]], Purple],
              " + ", Style[matrix[[3, 3]]*vector[[3]], Purple]];
        
        
        Print["   = ", Style[result[[3]], darkGreen]];
        
        
        Print[Style["\nV\[YAcute]sledn\[AAcute] homog\[EAcute]nna reprezent\[AAcute]cia bodu " <> pointName <> "':", Bold]];
        Print["[", Style[result[[1]], darkGreen], ", ", Style[result[[2]], darkGreen], ", ", Style[result[[3]], darkGreen], "]"];
        
        
        Print[Style["\nKart\[EAcute]zske s\[UAcute]radnice bodu " <> pointName <> "':", Bold]];
        Print["[", Style[result[[1]], darkGreen], ", ", Style[result[[2]], darkGreen], "]"];
    ];

UseckaPosun[inputVertices_] := 
    Module[{normalizedVertices, outputVertices, dx, dy, invDx, invDy,
            transformMatrix, homogeneousVertices, resultVertices, darkGreen = RGBColor[0, 0.5, 0]},
            
        
        normalizedVertices = NormalizeVertices[inputVertices];
        
        
        If[UseckaPosunVector =!= {0, 0},
            
            {dx, dy} = UseckaPosunVector;
            
            UseckaPosunVector = {0, 0};
        ,
            
            {dx, dy, _, outputVertices} = GetTranslationParameters[normalizedVertices];
        ];
        
        
        outputVertices = Map[{#[[1]] + dx, #[[2]] + dy} &, normalizedVertices];
        
        
        invDx = -dx;
        invDy = -dy;
        
        
        transformMatrix = {{1, 0, dx}, {0, 1, dy}, {0, 0, 1}};
        
        
        Print[Style[
            "GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE \[CapitalUAcute]SE\[CapitalCHacek]KY - POSUN", 
            Bold, 16]];
        Print[Style["========================================", Bold]];
        
        
        Print[Style["\nZADANIE:", Bold, 14]];
        Print["Majme \[UAcute]se\[CHacek]ku AB s bodmi:"];
        Print[MatrixForm[Map[Style[#, Blue] &, normalizedVertices, {2}]]];
        
        Print["\nVykonajte posun \[UAcute]se\[CHacek]ky v 2D priestore pomocou vektora posunu: \[CapitalDelta] = [", 
            Style[dx, Red], ", ", Style[dy, Red], "]"];
            
        Print["\nTransforma\[CHacek]n\[AAcute] matica posunu v homog\[EAcute]nnych s\[UAcute]radniciach:"];
        Print[MatrixForm[{
            {Style[1, Red], Style[0, Red], Style[dx, Red]}, 
            {Style[0, Red], Style[1, Red], Style[dy, Red]},
            {Style[0, Red], Style[0, Red], Style[1, Red]}
        }]];
        
        
        Print[Style["\nTE\[CapitalOAcute]RIA MATICOV\[CapitalEAcute]HO ZOBRAZENIA POSUNU:", Bold, 14]];
        Print["Pri posune v 2D priestore pou\[ZHacek]\[IAcute]vame homog\[EAcute]nne s\[UAcute]radnice, ktor\[EAcute] umo\[ZHacek]\[NHacek]uj\[UAcute] vyjadri\[THacek] posun ako maticov\[EAcute] n\[AAcute]sobenie:"];
        
        
        Print["\nTransforma\[CHacek]n\[AAcute] matica posunu:"];
        Print[MatrixForm[{
            {Style[1, Red], Style[0, Red], Style["dx", Red]}, 
            {Style[0, Red], Style[1, Red], Style["dy", Red]},
            {Style[0, Red], Style[0, Red], Style[1, Red]}
        }]];
        
        
        Print["\nAk je bod P = [x, y] v z\[AAcute]kladn\[YAcute]ch s\[UAcute]radniciach, v homog\[EAcute]nnych s\[UAcute]radniciach ho zap\[IAcute]\[SHacek]eme ako P = [x, y, 1].\nPosun potom m\[OHat]\[ZHacek]eme zap\[IAcute]sa\[THacek] ako maticov\[EAcute] n\[AAcute]sobenie:"];
        
        Print[Row[{
            MatrixForm[{
                {Style[1, Red], Style[0, Red], Style["dx", Red]}, 
                {Style[0, Red], Style[1, Red], Style["dy", Red]},
                {Style[0, Red], Style[0, Red], Style[1, Red]}
            }],
            " \[CenterDot] ",
            MatrixForm[{
                {Style["x", Blue]}, 
                {Style["y", Blue]},
                {Style[1, Blue]}
            }],
            " = ",
            MatrixForm[{
                {Style["x + dx", darkGreen]}, 
                {Style["y + dy", darkGreen]},
                {Style[1, darkGreen]}
            }]
        }]];
        
        Print["\nPre na\[SHacek]e hodnoty posunu:"];
        Print["dx = ", Style[dx, Red], " (posun v smere osi x)"];
        Print["dy = ", Style[dy, Red], " (posun v smere osi y)"];
        
        Print[Style["\nV\[CapitalYAcute]PO\[CapitalCHacek]ET POSUNU JEDNOTLIV\[CapitalYAcute]CH BODOV:", Bold, 14]];
        
        
        Table[
            Module[{x = normalizedVertices[[i, 1]], 
                   y = normalizedVertices[[i, 2]],
                   homogeneousVector, resultVector},
                   
                Print[Style["\nBOD " <> FromCharacterCode[64 + i] <> ":", Bold, 14]];
                Print["P\[OHat]vodn\[EAcute] s\[UAcute]radnice: [", Style[x, Blue], ", ", Style[y, Blue], "]"];
                Print["Homog\[EAcute]nne s\[UAcute]radnice: [", Style[x, Blue], ", ", Style[y, Blue], ", ", Style[1, Blue], "]"];
                
                
                homogeneousVector = {x, y, 1};
                resultVector = {x + dx, y + dy, 1};
                
                
                DisplayIntuitiveMathMatrixMultiplication[
                    transformMatrix, 
                    homogeneousVector, 
                    resultVector,
                    FromCharacterCode[64 + i]
                ];
                
                
                Print[Style["\nAlternat\[IAcute]vny v\[YAcute]po\[CHacek]et pomocou z\[AAcute]kladn\[YAcute]ch vzorcov posunu:", Bold]];
                Print["x' = x + dx  =>  ", Style[x, Blue], " + ", Style[dx, Red], " = ", Style[x + dx, darkGreen]];
                Print["y' = y + dy  =>  ", Style[y, Blue], " + ", Style[dy, Red], " = ", Style[y + dy, darkGreen]];
                
                Print[Style["\nV\[YAcute]sledn\[EAcute] s\[UAcute]radnice bodu " <> FromCharacterCode[64 + i] <> "':", Bold]];
                Print["[", Style[outputVertices[[i, 1]], darkGreen], ", ", Style[outputVertices[[i, 2]], darkGreen], "]"];
                
                Print[Style["--------------------------------------------------", Bold]];
            ],
            {i, 2}
        ];
        
        
        Print[Style["\nV\[CapitalYAcute]SLEDOK TRANSFORM\[CapitalAAcute]CIE CELEJ \[CapitalUAcute]SE\[CapitalCHacek]KY:", Bold, 14]];
        Print[Style["P\[OHat]vodn\[AAcute] \[UAcute]se\[CHacek]ka AB:", Blue, Bold]];
        Print[MatrixForm[normalizedVertices]];
        
        Print[Style["Posunut\[AAcute] \[UAcute]se\[CHacek]ka A'B':", Red, Bold]];
        Print[MatrixForm[outputVertices]];
        
        
        Print[Style["\nVIZUALIZ\[CapitalAAcute]CIA TRANSFORM\[CapitalAAcute]CIE:", Bold]];
        Print[CreateVisualization[normalizedVertices, outputVertices, {dx, dy}]];
        
        
        Print[Style["\nLEGENDA:", Bold, 14]];
        Print[Style["\[Bullet] Modr\[EAcute]", Blue], " body a \[CHacek]iary: P\[OHat]vodn\[AAcute] \[UAcute]se\[CHacek]ka"];
        Print[Style["\[Bullet] \[CapitalCHacek]erven\[EAcute]", Red], " body a \[CHacek]iary: Posunut\[AAcute] \[UAcute]se\[CHacek]ka"];
        Print[Style["\[Bullet] Zelen\[EAcute]", darkGreen], " preru\[SHacek]ovan\[EAcute] \[CHacek]iary: Posun jednotliv\[YAcute]ch bodov"];
        
        Print[Style["\nFAREBN\[CapitalEAcute] OZNA\[CapitalCHacek]ENIA V MATICOV\[CapitalYAcute]CH V\[CapitalYAcute]PO\[CapitalCHacek]TOCH:", Bold]];
        Print[Style["\[Bullet] \[CapitalCHacek]erven\[AAcute]:", Red], " Transforma\[CHacek]n\[AAcute] matica"];
        Print[Style["\[Bullet] Modr\[AAcute]:", Blue], " Vstupn\[EAcute] s\[UAcute]radnice"];
        Print[Style["\[Bullet] Fialov\[AAcute]:", Purple], " Medziv\[YAcute]po\[CHacek]ty"];
        Print[Style["\[Bullet] Zelen\[AAcute]:", darkGreen], " V\[YAcute]sledn\[EAcute] s\[UAcute]radnice"];
                
                
        Print[Style["\nP\[OHat]vodn\[EAcute] body (modr\[AAcute]):", Bold]];
        Table[
            Print[Row[{Style["Bod " <> FromCharacterCode[64 + i] <> ": ", 
                RGBColor[0.1, 0.1, 1]], normalizedVertices[[i]]}]], {i, 2}];
                
        Print[Style["\nV\[YAcute]sledn\[EAcute] body (\[CHacek]erven\[AAcute]):", Bold]];
        Table[
            Print[Row[{Style["Bod " <> FromCharacterCode[64 + i] <> "': ", 
                RGBColor[1, 0.1, 0.1]], outputVertices[[i]]}]], {i, 2}];
                

        Print[Style["\nInverzn\[AAcute] transform\[AAcute]cia:", Bold]];
		Print["Pre vr\[AAcute]tenie do p\[OHat]vodn\[EAcute]ho stavu pou\[ZHacek]ite vektor posunu:"];
		Print["\[CapitalDelta]' = [", invDx, ", ", invDy, "] (opa\[CHacek]n\[YAcute] vektor)"];
		
		
		Print[Style["\nMATEMATICK\[CapitalEAcute] VLASTNOSTI:", Bold, 14]];
		Print["\[Bullet] Posun zachov\[AAcute]va tvar a ve\:013ekos\[THacek] geometrick\[YAcute]ch \[UAcute]tvarov"];
		Print["\[Bullet] Posun zachov\[AAcute]va vzdialenosti medzi bodmi - je to izometria"];
		Print["\[Bullet] Posun zachov\[AAcute]va rovnobe\[ZHacek]nos\[THacek] priamok"];
		Print["\[Bullet] Posun zachov\[AAcute]va uhly medzi priamkami"];
		Print["\[Bullet] Posun zachov\[AAcute]va d\:013a\[ZHacek]ku \[UAcute]se\[CHacek]ky"];
		Print["\[Bullet] Na rozdiel od rot\[AAcute]cie, posun nem\[AAcute] \[ZHacek]iadny fixn\[YAcute] bod (bod, ktor\[YAcute] zost\[AAcute]va na rovnakom mieste)"];
		        
		
		outputVertices
    ];


UseckaPosun::invalidInput = "Neplatn\[YAcute] vstup. O\[CHacek]ak\[AAcute]va sa matica alebo zoznam s\[UAcute]radn\[IAcute]c \[UAcute]se\[CHacek]ky.";
UseckaPosun::insufficientVertices = "Nedostato\[CHacek]n\[YAcute] po\[CHacek]et bodov. S\[UAcute] potrebn\[EAcute] aspo\[NHacek] 2 body.";

End[];
EndPackage[];
