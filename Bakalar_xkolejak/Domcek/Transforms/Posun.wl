(* ::Package:: *)

(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)



BeginPackage["DomcekHardBalik`Transforms`Posun`", {"DomcekHardBalik`"}];

DomcekPosunVector::usage = "DomcekPosunVector je globalny vektor posunu [dx, dy], ktory sa pouziva v DomcekPosun funkcii.";

DomcekPosun::usage = 
    "DomcekPosun[vertices_] zobrazi interaktivny tutorial pre posun domceka a vrati nove vrcholy.";

Begin["`Private`"];

DomcekPosunVector = {0, 0};

NormalizeVertices[vertices_] := 
    Module[{processedVertices},
        
        processedVertices = Which[
            
            MatrixQ[vertices], vertices,
            
            
            ListQ[vertices] && Length[Dimensions[vertices]] == 2, 
                If[Length[DeleteDuplicates[Length /@ vertices]] > 1,
                    
                    Block[{cleanedVertices = Select[vertices, VectorQ]},
                        If[Length[cleanedVertices] >= 5,
                            cleanedVertices[[1;;5]],
                            Message[DomcekPosun::invalidInput]; 
                            Abort[]
                        ]
                    ],
                    
                    vertices
                ],
            
            
            ListQ[vertices] && Length[Dimensions[vertices]] == 1 && EvenQ[Length[vertices]], 
                Partition[vertices, 2],
                
            
            True, 
                Message[DomcekPosun::invalidInput]; 
                Abort[]
        ];
        
        If[Length[processedVertices] < 5,
            Message[DomcekPosun::insufficientVertices];
            Abort[]
        ];
        
        If[Max[Abs[processedVertices[[All, 1]]]] > 10 || Max[Abs[processedVertices[[All, 2]]]] > 10,
            
            processedVertices = processedVertices / (Max[Abs[Flatten[processedVertices]]] / 6);
        ];
        
        
        processedVertices[[1;;5]]
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
        
        
        DomcekPosunVector = {dx, dy};
        
        
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
            Line[Append[originalVertices, First[originalVertices]]],
            
            
            Red, Thick, Opacity[0.7],
            Line[Append[finalVertices, First[finalVertices]]],
            
            
            Green, Dashed,
            Table[
                Line[{originalVertices[[i]], finalVertices[[i]]}],
                {i, Length[originalVertices]}
            ],
            
            
            White, 
            Table[
                Disk[originalVertices[[i]], 0.45],
                {i, Length[originalVertices]}
            ],
            Table[
                Disk[finalVertices[[i]], 0.45],
                {i, Length[finalVertices]}
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
                {i, Length[originalVertices]}
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
        
        Print[Style["Maticovy zapis transformacie bodu " <> pointName <> ":", Bold]];
        
        
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
        
        
        Print[Style["Podrobny vypocet jednotlivych suradnic:", Bold]];
        
        
        Print[Style["\[Bullet] Vypocet novej x-ovej suradnice (1. riadok matice):", Brown]];
        
        
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
        
        
        Print[Style["\n\[Bullet] Vypocet novej y-ovej suradnice (2. riadok matice):", Brown]];
        
        
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
        
        
        Print[Style["\n\[Bullet] Vypocet homogennej suradnice (3. riadok matice):", Brown]];
        
        
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
        
        
        Print[Style["\nVysledna homogenna reprezentacia bodu " <> pointName <> "':", Bold]];
        Print["[", Style[result[[1]], darkGreen], ", ", Style[result[[2]], darkGreen], ", ", Style[result[[3]], darkGreen], "]"];
        
        
        Print[Style["\nKartezske suradnice bodu " <> pointName <> "':", Bold]];
        Print["[", Style[result[[1]], darkGreen], ", ", Style[result[[2]], darkGreen], "]"];
    ];

DomcekPosun[inputVertices_] := 
    Module[{normalizedVertices, outputVertices, dx, dy, invDx, invDy,
            transformMatrix, homogeneousVertices, resultVertices, darkGreen = RGBColor[0, 0.5, 0]},
            
        
        normalizedVertices = NormalizeVertices[inputVertices];
        
        
        If[DomcekPosunVector =!= {0, 0},
            
            {dx, dy} = DomcekPosunVector;
            
            DomcekPosunVector = {0, 0};
        ,
            
            {dx, dy, _, outputVertices} = GetTranslationParameters[normalizedVertices];
        ];
        
        
        outputVertices = Map[{#[[1]] + dx, #[[2]] + dy} &, normalizedVertices];
        
        
        invDx = -dx;
        invDy = -dy;
        
        
        transformMatrix = {{1, 0, dx}, {0, 1, dy}, {0, 0, 1}};
        
        
        Print[Style[
            "GEOMETRICKE TRANSFORMACIE DOMCEKA - POSUN", 
            Bold, 16]];
        Print[Style["========================================", Bold]];
        
        
        Print[Style["\nZADANIE:", Bold, 14]];
        Print["Majme domcek ABCDE s vrcholmi:"];
        Print[MatrixForm[Map[Style[#, Blue] &, normalizedVertices, {2}]]];
        
        Print["\nVykonajte posun domceka v 2D priestore pomocou vektora posunu: \[CapitalDelta] = [", 
            Style[dx, Red], ", ", Style[dy, Red], "]"];
            
        Print["\nTransformacna matica posunu v homogennych suradniciach:"];
        Print[MatrixForm[{
            {Style[1, Red], Style[0, Red], Style[dx, Red]}, 
            {Style[0, Red], Style[1, Red], Style[dy, Red]},
            {Style[0, Red], Style[0, Red], Style[1, Red]}
        }]];
        
        
        Print[Style["\nTEORIA MATICOVEHO ZOBRAZENIA POSUNU:", Bold, 14]];
        Print["Pri posune v 2D priestore pouzivame homogenne suradnice, ktore umoznuju vyjadrit posun ako maticove nasobenie:"];
        
        
        Print["\nTransformacna matica posunu:"];
        Print[MatrixForm[{
            {Style[1, Red], Style[0, Red], Style["dx", Red]}, 
            {Style[0, Red], Style[1, Red], Style["dy", Red]},
            {Style[0, Red], Style[0, Red], Style[1, Red]}
        }]];
        
        
        Print["\nAk je bod P = [x, y] v zakladnych suradniciach, v homogennych suradniciach ho zapiseme ako P = [x, y, 1].\nPosun potom mozeme zapisat ako maticove nasobenie:"];
        
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
        
        Print["\nPre nase hodnoty posunu:"];
        Print["dx = ", Style[dx, Red], " (posun v smere osi x)"];
        Print["dy = ", Style[dy, Red], " (posun v smere osi y)"];
        
        Print[Style["\nVYPOCET POSUNU JEDNOTLIVYCH VRCHOLOV:", Bold, 14]];
        
        
        Table[
            Module[{x = normalizedVertices[[i, 1]], 
                   y = normalizedVertices[[i, 2]],
                   homogeneousVector, resultVector},
                   
                Print[Style["\nVRCHOL " <> FromCharacterCode[64 + i] <> ":", Bold, 14]];
                Print["Povodne suradnice: [", Style[x, Blue], ", ", Style[y, Blue], "]"];
                Print["Homogenne suradnice: [", Style[x, Blue], ", ", Style[y, Blue], ", ", Style[1, Blue], "]"];
                
                
                homogeneousVector = {x, y, 1};
                resultVector = {x + dx, y + dy, 1};
                
                
                DisplayIntuitiveMathMatrixMultiplication[
                    transformMatrix, 
                    homogeneousVector, 
                    resultVector,
                    FromCharacterCode[64 + i]
                ];
                
                
                Print[Style["\nAlternativny vypocet pomocou zakladnych vzorcov posunu:", Bold]];
                Print["x' = x + dx  =>  ", Style[x, Blue], " + ", Style[dx, Red], " = ", Style[x + dx, darkGreen]];
                Print["y' = y + dy  =>  ", Style[y, Blue], " + ", Style[dy, Red], " = ", Style[y + dy, darkGreen]];
                
                Print[Style["\nVysledne suradnice vrcholu " <> FromCharacterCode[64 + i] <> "':", Bold]];
                Print["[", Style[outputVertices[[i, 1]], darkGreen], ", ", Style[outputVertices[[i, 2]], darkGreen], "]"];
                
                Print[Style["--------------------------------------------------", Bold]];
            ],
            {i, 5}
        ];
        
        
        Print[Style["\nVYSLEDOK TRANSFORMACIE CELEHO DOMCEKA:", Bold, 14]];
        Print[Style["Povodny domcek ABCDE:", Blue, Bold]];
        Print[MatrixForm[normalizedVertices]];
        
        Print[Style["Posunuty domcek A'B'C'D'E':", Red, Bold]];
        Print[MatrixForm[outputVertices]];
        
        
        Print[Style["\nVIZUALIZACIA TRANSFORMACIE:", Bold]];
        Print[CreateVisualization[normalizedVertices, outputVertices, {dx, dy}]];
        
        
        Print[Style["\nLEGENDA:", Bold, 14]];
        Print[Style["\[Bullet] Modre", Blue], " body a ciary: Povodny domcek"];
        Print[Style["\[Bullet] Cervene", Red], " body a ciary: Posunuty domcek"];
        Print[Style["\[Bullet] Zelene", darkGreen], " prerusovane ciary: Posun jednotlivych vrcholov"];
        
        Print[Style["\nFAREBNE OZNACENIA V MATRICOVYCH VYPOCTOCH:", Bold]];
        Print[Style["\[Bullet] Cervena:", Red], " Transformacna matica"];
        Print[Style["\[Bullet] Modra:", Blue], " Vstupne suradnice"];
        Print[Style["\[Bullet] Fialova:", Purple], " Medzivypocty"];
        Print[Style["\[Bullet] Zelena:", darkGreen], " Vysledne suradnice"];
                
                
        Print[Style["\nPovodne vrcholy (modra):", Bold]];
        Table[
            Print[Row[{Style["Vrchol " <> FromCharacterCode[64 + i] <> ": ", 
                RGBColor[0.1, 0.1, 1]], normalizedVertices[[i]]}]], {i, 5}];
                
        Print[Style["\nVysledne vrcholy (cervena):", Bold]];
        Table[
            Print[Row[{Style["Vrchol " <> FromCharacterCode[64 + i] <> "': ", 
                RGBColor[1, 0.1, 0.1]], outputVertices[[i]]}]], {i, 5}];
                

        Print[Style["\nInverzna transformacia:", Bold]];
        Print["Pre vratenie do povodneho stavu pouzite vektor posunu:"];
        Print["\[CapitalDelta]' = [", invDx, ", ", invDy, "] (opacny vektor)"];
        
        
        Print[Style["\nMATEMATICKE VLASTNOSTI:", Bold, 14]];
        Print["\[Bullet] Posun zachovava tvar a velkost geometrickych utvarov"];
        Print["\[Bullet] Posun zachovava vzdialenosti medzi bodmi - je to izometria"];
        Print["\[Bullet] Posun zachovava rovnobeznost priamok"];
        Print["\[Bullet] Posun zachovava uhly medzi priamkami"];
        Print["\[Bullet] Posun zachovava obsah a obvod domceka"];
        Print["\[Bullet] Na rozdiel od rotacie, posun nema ziadny fixny bod (bod, ktory zostava na rovnakom mieste)"];
                
        
        outputVertices
        
    ];


DomcekPosun::invalidInput = "Neplatny vstup. Ocakava sa matica alebo zoznam suradnic domceka.";
DomcekPosun::insufficientVertices = "Nedostatocny pocet vrcholov. Su potrebne aspon 5 vrcholov.";

End[];
EndPackage[];




