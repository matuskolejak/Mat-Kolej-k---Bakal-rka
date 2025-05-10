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




BeginPackage["DomcekHardBalik`Transforms`Rotacia`", {"DomcekHardBalik`"}];


DomcekRotacia::usage = 
    "DomcekRotacia[vertices_] zobrazi interaktivny tutorial pre rotaciu domceka okolo pociatku a vrati nove vrcholy.";


RotaciaUhol::usage = "RotaciaUhol je globalna premenna, ktora sa pouziva na ulozenie uhla rotacie v stupnoch.";

Begin["`Private`"];


RotaciaUhol = 0;

DomcekRotacia[inputVertices_] := 
    Module[{normalizedVertices, angle, popis, outputVertices, cosA, sinA, invAngle, cosInvA, sinInvA,
            popisUhla, complexAngles, finalVertices = None, transformMatrix, darkGreen = RGBColor[0, 0.5, 0]},
            
        
        normalizedVertices = If[Length[inputVertices] >= 5, 
                               inputVertices[[1;;5]], 
                               Message[DomcekRotacia::insufficientVertices]; Abort[]];
        
        
        If[RotaciaUhol != 0,
            angle = RotaciaUhol,
            
            complexAngles = {30, 45, 60, 90, 120, 135, 150, 180};
            angle = RandomChoice[complexAngles];
        ];
        
        
        popisUhla = If[angle > 0, 
            "v kladnom smere (proti smeru hodinovych ruciciek)", 
            "v zapornom smere (v smere hodinovych ruciciek)"];
        
        popis = "Rotacia o " <> ToString[Abs[angle]] <> "\[Degree] " <> popisUhla;
        cosA = GetExactTrigValue[angle, Cos];
        sinA = GetExactTrigValue[angle, Sin];
        
        
        outputVertices = Map[ExactRotatePoint[#, angle] &, normalizedVertices];
        
        
        RotaciaUhol = angle;
        
        
        transformMatrix = {{cosA, -sinA, 0}, {sinA, cosA, 0}, {0, 0, 1}};
        
        
        invAngle = -angle;
        cosInvA = GetExactTrigValue[invAngle, Cos];
        sinInvA = GetExactTrigValue[invAngle, Sin];
        
        
        Print[Style[
            "GEOMETRICKE TRANSFORMACIE DOMCEKA - ROTACIA", 
            Bold, 16]];
        Print[Style["========================================", Bold]];
        
        
        Print[Style["\nZADANIE:", Bold, 14]];
        Print["Majme domcek ABCDE s vrcholmi:"];
        Print[MatrixForm[Map[Style[#, Blue] &, normalizedVertices, {2}]]];
        
        Print["\nVykonajte rotaciu domceka v 2D priestore okolo pociatku suradnicovej sustavy [0,0] o uhol \[Alpha] = ", 
            Style[angle, Red], "\[Degree]."];
            
        Print["\nTransformacna matica rotacie v homogennych suradniciach:"];
        Print[MatrixForm[{
            {Style[cosA, Red], Style[-sinA, Red], Style[0, Red]}, 
            {Style[sinA, Red], Style[cosA, Red], Style[0, Red]},
            {Style[0, Red], Style[0, Red], Style[1, Red]}
        }]];
        
        
        Print[Style["\nTEORIA MATICOVEHO ZOBRAZENIA ROTACIE:", Bold, 14]];
        Print["Pri rotacii v 2D priestore pouzivame homogenne suradnice, ktore umoznuju vyjadrit rotaciu ako maticove nasobenie:"];
        
        
        Print["\nTransformacna matica rotacie:"];
        Print[MatrixForm[{
            {Style["cos(\[Alpha])", Red], Style["-sin(\[Alpha])", Red], Style[0, Red]}, 
            {Style["sin(\[Alpha])", Red], Style["cos(\[Alpha])", Red], Style[0, Red]},
            {Style[0, Red], Style[0, Red], Style[1, Red]}
        }]];
        
        
        Print["\nAk je bod P = [x, y] v zakladnych suradniciach, v homogennych suradniciach ho zapiseme ako P = [x, y, 1].\nRotaciu potom mozeme zapisat ako maticove nasobenie:"];
        
        Print[Row[{
            MatrixForm[{
                {Style["cos(\[Alpha])", Red], Style["-sin(\[Alpha])", Red], Style[0, Red]}, 
                {Style["sin(\[Alpha])", Red], Style["cos(\[Alpha])", Red], Style[0, Red]},
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
                {Style["x\[CenterDot]cos(\[Alpha]) - y\[CenterDot]sin(\[Alpha])", darkGreen]}, 
                {Style["x\[CenterDot]sin(\[Alpha]) + y\[CenterDot]cos(\[Alpha])", darkGreen]},
                {Style[1, darkGreen]}
            }]
        }]];
        
        Print["\nPre nasu hodnotu \[Alpha] = ", angle, "\[Degree]:"];
        Print["cos(\[Alpha]) = ", FormatExactValue[cosA]];
        Print["sin(\[Alpha]) = ", FormatExactValue[sinA]];
        
        Print[Style["\nVYPOCET ROTACIE JEDNOTLIVYCH VRCHOLOV:", Bold, 14]];
        
        
        Table[
            Module[{x = normalizedVertices[[i, 1]], 
                   y = normalizedVertices[[i, 2]],
                   homogeneousVector, resultVector},
                   
                Print[Style["\nVRCHOL " <> FromCharacterCode[64 + i] <> ":", Bold, 14]];
                Print["Povodne suradnice: [", Style[x, Blue], ", ", Style[y, Blue], "]"];
                Print["Homogenne suradnice: [", Style[x, Blue], ", ", Style[y, Blue], ", ", Style[1, Blue], "]"];
                
                
                homogeneousVector = {x, y, 1};
                resultVector = {x*cosA - y*sinA, x*sinA + y*cosA, 1};
                
                
                DisplayIntuitiveMathMatrixMultiplication[
                    transformMatrix, 
                    homogeneousVector, 
                    resultVector,
                    FromCharacterCode[64 + i]
                ];
                
                
                Print[Style["\nAlternativny vypocet pomocou zakladnych vzorcov rotacie:", Bold]];
                Print["x' = x\[CenterDot]cos(\[Alpha]) - y\[CenterDot]sin(\[Alpha])  =>  ", Style[x, Blue], " \[CenterDot] ", Style[FormatExactValue[cosA], Red], " - ", Style[y, Blue], " \[CenterDot] ", Style[FormatExactValue[sinA], Red], " = ", Style[FormatExactValue[outputVertices[[i, 1]]], darkGreen]];
                Print["y' = x\[CenterDot]sin(\[Alpha]) + y\[CenterDot]cos(\[Alpha])  =>  ", Style[x, Blue], " \[CenterDot] ", Style[FormatExactValue[sinA], Red], " + ", Style[y, Blue], " \[CenterDot] ", Style[FormatExactValue[cosA], Red], " = ", Style[FormatExactValue[outputVertices[[i, 2]]], darkGreen]];
                
                Print[Style["\nVysledne suradnice vrcholu " <> FromCharacterCode[64 + i] <> "':", Bold]];
                Print["[", Style[FormatExactValue[outputVertices[[i, 1]]], darkGreen], ", ", Style[FormatExactValue[outputVertices[[i, 2]]], darkGreen], "]"];
                
                Print[Style["--------------------------------------------------", Bold]];
            ],
            {i, 5}
        ];
        
        
        Print[Style["\nVYSLEDOK TRANSFORMACIE CELEHO DOMCEKA:", Bold, 14]];
        Print[Style["Povodny domcek ABCDE:", Blue, Bold]];
        Print[MatrixForm[normalizedVertices]];
        
        Print[Style["Rotovany domcek A'B'C'D'E':", Red, Bold]];
        Print[MatrixForm[outputVertices]];
        
        
        Print[Style["\nVIZUALIZACIA TRANSFORMACIE:", Bold]];
        Print[CreateVisualization[normalizedVertices, outputVertices, angle]];
        
        
        Print[Style["\nLEGENDA:", Bold, 14]];
        Print[Style["\[Bullet] Modre", Blue], " body a ciary: Povodny domcek"];
        Print[Style["\[Bullet] Cervene", Red], " body a ciary: Rotovany domcek"];
        Print[Style["\[Bullet] Zelene", darkGreen], " prerusovane ciary: Spojnice bodov s pociatkom"];
        Print[Style["\[Bullet] O: Pociatok suradnicovej sustavy - stred rotacie", Black]];
        
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
        Print["Pre vratenie do povodneho stavu pouzite rotaciu o opacny uhol:"];
        Print["\[Alpha]' = -\[Alpha] = ", invAngle, "\[Degree]"];
        
        
        Print[Style["\nMATEMATICKE VLASTNOSTI:", Bold, 14]];
        Print["\[Bullet] Rotacia okolo pociatku zachovava vzdialenost vsetkych bodov od pociatku sustavy [0,0]"];
        Print["\[Bullet] Rotacia nemeni velkost ani tvar geometrickych utvarov - je to izometria"];
        Print["\[Bullet] Rotacia zachovava uhly medzi useckami a ich dlzky"];
        Print["\[Bullet] Rotacia zachovava obsah a obvod domceka"];
        Print["\[Bullet] Pociatok suradnicovej sustavy [0,0] je fixny bod rotacie - zostava nezmeneny"];
        
        
        outputVertices
    ];
    

DomcekRotaciaNoDisplayWithParams[vertices_] := 
    Module[{angle, result},
        
        If[RotaciaUhol != 0,
            
            angle = RotaciaUhol,
            
            angle = RandomChoice[{30, 45, 60, 90, 120, 135, 180, 270}]
        ];
        
        
        RotaciaUhol = angle;
        
        
        result = Map[
            ExactRotatePoint[#, angle] &,
            vertices
        ];
        
        
        {angle, result}
    ];
    

DisplayIntuitiveMathMatrixMultiplication[matrix_, vector_, result_, pointName_] := 
    Module[{darkGreen = RGBColor[0, 0.5, 0]},
        
        Print[Style["Matricovy zapis transformacie bodu " <> pointName <> ":", Bold]];
        
        
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
              ") + (", Style[matrix[[1, 3]], Red], " \[CenterDot] ", Style[vector[[3]], Blue], ")"];
        
        
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
              ") + (", Style[matrix[[2, 3]], Red], " \[CenterDot] ", Style[vector[[3]], Blue], ")"];
        
        
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
              ") + (", Style[matrix[[3, 3]], Red], " \[CenterDot] ", Style[vector[[3]], Blue], ")"];
        
        
        Print["   = ", Style[matrix[[3, 1]]*vector[[1]], Purple], 
              " + ", Style[matrix[[3, 2]]*vector[[2]], Purple],
              " + ", Style[matrix[[3, 3]]*vector[[3]], Purple]];
        
        
        Print["   = ", Style[result[[3]], darkGreen]];
        
        
        Print[Style["\nVysledna homogenna reprezentacia bodu " <> pointName <> "':", Bold]];
        Print["[", Style[result[[1]], darkGreen], ", ", Style[result[[2]], darkGreen], ", ", Style[result[[3]], darkGreen], "]"];
        
        
        Print[Style["\nKartezske suradnice bodu " <> pointName <> "':", Bold]];
        Print["[", Style[result[[1]], darkGreen], ", ", Style[result[[2]], darkGreen], "]"];
    ];


NormalizeVertices[vertices_] := 
    Module[{processedVertices},
        
        processedVertices = Which[
            
            MatrixQ[vertices], vertices,
            
            
            ListQ[vertices] && Length[Dimensions[vertices]] == 2, 
                If[Length[DeleteDuplicates[Length /@ vertices]] > 1,
                    
                    Block[{cleanedVertices = Select[vertices, VectorQ]},
                        If[Length[cleanedVertices] >= 5,
                            cleanedVertices[[1;;5]],
                            Message[DomcekRotacia::invalidInput]; 
                            Abort[]
                        ]
                    ],
                    
                    vertices
                ],
            
            
            ListQ[vertices] && Length[Dimensions[vertices]] == 1 && EvenQ[Length[vertices]], 
                Partition[vertices, 2],
                
            
            True, 
                Message[DomcekRotacia::invalidInput]; 
                Abort[]
        ];
        
        
        If[Length[processedVertices] < 5,
            Message[DomcekRotacia::insufficientVertices];
            Abort[]
        ];
        
        
        If[Max[Abs[N[processedVertices[[All, 1]]]]] > 10 || Max[Abs[N[processedVertices[[All, 2]]]]] > 10,
            processedVertices = processedVertices / (Max[Abs[N[Flatten[processedVertices]]]] / 6);
        ];
        
        
        
        
        processedVertices[[1;;5]]
    ];


NormalizationAngle[angle_] := 
    Module[{normAngle},
        normAngle = Mod[angle, 360];
        If[normAngle > 180, normAngle = normAngle - 360];
        normAngle
    ];


GetExactTrigValue[angle_, func_] := 
    Module[{exactValue},
        exactValue = Switch[{angle, func},
            
            {0, Cos}, 1,
            {0, Sin}, 0,
            {30, Cos}, Sqrt[3]/2,
            {30, Sin}, 1/2,
            {45, Cos}, 1/Sqrt[2],
            {45, Sin}, 1/Sqrt[2],
            {60, Cos}, 1/2,
            {60, Sin}, Sqrt[3]/2,
            {90, Cos}, 0,
            {90, Sin}, 1,
            {120, Cos}, -1/2,
            {120, Sin}, Sqrt[3]/2,
            {135, Cos}, -1/Sqrt[2],
            {135, Sin}, 1/Sqrt[2],
            {150, Cos}, -Sqrt[3]/2,
            {150, Sin}, 1/2,
            {180, Cos}, -1,
            {180, Sin}, 0,
            {210, Cos}, -Sqrt[3]/2,
            {210, Sin}, -1/2,
            {225, Cos}, -1/Sqrt[2],
            {225, Sin}, -1/Sqrt[2],
            {240, Cos}, -1/2,
            {240, Sin}, -Sqrt[3]/2,
            {270, Cos}, 0,
            {270, Sin}, -1,
            {300, Cos}, 1/2,
            {300, Sin}, -Sqrt[3]/2,
            {315, Cos}, 1/Sqrt[2],
            {315, Sin}, -1/Sqrt[2],
            {330, Cos}, Sqrt[3]/2,
            {330, Sin}, -1/2,
            {360, Cos}, 1,
            {360, Sin}, 0,
            
            
            {-30, Cos}, Sqrt[3]/2,
            {-30, Sin}, -1/2,
            {-45, Cos}, 1/Sqrt[2],
            {-45, Sin}, -1/Sqrt[2],
            {-60, Cos}, 1/2,
            {-60, Sin}, -Sqrt[3]/2,
            {-90, Cos}, 0,
            {-90, Sin}, -1,
            {-120, Cos}, -1/2,
            {-120, Sin}, -Sqrt[3]/2,
            {-135, Cos}, -1/Sqrt[2],
            {-135, Sin}, -1/Sqrt[2],
            {-150, Cos}, -Sqrt[3]/2,
            {-150, Sin}, -1/2,
            {-180, Cos}, -1,
            {-180, Sin}, 0,
            {-210, Cos}, -Sqrt[3]/2,
            {-210, Sin}, 1/2,
            {-225, Cos}, -1/Sqrt[2],
            {-225, Sin}, 1/Sqrt[2],
            {-240, Cos}, -1/2,
            {-240, Sin}, Sqrt[3]/2,
            {-270, Cos}, 0,
            {-270, Sin}, 1,
            {-300, Cos}, 1/2,
            {-300, Sin}, Sqrt[3]/2,
            {-315, Cos}, 1/Sqrt[2],
            {-315, Sin}, 1/Sqrt[2],
            {-330, Cos}, Sqrt[3]/2,
            {-330, Sin}, 1/2,
            {-360, Cos}, 1,
            {-360, Sin}, 0,
            
            _, If[func === Cos, Cos[angle*Pi/180], Sin[angle*Pi/180]]
        ];
        exactValue
    ];
    

ExactRotatePoint[point_, angle_] := 
    Module[{x, y, cosA, sinA, newX, newY},
        x = point[[1]];
        y = point[[2]];
        cosA = GetExactTrigValue[angle, Cos];
        sinA = GetExactTrigValue[angle, Sin];
        
        newX = x*cosA - y*sinA;
        newY = x*sinA + y*cosA;
        
        
        newX = Simplify[newX];
        newY = Simplify[newY];
        
        {newX, newY}
    ];


DetectRotationAngle[originalVertices_, finalVertices_] := 
    Module[{validAngles = {}, angle, cosA, sinA, foundAngle = False, i = 1, point, rotatedPoint},
        
        
        angles = {30, 45, 60, 90, 120, 135, 150, 180, 210, 225, 240, 270, 300, 315, 330,
                 -30, -45, -60, -90, -120, -135, -150, -180, -210, -225, -240, -270, -300, -315, -330};
        
        
        While[i <= 5 && !foundAngle,
            point = originalVertices[[i]];
            rotatedPoint = finalVertices[[i]];
            
            
            If[point[[1]] != 0 || point[[2]] != 0,
                Do[
                    temp = ExactRotatePoint[point, a];
                    
                    If[Simplify[temp[[1]] - rotatedPoint[[1]]] == 0 && Simplify[temp[[2]] - rotatedPoint[[2]]] == 0,
                        AppendTo[validAngles, a];
                    ],
                    {a, angles}
                ];
                
                
                If[Length[validAngles] > 0,
                    foundAngle = True;
                ];
            ];
            
            i++;
        ];
        
        
        If[foundAngle, First[validAngles], None]
    ];


FormatExactValue[expr_] :=
    Module[{strExpr},
        
        strExpr = ToString[expr, InputForm];
        
        
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


CreateVisualization[originalVertices_, finalVertices_, angle_] := 
    Module[{allVertices, xMin, xMax, yMin, yMax, rangeBuffer, xRange, yRange, 
            labelOffsets, angleDeg = angle, angleRad = angle*Pi/180, brightGreen},
        
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
            
            
            brightGreen, Thick, Opacity[0.7],
            If[angleDeg > 0,
                
                {Thickness[0.005], Circle[{0, 0}, 1.2, {0, angleRad}],
                Opacity[0.15], EdgeForm[], Disk[{0, 0}, 1.2, {0, angleRad}]},
                
                {Thickness[0.005], Circle[{0, 0}, 1.2, {angleRad, 0}], 
                Opacity[0.15], EdgeForm[], Disk[{0, 0}, 1.2, {angleRad, 0}]}
            ],
            
            
            brightGreen, Dashed,
            Table[
                Line[{{0, 0}, originalVertices[[i]]}],
                {i, Length[originalVertices]}
            ],
            brightGreen, Dashed,
            Table[
                Line[{{0, 0}, finalVertices[[i]]}],
                {i, Length[finalVertices]}
            ],
            
            
            White, 
            Disk[{0, 0}, 0.45], 
            Table[
                Disk[originalVertices[[i]], 0.45],
                {i, 5}
            ],
            Table[
                Disk[finalVertices[[i]], 0.45],
                {i, 5}
            ],
            
            
            Black, PointSize[0.025], Point[{0, 0}], 
            Text[Style["O", Black, Bold, 14], {-0.5, -0.5}], 
            
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
            
            
            brightGreen, Opacity[1],
            Text[Style[ToString[Abs[angleDeg]] <> "\[Degree]", FontSize -> 16, Bold, Background -> White, 
                 brightGreen], 1.7*{Cos[angleRad/2], Sin[angleRad/2]}],
            
            
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


DomcekRotacia::invalidInput = "Neplatny vstup. Ocakava sa matica alebo zoznam suradnic domceka.";
DomcekRotacia::insufficientVertices = "Nedostatocny pocet vrcholov. Su potrebne aspon 5 vrcholov.";

End[];
EndPackage[];






