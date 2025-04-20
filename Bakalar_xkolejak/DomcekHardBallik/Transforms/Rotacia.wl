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


BeginPackage["DomcekHardBalik`Transforms`Rotacia`", {"DomcekHardBalik`"}];

(* Export public symbols *)
DomcekRotacia::usage = 
    "DomcekRotacia[vertices_] zobraz\[IAcute] interakt\[IAcute]vny tutori\[AAcute]l pre rot\[AAcute]ciu dom\[CHacek]eka okolo po\[CHacek]iatku a vr\[AAcute]ti nov\[EAcute] vrcholy.";

Begin["`Private`"];

(* Modified NormalizeVertices function to preserve exact values without any rounding *)
NormalizeVertices[vertices_] := 
    Module[{processedVertices},
        (* Process different input formats *)
        processedVertices = Which[
            (* If it's a matrix *)
            MatrixQ[vertices], vertices,
            
            (* If it's a list of vectors *)
            ListQ[vertices] && Length[Dimensions[vertices]] == 2, 
                If[Length[DeleteDuplicates[Length /@ vertices]] > 1,
                    (* If vectors have different lengths *)
                    Block[{cleanedVertices = Select[vertices, VectorQ]},
                        If[Length[cleanedVertices] >= 5,
                            cleanedVertices[[1;;5]],
                            Message[DomcekRotacia::invalidInput]; 
                            Abort[]
                        ]
                    ],
                    (* If vectors have same length *)
                    vertices
                ],
            
            (* If it's a one-dimensional vector, try to partition it *)
            ListQ[vertices] && Length[Dimensions[vertices]] == 1 && EvenQ[Length[vertices]], 
                Partition[vertices, 2],
                
            (* Otherwise *)
            True, 
                Message[DomcekRotacia::invalidInput]; 
                Abort[]
        ];
        
        (* Check number of vertices *)
        If[Length[processedVertices] < 5,
            Message[DomcekRotacia::insufficientVertices];
            Abort[]
        ];
        
        (* Scale vertices if they are too far, but preserve exact values *)
        If[Max[Abs[N[processedVertices[[All, 1]]]]] > 10 || Max[Abs[N[processedVertices[[All, 2]]]]] > 10,
            processedVertices = processedVertices / (Max[Abs[N[Flatten[processedVertices]]]] / 6);
        ];
        
        (* DO NOT round or simplify - preserve exact input values *)
        
        (* Return the first five vertices *)
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

(* Funkcia na presn\[YAcute] v\[YAcute]po\[CHacek]et rot\[AAcute]cie bodu *)
ExactRotatePoint[point_, angle_] := 
    Module[{x, y, cosA, sinA, newX, newY},
        x = point[[1]];
        y = point[[2]];
        cosA = GetExactTrigValue[angle, Cos];
        sinA = GetExactTrigValue[angle, Sin];
        
        newX = x*cosA - y*sinA;
        newY = x*sinA + y*cosA;
        
        (* Najprv plne rozvinieme v\[YAcute]raz a potom ho zjednodu\[SHacek]\[IAcute]me *)
        newX = Expand[newX];
        newY = Expand[newY];
        
        {newX, newY}
    ];

(* Pomocn\[AAcute] funkcia na detekciu vhodn\[YAcute]ch uhlov rot\[AAcute]cie z existuj\[UAcute]cich vrcholov *)
DetectRotationAngle[originalVertices_, finalVertices_] := 
    Module[{validAngles = {}, angle, cosA, sinA, foundAngle = False, i = 1, point, rotatedPoint},
        
        (* Zoznam uhlov na overenie *)
        angles = {30, 45, 60, 90, 120, 135, 150, 180, 210, 225, 240, 270, 300, 315, 330,
                 -30, -45, -60, -90, -120, -135, -150, -180, -210, -225, -240, -270, -300, -315, -330};
        
        (* H\:013ead\[AAcute]me uhol rot\[AAcute]cie porovn\[AAcute]van\[IAcute]m origin\[AAcute]lnych a rotovan\[YAcute]ch vrcholov *)
        While[i <= 5 && !foundAngle,
            point = originalVertices[[i]];
            rotatedPoint = finalVertices[[i]];
            
            (* Ak nie je bod v za\[CHacek]iatku, m\[OHat]\[ZHacek]eme zisti\[THacek] uhol *)
            If[point[[1]] != 0 || point[[2]] != 0,
                Do[
                    temp = ExactRotatePoint[point, a];
                    (* Porovnanie presn\[YAcute]ch hodn\[OHat]t *)
                    If[Simplify[temp[[1]] - rotatedPoint[[1]]] == 0 && Simplify[temp[[2]] - rotatedPoint[[2]]] == 0,
                        AppendTo[validAngles, a];
                    ],
                    {a, angles}
                ];
                
                (* Ak sme na\[SHacek]li uhol, ozna\[CHacek]\[IAcute]me to *)
                If[Length[validAngles] > 0,
                    foundAngle = True;
                ];
            ];
            
            i++;
        ];
        
        (* Ak sme na\[SHacek]li uhol, vr\[AAcute]time prv\[YAcute] n\[AAcute]jden\[YAcute], inak None *)
        If[foundAngle, First[validAngles], None]
    ];


FormatExactValue[expr_] :=
    Module[{strExpr},
        (* Konvertovanie na string *)
        strExpr = ToString[expr, InputForm];
        
        (* Vykonanie v\[SHacek]etk\[YAcute]ch nahraden\[IAcute] *)
        strExpr = StringReplace[strExpr, {
            "Sqrt[" ~~ n__ ~~ "]" :> "\[Sqrt]" <> n,
            "*" -> " \[CenterDot] ",
            "[" -> "",
            "]" -> ""
        }];
        
        strExpr
    ];

(* Funkcia na rozvinutie a zjednodu\[SHacek]enie v\[YAcute]razov pre v\[YAcute]po\[CHacek]ty *)
ExpandAndFormatExpression[expr_] :=
    Module[{expandedExpr, formattedExpr},
        (* Plne rozvinieme v\[YAcute]raz pre lep\[SHacek]ie zobrazenie krokov *)
        expandedExpr = Expand[expr];
        
        (* Form\[AAcute]tujeme rozvinut\[YAcute] v\[YAcute]raz *)
        formattedExpr = FormatExactValue[expandedExpr];
        
        formattedExpr
    ];

CreateVisualization[originalVertices_, finalVertices_, angle_] := 
    Module[{allVertices, xMin, xMax, yMin, yMax, rangeBuffer, xRange, yRange, 
            labelOffsets, angleDeg = angle, angleRad = angle*Pi/180, brightGreen,
            originalHouseLines, finalHouseLines},
        (* Defin\[IAcute]cia jasnej\[SHacek]ej zelenej farby pre uhol *)
        brightGreen = RGBColor[0, 0.8, 0.2];
        
        (* Spojenie v\[SHacek]etk\[YAcute]ch vrcholov pre v\[YAcute]po\[CHacek]et rozsahu *)
        allVertices = Join[originalVertices, finalVertices];
        
        (* V\[YAcute]po\[CHacek]et minim\[AAcute]lnych a maxim\[AAcute]lnych hodn\[OHat]t *)
        xMin = Min[N[allVertices[[All, 1]]]];
        xMax = Max[N[allVertices[[All, 1]]]];
        yMin = Min[N[allVertices[[All, 2]]]];
        yMax = Max[N[allVertices[[All, 2]]]];
        
        (* Buffer pre estetick\[YAcute] vzh\:013ead *)
        rangeBuffer = 2;
        
        (* Nastavenie rozsahu os\[IAcute] *)
        xRange = {Min[-11, xMin - rangeBuffer], Max[11, xMax + rangeBuffer]};
        yRange = {Min[-11, yMin - rangeBuffer], Max[11, yMax + rangeBuffer]};
        
        (* Definovanie spr\[AAcute]vneho poradia pre vykreslenie dom\[CHacek]eka *)
        (* Predpoklad\[AAcute]me, \[ZHacek]e body s\[UAcute] A=1(\:013eav\[YAcute] doln\[YAcute]), B=2(prav\[YAcute] doln\[YAcute]), C=3(prav\[YAcute] horn\[YAcute]), 
           D=4(\:013eav\[YAcute] horn\[YAcute]), E=5(vrchol strechy) *)
        originalHouseLines = {
            {originalVertices[[1]], originalVertices[[2]]},
            {originalVertices[[2]], originalVertices[[3]]},
            {originalVertices[[3]], originalVertices[[4]]},
            {originalVertices[[4]], originalVertices[[1]]},
            {originalVertices[[4]], originalVertices[[5]]},
            {originalVertices[[5]], originalVertices[[3]]}
        };
        
        finalHouseLines = {
            {finalVertices[[1]], finalVertices[[2]]},
            {finalVertices[[2]], finalVertices[[3]]},
            {finalVertices[[3]], finalVertices[[4]]},
            {finalVertices[[4]], finalVertices[[1]]},
            {finalVertices[[4]], finalVertices[[5]]},
            {finalVertices[[5]], finalVertices[[3]]}
        };
        
        (* Vypo\[CHacek]\[IAcute]ta\[THacek] offsety pre ozna\[CHacek]enia, aby sa neprekr\[YAcute]vali s \[CHacek]iarami *)
        labelOffsets = Table[
            Module[{originalVertex = originalVertices[[i]], 
                    finalVertex = finalVertices[[i]], 
                    offset = {0.7, 0.7}},
                
                (* Upravi\[THacek] offset pod\:013ea polohy vrcholov *)
                If[N[originalVertex[[1]]] > 8, offset[[1]] = -1.0];
                If[N[originalVertex[[2]]] > 8, offset[[2]] = -1.0];
                
                {offset, offset}
            ],
            {i, Length[originalVertices]}
        ];
        
        Graphics[{
            (* Grid *)
            LightGray, Thin,
            Table[Line[{{xRange[[1]], y}, {xRange[[2]], y}}], 
                {y, Ceiling[yRange[[1]]], Floor[yRange[[2]]], 2}],
            Table[Line[{{x, yRange[[1]]}, {x, yRange[[2]]}}], 
                {x, Ceiling[xRange[[1]]], Floor[xRange[[2]]], 2}],
            
            (* P\[OHat]vodn\[YAcute] dom\[CHacek]ek - modr\[YAcute] *)
            Blue, Thick, Opacity[0.7],
            Line /@ originalHouseLines,
            
            (* Rotate dom\[CHacek]ek - \[CHacek]erven\[YAcute] *)
            Red, Thick, Opacity[0.7],
            Line /@ finalHouseLines,
            
            (* Zobrazenie uhla rot\[AAcute]cie - jasn\[AAcute] zelen\[AAcute] s hrub\[SHacek]ou \[CHacek]iarou a vyplnen\[IAcute]m *)
            brightGreen, Thick, Opacity[0.7],
            If[angleDeg > 0,
                (* Kladn\[YAcute] uhol - proti smeru hodinov\[YAcute]ch ru\[CHacek]i\[CHacek]iek *)
                {Thickness[0.005], Circle[{0, 0}, 1.2, {0, angleRad}],
                Opacity[0.15], EdgeForm[], Disk[{0, 0}, 1.2, {0, angleRad}]},
                (* Z\[AAcute]porn\[YAcute] uhol - v smere hodinov\[YAcute]ch ru\[CHacek]i\[CHacek]iek *)
                {Thickness[0.005], Circle[{0, 0}, 1.2, {angleRad, 0}], 
                Opacity[0.15], EdgeForm[], Disk[{0, 0}, 1.2, {angleRad, 0}]}
            ],
            
            (* Pomocn\[EAcute] \[CHacek]iary - zelen\[EAcute] preru\[SHacek]ovan\[EAcute] *)
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
            
            (* Body s bielym pozadim - zv\[ADoubleDot]\[CHacek]\[SHacek]en\[EAcute] pre lep\[SHacek]iu vidite\:013enos\[THacek] *)
            White, 
            Disk[{0, 0}, 0.45], (* Zv\[YAcute]raznen\[YAcute] po\[CHacek]iatok *)
            Table[
                Disk[originalVertices[[i]], 0.45],
                {i, 5}
            ],
            Table[
                Disk[finalVertices[[i]], 0.45],
                {i, 5}
            ],
            
            
            (* Body - zv\[ADoubleDot]\[CHacek]\[SHacek]en\[EAcute] *)
            Black, PointSize[0.025], Point[{0, 0}], (* Po\[CHacek]iatok *)
            Text[Style["O", Black, Bold, 14], {-0.5, -0.5}], (* Ozna\[CHacek]enie po\[CHacek]iatku *)
            
            Blue, PointSize[0.025], Point[originalVertices],
            Red, PointSize[0.025], Point[finalVertices],
            
            (* Labels s optimalizovan\[YAcute]mi poz\[IAcute]ciami *)
            Table[
                {
                    Text[Style[FromCharacterCode[64 + i], Blue, Bold, 16], 
                        originalVertices[[i]] + labelOffsets[[i, 1]]],
                    Text[Style[FromCharacterCode[64 + i] <> "'", Red, Bold, 16], 
                        finalVertices[[i]] + labelOffsets[[i, 2]]]
                },
                {i, 5}
            ],
            
            (* Text s uhlom rot\[AAcute]cie - s lep\[SHacek]ou vidite\:013enos\[THacek]ou *)
            brightGreen, Opacity[1],
            Text[Style[ToString[Abs[angleDeg]] <> "\[Degree]", FontSize -> 16, Bold, Background -> White, 
                 brightGreen], 1.7*{Cos[angleRad/2], Sin[angleRad/2]}],
            
            (* Osi *)
            Black, Thick,
            Arrow[{{xRange[[1]], 0}, {xRange[[2]], 0}}],
            Arrow[{{0, yRange[[1]]}, {0, yRange[[2]]}}],
            Text[Style["x", Black, Bold], {xRange[[2]] - 0.5, -0.5}],
            Text[Style["y", Black, Bold], {-0.5, yRange[[2]] - 0.5}],
            
            (* \[CapitalCHacek]\[IAcute]sla na osiach *)
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

DomcekRotacia[inputVertices_] := 
    Module[{normalizedVertices, angle, popis, outputVertices, cosA, sinA, invAngle, cosInvA, sinInvA,
            popisUhla, complexAngles, detectInputOutput = False, finalVertices = None, exactOutputVertices},
            
        (* Display the initial vertices with expanded form *)
        normalizedVertices = If[Length[inputVertices] >= 5,
            Map[Function[point, 
                {Expand[point[[1]]], Expand[point[[2]]]}
            ], inputVertices[[1;;5]]],
            Message[DomcekRotacia::insufficientVertices];
            Abort[]
        ];
        
        (* Direct selection of an interesting angle without using GetRotationParameters *)
        complexAngles = {30, 45, 60, 120, 135, 150};
        angle = RandomChoice[complexAngles];
        
        popisUhla = If[angle > 0, 
            "v kladnom smere (proti smeru hodinov\[YAcute]ch ru\[CHacek]i\[CHacek]iek)", 
            "v z\[AAcute]pornom smere (v smere hodinov\[YAcute]ch ru\[CHacek]i\[CHacek]iek)"];
        
        popis = "Rot\[AAcute]cia o " <> ToString[Abs[angle]] <> "\[Degree] " <> popisUhla;
        cosA = GetExactTrigValue[angle, Cos];
        sinA = GetExactTrigValue[angle, Sin];
        
        (* Calculate output vertices - preserve exact values *)
        outputVertices = Map[ExactRotatePoint[#, angle] &, normalizedVertices];
        
        (* Kontrola transforma\[CHacek]nej matice - ak je st\[AAcute]le pr\[IAcute]li\[SHacek] jednoduch\[AAcute], vyberieme in\[YAcute] uhol *)
        If[cosA == 0 || cosA == 1 || cosA == -1 || sinA == 0 || sinA == 1 || sinA == -1,
            (* Vyn\[UAcute]time uhol 45\[Degree] alebo 60\[Degree] *)
            angle = RandomChoice[{45, 60}];
            cosA = GetExactTrigValue[angle, Cos];
            sinA = GetExactTrigValue[angle, Sin];
            
            (* Prepo\[CHacek]\[IAcute]tame v\[YAcute]stupn\[EAcute] vrcholy - zachov\[AAcute]vame presn\[EAcute] hodnoty *)
            outputVertices = Map[ExactRotatePoint[#, angle] &, normalizedVertices];
            
            (* Aktualizujeme popis *)
            popisUhla = "v kladnom smere (proti smeru hodinov\[YAcute]ch ru\[CHacek]i\[CHacek]iek)";
            popis = "Rot\[AAcute]cia o " <> ToString[angle] <> "\[Degree] " <> popisUhla;
        ];
        
        (* Inverzn\[YAcute] uhol *)
        invAngle = -angle;
        cosInvA = GetExactTrigValue[invAngle, Cos];
        sinInvA = GetExactTrigValue[invAngle, Sin];
        
        (* Nadpis *)
        Print[Style[
            "GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE DOM\[CapitalCHacek]EKA - ROT\[CapitalAAcute]CIA", 
            Bold, 16]];
        Print[Style["========================================", Bold]];
        
        (* ZADANIE *)
        Print[Style["\nZADANIE:", Bold, 14]];
        Print["Majme dom\[CHacek]ek s vrcholmi:"];
        
        (* Display vertices in expanded form *)
        Print[MatrixForm[Map[Style[#, Blue] &, normalizedVertices, {2}]]];
        
        Print["\nVykonajte rot\[AAcute]ciu dom\[CHacek]eka v 2D priestore okolo po\[CHacek]iatku s\[UAcute]radnicovej s\[UAcute]stavy [0,0] o uhol \[Alpha] = ", 
            Style[angle, Red], "\[Degree]."];
            
        Print["\nTransforma\[CHacek]n\[AAcute] matica rot\[AAcute]cie:"];
        Print[MatrixForm[{
            {Style[cosA, Red], Style[-sinA, Red]}, 
            {Style[sinA, Red], Style[cosA, Red]}
        }]];
        
        (* POSTUP *)
        Print[Style["\nPOSTUP:", Bold, 14]];
        Print["Pre v\[YAcute]po\[CHacek]et pou\[ZHacek]ijeme maticu rot\[AAcute]cie:"];
        Print[Style["Transforma\[CHacek]n\[AAcute] matica:", Bold]];
        Print[MatrixForm[{
            {Style["cos(\[Alpha])", Red], Style["-sin(\[Alpha])", Red]}, 
            {Style["sin(\[Alpha])", Red], Style["cos(\[Alpha])", Red]}
        }]];
        
        Print["\nNa\[SHacek]a hodnotu \[Alpha] = ", angle, "\[Degree]:"];
        Print["cos(\[Alpha]) = ", FormatExactValue[cosA]];
        Print["sin(\[Alpha]) = ", FormatExactValue[sinA]];
        
        Print["\nRot\[AAcute]ciu vykon\[AAcute]me pomocou vzorcov:"];
        Print["x' = x \[CenterDot] cos(\[Alpha]) - y \[CenterDot] sin(\[Alpha])"];
        Print["y' = x \[CenterDot] sin(\[Alpha]) + y \[CenterDot] cos(\[Alpha])"];
        
        Print["\nV\[YAcute]po\[CHacek]et pre jednotliv\[EAcute] vrcholy:"];
        
        (* V\[YAcute]po\[CHacek]et pre jednotliv\[EAcute] vrcholy - PRESN\[CapitalEAcute] HODNOTY *)
        Table[
            Module[{x = normalizedVertices[[i, 1]], 
                    y = normalizedVertices[[i, 2]],
                    newX, newY, expandNewX, expandNewY, 
                    simpleNewX, simpleNewY, invCalcX, invCalcY,
                    exactOutput, exactInvResult},
                    
                Print["\nVrchol ", FromCharacterCode[64 + i], ":"];
                Print[Style["P\[OHat]vodn\[EAcute] s\[UAcute]radnice:", Bold], " [", FormatExactValue[x], ", ", FormatExactValue[y], "]"];
                
                (* Vykonanie v\[YAcute]po\[CHacek]tov s exactn\[YAcute]mi hodnotami *)
                newX = x*cosA - y*sinA;
                newY = x*sinA + y*cosA;
                
                (* Rozvinut\[EAcute] v\[YAcute]razy pre lep\[SHacek]ie zobrazenie *)
                expandNewX = Expand[newX];
                expandNewY = Expand[newY];
                
                (* Zjednodu\[SHacek]enie v\[YAcute]razov bez konverzie na \[CHacek]\[IAcute]sla *)
                simpleNewX = Expand[expandNewX];
                simpleNewY = Expand[expandNewY];
                
                (* Detailn\[YAcute] postup pre x-ov\[UAcute] s\[UAcute]radnicu *)
                Print[Style["V\[YAcute]po\[CHacek]et x-ovej s\[UAcute]radnice:", Bold]];
                Print["x' = x \[CenterDot] cos(\[Alpha]) - y \[CenterDot] sin(\[Alpha])"];
                
                (* Zobrazenie s hodnotami *)
                Print["x' = ", FormatExactValue[x], " \[CenterDot] ", FormatExactValue[cosA], " - ", FormatExactValue[y], " \[CenterDot] ", FormatExactValue[sinA]];
                
                (* Rozvinut\[YAcute] v\[YAcute]po\[CHacek]et - zobraz\[IAcute]me aj medzikroky ako presn\[EAcute] v\[YAcute]razy *)
                If[x != 0 || y != 0, 
                    Print["x' = ", FormatExactValue[expandNewX]];
                ];
                
                (* Zobraz kone\[CHacek]n\[YAcute] v\[YAcute]sledok ako presn\[YAcute] v\[YAcute]raz *)
                Print["x' = ", FormatExactValue[simpleNewX]];
                
                (* Detailn\[YAcute] postup pre y-ov\[UAcute] s\[UAcute]radnicu *)
                Print[Style["V\[YAcute]po\[CHacek]et y-ovej s\[UAcute]radnice:", Bold]];
                Print["y' = x \[CenterDot] sin(\[Alpha]) + y \[CenterDot] cos(\[Alpha])"];
                
                (* Zobrazenie s hodnotami *)
                Print["y' = ", FormatExactValue[x], " \[CenterDot] ", FormatExactValue[sinA], " + ", FormatExactValue[y], " \[CenterDot] ", FormatExactValue[cosA]];
                
                (* Rozvinut\[YAcute] v\[YAcute]po\[CHacek]et - zobraz\[IAcute]me aj medzikroky ako presn\[EAcute] v\[YAcute]razy *)
                If[x != 0 || y != 0, 
                    Print["y' = ", FormatExactValue[expandNewY]];
                ];
                
                (* Zobraz kone\[CHacek]n\[YAcute] v\[YAcute]sledok ako presn\[YAcute] v\[YAcute]raz *)
                Print["y' = ", FormatExactValue[simpleNewY]];
                
                (* V\[YAcute]sledn\[EAcute] s\[UAcute]radnice - pou\[ZHacek]\[IAcute]vame Expand a Simplify pre spr\[AAcute]vne rozvinutie *)
                exactOutput = {simpleNewX, simpleNewY};
                Print[Style["V\[YAcute]sledn\[EAcute] s\[UAcute]radnice:", Bold], " [", FormatExactValue[exactOutput[[1]]], ", ", FormatExactValue[exactOutput[[2]]], "]"];
                
                (* Overenie inverznej transform\[AAcute]cie *)
                Print[Style["\nOverenie pomocou inverznej transform\[AAcute]cie:", Bold]];
                Print["Pre inverzn\[YAcute] uhol \[Alpha]' = ", invAngle, "\[Degree]:"];
                Print["cos(\[Alpha]') = ", FormatExactValue[cosInvA]];
                Print["sin(\[Alpha]') = ", FormatExactValue[sinInvA]];
                
                (* V\[YAcute]po\[CHacek]et inverznej transform\[AAcute]cie *)
                invCalcX = outputVertices[[i, 1]]*cosInvA - outputVertices[[i, 2]]*sinInvA;
                invCalcY = outputVertices[[i, 1]]*sinInvA + outputVertices[[i, 2]]*cosInvA;
                
                (* V\[YAcute]po\[CHacek]et pre x s presn\[YAcute]mi hodnotami *)
                Print["x = x' \[CenterDot] cos(\[Alpha]') - y' \[CenterDot] sin(\[Alpha]')"];
                Print["x = ", FormatExactValue[outputVertices[[i, 1]]], " \[CenterDot] ", 
                      FormatExactValue[cosInvA], " - ", FormatExactValue[outputVertices[[i, 2]]], 
                      " \[CenterDot] ", FormatExactValue[sinInvA]];
                
                (* Zobrazenie rozvinut\[EAcute]ho v\[YAcute]razu pre inverzn\[YAcute] v\[YAcute]po\[CHacek]et *)
                invCalcX = Expand[invCalcX];
                Print["x = ", FormatExactValue[invCalcX]];
                
                (* Zobraz kone\[CHacek]n\[YAcute] v\[YAcute]sledok overenia ako presn\[YAcute] v\[YAcute]raz *)
                Print["x = ", FormatExactValue[Simplify[invCalcX]], " = ", FormatExactValue[x]];
                
                (* V\[YAcute]po\[CHacek]et pre y s presn\[YAcute]mi hodnotami *)
                Print["y = x' \[CenterDot] sin(\[Alpha]') + y' \[CenterDot] cos(\[Alpha]')"];
                Print["y = ", FormatExactValue[outputVertices[[i, 1]]], " \[CenterDot] ", 
                      FormatExactValue[sinInvA], " + ", FormatExactValue[outputVertices[[i, 2]]], 
                      " \[CenterDot] ", FormatExactValue[cosInvA]];
                
                (* Zobrazenie rozvinut\[EAcute]ho v\[YAcute]razu pre inverzn\[YAcute] v\[YAcute]po\[CHacek]et *)
                invCalcY = Expand[invCalcY];
                Print["y = ", FormatExactValue[invCalcY]];
                
                (* Zobraz kone\[CHacek]n\[YAcute] v\[YAcute]sledok overenia ako presn\[YAcute] v\[YAcute]raz *)
                Print["y = ", FormatExactValue[Simplify[invCalcY]], " = ", FormatExactValue[y]];
                
                (* Maticov\[YAcute] z\[AAcute]pis - s presn\[YAcute]mi v\[YAcute]razmi *)
                Print[Style["\nMaticov\[YAcute] z\[AAcute]pis:", Bold]];
                
                (* V\[YAcute]po\[CHacek]et presn\[YAcute]ch v\[YAcute]razov pre v\[YAcute]sledky *)
                exactOutput = {Expand[outputVertices[[i, 1]]], Expand[outputVertices[[i, 2]]]};
                
                (* Zobrazenie maticov\[EAcute]ho z\[AAcute]pisu s presn\[YAcute]mi v\[YAcute]sledkami *)
                Print[
                    Row[{
                        MatrixForm[{
                            {Style[cosA, Red], Style[-sinA, Red]},
                            {Style[sinA, Red], Style[cosA, Red]}
                        }],
                        " \[CenterDot] ",
                        MatrixForm[{{x}, {y}}],
                        " = ",
                        MatrixForm[{{Style[exactOutput[[1]], Red]}, {Style[exactOutput[[2]], Red]}}]
                    }]
                ];
            ],
            {i, 5}
        ];
        
        (* V\[CapitalYAcute]SLEDOK - upraven\[EAcute] pre presn\[EAcute] v\[YAcute]razy v rozvinutom tvare *)
        Print[Style["\nV\[CapitalYAcute]SLEDOK:", Bold, 14]];
        Print["Vrcholy dom\[CHacek]eka po rot\[AAcute]cii:"];
        
        (* Zjednodu\[SHacek]en\[AAcute] matica v\[YAcute]sledku s presn\[YAcute]mi hodnotami v rozvinutom tvare *)
        Module[{expandedVertices},
            (* Pre ka\[ZHacek]d\[YAcute] bod rozvin\[UAcute]\[THacek] v\[YAcute]razy na \[CHacek]o najjednoduch\[SHacek]\[IAcute] tvar bez z\[AAcute]tvoriek *)
            expandedVertices = Map[Function[point, 
                {Expand[point[[1]]], Expand[point[[2]]]}
            ], outputVertices];
            
            (* Zapameta\[THacek] tento v\[YAcute]sledok pre n\[AAcute]vrat *)
            exactOutputVertices = expandedVertices;
            
            (* Vytvor\[IAcute]me pekne form\[AAcute]tovan\[UAcute] verziu matice na zobrazenie *)
            Print[MatrixForm[Map[Style[#, Red] &, expandedVertices, {2}]]];
        ];
        
        (* VIZUALIZ\[CapitalAAcute]CIA *)
        Print[Style["\nVIZUALIZ\[CapitalAAcute]CIA:", Bold, 14]];
        Print[CreateVisualization[normalizedVertices, outputVertices, angle]];
        
        (* LEGENDA *)
        Print[Style["\nLEGENDA:", Bold, 14]];
        Print["\[Bullet] Modr\[EAcute] body a \[CHacek]iary: P\[OHat]vodn\[YAcute] dom\[CHacek]ek"];
        Print["\[Bullet] \[CapitalCHacek]erven\[EAcute] body a \[CHacek]iary: Oto\[CHacek]en\[YAcute] dom\[CHacek]ek"];
        Print["\[Bullet] Zelen\[YAcute] obl\[UAcute]k: Zn\[AAcute]zornenie uhlu rot\[AAcute]cie"];
        Print["\[Bullet] Zelen\[EAcute] preru\[SHacek]ovan\[EAcute] \[CHacek]iary: Spojenia vrcholov s po\[CHacek]iatkom"];
        Print["\[Bullet] Mrie\[ZHacek]ka: S\[UAcute]radnicov\[YAcute] syst\[EAcute]m"];
        Print["\[Bullet] O: Po\[CHacek]iatok s\[UAcute]radnicovej s\[UAcute]stavy - stred rot\[AAcute]cie"];
        
        Print[Style["\nP\[OHat]vodn\[EAcute] vrcholy (modr\[AAcute]):", Bold]];
        Table[
            Print[Row[{Style["Vrchol " <> FromCharacterCode[64 + i] <> ": ", 
                RGBColor[0.1, 0.1, 1]], normalizedVertices[[i]]}]], {i, 5}];
                
        Print[Style["\nV\[YAcute]sledn\[EAcute] vrcholy (\[CHacek]erven\[AAcute]):", Bold]];
        Table[
            Print[Row[{Style["Vrchol " <> FromCharacterCode[64 + i] <> "': ", 
                RGBColor[1, 0.1, 0.1]], exactOutputVertices[[i]]}]], 
            {i, 5}];
                
        Print[Style["\nInverzn\[AAcute] transform\[AAcute]cia:", Bold]];
        Print["Pre vr\[AAcute]tenie do p\[OHat]vodn\[EAcute]ho stavu pou\[ZHacek]ite rot\[AAcute]ciu o opa\[CHacek]n\[YAcute] uhol:"];
        Print["\[Alpha]' = -\[Alpha] = ", invAngle, "\[Degree]"];
        
        (* MATEMATICK\[CapitalEAcute] VLASTNOSTI TRANSFORM\[CapitalAAcute]CIE - pridan\[EAcute] pre Hard bal\[IAcute]k *)
        Print[Style["\nMATEMATICK\[CapitalEAcute] VLASTNOSTI:", Bold, 14]];
        Print["\[Bullet] Rot\[AAcute]cia okolo po\[CHacek]iatku zachov\[AAcute]va vzdialenos\[THacek] v\[SHacek]etk\[YAcute]ch bodov od po\[CHacek]iatku s\[UAcute]stavy [0,0]"];
        Print["\[Bullet] Rot\[AAcute]cia nemen\[IAcute] ve\:013ekos\[THacek] ani tvar geometrick\[YAcute]ch \[UAcute]tvarov - je to izometria"];
        Print["\[Bullet] Rot\[AAcute]cia zachov\[AAcute]va uhly medzi \[UAcute]se\[CHacek]kami a ich d\:013a\[ZHacek]ky"];
        Print["\[Bullet] Rot\[AAcute]cia zachov\[AAcute]va obsah a obvod dom\[CHacek]eka"];
        Print["\[Bullet] Po\[CHacek]iatok s\[UAcute]radnicovej s\[UAcute]stavy [0,0] je fixn\[YAcute] bod rot\[AAcute]cie - zost\[AAcute]va nezmenen\[YAcute]"];
        
        (* \[CapitalSHacek]PECI\[CapitalAAcute]LNE HODNOTY GONIOMETRICK\[CapitalYAcute]CH FUNKCI\[CapitalIAcute] *)
        Print[Style["\nTABU\:013dKA \[CapitalSHacek]PECI\[CapitalAAcute]LNYCH HODN\[CapitalOHat]T:", Bold, 14]];
        Print["Hodnoty pre naj\[CHacek]astej\[SHacek]ie uhly:"];
        
        Print[TableForm[
            {
                {"\[Alpha]", "sin(\[Alpha])", "cos(\[Alpha])"},
                {"0\[Degree]", "0", "1"},
                {"30\[Degree]", "1/2", "\[Sqrt]3/2"},
                {"45\[Degree]", "1/\[Sqrt]2", "1/\[Sqrt]2"},
                {"60\[Degree]", "\[Sqrt]3/2", "1/2"},
                {"90\[Degree]", "1", "0"},
                {"120\[Degree]", "\[Sqrt]3/2", "-1/2"},
                {"135\[Degree]", "1/\[Sqrt]2", "-1/\[Sqrt]2"},
                {"150\[Degree]", "1/2", "-\[Sqrt]3/2"},
                {"180\[Degree]", "0", "-1"},
                {"270\[Degree]", "-1", "0"},
                {"360\[Degree]", "0", "1"}
            },
            TableHeadings -> None,
            TableAlignments -> Center,
            TableSpacing -> {1, 3}
        ]];
        
        (* Return the exact output vertices without any simplification *)
        exactOutputVertices
    ];
    
(* Defin\[IAcute]cia chybov\[YAcute]ch spr\[AAcute]v *)
DomcekRotacia::invalidInput = "Neplatn\[YAcute] vstup. O\[CHacek]ak\[AAcute]va sa matica alebo zoznam s\[UAcute]radn\[IAcute]c dom\[CHacek]eka.";
DomcekRotacia::insufficientVertices = "Nedostato\[CHacek]n\[YAcute] po\[CHacek]et vrcholov. S\[UAcute] potrebn\[EAcute] aspo\[NHacek] 5 vrcholy.";

End[];
EndPackage[];







