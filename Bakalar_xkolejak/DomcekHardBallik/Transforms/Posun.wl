(* ::Package:: *)

(* ::Package:: *)
(**)


BeginPackage["DomcekHardBalik`Transforms`Posun`", {"DomcekHardBalik`"}];


DomcekPosun::usage = 
    "DomcekPosun[vertices_] zobraz\[IAcute] interakt\[IAcute]vny tutori\[AAcute]l pre posun dom\[CHacek]eka a vr\[AAcute]ti nov\[EAcute] vrcholy.";

Begin["`Private`"];


NormalizeVertices[vertices_] := 
    Module[{processedVertices},
        (* Spracovanie r\[OHat]znych vstupn\[YAcute]ch form\[AAcute]tov *)
        processedVertices = Which[
            (* Ak je to matica *)
            MatrixQ[vertices], vertices,
            
            (* Ak je to zoznam vektorov *)
            ListQ[vertices] && Length[Dimensions[vertices]] == 2, 
                If[Length[DeleteDuplicates[Length /@ vertices]] > 1,
                    (* Ak s\[UAcute] vektory r\[OHat]znej d\:013a\[ZHacek]ky *)
                    Block[{cleanedVertices = Select[vertices, VectorQ]},
                        If[Length[cleanedVertices] >= 5,
                            cleanedVertices[[1;;5]],
                            Message[DomcekPosun::invalidInput]; 
                            Abort[]
                        ]
                    ],
                    (* Ak s\[UAcute] vektory rovnakej d\:013a\[ZHacek]ky *)
                    vertices
                ],
            
            (* Ak ide o jednorozmern\[YAcute] vektor, sk\[UAcute]sime ho rozdeli\[THacek] *)
            ListQ[vertices] && Length[Dimensions[vertices]] == 1 && EvenQ[Length[vertices]], 
                Partition[vertices, 2],
                
            (* Inak *)
            True, 
                Message[DomcekPosun::invalidInput]; 
                Abort[]
        ];
        
        
        
        
        If[Length[processedVertices] < 5,
            Message[DomcekPosun::insufficientVertices];
            Abort[]
        ];
        
        
        If[Max[Abs[processedVertices[[All, 1]]]] > 10 || Max[Abs[processedVertices[[All, 2]]]] > 10,
            (* \[CapitalSHacek]k\[AAcute]lovanie vrcholov, ak s\[UAcute] pr\[IAcute]li\[SHacek] \[DHacek]aleko *)
            processedVertices = processedVertices / (Max[Abs[Flatten[processedVertices]]] / 6);
        ];
        
        (* Vraciame presn\[EAcute] hodnoty bez zaokr\[UAcute]h\:013eovania *)
        processedVertices[[1;;5]]
    ];

(* Funkcia na generovanie parametrov posunu - UPRAVEN\[CapitalAAcute] PRE ZACHOVANIE PRESN\[CapitalYAcute]CH HODN\[CapitalOHat]T *)
GetTranslationParameters[vertices_] := 
    Module[{dx, dy, valid = False, newVertices, 
            allVertices, xMin, xMax, yMin, yMax, 
            invDx, invDy, invNewVertices, invVertices,
            narocnost},
        
        (* Anal\[YAcute]za p\[OHat]vodn\[YAcute]ch s\[UAcute]radn\[IAcute]c *)
        allVertices = vertices;
        xMin = Min[allVertices[[All, 1]]];
        xMax = Max[allVertices[[All, 1]]];
        yMin = Min[allVertices[[All, 2]]];
        yMax = Max[allVertices[[All, 2]]];
        
        (* N\[AAcute]ro\[CHacek]nej\[SHacek]ie hodnoty posunu pre Hard bal\[IAcute]k *)
        narocnost = RandomReal[]; (* N\[AAcute]hodn\[EAcute] \[CHacek]\[IAcute]slo pre ur\[CHacek]enie n\[AAcute]ro\[CHacek]nosti *)
        
        While[!valid,
            (* Generovanie konkr\[EAcute]tnych posunov - n\[AAcute]ro\[CHacek]nej\[SHacek]ie pre Hard bal\[IAcute]k *)
            If[narocnost < 0.7, 
                (* N\[AAcute]ro\[CHacek]nej\[SHacek]ie posuny - v\[ADoubleDot]\[CHacek]\[SHacek]ie hodnoty a/alebo aj z\[AAcute]porn\[EAcute] hodnoty *)
                dx = RandomChoice[{-7, -6, -5, -4, -3, 3, 4, 5, 6, 7}];
                dy = RandomChoice[{-7, -6, -5, -4, -3, 3, 4, 5, 6, 7}],
                
                (* Klasick\[EAcute] posuny ako v Medium bal\[IAcute]ku *)
                dx = RandomChoice[{-5, -4, -3, -2, 2, 3, 4, 5}];
                dy = RandomChoice[{-5, -4, -3, -2, 2, 3, 4, 5}]
            ];
            
            (* V\[YAcute]po\[CHacek]et nov\[YAcute]ch vrcholov po posune - PRESN\[CapitalEAcute] HODNOTY BEZ ZAOKR\[CapitalUAcute]H\:013dOVANIA *)
            newVertices = Map[{#[[1]] + dx, #[[2]] + dy} &, allVertices];
            
            (* V\[YAcute]po\[CHacek]et parametrov inverzn\[EAcute]ho posunu *)
            invDx = -dx;
            invDy = -dy;
            
            (* Test inverznej transform\[AAcute]cie - PRESN\[CapitalEAcute] POROVNANIE *)
            invNewVertices = Map[{#[[1]] + invDx, #[[2]] + invDy} &, newVertices];
            
            
            valid = 
                (* Kontrola, \[CHacek]i v\[YAcute]sledn\[EAcute] vrcholy s\[UAcute] v bezpe\[CHacek]nej oblasti *)
                AllTrue[newVertices, (Abs[#[[1]]] <= 14 && Abs[#[[2]]] <= 14) &] &&
                (* Kontrola minim\[AAcute]lnej vzdialenosti posunu - aspo\[NHacek] o 3 jednotky *)
                Sqrt[dx^2 + dy^2] >= 3 &&
                (* Kontrola, \[CHacek]i inverzn\[EAcute] vrcholy sa rovnaj\[UAcute] p\[OHat]vodn\[YAcute]m - PRESN\[CapitalEAcute] POROVNANIE *)
                Total[Flatten[Abs[allVertices - invNewVertices]]] < 0.00001 &&
                (* Zabr\[AAcute]nenie prieniku dom\[CHacek]ekov *)
                Min[
                    Table[
                        EuclideanDistance[allVertices[[i]], newVertices[[i]]],
                        {i, Length[allVertices]}
                    ]
                ] >= 3;
        ];
        
        (* Vytvorenie popisu *)
        {dx, dy, "Posun o [" <> ToString[dx] <> ", " <> ToString[dy] <> "]", newVertices}
    ];


CreateVisualization[originalVertices_, finalVertices_, translationVector_] := 
    Module[{allVertices, xMin, xMax, yMin, yMax, rangeBuffer, xRange, yRange, 
            labelOffsets, dx = translationVector[[1]], dy = translationVector[[2]],
            originalHouseLines, finalHouseLines},
        (* Spojenie v\[SHacek]etk\[YAcute]ch vrcholov pre v\[YAcute]po\[CHacek]et rozsahu *)
        allVertices = Join[originalVertices, finalVertices];
        
        (* V\[YAcute]po\[CHacek]et minim\[AAcute]lnych a maxim\[AAcute]lnych hodn\[OHat]t *)
        xMin = Min[allVertices[[All, 1]]];
        xMax = Max[allVertices[[All, 1]]];
        yMin = Min[allVertices[[All, 2]]];
        yMax = Max[allVertices[[All, 2]]];
        
        (* Buffer pre estetick\[YAcute] vzh\:013ead *)
        rangeBuffer = 2;
        
        (* Nastavenie rozsahu os\[IAcute] *)
        xRange = {Min[-11, xMin - rangeBuffer], Max[11, xMax + rangeBuffer]};
        yRange = {Min[-11, yMin - rangeBuffer], Max[11, yMax + rangeBuffer]};
        
        (* Vypo\[CHacek]\[IAcute]ta\[THacek] offsety pre ozna\[CHacek]enia, aby sa neprekr\[YAcute]vali s \[CHacek]iarami *)
        labelOffsets = Table[
            Module[{originalVertex = originalVertices[[i]], 
                    finalVertex = finalVertices[[i]], 
                    offset = {0.7, 0.7}},
                
                (* Upravi\[THacek] offset pod\:013ea polohy vrcholov *)
                If[originalVertex[[1]] > 8, offset[[1]] = -1.0];
                If[originalVertex[[2]] > 8, offset[[2]] = -1.0];
                
                {offset, offset}
            ],
            {i, Length[originalVertices]}
        ];
        
        (* Definovanie spr\[AAcute]vneho poradia pre vykreslenie dom\[CHacek]eka *)
        (* Predpoklad\[AAcute]me, \[ZHacek]e body s\[UAcute] A=1(\:013eav\[YAcute] doln\[YAcute]), B=2(prav\[YAcute] doln\[YAcute]), C=3(prav\[YAcute] stredn\[YAcute]), 
           D=4(\:013eav\[YAcute] stredn\[YAcute]), E=5(vrchol strechy) *)
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
            
            (* Posunut\[YAcute] dom\[CHacek]ek - \[CHacek]erven\[YAcute] *)
            Red, Thick, Opacity[0.7],
            Line /@ finalHouseLines,
            
            (* Pomocn\[EAcute] \[CHacek]iary - zelen\[EAcute] preru\[SHacek]ovan\[EAcute] *)
            Green, Dashed,
            Table[
                Line[{originalVertices[[i]], finalVertices[[i]]}],
                {i, Length[originalVertices]}
            ],
            
            (* Vyraznejsie body s bielym pozadim a vacsim polomerom *)
            White, 
            Table[
                Disk[originalVertices[[i]], 0.45],
                {i, 5}
            ],
            Table[
                Disk[finalVertices[[i]], 0.45],
                {i, 5}
            ],
            
            (* Body - vacsie a vyraznejsie *)
            Blue, PointSize[0.025], Point[originalVertices],
            Red, PointSize[0.025], Point[finalVertices],
            
            
            
            (* Labels s optimalizovan\[YAcute]mi poz\[IAcute]ciami *)
            Table[
                {
                    Text[Style[CharacterRange["A", "E"][[i]], Blue, Bold, 16], 
                        originalVertices[[i]] + labelOffsets[[i, 1]]],
                    Text[Style[CharacterRange["A", "E"][[i]] <> "'", Red, Bold, 16], 
                        finalVertices[[i]] + labelOffsets[[i, 2]]]
                },
                {i, 5}
            ],
            
            (* Osi *)
            Black, Thick,
            Arrow[{{xRange[[1]], 0}, {xRange[[2]], 0}}],
            Arrow[{{0, yRange[[1]]}, {0, yRange[[2]]}}],
            Text[Style["x", Black, Bold], {xRange[[2]] - 0.5, -0.5}],
            Text[Style["y", Black, Bold], {-0.5, yRange[[2]] - 0.5}],
            
            (* \[CapitalCHacek]\[IAcute]sla na osiach - PRESN\[CapitalEAcute] ZOBRAZENIE *)
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
    
    

DomcekPosun[inputVertices_] := 
    Module[{normalizedVertices, translationParams, outputVertices, dx, dy, invDx, invDy},
        (* Normaliz\[AAcute]cia vstupn\[YAcute]ch s\[UAcute]radn\[IAcute]c - ZACHOV\[CapitalAAcute]VA PRESN\[CapitalEAcute] HODNOTY *)
        normalizedVertices = NormalizeVertices[inputVertices];
        
        
        (* Tu skontrolujeme, \[CHacek]i pou\[ZHacek]\[IAcute]vame priamo zadan\[EAcute] vrcholy, ak \[AAcute]no, vypo\[CHacek]\[IAcute]tame posun pomocou nich *)
        If[Length[inputVertices] === 5 && validDomcekQ[inputVertices],
            (* Kontrola, \[CHacek]i m\[OHat]\[ZHacek]eme vypo\[CHacek]\[IAcute]ta\[THacek] posun z existuj\[UAcute]cich vrcholov *)
            (* Vygenerujeme n\[AAcute]hodn\[EAcute] cel\[EAcute] \[CHacek]\[IAcute]sla pre dx a dy *)
            dx = RandomChoice[{-7, -6, -5, -4, -3, 3, 4, 5, 6, 7}];
            dy = RandomChoice[{-7, -6, -5, -4, -3, 3, 4, 5, 6, 7}];
            
            (* V\[YAcute]po\[CHacek]et nov\[YAcute]ch vrcholov s presn\[YAcute]mi hodnotami *)
            outputVertices = Map[{#[[1]] + dx, #[[2]] + dy} &, normalizedVertices];
            
            (* Kontrola, \[CHacek]i s\[UAcute] v\[SHacek]etky vrcholy v bezpe\[CHacek]nej oblasti *)
            If[!AllTrue[outputVertices, (Abs[#[[1]]] <= 14 && Abs[#[[2]]] <= 14) &],
                (* Ak nie, vygenerujeme nov\[EAcute] parametre *)
                translationParams = GetTranslationParameters[normalizedVertices];
                dx = translationParams[[1]];
                dy = translationParams[[2]];
                outputVertices = translationParams[[4]];
            ],
            (* Ak nie s\[UAcute] validn\[EAcute] vrcholy, generujeme nov\[EAcute] parametre posunu *)
            translationParams = GetTranslationParameters[normalizedVertices];
            dx = translationParams[[1]];
            dy = translationParams[[2]];
            outputVertices = translationParams[[4]];
        ];
        
        (* Inverzn\[EAcute] parametre *)
        invDx = -dx;
        invDy = -dy;
        
        (* Nadpis *)
        Print[Style[
            "GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE DOM\[CapitalCHacek]EKA - POSUN", 
            Bold, 16]];
        Print[Style["========================================", Bold]];
        
        (* ZADANIE *)
        Print[Style["\nZADANIE:", Bold, 14]];
        Print["Majme dom\[CHacek]ek s vrcholmi:"];
        Print[MatrixForm[Map[Style[#, Blue] &, normalizedVertices, {2}]]];
        
        Print["\nVykonajte posun dom\[CHacek]eka v 2D priestore pomocou vektora posunu: \[CapitalDelta] = [", 
            Style[dx, Red], ", ", Style[dy, Red], "]"];
            
        Print["\nTransforma\[CHacek]n\[AAcute] matica posunu v homog\[EAcute]nnych s\[UAcute]radniciach:"];
        Print[MatrixForm[{
            {Style[1, Red], Style[0, Red], Style[dx, Red]}, 
            {Style[0, Red], Style[1, Red], Style[dy, Red]},
            {Style[0, Red], Style[0, Red], Style[1, Red]}
        }]];
        
        (* POSTUP *)
        Print[Style["\nPOSTUP:", Bold, 14]];
        Print["Pre v\[YAcute]po\[CHacek]et pou\[ZHacek]ijeme maticu posunu v homog\[EAcute]nnych s\[UAcute]radniciach:"];
        Print[Style["Transforma\[CHacek]n\[AAcute] matica:", Bold]];
        Print[MatrixForm[{
            {Style[1, Red], Style[0, Red], Style["dx", Red]}, 
            {Style[0, Red], Style[1, Red], Style["dy", Red]},
            {Style[0, Red], Style[0, Red], Style[1, Red]}
        }]];
        
        Print["\nPre na\[SHacek]e hodnoty:"];
        Print["dx = ", dx];
        Print["dy = ", dy];
        
        Print["\nPosun vykon\[AAcute]me pomocou vzorcov:"];
        Print["x' = x + dx"];
        Print["y' = y + dy"];
        
        Print["\nV\[YAcute]po\[CHacek]et pre jednotliv\[EAcute] vrcholy:"];
        
        (* V\[YAcute]po\[CHacek]et pre jednotliv\[EAcute] vrcholy - PRESN\[CapitalEAcute] HODNOTY *)
        Table[
            Module[{x = normalizedVertices[[i, 1]], 
                    y = normalizedVertices[[i, 2]]},
                Print["\nVrchol ", i, ":"];
                Print[Style["P\[OHat]vodn\[EAcute] s\[UAcute]radnice:", Bold], " [", x, ", ", y, "]"];
                
                (* Detailn\[YAcute] postup pre x-ov\[UAcute] s\[UAcute]radnicu *)
                Print[Style["V\[YAcute]po\[CHacek]et x-ovej s\[UAcute]radnice:", Bold]];
                Print["x' = x + dx = ", x, " + ", dx];
                Print["x' = ", outputVertices[[i, 1]]];
                
                (* Detailn\[YAcute] postup pre y-ov\[UAcute] s\[UAcute]radnicu *)
                Print[Style["V\[YAcute]po\[CHacek]et y-ovej s\[UAcute]radnice:", Bold]];
                Print["y' = y + dy = ", y, " + ", dy];
                Print["y' = ", outputVertices[[i, 2]]];
                
                (* Overenie inverznej transform\[AAcute]cie *)
                Print[Style["\nOverenie pomocou inverznej transform\[AAcute]cie:", Bold]];
                Print["x = x' + (-dx) = ", outputVertices[[i, 1]], " + (", invDx, ") = ", x];
                Print["y = y' + (-dy) = ", outputVertices[[i, 2]], " + (", invDy, ") = ", y];
                
                (* Maticov\[YAcute] z\[AAcute]pis *)
                Print[Style["\nMaticov\[YAcute] z\[AAcute]pis:", Bold]];
                Print[
                    Row[{
                        MatrixForm[{
                            {Style[1, Red], Style[0, Red], Style[dx, Red]},
                            {Style[0, Red], Style[1, Red], Style[dy, Red]},
                            {Style[0, Red], Style[0, Red], Style[1, Red]}
                        }],
                        " \[CenterDot] ",
                        MatrixForm[{{x}, {y}, {1}}],
                        " = ",
                        MatrixForm[{{outputVertices[[i, 1]]}, {outputVertices[[i, 2]]}, {1}}]
                    }]
                ];
            ],
            {i, 5}
        ];
        
        (* V\[CapitalYAcute]SLEDOK *)
        Print[Style["\nV\[CapitalYAcute]SLEDOK:", Bold, 14]];
        Print["Vrcholy dom\[CHacek]eka po posune:"];
        Print[MatrixForm[Map[Style[#, Red] &, outputVertices, {2}]]];
        
        (* VIZUALIZ\[CapitalAAcute]CIA *)
        Print[Style["\nVIZUALIZ\[CapitalAAcute]CIA:", Bold]];
        Print[CreateVisualization[normalizedVertices, outputVertices, {dx, dy}]];
        
        (* LEGENDA *)
        Print[Style["\nLEGENDA:", Bold, 14]];
        Print["\[Bullet] Modr\[EAcute] body a \[CHacek]iary: P\[OHat]vodn\[YAcute] dom\[CHacek]ek"];
        Print["\[Bullet] \[CapitalCHacek]erven\[EAcute] body a \[CHacek]iary: Posunut\[YAcute] dom\[CHacek]ek"];
        Print["\[Bullet] Zelen\[EAcute] preru\[SHacek]ovan\[EAcute] \[CHacek]iary: Posun jednotliv\[YAcute]ch vrcholov"];
        Print["\[Bullet] Mrie\[ZHacek]ka: S\[UAcute]radnicov\[YAcute] syst\[EAcute]m"];
        
        Print[Style["\nP\[OHat]vodn\[EAcute] vrcholy (modr\[AAcute]):", Bold]];
        Table[
            Print[Row[{Style["Vrchol " <> ToString[i] <> ": ", 
                RGBColor[0.1, 0.1, 1]], normalizedVertices[[i]]}]], {i, 5}];
                
        Print[Style["\nV\[YAcute]sledn\[EAcute] vrcholy (\[CHacek]erven\[AAcute]):", Bold]];
        Table[
            Print[Row[{Style["Vrchol " <> ToString[i] <> "': ", 
                RGBColor[1, 0.1, 0.1]], outputVertices[[i]]}]], {i, 5}];
                
        Print[Style["\nInverzn\[AAcute] transform\[AAcute]cia:", Bold]];
        Print["Pre vr\[AAcute]tenie do p\[OHat]vodn\[EAcute]ho stavu pou\[ZHacek]ite vektor posunu:"];
        Print["\[CapitalDelta]' = [", invDx, ", ", invDy, "] (opa\[CHacek]n\[YAcute] vektor)"];
        
        (* Vr\[AAcute]tenie nov\[YAcute]ch vrcholov pre pr\[IAcute]padn\[EAcute] \[DHacek]al\[SHacek]ie transform\[AAcute]cie - PRESN\[CapitalEAcute] HODNOTY *)
        outputVertices
    ];

(* Defin\[IAcute]cia chybov\[YAcute]ch spr\[AAcute]v *)
DomcekPosun::invalidInput = "Neplatn\[YAcute] vstup. O\[CHacek]ak\[AAcute]va sa matica alebo zoznam s\[UAcute]radn\[IAcute]c dom\[CHacek]eka.";
DomcekPosun::insufficientVertices = "Nedostato\[CHacek]n\[YAcute] po\[CHacek]et vrcholov. S\[UAcute] potrebn\[EAcute] aspo\[NHacek] 5 vrcholov.";

End[];
EndPackage[];
