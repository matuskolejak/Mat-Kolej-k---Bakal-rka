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


BeginPackage["TrojuholnikHardBalik`Transforms`Skosenie`", {"TrojuholnikHardBalik`"}];

(* Export public symbols *)
TrojuholnikSkosenie::usage = 
    "TrojuholnikSkosenie[vertices_] zobraz\[IAcute] interakt\[IAcute]vny tutori\[AAcute]l pre skosenie trojuholn\[IAcute]ka a vr\[AAcute]ti nov\[EAcute] vrcholy.";

Begin["`Private`"];

(* Funkcia na form\[AAcute]tovanie exaktn\[YAcute]ch hodn\[OHat]t *)
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

(* Pomocn\[AAcute] funkcia na normaliz\[AAcute]ciu vstupn\[YAcute]ch s\[UAcute]radn\[IAcute]c  *)
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
                        If[Length[cleanedVertices] >= 3,
                            cleanedVertices[[1;;3]],
                            Message[TrojuholnikSkosenie::invalidInput]; 
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
                Message[TrojuholnikSkosenie::invalidInput]; 
                Abort[]
        ];
        
        (* ZACHOV\[CapitalAAcute]VAME PRESN\[CapitalEAcute] HODNOTY - BEZ KONVERZIE NA NUMERICK\[CapitalEAcute] HODNOTY *)
        
        (* Kontrola po\[CHacek]tu vrcholov *)
        If[Length[processedVertices] < 3,
            Message[TrojuholnikSkosenie::insufficientVertices];
            Abort[]
        ];
        
        (* Kontrola, \[CHacek]i vrcholy zostan\[UAcute] v bezpe\[CHacek]nej oblasti, ale zachov\[AAcute]vame presn\[EAcute] hodnoty *)
        If[Max[Abs[N[processedVertices[[All, 1]]]]] > 10 || Max[Abs[N[processedVertices[[All, 2]]]]] > 10,
            (* \[CapitalSHacek]k\[AAcute]lovanie vrcholov, ak s\[UAcute] pr\[IAcute]li\[SHacek] \[DHacek]aleko, ale zachov\[AAcute]me presn\[YAcute] v\[YAcute]po\[CHacek]et *)
            processedVertices = processedVertices / (Max[Abs[N[Flatten[processedVertices]]]] / 6);
        ];
        
        
        
        (* N\[AAcute]vrat prv\[YAcute]ch troch vrcholov *)
        processedVertices[[1;;3]]
    ];


DetectShearingParameters[originalVertices_, finalVertices_] := 
    Module[{kx = 0, ky = 0, foundKx = False, foundKy = False, i = 1, 
            originalPoint, finalPoint, smer = ""},
        
        (* H\:013ead\[AAcute]me koeficienty skosenia porovn\[AAcute]van\[IAcute]m vrcholov *)
        While[i <= 3 && (!foundKx || !foundKy),
            originalPoint = originalVertices[[i]];
            finalPoint = finalVertices[[i]];
            
            (* Detekcia skosenia v smere osi x *)
            If[originalPoint[[2]] != 0 && !foundKx,
                kx = (finalPoint[[1]] - originalPoint[[1]])/originalPoint[[2]];
                (* Kontrola, \[CHacek]i ide skuto\[CHacek]ne o skosenie x *)
                If[AllTrue[Range[3], Function[j,
                    Simplify[finalVertices[[j, 1]] - originalVertices[[j, 1]] - 
                             kx*originalVertices[[j, 2]]] == 0 &&
                    finalVertices[[j, 2]] == originalVertices[[j, 2]]
                ]],
                    foundKx = True;
                    smer = "v smere osi x";
                ]
            ];
            
            (* Detekcia skosenia v smere osi y *)
            If[originalPoint[[1]] != 0 && !foundKy,
                ky = (finalPoint[[2]] - originalPoint[[2]])/originalPoint[[1]];
                (* Kontrola, \[CHacek]i ide skuto\[CHacek]ne o skosenie y *)
                If[AllTrue[Range[3], Function[j,
                    Simplify[finalVertices[[j, 2]] - originalVertices[[j, 2]] - 
                             ky*originalVertices[[j, 1]]] == 0 &&
                    finalVertices[[j, 1]] == originalVertices[[j, 1]]
                ]],
                    foundKy = True;
                    smer = "v smere osi y";
                ]
            ];
            
            i++;
        ];
        
        (* Kontrola \[CHacek]i sme na\[SHacek]li nejak\[EAcute] skosenie *)
        If[foundKx,
            {Simplify[kx], 0, smer, "Skosenie v smere osi x s koeficientom k = " <> FormatExactValue[kx]}
        ,
            If[foundKy,
                {0, Simplify[ky], smer, "Skosenie v smere osi y s koeficientom k = " <> FormatExactValue[ky]}
            ,
                None  (* \[CapitalZHacek]iadne skosenie nen\[AAcute]jden\[EAcute] *)
            ]
        ]
    ];

(* Funkcia na generovanie parametrov skosenia *)
GetShearingParameters[vertices_, finalVertices_: None] := 
    Module[{kx, ky, smer, valid = False, newVertices, 
            allVertices, xMin, xMax, yMin, yMax, 
            invKx, invKy, invNewVertices, invVertices,
            narocnost, popis, detectedParameters},
            
        (* Ak s\[UAcute] zadan\[EAcute] finalVertices, pok\[UAcute]sime sa detekova\[THacek] parametre skosenia *)
        If[finalVertices =!= None,
            detectedParameters = DetectShearingParameters[vertices, finalVertices];
            
            (* Ak sme na\[SHacek]li parametre, pou\[ZHacek]ijeme ich *)
            If[detectedParameters =!= None,
                kx = detectedParameters[[1]];
                ky = detectedParameters[[2]];
                smer = detectedParameters[[3]];
                popis = detectedParameters[[4]];
                
                Return[{kx, ky, smer, popis, finalVertices}];
            ]
        ];
        
        (* Anal\[YAcute]za p\[OHat]vodn\[YAcute]ch s\[UAcute]radn\[IAcute]c *)
        allVertices = vertices;
        xMin = Min[N[allVertices[[All, 1]]]];
        xMax = Max[N[allVertices[[All, 1]]]];
        yMin = Min[N[allVertices[[All, 2]]]];
        yMax = Max[N[allVertices[[All, 2]]]];
        
        (* N\[AAcute]ro\[CHacek]nej\[SHacek]ie hodnoty skosenia pre Hard bal\[IAcute]k *)
        narocnost = RandomReal[]; (* N\[AAcute]hodn\[EAcute] \[CHacek]\[IAcute]slo pre ur\[CHacek]enie n\[AAcute]ro\[CHacek]nosti *)
        
        While[!valid,
            (* Generovanie parametrov skosenia - n\[AAcute]ro\[CHacek]nej\[SHacek]ie pre Hard bal\[IAcute]k *)
            If[narocnost < 0.7, 
                (* N\[AAcute]ro\[CHacek]nej\[SHacek]ie koeficienty - zlomky a v\[ADoubleDot]\[CHacek]\[SHacek]ie hodnoty *)
                If[RandomReal[] < 0.5,
                    (* Skosenie v smere osi x *)
                    kx = RandomChoice[{-2, -3/2, -1, -1/2, 1/2, 1, 3/2, 2}];
                    ky = 0;
                    smer = "v smere osi x";
                    popis = "Skosenie v smere osi x s koeficientom k = " <> FormatExactValue[kx];
                ,
                    (* Skosenie v smere osi y *)
                    kx = 0;
                    ky = RandomChoice[{-2, -3/2, -1, -1/2, 1/2, 1, 3/2, 2}];
                    smer = "v smere osi y";
                    popis = "Skosenie v smere osi y s koeficientom k = " <> FormatExactValue[ky];
                ]
            ,
                (* Jednoduch\[SHacek]ie koeficienty *)
                If[RandomReal[] < 0.5,
                    (* Skosenie v smere osi x *)
                    kx = RandomChoice[{-1, 1/2, 1}];
                    ky = 0;
                    smer = "v smere osi x";
                    popis = "Skosenie v smere osi x s koeficientom k = " <> FormatExactValue[kx];
                ,
                    (* Skosenie v smere osi y *)
                    kx = 0;
                    ky = RandomChoice[{-1, 1/2, 1}];
                    smer = "v smere osi y";
                    popis = "Skosenie v smere osi y s koeficientom k = " <> FormatExactValue[ky];
                ]
            ];
            
            (* V\[YAcute]po\[CHacek]et nov\[YAcute]ch vrcholov po skosen\[IAcute] - PRESN\[CapitalYAcute] V\[CapitalYAcute]PO\[CapitalCHacek]ET *)
            newVertices = Map[
                Function[p,
                    {Simplify[p[[1]] + kx*p[[2]]], Simplify[p[[2]] + ky*p[[1]]]}
                ], 
                allVertices
            ];
            
            (* V\[YAcute]po\[CHacek]et parametrov inverzn\[EAcute]ho skosenia *)
            invKx = -kx;
            invKy = -ky;
            
            (* Test inverznej transform\[AAcute]cie - PRESN\[CapitalYAcute] V\[CapitalYAcute]PO\[CapitalCHacek]ET *)
            invNewVertices = Map[
                Function[p,
                    {Simplify[p[[1]] + invKx*p[[2]]], Simplify[p[[2]] + invKy*p[[1]]]}
                ], 
                newVertices
            ];
            
            (* Kontroly - PRESN\[CapitalEAcute] POROVNANIA, len N[] pre bezpe\[CHacek]n\[UAcute] oblas\[THacek] *)
            valid = 
                (* Kontrola, \[CHacek]i v\[YAcute]sledn\[EAcute] vrcholy s\[UAcute] v bezpe\[CHacek]nej oblasti *)
                AllTrue[newVertices, (Abs[N[#[[1]]]] <= 14 && Abs[N[#[[2]]]] <= 14) &] &&
                (* Kontrola, \[CHacek]i inverzn\[EAcute] vrcholy sa rovnaj\[UAcute] p\[OHat]vodn\[YAcute]m - PRESN\[CapitalEAcute] POROVNANIE *)
                AllTrue[Range[Length[allVertices]], Function[i, 
                    Simplify[allVertices[[i, 1]] - invNewVertices[[i, 1]]] == 0 && 
                    Simplify[allVertices[[i, 2]] - invNewVertices[[i, 2]]] == 0]] &&
                (* Zabr\[AAcute]nenie trivi\[AAcute]lnemu pr\[IAcute]padu kde ni\[CHacek] nemen\[IAcute] *)
                (kx != 0 || ky != 0);
        ];
        
        (* Vytvorenie popisu *)
        {kx, ky, smer, popis, newVertices}
    ];

(* Funkcia na vizualiz\[AAcute]ciu skosenia *)
CreateVisualization[originalVertices_, finalVertices_, k_, smer_] := 
    Module[{allVertices, xMin, xMax, yMin, yMax, rangeBuffer, xRange, yRange, 
            labelOffsets, kx, ky, brightGreen},
            
        (* Defin\[IAcute]cia jasnej\[SHacek]ej zelenej farby *)
        brightGreen = RGBColor[0, 0.8, 0.2];
            
        (* Nastavenie parametrov skosenia *)
        kx = If[smer == "v smere osi x", k, 0];
        ky = If[smer == "v smere osi y", k, 0];
        
        (* Spojenie v\[SHacek]etk\[YAcute]ch vrcholov pre v\[YAcute]po\[CHacek]et rozsahu - N[] len pre zobrazenie *)
        allVertices = Join[originalVertices, finalVertices];
        
        (* V\[YAcute]po\[CHacek]et minim\[AAcute]lnych a maxim\[AAcute]lnych hodn\[OHat]t - N[] pre vizualiz\[AAcute]ciu *)
        xMin = Min[N[allVertices[[All, 1]]]];
        xMax = Max[N[allVertices[[All, 1]]]];
        yMin = Min[N[allVertices[[All, 2]]]];
        yMax = Max[N[allVertices[[All, 2]]]];
        
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
                
                (* Upravi\[THacek] offset pod\:013eea polohy vrcholov - N[] len pre vizualiz\[AAcute]ciu *)
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
                {y, Ceiling[yRange[[1]]], Floor[yRange[[2]]], 1}],
            Table[Line[{{x, yRange[[1]]}, {x, yRange[[2]]}}], 
                {x, Ceiling[xRange[[1]]], Floor[xRange[[2]]], 1}],
            
            (* P\[OHat]vodn\[YAcute] trojuholn\[IAcute]k - modr\[YAcute] *)
            Blue, Thick, Opacity[0.7],
            Line[Append[originalVertices, First[originalVertices]]],
            
            (* Skosen\[YAcute] trojuholn\[IAcute]k - \[CHacek]erven\[YAcute] *)
            Red, Thick, Opacity[0.7],
            Line[Append[finalVertices, First[finalVertices]]],
            
            (* Pomocn\[EAcute] \[CHacek]iary - zelen\[EAcute] preru\[SHacek]ovan\[EAcute] *)
            brightGreen, Dashed,
            Table[
                Line[{originalVertices[[i]], finalVertices[[i]]}],
                {i, Length[originalVertices]}
            ],
            
            (* Body s bielym pozad\[IAcute]m - zv\[ADoubleDot]\[CHacek]\[SHacek]en\[EAcute] *)
            White, 
            Table[
                Disk[originalVertices[[i]], 0.45],
                {i, 3}
            ],
            Table[
                Disk[finalVertices[[i]], 0.45],
                {i, 3}
            ],
            
            
            
            (* Body - zv\[ADoubleDot]\[CHacek]\[SHacek]en\[EAcute] *)
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
                {i, 3}
            ],
            
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

(* Hlavn\[AAcute] funkcia *)
TrojuholnikSkosenie[inputVertices_] := 
    Module[{normalizedVertices, shearParams, outputVertices, kx, ky, smer, popis, 
            invKx, invKy, k, koefX, koefY, 
            detectInputOutput = False, finalVertices = None},
            
        (* Skontrolujeme, \[CHacek]i vstup m\[AAcute] viac ako 3 vrcholy - mo\[ZHacek]no obsahuje aj v\[YAcute]sledok predch\[AAcute]dzaj\[UAcute]cej transform\[AAcute]cie *)
        If[Length[inputVertices] > 3 && Mod[Length[inputVertices], 3] == 0,
            (* Rozdel\[IAcute]me vstup na vstupn\[EAcute] a o\[CHacek]ak\[AAcute]van\[EAcute] v\[YAcute]stupn\[EAcute] vrcholy *)
            normalizedVertices = inputVertices[[1;;3]];
            (* Ak m\[AAcute]me viacn\[AAcute]sobn\[EAcute] trojice, berieme posledn\[UAcute] ako o\[CHacek]ak\[AAcute]van\[YAcute] v\[YAcute]stup *)
            finalVertices = inputVertices[[-(3;;-1)]];
            detectInputOutput = True,
            (* Inak normalizujeme vstupn\[EAcute] vrcholy a \[ZHacek]iadne v\[YAcute]stupn\[EAcute] ne\[CHacek]ak\[AAcute]me *)
            normalizedVertices = NormalizeVertices[inputVertices];
        ];
        
        (* Z\[IAcute]skanie parametrov skosenia - bu\[DHacek] detekovan\[EAcute] alebo generovan\[EAcute] *)
        shearParams = If[detectInputOutput,
            GetShearingParameters[normalizedVertices, finalVertices],
            GetShearingParameters[normalizedVertices]
        ];
        
        kx = shearParams[[1]];
        ky = shearParams[[2]];
        smer = shearParams[[3]];
        popis = shearParams[[4]];
        outputVertices = shearParams[[5]];
        
        (* Nastavenie koeficientu pre zobrazenie v postupe *)
        k = If[smer == "v smere osi x", kx, ky];
        
        (* Inverzn\[EAcute] parametre *)
        invKx = -kx;
        invKy = -ky;
        
        (* Koeficienty pre transforma\[CHacek]n\[UAcute] maticu *)
        koefX = If[smer == "v smere osi x", k, 0];
        koefY = If[smer == "v smere osi y", k, 0];
        
        (* Nadpis *)
        Print[Style[
            "GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE TROJUHOLN\[CapitalIAcute]KA - SKOSENIE", 
            Bold, 16]];
        Print[Style["========================================", Bold]];
        
        (* ZADANIE *)
        Print[Style["\nZADANIE:", Bold, 14]];
        Print["Majme trojuholn\[IAcute]k ABC s vrcholmi:"];
        Print[MatrixForm[Map[Style[#, Blue] &, normalizedVertices, {2}]]];
        
        Print["\nVykonajte skosenie trojuholn\[IAcute]ka v 2D priestore " <> smer <> " s koeficientom k = ", 
            Style[FormatExactValue[k], Red], "."];
            
        Print["\nTransforma\[CHacek]n\[AAcute] matica skosenia:"];
        If[smer == "v smere osi x",
            Print[MatrixForm[{
                {Style[1, Red], Style[k, Red]}, 
                {Style[0, Red], Style[1, Red]}
            }]],
            Print[MatrixForm[{
                {Style[1, Red], Style[0, Red]}, 
                {Style[k, Red], Style[1, Red]}
            }]]
        ];
        
        (* POSTUP *)
        Print[Style["\nPOSTUP:", Bold, 14]];
        Print["Pre v\[YAcute]po\[CHacek]et pou\[ZHacek]ijeme maticu skosenia:"];
        Print[Style["Transforma\[CHacek]n\[AAcute] matica pre skosenie " <> smer <> ":", Bold]];
        
        If[smer == "v smere osi x",
            Print[MatrixForm[{
                {Style[1, Red], Style["k", Red]}, 
                {Style[0, Red], Style[1, Red]}
            }]];
            Print["\nVzorec pre v\[YAcute]po\[CHacek]et nov\[YAcute]ch s\[UAcute]radn\[IAcute]c pri skosen\[IAcute] v smere osi x:"];
            Print["x' = x + k\[CenterDot]y"];
            Print["y' = y"],
            
            Print[MatrixForm[{
                {Style[1, Red], Style[0, Red]}, 
                {Style["k", Red], Style[1, Red]}
            }]];
            Print["\nVzorec pre v\[YAcute]po\[CHacek]et nov\[YAcute]ch s\[UAcute]radn\[IAcute]c pri skosen\[IAcute] v smere osi y:"];
            Print["x' = x"];
            Print["y' = k\[CenterDot]x + y"]
        ];
        
        Print["\nNa\[SHacek]a hodnota k = ", FormatExactValue[k]];
        
        Print["\nV\[YAcute]po\[CHacek]et pre jednotliv\[EAcute] vrcholy:"];
        
        Table[
            Module[{x = normalizedVertices[[i, 1]], 
                   y = normalizedVertices[[i, 2]], 
                   newX, newY, expandedX, expandedY, simplifiedX, simplifiedY},
                   
                (* V\[YAcute]po\[CHacek]et nov\[YAcute]ch s\[UAcute]radn\[IAcute]c s presn\[YAcute]m v\[YAcute]sledkom *)
                If[smer == "v smere osi x",
                    newX = x + k*y;
                    newY = y,
                    
                    newX = x;
                    newY = k*x + y
                ];
                
                (* Rozvinut\[EAcute] a zjednodu\[SHacek]en\[EAcute] v\[YAcute]razy *)
                expandedX = Expand[newX];
                expandedY = Expand[newY];
                simplifiedX = Simplify[newX];
                simplifiedY = Simplify[newY];
                
                Print["\nVrchol ", FromCharacterCode[64 + i], ":"];
                Print[Style["P\[OHat]vodn\[EAcute] s\[UAcute]radnice:", Bold], " [", 
                      FormatExactValue[x], ", ", FormatExactValue[y], "]"];
                
                If[smer == "v smere osi x",
                    (* Detailn\[YAcute] postup pre x-ov\[UAcute] s\[UAcute]radnicu pri skosen\[IAcute] v smere x *)
                    Print[Style["V\[YAcute]po\[CHacek]et x-ovej s\[UAcute]radnice:", Bold]];
                    Print["x' = x + k\[CenterDot]y = ", FormatExactValue[x], " + ", 
                          FormatExactValue[k], "\[CenterDot]", FormatExactValue[y]];
                    
                    (* Ak je v\[YAcute]raz komplexn\[YAcute], zobraz\[IAcute]me rozvinut\[YAcute] tvar *)
                    If[y != 0 && k != 0,
                        Print["x' = ", FormatExactValue[x], " + ", FormatExactValue[k*y]]
                    ];
                    
                    Print["x' = ", FormatExactValue[simplifiedX]];
                    
                    (* Detailn\[YAcute] postup pre y-ov\[UAcute] s\[UAcute]radnicu pri skosen\[IAcute] v smere x *)
                    Print[Style["V\[YAcute]po\[CHacek]et y-ovej s\[UAcute]radnice:", Bold]];
                    Print["y' = y = ", FormatExactValue[y]],
                    
                    (* Detailn\[YAcute] postup pre x-ov\[UAcute] s\[UAcute]radnicu pri skosen\[IAcute] v smere y *)
                    Print[Style["V\[YAcute]po\[CHacek]et x-ovej s\[UAcute]radnice:", Bold]];
                    Print["x' = x = ", FormatExactValue[x]];
                    
                    (* Detailn\[YAcute] postup pre y-ov\[UAcute] s\[UAcute]radnicu pri skosen\[IAcute] v smere y *)
                    Print[Style["V\[YAcute]po\[CHacek]et y-ovej s\[UAcute]radnice:", Bold]];
                    Print["y' = k\[CenterDot]x + y = ", FormatExactValue[k], "\[CenterDot]", 
                          FormatExactValue[x], " + ", FormatExactValue[y]];
                    
                    (* Ak je v\[YAcute]raz komplexn\[YAcute], zobraz\[IAcute]me rozvinut\[YAcute] tvar *)
                    If[x != 0 && k != 0,
                        Print["y' = ", FormatExactValue[k*x], " + ", FormatExactValue[y]]
                    ];
                    
                    Print["y' = ", FormatExactValue[simplifiedY]]
                ];
                
                (* V\[YAcute]sledn\[EAcute] s\[UAcute]radnice - plne rozvinut\[EAcute] a zjednodu\[SHacek]en\[EAcute] *)
                Print[Style["V\[YAcute]sledn\[EAcute] s\[UAcute]radnice:", Bold], " [", 
                      FormatExactValue[simplifiedX], ", ", FormatExactValue[simplifiedY], "]"];
                
                (* Overenie inverznej transform\[AAcute]cie *)
                Print[Style["\nOverenie pomocou inverznej transform\[AAcute]cie:", Bold]];
                
                If[smer == "v smere osi x",
                    Print["Pre inverzn\[EAcute] skosenie pou\[ZHacek]ijeme k' = -k = ", FormatExactValue[-k]];
                    Print["x = x' + k'\[CenterDot]y' = ", FormatExactValue[outputVertices[[i, 1]]], 
                          " + (", FormatExactValue[-k], ")\[CenterDot]", 
                          FormatExactValue[outputVertices[[i, 2]]]];
                          
                    (* Rozvineme v\[YAcute]po\[CHacek]et pre lep\[SHacek]iu \[CHacek]itate\:013enos\[THacek] *)
                    If[outputVertices[[i, 2]] != 0 && k != 0,
                        Print["x = ", FormatExactValue[outputVertices[[i, 1]]], " + (", 
                              FormatExactValue[-k*outputVertices[[i, 2]]], ")"]
                    ];
                    
                    Print["x = ", FormatExactValue[Simplify[outputVertices[[i, 1]] - k*outputVertices[[i, 2]]]], 
                          " = ", FormatExactValue[x]];
                    Print["y = y' = ", FormatExactValue[outputVertices[[i, 2]]], 
                          " = ", FormatExactValue[y]],
                    
                    Print["Pre inverzn\[EAcute] skosenie pou\[ZHacek]ijeme k' = -k = ", FormatExactValue[-k]];
                    Print["x = x' = ", FormatExactValue[outputVertices[[i, 1]]], 
                          " = ", FormatExactValue[x]];
                    Print["y = k'\[CenterDot]x' + y' = (", FormatExactValue[-k], ")\[CenterDot]", 
                          FormatExactValue[outputVertices[[i, 1]]], " + ", 
                          FormatExactValue[outputVertices[[i, 2]]]];
                          
                    (* Rozvineme v\[YAcute]po\[CHacek]et pre lep\[SHacek]iu \[CHacek]itate\:013enos\[THacek] *)
                    If[outputVertices[[i, 1]] != 0 && k != 0,
                        Print["y = ", FormatExactValue[-k*outputVertices[[i, 1]]], " + ", 
                              FormatExactValue[outputVertices[[i, 2]]]]
                    ];
                    
                    Print["y = ", FormatExactValue[Simplify[-k*outputVertices[[i, 1]] + outputVertices[[i, 2]]]], 
                          " = ", FormatExactValue[y]]
                ];
                
                (* Maticov\[YAcute] z\[AAcute]pis - s plne rozvinut\[YAcute]mi v\[YAcute]razmi *)
                Print[Style["\nMaticov\[YAcute] z\[AAcute]pis:", Bold]];
                If[smer == "v smere osi x",
                    Print[
                        Row[{
                            MatrixForm[{
                                {Style[1, Red], Style[k, Red]},
                                {Style[0, Red], Style[1, Red]}
                            }],
                            " \[CenterDot] ",
                            MatrixForm[{{x}, {y}}],
                            " = ",
                            MatrixForm[{{Style[simplifiedX, Red]}, {Style[simplifiedY, Red]}}]
                        }]
                    ],
                    Print[
                        Row[{
                            MatrixForm[{
                                {Style[1, Red], Style[0, Red]},
                                {Style[k, Red], Style[1, Red]}
                            }],
                            " \[CenterDot] ",
                            MatrixForm[{{x}, {y}}],
                            " = ",
                            MatrixForm[{{Style[simplifiedX, Red]}, {Style[simplifiedY, Red]}}]
                        }]
                    ]
                ];
            ],
            {i, 3}
        ];
        
        
        Print[Style["\nV\[CapitalYAcute]SLEDOK:", Bold, 14]];
        Print["Vrcholy trojuholn\[IAcute]ka po skosen\[IAcute]:"];
        
        (* Plne rozvinut\[AAcute] a zjednodu\[SHacek]en\[AAcute] matica v\[YAcute]sledku *)
        Module[{expandedOutputVertices, matrixToShow},
            (* Rozvinieme a zjednodu\[SHacek]\[IAcute]me ka\[ZHacek]d\[YAcute] prvok v\[YAcute]slednej matice *)
            expandedOutputVertices = Map[Expand, outputVertices, {2}];
            
            (* Vytvor\[IAcute]me pekne form\[AAcute]tovan\[UAcute] verziu matice na zobrazenie *)
            matrixToShow = Map[Style[#, Red] &, expandedOutputVertices, {2}];
            
            (* Zobraz\[IAcute]me upraven\[UAcute] maticu *)
            Print[MatrixForm[matrixToShow]];
        ];
        
        (* VIZUALIZ\[CapitalAAcute]CIA *)
        Print[Style["\nVIZUALIZ\[CapitalAAcute]CIA:", Bold]];
        Print[CreateVisualization[normalizedVertices, outputVertices, k, smer]];
        
        (* LEGENDA *)
        Print[Style["\nLEGENDA:", Bold, 14]];
        Print["\[Bullet] Modr\[EAcute] body a \[CHacek]iary: P\[OHat]vodn\[YAcute] trojuholn\[IAcute]k"];
        Print["\[Bullet] \[CapitalCHacek]erven\[EAcute] body a \[CHacek]iary: Skosen\[YAcute] trojuholn\[IAcute]k"];
        Print["\[Bullet] Zelen\[EAcute] preru\[SHacek]ovan\[EAcute] \[CHacek]iary: Dr\[AAcute]ha pohybu vrcholov pri skosen\[IAcute]"];
        Print["\[Bullet] Mrie\[ZHacek]ka: S\[UAcute]radnicov\[YAcute] syst\[EAcute]m"];
        
        Print[Style["\nP\[OHat]vodn\[EAcute] vrcholy (modr\[AAcute]):", Bold]];
        Table[
            Print[Row[{Style["Vrchol " <> FromCharacterCode[64 + i] <> ": ", 
                RGBColor[0.1, 0.1, 1]], normalizedVertices[[i]]}]], {i, 3}];
                
        Print[Style["\nV\[YAcute]sledn\[EAcute] vrcholy (\[CHacek]erven\[AAcute]):", Bold]];
        Module[{expandedOutput},
            Table[
                expandedOutput = Expand[outputVertices[[i]]];
                Print[Row[{Style["Vrchol " <> FromCharacterCode[64 + i] <> "': ", 
                    RGBColor[1, 0.1, 0.1]], expandedOutput}]], {i, 3}];
        ];
                
        Print[Style["\nInverzn\[AAcute] transform\[AAcute]cia:", Bold]];
        Print["Pre vr\[AAcute]tenie do p\[OHat]vodn\[EAcute]ho stavu pou\[ZHacek]ite koeficient skosenia:"];
        If[smer == "v smere osi x",
            Print["k' = -k = ", FormatExactValue[-k], " v smere osi x"],
            Print["k' = -k = ", FormatExactValue[-k], " v smere osi y"]
        ];
        
        (* MATEMATICK\[CapitalEAcute] VLASTNOSTI TRANSFORM\[CapitalAAcute]CIE *)
        Print[Style["\nMATEMATICK\[CapitalEAcute] VLASTNOSTI:", Bold, 14]];
        Print["\[Bullet] Skosenie je line\[AAcute]rna transform\[AAcute]cia, ktor\[AAcute] zachov\[AAcute]va rovnobe\[ZHacek]nos\[THacek] priamok s jednou z os\[IAcute]"];
        If[smer == "v smere osi x",
            Print["\[Bullet] Pri skosen\[IAcute] v smere osi x zost\[AAcute]vaj\[UAcute] zachovan\[EAcute] y-ov\[EAcute] s\[UAcute]radnice v\[SHacek]etk\[YAcute]ch bodov"];
            Print["\[Bullet] Zvisl\[EAcute] priamky (rovnobe\[ZHacek]n\[EAcute] s osou y) zost\[AAcute]vaj\[UAcute] zvisl\[EAcute]"],
            Print["\[Bullet] Pri skosen\[IAcute] v smere osi y zost\[AAcute]vaj\[UAcute] zachovan\[EAcute] x-ov\[EAcute] s\[UAcute]radnice v\[SHacek]etk\[YAcute]ch bodov"];
            Print["\[Bullet] Vodorovn\[EAcute] priamky (rovnobe\[ZHacek]n\[EAcute] s osou x) zost\[AAcute]vaj\[UAcute] vodorovn\[EAcute]"]
        ];
        Print["\[Bullet] Skosenie men\[IAcute] uhly a tvary geometrick\[YAcute]ch \[UAcute]tvarov, ale zachov\[AAcute]va ich obsah"];
        Print["\[Bullet] Body le\[ZHacek]iace na osi, ktor\[AAcute] je rovnobe\[ZHacek]n\[AAcute] so smerom skosenia, sa nemenia"];
        Print["\[Bullet] Skosenie je af\[IAcute]nna transform\[AAcute]cia, ktor\[AAcute] nezachov\[AAcute]va vzdialenosti ani uhly"];
        
        (* \[CapitalSHacek]peci\[AAcute]lne pr\[IAcute]pady *)
        Print[Style["\n\[CapitalSHacek]PECI\[CapitalAAcute]LNE PR\[CapitalIAcute]PADY:", Bold, 14]];
        Print["\[Bullet] Pre k = 0 je skosenie identickou transform\[AAcute]ciou (bod sa nemen\[IAcute])"];
        Print["\[Bullet] Pre ve\:013ek\[EAcute] hodnoty |k| sa \[UAcute]tvar v\[YAcute]razne skos\[IAcute] a m\[OHat]\[ZHacek]e by\[THacek] \[THacek]a\[ZHacek]\[SHacek]ie rozpoznate\:013en\[YAcute]"];
        Print["\[Bullet] Pre k = 1 sa vytvor\[IAcute] rovnoramenn\[YAcute] pravouhl\[YAcute] trojuholn\[IAcute]k z p\[OHat]vodn\[EAcute]ho \[SHacek]tvorca"];
        Print["\[Bullet] Pre k = -1 sa vytvor\[IAcute] rovnoramenn\[YAcute] pravouhl\[YAcute] trojuholn\[IAcute]k z p\[OHat]vodn\[EAcute]ho \[SHacek]tvorca (v opa\[CHacek]nom smere)"];
        
        (* Vr\[AAcute]tenie nov\[YAcute]ch vrcholov pre pr\[IAcute]padn\[EAcute] \[DHacek]al\[SHacek]ie transform\[AAcute]cie *)
        outputVertices
    ];
    
(* Defin\[IAcute]cia chybov\[YAcute]ch spr\[AAcute]v *)
TrojuholnikSkosenie::invalidInput = "Neplatn\[YAcute] vstup. O\[CHacek]ak\[AAcute]va sa matica alebo zoznam s\[UAcute]radn\[IAcute]c trojuholn\[IAcute]ka.";
TrojuholnikSkosenie::insufficientVertices = "Nedostato\[CHacek]n\[YAcute] po\[CHacek]et vrcholov. S\[UAcute] potrebn\[EAcute] aspo\[NHacek] 3 vrcholy.";

End[];
EndPackage[];  
