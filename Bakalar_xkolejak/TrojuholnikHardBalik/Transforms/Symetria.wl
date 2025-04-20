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


BeginPackage["TrojuholnikHardBalik`Transforms`Symetria`", {"TrojuholnikHardBalik`"}];

(* Export public symbols *)
TrojuholnikSymetria::usage = 
    "TrojuholnikSymetria[vertices_] zobraz\[IAcute] interakt\[IAcute]vny tutori\[AAcute]l pre symetriu trojuholn\[IAcute]ka pod\:013ea priamky a vr\[AAcute]ti nov\[EAcute] vrcholy.";

Begin["`Private`"];

(* Pomocn\[AAcute] funkcia na normaliz\[AAcute]ciu vstupn\[YAcute]ch s\[UAcute]radn\[IAcute]c *)
NormalizeVertices[vertices_] := 
    Module[{normalizedVertices, processedVertices},
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
                            Message[TrojuholnikSymetria::invalidInput]; 
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
                Message[TrojuholnikSymetria::invalidInput]; 
                Abort[]
        ];
        
        (* Zachovanie presnej aritmetiky  *)
        normalizedVertices = processedVertices;
        
        (* Kontrola po\[CHacek]tu vrcholov *)
        If[Length[normalizedVertices] < 3,
            Message[TrojuholnikSymetria::insufficientVertices];
            Abort[]
        ];
        
        (* Kontrola *)
        If[Max[Abs[N[normalizedVertices[[All, 1]]]]] > 10 || Max[Abs[N[normalizedVertices[[All, 2]]]]] > 10,
            (* \[CapitalSHacek]k\[AAcute]lovanie vrcholov, ak s\[UAcute] pr\[IAcute]li\[SHacek] \[DHacek]aleko, 
               ale zachov\[AAcute]me presn\[UAcute] aritmetiku *)
            normalizedVertices = normalizedVertices / (Max[Abs[N[Flatten[normalizedVertices]]]] / 6);
        ];
        
         
        
        (* N\[AAcute]vrat prv\[YAcute]ch troch vrcholov *)
        normalizedVertices[[1;;3]]
    ];


ReflectPointOverLine[point_, a_, b_, c_] := 
    Module[{x, y, d, xNew, yNew, factor},
        x = point[[1]];
        y = point[[2]];
        
        (* Vzdialenost' bodu od priamky - pou\[ZHacek]\[IAcute]vame presn\[EAcute] v\[YAcute]razy *)
        factor = a^2 + b^2;  (* Vyhneme sa odmocnine v menovateli *)
        d = (a*x + b*y + c);
        
        (* V\[YAcute]po\[CHacek]et nov\[YAcute]ch s\[UAcute]radn\[IAcute]c s presnou aritmetikou *)
        xNew = x - 2*a*d/factor;
        yNew = y - 2*b*d/factor;
        
        (* Zjednodu\[SHacek]enie v\[YAcute]razov, zachov\[AAcute]vame presn\[EAcute] hodnoty *)
        xNew = Simplify[xNew];
        yNew = Simplify[yNew];
        
        (* \[CapitalZHacek]iadne zaokr\[UAcute]h\:013eovanie, ponech\[AAcute]vame v\[YAcute]razy v presnej forme *)
        {xNew, yNew}
    ];


GenerateSymmetryLine[vertices_] := 
    Module[{a, b, c, validLine = False, allVertices, 
            lineEquation, lineType, popis, xIntercept, yIntercept, 
            testVertices, specialLines, invNewVertices, 
            attempts = 0, maxAttempts = 100, 
            simpleCoefficients, simpleIntercepts, 
            initialVertices, targetVertices, invTransform},
        
        (* Nastavenie premenn\[YAcute]ch *)
        allVertices = vertices;
        initialVertices = vertices;
        
        (* Pomocn\[AAcute] funkcia pre aplik\[AAcute]ciu inverznej transform\[AAcute]cie *)
        invTransform[points_, a_, b_, c_] := 
            Map[ReflectPointOverLine[#, a, b, c] &, points];
        
        (* Jednoduch\[EAcute] hodnoty koeficientov *)
        simpleCoefficients = {-3, -2, -1, 1, 2, 3};
        
        (* Jednoduch\[EAcute] hodnoty pre c *)
        simpleIntercepts = {-4, -3, -2, -1, 0, 1, 2, 3, 4};
        
        (* \[CapitalSHacek]peci\[AAcute]lne, zauj\[IAcute]mav\[EAcute] typy priamok *)
        specialLines = {
            (* Os X: y = 0 *)
            {0, 1, 0, "os x", "symetria pod\:013ea osi x"},
            
            (* Os Y: x = 0 *)
            {1, 0, 0, "os y", "symetria pod\:013ea osi y"},
            
            (* Priamka y = x *)
            {1, -1, 0, "priamka y = x", "symetria pod\:013ea priamky y = x"},
            
            (* Priamka y = -x *)
            {1, 1, 0, "priamka y = -x", "symetria pod\:013ea priamky y = -x"},
            
            (* Horizont\[AAcute]lne priamky: y = k *)
            {0, 1, -1, "priamka y = 1", "symetria pod\:013ea priamky y = 1"},
            {0, 1, -2, "priamka y = 2", "symetria pod\:013ea priamky y = 2"},
            {0, 1, 1, "priamka y = -1", "symetria pod\:013ea priamky y = -1"},
            
            (* Vertik\[AAcute]lne priamky: x = k *)
            {1, 0, -1, "priamka x = 1", "symetria pod\:013ea priamky x = 1"},
            {1, 0, -2, "priamka x = 2", "symetria pod\:013ea priamky x = 2"},
            {1, 0, 1, "priamka x = -1", "symetria pod\:013ea priamky x = -1"}
        };
        
        (* HLAVN\[CapitalYAcute] INVERZN\[CapitalYAcute] ALGORITMUS *)
        (* 1. Najprv vytvor\[IAcute]m n\[AAcute]hodn\[YAcute] cie\:013eov\[YAcute] trojuholn\[IAcute]k *)
        (* 2. Potom n\[AAcute]jdem symetriu, ktor\[AAcute] prevedie p\[OHat]vodn\[YAcute] na cie\:013eov\[YAcute] *)
        (* 3. Nakoniec over\[IAcute]m, \[ZHacek]e inverzn\[AAcute] transform\[AAcute]cia vr\[AAcute]ti spr\[AAcute]vne p\[OHat]vodn\[YAcute] trojuholn\[IAcute]k *)
        
        While[!validLine && attempts < maxAttempts,
            attempts++;
            
            (* Sk\[UAcute]sime zvoli\[THacek] parametre priamky n\[AAcute]hodne *)
            If[RandomReal[] < 0.25,
                (* \[CapitalSHacek]peci\[AAcute]lne priamky *)
                {a, b, c, lineType, popis} = RandomChoice[specialLines];
            ,
                (* V\[SHacek]eobecn\[EAcute] priamky s jednoduch\[YAcute]mi koeficientami *)
                a = RandomChoice[simpleCoefficients];
                b = RandomChoice[simpleCoefficients];
                c = RandomChoice[simpleIntercepts];
                
                (* Zabezpe\[CHacek]\[IAcute]me, \[ZHacek]e aspo\[NHacek] jeden z koeficientov a, b je nenulov\[YAcute] *)
                While[a == 0 && b == 0,
                    a = RandomChoice[simpleCoefficients];
                ];
                
                (* Zjednodu\[SHacek]enie zlomkov *)
                If[GCD[Abs[a], Abs[b], Abs[c]] > 1,
                    {a, b, c} = {a, b, c}/GCD[Abs[a], Abs[b], Abs[c]];
                ];
                
                (* Generovanie popisu priamky *)
                If[b != 0,
                    (* Priamka v tvare y = mx + n *)
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
                    (* Priamka rovnobe\[ZHacek]n\[AAcute] s osou y: x = k *)
                    lineEquation = "x = " <> ToString[-c/a];
                    lineType = "priamka rovnobe\[ZHacek]n\[AAcute] s osou y";
                ];
                
                popis = "symetria pod\:013ea priamky " <> lineEquation;
            ];
            
            (* INVERZN\[CapitalYAcute] ALGORITMUS:
               1. Aplikova\[THacek] transform\[AAcute]ciu na p\[OHat]vodn\[EAcute] vrcholy
               2. Overi\[THacek], \[CHacek]i v\[YAcute]sledky sp\:013a\[NHacek]aj\[UAcute] na\[SHacek]e krit\[EAcute]ri\[AAcute] (v rozumnom rozsahu, dostato\[CHacek]ne odli\[SHacek]n\[EAcute])
               3. Aplikova\[THacek] t\[UAcute] ist\[UAcute] transform\[AAcute]ciu e\[SHacek]te raz (symetria je svojou vlastnou inverziou)
               4. Overi\[THacek], \[ZHacek]e sme sa vr\[AAcute]tili do p\[OHat]vodn\[EAcute]ho stavu
            *)
            
            (* 1. Aplik\[AAcute]cia symetrie na p\[OHat]vodn\[EAcute] vrcholy *)
            targetVertices = Map[ReflectPointOverLine[#, a, b, c] &, initialVertices];
            
            (* 2. Kontrola validity v\[YAcute]sledkov *)
            validLine = True;
            
            (* Kontrola rozsahu s\[UAcute]radn\[IAcute]c - Pou\[ZHacek]\[IAcute]vame N[] len pre kontrolu,
               nie pre modifik\[AAcute]ciu hodn\[OHat]t *)
            validLine = validLine && AllTrue[targetVertices, Function[vertex,
                Abs[N[vertex[[1]]]] <= 14 && Abs[N[vertex[[2]]]] <= 14
            ]];
            
            (* Kontrola minim\[AAcute]lnej zmeny - trojuholn\[IAcute]ky s\[UAcute] dostato\[CHacek]ne odli\[SHacek]n\[EAcute] *)
            If[validLine, 
                validLine = Min[Flatten[Map[Function[{orig, target}, 
                    N[EuclideanDistance[orig, target]]], 
                    Transpose[{initialVertices, targetVertices}]]]] >= 1;
            ];
            
            (* 3. Aplik\[AAcute]cia inverznej transform\[AAcute]cie (pri symetrii je to t\[AAcute] ist\[AAcute] transform\[AAcute]cia) *)
            If[validLine,
                invNewVertices = invTransform[targetVertices, a, b, c];
                
                (* 4. Overenie, \[ZHacek]e inverzn\[AAcute] transform\[AAcute]cia vr\[AAcute]ti p\[OHat]vodn\[YAcute] trojuholn\[IAcute]k *)
                (* Porovn\[AAcute]vame s pou\[ZHacek]it\[IAcute]m numerick\[YAcute]ch hodn\[OHat]t len pre kontrolu *)
                validLine = Total[Flatten[Abs[N[initialVertices] - N[invNewVertices]]]] < 0.1;
            ];
        ];
        
        (* Ak po maxim\[AAcute]lnom po\[CHacek]te pokusov nem\[AAcute]me validn\[UAcute] priamku, pou\[ZHacek]ijeme jednoduch\[UAcute] *)
        If[!validLine,
            a = 1; b = 1; c = 0;  (* Priamka y = -x *)
            lineEquation = "y = -x";
            lineType = "priamka y = -x";
            popis = "symetria pod\:013ea priamky y = -x";
            targetVertices = Map[ReflectPointOverLine[#, a, b, c] &, initialVertices];
        ];
        
        (* V\[YAcute]po\[CHacek]et priese\[CHacek]n\[IAcute]kov s osami *)
        xIntercept = If[a != 0, -c/a, None];
        yIntercept = If[b != 0, -c/b, None];
        
        (* Toto bude na\[SHacek]a definit\[IAcute]vna os symetrie *)
        {a, b, c, lineType, popis, targetVertices, xIntercept, yIntercept}
    ];

(* Funkcia na vizualiz\[AAcute]ciu symetrie s ozna\[CHacek]en\[IAcute]m priamky p na kraji *)
CreateVisualization[originalVertices_, finalVertices_, a_, b_, c_, lineType_, xIntercept_, yIntercept_] := 
    Module[{allVertices, xMin, xMax, yMin, yMax, rangeBuffer, xRange, yRange, 
            labelOffsets, linePoints, slope, intercept, brightGreen, labelPoint},
            
        (* Defin\[IAcute]cia jasnej\[SHacek]ej zelenej farby *)
        brightGreen = RGBColor[0, 0.8, 0.2];
        
        (* Spojenie v\[SHacek]etk\[YAcute]ch vrcholov pre v\[YAcute]po\[CHacek]et rozsahu *)
        allVertices = Join[originalVertices, finalVertices];
        
        (* V\[YAcute]po\[CHacek]et minim\[AAcute]lnych a maxim\[AAcute]lnych hodn\[OHat]t - pou\[ZHacek]\[IAcute]vame N[] len pre v\[YAcute]po\[CHacek]et rozsahu *)
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
                
                (* Upravi\[THacek] offset pod\:013eea polohy vrcholov *)
                If[N[originalVertex[[1]]] > 8, offset[[1]] = -1.0];
                If[N[originalVertex[[2]]] > 8, offset[[2]] = -1.0];
                
                {offset, offset}
            ],
            {i, Length[originalVertices]}
        ];
        
        (* Priamka symetrie - v\[YAcute]po\[CHacek]et bodov pre vykreslenie *)
        linePoints = If[b != 0,
            (* Priamka y = mx + n *)
            slope = -a/b;
            intercept = -c/b;
            {{xRange[[1]], slope*xRange[[1]] + intercept}, 
             {xRange[[2]], slope*xRange[[2]] + intercept}},
             
            (* Priamka x = k *)
            {{-c/a, yRange[[1]]}, {-c/a, yRange[[2]]}}
        ];
        
        (* V\[YAcute]po\[CHacek]et bodu pre umiestnenie ozna\[CHacek]enia "p" na kraji *)
        labelPoint = If[b != 0,
            (* Pre \[SHacek]ikm\[EAcute] a horizont\[AAcute]lne priamky - na pravom okraji *)
            Module[{edgeX, edgeY, offset},
                (* Pou\[ZHacek]ijeme prav\[YAcute] okraj *)
                edgeX = xRange[[2]] - 1.5;
                edgeY = slope*edgeX + intercept;
                
                (* Posunutie pre ozna\[CHacek]enie - zvis\[EAcute] nad priamkou *)
                offset = {0, 1.0};
                
                (* Ak je slope kladn\[YAcute], d\[AAcute]me label pod \[CHacek]iaru *)
                If[slope > 0, offset = {0, -1.0}];
                
                (* Ak je priamka horizont\[AAcute]lna alebo takmer, d\[AAcute]me label vpravo *)
                If[Abs[slope] < 0.3, offset = {1.2, 0}];
                
                {edgeX, edgeY} + offset
            ],
            (* Pre vertik\[AAcute]lne priamky - na vrchnom okraji *)
            {-c/a + 0.7, yRange[[2]] - 1.5}
        ];
        
        Graphics[{
            (* Grid *)
            LightGray, Thin,
            Table[Line[{{xRange[[1]], y}, {xRange[[2]], y}}], 
                {y, Ceiling[yRange[[1]]], Floor[yRange[[2]]], 2}],
            Table[Line[{{x, yRange[[1]]}, {x, yRange[[2]]}}], 
                {x, Ceiling[xRange[[1]]], Floor[xRange[[2]]], 2}],
            
            (* P\[OHat]vodn\[YAcute] trojuholn\[IAcute]k - modr\[YAcute] *)
            Blue, Thick, Opacity[0.7],
            Line[Append[originalVertices, First[originalVertices]]],
            
            (* Symetrick\[YAcute] trojuholn\[IAcute]k - \[CHacek]erven\[YAcute] *)
            Red, Thick, Opacity[0.7],
            Line[Append[finalVertices, First[finalVertices]]],
            
            (* Os symetrie - zelen\[AAcute] hrub\[SHacek]ia \[CHacek]iara *)
            brightGreen, Thickness[0.005],
            Line[linePoints],
            
            (* Ozna\[CHacek]enie priamky p - v\[ADoubleDot]\[CHacek]\[SHacek]ie a v\[YAcute]raznej\[SHacek]ie *)
            (* Biely obd\:013a\[ZHacek]nik za p\[IAcute]smenom pre lep\[SHacek]iu vidite\:013enos\[THacek] *)
            White, Opacity[0.7], Rectangle[labelPoint - {0.4, 0.4}, labelPoint + {0.4, 0.4}],
            (* P\[IAcute]smeno p *)
            brightGreen, Opacity[1.0],
            Text[Style["p", Bold, 18, brightGreen], labelPoint],
            
            (* Pomocn\[EAcute] \[CHacek]iary ukazuj\[UAcute]ce symetriu - zelen\[EAcute] preru\[SHacek]ovan\[EAcute] *)
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
            
            (* Osi s\[UAcute]radnicovej s\[UAcute]stavy *)
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


FormatLineEquation[a_, b_, c_] := 
    Module[{eqn = ""},
        (* Vytvori\[THacek] rovnicu priamky vo forme ax + by + c = 0 *)
        If[a == 0 && b == 0,
            (* Neplatn\[AAcute] priamka *)
            Return["Neplatn\[AAcute] rovnica priamky"];
        ];
        
        (* \[CHacek]as\[THacek] s x *)
        If[a != 0,
            eqn = If[a == 1, "x", If[a == -1, "-x", ToString[a] <> "x"]];
        ];
        
        (* \[CHacek]as\[THacek] s y *)
        If[b != 0,
            If[a != 0, 
                eqn = eqn <> If[b > 0, " + ", " - "];
                eqn = eqn <> If[Abs[b] == 1, "y", ToString[Abs[b]] <> "y"],
                eqn = If[b == 1, "y", If[b == -1, "-y", ToString[b] <> "y"]]
            ];
        ];
        
        (* Kon\[SHacek]tantn\[AAcute] \[CHacek]as\[THacek] *)
        If[c != 0,
            eqn = eqn <> If[c > 0, " + ", " - "] <> ToString[Abs[c]];
        ];
        
        (* Dokon\[CHacek]enie rovnice *)
        eqn <> " = 0"
    ];

(* Funkcia na v\[YAcute]po\[CHacek]et prieseku kolmice s priamkou *)
CalculateIntersection[point_, a_, b_, c_] := 
    Module[{x0, y0, x1, y1, t},
        x0 = point[[1]];
        y0 = point[[2]];
        
        (* Parameter t pre priesek *)
        t = -(a*x0 + b*y0 + c)/(a^2 + b^2);
        
        (* S\[UAcute]radnice priesku *)
        x1 = x0 + a*t;
        y1 = y0 + b*t;
        
        (* Zachov\[AAcute]vame presn\[EAcute] hodnoty *)
        {Simplify[x1], Simplify[y1]}
    ];

(* Hlavn\[AAcute] funkcia *)
TrojuholnikSymetria[inputVertices_] := 
    Module[{normalizedVertices, symmetryParams, outputVertices, 
            a, b, c, lineType, popis, xIntercept, yIntercept, 
            intersections, distances, lineEqn},
            
        (* Normaliz\[AAcute]cia vstupn\[YAcute]ch s\[UAcute]radn\[IAcute]c - bez zaokr\[UAcute]h\:013eovania *)
        normalizedVertices = NormalizeVertices[inputVertices];
        
        (* Z\[IAcute]skanie parametrov symetrie *)
        symmetryParams = GenerateSymmetryLine[normalizedVertices];
        a = symmetryParams[[1]];
        b = symmetryParams[[2]];
        c = symmetryParams[[3]];
        lineType = symmetryParams[[4]];
        popis = symmetryParams[[5]];
        outputVertices = symmetryParams[[6]];
        xIntercept = symmetryParams[[7]];
        yIntercept = symmetryParams[[8]];
        
        (* V\[YAcute]po\[CHacek]et priesekov kolm\[IAcute]c s osou symetrie pre ka\[ZHacek]d\[YAcute] vrchol *)
        intersections = Map[CalculateIntersection[#, a, b, c] &, normalizedVertices];
        
        (* V\[YAcute]po\[CHacek]et vzdialnost\[IAcute] medzi p\[OHat]vodn\[YAcute]mi bodmi a priesekmi *)
        distances = MapThread[
            EuclideanDistance[#1, #2] &, 
            {normalizedVertices, intersections}
        ];
        
        (* Form\[AAcute]tovanie rovnice priamky *)
        lineEqn = FormatLineEquation[a, b, c];
        
        (* Nadpis *)
        Print[Style[
            "GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE TROJUHOLN\[CapitalIAcute]KA - SYMETRIA", 
            Bold, 16]];
        Print[Style["========================================", Bold]];
        
        (* ZADANIE *)
        Print[Style["\nZADANIE:", Bold, 14]];
        Print["Majme trojuholn\[IAcute]k ABC s vrcholmi:"];
        Print[MatrixForm[Map[Style[#, Blue] &, normalizedVertices, {2}]]];
        
        Print["\nVykonajte symetriu v 2D priestore pod\:013ea priamky ", Style[lineEqn, RGBColor[0, 0.8, 0.2], Bold], "."]
        
        (* Transforma\[CHacek]n\[AAcute] matica symetrie *)
        Module[{M},
            (* V\[YAcute]po\[CHacek]et transforma\[CHacek]nej matice *)
            M = {{(b^2 - a^2)/(a^2 + b^2), -2*a*b/(a^2 + b^2)}, 
                 {-2*a*b/(a^2 + b^2), (a^2 - b^2)/(a^2 + b^2)}};
            
            
            
            Print["\nTransforma\[CHacek]n\[AAcute] matica symetrie:"];
            Print[MatrixForm[Map[Style[#, Red] &, M, {2}]]];
            
            If[a == 0, (* Os x *)
                Print["\nPre os x (y = 0) je transforma\[CHacek]n\[AAcute] matica:"];
                Print[MatrixForm[{
                    {Style[1, Red], Style[0, Red]}, 
                    {Style[0, Red], Style[-1, Red]}
                }]];
            ];
            
            If[b == 0, (* Os y *)
                Print["\nPre os y (x = 0) je transforma\[CHacek]n\[AAcute] matica:"];
                Print[MatrixForm[{
                    {Style[-1, Red], Style[0, Red]}, 
                    {Style[0, Red], Style[1, Red]}
                }]];
            ];
            
            If[a == b && a != 0, (* Priamka y = x *)
                Print["\nPre priamku y = x je transforma\[CHacek]n\[AAcute] matica:"];
                Print[MatrixForm[{
                    {Style[0, Red], Style[1, Red]}, 
                    {Style[1, Red], Style[0, Red]}
                }]];
            ];
            
            If[a == -b && a != 0, (* Priamka y = -x *)
                Print["\nPre priamku y = -x je transforma\[CHacek]n\[AAcute] matica:"];
                Print[MatrixForm[{
                    {Style[0, Red], Style[-1, Red]}, 
                    {Style[-1, Red], Style[0, Red]}
                }]];
            ];
        ];
        
        (* POSTUP *)
        Print[Style["\nPOSTUP:", Bold, 14]];
        Print["Pre v\[YAcute]po\[CHacek]et osovej symetrie pod\:013ea priamky ", Style[lineEqn, RGBColor[0, 0.8, 0.2], Bold], " pou\[ZHacek]ijeme vzorce:"];
        
        Print["\nV\[SHacek]eobecn\[YAcute] vzorec pre v\[YAcute]po\[CHacek]et symetrie bodu [x, y] pod\:013ea priamky ax + by + c = 0:"];
        Print["x' = x - 2a(ax + by + c)/(a\.b2 + b\.b2)"];
        Print["y' = y - 2b(ax + by + c)/(a\.b2 + b\.b2)"];
        
        Print["\nNa\[SHacek]e hodnoty koeficientov:"];
        Print["a = ", a];
        Print["b = ", b];
        Print["c = ", c];
        
        Print["\nPostup v\[YAcute]po\[CHacek]tu:"];
        Print["1. V\[YAcute]po\[CHacek]et v\[YAcute]razu (ax + by + c)/(a\.b2 + b\.b2) pre ka\[ZHacek]d\[YAcute] bod"];
        Print["2. Vyn\[AAcute]sobenie tohto v\[YAcute]razu koeficientom 2a pre x-ov\[UAcute] a 2b pre y-ov\[UAcute] s\[UAcute]radnicu"];
        Print["3. Od\[CHacek]\[IAcute]tanie v\[YAcute]sledku od p\[OHat]vodn\[YAcute]ch s\[UAcute]radn\[IAcute]c x a y"];
        
        Print["\nV\[YAcute]po\[CHacek]et pre jednotliv\[EAcute] vrcholy:"];
        
        Print["\nRovnica osi symetrie: ", lineEqn];
        Print["Normal\[AAcute]lov\[YAcute] vektor osi symetrie: (", a, ", ", b, ")"];
        
        (* Zvo\:013ete vhodn\[YAcute] popis pre r\[OHat]zne typy os symetrie *)
        Print["\nNa\[SHacek]a os symetrie je " <> lineType <> "." 
            <> If[xIntercept =!= None && yIntercept =!= None, 
                " Priesek s osou x: [" <> ToString[xIntercept] <> ", 0], " 
                <> "priesek s osou y: [0, " <> ToString[yIntercept] <> "]", 
                ""]
        ];
        
        (* V\[YAcute]po\[CHacek]et pre jednotliv\[EAcute] vrcholy *)
        Table[
            Module[{x = normalizedVertices[[i, 1]], 
                    y = normalizedVertices[[i, 2]],
                    newX = outputVertices[[i, 1]], 
                    newY = outputVertices[[i, 2]],
                    intersection = intersections[[i]],
                    distance = distances[[i]]},
                    
                Print["\nVrchol ", FromCharacterCode[64 + i], ":"];
                Print[Style["P\[OHat]vodn\[EAcute] s\[UAcute]radnice:", Bold], " [", x, ", ", y, "]"];
                
                (* Detailn\[YAcute] postup pre symetriu *)
                Print[Style["V\[YAcute]po\[CHacek]et symetrie bodu:", Bold]];
                
                (* Detailn\[YAcute] postup pre x-ov\[UAcute] s\[UAcute]radnicu *)
                Print[Style["V\[YAcute]po\[CHacek]et x-ovej s\[UAcute]radnice:", Bold]];
                Print["x' = x - 2a(ax + by + c)/(a\.b2 + b\.b2)"];
                Print["V tejto rovnici dop\[IAcute]\[SHacek]eme na\[SHacek]e hodnoty:"];
                Print["x' = ", x, " - 2\[CenterDot]", a, "\[CenterDot](", a, "\[CenterDot]", x, " + ", b, "\[CenterDot]", y, " + ", c, ")/(", a, "\.b2 + ", b, "\.b2)"];
                
                Module[{expr, denominator, numerator, simplifiedExpr},
                    (* V\[YAcute]po\[CHacek]et v\[YAcute]razu v z\[AAcute]tvorke *)
                    expr = a*x + b*y + c;
                    Print["x' = ", x, " - 2\[CenterDot]", a, "\[CenterDot](", expr, ")/(", a^2, " + ", b^2, ")"];
                    
                    denominator = a^2 + b^2;
                    (* Vyp\[IAcute]sa\[THacek] vz\[THacek]ah v zlomkovom tvare *)
                    Print["x' = ", x, " - 2\[CenterDot]", a, "\[CenterDot]", expr, "/", denominator];
                    
                    (* N\[AAcute]sobenie *)
                    numerator = 2*a*expr;
                    Print["x' = ", x, " - ", numerator, "/", denominator];
                    
                    (* Zjednodu\[SHacek]enie zlomku ak je to mo\[ZHacek]n\[EAcute] *)
                    If[IntegerQ[numerator] && IntegerQ[denominator] && GCD[Abs[numerator], denominator] > 1,
                        Block[{gcd = GCD[Abs[numerator], denominator]},
                            numerator = numerator/gcd;
                            denominator = denominator/gcd;
                            Print["x' = ", x, " - ", numerator, "/", denominator, " (po zjednodu\[SHacek]en\[IAcute] zlomku)"];
                        ]
                    ];
                    
                    (* Od\[CHacek]\[IAcute]tanie zlomku od cel\[EAcute]ho \[CHacek]\[IAcute]sla *)
                    If[denominator != 1,
                        simplifiedExpr = x - numerator/denominator;
                        Print["x' = ", simplifiedExpr];
                    ];
                    
                    Print["x' = ", newX];
                ];
                
                (* Detailn\[YAcute] postup pre y-ov\[UAcute] s\[UAcute]radnicu *)
                Print[Style["V\[YAcute]po\[CHacek]et y-ovej s\[UAcute]radnice:", Bold]];
                Print["y' = y - 2b(ax + by + c)/(a\.b2 + b\.b2)"];
                Print["V tejto rovnici dop\[IAcute]\[SHacek]eme na\[SHacek]e hodnoty:"];
                Print["y' = ", y, " - 2\[CenterDot]", b, "\[CenterDot](", a, "\[CenterDot]", x, " + ", b, "\[CenterDot]", y, " + ", c, ")/(", a, "\.b2 + ", b, "\.b2)"];
                
                Module[{expr, denominator, numerator, simplifiedExpr},
                    (* V\[YAcute]po\[CHacek]et v\[YAcute]razu v z\[AAcute]tvorke *)
                    expr = a*x + b*y + c;
                    Print["y' = ", y, " - 2\[CenterDot]", b, "\[CenterDot](", expr, ")/(", a^2, " + ", b^2, ")"];
                    
                    denominator = a^2 + b^2;
                    (* Vyp\[IAcute]sa\[THacek] vz\[THacek]ah v zlomkovom tvare *)
                    Print["y' = ", y, " - 2\[CenterDot]", b, "\[CenterDot]", expr, "/", denominator];
                    
                    (* N\[AAcute]sobenie *)
                    numerator = 2*b*expr;
                    Print["y' = ", y, " - ", numerator, "/", denominator];
                    
                    (* Zjednodu\[SHacek]enie zlomku ak je to mo\[ZHacek]n\[EAcute] *)
                    If[IntegerQ[numerator] && IntegerQ[denominator] && GCD[Abs[numerator], denominator] > 1,
                        Block[{gcd = GCD[Abs[numerator], denominator]},
                            numerator = numerator/gcd;
                            denominator = denominator/gcd;
                            Print["y' = ", y, " - ", numerator, "/", denominator, " (po zjednodu\[SHacek]en\[IAcute] zlomku)"];
                        ]
                    ];
                    
                    (* Od\[CHacek]\[IAcute]tanie zlomku od cel\[EAcute]ho \[CHacek]\[IAcute]sla *)
                    If[denominator != 1,
                        simplifiedExpr = y - numerator/denominator;
                        Print["y' = ", simplifiedExpr];
                    ];
                    
                    Print["y' = ", newY];
                ];
                
                (* Maticov\[YAcute] z\[AAcute]pis *)
                Print[Style["\nMaticov\[YAcute] z\[AAcute]pis:", Bold]];
                
                Module[{M, d, simplifiedM},
                    (* Z\[IAcute]skanie \[CHacek]istej\[SHacek]\[IAcute]ch hodn\[OHat]t pre maticu *)
                    simplifiedM = {
                        {(b^2 - a^2)/(a^2 + b^2), -2*a*b/(a^2 + b^2)},
                        {-2*a*b/(a^2 + b^2), (a^2 - b^2)/(a^2 + b^2)}
                    };
                    
                    (* Vektor posunutia pre osov\[UAcute] s\[UAcute]mernos\[THacek] *)
                    d = {-2*a*c/(a^2 + b^2), -2*b*c/(a^2 + b^2)};
                    
                    (* Zobrazenie matice n\[AAcute]sobenia *)
                    Print[
                        Row[{
                            MatrixForm[Map[Style[#, Red] &, simplifiedM, {2}]],
                            " \[CenterDot] ",
                            MatrixForm[{{x}, {y}}],
                            If[c != 0, 
                               Row[{" + ", MatrixForm[Map[Style[#, Red] &, d]]}], 
                               ""],
                            " = ",
                            MatrixForm[{{newX}, {newY}}]
                        }]
                    ];
                ];
                
                (* Overenie v\[YAcute]po\[CHacek]tu *)
                Print[Style["\nOverenie:", Bold]];
                Print["Ak apliku-jeme symetriu e\[SHacek]te raz na v\[YAcute]sledn\[YAcute] bod [", newX, ", ", newY, "], dostaneme p\[OHat]vodn\[YAcute] bod [", x, ", ", y, "]"];
            ],
            {i, 3}
        ];
        
        (* V\[CapitalYAcute]SLEDOK *)
        Print[Style["\nV\[CapitalYAcute]SLEDOK:", Bold, 14]];
        Print["Vrcholy trojuholn\[IAcute]ka po symetrii:"];
        Print[MatrixForm[Map[Style[#, Red] &, outputVertices, {2}]]];
        
        (* VIZUALIZ\[CapitalAAcute]CIA *)
        Print[Style["\nVIZUALIZ\[CapitalAAcute]CIA:", Bold]];
        Print[CreateVisualization[normalizedVertices, outputVertices, a, b, c, lineType, xIntercept, yIntercept]];
        
        (* LEGENDA *)
        Print[Style["\nLEGENDA:", Bold, 14]];
        Print["\[Bullet] Modr\[EAcute] body a \[CHacek]iary: P\[OHat]vodn\[YAcute] trojuholn\[IAcute]k"];
        Print["\[Bullet] \[CapitalCHacek]erven\[EAcute] body a \[CHacek]iary: Trojuholn\[IAcute]k po symetrii"];
        Print["\[Bullet] Zelen\[AAcute] priamka: Os symetrie"];
        Print["\[Bullet] Zelen\[EAcute] preru\[SHacek]ovan\[EAcute] \[CHacek]iary: Spojenia zodpovedaj\[UAcute]cich si vrcholov"];
        Print["\[Bullet] Mrie\[ZHacek]ka: S\[UAcute]radnicov\[YAcute] syst\[EAcute]m"];
        
        Print[Style["\nP\[OHat]vodn\[EAcute] vrcholy (modr\[AAcute]):", Bold]];
        Table[
            Print[Row[{Style["Vrchol " <> FromCharacterCode[64 + i] <> ": ", 
                RGBColor[0.1, 0.1, 1]], normalizedVertices[[i]]}]], {i, 3}];
                
        Print[Style["\nV\[YAcute]sledn\[EAcute] vrcholy (\[CHacek]erven\[AAcute]):", Bold]];
        Table[
            Print[Row[{Style["Vrchol " <> FromCharacterCode[64 + i] <> "': ", 
                RGBColor[1, 0.1, 0.1]], outputVertices[[i]]}]], {i, 3}];
                
        (* MATEMATICK\[CapitalEAcute] VLASTNOSTI TRANSFORM\[CapitalAAcute]CIE *)
        Print[Style["\nMATEMATICK\[CapitalEAcute] VLASTNOSTI:", Bold, 14]];
        Print["\[Bullet] Symetria pod\:013ea priamky je izometrick\[AAcute] transform\[AAcute]cia - zachov\[AAcute]va vzdialenosti medzi bodmi"];
        Print["\[Bullet] Symetria zachov\[AAcute]va ve\:013ekos\[THacek] a tvar geometrick\[YAcute]ch \[UAcute]tvarov"];
        Print["\[Bullet] Symetria zachov\[AAcute]va uhly medzi \[UAcute]se\[CHacek]kami a ich d\:013a\[ZHacek]ky"];
        Print["\[Bullet] Symetria zachov\[AAcute]va obsahy a obvody"];
        Print["\[Bullet] Body le\[ZHacek]iace na osi symetrie zost\[AAcute]vaj\[UAcute] po symetrii nezmenen\[EAcute]"];
        Print["\[Bullet] Aplikov\[AAcute]n\[IAcute]m symetrie dvakr\[AAcute]t z\[IAcute]skame p\[OHat]vodn\[YAcute] obraz - je to transform\[AAcute]cia r\[AAcute]du 2"];
        Print["\[Bullet] Spojnica bodu a jeho obrazu je v\[ZHacek]dy kolm\[AAcute] na os symetrie"];
        Print["\[Bullet] Stred spojnice bodu a jeho obrazu v\[ZHacek]dy le\[ZHacek]\[IAcute] na osi symetrie"];
        
        (* V\[YAcute]znamn\[EAcute] typy os\[IAcute] symetrie *)
        Print[Style["\nV\[CapitalYAcute]ZNAMN\[CapitalEAcute] OSI SYMETRIE:", Bold, 14]];
        Print["\[Bullet] Os x (y = 0): bod [x, y] sa zobraz\[IAcute] na [x, -y]"];
        Print["\[Bullet] Os y (x = 0): bod [x, y] sa zobraz\[IAcute] na [-x, y]"];
        Print["\[Bullet] Priamka y = x: bod [x, y] sa zobraz\[IAcute] na [y, x]"];
        Print["\[Bullet] Priamka y = -x: bod [x, y] sa zobraz\[IAcute] na [-y, -x]"];
                
        (* Vr\[AAcute]tenie nov\[YAcute]ch vrcholov pre pr\[IAcute]padn\[EAcute] \[DHacek]al\[SHacek]ie transform\[AAcute]cie *)
        outputVertices
    ];

(* Defin\[IAcute]cia chybov\[YAcute]ch spr\[AAcute]v *)
TrojuholnikSymetria::invalidInput = "Neplatn\[YAcute] vstup. O\[CHacek]ak\[AAcute]va sa matica alebo zoznam s\[UAcute]radn\[IAcute]c trojuholn\[IAcute]ka.";
TrojuholnikSymetria::insufficientVertices = "Nedostato\[CHacek]n\[YAcute] po\[CHacek]et vrcholov. S\[UAcute] potrebn\[EAcute] aspo\[NHacek] 3 vrcholy.";

End[];
EndPackage[];
