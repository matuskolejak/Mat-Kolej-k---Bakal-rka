(* ::Package:: *)

(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


BeginPackage["BodHardBalik`Transforms`Symetria`", {"BodHardBalik`"}];

(* Export public symbols *)
BodSymetria::usage = 
    "BodSymetria[coordinates_] zobraz\[IAcute] interakt\[IAcute]vny tutori\[AAcute]l pre symetriu bodu pod\:013ea priamky a vr\[AAcute]ti nov\[EAcute] s\[UAcute]radnice.";

Begin["`Private`"];

(* Pomocn\[AAcute] funkcia na normaliz\[AAcute]ciu vstupn\[YAcute]ch s\[UAcute]radn\[IAcute]c *)
NormalizeCoordinates[coordinates_] := 
    Module[{normalizedCoordinates, processedCoordinates},
        (* Spracovanie r\[OHat]znych vstupn\[YAcute]ch form\[AAcute]tov *)
        processedCoordinates = Which[
            (* Ak ide o vektor d\[IAcute]\[ZHacek]ky 2 *)
            VectorQ[coordinates] && Length[coordinates] == 2, coordinates,
            
            (* Ak ide o zoznam s jednou polo\[ZHacek]kou, ktor\[AAcute] je vektor d\[IAcute]\[ZHacek]ky 2 *)
            ListQ[coordinates] && Length[coordinates] == 1 && VectorQ[coordinates[[1]]] && 
                Length[coordinates[[1]]] == 2, coordinates[[1]],
            
            (* Ak ide o zoznam s dvoma \[CHacek]\[IAcute]slami *)
            ListQ[coordinates] && Length[coordinates] == 2 && 
                (NumberQ[coordinates[[1]]] || Head[coordinates[[1]]] === Rational) && 
                (NumberQ[coordinates[[2]]] || Head[coordinates[[2]]] === Rational), coordinates,
            
            (* Inak *)
            True, 
                Message[BodSymetria::invalidInput]; 
                Abort[]
        ];
        
        (* Zachovanie presnej aritmetiky *)
        normalizedCoordinates = processedCoordinates;
        
        (* Kontrola *)
        If[Max[Abs[N[normalizedCoordinates]]] > 10,
            (* \[CapitalSHacek]k\[AAcute]lovanie s\[UAcute]radn\[IAcute]c, ak s\[UAcute] pr\[IAcute]li\[SHacek] \[DHacek]aleko, 
               ale zachov\[AAcute]me presn\[UAcute] aritmetiku *)
            normalizedCoordinates = normalizedCoordinates / (Max[Abs[N[normalizedCoordinates]]] / 6);
        ];
        
        (* N\[AAcute]vrat normalizovan\[YAcute]ch s\[UAcute]radn\[IAcute]c *)
        normalizedCoordinates
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


GenerateSymmetryLine[coordinates_] := 
    Module[{a, b, c, validLine = False, 
            lineEquation, lineType, popis, xIntercept, yIntercept, 
            testCoordinates, specialLines, invNewCoordinates, 
            attempts = 0, maxAttempts = 100, 
            simpleCoefficients, simpleIntercepts, 
            initialCoordinates, targetCoordinates, invTransform},
        
        (* Nastavenie premenn\[YAcute]ch *)
        initialCoordinates = coordinates;
        
        (* Pomocn\[AAcute] funkcia pre aplik\[AAcute]ciu inverznej transform\[AAcute]cie *)
        invTransform[point_, a_, b_, c_] := ReflectPointOverLine[point, a, b, c];
        
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
        (* 1. Najprv vytvor\[IAcute]m n\[AAcute]hodn\[YAcute] cie\:013eov\[YAcute] bod *)
        (* 2. Potom n\[AAcute]jdem symetriu, ktor\[AAcute] prevedie p\[OHat]vodn\[YAcute] na cie\:013eov\[YAcute] *)
        (* 3. Nakoniec over\[IAcute]m, \[ZHacek]e inverzn\[AAcute] transform\[AAcute]cia vr\[AAcute]ti spr\[AAcute]vne p\[OHat]vodn\[YAcute] bod *)
        
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
               1. Aplikova\[THacek] transform\[AAcute]ciu na p\[OHat]vodn\[YAcute] bod
               2. Overi\[THacek], \[CHacek]i v\[YAcute]sledok sp\:013a\[NHacek]a na\[SHacek]e krit\[EAcute]ri\[AAcute] (v rozumnom rozsahu, dostato\[CHacek]ne odli\[SHacek]n\[YAcute])
               3. Aplikova\[THacek] t\[UAcute] ist\[UAcute] transform\[AAcute]ciu e\[SHacek]te raz (symetria je svojou vlastnou inverziou)
               4. Overi\[THacek], \[ZHacek]e sme sa vr\[AAcute]tili do p\[OHat]vodn\[EAcute]ho stavu
            *)
            
            (* 1. Aplik\[AAcute]cia symetrie na p\[OHat]vodn\[YAcute] bod *)
            targetCoordinates = ReflectPointOverLine[initialCoordinates, a, b, c];
            
            (* 2. Kontrola validity v\[YAcute]sledku *)
            validLine = True;
            
            (* Kontrola rozsahu s\[UAcute]radn\[IAcute]c - Pou\[ZHacek]\[IAcute]vame N[] len pre kontrolu,
               nie pre modifik\[AAcute]ciu hodn\[OHat]t *)
            validLine = validLine && 
                Abs[N[targetCoordinates[[1]]]] <= 14 && Abs[N[targetCoordinates[[2]]]] <= 14;
            
            (* Kontrola minim\[AAcute]lnej zmeny - body s\[UAcute] dostato\[CHacek]ne odli\[SHacek]n\[EAcute] *)
            If[validLine, 
                validLine = N[EuclideanDistance[initialCoordinates, targetCoordinates]] >= 1;
            ];
            
            (* 3. Aplik\[AAcute]cia inverznej transform\[AAcute]cie (pri symetrii je to t\[AAcute] ist\[AAcute] transform\[AAcute]cia) *)
            If[validLine,
                invNewCoordinates = invTransform[targetCoordinates, a, b, c];
                
                (* 4. Overenie, \[ZHacek]e inverzn\[AAcute] transform\[AAcute]cia vr\[AAcute]ti p\[OHat]vodn\[YAcute] bod *)
                (* Porovn\[AAcute]vame s pou\[ZHacek]it\[IAcute]m numerick\[YAcute]ch hodn\[OHat]t len pre kontrolu *)
                validLine = Total[Abs[N[initialCoordinates] - N[invNewCoordinates]]] < 0.1;
            ];
        ];
        
        (* Ak po maxim\[AAcute]lnom po\[CHacek]te pokusov nem\[AAcute]me validn\[UAcute] priamku, pou\[ZHacek]ijeme jednoduch\[UAcute] *)
        If[!validLine,
            a = 1; b = 1; c = 0;  (* Priamka y = -x *)
            lineEquation = "y = -x";
            lineType = "priamka y = -x";
            popis = "symetria pod\:013ea priamky y = -x";
            targetCoordinates = ReflectPointOverLine[initialCoordinates, a, b, c];
        ];
        
        (* V\[YAcute]po\[CHacek]et priese\[CHacek]n\[IAcute]kov s osami *)
        xIntercept = If[a != 0, -c/a, None];
        yIntercept = If[b != 0, -c/b, None];
        
        (* Toto bude na\[SHacek]a definit\[IAcute]vna os symetrie *)
        {a, b, c, lineType, popis, targetCoordinates, xIntercept, yIntercept}
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

(* Funkcia na vizualiz\[AAcute]ciu symetrie s ozna\[CHacek]en\[IAcute]m priamky p na kraji *)
CreateVisualization[originalCoordinates_, finalCoordinates_, a_, b_, c_, lineType_, xIntercept_, yIntercept_] := 
    Module[{allCoordinates, xMin, xMax, yMin, yMax, rangeBuffer, xRange, yRange, 
            labelOffsets, linePoints, slope, intercept, brightGreen, labelPoint},
            
        (* Defin\[IAcute]cia jasnej\[SHacek]ej zelenej farby *)
        brightGreen = RGBColor[0, 0.8, 0.2];
        
        (* Spojenie v\[SHacek]etk\[YAcute]ch s\[UAcute]radn\[IAcute]c pre v\[YAcute]po\[CHacek]et rozsahu *)
        allCoordinates = {originalCoordinates, finalCoordinates};
        
        (* V\[YAcute]po\[CHacek]et minim\[AAcute]lnych a maxim\[AAcute]lnych hodn\[OHat]t - pou\[ZHacek]\[IAcute]vame N[] len pre v\[YAcute]po\[CHacek]et rozsahu *)
        xMin = Min[N[allCoordinates[[All, 1]]]];
        xMax = Max[N[allCoordinates[[All, 1]]]];
        yMin = Min[N[allCoordinates[[All, 2]]]];
        yMax = Max[N[allCoordinates[[All, 2]]]];
        
        (* Buffer pre estetick\[YAcute] vzh\:013ead *)
        rangeBuffer = 2;
        
        (* Nastavenie rozsahu os\[IAcute] *)
        xRange = {Min[-11, xMin - rangeBuffer], Max[11, xMax + rangeBuffer]};
        yRange = {Min[-11, yMin - rangeBuffer], Max[11, yMax + rangeBuffer]};
        
        (* Vypo\[CHacek]\[IAcute]ta\[THacek] offset pre ozna\[CHacek]enie, aby sa neprekr\[YAcute]valo s \[CHacek]iarou *)
        labelOffsets = 
            Module[{originalCoord = originalCoordinates, 
                    finalCoord = finalCoordinates, 
                    offset = {0.7, 0.7}},
                
                (* Upravi\[THacek] offset pod\:013ea polohy s\[UAcute]radn\[IAcute]c *)
                If[N[originalCoord[[1]]] > 8, offset[[1]] = -1.0];
                If[N[originalCoord[[2]]] > 8, offset[[2]] = -1.0];
                
                {offset, offset}
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
            Line[{originalCoordinates, finalCoordinates}],
            
            (* Body s bielym pozad\[IAcute]m - zv\[ADoubleDot]\[CHacek]\[SHacek]en\[EAcute] *)
            White, 
            Disk[originalCoordinates, 0.45],
            Disk[finalCoordinates, 0.45],
            
            (* Body - zv\[ADoubleDot]\[CHacek]\[SHacek]en\[EAcute] *)
            Blue, PointSize[0.025], Point[originalCoordinates],
            Red, PointSize[0.025], Point[finalCoordinates],
            
            (* Labels pre s\[UAcute]radnice bodov *)
            Text[Style["P", Blue, Bold, 16], 
                  originalCoordinates + labelOffsets[[1]]],
            Text[Style["P'", Red, Bold, 16], 
                  finalCoordinates + labelOffsets[[2]]],
            
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

(* Hlavn\[AAcute] funkcia *)
BodSymetria[inputCoordinates_] := 
    Module[{normalizedCoordinates, symmetryParams, outputCoordinates, 
            a, b, c, lineType, popis, xIntercept, yIntercept, 
            intersection, distance, lineEqn},
            
        (* D\[CapitalOHat]LE\[CapitalZHacek]IT\[CapitalAAcute] OPRAVA: Zvl\[AAcute]dnu\[THacek] v\[SHacek]etky mo\[ZHacek]n\[EAcute] form\[AAcute]ty vstupu vr\[AAcute]tane v\[YAcute]sledkov z predch\[AAcute]dzaj\[UAcute]cej transform\[AAcute]cie *)
        Which[
            (* Ak je to samostatn\[YAcute] vektor, pou\[ZHacek]ijeme ho priamo *)
            VectorQ[inputCoordinates] && Length[inputCoordinates] == 2, 
                normalizedCoordinates = inputCoordinates,
            
            (* Ak je to zoznam s jedn\[YAcute]m vektorom *)
            ListQ[inputCoordinates] && Length[inputCoordinates] == 1 && 
                VectorQ[inputCoordinates[[1]]] && Length[inputCoordinates[[1]]] == 2,
                normalizedCoordinates = inputCoordinates[[1]],
            
            (* Ak je to zoznam \[CHacek]\[IAcute]sel, sk\[UAcute]sime ho interpretova\[THacek] ako s\[UAcute]radnice *)
            ListQ[inputCoordinates] && VectorQ[Flatten[inputCoordinates]], 
                If[EvenQ[Length[Flatten[inputCoordinates]]],
                    (* Ak m\[AAcute]me p\[AAcute]rny po\[CHacek]et \[CHacek]\[IAcute]sel, berieme posledn\[EAcute] dve ako aktu\[AAcute]lne s\[UAcute]radnice *)
                    normalizedCoordinates = Take[Flatten[inputCoordinates], -2],
                    (* Inak berieme prv\[EAcute] dve *)
                    normalizedCoordinates = Take[Flatten[inputCoordinates], 2]
                ],
            
            (* Ak\[YAcute]ko\:013evek in\[YAcute] pr\[IAcute]pad *)
            True, 
                normalizedCoordinates = NormalizeCoordinates[inputCoordinates]
        ];
        
        (* Z\[IAcute]skanie parametrov symetrie *)
        symmetryParams = GenerateSymmetryLine[normalizedCoordinates];
        a = symmetryParams[[1]];
        b = symmetryParams[[2]];
        c = symmetryParams[[3]];
        lineType = symmetryParams[[4]];
        popis = symmetryParams[[5]];
        outputCoordinates = symmetryParams[[6]];
        xIntercept = symmetryParams[[7]];
        yIntercept = symmetryParams[[8]];
        
        (* V\[YAcute]po\[CHacek]et prieseku kolmice s osou symetrie *)
        intersection = CalculateIntersection[normalizedCoordinates, a, b, c];
        
        (* V\[YAcute]po\[CHacek]et vzdialenosti medzi p\[OHat]vodn\[YAcute]m bodom a priesekom *)
        distance = EuclideanDistance[normalizedCoordinates, intersection];
        
        (* Form\[AAcute]tovanie rovnice priamky *)
        lineEqn = FormatLineEquation[a, b, c];
        
        (* Nadpis *)
        Print[Style[
            "GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE BODU - SYMETRIA", 
            Bold, 16]];
        Print[Style["========================================", Bold]];
        
        (* ZADANIE *)
        Print[Style["\nZADANIE:", Bold, 14]];
        Print["Majme bod P so s\[UAcute]radnicami:"];
        
        (* Zobrazenie s\[UAcute]radn\[IAcute]c v maticovom tvare *)
        Print[Row[{
            Style["P = ", Blue, Bold],
            MatrixForm[{{Style[normalizedCoordinates[[1]], Blue]}, 
                       {Style[normalizedCoordinates[[2]], Blue]}}]
        }]];
        
        
        
        Print["\nVykonajte symetriu v 2D priestore pod\:013ea priamky ", Style[lineEqn, RGBColor[0, 0.8, 0.2], Bold], "."];
        
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
        Print["1. V\[YAcute]po\[CHacek]et v\[YAcute]razu (ax + by + c)/(a\.b2 + b\.b2) pre dan\[YAcute] bod"];
        Print["2. Vyn\[AAcute]sobenie tohto v\[YAcute]razu koeficientom 2a pre x-ov\[UAcute] a 2b pre y-ov\[UAcute] s\[UAcute]radnicu"];
        Print["3. Od\[CHacek]\[IAcute]tanie v\[YAcute]sledku od p\[OHat]vodn\[YAcute]ch s\[UAcute]radn\[IAcute]c x a y"];
        
        Print["\nRovnica osi symetrie: ", lineEqn];
        Print["Normal\[AAcute]lov\[YAcute] vektor osi symetrie: (", a, ", ", b, ")"];
        
        (* Zvo\:013ete vhodn\[YAcute] popis pre r\[OHat]zne typy os symetrie *)
        Print["\nNa\[SHacek]a os symetrie je " <> lineType <> "." 
            <> If[xIntercept =!= None && yIntercept =!= None, 
                " Priesek s osou x: [" <> ToString[xIntercept] <> ", 0], " 
                <> "priesek s osou y: [0, " <> ToString[yIntercept] <> "]", 
                ""]
        ];
        
        (* V\[YAcute]po\[CHacek]et pre bod *)
        Module[{x = normalizedCoordinates[[1]], 
                y = normalizedCoordinates[[2]],
                newX = outputCoordinates[[1]], 
                newY = outputCoordinates[[2]]},
                
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
            
            (* Geometrick\[UAcute] interpret\[AAcute]ciu *)
            Module[{pp = intersection},
                Print[Style["\nGeometrick\[AAcute] interpret\[AAcute]cia:", Bold]];
                Print["Priesek kolmice z bodu P na priamku p: [", pp[[1]], ", ", pp[[2]], "]"];
                Print["Vzdialenost' bodu P od priamky p: ", N[distance]];
                Print["Bod P' je umiestnen\[YAcute] na tejto kolmici, na druhej strane priamky p, v rovnakej vzdialenosti ako bod P"];
                Print["Vzdialenost' bodu P' od priamky p: ", N[distance]];
            ];
        ];
        
        (* V\[CapitalYAcute]SLEDOK *)
        Print[Style["\nV\[CapitalYAcute]SLEDOK:", Bold, 14]];
        Print["S\[UAcute]radnice bodu po symetrii:"];
        
        (* Zobrazenie v\[YAcute]sledku v maticovom tvare *)
        Print[Row[{
            Style["P' = ", Red, Bold],
            MatrixForm[{{Style[outputCoordinates[[1]], Red]}, 
                       {Style[outputCoordinates[[2]], Red]}}]
        }]];
        
        
        
        (* VIZUALIZ\[CapitalAAcute]CIA *)
        Print[Style["\nVIZUALIZ\[CapitalAAcute]CIA:", Bold]];
        Print[CreateVisualization[normalizedCoordinates, outputCoordinates, a, b, c, lineType, xIntercept, yIntercept]];
        
        (* LEGENDA *)
        Print[Style["\nLEGENDA:", Bold, 14]];
        Print["\[Bullet] Modr\[YAcute] bod: P\[OHat]vodn\[YAcute] bod"];
        Print["\[Bullet] \[CapitalCHacek]erven\[YAcute] bod: Bod po symetrii"];
        Print["\[Bullet] Zelen\[AAcute] priamka: Os symetrie"];
        Print["\[Bullet] Zelen\[AAcute] preru\[SHacek]ovan\[AAcute] \[CHacek]iara: Spojenie p\[OHat]vodn\[EAcute]ho bodu a jeho obrazu"];
        Print["\[Bullet] Mrie\[ZHacek]ka: S\[UAcute]radnicov\[YAcute] syst\[EAcute]m"];
        
        Print[Style["\nP\[OHat]vodn\[EAcute] s\[UAcute]radnice (modr\[AAcute]):", Bold]];
        Print[Row[{Style["Bod P: ", RGBColor[0.1, 0.1, 1]], normalizedCoordinates}]];
                
        Print[Style["\nV\[YAcute]sledn\[EAcute] s\[UAcute]radnice (\[CHacek]erven\[AAcute]):", Bold]];
        Print[Row[{Style["Bod P': ", RGBColor[1, 0.1, 0.1]], outputCoordinates}]];
                
        (* MATEMATICK\[CapitalEAcute] VLASTNOSTI TRANSFORM\[CapitalAAcute]CIE *)
        Print[Style["\nMATEMATICK\[CapitalEAcute] VLASTNOSTI:", Bold, 14]];
        Print["\[Bullet] Symetria pod\:013ea priamky je izometrick\[AAcute] transform\[AAcute]cia - zachov\[AAcute]va vzdialenosti"];
        Print["\[Bullet] Spojnica bodu a jeho obrazu je v\[ZHacek]dy kolm\[AAcute] na os symetrie"];
        Print["\[Bullet] Stred spojnice bodu a jeho obrazu v\[ZHacek]dy le\[ZHacek]\[IAcute] na osi symetrie"];
        Print["\[Bullet] Body le\[ZHacek]iace na osi symetrie zost\[AAcute]vaj\[UAcute] po symetrii nezmenen\[EAcute]"];
        Print["\[Bullet] Aplikov\[AAcute]n\[IAcute]m symetrie dvakr\[AAcute]t z\[IAcute]skame p\[OHat]vodn\[YAcute] bod - je to transform\[AAcute]cia r\[AAcute]du 2"];
        
        (* V\[YAcute]znamn\[EAcute] typy os\[IAcute] symetrie *)
        Print[Style["\nV\[CapitalYAcute]ZNAMN\[CapitalEAcute] OSI SYMETRIE:", Bold, 14]];
        Print["\[Bullet] Os x (y = 0): bod [x, y] sa zobraz\[IAcute] na [x, -y]"];
        Print["\[Bullet] Os y (x = 0): bod [x, y] sa zobraz\[IAcute] na [-x, y]"];
        Print["\[Bullet] Priamka y = x: bod [x, y] sa zobraz\[IAcute] na [y, x]"];
        Print["\[Bullet] Priamka y = -x: bod [x, y] sa zobraz\[IAcute] na [-y, -x]"];
                
        (* Vr\[AAcute]tenie nov\[YAcute]ch s\[UAcute]radn\[IAcute]c pre pr\[IAcute]padn\[EAcute] \[DHacek]al\[SHacek]ie transform\[AAcute]cie *)
        outputCoordinates
    ];

(* Defin\[IAcute]cia chybov\[YAcute]ch spr\[AAcute]v *)
BodSymetria::invalidInput = "Neplatn\[YAcute] vstup. O\[CHacek]ak\[AAcute]vaj\[UAcute] sa s\[UAcute]radnice bodu v 2D priestore.";

End[];
EndPackage[];





(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)
