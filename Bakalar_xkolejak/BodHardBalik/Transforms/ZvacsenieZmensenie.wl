(* ::Package:: *)

(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


BeginPackage["BodHardBalik`Transforms`ZvacsenieZmensenie`", {"BodHardBalik`"}];

(* Export public symbols *)
BodZvacsenieZmensenie::usage = 
    "BodZvacsenieZmensenie[coordinates_] zobraz\[IAcute] interakt\[IAcute]vny tutori\[AAcute]l pre zv\[ADoubleDot]\[CHacek]\[SHacek]enie/zmen\[SHacek]enie bodu.";

Begin["`Private`"];

 
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


ExpandAndFormatExpression[expr_] :=
    Module[{expandedExpr, formattedExpr},
        (* Plne rozvinieme v\[YAcute]raz pre lep\[SHacek]ie zobrazenie krokov *)
        expandedExpr = Expand[expr];
        
        (* Form\[AAcute]tujeme rozvinut\[YAcute] v\[YAcute]raz *)
        formattedExpr = FormatExactValue[expandedExpr];
        
        formattedExpr
    ];

(* Pomocn\[AAcute] funkcia na identifik\[AAcute]ciu typu transform\[AAcute]cie *)
IdentifyTransformationType[sx_, sy_] := 
    Module[{typX, typY, popisX, popisY, spolX, spolY},
        typX = If[sx > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]ovanie", "zmen\[SHacek]ovanie"];
        typY = If[sy > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]ovanie", "zmen\[SHacek]ovanie"];
        
        popisX = If[sx > 1, 
            "zv\[ADoubleDot]\[CHacek]\[SHacek]ovanie " <> FormatExactValue[sx] <> "-kr\[AAcute]t",
            "zmen\[SHacek]ovanie na " <> FormatExactValue[1/sx] <> ". \[CHacek]as\[THacek]"
        ];
        
        popisY = If[sy > 1, 
            "zv\[ADoubleDot]\[CHacek]\[SHacek]ovanie " <> FormatExactValue[sy] <> "-kr\[AAcute]t",
            "zmen\[SHacek]ovanie na " <> FormatExactValue[1/sy] <> ". \[CHacek]as\[THacek]"
        ];
        
        spolX = If[sx > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]enie", "zmen\[SHacek]enie"];
        spolY = If[sy > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]enie", "zmen\[SHacek]enie"];
        
        (* Vr\[AAcute]tenie typu transform\[AAcute]cie a popisu *)
        Which[
            sx == sy,
                {typX, popisX, spolX},
            sx == 1,
                {typY <> " v smere osi y", popisY <> " v smere osi y", spolY <> " v smere osi y"},
            sy == 1,
                {typX <> " v smere osi x", popisX <> " v smere osi x", spolX <> " v smere osi x"},
            True,
                {"zmie\[SHacek]an\[AAcute] transform\[AAcute]cia", 
                 popisX <> " v smere osi x, " <> popisY <> " v smere osi y",
                 spolX <> " v smere osi x, " <> spolY <> " v smere osi y"}
        ]
    ];

(* Pomocn\[AAcute] funkcia na normaliz\[AAcute]ciu vstupn\[YAcute]ch s\[UAcute]radn\[IAcute]c *)
NormalizeCoordinates[coordinates_] := 
    Module[{processedCoordinates},
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
                Message[BodZvacsenieZmensenie::invalidInput]; 
                Abort[]
        ];
        
        (* ZACHOV\[CapitalAAcute]VAME PRESN\[CapitalEAcute] HODNOTY - BEZ KONVERZIE NA NUMERICK\[CapitalEAcute] HODNOTY *)
        
        (* Kontrola, \[CHacek]i s\[UAcute]radnice zostan\[UAcute] v bezpe\[CHacek]nej oblasti, ale zachov\[AAcute]vame presn\[EAcute] hodnoty *)
        If[Max[Abs[N[processedCoordinates]]] > 10,
            (* \[CapitalSHacek]k\[AAcute]lovanie s\[UAcute]radn\[IAcute]c, ak s\[UAcute] pr\[IAcute]li\[SHacek] \[DHacek]aleko, ale zachov\[AAcute]me presn\[YAcute] v\[YAcute]po\[CHacek]et *)
            processedCoordinates = processedCoordinates / (Max[Abs[N[processedCoordinates]]] / 6);
        ];
        
        (* N\[AAcute]vrat s\[UAcute]radn\[IAcute]c *)
        processedCoordinates
    ];

(* Funkcia na detekciu koeficientov \[SHacek]k\[AAcute]lovania medzi p\[OHat]vodn\[YAcute]mi a fin\[AAcute]lnymi s\[UAcute]radnicami *)
DetectScalingParameters[originalCoordinates_, finalCoordinates_] := 
    Module[{sx, sy, foundScaling = False, detectedSx, detectedSy},
        (* Ak aspo\[NHacek] jedna z p\[OHat]vodn\[YAcute]ch s\[UAcute]radn\[IAcute]c nie je nula, m\[OHat]\[ZHacek]eme detekova\[THacek] koeficient *)
        If[originalCoordinates[[1]] != 0 && finalCoordinates[[1]] != 0,
            detectedSx = finalCoordinates[[1]]/originalCoordinates[[1]];
            foundScaling = True;
        ];
        
        If[originalCoordinates[[2]] != 0 && finalCoordinates[[2]] != 0,
            detectedSy = finalCoordinates[[2]]/originalCoordinates[[2]];
            foundScaling = True;
        ];
        
        (* Ak sme na\[SHacek]li koeficienty, vr\[AAcute]time ich, inak None *)
        If[foundScaling, 
            {Simplify[detectedSx], Simplify[detectedSy]}, 
            None
        ]
    ];

(* Funkcia na generovanie parametrov \[SHacek]k\[AAcute]lovania *)
GenerateScalingParameters[] := 
    Module[{sx, sy, citatel, menovatel, typTransformacie, popis, valid = False, narocnost},
        (* Ur\[CHacek]enie n\[AAcute]ro\[CHacek]nosti - vy\[SHacek]\[SHacek]ia pravdepodobnos\[THacek] n\[AAcute]ro\[CHacek]nej\[SHacek]\[IAcute]ch pr\[IAcute]kladov pre Hard bal\[IAcute]k *)
        narocnost = RandomReal[];
        
        While[!valid,
            (* Generovanie n\[AAcute]hodn\[YAcute]ch zlomkov - n\[AAcute]ro\[CHacek]nej\[SHacek]ie pre Hard bal\[IAcute]k *)
            If[narocnost < 0.7,
                (* N\[AAcute]ro\[CHacek]nej\[SHacek]ie koeficienty - zlomky alebo nep\[AAcute]rne \[CHacek]\[IAcute]sla *)
                citatel = RandomChoice[{1, 2, 3}];
                menovatel = RandomChoice[{1, 2, 3}];
                sx = citatel/menovatel // Simplify;
                
                citatel = RandomChoice[{1, 2, 3}];
                menovatel = RandomChoice[{1, 2, 3}];
                sy = citatel/menovatel // Simplify;
                
                (* Zabezpe\[CHacek]i\[THacek], \[ZHacek]e aspo\[NHacek] jeden koeficient je zlomok *)
                If[IntegerQ[sx] && IntegerQ[sy],
                    If[RandomReal[] < 0.5,
                        sx = 1/sx,
                        sy = 1/sy
                    ]
                ],
                
                (* Jednoduch\[SHacek]ie koeficienty - naj\[CHacek]astej\[SHacek]ie cel\[EAcute] \[CHacek]\[IAcute]sla *)
                citatel = RandomInteger[{1, 2}];
                menovatel = RandomInteger[{1, 2}];
                sx = citatel/menovatel // Simplify;
                
                citatel = RandomInteger[{1, 2}];
                menovatel = RandomInteger[{1, 2}];
                sy = citatel/menovatel // Simplify;
            ];
            
            (* Kontroly validity *)
            valid = 
                (* Kontrola, \[CHacek]i koeficienty nie s\[UAcute] pr\[IAacute]li\[SHacek] mal\[EAcute] alebo ve\:013ek\[EAcute] *)
                1/4 <= sx <= 4 && 1/4 <= sy <= 4 &&
                (* Zabezpe\[CHacek]i\[THacek], \[ZHacek]e sx a sy nie s\[UAcute] oba rovn\[EAcute] 1 *)
                (sx != 1 || sy != 1);
        ];
        
        (* Identifik\[AAcute]cia typu transform\[AAcute]cie a popisu *)
        {typTransformacie, popis, spolPodst} = IdentifyTransformationType[sx, sy];
        
        (* V\[YAcute]stup parametrov *)
        {sx, sy, typTransformacie, popis, spolPodst}
    ];

(* Funkcia na vizualiz\[AAcute]ciu transform\[AAcute]cie *)
CreateVisualization[originalCoordinates_, finalCoordinates_, sx_, sy_, typTransformacie_] := 
    Module[{allCoordinates, xMin, xMax, yMin, yMax, rangeBuffer, xRange, yRange, 
            labelOffsets, brightGreen},
            
        (* Defin\[IAcute]cia jasnej\[SHacek]ej zelenej farby *)
        brightGreen = RGBColor[0, 0.8, 0.2];
            
        (* Spojenie v\[SHacek]etk\[YAcute]ch s\[UAcute]radn\[IAcute]c pre v\[YAcute]po\[CHacek]et rozsahu - pou\[ZHacek]ijeme N[] len pre zobrazenie *)
        allCoordinates = {originalCoordinates, finalCoordinates};
        
        (* V\[YAcute]po\[CHacek]et minim\[AAcute]lnych a maxim\[AAcute]lnych hodn\[OHat]t *)
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
        
        Graphics[{
            (* Grid *)
            LightGray, Thin,
            Table[Line[{{xRange[[1]], y}, {xRange[[2]], y}}], 
                {y, Ceiling[yRange[[1]]], Floor[yRange[[2]]], 2}],
            Table[Line[{{x, yRange[[1]]}, {x, yRange[[2]]}}], 
                {x, Ceiling[xRange[[1]]], Floor[xRange[[2]]], 2}],
            
            (* Pomocn\[EAcute] \[CHacek]iary - zelen\[EAcute] *)
            brightGreen, Dashed,
            Line[{originalCoordinates, finalCoordinates}],
            
            (* Body s bielym pozad\[IAcute]m - zv\[ADoubleDot]\[CHacek]\[SHacek]en\[EAcute] *)
            White, 
            Disk[originalCoordinates, 0.45],
            Disk[finalCoordinates, 0.45],
            
            (* Body - zv\[ADoubleDot]\[CHacek]\[SHacek]en\[EAcute] *)
            Blue, PointSize[0.025], Point[originalCoordinates],
            Red, PointSize[0.025], Point[finalCoordinates],
            
            (* Labels s optimalizovan\[YAcute]mi poz\[IAcute]ciami *)
            Text[Style["P", Blue, Bold, 16], 
                  originalCoordinates + labelOffsets[[1]]],
            Text[Style["P'", Red, Bold, 16], 
                  finalCoordinates + labelOffsets[[2]]],
            
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

(* Hlavn\[AAcute] funkcia pre zv\[ADoubleDot]\[CHacek]\[SHacek]enie/zmen\[SHacek]enie bodu *)
BodZvacsenieZmensenie[inputCoordinates_] := 
    Module[{sx, sy, typTransformacie, popis, spolPodst, finalCoordinates, 
            normalizedCoordinates, detectedScaling = None, detectedFinalCoordinates = None},
            
        (* Skontrolujeme, \[CHacek]i vstup m\[AAcute] viac ako 2 hodnoty - mo\[ZHacek]no obsahuje aj v\[YAcute]sledok predch\[AAcute]dzaj\[UAcute]cej transform\[AAcute]cie *)
        If[Length[Flatten[inputCoordinates]] > 2 && Mod[Length[Flatten[inputCoordinates]], 2] == 0,
            (* Rozdel\[IAcute]me vstup na vstupn\[EAcute] a o\[CHacek]ak\[AAcute]van\[EAcute] v\[YAcute]stupn\[EAcute] s\[UAcute]radnice *)
            normalizedCoordinates = Take[Flatten[inputCoordinates], 2];
            (* Berieme posledn\[EAcute] dve hodnoty ako o\[CHacek]ak\[AAcute]van\[YAcute] v\[YAcute]stup *)
            detectedFinalCoordinates = Take[Flatten[inputCoordinates], -2];
            
            (* Pok\[UAcute]sime sa detekova\[THacek] parametre \[SHacek]k\[AAcute]lovania zo s\[UAcute]radn\[IAcute]c *)
            detectedScaling = DetectScalingParameters[normalizedCoordinates, detectedFinalCoordinates],
            
            (* Inak pou\[ZHacek]ijeme vstupn\[EAcute] s\[UAcute]radnice ako s\[UAcute] *)
            
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
        ];
            
        (* 1. Ur\[CHacek]\[IAcute]me parametre transform\[AAcute]cie - bu\[DHacek] detekovan\[EAcute] alebo generovan\[EAcute] *)
        If[detectedScaling =!= None,
            sx = detectedScaling[[1]];
            sy = detectedScaling[[2]];
            {typTransformacie, popis, spolPodst} = IdentifyTransformationType[sx, sy];
            finalCoordinates = detectedFinalCoordinates,
            
            (* 1a. Generujeme parametre transform\[AAcute]cie *)
            {sx, sy, typTransformacie, popis, spolPodst} = GenerateScalingParameters[];
            
            (* 2. Aplikujeme transform\[AAcute]ciu na vstupn\[EAcute] s\[UAcute]radnice - PRESN\[CapitalYAcute] V\[CapitalYAcute]PO\[CapitalCHacek]ET *)
            finalCoordinates = {Simplify[normalizedCoordinates[[1]]*sx], Simplify[normalizedCoordinates[[2]]*sy]}
        ];
        
        (* 3. Za\[CHacek]iatok zobrazenia pre \[ZHacek]iaka *)
        Print[Style[
            "GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE BODU - ZV\[CapitalADoubleDot]\[CapitalCHacek]\[CapitalSHacek]ENIE/ZMEN\[CapitalSHacek]ENIE", 
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
        
        
        
        Print["\nVykonajte " <> spolPodst <> " bodu pomocou transforma\[CHacek]nej matice v 2D priestore:"];
        Print[MatrixForm[{
            {Style[sx, Red], Style[0, Red]}, 
            {Style[0, Red], Style[sy, Red]}
        }]];
        
        (* POSTUP *)
        Print[Style["\nPOSTUP:", Bold, 14]];
        Print["Pre v\[YAcute]po\[CHacek]et pou\[ZHacek]ijeme maticu " <> spolPodst <> " v tvare:"];
        Print[Style["Transforma\[CHacek]n\[AAcute] matica:", Bold]];
        Print[MatrixForm[{
            {Style["sx", Red], Style[0, Red]}, 
            {Style[0, Red], Style["sy", Red]}
        }]];
        Print["kde:"];
        Print["sx je koeficient " <> spolPodst <> " v smere osi x"];
        Print["sy je koeficient " <> spolPodst <> " v smere osi y"];
        
        Print["\nV na\[SHacek]om pr\[IAcute]pade:"];
        Print["sx = ", FormatExactValue[sx], " (" <> If[sx > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]ovanie", "zmen\[SHacek]ovanie"] <> " v smere osi x)"];
        Print["sy = ", FormatExactValue[sy], " (" <> If[sy > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]ovanie", "zmen\[SHacek]ovanie"] <> " v smere osi y)"];
        
        Print["\nV\[YAcute]po\[CHacek]et pre bod P:"];
        
        Module[{x = normalizedCoordinates[[1]], 
                y = normalizedCoordinates[[2]], 
                newX, newY, expandedX, expandedY, simplifiedX, simplifiedY},
            
            (* V\[YAcute]po\[CHacek]et nov\[YAcute]ch s\[UAcute]radn\[IAcute]c s presn\[YAcute]m v\[YAcute]sledkom *)
            newX = x*sx;
            newY = y*sy;
            
            (* Rozvinut\[EAcute] a zjednodu\[SHacek]en\[EAcute] v\[YAcute]razy *)
            expandedX = Expand[newX];
            expandedY = Expand[newY];
            simplifiedX = Simplify[newX];
            simplifiedY = Simplify[newY];
            
            Print[Style["P\[OHat]vodn\[EAcute] s\[UAcute]radnice:", Bold], " [", FormatExactValue[x], ", ", FormatExactValue[y], "]"];
            
            (* Detailn\[YAcute] postup pre x-ov\[UAcute] s\[UAcute]radnicu *)
            Print[Style["V\[YAcute]po\[CHacek]et x-ovej s\[UAcute]radnice:", Bold]];
            Print["x' = sx \[CenterDot] x = ", FormatExactValue[sx], " \[CenterDot] ", FormatExactValue[x]];
            
            (* Ak je v\[YAcute]raz komplexn\[YAcute], zobraz\[IAcute]me rozvinut\[YAcute] tvar *)
            If[x != 0 && sx != 1,
                Print["x' = ", FormatExactValue[expandedX]]
            ];
            
            Print["x' = ", FormatExactValue[simplifiedX]];
            
            (* Detailn\[YAcute] postup pre y-ov\[UAcute] s\[UAcute]radnicu *)
            Print[Style["V\[YAcute]po\[CHacek]et y-ovej s\[UAcute]radnice:", Bold]];
            Print["y' = sy \[CenterDot] y = ", FormatExactValue[sy], " \[CenterDot] ", FormatExactValue[y]];
            
            (* Ak je v\[YAcute]raz komplexn\[YAcute], zobraz\[IAcute]me rozvinut\[YAcute] tvar *)
            If[y != 0 && sy != 1,
                Print["y' = ", FormatExactValue[expandedY]]
            ];
            
            Print["y' = ", FormatExactValue[simplifiedY]];
            
            (* V\[YAcute]sledn\[EAcute] s\[UAcute]radnice - plne rozvinut\[EAcute] a zjednodu\[SHacek]en\[EAcute] *)
            Print[Style["V\[YAcute]sledn\[EAcute] s\[UAcute]radnice:", Bold], " [", 
                  FormatExactValue[simplifiedX], ", ", FormatExactValue[simplifiedY], "]"];
            
            (* Overenie inverznej transform\[AAcute]cie *)
            Print[Style["\nOverenie pomocou inverznej transform\[AAcute]cie:", Bold]];
            Print["x = x'/sx = ", FormatExactValue[simplifiedX], "/", FormatExactValue[sx], 
                  " = ", FormatExactValue[Simplify[simplifiedX/sx]]];
            Print["y = y'/sy = ", FormatExactValue[simplifiedY], "/", FormatExactValue[sy], 
                  " = ", FormatExactValue[Simplify[simplifiedY/sy]]];
            
            (* Maticov\[YAcute] z\[AAcute]pis - s plne rozvinut\[YAcute]mi v\[YAcute]razmi *)
            Print[Style["\nMaticov\[YAcute] z\[AAcute]pis:", Bold]];
            Print[
                Row[{
                    MatrixForm[{
                        {Style[sx, Red], Style[0, Red]},
                        {Style[0, Red], Style[sy, Red]}
                    }],
                    " \[CenterDot] ",
                    MatrixForm[{{x}, {y}}],
                    " = ",
                    MatrixForm[{{Style[simplifiedX, Red]}, {Style[simplifiedY, Red]}}]
                }]
            ];
        ];
        
        (* V\[CapitalYAcute]SLEDOK *)
        Print[Style["\nV\[CapitalYAcute]SLEDOK:", Bold, 14]];
        Print["S\[UAcute]radnice bodu po " <> spolPodst <> ":"];
        
        (* Zobrazenie v\[YAcute]sledku v maticovom tvare *)
        Module[{expandedOutput},
            (* Rozvinieme a zjednodu\[SHacek]\[IAcute]me v\[YAcute]sledn\[EAcute] s\[UAcute]radnice *)
            expandedOutput = Map[Expand, finalCoordinates];
            
            (* Zobraz\[IAcute]me upraven\[EAcute] s\[UAcute]radnice v maticovom tvare *)
            Print[Row[{
                Style["P' = ", Red, Bold],
                MatrixForm[{{Style[expandedOutput[[1]], Red]}, 
                            {Style[expandedOutput[[2]], Red]}}]
            }]];
            
            
        ];
        
        (* VIZUALIZ\[CapitalAAcute]CIA *)
        Print[Style["\nVIZUALIZ\[CapitalAAcute]CIA:", Bold]];
        Print[CreateVisualization[normalizedCoordinates, finalCoordinates, sx, sy, typTransformacie]];
        
        (* LEGENDA *)
        Print[Style["\nLEGENDA:", Bold, 14]];
        Print["\[Bullet] Modr\[YAcute] bod: P\[OHat]vodn\[YAcute] bod P"];
        Print["\[Bullet] \[CapitalCHacek]erven\[YAcute] bod: Bod po " <> spolPodst];
        Print["\[Bullet] Zelen\[AAcute] preru\[SHacek]ovan\[AAcute] \[CHacek]iara: Dr\[AAcute]ha zmeny ve\:013ekosti bodu"];
        Print["\[Bullet] Mrie\[ZHacek]ka: S\[UAcute]radnicov\[YAcute] syst\[EAcute]m"];
        
        Print[Style["\nP\[OHat]vodn\[EAcute] s\[UAcute]radnice (modr\[AAcute]):", Bold]];
        Print[Row[{Style["Bod P: ", RGBColor[0.1, 0.1, 1]], normalizedCoordinates}]];
                
        Print[Style["\nV\[YAcute]sledn\[EAcute] s\[UAcute]radnice (\[CHacek]erven\[AAcute]):", Bold]];
        Module[{expandedOutput},
            expandedOutput = Expand[finalCoordinates];
            Print[Row[{Style["Bod P': ", RGBColor[1, 0.1, 0.1]], expandedOutput}]];
        ];
                
        Print[Style["\nInverzn\[AAcute] transform\[AAcute]cia:", Bold]];
        Print["Pre vr\[AAcute]tenie do p\[OHat]vodn\[EAcute]ho stavu pou\[ZHacek]ite koeficienty:"];
        Print["sx' = 1/sx = ", FormatExactValue[1/sx], " (" <> If[sx < 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]ovanie", "zmen\[SHacek]ovanie"] <> " v smere osi x)"];
        Print["sy' = 1/sy = ", FormatExactValue[1/sy], " (" <> If[sy < 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]ovanie", "zmen\[SHacek]ovanie"] <> " v smere osi y)"];
        
        (* MATEMATICK\[CapitalEAcute] VLASTNOSTI TRANSFORM\[CapitalAAcute]CIE *)
        Print[Style["\nMATEMATICK\[CapitalEAcute] VLASTNOSTI:", Bold, 14]];
        If[sx == sy,
            Print["\[Bullet] Pri rovnak\[YAcute]ch koeficientoch (sx = sy = " <> FormatExactValue[sx] <> "):"],
            Print["\[Bullet] Pri r\[OHat]znych koeficientoch (sx = " <> FormatExactValue[sx] <> ", sy = " <> FormatExactValue[sy] <> "):"]
        ];
        
        If[sx == sy,
            Print["  - Proporcion\[AAcute]lne " <> If[sx > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]enie", "zmen\[SHacek]enie"] <> " bodu od po\[CHacek]iatku s\[UAcute]radnicovej s\[UAcute]stavy"];
            Print["  - Vzdialenosti od po\[CHacek]iatku sa " <> If[sx > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]uj\[UAcute] " <> FormatExactValue[sx] <> "-kr\[AAcute]t", "zmen\[SHacek]uj\[UAcute] na " <> FormatExactValue[1/sx] <> ". \[CHacek]as\[THacek]"]],
            
            Print["  - Neproporcion\[AAcute]lne " <> spolPodst <> " bodu v r\[OHat]znych smeroch"];
            Print["  - Vzdialenosti od osi y sa " <> If[sx > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]uj\[UAcute] " <> FormatExactValue[sx] <> "-kr\[AAcute]t", "zmen\[SHacek]uj\[UAcute] na " <> FormatExactValue[1/sx] <> ". \[CHacek]as\[THacek]"]];
            Print["  - Vzdialenosti od osi x sa " <> If[sy > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]uj\[UAcute] " <> FormatExactValue[sy] <> "-kr\[AAcute]t", "zmen\[SHacek]uj\[UAcute] na " <> FormatExactValue[1/sy] <> ". \[CHacek]as\[THacek]"]]
        ];
        
        Print["\[Bullet] Body le\[ZHacek]iace na osi x sa " <> If[sx > 1, "vzdaluj\[UAcute]", "pribli\[ZHacek]uj\[UAcute]"] <> " od po\[CHacek]iatku v smere osi x"];
        Print["\[Bullet] Body le\[ZHacek]iace na osi y sa " <> If[sy > 1, "vzdaluj\[UAcute]", "pribli\[ZHacek]uj\[UAcute]"] <> " od po\[CHacek]iatku v smere osi y"];
        Print["\[Bullet] Bod [0,0] (po\[CHacek]iatok s\[UAcute]radnicovej s\[UAcute]stavy) zost\[AAcute]va nezmenen\[YAcute]"];
        
        (* \[CapitalSHacek]PECI\[CapitalAAcute]LNE PR\[CapitalIAcute]PADY *)
        Print[Style["\n\[CapitalSHacek]PECI\[CapitalAAcute]LNE PR\[CapitalIAcute]PADY:", Bold, 14]];
        Print["\[Bullet] Ak sx = sy = 1, k zmene ned\[OHat]jde (identick\[AAcute] transform\[AAcute]cia)"];
        Print["\[Bullet] Ak sx = sy, ide o proporcion\[AAcute]lne " <> If[sx > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]enie", "zmen\[SHacek]enie"] <> " bodu"];
        Print["\[Bullet] Ak sx = 1, bod sa posun\[UAcute]va len v smere osi y"];
        Print["\[Bullet] Ak sy = 1, bod sa posun\[UAcute]va len v smere osi x"];
        Print["\[Bullet] Ak sx = -1, bod sa zrkadl\[IAcute] pod\:013ea osi y"];
        Print["\[Bullet] Ak sy = -1, bod sa zrkadl\[IAcute] pod\:013ea osi x"];
        
        (* Vr\[AAcute]tenie nov\[YAcute]ch s\[UAcute]radn\[IAcute]c pre \[DHacek]al\[SHacek]iu transform\[AAcute]ciu *)
        finalCoordinates
    ];
    
(* Defin\[IAcute]cia chybov\[YAcute]ch spr\[AAcute]v *)
BodZvacsenieZmensenie::invalidInput = "Neplatn\[YAcute] vstup. O\[CHacek]ak\[AAcute]vaj\[UAcute] sa s\[UAcute]radnice bodu v 2D priestore.";

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


(* ::Package:: *)
(**)
