(* ::Package:: *)

(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


BeginPackage["DomcekHardBalik`Transforms`ZvacsenieZmensenie`", {"DomcekHardBalik`"}];

(* Export public symbols *)
DomcekZvacsenieZmensenie::usage = 
    "DomcekZvacsenieZmensenie[vertices_] zobraz\[IAcute] interakt\[IAcute]vny tutori\[AAcute]l pre zv\[ADoubleDot]\[CHacek]\[SHacek]enie/zmen\[SHacek]enie dom\[CHacek]eka.";

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

(* Funkcia na detekciu koeficientov \[SHacek]k\[AAcute]lovania medzi p\[OHat]vodn\[YAcute]mi a fin\[AAcute]lnymi vrcholmi *)
DetectScalingParameters[originalVertices_, finalVertices_] := 
    Module[{sx, sy, foundScaling = False, detectedSx, detectedSy, i = 1, point, scaledPoint},
        While[i <= 5 && !foundScaling,
            point = originalVertices[[i]];
            scaledPoint = finalVertices[[i]];
            
            (* Ak aspo\[NHacek] jedna z p\[OHat]vodn\[YAcute]ch s\[UAcute]radn\[IAcute]c nie je nula, m\[OHat]\[ZHacek]eme detekova\[THacek] koeficient *)
            If[point[[1]] != 0 && scaledPoint[[1]] != 0,
                detectedSx = scaledPoint[[1]]/point[[1]];
                foundScaling = True;
            ];
            
            If[point[[2]] != 0 && scaledPoint[[2]] != 0,
                detectedSy = scaledPoint[[2]]/point[[2]];
                foundScaling = True;
            ];
            
            i++;
        ];
        
        (* Ak sme na\[SHacek]li koeficienty, vr\[AAcute]time ich, inak None *)
        If[foundScaling, 
            {Simplify[detectedSx], Simplify[detectedSy]}, 
            None
        ]
    ];

(* Funkcia na generovanie parametrov \[SHacek]k\[AAcute]lovania *)
GenerateScalingParameters[] := 
    Module[{sx, sy, typTransformacie, popis, valid = False, scaleOptions},
        
        (* Definujeme mo\[ZHacek]n\[EAcute] scaling faktory - v\[SHacek]etky zachov\[AAcute]vaj\[UAcute]ce pomer str\[AAcute]n *)
        scaleOptions = {
            (* Cel\[EAcute] \[CHacek]\[IAcute]sla *)
            2, 3, 1/2, 1/3,
            
            (* Jednoduch\[EAcute] zlomky *)
            3/2, 2/3, 4/3, 3/4, 5/4, 4/5,
            
            (* Odmocniny *)
            Sqrt[2], Sqrt[3], Sqrt[5], 
            1/Sqrt[2], 1/Sqrt[3], 1/Sqrt[5],
            
            (* Kombinovan\[EAcute] hodnoty *)
            2*Sqrt[2]/3, 3/(2*Sqrt[2]), 
            Sqrt[3]/2, 2/Sqrt[3],
            Sqrt[5]/2, 2/Sqrt[5]
        };
        
        (* Vyberieme n\[AAcute]hodn\[YAcute] scale faktor zo zoznamu *)
        sx = RandomChoice[scaleOptions];
        
        (* Pre zachovanie pomeru str\[AAcute]n *)
        sy = sx;
        
        (* Identifik\[AAcute]cia typu transform\[AAcute]cie a popisu *)
        {typTransformacie, popis, spolPodst} = IdentifyTransformationType[sx, sy];
        
        (* V\[YAcute]stup parametrov *)
        {sx, sy, typTransformacie, popis, spolPodst}
    ];

(* Hlavn\[AAcute] funkcia pre zv\[ADoubleDot]\[CHacek]\[SHacek]enie/zmen\[SHacek]enie dom\[CHacek]eka *)
DomcekZvacsenieZmensenie[inputVertices_] := 
    Module[{sx, sy, typTransformacie, popis, spolPodst, finalVertices, 
            normalizedVertices, detectedScaling = None, detectedFinalVertices = None},
            
        (* Skontrolujeme, \[CHacek]i vstup m\[AAcute] viac ako 5 vrcholov - mo\[ZHacek]no obsahuje aj v\[YAcute]sledok predch\[AAcute]dzaj\[UAcute]cej transform\[AAcute]cie *)
        If[Length[inputVertices] > 5 && Mod[Length[inputVertices], 5] == 0,
            (* Rozdel\[IAcute]me vstup na vstupn\[EAcute] a o\[CHacek]ak\[AAcute]van\[EAcute] v\[YAcute]stupn\[EAcute] vrcholy *)
            normalizedVertices = inputVertices[[1;;5]];
            (* Ak m\[AAcute]me viacn\[AAcute]sobn\[EAcute] p\[ADoubleDot]\[THacek]ice, berieme posledn\[UAcute] ako o\[CHacek]ak\[AAcute]van\[YAcute] v\[YAcute]stup *)
            detectedFinalVertices = inputVertices[[-(5;;-1)]];
            
            (* Pok\[UAcute]sime sa detekova\[THacek] parametre \[SHacek]k\[AAcute]lovania z vrcholov *)
            detectedScaling = DetectScalingParameters[normalizedVertices, detectedFinalVertices],
            
            (* Inak pou\[ZHacek]ijeme vstupn\[EAcute] vrcholy ako s\[UAcute] *)
            normalizedVertices = inputVertices
        ];
            
        (* 1. Ur\[CHacek]\[IAcute]me parametre transform\[AAcute]cie - bu\[DHacek] detekovan\[EAcute] alebo generovan\[EAcute] *)
        If[detectedScaling =!= None,
            sx = detectedScaling[[1]];
            sy = detectedScaling[[2]];
            {typTransformacie, popis, spolPodst} = IdentifyTransformationType[sx, sy];
            finalVertices = detectedFinalVertices,
            
            (* 1a. Generujeme parametre transform\[AAcute]cie *)
            {sx, sy, typTransformacie, popis, spolPodst} = GenerateScalingParameters[];
            
            (* 2. Aplikujeme transform\[AAcute]ciu na vstupn\[EAcute] vrcholy - PRESN\[CapitalYAcute] V\[CapitalYAcute]PO\[CapitalCHacek]ET *)
            finalVertices = Map[
                Function[point,
                    {Simplify[point[[1]]*sx], Simplify[point[[2]]*sy]}
                ],
                normalizedVertices
            ]
        ];
        
        (* 3. Za\[CHacek]iatok zobrazenia pre \[ZHacek]iaka *)
        Print[Style[
            "GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE DOM\[CapitalCHacek]EKA - ZV\[CapitalADoubleDot]\[CapitalCHacek]\[CapitalSHacek]ENIE/ZMEN\[CapitalSHacek]ENIE", 
            Bold, 16]];
        Print[Style["========================================", Bold]];
        
        (* ZADANIE *)
        Print[Style["\nZADANIE:", Bold, 14]];
        Print["Majme dom\[CHacek]ek s vrcholmi:"];
        Print[MatrixForm[Map[Style[#, Blue] &, normalizedVertices, {2}]]];
        
        Print["\nVykonajte " <> spolPodst <> " dom\[CHacek]eka v 2D priestore pomocou transforma\[CHacek]nej matice:"];
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
        
        Print["\nV\[YAcute]po\[CHacek]et pre jednotliv\[EAcute] vrcholy:"];
        
        Table[
            Module[{x = normalizedVertices[[i, 1]], 
                    y = normalizedVertices[[i, 2]], 
                    newX, newY, expandedX, expandedY, simplifiedX, simplifiedY},
                
                (* V\[YAcute]po\[CHacek]et nov\[YAcute]ch s\[UAcute]radn\[IAcute]c s presn\[YAcute]m v\[YAcute]sledkom *)
                newX = x*sx;
                newY = y*sy;
                
                (* Rozvinut\[EAcute] a zjednodu\[SHacek]en\[EAcute] v\[YAcute]razy *)
                expandedX = Expand[newX];
                expandedY = Expand[newY];
                simplifiedX = Simplify[newX];
                simplifiedY = Simplify[newY];
                
                Print["\nVrchol ", i, ":"];
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
            ],
            {i, 5}
        ];
        
        (* V\[CapitalYAcute]SLEDOK *)
        Print[Style["\nV\[CapitalYAcute]SLEDOK:", Bold, 14]];
        Print["Vrcholy dom\[CHacek]eka po " <> spolPodst <> ":"];
        
        (* Plne rozvinut\[AAcute] a zjednodu\[SHacek]en\[AAcute] matica v\[YAcute]sledku *)
        Module[{expandedOutputVertices, matrixToShow},
            (* Rozvinieme a zjednodu\[SHacek]\[IAcute]me ka\[ZHacek]d\[YAcute] prvok v\[YAcute]slednej matice *)
            expandedOutputVertices = Map[Expand, finalVertices, {2}];
            
            (* Vytvor\[IAcute]me pekne form\[AAcute]tovan\[UAcute] verziu matice na zobrazenie *)
            matrixToShow = Map[Style[#, Red] &, expandedOutputVertices, {2}];
            
            (* Zobraz\[IAcute]me upraven\[UAcute] maticu *)
            Print[MatrixForm[matrixToShow]];
        ];
        
        (* VIZUALIZ\[CapitalAAcute]CIA *)
        Print[Style["\nVIZUALIZ\[CapitalAAcute]CIA:", Bold, 14]];
        Print[CreateVisualization[normalizedVertices, finalVertices, sx, sy, typTransformacie]];
        
        (* LEGENDA *)
        Print[Style["\nLEGENDA:", Bold, 14]];
        Print["\[Bullet] Modr\[EAcute] body a \[CHacek]iary: P\[OHat]vodn\[YAcute] dom\[CHacek]ek"];
        Print["\[Bullet] \[CapitalCHacek]erven\[EAcute] body a \[CHacek]iary: Dom\[CHacek]ek po " <> spolPodst];
        Print["\[Bullet] Zelen\[EAcute] preru\[SHacek]ovan\[EAcute] \[CHacek]iary: Dr\[AAcute]ha zmeny ve\:013ekosti vrcholov"];
        Print["\[Bullet] Mrie\[ZHacek]ka: S\[UAcute]radnicov\[YAcute] syst\[EAcute]m"];
        
        Print[Style["\nP\[OHat]vodn\[EAcute] vrcholy (modr\[AAcute]):", Bold]];
        Table[
            Print[Row[{Style["Vrchol " <> ToString[i] <> ": ", 
                RGBColor[0.1, 0.1, 1]], normalizedVertices[[i]]}]], {i, 5}];
                
        Print[Style["\nV\[YAcute]sledn\[EAcute] vrcholy (\[CHacek]erven\[AAcute]):", Bold]];
        Module[{expandedOutput},
            Table[
                expandedOutput = Expand[finalVertices[[i]]];
                Print[Row[{Style["Vrchol " <> ToString[i] <> "': ", 
                    RGBColor[1, 0.1, 0.1]], expandedOutput}]], {i, 5}];
        ];
                
        Print[Style["\nInverzn\[AAcute] transform\[AAcute]cia:", Bold]];
        Print["Pre vr\[AAcute]tenie do p\[OHat]vodn\[EAcute]ho stavu pou\[ZHacek]ite koeficienty:"];
        Print["sx' = 1/sx = ", FormatExactValue[1/sx], " (" <> If[sx < 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]ovanie", "zmen\[SHacek]ovanie"] <> " v smere osi x)"];
        Print["sy' = 1/sy = ", FormatExactValue[1/sy], " (" <> If[sy < 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]ovanie", "zmen\[SHacek]ovanie"] <> " v smere osi y)"];
        
        (* MATEMATICK\[CapitalEAcute] VLASTNOSTI TRANSFORM\[CapitalAAcute]CIE - pridan\[EAcute] pre Hard bal\[IAcute]k *)
        Print[Style["\nMATEMATICK\[CapitalEAcute] VLASTNOSTI:", Bold, 14]];
        If[sx == sy,
            Print["\[Bullet] Pri rovnak\[YAcute]ch koeficientoch (sx = sy = " <> FormatExactValue[sx] <> "):"],
            Print["\[Bullet] Pri r\[OHat]znych koeficientoch (sx = " <> FormatExactValue[sx] <> ", sy = " <> FormatExactValue[sy] <> "):"]
        ];
        
        If[sx == sy,
            Print["  - Zachov\[AAcute]va sa tvar (podobnos\[THacek]) geometrick\[YAcute]ch \[UAcute]tvarov"];
            Print["  - Zachov\[AAcute]vaj\[UAcute] sa v\[SHacek]etky uhly medzi \[UAcute]se\[CHacek]kami"];
            Print["  - Pomer d\:013a\[ZHacek]ok \[UAcute]se\[CHacek]iek sa zachov\[AAcute]va"];
            Print["  - Obsah dom\[CHacek]eka sa " <> If[sx > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]\[IAcute] " <> FormatExactValue[sx^2] <> "-kr\[AAcute]t", "zmen\[SHacek]\[IAcute] na " <> FormatExactValue[1/sx^2] <> ". \[CHacek]as\[THacek]"]],
            
            Print["  - Men\[IAcute] sa tvar (podobnos\[THacek]) geometrick\[YAcute]ch \[UAcute]tvarov"];
            Print["  - Uhly medzi \[UAcute]se\[CHacek]kami sa menia"];
            Print["  - Pomer d\:013a\[ZHacek]ok \[UAcute]se\[CHacek]iek sa men\[IAcute] v z\[AAcute]vislosti od ich smeru"];
            Print["  - Obsah dom\[CHacek]eka sa " <> If[sx*sy > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]\[IAcute] " <> FormatExactValue[sx*sy] <> "-kr\[AAcute]t", "zmen\[SHacek]\[IAcute] na " <> FormatExactValue[1/(sx*sy)] <> ". \[CHacek]as\[THacek]"]]
        ];
        
        Print["\[Bullet] Nemen\[IAcute] sa poloha po\[CHacek]iatku s\[UAcute]radnicovej s\[UAcute]stavy [0,0]"];
        Print["\[Bullet] Body, ktor\[EAcute] le\[ZHacek]ia na osi x, zostan\[UAcute] le\[ZHacek]a\[THacek] na osi x (ich y-ov\[AAcute] s\[UAcute]radnica zost\[AAcute]va nulov\[AAcute])"];
        Print["\[Bullet] Body, ktor\[EAcute] le\[ZHacek]ia na osi y, zostan\[UAcute] le\[ZHacek]a\[THacek] na osi y (ich x-ov\[AAcute] s\[UAcute]radnica zost\[AAcute]va nulov\[AAcute])"];
        
        (* Vr\[AAcute]tenie nov\[YAcute]ch vrcholov pre \[DHacek]al\[SHacek]iu transform\[AAcute]ciu *)
        finalVertices
    ];

(* Funkcia na vizualiz\[AAcute]ciu transform\[AAcute]cie *)
CreateVisualization[originalVertices_, finalVertices_, sx_, sy_, typTransformacie_] := 
    Module[{allVertices, xMin, xMax, yMin, yMax, rangeBuffer, xRange, yRange, 
            labelOffsets, brightGreen, originalHouseLines, finalHouseLines},
            
        (* Defin\[IAcute]cia jasnej\[SHacek]ej zelenej farby *)
        brightGreen = RGBColor[0, 0.8, 0.2];
            
        (* Spojenie v\[SHacek]etk\[YAcute]ch vrcholov pre v\[YAcute]po\[CHacek]et rozsahu - pou\[ZHacek]ijeme N[] len pre zobrazenie *)
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
            
            (* Zv\[ADoubleDot]\[CHacek]\[SHacek]en\[YAcute]/zmen\[SHacek]en\[YAcute] dom\[CHacek]ek - \[CHacek]erven\[YAcute] *)
            Red, Thick, Opacity[0.7],
            Line /@ finalHouseLines,
            
            (* Pomocn\[EAcute] \[CHacek]iary - zelen\[EAcute] *)
            brightGreen, Dashed,
            Table[
                Line[{originalVertices[[i]], finalVertices[[i]]}],
                {i, Length[originalVertices]}
            ],
            
            (* Body s bielym pozad\[IAcute]m - zv\[ADoubleDot]\[CHacek]\[SHacek]en\[EAcute] *)
            White, 
            Table[
                Disk[originalVertices[[i]], 0.45],
                {i, 5}
            ],
            Table[
                Disk[finalVertices[[i]], 0.45],
                {i, 5}
            ],
            
            
            
            (* Body - zv\[ADoubleDot]\[CHacek]\[SHacek]en\[EAcute] *)
            Blue, PointSize[0.025], Point[originalVertices],
            Red, PointSize[0.025], Point[finalVertices],
            
            (* Labels s optimalizovan\[YAcute]mi poz\[IAcute]ciami - pou\[ZHacek]itie p\[IAcute]smen A, B, C, D, E *)
            Table[
                {
                    Text[Style[FromCharacterCode[64 + i], Blue, Bold, 16], 
                        originalVertices[[i]] + labelOffsets[[i, 1]]],
                    Text[Style[FromCharacterCode[64 + i] <> "'", Red, Bold, 16], 
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
    
End[];
EndPackage[];



