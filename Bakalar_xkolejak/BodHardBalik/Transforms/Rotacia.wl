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


BeginPackage["BodHardBalik`Transforms`Rotacia`", {"BodHardBalik`"}];

(* Export public symbols *)
BodRotacia::usage = 
    "BodRotacia[coordinates_] zobraz\[IAcute] interakt\[IAcute]vny tutori\[AAcute]l pre rot\[AAcute]ciu bodu okolo po\[CHacek]iatku a vr\[AAcute]ti nov\[EAcute] s\[UAcute]radnice.";

Begin["`Private`"];

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
                Message[BodRotacia::invalidInput]; 
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
        
        (* Zjednodu\[SHacek]\[IAcute]me v\[YAcute]razy ale ZACHOV\[CapitalAAcute]ME PRESN\[CapitalEAcute] HODNOTY *)
        newX = Simplify[newX];
        newY = Simplify[newY];
        
        {newX, newY}
    ];

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

(* Pomocn\[AAcute] funkcia na detekciu vhodn\[YAcute]ch uhlov rot\[AAcute]cie z existuj\[UAcute]cich koordin\[AAcute]t *)
DetectRotationAngle[originalCoordinates_, finalCoordinates_] := 
    Module[{validAngles = {}, angle, cosA, sinA, foundAngle = False, point, rotatedPoint},
        
        (* Zoznam uhlov na overenie *)
        angles = {30, 45, 60, 90, 120, 135, 150, 180, 210, 225, 240, 270, 300, 315, 330,
                 -30, -45, -60, -90, -120, -135, -150, -180, -210, -225, -240, -270, -300, -315, -330};
        
        point = originalCoordinates;
        rotatedPoint = finalCoordinates;
        
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
        
        (* Ak sme na\[SHacek]li uhol, vr\[AAcute]time prv\[YAcute] n\[AAcute]jden\[YAcute], inak None *)
        If[foundAngle, First[validAngles], None]
    ];

(* Funkcia na vizualiz\[AAcute]ciu rot\[AAcute]cie *)
CreateVisualization[originalCoordinates_, finalCoordinates_, angle_] := 
    Module[{allCoordinates, xMin, xMax, yMin, yMax, rangeBuffer, xRange, yRange, 
            labelOffsets, angleDeg = angle, angleRad = angle*Pi/180, brightGreen},
        (* Defin\[IAcute]cia jasnej\[SHacek]ej zelenej farby pre uhol *)
        brightGreen = RGBColor[0, 0.8, 0.2];
        
        (* Spojenie v\[SHacek]etk\[YAcute]ch s\[UAcute]radn\[IAcute]c pre v\[YAcute]po\[CHacek]et rozsahu *)
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
        
        (* Vypo\[CHacek]\[IAcute]ta\[THacek] offset pre ozna\[CHacek]enie, aby sa neprekr\[YAcute]vali s \[CHacek]iarami *)
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
            Line[{{0, 0}, originalCoordinates}],
            Line[{{0, 0}, finalCoordinates}],
            
            (* Body s bielym pozadim - zv\[ADoubleDot]\[CHacek]\[SHacek]en\[EAcute] pre lep\[SHacek]iu vidite\:013enos\[THacek] *)
            White, 
            Disk[{0, 0}, 0.45], (* Zv\[YAcute]raznen\[YAcute] po\[CHacek]iatok *)
            Disk[originalCoordinates, 0.45],
            Disk[finalCoordinates, 0.45],
            
            (* Body - zv\[ADoubleDot]\[CHacek]\[SHacek]en\[EAcute] *)
            Black, PointSize[0.025], Point[{0, 0}], (* Po\[CHacek]iatok *)
            Text[Style["O", Black, Bold, 14], {-0.5, -0.5}], (* Ozna\[CHacek]enie po\[CHacek]iatku *)
            
            Blue, PointSize[0.025], Point[originalCoordinates],
            Red, PointSize[0.025], Point[finalCoordinates],
            
            (* Labels s optimalizovan\[YAcute]mi poz\[IAcute]ciami *)
            Text[Style["P", Blue, Bold, 16], 
                    originalCoordinates + labelOffsets[[1]]],
            Text[Style["P'", Red, Bold, 16], 
                    finalCoordinates + labelOffsets[[2]]],
            
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

(* Hlavn\[AAcute] funkcia *)
BodRotacia[inputCoordinates_] := 
    Module[{normalizedCoordinates, angle, popis, outputCoordinates, cosA, sinA, invAngle, cosInvA, sinInvA,
            popisUhla, complexAngles, detectInputOutput = False, finalCoordinates = None, expandedOutputCoordinates},
            
        (* Skontrolujeme, \[CHacek]i vstup m\[AAcute] viac ako 2 hodnoty - mo\[ZHacek]no obsahuje aj v\[YAcute]sledok predch\[AAcute]dzaj\[UAcute]cej transform\[AAcute]cie *)
        If[Length[Flatten[inputCoordinates]] > 2 && Mod[Length[Flatten[inputCoordinates]], 2] == 0,
            (* Rozdel\[IAcute]me vstup na vstupn\[EAcute] a o\[CHacek]ak\[AAcute]van\[EAcute] v\[YAcute]stupn\[EAcute] s\[UAcute]radnice *)
            normalizedCoordinates = Take[Flatten[inputCoordinates], 2];
            (* Berieme posledn\[EAcute] dve hodnoty ako o\[CHacek]ak\[AAcute]van\[YAcute] v\[YAcute]stup *)
            finalCoordinates = Take[Flatten[inputCoordinates], -2];
            detectInputOutput = True,
            (* Inak normalizujeme vstupn\[EAcute] s\[UAcute]radnice a \[ZHacek]iadne v\[YAcute]stupn\[EAcute] ne\[CHacek]ak\[AAcute]me *)
            
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
        
        (* Priamy v\[YAcute]ber zauj\[IAcute]mav\[EAcute]ho uhla *)
        If[detectInputOutput,
            (* Ak s\[UAcute] zadan\[EAcute] v\[YAcute]stupn\[EAcute] s\[UAcute]radnice, pok\[UAcute]sime sa detekova\[THacek] uhol *)
            angle = DetectRotationAngle[normalizedCoordinates, finalCoordinates];
            
            (* Ak bol uhol detekovan\[YAcute], pou\[ZHacek]ijeme ho *)
            If[angle =!= None,
                popisUhla = If[angle > 0, 
                    "v kladnom smere (proti smeru hodinov\[YAcute]ch ru\[CHacek]i\[CHacek]iek)", 
                    "v z\[AAcute]pornom smere (v smere hodinov\[YAcute]ch ru\[CHacek]i\[CHacek]iek)"];
                
                popis = "Rot\[AAcute]cia o " <> ToString[Abs[angle]] <> "\[Degree] " <> popisUhla;
                cosA = GetExactTrigValue[angle, Cos];
                sinA = GetExactTrigValue[angle, Sin];
                
                (* D\[OHat]le\[ZHacek]it\[EAcute]: Pou\[ZHacek]ijeme presne zadan\[EAcute] finalCoordinates bez \[ZHacek]iadnej modifik\[AAcute]cie *)
                outputCoordinates = finalCoordinates,
                
                (* Inak vyberieme komplexnej\[SHacek]\[IAcute] uhol *)
                complexAngles = {30, 45, 60, 120, 135, 150};
                angle = RandomChoice[complexAngles];
                
                popisUhla = If[angle > 0, 
                    "v kladnom smere (proti smeru hodinov\[YAcute]ch ru\[CHacek]i\[CHacek]iek)", 
                    "v z\[AAcute]pornom smere (v smere hodinov\[YAcute]ch ru\[CHacek]i\[CHacek]iek)"];
                
                popis = "Rot\[AAcute]cia o " <> ToString[Abs[angle]] <> "\[Degree] " <> popisUhla;
                cosA = GetExactTrigValue[angle, Cos];
                sinA = GetExactTrigValue[angle, Sin];
                
                (* Vypo\[CHacek]\[IAcute]tame v\[YAcute]stupn\[EAcute] s\[UAcute]radnice *)
                outputCoordinates = ExactRotatePoint[normalizedCoordinates, angle]
            ],
            
            (* Ak \[ZHacek]iadne v\[YAcute]stupn\[EAcute] s\[UAcute]radnice neboli zadan\[EAcute], v\[ZHacek]dy vyberieme zauj\[IAcute]mav\[YAcute] uhol *)
            complexAngles = {30, 45, 60, 120, 135, 150};
            angle = RandomChoice[complexAngles];
            
            popisUhla = If[angle > 0, 
                "v kladnom smere (proti smeru hodinov\[YAcute]ch ru\[CHacek]i\[CHacek]iek)", 
                "v z\[AAcute]pornom smere (v smere hodinov\[YAcute]ch ru\[CHacek]i\[CHacek]iek)"];
            
            popis = "Rot\[AAcute]cia o " <> ToString[Abs[angle]] <> "\[Degree] " <> popisUhla;
            cosA = GetExactTrigValue[angle, Cos];
            sinA = GetExactTrigValue[angle, Sin];
            
            (* Vypo\[CHacek]\[IAcute]tame v\[YAcute]stupn\[EAcute] s\[UAcute]radnice *)
            outputCoordinates = ExactRotatePoint[normalizedCoordinates, angle]
        ];
        
        (* Kontrola transforma\[CHacek]nej matice - ak je st\[AAcute]le pr\[IAacute]li\[SHacek] jednoduch\[AAcute], vyberieme in\[YAcute] uhol *)
        If[cosA == 0 || cosA == 1 || cosA == -1 || sinA == 0 || sinA == 1 || sinA == -1,
            (* Vyn\[UAcute]time uhol 45\[Degree] alebo 60\[Degree] *)
            angle = RandomChoice[{45, 60}];
            cosA = GetExactTrigValue[angle, Cos];
            sinA = GetExactTrigValue[angle, Sin];
            
            (* Prepo\[CHacek]\[IAcute]tame v\[YAcute]stupn\[EAcute] s\[UAcute]radnice *)
            outputCoordinates = ExactRotatePoint[normalizedCoordinates, angle];
            
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
            "GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE BODU - ROT\[CapitalAAcute]CIA", 
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
        
        
        
        Print["\nVykonajte rot\[AAcute]ciu bodu v 2D priestore okolo po\[CHacek]iatku s\[UAcute]radnicovej s\[UAcute]stavy [0,0] o uhol \[Alpha] = ", 
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
        
        Print["\nV\[YAcute]po\[CHacek]et pre bod P:"];
        
        (* V\[YAcute]po\[CHacek]et pre bod - PRESN\[CapitalEAcute] HODNOTY *)
        Module[{x = normalizedCoordinates[[1]], 
                y = normalizedCoordinates[[2]],
                newX, newY, expandNewX, expandNewY, 
                simpleNewX, simpleNewY, invCalcX, invCalcY,
                expandedOutput, expandedInvResult},
                
            Print[Style["P\[OHat]vodn\[EAcute] s\[UAcute]radnice:", Bold], " [", x, ", ", y, "]"];
            
            (* Vykonanie v\[YAcute]po\[CHacek]tov s exactn\[YAcute]mi hodnotami *)
            newX = x*cosA - y*sinA;
            newY = x*sinA + y*cosA;
            
            (* Rozvinut\[EAcute] v\[YAcute]razy pre lep\[SHacek]ie zobrazenie *)
            expandNewX = Expand[newX];
            expandNewY = Expand[newY];
            
            (* Zjednodu\[SHacek]enie v\[YAcute]razov *)
            simpleNewX = Simplify[newX];
            simpleNewY = Simplify[newY];
            
            (* Detailn\[YAcute] postup pre x-ov\[UAcute] s\[UAcute]radnicu *)
            Print[Style["V\[YAcute]po\[CHacek]et x-ovej s\[UAcute]radnice:", Bold]];
            Print["x' = x \[CenterDot] cos(\[Alpha]) - y \[CenterDot] sin(\[Alpha])"];
            
            (* Zobrazenie s hodnotami *)
            Print["x' = ", x, " \[CenterDot] ", FormatExactValue[cosA], " - ", y, " \[CenterDot] ", FormatExactValue[sinA]];
            
            (* Rozvinut\[YAcute] v\[YAcute]po\[CHacek]et - zobraz\[IAcute]me aj medzikroky *)
            If[x != 0 || y != 0, 
                Print["x' = ", FormatExactValue[expandNewX]];
            ];
            
            (* Zobraz kone\[CHacek]n\[YAcute] v\[YAcute]sledok *)
            Print["x' = ", FormatExactValue[simpleNewX]];
            
            (* Detailn\[YAcute] postup pre y-ov\[UAcute] s\[UAcute]radnicu *)
            Print[Style["V\[YAcute]po\[CHacek]et y-ovej s\[UAcute]radnice:", Bold]];
            Print["y' = x \[CenterDot] sin(\[Alpha]) + y \[CenterDot] cos(\[Alpha])"];
            
            (* Zobrazenie s hodnotami *)
            Print["y' = ", x, " \[CenterDot] ", FormatExactValue[sinA], " + ", y, " \[CenterDot] ", FormatExactValue[cosA]];
            
            (* Rozvinut\[YAcute] v\[YAcute]po\[CHacek]et - zobraz\[IAcute]me aj medzikroky *)
            If[x != 0 || y != 0, 
                Print["y' = ", FormatExactValue[expandNewY]];
            ];
            
            (* Zobraz kone\[CHacek]n\[YAcute] v\[YAcute]sledok *)
            Print["y' = ", FormatExactValue[simpleNewY]];
            
            (* V\[YAcute]sledn\[EAcute] s\[UAcute]radnice - plne rozvinut\[EAcute] *)
            expandedOutput = {Expand[simpleNewX], Expand[simpleNewY]};
            Print[Style["V\[YAcute]sledn\[EAcute] s\[UAcute]radnice:", Bold], " [", FormatExactValue[expandedOutput[[1]]], ", ", FormatExactValue[expandedOutput[[2]]], "]"];
            
            (* Overenie inverznej transform\[AAcute]cie *)
            Print[Style["\nOverenie pomocou inverznej transform\[AAcute]cie:", Bold]];
            Print["Pre inverzn\[YAcute] uhol \[Alpha]' = ", invAngle, "\[Degree]:"];
            Print["cos(\[Alpha]') = ", FormatExactValue[cosInvA]];
            Print["sin(\[Alpha]') = ", FormatExactValue[sinInvA]];
            
            (* V\[YAcute]po\[CHacek]et inverznej transform\[AAcute]cie *)
            invCalcX = outputCoordinates[[1]]*cosInvA - outputCoordinates[[2]]*sinInvA;
            invCalcY = outputCoordinates[[1]]*sinInvA + outputCoordinates[[2]]*cosInvA;
            
            (* V\[YAcute]po\[CHacek]et pre x *)
            Print["x = x' \[CenterDot] cos(\[Alpha]') - y' \[CenterDot] sin(\[Alpha]')"];
            Print["x = ", FormatExactValue[outputCoordinates[[1]]], " \[CenterDot] ", 
                  FormatExactValue[cosInvA], " - ", FormatExactValue[outputCoordinates[[2]]], 
                  " \[CenterDot] ", FormatExactValue[sinInvA]];
            
            (* Rozvinut\[YAcute] v\[YAcute]po\[CHacek]et inverznej transform\[AAcute]cie *)
            Print["x = ", FormatExactValue[Expand[invCalcX]]];
            
            (* Zobraz kone\[CHacek]n\[YAcute] v\[YAcute]sledok overenia *)
            Print["x = ", FormatExactValue[Simplify[invCalcX]], " = ", x];
            
            (* V\[YAcute]po\[CHacek]et pre y *)
            Print["y = x' \[CenterDot] sin(\[Alpha]') + y' \[CenterDot] cos(\[Alpha]')"];
            Print["y = ", FormatExactValue[outputCoordinates[[1]]], " \[CenterDot] ", 
                  FormatExactValue[sinInvA], " + ", FormatExactValue[outputCoordinates[[2]]], 
                  " \[CenterDot] ", FormatExactValue[cosInvA]];
            
            (* Rozvinut\[YAcute] v\[YAcute]po\[CHacek]et inverznej transform\[AAcute]cie *)
            Print["y = ", FormatExactValue[Expand[invCalcY]]];
            
            (* Zobraz kone\[CHacek]n\[YAcute] v\[YAcute]sledok overenia *)
            Print["y = ", FormatExactValue[Simplify[invCalcY]], " = ", y];
            
            (* Maticov\[YAcute] z\[AAcute]pis - s plne rozvinut\[YAcute]mi v\[YAcute]razmi *)
            Print[Style["\nMaticov\[YAcute] z\[AAcute]pis:", Bold]];
            
            (* V\[YAcute]po\[CHacek]et rozvinut\[YAcute]ch v\[YAcute]razov pre v\[YAcute]sledky *)
            expandedOutput = {Expand[outputCoordinates[[1]]], Expand[outputCoordinates[[2]]]};
            
            (* Zobrazenie maticov\[EAcute]ho z\[AAcute]pisu s plne rozvinut\[YAcute]mi v\[YAcute]sledkami *)
            Print[
                Row[{
                    MatrixForm[{
                        {Style[cosA, Red], Style[-sinA, Red]},
                        {Style[sinA, Red], Style[cosA, Red]}
                    }],
                    " \[CenterDot] ",
                    MatrixForm[{{x}, {y}}],
                    " = ",
                    MatrixForm[{{Style[expandedOutput[[1]], Red]}, {Style[expandedOutput[[2]], Red]}}]
                }]
            ];
        ];
        
        (* V\[CapitalYAcute]SLEDOK - upraven\[EAcute] pre plne rozvinut\[EAcute] v\[YAcute]razy *)
        Print[Style["\nV\[CapitalYAcute]SLEDOK:", Bold, 14]];
        Print["S\[UAcute]radnice bodu po rot\[AAcute]cii:"];
        
        (* Plne rozvinut\[AAcute] a zjednodu\[SHacek]en\[AAcute] matica v\[YAcute]sledku *)
        Module[{matrixToShow},
            (* Rozvinieme a zjednodu\[SHacek]\[IAcute]me ka\[ZHacek]d\[YAcute] prvok v\[YAcute]sledn\[YAcute]ch s\[UAacute]radn\[IAcute]c *)
            expandedOutputCoordinates = {Expand[outputCoordinates[[1]]], Expand[outputCoordinates[[2]]]};
            
            (* Zobraz\[IAcute]me upraven\[EAcute] s\[UAcute]radnice v maticovom tvare *)
            Print[Row[{
                Style["P' = ", Red, Bold],
                MatrixForm[{{Style[expandedOutputCoordinates[[1]], Red]}, 
                            {Style[expandedOutputCoordinates[[2]], Red]}}]
            }]];
            
            
        ];
        
        (* VIZUALIZ\[CapitalAAcute]CIA *)
        Print[Style["\nVIZUALIZ\[CapitalAAcute]CIA:", Bold]];
        Print[CreateVisualization[normalizedCoordinates, outputCoordinates, angle]];
        
        (* LEGENDA *)
        Print[Style["\nLEGENDA:", Bold, 14]];
        Print["\[Bullet] Modr\[YAcute] bod: P\[OHat]vodn\[YAcute] bod P"];
        Print["\[Bullet] \[CapitalCHacek]erven\[YAcute] bod: Oto\[CHacek]en\[YAcute] bod P'"];
        Print["\[Bullet] Zelen\[YAcute] obl\[UAcute]k: Zn\[AAcute]zornenie uhlu rot\[AAcute]cie"];
        Print["\[Bullet] Zelen\[EAcute] preru\[SHacek]ovan\[EAcute] \[CHacek]iary: Spojenia bodov s po\[CHacek]iatkom"];
        Print["\[Bullet] Mrie\[ZHacek]ka: S\[UAcute]radnicov\[YAcute] syst\[EAcute]m"];
        Print["\[Bullet] O: Po\[CHacek]iatok s\[UAcute]radnicovej s\[UAcute]stavy - stred rot\[AAcute]cie"];
        
        Print[Style["\nP\[OHat]vodn\[EAcute] s\[UAcute]radnice (modr\[AAcute]):", Bold]];
        Print[Row[{Style["Bod P: ", RGBColor[0.1, 0.1, 1]], normalizedCoordinates}]];
                
        Print[Style["\nV\[YAcute]sledn\[EAcute] s\[UAcute]radnice (\[CHacek]erven\[AAcute]):", Bold]];
        Print[Row[{Style["Bod P': ", RGBColor[1, 0.1, 0.1]], expandedOutputCoordinates}]];
                
        Print[Style["\nInverzn\[AAcute] transform\[AAcute]cia:", Bold]];
        Print["Pre vr\[AAcute]tenie do p\[OHat]vodn\[EAcute]ho stavu pou\[ZHacek]ite rot\[AAcute]ciu o opa\[CHacek]n\[YAcute] uhol:"];
        Print["\[Alpha]' = -\[Alpha] = ", invAngle, "\[Degree]"];
        
        (* MATEMATICK\[CapitalEAcute] VLASTNOSTI TRANSFORM\[CapitalAAcute]CIE *)
        Print[Style["\nMATEMATICK\[CapitalEAcute] VLASTNOSTI:", Bold, 14]];
        Print["\[Bullet] Rot\[AAcute]cia okolo po\[CHacek]iatku zachov\[AAcute]va vzdialenos\[THacek] bodu od po\[CHacek]iatku s\[UAcute]stavy [0,0]"];
        Print["\[Bullet] Rot\[AAcute]cia je izometrick\[AAcute] transform\[AAcute]cia - zachov\[AAcute]va vzdialenosti"];
        Print["\[Bullet] Rot\[AAcute]cia zachov\[AAcute]va uhly medzi \[UAcute]se\[CHacek]kami a ich d\:013a\[ZHacek]ky"];
        Print["\[Bullet] Po\[CHacek]iatok s\[UAcute]radnicovej s\[UAcute]stavy [0,0] je fixn\[YAcute] bod rot\[AAcute]cie - zost\[AAcute]va nezmenen\[YAcute]"];
        Print["\[Bullet] Ak bod P le\[ZHacek]\[IAcute] na kruhu so stredom v po\[CHacek]iatku, po rot\[AAcute]cii zost\[AAcute]va na tom istom kruhu"];
        
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
        
        (* Vracame ROZVINUT\[CapitalEAcute] a ZJEDNODU\[CapitalSHacek]EN\[CapitalEAcute] s\[UAcute]radnice pre \[DHacek]al\[SHacek]ie transform\[AAcute]cie *)
        expandedOutputCoordinates
    ];
    
(* Defin\[IAcute]cia chybov\[YAcute]ch spr\[AAcute]v *)
BodRotacia::invalidInput = "Neplatn\[YAcute] vstup. O\[CHacek]ak\[AAcute]vaj\[UAcute] sa s\[UAcute]radnice bodu v 2D priestore.";

End[];
EndPackage[];
