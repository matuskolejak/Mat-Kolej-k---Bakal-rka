(* ::Package:: *)

(* ::Package:: *)
(**)


(* ::Package:: *)
(**)


(* ::Package:: *)
(**)




BeginPackage["UseckaHardBalik`Transforms`ZvacsenieZmensenie`", {"UseckaHardBalik`"}];


UseckaZvacsenieZmensenie::usage = 
    "UseckaZvacsenieZmensenie[vertices_] zobraz\[IAcute] interakt\[IAcute]vny tutori\[AAcute]l pre zv\[ADoubleDot]\[CHacek]\[SHacek]enie/zmen\[SHacek]enie \[UAcute]se\[CHacek]ky.";

ZvacsenieParams::usage = "ZvacsenieParams je globalny vektor {sx, sy} pre koeficienty zvacsenia/zmensenia, ktory sa pouziva v UseckaZvacsenieZmensenie funkcii.";

GenerateScalingParameters::usage = "GenerateScalingParameters[] generuje n\[AAcute]hodn\[EAcute] parametre pre zv\[ADoubleDot]\[CHacek]\[SHacek]enie/zmen\[SHacek]enie.";

Begin["`Private`"];


ZvacsenieParams = {0, 0};

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


DetectScalingParameters[originalVertices_, finalVertices_] := 
    Module[{sx, sy, foundScaling = False, detectedSx, detectedSy, i = 1, point, scaledPoint},
        While[i <= 2 && !foundScaling,
            point = originalVertices[[i]];
            scaledPoint = finalVertices[[i]];
            
            
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
        
        
        If[foundScaling, 
            {Simplify[detectedSx], Simplify[detectedSy]}, 
            None
        ]
    ];


GenerateScalingParameters[] := 
    Module[{sx, sy, citatel, menovatel, typTransformacie, popis, valid = False, narocnost},
        
        narocnost = RandomReal[];
        
        While[!valid,
            
            If[narocnost < 0.7,
                
                citatel = RandomChoice[{1, 2, 3}];
                menovatel = RandomChoice[{1, 2, 3}];
                sx = citatel/menovatel // Simplify;
                
                citatel = RandomChoice[{1, 2, 3}];
                menovatel = RandomChoice[{1, 2, 3}];
                sy = citatel/menovatel // Simplify;
                
                
                If[IntegerQ[sx] && IntegerQ[sy],
                    If[RandomReal[] < 0.5,
                        sx = 1/sx,
                        sy = 1/sy
                    ]
                ],
                
                
                citatel = RandomInteger[{1, 2}];
                menovatel = RandomInteger[{1, 2}];
                sx = citatel/menovatel // Simplify;
                
                citatel = RandomInteger[{1, 2}];
                menovatel = RandomInteger[{1, 2}];
                sy = citatel/menovatel // Simplify;
            ];
            
            
            valid = 
                
                1/4 <= sx <= 4 && 1/4 <= sy <= 4 &&
                
                (sx != 1 || sy != 1);
        ];
        
        
        {typTransformacie, popis, spolPodst} = IdentifyTransformationType[sx, sy];
        
        
        ZvacsenieParams = {sx, sy};
        
        
        {sx, sy, typTransformacie, popis, spolPodst}
    ];


UseckaZvacsenieZmensenieNoDisplayWithParams[inputVertices_] := 
    Module[{sx, sy, typTransformacie, popis, spolPodst, finalVertices, 
            normalizedVertices, detectedScaling = None, detectedFinalVertices = None},
            
        
        If[Length[inputVertices] > 2 && Mod[Length[inputVertices], 2] == 0,
            
            normalizedVertices = inputVertices[[1;;2]];
            detectedFinalVertices = inputVertices[[-(2;;-1)]];
            
            
            detectedScaling = DetectScalingParameters[normalizedVertices, detectedFinalVertices],
            
            
            normalizedVertices = inputVertices
        ];
            
        
        If[detectedScaling =!= None,
            sx = detectedScaling[[1]];
            sy = detectedScaling[[2]];
            {typTransformacie, popis, spolPodst} = 
                IdentifyTransformationType[sx, sy];
            finalVertices = detectedFinalVertices,
            
            
            If[ZvacsenieParams =!= {0, 0},
                
                {sx, sy} = ZvacsenieParams;
                
                ZvacsenieParams = {0, 0};
                {typTransformacie, popis, spolPodst} = 
                    IdentifyTransformationType[sx, sy],
            
                
                {sx, sy, typTransformacie, popis, spolPodst} = 
                    GenerateScalingParameters[]
            ];
            
            
            finalVertices = Map[
                Function[point,
                    {Simplify[point[[1]]*sx], Simplify[point[[2]]*sy]}
                ],
                normalizedVertices
            ]
        ];
        
        
        ZvacsenieParams = {sx, sy};
        
        
        {{sx, sy}, finalVertices}
    ];


DisplayIntuitiveMathMatrixMultiplication[matrix_, vector_, result_, pointName_] := 
    Module[{darkGreen = RGBColor[0, 0.5, 0]},
        
        Print[Style["Maticov\[YAcute] z\[AAcute]pis transform\[AAcute]cie bodu " <> pointName <> ":", Bold]];
        
        
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
        
        
        Print[Style["Podrobn\[YAcute] v\[YAcute]po\[CHacek]et jednotliv\[YAcute]ch s\[UAcute]radn\[IAcute]c:", Bold]];
        
        
        Print[Style["\[Bullet] V\[YAcute]po\[CHacek]et novej x-ovej s\[UAcute]radnice (1. riadok matice):", Brown]];
        
        
        Print[
            Grid[{
                {
                    "   [", 
                    Style[matrix[[1, 1]], Red], 
                    Style[matrix[[1, 2]], Red], 
                    "] \[CenterDot] [",
                    Style[vector[[1]], Blue],
                    Style[vector[[2]], Blue],
                    "]^T =",
                    Style[result[[1]], darkGreen]
                }
            }, Alignment -> Left]
        ];
        
        
        Print["   = (", Style[matrix[[1, 1]], Red], " \[CenterDot] ", Style[vector[[1]], Blue], 
              ") + (", Style[matrix[[1, 2]], Red], " \[CenterDot] ", Style[vector[[2]], Blue], ")"];
        
        
        Print["   = ", Style[matrix[[1, 1]]*vector[[1]], Purple], 
              " + ", Style[matrix[[1, 2]]*vector[[2]], Purple]];
        
        
        Print["   = ", Style[result[[1]], darkGreen]];
        
        
        Print[Style["\n\[Bullet] V\[YAcute]po\[CHacek]et novej y-ovej s\[UAcute]radnice (2. riadok matice):", Brown]];
        
        
        Print[
            Grid[{
                {
                    "   [", 
                    Style[matrix[[2, 1]], Red], 
                    Style[matrix[[2, 2]], Red], 
                    "] \[CenterDot] [",
                    Style[vector[[1]], Blue],
                    Style[vector[[2]], Blue],
                    "]^T =",
                    Style[result[[2]], darkGreen]
                }
            }, Alignment -> Left]
        ];
        
        
        Print["   = (", Style[matrix[[2, 1]], Red], " \[CenterDot] ", Style[vector[[1]], Blue], 
              ") + (", Style[matrix[[2, 2]], Red], " \[CenterDot] ", Style[vector[[2]], Blue], ")"];
        
        
        Print["   = ", Style[matrix[[2, 1]]*vector[[1]], Purple], 
              " + ", Style[matrix[[2, 2]]*vector[[2]], Purple]];
        
        
        Print["   = ", Style[result[[2]], darkGreen]];
        
        
        Print[Style["\nV\[YAcute]sledn\[AAcute] reprezent\[AAcute]cia bodu " <> pointName <> "':", Bold]];
        Print["[", Style[result[[1]], darkGreen], ", ", Style[result[[2]], darkGreen], "]"];
    ];

UseckaZvacsenieZmensenie[inputVertices_] := 
    Module[{sx, sy, typTransformacie, popis, spolPodst, finalVertices, 
            normalizedVertices, detectedScaling = None, detectedFinalVertices = None,
            transformMatrix, darkGreen = RGBColor[0, 0.5, 0]},
            
        
        If[Length[inputVertices] > 2 && Mod[Length[inputVertices], 2] == 0,
            
            normalizedVertices = inputVertices[[1;;2]];
            
            detectedFinalVertices = inputVertices[[-(2;;-1)]];
            
            
            detectedScaling = DetectScalingParameters[normalizedVertices, detectedFinalVertices],
            
            
            normalizedVertices = inputVertices
        ];
            
        
        If[detectedScaling =!= None,
            sx = detectedScaling[[1]];
            sy = detectedScaling[[2]];
            {typTransformacie, popis, spolPodst} = IdentifyTransformationType[sx, sy];
            finalVertices = detectedFinalVertices,
            
            
            If[ZvacsenieParams =!= {0, 0},
                
                {sx, sy} = ZvacsenieParams;
                
                ZvacsenieParams = {0, 0},
            
                
                {sx, sy, typTransformacie, popis, spolPodst} = GenerateScalingParameters[]
            ];
            
            
            finalVertices = Map[
                Function[point,
                    {Simplify[point[[1]]*sx], Simplify[point[[2]]*sy]}
                ],
                normalizedVertices
            ];
            
            
            {typTransformacie, popis, spolPodst} = IdentifyTransformationType[sx, sy]
        ];
        
        
        transformMatrix = {{sx, 0}, {0, sy}};
        
        
        Print[Style[
            "GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE \[CapitalUAcute]SE\[CapitalCHacek]KY - ZV\[CapitalADoubleDot]\[CapitalCHacek]\[CapitalSHacek]ENIE/ZMEN\[CapitalSHacek]ENIE", 
            Bold, 16]];
        Print[Style["========================================", Bold]];
        
        
        Print[Style["\nZADANIE:", Bold, 14]];
        Print["Majme \[UAcute]se\[CHacek]ku AB s bodmi:"];
        Print[MatrixForm[Map[Style[#, Blue] &, normalizedVertices, {2}]]];
        
        
        Print[Style["\nVykonajte " <> ToString[spolPodst] <> " \[UAcute]se\[CHacek]ky pomocou transforma\[CHacek]nej matice v 2D priestore:", Bold]];
        Print[MatrixForm[{
            {Style[sx, Red], Style[0, Red]}, 
            {Style[0, Red], Style[sy, Red]}
        }]];
        
        
        Print[Style["\nTE\[CapitalOAcute]RIA MATICOV\[CapitalEAcute]HO ZOBRAZENIA ZV\[CapitalADoubleDot]\[CapitalCHacek]\[CapitalSHacek]ENIA/ZMEN\[CapitalSHacek]ENIA:", Bold, 14]];
        Print["Pri zv\[ADoubleDot]\[CHacek]\[SHacek]en\[IAcute]/zmen\[SHacek]en\[IAcute] v 2D priestore pou\[ZHacek]\[IAcute]vame maticov\[EAcute] n\[AAcute]sobenie, ktor\[EAcute] umo\[ZHacek]\[NHacek]uje zmeni\[THacek] ve\:013ekos\[THacek] objektu nez\[AAcute]visle v smere os\[IAcute] x a y:"];
        
        
        Print["\nTransforma\[CHacek]n\[AAcute] matica zv\[ADoubleDot]\[CHacek]\[SHacek]enia/zmen\[SHacek]enia:"];
        Print[MatrixForm[{
            {Style["sx", Red], Style[0, Red]}, 
            {Style[0, Red], Style["sy", Red]}
        }]];
        
        
        Print["\nAk je bod P = [x, y] v z\[AAcute]kladn\[YAcute]ch s\[UAcute]radniciach, zv\[ADoubleDot]\[CHacek]\[SHacek]enie/zmen\[SHacek]enie m\[OHat]\[ZHacek]eme zap\[IAcute]sa\[THacek] ako maticov\[EAcute] n\[AAcute]sobenie:"];
        
        Print[Row[{
            MatrixForm[{
                {Style["sx", Red], Style[0, Red]}, 
                {Style[0, Red], Style["sy", Red]}
            }],
            " \[CenterDot] ",
            MatrixForm[{
                {Style["x", Blue]}, 
                {Style["y", Blue]}
            }],
            " = ",
            MatrixForm[{
                {Style["x\[CenterDot]sx", darkGreen]}, 
                {Style["y\[CenterDot]sy", darkGreen]}
            }]
        }]];
        
        Print["\nPre na\[SHacek]e hodnoty:"];
        Print["sx = ", Style[FormatExactValue[sx], Red], " (" <> If[sx > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]ovanie", "zmen\[SHacek]ovanie"] <> " v smere osi x)"];
        Print["sy = ", Style[FormatExactValue[sy], Red], " (" <> If[sy > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]ovanie", "zmen\[SHacek]ovanie"] <> " v smere osi y)"];
        
        Print[Style["\nV\[CapitalYAcute]PO\[CapitalCHacek]ET TRANSFORM\[CapitalAAcute]CIE JEDNOTLIV\[CapitalYAcute]CH BODOV:", Bold, 14]];
        
        
        Table[
            Module[{x = normalizedVertices[[i, 1]], 
                   y = normalizedVertices[[i, 2]],
                   resultVector},
                   
                Print[Style["\nBOD " <> FromCharacterCode[64 + i] <> ":", Bold, 14]];
                Print["P\[OHat]vodn\[EAcute] s\[UAcute]radnice: [", Style[FormatExactValue[x], Blue], ", ", Style[FormatExactValue[y], Blue], "]"];
                
                
                resultVector = {Simplify[x*sx], Simplify[y*sy]};
                
                
                DisplayIntuitiveMathMatrixMultiplication[
                    transformMatrix, 
                    {x, y}, 
                    resultVector,
                    FromCharacterCode[64 + i]
                ];
                
                
                Print[Style["\nAlternat\[IAcute]vny v\[YAcute]po\[CHacek]et pomocou z\[AAcute]kladn\[YAcute]ch vzorcov zv\[ADoubleDot]\[CHacek]\[SHacek]enia/zmen\[SHacek]enia:", Bold]];
                Print["x' = x\[CenterDot]sx  =>  ", Style[FormatExactValue[x], Blue], " \[CenterDot] ", Style[FormatExactValue[sx], Red], " = ", Style[FormatExactValue[resultVector[[1]]], darkGreen]];
                Print["y' = y\[CenterDot]sy  =>  ", Style[FormatExactValue[y], Blue], " \[CenterDot] ", Style[FormatExactValue[sy], Red], " = ", Style[FormatExactValue[resultVector[[2]]], darkGreen]];
                
                Print[Style["\nV\[YAcute]sledn\[EAcute] s\[UAcute]radnice bodu " <> FromCharacterCode[64 + i] <> "':", Bold]];
                Print["[", Style[FormatExactValue[resultVector[[1]]], darkGreen], ", ", Style[FormatExactValue[resultVector[[2]]], darkGreen], "]"];
                
                Print[Style["--------------------------------------------------", Bold]];
            ],
            {i, 2}  
        ];
        
        
        Print[Style["\nV\[CapitalYAcute]SLEDOK TRANSFORM\[CapitalAAcute]CIE CELEJ \[CapitalUAcute]SE\[CapitalCHacek]KY:", Bold, 14]];
        Print[Style["P\[OHat]vodn\[AAcute] \[UAcute]se\[CHacek]ka AB:", Blue, Bold]];
        Print[MatrixForm[normalizedVertices]];
        
        Print[Style["Transformovan\[AAcute] \[UAcute]se\[CHacek]ka A'B':", Red, Bold]];
        Print[MatrixForm[finalVertices]];
        
        
        Print[Style["\nVIZUALIZ\[CapitalAAcute]CIA TRANSFORM\[CapitalAAcute]CIE:", Bold]];
        Print[CreateVisualization[normalizedVertices, finalVertices, sx, sy, typTransformacie]];
        
        
        Print[Style["\nLEGENDA:", Bold, 14]];
        Print[Style["\[Bullet] Modr\[EAcute]", Blue], " body a \[CHacek]iary: P\[OHat]vodn\[AAcute] \[UAcute]se\[CHacek]ka"];
        Print[Style["\[Bullet] \[CapitalCHacek]erven\[EAcute]", Red], " body a \[CHacek]iary: Transformovan\[AAcute] \[UAcute]se\[CHacek]ka"];
        Print[Style["\[Bullet] Zelen\[EAcute]", darkGreen], " preru\[SHacek]ovan\[EAcute] \[CHacek]iary: Dr\[AAcute]ha zmeny ve\:013ekosti bodov"];
        
        Print[Style["\nFAREBN\[CapitalEAcute] OZNA\[CapitalCHacek]ENIA V MATICOV\[CapitalYAcute]CH V\[CapitalYAcute]PO\[CapitalCHacek]TOCH:", Bold]];
        Print[Style["\[Bullet] \[CapitalCHacek]erven\[AAcute]:", Red], " Transforma\[CHacek]n\[AAcute] matica"];
        Print[Style["\[Bullet] Modr\[AAcute]:", Blue], " Vstupn\[EAcute] s\[UAcute]radnice"];
        Print[Style["\[Bullet] Fialov\[AAcute]:", Purple], " Medziv\[YAcute]po\[CHacek]ty"];
        Print[Style["\[Bullet] Zelen\[AAcute]:", darkGreen], " V\[YAcute]sledn\[EAcute] s\[UAcute]radnice"];
                
                
        Print[Style["\nP\[OHat]vodn\[EAcute] body (modr\[AAcute]):", Bold]];
        Table[
            Print[Row[{Style["Bod " <> FromCharacterCode[64 + i] <> ": ", 
                RGBColor[0.1, 0.1, 1]], normalizedVertices[[i]]}]], {i, 2}];  
                
        Print[Style["\nV\[YAcute]sledn\[EAcute] body (\[CHacek]erven\[AAcute]):", Bold]];
        Module[{expandedOutput},
            Table[
                expandedOutput = Expand[finalVertices[[i]]];
                Print[Row[{Style["Bod " <> FromCharacterCode[64 + i] <> "': ", 
                    RGBColor[1, 0.1, 0.1]], expandedOutput}]], {i, 2}];  
        ];
                
        Print[Style["\nInverzn\[AAcute] transform\[AAcute]cia:", Bold]];
        Print["Pre vr\[AAcute]tenie do p\[OHat]vodn\[EAcute]ho stavu pou\[ZHacek]ite koeficienty:"];
        Print["sx' = 1/sx = ", FormatExactValue[1/sx], " (" <> If[sx < 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]ovanie", "zmen\[SHacek]ovanie"] <> " v smere osi x)"];
        Print["sy' = 1/sy = ", FormatExactValue[1/sy], " (" <> If[sy < 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]ovanie", "zmen\[SHacek]ovanie"] <> " v smere osi y)"];
        
        
        Print[Style["\nMATEMATICK\[CapitalEAcute] VLASTNOSTI:", Bold, 14]];
        If[sx == sy,
            Print["\[Bullet] Pri rovnak\[YAcute]ch koeficientoch (sx = sy = " <> FormatExactValue[sx] <> "):"],
            Print["\[Bullet] Pri r\[OHat]znych koeficientoch (sx = " <> FormatExactValue[sx] <> ", sy = " <> FormatExactValue[sy] <> "):"]
        ];
        
        If[sx == sy,
            Print["  - Zachov\[AAcute]va sa tvar (podobnos\[THacek]) geometrick\[YAcute]ch \[UAcute]tvarov"];
            Print["  - Zachov\[AAcute]vaj\[UAcute] sa v\[SHacek]etky uhly medzi \[UAcute]se\[CHacek]kami"];
            Print["  - Pomer d\:013a\[ZHacek]ok \[UAcute]se\[CHacek]iek sa zachov\[AAcute]va"];
            Print["  - D\:013a\[ZHacek]ka \[UAcute]se\[CHacek]ky sa " <> If[sx > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]\[IAcute] " <> FormatExactValue[sx] <> "-kr\[AAcute]t", "zmen\[SHacek]\[IAcute] na " <> FormatExactValue[1/sx] <> ". \[CHacek]as\[THacek]"]],
            
            Print["  - Men\[IAcute] sa sklon \[UAcute]se\[CHacek]ky, ak \[UAcute]se\[CHacek]ka nie je rovnobe\[ZHacek]n\[AAcute] s niektorou z os\[IAcute]"];
            Print["  - D\:013a\[ZHacek]ka \[UAcute]se\[CHacek]ky sa men\[IAcute] r\[OHat]zne v z\[AAcute]vislosti od jej smeru"]
        ];
        
        Print["\[Bullet] Nemen\[IAcute] sa poloha po\[CHacek]iatku s\[UAcute]radnicovej s\[UAcute]stavy [0,0]"];
        Print["\[Bullet] Body, ktor\[EAcute] le\[ZHacek]ia na osi x, zostan\[UAcute] le\[ZHacek]a\[THacek] na osi x (ich y-ov\[AAcute] s\[UAcute]radnica zost\[AAcute]va nulov\[AAcute])"];
        Print["\[Bullet] Body, ktor\[EAcute] le\[ZHacek]ia na osi y, zostan\[UAcute] le\[ZHacek]a\[THacek] na osi y (ich x-ov\[AAcute] s\[UAcute]radnica zost\[AAcute]va nulov\[AAcute])"];
        
        
        ZvacsenieParams = {sx, sy};
        
        
        finalVertices
    ];


CreateVisualization[originalVertices_, finalVertices_, sx_, sy_, typTransformacie_] := 
    Module[{allVertices, xMin, xMax, yMin, yMax, rangeBuffer, xRange, yRange, 
            labelOffsets, brightGreen},
            
        
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
            Line[originalVertices],
            
            
            Red, Thick, Opacity[0.7],
            Line[finalVertices],
            
            
            brightGreen, Dashed,
            Table[
                Line[{originalVertices[[i]], finalVertices[[i]]}],
                {i, Length[originalVertices]}
            ],
            
            
            White, 
            Table[
                Disk[originalVertices[[i]], 0.45],
                {i, 2}  
            ],
            Table[
                Disk[finalVertices[[i]], 0.45],
                {i, 2}  
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
                {i, 2}  
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


UseckaZvacsenieZmensenie::invalidInput = "Neplatn\[YAcute] vstup. O\[CHacek]ak\[AAcute]va sa matica alebo zoznam s\[UAcute]radn\[IAcute]c \[UAcute]se\[CHacek]ky.";
UseckaZvacsenieZmensenie::insufficientVertices = "Nedostato\[CHacek]n\[YAcute] po\[CHacek]et bodov. S\[UAcute] potrebn\[EAcute] aspo\[NHacek] 2 body pre \[UAcute]se\[CHacek]ku.";
    
End[];
EndPackage[];



