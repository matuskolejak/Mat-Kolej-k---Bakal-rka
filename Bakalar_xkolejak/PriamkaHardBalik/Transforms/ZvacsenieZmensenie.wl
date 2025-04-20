(* ::Package:: *)

(* ::Package:: *)
(**)


BeginPackage["PriamkaHardBalik`Transforms`ZvacsenieZmensenie`", {"PriamkaHardBalik`"}];

(* Export public symbols *)
PriamkaZvacsenieZmensenie::usage = 
    "PriamkaZvacsenieZmensenie[vertices_] zobra\[ZHacek]\[IAcute] interakt\[IAcute]vny tutori\[AAcute]l pre zv\[ADoubleDot]\[CHacek]\[SHacek]enie/zmen\[SHacek]enie priamky pomocou mat\[IAcute]c.";

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

(* Funkcia na kontrolu validity priamky *)
validLineQ[p1_, p2_] := 
    Module[{distance},
        (* Kontrola \[CHacek]i s\[UAcute] vstupn\[EAcute] body validn\[EAcute] *)
        If[!VectorQ[p1, NumberQ] || !VectorQ[p2, NumberQ] || 
           Length[p1] != 2 || Length[p2] != 2,
            Return[False]
        ];
        
        (* Kontrola, \[CHacek]i body nie s\[UAcute] pr\[IAcute]li\[SHacek] bl\[IAcute]zko pri sebe *)
        distance = EuclideanDistance[p1, p2];
        If[distance < 0.5, Return[False]];
        
        (* Kontrola, \[CHacek]i body nie s\[UAcute] pr\[IAcute]li\[SHacek] \[DHacek]aleko od seba *)
        If[distance > 20, Return[False]];
        
        (* Kontrola, \[CHacek]i body nie s\[UAcute] mimo pracovnej oblasti *)
        If[Max[Abs[p1[[1]]], Abs[p1[[2]]], Abs[p2[[1]]], Abs[p2[[2]]]] > 15,
            Return[False]
        ];
        
        (* Ak pre\[SHacek]li v\[SHacek]etky kontroly, priamka je validn\[AAcute] *)
        True
    ];

(* Funkcia na generovanie parametra \[SHacek]k\[AAcute]lovania *)
GenerateScalingFactor[] := 
    Module[{factor, valid = False, narocnost},
        (* Ur\[CHacek]enie n\[AAcute]ro\[CHacek]nosti *)
        narocnost = RandomReal[];
        
        While[!valid,
            (* Generovanie faktora pre \[SHacek]k\[AAcute]lovanie priamky *)
            If[narocnost < 0.7,
                (* N\[AAcute]ro\[CHacek]nej\[SHacek]ie hodnoty *)
                factor = RandomChoice[{1/3, 1/2, 2/3, 3/2, 2, 3}],
                
                (* Jednoduch\[SHacek]ie hodnoty *)
                factor = RandomChoice[{1/2, 2}]
            ];
            
            (* Kontrola validity *)
            valid = 1/4 <= factor <= 4 && factor != 1;
        ];
        
        factor
    ];

(* Funkcia na normaliz\[AAcute]ciu vstupn\[YAcute]ch bodov - ZACHOV\[CapitalAAcute]VA PRESN\[CapitalEAcute] HODNOTY *)
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
                        If[Length[cleanedVertices] >= 2,
                            cleanedVertices[[1;;2]],
                            Message[PriamkaTransformacia::invalidInput]; 
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
                Message[PriamkaTransformacia::invalidInput]; 
                Abort[]
        ];
        
        If[Length[processedVertices] < 2,
            Message[PriamkaTransformacia::insufficientVertices];
            Abort[]
        ];
        
        (* D\[CapitalOHat]LE\[CapitalZHacek]IT\[CapitalAAcute] ZMENA: Odstr\[AAcute]nime \[SHacek]k\[AAcute]lovanie bodov *)
        (* Star\[EAcute] \[SHacek]k\[AAcute]lovanie:
        If[Max[Abs[processedVertices[[All, 1]]]] > 10 || Max[Abs[processedVertices[[All, 2]]]] > 10,
            processedVertices = processedVertices / (Max[Abs[Flatten[processedVertices]]] / 6);
        ];
        *)
        
        (* Vraciame presn\[EAcute] hodnoty bez \[UAcute]prav *)
        processedVertices[[1;;2]]
    ];
    
(* Hlavn\[AAcute] funkcia pre zv\[ADoubleDot]\[CHacek]\[SHacek]enie/zmen\[SHacek]enie priamky *)
PriamkaZvacsenieZmensenie[inputVertices_] := 
    Module[{factor, typTransformacie, popis, finalVertices, midpoint, 
            normalizedVertices, scaleMatrix, transformMatrix, homogenA, homogenB, transformedA, transformedB,
            detectedScaling = None, detectedFinalVertices = None},
            
        (* Normaliz\[AAcute]cia vstupn\[YAcute]ch bodov - POZOR, berieme body priamo bez zmeny *)
        normalizedVertices = NormalizeVertices[inputVertices];
        
        (* Stred priamky - bude pou\[ZHacek]it\[YAcute] ako referen\[CHacek]n\[YAcute] bod *)
        midpoint = (normalizedVertices[[1]] + normalizedVertices[[2]])/2;
        
        (* Generovanie faktora \[SHacek]k\[AAcute]lovania pre priamku *)
        factor = GenerateScalingFactor[];
        
        (* Ur\[CHacek]enie typu transform\[AAcute]cie a popisu *)
        typTransformacie = If[factor > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]enie", "zmen\[SHacek]enie"];
        popis = If[factor > 1, 
                   "zv\[ADoubleDot]\[CHacek]\[SHacek]enie " <> ToString[factor] <> "-kr\[AAcute]t", 
                   "zmen\[SHacek]enie na " <> ToString[1/factor] <> ". \[CHacek]as\[THacek]"];
                   
        (* 3. Za\[CHacek]iatok zobrazenia pre \[ZHacek]iaka *)
        Print[Style[
            "GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE PRIAMKY - ZV\[CapitalADoubleDot]\[CapitalCHacek]\[CapitalSHacek]ENIE/ZMEN\[CapitalSHacek]ENIE POMOCOU MAT\[CapitalIAcute]C", 
            Bold, 16]];
        Print[Style["========================================", Bold]];
        
        (* ZADANIE *)
        Print[Style["\nZADANIE:", Bold, 14]];
        Print["Majme priamku AB s bodmi:"];
        Print[MatrixForm[Map[Style[#, Blue] &, normalizedVertices, {2}]]];
        
        Print["\nVykonajte " <> typTransformacie <> " priamky pomocou centr\[AAcute]lneho \[SHacek]k\[AAcute]lovania s koeficientom k = ", 
              Style[factor, Red], " vzh\:013eadom na stred priamky S = ", midpoint, "."];
        
        (* POSTUP *)
        Print[Style["\nPOSTUP:", Bold, 14]];
        
        Print[Style["\n1. Ur\[CHacek]enie stredu priamky (referen\[CHacek]n\[EAcute]ho bodu):", Bold]];
        Print["   S = (A + B)/2 = (", normalizedVertices[[1]], " + ", normalizedVertices[[2]], ")/2 = ", midpoint];
        Print["   Pozn\[AAcute]mka: V tomto pr\[IAcute]pade n\[AAcute]m bol stred priamky u\[ZHacek] zadan\[YAcute] v zadan\[IAcute] ako S = ", midpoint, "."];
        
        (* Vysvetlenie princ\[IAcute]pu centr\[AAcute]lneho \[SHacek]k\[AAcute]lovania *)
        Print[Style["\n\[CapitalCHacek]o je centr\[AAcute]lne \[SHacek]k\[AAcute]lovanie?", Bold]];
        Print["Centr\[AAcute]lne \[SHacek]k\[AAcute]lovanie znamen\[AAcute], \[ZHacek]e \[UAcute]se\[CHacek]ku (priamku) zv\[ADoubleDot]\[CHacek]\[SHacek]\[IAcute]me alebo zmen\[SHacek]\[IAcute]me vzh\:013eadom na ur\[CHacek]it\[YAcute] bod,"];
        Print["ktor\[YAcute] naz\[YAcute]vame stred \[SHacek]k\[AAcute]lovania. Tento bod sa pri transform\[AAcute]cii nepohne. Ostatn\[EAcute] body sa bu\[DHacek]:"];
        Print["1. Vz\[DHacek]a\:013euj\[UAcute] od stredu (ak k > 1) - vtedy ide o zv\[ADoubleDot]\[CHacek]\[SHacek]enie"];
        Print["2. Pribli\[ZHacek]uj\[UAcute] k stredu (ak k < 1) - vtedy ide o zmen\[SHacek]enie"];
        
        Print[Style["\n2. Vytvorenie transforma\[CHacek]nej matice:", Bold]];
        Print["Pre centr\[AAcute]lne \[SHacek]k\[AAcute]lovanie vzh\:013eadom na bod S s koeficientom k potrebujeme vytvori\[THacek] transforma\[CHacek]n\[UAcute] maticu."];
        Print["T\[AAcute]to matica mus\[IAcute] zabezpe\[CHacek]i\[THacek]:"];
        Print["   a) Bod S zostane na mieste (je to fixn\[YAcute] bod)"];
        Print["   b) V\[SHacek]etky ostatn\[EAcute] body sa posun\[UAcute] smerom k bodu S"];
        Print["   c) Vzdialenos\[THacek] od S sa zmen\[IAcute] k-kr\[AAcute]t"];
        
        Print[Style["\nAko sa vytv\[AAcute]ra transforma\[CHacek]n\[AAcute] matica pre centr\[AAcute]lne \[SHacek]k\[AAcute]lovanie?", Bold]];
        Print["Transform\[AAcute]ciu rozdel\[IAcute]me na tri jednoduch\[EAcute] kroky:"];
        Print["1. Posun bodu S do po\[CHacek]iatku s\[UAcute]radnicovej s\[UAcute]stavy [0,0]"];
        Print["2. \[CapitalSHacek]k\[AAcute]lovanie koeficientom k = ", factor];
        Print["3. Sp\[ADoubleDot]tn\[YAcute] posun po\[CHacek]iatku na poz\[IAcute]ciu bodu S"];
        
        Print[Style["\nKrok 1: Posun bodu S do po\[CHacek]iatku [0,0]", Bold]];
        Print["Mus\[IAcute]me posun\[UAcute]\[THacek] bod S = ", midpoint, " do bodu [0,0]."];
        Print["To znamen\[AAcute], \[ZHacek]e posunieme v\[SHacek]etky body o vektor: ", -midpoint];
        Print["Matica posunu v homog\[EAcute]nnych s\[UAcute]radniciach:"];
        
        (* Matica posunu stredu S do po\[CHacek]iatku *)
        transformMatrix = {
            {1, 0, -midpoint[[1]]},
            {0, 1, -midpoint[[2]]},
            {0, 0, 1}
        };
        
        Print[MatrixForm[transformMatrix]];
        
        Print[Style["\nKrok 2: \[CapitalSHacek]k\[AAcute]lovanie koeficientom k = ", factor, Bold]];
        Print["V po\[CHacek]iatku [0,0] vykon\[AAcute]me \[SHacek]k\[AAcute]lovanie s koeficientom k = ", factor, "."];
        Print["Matica \[SHacek]k\[AAcute]lovania v homog\[EAcute]nnych s\[UAcute]radniciach:"];
        
        (* Matica \[SHacek]k\[AAcute]lovania *)
        scaleMatrix = {
            {factor, 0, 0},
            {0, factor, 0},
            {0, 0, 1}
        };
        
        Print[MatrixForm[scaleMatrix]];
        
        Print[Style["\nKrok 3: Sp\[ADoubleDot]tn\[YAcute] posun do p\[OHat]vodnej poz\[IAcute]cie", Bold]];
        Print["Nakoniec posunieme v\[SHacek]etky body sp\[ADoubleDot]\[THacek], ale u\[ZHacek] v novej ve\:013ekosti."];
        Print["Posunieme o vektor: ", midpoint];
        Print["Matica sp\[ADoubleDot]tn\[EAcute]ho posunu v homog\[EAcute]nnych s\[UAcute]radniciach:"];
        
        (* Matica sp\[ADoubleDot]tn\[EAcute]ho posunu *)
        backMatrix = {
            {1, 0, midpoint[[1]]},
            {0, 1, midpoint[[2]]},
            {0, 0, 1}
        };
        
        Print[MatrixForm[backMatrix]];
        
        Print[Style["\nVytvorenie v\[YAcute]slednej transforma\[CHacek]nej matice:", Bold]];
        Print["Aby sme vykonali v\[SHacek]etky tri kroky naraz, potrebujeme n\[AAcute]sobi\[THacek] matice v opa\[CHacek]nom porad\[IAcute] (sprava do\:013eava):"];
        Print["V\[YAcute]sledn\[AAcute] matica = Matica kroku 3 \[Times] Matica kroku 2 \[Times] Matica kroku 1"];
        
        (* V\[YAcute]sledn\[AAcute] matica - n\[AAcute]sob\[IAcute]me sprava do\:013eava *)
        transformMatrix = backMatrix . scaleMatrix . transformMatrix;
        
        (* Zjednodu\[SHacek]enie matice *)
        transformMatrix = Simplify[transformMatrix];
        
        Print["V\[YAcute]po\[CHacek]et v\[YAcute]slednej matice:"];
        Print[MatrixForm[backMatrix], " \[Times] ", MatrixForm[scaleMatrix], " \[Times] ", MatrixForm[transformMatrix]];
        
        Print[Style["\nZjednodu\[SHacek]en\[AAcute] v\[YAcute]sledn\[AAcute] transforma\[CHacek]n\[AAcute] matica:", Bold]];
        Print["Po vyn\[AAcute]soben\[IAcute] a zjednodu\[SHacek]en\[IAcute] dost\[AAcute]vame maticu:"];
        Print[MatrixForm[transformMatrix]];
        
        (* Interpret\[AAcute]cia v\[YAcute]slednej matice *)
        Print[Style["\n\[CapitalCHacek]o znamenaj\[UAcute] hodnoty v tejto matici:", Bold]];
        Print["- V \:013eavom hornom rohu m\[AAcute]me hodnotu k = ", factor, " - koeficient \[SHacek]k\[AAcute]lovania pre x-ov\[UAcute] s\[UAcute]radnicu"];
        Print["- V strede matice m\[AAcute]me hodnotu k = ", factor, " - koeficient \[SHacek]k\[AAcute]lovania pre y-ov\[UAcute] s\[UAcute]radnicu"];
        Print["- Hodnoty v pravom st\:013apci s\[UAcute] korekcie pre zachovanie poz\[IAcute]cie bodu S"];
        
        (* Body v homog\[EAcute]nnych s\[UAcute]radniciach *)
        homogenA = Append[normalizedVertices[[1]], 1];
        homogenB = Append[normalizedVertices[[2]], 1];
        
        (* Aplik\[AAcute]cia transforma\[CHacek]nej matice na body *)
        transformedA = transformMatrix . homogenA;
        transformedB = transformMatrix . homogenB;
        
        (* Konverzia v\[YAcute]sledkov sp\[ADoubleDot]\[THacek] na 2D s\[UAcute]radnice *)
        finalVertices = {
            {transformedA[[1]], transformedA[[2]]},
            {transformedB[[1]], transformedB[[2]]}
        };
        
        Print[Style["\n3. Aplik\[AAcute]cia transform\[AAcute]cie na body priamky:", Bold]];
        Print["Aby sme mohli pou\[ZHacek]i\[THacek] na\[SHacek]u transforma\[CHacek]n\[UAcute] maticu, mus\[IAcute]me previes\[THacek] body do homog\[EAcute]nnych s\[UAcute]radn\[IAcute]c"];
        Print["(prid\[AAcute]me tretiu s\[UAcute]radnicu s hodnotou 1) a vyn\[AAcute]sobi\[THacek] ich maticou."];
        
        (* V\[YAcute]po\[CHacek]et transform\[AAcute]cie pre bod A - podrobnej\[SHacek]ie *)
        Print[Style["\nBod A:", Bold]];
        Print["P\[OHat]vodn\[EAcute] s\[UAcute]radnice bodu A: [", normalizedVertices[[1, 1]], ", ", normalizedVertices[[1, 2]], "]"];
        Print["Homog\[EAcute]nne s\[UAcute]radnice bodu A: [", homogenA[[1]], ", ", homogenA[[2]], ", ", homogenA[[3]], "]"];
        
        Print[Style["V\[YAcute]po\[CHacek]et transform\[AAcute]cie bodu A:", Bold]];
        Print["N\[AAcute]sob\[IAcute]me transforma\[CHacek]n\[UAcute] maticu s bodom A:"];
        Print[
            Row[{
                MatrixForm[transformMatrix],
                " \[Times] ",
                MatrixForm[{homogenA}[[1]]],
                " = "
            }]
        ];
        
        (* Podrobn\[YAcute] v\[YAcute]po\[CHacek]et pre bod A - po riadkoch *)
        Print[Style["V\[YAcute]po\[CHacek]et po riadkoch:", Bold]];
        
        Print["1. riadok (nov\[AAcute] x-ov\[AAcute] s\[UAcute]radnica):"];
        Print["   ", transformMatrix[[1,1]], " \[Times] ", homogenA[[1]], " + ", 
              transformMatrix[[1,2]], " \[Times] ", homogenA[[2]], " + ", 
              transformMatrix[[1,3]], " \[Times] ", homogenA[[3]]];
        Print["   = ", transformMatrix[[1,1]] * homogenA[[1]], " + ", 
              transformMatrix[[1,2]] * homogenA[[2]], " + ", 
              transformMatrix[[1,3]] * homogenA[[3]]];
        Print["   = ", transformedA[[1]]];
        
        Print["2. riadok (nov\[AAcute] y-ov\[AAcute] s\[UAcute]radnica):"];
        Print["   ", transformMatrix[[2,1]], " \[Times] ", homogenA[[1]], " + ", 
              transformMatrix[[2,2]], " \[Times] ", homogenA[[2]], " + ", 
              transformMatrix[[2,3]], " \[Times] ", homogenA[[3]]];
        Print["   = ", transformMatrix[[2,1]] * homogenA[[1]], " + ", 
              transformMatrix[[2,2]] * homogenA[[2]], " + ", 
              transformMatrix[[2,3]] * homogenA[[3]]];
        Print["   = ", transformedA[[2]]];
        
        Print["3. riadok (homog\[EAcute]nna s\[UAcute]radnica - zostane 1):"];
        Print["   ", transformMatrix[[3,1]], " \[Times] ", homogenA[[1]], " + ", 
              transformMatrix[[3,2]], " \[Times] ", homogenA[[2]], " + ", 
              transformMatrix[[3,3]], " \[Times] ", homogenA[[3]]];
        Print["   = ", transformMatrix[[3,1]] * homogenA[[1]], " + ", 
              transformMatrix[[3,2]] * homogenA[[2]], " + ", 
              transformMatrix[[3,3]] * homogenA[[3]]];
        Print["   = ", transformedA[[3]]];
        
        Print["Bod A sa transformuje na bod A' s homog\[EAcute]nnymi s\[UAcute]radnicami: [", 
              transformedA[[1]], ", ", transformedA[[2]], ", ", transformedA[[3]], "]"];
        Print["Bod A' s kart\[EAcute]zskymi s\[UAcute]radnicami: [", finalVertices[[1, 1]], ", ", finalVertices[[1, 2]], "]"];
        
        (* V\[YAcute]po\[CHacek]et transform\[AAcute]cie pre bod B - stru\[CHacek]nej\[SHacek]ie *)
        Print[Style["\nBod B:", Bold]];
        Print["P\[OHat]vodn\[EAcute] s\[UAcute]radnice bodu B: [", normalizedVertices[[2, 1]], ", ", normalizedVertices[[2, 2]], "]"];
        Print["Homog\[EAcute]nne s\[UAcute]radnice bodu B: [", homogenB[[1]], ", ", homogenB[[2]], ", ", homogenB[[3]], "]"];
        
        Print[Style["V\[YAcute]po\[CHacek]et transform\[AAcute]cie bodu B:", Bold]];
        Print["N\[AAcute]sob\[IAcute]me transforma\[CHacek]n\[UAcute] maticu s bodom B:"];
        Print[
            Row[{
                MatrixForm[transformMatrix],
                " \[Times] ",
                MatrixForm[{homogenB}[[1]]],
                " = ",
                MatrixForm[{transformedB}[[1]]]
            }]
        ];
        
        Print["Bod B sa transformuje na bod B' s kart\[EAcute]zskymi s\[UAcute]radnicami: [", 
              finalVertices[[2, 1]], ", ", finalVertices[[2, 2]], "]"];
        
        (* Kontrola a overenie *)
        Print[Style["\nOverenie v\[YAcute]sledku:", Bold]];
        Print["1. Stred priamky zostane na mieste:"];
        Print["   P\[OHat]vodn\[YAcute] stred: S = ", midpoint];
        Print["   Nov\[YAcute] stred: S' = (A' + B')/2 = (", finalVertices[[1]], " + ", 
              finalVertices[[2]], ")/2 = ", (finalVertices[[1]] + finalVertices[[2]])/2];
        
        Print["2. Zmena d\:013a\[ZHacek]ky priamky:"];
        Print["   P\[OHat]vodn\[AAcute] d\:013a\[ZHacek]ka priamky: |AB| = ", N[Norm[normalizedVertices[[2]] - normalizedVertices[[1]]]], " jednotiek"];
        Print["   Nov\[AAcute] d\:013a\[ZHacek]ka priamky: |A'B'| = ", N[Norm[finalVertices[[2]] - finalVertices[[1]]]], " jednotiek"];
        Print["   Pomer d\:013a\[ZHacek]ok: |A'B'|/|AB| = ", 
              N[Norm[finalVertices[[2]] - finalVertices[[1]]]/Norm[normalizedVertices[[2]] - normalizedVertices[[1]]]], 
              " \[TildeTilde] ", factor];
        
        (* V\[CapitalYAcute]SLEDOK *)
        Print[Style["\nV\[CapitalYAcute]SLEDOK:", Bold, 14]];
        Print["Po aplik\[AAcute]cii centr\[AAcute]lneho \[SHacek]k\[AAcute]lovania s koeficientom k = ", factor, " dost\[AAcute]vame:"];
        Print["Body priamky po " <> typTransformacie <> ":"];
        
        (* Upraven\[AAcute] a form\[AAcute]tovan\[AAcute] matica v\[YAcute]sledku *)
        Module[{expandedOutputVertices, matrixToShow},
            (* Rozvinieme a zjednodu\[SHacek]\[IAcute]me ka\[ZHacek]d\[YAcute] prvok v\[YAcute]slednej matice *)
            expandedOutputVertices = Map[Expand, finalVertices, {2}];
            
            (* Vytvor\[IAcute]me pekne form\[AAcute]tovan\[UAcute] verziu matice na zobrazenie *)
            matrixToShow = Map[Style[#, Red] &, expandedOutputVertices, {2}];
            
            (* Zobraz\[IAcute]me upraven\[UAcute] maticu *)
            Print[MatrixForm[matrixToShow]];
        ];
        
        (* ZHRNUTIE *)
        Print[Style["\nZHRNUTIE POSTUPU:", Bold, 14]];
        Print["1. Ur\[CHacek]enie stredu \[SHacek]k\[AAcute]lovania S = ", midpoint];
        Print["2. Vytvorenie transforma\[CHacek]nej matice:"];
        Print["   - Posun stredu S do po\[CHacek]iatku [0,0]"];
        Print["   - \[CapitalSHacek]k\[AAcute]lovanie koeficientom k = ", factor];
        Print["   - Sp\[ADoubleDot]tn\[YAcute] posun do p\[OHat]vodnej poz\[IAcute]cie stredu S"];
        Print["3. Aplik\[AAcute]cia transform\[AAcute]cie na body priamky:"];
        Print["   - Prevod bodov do homog\[EAcute]nnych s\[UAcute]radn\[IAcute]c (pridanie tretej s\[UAcute]radnice 1)"];
        Print["   - N\[AAcute]sobenie bodov transforma\[CHacek]nou maticou"];
        Print["   - Prevod v\[YAcute]sledku sp\[ADoubleDot]\[THacek] na kart\[EAcute]zske s\[UAcute]radnice"];
        
        Print[Style["\nVlastnosti centr\[AAcute]lneho \[SHacek]k\[AAcute]lovania:", Bold]];
        Print["1. Stred \[SHacek]k\[AAcute]lovania (bod S) zost\[AAcute]va na mieste - je to fixn\[YAcute] bod"];
        Print["2. Vzdialenosti ostatn\[YAcute]ch bodov od stredu sa " <> 
              If[factor > 1, "zv\[ADoubleDot]\[CHacek]\[SHacek]ia", "zmen\[SHacek]ia"] <> " k-kr\[AAcute]t (k = ", factor, ")"];
        Print["3. Priamky zost\[AAcute]vaj\[UAcute] priamkami, rovnobe\[ZHacek]n\[EAcute] priamky zost\[AAcute]vaj\[UAcute] rovnobe\[ZHacek]n\[YAcute]mi"];
        Print["4. Ve\:013ekosti uhlov sa zachov\[AAcute]vaj\[UAcute] (ide o podobnostn\[UAcute] transform\[AAcute]ciu)"];
        
        (* VIZUALIZ\[CapitalAAcute]CIA *)
        Print[Style["\nVIZUALIZ\[CapitalAAcute]CIA:", Bold]];
        Print[CreateVisualization[normalizedVertices, finalVertices, midpoint, factor]];
        
        (* LEGENDA *)
        Print[Style["\nLEGENDA:", Bold, 14]];
        Print["\[Bullet] Modr\[EAcute] body a \[CHacek]iary: P\[OHat]vodn\[AAcute] priamka"];
        Print["\[Bullet] \[CapitalCHacek]erven\[EAcute] body a \[CHacek]iary: Priamka po " <> typTransformacie];
        Print["\[Bullet] Zelen\[EAcute] preru\[SHacek]ovan\[EAcute] \[CHacek]iary: Dr\[AAcute]ha transform\[AAcute]cie bodov"];
        Print["\[Bullet] \[CapitalZHacek]lt\[YAcute] bod: Stred priamky (fixn\[YAcute] bod pre \[SHacek]k\[AAcute]lovanie)"];
        
        (* Vr\[AAcute]tenie nov\[YAcute]ch bodov pre \[DHacek]al\[SHacek]iu transform\[AAcute]ciu *)
        finalVertices
    ];

(* Funkcia pre vizualiz\[AAcute]ciu *)
CreateVisualization[originalVertices_, finalVertices_, midpoint_, factor_] := 
    Module[{allVertices, xMin, xMax, yMin, yMax, rangeBuffer, xRange, yRange, 
            labelOffsets, brightGreen, brightYellow},
            
        (* Defin\[IAcute]cia farieb *)
        brightGreen = RGBColor[0, 0.8, 0.2];
        brightYellow = RGBColor[1, 0.8, 0];
            
        (* Spojenie v\[SHacek]etk\[YAcute]ch bodov pre v\[YAcute]po\[CHacek]et rozsahu *)
        allVertices = Join[originalVertices, finalVertices, {midpoint}];
        
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
        
        (* Vypo\[CHacek]\[IAcute]ta\[THacek] offsety pre ozna\[CHacek]enia, aby sa neprekr\[YAcute]vali s \[CHacek]iarami *)
        labelOffsets = Table[
            Module[{originalVertex = originalVertices[[i]], 
                    finalVertex = finalVertices[[i]], 
                    offset = {0.7, 0.7}},
                
                (* Upravi\[THacek] offset pod\:013ea polohy bodov *)
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
            
            (* P\[OHat]vodn\[AAcute] priamka - modr\[AAcute] *)
            Blue, Thick, Opacity[0.7],
            Line[originalVertices],
            
            (* Zv\[ADoubleDot]\[CHacek]\[SHacek]en\[AAcute]/zmen\[SHacek]en\[AAcute] priamka - \[CHacek]erven\[AAcute] *)
            Red, Thick, Opacity[0.7],
            Line[finalVertices],
            
            (* Pomocn\[EAcute] \[CHacek]iary - zelen\[EAcute] *)
            brightGreen, Dashed,
            Table[
                Line[{originalVertices[[i]], finalVertices[[i]]}],
                {i, Length[originalVertices]}
            ],
            
            (* Stredov\[YAcute] bod - \[ZHacek]lt\[YAcute] *)
            brightYellow, 
            Disk[midpoint, 0.3],
            Black,
            Text[Style["S", Bold, 14], midpoint],
            
            (* Body s bielym pozad\[IAcute]m *)
            White, 
            Table[
                Disk[originalVertices[[i]], 0.45],
                {i, 2}
            ],
            Table[
                Disk[finalVertices[[i]], 0.45],
                {i, 2}
            ],
            
            (* Body *)
            Blue, PointSize[0.025], 
            Point[originalVertices],
            Text[Style["A", Blue, Bold, 14], originalVertices[[1]] + labelOffsets[[1, 1]]],
            Text[Style["B", Blue, Bold, 14], originalVertices[[2]] + labelOffsets[[2, 1]]],
            
            Red, PointSize[0.025], 
            Point[finalVertices],
            Text[Style["A'", Red, Bold, 14], finalVertices[[1]] + labelOffsets[[1, 2]]],
            Text[Style["B'", Red, Bold, 14], finalVertices[[2]] + labelOffsets[[2, 2]]],
            
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

(* Defin\[IAcute]cia chybov\[YAcute]ch spr\[AAcute]v *)
PriamkaZvacsenieZmensenie::invalidInput = "Neplatn\[YAcute] vstup. O\[CHacek]ak\[AAcute]va sa matica alebo zoznam s\[UAcute]radn\[IAcute]c priamky.";
PriamkaZvacsenieZmensenie::insufficientVertices = "Nedostato\[CHacek]n\[YAcute] po\[CHacek]et bodov. S\[UAcute] potrebn\[EAcute] aspo\[NHacek] 2 body.";
    
End[];
EndPackage[];
