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


BodRotacia::usage = 
      "BodRotacia[point_] zobraz\[IAcute] interakt\[IAcute]vny tutori\[AAcute]l pre rot\[AAcute]ciu \
bodu okolo po\[CHacek]iatku a vr\[AAcute]ti nov\[EAcute] s\[UAcute]radnice.";


RotaciaUhol::usage = 
  "RotaciaUhol je glob\[AAcute]lna premenn\[AAcute], ktor\[AAcute] sa pou\[ZHacek]\[IAcute]va na ulo\[ZHacek]enie \
uhla rot\[AAcute]cie v stup\[NHacek]och.";

Begin["`Private`"];


RotaciaUhol = 0;


BodRotacia[inputPoint_] := 
      Module[{normalizedPoint, angle, popis, outputPoint, cosA, sinA, 
    invAngle, cosInvA, sinInvA,
                popisUhla, complexAngles, finalPoint = None, 
    transformMatrix, darkGreen = RGBColor[0, 0.5, 0]},
               
           
           normalizedPoint = inputPoint;
           
           
           If[RotaciaUhol != 0,
                angle = RotaciaUhol,
                
                complexAngles = {30, 45, 60, 90, 120, 135, 150, 180};
                angle = RandomChoice[complexAngles];
            ];
           
           
           popisUhla = If[angle > 0, 
                 
     "v kladnom smere (proti smeru hodinov\[YAcute]ch ru\[CHacek]i\[CHacek]iek)", 
                 "v z\[AAcute]pornom smere (v smere hodinov\[YAcute]ch ru\[CHacek]i\[CHacek]iek)"];
           
           
   popis = "Rot\[AAcute]cia o " <> ToString[Abs[angle]] <> "\[Degree] " <> popisUhla;
           cosA = GetExactTrigValue[angle, Cos];
           sinA = GetExactTrigValue[angle, Sin];
           
           
           outputPoint = ExactRotatePoint[normalizedPoint, angle];
           
           
           RotaciaUhol = angle;
           
           
           
   transformMatrix = {{cosA, -sinA, 0}, {sinA, cosA, 0}, {0, 0, 1}};
           
           
           invAngle = -angle;
           cosInvA = GetExactTrigValue[invAngle, Cos];
           sinInvA = GetExactTrigValue[invAngle, Sin];
           
           
           Print[Style[
                 "GEOMETRICK\[CapitalEAcute] TRANSFORM\[CapitalAAcute]CIE BODU - ROT\[CapitalAAcute]CIA", 
                 Bold, 16]];
           
   Print[Style["========================================", Bold]];
           
           
           Print[Style["\nZADANIE:", Bold, 14]];
           Print["Majme bod P so s\[UAcute]radnicami:"];
           Print[Style[normalizedPoint, Blue, Bold]];
           
           
   Print["\nVykonajte rot\[AAcute]ciu bodu v 2D priestore okolo po\[CHacek]iatku \
s\[UAcute]radnicovej s\[UAcute]stavy [0,0] o uhol \[Alpha] = ", 
                Style[angle, Red], "\[Degree]."];
               
           
   Print["\nTransforma\[CHacek]n\[AAcute] matica rot\[AAcute]cie v homog\[EAcute]nnych s\[UAcute]radniciach:"];
           Print[MatrixForm[{
                  {Style[cosA, Red], Style[-sinA, Red], 
       Style[0, Red]}, 
                  {Style[sinA, Red], Style[cosA, Red], 
       Style[0, Red]},
                  {Style[0, Red], Style[0, Red], Style[1, Red]}
              }]];
           
           
           
   Print[Style["\nTE\[CapitalOAcute]RIA MATICOV\[CapitalEAcute]HO ZOBRAZENIA ROT\[CapitalAAcute]CIE:", Bold, 14]];
           
   Print["Pri rot\[AAcute]cii v 2D priestore pou\[ZHacek]\[IAcute]vame homog\[EAcute]nne s\[UAcute]radnice, \
ktor\[EAcute] umo\[ZHacek]\[NHacek]uj\[UAcute] vyjadri\[THacek] rot\[AAcute]ciu ako maticov\[EAcute] n\[AAcute]sobenie:"];
           
           
           Print["\nTransforma\[CHacek]n\[AAcute] matica rot\[AAcute]cie:"];
           Print[MatrixForm[{
                  {Style["cos(\[Alpha])", Red], 
       Style["-sin(\[Alpha])", Red], Style[0, Red]}, 
                  {Style["sin(\[Alpha])", Red], 
       Style["cos(\[Alpha])", Red], Style[0, Red]},
                  {Style[0, Red], Style[0, Red], Style[1, Red]}
              }]];
           
           
           
   Print["\nAk je bod P = [x, y] v z\[AAcute]kladn\[YAcute]ch s\[UAcute]radniciach, v \
homog\[EAcute]nnych s\[UAcute]radniciach ho zap\[IAcute]\[SHacek]eme ako P = [x, y, 1].\nRot\[AAcute]ciu \
potom m\[OHat]\[ZHacek]eme zap\[IAcute]sa\[THacek] ako maticov\[EAcute] n\[AAcute]sobenie:"];
           
           Print[Row[{
                  MatrixForm[{
                        {Style["cos(\[Alpha])", Red], 
         Style["-sin(\[Alpha])", Red], Style[0, Red]}, 
                        {Style["sin(\[Alpha])", Red], 
         Style["cos(\[Alpha])", Red], Style[0, Red]},
                        {Style[0, Red], Style[0, Red], Style[1, Red]}
                    }],
                  " \[CenterDot] ",
                  MatrixForm[{
                        {Style["x", Blue]}, 
                        {Style["y", Blue]},
                        {Style[1, Blue]}
                    }],
                  " = ",
                  MatrixForm[{
                        {Style[
          "x\[CenterDot]cos(\[Alpha]) - y\[CenterDot]sin(\[Alpha])", 
          darkGreen]}, 
                        {Style[
          "x\[CenterDot]sin(\[Alpha]) + y\[CenterDot]cos(\[Alpha])", 
          darkGreen]},
                        {Style[1, darkGreen]}
                    }]
              }]];
           
           Print["\nPre na\[SHacek]u hodnotu \[Alpha] = ", angle, "\[Degree]:"];
           Print["cos(\[Alpha]) = ", FormatExactValue[cosA]];
           Print["sin(\[Alpha]) = ", FormatExactValue[sinA]];
           
           Print[Style["\nV\[CapitalYAcute]PO\[CapitalCHacek]ET ROT\[CapitalAAcute]CIE BODU:", Bold, 14]];
           
           
           Module[{x = normalizedPoint[[1]], 
                    y = normalizedPoint[[2]],
                    homogeneousVector, resultVector},
                   
                Print[Style["\nBOD P:", Bold, 14]];
                
    Print["P\[OHat]vodn\[EAcute] s\[UAcute]radnice: [", Style[x, Blue], ", ", 
     Style[y, Blue], "]"];
                
    Print["Homog\[EAcute]nne s\[UAcute]radnice: [", Style[x, Blue], ", ", 
     Style[y, Blue], ", ", Style[1, Blue], "]"];
                
                
                homogeneousVector = {x, y, 1};
                resultVector = {x*cosA - y*sinA, x*sinA + y*cosA, 1};
                
                
                DisplayIntuitiveMathMatrixMultiplication[
                     transformMatrix, 
                     homogeneousVector, 
                     resultVector,
                     "P"
                 ];
                
                
                
    Print[Style[
      "\nAlternat\[IAcute]vny v\[YAcute]po\[CHacek]et pomocou z\[AAcute]kladn\[YAcute]ch vzorcov rot\[AAcute]cie:", 
      Bold]];
                
    Print["x' = x\[CenterDot]cos(\[Alpha]) - \
y\[CenterDot]sin(\[Alpha])  =>  ", Style[x, Blue], " \[CenterDot] ", 
     Style[FormatExactValue[cosA], Red], " - ", Style[y, Blue], 
     " \[CenterDot] ", Style[FormatExactValue[sinA], Red], " = ", 
     Style[FormatExactValue[outputPoint[[1]]], darkGreen]];
                
    Print["y' = x\[CenterDot]sin(\[Alpha]) + \
y\[CenterDot]cos(\[Alpha])  =>  ", Style[x, Blue], " \[CenterDot] ", 
     Style[FormatExactValue[sinA], Red], " + ", Style[y, Blue], 
     " \[CenterDot] ", Style[FormatExactValue[cosA], Red], " = ", 
     Style[FormatExactValue[outputPoint[[2]]], darkGreen]];
                
                Print[Style["\nV\[YAcute]sledn\[EAcute] s\[UAcute]radnice bodu P':", Bold]];
                
    Print["[", Style[FormatExactValue[outputPoint[[1]]], darkGreen], 
     ", ", Style[FormatExactValue[outputPoint[[2]]], darkGreen], 
     "]"];
                
                
    Print[Style["--------------------------------------------------", 
      Bold]];
            ];
           
           
           Print[Style["\nV\[CapitalYAcute]SLEDOK TRANSFORM\[CapitalAAcute]CIE BODU:", Bold, 14]];
           Print[Style["P\[OHat]vodn\[YAcute] bod P:", Blue, Bold]];
           Print[normalizedPoint];
           
           Print[Style["Rotovan\[YAcute] bod P':", Red, Bold]];
           Print[outputPoint];
           
           
           Print[Style["\nVIZUALIZ\[CapitalAAcute]CIA TRANSFORM\[CapitalAAcute]CIE:", Bold]];
           
   Print[CreateFixedVisualization[normalizedPoint, outputPoint, 
     angle]];
           
           
           Print[Style["\nLEGENDA:", Bold, 14]];
           
   Print[Style["\[Bullet] Modr\[EAcute]", Blue], " body a \[CHacek]iary: P\[OHat]vodn\[YAcute] bod"];
           
   Print[Style["\[Bullet] \[CapitalCHacek]erven\[EAcute]", Red], 
    " body a \[CHacek]iary: Rotovan\[YAcute] bod"];
           
   Print[Style["\[Bullet] Zelen\[EAcute]", darkGreen], 
    " preru\[SHacek]ovan\[EAcute] \[CHacek]iary: Spojnice bodov s po\[CHacek]iatkom"];
           
   Print[Style[
     "\[Bullet] O: Po\[CHacek]iatok s\[UAcute]radnicovej s\[UAcute]stavy - stred rot\[AAcute]cie", 
     Black]];
           
           
   Print[Style["\nFAREBN\[CapitalEAcute] OZNA\[CapitalCHacek]ENIA V MATICOV\[CapitalYAcute]CH V\[CapitalYAcute]PO\[CapitalCHacek]TOCH:", Bold]];
           
   Print[Style["\[Bullet] \[CapitalCHacek]erven\[AAcute]:", Red], " Transforma\[CHacek]n\[AAcute] matica"];
           
   Print[Style["\[Bullet] Modr\[AAcute]:", Blue], " Vstupn\[EAcute] s\[UAcute]radnice"];
           Print[Style["\[Bullet] Fialov\[AAcute]:", Purple], " Medziv\[YAcute]po\[CHacek]ty"];
           
   Print[Style["\[Bullet] Zelen\[AAcute]:", darkGreen], " V\[YAcute]sledn\[EAcute] s\[UAcute]radnice"];
                   
           Print[Style["\nP\[OHat]vodn\[YAcute] bod (modr\[AAcute]):", Bold]];
           
   Print[Row[{Style["Bod P: ", RGBColor[0.1, 0.1, 1]], 
      normalizedPoint}]];
                   
           Print[Style["\nV\[YAcute]sledn\[YAcute] bod (\[CHacek]erven\[AAcute]):", Bold]];
           
   Print[Row[{Style["Bod P': ", RGBColor[1, 0.1, 0.1]], outputPoint}]];
                   
           Print[Style["\nInverzn\[AAcute] transform\[AAcute]cia:", Bold]];
           
   Print["Pre vr\[AAcute]tenie do p\[OHat]vodn\[EAcute]ho stavu pou\[ZHacek]ite rot\[AAcute]ciu o opa\[CHacek]n\[YAcute] \
uhol:"];
           Print["\[Alpha]' = -\[Alpha] = ", invAngle, "\[Degree]"];
           
           
           Print[Style["\nMATEMATICK\[CapitalEAcute] VLASTNOSTI:", Bold, 14]];
           
   Print["\[Bullet] Rot\[AAcute]cia okolo po\[CHacek]iatku zachov\[AAcute]va vzdialenos\[THacek] \
v\[SHacek]etk\[YAcute]ch bodov od po\[CHacek]iatku s\[UAcute]stavy [0,0]"];
           
   Print["\[Bullet] Rot\[AAcute]cia nemen\[IAcute] ve\:013ekos\[THacek] ani tvar geometrick\[YAcute]ch \
\[UAcute]tvarov - je to izometria"];
           
   Print["\[Bullet] Rot\[AAcute]cia zachov\[AAcute]va uhly medzi \[UAcute]se\[CHacek]kami a ich \
d\:013a\[ZHacek]ky"];
           
   Print["\[Bullet] Po\[CHacek]iatok s\[UAcute]radnicovej s\[UAcute]stavy [0,0] je fixn\[YAcute] bod \
rot\[AAcute]cie - zost\[AAcute]va nezmenen\[YAcute]"];
           
           
           outputPoint
       ];


CreateFixedVisualization[originalPoint_, finalPoint_, angle_] := 
      Module[{allPoints, xMin, xMax, yMin, yMax, rangeBuffer, xRange, 
    yRange, 
                labelOffsets, angleDeg = angle, 
    angleRad = angle*Pi/180, brightGreen},
           
           brightGreen = RGBColor[0, 0.8, 0.2];
           
           
           allPoints = {originalPoint, finalPoint};
           
           
           xMin = Min[N[allPoints[[All, 1]]]];
           xMax = Max[N[allPoints[[All, 1]]]];
           yMin = Min[N[allPoints[[All, 2]]]];
           yMax = Max[N[allPoints[[All, 2]]]];
           
           
           rangeBuffer = 2;
           
           
           
   xRange = {Min[-12, xMin - rangeBuffer], 
     Max[12, xMax + rangeBuffer]};
           
   yRange = {Min[-12, yMin - rangeBuffer], 
     Max[12, yMax + rangeBuffer]};
           
           
           labelOffsets = {0.7, 0.7};
           
           Graphics[{
                 
                 LightGray, Thin,
                 Table[Line[{{xRange[[1]], y}, {xRange[[2]], y}}], 
                      {y, Ceiling[yRange[[1]]], Floor[yRange[[2]]], 
       1}],
                 Table[Line[{{x, yRange[[1]]}, {x, yRange[[2]]}}], 
                      {x, Ceiling[xRange[[1]]], Floor[xRange[[2]]], 
       1}],
                 
                 
                 brightGreen, Opacity[0.7],
                 If[angleDeg > 0,
                      
                      {Thickness[0.005], 
       Circle[{0, 0}, 
        Min[Norm[originalPoint], Norm[finalPoint]]/3, {0, angleRad}],
                       Opacity[0.15], EdgeForm[], 
       Disk[{0, 0}, 
        Min[Norm[originalPoint], Norm[finalPoint]]/3, {0, 
         angleRad}]},
                      
                      {Thickness[0.005], 
       Circle[{0, 0}, 
        Min[Norm[originalPoint], Norm[finalPoint]]/3, {angleRad, 
         0}], 
                       Opacity[0.15], EdgeForm[], 
       Disk[{0, 0}, 
        Min[Norm[originalPoint], Norm[finalPoint]]/3, {angleRad, 0}]}
                  ],
                 
                 
                 brightGreen, Dashed, Thickness[0.004],
                 Line[{{0, 0}, originalPoint}],
                 brightGreen, Dashed, Thickness[0.004], 
                 Line[{{0, 0}, finalPoint}],
                 
                 
                 White, 
                 Disk[{0, 0}, 0.3], 
                 Disk[originalPoint, 0.3],
                 Disk[finalPoint, 0.3],
                 
                 
                 Black, PointSize[0.02], Point[{0, 0}], 
                 Text[Style["O", Black, Bold, 14], {-0.4, -0.4}], 
                 
                 Blue, PointSize[0.02], Point[originalPoint],
                 Red, PointSize[0.02], Point[finalPoint],
                 
                 
                 
     Text[Style["P", Blue, Bold, 14], originalPoint + labelOffsets],
                 
     Text[Style["P'", Red, Bold, 14], finalPoint + labelOffsets],
                 
                 
                 brightGreen, Opacity[1],
                 
     Text[Style[ToString[Abs[angleDeg]] <> "\[Degree]", FontSize -> 14, Bold, 
       Background -> White, 
                        brightGreen], 
      Min[Norm[originalPoint], Norm[finalPoint]]/2*{Cos[angleRad/2], 
        Sin[angleRad/2]}],
                 
                 
                 Black, Thickness[0.004],
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
    

BodRotaciaNoDisplayWithParams[point_] := 
      Module[{angle, result},
           
           If[RotaciaUhol != 0,
                
                angle = RotaciaUhol,
                
                
    angle = RandomChoice[{30, 45, 60, 90, 120, 135, 180, 270}]
            ];
           
           
           RotaciaUhol = angle;
           
           
           result = ExactRotatePoint[point, angle];
           
           
           {angle, result}
       ];
    

DisplayIntuitiveMathMatrixMultiplication[matrix_, vector_, result_, 
   pointName_] := 
      Module[{darkGreen = RGBColor[0, 0.5, 0]},
           
           
   Print[Style[
     "Maticov\[YAcute] z\[AAcute]pis transform\[AAcute]cie bodu " <> pointName <> ":", 
     Bold]];
           
           
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
           
           
           
   Print[Style[
     "\[Bullet] V\[YAcute]po\[CHacek]et novej x-ovej s\[UAcute]radnice (1. riadok matice):", 
     Brown]];
           
           
           Print[
                Grid[{
                      {
                           "   [", 
                           Style[matrix[[1, 1]], Red], 
                           Style[matrix[[1, 2]], Red], 
                           Style[matrix[[1, 3]], Red], 
                           "] \[CenterDot] [",
                           Style[vector[[1]], Blue],
                           Style[vector[[2]], Blue],
                           Style[vector[[3]], Blue],
                           "]^T =",
                           Style[result[[1]], darkGreen]
                       }
                  }, Alignment -> Left]
            ];
           
           
           
   Print["   = (", Style[matrix[[1, 1]], Red], " \[CenterDot] ", 
    Style[vector[[1]], Blue], 
                  ") + (", Style[matrix[[1, 2]], Red], 
    " \[CenterDot] ", Style[vector[[2]], Blue], 
                  ") + (", Style[matrix[[1, 3]], Red], 
    " \[CenterDot] ", Style[vector[[3]], Blue], ")"];
           
           
           
   Print["   = (", Style[matrix[[1, 1]], Red], " \[CenterDot] ", 
    Style[vector[[1]], Blue], 
                  ") + (", Style[matrix[[1, 2]], Red], 
    " \[CenterDot] ", Style[vector[[2]], Blue], 
                  ") + (", Style[matrix[[1, 3]], Red], 
    " \[CenterDot] ", Style[vector[[3]], Blue], ")"];
           
           
           Print["   = ", Style[matrix[[1, 1]]*vector[[1]], Purple], 
                  " + ", Style[matrix[[1, 2]]*vector[[2]], Purple],
                  " + ", Style[matrix[[1, 3]]*vector[[3]], Purple]];
           
           
           Print["   = ", Style[result[[1]], darkGreen]];
           
           
           
   Print[Style[
     "\n\[Bullet] V\[YAcute]po\[CHacek]et novej y-ovej s\[UAcute]radnice (2. riadok \
matice):", Brown]];
           
           
           Print[
                Grid[{
                      {
                           "   [", 
                           Style[matrix[[2, 1]], Red], 
                           Style[matrix[[2, 2]], Red], 
                           Style[matrix[[2, 3]], Red], 
                           "] \[CenterDot] [",
                           Style[vector[[1]], Blue],
                           Style[vector[[2]], Blue],
                           Style[vector[[3]], Blue],
                           "]^T =",
                           Style[result[[2]], darkGreen]
                       }
                  }, Alignment -> Left]
            ];
           
           
           
   Print["   = (", Style[matrix[[2, 1]], Red], " \[CenterDot] ", 
    Style[vector[[1]], Blue], 
                  ") + (", Style[matrix[[2, 2]], Red], 
    " \[CenterDot] ", Style[vector[[2]], Blue], 
                  ") + (", Style[matrix[[2, 3]], Red], 
    " \[CenterDot] ", Style[vector[[3]], Blue], ")"];
           
           
           
   Print["   = (", Style[matrix[[2, 1]], Red], " \[CenterDot] ", 
    Style[vector[[1]], Blue], 
                  ") + (", Style[matrix[[2, 2]], Red], 
    " \[CenterDot] ", Style[vector[[2]], Blue], 
                  ") + (", Style[matrix[[2, 3]], Red], 
    " \[CenterDot] ", Style[vector[[3]], Blue], ")"];
           
           
           Print["   = ", Style[matrix[[2, 1]]*vector[[1]], Purple], 
                  " + ", Style[matrix[[2, 2]]*vector[[2]], Purple],
                  " + ", Style[matrix[[2, 3]]*vector[[3]], Purple]];
           
           
           Print["   = ", Style[result[[2]], darkGreen]];
           
           
           
   Print[Style[
     "\n\[Bullet] V\[YAcute]po\[CHacek]et homog\[EAcute]nnej s\[UAcute]radnice (3. riadok matice):", 
     Brown]];
           
          
           Print[
                Grid[{
                      {
                           "   [", 
                           Style[matrix[[3, 1]], Red], 
                           Style[matrix[[3, 2]], Red], 
                           Style[matrix[[3, 3]], Red], 
                           "] \[CenterDot] [",
                           Style[vector[[1]], Blue],
                           Style[vector[[2]], Blue],
                           Style[vector[[3]], Blue],
                           "]^T =",
                           Style[result[[3]], darkGreen]
                       }
                  }, Alignment -> Left]
            ];
           
           
           
   Print["   = (", Style[matrix[[3, 1]], Red], " \[CenterDot] ", 
    Style[vector[[1]], Blue], 
                  ") + (", Style[matrix[[3, 2]], Red], 
    " \[CenterDot] ", Style[vector[[2]], Blue], 
                  ") + (", Style[matrix[[3, 3]], Red], 
    " \[CenterDot] ", Style[vector[[3]], Blue], ")"];
           
           
           
   Print["   = (", Style[matrix[[3, 1]], Red], " \[CenterDot] ", 
    Style[vector[[1]], Blue], 
                  ") + (", Style[matrix[[3, 2]], Red], 
    " \[CenterDot] ", Style[vector[[2]], Blue], 
                  ") + (", Style[matrix[[3, 3]], Red], 
    " \[CenterDot] ", Style[vector[[3]], Blue], ")"];
           
           
           Print["   = ", Style[matrix[[3, 1]]*vector[[1]], Purple], 
                  " + ", Style[matrix[[3, 2]]*vector[[2]], Purple],
                  " + ", Style[matrix[[3, 3]]*vector[[3]], Purple]];
           
           
           Print["   = ", Style[result[[3]], darkGreen]];
           
           
           
   Print[Style[
     "\nV\[YAcute]sledn\[AAcute] homog\[EAcute]nna reprezent\[AAcute]cia bodu " <> pointName <> "':", 
     Bold]];
           
   Print["[", Style[result[[1]], darkGreen], ", ", 
    Style[result[[2]], darkGreen], ", ", 
    Style[result[[3]], darkGreen], "]"];
           
           
           
   Print[Style["\nKart\[EAcute]zske s\[UAcute]radnice bodu " <> pointName <> "':", 
     Bold]];
           
   Print["[", Style[result[[1]], darkGreen], ", ", 
    Style[result[[2]], darkGreen], "]"];
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
                 
                 _, 
     If[func === Cos, Cos[angle*Pi/180], Sin[angle*Pi/180]]
             ];
           exactValue
       ];
    

ExactRotatePoint[point_, angle_] := 
      Module[{x, y, cosA, sinA, newX, newY},
           x = point[[1]];
           y = point[[2]];
           cosA = GetExactTrigValue[angle, Cos];
           sinA = GetExactTrigValue[angle, Sin];
           
           newX = x*cosA - y*sinA;
           newY = x*sinA + y*cosA;
           
           
           newX = Simplify[newX];
           newY = Simplify[newY];
           
           {newX, newY}
       ];


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


BodRotacia::invalidInput = 
  "Neplatn\[YAcute] vstup. O\[CHacek]ak\[AAcute]va sa vektor s\[UAcute]radn\[IAcute]c bodu.";

End[];
EndPackage[];
