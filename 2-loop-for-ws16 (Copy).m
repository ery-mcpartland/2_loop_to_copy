(* ::Package:: *)

(* ::Input::Initialization:: *)
<<X`


(* ::Section:: *)
(*Load Functions*)


(* ::Input::Initialization:: *)
SetDirectory["/home/ery/Documents/Project/sbcc_stuff/2_loop_sbcc/Functions"];


CreateAmplitude[num_,denom_]:=FullSimplify[FermionLineExpand[\!\(\*
TagBox[
StyleBox[
RowBox[{"FermionLine", "[", 
RowBox[{
RowBox[{"List", "[", 
RowBox[{"1", ",", 
RowBox[{"Plus", "[", 
RowBox[{"p", ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "q"}], "]"}]}], "]"}], ",", "0"}], "]"}], ",", 
RowBox[{"List", "[", 
RowBox[{"1", ",", "p", ",", "mb"}], "]"}], ",", 
RowBox[{"DiracMatrix", "[", "num", "]"}]}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\),ChisholmExpand->False]/denom]


(* ::Input::Initialization:: *)
\!\(\*
TagBox[
RowBox[{"finalProjVec", "=", 
StyleBox[
RowBox[{"List", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"-", "2"}], ",", "d"}], "]"}], ",", 
RowBox[{"-", "1"}]}], "]"}], ",", 
RowBox[{"Power", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "q2"}], "]"}]}], "]"}], ",", 
RowBox[{"-", "3"}]}], "]"}], ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "2"}], ",", "q2"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{"d", ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}], ",", "q2"}], "]"}]}], "]"}]}], "]"}], ",", 
RowBox[{"DiracMatrix", "[", "]"}], ",", 
RowBox[{"LTensor", "[", 
RowBox[{"p", ",", "\\[Mu]"}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{"mb", ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", 
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}]}], "]"}], ",", "q2"}], "]"}], ",", 
RowBox[{"DiracMatrix", "[", 
RowBox[{"LTensor", "[", 
RowBox[{"DiracG", ",", "\\[Mu]"}], "]"}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "2"}], ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "d"}], "]"}], ",", "mb", ",", 
RowBox[{"DiracMatrix", "[", "]"}], ",", 
RowBox[{"LTensor", "[", 
RowBox[{"q", ",", "\\[Mu]"}], "]"}]}], "]"}]}], "]"}]}], "]"}]}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"-", "2"}], ",", "d"}], "]"}], ",", 
RowBox[{"-", "1"}]}], "]"}], ",", 
RowBox[{"Power", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "q2"}], "]"}]}], "]"}], ",", 
RowBox[{"-", "3"}]}], "]"}], ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "2"}], ",", "q2"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{"d", ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}], ",", "q2"}], "]"}]}], "]"}]}], "]"}], ",", 
RowBox[{"DiracMatrix", "[", "DiracG5", "]"}], ",", 
RowBox[{"LTensor", "[", 
RowBox[{"p", ",", "\\[Mu]"}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{"mb", ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "q2"}], "]"}]}], "]"}], ",", 
RowBox[{"DiracMatrix", "[", 
RowBox[{
RowBox[{"LTensor", "[", 
RowBox[{"DiracG", ",", "\\[Mu]"}], "]"}], ",", "DiracG5"}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{"2", ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "d"}], "]"}], ",", "mb", ",", 
RowBox[{"DiracMatrix", "[", "DiracG5", "]"}], ",", 
RowBox[{"LTensor", "[", 
RowBox[{"q", ",", "\\[Mu]"}], "]"}]}], "]"}]}], "]"}]}], "]"}]}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"-", "2"}], ",", "d"}], "]"}], ",", 
RowBox[{"-", "1"}]}], "]"}], ",", 
RowBox[{"Power", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "q2"}], "]"}]}], "]"}], ",", 
RowBox[{"-", "3"}]}], "]"}], ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{"mb", ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "q2"}], "]"}]}], "]"}], ",", 
RowBox[{"DiracMatrix", "[", 
RowBox[{"LTensor", "[", 
RowBox[{"DiracG", ",", "\\[Mu]"}], "]"}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "2"}], ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"-", "2"}], ",", "d"}], "]"}], ",", "q2"}], "]"}]}], "]"}], ",", 
RowBox[{"DiracMatrix", "[", "]"}], ",", 
RowBox[{"LTensor", "[", 
RowBox[{"p", ",", "\\[Mu]"}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "2"}], ",", "q2"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{"d", ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}], ",", "q2"}], "]"}]}], "]"}]}], "]"}], ",", 
RowBox[{"DiracMatrix", "[", "]"}], ",", 
RowBox[{"LTensor", "[", 
RowBox[{"q", ",", "\\[Mu]"}], "]"}]}], "]"}]}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"-", "2"}], ",", "d"}], "]"}], ",", 
RowBox[{"-", "1"}]}], "]"}], ",", 
RowBox[{"Power", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "q2"}], "]"}]}], "]"}], ",", 
RowBox[{"-", "3"}]}], "]"}], ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{"mb", ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", 
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}]}], "]"}], ",", "q2"}], "]"}], ",", 
RowBox[{"DiracMatrix", "[", 
RowBox[{
RowBox[{"LTensor", "[", 
RowBox[{"DiracG", ",", "\\[Mu]"}], "]"}], ",", "DiracG5"}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{"2", ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"-", "2"}], ",", "d"}], "]"}], ",", "q2"}], "]"}]}], "]"}], ",", 
RowBox[{"DiracMatrix", "[", "DiracG5", "]"}], ",", 
RowBox[{"LTensor", "[", 
RowBox[{"p", ",", "\\[Mu]"}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "2"}], ",", "q2"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{"d", ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}], ",", "q2"}], "]"}]}], "]"}]}], "]"}], ",", 
RowBox[{"DiracMatrix", "[", "DiracG5", "]"}], ",", 
RowBox[{"LTensor", "[", 
RowBox[{"q", ",", "\\[Mu]"}], "]"}]}], "]"}]}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"Rational", "[", 
RowBox[{"1", ",", "2"}], "]"}], ",", 
RowBox[{"Power", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"-", "2"}], ",", "d"}], "]"}], ",", 
RowBox[{"-", "1"}]}], "]"}], ",", 
RowBox[{"Power", "[", 
RowBox[{"mb", ",", 
RowBox[{"-", "1"}]}], "]"}], ",", 
RowBox[{"Power", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "q2"}], "]"}]}], "]"}], ",", 
RowBox[{"-", "2"}]}], "]"}], ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", 
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}]}], "]"}], ",", "q2"}], "]"}], ",", 
RowBox[{"DiracMatrix", "[", 
RowBox[{"LTensor", "[", 
RowBox[{"DiracG", ",", "\\[Mu]"}], "]"}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{"2", ",", "mb", ",", 
RowBox[{"DiracMatrix", "[", "]"}], ",", 
RowBox[{"LTensor", "[", 
RowBox[{"p", ",", "\\[Mu]"}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "2"}], ",", "mb", ",", 
RowBox[{"DiracMatrix", "[", "]"}], ",", 
RowBox[{"LTensor", "[", 
RowBox[{"q", ",", "\\[Mu]"}], "]"}]}], "]"}]}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"Rational", "[", 
RowBox[{"1", ",", "2"}], "]"}], ",", 
RowBox[{"Power", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"-", "2"}], ",", "d"}], "]"}], ",", 
RowBox[{"-", "1"}]}], "]"}], ",", 
RowBox[{"Power", "[", 
RowBox[{"mb", ",", 
RowBox[{"-", "1"}]}], "]"}], ",", 
RowBox[{"Power", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "q2"}], "]"}]}], "]"}], ",", 
RowBox[{"-", "2"}]}], "]"}], ",", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", 
RowBox[{"Power", "[", 
RowBox[{"mb", ",", "2"}], "]"}]}], "]"}], ",", "q2"}], "]"}], ",", 
RowBox[{"DiracMatrix", "[", 
RowBox[{
RowBox[{"LTensor", "[", 
RowBox[{"DiracG", ",", "\\[Mu]"}], "]"}], ",", "DiracG5"}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{"2", ",", "mb", ",", 
RowBox[{"DiracMatrix", "[", "DiracG5", "]"}], ",", 
RowBox[{"LTensor", "[", 
RowBox[{"p", ",", "\\[Mu]"}], "]"}]}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "2"}], ",", "mb", ",", 
RowBox[{"DiracMatrix", "[", "DiracG5", "]"}], ",", 
RowBox[{"LTensor", "[", 
RowBox[{"q", ",", "\\[Mu]"}], "]"}]}], "]"}]}], "]"}]}], "]"}]}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True]}],
FullForm]\);


(* ::Input::Initialization:: *)
ReduceToScalar[amp_]:=amp/. FermionLine[s1_,s2_,x_DiracMatrix]:> Table[Contract[Spur[finalProjVec[[i]],LDot[p-q,\[Gamma]],x,LDot[p,\[Gamma]]+mb \[DoubleStruckOne]]]/.{LDot[q,q]->q2,LDot[p,p]->mb^2,LDot[p,q]->(mb^2+q2)/2,\[ScriptD]->d},{i,1,6}]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}


(* ::Input::Initialization:: *)
<<TensorIntegrals`


(* ::Input::Initialization:: *)
<<jIntegrals`


(* ::Input::Initialization:: *)
<<MasterIntegralReplacements`
<<MasterIntegralReplacementsMassless`
<<MasterIntegralReplacementsbQuark`


(* ::Input::Initialization:: *)
WardIDCheck[int_]:=Coefficient[int,CStruc]+(q2/mb) Coefficient[int,AStruc]+(((mb^2+q2)) /(2mb)) Coefficient[int,BStruc]


(* ::Section:: *)
(*Definitions*)


(* ::Subsection:: *)
(*Operators*)


(* ::Input::Initialization:: *)
opbq["1s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1],DiracPR];
opqs["1s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1],DiracPR];
opbq["2s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1],DiracPR];
opqs["2s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1],DiracPR];
opqq["3s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1]];
opbs["3s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1],DiracPR];
opqq["4s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1]];
opbs["4s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1],DiracPR];
opqq["5s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1],LTensor[DiracG,\[Alpha]2],LTensor[DiracG,\[Alpha]3]];
opbs["5s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1],LTensor[DiracG,\[Alpha]2],LTensor[DiracG,\[Alpha]3],DiracPR];
opqq["6s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1],LTensor[DiracG,\[Alpha]2],LTensor[DiracG,\[Alpha]3]];
opbs["6s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1],LTensor[DiracG,\[Alpha]2],LTensor[DiracG,\[Alpha]3],DiracPR];
opqq["11s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1],DiracPR];
opbs["11s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1],DiracPR];
opqq["12s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1],DiracPL];
opbs["12s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1],DiracPR];
opqq["13s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1],DiracPL];
opbs["13s"]=DiracMatrix[LTensor[DiracG,\[Alpha]1],DiracPR];
opqq["19s"]=DiracMatrix[DiracPR];
opbs["19s"]=DiracMatrix[DiracPL];
opqq["20s"]=DiracMatrix[DiracPR];
opbs["20s"]=DiracMatrix[DiracPL];
opqq["25s"]=DiracMatrix[DiracPL];
opbs["25s"]=DiracMatrix[DiracPL];
opqq["30s"]=DiracMatrix[DiracPL];
opbs["30s"]=DiracMatrix[DiracPL];
opqq["26s"]=DiracMatrix[LTensor[DiracS,\[Alpha]1,\[Alpha]2],DiracPL];
opbs["26s"]=DiracMatrix[LTensor[DiracS,\[Alpha]1,\[Alpha]2],DiracPL];
opqq["31s"]=DiracMatrix[LTensor[DiracS,\[Alpha]1,\[Alpha]2],DiracPL];
opbs["31s"]=DiracMatrix[LTensor[DiracS,\[Alpha]1,\[Alpha]2],DiracPL];


(* ::Subsection:: *)
(*Colour Factors*)


(* ::Input::Initialization:: *)
CFa["1s"]=SF CF1Op1;
CFa["2s"]=SF CF1Op2;
CFa["3s"]=SF CF1Op3;
CFa["4s"]=SF CF1Op4;
CFa["5s"]=SF CF1Op3;
CFa["6s"]=SF CF1Op4;
CFa["11s"]=SF CF1Op3;
CFa["12s"]=SF CF1Op4;
CFa["13s"]=SF CF1Op3;
CFa["19s"]=SF CF1Op3;
CFa["20s"]=SF CF1Op4;
CFa["25s"]=SF CF1Op3;
CFa["30s"]=SF CF1Op4;
CFa["26s"]=SF CF1Op3;
CFa["31s"]=SF CF1Op4;
CFa["1s"]=SF CF2Op1;
CFe["2s"]=SF CF2Op2;
CFe["3s"]=SF CF2Op3;
CFe["4s"]=SF CF2Op4;
CFe["5s"]=SF CF2Op3;
CFe["6s"]=SF CF2Op4;
CFe["11s"]=SF CF2Op3;
CFe["12s"]=SF CF2Op4;
CFe["13s"]=SF CF2Op3;
CFe["19s"]=SF CF2Op3;
CFe["20s"]=SF CF2Op4;
CFe["25s"]=SF CF2Op3;
CFe["30s"]=SF CF2Op4;
CFe["26s"]=SF CF2Op3;
CFe["31s"]=SF CF2Op4;


(* ::Section:: *)
(*Op3-Op10 Amplitudes*)


(* ::Input::Initialization:: *)
traceamp["masslessquark",a1,a_]:=Simplify[Spur[opqq[a],LDot[DiracG ,(l+r) ],LTensor[DiracG,\[Nu]1],LDot[DiracG ,(l)] ]CreateAmplitude[-CFa[a] (Qb)DiracMatrix[LTensor[DiracG,\[Nu]1],LDot[DiracG, (r+p-q)],opbs[a],LDot[DiracG,(p-q)]+mb \[DoubleStruckOne],LTensor[DiracG,\[Mu]]],P2hat P3hat P4 P5 mb^2]];
traceamp["cquark",a1,a_]:=Simplify[Spur[opqq[a],LDot[DiracG ,(l+r) ]+mc \[DoubleStruckOne],LTensor[DiracG,\[Nu]1],LDot[DiracG ,(l)]+mc \[DoubleStruckOne]]CreateAmplitude[-CFa[a] (Qb)DiracMatrix[LTensor[DiracG,\[Nu]1],LDot[DiracG, (r+p-q)],opbs[a],LDot[DiracG,(p-q)]+mb \[DoubleStruckOne],LTensor[DiracG,\[Mu]]],P2 P3 P4 P5 mb^2]];
traceamp["bquark",a1,a_]:=Simplify[Spur[opqq[a],LDot[DiracG ,(l+r) ]+mb \[DoubleStruckOne],LTensor[DiracG,\[Nu]1],LDot[DiracG ,(l)]+mb \[DoubleStruckOne]]CreateAmplitude[-CFa[a] (Qb)DiracMatrix[LTensor[DiracG,\[Nu]1],LDot[DiracG, (r+p-q)],opbs[a],LDot[DiracG,(p-q)]+mb \[DoubleStruckOne],LTensor[DiracG,\[Mu]]],P2mb P3mb P4 P5 mb^2]];


(* ::Section:: *)
(*Calculations*)


(* ::Subsection:: *)
(*Scalar*)


(* ::Subsubsection:: *)
(*a (3-40)*)


(* ::Input::Initialization:: *)
Do[scalar[diag,oper]=If[traceamp["cquark",diag,oper]===0,0,Sum[PropagatorReplace["a",Contract[ReduceToScalar[traceamp["cquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{a1}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]


(* ::Input::Initialization:: *)
Do[scalar["massless",diag,oper]=If[traceamp["masslessquark",diag,oper]===0,0,Sum[PropagatorReplace["ahat",Contract[ReduceToScalar[traceamp["masslessquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{a1}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]
Do[scalar["bquark",diag,oper]=If[traceamp["bquark",diag,oper]===0,0,Sum[PropagatorReplace["amb",Contract[ReduceToScalar[traceamp["bquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{a1}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]


(* ::Subsubsection:: *)
(*write to file*)


(* ::Input::Initialization:: *)
SetDirectory[NotebookDirectory[]];
file=OpenWrite["scalar_results.m"];
Do[
WriteString[file,"scalar[",diag,",",oper,"]= ",scalar[diag,oper]//InputForm,"\n\n"],{diag,{a1}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}];
Do[
WriteString[file,"scalar[",quarktype,",",diag,",",oper,"]= ",scalar[quarktype,diag,oper]//InputForm,"\n\n"],{diag,{a1}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}},{quarktype,{"massless","bquark"}}];
Close[file];


(* ::Subsection:: *)
(*J Integrals*)


(* ::Input::Initialization:: *)
Do[jIntegral[diag,oper]=(jIntegralForm["a",scalar[diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{a1,a2,a3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]


(* ::Input::Initialization:: *)
Do[jIntegral["massless",diag,oper]=(jIntegralForm["ahat",scalar["massless",diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{a1,a2,a3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]
Do[jIntegral["mb",diag,oper]=(jIntegralForm["amb",scalar["bquark",diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{a1,a2,a3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]


(* ::Input::Initialization:: *)
SetDirectory[NotebookDirectory[]];
file=OpenWrite["j_integral_results.m"];
Do[
WriteString[file,"jIntegral[",diag,",",oper,"]= ",jIntegral[diag,oper]//InputForm,"\n\n"],{diag,{a1}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}];
Do[
WriteString[file,"jIntegral[",quarktype,",",diag,",",oper,"]= ",jIntegral[quarktype,diag,oper]//InputForm,"\n\n"],{diag,{a1}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}},{quarktype,{"massless","mb"}}];
Close[file];


(* ::Subsection:: *)
(*Master Integrals*)


(* ::Input::Initialization:: *)
Do[master[diag,oper]=Simplify[Collect[ScalelessReplacementRuleA[jIntegral[diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.a[1,1,0,0,0,0,0]->Ja1/.a[1,1,0,0,1,0,0]->Ja2/.a[2,1,0,0,1,0,0]->Ja3/.a[0,1,0,1,1,0,0]->Ja4/.a[0,1,1,0,1,0,0]->Ja5/.a[1,1,0,1,1,0,0]->Ja6/.a[2,1,0,1,1,0,0]->Ja7,{AStruc,BStruc,CStruc}]],{diag,{a1,a2,a3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]
Do[master["massless",diag,oper]=Simplify[Collect[ScalelessReplacementRuleMasslessA[jIntegral["massless",diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.ahat[1,1,0,0,0,0,0]->Ja1hat/.ahat[1,1,0,0,1,0,0]->Ja2hat/.ahat[2,1,0,0,1,0,0]->Ja3hat/.ahat[0,1,0,1,1,0,0]->Ja4hat/.a[0,1,1,0,1,0,0]->Ja5hat/.ahat[1,1,0,1,1,0,0]->Ja6hat/.ahat[2,1,0,1,1,0,0]->Ja7hat,{AStruc,BStruc,CStruc}]],{diag,{a1,a2,a3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]
Do[master["bquark",diag,oper]=Simplify[Collect[ScalelessReplacementRulebQuarkA[jIntegral["mb",diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.amb[1,1,0,0,0,0,0]->Ja1mb/.amb[1,1,0,0,1,0,0]->Ja2mb/.amb[2,1,0,0,1,0,0]->Ja3mb/.amb[0,1,0,1,1,0,0]->Ja4mb/.amb[0,1,1,0,1,0,0]->Ja5mb/.amb[1,1,0,1,1,0,0]->Ja6mb/.amb[2,1,0,1,1,0,0]->Ja7mb,{AStruc,BStruc,CStruc}]],{diag,{a1,a2,a3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]


(* ::Input::Initialization:: *)
SetDirectory[NotebookDirectory[]];
file=OpenWrite["master_integral_results.m"];
Do[
WriteString[file,"master[",diag,",",oper,"]= ",master[diag,oper]//InputForm,"\n\n"],{diag,{a1}},{oper,{"11","12","13","19","20","25","26","30","31"}}];
Do[
WriteString[file,"master[",quarktype,",",diag,",",oper,"]= ",master[quarktype,diag,oper]//InputForm,"\n\n"],{diag,{a1}},{oper,{"11","12","13","19","20","25","26","30","31"}},{quarktype,{"massless","bquark"}}];
Close[file];


Print["Finished"]
