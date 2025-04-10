(* ::Package:: *)

(* ::Input::Initialization:: *)
<<X`


(* ::Section:: *)
(*Load Functions*)


(* ::Input::Initialization:: *)
SetDirectory["/home/ery/Documents/Project/sbcc_stuff/2_loop_sbcc/Functions"];


CreateAmplitude[num_,denom_]:=FullSimplify[FermionLineExpand[\[LeftAngleBracket]\[ScriptU][p-q,0], num ,\[ScriptU][p,mb]\[RightAngleBracket],ChisholmExpand->False]/denom]


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
ReduceToScalar[amp_]:=amp/. FermionLine[s1_,s2_,x_DiracMatrix]:> Table[Contract[Spur[finalProjVec[[i]],LDot[p-q,\[Gamma]],x,LDot[p,\[Gamma]]+mb \[DoubleStruckOne]]]/.{q . q->q2,p . p->mb^2,p . q->(mb^2+q2)/2,\[ScriptD]->d},{i,1,6}]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}


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


(* ::Input:: *)
(*opbq["1s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1],\[DoubleStruckCapitalP]R];*)
(*opqs["1s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1],\[DoubleStruckCapitalP]R];*)
(*opbq["2s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1],\[DoubleStruckCapitalP]R];*)
(*opqs["2s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1],\[DoubleStruckCapitalP]R];*)
(*opqq["3s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1]];*)
(*opbs["3s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1],\[DoubleStruckCapitalP]R];*)
(*opqq["4s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1]];*)
(*opbs["4s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1],\[DoubleStruckCapitalP]R];*)
(*opqq["5s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1],LTensor[\[Gamma],\[Alpha]2],LTensor[\[Gamma],\[Alpha]3]];*)
(*opbs["5s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1],LTensor[\[Gamma],\[Alpha]2],LTensor[\[Gamma],\[Alpha]3],\[DoubleStruckCapitalP]R];*)
(*opqq["6s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1],LTensor[\[Gamma],\[Alpha]2],LTensor[\[Gamma],\[Alpha]3]];*)
(*opbs["6s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1],LTensor[\[Gamma],\[Alpha]2],LTensor[\[Gamma],\[Alpha]3],\[DoubleStruckCapitalP]R];*)
(*opqq["11s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1],\[DoubleStruckCapitalP]R];*)
(*opbs["11s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1],\[DoubleStruckCapitalP]R];*)
(*opqq["12s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1],\[DoubleStruckCapitalP]L];*)
(*opbs["12s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1],\[DoubleStruckCapitalP]R];*)
(*opqq["13s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1],\[DoubleStruckCapitalP]L];*)
(*opbs["13s"]=DiracMatrix[LTensor[\[Gamma],\[Alpha]1],\[DoubleStruckCapitalP]R];*)
(*opqq["19s"]=DiracMatrix[\[DoubleStruckCapitalP]R];*)
(*opbs["19s"]=DiracMatrix[\[DoubleStruckCapitalP]L];*)
(*opqq["20s"]=DiracMatrix[\[DoubleStruckCapitalP]R];*)
(*opbs["20s"]=DiracMatrix[\[DoubleStruckCapitalP]L];*)
(*opqq["25s"]=DiracMatrix[\[DoubleStruckCapitalP]L];*)
(*opbs["25s"]=DiracMatrix[\[DoubleStruckCapitalP]L];*)
(*opqq["30s"]=DiracMatrix[\[DoubleStruckCapitalP]L];*)
(*opbs["30s"]=DiracMatrix[\[DoubleStruckCapitalP]L];*)
(*opqq["26s"]=DiracMatrix[LTensor[DiracS,\[Alpha]1,\[Alpha]2],\[DoubleStruckCapitalP]L];*)
(*opbs["26s"]=DiracMatrix[LTensor[DiracS,\[Alpha]1,\[Alpha]2],\[DoubleStruckCapitalP]L];*)
(*opqq["31s"]=DiracMatrix[LTensor[DiracS,\[Alpha]1,\[Alpha]2],\[DoubleStruckCapitalP]L];*)
(*opbs["31s"]=DiracMatrix[LTensor[DiracS,\[Alpha]1,\[Alpha]2],\[DoubleStruckCapitalP]L];*)


(* ::Subsection:: *)
(*Colour Factors*)


(* ::Input:: *)
(*CFa["1s"]=SF CF1Op1;*)
(*CFa["2s"]=SF CF1Op2;*)
(*CFa["3s"]=SF CF1Op3;*)
(*CFa["4s"]=SF CF1Op4;*)
(*CFa["5s"]=SF CF1Op3;*)
(*CFa["6s"]=SF CF1Op4;*)
(*CFa["11s"]=SF CF1Op3;*)
(*CFa["12s"]=SF CF1Op4;*)
(*CFa["13s"]=SF CF1Op3;*)
(*CFa["19s"]=SF CF1Op3;*)
(*CFa["20s"]=SF CF1Op4;*)
(*CFa["25s"]=SF CF1Op3;*)
(*CFa["30s"]=SF CF1Op4;*)
(*CFa["26s"]=SF CF1Op3;*)
(*CFa["31s"]=SF CF1Op4;*)
(*CFa["1s"]=SF CF2Op1;*)
(*CFe["2s"]=SF CF2Op2;*)
(*CFe["3s"]=SF CF2Op3;*)
(*CFe["4s"]=SF CF2Op4;*)
(*CFe["5s"]=SF CF2Op3;*)
(*CFe["6s"]=SF CF2Op4;*)
(*CFe["11s"]=SF CF2Op3;*)
(*CFe["12s"]=SF CF2Op4;*)
(*CFe["13s"]=SF CF2Op3;*)
(*CFe["19s"]=SF CF2Op3;*)
(*CFe["20s"]=SF CF2Op4;*)
(*CFe["25s"]=SF CF2Op3;*)
(*CFe["30s"]=SF CF2Op4;*)
(*CFe["26s"]=SF CF2Op3;*)
(*CFe["31s"]=SF CF2Op4;*)


(* ::Section::Closed:: *)
(*Op1 and Op2 Amplitudes*)


(* ::Input:: *)
(*amp[a1,a_]:=Simplify[CreateAmplitude[-CFa[a] (Qb)DiracMatrix[LTensor[\[Gamma],\[Nu]],\[Gamma] . (r+p-q),opqs[a],\[Gamma] . (l+r)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]],\[Gamma] . (l)+mc \[DoubleStruckOne],opbq[a],\[Gamma] . (p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]]],P2 P3 P4 P5 mb^2]];*)


(* ::Input:: *)
(*amp[a2,a_]:=Simplify[CreateAmplitude[CFa[a] (Qs)DiracMatrix[LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p-q),LTensor[\[Gamma],\[Mu]],\[Gamma] . (r+p),opqs[a],\[Gamma] . (l+r)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mc \[DoubleStruckOne],opbq[a]],P2 P3 P4 P5 P8]];*)


(* ::Input:: *)
(*amp[a3,a_]:=CreateAmplitude[CFa [a](Qs)DiracMatrix[LTensor[\[Gamma],\[Mu]],\[Gamma] . p,LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p),opqs[a],\[Gamma] . (l+r)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mc \[DoubleStruckOne],opbq[a]],P2 P3 P4 P8 mb^2];*)


(* ::Input:: *)
(*amp[b1,a_]:=CreateAmplitude[-CFb [a](Qb)DiracMatrix[opqs[a],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)+mc \[DoubleStruckOne],opbq[a],\[Gamma] . (r+p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]]],P2 P3 P4 P10 mb^2];*)


(* ::Input:: *)
(*amp[b2,a_]:=CreateAmplitude[CFb[a](Qb) DiracMatrix[opqs[a],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)+mc \[DoubleStruckOne],opbq[a],\[Gamma] . (r+p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (p+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1]],P2 P3 P4 P10 P11];*)
(*amp[b3,a_]:=CreateAmplitude[CFb[a] (Qs)DiracMatrix[LTensor[\[Gamma],\[Mu]],\[Gamma] . p,opqs[a],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)+mc \[DoubleStruckOne],opbq[a],\[Gamma] . (p+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1]],P2 P3 P4 P11 mb^2];*)


(* ::Input:: *)
(*amp[c1,a_]:=CreateAmplitude[CFc[a] (Qc) DiracMatrix[LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p-q),opqs[a],\[Gamma] . (l+r)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q)+mc \[DoubleStruckOne],opbq[a]],P1 P2 P3 P4 P5];*)


(* ::Input:: *)
(*amp[c2,a_]:=CreateAmplitude[CFc[a] (Qc)DiracMatrix[LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p-q),opqs[a],-\[Gamma] . (l+q)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],-\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],-\[Gamma] . (l+r)+mc \[DoubleStruckOne],opbq[a]],P1 P2 P3 P4 P5];*)


(* ::Input:: *)
(*amp[d1,a_]:=CreateAmplitude[CFd[a] (Qc) DiracMatrix[opqs[a],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+q+r)+mc \[DoubleStruckOne],opbq[a],\[Gamma] . (p+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1]],P1 P2 P12 P4 P11];*)


(* ::Input:: *)
(*amp[d2,a_]:=CreateAmplitude[CFd [a](Qc)DiracMatrix[opqs[a],-\[Gamma] . (l+q+r)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],-\[Gamma] . (l+q)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],-\[Gamma] . (l)+mc \[DoubleStruckOne],opqs[a],\[Gamma] . (p+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1]],P1 P2 P12 P4 P11];*)


(* ::Code:: *)
(*amp[e0i,a_]:=CreateAmplitude[-CFe[a] (Qb) DiracMatrix[opqs[a],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],*)
(*(\[Gamma] . (l+r)+mc \[DoubleStruckOne]),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mc \[DoubleStruckOne],opbq[a],\[Gamma] . (p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]]],P2^2 P3 P4 mb^2];*)


(* ::Input:: *)
(*amp[e0ii,a_]:=CreateAmplitude[CFe[a] (Qs) DiracMatrix[LTensor[\[Gamma],\[Mu]],\[Gamma] . p ,opqs[a],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],*)
(*(\[Gamma] . (l+r)+mc \[DoubleStruckOne]),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mc \[DoubleStruckOne],opbq[a]],P2^2 P3 P4 mb^2];*)


(* ::Input:: *)
(*amp[e1,a_]:=FullSimplify[CreateAmplitude[CFe[a] (Qc)DiracMatrix[opqs[a],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q+r)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+q)+mc \[DoubleStruckOne],opbq[a]],P1 P2 P3 P4 P12]];*)


(* ::Input:: *)
(*amp[e2,a_]:=FullSimplify[CreateAmplitude[CFe[a] (Qc) DiracMatrix[opqs[a],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)+ mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q)+mc \[DoubleStruckOne],opbq[a]],P1 P2^2 P3 P4]];*)


(* ::Input:: *)
(*amp[e3,a_]:=FullSimplify[CreateAmplitude[CFe[a] (Qc)DiracMatrix[opqs[a],\[Gamma] . (l)+ mc \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+q+r)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+q)+mc \[DoubleStruckOne],opbq[a]],P1^2 P2 P4 P12]];*)


(* ::Section::Closed:: *)
(*Op3-Op10 Amplitudes*)


(* ::Input:: *)
(*traceamp["masslessquark",a1,a_]:=Simplify[Spur[opqq[a],\[Gamma] . (l+r) ,LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l) ]CreateAmplitude[-CFa[a] (Qb)DiracMatrix[LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p-q),opbs[a],\[Gamma] . (p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]]],P2hat P3hat P4 P5 mb^2]];*)
(*traceamp["cquark",a1,a_]:=Simplify[Spur[opqq[a],\[Gamma] . (l+r)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mc \[DoubleStruckOne]]CreateAmplitude[-CFa[a] (Qb)DiracMatrix[LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p-q),opbs[a],\[Gamma] . (p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]]],P2 P3 P4 P5 mb^2]];*)
(*traceamp["bquark",a1,a_]:=Simplify[Spur[opqq[a],\[Gamma] . (l+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mb \[DoubleStruckOne]]CreateAmplitude[-CFa[a] (Qb)DiracMatrix[LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p-q),opbs[a],\[Gamma] . (p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]]],P2mb P3mb P4 P5 mb^2]];*)
(**)


(* ::Input:: *)
(*traceamp["masslessquark",a2,a_]:=Simplify[Spur[opqq[a],\[Gamma] . (l+r),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)]CreateAmplitude[CFa[a] (Qs)DiracMatrix[LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p-q),LTensor[\[Gamma],\[Mu]],\[Gamma] . (r+p),opbs[a]],P2hat P3hat P4 P5 P8]];*)
(*traceamp["cquark",a2,a_]:=Simplify[Spur[opqq[a],\[Gamma] . (l+r)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mc \[DoubleStruckOne]]CreateAmplitude[CFa[a] (Qs)DiracMatrix[LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p-q),LTensor[\[Gamma],\[Mu]],\[Gamma] . (r+p),opbs[a]],P2 P3 P4 P5 P8]];*)
(*traceamp["bquark",a2,a_]:=Simplify[Spur[opqq[a],\[Gamma] . (l+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mb \[DoubleStruckOne]]CreateAmplitude[CFa[a] (Qs)DiracMatrix[LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p-q),LTensor[\[Gamma],\[Mu]],\[Gamma] . (r+p),opbs[a]],P2mb P3mb P4 P5 P8]];*)
(**)


(* ::Input:: *)
(*traceamp["masslessquark",a3,a_]:=Spur[opqq[a],\[Gamma] . (l+r),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)]CreateAmplitude[CFa [a](Qs)DiracMatrix[LTensor[\[Gamma],\[Mu]],\[Gamma] . p,LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p),opbs[a]],P2hat P3hat P4 P8 mb^2];*)
(*traceamp["cquark",a3,a_]:=Spur[opqq[a],\[Gamma] . (l+r)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mc \[DoubleStruckOne]]CreateAmplitude[CFa [a](Qs)DiracMatrix[LTensor[\[Gamma],\[Mu]],\[Gamma] . p,LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p),opbs[a]],P2 P3 P4 P8 mb^2];*)
(*traceamp["bquark",a3,a_]:=Spur[opqq[a],\[Gamma] . (l+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mb \[DoubleStruckOne]]CreateAmplitude[CFa [a](Qs)DiracMatrix[LTensor[\[Gamma],\[Mu]],\[Gamma] . p,LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p),opbs[a]],P2mb P3mb P4 P8 mb^2];*)
(**)


(* ::Input:: *)
(*traceamp["masslessquark",b1,a_]:=Spur[opqq[a],\[Gamma] . (l),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)]CreateAmplitude[-CFa [a](Qb)DiracMatrix[opbs[a],\[Gamma] . (r+p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]]],P2hat P3hat P4 P10 mb^2];*)
(*traceamp["cquark",b1,a_]:=Spur[opqq[a],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)+mc \[DoubleStruckOne]]CreateAmplitude[-CFa [a](Qb)DiracMatrix[opbs[a],\[Gamma] . (r+p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]]],P2 P3 P4 P10 mb^2];*)
(*traceamp["bquark",b1,a_]:=Spur[opqq[a],\[Gamma] . (l)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)+mb \[DoubleStruckOne]]CreateAmplitude[-CFa [a](Qb)DiracMatrix[opbs[a],\[Gamma] . (r+p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]]],P2mb P3mb P4 P10 mb^2];*)
(**)


(* ::Input:: *)
(*traceamp["masslessquark",b2,a_]:=Spur[opqq[a],\[Gamma] . (l),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)]CreateAmplitude[CFa[a](Qb) DiracMatrix[opbs[a],\[Gamma] . (r+p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (p+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1]],P2hat P3hat P4 P10 P11];*)
(*traceamp["cquark",b2,a_]:=Spur[opqq[a],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)+mc \[DoubleStruckOne]]CreateAmplitude[CFa[a](Qb) DiracMatrix[opbs[a],\[Gamma] . (r+p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (p+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1]],P2 P3 P4 P10 P11];*)
(*traceamp["bquark",b2,a_]:=Spur[opqq[a],\[Gamma] . (l)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)+mb \[DoubleStruckOne]]CreateAmplitude[CFa[a](Qb) DiracMatrix[opbs[a],\[Gamma] . (r+p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (p+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1]],P2mb P3mb P4 P10 P11];*)


(* ::Input:: *)
(*traceamp["masslessquark",b3,a_]:=Spur[opqq[a],\[Gamma] . (l),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)]CreateAmplitude[CFa[a] (Qs)DiracMatrix[LTensor[\[Gamma],\[Mu]],\[Gamma] . p,opbs[a],\[Gamma] . (p+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1]],P2hat P3hat P4 P11 mb^2];*)
(*traceamp["cquark",b3,a_]:=Spur[opqq[a],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)+mc \[DoubleStruckOne]]CreateAmplitude[CFa[a] (Qs)DiracMatrix[LTensor[\[Gamma],\[Mu]],\[Gamma] . p,opbs[a],\[Gamma] . (p+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1]],P2 P3 P4 P11 mb^2];*)
(*traceamp["bquark",b3,a_]:=Spur[opqq[a],\[Gamma] . (l)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)+mb \[DoubleStruckOne]]CreateAmplitude[CFa[a] (Qs)DiracMatrix[LTensor[\[Gamma],\[Mu]],\[Gamma] . p,opbs[a],\[Gamma] . (p+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1]],P2mb P3mb P4 P11 mb^2];*)


(* ::Input:: *)
(*traceamp["cquark",c1,a_]:=Spur[opqq[a],\[Gamma] . (l+r)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q)+mc \[DoubleStruckOne]]CreateAmplitude[CFa[a] (Qc) DiracMatrix[LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p-q),opbs[a]],P1 P2 P3 P4 P5];*)
(*traceamp["masslessquark",c1,a_]:=Spur[opqq[a],\[Gamma] . (l+r),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l),LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q)]CreateAmplitude[CFa[a] (Qc) DiracMatrix[LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p-q),opbs[a]],P1hat P2hat P3hat P4 P5];*)
(*traceamp["bquark",c1,a_]:=Spur[opqq[a],\[Gamma] . (l+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q)+mb \[DoubleStruckOne]]CreateAmplitude[CFa[a] (Qc) DiracMatrix[LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p-q),opbs[a]],P1mb P2mb P3mb P4 P5];*)


(* ::Input:: *)
(*traceamp["cquark",c2,a_]:=Spur[opqq[a],-\[Gamma] . (l+q)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],-\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],-\[Gamma] . (l+r)+mc \[DoubleStruckOne]]CreateAmplitude[CFa[a] (Qc)DiracMatrix[LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p-q),opbs[a]],P1 P2 P3 P4 P5];*)
(*traceamp["masslessquark",c2,a_]:=Spur[opqq[a],-\[Gamma] . (l+q),LTensor[\[Gamma],\[Mu]],-\[Gamma] . (l),LTensor[\[Gamma],\[Nu]1],-\[Gamma] . (l+r)]CreateAmplitude[CFa[a] (Qc)DiracMatrix[LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p-q),opbs[a]],P1hat P2hat P3hat P4 P5];*)
(*traceamp["bquark",c2,a_]:=Spur[opqq[a],-\[Gamma] . (l+q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],-\[Gamma] . (l)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],-\[Gamma] . (l+r)+mb \[DoubleStruckOne]]CreateAmplitude[CFa[a] (Qc)DiracMatrix[LTensor[\[Gamma],\[Nu]1],\[Gamma] . (r+p-q),opbs[a]],P1mb P2mb P3mb P4 P5];*)


(* ::Input:: *)
(*traceamp["cquark",d1,a_]:=Spur[opqq[a],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+q+r)+mc \[DoubleStruckOne]]CreateAmplitude[CFa[a] (Qc) DiracMatrix[opbs[a],\[Gamma] . (p+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1]],P1 P2 P12 P4 P11];*)
(*traceamp["masslessquark",d1,a_]:=Spur[opqq[a],\[Gamma] . (l),LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+q+r)]CreateAmplitude[CFa[a] (Qc) DiracMatrix[opbs[a],\[Gamma] . (p+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1]],P1hat P2hat P12hat P4 P11];*)
(*traceamp["bquark",d1,a_]:=Spur[opqq[a],\[Gamma] . (l)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+q+r)+mb \[DoubleStruckOne]]CreateAmplitude[CFa[a] (Qc) DiracMatrix[opbs[a],\[Gamma] . (p+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1]],P1mb P2mb P12mb P4 P11];*)


(* ::Input:: *)
(*traceamp["cquark",d2,a_]:=Spur[opqq[a],-\[Gamma] . (l+q+r)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],-\[Gamma] . (l+q)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],-\[Gamma] . (l)+mc \[DoubleStruckOne]]CreateAmplitude[CFa [a](Qc)DiracMatrix[opbs[a],\[Gamma] . (p+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1]],P1 P2 P12 P4 P11];*)
(*traceamp["bquark",d2,a_]:=Spur[opqq[a],-\[Gamma] . (l+q+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],-\[Gamma] . (l+q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],-\[Gamma] . (l)+mb \[DoubleStruckOne]]CreateAmplitude[CFa [a](Qc)DiracMatrix[opbs[a],\[Gamma] . (p+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1]],P1mb P2mb P12mb P4 P11];*)
(*traceamp["masslessquark",d2,a_]:=Spur[opqq[a],-\[Gamma] . (l+q+r),LTensor[\[Gamma],\[Nu]1],-\[Gamma] . (l+q),LTensor[\[Gamma],\[Mu]],-\[Gamma] . (l)]CreateAmplitude[CFa [a](Qc)DiracMatrix[opbs[a],\[Gamma] . (p+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1]],P1hat P2hat P12hat P4 P11];*)
(**)


(* ::Code::Plain:: *)
(*traceamp["cquark",e0i,a_]:=Spur[opqq[a],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],*)
(*(\[Gamma] . (l+r)+mc \[DoubleStruckOne]),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mc \[DoubleStruckOne]]CreateAmplitude[-CFe[a](Qb)DiracMatrix[opbs[a],\[Gamma] . (p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]]],P2^2 *)
(*P3 P4 mb^2];*)
(*traceamp["masslessquark",e0i,a_]:=Spur[opqq[a],\[Gamma] . (l),LTensor[\[Gamma],\[Nu]1],*)
(*(\[Gamma] . (l+r)),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)]CreateAmplitude[-CFe[a](Qb)DiracMatrix[opbs[a],\[Gamma] . (p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]]],P2hat^2 *)
(*P3hat P4 mb^2];*)
(*traceamp["bquark",e0i,a_]:=Spur[opqq[a],\[Gamma] . (l)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],*)
(*(\[Gamma] . (l+r)+mb \[DoubleStruckOne]),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mb \[DoubleStruckOne]]CreateAmplitude[-CFe[a](Qb)DiracMatrix[opbs[a],\[Gamma] . (p-q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]]],P2mb^2*)
(* P3mb P4 mb^2];*)


(* ::Input:: *)
(*traceamp["cquark",e0ii,a_]:=Spur[opqq[a],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],*)
(*(\[Gamma] . (l+r)+mc \[DoubleStruckOne]),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mc \[DoubleStruckOne]]CreateAmplitude[CFe[a] (Qs) DiracMatrix[LTensor[\[Gamma],\[Mu]],\[Gamma] . p ,opbs[a]],P2^2 P3 P4 mb^2];*)
(*traceamp["masslessquark",e0ii,a_]:=Spur[opqq[a],\[Gamma] . (l),LTensor[\[Gamma],\[Nu]1],*)
(*(\[Gamma] . (l+r)),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)]CreateAmplitude[CFe[a] (Qs) DiracMatrix[LTensor[\[Gamma],\[Mu]],\[Gamma] . p ,opbs[a]],P2hat^2 P3hat P4 mb^2];*)
(*traceamp["bquark",e0ii,a_]:=Spur[opqq[a],\[Gamma] . (l)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],*)
(*(\[Gamma] . (l+r)+mb \[DoubleStruckOne]),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mb \[DoubleStruckOne]]CreateAmplitude[CFe[a] (Qs) DiracMatrix[LTensor[\[Gamma],\[Mu]],\[Gamma] . p ,opbs[a]],P2mb^2 P3mb P4 mb^2];*)


(* ::Input:: *)
(*traceamp["cquark",e1,a_]:=Simplify[Spur[opqq[a],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q+r)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+q)+mc \[DoubleStruckOne]]CreateAmplitude[CFe[a] (Qc)DiracMatrix[opbs[a]],P1 P2 P3 P4 P12]];*)
(*traceamp["masslessquark",e1,a_]:=Simplify[Spur[opqq[a],\[Gamma] . (l),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r),LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q+r),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+q)]CreateAmplitude[CFe[a] (Qc)DiracMatrix[opbs[a]],P1hat P2hat P3hat P4 P12hat]];*)
(*traceamp["bquark",e1,a_]:=Simplify[Spur[opqq[a],\[Gamma] . (l)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+q)+mb \[DoubleStruckOne]]CreateAmplitude[CFe[a] (Qc)DiracMatrix[opbs[a]],P1mb P2mb P3mb P4 P12mb]];*)


(* ::Input:: *)
(*traceamp["cquark",e2,a_]:=Simplify[Spur[opqq[a],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)+ mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q)+mc \[DoubleStruckOne]]CreateAmplitude[CFe[a](Qc)DiracMatrix[opbs[a]],P1 P2^2 P3 P4]];*)
(*traceamp["masslessquark",e2,a_]:=Simplify[Spur[opqq[a],\[Gamma] . (l),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l),LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q)]CreateAmplitude[CFe[a](Qc)DiracMatrix[opbs[a]],P1hat P2hat^2 P3hat P4]];*)
(*traceamp["bquark",e2,a_]:=Simplify[Spur[opqq[a],\[Gamma] . (l)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+r)+ mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q)+mb \[DoubleStruckOne]]CreateAmplitude[CFe[a](Qc)DiracMatrix[opbs[a]],P1mb P2mb^2 P3mb P4]];*)


(* ::Input:: *)
(*traceamp["cquark",e3,a_]:=Simplify[Spur[opqq[a],\[Gamma] . (l)+ mc \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+q+r)+mc \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+q)+mc \[DoubleStruckOne]]CreateAmplitude[CFe[a] (Qc)DiracMatrix[opbs[a]],P1^2 P2 P4 P12]];*)
(*traceamp["masslessquark",e3,a_]:=Simplify[Spur[opqq[a],\[Gamma] . (l),LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+q+r),LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+q)]CreateAmplitude[CFe[a] (Qc)DiracMatrix[opbs[a]],P1hat^2 P2hat P4 P12hat]];*)
(*traceamp["bquark",e3,a_]:=Simplify[Spur[opqq[a],\[Gamma] . (l)+ mb \[DoubleStruckOne],LTensor[\[Gamma],\[Mu]],\[Gamma] . (l+q)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+q+r)+mb \[DoubleStruckOne],LTensor[\[Gamma],\[Nu]1],\[Gamma] . (l+q)+mb \[DoubleStruckOne]]CreateAmplitude[CFe[a] (Qc)DiracMatrix[opbs[a]],P1mb^2 P2mb P4 P12mb]];*)


(* ::Section:: *)
(*Calculations*)


(* ::Subsection:: *)
(*Scalar*)


(* ::Subsubsection:: *)
(*a (3-40)*)


(* ::Input:: *)
(*Do[scalar[diag,oper]=If[traceamp["cquark",diag,oper]===0,0,Sum[PropagatorReplace["a",Contract[ReduceToScalar[traceamp["cquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{a1,a2,a3}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[scalar["massless",diag,oper]=If[traceamp["masslessquark",diag,oper]===0,0,Sum[PropagatorReplace["ahat",Contract[ReduceToScalar[traceamp["masslessquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{a1,a2,a3}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)
(*Do[scalar["bquark",diag,oper]=If[traceamp["bquark",diag,oper]===0,0,Sum[PropagatorReplace["amb",Contract[ReduceToScalar[traceamp["bquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{a1,a2,a3}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Subsubsection:: *)
(*a (1-2)*)


(* ::Input:: *)
(*Do[scalar[diag,oper]=If[amp[diag,oper]===0,0,Sum[PropagatorReplace["a",Contract[ReduceToScalar[amp[diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{a1,a2,a3}},{oper,{"1s","2s"}}]*)


(* ::Input:: *)
(*Do[scalar["massless",diag,oper]=If[amp["masslessquark",diag,oper]===0,0,Sum[PropagatorReplace["ahat",Contract[ReduceToScalar[amp["masslessquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{a1,a2,a3}},{oper,{"1s","2s"}}]*)
(*Do[scalar["bquark",diag,oper]=If[amp["bquark",diag,oper]===0,0,Sum[PropagatorReplace["amb",Contract[ReduceToScalar[amp["bquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{a1,a2,a3}},{oper,{"1s","2s"}}]*)


(* ::Subsubsection:: *)
(*b (3-40)*)


(* ::Input:: *)
(*Do[scalar[diag,oper]=If[traceamp["cquark",diag,oper]===0,0,Sum[PropagatorReplace["b",Contract[ReduceToScalar[traceamp["cquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{b1,b2,b3}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[scalar["massless",diag,oper]=If[traceamp["masslessquark",diag,oper]===0,0,Sum[PropagatorReplace["bhat",Contract[ReduceToScalar[traceamp["masslessquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{b1,b2,b3}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)
(*Do[scalar["bquark",diag,oper]=If[traceamp["bquark",diag,oper]===0,0,Sum[PropagatorReplace["bmb",Contract[ReduceToScalar[traceamp["bquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{b1,b2,b3}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Subsubsection:: *)
(*b (1-2)*)


(* ::Input:: *)
(*Do[scalar[diag,oper]=If[amp[diag,oper]===0,0,Sum[PropagatorReplace["b",Contract[ReduceToScalar[amp[diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{b1,b2,b3}},{oper,{"1s","2s"}}]*)


(* ::Input:: *)
(*Do[scalar["massless",diag,oper]=If[amp["masslessquark",diag,oper]===0,0,Sum[PropagatorReplace["bhat",Contract[ReduceToScalar[amp["masslessquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{b1,b2,b3}},{oper,{"1s","2s"}}]*)
(*Do[scalar["bquark",diag,oper]=If[amp["bquark",diag,oper]===0,0,Sum[PropagatorReplace["bmb",Contract[ReduceToScalar[amp["bquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{b1,b2,b3}},{oper,{"1s","2s"}}]*)


(* ::Subsubsection:: *)
(*c (3-40)*)


(* ::Input:: *)
(*Do[scalar[diag,oper]=If[traceamp["cquark",diag,oper]===0,0,Sum[PropagatorReplace["c",Contract[ReduceToScalar[traceamp["cquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{c1,c2}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[scalar["massless",diag,oper]=If[traceamp["masslessquark",diag,oper]===0,0,Sum[PropagatorReplace["chat",Contract[ReduceToScalar[traceamp["masslessquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{c1,c2}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)
(*Do[scalar["bquark",diag,oper]=If[traceamp["bquark",diag,oper]===0,0,Sum[PropagatorReplace["cmb",Contract[ReduceToScalar[traceamp["bquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{c1,c2}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Subsubsection:: *)
(*c (1-2)*)


(* ::Input:: *)
(*Do[scalar[diag,oper]=If[amp[diag,oper]===0,0,Sum[PropagatorReplace["c",Contract[ReduceToScalar[amp[diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{c1,c2}},{oper,{"1s","2s"}}]*)


(* ::Input:: *)
(*Do[scalar["massless",diag,oper]=If[amp["masslessquark",diag,oper]===0,0,Sum[PropagatorReplace["chat",Contract[ReduceToScalar[amp["masslessquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{c1,c2}},{oper,{"1s","2s"}}]*)
(*Do[scalar["bquark",diag,oper]=If[amp["bquark",diag,oper]===0,0,Sum[PropagatorReplace["cmb",Contract[ReduceToScalar[amp["bquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{c1,c2}},{oper,{"1s","2s"}}]*)


(* ::Subsubsection:: *)
(*d (3-40)*)


(* ::Input:: *)
(*Do[scalar[diag,oper]=If[traceamp["cquark",diag,oper]===0,0,Sum[PropagatorReplace["d",Contract[ReduceToScalar[traceamp["cquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{d1,d2}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[scalar["massless",diag,oper]=If[traceamp["masslessquark",diag,oper]===0,0,Sum[PropagatorReplace["dhat",Contract[ReduceToScalar[traceamp["masslessquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{d1,d2}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)
(*Do[scalar["bquark",diag,oper]=If[traceamp["bquark",diag,oper]===0,0,Sum[PropagatorReplace["dmb",Contract[ReduceToScalar[traceamp["bquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{d1,d2}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Subsubsection:: *)
(*d (1-2)*)


(* ::Input:: *)
(*Do[scalar[diag,oper]=If[amp[diag,oper]===0,0,Sum[PropagatorReplace["d",Contract[ReduceToScalar[amp[diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{d1,d2}},{oper,{"1s","2s"}}]*)


(* ::Input:: *)
(*Do[scalar["massless",diag,oper]=If[amp["masslessquark",diag,oper]===0,0,Sum[PropagatorReplace["dhat",Contract[ReduceToScalar[amp["masslessquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{d1,d2}},{oper,{"1s","2s"}}]*)
(*Do[scalar["bquark",diag,oper]=If[amp["bquark",diag,oper]===0,0,Sum[PropagatorReplace["dmb",Contract[ReduceToScalar[amp["bquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{d1,d2}},{oper,{"1s","2s"}}]*)


(* ::Subsubsection:: *)
(*e (3-40)*)


(* ::Input:: *)
(*Do[scalar[diag,oper]=If[traceamp["cquark",diag,oper]===0,0,Sum[PropagatorReplace["e",Contract[ReduceToScalar[traceamp["cquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{e0i,e0ii,e1,e2,e3}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[scalar["massless",diag,oper]=If[traceamp["masslessquark",diag,oper]===0,0,Sum[PropagatorReplace["ehat",Contract[ReduceToScalar[traceamp["masslessquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{e0i,e0ii,e1,e2,e3}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)
(*Do[scalar["bquark",diag,oper]=If[traceamp["bquark",diag,oper]===0,0,Sum[PropagatorReplace["emb",Contract[ReduceToScalar[traceamp["bquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{e0i,e0ii,e1,e2,e3}},{oper,{"3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Subsubsection:: *)
(*e (1-2)*)


(* ::Input:: *)
(*Do[scalar[diag,oper]=If[amp[diag,oper]===0,0,Sum[PropagatorReplace["e",Contract[ReduceToScalar[amp[diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{e0i,e0ii,e1,e2,e3}},{oper,{"1s","2s"}}]*)


(* ::Input:: *)
(*Do[scalar["massless",diag,oper]=If[amp["masslessquark",diag,oper]===0,0,Sum[PropagatorReplace["ehat",Contract[ReduceToScalar[amp["masslessquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{e0i,e0ii,e1,e2,e3}},{oper,{"1s","2s"}}]*)
(*Do[scalar["bquark",diag,oper]=If[amp["bquark",diag,oper]===0,0,Sum[PropagatorReplace["emb",Contract[ReduceToScalar[amp["bquark",diag,oper]]]/.{LTensor[LeviCivitaE,{l},{r},{p},{q}]->0}][[i]]Struct[i],{i,1,6}]/.(mb^2-q2)->t],{diag,{e0i,e0ii,e1,e2,e3}},{oper,{"1s","2s"}}]*)


(* ::Subsubsection:: *)
(*write to file*)


(* ::Input:: *)
(*SetDirectory[];*)
(*file=OpenWrite["scalar_results.m"];*)
(*Do[*)
(*WriteString[file,"scalar[",diag,",",oper,"]= ",scalar[diag,oper]//InputForm,"\n\n"],{diag,{a1,a2,a3,b1,b2,b3,c1,c2,d1,d2,e0i,e0ii,e1,e2,e3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}];*)
(*Do[*)
(*WriteString[file,"scalar[",quarktype,",",diag,",",oper,"]= ",scalar[quarktype,diag,oper]//InputForm,"\n\n"],{diag,{a1,a2,a3,b1,b2,b3,c1,c2,d1,d2,e0i,e0ii,e1,e2,e3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}},{quarktype,{"massless","bquark"}}];*)
(*Close[file];*)


(* ::Subsection:: *)
(*J Integrals*)


(* ::Input:: *)
(*Do[jIntegral[diag,oper]=(jIntegralForm["a",scalar[diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{a1,a2,a3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[jIntegral["massless",diag,oper]=(jIntegralForm["ahat",scalar["massless",diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{a1,a2,a3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)
(*Do[jIntegral["mb",diag,oper]=(jIntegralForm["amb",scalar["bquark",diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{a1,a2,a3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[jIntegral[diag,oper]=(jIntegralForm["b",scalar[diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{b1,b2,b3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[jIntegral["massless",diag,oper]=(jIntegralForm["bhat",scalar["massless",diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{b1,b2,b3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)
(*Do[jIntegral["mb",diag,oper]=(jIntegralForm["bmb",scalar["bquark",diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{b1,b2,b3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[jIntegral[diag,oper]=(jIntegralForm["c",scalar[diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{c1,c2}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[jIntegral["massless",diag,oper]=(jIntegralForm["chat",scalar["massless",diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{c1,c2}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)
(*Do[jIntegral["mb",diag,oper]=(jIntegralForm["cmb",scalar["bquark",diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{c1,c2}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[jIntegral[diag,oper]=(jIntegralForm["d",scalar[diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{d1,d2}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[jIntegral["massless",diag,oper]=(jIntegralForm["dhat",scalar["massless",diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{d1,d2}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)
(*Do[jIntegral["mb",diag,oper]=(jIntegralForm["dmb",scalar["bquark",diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{d1,d2}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[jIntegral[diag,oper]=(jIntegralForm["e",scalar[diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{e0i,e0ii,e1,e2,e3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[jIntegral["massless",diag,oper]=(jIntegralForm["ehat",scalar["massless",diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{e0i,e0ii,e1,e2,e3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)
(*Do[jIntegral["mb",diag,oper]=(jIntegralForm["emb",scalar["bquark",diag,oper]]/.t->(mb^2-q2)/.{Struct[1]->2AStruc-Struct[2],Struct[3]->2BStruc-Struct[4],Struct[5]->2CStruc mb+Struct[6]}//Simplify),{diag,{e0i,e0ii,e1,e2,e3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*SetDirectory[];*)
(*file=OpenWrite["j_integral_results.m"];*)
(*Do[*)
(*WriteString[file,"jIntegral[",diag,",",oper,"]= ",jIntegral[diag,oper]//InputForm,"\n\n"],{diag,{a1,a2,a3,b1,b2,b3,c1,c2,d1,d2,e0i,e0ii,e1,e2,e3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}];*)
(*Do[*)
(*WriteString[file,"jIntegral[",quarktype,",",diag,",",oper,"]= ",jIntegral[quarktype,diag,oper]//InputForm,"\n\n"],{diag,{a1,a2,a3,b1,b2,b3,c1,c2,d1,d2,e0i,e0ii,e1,e2,e3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}},{quarktype,{"massless","mb"}}];*)
(*Close[file];*)


(* ::Subsection:: *)
(*Master Integrals*)


(* ::Input:: *)
(*Do[master[diag,oper]=Simplify[Collect[ScalelessReplacementRuleA[jIntegral[diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.a[1,1,0,0,0,0,0]->Ja1/.a[1,1,0,0,1,0,0]->Ja2/.a[2,1,0,0,1,0,0]->Ja3/.a[0,1,0,1,1,0,0]->Ja4/.a[0,1,1,0,1,0,0]->Ja5/.a[1,1,0,1,1,0,0]->Ja6/.a[2,1,0,1,1,0,0]->Ja7,{AStruc,BStruc,CStruc}]],{diag,{a1,a2,a3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)
(*Do[master["massless",diag,oper]=Simplify[Collect[ScalelessReplacementRuleMasslessA[jIntegral["massless",diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.ahat[1,1,0,0,0,0,0]->Ja1hat/.ahat[1,1,0,0,1,0,0]->Ja2hat/.ahat[2,1,0,0,1,0,0]->Ja3hat/.ahat[0,1,0,1,1,0,0]->Ja4hat/.a[0,1,1,0,1,0,0]->Ja5hat/.ahat[1,1,0,1,1,0,0]->Ja6hat/.ahat[2,1,0,1,1,0,0]->Ja7hat,{AStruc,BStruc,CStruc}]],{diag,{a1,a2,a3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)
(*Do[master["bquark",diag,oper]=Simplify[Collect[ScalelessReplacementRulebQuarkA[jIntegral["mb",diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.amb[1,1,0,0,0,0,0]->Ja1mb/.amb[1,1,0,0,1,0,0]->Ja2mb/.amb[2,1,0,0,1,0,0]->Ja3mb/.amb[0,1,0,1,1,0,0]->Ja4mb/.amb[0,1,1,0,1,0,0]->Ja5mb/.amb[1,1,0,1,1,0,0]->Ja6mb/.amb[2,1,0,1,1,0,0]->Ja7mb,{AStruc,BStruc,CStruc}]],{diag,{a1,a2,a3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[master[diag,oper]=Collect[Simplify[ScalelessReplacementRuleB[jIntegral[diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.b[0,1,0,0,1,0,0]->Jb1/.b[1,1,0,0,0,0,0]->Jb2/.b[1,1,0,1,0,0,0]->Jb3/.b[1,1,0,0,1,0,0]->Jb4/.b[2,1,0,0,1,0,0]->Jb5/.b[0,1,0,1,1,0,0]->Jb6/.b[1,1,0,1,1,0,0]->Jb7/.b[2,1,0,1,1,0,0]->Jb8/.b[1,1,0,2,1,0,0]->Jb9/.b[1,0,1,1,1,0,0]->Jb10],{CStruc,BStruc,AStruc}],{diag,{b1,b2,b3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[master["massless",diag,oper]=Collect[Simplify[ScalelessReplacementRuleMasslessB[jIntegral["massless",diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.bhat[0,1,0,0,1,0,0]->Jb1hat/.bhat[1,1,0,0,0,0,0]->Jb2hat/.bhat[1,1,0,1,0,0,0]->Jb3hat/.bhat[1,1,0,0,1,0,0]->Jb4hat/.bhat[2,1,0,0,1,0,0]->Jb5hat/.bhat[0,1,0,1,1,0,0]->Jb6hat/.bhat[1,1,0,1,1,0,0]->Jb7hat/.bhat[2,1,0,1,1,0,0]->Jb8hat/.bhat[1,1,0,2,1,0,0]->Jb9hat/.bhat[1,0,1,1,1,0,0]->Jb10hat],{CStruc,BStruc,AStruc}],{diag,{b1,b2,b3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)
(*Do[master["bquark",diag,oper]=Collect[Simplify[ScalelessReplacementRulebQuarkB[jIntegral["mb",diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.bmb[0,1,0,0,1,0,0]->Jb1mb/.bmb[1,1,0,0,0,0,0]->Jb2mb/.bmb[1,1,0,1,0,0,0]->Jb3mb/.bmb[1,1,0,0,1,0,0]->Jb4mb/.bmb[2,1,0,0,1,0,0]->Jb5mb/.bmb[0,1,0,1,1,0,0]->Jb6mb/.bmb[1,1,0,1,1,0,0]->Jb7mb/.bmb[2,1,0,1,1,0,0]->Jb8mb/.bmb[1,1,0,2,1,0,0]->Jb9mb/.bmb[1,0,1,1,1,0,0]->Jb10mb],{CStruc,BStruc,AStruc}],{diag,{b1,b2,b3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[master[diag,oper]=Simplify[Collect[ScalelessReplacementRuleC[jIntegral[diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.c[0,1,1,0,0,0,0]->Jc1/.c[1,0,1,0,1,0,0]->Jc2/.c[1,0,1,1,0,0,0]->Jc3/.c[1,1,1,0,0,0,0]->Jc4/.c[1,1,1,0,1,0,0]->Jc5/.c[1,2,1,0,1,0,0]->Jc6/.c[2,0,1,0,1,0,0]->Jc7/.c[2,0,1,1,0,0,0]->Jc8/.c[2,1,1,0,1,0,0]->Jc9,{CStruc,BStruc,AStruc}]],{diag,{c1,c2}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[master["massless",diag,oper]=Simplify[Collect[ScalelessReplacementRuleMasslessC[jIntegral["massless",diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.chat[0,1,1,0,0,0,0]->Jc1hat/.chat[1,0,1,0,1,0,0]->Jc2hat/.chat[1,0,1,1,0,0,0]->Jc3hat/.chat[1,1,1,0,0,0,0]->Jc4hat/.chat[1,1,1,0,1,0,0]->Jc5hat/.chat[1,2,1,0,1,0,0]->Jc6hat/.chat[2,0,1,0,1,0,0]->Jc7hat/.chat[2,0,1,1,0,0,0]->Jc8hat/.chat[2,1,1,0,1,0,0]->Jc9hat,{CStruc,BStruc,AStruc}]],{diag,{c1,c2}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)
(*Do[master["bquark",diag,oper]=Simplify[Collect[ScalelessReplacementRulebQuarkC[jIntegral["mb",diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.cmb[0,1,1,0,0,0,0]->Jc1mb/.cmb[1,0,1,0,1,0,0]->Jc2mb/.cmb[1,0,1,1,0,0,0]->Jc3mb/.cmb[1,1,1,0,0,0,0]->Jc4mb/.cmb[1,1,1,0,1,0,0]->Jc5mb/.cmb[1,2,1,0,1,0,0]->Jc6mb/.cmb[2,0,1,0,1,0,0]->Jc7mb/.cmb[2,0,1,1,0,0,0]->Jc8mb/.cmb[2,1,1,0,1,0,0]->Jc9mb,{CStruc,BStruc,AStruc}]],{diag,{c1,c2}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[master[diag,oper]=Collect[Simplify[ScalelessReplacementRuleD[jIntegral[diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.d[0,1,1,0,0,0,0]->Jd1/.d[0,0,1,0,1,0,0]->Jd2/.d[0,1,1,0,1,0,0]->Jd3/.d[0,1,1,1,0,0,0]->Jd4/.d[0,2,1,1,0,0,0]->Jd5/.d[1,0,1,0,1,0,0]->Jd6/.d[2,0,1,0,1,0,0]->Jd7/.d[1,1,0,0,1,0,0]->Jd8/.d[1,1,1,0,0,0,0]->Jd9/.d[0,1,1,1,1,0,0]->Jd10/.d[0,2,1,1,1,0,0]->Jd11/.d[1,1,1,0,1,0,0]->Jd12/.d[1,2,1,0,1,0,0]->Jd13/.d[2,1,1,0,1,0,0]->Jd14/.d[1,1,2,0,1,0,0]->Jd15],{CStruc,BStruc,AStruc}],{diag,{d1,d2}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[master["massless",diag,oper]=Collect[Simplify[ScalelessReplacementRuleMasslessD[jIntegral["massless",diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.dhat[0,1,1,0,0,0,0]->Jd1hat/.dhat[0,0,1,0,1,0,0]->Jd2hat/.dhat[0,1,1,0,1,0,0]->Jd3hat/.dhat[0,1,1,1,0,0,0]->Jd4hat/.dhat[0,2,1,1,0,0,0]->Jd5hat/.dhat[1,0,1,0,1,0,0]->Jd6hat/.dhat[2,0,1,0,1,0,0]->Jd7hat/.dhat[1,1,0,0,1,0,0]->Jd8hat/.dhat[1,1,1,0,0,0,0]->Jd9hat/.dhat[0,1,1,1,1,0,0]->Jd10hat/.dhat[0,2,1,1,1,0,0]->Jd11hat/.dhat[1,1,1,0,1,0,0]->Jd12hat/.dhat[1,2,1,0,1,0,0]->Jd13hat/.dhat[2,1,1,0,1,0,0]->Jd14hat/.dhat[1,1,2,0,1,0,0]->Jd15hat],{CStruc,BStruc,AStruc}],{diag,{d1,d2}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)
(*Do[master["bquark",diag,oper]=Collect[Simplify[ScalelessReplacementRulebQuarkD[jIntegral["mb",diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.dmb[0,1,1,0,0,0,0]->Jd1mb/.dmb[0,0,1,0,1,0,0]->Jd2mb/.dmb[0,1,1,0,1,0,0]->Jd3mb/.dmb[0,1,1,1,0,0,0]->Jd4mb/.dmb[0,2,1,1,0,0,0]->Jd5mb/.dmb[1,0,1,0,1,0,0]->Jd6mb/.dmb[2,0,1,0,1,0,0]->Jd7mb/.dmb[1,1,0,0,1,0,0]->Jd8mb/.dmb[1,1,1,0,0,0,0]->Jd9mb/.dmb[0,1,1,1,1,0,0]->Jd10mb/.dmb[0,2,1,1,1,0,0]->Jd11mb/.dmb[1,1,1,0,1,0,0]->Jd12mb/.dmb[1,2,1,0,1,0,0]->Jd13mb/.dmb[2,1,1,0,1,0,0]->Jd14mb/.dmb[1,1,2,0,1,0,0]->Jd15mb],{CStruc,BStruc,AStruc}],{diag,{d1,d2}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[master[diag,oper]=Collect[Simplify[ScalelessReplacementRuleE[jIntegral[diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.e[0,1,0,0,1,0,0]->Je1/.e[0,1,0,1,1,0,0]->Je2/.e[0,1,1,0,1,0,0]->Je3/.e[0,2,0,1,1,0,0]->Je4/.e[1,1,1,0,1,0,0]->Je5],{CStruc,BStruc,AStruc}],{diag,{e0i,e0ii,e1,e2,e3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*Do[master["massless",diag,oper]=Collect[Simplify[ScalelessReplacementRuleMasslessE[jIntegral["massless",diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.ehat[0,1,0,0,1,0,0]->Je1hat/.ehat[0,1,0,1,1,0,0]->Je2hat/.ehat[0,1,1,0,1,0,0]->Je3hat/.ehat[0,2,0,1,1,0,0]->Je4hat/.ehat[1,1,1,0,1,0,0]->Je5hat],{CStruc,BStruc,AStruc}],{diag,{e0i,e0ii,e1,e2,e3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)
(*Do[master["bquark",diag,oper]=Collect[Simplify[ScalelessReplacementRulebQuarkE[jIntegral["mb",diag,oper]]/.{t->(mb^2-q2),\[ScriptD]->d}/.{z->mc^2/mb^2,s->q2/mb^2}/.emb[0,1,0,0,1,0,0]->Je1mb/.emb[0,1,0,1,1,0,0]->Je2mb/.emb[0,1,1,0,1,0,0]->Je3mb/.emb[0,2,0,1,1,0,0]->Je4mb/.emb[1,1,1,0,1,0,0]->Je5mb],{CStruc,BStruc,AStruc}],{diag,{e0i,e0ii,e1,e2,e3}},{oper,{"1s","2s","3s","4s","5s","6s","11s","12s","13s","19s","20s","25s","26s","30s","31s"}}]*)


(* ::Input:: *)
(*SetDirectory[];*)
(*file=OpenWrite["master_integral_results.m"];*)
(*Do[*)
(*WriteString[file,"master[",diag,",",oper,"]= ",master[diag,oper]//InputForm,"\n\n"],{diag,{a1,a2,a3,b1,b2,b3,c1,c2,d1,d2,e0i,e0ii,e1,e2,e3}},{oper,{"11","12","13","19","20","25","26","30","31"}}];*)
(*Do[*)
(*WriteString[file,"master[",quarktype,",",diag,",",oper,"]= ",master[quarktype,diag,oper]//InputForm,"\n\n"],{diag,{a1,a2,a3,b1,b2,b3,c1,c2,d1,d2,e0i,e0ii,e1,e2,e3}},{oper,{"11","12","13","19","20","25","26","30","31"}},{quarktype,{"massless","bquark"}}];*)
(*Close[file];*)
