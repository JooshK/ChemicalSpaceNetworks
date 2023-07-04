(* ::Package:: *)

(* ::Input::Initialization:: *)
SetDirectory@NotebookDirectory[]


(* ::Input::Initialization:: *)
Needs["WolframChemistry`MoleculeFingerprints`"]


(* ::Input::Initialization:: *)
<<InputFunctions.wl


(* ::Input::Initialization:: *)
Options[generateInput]=Options[assignFingerprintsList];
generateInput[userInput_,opts:OptionsPattern[]]:=Module[{dataset},
dataset=Which[
StringQ[userInput]&&StringEndsQ[userInput,".csv"],generateDataset[userInput],
StringQ[userInput]&&StringEndsQ[userInput,".smi"],Import[userInput],
ListQ[userInput]&&moleculeListQ[userInput],userInput,
ListQ[userInput]&&validSmilesQ[First@userInput],Molecule/@userInput,
True,$Failed];
If[dataset===$Failed,Return[$Failed]];
If[ListQ[userInput],
assignFingerprintsList[dataset,"Fingerprint"->OptionValue["Fingerprint"]],
assignFingerprints[dataset,"Fingerprint"->OptionValue["Fingerprint"]]]
]


(* ::Input::Initialization:: *)
Options[generateGraphWithMols]=Join[Options[MoleculeDistance],{"Embedding"->"GravityEmbedding"}];
generateGraphWithMols[fingerprintData_Dataset,cutoff_,OptionsPattern[]]:=Module[{fingerprints,edges},
fingerprints=KeyValueMap[{#1,#2["Fingerprint"]}&,fingerprintData]//Normal;
edges=ParallelMap[UndirectedEdge[Sequence@@First/@#]->N@*OptionValue["DistanceFunction"]@@Last/@#&,Subsets[fingerprints,{2}]]//Association;
Select[edges,#<=cutoff&]//Keys//Graph[Tooltip[#,MoleculePlot[fingerprintData[#,"Molecule"]]]&/@Union@@List@@@#,#,GraphLayout->OptionValue["Embedding"]]&]


(* ::Input::Initialization:: *)
Options[generateGraph]=Join[Options[MoleculeDistance],{"Embedding"->"GravityEmbedding"}];
generateGraph[fingerprintData_Dataset,cutoff_,OptionsPattern[]]:=Module[{fingerprints,edges},
fingerprints=KeyValueMap[{#1,#2["Fingerprint"]}&,fingerprintData]//Normal;
edges=ParallelMap[UndirectedEdge[Sequence@@First/@#]->N@*OptionValue["DistanceFunction"]@@Last/@#&,Subsets[fingerprints,{2}]]//Association;
Select[edges,#<=cutoff&]//Keys//Graph[Tooltip/@Union@@List@@@#,#,GraphLayout->OptionValue["Embedding"]]&]


(* ::Input::Initialization:: *)
Options[chemicalSpaceNetwork]:=Join[{"Embed Molecules"->False},Options[generateGraph],Options[generateInput]];
chemicalSpaceNetwork[userInput_,cutoff_Real?NumberQ,opts:OptionsPattern[]]:=With[{fingerprints=generateInput[userInput]},
If[OptionValue["Embed Molecules"],generateGraphWithMols[fingerprints],generateGraph[fingerprints]]]
