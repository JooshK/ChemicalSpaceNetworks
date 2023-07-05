(* ::Package:: *)

(* ::Input::Initialization:: *)
SetDirectory@NotebookDirectory[]


(* ::Input::Initialization:: *)
Needs["WolframChemistry`MoleculeFingerprints`"]


(* ::Input::Initialization:: *)
<<../InputFunctions.wl


(* ::Input::Initialization:: *)
defualtpKiToColor[pKi_]:=Which[
pKi<5,Darker@Red,
5<=pKi<6,Red,
6<=pKi<7,Orange,
7<=pKi<8,Yellow,
8<=pKi<9,Darker@Green,
9<=pKi<10,LightBlue,
10<=pKi,Blue,True,White]


(* ::Input::Initialization:: *)
Options[generateInput]=Join[{"BioData"->False},Options[assignFingerprintsList]];
generateInput[userInput_,opts:OptionsPattern[]]:=Module[{dataset},
dataset=Which[
StringQ[userInput]&&StringEndsQ[userInput,".csv"],If[TrueQ[OptionValue["BioData"]],generateDatasetBio[userInput],generateDataset[userInput]],
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
generateGraph[fingerprintData_Dataset,cutoff_,OptionsPattern[]]:=Module[{fingerprints,edges,vertices,style,graph},
fingerprints=KeyValueMap[{#1,#2["Fingerprint"]}&,fingerprintData]//Normal;
edges=ParallelMap[UndirectedEdge[Sequence@@First/@#]->N@*OptionValue["DistanceFunction"]@@Last/@#&,Subsets[fingerprints,{2}]]//Association;
graph=Select[edges,#<=cutoff&]//Keys//Graph[Tooltip/@Union@@List@@@#,#,GraphLayout->OptionValue["Embedding"]]&
]


(* ::Input::Initialization:: *)
Options[chemicalSpaceNetwork]:=Join[{"Embed Molecules"->False},{"Coloring"->defualtpKiToColor},Options[generateGraph],Options[generateInput]]
chemicalSpaceNetwork[userInput_,cutoff_,opts:OptionsPattern[]]:=Module[{fingerprints,vertices,style,graph,legend},
fingerprints=generateInput[userInput,"BioData"->OptionValue["BioData"]];
graph=If[OptionValue["Embed Molecules"],generateGraphWithMols[fingerprints,cutoff],generateGraph[fingerprints,cutoff]];
If[OptionValue["BioData"],
vertices=VertexList[graph];
style=#->OptionValue["Coloring"][fingerprints[#,"pKi"]]&/@vertices;
legend=Row[{BarLegend[{OptionValue["Coloring"],{4,11}},6],Rotate["pKi",90Degree]}];
SetProperty[graph,VertexStyle->style]//Legended[#,legend]&
]
]



