(* ::Package:: *)

(* ::Input::Initialization:: *)
Needs["WolframChemistry`MoleculeFingerprints`"]


(* ::Input::Initialization:: *)
Options[generateEdges]=Options[MoleculeDistance];
generateEdges[data_Dataset,opts:OptionsPattern[]]:=With[{mols=KeyValueMap[{#1,#2["Fingerprint"]}&,data]//Normal},
ParallelMap[UndirectedEdge[Sequence@@First/@#]->N@*OptionValue["DistanceFunction"]@@Last/@#&,Subsets[mols,{2}]]//Association]


(* ::Input::Initialization:: *)
Options[generateDistanceMatrix]=Options[MoleculeDistanceMatrix];
generateDistanceMatrix[data_Dataset]:=
MoleculeDistanceMatrix[Normal@Values@data[All,"Molecule"]]


(* ::Input::Initialization:: *)
Options[generateGraph]={"Embedding"->"GravityEmbedding"};
generateGraph[edges_,cutoff_Real,OptionsPattern[]]:=
Select[edges,#<=cutoff&]//Keys//Graph[Tooltip/@Union@@List@@@#,#,GraphLayout->OptionValue["Embedding"]]&;
