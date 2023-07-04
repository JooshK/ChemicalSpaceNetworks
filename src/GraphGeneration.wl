(* ::Package:: *)

(* ::Input::Initialization:: *)
Needs["WolframChemistry`MoleculeFingerprints`"]
Options[generateGraph]=Join[Options[MoleculeDistance],{"Embedding"->"GravityEmbedding"}];
generateGraph[fingerprintData_Data,cutoff_,OptionsPattern[]]:=Module[{mols=KeyValueMap[{#1,#2["Fingerprint"]}&,fingerprintData]//Normal,edges},     edges=ParallelMap[UndirectedEdge[Sequence@@First/@#]->N@*OptionValue["DistanceFunction"]@@Last/@#&,Subsets[mols,{2}]]//Association;Select[edges,#<=cutoff&]//Keys//Graph[Tooltip/@Union@@List@@@fingerprintData[[#,"Molecule"]],#,GraphLayout->OptionValue["Embedding"]]&;]
