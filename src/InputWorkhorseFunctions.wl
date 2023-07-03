(* ::Package:: *)

(* ::Input::Initialization:: *)
Needs["WolframChemistry`MoleculeFingerprints`"]


(* ::Input::Initialization:: *)
validSmilesQ[smile_?StringQ]:=
MatchQ[Quiet[Molecule[smile]],Except[$Failed]]


(* ::Input::Initialization:: *)
moleculeListQ[list_]:=AllTrue[list,MoleculeQ]
cleanDataset[data_Dataset]:=
data[Select[StringFreeQ[#Smiles,"."]&]];
removeDuplicates[data_Dataset]:=
data[GroupBy[#,#"Molecule ChEMBL ID"&->(<|"Smiles"->#"Smiles","pKi"->#"pKi"|>&),Mean]&]


(* ::Input::Initialization:: *)
moleculeGenerate[file_]:=
Module[{data,dataset},
data=Import[file,"Table","FieldSeparators"->";","RepeatedSeparators"->False];
dataset=Dataset[AssociationThread[First[data]->#]&/@Rest[data]];
dataset=Molecule/@dataset[All,"Smiles"];
DeleteDuplicates[dataset]
]


(* ::Input::Initialization:: *)
generateDataset[file_]:=Module[{dataset,keys},
dataset=Import[file,"Table","FieldSeparators"->";","RepeatedSeparators"->False];
keys=First@dataset;
dataset=AssociationThread[keys,#]&/@Rest@dataset//Dataset;
dataset[Select[NumberQ[Slot["Standard Value"]]&],<|"Molecule ChEMBL ID"->#"Molecule ChEMBL ID","Smiles"->#"Smiles","pKi"->9-Log10[#"Standard Value"]|>&]//SortBy[#"Molecule ChEMBL ID"&]]


(* ::Input::Initialization:: *)
Options[assignFingerprints]=Join[{"Fingerprint"->TopologicalFingerprint},Options[TopologicalFingerprint]];
assignFingerprints[dataset_Dataset,opts:OptionsPattern[]]:=dataset[All,
With[{mol=Molecule[#"Smiles",IncludeHydrogens->False]},<|##,
"Molecule"->mol,"Fingerprint"->OptionValue["Fingerprint"][mol]|>]&];


(* ::Input::Initialization:: *)
Options[assignFingerprintsList]={"Fingerprint"->TopologicalFingerprint};
assignFingerprintsList[molList_,OptionsPattern[]]:=Dataset[AssociationThread[{"Molecule","Fingerprint"}->#]&/@Map[{#,OptionValue["Fingerprint"][#]}&,molList]]

