(* ::Package:: *)

(* ::Input::Initialization:: *)
Needs["WolframChemistry`MoleculeFingerprints`"];
validSmilesQ[smile_String] :=
    MatchQ[Quiet[Molecule[smile]], Except[$Failed]]


(* ::Input::Initialization:: *)
moleculeListQ[list_] := AllTrue[list, MoleculeQ]


(* ::Input::Initialization:: *)
cleanDataset[data_Dataset] :=
    data[Select[StringFreeQ[#Smiles, "."]&]]


(* ::Input::Initialization:: *)
SetAttributes[makeID, Listable];
makeID[prefix_String, number_Integer?Positive, maxDigits_Integer?Positive] :=
    prefix <> IntegerString[number, 10, Max[maxDigits, Ceiling[Log10[number]]]]


(* ::Input::Initialization:: *)
Options[generateDataset]={"FieldSeparator"->";"};
generateDataset[file_,OptionsPattern[]] := Module[{data, dataset, idKey, ids},
  data = Import[file, "Table", "FieldSeparators" -> OptionValue["FieldSeperator"], "RepeatedSeparators" -> False];
  dataset = Dataset[AssociationThread[First[data] -> #]& /@ Rest[data]];
  idKey = SelectFirst[Keys[First[dataset]] // Normal, StringMatchQ[RegularExpression["(?i).*ID.*"]]];
  ids = If[MissingQ[idKey], makeID["ID", Range[Length[dataset]], Ceiling[Log10[Length[dataset]]]], Normal@dataset[All, idKey]];
  AssociationThread[ids, Normal@dataset[All, {"Smiles"}]] // Dataset // cleanDataset
]


(* ::Input::Initialization:: *)
Options[generateDatasetBio]={"FieldSeparator"->";"};
generateDatasetBio[file_,biotype_,OptionsPattern[]] := Module[{data, dataset, idKey, ids},
  data = Import[file, "Table", "FieldSeparators" -> OptionValue["FieldSeparator"], "RepeatedSeparators" -> False];
  dataset = Dataset[AssociationThread[First[data] -> #]& /@ Rest[data]];
  idKey = SelectFirst[Keys[First[dataset]] // Normal, StringMatchQ[RegularExpression["(?i).*ID.*"]]];
  ids = If[MissingQ[idKey], makeID["ID", Range[Length[dataset]], Ceiling[Log10[Length[dataset]]]], Normal@dataset[All, idKey]];
  dataset = AssociationThread[ids, Normal@dataset[All, {"Smiles", "Standard Value"}]] // Dataset;
  dataset = dataset[Select[NumericQ[#"Standard Value"]&]] // cleanDataset;
  dataset[All, {"Standard Value" -> (9 - Log10[#]&)}][All, KeyMap[# /. "Standard Value" -> biotype&]]
]


(* ::Input::Initialization:: *)
Options[assignFingerprintsList] = Join[{"Fingerprint" -> TopologicalFingerprint}, Options[TopologicalFingerprint],Options[generateDataset]];
assignFingerprintsList[molList_, OptionsPattern[]] :=
    Dataset[AssociationThread[{"Molecule", "Fingerprint"} -> #]& /@ Map[{#, OptionValue["Fingerprint"][#]}&, molList]]


(* ::Input::Initialization:: *)
Options[assignFingerprints] = Join[{"Fingerprint" -> TopologicalFingerprint}, Options[TopologicalFingerprint],Options[generateDataset]];
assignFingerprints[dataset_Dataset, opts : OptionsPattern[]] := dataset[All,
  With[{mol = Molecule[#"Smiles", IncludeHydrogens -> False]}, <|##,
    "Molecule" -> mol, "Fingerprint" -> OptionValue["Fingerprint"][mol]|>]&];
