
# Chemical Space Networks in Mathematica

A repository that holds code for an implementation of Chemical Space Networks in Mathematica. For more information see  *[Maggiora, G.M., Bajorath, J. Chemical space networks: a powerful new paradigm for the description of chemical space. J Comput Aided Mol Des 28, 795–802 (2014)](https://link.springer.com/article/10.1007/s10822-014-9760-0#citeas)* as well as [my post](https://community.wolfram.com/groups/-/m/t/2958091?p_p_auth=72PcUJjx) on wolfram community. 




## Instructions

### ChemicalSpaceNetwork - The Function

The recommended way to use the ChemicalSpaceNetwork function is through the Wolfram Function repository by calling

```mathematica
ResourceFunction["ChemicalSpaceNetwork"]
```

If you want to run the code locally, download or clone the repository. The file structure includes 
```markdown
├── src
│   ├── nb
│   │   ├── notebook files
│   ├── src
│   │   ├── *.wl files
├── Data
│   ├── sample datasets
```
The notebook "chemicalSpaceNetwork.nb" holds the same data as "chemicalSpaceNetwork.wl" just in notebook format, run either file and the chemicalSpaceNetwork endpoint will be exposed.

### GUI Instructions
If you want to access the UI within Mathematica, first run the function `ChemicalSpaceNetwork` and ensure that the option `"UI" -> True`. Then download and run the code contained in the userInterface.nb file. 

**Warning** The chemicalSpaceNetwork.nb file is not cross compatible with the user interface, in order to use the UI you should use either the resource function or package script chemicalSpaceNetwork.wl

## Sample Datasets

Included under the `Data` folder are three sample datasets for three targets. The data was collected from the ChEMBL database using the same workflow described in *[Scalfani, V.F., Patel, V.D. & Fernandez, A.M. Visualizing chemical space networks with RDKit and NetworkX. J Cheminform 14, 87 (2022)](https://doi.org/10.1186/s13321-022-00664-x)*. 

The dataset includes the one used in *Scalfani* as well as two additional targets, the D5 dopamine receptor and the cyclin dependent kinase protein family. 

## Documentation

For further documentation and examples see [this post](https://community.wolfram.com/groups/-/m/t/2958091?p_p_auth=72PcUJjx) as well as the function repository submission. 
## Acknowledgements

 - This proejct would not have been possible without contributions from the Wolfram Research, including help from my mentor Robert Nachbar, as well as Stephen Wolfram.