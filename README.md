## cleanRNAseq

# Package containing function to filter out genes with low expression in RNAseq data.

## Description
This package contains a function that checks data for individual genes that express below a certain threshold across a given percentage of samples, filters them out, and returns a dataframe with the genes that succesfully met the user-provided criteria. Upon completion, a message summarazing how many genes were removed is also output.

## Getting started
Load in the `remotes` package and install package from repo:
```
library(remotes)
install_github("bjpolzin/cleanRNAseq")
```

## Usage
### Arguments
* `df`: expression data. A data frame in which columns are genes and rows are samples.

* `min_expr`: minimum expression. The minimum value that a gene must be expressed across the percentage of samples selected. If no user input is provided the default is 10.

* `percent_cutoff`: percentage cutoff. The percent of samples that minimum gene expression (`min_expr`) is required. If no user input is provided the default is 90.

* `metric`: metric/type of data. The type of data (e.g. FPKM, RSEM, raw count, etc.) the expression data is formatted in. This does not change how analysis is done, it simply makes the summary message of the function relevant to your specific analysis. If no user input is provided the the default is "unit".

### Example
If you have the RNAseq dataframe `datExpr` of FPKM values and want to filter out genes with less than 5 FPKM in 60% of your samples, the function would be as follows:
```
df_filtered -> filter_low_genes(datExpr, min_expr = 5, 
                                 percent_cutoff = 90, 
                                 metric = "FPKM")
```

`df_filtered` will contain your new dataframe, and a message in the console will say:

```
ATTENTION: A total of 678 genes were removed. This was based 
on your parameters that stated a minimum of 5 FPKM must be in at 
least 90% (≥9) of the 10 samples provided.
```

## Creator
Brandon Polzin (2023)
