
library(devtools)
install_github("genomicsclass/GSE5859Subset")

library(GSE5859Subset)
data(GSE5859Subset)

head(geneExpression)

dim(geneExpression)


head(sampleInfo)
dim(sampleInfo)

identical(colnames(geneExpression), sampleInfo$filename)
#abbiamo comparato i nomi del file geneespression con il nome delle colonne 
#del file filenema (il risultato è stato TRUE)

