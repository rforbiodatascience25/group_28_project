library("Biobase")
library("GEOquery")
GEO_data <- getGEO(filename='_raw/full_dataset.soft.gz')
data_table <- Table(GEO_data)





