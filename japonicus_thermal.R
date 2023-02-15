library(thermPerf)
library(ggplot2)

## Compute the thermal responses for Aedes Japonicus
## Data taken from https://parasitesandvectors.biomedcentral.com/articles/10.1186/s13071-018-2659-1

Path <- "/home/marta/Documentos/PHD/2023/INVASIBILITY/data/japonicus_thermal.csv"
Japonicus <- read.csv(Path)
head(Japonicus)

