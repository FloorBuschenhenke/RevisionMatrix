## bekijken van tabellen
library(tidyverse)
# matrixsessie1 <- read.csv("data/meijen_revmatrix_sessionid_1.csv")
matrixsessie2 <- read.csv("data/meijen_revmatrix_sessionid_4.csv")
options(scipen=999)
#kennelijk is seq een getal weer
view(matrixsessie2)

ga_metSeq_sessie2 <- read.csv("data/meijen_GA_sessionid_4.csv")
view(ga_metSeq_sessie2)

matrixsessie1 <- read.csv("data/meijen_revmatrix_sessionid_1.csv")
view(matrixsessie1)
matrixsessie3 <- read.csv("data/meijen_revmatrix_sessionid_7.csv")
view(matrixsessie3)
matrixsessie4 <- read.csv("data/meijen_revmatrix_sessionid_8.csv")
view(matrixsessie4)
matrixsessie5 <- read.csv("data/meijen_revmatrix_sessionid_10.csv")
view(matrixsessie5)
matrixsessie6 <- read.csv("data/meijen_revmatrix_sessionid_11.csv")
view(matrixsessie6)
