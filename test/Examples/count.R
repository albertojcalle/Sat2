{
library(data.table)
setwd("/home/alberto/github/2sat/test/Examples/SatGenerator/")


dtAnalisis <- data.table(vars = NA)
for(file in dir("SAT")){
  vars <- unlist(strsplit(file, split = "-"))[3]
  vars <- gsub("l", "" ,vars)
  dtAnalisis <- rbind(dtAnalisis, data.table(vars = vars))
}
dtAnalisis <- dtAnalisis[, sats := .N, by = vars][!is.na(vars)]
dtAnalisis <- unique(dtAnalisis, by = "vars")
  
dtAnalisis2 <- data.table(vars = NA)
for(file in dir("UNSAT")){
  vars <- unlist(strsplit(file, split = "-"))[3]
  vars <- gsub("l", "" ,vars)
  dtAnalisis2 <- rbind(dtAnalisis2, data.table(vars = vars))
}
dtAnalisis2 <- dtAnalisis2[, unsats := .N, by = vars][!is.na(vars)]
dtAnalisis2 <- unique(dtAnalisis2, by = "vars")


dtResultado <- merge (dtAnalisis, dtAnalisis2, by = "vars")

dtResultado[, ratio := sats/(unsats+sats)]
setorder(dtResultado, vars)
library(ggplot2)
plot(dtResultado$vars, dtResultado$ratio)
}
