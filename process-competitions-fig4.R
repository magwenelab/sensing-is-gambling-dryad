source("load-libraries.R")
source("functions.R")

readData <- function(datFile, gateFile, phenoFile, ...){
    dat <- read.csv(datFile, as.is=T)
    gatePheno <- read.csv(gateFile, as.is=T)
    pheno <- read.csv(phenoFile, as.is=T)
    dat <- merge(dat, pheno, by = "file")
    dat <- merge(dat, gatePheno, by="Path")
    dat <- transform(dat, percent_cells = as.numeric(X...))
    dat
}

folder = "plate-competitions"

## Read in the data
metaData = read.csv(file.path(folder, "metadata.csv"), as.is=T) %>%
    transform(file = file.path(folder, file))
colnames(metaData)[colnames(metaData) == "file"] = "phenoFile"
metaData[["datFile"]] = gsub( "_pheno", "", metaData[["phenoFile"]])
metaData[["gateFile"]] = file.path(folder,"gate_names.csv")
theData = ddply(metaData, .(replicate, epoch), splat(readData))

## The first epoch is split into the SC+5FOA and SC-ura group,
## so use the same data for each.
firstEpoch = subset(theData, is.na(treatment))

theData2 =
  rbind(
      subset(theData, !is.na(treatment)),
      transform(firstEpoch, treatment = "FOA"),
      transform(firstEpoch, treatment = "ura"))

## Write file for Dryad

theData2[,c("replicate", "epoch", "pool", "treatment", "name",
            "percent_cells", "Count.mL", "Count")] %>%
    subset(!(replicate %in% c("pilot", "pilot2"))) %>% # remove pilot data
    transform( pool = factor(as.character(pool),
                             levels=c("A", "B", "C"),
                             labels=c("red vs green",
                                      "plastic vs P2",
                                      "plastic vs P1"))) %>% 
    write.csv("2017-Maxwell-Magwene-when-sensing-is-gambling-fig4.csv")
