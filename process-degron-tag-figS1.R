source("load-libraries.R")
source("functions.R")

getData <- function(datname, phenoname){
    dat <- read.table(datname,
                      sep="\t",
                      header=FALSE,
                      row.names=1,
                      skip=1)[,-13]
    colnames(dat) <- 1:12
    dat <- melt(as.matrix(dat), varnames=c("row", "col"))
    platemap <- read.csv(phenoname)
    merge(platemap, dat, by=c("row", "col"))
}


folder = "2015-02-21_test_UbIEk-URA3_fitness"

foa <- getData(file.path(folder, "2015-02-21_UbI_SC5FOA.asc"),
               file.path(folder, "pheno_5foa.csv")) %>%
    transform(estradiol=factor(estradiol,
                  levels=c(0,1), labels=c("- estradiol", "+ estradiol")))

write.csv(foa, "2017-Maxwell-Magwene-when-sensing-is-gambling-figS1a.csv")

ura <- getData(file.path(folder, "2015-02-21_UbI_SCnoUra.asc"),
               file.path(folder, "pheno_noUra.csv"))

write.csv(ura, "2017-Maxwell-Magwene-when-sensing-is-gambling-figS1b.csv")
