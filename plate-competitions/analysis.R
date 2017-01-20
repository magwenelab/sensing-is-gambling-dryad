library(ggplot2); library(magrittr); library(plyr); library(reshape2); library(wesanderson)

codeDir <- file.path(Sys.getenv("CM_GIT"), "rcolin")
source(file.path(codeDir, "theme_clean.R"))
source(file.path(codeDir, "budscarCountUtilities.R"))

readData <- function(datFile, gateFile, phenoFile, ...){
    dat <- read.csv(datFile, as.is=T)
    gatePheno <- read.csv(gateFile, as.is=T)
    pheno <- read.csv(phenoFile, as.is=T)
    dat <- merge(dat, pheno, by = "file")
    dat <- merge(dat, gatePheno, by="Path")
    dat <- transform(dat, percent_cells = as.numeric(X...))
    dat
}

metaData = read.csv("metadata.csv", as.is=T)
colnames(metaData)[colnames(metaData) == "file"] = "phenoFile"
metaData[["datFile"]] = gsub( "_pheno", "", metaData[["phenoFile"]])
metaData[["gateFile"]] = "gate_names.csv"

theData = ddply(metaData, .(replicate, epoch), splat(readData))

firstEpoch = subset(theData, is.na(treatment))

theData2 =
  rbind(
      subset(theData, !is.na(treatment)),
      transform(firstEpoch, treatment = "FOA"),
      transform(firstEpoch, treatment = "ura"))

theData.c <- subset(theData2, name %in% c("GFP", "mCherry", "unstained")) %>%
  dcast(replicate + epoch + pool + treatment ~ name, value.var="percent_cells") %>%
  transform(total_GFP = GFP + unstained)

theData.m <- subset(theData.c, select = -GFP) %>%
  melt(id.vars=c("replicate", "epoch", "pool", "treatment" ))


library(colortools)

mean_cl_sem <- function(x){
    x <- x[!is.na(x)]
    data.frame( y = mean(x),
               ymax = mean(x) + sd(x)/sqrt(length(x)),
               ymin = mean(x) - sd(x)/sqrt(length(x))
               )
}




pdf("competitions_plot.pdf", height=2, width=3)
theData.c %>%
    subset(!(replicate %in% c("pilot", "pilot2"))) %>% 
    ggplot(aes(x=epoch, y=mCherry, pch=pool,lty=pool, group=pool))+
    facet_grid(treatment~.)+
    stat_summary(fun.y="mean", col="grey25", geom="line")+
    stat_summary(fun.y="mean", geom="point", size=1)+
    stat_summary(fun.data="mean_cl_sem", lty=1, geom="linerange")+
    theme_clean()+
    scale_shape_manual(values=c(1,2,5))
dev.off()
