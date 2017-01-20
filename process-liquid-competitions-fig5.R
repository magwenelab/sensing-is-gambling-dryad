source("load-libraries.R")
source("functions.R")

gatePheno <- read.csv("liquid-competitions-raw-data/gate_pheno.csv", as.is=T)

########### Process data for Rep1

dat <- read.csv("liquid-competitions-raw-data/2015-08-06_plastic_vs_constitutive_sanitized.csv", as.is=T)
pheno <- read.csv("liquid-competitions-raw-data/2015-08-16_pheno.csv", as.is=T)
dat <- merge(dat, pheno, by = "file")
dat <- merge(dat, gatePheno, by="Name") %>%
  transform(percent_cells = as.numeric(X...))
dat2 <- subset(dat, media !="sH20")
exp1.c <- subset(dat, media == "sH20") %>%
  transform( Count.mL = Count.mL/50) %>%
  merge(dat2, by = c("group", "gate", "estradiol", "group_name")) %>%
  plyr::summarize(gate=gate, media=media.y, estradiol=estradiol,
                  group_name=group_name,
                  postselection=Count.mL.y,
                  preselection=Count.mL.x)

write.csv(exp1.c, "liquid-competitions-processed-data/liquid-competitions-rep1.csv")

########### Process data for Rep2

dat <- read.csv("liquid-competitions-raw-data/2015-08-19_postselection_sanitized.csv", as.is=T)
dat2 <- read.csv("liquid-competitions-raw-data/2015-08-18_second_compete_vs_specialists_preselection.csv", as.is=T)
pheno <- read.csv("liquid-competitions-raw-data/2015-08-19_postselection_sanitized_pheno.csv", as.is=T)
pheno2 <- read.csv("liquid-competitions-raw-data/2015-08-18_second_compete_vs_specialists_preselection_pheno.csv", as.is=T)
dat <- merge(dat, pheno, by = "file")
dat <- transform(dat, percent_cells = as.numeric(X...), gate = Name)
dat2 <- merge(dat2, pheno2, by = "file")
dat2 <- merge(dat2, gatePheno, by="Name")
dat2 <- transform(dat2, percent_cells = as.numeric(X...))
exp2 <- ldply(list( postselection = dat, preselection=dat2), function(x) x, .id="select")
exp2.c <- dcast(exp2, gate + media + estradiol + group_name ~ select, value.var="Count.mL")

########## Process data for rep3

readData <- function(datFile, gateFile, phenoFile){
    dat <- read.csv(datFile, as.is=T)
    gatePheno <- read.csv(gateFile, as.is=T)
    pheno <- read.csv(phenoFile, as.is=T)
    dat <- merge(dat, pheno, by = "file")
    dat <- merge(dat, gatePheno, by="Name")
    dat <- transform(dat, percent_cells = as.numeric(X...))
    dat
}

exp3 <- ldply(
    list(postselection = readData("liquid-competitions-raw-data/2015-08-25_postselection_epoch1_sanitized.csv",
             "liquid-competitions-raw-data/gate_pheno.csv",
             "liquid-competitions-raw-data/2015-08-25_postselection_epoch1_sanitized_pheno_cost_of_plasticity.csv"),
         preselection = readData("liquid-competitions-raw-data/2015-08-24_preselection_sanitized.csv",
             "liquid-competitions-raw-data/gate_pheno.csv",
             "liquid-competitions-raw-data/2015-08-24_preselection_sanitized_pheno.csv")),
    .id = "select"
    )

exp3.c <- dcast(exp3, gate + media + estradiol + group_name ~ select, value.var="Count.mL")

write.csv(exp3.c, "liquid-competitions-processed-data/liquid-competitions-rep3.csv")

########## collate data

data_folder <- "liquid-competitions-processed-data"

exp1.c <- read.csv(file.path(data_folder, "liquid-competitions-rep1.csv")) %>%
    transform(postselection=2*postselection) # All other post-selections
                                        # were measured at
                                        # 1:50. This one was 1:100
exp2.c <- read.csv(file.path(data_folder, "liquid-competitions-rep2.csv"))
exp3.c <- read.csv(file.path(data_folder, "liquid-competitions-rep3.csv"))

theData <- ldply(list(exp1 = exp1.c,
                               exp2 = exp2.c,
                               exp3=exp3.c), function(x) x,
                          .id = "experiment") %>% 
    subset(gate %in% c("mCherry", "GFP")) %>%
    transform(replicate = experiment,
              experiment = NULL,
              estradiol = factor(estradiol, levels=c(0,1),
                                 labels=c("0nM", "100nM"))) %>%
    subset( media %in% c("SC-ura", "SC+5FOA"))

write.csv(theData[,c("replicate",
                     "gate",
                     "media",
                     "estradiol",
                     "group_name",
                     "postselection",
                     "preselection")],
          "2017-Maxwell-Magwene-when-sensing-is-gambling-fig5-and-figS2.csv")


