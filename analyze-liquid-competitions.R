
source("load-libraries.R")
source("functions.R")


theData <- read.csv("2017-Maxwell-Magwene-when-sensing-is-gambling-fig5-and-figS2.csv")

######### Data for figure 5

costOfPlasticity <- theData %>% 
    subset(group_name != "mCherry-vs-GFP") %>% 
    subset((media == "SC-ura") & (estradiol == "100nM") |
           (media == "SC+5FOA") & (estradiol == "0nM")) %>%
    transform(estradiol2 = ifelse(estradiol == "100nM",
                                  "with estradiol", "without estradiol"))

## Function to calculate the statistics needed for sT
stat_wrapper <- function(x){
    data.frame(
        gate=x$gate,
        ## percent of each strain pre and post-selection
        p.post = x$postselection/sum(x$postselection), 
        p.pre = x$preselection/sum(x$preselection), 
        ## post-selection was measured at a dilution of 1:50
        generations = log2((50*x$postselection)/x$preselection))} 

## Calculate the statistics needed for sT
cop2 <- ddply(costOfPlasticity,
              .(replicate, media, group_name, estradiol2),
              stat_wrapper) 

cop2_plastic <- subset(cop2, gate == "mCherry") %>%
    transform(s = log(p.post/(1-p.post)-log(p.pre/(1-p.pre))),
              T = 1/generations) %>%
    transform(sT = s*T) %>% 
    transform(media2 = paste0(media,", ", estradiol2)) 

cop2_plastic[,c("replicate", "media2", "group_name",
                "s", "T", "sT")] %>%
    write.csv("2017-Maxwell-Magwene-when-sensing-is-gambling-fitness-calculations.csv")
