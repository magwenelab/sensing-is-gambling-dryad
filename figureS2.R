source("load-libraries.R")
source("functions.R")


theData <- read.csv("2017-Maxwell-Magwene-when-sensing-is-gambling-fig5-and-figS2.csv")

## Data for figure S2
compareTags <- theData %>%
    subset((media == "SC-ura") & (estradiol == "100nM") |
           (media == "SC+5FOA") & (estradiol == "0nM")) %>%
    subset(!((media == "SC-ura") & (group_name == "vs-ura3"))) %>%
    subset(!((media == "SC+5FOA") & (group_name == "vs-URA3"))) %>% 
    transform(estradiol2 = ifelse(estradiol == "100nM",
                                  "with estradiol", "without estradiol")) %>%
    transform(generations = log2((50*postselection)/preselection)) %>%
    transform(group_name = factor(as.character(group_name),
                                  levels=c("mCherry-vs-GFP",
                                           "vs-ura3",
                                           "vs-URA3"),
                                  labels=c("plastic",
                                           "P2",
                                           "P1")))

figS2 <- ggplot(compareTags,
       aes(y=generations,
       x=group_name,lty=gate,
       col=gate,group=gate))+
    facet_grid(~media+estradiol2, scale="free_x")+
    stat_summary(fun.data="mean_cl_sem", geom="pointrange",lty=1)+
    stat_summary(fun.y="mean", geom="line")+
    scale_color_manual(values=wes_palette("Rushmore", 5)[c(3,5)])+
    theme_clean()+
    scale_linetype_manual("", values=c(2,1))

ggsave("figS2.pdf", figS2, height=2, width=5)
