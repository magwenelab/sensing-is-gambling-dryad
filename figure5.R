
source("load-libraries.R")
source("functions.R")

cop2_plastic <- read.csv("2017-Maxwell-Magwene-when-sensing-is-gambling-fitness-calculations.csv") %>%
    transform(media2=factor(as.character(media2),
                            levels=c("SC-ura, with estradiol",
                                     "SC+5FOA, without estradiol")))


fig5 <- ggplot(cop2_plastic,
       aes(y=sT, x=group_name))+
    stat_summary(fun.data = "mean_cl_sem",pch=2)+
    facet_grid(.~media2)+
    theme_bw()+xlab("")+
    scale_shape_manual(values=c(1,2))


ggsave("fig5.pdf", fig5, width = 8, height=4)
