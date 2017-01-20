source("load-libraries.R")
source("functions.R")

theData2 <- read.csv("2017-Maxwell-Magwene-when-sensing-is-gambling-fig4.csv", as.is=T)
    

theData.c <- subset(theData2,
                    name %in% c("GFP", "mCherry", "unstained")) %>%
    dcast(replicate + epoch + pool + treatment ~ name,
          value.var="percent_cells") %>%
    transform(total_GFP = GFP + unstained)  %>%
    transform( pool = factor(as.character(pool),
                      levels=c("red vs green",
                               "plastic vs P1",
                               "plastic vs P2"))) %>%
    transform( treatment = factor(as.character(treatment),
                                  levels=c("FOA", "ura"),
                                  labels=c("SC+5FOA first",
                                           "SC-ura first")))

fig4 <- theData.c %>%
    ggplot(aes(x=epoch, y=mCherry, pch=pool, lty=pool, col=pool, group=pool))+
    facet_grid(treatment~.)+
    stat_summary(fun.y="mean", geom="line")+
    stat_summary(fun.y="mean", geom="point", size=1.5, col="black")+
    stat_summary(fun.data="mean_cl_sem", lty=1, geom="linerange")+
    theme_clean()+scale_color_manual(values=c("black", "blue", "darkorange"))+
    scale_shape_manual(values=c(1,5,2))+
    scale_linetype_manual(values=c(3, 1, 1))+
    ylab("% red plastic strain")

ggsave("fig4.pdf", fig4, height=3, width=5)
