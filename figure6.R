source("functions.R")
source("load-libraries.R")

im.m.sm <- read.csv("2017-Maxwell-Magwene-when-sensing-is-gambling-smoothed-plate-patterns.csv")

## Calculate confidence intervals
im.m.sm2 <- ddply(im.m.sm, .(strain, media, x),
                  plyr::summarize, y = mean(sm),
                  sd = sd(sm), n = length(sm),
                  ymin = y-sd/sqrt(n),
                  ymax = y+sd/sqrt(n))


imForPlot <- im.m.sm2 %>% # crop off edges of plate
    subset( x > 50) %>%
    subset( x < 494) %>%
    transform(strain2 = factor(as.character(strain),
                               levels=c("CMY111", "CMY115", "CMY121", "CMY136"),
                               labels=c("P1", "D/D", "WT", "P2")))

fig6 <- ggplot(imForPlot, aes(x=(x/360)*2.54, # convert pixels to cm
             y=y,ymax=ymax, ymin=ymin,
             col=strain2,
             group=paste(strain2)))+
    facet_grid(media~.)+
    geom_ribbon(col="grey80", alpha=0.5, fill="grey80",lty=1)+
    geom_line(size=0.3)+    
    theme_bw()+
    scale_x_continuous("Distance from spot (cm)")+
    ylab("Max intensity")+
    scale_color_manual("",values=c("blue", "darkred", "black", "darkorange"))

ggsave("fig6.pdf", fig6, height=3, width=5)

