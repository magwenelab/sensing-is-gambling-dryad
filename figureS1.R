source("load-libraries.R")
source("functions.R")

gen <- c("URA3",
         "ura3",
         "Z4EVpr:URA3",
         "Z4EVpr:Ub-R-URA3",
         "Z4EVpr:Ub-Q-URA3",
         "Z4EVpr:Ub-I-URA3")

foa <- read.csv("2017-Maxwell-Magwene-when-sensing-is-gambling-figS1a.csv") %>%
    transform(ura3 = factor(ura3, levels=gen))
ura <- read.csv( "2017-Maxwell-Magwene-when-sensing-is-gambling-figS1b.csv") %>% 
    transform(ura3 = factor(ura3, levels=gen))



p <- ggplot(foa,
       aes(x=X5FOA, y=log2(value), col=ura3, group=paste(ura3, strain)))+
    facet_wrap(~estradiol)+
    geom_line()+geom_point()+theme_clean()+
    scale_color_manual("", values=c("black", "grey50", "blue",
                                    "purple", "darkorange", "red"))+
    xlab("[5-FOA] (mg/ml)")+
    ylab("log2 OD600")+
    ggtitle("Growth in SC + 50mg/L uracil")

q <- ggplot(ura,
       aes(x=estradiol, y=log2(value), col=ura3, group=paste(ura3, strain)))+
    geom_line()+geom_point()+theme_clean()+
    scale_x_log10()+
    annotation_logticks(sides="b")+
    scale_color_manual("", values=c("black", "grey50", "blue",
                                    "purple", "darkorange", "red"))+
    xlab("[estradiol] (ul/ml)")+
    ylab("log2 OD600")+
    ggtitle("Growth in SC-ura")


pdf("figS1.pdf", useDingbats=FALSE)
grid.arrange(p,q, nrow=2)
dev.off()
