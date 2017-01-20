source("load-libraries.R")
source("functions.R")


sem <- function(x){
    x <- x[!is.na(x)]
    return( sd(x)/sqrt(length(x)))}


tradeOffsCast <- read.csv("2017-Maxwell-Magwene-when-sensing-is-gambling-fig3.csv")

tradeOffsCast2 <- mutate(tradeOffsCast,
                         doublings=log2(post/pre)) %>%
  dcast(rep + estradiol + strain + ura3 ~ media,
        value.var="post")

tradeOffsCastMeans = ddply(tradeOffsCast2,
      .(estradiol, strain, ura3),
      plyr::summarize,
      foaSEM = sem(`SC+5FOA`),
      uraSEM = sem(`SC-ura`),
      `SC-ura`=mean(`SC-ura`),
      `SC+5FOA`=mean(`SC+5FOA`)) %>%
    mutate(position= round(log2(estradiol)))

fig3 <- transform(tradeOffsCastMeans,strain = factor(as.character(strain),
                              levels=c("CMY138", "CMY97", "PMY1639"),
                              labels=c("P2", "plastic", "P1"))) %>% 
    ggplot(aes(x=SC.5FOA, y=SC.ura,
               group=estradiol, col=strain,
               pch=strain,
               label=position))+
    geom_errorbar(aes(ymax=SC.ura + uraSEM,
                      ymin=SC.ura - uraSEM), lty=1)+
    geom_errorbarh(aes(xmax=SC.5FOA + foaSEM,
                       xmin=SC.5FOA - foaSEM), lty=1)+
    geom_point()+
    scale_color_manual("",values=c("darkorange", "black", "blue"))+
    scale_shape_manual("",values=c(1,16,1))+
    theme_clean()+
    xlab("Growth in E2 (cells/nl)")+
    ylab("Growth in E1 (cells/nl)")

ggsave("fig3.pdf", fig3, width=5, height=4)

