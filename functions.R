source("load-libraries.R")

theme_clean <- function (base_size = 12, base_family = "") 
{
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(strip.background = element_rect(color='grey60', fill='grey95', size=0.2),
          panel.border = element_rect(fill=NA, color='grey60', size=0.2),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.key=element_blank())
}

mean_cl_sem <- function(x){
    x <- x[!is.na(x)]
    data.frame( y = mean(x),
               ymax = mean(x) + sd(x)/sqrt(length(x)),
               ymin = mean(x) - sd(x)/sqrt(length(x)))
}
