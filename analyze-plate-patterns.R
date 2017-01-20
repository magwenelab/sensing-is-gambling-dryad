source("load-libraries.R")

im.m <- read.csv("2017-Maxwell-Magwene-when-sensing-is-gambling-fig6.csv")

# Smooth with a median filter
im.m.sm <- ddply(im.m, .(strain, media, replicate),
                 plyr::summarize,
                 sm=runmed(intensity, 51),
                 original = intensity,
                 x = pixels_from_center)

write.csv(im.m.sm, "2017-Maxwell-Magwene-when-sensing-is-gambling-smoothed-plate-patterns.csv")
