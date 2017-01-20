source("functions.R")
source("load-libraries.R")


pheno <- read.csv("cropped-scans/patterns-of-growth-metadata.csv", as.is=T, header=T)
pheno$strain = paste("CMY", pheno$strain, sep="")

readFile = splat( function(file, ...) readTIFF(file.path("cropped-scans", file), as.is=T))
im <- daply(pheno, .(file), readFile)
im = 2^16 - im
# Find the background intensity level
im.m.tmp <- melt(im)
im.m.tmp <- merge(im.m.tmp, pheno)
im.m.tmp$noGrow = with(im.m.tmp,
    ((media == "foa") & (strain == "CMY136")) |
    ((media == "ura") & (strain == "CMY111")))

# Subtract out background
im = im - quantile(im.m.tmp$value[im.m.tmp$noGrow], 0.99)
im[im < 0] = 0
im.means <- aaply(im, c(1,3), max)
im.m <- melt(im.means)
im.m <- merge(im.m, pheno)
im.m <- im.m[ order(im.m$file, im.m$Var2 ),]

colnames(im.m)[colnames(im.m) == "Var2"] <- "pixels_from_center"
colnames(im.m)[colnames(im.m) == "value"] <- "intensity"


write.csv(im.m,"2017-Maxwell-Magwene-when-sensing-is-gambling-fig6.csv")
