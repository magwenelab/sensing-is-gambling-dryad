source("load-libraries.R")
source("functions.R")

metaData <- read.csv("fig3-data/pheno.csv", as.is=T) %>%
    transform(thePheno = file.path("fig3-data/", thePheno),
              theData = file.path("fig3-data/", theData))

readSanitizedMacsQuant <-
    function( mD , gates){
        macsData = read.csv(mD, as.is=T) %>%
          subset( Path %in% gates) %>%
          subset( select = c("Path", "Count", "Count.mL", "file"))
        filesPerGate <- ddply(macsData,
                              .(Path, file),
                              function(x) c(nfiles = length(x$file)))
        if( any( filesPerGate$nfile > 1) ){
            stop( "More than one file per gate. Examine data file")
        }
        macsData
    }

readAndMerge <-
    function( theData, thePheno, ...){
        mD <- readSanitizedMacsQuant( theData, "P1\\P2")
        pD <- read.csv(thePheno, as.is=T)
        merge( mD, pD, by="file")
    }

tradeOffs <- ddply( metaData, .(theData, thePheno, rep, selection),
                   splat(readAndMerge))
colnames(tradeOffs) <- tolower( colnames(tradeOffs))
tradeOffs$estradiol <- round(tradeOffs$estradiol, 7) # This fixes a bug from excel that led to slightly different values for the estradiol concentration

tradeOffsCast <- transform(tradeOffs,
                           count.nl = (count.ml/1e6)*50) %>% # Measured
                                                             # at 1:50
                                                             # dilution
    dcast( rep + media + estradiol + strain + ura3 ~ selection,
          value.var="count.nl")


write.csv(tradeOffsCast,
          "2017-Maxwell-Magwene-when-sensing-is-gambling-fig3.csv")
