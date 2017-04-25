# Data generation

# library(MONECA)
# mob.mat      <- read.csv("~/Dropbox/DINECA/Data/Moneca.csv", row.names = 1, header = TRUE, sep = ';', fileEncoding  ="UTF-8", check.names = FALSE)
# mob.mat      <- as.matrix(mob.mat)
# l            <- ncol(mob.mat)
# label        <- strtrim(rownames(mob.mat), 40)
# label.kode   <- read.csv("~/Dropbox/DINECA/Data/Oversat Moneca kategorier.csv", sep = ";", fileEncoding = "latin1")
# label        <- paste(label.kode$DISCO, colnames(mob.mat), sep = ": ")
# dimnames(mob.mat) <- list(label, label)
# mob.seg      <- segments <- moneca(mob.mat, segment.levels = 3)
# save(mob.mat, mob.seg, file = "~/MONECA/data/occupations.rda")

#' Occupational mobility
#' 
#' @name occupations
#' @docType data
#' @examples
#' data(occupations)
#' 
#' 
NULL
