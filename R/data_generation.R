# Data generation

# mob.mat      <- read.csv("~/My Dropbox/DINECA/Data/Moneca.csv", row.names = 1, header = TRUE, sep = ';', fileEncoding  ="UTF-8", check.names = FALSE)
# mob.mat      <- as.matrix(mob.mat)
# l            <- ncol(mob.mat) 
# label        <- strtrim(rownames(mob.mat), 40)
# label.kode   <- read.csv("~/My Dropbox/DINECA/Data/Oversat Moneca kategorier.csv", sep = ";", fileEncoding = "latin1")
# label        <- paste(label.kode$DISCO, label.kode$Dansk.Moneca.label, sep = ": ")
# dimnames(mob.mat) <- list(label, label)
# save(mob.mat, file = "~/MONECA/data/occupations.rda")

#' Occupational mobility
#' 
#' @name occupations
#' @docType data
NULL
