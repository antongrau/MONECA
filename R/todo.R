# ## Todo
# 
# # Få den fucking plot funktion til at virke igen - vi skal til sna pakken
# library(igraph)
# library(MONECA)
# library(soc.elite)
# 
# # devtools::install_github("gaborcsardi/pkgconfig")
# # devtools::install_github("igraph/rigraph")
# 
# # library(devtools)
# #
# # devtools::install_github("gaborcsardi/pkgconfig")
# # devtools::install_github("igraph/rigraph")
# 
# 
# data(occupations)
# 
# gg.moneca(mob.seg, show.text = F)
# 
# wm <- weight.matrix(seg$mat.list[[1]])
# 
# seg    <- anton(mob.mat)
# 
# segmenter <- seg
# edges <- segment.edges(seg, segment.reduction = 0)
# 
# 
# attraction=c(30, 20, 15, 10, 5)
# attraction=c(320, 40, 10, 4, 2)
# attraction
# lay <- layout.matrix(seg, attraction = attraction, weight.adjustment = 1, start.temp = 20, niter = 10000, tie.adjustment = 0.4)
# 
# gg.moneca (seg, layout = lay, show.text = F, edges = log(edges+1), edge.color = "black", edge.size = 0.2, border.padding = 0.8)
# 
# # Først en edge matrice:
# 
# edges <- segment.edges(seg)
# 
# graph <- graph.adjacency(edges, mode="directed", weighted=TRUE, diag=NULL)
# 
# ## ---
# attraction=c(250, 100, 15, 5, 3)
# area.size=200000
# niveau=seq(segmenter$segment.list)
# mode="directed"
# weight.adjustment = 2
# 
# seg              <- segmenter
# seg$segment.list <- segmenter$segment.list[niveau]
# seg$mat.list     <- segmenter$mat.list[niveau]
# 
# #   mx              <- segmenter$mat.list[[1]]
# #   l               <- nrow(mx)
# #   mx.1_exp        <- as.array(mx[,l]) %*% t(as.array(mx[l,]) / mx[l,l])
# #   mx.1_net        <- mx/mx.1_exp
# #   mx.1            <- mx.1_net[-l,-l]
# #   mx.attract      <- mx.1
# #
# 
# wm              <- weight.matrix(segmenter$mat.list[[1]], cut.off=0, diagonal=TRUE, symmetric=FALSE)
# mx.attract      <- wm
# gra.lay         <- graph.adjacency(mx.attract, mode="directed", weighted=TRUE, diag=NULL)
# 
# 
# assign.attraction <- function(mx.attract, segment, attract){
#   for (i in 1:length(segment)) mx.attract[segment[[i]],segment[[i]]] <- attract
#   return(mx.attract)
# }
# 
# for (i in length(seg$segment.list):2){
#   segment       <- seg$segment.list[[i]]
#   mx.attract    <- assign.attraction(mx.attract, segment, attraction[i-1])
# }
# 
# diag(mx.attract) <- 0
# gra.lay          <- graph.adjacency(mx.attract, mode=mode, weighted=TRUE, diag=NULL)
# 
# 
# a                <- rowSums(wm)
# b                <- colSums(wm)
# start            <- cbind(max(a) - a, max(b)- b)
# start            <- norm_coords(start, xmin = -100, xmax = 100, ymin = -100, ymax = 100)
# 
# layout           <- layout_with_fr(gra.lay,coords = start, weights=E(gra.lay)$weight*weight.adjustment, niter = 1000)
# layout[, 1:2]    <- norm_coords(layout[, 1:2], xmin = 1, xmax = 10^10, ymin = 1, ymax = 10^10)
# 
# 
# 
# # # # TODO
# # # # Mode er usystematisk implementeret i anton, jonas og find.segment : husk at weight matrix også er inde over
# # #
# # # #
# # # Ego kort
# # # Vi har ikke styr på rækker og kolonner her, så vi ved ikke hvad der sender og modtager
# #
# # library(MONECA)
# # data(occupations)
# # mxa.b <- mob.mat
# # segmenter <- anton(mxa.b, segment.levels = 3)
# # ego.plot(segmenter, mob.mat, id = 5, edge.color = "green")
# #
# # # add.table.to.plot <- function()
# #
# # # Annotate en tabel på
# # #   sum.stat  <- c("Beskæftigede" = as.numeric(stor.beskæftigede[id]),
# # #                  "Andel af alle beskæftigede %" = round(as.numeric(stor.beskæftigede[id]/sum(stor.beskæftigede)), 3),
# # #                           "Intern mobilitet %" = round(as.numeric(intern.mobilitet[id]), 2),
# # #                           "Organisationsgrad 2011 %" = round(organiserede.andel[id, ncol(organiserede.andel)], 2))
# #
# # # + annotate("text", x = Inf, y = -Inf, color = "black", vjust = -0.5, hjust = 1 ,label = sum.stat)