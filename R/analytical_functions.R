###################################

#' Jonas
#' 
#' An algorithm for creating discrete groups on the basis of a weighted network and a mobility table
#' 
#' @param mat is the mobility table with row and column margins XXXX Er det her sandt? Eller spiser den en relative risk matrice? .
#' @param kliker is a list of cliques
#' @param cut.off is the minimum weight or relative risk accepted as a network tie
#' @param mode defines whether mat is symmetric. If mode is "Mutual" - ties are only created for mutual relations. If mode is "Unmutual", relations are not required to be mutual to result in a tie.
#' @param delete.upper.tri defines whether the upper triangle of the matrix is to be deleted. This results in speed gains.
#' @return membership - a numeric vector with discrete group memberships for each row in the matrix.
#' @return cliques a list of row indices for each clique
#' @export

jonas <- function(mat, kliker, cut.off=1, mode="symmetric", delete.upper.tri=TRUE){
  
  ##################
  # Matrice modificering
  
  if(identical(mode, "Mutual")){
    mat[mat < cut.off]   <- NA
    mat                  <- mat + t(mat)
  }
  
  if(identical(mode, "Unmutual")){
    mat[mat < cut.off]  <- 0
    mat                 <- mat + t(mat)
    mat[mat==0]         <- NA
  }
  
  if(identical(delete.upper.tri, TRUE)) {
    mat[upper.tri(mat)] <- NA
  }
    
  # Her defineres output vectoren:
  gruppe               <- vector(mode="numeric",length=nrow(mat))
  names(gruppe)        <- rownames(mat)
  # Her laves den matrice der skal "slettes i"
  max.mat              <- mat
  
  ############################
  # Her defineres hjælpefunktionen
  bejler.test           <- function(kliker, potentiel.klike){
    klike.match         <- vector(length=length(kliker))
    for (j in 1:length(kliker)){
      kl                <- kliker[[j]]
      klike.match[[j]]  <- all(potentiel.klike %in% kl)
    }
    any(klike.match)
  }
  
  #############################################################
  # Progress bar
  loop.length           <-  sum(mat > cut.off, na.rm=TRUE)
  pb                    <- txtProgressBar(min = 1, max = loop.length, style=3)
  for (i in 1:loop.length){
    
    setTxtProgressBar(pb, i, label=paste(round(i/loop.length*100, 0), "% ready!"))
    ###########################################################
    
    max.ind             <- which(max.mat==max(max.mat, na.rm=TRUE), arr.ind=TRUE)[1,]
    #gruppe[max.ind]  <- i
    max.mat[max.ind[1], max.ind[2]] <- NA
    
    gruppe.tilbejles    <- gruppe[max.ind] # Hvilken gruppe bejler den til?
    bejler              <- max.ind 
    
    ### Find den tilbejlede gruppe
    
    if(sum(gruppe.tilbejles)==0){
      gruppe[max.ind] <- i  
    }
    if(sum(gruppe.tilbejles)!=0){
      
      gruppe.tilbejles <- gruppe.tilbejles[gruppe.tilbejles!=0] 
      gruppe.medlemmer <- which(gruppe %in% gruppe.tilbejles)   # Det her skal ske før vi ved med sikkerhed hvilken af grupperne der er den rigtige
      gruppe.stoerrelse <- table(gruppe[gruppe.medlemmer])       # Her findes den største af grupperne
      gruppe.tildeles  <- as.numeric(names(gruppe.stoerrelse))[which.max(gruppe.stoerrelse)[1]]        # Her skal den finde den største af grupperne - Den tager det første element - så hvis der er to der er lige store så vælger den "tilfældigt" den største - som også vil være den mest cohesive???
      
      #### Test kliker
      potentiel.klike  <- unique(sort(c(gruppe.medlemmer, bejler)))
      
      test <- bejler.test(kliker, potentiel.klike)              # Her tester vi om den potentielle kliker er en faktisk klike
      if (test==TRUE){
        gruppe[potentiel.klike] <- gruppe.tildeles             
      }
    }
  }
  sub <- gruppe[gruppe==0] 
  gruppe[gruppe==0] <- 1:length(sub) + max(gruppe)
  g <- as.factor(gruppe) 
  levels(g) <- 1:nlevels(g)
  
  # Her laver vi de nye kliker
  l        <- levels(g)
  ud.list  <- list()
  for ( i in 1:length(l)) ud.list[[i]]<- which(g == l[i])
  
  out <- list()
  out$membership   <- g
  out$cliques      <- ud.list
  return(out)
}

#' Anton
#'
#' Creating nested segments using the \link{jonas} function.
#'
#' The anton function is the function that controls Jonas and makes it do all the hard work. 
#' @param mx is a raw mobility table with row and column margins
#' @param segment.levels defines the number levels of nested segments to be returned
#' @param cut.off is the minimum weight or relative risk accepted as a network tie
#' @param mode defines whether mat is symmetric. If mode is "Mutual" - ties are only created for mutual relations. If mode is "Unmutual", relations are not required to be mutual to result in a tie.
#' @param delete.upper.tri defines whether the upper triangle of the matrix is to be deleted. This results in speed gains.
#' @return segment.list returns a list of cliques for each level. The row indices correspond to the first level aka. the rows from mx.
#' @return mat.list returns a list with a mobility table for each level with the number of rows and columns corresponding to the number of segments for each level
#' @seealso \link{jonas}, \link{jonas.plot}
#' @export

anton <- function(mx=mx, segment.levels=3, cut.off=1, mode="symmetric", delete.upper.tri=TRUE, small.cell.reduction=0){
  
  # Find segmenterne på baggrund af en matrice
  # Det er her jonas options skal angives  
  find.segments   <- function(mx, cut.off=1, mode=mode, delete.upper.tri=delete.upper.tri, small.cell.reduction=small.cell.reduction){
    
#     l               <- nrow(mx)
#     mx.1_exp        <- as.array(mx[,l]) %*% t(as.array(mx[l,]) / mx[l,l])
#     mx.1_net        <- mx/mx.1_exp
#     mx.1            <- mx.1_net[-l,-l]
#     mx.1i           <- as.matrix(mx.1)
#     mx.1i[mx.1i < cut.off]  <- NA                # Her er cutoff pointet - det skal op i options senere
#     mx.1i           <- mx.1i + t(mx.1i)
#     diag(mx.1i)     <- NA

    mx.1i           <- weight.matrix(mx, cut.off, small.cell.reduction=small.cell.reduction)
    
    gra.1ii         <- graph.adjacency(adjmatrix=mx.1i, mode="undirected")
    klike           <- cliques(gra.1ii)
    clust.1         <- jonas(mx.1i, klike, cut.off=cut.off)
    
    return(clust.1)
  }
  
  segment.matrix  <- function(mx, segments){
    
    grupper.1       <- c(segments$membership, length(segments$membership)+1)
    mx.2_r          <- rowsum(mx, grupper.1)
    mx.2_r_t        <- t(mx.2_r)
    mx.2_rc_t       <- rowsum(mx.2_r_t, grupper.1)
    mx.2g           <- t(mx.2_rc_t)
    return(mx.2g)
  }
  
  level.down <- function(niv.nu, niv.ned){
    # nak isolates
    a               <- unlist(lapply(niv.nu, length))
    niv.nu          <- niv.nu[a>1]
    
    ud <- list()
    
    for(i in 1:length(niv.nu)){
      d                 <- niv.nu[[i]]
      ud[[i]]           <- unlist(niv.ned[d])
    }
    
    return(ud)
  }
  
  create.segments <- function(out.put, mx){
    
    seg.list        <- list()
    seg.list[[1]]   <- as.list(1:(nrow(mx)-1))
    
    niv.nu          <- out.put[[1]]$segments$cliques
    # nak isolates
    a               <- unlist(lapply(niv.nu, length))
    seg.list[[2]]   <- niv.nu[a>1]
    
    for (n in 2:segment.levels){
      
      nu  <- n
      ned <- n
      
      niv.nu     <- out.put[[nu]]$segments$cliques
      niv.ned    <- out.put[[n-1]]$segments$cliques
      
      for (i in 1:(n-1)){
        ned <- ned-1
        niv.ned <- out.put[[ned]]$segments$cliques
        niv.nu  <- level.down(niv.nu, niv.ned)  
      }
      seg.list[[n+1]] <- niv.nu
    }
    return(seg.list)
  }
  
  
  # Her finder vi segmenterne
  mat.list        <- list()
  mat.list[[1]]   <- mx
  segments        <- find.segments(mx, cut.off=cut.off, mode=mode, delete.upper.tri=delete.upper.tri, small.cell.reduction=small.cell.reduction)
  mx.2g           <- segment.matrix(mx, segments)
  mat.list[[2]]   <- mx.2g
  out.put         <- list()
  out.put[[1]]    <- list(segments=segments, mat=mx.2g)
  
  for (i in 2:segment.levels){
    segments        <- find.segments(mx.2g, cut.off=cut.off, mode=mode, delete.upper.tri=delete.upper.tri, small.cell.reduction=small.cell.reduction)
    mx.2g           <- segment.matrix(mx.2g, segments)
    mat.list[[i+1]] <- mx.2g
    out.put[[i]]    <- list(segments=segments, mat=mx.2g)
  }
  
  
  # Her laves segmenterne
  segment.list    <- create.segments(out.put, mx)
  
  # Her laves output
  
  out <- list(segment.list=segment.list, mat.list=mat.list, small.cell.reduction=small.cell.reduction)
  
  return(out)
}

#############################################
#' Weight.matrix
#' 
#' Creates a matrix of weights for the jonas function.
#' @param mx 
#' @param cut.off is the minimum weight allowed for a tie between nodes
#' @param symmetric defines whether the matrix is forced into symmetry
#' @return a matrix of relative risks
#' @export

weight.matrix <- function(mx, cut.off = 1, symmetric = TRUE, diagonal = NULL, small.cell.reduction = 0){

  l               <- nrow(mx)
  o.r.s           <- mx[-l, l]
  o.c.s           <- mx[l, -l]
  total.total     <- mx[l,l]
  row.share       <- o.r.s/total.total
  col.share       <- o.c.s/total.total
  total.mobility  <- sum(mx[-l,-l])
#   r.s             <- rowSums(mx[-l, -l])
#   c.s             <- colSums(mx[-l, -l])
#   mx.1_exp        <- o.r.s %*% t(o.c.s)/sum(o.r.s)    # Unweighted Relative Risks
  mx.1_exp        <- row.share %*% t(col.share)*total.mobility
  mx.red          <- mx[-l,-l]
  mx.red[mx.red < small.cell.reduction] <- 0
  mx.1_net        <- mx.red/mx.1_exp
  mx.1            <- mx.1_net
  mx.1i           <- as.matrix(mx.1)
  mx.1i[mx.1i < cut.off]  <- NA               
  if (identical(symmetric, TRUE))    mx.1i           <- mx.1i + t(mx.1i)
  if(is.null(diagonal)) diag(mx.1i)     <- NA
  
  return(mx.1i)  
}




#########################################################################
# Plotting

#' Segment colors
#' 
#' Creates a grey scale for the segments
#' 
#' @param segmenter
#' @export

segment.colors <- function(segmenter){
  n.segments <- length(segmenter$segment.list)
  
  
  segment.grey  <- function(segmenter, niveau){
    
    mat               <- segmenter$mat.list[[niveau]]
    l.seg             <- length(segmenter$segment.list[[niveau]])
    diag.mat          <- diag(mat)[-nrow(mat)]
    row.mat           <- mat[-nrow(mat), nrow(mat)]
    value             <- round((1-diag.mat/row.mat)*100)
    color             <- paste("grey", value, sep="")
    return(color)
  }
  
  colors <- list() 
  for (i in 1:n.segments) colors[[i]] <- segment.grey(segmenter, i)
  
  groups <- unlist(lapply(segmenter$segment.list, length))
  
  for ( i in 1:length(groups)) colors[[i]]  <-  colors[[i]][1:groups[i]]
  
  return(colors)
}


########################################################################
# 

#' Layout matrix
#' 
#' A matrix with the coordinates of the segments
#' @param segmenter a segment object
#' @param attraction the distance between the segment points for each level.
#' @param area.size the size of the plot area - see \link{layout.fruchterman.reingold}
#' @param niveau the included levels
#' @param mode the mode
#' @export
# old attraction attraction=c(200, 100, 15, 5, 3)
layout.matrix <- function(segmenter, attraction=c(320, 40, 10, 4, 2), niveau=seq(segmenter$segment.list), mode = "directed", weight.adjustment = 1, start.temp = 20, niter = 10000, tie.adjustment = 0.4, ...){
  
  seg              <- segmenter
  seg$segment.list <- segmenter$segment.list[niveau]
  seg$mat.list     <- segmenter$mat.list[niveau]
  
#   mx              <- segmenter$mat.list[[1]]
#   l               <- nrow(mx)
#   mx.1_exp        <- as.array(mx[,l]) %*% t(as.array(mx[l,]) / mx[l,l])
#   mx.1_net        <- mx/mx.1_exp
#   mx.1            <- mx.1_net[-l,-l]
#   mx.attract      <- mx.1
#   
  mx.attract      <- weight.matrix(segmenter$mat.list[[1]], cut.off = 0, diagonal=TRUE, symmetric=FALSE)
  mx.attract      <- mx.attract ^ tie.adjustment
  
  gra.lay         <- graph.adjacency(mx.attract, mode="directed", weighted=TRUE, diag=NULL)
  
  
  assign.attraction <- function(mx.attract, segment, attract){
    for (i in 1:length(segment)) mx.attract[segment[[i]],segment[[i]]] <- attract
    return(mx.attract)
  }
  
  for (i in length(seg$segment.list):2){
    segment       <- seg$segment.list[[i]]
    mx.attract    <- assign.attraction(mx.attract, segment, attraction[i-1])
  }
  
  diag(mx.attract) <- 0
  gra.lay          <- graph.adjacency(mx.attract, mode=mode, weighted=TRUE, diag=NULL)

  # wm               <- weight.matrix(segmenter)
  # a                <- rowSums(wm)
  # b                <- colSums(wm)
  # start            <- cbind(max(a) - a, max(b)- b)
  # start            <- norm_coords(start, xmin = -100, xmax = 100, ymin = -100, ymax = 100)
  # 
  layout           <- layout_with_fr(gra.lay, weights=E(gra.lay)$weight*weight.adjustment, niter = niter, start.temp = start.temp, ...)
  layout[, 1:2]    <- norm_coords(layout[, 1:2], xmin = 1, xmax = 10^10, ymin = 1, ymax = 10^10)
  layout
}

#' Segment edges
#' 
#' The coordinates of the edges
#' @export
segment.edges <- function(segmenter, cut.off=1, mode="directed", niveau=seq(segmenter$segment.list), segment.reduction=seq(segmenter$segment.list), method="all", top=3, diagonal=NULL, small.cell.reduction=0){
  
  mx                     <- segmenter$mat.list[[1]]
  seg                    <- segmenter
  seg$segment.list <- segmenter$segment.list[niveau]
  seg$mat.list     <- segmenter$mat.list[niveau]
  
#   
#   l               <- nrow(mx)
#   mx.1_exp        <- as.array(mx[,l]) %*% t(as.array(mx[l,]) / mx[l,l])
#   mx.1_net        <- mx/mx.1_exp
#   mx.1            <- mx.1_net[-l,-l]
#   mx.edges        <- mx.1
#   
#   # Cut reduction
#   mx.edges[mx.edges < cut] <- 0
#   
#   # Diagonal
#   diag(mx.edges)           <- 0
  mx.edges <- weight.matrix(mx, cut.off=cut.off, symmetric=FALSE, diagonal=diagonal, small.cell.reduction=small.cell.reduction) # Sæt tilbage
  mx.edges[is.na(mx.edges)] <- 0
    
  # Segment reduction
  if(identical(segment.reduction, 0)==FALSE){
  segments <- unlist(seg$segment.list[segment.reduction], recursive=FALSE)  
    for (i in 1:length(segments)){
      mx.edges[segments[[i]], segments[[i]]] <- 0
    }
  }
  
  if (identical(method, "top.out")){
    mx.sort <- matrix(nrow=top, ncol=ncol(mx.edges))
    mx.sort[1:top,] <- apply(mx.edges, 1, sort, decreasing=TRUE)[1:top,]
    
    for (i in 1:(nrow(mx.edges))){
      mx.edges[i,][(mx.edges[i,] %in% mx.sort[,i])==FALSE] <- 0
        }
  }
    
  if (identical(method, "top.in")){
    mx.sort <- matrix(nrow=top, ncol=ncol(mx.edges))
    mx.sort[1:top,] <- apply(mx.edges, 2, sort, decreasing=TRUE)[1:top,]
      
      for (i in 1:(nrow(mx.edges))){
        mx.edges[,i][(mx.edges[,i] %in% mx.sort[,i])==FALSE] <- 0
      }
      
  }

  
  
  
  # 80% reduction of percentage of all mobility
  # 5 strongest edges
  #gra.edges <- graph.adjacency(mx.edges, mode=mode, weighted=TRUE, diag=NULL)
  return(mx.edges)
}


#' Plot Jonas
#' 
#' Plot the results from a Jonas analysis
#' @export

jonas.plot <- function(segmenter,
                       layout             = layout.matrix(segmenter),
                       edges              = segment.edges(segmenter),
                       mode               = "directed",
                       niveau             = seq(segmenter$segment.list),
                       vertex.size        = 5,
                       vertex.frame.color = "black",
                       edge.curved        = FALSE,
                       vertex.color       = "grey50",
                       vertex.label.color = "black",
                       vertex.label.cex   = 0.5,
                       vertex.label.dist  = 0.12,
                       edge.arrow.size = 0.1,
                       mark.col        = NULL, 
                       mark.expand     = 10, 
                       border.col      = "black",
                       edge.width      = 1,
                       edge.color      = "black"){
  
  niveau                 <- niveau[niveau != 1]
  seg                    <- segmenter
  seg$segment.list       <- segmenter$segment.list[niveau]
  seg$mat.list           <- segmenter$mat.list[niveau]
  segments               <- unlist(seg$segment.list, recursive=FALSE)
  
  mat.edges              <- edges
  gra.edges              <- graph.adjacency(mat.edges, mode=mode, weighted=TRUE, diag=NULL)                     
  el                     <- get.edgelist(gra.edges, names=FALSE)
  # Edge colors
  if(is.matrix(edge.color) == TRUE){
    mat.color            <- edge.color
    mat.color[mat.edges == 0] <- NA
    e.colors             <- vector(length=nrow(el))
    for (i in 1:nrow(el)){
      e.colors[i]        <- mat.color[el[i,1], el[i,2]]
    }
    gra.edges            <- set.edge.attribute(gra.edges, "color", index=E(gra.edges), e.colors)
  }
  
  plot(gra.edges, vertex.size=vertex.size, vertex.label=V(gra.edges)$name, vertex.frame.color=vertex.frame.color, layout=layout, edge.curved=edge.curved, vertex.color=vertex.color, vertex.label.color=vertex.label.color, vertex.label.family="sans", vertex.label.cex=vertex.label.cex, vertex.label.dist=vertex.label.dist, vertex.label.degree=pi/2, edge.arrow.size=edge.arrow.size, mark.groups=segments, mark.border=border.col, mark.col=NULL, mark.expand=10, edge.width=1, edge.color=edge.color)
 }


###############################################################################
#### Segment membership

#' Segment membership
#' 
#' A dataframe with the segment membership for each category
#' @export

segment.membership <- function(segmenter, niveau=seq(segmenter$segment.list)){

org.name <- rownames(segmenter$mat.list[[1]])
org.name <- org.name[-length(org.name)]

position <- vector(length=length(org.name))
for (niv in niveau){
seg.niv <- segmenter$segment.list[[niv]]
for (i in 1:length(seg.niv)){
position[seg.niv[[i]]] <- rep(paste(niv, i, sep="."), length(seg.niv[[i]]))
}
}

out.mat <- data.frame(name=org.name, membership=position)
out.mat
}  
#   out <- list()
#   
#   for (i in niveau){
#   niv     <- niveau[i]
#   seg     <- segmenter$segment.list[[niv]]
#   node    <- unlist(seg)
#   l       <- 1:length(seg)
# 
#   member <- vector()  
#   for( u in l){
#   member  <- c(member,rep(u, length(seg[[u]])))
#  }
# 
#   out[[i]]  <- data.frame(node,member)
#  }
# 
#   return(out)
# }
#   

#' Christoph
#' 
#' Create a two-level segment-object with a forced solution
#' 
#' @export

christoph <-function(segmenter, variable){

new.seg                       <- list()
new.seg$segment.list          <- list()
new.seg$mat.list              <- list()
new.seg$small.cell.reduction  <- segmenter$small.cell.reduction

# Første niveau

new.seg$segment.list[[1]]  <- segmenter$segment.list[[1]]
new.seg$mat.list[[1]]      <- segmenter$mat.list[[1]]
mxa                        <- segmenter$mat.list[[1]]

# Andet niveau
out.list <- list()
for(i in 1:nlevels(as.factor(variable))){
  var <- as.factor(variable)
  levs <- levels(var)
  out.list[[i]]<- which(var == levs[i])  
}

new.seg$segment.list[[2]] <- out.list

grupper.1       <- c(variable, length(out.list)+1)
mx.2_r          <- rowsum(mxa, grupper.1)
mx.2_r_t        <- t(mx.2_r)
mx.2_rc_t       <- rowsum(mx.2_r_t, grupper.1)
mx.2g           <- t(mx.2_rc_t)

new.seg$mat.list[[2]] <- mx.2g

return(new.seg)
}


