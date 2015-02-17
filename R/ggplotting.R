#########################################
### GGPLOTTING
# Vi skal kunne angive styrken af den interne mobilitet i farveskalaen (nok i virkeligheden alpha) for borders

# library(igraph)
# library(ggplot2)
#source("~/My Dropbox/R/Elite/soc.sna//soc.sna.R")
# layout             = layout.matrix(segmenter)
# 
# layout <- lay
# niveau             = seq(segmenter$segment.list)
# 
# edges              = log(segment.edges(segmenter)+1)
# mode               = "directed"
# 
# vertex.size        = "row.total"
# vertex.fill        = "segment"
# vertex.alpha       = 1
# vertex.color       = "black"
# vertex.shape       = 21
# 
# show.edges         = TRUE
# edge.size          = 1
# edge.alpha         = "weight"
# edge.color         = "weight"
# edge.line          = "solid"
# 
# show.text          = TRUE
# text.size          = 3
# text.color        = "black"
# text.alpha         = 1
# text.vjust         = 1.5
# 
# show.borders       = TRUE
# border.size        = 1
# border.fill        = NA
# border.color       = "black"
# border.alpha       = 1
# border.padding     = 1
# 
# legend             = "side"
# 
# border.text        = TRUE
# border.text.size   = 4
# border.text.color  = "black"
# border.text.vjust  = -0.2

#' ggplots by Jonas
#' 
#' Plotting of MONECA objects
#' 
#' @param niveau
#' @param layout
#' @param edges
#' @param mode
#' @param vertex.size
#' @param vertex.fill
#' @param vertex.alpha
#' @param vertex.color
#' @param vertex.shape
#' @param show.edges
#' @param edge.size
#' @param edge.alpha
#' @param edge.color
#' @param edge.line
#' @param show.text
#' @param text.size
#' @param text.color
#' @param text.alpha
#' @param text.vjust
#' @param show.borders
#' @param border.size
#' @param border.fill
#' @param border.color
#' @param border.alpha
#' @param border.padding
#' @param border.text
#' @param border.labels
#' @param border.text.size
#' @param border.text.color
#' @param border.text.vjust
#' @param border.text.hjust
#' @param midpoints
#' @param midpoint.arrow
#' @param legend
#' @export
gg.jonas               <- function(segmenter,
                                   niveau             = seq(segmenter$segment.list),
                                   layout             = layout.matrix(segmenter),
                                   edges              = log(segment.edges(segmenter)+1),
                                   mode               = "directed",
                                   
                                   vertex.size        = "total",
                                   vertex.fill        = "segment",
                                   vertex.alpha       = 1,
                                   vertex.color       = "black",
                                   vertex.shape       = 21,
                                   
                                   show.edges         = TRUE,            
                                   edge.size          = 1,
                                   edge.alpha         = "weight",
                                   edge.color         = "weight",
                                   edge.line          = "solid",
                                                                      
                                   show.text          = TRUE,
                                   text.size          = 3,
                                   text.color        = "black",
                                   text.alpha         = 1,
                                   text.vjust         = 1.5,
                                   
                                   show.borders       = TRUE,
                                   border.size        = 1,
                                   border.fill        = NA,
                                   border.color       = "black",
                                   border.alpha       = 1,
                                   border.padding     = 1,
                                   border.text        = TRUE,
                                   border.labels      = "segments",
                                   border.text.size   = 4,
                                   border.text.color  = "black",
                                   border.text.vjust  = -0.2,
                                   border.text.hjust  = 1,
                                   
                                   midpoints          = TRUE,
                                   midpoint.arrow      = arrow(angle = 20, length = unit(0.33, "cm"), ends = "last", type = "closed"),
                                                                      
                                   legend             = "side",
                                   ...
                                   ){

if(identical(border.labels, "segments")){
membership             <- segment.membership(segmenter, niveau = niveau)[,2]
layout                 <- data.frame(layout, membership = membership)
colnames(layout)       <- c("X", "Y", "Membership")
}

if(length(border.labels) == nrow(layout)){
  layout               <- data.frame(layout, membership = border.labels)
colnames(layout)       <- c("X", "Y", "Membership")
}
  
  
niveau                 <- niveau[niveau != 1]
seg                    <- segmenter
seg$segment.list       <- segmenter$segment.list[niveau]
seg$mat.list           <- segmenter$mat.list[niveau]
segments               <- unlist(seg$segment.list, recursive=FALSE)



mat.edges              <- edges
gra.edges              <- graph.adjacency(mat.edges, mode=mode, weighted=TRUE, diag=NULL)                     

scale_modifications    <- list()

if(identical(edge.color, "weight")){
  edge.color  <- E(gra.edges)$weight
  scale_modifications$edge.color <- scale_color_continuous(high = "darkblue", low = "azure1")
}

if(identical(edge.alpha, "weight"))   edge.alpha   <- E(gra.edges)$weight

if(identical(vertex.fill, "segment")){
  vertex.fill  <- segment.membership(segmenter, niveau = niveau)$membership
  scale_modifications$vertex.fill <- scale_fill_discrete(guide = "none")
}

if(identical(vertex.size, "total")){
  mat                 <- segmenter$mat.list[[1]]
  totals              <- (mat[nrow(mat),] + mat[, nrow(mat)]) / 2
  totals              <- totals[-length(totals)]
  vertex.size         <- totals
  scale_modifications$vertex.size <- scale_size_continuous(range = c(4,10))
} 

if(identical(vertex.size, "col.total")){
  col.total           <- data.frame(t(segmenter$mat.list[[1]]))$Total
  vertex.size         <- row.total[-length(col.total)]
  scale_modifications$vertex.size <- scale_size_continuous(range = c(4,10))
} 


p                      <- graph.plot(gra.edges, layout=layout,
                          vertex.color = vertex.color, vertex.fill = vertex.fill, vertex.shape = vertex.shape,
                          vertex.size = vertex.size, vertex.alpha = vertex.alpha,
                          edges = show.edges, edge.color = edge.color, edge.alpha = edge.alpha,
                          edge.size = edge.size, edge.line = edge.line, edge.order = FALSE,
                          text = show.text, text.size = text.size, text.colour = text.color,
                          text.alpha = text.alpha, legend = legend, text.vjust = text.vjust, midpoints = midpoints, midpoint.arrow = midpoint.arrow)

circleFun <- function(center = c(0, 0), diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

segment.circles.hull     <- function(layout, group, diameter){
x                        <- layout[group, 1:2]
membership.seg           <- unique(as.character(layout$Membership[group]))
list.of.circles          <- apply(x, 1, circleFun, diameter = diameter)
all.circle.coordinates   <- do.call(rbind, list.of.circles)
circle.hull              <- all.circle.coordinates[chull(all.circle.coordinates),]
cbind(circle.hull, group = runif(1, min = 0, max = 999999999), membership = membership.seg)
}

annotate_segments        <- function(layout, seg.list, diameter, border.alpha){
  segment.circles          <- lapply(seg.list, segment.circles.hull, layout = layout, diameter = max(layout[, 1:2]) / diameter)
  segment.circles          <- do.call(rbind, segment.circles)
  annotate(geom = "polygon", x = segment.circles$x, y=segment.circles$y, group = segment.circles$group, fill = NA, color = "black", alpha = border.alpha)
}

if(identical(show.borders, TRUE)){
list.annotate            <- lapply(seg$segment.list, annotate_segments, layout = layout, diameter = (1/border.padding) * 20, border.alpha = border.alpha)
p                        <- p + list.annotate
}

if(identical(border.text, TRUE) & length(niveau) > 0){
  
  border.padding.diameter  <- max(layout[, 1:2]) / ((1/border.padding) * 20)
  seg.circles              <- list()
  for ( i in 1:length(seg$segment.list)){
  segment.circles          <- lapply(seg$segment.list[[i]], segment.circles.hull, layout = layout, diameter =  border.padding.diameter)
  segment.circles          <- do.call(rbind, segment.circles)
  seg.circles[[i]]         <- segment.circles
  }
  segment.circles          <- do.call(rbind, seg.circles)
  max.circles              <- aggregate(segment.circles$y, by = list(segment.circles$membership), FUN = max)
  max.segments             <- segment.circles[(segment.circles$y %in% max.circles$x) & (segment.circles$membership %in% max.circles$Group.1),]
  
  max.segments$xend        <- max.segments$x + ((border.padding.diameter * 2) * (border.text.size / 3.9))
  
  list.annotate            <- list(annotate(geom = "text", x = max.segments$xend, y=max.segments$y, label = max.segments$membership, color = border.text.color, size = border.text.size, vjust = border.text.vjust, hjust = border.text.hjust),  
                                   annotate(geom = "segment", x = max.segments$x, xend = max.segments$xend , y = max.segments$y, yend = max.segments$y, color = border.color, alpha = border.alpha))
  
  
  p                        <- p + list.annotate
  
  tab.mem                  <- table(layout$Membership)
  singles.layout           <- layout[layout$Membership %in% names(tab.mem)[tab.mem == 1],]
  singles.layout           <- data.frame(x = singles.layout$X, y = singles.layout$Y, group = runif(1, min = 0, max = 999999999), membership = singles.layout$Membership)
  singles.layout$y         <- singles.layout$y + (border.padding.diameter * 0.25)
  singles.layout$xend      <- singles.layout$x + ((border.padding.diameter * 2) * (border.text.size / 3.9))
  singles.layout$x         <- singles.layout$x + (border.padding.diameter * 0.25)
  
  list.annotate            <- list(annotate(geom = "text", x = singles.layout$xend, y=singles.layout$y, label = singles.layout$membership, color = border.text.color, size = border.text.size, vjust = border.text.vjust, hjust = border.text.hjust),  
                                   annotate(geom = "segment", x = singles.layout$x, xend = singles.layout$xend , y = singles.layout$y, yend = singles.layout$y, color = border.color, alpha = border.alpha))
  
  p                        <- p + list.annotate
}


p + scale_modifications
}


##################################################
###  Graph plot
graph.plot <- function(graph, layout = layout.fruchterman.reingold(graph),
                       vertex.color = "black", vertex.fill = "grey60", vertex.shape = 21, vertex.size = 3, vertex.alpha = 1,
                       edges = TRUE, edge.color = "black", edge.alpha = 0.2, edge.size = 1, edge.line = "solid", edge.order = FALSE,
                       text = FALSE, text.size = 3, text.colour = "black", text.alpha = 1, legend = "side", text.vjust = 1.5, midpoints = FALSE,
                       midpoint.arrow = arrow(angle = 20, length = unit(0.33, "cm"), ends = "last", type = "closed")){
  
  
  
  vertex.coords           <- as.data.frame(vertex.coord(graph, layout))
  
  vertex.l                <- list(color=vertex.color, fill=vertex.fill, shape=vertex.shape, size=vertex.size, alpha=vertex.alpha)
  v.i                     <- unlist(lapply(vertex.l, length)) == 1
  vertex.attributes       <- vertex.l[v.i]
  vertex.aes              <- vertex.l[v.i==FALSE]
  vertex.aes$x            <- vertex.coords$x
  vertex.aes$y            <- vertex.coords$y
  
  
  if(identical(edges, TRUE)){
    
    edge.coords             <- edge.coord(graph, layout)
    edge.l                  <- list(color=edge.color, alpha=edge.alpha, size=edge.size, linetype=edge.line)
    e.i                     <- unlist(lapply(edge.l, length)) == 1
    edge.attributes         <- edge.l[e.i]
    edge.attributes$lineend <- "butt"
    edge.aes                <- edge.l[e.i==FALSE]
    edge.aes$x              <- edge.coords$start.x
    edge.aes$y              <- edge.coords$start.y
    edge.aes$xend           <- edge.coords$slut.x
    edge.aes$yend           <- edge.coords$slut.y
    
    if(identical(edge.order, FALSE) == FALSE){
      edge.aes              <- as.list(as.data.frame(edge.aes)[order(edge.order),])
    } 
  }
  
  if(identical(midpoints, TRUE)){
    midpoint.attributes         <- edge.attributes
    midpoint.attributes$arrow   <- midpoint.arrow 
    midpoint.aes                <- edge.aes
    midpoint.aes$x              <- (edge.coords$start.x + edge.coords$slut.x) / 2
    midpoint.aes$y              <- (edge.coords$start.y + edge.coords$slut.y) / 2
    
#     ax                          <- edge.coords$slut.x - midpoint.aes$x
#     ay                          <- edge.coords$slut.y - midpoint.aes$y
#     crazy                       <- (edge.coords$slut.y / 10000) * 0.001
#     els                         <- edge.coords$slut.y < midpoint.aes$y

# Her finder bevæger vi os 1/l hen af vectoren imod slutpunktet. x1 kan så være midpunktet.
# l = sqrt((x2 - x1)^2 + (y2 -y1)^2)
# x3 = x1 + (1/l) * (x2 - x1)
# y3 = y1 + (1/l) * (y2 - y1)

    L                            <- sqrt(((edge.coords$slut.x - midpoint.aes$x)^2) + ((edge.coords$slut.y - midpoint.aes$y)^2))
    midpoint.aes$xend            <- midpoint.aes$x + (1/L) * (edge.coords$slut.x - midpoint.aes$x)
    midpoint.aes$yend            <- midpoint.aes$y + (1/L) * (edge.coords$slut.y - midpoint.aes$y)
    #midpoint.aes$xend           <- midpoint.aes$x + ((ax / ay) * crazy)
    #midpoint.aes$yend           <- midpoint.aes$y + crazy
#    midpoint.aes$yend[els]      <- midpoint.aes$y[els] - crazy
    midpoint.aes$group          <- paste(midpoint.aes$x, midpoint.aes$y)
    
    }
  
  text.l                  <- list(size=text.size, color=text.colour, alpha=text.alpha, vjust=text.vjust, lineheight=1)
  t.i                     <- unlist(lapply(text.l, length)) == 1
  text.attributes         <- text.l[t.i]
  text.aes                <- text.l[t.i==FALSE]
  text.aes$x              <- vertex.coords$x
  text.aes$y              <- vertex.coords$y
  text.aes$label          <- rownames(vertex.coords)
  
  # Plot edges
  p <- ggplot()
  
  if(identical(edges, TRUE)){
    edge.attributes$mapping     <- do.call("aes", edge.aes)
    p <- p + do.call("geom_segment", edge.attributes, quote=TRUE)
  }
  
  # Plot midpoints
  
  if(identical(midpoints, TRUE)){
    midpoint.attributes$mapping     <- do.call("aes", midpoint.aes)
    p <- p + do.call("geom_segment", midpoint.attributes, quote=TRUE)
  }
  
  # Plot vertices
  vertex.attributes$mapping     <- do.call("aes", vertex.aes)
  p <- p + do.call("geom_point", vertex.attributes, quote=TRUE)
  
  # Plot text
  if(text==TRUE){
    text.attributes$mapping     <- do.call("aes", text.aes)
    p <- p + do.call("geom_text", text.attributes, quote=TRUE)
  }
  
  # Formatting
  p <- p + theme_bw()
  p <- p + labs(alpha="Alpha", shape="Shape", color="Color", linetype="Linetype", size="Size", fill="Fill")
  
  if(legend == "bottom")  p <- p + theme(legend.position = "bottom", legend.direction = "horizontal", legend.box = "horizontal")
  if(legend == "none")    p <- p + theme(legend.position = "none")
  
  p + theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
  
  
  
}



##################################
# Data object for plotting
# Det her kan optimeres ved at lave en lang factor, ændre levels, som nu indeholder både afsender og modtager og derefter dele den i 2 igen.
# Måske kan det også klares med merge()

edge.coord <- function(graph, layout){
  
  graph.names       <- V(graph)$name
  el                <- data.frame(get.edgelist(graph))
  
  el.X1.levels.x    <- levels(el$X1)
  el.X1.levels.y    <- levels(el$X1)
  el.X2.levels.x    <- levels(el$X2)
  el.X2.levels.y    <- levels(el$X2)
  
  for (i in 1:length(graph.names)){
    navn             <- graph.names[i]
    navn.el.pos.1    <- which(el.X1.levels.x == navn)
    navn.el.pos.2    <- which(el.X2.levels.x == navn)
    el.X1.levels.x[navn.el.pos.1]      <- layout[i, 1] 
    el.X1.levels.y[navn.el.pos.1]      <- layout[i, 2] 
    el.X2.levels.x[navn.el.pos.2]      <- layout[i, 1] 
    el.X2.levels.y[navn.el.pos.2]      <- layout[i, 2] 
  }
  
  out                   <- data.frame(start.x = el$X1, start.y = el$X1, slut.x = el$X2, slut.y = el$X2, weight = E(graph)$weight)
  levels(out$start.x)   <- el.X1.levels.x
  levels(out$start.y)   <- el.X1.levels.y
  levels(out$slut.x)    <- el.X2.levels.x
  levels(out$slut.y)    <- el.X2.levels.y
  
  out                   <- apply(out, 2, as.character)
  out                   <- apply(out, 2, as.numeric)
  
  as.data.frame(out)
}

vertex.coord <- function(graph, layout=layout.fruchterman.reingold(graph)){
  rownames(layout)  <- V(graph)$name
  layout            <- as.data.frame(layout, rownames(layout))
  colnames(layout)  <- c("x", "y")
  layout
}


# Eksempler
# 
# class.mem       <- segment.membership(segmenter)$membership
# class.total     <- data.frame(segmenter$mat.list[[1]])$Total
# class.total     <- class.total[-length(class.total)]
# 
# gg.jonas(segmenter)
# 
# gg.jonas(segmenter, show.text = FALSE, legend = "bottom")
# 
# gg.jonas(segmenter, vertex.fill = class.mem, vertex.size = class.total)
# 
# edges.trunk <- edges
# 
# edges.trunk[edges >= 5] <- 5
# 
# gg.jonas(segmenter, edges = edges.trunk)

# Løsningen var at tage den convekse hull af punkterne - tegne cirkler for hvor hvert punkt i hullen og derefter tage hullen af disse cirkler.
