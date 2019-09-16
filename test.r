library(MONECA)
library(eliter)

data("occupations")

segments <- moneca(mob.mat, segment.levels = 3)

segments
MONECA::first.level.summary(segments)
MONECA::layout.matrix(segments)
MONECA::segment.colors(segments)
MONECA::segment.edges(segments)
MONECA::segment.edges(segments, cut.off = 1, method = "all", segment.reduction = 0, level = 1)
MONECA::segment.membership(segments)
MONECA::segment.quality(segments)
MONECA::stair.plot(segments)
MONECA::vertex.mobility(segments)
MONECA::weight.matrix(mob.mat)

MONECA::moneca.plot(segments)

MONECA::gg.moneca(segments) 

gg.moneca(segments = segments,
          edges = segment.edges(segments, cut.off = 1, method = "all", segment.reduction = 0, level = 1))

segments
MONECA::layout.matrix(segments)
