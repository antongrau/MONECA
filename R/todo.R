# # TODO
# # Mode er usystematisk implementeret i anton, jonas og find.segment : husk at weight matrix også er inde over
# 
# # 
# Ego kort
# Vi har ikke styr på rækker og kolonner her, så vi ved ikke hvad der sender og modtager

# library(MONECA)
# data(occupations)
# mxa.b <- mob.mat
# segmenter <- anton(mxa.b, segment.levels = 3)
# ego.plot(segmenter, mob.mat, id = 5)

# add.table.to.plot <- function()

# Annotate en tabel på
#   sum.stat  <- c("Beskæftigede" = as.numeric(stor.beskæftigede[id]),
#                  "Andel af alle beskæftigede %" = round(as.numeric(stor.beskæftigede[id]/sum(stor.beskæftigede)), 3),
#                           "Intern mobilitet %" = round(as.numeric(intern.mobilitet[id]), 2),
#                           "Organisationsgrad 2011 %" = round(organiserede.andel[id, ncol(organiserede.andel)], 2))

# + annotate("text", x = Inf, y = -Inf, color = "black", vjust = -0.5, hjust = 1 ,label = sum.stat)