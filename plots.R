suppressPackageStartupMessages(library(dplyr))
library(ggplot2)
library(Cairo)
library(RColorBrewer)

theme_miniBW <- function(base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.border = element_blank(), axis.line = element_line(colour = "black"))
}
theme_set(theme_miniBW())


#------ data preparation ------#

#data <- mutate(data, ASscore= correctAnchorHasBeenSelected/numSelectedHooks)
data <- mutate(data, ASscore= ifelse(numSelectedHooks>1, .5, correctAnchorHasBeenSelected))
SAS <- aggregate(ASscore~interface+block+id, data, mean)
nAS <- aggregate(numSelectedHooks~interface+block+id, data, mean)
CAS <- aggregate(correctAnchorHasBeenSelected~interface+block+id, data, sum)
CAS <- mutate(CAS, controlStart= id %in% seq(1:6))
comp <- merge(CAS, collapsed)
comp <- merge(comp, SAS)
comp <- merge(comp, nAS)


#------ density Correct Anchor Selected ------#

densityPlot <- function(d, measure, xlim, ylim){
  plot(density(d[[measure]]), xlim=xlim, ylim=ylim, main=unique(d$interface), xlab=measure)
}
histogram <- function(d, measure, breaks, ylim){
  hist(d[[measure]], breaks=breaks, ylim=ylim, main=unique(d$interface), xlab=measure)
}

par(mfrow=c(2,2))
#by(CAS, CAS$interface, densityPlot, "correctAnchorHasBeenSelected", c(-20,60), c(0,.046))
#by(CAS, CAS$interface, histogram, "correctAnchorHasBeenSelected", 10, c(0,6))

#by(CAS, CAS$interface, densityPlot, "correctAnchorHasBeenSelected", c(-5,15), c(0,.15))
#by(CAS, CAS$interface, histogram, "correctAnchorHasBeenSelected", 10, c(0,5))
par(mfrow=c(1,1))

plot(density(subset(CAS, interface!="Control")$correctAnchorHasBeenSelected), xlab="Num Correct Anchor Selected", main="")
#hist(subset(CAS, interface!="Control")$correctAnchorHasBeenSelected, xlab="Num Correct Anchor Selected", main="", breaks=10)


#------ scatter plot Performance vs CAS ------#

tableauPalette <- c("#1F77B4", "#17BECF", "#FF7F0E", "#9467BD")
DarkYlGnBu <- c("#7fcdbb", "#41b6c4", "#225ea8", "#000000")
scatter <- ggplot(comp, aes(x=correctAnchorHasBeenSelected, y=shortDuration)) + scale_colour_manual(values=tableauPalette)

#scatter <- scatter + geom_point(aes(size=block, shape=block, color=interface)) + scale_shape_manual(values = c(21, 19)) + scale_size_manual(values=c(3,2))
#scatter <- scatter + geom_line(aes(group=id, color=interface))

scatter <- scatter + geom_jitter(size=3, aes(color=interface))

#scatter <- scatter + facet_grid(~ block)
scatter <- scatter + facet_grid(controlStart ~ block)
#scatter <- scatter + facet_grid( ~ controlStart)


CairoWin()    # separate rendering window for antialiasing

print(scatter + theme_bw())

# save output to file
CairoPNG("C:/Users/Antoine/Dropbox/research/Experiment/figures/mturk/0.png",1600, 900)
#CairoPNG("C:/Users/Antoine/Dropbox/research/Experiment/figures/lab/0.png",1800, 900)
# wait a couple seconds before calling again print(scatter)

# CAS vs numAS ~ interface + block
scatter <- ggplot(comp, aes(x=numSelectedHooks, y=correctAnchorHasBeenSelected)) + scale_colour_manual(values=tableauPalette)

#scatter <- scatter + geom_point(aes(size=block, shape=block, color=interface)) + scale_shape_manual(values = c(21, 19)) + scale_size_manual(values=c(3,2))
#scatter <- scatter + geom_line(aes(group=id, color=interface))

scatter <- scatter + geom_jitter(size=3, aes(color=interface))
scatter <- scatter + geom_jitter(shape=21, aes(color=interface, size=-shortDuration))
scatter <- scatter + facet_grid(~ block) + theme_bw()

print(scatter)


# duration & numAS vs CAS ~ interface + block
scatter <- ggplot(comp, aes(x=correctAnchorHasBeenSelected, y=shortDuration)) + scale_colour_manual(values=tableauPalette)

scatter <- scatter + geom_point(aes(size=numSelectedHooks, shape=block, color=interface)) + scale_shape_manual(values = c(21, 19))
scatter <- scatter + geom_line(aes(group=id, color=interface))

#scatter <- scatter + geom_point(shape=21, aes(color=interface, size=numSelectedHooks))
#scatter <- scatter + facet_grid(~ block) + theme_bw()

print(scatter)


#------ comparative distributions : block ------#

distribution <- ggplot(subset(CAS, interface!= "Control"), aes(x=correctAnchorHasBeenSelected))
distribution <- distribution + geom_density(trim=FALSE, aes(color=block)) + scale_color_manual(values=DarkYlGnBu)
#distribution <- distribution + coord_cartesian(xlim = c(-5, 25)) 
print(distribution)

distribution <- ggplot(subset(SAS, interface!= "Control"), aes(x=ASscore))
distribution <- distribution + geom_density(trim=FALSE, aes(color=block)) + scale_color_manual(values=DarkYlGnBu)
print(distribution)


#------ comparative distributions : interface ------#

distribution <- ggplot(subset(CAS, interface!= "Control"), aes(x=correctAnchorHasBeenSelected))
distribution <- distribution + geom_density(trim=FALSE, aes(color=interface)) + scale_color_manual(values=tableauPalette)
#distribution <- distribution + coord_cartesian(xlim = c(-5, 25)) 
print(distribution)

distribution <- ggplot(subset(SAS, interface!= "Control"), aes(x=ASscore))
distribution <- distribution + geom_density(trim=FALSE, aes(color=interface)) + scale_color_manual(values=tableauPalette)
print(distribution)