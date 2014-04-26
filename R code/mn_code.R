library(maps)
# xlim is for longitude
# ylim is for latitude


map('state','minnesota',xlim=c(-98,-89),ylim=c(43.5,49.5),add=F)
dev.copy(pdf,'mn.pdf')
dev.off()

box()
dev.copy(pdf,'mn_box.pdf')
dev.off()

rlo <- runif(100,min=-98,max=-89)
rla <- runif(100,min=43.5,max=49.5)
points(rlo,rla,pch=".")
dev.copy(pdf,'mn_box_pts.pdf')
dev.off()
