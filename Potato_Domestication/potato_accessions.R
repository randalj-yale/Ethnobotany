library(car)
library(geiger)
library(nlme)
potato=(read.csv("potato_accessions.csv", header = TRUE, row.names=1))
model = lm (potato$landrace.2x+potato$landrace.4x ~ potato$starch.content+potato$total.glycoalkaloid, potato)
model1 = lm (potato$landrace.2x+potato$landrace.4x ~ potato$mean.lesion+potato$flower.color+potato$Collection.Site+potato$habit++potato$Cultivation.Zone+potato$stolon.type+potato$circadian.average+potato$Elevation..MASL.)
avPlots(model)
avPlots(model1)
##### wild potato phylogeny
attempt = read.newick("attempt.txt")
plot(attempt)
tip = c("S_etuberosum", "S_chomatophium", "S_megistacrolobum", "S_verrucosum", "S_boliviense", "S_infundibuliforme", "S_candolleanum", "S_spegazzinii", "S_gourlayi", "S_vernei", "S_microdontum", "S_okadae")
attempt1= drop.tip(attempt, tip)
root1 = force.ultrametric(attempt1)
data1 = read.csv("data1.csv")
tip1 = c("S_etuberosum", "S_chomatophium", "S_ehrenbergii", "S_raphanifolium", "S_medians", "S_leptophyes", "S_brevicaule", "S_boliviense", "S_candolleanum", "S_okadae", "S_kurtzianum")
attempt2 = drop.tip(attempt, tip1)
data2 = read.csv("data2.csv")
potato_tree = reroot(attempt,1)
landrace2 = data1$landrace.2x
landrace4 = data1$landrace.4x
glyco = data1$total.glycoalkaloid
landrace2 = data2$landrace.2x
landrace4 = data2$landrace.4x
starch = data2$starch.content
pglsModel3<-gls(landrace2*landrace4~glyco, correlation=corBrownian(phy=root1), method="ML")
anova(pglsModel3)
pglsModel2<-gls(landrace2*landrace4~starch, correlation=corBrownian(phy=attempt2), method="ML")
anova(pglsModel2)

res = resid(pglsModel3)
qqnorm(res)
qqline(res)
res2 = resid(pglsModel2)
qqnorm(res2)
qqline(res2)