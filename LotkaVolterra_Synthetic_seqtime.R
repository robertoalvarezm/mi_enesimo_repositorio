library(seqtime)
library(ggplot2)
library(reshape2)


## Generar la matriz dde interacciones vía el modelo LV----

# Number of taxa
N=50
#Time-points
S=20
## Esto genera la matriz de interacciones
## modular y free-scale, el porcentaje
## de conexiones positivas es pep y la 
#conectancia es c=(2*# de conexiones)/(N(N-1))
A=generateA(N,"klemm",pep=10,c=0.05)


plotA(A, header = "Klemm-Eguiliz interaction
      matrix")



dataset <-generateDataSet(S, A)
dataset <-  seqtime::normalize(dataset)
dataset_melt <- melt(dataset)
colnames(dataset_melt) = c("Species", "Sample", "Abundance")
ggplot(data=dataset_melt, aes(x=Sample, y=Abundance, width=1)) + 
  geom_bar(aes(y = Abundance, x= Sample, fill=Species), data=dataset_melt, stat="identity", show.legend=F) + 
  theme(aspect.ratio=.4) + 
  theme_classic()+ 
  ylab("Relative abundance") + 
  xlab("Sample")

### Graficar con lineas ----

tsplot(dataset,main="Generalized Lotka-Volterra")
Aest=limits(dataset,bagging.iter=200)$Aest

par(mfrow=c(1,2))
plotA(A,header="known")
plotA(Aest,header="inferred")

crossCor=cor(A,Aest)
mean(diag(crossCor), na.rm=TRUE)
limitsqual=limitsQuality(A,Aest,plot=TRUE)
