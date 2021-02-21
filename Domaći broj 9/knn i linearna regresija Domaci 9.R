library(dplyr)
library(MASS)
library(lattice)
library(class)

# podesavanje broja "semena" za reprodukciju podataka u buducnosti
seed <- 123456
set.seed(seed)

# generacija proseka dve klase
Sigma <- matrix(c(1,0,0,1),nrow = 2, ncol = 2)
means_1 <- mvrnorm(n = 10, mu = c(1,0), Sigma)
means_2 <- mvrnorm(n = 10, mu = c(0,1), Sigma)

head(means_1)
tail(means_2)

# nasumicno izaberite m_k sa verovatnoæom 1/10
# funkcija za generisanje opazanja; # Kako se generise funkcija?, proveriti=   ??function
genObs <- function(classMean, classSigma, size, ...)
{
  # provera inputa
  if(!is.matrix(classMean)) stop("classMean mora biti matrica")
  nc <- ncol(classMean)
  nr <- nrow(classMean)
  if(nc != 2) stop("classMean mora biti matrica sa 2 kolone")
  if(ncol(classSigma) != 2) stop(" dimenzija classSigma je pogresna")
  
  # srednja vrednst svake obzervacije
  # izaberite nasumicno m_k
  meanObs <- classMean[sample(1:nr, size = size, replace = TRUE),]
  obs <- t(apply(meanObs, 1, function(x) mvrnorm(n = 1, mu = x, Sigma = classSigma )) )
  colnames(obs) <- c('x1','x2')
  return(obs)
}

obs100_1 <- genObs(classMean = means_1, classSigma = Sigma/5, size = 100)
head(obs100_1)
obs100_2 <- genObs(classMean = means_2, classSigma = Sigma/5, size = 100)
head(obs100_2)

# generisanje label
y <- rep(c(0,1), each = 100)

# matrica podataka obuke algoritma
trainMat <- as.data.frame(cbind(y, rbind(obs100_1, obs100_2)))

# plotiranje matrice podataka obuje
with(trainMat, xyplot(x2 ~ x1,groups = y, col=c('blue', 'red')))

# sada fitujemo dva modela
# model 1: model linerane regresije
lmfits <- lm(y ~ x1 + x2 , data = trainMat)

# trazenje linije nagiba i presretanja za granicu odluke
intercept <- -(lmfits$coef[1] - 0.5) / lmfits$coef[3]
intercept
slope <- - lmfits$coef[2] / lmfits$coef[3]
slope

# Figure 2.1
xyplot(x2 ~ x1, groups = y, col = c('blue', 'orange'), data = trainMat,
       panel = function(...)
       {
         panel.xyplot(...)
         panel.abline(intercept, slope)
       },
       main = 'Linear Regression of 0/1 Response')    

# model2: knn metod
# trazenje ranga za x1 and x2
rx1 <- range(trainMat$x1)
print(rx1)
rx2 <- range(trainMat$x2)
print(rx2)
# dobijanje resetke ili natrice u prostoru predvidanja
px1 <- seq(from = rx1[1], to = rx1[2], by = 0.1 )
px2 <- seq(from = rx2[1], to = rx2[2], by = 0.1 )
xnew <- expand.grid(x1 = px1, x2 = px2)
head(xnew)
# konturna mapa
knn15 <- knn(train = trainMat[,2:3], test = xnew, cl = trainMat[,1], k = 15, prob = TRUE)
head(knn15)
prob <- attr(knn15, "prob")
prob <- ifelse(knn15=="1", prob, 1-prob)
prob15 <- matrix(prob, nrow = length(px1), ncol = length(px2))

# Figure 2.2
par(mar = rep(2,4))
contour(px1, px2, prob15, levels=0.5, labels="", xlab="", ylab="", main=
          "15-nearest neighbour", axes=FALSE)
points(trainMat[,2:3], col=ifelse(trainMat[,1]==1, "coral", "cornflowerblue"))
points(xnew, pch=".", cex=1.2, col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
box()

