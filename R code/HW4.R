#1. 선형대수와 R
A <- matrix(data = 1:36, nrow = 6)
A
B <- matrix(data = 1:30, nrow = 6)
B
A %*% B

A <- matrix(data = 1:36, nrow = 6)
A
B <- matrix(data = 11:46, nrow = 6)
B
A * B

X <- matrix(data=1:10, nrow= 10)
X
Y <- matrix(data= 11:20, nrow= 10)
Y
dotProduct <- function(X,Y) {
  as.vector(t(X) %*% Y)
}
dotProduct(X,Y)

A <- matrix (data = 1:25 , nrow = 5)
B <- matrix (data = 26 :50 , nrow = 5)
C <- matrix (data = 51 :75 , nrow = 5)
A %*% (B + C)
A %*% B + A %*% C

A <- matrix (data = 1:25 , nrow = 5)
B <- matrix (data = 26 :50 , nrow = 5)
C <- matrix (data = 51 :75 , nrow = 5)
(A %*% B) %*% C
A %*% (B %*% C)

A <- matrix (data = 1:25 , nrow = 5)
B <- matrix (data = 26 :50 , nrow = 5)
A %*% B
B %*% A

A <- matrix (data = 1:25 , nrow = 5, ncol = 5, byrow = TRUE )
A
t(A)

A <- matrix (data = 1:25 , nrow = 5)
B <- matrix (data = 25 :49 , nrow = 5)
t(A %*% B)
t(B) %*% t(A)

A <- matrix (data = c(1,3,2,4,2,4,3,5,1,6,7,2,1,5,6,7), nrow = 4, byrow = TRUE )
A
B <- matrix (data = c(1, 2, 3, 4), nrow = 4)
B
solve (a = A, b = B)

I <- diag (x = 1, nrow = 5, ncol = 5)
I
A <- matrix (data = 1:25 , nrow = 5)
A %*% I
I %*% A

A <- matrix (data = c(1,2,3,1,2,3,4,5,6,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,3), nrow = 5)
A
library (MASS)
ginv (A)
ginv (A) %*% A
A %*% ginv (A)

A <- matrix (data = c(1, 3, 2, 4, 2, 4, 3, 5, 1, 6, 7, 2, 1, 5, 6, 7), nrow = 4, byrow = TRUE )
A
B <- matrix (data = c(1, 2, 3, 4), nrow = 4)
B
library (MASS )
X <- ginv (A) %*% B
X

A <- matrix (data = c(1,3,2,4,2,4,3,5,1,6,7,2,1,5,6,7), nrow = 4, byrow = TRUE )
A
det (A)

lpNorm <- function (A, p) {
  if (p >= 1 & dim (A)[[ 2]] == 1 && is.infinite (p) == FALSE ) {
    sum ((apply (X = A, MARGIN = 1, FUN = abs)) ** p) ** (1 / p)
  } else if (p >= 1 & dim (A)[[ 2]] == 1 & is.infinite (p)) {
    max (apply (X = A, MARGIN = 1, FUN = abs)) # Max Norm
  } else {
    invisible (NULL )
  }
}
lpNorm (A = matrix (data = 1:10 ), p = 1)
lpNorm (A = matrix (data = 1:10 ), p = 2) # Euclidean Distance
lpNorm (A = matrix (data = 1:10 ), p = 3)
lpNorm (A = matrix (data = -100 :10 ), p = Inf )

lpNorm (A = matrix (data = rep (0, 10 )) , p = 1) == 0
lpNorm (A = matrix (data = 1:10 ) + matrix (data = 11 :20 ), p = 1) <= lpNorm (A = matrix (data = 1:10 ), p = 1) + lpNorm (A = matrix (data = 11 :20 ), p = 1)
tempFunc <- function (i) {
  lpNorm (A = i * matrix (data = 1:10 ), p = 1) == abs (i) * lpNorm (A = matrix (data = 1:10 ), p = 1)
}
all (sapply (X = -10 :10 , FUN = tempFunc ))

frobeniusNorm <- function (A) {
  (sum (( as.numeric (A)) ** 2)) ** (1 / 2)
}
frobeniusNorm (A = matrix (data = 1:25 , nrow = 5))

A <- diag (x = c(1:5, 6, 1, 2, 3, 4), nrow = 10 )
A
X <- matrix (data = 21 :30 )
X
A %*% X
library (MASS )
ginv (A)

A <- matrix (data = c(1, 2, 2, 1), nrow = 2)
A
all (A == t(A))

lpNorm (A = matrix (data = c(1, 0, 0, 0)) , p = 2)

X <- matrix (data = c(11 , 0, 0, 0))
Y <- matrix (data = c(0, 11 , 0, 0))
all (t(X) %*% Y == 0)

X <- matrix (data = c(1, 0, 0, 0))
Y <- matrix (data = c(0, 1, 0, 0))
lpNorm (A = X, p = 2) == 1
lpNorm (A = Y, p = 2) == 1
all (t(X) %*% Y == 0)

A <- matrix (data = c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, byrow = TRUE )
A
all (t(A) %*% A == A %*% t(A))
all (t(A) %*% A == diag (x = 1, nrow = 3))
library (MASS )
all (t(A) == ginv (A))

A <- matrix (data = 1:25 , nrow = 5, byrow = TRUE )
A
y <- eigen (x = A)
library (MASS )
all.equal (y$vectors %*% diag (y$values ) %*% ginv (y$vectors ), A)

A <- matrix (data = 1:36 , nrow = 6, byrow = TRUE )
A
y <- svd (x = A)
y
all.equal (y$u %*% diag (y$d) %*% t(y$v), A)

A <- matrix (data = 1:25 , nrow = 5)
A
B <- ginv (A)
B
y <- svd (A)
all.equal (y$v %*% ginv (diag (y$d)) %*% t(y$u), B)

A <- diag (x = 1:10 )
A
library (psych )
tr (A)
alternativeFrobeniusNorm <- function (A) {
  sqrt (tr (t(A) %*% A))
}
alternativeFrobeniusNorm (A)
frobeniusNorm (A)
all.equal (tr (A), tr (t(A)))
A <- diag (x = 1:5)
A
B <- diag (x = 6:10 )
B
C <- diag (x = 11 :15 )
C
all.equal (tr (A %*% B %*% C), tr (C %*% A %*% B))
all.equal (tr (C %*% A %*% B), tr (B %*% C %*% A))

#2. Singular Value Decomposition
education.by.readership <- matrix(c(5,18,19,12,3,7,46,29,40,7,2,20,39,49,16), 
  nrow <- 5)
dimnames(education.by.readership) <- list(
    "Level of education" <- c("Some primary", "Primary completed", "Some secondary", "Secondary completed", "sum tertiary"),
    "Category of readership" <- c("Glance", "Fairly thorough", "Very thorough"))
print(education.by.readership)
O <- education.by.readership / sum(education.by.readership)
print(O)
rowSums(O)
colSums(O)
E <- rowSums(O) %o% colSums(O)
print(E)
Z <- (O - E) / sqrt(E)
print(Z)

SVD = svd(Z)
print(SVD)

sum(SVD$d * SVD$u[5, ] * SVD$v[2, ])

SVD$u %*% diag(SVD$d) %*% t(SVD$v)

variance.explained = prop.table(svd(Z)$d^2)

library(MASS)
a <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1), 9, 4)
print(a)
a.svd <- svd(a)
a.svd$d
ds <- diag(1/a.svd$d[1:3])
u <- a.svd$u
v <- a.svd$v
us <- as.matrix(u[, 1:3])
vs <- as.matrix(v[, 1:3])
(a.ginv <- vs %*% ds %*% t(us))
# using the function ginv defined in MASS
ginv(a)

#버전이 바뀌면서 ReadImages가 OpenImageR로 바뀜
#install.packages("OpenImageR")
library(OpenImageR)
x <- readImage("pansy.jpg")
dim(x)
#버전이 바뀌면서 함수도 모두 바뀜
plot(x, useRaster = TRUE)
r <- imagematrix(x, type = "grey")
plot(r, useRaster = TRUE)
r.svd <- svd(r)
d <- diag(r.svd$d)
dim(d)
u <- r.svd$u
v <- r.svd$v
plot(1:length(r.svd$d), r.svd$d)
# first approximation
u1 <- as.matrix(u[-1, 1])
v1 <- as.matrix(v[-1, 1])
d1 <- as.matrix(d[1, 1])
l1 <- u1 %*% d1 %*% t(v1)
l1g <- imagematrix(l1, type = "grey")
plot(l1g, useRaster = TRUE)
# more approximation
depth <- 5
us <- as.matrix(u[, 1:depth])
vs <- as.matrix(v[, 1:depth])
ds <- as.matrix(d[1:depth, 1:depth])
ls <- us %*% ds %*% t(vs)
lsg <- imagematrix(ls, type = "grey")
plot(lsg, useRaster = TRUE)

library(foreign)
auto <- read.dta("http://statistics.ats.ucla.edu/stat/data/auto.dta")
pca.m1 <- prcomp(~trunk + weight + length + headroom, data = auto, scale = TRUE)
screeplot(pca.m1)
# spectral decomposition: eigen values and eigen vectors
xvars <- with(auto, cbind(trunk, weight, length, headroom))
corr <- cor(xvars)
a <- eigen(corr)
(std <- sqrt(a$values))
(rotation <- a$vectors)
# svd approach
df <- nrow(xvars) - 1
zvars <- scale(xvars)
z.svd <- svd(zvars)
z.svd$d/sqrt(df)
z.svd$v

cnut <- read.dta("http://statistics.ats.ucla.edu/stat/data/cerealnut.dta")
# centering the variables
mds.data <- as.matrix(sweep(cnut[, -1], 2, colMeans(cnut[, -1])))
dismat <- dist(mds.data)
mds.m1 <- cmdscale(dismat, k = 8, eig = TRUE)
mds.m1$eig
mds.m1 <- cmdscale(dismat, k = 2, eig = TRUE)
x <- mds.m1$points[, 1]
y <- mds.m1$points[, 2]
plot(x, y)
text(x + 20, y, label = cnut$brand)
# eigenvalues
xx <- svd(mds.data %*% t(mds.data))
xx$d
# coordinates
xxd <- xx$v %*% sqrt(diag(xx$d))
x1 <- xxd[, 1]
y1 <- xxd[, 2]
plot(x1, y1)
text(x1 + 20, y1, label = cnut$brand)

#3. Similarity and Dissimilarity
# Subset of the data
set.seed (123 )
ss <- sample (1:50 , 15 ) # Take 15 random rows
df <- USArrests [ss , ] # Subset the 15 rows 
df.scaled <- scale (df ) # Standardize the 

dist.eucl <- dist (df.scaled , method = "euclidean" )

# Reformat as a matrix
# Subset the first 3 columns and rows and Round the values
round (as.matrix (dist.eucl )[ 1:3, 1:3], 1)

# Compute
#install.packages("factoextra")
library ("factoextra" )
dist.cor <- get_dist (df.scaled , method = "pearson" )
# Display a subset
round (as.matrix (dist.cor )[ 1:3, 1:3], 1)

library (cluster )
# Load data
data (flower )
head (flower , 3)
# Data structure
str (flower )
dd <- daisy (flower )
round (as.matrix (dd )[ 1:3, 1:3], 2)

library (factoextra )
fviz_dist (dist.eucl )

#4. Multidimensional Scaling
url <- "http://rosetta.reltech.org/TC/v1 5/Mapping/data/dist -Aus.csv"
#dist.au <- read.csv(url)

dist.au <- read.csv("dist-Aus.csv")
dist.au
row.names(dist.au) <- dist.au[, 1]
dist.au <- dist.au[, -1]
dist.au

fit <- cmdscale(dist.au, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]

plot(x, y, pch = 19, xlim = range(x) + c(0, 600))
city.names <- c("Adelaide", "Alice Springs", "Brisbane", "Darwin", "Hobart", "Melbourne", "Perth", "Sydney")
text(x, y, pos = 4, labels = city.names)

x <- 0 - x
y <- 0 - y
plot(x, y, pch = 19, xlim = range(x) + c(0, 600))
text(x, y, pos = 4, labels = city.names)

#install.packages("igraph")
library(igraph)
g <- graph.full(nrow(dist.au))
V(g)$label <- city.names
layout <- layout.mds(g, dist = as.matrix(dist.au))
plot(g, layout = layout, vertex.size = 3)

data ("swiss" )
head (swiss )

# Load required packages
library (magrittr)
library (dplyr)
library (ggpubr)
# C ompute MDS
## The infix operator “%>% ” is not part of base R,
## but defined by the package magrittr .
## It works like a pipe , pass ing the LHS to first argument of RHS .
mds <- swiss %>% dist () %>% cmdscale () %>% as_tibble ()
colnames (mds ) <- c("Dim.1" , "Dim.2" )
# Plot MDS
ggscatter (mds , x = "Dim.1" , y = "Dim.2" ,
  label = rownames (swiss ),
  size = 1,
  repel = TRUE )

# K -means clustering
clust <- kmeans (mds , 3)$cluster %>% as.factor ()
mds <- mds %>% mutate (groups = clust )
# Plot and color by groups
ggscatter (mds , x = "Dim.1" , y = "Dim.2" ,
  label = rownames (swiss ),
  color = "groups" ,
  palette = "jco" ,
  size = 1,
  ellipse = TRUE ,
  ellipse.type = "convex" ,
  repel = TRUE )

library (magrittr )
library (dplyr )
library (ggpubr )

# C ompute MDS
library (MASS )
mds <- swiss %>% dist () %>% isoMDS () %>% .$points %>% as_tibble ()
colnames (mds ) <- c("Dim.1" , "Dim.2" )
# Plot MDS
ggscatter (mds , x = "Dim.1" , y = "Dim.2" ,
  label = rownames (swiss ),
  size = 1,
  repel = TRUE )

# Compute MDS
library (MASS )
mds <- swiss %>% dist () %>% sammon () %>% .$points %>% as_tibble ()
colnames (mds ) <- c("Dim.1" , "Dim.2" )
# Plot MDS
ggscatter (mds , x = "Dim.1" , y = "Dim.2" ,
  label = rownames (swiss ),
  size = 1,
  repel = TRUE )

res.cor <- cor (mtcars , method = "spearman" )
mds.cor <- (1 - res.cor ) %>% cmdscale () %>% as_tibble ()
colnames (mds.cor ) <- c("Dim.1" , "Dim.2" )
ggscatter (mds.cor , x = "Dim.1" , y = "Dim.2" ,
  size = 1,
  label = colnames (res.cor ),
  repel = TRUE )

#5. Principal Components Analysis
library(datasets)
data(USArrests)
summary(USArrests)
myData <- USArrests
fit <- princomp(myData, cor=TRUE)
summary(fit)
loadings(fit)
plot(fit, type="lines")
fit$scores
biplot(fit)

ramen <-matrix(c(2,1,5,2,3,4,4,1,3,5,4,5,3,2,5,3,4,2,3,5,5,1,4,3,5,2,3,1,2,3), ncol=3)
rownames(ramen) <- c(" 쇠고기라면 ", " 해물라면 ", " 얼큰라면 ", " 떡라면 ", " 짬뽕라면 ",
  "만두라면 ", " ", "치즈라면 ", " 된장라면 ", " 볶음라면 ", " 김치라면 ")
colnames(ramen) <- c(" 면", "그릇 ", " 국물 ")
print( ramen )
pc<-prcomp( ramen , scale=TRUE)
print( pc )
summary(pc)

predict(pc)
biplot(pc)

