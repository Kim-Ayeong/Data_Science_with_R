#1. 변수와 벡터 생성
#(1)프린트
a <- 1
x <- 3
a
print(a)
print(x)

#(2)c()
x <- c("fee", "fie", "foe", "fum")
print(x)
c("Everyone", "loves", "stats.")
c(1,1,2,3,5,8,13,21)
c(1*pi, 2*pi, 3*pi, 4*pi)
c(TRUE, TRUE, FALSE, TRUE)
v1 <- c(1,2,3)
v2 <- c(4,5,6)
v3 <- c("A", "B", "C")

#2. 수열
1:5
b <- 2:10
b
10:19
9:0
e <- 10:2
e
seq(from=0, to=20, by=2)
seq(from=0, to=20, length.out=5)
seq(from=1.0, to=2.0, length.out=5)
seq(0, 10, by=1)
seq(0, 10, length=20)
n <- 0
1:n
rep(1, times=5)
rep(1:2, each=2)
c <- 1:5
c
rep(c, 5)
rep(c, each=5)

#3. 데이터 유형과 객체
#(1)Numeric
a <- 3
a

#(2)Character
b <- "Charcter"
b

#(3)paste(“붙일 내용”, sep="")
A <- c("a", "b", "c")
A
paste("a", "b", sep="")
paste(A, c("d", "e"))
f <- paste(A, 10)
f
paste(A, 10, sep="")
paste(A, 1:10, sep="")
paste("Everybody", "loves", "cats.")
paste("Everybody", "loves", "cats.", sep="-")
paste("Everybody", "loves", "cats.", sep="")

#(4)Substr(문자열, 시작, 끝)
ss <- c("Moe", "Larry", "Curly")
substr("BigDataAnalysis", 1, 4)
substr(ss, 1, 3)

#(5)논리값
c <- TRUE
c
d <- T
d
e <- FALSE
e
f <- F
f
a <- 3
a == pi
a != pi
a < pi
a > pi
a <= pi
a >= pi
a = pi
a == pi

#(6)Matrix
theData <- c(1.1, 1.2, 2.1, 2.2, 3.1, 3.2)
mat <- matrix(theData, 2, 3)
mat
dim(mat)
mat
t(mat)
mat%*%t(mat)
diag(mat)
mat
colnames(mat) <- c("IBM", "MSFT", "GOOG")
rownames(mat) <- c("IBM", "MSFT")
mat
mat
mat[1, ] #첫째 행
mat[ ,3] #셋째 열
A <- matrix(0,4,5)
A
A <- matrix(1:20, 4, 5)
A
A[c(1,4), c(2,3)]
A[c(1,4), c(2,3)] <- 1
A
A + 1

#(7)list
lst <- list(3.14, "Moe", c(1, 1, 2, 3), mean)
lst
a <- 1:10
b <- matrix(1:10, 2, 5)
c <- c("name1", "name2")
alst <- list(x=a, y=b, z=c)
alst
alst$x
blst <- list(d=2:10*10)
blst
alst$x
alst[[1]]
alst[[1]][2]
alst[[2]]
ablst <- c(alst, blst)
ablst
score1 <- list(10, 20, 30, 40, 50)
score2 <- list(c("a", "b"))
score1[score1 > 40]
score1 > 40
score1[c(FALSE, FALSE, FALSE, FALSE, TRUE)]
score12 <- list(score1, score2)
score12
score12[1]
score12[[2]]
score12[[2]][1]
score12[[1]][1]
score12[[1]][2]
unlist(score1)
unlist(score2)
unlist(score12)

#(8)데이터프레임
a <- c(1, 2, 4, 6, 3, 4)
b <- c(6, 4, 2, 4, 3.2, 4)
c <- c(7, 6, 4, 2, 5, 6)
d <- c(2, 4, 3, 1, 5, 6)
e <- data.frame(a, b, c, d)
e
data(iris)
head(iris)
newRow <- data.frame(Sepal.Length=3.0, Sepal.Width=3.2, Petal.Length=1.6, Petal.Width=0.3, Species="newsetosa")
newRow
iris <- rbind(iris, newRow)
iris
dim(iris)
newcol <- 1:151
cbind(iris, newcol)
name <- c("john", "peter", "jennifer")
gender <- factor(c("m", "m", "f"))
hw1 <- c(60, 60, 80)
hw2 <- c(40, 50, 30)
grades <- data.frame(name, gender, hw1, hw2)
grades
grades[1, 2]
grades[ , "name"]
grades$name
grades[grades$gender == "m", ]
grades[ , "hw1"]
data(iris)
head(iris)
subset(iris, select=Species, subset=(Petal.Length > 1.7))
subset(iris, select=c(Sepal.Length, Petal.Length, Species), subset=c(Sepal.Width==3.0 & Petal.Width==0.2))
head(with(iris, Species))
name <- c("Moe", "Larry", "Curly", "Harry")
year.born <- c(1887, 1982, 1983, 1964)
place.born <- c("BensonHurst", "Philadelphia", "Brooklyn", "Moscow")
born <- data.frame(name, year.born, place.born)
born
name <- c("Curly", "Moe", "Larry")
year.died <- c(1952, 1975, 1975)
died <- data.frame(name, year.died)
died
merge(born, died, by="name")
data(mtcars)
head(mtcars)
colnames(mtcars)
mtcars[1:5, c("mpg", "cyl")]
mtcars[(mtcars$gear > 3) & (mtcars$cyl > 7 | mtcars$mpg > 21), c("mpg", "cyl", "gear")]

#(9)벡터에 있는 원소 선택
fib <- c(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
fib
fib[1]
fib[3]
fib[1:3]
fib[c(1, 2, 4, 8)]
fib[-1]
fib[-c(1:3)]
fib < 10
fib[fib < 10]
fib%%2 == 0
fib[fib%%2 == 0]
c <- 1:10
c
d <- 1:5
d[c(1, 3)]
c[c(2, 3)]
d[c(1:3, 5)]
c[c > 5 & c < 10]
c[as.logical((c > 8) + (c < 3))]
years <- c(1960, 1964, 1976, 1994)
names(years)<- c("Kennedy", "Johnson", "Carter", "Clinton")
years
years["Carter"]
years["Clinton"]

#(10)자료형 데이터 구조 변환
as.numeric("3.14")
as.integer(3.14)
as.numeric("foo")
as.character(101)
as.numeric(FALSE)
as.numeric(TRUE)

#(11)문자열을 날짜로 변환
Sys.Date( ) #case-sensitive
as.Date("2018-09-28")
as.Date("09/28/2018")
as.Date("09/28/2018", format="%m/%d/%Y")

#(12)날짜를 문자열로 변환
as.Date("09/28/2018", format="%m/%d/%Y")
format(Sys.Date())
as.character(Sys.Date())
format(Sys.Date(), format="%m/%d/%Y")
format(Sys.Date(), '%a')
format(Sys.Date(), '%b')
format(Sys.Date(), '%B')
format(Sys.Date(), '%d')
format(Sys.Date(), '%m')
format(Sys.Date(), '%y')
format(Sys.Date(), '%Y')

#(13)벡터의 기본 연산
x <- c(0, 1, 2, 3, 5, 8, 13, 21, 34)
y <- log(x+1)
y
mean(x)
median(x)
sd(x)
var(x)
cor(x, y)
c <- 1:10
c
1/c
c^2
c^2 + 1
log(c)
sapply(c, log)
c <- 1:10
c
d <- (1:10)*10
d
c + d
c * d
d ^ c
var(c)
log(c)
sum((c - mean(c))^2)/(length(c)-1)
c <- 1:10
c[log(c) < 2]
c[log(c) < 2] <- 3
c
length(c) <- 20
c
c[25] <- 1
c
length(c) <- 10
c

#4. 유용한 기타 함수
#(1)데이터를 저장하고 출력하는 방법
a <- c(1, 2, 3, 4, 5)
write.csv(a, "test.csv")
b <- read.csv("test.csv")
save(a, file="test.Rdata")
a <- 0
load("test.Rdata")
a
print(a)
rm(a)  #또는 rm(list=c("a"))
ls()
rm(list=c("a")) #rm(a)로 이미 지움
ls()
rm(list=ls(all=TRUE))
ls()

#(2)기타 유틸리티 함수