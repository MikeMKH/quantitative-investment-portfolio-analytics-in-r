x <- rnorm(1:100)

set.seed(154)
y <- rnorm(1:5)

set.seed(154)
z <- rnorm(1:10)

# x is different per run since it is random without a seed
plot(x, type='l', panel.first=grid(), main="Random Dataset X")
plot(y, type='l', panel.first=grid(), main="Random Dataset Y")
plot(z, type='l', panel.first=grid(), main="Random Dataset Z")

set.seed(78)
matrix.dat <-data.frame(matrix(rnorm(15), ncol=3))

row.sum <-apply(matrix.dat, MARGIN=1, FUN=sum)
matrix.dat <-cbind(matrix.dat, row.sum)

col.sum <-apply(matrix.dat, MARGIN=2, FUN=sum)
matrix.dat <-rbind(matrix.dat, col.sum)

matrix.dat

colnames(matrix.dat)[1:3] <- c("A", "B", "C")
rownames(matrix.dat) <- c("i", "ii", "iii", "iv", "v", "col.sum")
matrix.dat

set.seed(132)
x <- data.frame(matrix(rnorm(6), ncol=2))
set.seed(98)
y <- rnorm(3)
x$new <-y
x