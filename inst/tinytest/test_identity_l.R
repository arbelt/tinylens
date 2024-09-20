
library(tinylens)

x <- 1:10
expect_identical(x, view(x, id_l))

.v <- view(x, id_l)
expect_identical(x, set(x, id_l, .v))
