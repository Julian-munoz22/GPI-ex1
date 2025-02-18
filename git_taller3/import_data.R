library	(ggplot2)
set.seed(123)
n - 100
datos_sinteticos - data.frame(
			      id=1:n,
			      edad=sample(18:65, n, replace = TRUE),
			      ingreso = round(rnorm(n, mean = 50000, sd = 15000),2),
			      categoria = sample (c("A","B","C"), n, replace = TRUE)
			      )


import pandas as pd:wd
