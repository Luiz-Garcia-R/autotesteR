## ---- pre.learn.R ----

raw_data <- data.frame(
  Sample = paste0("S", 1:12),
  Var1 = rnorm(12, 5, 2),
  Var2 = rnorm(12, 10, 3),
  Var3 = sample(c("A", "B"), 12, replace = TRUE)
)

metadata <- data.frame(
            Sample = paste0("S", 1:12),
            Group = rep(c("Control", "Case"), each = 6),
            Output = rep(c("Control", "Case"), each = 6)
)

pre.learn(raw_data, metadata)
