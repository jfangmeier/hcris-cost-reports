library(vcdExtra)

df <- data.frame(gender = c("Male","Male","Female","Female"),
                 enrollment = c(0,1,0,1),
                 freq = c(50,25,50,15)) %>% 
  expand.dft(freq = "freq")

# NOT RUN {
df <- tibble::tibble(x = c("a", "b"), n = c(1, 2))
uncount(df, n)
uncount(df, n, .id = "id")

# You can also use constants
uncount(df, 2)

# Or expressions
uncount(df, 2 / n)
# }
