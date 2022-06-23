
# setup -------------------------------------------------------------------
library(tidyverse)
library(fixest)
# super minimal
theme_super_minimal = function(base_size = 14){
  theme_minimal(base_size) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())
}
ggplot2::theme_set(theme_super_minimal())


# heteroskedasticity ------------------------------------------------------

df = data.frame(x = 1:100)

# constant variance
set.seed(1234)
df = df %>% 
  mutate(y = 2*x + rnorm(n = 100, mean = 0, sd = 20))
df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point()
m1 = fixest::feols(y ~ x + 0, data = df)
summary(m1)


# non-constant variance
set.seed(1234)
df = df %>% mutate(y2 = 2*x + rnorm(n = 100, mean = 0, sd = x^1.7))
df %>% 
  ggplot(aes(x = x, y = y2)) + 
  geom_point()
m2 = fixest::feols(y2 ~ x + 0, data = df)
summary(m2)
summary(m2, se = "hetero")


# clustered errors --------------------------------------------------------
endowment_df = read_csv("data/apicella_al_2011.csv")

X = endowment_df %>% 
  mutate(lighter_times_distance = lighter * distance_to_mangola) %>% 
  select(magnola_region, distance_to_mangola, lighter, lighter_times_distance) %>% 
  mutate(intercept = 1) %>% 
  relocate(intercept) %>% 
  as.matrix()

k = ncol(X)
n = nrow(X)

y = endowment_df %>% pull(trade)

betas = solve(crossprod(X)) %*% crossprod(X,y) 
y_pred = X %*% betas
u = y - y_pred

vcov_nonclustered = (sum(u^2)/(n - k)) * solve(crossprod(X))
sqrt(diag(vcov_nonclustered))

endowment_model = fixest::feols(trade ~ magnola_region + distance_to_mangola + lighter + lighter*distance_to_mangola, data= endowment_df)
summary(endowment_model)

# X^T ee^T X for each cluster (so the obervations and residuals for each cluster j)
# so this is cool
# you need to subset X and u by cluster
# what if the data are not sorted by cluster? (this is what you did in pgmm)
# the cool thing is that you can subset vectors/matrices with booleans
# the TRUEs are the indices you subset on 
# for instance, these are the indices for the first campname:
as.numeric(factor(df$campname)) == 1
# so just pass this as the subset condition inside [] for the matrix or vector
u[as.numeric(factor(df$campname)) == 1]
X[as.numeric(factor(df$campname)) == 1, , drop = FALSE]

# now you can do a loop, storing all the clusters in a list
c = length(unique(endowment_df$campname)) # nclusters
c = vector(mode = "list", length = c)
for(i in seq_along(c)) { 
  j = as.numeric(factor(df$campname)) == i
  Xj = X[j, , drop = FALSE]
  uj = u[j]
  c[[i]] = t(Xj) %*% tcrossprod(uj) %*% Xj
}
# now you can reduce the list into a single weighting matrix:
omega = Reduce(f = "+", x = c)
# and calculate the clustered standard errors
# var[B] = (X^T X)^-1 X^T S X (X^T X)^-1
# the X^T S X is just omega, a k x k matrix
XtXinv = solve(crossprod(X))
vcov = (solve(crossprod(X)) %*% omega %*% solve(crossprod(X))) * n_camps/(n_camps-1) * (n-1)/(n-k)
# and then sqrt(diag(vcov)) for the standard errors
# but I think there are some small sample corrections I'm not making
gls = fixest::feols(model, data = df)
summary(gls, cluster = "campname")


# Reduce is very cool
# in this case it is performing component-wise addition across matrices
# ie you add the i,j element of each matrix together
# consider this example:
set.seed(1234)
mats = list(matrix(data = sample.int(20), 2, 2), 
            matrix(data = sample.int(20), 2, 2))
# with a loop
reduce_mats = matrix(data = NA, nrow = 2, ncol = 2)
for(i in 1:nrow(m)){
  for(j in 1:ncol(m)){
    reduce_mats[i,j] = mats[[1]][i,j] + mats[[2]][i,j]
  }
}
reduce_mats
# with reduce:
Reduce(x = mats, f = "+")
