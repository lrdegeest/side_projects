library(tidyverse)

# abadie et al (2010)
df = read_csv("smoking.csv")

# super minimal
theme_super_minimal = function(base_size = 14){
  theme_minimal(base_size) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())
}


# proposition 99 in 1988: 0.25$ tax on tobacco products, and other stuff
# just looking at the data here
df %>% 
  group_by(year, california) %>% 
  summarise(mean_cigsale = mean(cigsale)) %>% 
  ggplot(aes(year, mean_cigsale, color = california)) + 
  geom_line() + 
  geom_vline(xintercept = 1988)

# build the synthetic control
features = c("cigsale", "retprice")
df_wides_list = vector(mode = 'list', length = length(features))
for(i in seq_along(df_wides_list)){
  df_wides_list[[i]] = df %>% 
    filter(!after_treatment) %>% 
    select(state, year, features[i]) %>% 
    pivot_wider(names_from = state, values_from = features[i])
}

df_wide = bind_rows(df_wides_list)
df_wide = df_wide %>% select(-year)
y = df_wide[[3]] %>% as.numeric()
X = df_wide[,-3] %>% as.matrix()

# (38 x 1) vector because each non-CA state in the data gets a weight
# get the weights with ols
w = lm(y~X+0) %>% coef()

# non-CA states will be (31 x 38) matrix
non_cali = df %>% 
  filter(!california) %>% 
  select(state, year, cigsale) %>% 
  pivot_wider(names_from = state, values_from = cigsale) %>% 
  select(-year) %>% 
  as.matrix()

# create the synthetic control
# just the dot product of non_cali states and the ols coefficients
synth_cali = as.numeric(non_cali %*% w)

# plot it against CA
# looks terrible
# extrapolation :(
df %>% 
  filter(california) %>%
  select(year, cigsale) %>% 
  rename(cigsale_ca = cigsale) %>% 
  mutate(cigsale_synth = synth_cali) %>% 
  pivot_longer(cols = !year, names_to = "state", values_to = "cigsales") %>% 
  ggplot(aes(x = year, y = cigsales, color = state)) + 
  geom_line() + 
  geom_vline(xintercept = 1988)

# put all that in a function so I can compare other w's
# didn't really use this tho
add_synth_to_data = function(.data, weights, plot = TRUE){
  non_cali = .data %>% 
    filter(!california) %>% 
    select(state, year, cigsale) %>% 
    pivot_wider(names_from = state, values_from = cigsale) %>% 
    select(-year) %>% 
    as.matrix()
  synth_cali = as.numeric(non_cali %*% weights)
  d = .data %>% 
    filter(california) %>%
    select(year, cigsale) %>% 
    rename(california = cigsale) %>% 
    mutate(synthetic = synth_cali) %>% 
    pivot_longer(cols = !year, names_to = "state", values_to = "cigsales")
  if(!plot){
    return(d)
  }
  else{
    d %>% 
      ggplot(aes(x = year, y = cigsales, color = state)) + 
      geom_line() + 
      geom_vline(xintercept = 1988, lty = 2)
  }
}

# see how badly ols extrapolates?!
add_synth_to_data(.data = df, weights = w)



# ok now the abadie et al optimization method
l2norm = function(w){
  return(sqrt(mean((y - X %*% w)^2)))
}
N = nrow(X)
w_start = rep(1, N)
m1 = nloptr::slsqp(fn = l2norm,
                  x0 = w_start,
                  lower = rep(0, N),
                  upper = rep(1, N),
                  heq = function(x) sum(x) - 1)
add_synth_to_data(.data = df, weights = m1$par) + 
  theme_super_minimal() + 
  geom_line(size = 1) + 
  scale_color_manual(values = c("gray", "blue"), labels = c("California", 'Synthetic California'), name = "") + 
  theme(legend.position=c(.75, 0.75)) + 
  labs(x = "Year", y = "Cigarette Sales")


# anyway, how to do inference?
# "if we pretend the treatment happened to any other state, how often would we get an effect
## as extreme as the one we saw for CA?" basically a p-value

# now do permutation test
# loop through each state and construct its synthetic control and calculate ate(t)
permutation_test = function(.data, plot = TRUE, slsqp = TRUE, ols_start = TRUE){
  features = c("cigsale", "retprice")
  df_wides_list = vector(mode = 'list', length = length(features))
  for(i in seq_along(df_wides_list)){
    df_wides_list[[i]] = .data %>% 
      filter(!after_treatment) %>% 
      select(state, year, features[i]) %>% 
      pivot_wider(names_from = state, values_from = features[i])
  }
  df_wide = bind_rows(df_wides_list)
  df_wide = df_wide %>% select(-year)
  all_synths_list = vector(mode = 'list', length = ncol(df_wide))
  if(slsqp){
    l2norm = function(w){
      return(sqrt(mean((y - X %*% w)^2)))
    }  
  }
  else{
    l2norm = function(w){
      wstar = sqrt(mean((y - X %*% w)^2))
      return(abs(1e6 * (sum(wstar) - 1)))
    }
  }
  for(i in seq_along(all_synths_list)){
    y = df_wide[[i]] %>% as.numeric()
    X = df_wide[,-i] %>% as.matrix()
    N = nrow(X)
    lower = rep(0, N)
    upper = rep(1, N)
    if(ols_start) w_start = pmin(abs(solve(t(X) %*% X) %*% (t(X) %*% y)), 1)
    else w_start = rep(1/N, N)
    if(slsqp){
      m = nloptr::slsqp(fn = l2norm,
                        x0 = w_start,
                        lower = lower,
                        upper = upper,
                        heq = function(x) sum(x) - 1)
    } else{
      m = optim(fn = l2norm, 
            par =  w_start, 
            method = 'L-BFGS-B',
            lower = lower, 
            upper = upper)
    }
    weights = m$par
    donor_pool = .data %>% 
      filter(state != i) %>% 
      select(state, year, cigsale) %>% 
      pivot_wider(names_from = state, values_from = cigsale) %>% 
      select(-year) %>% 
      as.matrix()
    synth_state = as.numeric(donor_pool %*% weights)
    d = .data %>% 
      filter(state == i) %>%
      select(year, cigsale) %>% 
      mutate(synthetic = synth_state,
             diff = cigsale - synth_state,
             state = i)
    all_synths_list[[i]] = d
  }
  all_synths = bind_rows(all_synths_list)
  all_synths = all_synths %>% 
    mutate(california = ifelse(state == 3, 1, 0))
  if(plot){
    all_synths %>% 
      filter(state != 3) %>% 
      ggplot(aes(x = year, y = diff, group = state)) +
      geom_line(alpha = 0.5, color = 'gray') +
      geom_line(data = filter(all_synths, state == 3), aes(x = year, y = diff), color = 'blue', size = 1) +  
      geom_vline(xintercept = 1988, lty = 2) + 
      theme_super_minimal() + 
      labs(x = "Year", y = "ATE")
  }
  else return(all_synths)
}

# run fisher's test and plot
p1 = permutation_test(.data = df, plot = TRUE, slsqp = TRUE)
p1 = p1 + labs(title = "SLSQP") + ylim(-60, 120)
p2 = permutation_test(.data = df, plot = TRUE, slsqp = FALSE)
p2 = p2 + labs(title = "L-BFGS-B", ytitle = "") + ylim(-60, 120)
p_compare = p1 + p2 + 
  plot_annotation(
    title = "Comparing optimization routines for synthetic controls",
    caption = "Abadie, Diamond and Hainmueller (2010)", 
    theme = theme(plot.title = element_text(size = 18, face = 'bold')))

# do a speed test
# SLSQP vs LBFGS
speed_test = microbenchmark::microbenchmark("SLSQP" = permutation_test(df, plot = FALSE), 
                                            "L-BFGS-B" = permutation_test(df, plot = FALSE, slsqp = FALSE),
                                            times = 1e3)

# plot speed test
p_speed_test = speed_test %>% 
  ggplot(aes(x = expr, y = time/1e9, fill = expr)) + 
  geom_violin() + 
  ylim(c(0,20)) + 
  #coord_flip() + 
  scale_fill_manual(values = c("red", "orange")) + 
  theme_super_minimal() + 
  labs(x = "", y = "Time (seconds)", 
       #title = "Speed Test",
       title = "100 Permutation Tests") + 
  guides(fill = FALSE)

# combo plot
((p1 + p2) / p_speed_test) + 
  plot_annotation(
    tag_levels = c('A', '1'), # goddamn it why didn't this work?!
    title = "Optimization routines \nfor synthetic controls",
    caption = "Data from Abadie, Diamond and Hainmueller (2010)",
    theme = theme(plot.title = element_text(size = 24, face = 'bold'))) & 
  theme(plot.tag = element_text(size = 12, hjust = -2, vjust = 0, face = 'bold'))


