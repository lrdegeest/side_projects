# hash demos
library(digest)
hash1 <- digest("This lecture is awesome", algo = "sha256")
hash2 <- digest("This lecture is boring", algo = "sha256")
hash3 <- digest("What is this lecture even about and why am I here", algo = "sha256")
print(hash1)
print(hash2)
print(hash3)
nchar(hash1); nchar(hash2); nchar(hash3)

# calculate probability of attack (Nakamoto 2008)
attack_threat <- function(q,z){
  p = 1 - q
  lambda = z*(q/p)
  poisson = exp(-lambda)
  a = q/p
  getP <- function(k){
    poisson = ((lambda^k)*exp(-lambda)) / factorial(k)
    power = z-k 
    P = poisson*(1-a^power)
    return(P)
  }
  values <- seq(0,z,1)
  output <- sapply(values, function(k) getP(k))
  P = 1 - sum(output)
  return(P)
}

blocks = seq(0,24,1)
for(i in c(0.15, 0.25, 0.35, 0.45, 0.50)) {
  assign(paste0("q",i), sapply(blocks, function(z) attack_threat(i,z)))
}
df <- data.frame(blocks, q0.15, q0.25, q0.35, q0.45, q0.5)
df.long <- reshape2::melt(df, id="blocks")

ggplot2::ggplot(df.long, aes(blocks, value, color=variable)) + 
  geom_line(size=1) + 
  labs(x="Blocks", y = "P(rehash the chain)") + 
  theme_classic() + 
  scale_y_continuous(limits=c(0,1), breaks = c(0.00, 0.25, 0.5, 0.75, 1.00)) + 
  scale_color_discrete(name="Network control",
                       labels=c("15%", "25%", "35%", "45%", "50%")) + 
  theme(legend.justification=c(1,0), legend.position=c(0.85,0.15)) 


library(igraph)
g <- barabasi.game(100)
V(g)$size <- degree(g, mode="all")*2
plot(g,vertex.label= NA, edge.arrow.size=0.02, vertex.color="orange", vertex.frame.color="#ffffff")

plot_degree_distribution = function(graph) {
  # this function c/o https://chengjunwang.com/web_data_analysis/demo2_simulate_networks/
  # calculate degree
  d = degree(graph, mode = "all")
  dd = degree.distribution(graph, mode = "all", cumulative = FALSE)
  degree = 1:max(d)
  probability = dd[-1]
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  # plot
  plot(probability ~ degree, log = "xy", xlab = "Degree (log)", ylab = "Probability (log)", 
       col = "orange", pch=19, main = "Degree Distribution")
}