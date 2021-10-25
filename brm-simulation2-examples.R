# simulation study 2: clustering coefficient effect in false memory
# replication of Vitevitch et al. (2012) experiment 1
# Siew. (submitted). spreadr: A R package to simulate spreading activation in a network. 

# updated 25th October 2021 for teaching demonstration 
# https://osf.io/a9bv6/ 

# give activation to neighbors, but not target
# to make sure that the total start activation is the same in all simulations
# take 100 units and divide equally across neighbors of target node
# final activation of target node

library(spreadr)
library(dplyr)
library(igraph)
options(stringsAsFactors = FALSE)

# load data

load('ego2hopnets_24.RData') # 24 networks 
v2011 <- read.csv('24toynets.csv') # degree and clustering coefficient values for each of the 24 words

# set up 

retentions <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)

# spreading activation simulation (single)

df <- data.frame(node = as.vector(neighbors(ego2hopnets_24[[1]], as.character(v2011$name[1]))$name),
                 activation = 100/length(neighbors(ego2hopnets_24[[1]], as.character(v2011$name[1]))),
                 stringsAsFactors = F)
# simulation
sim_result <- spreadr::spreadr(start_run = df, decay = 0,
                               retention = 0.5, suppress = 0,
                               network = ego2hopnets_24[[1]], time = 10)

# spreading activation simulation

t1 <- Sys.time() # 24*9 = 216 simulations

for (j in 1:9) {
  r <- retentions[j]
  for (i in 1:nrow(v2011)) {
    # make start df
    # give activation to neighbors, but not target!
    # to make sure that the total start activation is the same in all simulations,
    # take 100 units and divide equally across neighbors of target node
    df <- data.frame(node = as.vector(neighbors(ego2hopnets_24[[i]], as.character(v2011$name[i]))$name),
                     activation = 100/length(neighbors(ego2hopnets_24[[i]], as.character(v2011$name[i]))),
                     stringsAsFactors = F)
    # simulation
    sim_result <- spreadr::spreadr(start_run = df, decay = 0,
                                   retention = r, suppress = 0,
                                   network = ego2hopnets_24[[i]], time = 10)
    write.csv(sim_result, file = paste0('output/', i, '_', v2011$name[i], '_', r, '_sim2.csv'))
  }
}

Sys.time()-t1 # ~15s

# compile output - get final activation of target node in all simulations

final_act <- data.frame(name = vector(), final_act = vector(), retention = vector())

for (j in 1:9) {
  r <- retentions[j]
  for (i in 1:nrow(v2011)) {
    data <- read.csv(paste0(i, '_', v2011$name[i], '_', r, '_sim2.csv'))
    max_t <- max(data$time) # last time step
    data <- data %>% filter(time == max_t) %>% filter(node == as.character(v2011$name[i]))
    data <- data[2:3]
    data$retention <- r
    colnames(data) <- c('name', 'final_act', 'retention')
    final_act <- rbind(final_act, data)
  }
}

# regression analysis - leave data in long format

v2011_sim <- left_join(final_act, v2011, by = 'name')

m1 <- lm(final_act ~ scale(retention) + scale(degree) + scale(clustering), data = v2011_sim) 

summary(m1)

# result: words with lower clustering coefficient had higher activation, aligns with more false alarms 
