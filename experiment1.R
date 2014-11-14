
# experiment 1 for Birmingham

N <- 2
nperiods <- 2
sessno <- 1
seed <- 47628934 + 31322789 * sessno

library(betr)
library(tidyr)
library(dplyr)

ready_fn <- function() {
  mydf <<- experiment_data_frame(N=N, periods=nperiods, dict1=NA, profit=NA, 
        rank=rep(sample(N), nperiods), partner=NA)
}

expt <- experiment(N=N, clients_in_url=TRUE, on_ready=ready_fn, seats_file=NULL,
      seed=seed)

s_instrns <- text_stage(page=b_brew("instr.brew"), wait=TRUE)

s_dict1 <- form_stage(page=b_brew("dict1.brew"), 
      fields=list(dict1=is_one_of(0:10*10)),
      titles=list(dict1="Slider"), data_frame="mydf", name="Dictator 1")

prog1 <- program(run="last", function(id, period, ...){
  p1 <- mydf$period==period
  matches <- mydf$rank[p1] %% N + 1
  mydf$partner[p1] <<- match(matches, mydf$rank[p1])
  mydf$profit[p1] <<- with(mydf[p1,], as.numeric(dict1[partner]) + 100 - 
        as.numeric(dict1))
}, name="Program 1")

final_calcs <- program(run="last",
  function(...) {
    globals <<- mydf %>% select(id, period, profit) %>% spread(period, profit)
    globals$totalprofit <<- rowSums(globals[-1])
  }, name="Final calculations")

add_stage(expt, s_instrns, period(), s_dict1, prog1,
      period(), final_calcs)

