
# experiment 1 for Birmingham

N <- 1

library(betr)
library(tidyr)
library(dplyr)

ready_fn <- function() {
  mydf <<- experiment_data_frame(N=N, periods=2, dict1=NA)
}

expt <- experiment(N=N, clients_in_url=TRUE, on_ready=ready_fn, seats_file=NULL)

s_instrns <- text_stage(page=b_brew("instr.brew"), wait=TRUE)

s_dict1 <- form_stage(page=b_brew("dict1.brew"), 
      fields=list(dict1=is_one_of(0:10*10)),
      titles=list(dict1="Slider"), data_frame="mydf", name="Dictator 1")

prog1 <- program(run="last", function(...){
  p1 <- mydf$period==1
  mydf$match[p1] <- sample(1:N)
  mydf$profit[p1] <- mydf$dict1[p1][ mydf$match[p1] ]
  mydf$profit[p1] <- with(mydf[p1,], as.numeric(profit) + 100 - as.numeric(dict1))
})

final_calcs <- program(run="last",
  function(...) {
    globals <<- mydf %>% select(id, period, profit) %>% spread(period, profit, 
          convert=TRUE)
    globals$totalprofit <<- rowSums(globals[-1])
  }, name="Final calculations")

add_stage(expt, s_instrns, period(), s_dict1, prog1,
      period(), final_calcs)
ready(expt)
web_test(expt)

