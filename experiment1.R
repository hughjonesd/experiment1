
# experiment 1 for Birmingham

# TODO:
# instructions: privacy etc... payments... stages separate...
# HG: how? 
# TG?
# friends: adjust frcount in friendships.brew
# real class1.txt file etc
# put it up on website
# check on iPad
# titles for each stage, for pupils

N <- 2
sessno <- 1
seed <- c(175804510L, 326704365L, 215164818L, 425463189L, 30750106L, 
      35380967L, 36912668L, 86165470L, 850662828L, 6737400L)[sessno] 
classno <- 1
classfile <- c("class1.txt", "class2.txt")[classno]
classnames <- scan(classfile, what="character", sep="\n", quiet=TRUE)

library(betr)
library(tidyr)
library(dplyr)

ready_fn <- function() {
  mydf <<- experiment_data_frame(expt, dict1=NA, offer2=NA,
        accept2=NA, accepted2=NA, profit=NA, friends=NA, 
        myfriend1=NA, myfriend2=NA, myfriend3=NA,
        rank=sample(N), role=NA, pair=NA, stringsAsFactors=FALSE)
  mydf$friends <- I(mydf$friends) # allow a list
  globals <<- NA
}

expt <- experiment(N=N, clients_in_url=TRUE, on_ready=ready_fn, seats_file=NULL,
      seed=seed, randomize_ids=TRUE)

s_instrns <- text_stage(page=b_brew("instr.brew"), wait=TRUE)

s_dict <- form_stage(page=b_brew("dict1.brew"), 
      fields=list(dict1=is_one_of(0:10*10)),
      titles=list(dict1="Amount to give"), data_frame="mydf", 
      name="Dictator Game")

s_prog_dict <- program(run="last", 
  function(id, period, ...){
    pd <- mydf$period == period
    pair <- rep(1:floor(N/2), 2)
    if (N %% 2 > 0) pair <- c(pair, 1)
    pair <- sample(pair)
    role <- rep("A", N)
    role[!duplicated(pair)] <- "B"
    mydf$role[pd] <<- role
    mydf$pair[pd] <<- pair
    for (pr in mydf$pair[pd]) {
      given <- as.numeric(mydf$dict1[pd & mydf$pair[pd]==pr & 
            mydf$role[pd]=="A"])
      mydf$profit[pd & mydf$pair[pd]==pr & mydf$role[pd]=="A"] <<- 100 - given
      mydf$profit[pd & mydf$pair[pd]==pr & mydf$role[pd]=="B"] <<- given
    }
  }, 
  name="DG profit calculations")

s_prog_ug_prepare <- program(run="first",
  function(id, period, ...){
    pd <- mydf$period==period
    # put everyone in pairs with a pair number. One "pair" may have 3.
    # the first of each pair is B. Others are A.
    # everyone plays both roles and is matched with one person of the other
    # role in their pair.
    # any "extra" person is an A, so one B may respond to two offers.
    pair <- rep(1:floor(N/2), 2)
    if (N %% 2 > 0) pair <- c(pair, 1)
    pair <- sample(pair)
    role <- rep("A", N)
    role[!duplicated(pair)] <- "B"
    mydf$role[pd] <<- role
    mydf$pair[pd] <<- pair
  },
  name="UG setup")

s_ug <- form_stage(page=b_brew("ug2.brew"),
      fields=list(offer2=is_one_of(0:10*10), accept2=is_one_of(0:10*10)),
      titles=list(offer2="Amount to offer", accept2="Minimum amount to accept"), 
      data_frame="mydf", name="Ultimatum Game")

s_prog_ug <- program(run="last", 
  function(id, period, ...){
    pd <- mydf$period==period
    mydf$profit[pd] <- 0
    # for each B: match with 1/2 As. Figure out payments for all
    mydfp <- mydf[pd,]
    for (pr in mydfp$pair) {
      thresh <- as.numeric(mydfp$accept2[mydfp$role=="B" & mydfp$pair==pr])
      # may be 2 offers
      offer <- as.numeric(mydfp$offer2[mydfp$role=="A" & mydfp$pair==pr])
      mydf$accepted2[pd & mydf$role=="A" & mydf$pair==pr] <<- offer >= thresh
      mydf$profit[pd & mydf$role=="A" & mydf$pair==pr] <<- 
            (100 - offer) * (offer >= thresh)
      mydf$profit[pd & mydf$role=="B" & mydf$pair==pr] <<- 
            sum(offer * (offer >= thresh))
    }
  }, 
  name="UG profit calculations")

frcheck <- function(title, values, id, period, params) {
  if (length(values)==1) return("Please tick more than one checkbox to show
    who else hangs around with this pupil.")
  if (all(values %in% classnames)) return(NULL)
  wrong <- setdiff(frs, classnames)
  return(paste("Unrecognized pupil names: ", paste(wrong, collapse=", "), 
        sep=""))
}

 
s_friends <-  form_stage(
      page=b_brew("friendships.brew"),
      fields=list(friends=frcheck), titles=list(friends="Groups of friends"),
      data_frame="mydf", multi_params="AsIs",
      name="Questionnaire: friendship networks")

myfrcheck <- function (title, value, id, period, params) {
  friends <- params[nchar(params)>0]
  if (anyDuplicated(friends)) 
        return("Please choose different names in all 3 boxes, 
        or leave them blank")
  return(NULL)
}

s_myfriends <- form_stage(page=b_brew("myfriends.brew"),
  fields=list(myfriend1=myfrcheck, myfriend2=myfrcheck, myfriend3=myfrcheck),
  titles=list(myfriend1="Friend 1", myfriend2="Friend 2", myfriend3="Friend 3"),
  data_frame="mydf",
  name="Questionnaire: my friends")

s_final_calcs <- program(run="first",
  function(...) {
    globals <<- mydf %>% select(id, period, profit) %>% spread(period, profit)
    globals$totalprofit <<- rowSums(globals[-1], na.rm=TRUE)
  }, 
  name="Final calculations")

s_show_result <- text_stage(page=b_brew("results.brew"), name="Final results")

add_stage(expt, 
      s_instrns, 
      period(wait_for="all"), s_dict, s_prog_dict, 
      period(wait_for="all"), s_prog_ug_prepare, s_ug, s_prog_ug,
  #    period(wait_for="all"), s_prog_hg_prepare, s_hg3, s_prog3,
      period(wait_for="all"), s_friends, 
      period(wait_for="none"), s_friends, 
      period(wait_for="none"), s_friends, 
      period(wait_for="none"), s_myfriends, 
      period(wait_for="all"), s_final_calcs, s_show_result)

