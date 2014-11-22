
# experiment 1 for Birmingham

# TODO:
# paper consent forms
# randomizers for entry

# COMPUTER TODO:
# friends: adjust frcount in friendships.brew
# real class1.txt file etc
# dealing with withdrawals: reload, re-enter N.
# emphasize PRIVATE questions
# simple language (consent form)
# advisory timer

ciu <- TRUE
showup <- 20 # fee in pence. NB: this is already on their desk, so we don't
# include in the totalprofit figure
N <- as.numeric(readline("Enter this session's N: "))
sessno <- as.numeric(readline("Enter the number of this session (1-10): "))
seed <- c(175804510L, 326704365L, 215164818L, 425463189L, 30750106L, 
      35380967L, 36912668L, 86165470L, 850662828L, 6737400L)[sessno] 
classno <- as.numeric(readline("Enter the class number: "))
classfile <- paste0("class", classno, ".txt")
classnames <- sort(scan(classfile, what="character", sep="\n", quiet=TRUE))
surnames <- sub(".* ", "", classnames) # an assumption here
firstnames <- sub(" .*", "", classnames)
classnames <- classnames[order(surnames, firstnames)]
cat("Session number is", sessno, "and class number is", classno, ".\n")
cat("First 3 class names:", paste(classnames[1:3], collapse=", "), ".\n")

library(betr)
library(reshape2)

ready_fn <- function() {
  mydf <<- experiment_data_frame(expt, dict1=NA, offer2=NA,
        accept2=NA, accepted2=NA, hchoice=NA, coinflip=NA, coinflip.real=NA,
        profit=NA, friends=NA, 
        myfriend1=NA, myfriend2=NA, myfriend3=NA,
        friendlike1=NA, friendlike2=NA, friendlike3=NA,
        myname=NA, myname2=NA,
        rank=sample(N), role=NA, pair=NA, stringsAsFactors=FALSE)
  globals <<- NA
}

expt <- experiment(N=N, clients_in_url=ciu, on_ready=ready_fn, seats_file=NULL,
      seed=seed, randomize_ids=TRUE, autostart=TRUE, client_refresh=30)

s_consent <- text_stage(page=b_brew("consent.brew"), wait=TRUE, name="Consent")
s_rules <- text_stage(page=b_brew("rules.brew"), wait=TRUE, name="Rules")
s_instr <- text_stage(page=b_brew("instr.brew"), wait=TRUE, name="Instructions")
s_instr2 <- text_stage(page=b_brew("instr2.brew"), wait=TRUE, name="Instructions 2")

s_dict <- form_stage(page=b_brew("dict1.brew"), 
      fields=list(dict1=is_one_of(0:10*10)),
      titles=list(dict1="Amount to give"), data_frame="mydf", 
      name="Dictator Game")

s_prog_dict <- program(run="last", 
  function(id, period, ...){
    pd <- mydf$period == period
    mydf$profit[pd] <<- 0
    pair <- rep(1:floor(N/2), 2)
    if (N %% 2 > 0) pair <- c(pair, 1)
    pair <- sample(pair)
    role <- rep("A", N)
    role[!duplicated(pair)] <- "B"
    mydf$role[pd] <<- role
    mydf$pair[pd] <<- pair
    for (pr in unique(mydf$pair[pd])) {
      given <- as.numeric(mydf$dict1[pd & mydf$pair[pd]==pr & 
            mydf$role[pd]=="A"])
      mydf$profit[pd & mydf$pair[pd]==pr & mydf$role[pd]=="A"] <<- 100 - given
      mydf$profit[pd & mydf$pair[pd]==pr & mydf$role[pd]=="B"] <<- 
            sum(given)
    }
  }, 
  name="DG profit calculations")

s_ug <- form_stage(page=b_brew("ug2.brew"),
      fields=list(offer2=is_one_of(0:10*10)),
      titles=list(offer2="Amount to offer"), 
      data_frame="mydf", 
      name="Ultimatum Game part 1")

s_ug_cont <- form_stage(page=b_brew("ugcont.brew"),
  fields=list(accept2=is_one_of(0:10*10)),
  titles=list(accept2="Minimum amount to accept"), 
  data_frame="mydf", 
  name="Ultimatum Game part 2")

s_prog_ug <- program(run="last", 
  function(id, period, ...){
    pd <- mydf$period==period
    mydf$profit[pd] <<- 0
    pair <- rep(1:floor(N/2), 2)
    if (N %% 2 > 0) pair <- c(pair, 1)
    pair <- sample(pair)
    role <- rep("A", N)
    role[!duplicated(pair)] <- "B"
    mydf$role[pd] <<- role
    mydf$pair[pd] <<- pair
    # for each B: match with 1/2 As. Figure out payments for all
    mydfp <- mydf[pd,]
    for (pr in na.omit(unique(mydfp$pair))) {
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


s_ig <- form_stage(
      page=b_brew("integrity.brew"),
      fields=list(hchoice=is_one_of(c("keep", "give")), coinflip=is_one_of(
      c("no", "heads", "tails")), coinflip.real=is_one_of(c("no", "heads",
      "tails"))), titles=list(hchoice="Choice", coinflip="Coin flip", 
      coinflip.real=""),
      data_frame="mydf",
      name="Integrity game")

s_prog_ig <- program(run="last",
  fn=function(id, period, ...) {
    pd <- mydf$period==period
    mydf$profit[pd] <<- 0
    pair <- rep(1:floor(N/2), 2)
    if (N %% 2 > 0) pair <- c(pair, 1)
    pair <- sample(pair)
    role <- rep("A", N)
    role[!duplicated(pair)] <- "B"
    mydf$role[pd] <<- role
    mydf$pair[pd] <<- pair
    # for each B: match with 1/2 As. Figure out payments for all.
    for (pr in na.omit(unique(mydf$pair))) {
      # may be 2 As each giving or not
      mydf$profit[pd & mydf$role=="A" & mydf$pair==pr] <<- 100 * 
            (mydf$hchoice[pd & mydf$role=="A" & mydf$pair==pr] == "keep")
      mydf$profit[pd & mydf$role=="B" & mydf$pair==pr] <<- 100 *
        sum(mydf$hchoice[pd & mydf$role=="A" & mydf$pair==pr] == "give")
    }
  },
  name="IG profit calculations")

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
      data_frame="mydf", multi_params="paste",
      name="Questionnaire: friendship networks")

myfrcheck <- function (title, value, id, period, params) {
  friends <- params[nchar(params)>0]
  if (anyDuplicated(friends)) 
        return("Please choose different names in all 3 boxes, 
        or leave them blank")
  return(NULL)
}

nocheck <- function(...) NULL

s_myfriends <- form_stage(page=b_brew("myfriends.brew"),
      fields=list(myfriend1=myfrcheck, myfriend2=nocheck, myfriend3=nocheck),
      titles=list(myfriend1="Friend 1", myfriend2="Friend 2", myfriend3="Friend 3"),
      data_frame="mydf",
      name="Questionnaire: my friends")

s_friends_like <- form_stage(page=b_brew("friendslike.brew"),
      fields=list(friendlike1=myfrcheck, friendlike2=nocheck, friendlike3=nocheck),
      titles=list(friendlike1="Friend 1", friendlike2="Friend 2", friendlike3="Friend 3"),
      data_frame="mydf",
      name="Questionnaire: friends I'd like")

namecheck <- function(title, value, id, period, params) {
  if (value == "" && params$myname2 == "") return("Please enter your name") 
  return(NULL)
}

s_qnaire <- form_stage(page=b_brew("qnaire.brew"),
      fields=list(myname=namecheck, myname2=nocheck),
      titles=list(myname="Name", myname2=""),
      data_frame="mydf",
      name="Questionnaire: other")
  
s_final_calcs <- program(run="first",
  function(...) {
    globals <<- dcast(melt(mydf[,c("id", "period", "profit")], id=1:2), 
          id ~ period)
    globals$totalprofit <<- rowSums(globals[-1], na.rm=TRUE)
    globals$totalprofit <<- globals$totalprofit
    write_data(expt, mydf)
  }, 
  name="Final calculations")

s_show_result <- text_stage(page=b_brew("results.brew"), name="Final results")

add_stage(expt, checkpoint(),
      s_consent, checkpoint(), s_rules, checkpoint(), s_instr,
      checkpoint(), s_instr2,
      period(wait_for="none"), s_dict, s_prog_dict, 
      period(wait_for="none"), s_ug, checkpoint("none"), s_ug_cont, s_prog_ug,
      period(wait_for="none"), s_ig, s_prog_ig,
      period(wait_for="none"), s_friends, 
      period(wait_for="none"), s_friends, 
      period(wait_for="none"), s_friends, 
      period(wait_for="none"), s_myfriends, s_friends_like, s_qnaire,
      period(wait_for="all"), s_final_calcs, s_show_result
      )

load_commands(expt)

