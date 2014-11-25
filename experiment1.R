
# experiment 1 for Birmingham

ciu <- TRUE
testmode <- TRUE # for auth
countdown <- 120 # 2 mins before you hassle subjects
N <- as.numeric(readline("Enter this session's N: "))
sessno <- as.numeric(readline("Enter the number of this session (1-10): "))
seed <- c(175804510L, 326704365L, 215164818L, 425463189L, 30750106L, 
      35380967L, 36912668L, 86165470L, 850662828L, 6737400L)[sessno] 
classno <- as.numeric(readline("Enter the class number: "))
shn <- read.csv("Shell names info.csv", stringsAsFactors=FALSE)
classcode <- c("SHP", "SHJ", "SHM", "SHB", "SHF")[classno]
shn <- shn[shn$Reg==classcode,]
classnames <- paste(shn$Forename, shn$Surname)
classnames <- classnames[order(shn$Surname, shn$Forename)]
cat("Session number is", sessno, "and class is", classcode, ".\n")
cat("First 3 class names:", paste(classnames[1:3], collapse=", "), ".\n")

library(betr)
library(reshape2)

ready_fn <- function() {
  mydf <<- experiment_data_frame(expt, dict1=NA, offer2=NA,
        accept2=NA, accepted2=NA, hchoice=NA, coinflip=NA, coinflip.real=NA,
        profit=NA, ngroups=NA, friends=NA, myfriends=NA, friendslike=NA,
        myname=NA, myname2=NA,
        guessname1=NA, guessname2=NA, guessname3=NA,
        guess1=NA, guess2=NA, guess3=NA,
        paidguess=NA,
        rank=sample(N), role=NA, pair=NA, stringsAsFactors=FALSE)
  globals <<- NA
  timeout <<- NA
}

auth_fn <- function (ip, params, cookies) {
  if (testmode) return(TRUE)
  if (ip %in% "91.125.232.165") return(TRUE)
  if ("betr-seat" %in% names(cookies)) return(TRUE)
  return(FALSE)
}


expt <- experiment(N=N, clients_in_url=ciu, on_ready=ready_fn, auth=auth_fn,
      seed=seed, randomize_ids=TRUE, autostart=TRUE, client_refresh=5)

# CHECK FUNCTIONS ===================


nocheck <- function(...) NULL

namecheck <- function(title, value, id, period, params) {
  if (value == "" && params$myname2 == "") return("Please enter your name") 
  return(NULL)
}

frcheck <- function(title, values, id, period, params) {
  if (length(values)==1) return("Please tick more than one checkbox to show
    who else hangs around with this pupil.")
  if (all(values %in% classnames)) return(NULL)
  wrong <- setdiff(frs, classnames)
  return(paste("Unrecognized pupil names: ", paste(wrong, collapse=", "), 
    sep=""))
}

frlikecheck <- function(title, values, id, period, params) {
  if (length(values) > 3) return("Please only tick up to 3 pupils")
  if (all(values %in% classnames)) return(NULL)
  wrong <- setdiff(frs, classnames)
  return(paste("Unrecognized pupil names: ", paste(wrong, collapse=", "), 
    sep=""))
}

pupilcheck <- function(title, values, id, period, params) {
  if (all(values %in% classnames)) return(NULL)
  wrong <- setdiff(frs, classnames)
  return(paste("Unrecognized pupil names: ", paste(wrong, collapse=", "), 
    sep=""))
}





# STAGES ================

s_consent <-text_stage(page=b_brew("consent.brew"), wait=TRUE, name="Consent")
s_rules <-  text_stage(page=b_brew("rules.brew"), wait=TRUE, name="Rules")
s_instr <-  text_stage(page=b_brew("instr.brew"), wait=TRUE, name="Instructions")
s_instr2 <- text_stage(page=b_brew("instr2.brew"), wait=TRUE, name="Instructions 2")
s_instr3 <- text_stage(page=b_brew("instr3.brew"), wait=TRUE, name="Instructions 3")

s_prog_timer <- program(run="all", function(id, period) {
  timeout[id] <<- as.numeric(Sys.time()) + countdown 
}, name="Start timer")


s_instr_dict <- text_stage(page=b_brew("instr_dict.brew"), wait=TRUE, 
      name="Stage 1 Instructions")

s_dict <- form_stage(page=b_brew("dict1.brew"), 
      fields=list(dict1=is_one_of(0:10*10)),
      titles=list(dict1="Amount to give"), data_frame="mydf", 
      name="Stage 1 Dictator Game")

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
      given <- as.numeric(mydf$dict1[pd & mydf$pair[pd]==pr & mydf$role[pd]=="A"])
      mydf$profit[pd & mydf$pair[pd]==pr & mydf$role[pd]=="A"] <<- 100 - given
      mydf$profit[pd & mydf$pair[pd]==pr & mydf$role[pd]=="B"] <<- sum(given)
    }
  }, 
  name="DG profit calculations")

s_instr_ug <- text_stage(page=b_brew("instr_ug.brew"), wait=TRUE, 
      name="Stage 2 Part 1 Instrns")

s_ug <- form_stage(page=b_brew("ug2.brew"),
      fields=list(offer2=is_one_of(0:10*10)),
      titles=list(offer2="Amount to offer"), 
      data_frame="mydf", 
      name="Stage 2 UG, Part 1")

s_instr_ugcont <- text_stage(page=b_brew("instr_ugcont.brew"), wait=TRUE, 
      name="Stage 2 Part 2 Instrns")

s_ug_cont <- form_stage(page=b_brew("ugcont.brew"),
      fields=list(accept2=is_one_of(0:10*10)),
      titles=list(accept2="Minimum amount to accept"), 
      data_frame="mydf", 
      name="Stage 2 UG, Part 2")

s_prog_ug <- program(run="last", 
  function(id, period, ...){
    pd <- mydf$period==period
    mydf$profit[pd] <<- 0
    pair <- mydf$pair[pd]
    pdp <- pair[duplicated(pair)] 
    pair[duplicated(pair)] <- (pdp+ .5) %% max(pair) + .5
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

s_instr_ig <- text_stage(page=b_brew("instr_ig.brew"), wait=TRUE, 
  name="Stage 3 Instructions")

s_ig <- form_stage(
      page=b_brew("integrity.brew"),
      fields=list(hchoice=is_one_of(c("keep", "give")), coinflip=is_one_of(
      c("no", "heads", "tails")), coinflip.real=is_one_of(c("no", "heads",
      "tails"))), titles=list(hchoice="Choice", coinflip="Coin flip", 
      coinflip.real=""),
      data_frame="mydf",
      name="Stage 3 Integrity Game")

s_prog_ig <- program(run="last",
  fn=function(id, period, ...) {
    pd <- mydf$period==period
    mydf$profit[pd] <<- 0
    pair <- mydf$pair[pd]
    pdp <- pair[duplicated(pair)] 
    pair[duplicated(pair)] <- (pdp+ .5) %% max(pair) + .5
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


write_payment_data <- function() {
  globals <<- dcast(melt(mydf[,c("id", "period", "profit")], id=1:2), 
    id ~ period)
  globals$totalprofit <<- rowSums(globals[-1], na.rm=TRUE)
  globals$totalprofit <<- globals$totalprofit
  globals <<- merge_subjects(expt, globals)[,c("seat", "id", "IP", "client",
    "totalprofit")]
  globals <<- globals[order(globals$seat, globals$id),]
  payfile <- paste0("session-", sessno, "-paydata.csv")
  write.csv(globals, file=payfile)
  cat("Payment data written to", sQuote(payfile), ".\n")
  cat("Look at 'globals' to display it now.\n")
}

s_q_intro <- text_stage(page=b_brew("q_intro.brew"), name="Questionnaire Intro")

s_qnaire <- form_stage(page=b_brew("qnaire.brew"),
      fields=list(myname=namecheck, myname2=nocheck),
      titles=list(myname="Name", myname2=""),
      data_frame="mydf",
      name="Questionnaire: name")

s_prog_prepare_guess <- program(run="first", 
  fn=function(id, period, ...) {
    pd <- mydf$period==period
    mydf$myname[pd] <<- ifelse (nzchar(mydf$myname[pd]), mydf$myname[pd], mydf$myname2[pd])
    mynames <- mydf$myname[pd]
    l <- length(mynames)
    mydf$guessname1[pd] <<- mynames[c(2:l, 1)]
    mydf$guessname2[pd] <<- mynames[c(3:l, 1:2)]
    mydf$guessname3[pd] <<- mynames[c(4:l, 1:3)]
  },
  name="Prepare guess names")

s_guess <- form_stage(page=b_brew("guesses.brew"),
      fields=list(guess1=is_one_of(0:10*10), guess2=is_one_of(0:10*10), 
      guess3=is_one_of(0:10*10)),
      titles=list(guess1="Guess 1", guess2="Guess 2", guess3="Guess 3"),
      data_frame="mydf",
      name="Questionnaire: guesses")

s_prog_guess <- program(run="last", 
  fn=function(id, period, ...) {
    pd <- mydf$period==period
    g <- sample(1:3, 1)
    aim <- sapply(mydf$id[pd], function(id, g) {
      guessfield <- paste0("guessname", g)
      guessname <- mydf[pd & mydf$id==id, guessfield]
      tgtid <- mydf$id[pd & mydf$myname == guessname]
      tguess <- mydf$dict1[mydf$period==1 & mydf$id==tgtid]
      return(tguess == mydf[,paste0("guess", g)])
    }, g=g)
    mydf$profit[pd] <<- mydf$profit[pd] + 50 * aim
    mydf$paidguess[pd] <<- g
  },
  name="Calculate guess profits")

s_prog_paydata <- program(run="last", write_payment_data,  name="Write payment data")

s_friendsintro <-  form_stage(
  page=b_brew("friends_intro.brew"),
  fields=list(ngroups=is_one_of(-1:4)), 
  titles=list(ngroups="Number of groups in your class"),
  data_frame="mydf", multi_params="paste",
  name="Questionnaire: friends intro")

frpagefn <- function(id, period, params, errors) {
  ng <- mydf$ngroups[mydf$id==id & ! is.na(mydf$ngroups)]
  done <- FALSE
  # 0 or -1 also gets hit by this
  if (length(na.omit(mydf$friends[mydf$id==id])) >= ng) return(NEXT)
  # if they couldn't think of anyone else, and it's not the first qn
  if (is.na(mydf$friends[mydf$id==id & mydf$period==period-1]) &&
        is.na(mydf$ngroups[mydf$id==id & mydf$period==period])) return(NEXT) 
  return(b_brew("friendships.brew")(id, period, params, errors))
}

s_friends <-  form_stage(
      page=frpagefn,
      fields=list(friends=frcheck), 
      titles=list(friends="Groups of friends"),
      data_frame="mydf", multi_params="paste",
      name="Questionnaire: friendship networks")

s_myfriends <- form_stage(page=b_brew("myfriends.brew"),
      fields=list(myfriends=pupilcheck),
      titles=list(myfriends="My Friends"),
      data_frame="mydf", multi_params="paste",
      name="Questionnaire: my friends")

s_friends_like <- form_stage(page=b_brew("friendslike.brew"),
      fields=list(friendslike=frlikecheck),
      titles=list(friendslike="Friends I would like"),
      data_frame="mydf", multi_params="paste",
      name="Questionnaire: friends I'd like")

s_final_calcs <- program(run="first",
  function(...) {
    fdata <- merge_subjects(expt, mydf)
    write_data(expt, fdata)
    write_payment_data()
  }, 
  name="Final calculations")

s_show_result <- text_stage(page=b_brew("results.brew"), name="Final results")

add_stage(expt, checkpoint(),
      s_consent, s_rules,  s_instr, s_instr2,  s_instr3,
      period(wait_for="all"), s_instr_dict, checkpoint(), 
      s_prog_timer, s_dict, s_prog_dict, 
      period(wait_for="all"), s_instr_ug, checkpoint(),
      s_prog_timer, s_ug, checkpoint(), 
      s_instr_ugcont, checkpoint(),
      s_prog_timer, s_ug_cont, s_prog_ug,
      period(wait_for="all"), s_instr_ig, checkpoint(), 
      s_prog_timer, s_ig, s_prog_ig, 
      s_q_intro, 
      s_prog_timer, s_qnaire, 
      s_prog_timer, s_guess, s_prog_paydata,
      period(wait_for="none"), s_prog_timer, s_friendsintro, s_friends, 
      period(wait_for="none"), s_prog_timer, s_friends, 
      period(wait_for="none"), s_prog_timer, s_friends, 
      period(wait_for="none"), s_prog_timer, s_friends, 
    #  period(wait_for="none"), s_prog_timer, s_friends, 
    #  period(wait_for="none"), s_prog_timer, s_friends, 
      period(wait_for="none"), s_prog_timer, s_myfriends, s_prog_timer, 
      s_friends_like, 
      period(wait_for="all"), s_final_calcs, s_show_result
      )

load_commands(expt)
SAFENEXT <- function() {
  if (! all(sapply(expt$stages[expt$subjects$stage], class) %in% 
          "TextStage") || ! all(expt$stages[expt$subjects$stage]$wait)) {
    message("Some subjects are not at 'wait' TextStages. Not moving on.")
    return(invisible())
  }
  next_stage(expt)
}

