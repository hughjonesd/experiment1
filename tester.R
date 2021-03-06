#!/usr/bin/Rscript

library(RCurl)
N <- 16
pn <- commandArgs(TRUE)
if (length(pn)) N <- pn
cat(commandArgs(), "\n")
while (TRUE) {
  res <- sapply(1:N, function(i) 
          getURL(paste0("http://54.194.17.233/betr/betr/client-", i)))
   if (! any(grepl("betr", res))) cat("Error\n") else cat("OK\n")
}

# plan:
# create a test case:
# Simplify experiment1 to bare minimum (brumdebug branch)
# Simplify betr (brumdebug branch)
# Reliably reproduce bug
# Record what does/doesn't trigger it
