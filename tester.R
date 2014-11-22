<<<<<<< HEAD
#!/usr/bin/Rscript
library(RCurl)

N <- 19
while (TRUE) {
  for (i in 1:N) getURL(paste0("http://54.194.17.233/betr/betr/client-", i))
  Sys.sleep(sample(1))
}

# plan:
# create a test case:
# Simplify experiment1 to bare minimum (brumdebug branch)
# Simplify betr (brumdebug branch)
# Reliably reproduce bug
# Record what does/doesn't trigger it
