
N <- 27
dfrs <- list()
for (stage in 1:3) {
  dfr <- data.frame(seat=sample(1:N))
  pairs <- rep(1:floor(N/2), 2)
  if (N %% 2) pairs <- c(1, pairs)
  pairs <- sample(pairs)
  dfr$pair <- pairs
  role <- rep("A", N)
  role[!duplicated(pairs)] <- "B"
  dfr$other[role=="B"] <- sapply(dfr$pair[role=="B"], function(x) paste(which(role=="A" & dfr$pair==x), collapse=","))
  dfr$other[role=="A"] <- sapply(dfr$pair[role=="A"], function(x) which(role=="B" & dfr$pair==x)[1])
  dfr$role <- role
  dfr <- dfr[order(dfr$seat),]
  dfr$pair <- NULL 
  rownames(dfr) <- 1:N
  dfrs[[stage]] <- dfr
  cat("Stage ", stage, " data frame in dfrs[[", stage, "]]:\n")
  print(dfr)
}




