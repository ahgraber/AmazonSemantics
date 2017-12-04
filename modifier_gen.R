# function that assigns sentiment modifier to an average rating score
modifier_gen <- function(x, upr, lwr) {
  r <- vector(mode = "integer", length = length(x))
  for (i in 1:length(x)) {
    if (x[i] >= upr) {
      r[i] <- 1
    } else if (x[i] > lwr) {
      r[i] <- 0
    } else {
      r[i] <- -1
    }
  }
  return(r)
}

### for reference, the below would also work
  # r <- data_frame(x) %>%
  #   mutate(score = case_when(
  #     avg_rating >= upr ~ 1,
  #     avg_rating > lwr ~ 0,
  #     avg_rating <= lwr ~ -1
  #     )
  #   ) %>%
  #   select(score)
  # 
  # return(r)
