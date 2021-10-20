# Code for projecting rest of season performance.
# Date: 12/26/2020
# This analysis was used in this article:
# https://threesandlayups.com/2020/12/26/a-simple-model-for-determining-how-real-the-nets-are/
###################################################################################

# Get regular season results from 2007-2011, 2013-2019.
x <- read.csv("https://raw.githubusercontent.com/tvbassine/useful_nba_datasets/master/data_2007_2019.csv", stringsAsFactors = F)

# Get pre-season win total over/unders
w <- read.csv("https://raw.githubusercontent.com/tvbassine/useful_nba_datasets/master/preseason_win_total_ou_2007_2019.csv",
              stringsAsFactors = F)

# Get each team's point diff through j games and remaining 82 - j:
z <- data.frame(games = seq(5,60,1),
                coeff_0 = 0,
                coeff_1 = 0,
                coeff_int = 0,
                mean_abs_error = 0,
                stringsAsFactors = F)

for(j in 5:60){
  
  w$point_diff_8 <- 0
  w$point_diff_74 <- 0
  
  for(i in 1:nrow(w)){
    temp <- x[x$away_year == w$team_year[i] | x$home_year == w$team_year[i],]
    scores <- temp$h_margin
    scores[temp$away_year == w$team_year[i]] <- -1 * scores[temp$away_year == w$team_year[i]]
    w$point_diff_8[i] <- mean(scores[1:j])
    w$point_diff_74[i] <- 82 * mean(scores[(j+1):length(scores)] > 0)
  }
  
  
  # Build my model, yo:
  
  summary(fit0 <- lm(point_diff_74 ~ W.L.O.U + point_diff_8, data = w ))
  
  ind <- which(z$games == j)
  z$coeff_0[ind] = fit0$coefficients[2]
  z$coeff_1[ind] = fit0$coefficients[3]
  z$coeff_int[ind] = fit0$coefficients[1]
  
  z$mean_abs_error[ind] = mean(abs(fit0$residuals))
  
  print(ind)
  
}


# What does this model says about projecting performance after 10 games?

# Create a matrix of point differentials and play through 10 games:
a = expand.grid(prior = seq(22, 60, 1),
            point_diff_10 = seq(-10,10,.5),
            stringsAsFactors = F)
a$post <- a$prior * z$coeff_0[6] + a$point_diff_10 * z$coeff_1[6] + z$coeff_int[6]

View(a[a$prior == 52,])

plot(a$point_diff_10[a$prior == 52],
     a$post[a$prior == 52],
     xlab = 'Point Differential in First 10 Games',
     ylab = 'Projected Win Pace in Remaining 72 Games',
     type = 'l',
     main = 'How to Project the Remainder of the \nSeason for a 52 Win Projected Team',
     lwd = 2,
     col = 'blue')

for(i in seq(42,55,1)){
  abline(h = i, lty =2)
}

for(i in seq(-10,10,1)){
  abline(v = i, lty =2)
}

######################################################################
# Plot projection lines on same graph:

a = expand.grid(prior = seq(22, 60, 1),
                point_diff_10 = seq(-10,10,.5),
                stringsAsFactors = F)

# After 10 games:
a$post <- a$prior * z$coeff_0[6] + a$point_diff_10 * z$coeff_1[6] + z$coeff_int[6]

View(a[a$prior == 52,])

plot(a$point_diff_10[a$prior == 52],
     a$post[a$prior == 52],
     xlab = 'Point Differential So Far',
     ylab = 'Projected Win Pace for the Remainder of the Season',
     type = 'l',
     main = 'Updated Projected Win Pace For A Team Projected\n To Win 52 Games Before The Season',
     lwd = 2,
     col = 'blue',
     xlim = c(-5,10),
     ylim = c(35, 60))

for(i in seq(35,60,5)){
  abline(h = i, lty =2, lwd = .5)
}

for(i in seq(-5,10,1)){
  abline(v = i, lty =2, lwd = .5)
}

# After 20 games:
a$post <- a$prior * z$coeff_0[16] + a$point_diff_10 * z$coeff_1[16] + z$coeff_int[16]

lines(a$point_diff_10[a$prior == 52],
     a$post[a$prior == 52],
     type = 'l',
     lwd = 2,
     col = 'red')


# After 30 games:
a$post <- a$prior * z$coeff_0[26] + a$point_diff_10 * z$coeff_1[26] + z$coeff_int[26]

lines(a$point_diff_10[a$prior == 52],
     a$post[a$prior == 52],
     lwd = 2,
     col = 'green')


# After 40 games:
a$post <- a$prior * z$coeff_0[36] + a$point_diff_10 * z$coeff_1[36] + z$coeff_int[36]

lines(a$point_diff_10[a$prior == 52],
     a$post[a$prior == 52],
     lwd = 2,
     col = 'purple')

text(-3, 48, 'After 10 Games Played', col = 'blue')
text(-3, 44, 'After 20 Games Played', col = 'red')
text(-3, 40, 'After 30 Games Played', col = 'green')
text(-2, 37, 'After 40 Games Played', col = 'purple')
