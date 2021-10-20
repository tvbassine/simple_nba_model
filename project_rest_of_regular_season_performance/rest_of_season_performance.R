setwd("~/Desktop/NBA Projection System/data")
x <- read.csv("data_2007_2019.csv", stringsAsFactors = F)


years <- c(2007:2011,2013:2019)
out <- list()

for(i in 1:length(years)){
  yr <- years[i]
  file = paste('~/Desktop/NBA Projection System/data/preseason_', yr, '.csv', sep = '')
  out[[i]] <- read.csv(file, stringsAsFactors = F)
  out[[i]]$year <- yr
}

w <- do.call('rbind', out)

w$team_year <- paste(w$Team, w$year, sep = '_')

w$pd <- 0

# Get point diff over full season:
for(i in 1:nrow(w)){
  temp <- x[x$away_year == w$team_year[i] | x$home_year == w$team_year[i],]
  scores <- temp$h_margin
  scores[temp$away_year == w$team_year[i]] <- -1 * scores[temp$away_year == w$team_year[i]]
  w$pd[i] <- mean(scores)
}



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
  w$point_diff_74[i] <- mean(scores[(j+1):length(scores)])
}


# Build my model, yo:

# Everything converted into point diff model:
w$point_diff_0 <- (1/2.5) * (w$W.L.O.U - 41)
summary(fit0 <- lm(point_diff_74 ~ point_diff_0 + point_diff_8, data = w ))

ind <- which(z$games == j)
z$coeff_0[ind] = fit0$coefficients[2]
z$coeff_1[ind] = fit0$coefficients[3]
z$coeff_int[ind] = fit0$coefficients[1]

z$mean_abs_error[ind] = mean(abs(fit0$residuals))

print(ind)

}

plot(z$games, z$coeff_0, type = 'b', col = 'blue',
     ylim = c(0,1), pch = 19,
     main = 'How To Project Rest Of Regular Season Performance?',
     ylab = 'Regression Coefficient',
     xlab = 'Games Played')
lines(z$games, z$coeff_1, type = 'b', col = 'red',
     ylim = c(0,1), pch = 19)
for(i in seq(0,1,.1)){
  abline(h = i, lty = 2, lwd = .5)
}
text(30,.15, 'Preseason Betting Market Wins', col = 'blue')
text(30,.75, 'Current Point Differential', col = 'red')

plot(z$games, z$mean_abs_error)


###############################################################
