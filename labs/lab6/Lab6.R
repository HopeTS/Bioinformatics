## Robby Bergers
## Lab 6

library(ggplot2)
library(cowplot)
library(dplyr)


# 1a:
  p = 0.5
  a1.s = 498 + 437
  a1.phat = 498 / a1.s
  a1.mean = p
  a1.sd = sd = sqrt(p*(1-p)/a1.s)
  a1.sd
  a1.mean
  
# 1b:
  b1.mu = p
  b1.df.phat = data.frame(x = seq(b1.mu-4*a1.sd, b1.mu+4*a1.sd, length.out = 100)) %>%
    mutate(y = dnorm(x, mean = b1.mu, sd = a1.sd))
  b1.plot.phat = ggplot(b1.df.phat) + geom_line(aes(x, y)) +
    theme_classic() + ggtitle("a1.phat ~ N(0.5, sqrt((.5)(.5)/935") +
    labs(x = "a1.phat", y = "density") +
    geom_vline(xintercept = a1.phat) +
    geom_text(aes(x=a1.phat-.04, y=6), label = "observed phat")
  b1.plot.phat
  
# 1c:
  c1.z = (a1.phat - b1.mu)/a1.sd
  c1.z
  c1.df.z = data.frame(x = seq(-4,4, length.out = 100)) %>%
    mutate(y=dnorm(x))
  c1.plot.z = ggplot(c1.df.z) + geom_line(aes(x, y)) +
    theme_classic() + ggtitle("Z ~ N(0,1)") +
    labs(x = "Z", y = "Density") +
    geom_vline(xintercept = c1.z) +
    geom_text(aes(x=c1.z-.8, y = .25), label = "Observed Z")
  c1.plot.z
  
# 1d:
  d1.p = prop.test(498, 935, p = .50, correct = FALSE)
  d1.p$statistic
  sqrt(d1.p$statistic)
  d1.p$p.value 
  
# 1e:
  print('With the p value, the null hypothesis that American adults generally prioritize law and order over reducing bias is rejected.')
  
# 1f:
  print('We conclude that the majority of Americans do not prioritize law and order, when in reality, the majority do prioritize law and order.')
  
  
# 2a:
  print('If h0 is true, and we roll a standard die 50 times, the psychic will successfully predict the outcome 12 times.')
  print('The alternative hypothesis is that the psychic will not be able to successfully predict the outcome 12 times.')
  
# 2b:
  b2.res = prop.test(12, 50, p = (1/6), correct = TRUE)
  sqrt(b2.res$statistic)
  
# 2c:
  b2.res$p.value
  
# 2d:
  print('The null hypothesis that the psychic is able to correectly predict the outcome of rolling a die 12 times is rejected because p = 0.166, while p hat = 0.229.')
  
# 2e:
  print('We conclude that the psychic is able to correctly predict the outcome of a die roll 12 times out of 50 when in reality the psychic is not able to correctly predict the outcome 12 times out of 50.')
  
# 3:
  # Null Hypothesis
  print('If h0 is true, then the percentage of adults that use cash for most purchases has decreased over the past 8 years.')
  # Alternative hypothesis
  print('If the data does not support h0, then the percentage of adults that use cash for most purchases has not increased over the past 8 years.')
  # z test statistic
  three.res = prop.test(246, 1024, p = .36, correct = TRUE)
  sqrt(three.res$statistic)
  # p value
  three.res$p.value
  # Conclusion
  print('The p value is substantially less than p, therefore the hypothesis that the percentage of Americans that use cash for most purchases has decreased over the past 8 years is rejected.')
  
# 4a:
  a4.z = abs(3.32)
  a4.pval = 2*(0.05)*(-a4.z)
  a4.pval
  print('The null hypothesis would be rejected.')
  
# 4b:
  b4.z = abs(-1.3)
  b4.pval = 2*(0.05)*(-b4.z)
  b4.pval
  print('The null hypothesis would be rejected.')
  
# 4c:
  b4.z = abs(-2.02)
  b4.pval = 2*(0.05)*(-b4.z)
  b4.pval
  print('The null hypothesis would be rejected.')

  