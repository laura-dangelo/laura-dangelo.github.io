library(ggplot2)

load("Cuckoo.rda")

Cuckoo


plot(as.numeric(Cuckoo$Bird), Cuckoo$Length)

ggplot(Cuckoo, aes(x=Bird, y=Length, fill=Bird)) +
  geom_boxplot(alpha = 0.3) +
  geom_jitter(aes(color=Bird), size=1.3, alpha=0.8, width = 0.1, shape =3, stroke=1) +
  theme_bw() +
  theme(
    legend.position="none",
    axis.text=element_text(size=12),
    axis.title=element_text(size=12)
  ) +
  xlab("Bird species")


# we restrict interest to robins and wrens
data = Cuckoo[(Cuckoo$Bird == "robin")|(Cuckoo$Bird == "wren"),]
rownames(data) = 1:nrow(data)
data

ggplot(data, aes(x=Bird, y=Length, fill=Bird)) +
  geom_boxplot(alpha = 0.3) +
  geom_jitter(aes(color=Bird), size=1.3, alpha=0.8, width = 0.1, shape =3, stroke=1) +
  theme_bw() +
  theme(
    legend.position="none",
    axis.text=element_text(size=12),
    axis.title=element_text(size=12)
  ) +
  xlab("Bird species")



# we start with the two-sample t-test 
# we compute the group-specific sample size, means, variances

n = sum(data$Bird=="robin")
n

m = sum(data$Bird=="wren")
m


mean_robin = mean(data$Length[data$Bird == "robin"])
mean_robin

mean_wren = mean(data$Length[data$Bird == "wren"])
mean_wren


var_robin = var(data$Length[data$Bird == "robin"])
var_robin

var_wren = var(data$Length[data$Bird == "wren"])
var_wren

var_est = ((n-1)*var_robin + (m-1)*var_wren) / (n+m-2)
var_est 

t_obs = (mean_wren - mean_robin) / sqrt( var_est * (m+n) / (m*n) )
t_obs

pvalue = 2 * pt(abs(t_obs), n+m-2, lower.tail = FALSE)
pvalue


## we can do the same test using a simple linear model

simple_lm = lm(Length ~ Bird, data = data)

# let's see how the Bird variable (characters) is encoded into a numeric variable
X = model.matrix(simple_lm)
X

summary(simple_lm)

ggplot(data.frame("x" = X[,2], "y" = data$Length), aes(x = x, y = y, col = x)) + 
  scale_color_gradient(low="salmon", high = "turquoise3") +
  geom_jitter(width = 0.02) + 
  geom_abline(slope = coef(simple_lm)[["Birdwren"]], 
              intercept = coef(simple_lm)[["(Intercept)"]] ) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  xlab("x") +
  scale_x_continuous(breaks = c(0,1), labels=c(0,1), limits = c(-0.2,1.2))


coef(simple_lm)["(Intercept)"]
mean_robin

coef(simple_lm)["(Intercept)"] + coef(simple_lm)["Birdwren"]
mean_wren


