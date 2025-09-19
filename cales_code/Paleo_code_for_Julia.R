### Hi Julia and Bella
### Here is some code that I use to:
### 1) interpolate dates
### 2) add a weight term to time series data
### 3) manipulate data in various ways to run GAMs

### I have using pigment time-series data in the GAM section
### and am providing a couple different options for you to work through

### This follows my general work flow that I use in analyzing paleo data

### First read in the packages
library(tidypaleo) ### for dealing with dates and vertical plotting
library(tidyr) ## data management
library(dplyr) ## data transfomration
library(ggplot2) ### plotting
library(cowplot) ## for combining plots
library(mgcv) ### for GAMs 
library(gratia) ## for improving GAMs

### Interpolate 210Pb dates
## read in the .csv
dates <- read.csv(file.choose(), header=TRUE)
## I do file.choose but whatever works

## turn it into an age-depth model (adm) that tidypaleo recognizes
adm <- age_depth_model(
  dates,
  depth=Middepth.cm, age=Year
)
### You need to have the depth and ages from Flett output

## plot the age-depth model
plot(adm)
## very basic, but shows the trend

## In order to model or do any other analyses by time we 
## need to interpolate the missing dates between the estimate points
## we can do this with a predict function
interp <- predict(adm, depth = seq(0.25, 20.0, 1.0))
interp
## we set at 0.25 which is the mid-depth of our first interval 0.0-0.5
## I've put 20.0 as a hypothetical bottom date - this is the last dated interval of your core
## 1.0 is the typical resolution for pigment. You can also change the resolution to 0.5 to see
## the dates for every sediment interval

### Predict will also extrapolate dates if you extend the interval to below our dated section
### this can be done with care, but it is generally a good idea to avoid extrapolating if you can
print(interp, n=40) ## need to print to see the whole output
## now we can see the whole model with every point interpolated with a date

## save these interpolations in a new .csv
write.csv(interp, "adm_interp.csv")
## just may have to go in and add your top intveral - i.e., 0.0-0.5 if you did not date that one

##################################################################################################

## Add weight to paleo data
### This is not very elegant but it does work
## You need to have a top row of blank cells to make this work - i.e, 0 depth and year of sampling
#### read in your data (now with dates!)
paleo <- read.csv(file.choose(), header=TRUE)
### mutate your data
paleo <- paleo %>%
  mutate(interval = lag(Year) - Year) %>% ### use the interpolated dates to calculate temporal averaging
  slice(-1) %>% ## cut the top row of blank data
  mutate(weight = interval / mean(interval)) %>% ### make the weight term 
  select(basin, Core, Midpoint_depth, Year, Allo, Diato, Lutein,
         Cantha, Echine, Phaeo_B, B.car, interval, weight) ## then select whatever you need
          ## this has a series of pigments
### Now you have a weight term

### If you have multiple cores with individual dating models you can bring them together
### to model them all together, if you'd like
all_data<- bind_rows(core1, core2, core3)

###################################################################################################
### Here's some bonus code on how I plot parameters from cores together
### define your parameters beforehand for easier plotting
### the first is your column title in the .csv
## the second is the label you want in the plot. Doing it here allows for easy faceting
### Here I'm using geochemistry data
PARAMS <- c('d15N', 'd13C', 'N.', 'C.', 'CN_ratio')
PARAM.LABS <- c('δ15N ‰', 'δ13C ‰', '%N', '%C', 'C:N Ratio')

#### data read (or use what you have already modified with weights)
dat <- read.csv(file.choose(), header=TRUE)
### now pivot and manipulate the data into long form for faceting
dat.long <-
  select(dat, Lake, Middepth_cm, Year, all_of(PARAMS)) %>% ### you want the lake/core, the date, and all the data
  ### depth is optional
  pivot_longer(all_of(PARAMS), names_to = 'param', values_to = 'value') %>% ### pivot the data 'long'
  mutate(param = factor(param, levels = PARAMS), ### must be a factor for faceting
         param.lab = case_when(param == 'd15N' ~ 'δ15N ‰',
                               param == 'd13C' ~ 'δ13C ‰',
                               param == 'N.' ~ '%N',
                               param == 'C.' ~ '%C',
                               param == 'CN_ratio' ~ 'C:N Ratio') %>%
           factor(levels = c('%C', 'δ13C ‰', '%N', 'δ15N ‰', 'C:N Ratio')), ### this organizes the parameters in order
         Lake = factor(Lake, levels = c('Lake1', 'Lake2', 'Lake3'))) ### same for the lake/core
### this factor order will let us see C dynamics first, then N, which is typical
## but do what makes most sense for the data

### filter by lake if you want to look at things one at a time. 
### You don't need to do this if you want to plot everything. 
dat.l2 <- dat.long %>%
  filter(!(Lake %in% c("Lake2")))
### for example

# simply plot here
isotopes_plot <- ggplot(dat.l2, aes(Year, value)) +
  facet_grid(param.lab ~ Lake, scales = 'free_y') +### this plots each data type by lake
  ### free y shows differences between lakes and data. 
  ### We may need free x for the different time scales, but let's wait for now 
  geom_point(na.rm = TRUE) + ## basic points
  #geom_line() ## you can add lines if you want 
  ## also play around with different point shapes or line types or colours for the different
  ## isotopes or lakes in you want
  labs(x = 'Year C.E.', y = NULL) +
  theme_bw()+
  theme(strip.placement = 'outside')+
  theme(strip.background = element_blank())
isotopes_plot

### save
ggsave(plot=isotopes_plot, filename="isotopes_only2.pdf", dpi=300, scale=1)

###########################################################################################################
### next, here is code for modeling pigments
### this can be done in several ways
### the first is doing it one pigment at a time for each lake

#### Use data with interpolated dates and weights added

#### now down to modeling
### first, the simplest model using B.carotene, as an example
###
m.beta <- gam(B.car ~s(Year, k = 10), # this is simply the trend by date
              data = dat, ## the data used
              family=Gamma(link="log"), ## the family depends on the type of data. 
                  ## Use Gamma for concentration data
              bs='tp',## spline - thin plate is ok for simple models like this 
              method="REML", ## method - always use REML for gam
              weights = weight) ## here we add the weight term

# now check if the gam is good
summary(m.beta)
plot(m.beta)
gam.check(m.beta)
## ## check your edf, k, and p values
### you want to be careful that you have not over-fitted the model
### sometimes it's hard to see that until you plot it but you can fit it by lowering k
### you will get warnings if k is too high or low, and the rest is careful consideration
### Watch for the edf warning in the output


### now that you have a good model, you need to predict new data over your original data
### before we predict, we need to transform the data to deal
## with the Gamma family
fam<-family(m.beta)
fam
str(fam) 
ilink <- fam$linkinv
ilink 
Gamma()$linkinv 
ilink <- family(m.beta)$linkinv 

### now we predict 
pred <- with(dat, data.frame(Year = seq(min(Year), max(Year), ## this is making fake dates to model through
                                        length.out = 200))) ### 200-300 are usually good numbers of data points
pred <- cbind(pred,
              data.frame(predict(m.beta, pred, se.fit = TRUE))) # add the prediction to your data
## now we use the ilink to account for the transformation
pred <- transform(pred, fit = ilink(fit), 
                  upper = ilink(fit + (2 * se.fit)),
                  lower = ilink(fit - (2 * se.fit))) ## and this creates confidence intervals for your model

### now we could plot the data, but right now the model is just saying if it significantly explains the data or not
## we can figure when in time these significant changes are occurring

### this is super janky, but you can add in these functions that Gavin Simpson wrote to do this
## this function finds the derivative of the fitted trends that
## we will produce and will test if it deviates from 0, 
## i.e., is a significant deviation from the trend line
## just run the following functions. 
## I keep these saved in a separate sheet so I never accidentally change them

## First the function to derive from the trend
# JP - taking the first derivative of the trend (raw code that goes into Gavin's r package)
Deriv <- function(mod, n = 200, eps = 1e-7, newdata, term) {
  if(inherits(mod, "gamm"))
    mod <- mod$gam
  m.terms <- attr(terms(mod), "term.labels")
  if(missing(newdata)) {
    newD <- sapply(model.frame(mod)[, m.terms, drop = FALSE],
                   function(x) seq(min(x), max(x), length = n))
    names(newD) <- m.terms
  } else {
    newD <- newdata
  }
  X0 <- predict(mod, data.frame(newD), type = "lpmatrix")
  newD <- newD + eps
  X1 <- predict(mod, data.frame(newD), type = "lpmatrix")
  Xp <- (X1 - X0) / eps
  Xp.r <- NROW(Xp)
  Xp.c <- NCOL(Xp)
  ## dims of bs
  bs.dims <- sapply(mod$smooth, "[[", "bs.dim") - 1
  ## number of smooth terms
  t.labs <- attr(mod$terms, "term.labels")
  ## match the term with the the terms in the model
  if(!missing(term)) {
    want <- grep(term, t.labs)
    if(!identical(length(want), length(term)))
      stop("One or more 'term's not found in model!")
    t.labs <- t.labs[want]
  }
  nt <- length(t.labs)
  ## list to hold the derivatives
  lD <- vector(mode = "list", length = nt)
  names(lD) <- t.labs
  for(i in seq_len(nt)) {
    Xi <- Xp * 0
    want <- grep(t.labs[i], colnames(X1))
    Xi[, want] <- Xp[, want]
    df <- Xi %*% coef(mod)
    df.sd <- rowSums(Xi %*% mod$Vp * Xi)^.5
    lD[[i]] <- list(deriv = df, se.deriv = df.sd)
  }
  class(lD) <- "Deriv"
  lD$gamModel <- mod
  lD$eps <- eps
  lD$eval <- newD - eps
  lD ##return
}

### next is the function to produce confidence intervals of the derivative
confint.Deriv <- function(object, term, alpha = 0.05, ...) {
  l <- length(object) - 3
  term.labs <- names(object[seq_len(l)])
  if(missing(term)) {
    term <- term.labs
  } else { ## how many attempts to get this right!?!?
    ##term <- match(term, term.labs)
    ##term <- term[match(term, term.labs)]
    term <- term.labs[match(term, term.labs)]
  }
  if(any(miss <- is.na(term)))
    stop(paste("'term'", term[miss], "not a valid model term."))
  res <- vector(mode = "list", length = length(term))
  names(res) <- term
  residual.df <- df.residual(object$gamModel)
  tVal <- qt(1 - (alpha/2), residual.df)
  ##for(i in term.labs[term]) {
  for(i in term) {
    upr <- object[[i]]$deriv + tVal * object[[i]]$se.deriv
    lwr <- object[[i]]$deriv - tVal * object[[i]]$se.deriv
    res[[i]] <- list(upper = drop(upr), lower = drop(lwr))
  }
  res$alpha = alpha
  res
}

### this allows the previous functions to determine both positive
## and negative deviations in trend

# JP - signifD (statistically significant increase or decrease from the trent)
signifD <- function(x, d, upper, lower, eval = 0) {
  miss <- upper > eval & lower < eval
  incr <- decr <- x
  want <- d > eval
  incr[!want | miss] <- NA
  want <- d < eval
  decr[!want | miss] <- NA
  list(incr = incr, decr = decr)
}

### this is a function to plot our derivatives (though in actual plots
## we just add in other lines to make it look nicer)
plot.Deriv <- function(x, alpha = 0.05, polygon = TRUE,
                       sizer = FALSE, term,
                       eval = 0, lwd = 3,
                       col = "lightgrey", border = col,
                       ylab, xlab, main, ...) {
  l <- length(x) - 3
  ## get terms and check specified (if any) are in model
  term.labs <- names(x[seq_len(l)])
  if(missing(term)) {
    term <- term.labs
  } else {
    term <- term.labs[match(term, term.labs)]
  }
  if(any(miss <- is.na(term)))
    stop(paste("'term'", term[miss], "not a valid model term."))
  if(all(miss))
    stop("All terms in 'term' not found in model.")
  l <- sum(!miss)
  nplt <- n2mfrow(l)
  tVal <- qt(1 - (alpha/2), df.residual(x$gamModel))
  if(missing(ylab))
    ylab <- expression(italic(hat(f)*"'"*(x)))
  if(missing(xlab)) {
    xlab <- attr(terms(x$gamModel), "term.labels")
    names(xlab) <- xlab
  }
  if (missing(main)) {
    main <- term
    names(main) <- term
  }
  ## compute confidence interval
  CI <- confint(x, term = term)
  ## plots
  layout(matrix(seq_len(l), nrow = nplt[1], ncol = nplt[2]))
  for(i in term) {
    upr <- CI[[i]]$upper
    lwr <- CI[[i]]$lower
    ylim <- range(upr, lwr)
    plot(x$eval[,i], x[[i]]$deriv, type = "n",
         ylim = ylim, ylab = ylab, xlab = xlab[i], main = main[i], ...)
    if(isTRUE(polygon)) {
      polygon(c(x$eval[,i], rev(x$eval[,i])),
              c(upr, rev(lwr)), col = col, border = border)
    } else {
      lines(x$eval[,i], upr, lty = "dashed")
      lines(x$eval[,i], lwr, lty = "dashed")
    }
    abline(h = 0, ...)
    if(isTRUE(sizer)) {
      lines(x$eval[,i], x[[i]]$deriv, lwd = 1)
      S <- signifD(x[[i]]$deriv, x[[i]]$deriv, upr, lwr,
                   eval = eval)
      lines(x$eval[,i], S$incr, lwd = lwd, col = "blue")
      lines(x$eval[,i], S$decr, lwd = lwd, col = "red")
    } else {
      lines(x$eval[,i], x[[i]]$deriv, lwd = 2)
    }
  }
  layout(1)
  invisible(x)
}


## with all that, now you can derive from your model!
dt.Der<-Deriv(m.beta) ## derive
plot(dt.Der, sizer = TRUE)
Term<-"Year" 
dt.dci<-confint(dt.Der, term = Term) ## make confidence intervals
dt.dsig<-signifD(pred2$fit, d = dt.Der[[Term]]$deriv,
                 dt.dci[[Term]]$upper, dt.dci[[Term]]$lower) ### show significance

### now we make a simple plot
p.beta <- 
  ggplot(dat)+
  geom_point(aes(Year, B.car))+
  geom_ribbon(data=pred2, aes(ymin=lower, ymax=upper, x=Year), alpha=0.2)+
  geom_line(data=pred2, aes(Year, fit), size=1, color="black")+
  geom_line(data=pred2, aes(Year, dt.dsig$decr), color="#CC9933", lwd=2)+ ## this highlights what areas have significant changes
  geom_line(data=pred2, aes(Year, dt.dsig$incr), color="#CC9933", lwd=2)+
  scale_x_continuous(breaks=seq(1980,2025,by=5))+
  ylab('Beta carotene')+
  xlab('Year')+
  theme_bw()
p.beta

### so that's fine, but you likely have multiple pigments and lakes
### You can model these together in larger models
### This is a little more complex, but saves time in the long run I think, 
### And makes for pretty easy plotting

### Let's jump back here
## say you have combined all the pigments from your lakes together
lakes <- bind_rows(lake1, lake2, lake3)

##### First you pivot like we did above
lakes <- lakes %>%
  select(Core, Year, interval, weight, pigment names) %>% #list all pigments you want to model
  pivot_longer(cols = -c('basin', 'Core', 'interval', 'Year', 'weight'),
               names_to = 'pigment',
               values_to = 'conc') %>% # here we pivot the pigments
  mutate(pigment = factor(pigment)) %>%
  filter(!is.na(conc)) %>% ## remove NAs (can't model those)
  filter(Year >= 0000) %>% ## set your base year, usually the earliest shared date among the cores. 
                            ## this depends on the dating models
  mutate(core_pigment = interaction(Core, pigment, sep = '_')) %>% ## here we make a term to mark the interactions of each
  ### pigment in each core
  mutate(pigment.expr = case_when(pigment == 'Diato' ~ 'Diatoxanthin',
                                  pigment == 'Allo' ~ 'Alloxanthin',
                                  pigment == 'Phaeo_B' ~ 'Pheophytin b',
                                  pigment == 'Lutein' ~ 'Lutein-Zeaxanthin',
                                  pigment == 'Echine' ~ 'Echinenone',
                                  pigment == 'Cantha' ~ 'Canthaxanthin',
                                  pigment == 'B.car' ~ 'Beta-carotene'),
         pigment.expr = factor(pigment.expr, levels = c('Diatoxanthin',
                                                        'Alloxanthin',
                                                        'Pheophytin b',
                                                        'Lutein-Zeaxanthin',
                                                        'Echinenone',
                                                        'Canthaxanthin',
                                                        'Beta-carotene')), ### set your pigment labels and order them
         ## this is a pretty standard arrangement - by major groups and then total production last
         core = factor(core, levels = c('core1',
                                        'core2',
                                        'core3'))) #same with your cores

### test for 0
sum(lsf_l$conc == 0) 

### I like to do a little plot test to see how all the pigments look toegher
ggplot(lakes, aes(Year, conc)) +
  facet_grid(pigment.expr ~ Core, scales = 'free_y') +
  geom_point(alpha = 0.3) +
  geom_line()+
  #geom_smooth(alpha = 0.3, formula = y ~ s(x), method = 'gam') + ### this is a fake gam smooth, not really modeled
  scale_color_brewer(type = 'qual', palette = 6) + ## simple colours, doesn't really matter here
  scale_fill_brewer(type = 'qual', palette = 6)+
  labs(x = "Year", y = 'Pigment concentration')+
  theme_bw()+
  theme(strip.background = element_blank())
### just to see if you have major outliers or things like that

### presuming this is good, now we model more complexly
ml <- gam(list(conc ~ s(Year, core_pigment, bs = 'fs', k = 10), # mean formula - core_pigment through time
               ~ s(interval, bs = 'cr', k = 7) + # scale formula 
                 s(Year, core_pigment, bs = 'fs', k = 10)),
          family = gammals(), ### here we use gamma-location-scale to model this together
          data = lakes,
          weights = weight,
          method = 'REML') 
### this may take a while to run depending on the number of interactions between pigments and cores

### then check results check it out
summary(ml) 
draw(ml) 
appraise(ml) 
gam.check(ml)

### now we need to predict like before
### first making the new data
newd_l <- expand_grid(Year = seq(1900, 2020, length.out = 300), ### this is just the length of the time series
                      ### it should match your date truncation from above
                      Core = unique(lsf_l$Core), ## keeps the cores separate
                      pigment = unique(lsf_l$pigment)) %>% ## and the pigments too
  mutate(core_pigment = interaction(Core, pigment, sep = '_')) %>% ### interaction term
  left_join(group_by(lsf_l, Core) %>% summarize(interval = mean(interval)),
            by = 'Core') ### and combine


### now we can also derive significant periods using function written in this script
source('C:/Users/gushulac/Dropbox/U Manitoba/Research/SLR Project/variance-simulation-and-derivatives.R')
# I will this to you as well. Just link to it in your file tree and it should work. 

### Now we go on to test the significance of the trend through time
## There are two parts of this 
## First, slopes
slopes.mu_l <- 
  gammals_mean_deriv(model = lsf_ml, data = newd_l, var = 'Year', nsims = 1e4) %>%
  group_by(Year, core_pigment) %>%
  summarize(mu.deriv = median(derivative),
            lwr.mu.deriv = quantile(derivative, probs = 0.025),
            upr.mu.deriv = quantile(derivative, probs = 0.975),
            .groups = 'drop')

##then the means 
mu_l <- gammals_mean(model = lsf_ml, data = newd_l, nsims = 1e4) %>%
  group_by(Year, core_pigment) %>%
  summarize(mu = median(mean),
            lwr.mu = quantile(mean, probs = 0.025),
            upr.mu = quantile(mean, probs = 0.975),
            .groups = 'drop')

### now we predict, by combining the new data with the slope and means
## and then set the same factors and labels as above for easy plotting
pred_l <-
  newd_l %>%
  left_join(mu_l, by = c('Year', 'core_pigment')) %>%
  left_join(slopes.mu_l, by = c('Year', 'core_pigment')) %>%
  mutate(signif.mu = lwr.mu.deriv > 0 | upr.mu.deriv < 0) %>%
  mutate(pigment.expr = case_when(pigment == 'Diato' ~ 'Diatoxanthin',
                                  pigment == 'Allo' ~ 'Alloxanthin',
                                  pigment == 'Phaeo_B' ~ 'Pheophytin b',
                                  pigment == 'Lutein' ~ 'Lutein-Zeaxanthin',
                                  pigment == 'Echine' ~ 'Echinenone',
                                  pigment == 'Cantha' ~ 'Canthaxanthin',
                                  pigment == 'B.ca' ~ 'Beta-carotene'),
         pigment.expr = factor(pigment.expr, levels = c('Diatoxanthin',
                                                        'Alloxanthin',
                                                        'Pheophytin b',
                                                        'Lutein-Zeaxanthin',
                                                        'Echinenone',
                                                        'Canthaxanthin',
                                                        'Beta-carotene')),
         core = factor(core, levels = c('core1', 'core2', 'core3'))) %>%
  ### this last bit allows us to highlight the significant parts of the trend lines
  arrange(core_pigment) %>%
  mutate(segm.mu.bool = signif.mu != lag(signif.mu) |
           core_pigment != lag(core_pigment),
         segm.mu = 1)


## this is a short function needed to keep the same pigments slopes and means 
## separated by their groups, which is cores for us 
for(i in 2:nrow(pred_l)) {
  if(pred_l$segm.mu.bool[i]) {
    pred_l$segm.mu[i] <- pred_l$segm.mu[i - 1] + 1
  } else {
    pred_l$segm.mu[i] <- pred_l$segm.mu[i - 1]
  }
}

### now if all that worked, you will be able to make a large plot of all the pigments and cores together
big_plot <- 
  ggplot()+
  facet_grid(pigment.expr ~ core, scales='free_y')+ ## facets - pigments by core
  geom_point(aes(Year, conc), ## data - concentration by time
             lsf_l, alpha=0.5)+ ### export lsf_l_s
  geom_ribbon(data=subset(pred_l, pigment.expr=="Diatoxanthin"), ### these are the confidence intervals for each pigment
              aes(Year, ymin=lwr.mu, ymax=upr.mu, fill=Core), alpha=0.3)+ 
  geom_ribbon(data=subset(pred_l, pigment.expr=="Alloxanthin"), 
              aes(Year, ymin=lwr.mu, ymax=upr.mu, fill=Core), alpha=0.3)+
  geom_ribbon(data=subset(pred_l, pigment.expr=="Pheophytin b"), 
              aes(Year, ymin=lwr.mu, ymax=upr.mu, fill=Core), alpha=0.3)+
  geom_ribbon(data=subset(pred_l, pigment.expr=="Lutein-Zeaxanthin"), 
              aes(Year, ymin=lwr.mu, ymax=upr.mu, fill=Core), alpha=0.3)+
  geom_ribbon(data=subset(pred_l, pigment.expr=="Echinenone"), 
              aes(Year, ymin=lwr.mu, ymax=upr.mu, fill=Core), alpha=0.3)+
  geom_ribbon(data=subset(pred_l, pigment.expr=="Canthaxanthin"), 
              aes(Year, ymin=lwr.mu, ymax=upr.mu, fill=Core), alpha=0.3)+
  geom_ribbon(data=subset(pred_l, pigment.expr=="Beta-carotene"), 
              aes(Year, ymin=lwr.mu, ymax=upr.mu, fill=Core), alpha=0.3)+
  geom_line(aes(Year, mu, group=segm.mu), filter(pred_l, signif.mu), 
            lwd = 1.5) + ### signficant line
  geom_line(data=subset(pred_l, pigment.expr=="Diatoxanthin"), ### lines for each pigment
            aes(Year, mu, color=Core), size=0.75)+
  geom_line(data=subset(pred_l, pigment.expr=="Alloxanthin"), 
            aes(Year, mu, color=Core), size=0.75)+
  geom_line(data=subset(pred_l, pigment.expr=="Pheophytin b"), 
            aes(Year, mu, color=Core), size=0.75)+
  geom_line(data=subset(pred_l, pigment.expr=="Lutein-Zeaxanthin"), 
            aes(Year, mu, color=Core), size=0.75)+
  geom_line(data=subset(pred_l, pigment.expr=="Echinenone"), 
            aes(Year, mu, color=Core), size=0.75)+
  geom_line(data=subset(pred_l, pigment.expr=="Canthaxanthin"), 
            aes(Year, mu, color=Core), size=0.75)+
  geom_line(data=subset(pred_l, pigment.expr=="Beta-carotene"), 
            aes(Year, mu, color=Core), size=0.75)+
  theme(legend.position = 'none') +
  scale_color_manual(values =c("#332288", "#CC6677","#DDCC77", ### these were originally colour for individual cores
                               ### but you can make this by pigment here, or 
                               ### or set them individually in the pigment lines/ribbons above
                               "#117733", "#882255", "#999933")) +
  scale_fill_manual(values =c("#332288", "#CC6677", "#DDCC77",
                              "#117733", "#882255", "#999933")) +
  labs(x = 'Year C.E.', y = expression(Pigment~concentration~(nmol~g^{-1}~C)))+ ## expression for doing math symbols and 
  ## scientific notation
  theme(strip.background = element_blank())+ ### then just details on text and spacing
  theme(strip.text = element_text(size=11))+
  theme(axis.title = element_text(size=11))+
  theme_bw()+
  theme(strip.background = element_blank())+
  theme(panel.spacing = unit(1, "lines"))
big_plot
# save
ggsave("bigplot.pdf", big_plot, scale=1, dpi=300)

