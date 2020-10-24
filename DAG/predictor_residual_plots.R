(1) **Predictor residual plots**. These plots show the outcome against residual predictor values. They are useful for understanding the statistical model, but not much else. 

A **predictor residual** is the average prediction error when we use all of the other predictor variables to model a predictor of interest.

The variation that is not expected by the model of the mean, μ, is a function of the other predictors.


Example: 
  
  a) compute predictor residuals for marriage rate: 
  
  $M_{i} ∼ Normal(\mu_{i}, \sigma)$ 
  
  $\mu_{i} = \alpha + \beta A_{i}$
    
    
    ```{r m5.4}
  #To compute predictor residuals for marriage rate, we just use the other predictor (age of marriage) to model it.
  m5.4 <- quap(
    alist(
      M ~ dnorm( mu , sigma ) ,
      mu <- a + bAM * A ,
      a ~ dnorm( 0 , 0.2 ) ,
      bAM ~ dnorm( 0 , 0.5 ) ,
      sigma ~ dexp( 1 )
    ) , data = d )
  
  ```
  
  We compute the **predictor residuals** by subtracting the observed marriage rate in each State from the predicted rate, based upon the model above
  
  ```{r m5.4 2}
  mu.m <- link(m5.4)
  mu_mean.m <- apply( mu.m , 2 , mean )
  mu_resid.m <- d$M - mu_mean.m
  
  d.m <- bind_cols( d, estimate.m= mu_mean.m, mu_resid.m= mu_resid.m)
  
  pres.m <- 
    d.m %>% 
    ggplot(aes(Loc, mu_resid.m))+
    geom_point()+
    geom_hline(yintercept=0)+
    theme_bw()
  pres.m
  ```
  
  
  The residuals are variation in marriage rate that is left over, after taking out the purely linear relationship between the two variables. When a residual is positive, that means that the observed rate was in excess of what the model expects, given the median age at marriage in that State. When a residual is negative, that means the observed rate was below what the model expects. Example:  States with positive residuals have high marriage rates for their median age of marriage, while States with negative residuals have low rates for their median age of marriage.
  
  
  ```{r residual posterior plot M variation adjusted for A}
  #https://bookdown.org/content/4857/the-many-variables-the-spurious-waffles.html#plotting-multivariate-posteriors.
  
  
  p1 <-
    d.m %>% 
    ggplot(aes(x = A, y = M)) +
    geom_point(size = 2, shape = 1, color = "firebrick4") +
    geom_segment(aes(xend = A, yend = estimate.m), 
                 size = 1/4) +
    geom_line(aes(y = estimate.m), 
              color = "firebrick4") +
    labs(x = "Age at marriage (std)",
         y = "Marriage rate (std)") +
    geom_text_repel(data = . %>% filter(Loc %in% c("WY", "ND", "ME", "HI", "DC")),  
                    aes(label = Loc), 
                    size = 3, seed = 14) +
    coord_cartesian(ylim = range(d.m$M)) +
    theme_bw() +
    theme(panel.grid = element_blank()) 
  
  p1
  ```
  
  Plot these residuals against divorce rate, overlaying the linear regression of the two variables.You can think of this plot as displaying the linear relationship between divorce and marriage rates, having conditioned already on median age of marriage. The vertical dashed line indicates marriage rate that exactly matches the expectation from median age at marriage. So States to the right of the line have higher marriage rates than expected. States to the left of the line have lower rates. Average divorce rate on both sides of the line is about the same, and so the regression line demonstrates little relationship between divorce and marriage rates.
  
  ```{r residual posterior plot M variation against D}
  #https://bookdown.org/content/4857/the-many-variables-the-spurious-waffles.html#plotting-multivariate-posteriors.
  
  
  p2 <-
    d.m %>% 
    ggplot(aes(x = mu_resid.m, y = D)) +
    stat_smooth(method = "lm", fullrange = T,
                color = "firebrick4", fill = "firebrick4", 
                alpha = 1/5, size = 1/2) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
    geom_point(size = 2, color = "firebrick4", alpha = 2/3) +
    geom_text_repel(data = . %>% filter(Loc %in% c("WY", "ND", "ME", "HI", "DC")),  
                    aes(label = Loc), 
                    size = 3, seed = 5) +
    scale_x_continuous(limits = c(-2, 2)) +
    coord_cartesian(xlim = range(d.m$mu_resid.m)) +
    labs(x = "Marriage rate residuals",
         y = "Divorce rate (std)") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  
  p2
  ```
  
  
  b) compute predictor residuals for median age of marriage: 
  
  $A_{i} ∼ Normal(\mu_{i}, \sigma)$ 
  
  $\mu_{i} = \alpha + \beta M_{i}$
    
    To compute predictor residuals for median age of marriage, we just use the other predictor (marriage rate) to model it.
  
  ```{r m5.5}
  
  m5.5 <- quap(
    alist(
      A ~ dnorm( mu , sigma ) ,
      mu <- a + bMA * M ,
      a ~ dnorm( 0 , 0.2 ) ,
      bMA ~ dnorm( 0 , 0.5 ) ,
      sigma ~ dexp( 1 )
    ) , data = d )
  
  ```
  
  We compute the **predictor residuals** by subtracting the observed median age of marriage in each State from the predicted median age of marriage, based upon the model above
  
  ```{r m5.5 2}
  mu.a <- link(m5.5)
  mu_mean.a <- apply( mu.a , 2 , mean )
  mu_resid.a <- d$A - mu_mean.a
  
  d.a <- bind_cols( d, estimate.a= mu_mean.a, mu_resid.a= mu_resid.a)
  
  pres.a <- 
    d.a %>% 
    ggplot(aes(Loc, mu_resid.a))+
    geom_point()+
    geom_hline(yintercept=0)+
    theme_bw()
  pres.a
  ```
  
  
  Regression of A on M and the residuals: the residuals are variation in median age of marriage that is left over, after taking out the purely linear relationship between the two variables. When a residual is positive, that means that the observed age was in excess of what the model expects, given the marriage rate in that State. When a residual is negative, that means the observed are was below what the model expects.
  
  
  ```{r residual posterior plot A variation adjusted for M}
  #https://bookdown.org/content/4857/the-many-variables-the-spurious-waffles.html#plotting-multivariate-posteriors.
  
  
  p3 <-
    d.a %>% 
    ggplot(aes(x = M, y = A)) +
    geom_point(size = 2, shape = 1, color = "firebrick4") +
    geom_segment(aes(xend = M, yend = estimate.a), 
                 size = 1/4) +
    geom_line(aes(y = estimate.a), 
              color = "firebrick4") +
    labs(x = "Age at marriage (std)",
         y = "Marriage rate (std)") +
    geom_text_repel(data = . %>% filter(Loc %in% c("DC", "HI", "ID")),  
                    aes(label = Loc), 
                    size = 3, seed = 14) +
    coord_cartesian(ylim = range(d.a$A)) +
    theme_bw() +
    theme(panel.grid = element_blank()) 
  
  p3
  ```
  
  States to the right of the vertical dashed line have older-than-expected median age at marriage, while those to the left have younger-than-expected median age at marriage. Now we find that the average divorce rate on the right is lower than the rate on the left, as indicated by the regression line. States in which people marry older than expected for a given rate of marriage tend to have less divorce.
  
  ```{r residual posterior plot A variation against D}
  #https://bookdown.org/content/4857/the-many-variables-the-spurious-waffles.html#plotting-multivariate-posteriors.
  
  
  p4 <-
    d.a %>% 
    ggplot(aes(x = mu_resid.a, y = D)) +
    stat_smooth(method = "lm", fullrange = T,
                color = "firebrick4", fill = "firebrick4", 
                alpha = 1/5, size = 1/2) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
    geom_point(size = 2, color = "firebrick4", alpha = 2/3) +
    geom_text_repel(data = . %>% filter(Loc %in% c("ID", "HI", "DC")),  
                    aes(label = Loc), 
                    size = 3, seed = 5) +
    scale_x_continuous(limits = c(-2, 3)) +
    coord_cartesian(xlim = range(d.a$mu_resid.a),
                    ylim = range(d.a$D)) +
    labs(x = "Age at marriage residuals",
         y = "Divorce rate (std)") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  
  p4
  ```
  So what’s the point of all of this? There’s conceptual value in seeing the model-based predictions displayed against the outcome, after subtracting out the influence of other pre- dictors. The plots in Figure 5.4 do this. But this procedure also brings home the message that regression models measure the remaining association of each predictor with the out- come, after already knowing the other predictors. In computing the predictor residual plots, you had to perform those calculations yourself. 
  
  
  (2) **Posterior prediction plots**. These show model-based predictions against raw data,
  or otherwise display the error in prediction. They are tools for checking fit and
  assessing predictions. They are not causal tools.