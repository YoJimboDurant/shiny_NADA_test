# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  # First we create a reactive function that is reactive to either distribution changes or pressing the action button
  datasetInput <- reactive({
    if(input$distribution=='Log-Normal'){
      isolate({
        
        # total number of samples
        totalObs <- input$obs1 + input$obs2 + input$obs3
        
        #generate data
        dist <- rlnorm(totalObs * input$simNum, input$meanlog, input$sdlog)
        
        #generate censoring values
        cenValue1 <- qlnorm(input$censQ1, meanlog = input$meanlog, input$sdlog)
        cenValue2 <- qlnorm(input$censQ2, meanlog = input$meanlog, input$sdlog)
        cenValue3 <- qlnorm(input$censQ3, meanlog = input$meanlog, input$sdlog)

        # permute the order just for good measure
        cenValues <- c(rep(cenValue1, input$obs1), rep(cenValue2, input$obs2), rep(cenValue3, input$obs3))
        cenValue <- cenValues
        cenValue <- sample(cenValues, totalObs, replace=FALSE)
        
        cenValue <- rep(cenValue, times = input$simNum)
        cen = cenValue > dist
        
        # True mean and Var
        pop.mean <- exp(input$meanlog + 0.5 *input$sdlog ^2)
        pop.var <- (exp(input$sdlog^2)-1) * exp(2 *input$meanlog +input$sdlog ^2)
        
        # Assemble data frame
       
        dfx <- data.frame(value=dist, cen=cen, cenValue=cenValues)
        dfx$obs <- dfx$value
        dfx$obs[dfx$cen] <-cenValue[dfx$cen]
        dfx$obs_zero <- dfx$value
        dfx$obs_zero[dfx$cen] <- 0
        dfx$obs_half <-dfx$value
        dfx$obs_half[dfx$cen] <- 0.5 * cenValue[dfx$cen]
        dfx$obs_full <- dfx$value
        dfx$obs_full[dfx$cen] <- cenValue[dfx$cen]
        dfx$obs_sqrt2 <- dfx$value
        dfx$obs_sqrt2[dfx$cen] <- cenValue[dfx$cen]/sqrt(2)
        dfx$run <- input$myValue
        dfx$sampleNum <- rep(1:input$simNum, each=totalObs)
      
      })
      # this makes it reactive to the action button
      myvalue <- input$myValue1
     
    }
    
    if(input$distribution=='Gamma'){
      isolate({
        dist <- rgamma(input$obs * input$simNum, shape=input$shape, scale=input$scale)
        cenValue1 <- qgamma(input$cenRate1, shape=input$shape, scale=input$scale)
        cenValue2 <- qgamma(input$cenRate2, shape=input$shape, scale=input$scale)
        cenValue3 <- qgamma(input$cenRate3, shape=input$shape, scale=input$scale)
        cenValues <- c(cenValue1, cenValue2, cenValue3)
        mylength <- input$obs*input$simNum
        myweights <- c(input$cenWeight1, input$cenWeight2, input$cenWeight3)
        myweights <- myweights/sum(myweights)
        cenValue <- sample(cenValues, mylength, replace=TRUE, prob=myweights)
        cen = cenValue > dist
        pop.mean <- input$shape * input$scale
        pop.var <- input$shape * input$scale^2
        
        dfx <- data.frame(value=dist, cen=cen, cenValue=cenValue)
        dfx$obs <- dfx$value
        dfx$obs[dfx$cen] <-cenValue[dfx$cen]
        dfx$obs_zero <- dfx$value
        dfx$obs_zero[dfx$cen] <- 0
        dfx$obs_half <-dfx$value
        dfx$obs_half[dfx$cen] <- 0.5 * cenValue[dfx$cen]
        dfx$obs_full <- dfx$value
        dfx$obs_full[dfx$cen] <- cenValue[dfx$cen]
        dfx$obs_sqrt2 <- dfx$value
        dfx$obs_sqrt2[dfx$cen] <- cenValue[dfx$cen]/sqrt(2)
        dfx$run <- input$myValue
        dfx$sampleNum <- rep(1:input$simNum, input$obs)
      })
      myvalue <- input$myValue1
    }
    
    if(input$distribution=='Normal'){
      isolate ({
        dist <- rnorm(input$obs * input$simNum, mean=input$mu, sd = input$sd)
        cenValue1 <- qnorm(input$cenRate1, mean = input$mu, sd =input$sd)
        cenValue2 <- qnorm(input$cenRate2, mean = input$mu, sd =input$sd)
        cenValue3 <- qnorm(input$cenRate3, mean = input$mu, sd = input$sd)
        cenValues <- c(cenValue1, cenValue2, cenValue3)
        mylength <- input$obs*input$simNum
        myweights <- c(input$cenWeight1, input$cenWeight2, input$cenWeight3)
        myweights <- myweights/sum(myweights)
        cenValue <- sample(cenValues, mylength, replace=TRUE, prob=myweights)
        cen = cenValue > dist
        pop.mean = input$mu
        pop.var=input$sd^2
        
        dfx <- data.frame(value=dist, cen=cenValue>dist, cenValue=cenValue)
        dfx$obs <- dfx$value
        dfx$obs[dfx$cen] <- cenValue[dfx$cen]
        dfx$obs_zero <- dfx$value
        dfx$obs_zero[dfx$cen] <- 0
        dfx$obs_half <-dfx$value
        dfx$obs_half[dfx$cen] <- 0.5 * cenValue[dfx$cen]
        dfx$obs_full <- dfx$value
        dfx$obs_full[dfx$cen] <- cenValue[dfx$cen]
        dfx$obs_sqrt2 <- dfx$value
        dfx$obs_sqrt2[dfx$cen] <- cenValue[dfx$cen]/sqrt(2)
        dfx$run <- input$myValue
        dfx$sampleNum <- rep(1:input$simNum, input$obs)
      })
      
      myvalue <- input$myValue1
      
    }
    
    # calculate means and SD by method
    sample.results <-ddply(dfx, .(sampleNum), summarise, sample.mean=mean(value), sample.sd = sd(value))
    ros.results <- dlply(dfx, .(sampleNum), function(dfx) ros(dfx$obs,dfx$cen))
    ros.mean <- ldply(ros.results, mean)
    ros.sd <-ldply(ros.results, sd)
    
    km.results <- dlply(dfx, .(sampleNum), function(dfx) cenfit(dfx$obs, dfx$cen))
    km.mean <- ldply(km.results, mean)
    km.sd <-ldply(km.results, sd)
    
    mle.results <- dlply(dfx, .(sampleNum), function(dfx) cenmle(dfx$obs, dfx$cen))
    mle.mean <- ldply(mle.results, mean)
    mle.sd <-ldply(mle.results, sd)
    
    # calculate substituted means and SD
    zero.results <- ddply(dfx, .(sampleNum), summarise, sample.mean=mean(obs_zero), sample.sd = sd(obs_zero))
    half.results <- ddply(dfx, .(sampleNum), summarise, sample.mean=mean(obs_half), sample.sd = sd(obs_half))
    full.results <- ddply(dfx, .(sampleNum), summarise, sample.mean=mean(obs_full), sample.sd = sd(obs_full))
    sqrt2.results <- ddply(dfx, .(sampleNum), summarise, sample.mean=mean(obs_sqrt2), sample.sd = sd(obs_sqrt2))
    
    #because I want to use censummary later, I return a list packed with the data frame of results and the initial data
    
    results = list(results = data.frame(
      sample.mean =sample.results$sample.mean,
      sample.sd = sample.results$sample.sd,
      ros.mean=ros.mean$V1,
      ros.sd=ros.sd$V1,
      km.mean=km.mean$mean,
      km.sd=km.sd$V1,
      mle.mean=mle.mean$mean,
      mle.sd=mle.sd$V1,
      sub0.mean = zero.results$sample.mean,
      sub0.sd = zero.results$sample.sd,
      subHalf.mean = half.results$sample.mean,
      subHalf.sd = half.results$sample.sd,
      subFull.mean = full.results$sample.mean,
      subFull.sd = full.results$sample.sd,
      #  ,subSqrt2.mean = sqrt2.results$sample.mean,
      # subSqrt2.sd = sqrt2.results$sample.sd
      pop.mean = rep(pop.mean, input$simNum),
      pop.var = rep(pop.var, input$simNum)
    ),
                   data = dfx
    )
    
    
    return(results)
  })
  
  
  # this serves the results to the UI using output
  output$Meanview <- renderTable({
    results <- datasetInput()
    results <- results$results
    summary(results[grep("mean", names(results))])
  })
  
  output$SDview <- renderTable({
    results <- datasetInput()
    results <- results$results
    summary(results[grep("sd", names(results))])
  })
  
  output$Meangraph <- renderPlot({
    results <- datasetInput()
    results <- results$results
    results <- results[grep("mean", names(results))]
    boxplot(results[!grepl("pop.", names(results))],
            col="light blue",
            ylab="Mean",      # y axis label
            xlab="Censoring Method",    # x axis label
            main="Simulated Means",    # graphic title
            las=1,                # controls the orientation of the axis labels (1=horizontal)
            par(list(cex=0.8)))
    abline(h=results$pop.mean[1])
  })
  
  output$SDgraph <- renderPlot({
    results <- datasetInput()
    results <- results$results
    boxplot(results[grep("sd", names(results))],
            ylab="Std. Dev.",
            xlab="Censoring Method",
            main="Simulated Std. Dev.",
            las=1,
            par(list(cex=0.8)),
            col="light blue")
    abline(h=sqrt(results$pop.var[1]))
  })
  
  output$distGraph <-renderPlot({
    if(input$distribution=='Log-Normal'){
      .x <- seq(qlnorm(0.01, input$meanlog, input$sdlog), qlnorm(0.99, input$meanlog, input$sdlog), length.out=100)
      plot(.x, dlnorm(.x, meanlog=input$meanlog, sdlog=input$sdlog), xlab="x", ylab="Density",
           type="l")
      abline(h=0, col="gray")
    }
    
    if(input$distribution=='Gamma'){
      .x <- seq(qgamma(0.01, shape=input$shape, scale=input$scale),qgamma(0.99, shape=input$shape, scale=input$scale) , length.out=100)
      plot(.x, dgamma(.x, shape=input$shape, scale=input$scale), xlab="x", ylab="Density",
           type="l")
      abline(h=0, col="gray")
    }
    
    if(input$distribution=='Normal'){
      .x <- seq(qnorm(0.01, mean=input$mu, sd=input$sd),qnorm(0.99, mean=input$mu, sd=input$sd), length.out=100)
      plot(.x, dnorm(.x, mean=input$mu, sd=input$sd), xlab="x", ylab="Density",
           type="l")
      abline(h=0, col="gray")
    }
    
  })
  
  output$RMSEplot.mean <- renderPlot({
    results <- datasetInput()
    results <- results$results
    sampleRMSE <- sqrt(sum(((results$sample.mean - results$pop.mean)/results$pop.mean)^2)/input$simNum)
    rosRMSE <- sqrt(sum(((results$ros.mean - results$pop.mean)/results$pop.mean)^2)/input$simNum)
    kmRMSE <- sqrt(sum(((results$km.mean - results$pop.mean)/results$pop.mean)^2)/input$simNum)
    mleRMSE <- sqrt(sum(((results$mle.mean - results$pop.mean)/results$pop.mean)^2)/input$simNum)
    sub0RMSE <- sqrt(sum(((results$sub0.mean - results$pop.mean)/results$pop.mean)^2)/input$simNum)
    subHalfRMSE <- sqrt(sum(((results$subHalf.mean - results$pop.mean)/results$pop.mean)^2)/input$simNum)
    subFullRMSE <- sqrt(sum(((results$subFull.mean - results$pop.mean)/results$pop.mean)^2)/input$simNum)
    
    barplot(c(sampleRMSE, rosRMSE, kmRMSE, mleRMSE, sub0RMSE, subHalfRMSE, subFullRMSE),
            names.arg=c("Sample", "ROS", "KM", "MLE", "Sub \nZero", "Sub \nHalf", "Sub \nFull"),
            main="Mean RMSE", ylab="Mean RMSE", col="light blue")
    abline(h=0, col="light grey", lty=2)
  })
  
  output$RMSEplot.sd <- renderPlot({
    results <- datasetInput()
    results <- results$results
    results$pop.sd <-sqrt(results$pop.var)
    samplesdRMSE <- sqrt(sum(((results$sample.sd - results$pop.sd)/results$pop.sd)^2)/input$simNum)
    rossdRMSE <- sqrt(sum(((results$ros.sd - results$pop.sd)/results$pop.sd)^2)/input$simNum)
    kmsdRMSE <- sqrt(sum(((results$km.sd - results$pop.sd)/results$pop.sd)^2)/input$simNum)
    mlesdRMSE <- sqrt(sum(((results$mle.sd - results$pop.sd)/results$pop.sd)^2)/input$simNum)
    sub0sdRMSE <- sqrt(sum(((results$sub0.sd - results$pop.sd)/results$pop.sd)^2)/input$simNum)
    subsdHalfRMSE <- sqrt(sum(((results$subHalf.sd - results$pop.sd)/results$pop.sd)^2)/input$simNum)
    subsdFullRMSE <- sqrt(sum(((results$subFull.sd - results$pop.sd)/results$pop.sd)^2)/input$simNum)
    
    barplot(c(samplesdRMSE, rossdRMSE, kmsdRMSE, mlesdRMSE, sub0sdRMSE, subsdHalfRMSE, subsdFullRMSE),
            names.arg=c("Sample", "ROS", "KM", "MLE", "Sub \nZero", "Sub \nHalf", "Sub \nFull"),
            main="Std. Dev. RMSE", ylab="Std. Dev. RMSE", col="light blue")
    
    abline(h=0, col="light grey", lty=2)
  })
  
  output$Bplot.mean <- renderPlot({
    results <- datasetInput()
    results <- results$results
    sampleBias <- sum(((results$sample.mean - results$pop.mean)/results$pop.mean))/input$simNum
    rosBias <- sum(((results$ros.mean - results$pop.mean)/results$pop.mean))/input$simNum
    kmBias <- sum(((results$km.mean - results$pop.mean)/results$pop.mean))/input$simNum
    mleBias <- sum(((results$mle.mean - results$pop.mean)/results$pop.mean))/input$simNum
    sub0Bias <- sum(((results$sub0.mean - results$pop.mean)/results$pop.mean))/input$simNum
    subHalfBias <- sum(((results$subHalf.mean - results$pop.mean)/results$pop.mean))/input$simNum
    subFullBias <- sum(((results$subFull.mean - results$pop.mean)/results$pop.mean))/input$simNum
    
    barplot(c(sampleBias, rosBias, kmBias, mleBias, sub0Bias, subHalfBias, subFullBias),
            names.arg=c("Sample", "ROS", "KM", "MLE", "Sub \nZero", "Sub \nHalf", "Sub \nFull"),
            main="Mean Bias", ylab="Mean Bias", col="light blue")
    abline(h=0, col="light grey", lty=2)
  })
  
  output$Bplot.sd <- renderPlot({
    results <- datasetInput()
    results <- results$results
    results$pop.sd <-sqrt(results$pop.var)
    samplesdBias <- sum(((results$sample.sd - results$pop.sd)/results$pop.sd))/input$simNum
    rossddBias <- sum(((results$ros.sd - results$pop.sd)/results$pop.sd))/input$simNum
    kmsdBias <- sum(((results$km.sd - results$pop.sd)/results$pop.sd))/input$simNum
    mlesdBias <- sum(((results$mle.sd - results$pop.sd)/results$pop.sd))/input$simNum
    sub0sdBias <- sum(((results$sub0.sd - results$pop.sd)/results$pop.sd))/input$simNum
    subsdHalfBias <- sum(((results$subHalf.sd - results$pop.sd)/results$pop.sd))/input$simNum
    subsdFullBias <- sum(((results$subFull.sd - results$pop.sd)/results$pop.sd))/input$simNum
    
    barplot(c(samplesdBias, rossddBias, kmsdBias, mlesdBias, sub0sdBias, subsdHalfBias, subsdFullBias),
            names.arg=c("Sample", "ROS", "KM", "MLE", "Sub \nZero", "Sub \nHalf", "Sub \nFull"),
            main="Std. Dev. Bias", ylab="Std. Dev. Bias", col="light blue")
    abline(h=0, col="light grey", lty=2)
  })
  
  output$censum.all <- renderTable ({
    results <- datasetInput()
    dfx <- results$data
    allsum <-t(data.frame(censummary(dfx$obs, dfx$cen)$all))
    rownames(allsum) <- "Summary Statistics"
    return(allsum)
  })
  
  
  output$limits.all <- renderTable ({
    results <- datasetInput()
    dfx <- results$data
    allLim <-(data.frame(censummary(dfx$obs, dfx$cen)$limits))
    return(allLim)
  })
  
  output$totalObs <- renderText({
    
    x <- input$obs1 + input$obs2
    return(x)
  })
  
  output$GSD <- renderText({
    x <- exp(input$sdlog)
    return(x)
  })

  output$GM <- renderText({
    x <- exp(input$meanlog)
    return(x)
  })
  
  
  output$DL1 <- renderText({
   
    x <- getDetLim(input, "censQ1")
    return(x)
  })

  output$DL2 <- renderText({
    
    x <- getDetLim(input, "censQ2")
    return(x)
  })
  
  
  output$DL3 <- renderText({
    
    x <- getDetLim(input, "censQ3")
    return(x)
  })
  
})