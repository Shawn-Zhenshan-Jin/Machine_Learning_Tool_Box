CVplot <- function(results, groupVar, classification = FALSE, threeDPlot = FALSE){
  #################################################################################################
  ## Attention: 1. 2D plot for more than one parameters is not accurate
  ##            2. 3D plot for more than two parameters is not accurate
  ##
  ## Functions:
  ## 1. Find the optimal parameters for one or more tunning parameters
  ## 2. Visualize the optimal tuning parameters and Cross validation in 2D and 3D plot
  ## 
  ## Input:
  ## results = Cross validation results(tunning parameters; 'trainMSE'; 'validMSE')
  ## groupVar = Name of Tuning parameters in results
  ## 
  ## Ouput:
  ## opatimalParams(list): Optimal value for each tunning parameters
  ## CVplot_TrainingValidation(list): CVplot for both training and validation MSE
  ## CVplot_Validation(list): CVplot only for validation MSE
  ## CVplot3D(automatically output): 3D cv plot for pair of tuning paramters
  
  library(ggplot2);library(dplyr)
  
  ## Test code
  # setwd('/home/zhenshan/Dropbox')
  # ComplexPolyResult <- read.csv('STAT640Competition/data/cvResults/CVresultsComplexPolynomial.csv')
  # results <- ComplexPolyResult
  # groupVar <- c('Degree', 'Offset')
  # classification <- TRUE
  # threeDPlot <- FALSE
  # results <- ridgeResult
  # groupVar <- 'Lambda'
  # classification <- FALSE
  # threeDPlot <- FALSE
  
  #######
  ## Preparation
  #######
  # Keep consistent with y label in plot
  if(classification){
    yLab = 'Error'
  }else{
    yLab = 'MSE'
  }
  
  #######
  ## 2D plot
  #######
  # Individual Tuning Parameters(2D Visualization)
  optimalParamListOneStandard <- list()
  optimalParamListMin <- list()
  CVplotTrainingValidation <- list()
  CVplotValidation <- list()
  
  for (i in 1:length(groupVar)){
    i <- 1
    vars <- groupVar[i]
    # Calculating basic data for plotting
    resultsSingleMean <- results %>%
      as.data.frame %>%
      group_by_(vars) %>%
      summarise(valMSE_se = sqrt(var(validMSE)), trainMSE_se = sqrt(var(trainMSE)), 
                valMSE_mean = mean(validMSE), trainMSE_mean = mean(trainMSE), 
                valMSE_min=valMSE_mean - valMSE_se, valMSE_max=valMSE_mean + valMSE_se)
    # Find the optimal parameter(MSE One standard rule) in the left hand side(i.e. parameter with most parsimony)
    opParamIdx <- resultsSingleMean %>%
      filter(as.numeric(rownames(.)) < match(min(valMSE_mean),valMSE_mean)) %>%
      summarise(sum(valMSE_min > min(valMSE_mean)) + 1) 
    
    opParam <- as.numeric(resultsSingleMean[as.numeric(opParamIdx),][1])
    OptimalParam <- paste("Optimal", vars, ":", round(opParam,4))
    optimalParamListOneStandard[[i]] <- opParam
    
    # Find the optimal parameter(MSE Minimum rule) in the left hand side(i.e. parameter with most parsimony)
    opParamMin <- as.numeric(resultsSingleMean[which.min(resultsSingleMean$valMSE_mean),][1])
    OptimalParamMin <- paste("Optimal Minimum", vars, ":", round(opParamMin,4))
    optimalParamListMin[[i]] <- opParamMin
    
    plotTrainValidation <- ggplot(aes_string(vars), data = data.frame(resultsSingleMean, opParam))  + 
      geom_errorbar(aes(ymin=valMSE_min, ymax=valMSE_max), width=.1, colour = 'grey') +
      geom_line(aes(y = valMSE_mean, colour = "Validation")) +
      geom_point(aes(y = valMSE_mean), colour = 'blue') +
      geom_line(aes(y = trainMSE_mean, colour = "Training"))+
      geom_vline(aes(xintercept= opParam, colour = "OneStandardError"), linetype = "longdash") +
      geom_vline(aes(xintercept= opParamMin,  colour = "MinimumMSE"), linetype = "longdash") +
      scale_colour_manual("", 
                          breaks = c("Validation", "Training", "OneStandardError", "MinimumMSE"),
                          values = c("Validation" = "blue", "Training" = "red", "OneStandardError"= 'black', "MinimumMSE" = 'purple'))+
      ylab(yLab) +
      theme(legend.text = element_text(size = 15, colour = "black", angle = 0))+
      theme(axis.title.x=element_text(size=20),
            axis.title.y=element_text(size=20))+
      labs(title='Cross Validation') +
      theme(plot.title = element_text(size = rel(2)),
            strip.text = element_text(size=15))+
      theme(axis.text.x=element_text(size=10,colour ='black'))+
      theme(axis.text.y=element_text(size=10,colour ='black'))
    CVplotTrainingValidation[[i]] <- plotTrainValidation
    
    plotValidation <- ggplot(aes_string(vars), data = data.frame(resultsSingleMean, opParam))  + 
      geom_errorbar(aes(ymin=valMSE_min, ymax=valMSE_max), width=.1, colour = 'grey') +
      geom_line(aes(y = valMSE_mean, colour = "Validation")) +
      geom_point(aes(y = valMSE_mean), colour = 'blue') +
      geom_vline(aes(xintercept= opParam, colour = "OneStandardError"), linetype = "longdash") +
      geom_vline(aes(xintercept= opParamMin, colour = "MinimumMSE"), linetype = "longdash") +
      scale_colour_manual("", 
                          breaks = c("Validation", "OneStandardError", "MinimumMSE"),
                          values = c("Validation" = "blue", "OneStandardError"= 'black', "MinimumMSE" = 'purple'))+
      ylab(yLab) +
      theme(legend.text = element_text(size = 15, colour = "black", angle = 0))+
      theme(axis.title.x=element_text(size=20),
            axis.title.y=element_text(size=20))+
      labs(title='Cross Validation') +
      theme(plot.title = element_text(size = rel(2)),
            strip.text = element_text(size=15))+
      theme(axis.text.x=element_text(size=10,colour ='black'))+
      theme(axis.text.y=element_text(size=10,colour ='black'))
    CVplotValidation[[i]] <- plotValidation
  }
  # Add group variable name to list
  names(optimalParamListOneStandard) <- groupVar
  names(optimalParamListMin) <- groupVar
  names(CVplotTrainingValidation) <- groupVar
  names(CVplotValidation) <- groupVar
  
  #######
  ## 3D plot
  #######  
  if (threeDPlot){
    if((lenVars <- length(groupVar)) > 1){
      library(scatterplot3d)
      for(x_idx in 1:(lenVars - 1)){
        for(y_idx in (x_idx + 1):lenVars){
          # Calculate the mean MSE for each pair of tuning paramters
          # x_idx <- 1
          # y_idx <- 2
          resultsPairMean <- results %>%
            group_by_(.dots = c(groupVar[x_idx], groupVar[y_idx])) %>%
            summarise(valMSE_se_pair = sqrt(var(validMSE)), valMSE_mean_pair = mean(validMSE), 
                      valMSE_min_pair=valMSE_mean_pair - valMSE_se_pair, valMSE_max_pair=valMSE_mean_pair + valMSE_se_pair)
          
          # Find the minimum tuning parameters
          minParamList <- c()
          minParamIdx <- match(min(resultsPairMean$valMSE_mean_pair),resultsPairMean$valMSE_mean_pair)
          minParamList <- c(as.numeric(resultsPairMean[minParamIdx,][2]), minParamList)
          minParamList <- c(as.numeric(resultsPairMean[as.numeric(minParamIdx),][1]), minParamList)
          
          # Retrive MSE standard error of optimal pair parameters
          filter_condition_min <- paste(groupVar[x_idx], "==",  as.numeric(minParamList[x_idx]), "&", groupVar[y_idx], "==",  as.numeric(minParamList[y_idx]))
          minValMSE_mean_pair <- resultsPairMean %>%
            filter_(filter_condition_min)
          
          # Assign x,y,z in 3D plot
          x <- as.numeric(resultsPairMean[which(colnames(resultsPairMean)==groupVar[x_idx])][[1]])
          y <- as.numeric(resultsPairMean[which(colnames(resultsPairMean)==groupVar[y_idx])][[1]])
          z <- resultsPairMean$valMSE_mean_pair
          
          ## x,y,z for optimal tuning parameters
          xOptimal <- minValMSE_mean_pair[1]
          yOptimal <- minValMSE_mean_pair[2]
          zOptimal <- min(resultsPairMean$valMSE_mean_pair)
          # up and bottom SE plane
          zOptimalUp <- zOptimal + minValMSE_mean_pair[3]
          zOptimalBottom <- zOptimal - minValMSE_mean_pair[3]
          
          
          s3d <- scatterplot3d(x, y, z, highlight.3d=TRUE, col.axis="blue",type = 'p',
                               col.grid="lightblue", main="Elastic Net  CV Erro 3D", angle = 20, 
                               pch=16, xlab = groupVar[x_idx], ylab = groupVar[y_idx], zlab = 'Validation MSE')
          s3d$points3d(xOptimal, yOptimal, zOptimal, y, col="blue", type="h", pch=16)
          s3d$plane3d(zOptimalUp,0,0, lty.box = "solid", draw_polygon = TRUE, draw_lines = TRUE, lty = 'solid')
          s3d$plane3d(zOptimalBottom,0,0, lty.box = "solid", draw_polygon = TRUE, draw_lines = TRUE, lty = 'solid')
          
          legend("topright", inset=.05,      # location and inset
                 bty="n", cex=0.7,              # suppress legend box, shrink text 50%
                 title=" ",
                 c("Optimal Tuning", "Expected Validation Error", "One Standard Plane"), fill=c("blue", "red", "grey"))
        }
      }
    }
  }
  
  return(list(opatimalParams = optimalParamListOneStandard, opatimalParamsMin = optimalParamListMin, CVplot_TrainingValidation = CVplotTrainingValidation,
              CVplot_Validation = CVplotValidation))
}