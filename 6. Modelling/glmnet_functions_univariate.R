
## ---- aux-functions --------

# enable use of {progressr} to see progress bars

###################################################################3
# Auxiliary functions used to consolidate code used multiple times
###################################################################3

#' pseudo-log transformation - accounting for zeros and negatives values
#' @param  x a number
#' @output sign(x)*log(abs(x)+1)
pseudo_log_trans <- function(x) {
  sign(x) * log(abs(x) + 1)
  
}

# train, test, validation split
#' split a dataset into a list containing train, test a validation subests
#' @param  dataset to be partition
#' @param  traintest_perc percentage of complete dataset that should be train+test, in opposition to validation data
#' @param  train_perc     percentage to train+test subset to partition as train data
#' @output list containing data partitions
split_dataset <-function(dataset, traintest_perc = 0, train_perc = 0.85,id_col) {
  data_split <- list()
  
  if(traintest_perc==0){
    train_test <- dataset
  }else{
    train_validation_split <- initial_split(dataset, traintest_perc)
    train_test <- training(train_validation_split)
    data_split$validation <- testing(train_validation_split)
  }
  
  train_test_split <- initial_split(train_test, train_perc)
  data_split$training      <- training(train_test_split)
  data_split$testing       <- testing(train_test_split)
  
  #record split ids for comparison
  data_split$split <-
    bind_rows(
      data_split$training   |> select(all_of(id_col)) |> mutate(split = "training"),
      data_split$testing    |> select(all_of(id_col)) |> mutate(split ="testing"))
  
  if(traintest_perc>0){
    data_split$split <-  data_split$split |>
      bind_rows(data_split$validation |>
                  select(all_of(id_col)) |> 
                  mutate(split ="validation"))
  }
  
  
  return(data_split)
}

#' generate plot containing vilion+box plots for all covariates
#' @param  dataset a Polish bankruptcy dataset
#' @param  title_text title for the whole plot
#' @param  subtitle_text subtitle for the whole plot
#' @output ggplot object
distribution_plots <- function(dataset, title_text, subtitle_text) {
  dataset %>%
    select(-id) %>%
    pivot_longer(-class, names_to = "metric", values_to = "value") %>%
    mutate(metric = factor(metric,
                           levels = (colnames(dataset)[!(colnames(dataset) %in% c("class", "id"))] %>% mixedsort())))  %>%
    ggplot(aes(x = "", y = value)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.1,
                 color = "grey",
                 alpha = 0.2) +
    facet_wrap(. ~ metric, nrow = 6, scales = "free") +
    labs(title = title_text,
         subtitle = subtitle_text)
}

# reused from my DMMLI's project, with minor adaptations
#' get summary of prediction performance (and prediction data)
#' @param  model_class fitted model
#' @param observations 1-attribute tibble containing all true observations
#' @param test_data    test dataset
#' @param include_data whether to include prediction data in output (default FALSE)
#' @output summary with key performance metrics and prediction results (if included)
get_summary <- function(model_class, observations, test_data, include_data = FALSE) {
  
  #model_class=nets.model
  #observations=y.test
  #test_data=x.test
  #include_data = FALSE
  
  results_base <- observations |> 
    as_tibble(rownames="variable") |>
    pivot_longer(-variable,names_to = "response",values_to="actual") |>
    left_join(
      model_class |> 
        predict(test_data, type = "response") |>
        as_tibble(rownames="variable")        |>
        rename("prediction"="s0")               ,
      by=c("variable")) |>
    mutate(square_error=(prediction-actual)^2)
  
  results_party <- results_base |>
    group_by(response)|>
    summarise(RMSE=(sqrt(mean(square_error))),.groups="drop")

  
  results_party |>
    mutate(response=str_c("RMSE_",response)) |>
    pivot_wider(names_from = "response",values_from="RMSE")
  
}

#' get summary of prediction performance (and prediction data)
#' @param  dataset_split output of split_dataset()
#' @output list with best lasso results
lasso_selection <- function(dataset_split,id_col,response,other_responses,family="gaussian") {

  x.train <-  dataset_split$training |> column_to_rownames(id_col)  |> select(-all_of(c(response,other_responses))) # |> as.matrix()
  x.train <- model.matrix( ~ .+1, data = x.train)
  y.train <-  dataset_split$training |>  column_to_rownames(id_col) |> 
               select(all_of(response)) |> as.matrix()
  
  cv.lasso  <-
    cv.glmnet(x.train, y.train, family = family, alpha = 1)
  
  lasso.model <- glmnet(
    x.train,
    y.train,
    family = family,
    alpha = 1,
    lambda = cv.lasso$lambda.1se
  )
  
  coefs <- coef(lasso.model)
  coefs <- coefs@Dimnames[[1]][coefs@i[-1] + 1]
  
  output <- list()
  
  output$cv         <- cv.lasso
  output$model      <- lasso.model
  output$covariates <- coefs
  
  return(output)
  
}
#' assess performance of elastic net models for a range of regularisation parameters
#' @param  dataset_split output of split_dataset(), using glmnet
#' @param  alpha_grid vector of alphas
#' @param  lambda single value for lambda - if null, will use cv.glmnet() to calculate 1.s.e value
#' @param  formula formula object for logistic regression (class ~ Var1 + Var2 + .....)
#' @output tibble with performance metrics for each model
assess_net <-function(dataset_split,
                       alpha_grid = seq(0, 1, 0.025),
                       id_col,
                       response,
                      predictors,
                       preset_lambda = NULL,
                       family ="gaussian") {
  
   predictors <- predictors[str_detect(predictors,"Intercept",TRUE)]
  
    # prep data for glmnet()
    x.train <-  dataset_split$training |> column_to_rownames(id_col)  |> select(all_of(predictors)) # |> as.matrix()
    x.train <-  model.matrix(~.+1, data = x.train)
    y.train <-  dataset_split$training |>  column_to_rownames(id_col) |> 
                select(all_of(response)) |> as.matrix()
    
    x.test <-  dataset_split$testing |> column_to_rownames(id_col)  |> select(all_of(predictors)) # |> as.matrix()
    x.test <-  model.matrix(~.+1, data = x.test)
    y.test <-  dataset_split$testing |>  column_to_rownames(id_col) |> 
               select(all_of(response)) |> as.matrix()
    
    
    #ridge regression


    for (i in 1:length(alpha_grid)) {
      
      if (is.null(preset_lambda)) {
        cv.net <-
        cv.glmnet(x.train, y.train, family = family, alpha = alpha_grid[i])
        lambda <- cv.net$lambda.1se
      }else{
        lambda <- preset_lambda
      }
      
      nets.model <-
        glmnet(
          x.train,
          y.train,
          family = family,
          alpha = alpha_grid[i],
          lambda = lambda
        )
      
      
      metrics <- get_summary(nets.model, y.test, x.test) %>%
        add_column(number     =  i,
                   alpha      =  alpha_grid[i],
                   lambda     =  lambda) %>%
        relocate(number, alpha, lambda, .before = 1)
      
      betas <- as.matrix(nets.model$beta) |>
        as_tibble(rownanmes="variable") |>
        rename("coefficient"="s0")
     
      
      metrics <- metrics |> mutate(coeff=list(betas))
      
      
      if (exists("nets.metrics")) {
        nets.metrics <- rbind(nets.metrics, metrics)
      } else{
        nets.metrics <- metrics
      }
      
      
    }
    
    return(nets.metrics)
  }

#' ggplot version of plot(cv.gmlnet)
#' @param  cv_glmnet cv.glmnet object
#' @param  min_lambda value to draw a vertical line
#' @param  legend_y vertical placement of lambda value along vertical line
#' @param  round_lambda rounding for reference lambda presented in the plot
#' @param  points colour for central error estimates
#' @param  bar colour for confidence bars
#' @output ggplot object
lambda_penalty_plot <-
  function(cv_glmnet,min_lambda,legend_y = 0, round_lambda = 5, points = "red", bars = "gray80") {
    p   <- tibble(
      log_lambda = log(cv_glmnet$lambda),
      cvm   =  cv_glmnet$cvm,
      cvsd  =  cv_glmnet$cvsd,
      cvup  =  cv_glmnet$cvup,
      cvlo  =  cv_glmnet$cvlo,
      nzero =  cv_glmnet$nzero
    ) %>%
      ggplot() +
      geom_vline(
        xintercept = log(min_lambda),
        colour = "red",
        linetype = "dashed",
        show.legend = FALSE
      ) +
      geom_text(
        aes(
          x = log(min_lambda),
          label = str_c("1 s.e.:", round(log(min_lambda), round_lambda)),
          y = legend_y
        ),
        colour = "red",
        angle = 90,
        vjust = -1,
        text = element_text(size = 11)
      ) +
      geom_errorbar(aes(x = log_lambda, ymin = cvlo, ymax = cvup), colour =
                      bars) +
      geom_point(aes(x = log_lambda, y = cvm), colour = points) +
      labs(x = expression("Log(" ~ lambda ~ ")"),
           y = cv_glmnet$name)
    
    
    return(p)
    
  }

#' ggplot version of plot(glmnet,xvar="lambda",label=TRUE)
#' @param a glmnet object
#' @output ggplot object
coef_plot <- function(a) {
  # from https://stackoverflow.com/questions/36656752/plotting-cv-glmnet-in-r
  betas = as.matrix(a$glmnet.fit$beta)
  lambdas = a$glmnet.fit$lambda
  names(lambdas) = colnames(betas)
  

  p <- as.data.frame(betas) %>%
    rownames_to_column("variable") %>%
    pivot_longer(-variable) %>%
    mutate(lambda = lambdas[name]) %>%
    ggplot(aes(x = lambda, y = value, col = variable)) +
    geom_vline(
      xintercept = a$lambda.1se,
      colour = "red",
      linetype = "dashed",
      show.legend = FALSE
    ) +
    geom_text(
      aes(
        x = a$lambda.1se,
        label = round(a$lambda.1se, 5),
        y = min(betas)
      ),
      colour = "red",
      angle = 90,
      vjust = -1,
      size =  3
    ) +
    geom_line() +
    geom_label_repel(
      data =  ~ subset(.x, lambda == min(lambda)),
      aes(label = variable),
      nudge_x = -0.5
    ) +
    scale_x_log10() +
    labs(x = expression(lambda),
         y = "coefficient") +
    theme_minimal()+
    theme(legend.position = "none")
  
  
  return(p)
  
}

#' plot showing glmnet coefficients, ordered by absolute vlaue
#' @param model glmnet object
#' @output ggplot object
coef_lollipop <- function(model, plot_colours=c("red","blue")) {
  
  
  coefs <- extract_coefs(model)
  
  if(nrow(coefs>0)){

  p <- coefs |>
    mutate(variable = str_c(variable, " :: ", formatC(value, 3))) |>
    mutate(variable = fct_reorder(variable, abs(value))) |>
    mutate(sign=as.character(value>=0)) |>
    ggplot(aes(x = variable, y = value,colour=sign,fill=sign)) +
    geom_segment(aes(
      x = variable,
      xend = variable,
      y = 0,
      yend = value )) +
    geom_point(size = 4,
               alpha = 0.6) +
    coord_flip() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    ) +
    scale_fill_manual(values=plot_colours) +
    scale_colour_manual(values=plot_colours)+
    labs(x = "Covariate",
         y = "Coefficient")
  }else{
    p <- ggplot()
  }
  
  return(p)  

}

#extract non-zero coefficients from lasso model
extract_coefs <- function(model){
  beta_i <- model$glmnet.fit$beta
  lambdas <- model$glmnet.fit$lambda
  

  names(lambdas) <- colnames(beta_i)
  
  lastone <- tibble(s=names(which(lambdas>model$lambda.1se)))  |>
      mutate(order=as.numeric(str_extract(s,"(\\d)+"))) |>
      slice_max(order) |>
      pull(s)
  
  if(length(lastone)==0){
    beta_i <- tibble()
  
    }else{
    
  
  beta_i <- beta_i |>
      as.matrix() |>
      as.data.frame() |>
      rownames_to_column("variable") |>
      select(any_of(c("variable",lastone)))    |>
      rename("value"=lastone) |>
      filter(abs(value)>0|variable=="(Intercept)")   
    } 
  
  return(beta_i)
  
}

#' show "heatmap"  plots with accuracy, sensitivity, specificity  for assess_net resuslts
#' @param assess_net_output output of assess_net function
#' @output ggplot object
enet_grid_plot <- function(assess_net_output,title_text="",subtitle_text=""){
  
  df <- assess_net_output |>
    select(alpha, lambda, contains("RMSE")) |>
    pivot_longer(c(-alpha, -lambda), names_to = "metric", values_to = "value")  |>
    mutate(lambda = formatC(lambda, 4),
           alpha  = formatC(alpha, 3)) 
  
  values_limits <- c(min(df$value),max(df$value))
  
  df|>
    ggplot() +
    geom_tile(
      aes(x = alpha, y = lambda, fill = value),
      na.rm = TRUE,
      colour = "white",
      width = 0.4
    ) + 
    facet_wrap(. ~ metric, nrow = 2) +
    theme_minimal() +
    theme(legend.position = "right",
          legend.direction = "vertical",
          axis.text.x  = element_text(angle=90)) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")[c(9, 7, 5, 3, 1)],
                         na.value="grey90",
                         limits=values_limits) + #,
    #values=c(0,0.5,0.65,0.7,0.75,1)) +
    labs(
      title = title_text,
      subtitle= subtitle_text,
      x = expression("" ~ alpha ~ ""),
      y = expression("" ~ lambda ~ ""),
      fill = "Value"
    ) 
}


