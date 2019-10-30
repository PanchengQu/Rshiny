# Define server logic required to draw a histogram
Server <- shinyServer(function(input, output, session) {
  
  ###############################################################################
  # Probability Distribution and Central Limit Theorem
  ###############################################################################
  
  # Defining population function for storing population data
  population <- reactive({ #executes the code inside {} and render data to population object
    if (input$dist == "norm") {rnorm(input$pop_size,mean=as.numeric(input$miu),sd=as.numeric(input$sigma))}
    else if (input$dist == "unif") {runif(input$pop_size,min=as.numeric(input$a),max=as.numeric(input$b))}
    else if (input$dist == "exp") {rexp(input$pop_size, rate = as.numeric(input$lambda))}
    else if (input$dist == "binom") {rbinom(input$pop_size, size=as.numeric(input$n), prob=as.numeric(input$p))}
    else if (input$dist == "nbinom") {rnbinom(input$pop_size, size=as.numeric(input$r), prob=as.numeric(input$p2))}
    else if (input$dist == "pois") {rpois(input$pop_size, lambda=as.numeric(input$lambda2))}
    else if (input$dist == "geom") {rgeom(input$pop_size, prob=as.numeric(input$p3))}
    else if (input$dist == "hyper") {rhyper(input$pop_size,m=as.numeric(input$M), n=as.numeric(input$N), k=as.numeric(input$K))}
    else if (input$dist == "chisq") {rchisq(input$pop_size,df=as.numeric(input$df))}
    else if (input$dist == "t") {rt(input$pop_size, df=as.numeric(input$df2))}
    else if (input$dist == "beta") {rbeta(input$pop_size, shape1=as.numeric(input$Alpha), shape2=as.numeric(input$Beta))}
    else if (input$dist == "gamma") {rgamma(input$pop_size, shape=as.numeric(input$k), scale=as.numeric(input$Theta))}
  }
  )
  
  #Defining sample mean function for storing sample mean data
  smpl_mean <- reactive({ #executes the code inside {} and render data to smpl_mean object
    for (i in 1:input$smpl_iterate) {
      if (i==1) {
        smpl_mean <- c(mean(sample(population(), input$smpl_size, replace = TRUE ))) #creating object for the first time
      } else {
        smpl_mean <- c(smpl_mean,mean(sample(population(), input$smpl_size, replace = TRUE ))) #apending data to existing smpl_mean object
      }
    }
    smpl_mean #printing smpl_mean object in order to return via reactive function to main smpl_mean object
  })
  
  #Rendering summary statistics and data information of population and sample mean data
  output$pop_summary <- renderPrint({summary(population())})
  output$pop_structure <- renderPrint({str(population())})
  output$smpl_mean_summary <- renderPrint({summary(smpl_mean())})
  output$smpl_mean_structure <- renderPrint({str(smpl_mean())})
  
  #Rendering population plot
  output$plot_pop <-renderPlot({
    plot(density(population()),axes=FALSE,xlab="",ylab="",main="", col="blue",lwd=2) #density plot
    par(new=TRUE) #new plot should not clean the frame before drawing as if it were on a new device
    hist(population(), freq = FALSE,main="Population Histogram & Density Plot", xlab = "") #ploting histogram
    abline(v = mean(population()), col = "red", lwd = 2) #ploting straight vertical red line for mean
    text(x=mean(population()), y=0,labels="Mean",col="red")
  })
  
  #Rendering sample plot
  output$plot_smpl_mean <-renderPlot({
    par(mfrow = c(1, 2))
    max_y<-max(hist(smpl_mean(),plot = FALSE)$density,
               dnorm(seq(min(smpl_mean()), max(smpl_mean()), 0.0001),mean(smpl_mean()), sd(smpl_mean())))
    hist(smpl_mean(), freq = FALSE, main="Sample Mean Histogram",  
         cex.main = 1.25, ylim=c(0,max_y), xlab="Blue line = N(sample mean, sample variance)")
    abline(v = mean(smpl_mean()), col = "red", lwd = 2)
    text(x=mean(population()), y=0,labels="Mean",col="red")
    lines(x = seq(min(smpl_mean()), max(smpl_mean()), 0.0001), 
          y = dnorm(seq(min(smpl_mean()), max(smpl_mean()), 0.0001),mean(smpl_mean()), sd(smpl_mean())),
          col = "blue", lwd = 2)
    qqnorm(smpl_mean())
    qqline(smpl_mean(), col = "blue",lwd=2)
  })
  
  
  ###############################################################################
  # Quantile Plots and Skewness
  ###############################################################################
  output$QQplots <- renderPlot({
    input$update
    n <- 1000
    x <- if (input$type == "Light-tailed"){
      runif(n)
    } else if(input$type == "Heavy-tailed"){
      rt(n, 2)
    } else if(input$type == "Normal"){
      rnorm(n)
    } else if(input$type == "Negatively skewed"){
      rbeta(n, 15, 2)
    } else if(input$type == "Positively skewed"){
      rgamma(n, 2)
    }
    
    par(mfrow = c(2, 2), mar = c(2, 2, 2, 2), oma = c(0, 0, 5, 0))
    
    qqnorm(x)
    qqline(x, col = "red")
    ### Parameters of corresponding normal distribution
    mu <- mean(x)
    sigma <- sd(x)
    ### Create vectors m quantiles of normal and chosen distributions
    normal_quantiles <- qnorm(c(.001, seq(.05, .99, .05), .999), mu, sigma)
    names(normal_quantiles) <- paste0(seq(0, 100, 5), "%")
    sample_quantiles <- quantile(x, seq(0, 1, .05))
    
    if (input$type == "Light-tailed"){
      density_quantiles <- dunif(sample_quantiles)
    } else if(input$type == "Heavy-tailed"){
      density_quantiles <- dt(sample_quantiles, 2)
    } else if(input$type == "Normal"){
      density_quantiles <- dnorm(sample_quantiles, mu, sigma)
    } else if(input$type == "Negatively skewed"){
      density_quantiles <- dbeta(sample_quantiles, 15, 2)
    } else if(input$type == "Positively skewed"){
      density_quantiles <- dgamma(sample_quantiles, 2)
    }
    ### Plot both distributions and quantiles
    plot(normal_quantiles, dnorm(normal_quantiles, mu, sigma), type = "b",
         xlim = range(normal_quantiles, sample_quantiles), 
         ylim = range(dnorm(normal_quantiles, mu, sigma), density_quantiles),
         col = "red", pch = 0,
         main = "Densities (lines) and quantiles (symbols)")
    lines(sample_quantiles, density_quantiles, type = "b")
    
    barplot(height = rbind(sample_quantiles, normal_quantiles), beside = TRUE,
            col = c("black", "red"),
            main = "Sample quantiles vs Normal quantiles")
    boxplot(x, rnorm(length(x), mu, sigma), border = c("black", "red"),
            names = c("Sample", "Corresponding Normal"), 
            main = "Sample quantiles vs Normal quantiles")
    title(main = if (input$type == "Light-tailed"){
      "Sample quantiles drawn from Uniform(0,1) (black) against corresponding normal quantiles (red)"
    } else if(input$type == "Heavy-tailed"){
      expression(bold(paste("Sample quantiles drawn from t"[2], " (black) against corresponding normal quantiles (red)")))
    } else if(input$type == "Normal"){
      paste0("Sample quantiles drawn from Normal(", round(mu, 1), ",", round(sigma^2, 1), ") (black) against corresponding normal quantiles (red)")
    } else if(input$type == "Negatively skewed"){
      "Sample quantiles drawn from Beta(15,2) (black) against corresponding normal quantiles (red)"
    } else if(input$type == "Positively skewed"){
      "Sample quantiles drawn from Gamma(2) (black) against corresponding normal quantiles (red)"
    }, outer = TRUE, cex.main = 1.5)
  })
  
  
  
  
  ###############################################################################
  # Import data and run MLR with CheckboxGroup
  ###############################################################################
  dsnames <- c()
  data_set <- reactive({
    req(input$file1)
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL) }
    
    if (input$fileType_Input == "1") {
      data_set<-read.csv(inFile$datapath,
                         header = TRUE,
                         stringsAsFactors = FALSE)
    } else {
      data_set<-read_excel(inFile$datapath)
    }
  })
  
  observe({
    req(input$file1)
    dsnames <- names(data_set())
    # cb_options <- list()
    # cb_options[dsnames] <- dsnames
    updateCheckboxGroupInput(session, "inCheckboxGroup",
                             label = "Select explanatory variables",
                             choices = dsnames,  
                             selected = "")
    updateSelectInput(session, "selectInput",
                      label = "Select a response variable",
                      choices = dsnames,  
                      selected = "")
  })
  
  
  output$contents<- renderTable({
    selected_data <- data_set()[,c(input$selectInput,input$inCheckboxGroup)]
    selected_data
  })
  
  
  
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
  })
  
  output$text1<-renderPrint({
    model<-lm(as.formula(paste(input$selectInput,"~",paste(input$inCheckboxGroup,collapse="+"))),data=data_set())
    summary(model)
    
  })
  output$text2<-renderPrint({
    model<-lm(as.formula(paste(input$selectInput,"~",paste(input$inCheckboxGroup,collapse="+"))),data=data_set())
    anova(model)
  })
  
  ###############################################################################
  # Build glm models and predict values based on that
  ###############################################################################
  glm1 <- reactive({
    input_x <- input$x
    input_y <- input$y
    indVar <- myData[,which(colnames(myData) == input_x)]
    depVar <- myData[,which(colnames(myData) == input_y)]
    
    x.grid <- seq(min(c(indVar,depVar)),max(c(indVar,depVar)),by=0.1)
    # Define glm family and link functions
    input_family <- input$family
    input_link <- input$link
    
    if (input_link == "Canonical Link")
    {if (input_family == "binomial"){input_link = "(link='logit')"}
      if (input_family == "gaussian"){input_link = "(link='identity')"}
      if (input_family == "Gamma"){input_link = "(link='inverse')"}
      if (input_family == "inverse.gaussian"){input_link = "(link='1/mu^2')"}
      if (input_family == "poisson"){input_link = "(link='log')"}
      if (input_family == "quasi"){input_link = "(link='identity')"}
      if (input_family == "quasibinomial"){input_link = "(link='logit')"}
      if (input_family == "quasipoisson"){input_link = "(link='log')"}}
    
    family_link <- eval(parse(text = paste(input_family, input_link, sep = "")))
    
    # input_x <- eval(parse(text = input_x))
    # input_y <- eval(parse(text = input_y))
    # 
    if (input$t_x == "None"){glm1 <- glm(depVar ~ indVar, family=family_link)}
    if (input$t_x == "log"){glm1 <- glm(depVar ~ log(indVar), family=family_link)}
    if (input$t_x == "sqrt"){glm1 <- glm(depVar ~ sqrt(indVar), family=family_link)}
    if (input$t_x == "square"){glm1 <- glm(depVar ~ indVar**2, family=family_link)}
    if (input$t_x == "third power"){glm1 <- glm(depVar ~ indVar**3, family=family_link)}
    glm1
    
  })
  
  # Make predictions for glm1
  glm1pred <- reactive({
    input_x <- input$x
    input_y <- input$y
    indVar <- myData[,which(colnames(myData) == input_x)]
    depVar <- myData[,which(colnames(myData) == input_y)]
    x.grid <- seq(min(c(indVar,depVar)),max(c(indVar,depVar)),by=0.1)
    newdataframe = data.frame(indVar=x.grid)
    predict(glm1(),newdata=newdataframe,se.fit=TRUE)
  })
  
  # output scatterplot and regression lines
  output$plot_glm1 <- renderPlot({
    input_x <- input$x
    input_y <- input$y
    indVar <- myData[,which(colnames(myData) == input_x)]
    depVar <- myData[,which(colnames(myData) == input_y)]
    x.grid <- seq(min(c(indVar,depVar)),max(c(indVar,depVar)),by=0.1)
    # Plot the scatterplot with the data given
    plot(depVar ~ indVar,col="lightblue",
         pch=15,log="x",xlab=input_x,ylab=input_y,
         xlim = c(min(c(indVar,depVar)), max(c(indVar,depVar))),
         ylim = c(min(c(indVar,depVar)), max(c(indVar,depVar))))
    
    z <- qnorm(0.975,mean=0,sd=1,lower.tail=TRUE)     # That's the z-value
    UL95 <- glm1pred()$fit + (z*glm1pred()$se.fit)    # Upper and lower CI limits
    LL95 <- glm1pred()$fit - (z*glm1pred()$se.fit)  
    # for linear predictor
    
    lines(x.grid,exp(glm1pred()$fit),col="blue")
    lines(x.grid,exp(UL95),lty=2,col="blue")
    lines(x.grid,exp(LL95),lty=2,col="blue")
    
    legend("topleft",col=c("lightblue","blue","blue"),
           pch=c(15,NA,NA),lty=c(NA,1,2),
           legend=c("Observations","Fitted relationship","95% CI"))
  })
  
  # diagnostic plot for glm1
  output$diagnostic_plot1 <- renderPlot({
    # RegressionPlots(glm1())
    par(mfrow = c(2,2))
    plot(glm1(), which = 1:4)
  })
  
  output$summary_glm1 <- renderPrint({
    summary(glm1())
  })
  
  
  ###############################################################################
  # Build higher dimensional glm models and predict values based on that
  ###############################################################################
  glm2 <- reactive({
    selected_data <- data_set()[,c(input$selectInput,input$inCheckboxGroup)]
    indVar <- as.matrix(selected_data[,-1])
    depVar <- as.matrix(selected_data[,1])
    
    
    # Define glm family and link functions
    input_family <- input$family2
    input_link <- input$link2
    
    if (input_link == "Canonical Link")
    {if (input_family == "binomial"){input_link = "(link='logit')"}
      if (input_family == "gaussian"){input_link = "(link='identity')"}
      if (input_family == "Gamma"){input_link = "(link='inverse')"}
      if (input_family == "inverse.gaussian"){input_link = "(link='1/mu^2')"}
      if (input_family == "poisson"){input_link = "(link='log')"}
      if (input_family == "quasi"){input_link = "(link='identity')"}
      if (input_family == "quasibinomial"){input_link = "(link='logit')"}
      if (input_family == "quasipoisson"){input_link = "(link='log')"}}
    
    family_link <- eval(parse(text = paste(input_family, input_link, sep = "")))
    
    glm2 <- glm(depVar ~ indVar, family=family_link)
    glm2
  })
  
  # diagnostic plot for glm1
  output$diagnostic_plot2 <- renderPlot({
    # RegressionPlots(glm1())
    par(mfrow = c(2,2))
    plot(glm2(), which = 1:4)
  })
  
  output$summary_glm2 <- renderPrint({
    summary(glm2())
  })

  
  ###############################################################################
  # Survival Analysis
  ###############################################################################
  datasetInput <- reactive({
    switch(input$Dataset,
           "lung" = lung,
           "cgd" = cgd,
           "pbc" = pbc)
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  covariateInput <- reactive({
    input$covariate
  })
  
  output$DataSetTable <- renderDataTable({dataset <- datasetInput()
  datatable(dataset)})
  output$DataSetPlot1 <- renderPlot({
    dataset <- datasetInput()
    covariate <- as.character(noquote(covariateInput()))
    fit <- survfit(Surv(time, status) ~ dataset[,covariate], data = dataset)
    p <- plot(fit)
    p
  })
  
  observe({
    updateSelectInput(session, "covariate", choices = names(datasetInput())) 
  })
  
  observe(if (input$Dataset == 'lung'){ output$DataSetInfo <- renderText({
    paste('NCCTG Lung Cancer Data',
          'Survival in patients with advanced lung cancer from the North Central Cancer Treatment Group. Performance scores rate how well the patient can perform usual daily activities.',
          
          '',
          
          'inst:	 Institution code',
          'time: Survival time in days',
          'status: censoring status 1 = censored, 2 = dead',
          'age: age in years',
          'sex: Male = 1 Female = 2',
          'ph.ecog: ECOG performance score(0 = good 5 = dead',
          'ph.karno: Karnofsky performance score (bad=0-good=100) rated by physician',
          'pat.karno: Karnofsky performance score as rated by patient',
          'meal.cal: Calories consumed at meals',
          'wt.loss: Weight loss in last six months',
          '',
          'Source: Terry Therneau',
          sep="\n")
  })
  
  })
  observe(if (input$Dataset == 'cgd'){output$DataSetInfo <- renderText({
    paste('Chronic Granulotomous Disease data',
          'Data are from a placebo controlled trial of gamma interferon in chronic granulotomous disease (CGD). Contains the data on time to serious infections observed through end of study for each patient.',
          '',
          'The cgd0 data set is in the form found in the references, with one line per patient and no recoding of the variables. The cgd data set (this one) has been cast into (start, stop] format with one line per event, and covariates such as center recoded as factors to include meaningful labels.',
          '',
          'id subject identification number',
          'center enrolling center',
          'random date of randomization',
          'treatment placebo or gamma interferon',
          'sex sex',
          'age age in years, at study entry',
          'height height in cm at study entry',
          'weight weight in kg at study entry',
          'inherit pattern of inheritance',
          'steroids use of steroids at study entry,1=yes',
          'propylac use of prophylactic antibiotics at study entry',
          'hos.cat a categorization of the centers into 4 groups',
          'tstart, tstop start and end of each time interval',
          'status 1=the interval ends with an infection',
          'enum observation number within subject',
          '',
          'source: Fleming and Harrington, Counting Processes and Survival Analysis, appendix D.2.',
          sep="\n")
  })})
  
  observe(if (input$Dataset == 'pbc'){output$DataSetInfo <- renderText({
    paste('Mayo Clinic Primary Biliary Cirrhosis Data',
          '',
          'This data is from the Mayo Clinic trial in primary biliary cirrhosis (PBC) of the liver conducted between 1974 and 1984. A total of 424 PBC patients, referred to Mayo Clinic during that ten-year interval, met eligibility criteria for the randomized placebo controlled trial of the drug D-penicillamine. The first 312 cases in the data set participated in the randomized trial and contain largely complete data. The additional 112 cases did not participate in the clinical trial, but consented to have basic measurements recorded and to be followed for survival. Six of those cases were lost to follow-up shortly after diagnosis, so the data here are on an additional 106 cases as well as the 312 randomized participants.',
          '',
          'age:	 in years',
          'albumin:	 serum albumin (g/dl)',
          'alk.phos:	 alkaline phosphotase (U/liter)',
          'ascites:	 presence of ascites',
          'ast:	 aspartate aminotransferase, once called SGOT (U/ml)',
          'bili:	 serum bilirunbin (mg/dl)',
          'chol:	 serum cholesterol (mg/dl)',
          'copper:	 urine copper (ug/day)',
          'edema:	 0 no edema, 0.5 untreated or successfully treated 1 edema despite diuretic therapy',
          'hepato:	 presence of hepatomegaly or enlarged liver',
          'id:	 case number',
          'platelet:	 platelet count',
          'protime:	 standardised blood clotting time',
          'sex:	 m/f',
          'spiders:	 blood vessel malformations in the skin',
          'stage:	 histologic stage of disease (needs biopsy)',
          'status:	 status at endpoint, 0/1/2 for censored, transplant, dead',
          'time:	 number of days between registration and the earlier of death,',
          'transplantion, or study analysis in July, 1986',
          'trt:	 1/2/NA for D-penicillmain, placebo, not randomised',
          'trig:	 triglycerides (mg/dl)',
          sep="\n")
  })})
   
  ###############################################################################
  # Markov Process
  ###############################################################################
  # output$matrix <- renderUI({
  #   matrixInput("trans_prob", "Input the transition probablity matrix",
  #               as.data.frame(diag(1, nrow = input$dimension, ncol = input$dimension)))
  # })
  # 
  # var <- reactive({
  #   stateNames <-  c()
  #   for (i in 1:input$dimension){
  #     stateNames[i] = paste("state",i, sep = " ")
  #   }
  #   stateNames
  # }) 
  # 
  # output$select <- renderUI({
  #   selectInput("start", h6("Select starting state: "), 
  #               choices = var())
  # })
  # 
  # output$pas_prob <- renderPrint({
  #   trans_matrix <- matrix(input$trans_prob, nrow = input$dimension, 
  #                          ncol = input$dimension)
  #   matrix_row_sum <- rowSums(trans_matrix)
  #   norm_matrix <- round(trans_matrix / matrix_row_sum, 2)
  #   norm_matrix[,input$dimension] <- 1-rowSums(norm_matrix[,-input$dimension])
  #   stateNames = c()
  #   for (i in 1:input$dimension){
  #     stateNames[i] = paste("state",i, sep = " ")
  #   }
  #   dcmc <- new("markovchain", transitionMatrix = norm_matrix, 
  #               states = stateNames)
  #   round(firstPassage(dcmc, input$start, input$step),3)
  #   
  # })
  # 
  # output$trans_plot <- renderPlot({
  #   trans_matrix <- matrix(input$trans_prob, nrow = input$dimension, 
  #                          ncol = input$dimension)
  #   matrix_row_sum <- rowSums(trans_matrix)
  #   norm_matrix <- round(trans_matrix / matrix_row_sum, 2)
  #   norm_matrix[,input$dimension] <- 1-rowSums(norm_matrix[,-input$dimension])
  #   stateNames = c()
  #   for (i in 1:input$dimension){
  #     stateNames[i] = paste("state",i, sep = " ")
  #   }
  #   dcmc <- new("markovchain", transitionMatrix = norm_matrix, 
  #               states = stateNames)
  #   plot(dcmc, main = "State Space Diagram")
  #   mtext("same coulored dots indicate the states are in the same class")
  # })
  # 
  # output$summary <- renderPrint({
  #   trans_matrix <- matrix(input$trans_prob, nrow = input$dimension, 
  #                          ncol = input$dimension)
  #   matrix_row_sum <- rowSums(trans_matrix)
  #   norm_matrix <- round(trans_matrix / matrix_row_sum, 2)
  #   norm_matrix[,input$dimension] <- 1-rowSums(norm_matrix[,-input$dimension])
  #   stateNames = c()
  #   for (i in 1:input$dimension){
  #     stateNames[i] = paste("state",i, sep = " ")
  #   }
  #   dcmc <- new("markovchain", transitionMatrix = norm_matrix, 
  #               states = stateNames)
  #   summary_dcmc <- summary(dcmc)
  #   
  # })
  
  ####################################################################
  # RandomForest --- Titanic Data
  ###############################################################################
  
  ################################################################################
  # Data wrangling comes from Kaggle kernels
  train<-read.csv('Runlong Yu (Train).csv')
  test<-read.csv('Runlong Yu (Test).csv')
  train$set<-"train"
  test$set<-"test"
  #Add response column in test so that they have equal number
  #of columns
  test$Survived<-NA
  
  #Combine both datasets
  full<-rbind(train,test)
  
  
  full <- full %>%
    
    mutate(
      
      Age = ifelse(is.na(Age), mean(full$Age, na.rm=TRUE), Age),
      
      Age_Group = case_when(Age < 13 ~ "Age.00_12", 
                            
                            Age >= 13 & Age < 18 ~ "Age.13_17",
                            
                            Age >= 18 & Age < 60 ~ "Age.18_59",
                            
                            Age >= 60 ~ "Age.60+"))
  
  full_display=full
  
  full$Embarked <- replace(full$Embarked, which(is.na(full$Embarked)), 'S')
  
  names <- full$Name
  
  title <-  gsub("^.*, (.*?)\\..*$", "\\1", names)
  
  full$title <- title
  
  
  full$title[full$title == 'Mlle']        <- 'Miss' 
  
  full$title[full$title == 'Ms']          <- 'Miss'
  
  full$title[full$title == 'Mme']         <- 'Mrs' 
  
  full$title[full$title == 'Lady']          <- 'Miss'
  
  full$title[full$title == 'Dona']          <- 'Miss'
  
  
  #In case of losing too much of predictive power, create a third category "Officer"
  
  full$title[full$title == 'Capt']        <- 'Officer' 
  
  full$title[full$title == 'Col']        <- 'Officer' 
  
  full$title[full$title == 'Major']   <- 'Officer'
  
  full$title[full$title == 'Dr']   <- 'Officer'
  
  full$title[full$title == 'Rev']   <- 'Officer'
  
  full$title[full$title == 'Don']   <- 'Officer'
  
  full$title[full$title == 'Sir']   <- 'Officer'
  
  full$title[full$title == 'the Countess']   <- 'Officer'
  
  full$title[full$title == 'Jonkheer']   <- 'Officer' 
  
  #Parch&SibSp
  #Families are binned into a discretized feature based on family member count.
  
  full$FamilySize <-full$SibSp + full$Parch + 1 
  
  full$FamilySized[full$FamilySize == 1] <- 'Single' 
  
  full$FamilySized[full$FamilySize < 5 & full$FamilySize >= 2] <- 'Small' 
  
  full$FamilySized[full$FamilySize >= 5] <- 'Big' 
  
  full$FamilySized=as.factor(full$FamilySized)
  
  #ticket
  ticket.unique <- rep(0, nrow(full))
  
  tickets <- unique(full$Ticket)
  
  for (i in 1:length(tickets)){
    current.ticket<-tickets[i]
    party.indexes<-which(full$Ticket==current.ticket)
    for(k in 1:length(party.indexes)){
      ticket.unique[party.indexes[k]]<- length(party.indexes)
    }
  }
  full$ticket.unique <- ticket.unique
  
  
  full$ticket.size[full$ticket.unique == 1]   <- 'Single'
  
  full$ticket.size[full$ticket.unique < 5 & full$ticket.unique>= 2]   <- 'Small'
  
  full$ticket.size[full$ticket.unique >= 5]   <- 'Big'
  
  full <- full %>%
    
    mutate(Survived = case_when(Survived==1 ~ "Yes", 
                                
                                Survived==0 ~ "No"))
  
  features<-full[1:891,c("Pclass","title","Sex","Embarked","FamilySize","ticket.size")]
  features$Survived=as.factor(train$Survived)
  
  set.seed(500)
  
  ind=createDataPartition(features$Survived,times=1,p=0.8,list=FALSE)
  
  train_val=features[ind,]
  
  train_val<-mutate_if(train_val,is.character, as.factor)
  
  ###############################################################################
  # Producing output for DataTable and RandomForest Classifier
  output$table <- renderDataTable({
    full_filterd <- full_display %>%
      filter(Sex %in% input$sex & Pclass %in% input$class & Survived %in% input$survived & Age_Group %in% input$age_group) %>%
      select(PassengerId:Fare)
    datatable(data = full_filterd, 
              options = list(pageLength = 10), 
              rownames = FALSE)
    
  })
  output$rf<-renderPlot({
    
    set.seed(17)
    model=randomForest(x = train_val[, -7], y = train_val[, 7], ntree =input$ntree,nodesize =input$node,mtry=input$mtry) 
    plot(model)
    output$rfoutcome <- renderPrint({
      print(model, digits = 3, signif.stars = FALSE)
    })
  })
  output$error1<-renderText({
    
    print('The green curve represents the error in predicting the passenger to be alive.')
  })
  output$error2<-renderText({
    
    print('The red curve represents the error in predicting the passenger to be dead.')
  })
  output$error3<-renderText({
    
    print('The black curve represents the out of bag error.')
  })
  output$error4<-renderText({
    
    print('More details of the model are listed below:')
  })
   
  
   
}) # End of the ShinyServer
shinyApp(ui, Server)