#file which contains the server logic to serve and interact with the UI elements

library(shiny)
library(lattice)
library(DT)
library(e1071)
library(PerformanceAnalytics)
library(dplyr)
library(dbscan)
library(rpart)
library(caret)

# Define server logic 
shinyServer(function(input, output) {
  
  output$main_image <- renderImage({
    list(src = "/Users/alexplutalov/Practise 2020/Burnout/Shiny/www/logo.svg",
         alt = "This is alternate text",
         height = 64
    )
  }, deleteFile = FALSE)
  
  output$table_image <- renderImage({
    list(src = "/Users/alexplutalov/Practise 2020/Burnout/Shiny/www/table.png",
         alt = "This is alternate text",
         height = 128
    )
  }, deleteFile = FALSE)
  
  #Table render burnout
  output$table <- DT::renderDataTable({
    DT::datatable(
      burn, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15,
        searching = FALSE
      )
    )
  })
  
  #Table render sound
  output$table_crisis <- DT::renderDataTable({
    DT::datatable(
      sound, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15,
        searching = FALSE
      )
    )
  })
  
  
  
  
  # SRK Analysis
  
  # Реактивное выражение для генерации запрошенного распределения ----
  # Это вызывается всякий раз, когда изменяются входы.
  # Функции вывода, определенные ниже, затем используют значение,
  # вычисленное из этого выражения
  d <- reactive({
    
    dist(input$n)
  })
  

  
  # Построение графика fday
  output$fda <- renderPlot({
    n <- input$n
    
    norm <- function(x){
      + (x-min(x))/(max(x)-min(x))}
    
    withProgress(message = 'Построение графика', value = 0, {
      # Number of times we'll go through the loop
      m <- 10
      
      for (i in 1:m) {
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/m, detail = paste("Ожидайте", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    musfday <-function (x){
      x1=(x-1)*2+1
      qq=as.vector(as.matrix(q[x1]))
      length(qq)=length(grep("K",qq)) 
      xx=apply((fday[,c(qq)]),2,norm)
      a=c(1:28)
      b=names(q[x1])
      c="February"
      levelplot(xx,type="l",aspect="fill",col.regions=gray(1:256/256),xlab=c,xlim=a,main=b)
    }
    
    musfday(n)
  })
  
  # Построение графика fdm
  output$fdm <- renderPlot({
    n <- input$n
    
    norm <- function(x){
      + (x-min(x))/(max(x)-min(x))}
    
    withProgress(message = 'Построение графика', value = 0, {
      # Number of times we'll go through the loop
      m <- 10
      
      for (i in 1:m) {
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/m, detail = paste("Ожидайте", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    musfdaymed <-function (x){
      x1=(x-1)*2+1
      qq=as.vector(as.matrix(q[x1]))
      length(qq)=length(grep("K",qq)) 
      xx=apply((fdaymed[,c(qq)]),2,norm)
      a=c(1:28)
      b=names(q[x1])
      c="February"
      levelplot(xx,type="l",aspect="fill",col.regions=gray(1:256/256),main=b,xlab=c,xlim=a)}
    
    musfdaymed(n)
  })
  
  # Построение графика fhr
  output$fhr <- renderPlot({
    n <- input$n
    
    norm <- function(x){
      + (x-min(x))/(max(x)-min(x))}
    
    withProgress(message = 'Построение графика', value = 0, {
      # Number of times we'll go through the loop
      m <- 10
      
      for (i in 1:m) {
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/m, detail = paste("Ожидайте", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    musfhr <-function (x){
      x1=(x-1)*2+1
      qq=as.vector(as.matrix(q[x1]))
      length(qq)=length(grep("K",qq)) 
      xx=apply((fhr[,c(qq)]),2,norm)
      b=names(q[x1])
      c="February(Day/Hours)"
      d="Channel"
      levelplot(xx,type="l",aspect="fill",col.regions=gray(1:256/256),main=b,xlab=c,ylab=d)}
    
    musfhr(n)
  })
  
  # Построение графика fhm
  output$fhm <- renderPlot({
    n <- input$n
    
    norm <- function(x){
      + (x-min(x))/(max(x)-min(x))}
    
    withProgress(message = 'Построение графика', value = 0, {
      # Number of times we'll go through the loop
      m <- 10
      
      for (i in 1:m) {
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/m, detail = paste("Ожидайте", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    musfhrmed <-function (x){
      x1=(x-1)*2+1
      qq=as.vector(as.matrix(q[x1]))
      length(qq)=length(grep("K",qq)) 
      xx=apply((fhrmed[,c(qq)]),2,norm)
      b=names(q[x1])
      c="February(Day/Hours)"
      d="Channel"
      levelplot(xx,type="l",aspect="fill",col.regions=gray(1:256/256),main=b,xlab=c,ylab=d)}
    
    musfhrmed(n)
  })
  
  # Построение графика feb
  output$feb <- renderPlot({
    n <- input$n
    
    norm <- function(x){
      + (x-min(x))/(max(x)-min(x))}
    
    names(feb)=as.vector(as.matrix(channel))
    
    withProgress(message = 'Построение графика', value = 0, {
      # Number of times we'll go through the loop
      m <- 10
      
      for (i in 1:m) {
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/m, detail = paste("Ожидайте", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    musfeb <-function (x){
      x1=(x-1)*2+1
      qq=as.vector(as.matrix(q[x1]))
      length(qq)=length(grep("K",qq)) 
      xx=apply((feb[,c(qq)]),2,norm)
      b=names(q[x1])
      c="February(Day/Hour/Minute)"
      d="Channel"
      levelplot(xx,type="l",aspect="fill",col.regions=gray(1:256/256),main=b,xlab=c,ylab=d)}
    
    musfeb(n)
  })
  
  
  # Linear regression
  
  
  selectData <- reactive({
    burn[, c(input$e1, input$e2)]
  })
  
      # Scatter Plot
      output$scater_line<-renderPlot(
        {
      
          withProgress(message = 'Построение графика', value = 0, {
            # Number of times we'll go through the loop
            m <- 10
            
            for (i in 1:m) {
              
              # Increment the progress bar, and update the detail text.
              incProgress(1/m, detail = paste("Ожидайте", i))
              
              # Pause for 0.1 seconds to simulate a long computation.
              Sys.sleep(0.1)
            }
          })
          
          scatter.smooth(data.frame(selectData()))
      
        }) #close renderPlot
      
      # Box Plot
      output$box_plot<-renderPlot(
        {
          
          withProgress(message = 'Построение графика', value = 0, {
            # Number of times we'll go through the loop
            m <- 10
            
            for (i in 1:m) {
              
              # Increment the progress bar, and update the detail text.
              incProgress(1/m, detail = paste("Ожидайте", i))
              
              # Pause for 0.1 seconds to simulate a long computation.
              Sys.sleep(0.1)
            }
          })
          
          par(mfrow=c(1, 2))  # divide graph area in 2 columns
          boxplot(burn[,input$e1], sub=paste("Outlier rows: ", boxplot.stats(burn[,input$e1])$out))
          boxplot(burn[,input$e2], sub=paste("Outlier rows: ", boxplot.stats(burn[,input$e2])$out))
        }) #close renderPlot
      
      # Density plot
      output$den_plot<-renderPlot(
        {
          
          withProgress(message = 'Построение графика', value = 0, {
            # Number of times we'll go through the loop
            m <- 10
            
            for (i in 1:m) {
              
              # Increment the progress bar, and update the detail text.
              incProgress(1/m, detail = paste("Ожидайте", i))
              
              # Pause for 0.1 seconds to simulate a long computation.
              Sys.sleep(0.1)
            }
          })
          
          par(mfrow=c(1, 2))  # divide graph area in 2 columns
          plot(density(burn[,input$e1]), main=names(selectData()[1]), ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(burn[,input$e1]), 2)))
          polygon(density(burn[,input$e1]), col="red")
          plot(density(burn[,input$e2]), main=names(selectData()[2]), ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(burn[,input$e2]), 2)))
          polygon(density(burn[,input$e2]), col="red")
          
        }) #close renderPlot
      
      # Correlation plot
        output$cor_plot<-renderPlot(
          {
            
            withProgress(message = 'Построение графика', value = 0, {
              # Number of times we'll go through the loop
              m <- 10
              
              for (i in 1:m) {
                
                # Increment the progress bar, and update the detail text.
                incProgress(1/m, detail = paste("Ожидайте", i))
                
                # Pause for 0.1 seconds to simulate a long computation.
                Sys.sleep(0.1)
              }
            })
            
            chart.Correlation(burn,histogram = input$cor_hist,method = input$cor_method)
          }) #close renderPlot
      
      # Linear model
        
        l_mod <- reactive({
          
          lm(burn[,input$e2] ~ burn[,input$e1],
            data=burn)
          
          })
        
        output$linear_mod <- renderPrint({
          l_mod()
        })
        
        output$linear_sum <- renderPrint({
          summary(l_mod())
        })
      
        # Predicting
        
        # Create Training and Test data
        
        set.seed(100)  # setting seed to reproduce results of random sampling
        
          trainingRowIndex <- reactive({
            sample(1:nrow(burn), (input$train_n/100)*nrow(burn))  # row indices for training data
          })
          
          trainingData <- reactive({
            burn[trainingRowIndex(), ]  # model training data
          })
          
            testData <- reactive({
            burn[-trainingRowIndex(), ]   # test datas
        })
        
        # Build the model on training data
        
            model <- reactive({
              form <- as.formula( paste( names(burn)[names(burn) %in% input$e2], "~", paste(names(burn)[names(burn) %in% input$e1], collapse="+")))
            lm(form, data=trainingData())
            })
            
        # Prediction
        res_pred <- reactive({
          
          predict(model(),testData())
          
        })
        
        mod <- reactive({
          form <- as.formula( paste( names(burn)[names(burn) %in% input$e2], "~", paste(names(burn)[names(burn) %in% input$e1], collapse="+")))
          model<-lm(form, data=trainingData())
          predict(model,testData())
        })
        
        
        # Predict and actual
        actuals_preds <- reactive({
          act <- data.frame(testData())
          data.frame(cbind(actuals=act[,input$e2], predicteds=res_pred()))# make actuals_predicteds dataframe.
        })
        
        
        # Accuracy
        
        cor_accuracy <- reactive({
          cor(actuals_preds())
        })
      
        min_max_accuracy <- reactive({
          mean(apply(actuals_preds(), 1, min) / apply(actuals_preds(), 1, max))*100 
        })
        
        mape <- reactive({
          act_pr <- data.frame(actuals_preds())
          mean(abs((act_pr$predicteds - act_pr$actuals))/act_pr$actuals)*100  
        })
        
        output$pred_sum <- renderPrint({
          summary(res_pred())
        })

        
        output$test_diag <- renderPrint({
          actuals_preds()
        })
        
        output$tpred_acur <- renderPrint({
          min_max_accuracy()
        })
        
        output$tpred_cor <- renderPrint({
          cor_accuracy()
        })
          
        output$tpred_mape <- renderPrint({
          mape()
        })
        
        # KMeans
        # Combine the selected variables into a new data frame
        
        selectedData <- reactive({
          sound[,c(input$freq_1, input$freq_2)]
        })
        
        
        clusters <- reactive({
          kmeans(selectedData(), input$clusters)
        })
        
        output$plot_kmean <- renderPlot({
          palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                    "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
          
          
          withProgress(message = 'Построение графика', value = 0, {
            # Number of times we'll go through the loop
            m <- 10
            
            for (i in 1:m) {
              
              # Increment the progress bar, and update the detail text.
              incProgress(1/m, detail = paste("Ожидайте", i))
              
              # Pause for 0.1 seconds to simulate a long computation.
              Sys.sleep(0.1)
            }
          })
          
          par(mar = c(5.1, 4.1, 0, 1))
          plot(selectedData(),
               col = clusters()$cluster,
               pch = 20, cex = 3)
          points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
        })
        
        output$kmean_accuracy <- renderPrint({
          data = clusters()
          ((sum(data$cluster==sclass))/length(sclass))*100
        })
        
        
        #Naive bayes
        
        # Create Training and Test data
        
        set.seed(100)  # setting seed to reproduce results of random sampling
        
        NB_Data <- reactive({
          sound[, c(input$freq_nb_1, input$freq_nb_2)]
        })
        
        indxTrain <- reactive({
          dat <- data.frame(NB_Data())
          sample(1:nrow(dat), (input$tr_n_nb/100)*nrow(dat))
        })
        
        train_nb <- reactive({
          dat <- data.frame(NB_Data())
          dat[indxTrain(),]  # model training data
        })
        
        test_nb <- reactive({
          dat <- data.frame(NB_Data())
          dat[-indxTrain(),]   # test datas
        })
        
        model_nb <- reactive({
          x <- data.frame(train_nb())
          y <-as.factor(sclass[indxTrain()])
          naiveBayes(x, y)
        })
        
        pred_nb <- reactive({
          predict(model_nb(), test_nb())
        })
        
        pred_nb_full <- reactive({
          predict(model_nb(), NB_Data())
        })
        
        output$pred_nb_class <- renderTable({
          tab<-table(pred_nb(), sclass[-indxTrain()])
          data.frame(cbind((c(tab[1],tab[2])),(c(tab[3],tab[4]))))
        })
        
        output$pred_nb_class_full <- renderTable({
          tab_full<-table(pred_nb_full(), sclass)
          data.frame(cbind((c(tab_full[1],tab_full[2])),(c(tab_full[3],tab_full[4]))))
        })
        
        nb_clusters<- reactive({
          as.integer(pred_nb())
          #factor(as.integer(pred_nb()))
        })
        
        nb_clusters_full<- reactive({
          as.integer(pred_nb_full())
          #factor(as.integer(pred_nb_full()))
        })

        
        output$nb_plot_test<-renderPlot(
          {
            palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                      "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
            
            
            withProgress(message = 'Построение графика', value = 0, {
              # Number of times we'll go through the loop
              m <- 10
              
              for (i in 1:m) {
                
                # Increment the progress bar, and update the detail text.
                incProgress(1/m, detail = paste("Ожидайте", i))
                
                # Pause for 0.1 seconds to simulate a long computation.
                Sys.sleep(0.1)
              }
            })
            
            hullplot(test_nb(), nb_clusters(),main = "Naive Bayes cluster")
            
            #par(mar = c(5.1, 4.1, 0, 1))
            #plot(test_nb(),
            #     col = nb_clusters(),
            #     pch = 20, cex = 3)
          })
        
        output$nb_class<-renderText({
          
          #snb_clusters()
          #nb_clusters_full()
        })
        
        output$nb_plot_full<-renderPlot(
          {
            palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                      "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
            
            
            withProgress(message = 'Построение графика', value = 0, {
              # Number of times we'll go through the loop
              m <- 10
              
              for (i in 1:m) {
                
                # Increment the progress bar, and update the detail text.
                incProgress(1/m, detail = paste("Ожидайте", i))
                
                # Pause for 0.1 seconds to simulate a long computation.
                Sys.sleep(0.1)
              }
            })
            
            hullplot(NB_Data(), nb_clusters_full(),main = "Naive Bayes cluster")
            
            #par(mar = c(5.1, 4.1, 0, 1))
            #plot(NB_Data(),
            #     col = nb_clusters(),
            #     pch = 20, cex = 3)
          })
        
        output$train_select <- renderText({ 
          paste("Размер обучающей выборки", input$tr_n_nb)
        })
        
        output$accuracy_nb<-renderText({
          
          paste("Точность ",mean(nb_clusters()==sclass[-indxTrain()])*100,"%")
        })
        
        output$accuracy_nb_full<-renderText({
          
          paste("Точность ",mean(nb_clusters_full()==sclass)*100,"%")
        })
        
        
        
        
        #SVM
        
        SVM_Data <- reactive({
          sound[, c(input$freq_svm_1, input$freq_svm_2)]
        })

        
        Radialsvm <- reactive({
          x <- data.frame(SVM_Data())
          names(x)[1] <- "x1"
          names(x)[2] <- "x2"
          x$y <- svm_class
          svm(y ~ ., data=x,kernel="radial",cost=input$cparam, gamma=input$gamma,scale=F)
        })
        
        
        pred_svm <- reactive({
          x <- data.frame(SVM_Data())
          names(x)[1] <- "x1"
          names(x)[2] <- "x2"
          x$y <- svm_class
          predict(Radialsvm(),x[,-3])
        })
        
        
        
        #SVM plot test
  
  output$svmplot<-renderPlot(
    {
      
      withProgress(message = 'Построение графика', value = 0, {
        # Number of times we'll go through the loop
        m <- 4
        
        for (i in 1:m) {
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/m, detail = paste("Ожидайте", i))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
        }
      })
      
      x <- data.frame(SVM_Data())
      names(x)[1] <- "x1"
      names(x)[2] <- "x2"
      x$y <- svm_class
      
      plot(Radialsvm(),x)
    
      })#close renderPlot
  
  
  output$predmatrix<- renderTable({
    table(pred_svm(),actual=svm_class)
    })
  
  output$accuracy_svm<-renderText({
    
    paste("Точность ",mean(pred_svm()==svm_class)*100,"%")
    })
  
 
  #DBSCAN
  
  dbs_Data <- reactive({
    sound[, c(input$freq_dbs_1, input$freq_dbs_2)]
  })
  
  res <- reactive({
    dbs_dat <- data.frame(dbs_Data())
    dbscan(dbs_dat, eps = input$eps, minPts = input$minpts)
  })
  
  pred_dbs <- reactive({
    predict(res(), dbs_Data(), data = dbs_Data())
  })
  
  output$predmatrix_dbs<- renderTable({
    table(pred_dbs(),actual=sclass)
  })
  
  output$accuracy_dbs<-renderText({
    
    paste("Точность ",mean(pred_dbs()==sclass)*100,"%")
  })
  
 
  
  output$hullplot<-renderPlot(
    {
      
      withProgress(message = 'Построение графика', value = 0, {
        # Number of times we'll go through the loop
        m <- 4
        
        for (i in 1:m) {
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/m, detail = paste("Ожидайте", i))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
        }
      })

      hullplot(dbs_Data(), res())

    })
  
  output$pred_plot<-renderPlot(
    {
      
      withProgress(message = 'Построение графика', value = 0, {
        # Number of times we'll go through the loop
        m <- 4
        
        for (i in 1:m) {
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/m, detail = paste("Ожидайте", i))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
        }
      })
      
      hullplot(dbs_Data(), pred_dbs())

      #plot(dbs_Data(), col = pred_dbs(), pch = pred_dbs())
      
    })
  
})