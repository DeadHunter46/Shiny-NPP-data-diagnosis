#Shiny app to create a Support vector machine application 
library(shiny)
library(shinythemes) #to load shiny themes



# Подключаем базу данных

#Burnout93
burnout <- read.csv('/Users/alexplutalov/Practise 2020/Burnout/Shiny/data/Burnout.csv', header = TRUE, dec = ',', stringsAsFactors = FALSE)

burn$consumption <-as.numeric(burnout$consumption)
burn$pressure <-as.numeric(burnout$pressure)
burn$temperature <-as.numeric(burnout$temperature)
burn$crit_power <-as.numeric(burnout$crit_power)
y<-as.factor(burnout$crit_power)

#SRK
alldata <- load("/Users/alexplutalov/Shiny app/SRK/data/All.RData")
fda <- load("/Users/alexplutalov/Shiny app/SRK/data/fda.Rdata")
fbase <- load("/Users/alexplutalov/Shiny app/SRK/data/feb.Rdata")
fdm <- load("/Users/alexplutalov/Shiny app/SRK/data/fdm.Rdata")
fha<- load("/Users/alexplutalov/Shiny app/SRK/data/fha.Rdata")
fhm <- load("/Users/alexplutalov/Shiny app/SRK/data/fhm.Rdata")

#Burnout sound(var25)
s <- read.csv('/Users/alexplutalov/Practise 2020/Burnout/Shiny/data/sound.csv', header = FALSE, dec = ',', stringsAsFactors = FALSE)
sound<-apply(s, 2, as.numeric)
sclass<-c(rep(1,23), rep(2,21))
svm_class<-as.factor(c(rep(1,23), rep(-1,21)))

#creating the UI for the app
shinyUI(navbarPage("Data analysis",
                   
                   #setting theme
                   theme = shinytheme("yeti"),
                   
                   tabPanel("Главная",
                            
                            tags$div(imageOutput('main_image'),
                                style = "background-color:#333333;
                                height: 176px;
                                width: 100%;
                                padding-top: 56px;
                                padding-bottom: 56px;
                                padding-left: 56px;"
                                     ),
                            tags$div("Приложение к магистерской диссертации",style="color:black;
                            padding-left: 56px;
                            padding-top: 56px;
                            font-size: 24px;
                            font-weight: 400;
                                      "),
                            tags$div(tags$div("Система обработки данных",style="color:black;
                            padding-left: 56px;
                            font-size: 56px;
                            font-weight: 800;
                                      "),
                                     tags$div("Построенная на программном комплексе «Shiny»",style="color:black;
                            
                            padding-left: 56px;
                            font-size: 48px;
                            font-weight: 600;
                                      ")
                                      ,style="padding-bottom: 32px;
                                      padding-top: 16px
                                      "),
                            tags$hr(style="color:#cccccc;
                                      "),
                            tags$div(tags$div("Автор:",style="color:black;
                            padding-left: 56px;
                            font-size: 20px;
                            font-weight: 400;
                                      "),
                                     tags$div("Cтудент группы ТД-М18",style="color:black;
                                     padding-left: 56px;
                                     font-size: 24px;
                                     font-weight: 600;
                                      "),
                                     tags$div("Плуталов Александр",style="color:black;
                                     padding-left: 56px;
                                     font-size: 24px;
                                     font-weight: 600;
                                      ")
                                     ,style="padding-bottom: 32px;
                                      padding-top: 32px
                                      ")
                            

                   ),

                   navbarMenu("СРК",
                              tabPanel("По Дням",
                                       
                                       # Боковая панель для ввода ----
                                       sidebarPanel(
                                         
                                         # Вход: ползунок для номера столбца ----
                                         sliderInput("n", label = h3("Столбец данных"), min = 1, 
                                                     max = 57, value = 1),
                                         hr(),
                                         
                                         helpText("Внимание: Для выбора столбца из выборки данных",
                                                  "вы можете использовать ползунок,",
                                                  "чтобы выбрать нужный вам столбец.")
                                         
                                       ),
                                       
                                       # Основная панель для отображения выходов
                                       mainPanel(
                                         
                                         # Разделение на вкладки
                                         tabsetPanel(type = "tabs",
                                                     tabPanel("Усреднение по дням", titlePanel("Обработка данных СРК за Февраль"), plotOutput("fda")),
                                                     tabPanel("Медианы по дням", plotOutput("fdm"))
                                         )
                                         
                                       )
                              ), 
                              
                              tabPanel("По Дням и часам",
                                       
                                       # Боковая панель для ввода ----
                                       sidebarPanel(
                                         
                                         # Вход: ползунок для номера столбца ----
                                         sliderInput("n", label = h3("Столбец данных"), min = 1, 
                                                     max = 57, value = 1),
                                         hr(),
                                         
                                         helpText("Внимание: Для выбора столбца из выборки данных",
                                                  "вы можете использовать ползунок,",
                                                  "чтобы выбрать нужный вам столбец.")
                                         
                                       ),

                                       mainPanel(
                                         
                                         # Разделение на вкладки
                                         tabsetPanel(type = "tabs",
                                                     tabPanel("Усреднение по часам", plotOutput("fhr")),
                                                     tabPanel("Медианы по часам", plotOutput("fhm")),
                                                     tabPanel("Усреднение по минутам", plotOutput("feb"))
                                         )
                                         
                                       )
                              )
                   
                   
                   ),
                   
                   navbarMenu("Кризис теплообмена",
                              
                              tabPanel("Таблица данных",
                                       DT::dataTableOutput("table")
                              ),
                              tabPanel("Линейная регрессия",
                                       
                                       #the sidebar panel consisting of the SVM parameters sliders
                                       sidebarPanel(
                                         strong(h4("Выберите признаки",style="color:black")),
                                         
                                         selectizeInput(
                                           'e1', 'Первый признак',
                                           choices = names(burn)
                                         ),
                                         
                                         selectizeInput(
                                           'e2', 'Второй признак',
                                           choices = names(burn),
                                           selected = names(burn)[2]
                                         ),
                                         
                                         checkboxInput('cor_check','Построить корреляционную матрицу', value = F)
                                         
                                       ),
                                       
                                       #the main panel
                                       mainPanel(
                                         tabsetPanel(type = "tabs",
                                                     tabPanel("Scatter Plot", plotOutput("scater_line")),
                                                     tabPanel("BoxPlot", plotOutput("box_plot")),
                                                     tabPanel("Density plot", plotOutput("den_plot"))
                                                     
                                         ),
                                         conditionalPanel(
                                           condition = "input.cor_check == 1",
                                             # Если чекбокс активен появляется график и настройки для него
                                           h4("Корреляционная матрица",style="color:black"),
                                           hr(),
                                           radioButtons('cor_method', 'Метод корреляции', c(Pearson = "pearson", Kendall = "kendall", Spearman = "spearman"), selected = NULL, inline = TRUE),
                                           checkboxInput('cor_hist','Добавить гистограмму', value = F),
                                           hr(),
                                           plotOutput("cor_plot")
                                         ),
                                         
                                         h4("Линейная регрессия",style="color:black"),
                                         tabsetPanel(type = "tabs",
                                                     tabPanel("Linear model",
                                                              verbatimTextOutput("linear_mod"),
                                                              checkboxInput('line_sum','Отобразить Summary', value = F),
                                                              conditionalPanel(
                                                                condition = "input.line_sum == 1",
                                                                verbatimTextOutput("linear_sum")
                                                              )
                                                              ),
                                                     tabPanel("Predicting",
                                                              sliderInput("train_n", label = h6("Размер обучающей выборки"), min = 10, 
                                                                          max = 100, value = 80),
                                                              checkboxInput('pred_sum_check','Отобразить Summary', value = F),
                                                              conditionalPanel(
                                                                condition = "input.pred_sum_check == 1",
                                                                verbatimTextOutput("pred_sum")
                                                              ),
                                                              hr(),
                                                              h5("Точность предсказания в процентах",style="color:black"),
                                                              verbatimTextOutput("tpred_acur"),
                                                              hr(),
                                                              h5("Корреляция точности",style="color:black"),
                                                              verbatimTextOutput("tpred_cor"),
                                                              hr(),
                                                              h5("Среднее абсолютное отклонение в процентах",style="color:black"),
                                                              verbatimTextOutput("tpred_mape"),
                                                              hr(),
                                                              h5("Результаты предсказания",style="color:black"),
                                                              verbatimTextOutput("test_diag")
                                                              
                                                              
                                                              
                                                              )
                                                     
                                         )
                                         

                                         
                                         
                                       )
                                       
                              )
                              
                              
                              
                   ),
                   
                   navbarMenu("Спектр акустических шумов",
                              
                              tabPanel("Таблица данных",
                                       DT::dataTableOutput("table_crisis")
                              ),
                              
                              tabPanel("Kmean",
                                       sidebarLayout(
                                         sidebarPanel(
                                           sliderInput("freq_1", "Частота 1", min = 1, 
                                                       max = 200, value = 80),
                                           sliderInput("freq_2", "Частота 2", min = 1, 
                                                       max = 200, value = 70),
                                           numericInput('clusters', 'Количество кластеров', 2, min = 1, max = 5)
                                         ),
                                         mainPanel(
                                           plotOutput("plot_kmean"),
                                           h5("Точность",style="color:black"),
                                           verbatimTextOutput("kmean_accuracy")
                                         )
                                       )
                              ),
                              tabPanel("Naive Bayes",
                                       sidebarLayout(
                                         sidebarPanel(
                                           sliderInput("freq_nb_1", "Частота 1", min = 10, 
                                                       max = 200, value = 80),
                                           sliderInput("freq_nb_2", "Частота 2", min = 1, 
                                                       max = 200, value = 70),
                                           sliderInput("tr_n_nb", "Размер обучающей выборки, %", min = 1, 
                                                       max = 100, value = 70)
                                           ),
                                         
                                         mainPanel(
                                           tabsetPanel(type = "tabs",
                                                       tabPanel("Проверочная выборка",
                                                                tags$div("",style="color:black;
                                                                padding-top: 32px"),
                                                                textOutput("train_select"),
                                                                tags$div("",style="color:black;
                                                                padding-top: 32px"),
                                                                verbatimTextOutput("nb_class"),
                                                                plotOutput("nb_plot_test"),
                                                                tableOutput("pred_nb_class"),
                                                                textOutput("accuracy_nb"),
                                                                hr(),
                                                                tags$div("Инструкция по чтению таблицы",style="color:black;
                                                                font-size: 16px;
                                                                font-weight: 800;"),
                                                                tags$div(imageOutput('table_image'),
                                                                         style = "height: 176px;
                                                                         width: 100%;
                                                                         padding-top: 24px;
                                                                         padding-bottom: 24px;"),
                                                                tags$div("А и В -  классы",style="color:black;
                                                                font-size: 16px;
                                                                font-weight: 400;"),
                                                                tags$div("Числа, находящиеся на пересечении одинаковых классов
                                                                - количество правильно предсказанных классов",style="color:black;
                                                                font-size: 16px;
                                                                font-weight: 400;"),
                                                                tags$div("Числа, находящиеся на пересечении разных классо
                                                                - количество не правильно предсказанных классов",
                                                                style="color:black;
                                                                font-size: 16px;
                                                                font-weight: 400;")
                                                                ),
                                                       tabPanel("Полная выборка",
                                                                tags$div("",style="color:black;
                                                                padding-top: 32px"),
                                                                plotOutput("nb_plot_full"),
                                                                hr(),
                                                                tableOutput("pred_nb_class_full"),
                                                                textOutput("accuracy_nb_full")
                                                                )
                                           )
                                           
                                         )
                                       )
                              ),
                              tabPanel("SVM",
                                       sidebarLayout(
                                         sidebarPanel(
                                           sliderInput("freq_svm_1", "Частота 1", min = 10, 
                                                       max = 200, value = 80),
                                           sliderInput("freq_svm_2", "Частота 2", min = 1, 
                                                       max = 200, value = 70),
                                           sliderInput("cparam","Параметр регуляризации C",value = 10, min = 0.1,
                                                       max = 100, step = 0.5),
                                           sliderInput("gamma","Параметр настройки Gamma",value = 1, min = 0.001,
                                                       max = 20, step = 0.05)
                                         ),
                                         
                                         mainPanel(
                                           plotOutput("svmplot"),#to display the plot output
                                           hr(),
                                           tableOutput("predmatrix"), #to display the confusion matrix
                                           textOutput("accuracy_svm")
                                           
                                         )
                                       )
                              ),
                              tabPanel("DBScan",
                                       sidebarLayout(
                                         sidebarPanel(
                                           sliderInput("freq_dbs_1", "Частота 1", min = 10, 
                                                       max = 200, value = 80),
                                           sliderInput("freq_dbs_2", "Частота 2", min = 1, 
                                                       max = 200, value = 70),
                                           sliderInput("eps","Размер окрестности соседа",value = 0.05, min = 0.1,
                                                       max = 10, step = 0.1),
                                           sliderInput("minpts","Минимальное количество точек в окрестности",value = 5, min = 1,
                                                       max = 20, step = 1)
                                         ),
                                         
                                         mainPanel(
                                           tabsetPanel(type = "tabs",
                                                       tabPanel("HullPlot",
                                                                plotOutput("hullplot")#to display the plot output
                                                       
                                           ),
                                           tabPanel("Prediction",
                                                    plotOutput("pred_plot"),#to display the plot output
                                                    hr(),
                                                    tableOutput("predmatrix_dbs"), #to display the confusion matrix
                                                    textOutput("accuracy_dbs")
                                           )
                                         )
                                           
                                           
                                         )
                                       )
                              )
                              
                   )
              
                        
)
)



