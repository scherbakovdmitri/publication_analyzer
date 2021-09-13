library(shiny)
library(shinyjs)
require(tidyverse)
library('rvest')
library('httr')
library(lubridate)
library(plotly)
  

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Анализ библиометрических показателей"),
  h4("В этом разделе можно собрать библиотечную статистику из РНБ и ГПИБ"),
  h5("основной показатель - количество публикаций за период, знак * в запросе позволяет искать слова с разными окончаниями"),
  #h5("из результатов удаляются наименования с одинаковым заглавием"),
    sidebarLayout(
    sidebarPanel(
      radioButtons("Source", "Выберите библиотеку:",c("ГПИБ","РНБ")),
      textInput("Search","Введите запрос", value = "Бонч-Бруевич*"), #"Чернышевск*"),
      actionButton("searchButton", "Выполнить поиск", class = "btn-success"),
      hidden(downloadButton("downloadData", "Скачать данные")),
      br(),
      hidden(radioButtons("GraphType", "Выберите тип диаграммы:",c("Обычная","Гистограмма"))),
      hidden(checkboxInput("Exclude", "Исключить слова запроса из поля 'Автор публикации' (пример-пояснение: не выводить работы, автором которых был сам Чернышевский)", value = T))
    ),
    mainPanel(
      #tableOutput("SearchWindow"),
      plotOutput("SearchPlot"),
      hidden(sliderInput("year1", "Временной диапазон:",sep='',width='100%',
                         min = 1700, max = 2021, value = c(1900,2021))),
      hidden(h5('Топ-10 авторов по заданному поисковому запросу:')),
      tableOutput("dftotal")
    )
    
  ),

  hr(),
  h4("В этом разделе можно сравнить статистику, собранную выше"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Загрузить набор данных 1", accept = ".csv"),
      fileInput("file2", "Загрузить набор данных 2", accept = ".csv"),
      fileInput("file3", "Загрузить набор данных 3", accept = ".csv"),
      #submitButton(text = "Отобразить график", icon = NULL, width = NULL)
      actionButton("goButton", "Отобразить график", class = "btn-success")
      ),
    mainPanel(
      #tableOutput("contents")
      plotOutput("contents"),
      hidden(sliderInput("year", "Временной диапазон:",sep='',width='100%',
                       min = 1700, max = 2021, value = c(1900,2021)))
      )
 
  ),

)
server <- function(input, output,session) {
  df1=data.frame()
  df2=data.frame()
  df3=data.frame()
  df.total=data.frame()
  
  dataModal <- function(failed = FALSE) {
    total=0
    if (input$Source=="РНБ")
    {
      url=paste0("https://primo.nlr.ru/primo_library/libweb/action/search.do?fn=search&ct=search&initialSearch=true&mode=Advanced&tab=default_tab&indx=1&dum=true&srt=rank&vid=07NLR_VU1&frbg=&vl%28199890271UI0%29=title&vl%28199890271UI0%29=title&vl%28199890271UI0%29=lsr24&vl%281UIStartWith0%29=contains&vl%28freeText0%29=",
                 URLencode(input$Search),
                 "&vl%28boolOperator0%29=AND&vl%28199949086UI1%29=lsr24&vl%28199949086UI1%29=title&vl%28199949086UI1%29=lsr24&vl%281UIStartWith1%29=contains&vl%28freeText1%29=&vl%28boolOperator1%29=AND&vl%28267247494UI2%29=lsr24&vl%28267247494UI2%29=title&vl%28267247494UI2%29=lsr24&vl%281UIStartWith2%29=contains&vl%28freeText2%29=&vl%28boolOperator2%29=AND&vl%28267247768UI3%29=lsr24&vl%28267247768UI3%29=title&vl%28267247768UI3%29=lsr24&vl%281UIStartWith3%29=contains&vl%28freeText3%29=&vl%28boolOperator3%29=AND&vl%28199950180UI4%29=all_items&vl%28199950185UI5%29=all_items&vl%28422913607UI6%29=all_items&Submit=",
                 URLencode("Поиск"))
      uastring <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.1.1 Safari/605.1.15"
      page<- rvest::session(url,user_agent(uastring))
      
      total=page %>% html_element("#resultsNumbersTileBottom") %>% html_text2() %>% str_extract("из[0-9 ]+в") %>% gsub("[^0-9]","",x=.) %>% as.integer
      if (is.na(total)) {total=0;}
      total_time = (ceiling(total/20)+1)*2 
    }
    else
    {
      url<-paste0("https://unis.shpl.ru/Pages/Search/BooksSearch.aspx?q=",
                  URLencode(input$Search),
                  "&t=1%3b2")
      uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
      page<- rvest::session(url,user_agent(uastring))
      total=(page %>% html_form())[[1]] %>% pluck(5) %>% pluck('ctl00$MainPlaceHolder$ToolsContainerControl$PagingControl$_hfTotalRecords') %>% 
        pluck(3) %>% as.integer
      total_time=(ceiling(total/200)+1)*5 
    }
    if (total==0) total_time=0;
    
    modalDialog(easyClose=T,
      #textInput("dataset", "Choose data set",
      #          placeholder = 'Try "mtcars" or "abc"'
      #),
    span(paste0("Найдено: ",total," изданий. Процесс займет около ",total_time," сек., продолжить?")),
    #  if (failed)
    #    div(tags$b("Invalid name of data object", style = "color: red;")),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ok", "ОК")
    )
    )
    
  }
  
  observeEvent(input$goButton,
  {
    
    file1 <- input$file1
    ext <- tools::file_ext(file1$datapath)
    req(file1)
    validate(need(ext == "csv", "Загрузите сюда сохраненный csv файл"))
    df.1=read.csv(file1$datapath)
    df.total=df.1 %>% mutate(Источник=file1$name) 
    
    file2 <- input$file2
    if (!is.null(file2))
    {
    ext <- tools::file_ext(file2$datapath)
    validate(need(ext == "csv", "Загрузите сюда сохраненный csv файл"))
    df2=read.csv(file2$datapath)
    df.total=bind_rows(df.total,df2%>% mutate(Источник=file2$name))
    }
    
    file3 <- input$file3
    if (!is.null(file3))
    {
      ext <- tools::file_ext(file3$datapath)
      validate(need(ext == "csv", "Загрузите сюда сохраненный csv файл"))
      df3=read.csv(file3$datapath)
      df.total=bind_rows(df.total,df3%>% mutate(Источник=file3$name) )
    }
  
  output$contents <- renderPlot({
    if (input$goButton>0) 
    {
      by=((lubridate::parse_date_time(input$year,'y')[2]-lubridate::parse_date_time(input$year,'y')[1])/30) %>% lubridate::as.duration() 
      
      shinyjs::show('year',time=1)
      df.total %>% 
      distinct(Title,.keep_all=T) %>% 
      #group_by(Year) %>% tally %>% arrange(desc(n))
      ggplot(aes(fill=Источник, x=lubridate::parse_date_time(Year,'y')))+
      geom_bar(position=position_dodge2(padding=0,preserve ="single",width=as.duration("1 year")))+
      theme(legend.position='top')+
      scale_x_datetime(date_breaks=paste0(ceiling(by/duration("1 year"))," years"), date_labels = "%Y",limits=lubridate::parse_date_time(input$year,'y'))+
      xlab("Год издания")+ylab("Кол-во публикаций") 
    
    
    
    }
      
    })
  
  })
  
  

  
  observeEvent(input$ok,
 {
   removeModal()
                if (input$Source=="РНБ")
                {
                  url=paste0("https://primo.nlr.ru/primo_library/libweb/action/search.do?fn=search&ct=search&initialSearch=true&mode=Advanced&tab=default_tab&indx=1&dum=true&srt=rank&vid=07NLR_VU1&frbg=&vl%28199890271UI0%29=title&vl%28199890271UI0%29=title&vl%28199890271UI0%29=lsr24&vl%281UIStartWith0%29=contains&vl%28freeText0%29=",
                             URLencode(input$Search),
                             "&vl%28boolOperator0%29=AND&vl%28199949086UI1%29=lsr24&vl%28199949086UI1%29=title&vl%28199949086UI1%29=lsr24&vl%281UIStartWith1%29=contains&vl%28freeText1%29=&vl%28boolOperator1%29=AND&vl%28267247494UI2%29=lsr24&vl%28267247494UI2%29=title&vl%28267247494UI2%29=lsr24&vl%281UIStartWith2%29=contains&vl%28freeText2%29=&vl%28boolOperator2%29=AND&vl%28267247768UI3%29=lsr24&vl%28267247768UI3%29=title&vl%28267247768UI3%29=lsr24&vl%281UIStartWith3%29=contains&vl%28freeText3%29=&vl%28boolOperator3%29=AND&vl%28199950180UI4%29=all_items&vl%28199950185UI5%29=all_items&vl%28422913607UI6%29=all_items&Submit=",
                             URLencode("Поиск"))
                  uastring <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.1.1 Safari/605.1.15"
                  page<- rvest::session(url,user_agent(uastring))
                  
                  total=page %>% html_element("#resultsNumbersTileBottom") %>% html_text2() %>% str_extract("из[0-9 ]+в") %>% gsub("[^0-9]","",x=.) %>% as.integer
                  
                  progress <- shiny::Progress$new(session, min=1, max=(ceiling(total/20)+1))
                  on.exit(progress$close())
                  progress$set(message = 'Сканирование каталога...',
                               detail = paste0('Всего результатов: ',total))
  
                  #df.total=data.frame()
                  for (i in 1:(ceiling(total/20)+1))
                  {
                    print(i)
                    progress$set(value = i)
                    tryCatch(
                      {
                        res=(page %>% html_elements(".EXLSummary"))[-1]
                        Year=res %>% html_element(".EXLResultFourthLine") %>% html_text() %>% str_extract("[0-9]{4}") %>% as.integer
                        Title=res %>% html_element(".EXLResultTitle") %>% html_text() %>% gsub("[\n\t]","",x=.) 
                        Author=res %>% html_element(".EXLResultAuthor") %>% html_text() #%>% gsub("[\n\t]","",x=.) 
                        #Type=page %>% html_element(".EXLThumbnailCaption") %>% html_text() 
                        
                        
                        df=data.frame(Year,Title,Author) 
                        df.total=bind_rows(df.total,df)
                        print(df %>% dim)
                        page=page %>% session_follow_link(css='.EXLNext')
                        
                      },
                      finally=next
                    )
                    
                  }
                  df.ch.rnb=df.total
                  

                  
                  
                }
                else
                {

                  
                  url<-paste0("https://unis.shpl.ru/Pages/Search/BooksSearch.aspx?q=",
                              URLencode(input$Search),
                              "&t=1%3b2")
                  
                  #pageheaders=add_headers("ctl00$MainPlaceHolder$ToolsContainerControl$PagingControl$_tbCurrentPage"="3")
                  uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
                  page<- rvest::session(url,user_agent(uastring))
                  total=(page %>% html_form())[[1]] %>% pluck(5) %>% pluck('ctl00$MainPlaceHolder$ToolsContainerControl$PagingControl$_hfTotalRecords') %>% 
                    pluck(3) %>% as.integer
                  
                  progress <- shiny::Progress$new(session, min=1, max=(ceiling(total/200)+1))
                  on.exit(progress$close())
                  progress$set(message = 'Сканирование каталога...',
                               detail = paste0('Всего результатов: ',total))
                  
                  #df.total=data.frame()
                  for (i in 1:(ceiling(total/200)+1))
                  {
                    #Sys.sleep(20)
                    print(i)
                    progress$set(value = i)
                    
                    df=page %>% read_html %>% html_elements(".grid-view") %>% html_table() %>% pluck(1) 
                    df.total=bind_rows(df.total,df)
                    print(df %>% dim)
                    
                    page=page %>% session_submit(x=.,(page %>% html_form())[[1]] %>%                      
                                                   html_form_set(
                                                     "ctl00$MainPlaceHolder$PagingFooterControl$CurrentPageTbox"=i,
                                                     "ctl00$MainPlaceHolder$ToolsContainerControl$PagingControl$_tbCurrentPage"=i,
                                                     "ctl00$MainPlaceHolder$PagingFooterControl$RecordsPerPageDdl"=200,
                                                     "ctl00$MainPlaceHolder$ToolsContainerControl$PagingControl$_ddlRecordsPerPage"=200
                                                   ),
                                                 "ctl00$MainPlaceHolder$PagingFooterControl$ForwardBtn")
                    
                
                    
                  }
                  df.total=df.total %>% 
                    rename(Year='Год издания',Title=Название,Author=`Автор/Редактор`) 
                  
                  
                }
    
   updateSliderInput(session, "year1",
                     value = c(min(as.integer(df.total$Year),na.rm=T),max(as.integer(df.total$Year),na.rm=T)))
                                   
   output$SearchPlot <- renderPlot({
     by=((lubridate::parse_date_time(input$year1,'y')[2]-lubridate::parse_date_time(input$year1,'y')[1])/30) %>% lubridate::as.duration() 
     #breaks=lubridate::floor_date(unit='month',seq(lubridate::parse_date_time(input$year1,'y')[1], by = by, length.out = 30))
     shinyjs::show('year1')
     shinyjs::show('TableTitle')
     shinyjs::show('downloadData')
     shinyjs::show('GraphType')
     shinyjs::show('Exclude')
     
     
     df.total %>%
       filter(if (input$Exclude==T) !str_detect(Author,gsub("\\*","",input$Search)) else T) %>% 
       distinct(Title,.keep_all=T) %>% 
       mutate(Year=lubridate::parse_date_time(Year,'y')) %>% 
       ggplot(aes(x=Year))+
       (if(input$GraphType=='Обычная') 
         geom_bar(position=position_dodge2(padding=0,preserve ="single",width=as.duration("1 year")))
        else geom_histogram()) +
       scale_x_datetime(date_breaks=paste0(ceiling(by/duration("1 year"))," years"), date_labels = "%Y",limits=lubridate::parse_date_time(input$year1,'y'))+
       #scale_x_datetime(breaks=breaks,date_labels = "%Y",limits=lubridate::parse_date_time(input$year1,'y'))+
       xlab("Год издания")+ylab("Кол-во публикаций")
   })
   
   output$dftotal <- renderTable({

     df.total %>%
       filter(if (input$Exclude==T) !str_detect(Author,gsub("\\*","",input$Search)) else T) %>% 
       distinct(Title,.keep_all=T) %>% 
       mutate(Year=lubridate::parse_date_time(Year,'y')) %>% 
       group_by(Author) %>% tally(sort=T) %>% head(10)
   })
   
   output$downloadData <- downloadHandler(
     filename = function() {
       paste(input$Source,"-",input$Search,"-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       if (nrow(df.total)>0)
       write.csv(df.total %>% 
                   filter(if (input$Exclude==T) !str_detect(Author,gsub("\\*","",input$Search)) else T) 
                   , file)
     }
   )
                 
                 
  })
  

  
  observeEvent(input$searchButton, {
    showModal(dataModal())
  }) 
  

  
}
shinyApp(ui = ui, server = server)
