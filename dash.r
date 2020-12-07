suppressWarnings(library(shiny))
suppressWarnings(library(shinythemes))
suppressWarnings(library(curl))
suppressWarnings(library(ini))
suppressWarnings(library(jsonlite))

config<-read.ini("config.ini") #read config files
dash<-readChar("summary.html",file.info("summary.html")$size)#read dashboard template
ints.row<-readChar("instruments.table.rows.html", file.info("instruments.table.rows.html")$size)
ints.shell<-readChar("instruments.table.shell.html", file.info("instruments.table.shell.html")$size)

oanda.accounts<-function(account,path){
    base.url<-config$oanda$url
    a<-new_handle()
    handle_setheaders(a,"Authorization" = paste("Bearer", config$oanda$token),"Accept-Datetime-Format" = "UNIX")
    if(missing(account) & missing(path)){
        b<-curl_fetch_memory(url=paste(base.url,"account"),handle=a)
    }else if(!missing(account) & missing(path)){        
        b<-curl_fetch_memory(url=paste(base.url,"accounts/",account,sep=""),handle=a)
    }else if(!missing(account) & !missing(path)){
        b<-curl_fetch_memory(url=paste(base.url,"accounts/",account,"/",path,sep=""),handle=a)
    }else{
        b<-NULL
    }
    
    if(is.null(b)) {
        return("Please check input parameters")
    }else{
        return(jsonlite::parse_json(rawToChar(b$content)))
    }
    
}

ui <- fluidPage(#theme = shinytheme("cerulean"),
  titlePanel(config$company$name),#set the value of your name or your company name in the config file.
  
  sidebarLayout(
    sidebarPanel(width=2,
        h2("Account Activity"),
        # actionButton("summary","Summary",class="btn-success btn-sm"),
        selectInput("accounts","Accounts",choices=strsplit(config$oanda$accounts,",")[[1]],selected=strsplit(config$oanda$accounts,",")[[1]][1])

    ),
    mainPanel(width=10,
        tabsetPanel(
            tabPanel("Summary",fluid=T, htmlOutput("summary")),
            tabPanel("Account Detail",fluid=T, htmlOutput("accountDetail")),
            tabPanel("Account Activity",fluid=T, h3("Account activity goes here"))
        )                
        # conditionalPanel(condition="input.instruments % 2==1", htmlOutput("instrumentsTable"))
    )#end of main panel
  )#end of sidebarlayout
)#end of UI

server <- function(input, output) {
    autoInvalidate <- reactiveTimer(5000)    
    output$summary <- renderUI({
      autoInvalidate()      
      final_dash<-""
      for(account in strsplit(config$oanda$accounts,",")[[1]]){#itterate through the accounts    
          current.account.dash<-dash #save a local copy to manupilate        
          summary <- oanda.accounts(account,"summary")
          
          for ( tag in names(summary$account) ){
              if(grepl("time",tolower(tag),fixed=TRUE)){#we need to convert time
                date<-as.Date(as.POSIXct(as.numeric(summary$account[[tag]]),origin="1970-01-01"))
                current.account.dash<-sub(sprintf("\\b%s\\b",tag), date, current.account.dash)
              }else{
                current.account.dash<-sub(sprintf("\\b%s\\b",tag), summary$account[[tag]], current.account.dash)
              }      
          }
          current.account.dash<-sub("lastUpdateTimeStamp",Sys.time(),current.account.dash)
          final_dash <- paste(final_dash,current.account.dash,sep="\n") #saving it to the final dashboard UI
      }
    HTML(final_dash)
    })
    #When I select the account. It should fetch data corresponding to that account and get it back.
    output$accountDetail<-renderUI({ 
        #Detail about a single account
        current.account.dash<-dash #save a local copy to manupilate        
        summary <- oanda.accounts(input$accounts,"summary")#download account specific details
        
        for ( tag in names(summary$account) ){
            if(grepl("time",tolower(tag),fixed=TRUE)){#we need to convert time
            date<-as.Date(as.POSIXct(as.numeric(summary$account[[tag]]),origin="1970-01-01"))
            current.account.dash<-sub(sprintf("\\b%s\\b",tag), date, current.account.dash)
            }else{
            current.account.dash<-sub(sprintf("\\b%s\\b",tag), summary$account[[tag]], current.account.dash)
            }      
        }
        current.account.dash<-sub("lastUpdateTimeStamp",Sys.time(),current.account.dash)
        
        #Instrument restriction/specification of this account         
        inst<-oanda.accounts(input$accounts,"instruments")        
        total_rows=""
        for(y in inst$instruments){
            temp<-ints.row # for every new currency restablish the template for rows
            for(name in names(y)){
                if(name=="financing"){        
                    finance <- y[[name]]
                    temp<- sub("longRate", finance[["longRate"]], temp)#long rate
                    temp<- sub("shortRate", finance[["shortRate"]], temp)#short rate
                    for(x in finance$financingDaysOfWeek){
                        temp<- sub(x$dayOfWeek, x$daysCharged, temp)#short rate
                    }
                }else if(name=="tags"){

                }else{
                    # temp<- sub(name, y[name], temp)
                    if(name=="maximumOrderUnits"){
                        temp<- sub(name, format(as.numeric(y[[name]]),big.mark=","), temp)                            
                    }else{
                        temp<- sub(name, y[[name]], temp)
                    }
                }        
            }
            total_rows<-paste(total_rows,temp,sep="\n")
        }    
        instruments.table<-sub("insertRowsHere",total_rows,ints.shell)
        
        HTML( paste0(current.account.dash, instruments.table) )#finally render the html                
    })
    
}

shinyApp(ui = ui, server = server)