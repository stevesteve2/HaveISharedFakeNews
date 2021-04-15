# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

#Load in Packages
library(shiny)
library(rtweet)
library(dplyr)
library(glue)
library(reactable)
library(purrr)
library(quanteda)
library(rtweet)
library(mongolite)
library(ids)
library(shinythemes)
library(tidytext)
library(htmltools)
library(tiktokr)
library(reticulate)
library(scales)
library(shinyscreenshot)

#library(tiktokr)
#library(reticulate)
#use_python(py_config()$python)
#tk_install()
#tk_init()
#user_posts <- tk_posts(scope = "user", query = "steverathje", n = 1000)
#print(user_posts)

#HTML
bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

###WWW
if (!fs::dir_exists("www")) fs::dir_create("www")

#Create Dictionaries
ProDemFakeNews <- scan("Dictionaries/ProDemocratFakeNews.txt", what='character', sep="\n", skipNul = TRUE)
ProDemFakeNews <- strsplit(ProDemFakeNews, "[[:space:]]+")
ProRepubFakeNews <- scan("Dictionaries/ProRepublicanFakeNews.txt", what='character', sep="\n", skipNul = TRUE)
ProRepubFakeNews <- strsplit(ProRepubFakeNews, "[[:space:]]+")
FakeNewsGeneral <- scan("Dictionaries/FakeNewsGeneral.txt", what='character', sep="\n", skipNul = TRUE)
FakeNewsGeneral <- strsplit(FakeNewsGeneral, "[[:space:]]+")
DemocratRealNews <- scan("Dictionaries/DemocratRealNews.txt", what='character', sep="\n", skipNul = TRUE)
DemocratRealNews <- strsplit(DemocratRealNews, "[[:space:]]+")
LeaningDemocratic <- scan("Dictionaries/LeaningDemocratic.txt", what='character', sep="\n", skipNul = TRUE)
LeaningDemocratic <- strsplit(LeaningDemocratic, "[[:space:]]+")
LeaningRepublican <- scan("Dictionaries/LeaningRepublican.txt", what='character', sep="\n", skipNul = TRUE)
LeaningRepublican <- strsplit(LeaningRepublican, "[[:space:]]+")
ProRepublican <- scan("Dictionaries/ProRepublicanRealNews.txt", what='character', sep="\n", skipNul = TRUE)
ProRepublican <- strsplit(ProRepublican, "[[:space:]]+")
RealNewsGeneral <- scan("Dictionaries/RealNewsMixedNeutral.txt", what='character', sep="\n", skipNul = TRUE)
RealNewsGeneral <- strsplit(RealNewsGeneral, "[[:space:]]+")
HyperPartisan <- scan("Dictionaries/HyperPartisan.txt", what='character', sep="\n", skipNul = TRUE)
HyperPartisan <- strsplit(HyperPartisan, "[[:space:]]+")

#Read in Congress dataset 
congress <- readRDS("CongressScores.rds")
congress$favorite_count <- NULL
congress$following_count <- NULL
congress$retweet_count <- NULL

#Condition - Randomly Assign to Condition 1 or 2
condition = 1
#condition = floor(runif(1, min=0, max=2))
print(condition)

#Define fields
fields <- c("name", "score")

if (condition == 0) {
  title = "What News Have You Shared?"
}
if (condition == 1) {
  title = "What Is Your Social Media Reputation Score?"
}

#Note: Babolyn Bee (Satire) was removed from the sources
dictionary = quanteda::dictionary(list(ProDemFakeNews = ProDemFakeNews, 
                                       ProRepubFakeNews = ProRepubFakeNews,
                                       FakeNewsGeneral = FakeNewsGeneral,
                                       DemocratRealNews = DemocratRealNews,
                                       LeaningDemocratic = LeaningDemocratic,
                                       LeaningRepublican = LeaningRepublican,
                                       ProRepublican = ProRepublican, 
                                       RealNewsGeneral = RealNewsGeneral, 
                                       HyperPartisan = HyperPartisan))

#Create Dictionaries
PositiveAffect <- scan("Dictionaries/PositiveAffectLIWC2007.txt", what='character', sep="\n", skipNul = TRUE)
PositiveAffect <- strsplit(PositiveAffect, "[[:space:]]+")
NegativeAffect <- scan("Dictionaries/NegativeAffectLIWC2007.txt", what='character', sep="\n", skipNul = TRUE)
NegativeAffect <- strsplit(NegativeAffect, "[[:space:]]+")
MoralEmotional <- scan("Dictionaries/MoralEmotional.txt", what='character', sep="\n", skipNul = TRUE)
MoralEmotional <- strsplit(MoralEmotional, "[[:space:]]+")
PolarizationDictionary <- scan("Dictionaries/PolarizationDictionary.txt", what='character', sep="\n", skipNul = TRUE)
PolarizationDictionary <- strsplit(PolarizationDictionary, "[[:space:]]+")

TopRepublican <- scan("Dictionaries/MostFamousRepublicans.txt", what='character', sep="\t", skipNul = TRUE)
TopDemocrat <- scan("Dictionaries/MostFamousDemocrats.txt", what='character', sep="\t", skipNul = TRUE)
DemocratCongress <- scan("Dictionaries/DemocratCongress.txt", what='character', sep="\t", skipNul = TRUE)
RepublicanCongress <- scan("Dictionaries/RepublicansCongress.txt", what='character', sep="\t", skipNul = TRUE)
liberalidentity = c("socialist*", "communist*", "marxist*", "leftist*", "liberal*", "left-wing*", "progressive*", "social justice warrior", "antifa", "democrat*", "dem", "dems", "libs")
conservativeidentity = c("conservative*", "gop", "republican*", "libertarian*", "alt-right", "right-wing", "fascist*", "far-right", "far right", "repub", "repubs", "maga")

#Note: Babolyn Bee (Satire) was removed from the sources
#the words people and political were removed from the polarization dictionary
emotiondictionary = quanteda::dictionary(list(PositiveAffect = PositiveAffect,
                                              NegativeAffect = NegativeAffect,
                                              MoralEmotional = MoralEmotional,
                                              Polarization = PolarizationDictionary,
                                              Democrat = TopDemocrat,
                                              Democrat = DemocratCongress,
                                              Democrat = liberalidentity,
                                              Republican = TopRepublican,
                                              Republican = RepublicanCongress,
                                              Republican = conservativeidentity))


token <- rtweet::create_token(
   app = "Second Tweet Scraper",
   consumer_key = "RCWVl1UC6pnYv7BDFArdaSiyI",
   consumer_secret = "R2bduY0muh7LHzfUbeSNIpEjvYFhin3kAPeXWQb28F30LLCxOT",
   access_token = "2170520472-51D7PwZFLymx6Ki36Il22ppJtdGBaApCl6gICf2",
   access_secret = "GML5RV1GzxwqOPb4TjJ422pE9gIBzAMvmvzeckb1JiTUX",
   set_renv = FALSE
 )

ui <- function(request) { fluidPage(
  
  shinyUI(navbarPage(title = "Have I Shared Fake News?", theme = shinytheme("cerulean"),

                    #### First Page ####
                    tabPanel("New Sites Shared", 
                            #titlePanel(title),
                            sidebarLayout(
                            sidebarPanel(
                             p(
                               "Enter your Twitter handle, and we'll tell you how much", strong("fake news"), "you've shared on your timeline. 
                               We'll also tell you the", strong("political slant"), "of the news you tend to share. Additionally, we'll tell you 
                               how much news you share from hyperpartisan websites, which often share false and misleading claims." 
                             ),
                             p("You can also enter the other people's Twitter handles (e.g., @JoeBiden)"),
                             p(""),
                             p("If you want to learn about the kind of language that you use on your Twitter account, click on the 'Language Used' tab."),
                             p(""),
                             p(
                               "Learn more about our methods under the 'about' page." 
                             ),
                             textInput(inputId = "username", label = "Insert Twitter handle (@)"),
                             radioButtons("myTwitter", label = NULL,
                                         choices = list("This is my Twitter handle" = 1, "This is someone else's Twitter handle" = 2)),
                             actionButton(inputId = "search", label = "Get News Scores"),
                             p(""),
                             actionButton("screenshot", "Share a screenshot of your results"),
                             p(""),
                             p("Share on Twitter:"), 
                             tags$a(href="https://twitter.com/intent/tweet?text=See%20if%20you%20shared%20fake%20news%20on%20Twitter%20using&url=https://steverathje.shinyapps.io/HaveISharedFakeNews/", "Tweet", class="twitter-share-button"),
                             # Copy the script from https://dev.twitter.com/web/javascript/loading into your app
                             # You can source it from the URL below. It must go after you've created your link
                             includeScript("http://platform.twitter.com/widgets.js"), 
                            ),
                             #Show a plot of the generated distribution
                             mainPanel(
                               textOutput("Counts"),
                               p(""),
                               strong(textOutput("Title")),
                               textOutput("Explanation1"),
                               p(""), 
                               reactableOutput("ScoresTable"),
                               p(""),
                               strong(textOutput("Title2")),
                               textOutput("Explanation2"), 
                               p(""),
                               reactableOutput("Table"),
                               p(""),
                               p(""),
                               #strong(textOutput("Title3")),
                               #p(""),
                               #textOutput("Explanation3"),
                               p(""),
                               uiOutput("Question"),
                               p(""),
                               #uiOutput("DemocratFavorable"),
                               #uiOutput("RepublicanFavorable"),
                               #uiOutput("lifeSatisfaction"),
                               #uiOutput("mentalHealth"),
                               textOutput("ProgressBarComplete"),
                               #uiOutput("SharingSubmit")
                             )),
                            ),
                    
                    #### Second Page ####
                    tabPanel("Language Used",
                      sidebarLayout(
                        sidebarPanel(
                          p(
                            "Enter your Twitter handle, and we'll tell you how much",
                            strong("positive, "),
                            "and",
                            strong("negative, "),
                            "language you've shared on your timeline.",
                            "We'll also share how much ",
                            strong("moral-emotional"),
                            "and",
                            strong("polarizing"),
                            "language you have shared, which tend to be associated with social media echo chambers."
                          ),
                          p(
                            "You can also enter the other people's Twitter handles (e.g., @JoeBiden)"
                          ),
                          p("Learn more about our methods under the 'about' page."),
                          textInput(inputId = "username2", label = "Insert Twitter handle (@)"),
                          
                          radioButtons(
                            "myTwitter2",
                            label = NULL,
                            choices = list(
                              "This is my Twitter handle" = 1,
                              "This is someone else's Twitter handle" = 2
                            )
                          ),
                          actionButton(inputId = "search2", label = "Get Language Scores"),
                          p(""),
                          actionButton("screenshot", "Share a screenshot of your results"),
                        ),
                        #Show a plot of the generated distribution
                        mainPanel(
                          textOutput("CountsEmotion"),
                          p(""),
                          strong(textOutput("TitleEmotion")),
                          textOutput("ExplanationEmotion"), 
                          textOutput("Tweetcount2"),
                          reactableOutput("ScoresTable2"),
                          p(""),
                          strong(textOutput("TitleEmotion2")),
                          textOutput("ExplanationEmotion2"),
                          p(""),
                          #textOutput("Moral"),
                          #textOutput("Positive"),
                          #textOutput("Negative"),
                          #textOutput("Polarizing"),
                          p(""),
                          reactableOutput("Table2"),
                          p(""), 
                          uiOutput("Question2"),
                          textOutput("ProgressBarComplete2")
                        ),
                      )
                    ),
                    
                    #### Fifth Page ####
                    #tabPanel("Questionnaires",
                    #         p("We could potentially add individual differences measures here. For instance:"),
                    #         radioButtons("MIST1", label = ("How accurate is this headline?"),
                    #                      choices = list("Not at all accurate" = 1, "Not very accurate" = 2, "Somewhat accurate" = 3, "Very accurate" = 4)),
                    #),
                    
                    tabPanel("Politicians Who Share Fake News",
                             p("Below are the fake news scores of all US congressmembers. By default, it is sorted by the most hyperpartisan news shared, but you can click on individual columns to sort by other factors. All scores were retreived on March 11, 2021, and are based on the most recent 3200 tweets from each politician at that time point."),
                             reactable(congress, 
                                       columns = list(name = colDef("Name"),
                                              twitter = colDef("Handle"),
                                              party = colDef("Party"),
                                              HyperPartisan_Count = colDef("Hyper- partisan News"),
                                              ProDemFakeNews_Count = colDef("Democrat Fake News"),
                                              ProDemRepubFakeNews_Count = colDef("Republican Fake News"),
                                              FakeNewsGeneral_Count = colDef("Fake News (General)"),
                                              LeaningDemocratic_Count = colDef("Leaning Democrat Real News"),
                                              DemocratRealNews_Count = colDef("Democrat Real News"),
                                              LeaningRepublican_Count = colDef("Leaning Republican Real News"),
                                              ProRepublican_Count = colDef("Republican  Real News"),
                                              RealNewsGeneral_Count = colDef ("Real News (General)"),
                                              followers_count = colDef("Followers")
                                              ))
                    ),
                  
                    ##### Sixth Page ####
                    tabPanel("About", 
                             p(
                               "This tool was developed by Steve Rathje (",
                               a("@steverathje2", href = "https://twitter.com/steverathje2"),
                               "), a PhD student at the University of Cambridge, with supervision from professor Sander van der Linden (",
                               a("@Sander_vdLinden", href = "https://twitter.com/Sander_vdLinden"),
                               "), a Psychology Professor at the University of Cambridge, and professor Jay van Bavel (", 
                               a("@jayvanbavel", href = "https://twitter.com/jayvanbavel"),
                               "), a Psychology Professor at New York University"
                             ),
                             
                             p("If you have any questions or suggestions about this app, please contact Steve Rathje at sjr210@cam.ac.uk"),
                             p(""),
                             
                             h4("Measuring Fake News"),
                             p(
                               "You can read about the URLS that are considered fake, left-leaning, and right-leaning ",  
                               a("here.", href = "https://osf.io/7je9y/"),
                               "This list was used in multiple scientific publications, and was assembled from a list 
                               of fake news from various fact-checking organizations, such as Politifact, Buzzfeed, and FactCheck.org.
                               The list of real news websites is from", 
                               a("allsides.com.", href = "https://www.allsides.com/"),
                               "The list of hyperpartisan websites is from this ", 
                               a("publication.", href = "https://www.pnas.org/content/116/7/2521"),
                             ),
                             
                             p(
                               "Please interpret these results with caution. We recognize that you may share certain URLs to express disagreement with them,
                               and that the kind of URL you share does not necessarily reflect your true beliefs."
                             ),
                             
                             p(
                               "Also, since this application only looks at websites that have have shared fake or hyperpartisan claims
                               in the past, it cannot detect individual fake news claims. It cannot tell if you are 
                               sharing a real news story from an untrustworthy website, or if you are sharing fake news in a meme or in 
                               tweet text."
                             ),
                             
                            p(
                               "Despite these limitations, we hope this is a useful tool for you to help understand
                               your social media diet. We hope it helps you post content that aligns with your values."
                             ),
                            
                            h4("Measuring Language"),
                            p(
                              "Positive emotion words are words like 'laugh,' 'share,' or 'honest.' 
                               Negative emotion words are words like 'sadness,' 'fail,' or 'lame.' 
                               Emotional language on social networks is contagious. For example, if you share more 
                               positive emotions, your friends and followers also tend to share more 
                               positive emotions. See more ",
                              a("here.", href = "https://www.pnas.org/content/111/24/8788"),
                            ),
                            
                            p(
                              "Moral-emotional words are words like 'evil,' 'greed,' 'hero,' or 'immoral.'
                               These words express both moral and emotional content. While tweets containing 
                               these words tend to receive more retweets, they also tend to be retweeted more 
                               by ingroup members, and not by outgroup members. See more ",
                              a("here.", href = "https://www.pnas.org/content/114/28/7313"),
                            ),
                            
                            p(
                              "Polarizing words are words that are more likely to be shared and retweeted within 
                              online echo chambers. See more ",
                              a("here.", href = "https://psyarxiv.com/xjd64/"),
                            ),
                            
                            h4("Deleting data"), 
                            p(
                              paste("If you consented to having your data from this app stored and analyzed 
                              but would like to delete your data, please email sjr210@cam.ac.uk and share your 
                              randomly generated user ID code. As a reminder, your code is:")
                            ), 
                            
                            textOutput("userid"),
                            
                            p(""),
                            p("For more details about how we store data, please read our full privacy policy", a("here.", href="HaveISharedFakeNewsPrivacy.html")),
                            )
                    
                    )
          ),
)}

# Define server logic
server <- function(input, output, session) {
  
  #Connect to database
  url_path <- "mongodb+srv://admin-steve:steve@cluster0.l1fjf.mongodb.net/fakenews" 
  mongo <- mongo(collection = "fakenews", db = "fakenews", 
                 url = url_path, 
                 verbose = TRUE)
  
  #Set user ID 
  userid <- ids::uuid(1, use_time = TRUE)
  date <- Sys.time()
  
  #Set first item in database 
  mongo$insert(data.frame(userid, date))
  
    tweet_df <- reactive({
        timeline <- get_timeline(input$username, n = 3200, token = token)
        validate( 
          need(try(length(timeline) != 0), "That Twitter handle doesn't exist or has no public tweets.")
        )
        followers <- timeline$followers_count[1]
        following <- timeline$friends_count[1]
        length <- nrow(timeline)
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"numTweets": ', length,'}}', sep=""), upsert = TRUE)
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"followers": ', followers,'}}', sep=""), upsert = TRUE)
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"following": ', following,'}}', sep=""), upsert = TRUE)
        timeline <- unlist(timeline$urls_expanded_url)
        timeline <-  gsub("https://", "", timeline)
        timeline <-  gsub("http://", "", timeline)
        timeline <-  gsub("https://www.", "", timeline)
        timeline <-  gsub("http://www.", "", timeline)
        timeline <-  gsub("www.", "", timeline)
        timelinecorpus = corpus(timeline)
        timeline_dict = dfm(timelinecorpus, dictionary = dictionary)
        timeline_dict_df <- quanteda::convert(timeline_dict, to='data.frame')
        timeline_dict_df <- cbind(timeline, timeline_dict_df)
        return(timeline_dict_df)
    })
    
# #  
#     get_scores <- reactive({
#         timeline_dict_df <- req(tweet_df())
#         ProDemFakeNews_Count <- c(sum(timeline_dict_df$ProDemFakeNews))
#         ProDemRepubFakeNews_Count <- c(sum(timeline_dict_df$ProRepubFakeNews))
#         FakeNewsGeneral_Count <- c(sum(timeline_dict_df$FakeNewsGeneral))
#         DemocratRealNews_Count <- c(sum(timeline_dict_df$DemocratRealNews))
#         LeaningDemocratic_Count <- c(sum(timeline_dict_df$LeaningDemocratic)) 
#         ProRepublican_Count <- c(sum(timeline_dict_df$ProRepublican))
#         LeaningRepublican_Count <- c(sum(timeline_dict_df$LeaningRepublican)) 
#         RealNewsGeneral_Count = c(sum(timeline_dict_df$RealNewsGeneral))
#         HyperPartisan_Count = c(sum(timeline_dict_df$HyperPartisan))
#         sum <- sum(ProDemFakeNews_Count, ProDemRepubFakeNews_Count, FakeNewsGeneral_Count, DemocratRealNews_Count, LeaningDemocratic_Count, ProRepublican_Count, LeaningRepublican_Count, RealNewsGeneral_Count, HyperPartisan_Count)
#         handle <- input$username
#         Tweetcount <- length(timeline_dict_df$timeline)
#         timeline_dict_df$timeline2 <- gsub("\\/.*","",timeline_dict_df$timeline)
#         h <- subset(timeline_dict_df, is.na(timeline_dict_df$timeline) == FALSE)
#         
#         topheadlines <- h %>%
#           dplyr::select(timeline2, ProDemFakeNews, ProRepubFakeNews, FakeNewsGeneral, DemocratRealNews, LeaningDemocratic, LeaningRepublican, ProRepublican, RealNewsGeneral, HyperPartisan) %>%
#           group_by(timeline2)
#         
#         topheadlines$Category <- NA
#         topheadlines$Category <- ifelse(topheadlines$ProDemFakeNews > 0, "Pro-Democrat Fake News", topheadlines$Category)
#         topheadlines$Category <- ifelse(topheadlines$ProRepubFakeNews > 0, "Pro-Republican Fake News", topheadlines$Category)
#         topheadlines$Category <- ifelse(topheadlines$FakeNewsGeneral > 0, "Fake News (General)", topheadlines$Category)
#         topheadlines$Category <- ifelse(topheadlines$DemocratRealNews > 0, "Democrat Real News", topheadlines$Category)
#         topheadlines$Category <- ifelse(topheadlines$LeaningDemocratic > 0, "Leaning Democrat Real News", topheadlines$Category)
#         topheadlines$Category <- ifelse(topheadlines$LeaningRepublican > 0, "Leaning Republican Real News", topheadlines$Category)
#         topheadlines$Category <- ifelse(topheadlines$ProRepublican > 0, "Pro Republican Real News", topheadlines$Category)
#         topheadlines$Category <- ifelse(topheadlines$RealNewsGeneral > 0, "Real News (General)", topheadlines$Category)
#         topheadlines$Category <- ifelse(topheadlines$HyperPartisan > 0, "HyperPartisan", topheadlines$Category)
#         topheadlines <- data.frame(topheadlines)
#         topheadlinesclassified <- subset(topheadlines, is.na(topheadlines$Category) == FALSE)
# 
#         sort <- topheadlinesclassified %>%
#           group_by(timeline2, Category) %>%
#           dplyr::summarize(n = n()) %>%
#           arrange(desc(n))
#         sort <- as.data.frame(sort)
#         return(c(ProDemFakeNews_Count, ProDemRepubFakeNews_Count, FakeNewsGeneral_Count, DemocratRealNews_Count, LeaningDemocratic_Count, ProRepublican_Count, LeaningRepublican_Count, RealNewsGeneral_Count, Tweetcount, handle, HyperPartisan_Count, sort, sum))
#     })
#     
    get_scores2 <- reactive({
      timeline_dict_df <- req(tweet_df())
      ProDemFakeNews_Count <- c(sum(timeline_dict_df$ProDemFakeNews))
      ProDemRepubFakeNews_Count <- c(sum(timeline_dict_df$ProRepubFakeNews))
      FakeNewsGeneral_Count <- c(sum(timeline_dict_df$FakeNewsGeneral))
      DemocratRealNews_Count <- c(sum(timeline_dict_df$DemocratRealNews))
      LeaningDemocratic_Count <- c(sum(timeline_dict_df$LeaningDemocratic)) 
      ProRepublican_Count <- c(sum(timeline_dict_df$ProRepublican))
      LeaningRepublican_Count <- c(sum(timeline_dict_df$LeaningRepublican)) 
      RealNewsGeneral_Count = c(sum(timeline_dict_df$RealNewsGeneral))
      HyperPartisan_Count = c(sum(timeline_dict_df$HyperPartisan))
      sum <- sum(ProDemFakeNews_Count, ProDemRepubFakeNews_Count, FakeNewsGeneral_Count, DemocratRealNews_Count, LeaningDemocratic_Count, ProRepublican_Count, LeaningRepublican_Count, RealNewsGeneral_Count, HyperPartisan_Count)
      handle <- input$username
      Tweetcount <- length(timeline_dict_df$timeline)
      timeline_dict_df$timeline2 <- gsub("\\/.*","",timeline_dict_df$timeline)
      h <- subset(timeline_dict_df, is.na(timeline_dict_df$timeline) == FALSE)
      
      topheadlines <- h %>%
        dplyr::select(timeline2, ProDemFakeNews, ProRepubFakeNews, FakeNewsGeneral, DemocratRealNews, LeaningDemocratic, LeaningRepublican, ProRepublican, RealNewsGeneral, HyperPartisan) %>%
        group_by(timeline2)
      
      topheadlines$Category <- NA
      topheadlines$Category <- ifelse(topheadlines$ProDemFakeNews > 0, "Pro-Democrat Fake News", topheadlines$Category)
      topheadlines$Category <- ifelse(topheadlines$ProRepubFakeNews > 0, "Pro-Republican Fake News", topheadlines$Category)
      topheadlines$Category <- ifelse(topheadlines$FakeNewsGeneral > 0, "Fake News (General)", topheadlines$Category)
      topheadlines$Category <- ifelse(topheadlines$DemocratRealNews > 0, "Democrat Real News", topheadlines$Category)
      topheadlines$Category <- ifelse(topheadlines$LeaningDemocratic > 0, "Leaning Democrat Real News", topheadlines$Category)
      topheadlines$Category <- ifelse(topheadlines$LeaningRepublican > 0, "Leaning Republican Real News", topheadlines$Category)
      topheadlines$Category <- ifelse(topheadlines$ProRepublican > 0, "Pro-Republican Real News", topheadlines$Category)
      topheadlines$Category <- ifelse(topheadlines$RealNewsGeneral > 0, "Real News (General)", topheadlines$Category)
      topheadlines$Category <- ifelse(topheadlines$HyperPartisan > 0, "Hyperpartisan", topheadlines$Category)
      topheadlines <- data.frame(topheadlines)
      topheadlinesclassified <- subset(topheadlines, is.na(topheadlines$Category) == FALSE)
      
      sort <- topheadlinesclassified %>%
        group_by(timeline2, Category) %>%
        dplyr::summarize(n = n()) %>%
        arrange(desc(n))
      sort <- as.data.frame(sort)
      Category <- c("Pro-Democrat Fake News", "Pro-Republican Fake News", "Fake News (General)", "Hyperpartisan", "Leaning Democrat Real News", "Democrat Real News", "Real News (General)", "Leaning Republican Real News", "Republican Real News") 
      Counts <- c(ProDemFakeNews_Count, ProDemRepubFakeNews_Count, FakeNewsGeneral_Count, HyperPartisan_Count, DemocratRealNews_Count, LeaningDemocratic_Count, RealNewsGeneral_Count, LeaningRepublican_Count, ProRepublican_Count)
      dataframe <- data.frame(Category, Counts)
      dataframe$Percent <- ifelse(dataframe$Counts > 0, paste((round((dataframe$Counts / sum)*100, digits = 2)), "%", sep = ""), 0)
      if (condition == 1) {
      dataframe$TwitterAverage <- c("0.30%", "0.36%", "0.05%", "12.72%", "13.15%", "41.81%", "14.17%", "2.94%", "14.51%")
      }
      dataframe <- dataframe %>%
        group_by(Counts) %>%
        arrange(desc(Counts))
      
      if (input$myTwitter != 2) {
       #mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"Tweetcount": ', counts[9],'}}', sep=""), upsert = TRUE)
       mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"ProDemFake": ', ProDemFakeNews_Count,'}}', sep=""), upsert = TRUE)
       mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"ProRepubFake": ', ProDemRepubFakeNews_Count,'}}', sep=""), upsert = TRUE)
       mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"GeneralFake": ', FakeNewsGeneral_Count,'}}', sep=""), upsert = TRUE)
       mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"ProDemReal": ', DemocratRealNews_Count,'}}', sep=""), upsert = TRUE)
       mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"LeanDemReal": ', LeaningDemocratic_Count,'}}', sep=""), upsert = TRUE)
       mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"ProRepubReal": ', ProRepublican_Count,'}}', sep=""), upsert = TRUE)
       mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"LeanRepubReal": ', LeaningRepublican_Count,'}}', sep=""), upsert = TRUE)
       mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"RealGeneral": ', RealNewsGeneral_Count,'}}', sep=""), upsert = TRUE)
       mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"handle": "', as.character(input$username),'"}}', sep=""), upsert = TRUE)
       mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"userid": "', userid,'"}}', sep=""), upsert = TRUE)
      } else {
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"handleNOTUSER": "', as.character(input$username),'"}}', sep=""), upsert = TRUE)
      }
      
      return(dataframe)
    })
    
    create_table <- reactive({
      timeline_dict_df <- req(tweet_df())
      timeline_dict_df$timeline2 <- gsub("\\/.*","",timeline_dict_df$timeline)
      h <- subset(timeline_dict_df, is.na(timeline_dict_df$timeline) == FALSE)
      
      topheadlines <- h %>%
        dplyr::select(timeline2, ProDemFakeNews, ProRepubFakeNews, FakeNewsGeneral, DemocratRealNews, LeaningDemocratic, LeaningRepublican, ProRepublican, RealNewsGeneral, HyperPartisan) %>%
        group_by(timeline2)
      
      topheadlines$Category <- NA
      topheadlines$Category <- ifelse(topheadlines$DemocratRealNews > 0, "Democrat Real News", topheadlines$Category)
      topheadlines$Category <- ifelse(topheadlines$LeaningDemocratic > 0, "Leaning Democrat Real News", topheadlines$Category)
      topheadlines$Category <- ifelse(topheadlines$LeaningRepublican > 0, "Leaning Republican Real News", topheadlines$Category)
      topheadlines$Category <- ifelse(topheadlines$ProRepublican > 0, "Pro Republican Real News", topheadlines$Category)
      topheadlines$Category <- ifelse(topheadlines$RealNewsGeneral > 0, "Real News (General)", topheadlines$Category)
      topheadlines$Category <- ifelse(topheadlines$HyperPartisan > 0, "Hyperpartisan", topheadlines$Category)
      topheadlines$Category <- ifelse(topheadlines$ProDemFakeNews > 0, "Pro-Democrat Fake News", topheadlines$Category)
      topheadlines$Category <- ifelse(topheadlines$ProRepubFakeNews > 0, "Pro-Republican Fake News", topheadlines$Category)
      topheadlines$Category <- ifelse(topheadlines$FakeNewsGeneral > 0, "Fake News (General)", topheadlines$Category)
      topheadlines <- data.frame(topheadlines)
      topheadlinesclassified <- subset(topheadlines, is.na(topheadlines$Category) == FALSE)
      
      sort <- topheadlinesclassified %>%
        group_by(timeline2, Category) %>%
        dplyr::summarize(n = n()) %>%
        arrange(desc(n))
      sort <- as.data.frame(sort)
      return(sort)
    })
    
    tweet_df2 <- reactive({
      timeline <- get_timeline(input$username2, n = 3200, token = token)
      validate( 
        need(try(length(timeline) != 0), "That Twitter handle doesn't exist or has no public tweets.")
      )
      followers <- timeline$followers_count[1]
      following <- timeline$friends_count[1]
      length <- nrow(timeline)
      mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"numTweetsEMOTION": ', length,'}}', sep=""), upsert = TRUE)
      mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"followersEMOTION": ', followers,'}}', sep=""), upsert = TRUE)
      mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"followingEMOTION": ', following,'}}', sep=""), upsert = TRUE)
      retweets <- timeline$retweet_count
      timeline <- timeline$text
      timelinecorpus = corpus(timeline)
      timeline_dict = dfm(timelinecorpus, dictionary = emotiondictionary)
      timeline_dict_df <- quanteda::convert(timeline_dict, to='data.frame')
      timeline_dict_df <- cbind(timeline, retweets, timeline_dict_df)
      return(timeline_dict_df)
    })
    
    # get_word_scores <- reactive({
    #   timeline_dict_df <- req(tweet_df2())
    #   Tweetcount <- length(timeline_dict_df$timeline)
    #   MoralEmotional_Count <- sum(timeline_dict_df$MoralEmotional)
    #   PositiveEmotion_Count <- sum(timeline_dict_df$PositiveAffect)
    #   NegativeEmotion_Count <- sum(timeline_dict_df$NegativeAffect)
    #   Polarizing_Count <- sum(timeline_dict_df$Polarization)
    #   Democrat_Count <- sum (timeline_dict_df$Democrat)
    #   Republican_Count <- sum(timeline_dict_df$Republican)
    #   MoralPerTweet <- round((MoralEmotional_Count / Tweetcount), digits = 2)
    #   PositivePerTweet <- round((PositiveEmotion_Count / Tweetcount), digits = 2)
    #   NegativePerTweet <- round((NegativeEmotion_Count / Tweetcount), digits = 2)
    #   PolarizingPerTweet <- round((Polarizing_Count / Tweetcount), digits = 2)
    #   DemocratPerTweet <- round((Democrat_Count / Tweetcount), digits = 2)
    #   RepublicanPerTweet <- round((Republican_Count / Tweetcount), digits = 2)
    #   print(DemocratPerTweet)
    #   print(RepublicanPerTweet)
    #   return(c(MoralEmotional_Count, PositiveEmotion_Count, NegativeEmotion_Count, Tweetcount, MoralPerTweet, PositivePerTweet, NegativePerTweet, PolarizingPerTweet))
    # })
    
    get_word_scores_2 <- reactive({
      timeline_dict_df <- req(tweet_df2())
      Tweetcount <- length(timeline_dict_df$timeline)
      MoralEmotional_Count <- sum(timeline_dict_df$MoralEmotional)
      PositiveEmotion_Count <- sum(timeline_dict_df$PositiveAffect)
      NegativeEmotion_Count <- sum(timeline_dict_df$NegativeAffect)
      Polarizing_Count <- sum(timeline_dict_df$Polarization)
      Democrat_Count <- sum(timeline_dict_df$Democrat)
      Republican_Count <- sum(timeline_dict_df$Republican)
      MoralPerTweet <- round((MoralEmotional_Count / Tweetcount), digits = 2)
      PositivePerTweet <- round((PositiveEmotion_Count / Tweetcount), digits = 2)
      NegativePerTweet <- round((NegativeEmotion_Count / Tweetcount), digits = 2)
      PolarizingPerTweet <- round((Polarizing_Count / Tweetcount), digits = 2)
      DemocratPerTweet <- round((Democrat_Count / Tweetcount), digits = 2)
      RepublicanPerTweet <- round((Republican_Count / Tweetcount), digits = 2)

      Category <- c("Moral-Emotional", "Positive Emotion", "Negative Emotion", "Polarizing")
      Counts <- c(MoralPerTweet, PositivePerTweet, NegativePerTweet, PolarizingPerTweet)
      Norm <- c("0.26", "0.92", "0.67", "0.27")
      
      if (input$myTwitter2 != 2) {
        #mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"Tweetcount": ', counts[9],'}}', sep=""), upsert = TRUE)
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"MoralEmotional_Count": ', MoralEmotional_Count,'}}', sep=""), upsert = TRUE)
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"PositiveEmotion_Count": ', PositiveEmotion_Count,'}}', sep=""), upsert = TRUE)
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"NegativeEmotion_Count": ', NegativeEmotion_Count,'}}', sep=""), upsert = TRUE)
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"Polarizing_Count": ', Polarizing_Count,'}}', sep=""), upsert = TRUE)
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"handleEMOTION": "', as.character(input$username2),'"}}', sep=""), upsert = TRUE)
      } else {
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"handleNOTUSEREMOTION": "', as.character(input$username),'"}}', sep=""), upsert = TRUE)
      }

      dataframe <- data.frame(Category, Counts) %>%
        arrange(desc(Counts))
      
      if (condition == 1) { 
        dataframe$Norm <- c("0.26", "0.92", "0.67", "0.27")
      }
      
      #dataframe$Percent <- ifelse(dataframe$Counts > 0, paste((round((dataframe$Counts / sum)*100, digits = 2)), "%", sep = ""), 0)
      return(dataframe)
    })
    
    create_table2 <- reactive({
      timeline_dict_df <- req(tweet_df2())
      
      topwords <- timeline_dict_df %>%
        dplyr::select(timeline, retweets) %>%
        group_by(timeline)
      head(topwords)
      
      ngrams  <- topwords %>%
        unnest_tokens(word, timeline, token = "ngrams", n = 1) 
      
      ngramssort <- ngrams %>%
        group_by(word) %>%
        dplyr::summarize(n = n(),
                         avg_shares = mean(retweets)) %>%
        arrange(desc(n))
      
      wordcorpus = corpus(ngramssort, 
                          text_field = "word")
      word_dict = dfm(wordcorpus, dictionary = emotiondictionary)
      word_dict_df <- quanteda::convert(word_dict, to='data.frame')
      word_dict_df <- cbind(ngramssort, word_dict_df)
      
      word_dict_df$Category <- NA
      word_dict_df$Category <- ifelse(word_dict_df$PositiveAffect > 0, "Positive Affect", word_dict_df$Category)
      word_dict_df$Category <- ifelse(word_dict_df$NegativeAffect > 0, "Negative Affect", word_dict_df$Category)
      word_dict_df$Category <- ifelse(word_dict_df$Polarization > 0, "Polarizing", word_dict_df$Category)
      word_dict_df$Category <- ifelse(word_dict_df$MoralEmotional > 0, "Moral-Emotional", word_dict_df$Category)
      #word_dict_df$Category <- ifelse(word_dict_df$Democrat > 0, "Democrat", word_dict_df$Category)
      #word_dict_df$Category <- ifelse(word_dict_df$Republican > 0, "Republican", word_dict_df$Category)
      topwordsclassified <- subset(word_dict_df, is.na(word_dict_df$Category) == FALSE)
      
      sort <- topwordsclassified %>%
        dplyr::select(word, Category, n) 
      sort <- as.data.frame(sort)
      return(sort)
    })
    
    showModal(modalDialog(
      title = "Take Part in Our Study",
      p(
      "This app was created as part of an academic research study. 
      Do you consent to participating in this study and sharing anonymized information from your use of this app?"
      ), 
      p(
      "All data will be kept completely anonymous. You must be 18 years or older to participate. 
      This project has been reviewed by the Cambridge Psychology Research Ethics Committee.",
      ),
      p(
      "If you consent to participating in this study, click 'Yes, I consent.' 
      If you do not want to participate in the study or share your data, click 'No, I do not consent.' 
      If you click 'No, I do not consent,' you can still use the app without sharing your data."
      ), 
      h4("Privacy Policy"), 
      p(
        "To safeguard your privacy, we will only collect data on information you choose to share with us if you 
        consent to participating in the study. This includes your Twitter handle, public information on your Twitter timeline retrieved though the Twitter API,
        and any questions you voluntarily choose to answer. Please not that the only information we can access from your Twitter profile is information that you make
        publically available on the internet. This app will not work if you have set your Twitter account to private. Aggregate data will be used for research purposes to understand
        people's social media behavior. Limited, de-identified raw data may also be shared (with strict privacy protections to ensure no personal data is identifiable) to conform 
        with open science pratices by academic journals. If you wish to delete your data, we generate a unique, anonymous, randomly generated ID, and you can use it to request deletion of your data. 
        Alternatively, if you lose this ID, you can tell us the Twitter handle that you entered and we can delete the data.
        Please direct all inquiries about this consent form or privacy policy to Steve Rathje (sjr210@cam.ac.uk)."
      ),
      p("To view the full privacy policy, please click ", a("here.", href="HaveISharedFakeNewsPrivacy.html")),
      p(
        "This privacy policy was updated on Jan 12, 2020."
      ),
      p(""),
      p(strong("Please note: if app performance is slow, it is likely because many people are using it at once. Please come back at another time if the app is experiencing delays.")),
      easyClose = FALSE,
      footer = tagList(
        actionButton("consent", "Yes, I consent"),
        modalButton("No, I do not consent")
      )
    ))
    
    observeEvent(input$search, {
      if (input$myTwitter != 2) {
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"handleUserInput": "', as.character(input$username),'"}}', sep=""), upsert = TRUE)
      } else {
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"handleUserInputOTHER": "', as.character(input$username),'"}}', sep=""), upsert = TRUE)
      }
      if (is.null(input$submitModal2)) { #Check if they've already filled out these answers
      showModal(
        (modalDialog(
          p("Your results are currently loading. This should take only 15 seconds. While your results load, will you answer some quick questions?"),
          sliderInput("democratFavorable", "How favorable do you feel toward Democrats (0 = very unfavorable and 100 = very favorable)?",
                      min = 0, max = 100, 
                      value = 50,
                      width = '75%'),
          sliderInput("republicanFavorable", "How favorable do you feel toward Republicans (0 = very unfavorable and 100 = very favorable)?",
                      min = 0, max = 100, 
                      value = 50,
                      width = '75%'),
          sliderInput("lifeSatisfaction", label = ("In general, how satisfied are you with your life (0 = very unsatisfied and 100 = very satisfied)?"),
                      min = 0, max = 100, 
                      value = 50,
                      width = '75%'),
          sliderInput("mentalHealth", label = ("In general, how would you rate your mental health? (0 = poor and 100 = very good)?"),
                       min = 0, max = 100, 
                       value = 50,
                       width = '75%'),
          sliderInput("vaccineLikely", label = ("How likely are you to get vaccinated for COVID-19 when it becomes available? (0 = very unlikely and 100 = very likely)? If you have already received the vaccine, you may select 100."),
                      min = 0, max = 100, 
                      value = 50,
                      width = '75%'),
          footer = tagList(
            actionButton("submitModal2", "Submit"),
          )
        )
        ))
      }
      progress <- shiny::Progress$new()
      progress$set(message = "Calculating scores.", value = 0.75)
        output$Counts <- renderText({ 
         length <- nrow(tweet_df())
          paste("We scrapped ", length, " total tweets from this profile.")
        })
        output$Title <- renderText({
          paste("Fake News and Bias Scores")
        })
        output$Explanation1 <- renderText({
          paste("Below is the the number of times this account shared news from each category.
                We also show you the percentage of this account's news diet that is shared from each category.",
                if (condition == 1) {
                  "Additionally, we show how this account's news diet compares to all news links shared in a random sample of 300,000 tweets from the United States."
                }
                )
        })
        output$ScoresTable <- renderReactable({ 
          reactable(data <- get_scores2(),
                    if (condition == 1) {
                    columns = list(
                      Category = colDef(
                        name = "Category"
                        # style = function(value) {
                        #   if (value == "Leaning Democrat Real News" | value == "Democrat Real News") {
                        #     list(color = "#599ad3")
                        #   } else if (value == "Leaning Republican Real News" | value == "Republican Real News") {
                        #     list(color = "#F8766D")
                        #   #} else if (value == "HyperPartisan") {
                        #   #  list(color = "black")
                        #   } else if (value == "Real News (General)") {
                        #     list(color = "#9370DB	")
                        #   }
                        # }
                      ),
                      Counts = colDef(
                        name = "Times Shared",
                        cell = function(value) {
                          width <- paste0(value * 100 / max(data$Counts), "%")
                          # Add thousands separators
                          value <- format(value, big.mark = ",")
                          value <- format(value, width = 9, justify = "right")
                          bar_chart(value, width = width, fill = "#3fc1c9")
                        },
                        # And left-align the columns
                        align = "left",
                        style = list(fontFamily = "monospace", whiteSpace = "pre")
                      ),
                      Percent = colDef(
                        name = "%",
                        align = "left",
                        style = list(fontFamily = "monospace", whiteSpace = "pre")
                      ),
                      TwitterAverage = colDef(
                        name = "Twitter average (%)",
                        align = "left", 
                        style = list(fontFamily = "monospace", whiteSpace = "pre")
                      )
                    )
                    } else {
                      columns = list(
                        Category = colDef(
                          name = "Category"
                          # style = function(value) {
                          #   if (value == "Leaning Democrat Real News" | value == "Democrat Real News") {
                          #     list(color = "#599ad3")
                          #   } else if (value == "Leaning Republican Real News" | value == "Republican Real News") {
                          #     list(color = "#F8766D")
                          #     #} else if (value == "HyperPartisan") {
                          #     #  list(color = "black")
                          #   } else if (value == "Real News (General)") {
                          #     list(color = "#9370DB	")
                          #   }
                          # }
                        ),
                        Counts = colDef(
                          name = "Times Shared",
                          cell = function(value) {
                            width <- paste0(value * 100 / max(data$Counts), "%")
                            # Add thousands separators
                            value <- format(value, big.mark = ",")
                            value <- format(value, width = 9, justify = "right")
                            bar_chart(value, width = width, fill = "#3fc1c9")
                          },
                          # And left-align the columns
                          align = "left",
                          style = list(fontFamily = "monospace", whiteSpace = "pre")
                        ),
                        Percent = colDef(
                          name = "%",
                          align = "left",
                          style = list(fontFamily = "monospace", whiteSpace = "pre")
                        )
                      )
                    }
                    )
        })
        output$Title2 <- renderText({
          paste("Most Shared News Sites")
        })
        output$Explanation2 <- renderText({
          paste("Below are more details about the specific sources this account shared.")
        })
        output$Table <- renderReactable({ 
          reactable(data <- create_table(),
                    columns = list(
                      timeline2 = colDef(name = "Website"),
                      Category = colDef(
                        name = "Category",
                        style = function(value) {
                          if (value == "Leaning Democrat Real News" | value == "Democrat Real News") {
                            list(color = "#599ad3")
                          } else if (value == "Leaning Republican Real News" | value == "Pro Republican Real News") {
                            list(color = "#F8766D")
                          } else if (value == "Hyperpartisan" | value == "Pro-Democrat Fake News" | value == "Pro-Republican Fake News") {
                            list(color = "#FFC0CB")
                          } else if (value == "Real News (General)") {
                            list(color = "#9370DB")
                          }
                        }
                      ),
                      n = colDef(
                        name = "Times Shared",
                        cell = function(value) {
                          width <- paste0(value * 100 / max(data$n), "%")
                          # Add thousands separators
                          value <- format(value, big.mark = ",")
                          value <- format(value, width = 9, justify = "right")
                          bar_chart(value, width = width, fill = "#3fc1c9")
                        },
                        # And left-align the columns
                        align = "left",
                        style = list(fontFamily = "monospace", whiteSpace = "pre")
                      )
                    )
                    )
        })
        output$Title3 <- renderText({
          if (input$myTwitter == 1) {
            paste("Will you answer some follow-up questions?")
          }
        })
        output$Explanation3 <- renderText({
          if (input$myTwitter == 1) {
            paste("Please answer the following questions and help contribute to scientific research.")
          }
        })
        
        # output$Question <- renderUI({ 
        #   if (input$myTwitter == 1) {
        #   checkboxGroupInput("goals", "Over the next month, what kind of news do you want to share? Check all that apply:",
        #                      c("I want to share more accurate news" = "accurate",
        #                        "I want to share more politically balanced news" = "balance",
        #                        "I do not want to change how I share news" = "nochange"),
        #                      width = 1000, 
        #                      )
        #   }
        # })
        # 
        output$Question <- renderUI({ 
          if (input$myTwitter == 1) {
            checkboxGroupInput("goals", "Over the next month, what kind of news do you want to share? Check all that apply:",
                               c("I want to share more accurate news" = 1,
                                 "I want to share more politically balanced news" = 2,
                                 "I do not want to change how I share news" = 3),
                               width = 1000, 
            )
          }
          
        })
        
        output$SharingSubmit <- renderUI({ 
          actionButton("sharingSubmit", "Submit Answers")
        })
        
        output$DemocratFavorable<- renderUI({
          if (input$myTwitter == 1) {
          sliderInput("democratFavorable", "How favorable do you feel toward Democrats (0 = very unfavorable and 100 = very favorable)?",
                      min = 0, max = 100, 
                      value = 50,
                      width = '75%')
          }
        })
        
        output$RepublicanFavorable<- renderUI({
          if (input$myTwitter == 1) {
          sliderInput("republicanFavorable", "How favorable do you feel toward Republicans (0 = very unfavorable and 100 = very favorable)?",
                      min = 0, max = 100, 
                      value = 50,
                      width = '75%')
          }
        })

         output$lifeSatisfaction<- renderUI({
          if (input$myTwitter == 1) {
             radioButtons("lifeSatisfaction", label = ("In general, how satisfied are you with your life?"),
                          choices = list("Very dissatisfied" = 1, " " = 2, " " = 3, "Very satisfied" = 4), selected = character(0), inline = TRUE,  width = '100%')
           }
         })
        
         output$mentalHealth <- renderUI({
           if (input$myTwitter == 1) {
             radioButtons("mentalHealth", label = ("In general, how would you rate your mental health?"),
                          choices = list("Poor" = 1, "Fair" = 2, "Good" = 3, "Very Good" = 4), selected = character(0), inline = TRUE, width = '100%')
           }
         })

        output$ProgressBarComplete <- renderText({
          on.exit(progress$close())
        })
        
        #Save data 
        # if (input$myTwitter == 1) {
        # mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"Tweetcount": ', counts[9],'}}', sep=""), upsert = TRUE)
        # mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"ProDemFake": ', counts[1],'}}', sep=""), upsert = TRUE)
        # mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"ProRepubFake": ', counts[2],'}}', sep=""), upsert = TRUE)
        # mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"GeneralFake": ', counts[3],'}}', sep=""), upsert = TRUE)
        # mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"ProDemReal": ', counts[4],'}}', sep=""), upsert = TRUE)
        # mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"LeanDemReal": ', counts[5],'}}', sep=""), upsert = TRUE)
        # mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"ProRepubReal": ', counts[6],'}}', sep=""), upsert = TRUE)
        # mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"LeanRepubReal": ', counts[7],'}}', sep=""), upsert = TRUE)
        # mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"RealGeneral": ', counts[8],'}}', sep=""), upsert = TRUE)
        # mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"handle": "', as.character(counts[10]),'"}}', sep=""), upsert = TRUE)
        # mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"userid": "', userid,'"}}', sep=""), upsert = TRUE)
        # }
    })
    
    observeEvent(input$search2, {
      if (input$myTwitter != 2) {
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"handleUserInputEMOTION": "', as.character(input$username2),'"}}', sep=""), upsert = TRUE)
      } else {
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"handleUserInputEMOTIONOTHER": ', as.character(input$username2),'"}}', sep=""), upsert = TRUE)
      }
      if (is.null(input$submitModal3)) { #Check if they've already filled out these answers
        showModal(
          (modalDialog(
            p("Your results are currently loading. This should take only 15 seconds. While your results load, will you answer some quick questions?"),
            sliderInput("Open1", "I think that paying attention to people who disagree with me is a waste of time (0 = strongly disagree, 10 = strongly agree).",
                        min = 0, max = 10, 
                        value = 5,
                        width = '75%'),
            sliderInput("Open2", "I feel no shame learning from someone who knows more than me (0 = strongly disagree, 10 = strongly agree).",
                        min = 0, max = 10, 
                        value = 5,
                        width = '75%'),
            sliderInput("Open2", "If I do not know much about some topic, I don’t mind being taught about it, even if I know about other topics. (0 = strongly disagree, 10 = strongly agree).",
                        min = 0, max = 10, 
                        value = 5,
                        width = '75%'),
            sliderInput("Open3", label = ("Even when I have high status, I don’t mind learning from others who have lower status (0 = strongly disagree, 10 = strongly agree)."),
                        min = 0, max = 10, 
                        value = 5,
                        width = '75%'),
            sliderInput("Open4", label = ("Only wimps admit that they’ve made mistakes (0 = strongly disagree, 10 = strongly agree)."),
                        min = 0, max = 10, 
                        value = 5,
                        width = '75%'),
            sliderInput("Open5", label = ("I don’t take people seriously if they’re very different from me. (0 = strongly disagree, 10 = strongly agree)."),
                        min = 0, max = 10, 
                        value = 5,
                        width = '75%'),
            footer = tagList(
              actionButton("submitModal3", "Submit"),
            )
          )
          ))
      }
      progress <- shiny::Progress$new()
      progress$set(message = "Calculating scores.", value = 0.75)
      output$CountsEmotion <- renderText({
        length <- nrow(tweet_df())
        paste("We scrapped ", length, " total tweets from this profile.")
      })
      output$TitleEmotion <- renderText({
        paste("Language Analysis")
      })
      output$ExplanationEmotion <- renderText({
        paste("Below is the the number of times this account shared words from each category.",
              if (condition == 1) {
                "Additionally, we show how this account's language compares to that in a sample of 300,000 tweets from the United States."
              }
        )
      })
        #counts <- get_word_scores()
        # output$Tweetcount2 <- renderText({ 
        #   paste("We scrapped ", (counts[4]), " total tweets from this profile.")
        # })
        # output$Moral <- renderText({ 
        #   paste("Average number of moral-emotional words per tweet: ", (counts[5]))
        # })
        # output$Positive <- renderText({ 
        #   paste("Average number of positive words per tweet: ", (counts[6]))
        # })
        # output$Negative <- renderText({ 
        #   paste("Average number of negative words per tweet: ", (counts[7]))
        # })
        # output$Polarizing <- renderText({ 
        #   paste("Average number of polarizing words per tweet: ", (counts[8]))
        # })
        output$Table2 <- renderReactable({ 
          reactable(data <- create_table2(),
                    columns = list(word = colDef(name = "Word"),
                                   Category = colDef(name = "Category", 
                                                     style = function(value) {
                                                       if (value == "Positive Affect") {
                                                         list(color = "blue")
                                                       } else if (value == "Negative Affect") {
                                                         list(color = "red")
                                                       } else if (value == "Moral-Emotional") {
                                                         list(color = "#9370DB")
                                                       } else if (value == "Polarizing") {
                                                         list(color = "#FFC0CB")
                                                       } 
                                                     }
                                                ),
                                   n = colDef(name = "Times Shared",
                                              cell = function(value) {
                                                width <- paste0(value * 100 / max(data$n), "%")
                                                # Add thousands separators
                                                value <- format(value, big.mark = ",")
                                                value <- format(value, width = 9, justify = "right")
                                                bar_chart(value, width = width, fill = "#3fc1c9")
                                              },
                                              # And left-align the columns
                                              align = "left",
                                              style = list(fontFamily = "monospace", whiteSpace = "pre")))
          )
        })
        output$TitleEmotion2 <- renderText({
          paste("Most Used Words")
        })
        output$ExplanationEmotion2 <- renderText({
          paste("Below are more details about the most used words from each category.")
        })
        output$ScoresTable2 <- renderReactable({
          reactable(data <- get_word_scores_2(),
                    if (condition == 1) {
                    columns = list(Category = colDef(name = "Category"),
                                   Counts = colDef(name = "Average per tweet",
                                                   cell = function(value) {
                                                     width <- paste0(value * 100 / max(data$Counts), "%")
                                                     # Add thousands separators
                                                     value <- format(value, big.mark = ",")
                                                     value <- format(value, width = 9, justify = "right")
                                                     bar_chart(value, width = width, fill = "#3fc1c9")
                                                   },
                                                   align = "left",
                                                   style = list(fontFamily = "monospace", whiteSpace = "pre")),
                                   TwitterAverage = colDef(name = "Average across Twitter",
                                                 align = "left",
                                                 style = list(fontFamily = "monospace", whiteSpace = "pre")))
                    } else {
                      columns = list(Category = colDef(name = "Category"),
                                     Counts = colDef(name = "Average per tweet",
                                                     cell = function(value) {
                                                       width <- paste0(value * 100 / max(data$Counts), "%")
                                                       # Add thousands separators
                                                       value <- format(value, big.mark = ",")
                                                       value <- format(value, width = 9, justify = "right")
                                                       bar_chart(value, width = width, fill = "#3fc1c9")
                                                     },
                                                     align = "left",
                                                     style = list(fontFamily = "monospace", whiteSpace = "pre")))
                    }
                    )
        })
        output$Question2 <- renderUI({ 
        if (input$myTwitter2 == 1) {
            checkboxGroupInput("goals2", "Over the next month, what kind of news do you want to share? Check all that apply:",
                               c("I want to share more positive language" = 1,
                                 "I want to share less polarizing language" = 2,
                                 "I do not want to change the language I share" = 3),
                               width = 1000,
            )
        }
      })
      output$ProgressBarComplete2 <- renderText({
          on.exit(progress$close())
      })
    })

    observeEvent(input$consent, {
      
      showModal(modalDialog(
        title = "Please answer a few questions", 
        radioButtons("gender", label = ("What is your gender?"),
                     choices = list("Male" = 1, "Female" = 2, "Transgender Male" = 3, "Transgender Female" = 4, "Non-Binary/Other" = 5), selected = character(0)),
        numericInput("age", label = ("How old are you?"), value = NULL), 
        radioButtons("education", label = ("What is the highest level of education you've completed?"),
                     choices = list("High School or Less" = 1, "Some College" = 2, "Bachelor's Degree" = 3, "Higher Degree" = 4), selected = character(0)),
        radioButtons("politics", label = ("What is your political orientation?"),
                     choices = list("Extremely liberal" = 1, "Liberal" = 2, "Slightly Liberal" = 3, "Moderate" = 4, "Slightly Conservative" = 5, "Conservative" = 6, "Extremely Conservative" = 7), selected = character(0)),
        checkboxGroupInput("ethnicity", "Please choose whichever ethnicity that you identify with (you may choose more than one option):",
                              c("White/Caucasian" = 1,
                                "Black or African American" = 2,
                                "American Indian or Alaska Native" = 3,
                                "Asian" = 4, 
                                "Native Hawaiian or Pacific Islander" = 5,
                                "Other" = 6),
                              width = 1000,
                              ),
        footer = tagList(
          actionButton("submit", "Submit answers"),
        )
      ))
    })
    
    observeEvent(input$submit, {
      gender <- input$gender
      age <- input$age
      education <- input$education
      politics <- input$politics
      ethnicity <- input$ethnicity
      if (!is.null(gender)) {
      mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"Gender": ', input$gender,'}}', sep=""), upsert = TRUE)
      }
      if (!is.na(age)) {
      mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"Age": ', input$age,'}}', sep=""), upsert = TRUE)
      }
      if (!is.null(education)) {
      mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"Education": ', input$education,'}}', sep=""), upsert = TRUE)
      }
      if (!is.null(politics)) {
      mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"Politics": ', input$politics,'}}', sep=""), upsert = TRUE)
      }
      if (!is.null(ethnicity)) {
        ethnicity <- toString(ethnicity)
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"Ethnicity": "', ethnicity,'"}}', sep=""), upsert = TRUE)
      }
    })
    
    observeEvent(input$consent, {
      output$userid <- renderText({
        paste(userid)
      })
      mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"Consent": ', input$consent,'}}', sep=""), upsert = TRUE)
    })
    
    observeEvent(input$myTwitter, {
      mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"myTwitter": ', 1,'}}', sep=""), upsert = TRUE)
    })
    
    observeEvent(input$submitModal2, {
      removeModal()
      democratFavorable <- input$democratFavorable
      republicanFavorable <- input$republicanFavorable
      lifeSatisfaction <- input$lifeSatisfaction
      mentalHealth <- input$mentalHealth
      vaccineLikely <- input$vaccineLikely
      if (!is.null(democratFavorable)) {
      mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"democratFavorable": ', input$democratFavorable,'}}', sep=""), upsert = TRUE)
      }
      if (!is.null(republicanFavorable)) {
      mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"republicanFavorable": ', input$republicanFavorable,'}}', sep=""), upsert = TRUE)
      }
      if (!is.null(lifeSatisfaction)) {
      mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"lifeSatisfaction": ', input$lifeSatisfaction,'}}', sep=""), upsert = TRUE)
      }
      if (!is.null(mentalHealth)) {
      mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"mentalHealth": ', input$mentalHealth,'}}', sep=""), upsert = TRUE)
      }
      if (!is.null(vaccineLikely)) {
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"vaccineLikely": ', input$vaccineLikely,'}}', sep=""), upsert = TRUE)
      }
    })
    
    observeEvent(input$submitModal3, {
      removeModal()
      Open1 <- input$Open1
      Open2 <- input$Open2
      Open3 <- input$Open3
      Open4 <- input$Open4
      Open5 <- input$Open5
      if (!is.null(Open1)) {
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"Open1": ', input$Open1,'}}', sep=""), upsert = TRUE)
      }
      if (!is.null(Open2)) {
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"Open2": ', input$Open2,'}}', sep=""), upsert = TRUE)
      }
      if (!is.null(Open3)) {
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"Open3": ', input$Open3,'}}', sep=""), upsert = TRUE)
      }
      if (!is.null(Open4)) {
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"Open4": ', input$Open4,'}}', sep=""), upsert = TRUE)
      }
      if (!is.null(Open5)) {
        mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"Open5": ', input$Open5,'}}', sep=""), upsert = TRUE)
      }
    })
    
    observeEvent(input$sharingGoals, {
      input <- toString(input$goals)
      mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"sharingGoals": "', input,'"}}', sep=""), upsert = TRUE)
    })
    
    # observeEvent(input$sharingSubmit, {
    #   input <- toString(input$goals)
    #   mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"sharingGoals": "', input,'"}}', sep=""), upsert = TRUE)
    # })
    # 
    
    observeEvent(input$submit, {
    showModal(
      (modalDialog(
        p("Your unique user id is: ", userid),
        p("We have assigned you a user id so that we can anonymize your data.
          If you want to have us delete your data, please contact sjr@cam.ac.uk with this user id.
          This id will be available under the 'about' page if you need to access it again.")
      )
      ))
    })
    
    observeEvent(input$screenshot, {
      screenshot()
      mongo$update(paste('{"userid": "', userid,'"}', sep=""), paste('{"$set": {"screenshort": ', 1,'}}', sep=""), upsert = TRUE)
    })
}

# Run the application #
shinyApp(ui = ui, server = server)
