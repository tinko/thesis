### THESIS PROJECT SCRIPTS
###
### topic           The effects of Direct-to-Fan communication
###                 via Social Media Networks
###                 on Artist’s Authenticity
###
### student         Tinko Georgiev
###
### student ID      11020037
###
### language        R
### version.string  R version 3.0.2 (2013-09-25)
### nickname        Frisbee Sailing 
### 
### 

### Load required libraries
library(ggplot2)      # Provides functions that let us plot the graphs
library(psych)        # Provides functions for descriptive statistics
library(scales)       # Provides functions for formatting scales

### Data Import
setwd("C:/Users/Tinko Georgiev/SkyDrive/Thesis")
Responses.HeikeLanghans.Coded = 
  read.csv("Artist Authenticity Inventory - Heike Langhans.csv", na.strings=c("999", "NA", " ", ""))
Responses.HeikeLanghans.Raw = 
  read.csv("Artist Authenticity Inventory - Heike Langhans - RAW-DATA.csv", na.strings=c("999", "NA", " ", ""))

### Rename some columns for easier access
names(Responses.HeikeLanghans.Coded) = 
  sub("^In.the.past.month..how.many.times.have.you.interracted.with.posts.by.Heike...Read.posts.$", "ReadPosts", 
      names(Responses.HeikeLanghans.Coded))
names(Responses.HeikeLanghans.Coded) = 
  sub("^In.the.past.month..how.many.times.have.you.interracted.with.posts.by.Heike...Replied.to.posts.$", "RepliedToPosts", 
      names(Responses.HeikeLanghans.Coded))
names(Responses.HeikeLanghans.Coded) = 
  sub("^In.the.past.month..how.many.times.have.you.interracted.with.posts.by.Heike...Without.replying..liked..re.posted..etc..$", 
      "WithoutReplying", names(Responses.HeikeLanghans.Coded))
names(Responses.HeikeLanghans.Coded) = 
  sub("^In.which.of.these.languages.are.you.comfortable.replying.to.posts.by.Heike.$", "Language", 
      names(Responses.HeikeLanghans.Coded))
names(Responses.HeikeLanghans.Coded) = 
  sub("^Have.you.ever.commented.replied.on.a.post.by.Heike.Langhans.on.social.media.networks.$", "Commented", 
      names(Responses.HeikeLanghans.Coded))

### Calculate Perceived Authenticity
### mean(
###      Do you believe that Heike Langhans is an authentic artist?,
###      Do you believe that Heike expresses her true self on social media networks?,
###      ) * How significant is Heike's authenticity to you?
Responses.HeikeLanghans.Coded$Authenticity.Perceived = 
  rowMeans(Responses.HeikeLanghans.Coded[6:7]) * 
  Responses.HeikeLanghans.Coded$How.significant.is.Heike.s.authenticity.to.you....

### Calculate AAI Authenticity
### mean(
###      Heike represents the genre she works in (music, lyrics, aesthetics),
###      Heike shows consistency between her beliefs and actions,
###      Heike avoids trading off the essence of the genre strictly for profit,
###      Heike admits mistakes when they occur (personal or professional),
###      Heike makes her fans feel they belong to the same sub-culture,
###      Heike resists pressures on her to do things contrary to her beliefs,
###      Heike maintains the standards and style of the genre she works in (music, lyrics, aesthetics),
###      Heike acknowledges how others see her skills and abilities,
###      ) * How significant is Heike's authenticity to you?
Responses.HeikeLanghans.Coded$Authenticity.AAI = 
  rowMeans(Responses.HeikeLanghans.Coded[13:20]) * 
  Responses.HeikeLanghans.Coded$How.significant.is.Heike.s.authenticity.to.you....

### Create new dataframes for easier manupulation
Responses.HeikeLanghans.Coded.Commented = Responses.HeikeLanghans.Coded[Responses.HeikeLanghans.Coded$Commented == 'Yes',]
Responses.HeikeLanghans.Coded.NeverCommented = Responses.HeikeLanghans.Coded[Responses.HeikeLanghans.Coded$Commented == 'No',]

### Create Language dataframe
### Calculated manually the number of each language
Language.Dataframe = data.frame(
  matrix(vector(), 9, 3,
         dimnames=list(c(), c("Language", "Frequency", "Percentage"))),
  stringsAsFactors=F)
Language.Dataframe$Language = c("English", "Spanish", "German", "Other", "French", "Italian", "Dutch", "Swedish", "Norwegian")
Language.Dataframe$Frequency = c(114, 12, 11, 7, 6, 5, 2, 1, 1)
Language.Dataframe$Percentage = c(1.00, .1053, .0965, .0614, .0526, .0439, .0175, .0088, .0088)



### Prepare Authenticity
###
### Prepare Perceived Authenticity - commented
Authenticity.Perceived.Commented = data.frame(
  matrix(vector(), nrow(Responses.HeikeLanghans.Coded.Commented), 9,
         dimnames=list(c(), c("Authenticity", "ReadPosts", "RepliedToPosts", "WithoutReplying", "Type", "Commented",
                              "Age", "Gender", "Country"))),
  stringsAsFactors=F)
Authenticity.Perceived.Commented$Authenticity = Responses.HeikeLanghans.Coded.Commented$Authenticity.Perceived
Authenticity.Perceived.Commented$ReadPosts = Responses.HeikeLanghans.Coded.Commented$ReadPosts
Authenticity.Perceived.Commented$RepliedToPosts = Responses.HeikeLanghans.Coded.Commented$RepliedToPosts
Authenticity.Perceived.Commented$WithoutReplying = Responses.HeikeLanghans.Coded.Commented$WithoutReplying
Authenticity.Perceived.Commented$Age = Responses.HeikeLanghans.Coded.Commented$Age
Authenticity.Perceived.Commented$Gender = Responses.HeikeLanghans.Coded.Commented$Gender
Authenticity.Perceived.Commented$Country = Responses.HeikeLanghans.Coded.Commented$Country
Authenticity.Perceived.Commented$Type = "Perceived"
Authenticity.Perceived.Commented$Commented = "Yes"

### Prepare AAI Calculated Authenticity - commented
Authenticity.AAI.Commented = data.frame(
  matrix(vector(), nrow(Responses.HeikeLanghans.Coded.Commented), 9,
         dimnames=list(c(), c("Authenticity", "ReadPosts", "RepliedToPosts", "WithoutReplying", "Type", "Commented",
                              "Age", "Gender", "Country"))),
  stringsAsFactors=F)
Authenticity.AAI.Commented$Authenticity = Responses.HeikeLanghans.Coded.Commented$Authenticity.AAI
Authenticity.AAI.Commented$ReadPosts = Responses.HeikeLanghans.Coded.Commented$ReadPosts
Authenticity.AAI.Commented$RepliedToPosts = Responses.HeikeLanghans.Coded.Commented$RepliedToPosts
Authenticity.AAI.Commented$WithoutReplying = Responses.HeikeLanghans.Coded.Commented$WithoutReplying
Authenticity.AAI.Commented$Age = Responses.HeikeLanghans.Coded.Commented$Age
Authenticity.AAI.Commented$Gender = Responses.HeikeLanghans.Coded.Commented$Gender
Authenticity.AAI.Commented$Country = Responses.HeikeLanghans.Coded.Commented$Country
Authenticity.AAI.Commented$Type = "Calculated (AAI)"
Authenticity.AAI.Commented$Commented = "Yes"

### Prepare Perceived Authenticity - never commented
Authenticity.Perceived.NeverCommented = data.frame(
  matrix(vector(), nrow(Responses.HeikeLanghans.Coded.NeverCommented), 9,
         dimnames=list(c(), c("Authenticity", "ReadPosts", "RepliedToPosts", "WithoutReplying", "Type", "Commented",
                              "Age", "Gender", "Country"))),
  stringsAsFactors=F)
Authenticity.Perceived.NeverCommented$Authenticity = Responses.HeikeLanghans.Coded.NeverCommented$Authenticity.Perceived
Authenticity.Perceived.NeverCommented$ReadPosts = Responses.HeikeLanghans.Coded.NeverCommented$ReadPosts
Authenticity.Perceived.NeverCommented$RepliedToPosts = Responses.HeikeLanghans.Coded.NeverCommented$RepliedToPosts
Authenticity.Perceived.NeverCommented$WithoutReplying = Responses.HeikeLanghans.Coded.NeverCommented$WithoutReplying
Authenticity.Perceived.NeverCommented$Age = Responses.HeikeLanghans.Coded.NeverCommented$Age
Authenticity.Perceived.NeverCommented$Gender = Responses.HeikeLanghans.Coded.NeverCommented$Gender
Authenticity.Perceived.NeverCommented$Country = Responses.HeikeLanghans.Coded.NeverCommented$Country
Authenticity.Perceived.NeverCommented$Type = "Perceived"
Authenticity.Perceived.NeverCommented$Commented = "No"

### Prepare AAI Calculated Authenticity - never commented
Authenticity.AAI.NeverCommented = data.frame(
  matrix(vector(), nrow(Responses.HeikeLanghans.Coded.NeverCommented), 9,
         dimnames=list(c(), c("Authenticity", "ReadPosts", "RepliedToPosts", "WithoutReplying", "Type", "Commented",
                              "Age", "Gender", "Country"))),
  stringsAsFactors=F)
Authenticity.AAI.NeverCommented$Authenticity = Responses.HeikeLanghans.Coded.NeverCommented$Authenticity.AAI
Authenticity.AAI.NeverCommented$ReadPosts = Responses.HeikeLanghans.Coded.NeverCommented$ReadPosts
Authenticity.AAI.NeverCommented$RepliedToPosts = Responses.HeikeLanghans.Coded.NeverCommented$RepliedToPosts
Authenticity.AAI.NeverCommented$WithoutReplying = Responses.HeikeLanghans.Coded.NeverCommented$WithoutReplying
Authenticity.AAI.NeverCommented$Age = Responses.HeikeLanghans.Coded.NeverCommented$Age
Authenticity.AAI.NeverCommented$Gender = Responses.HeikeLanghans.Coded.NeverCommented$Gender
Authenticity.AAI.NeverCommented$Country = Responses.HeikeLanghans.Coded.NeverCommented$Country
Authenticity.AAI.NeverCommented$Type = "Calculated (AAI)"
Authenticity.AAI.NeverCommented$Commented = "No"

### Merge Authenticity
Authenticity = rbind(
  Authenticity.Perceived.Commented,
  Authenticity.AAI.Commented,
  Authenticity.Perceived.NeverCommented,
  Authenticity.AAI.NeverCommented
)



### Prepare Interaction
###
### Read Posts
tmp.df = data.frame(table(Responses.HeikeLanghans.Raw$
                            In.the.past.month..how.many.times.have.you.interracted.with.posts.by.Heike...Read.posts.))
Interaction.ReadPosts = data.frame(
  matrix(vector(), nrow(tmp.df), 3,
         dimnames=list(c(), c("Type", "Rate", "Frequency"))),
  stringsAsFactors=F)
Interaction.ReadPosts$Type = "Read posts"
Interaction.ReadPosts$Rate = tmp.df$Var1
Interaction.ReadPosts$Frequency = tmp.df$Freq
### Replied to Posts
tmp.df = data.frame(table(Responses.HeikeLanghans.Raw$
                            In.the.past.month..how.many.times.have.you.interracted.with.posts.by.Heike...Replied.to.posts.))
Interaction.RepliedToPosts.Dataframe = data.frame(
  matrix(vector(), nrow(tmp.df), 3,
         dimnames=list(c(), c("Type", "Rate", "Frequency"))),
  stringsAsFactors=F)
Interaction.RepliedToPosts.Dataframe$Type = "Replied to posts"
Interaction.RepliedToPosts.Dataframe$Rate = tmp.df$Var1
Interaction.RepliedToPosts.Dataframe$Frequency = tmp.df$Freq
### Without Replying
tmp.df = data.frame(table(
  Responses.HeikeLanghans.Raw$
    In.the.past.month..how.many.times.have.you.interracted.with.posts.by.Heike...Without.replying..liked..re.posted..etc..))
Interaction.WithoutReplying.Dataframe = data.frame(
  matrix(vector(), nrow(tmp.df), 3,
         dimnames=list(c(), c("Type", "Rate", "Frequency"))),
  stringsAsFactors=F)
Interaction.WithoutReplying.Dataframe$Type = "Without replying"
Interaction.WithoutReplying.Dataframe$Rate = tmp.df$Var1
Interaction.WithoutReplying.Dataframe$Frequency = tmp.df$Freq
rm(tmp.df)

### Merge Interaction
Interaction = rbind(
  Interaction.ReadPosts,
  Interaction.RepliedToPosts.Dataframe,
  Interaction.WithoutReplying.Dataframe
)
Interaction$Percent = round(Interaction$Frequency/114, 4)




### Perceived v Calculated (AAI) - ReadPosts
Regression.Filter.Commented = "Yes"
Regression.X = "Authenticity"
Regression.X.Type.1 = "Perceived"
Regression.X.Type.2 = "Calculated (AAI)"
Regression.Y = "ReadPosts"
Regression.Filter.Interaction = "ReadPosts"
Regression.Title = 
  "Regression of 'Read Posts' communication\nin the past month on Authenticity\n(as perceived by fans who have commented)"
Regression.AxisX = "Authenticity"
Regression.AxisY = "Interaction rate"
Regression.Color = "Type"
Regression.DataFrame = Authenticity[Authenticity$Commented==Regression.Filter.Commented,]
### Calculate Correlations
Regression.Cor.1 = round(
  cor(eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.1,]$",Regression.X,sep=""))), 
      eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.1,]$",Regression.Y,sep="")))),4)
Regression.Cor.2 = round(
  cor(eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.2,]$",Regression.X,sep=""))), 
      eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.2,]$",Regression.Y,sep="")))),4)
### Plot regressions
Regression.Plot = 
  ggplot(Regression.DataFrame, aes(eval(parse(text=Regression.X)), eval(parse(text=Regression.Y)), color=Type)) + 
  geom_point() + stat_smooth(method=lm, aes(fill = Type), size=1, alpha=0.1) +
  geom_text(aes(0.08, 0.12, label=paste("r = ", Regression.Cor.1,sep=""), color=Regression.X.Type.1)) +
  geom_text(aes(0.17, 0.12, label=paste("r = ", Regression.Cor.2,sep=""), color=Regression.X.Type.2)) +
  scale_x_continuous(limits=c(0, 1), breaks=c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels=c("Very\nUnauthentic", "Unauthentic", "Neither", 
                              "Authentic", "Very\nAuthentic")) +
  scale_y_continuous(limits=c(0, 1), breaks=c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels=c("None", "Once or twice", "3-5 times", "6-10 times", "More than 10 times")) +    
  labs(list(title = Regression.Title, x = Regression.AxisX, y = Regression.AxisY)) + 
  coord_flip() +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Regression.Plot
### Save regression as image
ggsave(file=paste("regression-merged-authenticity-",Regression.Filter.Interaction,"-",Regression.Filter.Commented,".png", sep=""),
       plot = last_plot(), dpi=300)


### Perceived v Calculated (AAI) - ReadPosts
Regression.Filter.Commented = "No"
Regression.X = "Authenticity"
Regression.X.Type.1 = "Perceived"
Regression.X.Type.2 = "Calculated (AAI)"
Regression.Y = "ReadPosts"
Regression.Filter.Interaction = "ReadPosts"
Regression.Title = 
  "Regression of 'Read Posts' communication\nin the past month on Authenticity\n(as perceived by fans who have never commented)"
Regression.AxisX = "Authenticity"
Regression.AxisY = "Interaction rate"
Regression.Color = "Type"
Regression.DataFrame = Authenticity[Authenticity$Commented==Regression.Filter.Commented,]
### Calculate Correlations
Regression.Cor.1 = round(
  cor(eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.1,]$",Regression.X,sep=""))), 
      eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.1,]$",Regression.Y,sep="")))),4)
Regression.Cor.2 = round(
  cor(eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.2,]$",Regression.X,sep=""))), 
      eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.2,]$",Regression.Y,sep="")))),4)
### Plot regressions
Regression.Plot = 
  ggplot(Regression.DataFrame, aes(eval(parse(text=Regression.X)), eval(parse(text=Regression.Y)), color=Type)) + 
  geom_point() + stat_smooth(method=lm, aes(fill = Type), size=1, alpha=0.1) +
  geom_text(aes(0.08, 0.12, label=paste("r = ", Regression.Cor.1,sep=""), color=Regression.X.Type.1)) +
  geom_text(aes(0.17, 0.12, label=paste("r = ", Regression.Cor.2,sep=""), color=Regression.X.Type.2)) +
  scale_x_continuous(limits=c(0, 1), breaks=c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels=c("Very\nUnauthentic", "Unauthentic", "Neither", 
                              "Authentic", "Very\nAuthentic")) +
  scale_y_continuous(limits=c(0, 1), breaks=c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels=c("None", "Once or twice", "3-5 times", "6-10 times", "More than 10 times")) +    
  labs(list(title = Regression.Title, x = Regression.AxisX, y = Regression.AxisY)) + 
  coord_flip() +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Regression.Plot
### Save regression as image
ggsave(file=paste("regression-merged-authenticity-",Regression.Filter.Interaction,"-",Regression.Filter.Commented,".png", sep=""),
       plot = last_plot(), dpi=300)


### Perceived v Calculated (AAI) - RepliedToPosts
Regression.Filter.Commented = "Yes"
Regression.X = "Authenticity"
Regression.X.Type.1 = "Perceived"
Regression.X.Type.2 = "Calculated (AAI)"
Regression.Y = "RepliedToPosts"
Regression.Filter.Interaction = "RepliedToPosts"
Regression.Title = 
  "Regression of 'Replied to Posts' communication\nin the past month on Authenticity\n(as perceived by fans who have commented)"
Regression.AxisX = "Authenticity"
Regression.AxisY = "Interaction rate"
Regression.Color = "Type"
Regression.DataFrame = Authenticity[Authenticity$Commented==Regression.Filter.Commented,]
### Calculate Correlations
Regression.Cor.1 = round(
  cor(eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.1,]$",Regression.X,sep=""))), 
      eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.1,]$",Regression.Y,sep="")))),4)
Regression.Cor.2 = round(
  cor(eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.2,]$",Regression.X,sep=""))), 
      eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.2,]$",Regression.Y,sep="")))),4)
### Plot regressions
Regression.Plot = 
  ggplot(Regression.DataFrame, aes(eval(parse(text=Regression.X)), eval(parse(text=Regression.Y)), color=Type)) + 
  geom_point() + stat_smooth(method=lm, aes(fill = Type), size=1, alpha=0.1) +
  geom_text(aes(0.08, 0.88, label=paste("r = ", Regression.Cor.1,sep=""), color=Regression.X.Type.1)) +
  geom_text(aes(0.17, 0.88, label=paste("r = ", Regression.Cor.2,sep=""), color=Regression.X.Type.2)) +
  scale_x_continuous(limits=c(0, 1), breaks=c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels=c("Very\nUnauthentic", "Unauthentic", "Neither", 
                              "Authentic", "Very\nAuthentic")) +
  scale_y_continuous(limits=c(0, 1), breaks=c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels=c("None", "Once or twice", "3-5 times", "6-10 times", "More than 10 times")) +    
  labs(list(title = Regression.Title, x = Regression.AxisX, y = Regression.AxisY)) + 
  coord_flip() +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Regression.Plot
### Save regression as image
ggsave(file=paste("regression-merged-authenticity-",Regression.Filter.Interaction,"-",Regression.Filter.Commented,".png", sep=""),
       plot = last_plot(), dpi=300)



### Perceived v Calculated (AAI) - RepliedToPosts
Regression.Filter.Commented = "No"
Regression.X = "Authenticity"
Regression.X.Type.1 = "Perceived"
Regression.X.Type.2 = "Calculated (AAI)"
Regression.Y = "RepliedToPosts"
Regression.Filter.Interaction = "RepliedToPosts"
Regression.Title = "Regression of\n'Replied to Posts' in the past month on Authenticity\n(fans who have never commented)"
Regression.AxisX = "Authenticity"
Regression.AxisY = "Interaction rate"
Regression.Color = "Type"
Regression.DataFrame = Authenticity[Authenticity$Commented==Regression.Filter.Commented,]
### Calculate Correlations
Regression.Cor.1 = round(
  cor(eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.1,]$",Regression.X,sep=""))), 
      eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.1,]$",Regression.Y,sep="")))),4)
Regression.Cor.2 = round(
  cor(eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.2,]$",Regression.X,sep=""))), 
      eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.2,]$",Regression.Y,sep="")))),4)
### Plot regressions
Regression.Plot = 
  ggplot(Regression.DataFrame, aes(eval(parse(text=Regression.X)), eval(parse(text=Regression.Y)), color=Type)) + 
  geom_point() + stat_smooth(method=lm, aes(fill = Type), size=1, alpha=0.1) +
  geom_text(aes(0.08, 0.88, label=paste("r = ", Regression.Cor.1,sep=""), color=Regression.X.Type.1)) +
  geom_text(aes(0.17, 0.88, label=paste("r = ", Regression.Cor.2,sep=""), color=Regression.X.Type.2)) +
  scale_x_continuous(limits=c(0, 1), breaks=c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels=c("Very\nUnauthentic", "Unauthentic", "Neither", 
                              "Authentic", "Very\nAuthentic")) +
  scale_y_continuous(limits=c(0, 1), breaks=c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels=c("None", "Once or twice", "3-5 times", "6-10 times", "More than 10 times")) +    
  labs(list(title = Regression.Title, x = Regression.AxisX, y = Regression.AxisY)) + 
  coord_flip() +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Regression.Plot
### Save regression as image
ggsave(file=paste("regression-merged-authenticity-",Regression.Filter.Interaction,"-",Regression.Filter.Commented,".png", sep=""),
       plot = last_plot(), dpi=300)


### Perceived v Calculated (AAI) - WithoutReplying
Regression.Filter.Commented = "Yes"
Regression.X = "Authenticity"
Regression.X.Type.1 = "Perceived"
Regression.X.Type.2 = "Calculated (AAI)"
Regression.Y = "WithoutReplying"
Regression.Filter.Interaction = "WithoutReplying"
Regression.Title = 
  "Regression of 'Without replying' communication\nin the past month on Authenticity\n(as perceived by fans who have commented)"
Regression.AxisX = "Authenticity"
Regression.AxisY = "Interaction rate"
Regression.Color = "Type"
Regression.DataFrame = Authenticity[Authenticity$Commented==Regression.Filter.Commented,]
### Calculate Correlations
Regression.Cor.1 = round(
  cor(eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.1,]$",Regression.X,sep=""))), 
      eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.1,]$",Regression.Y,sep="")))),4)
Regression.Cor.2 = round(
  cor(eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.2,]$",Regression.X,sep=""))), 
      eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.2,]$",Regression.Y,sep="")))),4)
### Plot regressions
Regression.Plot = 
  ggplot(Regression.DataFrame, aes(eval(parse(text=Regression.X)), eval(parse(text=Regression.Y)), color=Type)) + 
  geom_point() + stat_smooth(method=lm, aes(fill = Type), size=1, alpha=0.1) +
  geom_text(aes(0.08, 0.88, label=paste("r = ", Regression.Cor.1,sep=""), color=Regression.X.Type.1)) +
  geom_text(aes(0.17, 0.88, label=paste("r = ", Regression.Cor.2,sep=""), color=Regression.X.Type.2)) +
  scale_x_continuous(limits=c(0, 1), breaks=c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels=c("Very\nUnauthentic", "Unauthentic", "Neither", 
                              "Authentic", "Very\nAuthentic")) +
  scale_y_continuous(limits=c(0, 1), breaks=c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels=c("None", "Once or twice", "3-5 times", "6-10 times", "More than 10 times")) +    
  labs(list(title = Regression.Title, x = Regression.AxisX, y = Regression.AxisY)) + 
  coord_flip() +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Regression.Plot
### Save regression as image
ggsave(file=paste("regression-merged-authenticity-",Regression.Filter.Interaction,"-",Regression.Filter.Commented,".png", sep=""),
       plot = last_plot(), dpi=300)

### Perceived v Calculated (AAI) - WithoutReplying
Regression.Filter.Commented = "No"
Regression.X = "Authenticity"
Regression.X.Type.1 = "Perceived"
Regression.X.Type.2 = "Calculated (AAI)"
Regression.Y = "WithoutReplying"
Regression.Filter.Interaction = "WithoutReplying"
Regression.Title = 
  "Regression of 'Without Replying' communication\nin the past month on Authenticity\n(as perceived by fans who have never commented)"
Regression.AxisX = "Authenticity"
Regression.AxisY = "Interaction rate"
Regression.Color = "Type"
Regression.DataFrame = Authenticity[Authenticity$Commented==Regression.Filter.Commented,]
### Calculate Correlations
Regression.Cor.1 = round(
  cor(eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.1,]$",Regression.X,sep=""))), 
      eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.1,]$",Regression.Y,sep="")))),4)
Regression.Cor.2 = round(
  cor(eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.2,]$",Regression.X,sep=""))), 
      eval(parse(text=paste("Regression.DataFrame[Regression.DataFrame$Type==Regression.X.Type.2,]$",Regression.Y,sep="")))),4)
### Plot regressions
Regression.Plot = 
  ggplot(Regression.DataFrame, aes(eval(parse(text=Regression.X)), eval(parse(text=Regression.Y)), color=Type)) + 
  geom_point() + stat_smooth(method=lm, aes(fill = Type), size=1, alpha=0.1) +
  geom_text(aes(0.08, 0.88, label=paste("r = ", Regression.Cor.1,sep=""), color=Regression.X.Type.1)) +
  geom_text(aes(0.17, 0.88, label=paste("r = ", Regression.Cor.2,sep=""), color=Regression.X.Type.2)) +
  scale_x_continuous(limits=c(0, 1), breaks=c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels=c("Very\nUnauthentic", "Unauthentic", "Neither", 
                              "Authentic", "Very\nAuthentic")) +
  scale_y_continuous(limits=c(0, 1), breaks=c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels=c("None", "Once or twice", "3-5 times", "6-10 times", "More than 10 times")) +    
  labs(list(title = Regression.Title, x = Regression.AxisX, y = Regression.AxisY)) + 
  coord_flip() +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Regression.Plot
### Save regression as image
ggsave(file=paste("regression-merged-authenticity-",Regression.Filter.Interaction,"-",Regression.Filter.Commented,".png", sep=""),
       plot = last_plot(), dpi=300)


### Plot Distributions
###
### Language
Frequency.Plot.Title = "Distribution of Language"
Frequency.Plot.AxisX = ""
Frequency.Plot.AxisY = "% respondents"
### Plot distribution as Bar chart
Frequency.Plot = ggplot(Language.Dataframe, aes(Language, Percentage, fill=Language)) + 
  geom_histogram() +
  geom_text(aes(label = sprintf("%1.2f%%", Percentage*100), y=0.05)) +
  scale_y_continuous(labels = percent) +
  labs(list(title = Frequency.Plot.Title, x = Frequency.Plot.AxisX, y = Frequency.Plot.AxisY)) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Frequency.Plot
ggsave(file="distribution-of-language-bar.png", plot = last_plot(), dpi=300)
### Plot distribution as Polar Coord chart
Frequency.Plot = ggplot(Language.Dataframe, aes(Language, Percentage, fill=Language)) + 
  geom_bar(width = 1) + coord_polar() +
  scale_y_continuous(labels = percent) +
  labs(list(title = Frequency.Plot.Title, x = Frequency.Plot.AxisX, y = Frequency.Plot.AxisY)) + 
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Frequency.Plot
### Save distribution as image
ggsave(file="distribution-of-language-polarcoord.png", plot = last_plot(), dpi=300)


### Age
Frequency.Plot.Title = "Distribution of Age"
Frequency.Plot.AxisX = "Age (Years)"
Frequency.Plot.AxisY = "% respondents"
### Plot distribution as Bar chart
tmp.df = data.frame(prop.table(table(Responses.HeikeLanghans.Coded$Age)))
Frequency.Plot = ggplot(tmp.df, aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_histogram() +
  geom_text(aes(label = sprintf("%1.2f%%", Freq*100), y=0.02)) +
  scale_y_continuous(labels=percent) +
  labs(list(title = Frequency.Plot.Title, x = Frequency.Plot.AxisX, y = Frequency.Plot.AxisY)) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Frequency.Plot
### Save distribution as image
ggsave(file=tolower(paste(gsub(" ", "-", Frequency.Plot.Title, fixed=TRUE), ".png", sep="")), plot = last_plot(), dpi=300)

### Country
Frequency.Plot.Title = "Distribution of Country"
Frequency.Plot.AxisX = ""
Frequency.Plot.AxisY = "% respondents"
### Plot distribution as Bar chart
tmp.df = data.frame(prop.table(table(Responses.HeikeLanghans.Coded$Country)))
Frequency.Plot = ggplot(tmp.df, aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_histogram() +
  scale_y_continuous(labels=percent) +
  labs(list(title = Frequency.Plot.Title, x = Frequency.Plot.AxisX, y = Frequency.Plot.AxisY)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Frequency.Plot
### Save distribution as image
ggsave(file=tolower(paste(gsub(" ", "-", Frequency.Plot.Title, fixed=TRUE), ".png", sep="")), plot = last_plot(), dpi=300)

### Gender
Frequency.Plot.Title = "Distribution of Gender"
Frequency.Plot.AxisX = ""
Frequency.Plot.AxisY = "% respondents"
### Plot distribution as Bar chart
tmp.df = data.frame(prop.table(table(Responses.HeikeLanghans.Coded$Gender)))
Frequency.Plot = ggplot(tmp.df, aes(x=Var1, y=Freq, fill=Var1)) +   
  geom_histogram() +
  geom_text(aes(label = sprintf("%1.2f%%", Freq*100), y=0.05)) +
  scale_y_continuous(labels=percent) +
  labs(list(title = Frequency.Plot.Title, x = Frequency.Plot.AxisX, y = Frequency.Plot.AxisY)) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Frequency.Plot
### Save distribution as image
ggsave(file=tolower(paste(gsub(" ", "-", Frequency.Plot.Title, fixed=TRUE), ".png", sep="")), plot = last_plot(), dpi=300)

### Have you ever commented/replied to a post by Heike Langhans on social media networks?"
Frequency.Plot.Title = "Have you ever commented/replied to a post\nby Heike Langhans on social media networks?"
Frequency.Plot.AxisX = ""
Frequency.Plot.AxisY = "% respondents"
### Plot distribution as Bar chart
tmp.df = data.frame(prop.table(table(Responses.HeikeLanghans.Coded$Commented)))
Frequency.Plot = ggplot(tmp.df, aes(x=Var1, y=Freq, fill=Var1)) +   
  geom_histogram() +
  geom_text(aes(label = sprintf("%1.2f%%", Freq*100), y=0.05)) +
  scale_y_continuous(labels=percent) +
  labs(list(title = Frequency.Plot.Title, x = Frequency.Plot.AxisX, y = Frequency.Plot.AxisY)) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Frequency.Plot
### Save distribution as image
ggsave(file=tolower(paste(gsub("\n", "-", gsub("?", "", gsub("/", "", 
                                                             gsub(" ", "-", Frequency.Plot.Title, fixed=TRUE), fixed=TRUE), 
                                               fixed=TRUE), fixed=TRUE), ".png", sep="")), plot = last_plot(), dpi=300)

### Do you believe that Heike Langhans is an authentic artist?
Frequency.Plot.Title = "Do you believe that Heike Langhans\nis an authentic artist?"
Frequency.Plot.AxisX = ""
Frequency.Plot.AxisY = "% respondents"
### Plot distribution as Bar chart
tmp.df = data.frame(prop.table(table(Responses.HeikeLanghans.Raw$Do.you.believe.that.Heike.Langhans.is.an.authentic.artist....)))
Frequency.Plot = ggplot(tmp.df, aes(x=Var1, y=Freq, fill=Var1)) +   
  geom_histogram() +
  geom_text(aes(label = sprintf("%1.2f%%", Freq*100), y=0.05)) +
  scale_y_continuous(labels=percent) +  
  scale_x_discrete(limits=c("Don't believe at all", "Don't believe", "I don't know","Believe","Strongly believe")) +
  labs(list(title = Frequency.Plot.Title, x = Frequency.Plot.AxisX, y = Frequency.Plot.AxisY)) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Frequency.Plot
### Save distribution as image
ggsave(file=tolower(paste(gsub("\n", "-", gsub("?", "", gsub("/", "", 
                                                             gsub(" ", "-", Frequency.Plot.Title, fixed=TRUE), fixed=TRUE), 
                                               fixed=TRUE), fixed=TRUE), ".png", sep="")), plot = last_plot(), dpi=300)

### Do you believe that Heike Langhans expresses her true self on social media networks?
Frequency.Plot.Title = "Do you believe that Heike Langhans\nexpresses her true self on social media networks?"
Frequency.Plot.AxisX = ""
Frequency.Plot.AxisY = "% respondents"
### Plot distribution as Bar chart
tmp.df = data.frame(prop.table(table(Responses.HeikeLanghans.Raw$
                                       Do.you.believe.that.Heike.expresses.her.true.self.on.social.media.networks....)))
Frequency.Plot = ggplot(tmp.df, aes(x=Var1, y=Freq, fill=Var1)) +   
  geom_histogram() +
  geom_text(aes(label = sprintf("%1.2f%%", Freq*100), y=0.05)) +
  scale_y_continuous(labels=percent) +  
  scale_x_discrete(limits=c("Don't believe at all", "Don't believe", "I don't know","Believe","Strongly believe")) +
  labs(list(title = Frequency.Plot.Title, x = Frequency.Plot.AxisX, y = Frequency.Plot.AxisY)) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Frequency.Plot
### Save distribution as image
ggsave(file=tolower(paste(gsub("\n", "-", gsub("?", "", gsub("/", "", 
                                                             gsub(" ", "-", Frequency.Plot.Title, fixed=TRUE), fixed=TRUE), 
                                               fixed=TRUE), fixed=TRUE), ".png", sep="")), plot = last_plot(), dpi=300)

### How significant is Heike Langhans' authenticity to you?
Frequency.Plot.Title = "How significant is\nHeike Langhans\' authenticity to you?"
Frequency.Plot.AxisX = ""
Frequency.Plot.AxisY = "% respondents"
### Plot distribution as Bar chart
tmp.df = data.frame(prop.table(table(Responses.HeikeLanghans.Raw$How.significant.is.Heike.s.authenticity.to.you....)))
Frequency.Plot = ggplot(tmp.df, aes(x=Var1, y=Freq, fill=Var1)) +   
  geom_histogram() +
  geom_text(aes(label = sprintf("%1.2f%%", Freq*100), y=0.05)) +
  scale_y_continuous(labels=percent) +  
  scale_x_discrete(limits=c("Very insignificant", "Insignificant", "I don't know","Significant","Very significant")) +
  labs(list(title = Frequency.Plot.Title, x = Frequency.Plot.AxisX, y = Frequency.Plot.AxisY)) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Frequency.Plot
### Save distribution as image
ggsave(file=tolower(paste(gsub("\n", "-", gsub("?", "", gsub("/", "", 
                                                             gsub(" ", "-", Frequency.Plot.Title, fixed=TRUE), fixed=TRUE), 
                                               fixed=TRUE), fixed=TRUE), ".png", sep="")), plot = last_plot(), dpi=300)






### Age by Gender
Frequency.Plot.Title = "Distribution of Age by Gender"
Frequency.Plot.AxisX = "Age (Years)"
Frequency.Plot.AxisY = "% respondents"
### Plot distribution as Bar chart
tmp.df = data.frame(prop.table(table(Responses.HeikeLanghans.Raw[2:3])))
Frequency.Plot = ggplot(tmp.df, aes(x=Age, y=Freq, fill=Age)) +   
  geom_histogram() +
  geom_text(aes(label = sprintf("%1.2f%%", Freq*100), y=0.015)) +
  scale_y_continuous(labels=percent) +  
  scale_x_discrete(labels=c("18 to 23","24 to 30","31 to 40","41 to 50","50 or over","Prefer\nnot to say")) +
  facet_grid(. ~ Gender) +
  labs(list(title = Frequency.Plot.Title, x = Frequency.Plot.AxisX, y = Frequency.Plot.AxisY)) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Frequency.Plot
### Save distribution as image
ggsave(file=tolower(paste(gsub(" ", "-", Frequency.Plot.Title, fixed=TRUE), ".png", sep="")), plot = last_plot(), dpi=300)  

### Commented by Gender
Frequency.Plot.Title = "Have you ever commented/replied on a post by Gender"
Frequency.Plot.AxisX = ""
Frequency.Plot.AxisY = "% respondents"
### Plot distribution as Bar chart
tmp.df = data.frame(prop.table(table(Responses.HeikeLanghans.Raw[,c(3,5)])))
Frequency.Plot = 
  ggplot(tmp.df, aes(
    x=Have.you.ever.commented.replied.on.a.post.by.Heike.Langhans.on.social.media.networks., 
    y=Freq, 
    fill=Have.you.ever.commented.replied.on.a.post.by.Heike.Langhans.on.social.media.networks.)) +   
  geom_histogram() +
  geom_text(aes(label = sprintf("%1.2f%%", Freq*100), y=0.04)) +
  scale_y_continuous(labels=percent) +  
  facet_grid(. ~ Gender) +
  labs(list(title = Frequency.Plot.Title, x = Frequency.Plot.AxisX, y = Frequency.Plot.AxisY)) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Frequency.Plot
### Save distribution as image
ggsave(file=tolower(paste(gsub("\n", "-", gsub("?", "", gsub("/", "", 
                                                             gsub(" ", "-", Frequency.Plot.Title, fixed=TRUE), fixed=TRUE), 
                                               fixed=TRUE), fixed=TRUE), ".png", sep="")), plot = last_plot(), dpi=300)

### Commented by Age
Frequency.Plot.Title = "Have you ever commented or replied on a post by Age"
Frequency.Plot.AxisX = ""
Frequency.Plot.AxisY = "% respondents"
### Plot distribution as Bar chart
tmp.df = data.frame(prop.table(table(Responses.HeikeLanghans.Raw[,c(2,5)])))
Frequency.Plot = 
  ggplot(tmp.df, aes(
    x=Have.you.ever.commented.replied.on.a.post.by.Heike.Langhans.on.social.media.networks., 
    y=Freq, 
    fill=Have.you.ever.commented.replied.on.a.post.by.Heike.Langhans.on.social.media.networks.)) +   
  geom_histogram() +
  geom_text(aes(label = sprintf("%1.2f%%", Freq*100), y=0.02, size=1)) +
  scale_y_continuous(labels=percent) +  
  facet_grid(. ~ Age) +
  labs(list(title = Frequency.Plot.Title, x = Frequency.Plot.AxisX, y = Frequency.Plot.AxisY)) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Frequency.Plot
### Save distribution as image
ggsave(file=tolower(paste(gsub("\n", "-", gsub("?", "", 
                                               gsub("/", "", gsub(" ", "-", Frequency.Plot.Title, fixed=TRUE), fixed=TRUE), 
                                               fixed=TRUE), fixed=TRUE), ".png", sep="")), plot = last_plot(), dpi=300)

### Interaction
Frequency.Plot.Title = "Distribution of Interaction in the past month"
Frequency.Plot.AxisX = "Interaction type"
Frequency.Plot.AxisY = "Interaction rate"
### Plot distribution as Bar chart
Frequency.Plot = 
  ggplot(Interaction, aes(Type, Rate)) + 
  geom_tile(aes(fill = Percent)) +
  geom_text(aes(label = sprintf("%1.2f%%", Percent*100))) +
  scale_y_discrete(
    limits=c("Never", "Once or twice", "3 - 5 times", "6 - 10 times", "More than 10 times"),
    labels=c("None", "Once or twice", "3 - 5 times", "6 - 10 times", "More than 10 times")
  ) + 
  scale_x_discrete(
    limits=c("Without replying", "Replied to posts", "Read posts")
  ) + 
  scale_fill_gradient(name = "Frequency", low = "white", high = "steelblue", labels = percent_format()) +
  labs(list(title = Frequency.Plot.Title, x = Frequency.Plot.AxisX, y = Frequency.Plot.AxisY)) + 
  coord_flip() +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Frequency.Plot
### Save distribution as image
ggsave(file=tolower(paste(gsub(" ", "-", Frequency.Plot.Title, fixed=TRUE), ".png", sep="")), plot = last_plot(), dpi=300)


### How significant by Gender
Frequency.Plot.Title = "How significant is Heike Langhans' authenticity by Gender"
Frequency.Plot.AxisX = ""
Frequency.Plot.AxisY = "% respondents"
### Plot distribution as Bar chart
tmp.df = data.frame(prop.table(table(Responses.HeikeLanghans.Raw[,c(3,8)])))
Frequency.Plot = 
  ggplot(tmp.df, aes(
    x=How.significant.is.Heike.s.authenticity.to.you...., 
    y=Freq, 
    fill=How.significant.is.Heike.s.authenticity.to.you....)) +   
  geom_histogram() +
  geom_text(aes(label = sprintf("%1.2f%%", Freq*100), y=0.04)) +
  scale_x_discrete(
    limits=c("Very insignificant", "Insignificant", "I don't know", "Significant", "Very significant"),
    labels=c("Very\ninsignificant", "Insignificant", "I don't know", "Significant", "Very\nsignificant")
  ) + 
  scale_y_continuous(labels=percent) +  
  facet_grid(. ~ Gender) +
  labs(list(title = Frequency.Plot.Title, x = Frequency.Plot.AxisX, y = Frequency.Plot.AxisY)) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight=1, face="bold", color="black", size=18))
Frequency.Plot
### Save distribution as image
ggsave(file=tolower(paste(gsub("\n", "-",
                               gsub("?", "", gsub("/", "", gsub(" ", "-", Frequency.Plot.Title, fixed=TRUE), fixed=TRUE), 
                                               fixed=TRUE), fixed=TRUE), ".png", sep="")), plot = last_plot(), dpi=300)



### Genderal Tendency Statistics

summary(Responses.HeikeLanghans.Coded$ReadPosts)
summary(Responses.HeikeLanghans.Coded$RepliedToPosts)
summary(Responses.HeikeLanghans.Coded$WithoutReplying)

describe(Responses.HeikeLanghans.Coded$ReadPosts)
describe(Responses.HeikeLanghans.Coded$RepliedToPosts)
describe(Responses.HeikeLanghans.Coded$WithoutReplying)

summary(Responses.HeikeLanghans.Coded$Do.you.believe.that.Heike.Langhans.is.an.authentic.artist....)
summary(Responses.HeikeLanghans.Coded$Do.you.believe.that.Heike.expresses.her.true.self.on.social.media.networks....)
summary(Responses.HeikeLanghans.Coded$How.significant.is.Heike.s.authenticity.to.you....)

describe(Responses.HeikeLanghans.Coded$Do.you.believe.that.Heike.Langhans.is.an.authentic.artist....)
describe(Responses.HeikeLanghans.Coded$Do.you.believe.that.Heike.expresses.her.true.self.on.social.media.networks....)
describe(Responses.HeikeLanghans.Coded$How.significant.is.Heike.s.authenticity.to.you....)

### AAI items
summary(Responses.HeikeLanghans.Coded$Heike.represents.the.genre.she.works.in..music..lyrics..aesthetics....)
describe(Responses.HeikeLanghans.Coded$Heike.represents.the.genre.she.works.in..music..lyrics..aesthetics....)

summary(Responses.HeikeLanghans.Coded$Heike.shows.consistency.between.her.beliefs.and.actions...)
describe(Responses.HeikeLanghans.Coded$Heike.shows.consistency.between.her.beliefs.and.actions...)

summary(Responses.HeikeLanghans.Coded$Heike.avoids.trading.off.the.essence.of.the.genre.strictly.for.profit...)
describe(Responses.HeikeLanghans.Coded$Heike.avoids.trading.off.the.essence.of.the.genre.strictly.for.profit...)

summary(Responses.HeikeLanghans.Coded$Heike.admits.mistakes.when.they.occur..personal.or.professional....)
describe(Responses.HeikeLanghans.Coded$Heike.admits.mistakes.when.they.occur..personal.or.professional....)

summary(Responses.HeikeLanghans.Coded$Heike.makes.her.fans.feel.they.belong.to.the.same.sub.culture...)
describe(Responses.HeikeLanghans.Coded$Heike.makes.her.fans.feel.they.belong.to.the.same.sub.culture...)

summary(Responses.HeikeLanghans.Coded$Heike.resists.pressures.on.her.to.do.things.contrary.to.her.beliefs...)
describe(Responses.HeikeLanghans.Coded$Heike.resists.pressures.on.her.to.do.things.contrary.to.her.beliefs...)

summary(Responses.HeikeLanghans.Coded$
          Heike.maintains.the.standards.and.style.of.the.genre.she.works.in..music..lyrics..aesthetics....)
describe(Responses.HeikeLanghans.Coded$
           Heike.maintains.the.standards.and.style.of.the.genre.she.works.in..music..lyrics..aesthetics....)

summary(Responses.HeikeLanghans.Coded$Heike.acknowledges.how.others.see.her.skills.and.abilities...)
describe(Responses.HeikeLanghans.Coded$Heike.acknowledges.how.others.see.her.skills.and.abilities...)




### Correlations COMMENTED = YES
### Perceived
Correlation.Perceived.Commented = Responses.HeikeLanghans.Coded.Commented[6:11]
names(Correlation.Perceived.Commented) = 
  sub("^Do.you.believe.that.Heike.Langhans.is.an.authentic.artist....$", "Authentic", 
      names(Correlation.Perceived.Commented))
names(Correlation.Perceived.Commented) = 
  sub("^Do.you.believe.that.Heike.expresses.her.true.self.on.social.media.networks....$", "Expresses", 
      names(Correlation.Perceived.Commented))
names(Correlation.Perceived.Commented) = 
  sub("^How.significant.is.Heike.s.authenticity.to.you....$", "Significant", names(Correlation.Perceived.Commented))

Correlation.Perceived.Commented$Perceived.Authenticity = 
  rowMeans(Correlation.Perceived.Commented[1:2]) * Correlation.Perceived.Commented$Significant

round(cor(Correlation.Perceived.Commented$ReadPosts, Correlation.Perceived.Commented$Perceived.Authenticity), 4)
round(cor(Correlation.Perceived.Commented$RepliedToPosts, Correlation.Perceived.Commented$Perceived.Authenticity), 4)
round(cor(Correlation.Perceived.Commented$WithoutReplying, Correlation.Perceived.Commented$Perceived.Authenticity), 4)

fit = lm(Perceived.Authenticity ~ ReadPosts + RepliedToPosts + WithoutReplying, Correlation.Perceived.Commented)
summary(fit)
anova(fit)

### AAI Calculated
Correlation.AAI.Calulated.Commented = Responses.HeikeLanghans.Coded.Commented[,c(8:11,13:20)]
names(Correlation.AAI.Calulated.Commented) = 
  sub("^Heike.represents.the.genre.she.works.in..music..lyrics..aesthetics....$", "AAI.1", 
      names(Correlation.AAI.Calulated.Commented))
names(Correlation.AAI.Calulated.Commented) = 
  sub("^Heike.shows.consistency.between.her.beliefs.and.actions...$", "AAI.2", 
      names(Correlation.AAI.Calulated.Commented))
names(Correlation.AAI.Calulated.Commented) = 
  sub("^Heike.avoids.trading.off.the.essence.of.the.genre.strictly.for.profit...$", "AAI.3", 
      names(Correlation.AAI.Calulated.Commented))
names(Correlation.AAI.Calulated.Commented) = 
  sub("^Heike.admits.mistakes.when.they.occur..personal.or.professional....$", "AAI.4", 
      names(Correlation.AAI.Calulated.Commented))
names(Correlation.AAI.Calulated.Commented) = 
  sub("^Heike.makes.her.fans.feel.they.belong.to.the.same.sub.culture...$", "AAI.5", 
      names(Correlation.AAI.Calulated.Commented))
names(Correlation.AAI.Calulated.Commented) = 
  sub("^Heike.resists.pressures.on.her.to.do.things.contrary.to.her.beliefs...$", "AAI.6", 
      names(Correlation.AAI.Calulated.Commented))
names(Correlation.AAI.Calulated.Commented) = 
  sub("^Heike.maintains.the.standards.and.style.of.the.genre.she.works.in..music..lyrics..aesthetics....$", "AAI.7", 
      names(Correlation.AAI.Calulated.Commented))
names(Correlation.AAI.Calulated.Commented) = 
  sub("^Heike.acknowledges.how.others.see.her.skills.and.abilities...$", "AAI.8", 
      names(Correlation.AAI.Calulated.Commented))
names(Correlation.AAI.Calulated.Commented) = 
  sub("^How.significant.is.Heike.s.authenticity.to.you....$", "Significant", 
      names(Correlation.AAI.Calulated.Commented))

Correlation.AAI.Calulated.Commented$AAI.Authenticity = 
  rowMeans(Correlation.AAI.Calulated.Commented[5:12]) * Correlation.AAI.Calulated.Commented$Significant

cor(Correlation.AAI.Calulated.Commented$ReadPosts, Correlation.AAI.Calulated.Commented$AAI.Authenticity)
cor(Correlation.AAI.Calulated.Commented$RepliedToPosts, Correlation.AAI.Calulated.Commented$AAI.Authenticity)
cor(Correlation.AAI.Calulated.Commented$WithoutReplying, Correlation.AAI.Calulated.Commented$AAI.Authenticity)

fit = lm(AAI.Authenticity ~ ReadPosts + RepliedToPosts + WithoutReplying, Correlation.AAI.Calulated.Commented)
summary(fit)
anova(fit)








### Correlations COMMENTED = NO
### Perceived
Correlation.Perceived.NeverCommented = Responses.HeikeLanghans.Coded.NeverCommented[6:11]
names(Correlation.Perceived.NeverCommented) = 
  sub("^Do.you.believe.that.Heike.Langhans.is.an.authentic.artist....$", "Authentic", 
      names(Correlation.Perceived.NeverCommented))
names(Correlation.Perceived.NeverCommented) = 
  sub("^Do.you.believe.that.Heike.expresses.her.true.self.on.social.media.networks....$", "Expresses", 
      names(Correlation.Perceived.NeverCommented))
names(Correlation.Perceived.NeverCommented) = 
  sub("^How.significant.is.Heike.s.authenticity.to.you....$", "Significant", names(Correlation.Perceived.NeverCommented))

Correlation.Perceived.NeverCommented$Perceived.Authenticity = 
  rowMeans(Correlation.Perceived.NeverCommented[1:2]) * Correlation.Perceived.NeverCommented$Significant

round(cor(Correlation.Perceived.NeverCommented$ReadPosts, Correlation.Perceived.NeverCommented$Perceived.Authenticity), 4)
round(cor(Correlation.Perceived.NeverCommented$WithoutReplying, Correlation.Perceived.NeverCommented$Perceived.Authenticity), 4)

fit = lm(Perceived.Authenticity ~ ReadPosts + WithoutReplying, Correlation.Perceived.NeverCommented)
summary(fit)
anova(fit)

### AAI Calculated
Correlation.AAI.Calulated.NeverCommented = Responses.HeikeLanghans.Coded.NeverCommented[,c(8:11,13:20)]
names(Correlation.AAI.Calulated.NeverCommented) = 
  sub("^Heike.represents.the.genre.she.works.in..music..lyrics..aesthetics....$", "AAI.1", 
      names(Correlation.AAI.Calulated.NeverCommented))
names(Correlation.AAI.Calulated.NeverCommented) = 
  sub("^Heike.shows.consistency.between.her.beliefs.and.actions...$", "AAI.2", 
      names(Correlation.AAI.Calulated.NeverCommented))
names(Correlation.AAI.Calulated.NeverCommented) = 
  sub("^Heike.avoids.trading.off.the.essence.of.the.genre.strictly.for.profit...$", "AAI.3", 
      names(Correlation.AAI.Calulated.NeverCommented))
names(Correlation.AAI.Calulated.NeverCommented) = 
  sub("^Heike.admits.mistakes.when.they.occur..personal.or.professional....$", "AAI.4", 
      names(Correlation.AAI.Calulated.NeverCommented))
names(Correlation.AAI.Calulated.NeverCommented) = 
  sub("^Heike.makes.her.fans.feel.they.belong.to.the.same.sub.culture...$", "AAI.5", 
      names(Correlation.AAI.Calulated.NeverCommented))
names(Correlation.AAI.Calulated.NeverCommented) = 
  sub("^Heike.resists.pressures.on.her.to.do.things.contrary.to.her.beliefs...$", "AAI.6", 
      names(Correlation.AAI.Calulated.NeverCommented))
names(Correlation.AAI.Calulated.NeverCommented) = 
  sub("^Heike.maintains.the.standards.and.style.of.the.genre.she.works.in..music..lyrics..aesthetics....$", "AAI.7", 
      names(Correlation.AAI.Calulated.NeverCommented))
names(Correlation.AAI.Calulated.NeverCommented) = 
  sub("^Heike.acknowledges.how.others.see.her.skills.and.abilities...$", "AAI.8", 
      names(Correlation.AAI.Calulated.NeverCommented))
names(Correlation.AAI.Calulated.NeverCommented) = 
  sub("^How.significant.is.Heike.s.authenticity.to.you....$", "Significant", 
      names(Correlation.AAI.Calulated.NeverCommented))

Correlation.AAI.Calulated.NeverCommented$AAI.Authenticity = 
  rowMeans(Correlation.AAI.Calulated.NeverCommented[5:12]) * Correlation.AAI.Calulated.NeverCommented$Significant

cor(Correlation.AAI.Calulated.NeverCommented$ReadPosts, Correlation.AAI.Calulated.NeverCommented$AAI.Authenticity)
cor(Correlation.AAI.Calulated.NeverCommented$WithoutReplying, Correlation.AAI.Calulated.NeverCommented$AAI.Authenticity)

fit = lm(AAI.Authenticity ~ ReadPosts + WithoutReplying, Correlation.AAI.Calulated.NeverCommented)
summary(fit)
anova(fit)
