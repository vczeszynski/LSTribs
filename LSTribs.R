
#GENERAL DATAFRAME MANAGEMENT
#load necessary packages

library(car)
library(tidyr)
library(ggExtra)
library(scales)
library(dplyr)
library(gridExtra)
library(cowplot)
library(MASS)
library(ggplot2)


# set working directory
setwd("C:/Users/vanes/OneDrive/Documents/R/LS_Tribs")

#Import data file

LS <- read.csv("LS_tribs.csv")

#Check data matrix
summary(LS)

#log10 transform data to improve normality
LS$log10area <- log10(LS$Watershed_area)
LS$log10SUVA <- log10(LS$SUVA)
LS$log10DOC <- log10(LS$DOC)
LS$log10wetland <- log10(LS$Wetland)
LS$log10agriculture <- log10(LS$Agriculture)
LS$log10urban <- log10(LS$Urban)
LS$log10forest <- log10(LS$Forest)
LS$log10bedrock <- log10(LS$Bedrock_water)
LS$log10saturatedK <- log10(LS$Saturated_K)
LS$log10drift <- log10(LS$Drift_thickness)
LS$log10E2E3 <- log10(LS$E2E3)


#SIMPLE LINEAR REGRESSION MODELS FOR DOC CONCENTRATION AND COMPOSITION

WetlandPlot <- ggplot(data = LS, aes(x=Wetland, y= DOC)) + theme_classic() + geom_point() + xlab("% Wetland") + ylab("DOC (mg/L)") + theme(text=element_text(color = "black",size = 16,angle = 0,lineheight = 0.9)) + scale_y_log10()

WetlandPlot

ggplot(data = LS, aes(x=Agriculture, y= DOC)) + theme_classic() + geom_point() + xlab("% Agriculture") + ylab("DOC (mg/L)") + theme(text=element_text(color = "black",size = 16,angle = 0,lineheight = 0.9)) + scale_y_log10()

ggplot(data = LS, aes(x=Forest, y= DOC)) + theme_classic() + geom_point() + xlab("% Forest") + ylab("") + theme(text=element_text(color = "black",size = 16,angle = 0,lineheight = 0.9))

DriftPlot <- ggplot(data = LS, aes(x=Drift_thickness, y= DOC)) + theme_classic() + geom_point() + xlab("Drift Thickness (m)") + ylab("") + theme(text=element_text(color = "black",size = 16,angle = 0,lineheight = 0.9)) + scale_y_log10() + scale_x_log10()

DriftPlot

CondPlot <- ggplot(data = LS, aes(x=Saturated_K, y= DOC)) + theme_classic() + geom_point() + xlab("Hydraulic Conductivity (m/d)") + ylab("") + theme(text=element_text(color = "black",size = 16,angle = 0,lineheight = 0.9)) + scale_y_log10() + scale_x_log10()
CondPlot

AreaPlot <- ggplot(data = LS, aes(x=Watershed_area, y= DOC)) + theme_classic() + geom_point() + xlab("Watershed Area (km2)") + ylab("DOC (mg/L)") + theme(text=element_text(color = "black",size = 16,angle = 0,lineheight = 0.9)) + scale_y_log10() + scale_x_log10()
AreaPlot

SUVAWetland <- ggplot(data = LS, aes(x=Wetland, y= SUVA)) + theme_classic() + geom_point() + xlab("% Wetland") + ylab("SUVA (L/mg-C/m)") + theme(text=element_text(color = "black",size = 16,angle = 0,lineheight = 0.9)) + scale_y_continuous(breaks = c(2,4,6,8), lim = c(0,8.5))
SUVAWetland

SUVACond <- ggplot(data = LS, aes(x=Saturated_K, y= SUVA)) + theme_classic() + geom_point() + xlab("Hydraulic Conductivity (m/d)") + ylab("") + theme(text=element_text(color = "black",size = 16,angle = 0,lineheight = 0.9)) + scale_y_continuous(breaks = c(2,4,6,8), lim = c(0,8.5)) + scale_x_log10()
SUVACond

E2E3Wetland <- ggplot(data = LS, aes(x=Wetland, y= E2E3)) + theme_classic() + geom_point() + xlab("% Wetland") + ylab("E2:E3") + theme(text=element_text(color = "black",size = 16,angle = 0,lineheight = 0.9)) + scale_y_continuous(breaks = c(2,4,6,8), lim = c(0,8.5))
E2E3Wetland

E2E3Cond<- ggplot(data = LS, aes(x=Saturated_K, y= E2E3)) + theme_classic() + geom_point() + xlab("Hydraulic Conductivity (m/d)") + ylab("") + theme(text=element_text(color = "black",size = 16,angle = 0,lineheight = 0.9)) + scale_y_continuous(breaks = c(2,4,6,8), lim = c(0,8.5)) + scale_x_log10()
E2E3Cond

#Figure 2

Figure2 <- Fig2 <- plot_grid(AreaPlot, DriftPlot, WetlandPlot, CondPlot, SUVAWetland, SUVACond, E2E3Wetland, E2E3Cond, nrow = 4, ncol = 2, labels = c("A", "B" , "C", "D", "E", "F", "G", "H"))
Figure2

ggsave("LSMLR_Plots.pdf", plot = ggplot2::last_plot(), width = 6.5, height = 11, device = NULL, path = NULL, scale = 1, dpi=600)


#MULTIPLE LINEAR REGRESSION MODELS FOR DOC CONCENTRATION AND COMPOSITION
#BIC model selection and final selected (reduced) model output

#DOC

DOCfull<-lm(data = LS, log10DOC ~ Agriculture*log10area*log10saturatedK*log10drift 
            + Wetland* log10area*log10saturatedK*log10drift 
            + Forest*log10area*log10saturatedK*log10drift)

DOCstepBIC<-stepAIC(DOCfull, direction='backward', criterion='BIC' , k = log(199))

DOCreduced <- lm (data = LS, log10DOC ~Agriculture + log10area + log10saturatedK + log10drift + Wetland + Forest + log10area:log10saturatedK + Agriculture:log10drift + 
  log10saturatedK:log10drift + log10saturatedK:Wetland + log10area:Forest)

summary(DOCreduced)


#SUVA 

SUVAfull <- lm(data = LS, SUVA ~Agriculture*log10area*log10saturatedK*log10drift 
               + Wetland* log10area*log10saturatedK*log10drift 
               + Forest*log10area*log10saturatedK*log10drift)


SUVAstepBIC <-stepAIC(SUVAfull, direction='backward', criterion='BIC' , k = log(199))

SUVAreduced <- lm (data = LS, SUVA ~ Agriculture + log10saturatedK + log10drift + Wetland + Agriculture:log10saturatedK + Agriculture:log10drift + log10saturatedK:log10drift + log10saturatedK:Wetland + Agriculture:log10saturatedK:log10drift)

summary(SUVAreduced)


#E2E3

E2E3full <- lm(data = LS,E2E3 ~Agriculture*log10area*log10saturatedK*log10drift 
               + Wetland* log10area*log10saturatedK*log10drift 
               + Forest*log10area*log10saturatedK*log10drift)

E2E3stepBIC <- stepAIC(E2E3full, direction ='backward', criterion='BIC' , k = log(199))

E2E3reduced <- lm(data = LS, E2E3 ~ log10area + log10saturatedK + log10drift + Wetland)

summary(E2E3reduced)



#Contour DOC load figure
my_theme = theme(axis.title.x = element_text(size = 16),
                 axis.text.x = element_text(size = 16),
                 axis.title.y = element_text(size = 16),
                 axis.text.y = element_text(size = 16))

DOCcontour<-ggplot()+
  geom_point(data = LS, aes(x = Discharge, y = DOC), size = 3)+
  scale_x_log10(limits = c(200000, 2000000000), breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(limits = c(1, 35))+
  theme_classic()+xlab("Discharge (L/day)")+ylab("DOC (mg/L)")+my_theme+
  expand_limits(y=c(10, 35))+annotation_logticks(base = 10, sides = "l")+
  coord_trans()+ geom_abline(intercept = 6, slope = -1)+
  geom_abline(intercept = 7, slope = -1)+
  geom_abline(intercept = 8, slope = -1)+
  geom_abline(intercept = 9, slope = -1)+
  geom_abline(intercept = 10, slope = -1)
DOCcontour+theme(legend.text=element_text(size=14))+theme(legend.title=element_text(size=14))

my_theme = theme(axis.title.x = element_text(size = 16, face = "bold"),
                 axis.text.x = element_text(size = 16),
                 axis.title.y = element_text(size = 16, face = "bold"),
                 axis.text.y = element_text(size = 16))

summary(LS$Yield)

DOCcontour<-ggplot()+
  geom_point(data = LS, aes(x = Discharge, y = DOC,color=Yield), size = 3.5)+
  scale_x_log10(limits = c(200000, 2000000000), breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(limits = c(1, 35))+
  theme_classic()+xlab("Discharge (L/day)")+ylab("DOC (mg/L)")+
  scale_color_gradient2("Yield",
                        low = "blue",
                        mid = "grey",
                        high = "red",
                        midpoint = 5.5)+my_theme+
  expand_limits(y=c(10, 35))+annotation_logticks(base = 10, sides = "l")+
  coord_trans()+
  
  geom_abline(intercept = 6, slope = -1)+
  geom_abline(intercept = 7, slope = -1)+
  geom_abline(intercept = 8, slope = -1)+
  geom_abline(intercept = 9, slope = -1)+
  geom_abline(intercept = 10, slope = -1)

DOCcontour+theme(legend.text=element_text(size=14))+theme(legend.title=element_text(size=14))

#High SUVA values (>3) indicate high aromatic and stable structures. These structures are difficult for organisms to break down and are generally unfavorable sources of C. These are considered low-quality C sources
Low_Quality <- LS %>% filter(SUVA > 3)
sum(Low_Quality$Load)
# 47,388.2

#Low values (<3) indicate low aromatic and stable structures. These structures are easier for organisms to break down and are generally favorable sources of C. These are considered high-quality C sources
High_Quality <- LS %>% filter(SUVA <= 3)
sum(High_Quality$Load)
# 26,364.77

##Histograms for Figure 1

DOCHist <- ggplot(LS, aes(x=DOC)) + geom_histogram(binwidth = 1, fill = "black", color= "black") + theme_classic() + xlab("DOC (mg/L)") + ylab("Number of observations") + theme(text=element_text(color = "black",size = 24,angle = 0,lineheight = 0.9)) + scale_x_continuous(lim = c(0,35), expand = c(0,0)) + scale_y_continuous(lim = c(0,35), expand = c(0,0))

DOCHist
ggsave("DOCHist.png", plot = ggplot2::last_plot(), width = 6.5, height = 4.5, device = NULL, path = NULL, scale = 1, dpi=600)

SUVAHist <- ggplot(LS, aes(x=SUVA)) + geom_histogram(binwidth = 1, fill = "black", color= "black") + theme_classic() + xlab(expression(paste("SUVA (L/mg-C/m)"))) + ylab("Number of observations") + theme(text=element_text(color = "black",size = 24,angle = 0,lineheight = 0.9)) + scale_x_continuous(breaks = c(0,2,4,6,8,10),lim = c(0,11), expand = c(0,0)) + scale_y_continuous(breaks = c(0,25,50,75,100,125),lim = c(0,135), expand = c(0,0)) 

SUVAHist
ggsave("SUVAHist.png", plot = ggplot2::last_plot(), width = 6.5, height = 4.5, device = NULL, path = NULL, scale = 1, dpi=600)

E2E3Hist <- ggplot(LS, aes(x=E2E3)) + geom_histogram(binwidth = 1, fill = "black", color= "black") + theme_classic() + xlab("E2:E3") + ylab("Number of observations") + theme(text=element_text(color = "black",size = 24,angle = 0,lineheight = 0.9)) + scale_x_continuous(breaks = c(0,2,4,6,8,10), lim = c(0,11), expand = c(0,0)) + scale_y_continuous(lim = c(0,110), expand = c(0,0)) 

E2E3Hist
ggsave("E2E3st.png", plot = ggplot2::last_plot(), width = 6.5, height = 4.5, device = NULL, path = NULL, scale = 1, dpi=600)


summary(log10DOC)


OrderHist <- ggplot(LS, aes(x= Strahler_order)) + geom_histogram(binwidth = 1, fill = "black", color= "black") + theme_classic() + xlab("Strahler order") + ylab("Number of observations") + theme(text=element_text(color = "black",size = 24,angle = 0,lineheight = 0.9)) + scale_x_continuous(breaks = c(1,2,3,4,5,6,7), lim = c(0.5,7), expand = c(0,0)) + scale_y_continuous(lim = c(0,105), expand = c(0,0)) 

OrderHist
ggsave("OrderHist.png", plot = ggplot2::last_plot(), width = 6.5, height = 4.5, device = NULL, path = NULL, scale = 1, dpi=600)
