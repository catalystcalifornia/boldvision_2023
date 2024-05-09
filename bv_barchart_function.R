####User-Defined R Functions to create Visuals for the Bold vision Project
#Author: Maria Khan

# The following file aims to serve as a visuals template for the 2023 Bold Vision Indicators. Report is set to release by end of Q1 of 2024. 


#### Loading Libraries ####
library(extrafont)
library(tidyverse)
library(here)
library(dplyr)
library(data.table)
library(sf)
library(ggplot2)
library(RPostgreSQL)
library(formattable)
library(svglite)
library(stringr)
library(tidyr)
library(showtext)
library(scales)
library(kableExtra)
library(flextable)
library(ggchicklet)

#### Bold Vision Style Guide ####

##Colors

gray <- "#D6D7D6"
pink <- "#F75EC1"
dark_pink <- "#EF4A66"
orange <- "#F57E20"
yellow <- "#FFBF00"
light_green <- "#00A75A"
dark_green <- "#00864A"
blue  <- "#2A12B2"


## FONTS ## 

font_add(family = "Manifold CF", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Manifold/Fonts/manifoldcf-heavy.otf")
font_add(family = "HelveticaNeueLTStdMdCn", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Helvetica Neue LT Std/HelveticaNeueLTStd-MdCn.otf")
font_add(family = "HelveticaNeueLTStdHvCn", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Helvetica Neue LT Std/HelveticaNeueLTStd-HvCn.otf")
font_add(family = "HelveticaNeueLTStdMdCnO", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Helvetica Neue LT Std/HelveticaNeueLTStd-MdCnO.otf")


# font_import()
loadfonts(device = "win")
windowsFonts()
showtext_auto()

# define fonts in chart
font_title <- "Manifold CF"
font_subtitle <- "Manifold CF"
font_caption <- "HelveticaNeueLTStdMdCn"
font_bar_label <- "HelveticaNeueLTStdHvCn"
font_axis_label <- "HelveticaNeueLTStdMdCn"


#### Function: Vertical Bar Chart by Subgroup (RACE/ETHNICITY) ####
fx_barchart_subgroup <- function(
    df,
    domain, #you must input the domain exactly a the folders are named so "Systems Impact" or "Positive Youth Development" for example
    indicator, 
    title,  #we are looking for a title that is systems-focused
    subtitle, #explanation of what the we are looking at
    caption_datasource,
    caption_racenote, #only input the full names for the groups that are in acronyms
    caption_indicator_def, #define the indicator
    data_unit #define the unit of the data and remember to use quotations (i.e. "%" or "per 1k")
) {
  
  
  subgroup_visual <- ggplot(
    data = df,
    aes(x=reorder(race, rate), y=rate)) +
    geom_chicklet(radius = grid:: unit(3, "mm"), width = 0.85, fill = ifelse(domain == 'Positive Youth Development', yellow, 
                                                                             ifelse(domain == 'Healthy Built Environment', dark_green,
                                                                                    ifelse(domain == 'Youth Power', pink,
                                                                                           ifelse(domain == 'Systems Impact', orange, gray))))) +
    # bar style
    expand_limits(y = max(df_subgroup$rate) + .09) +
    scale_x_discrete(labels  = function(race_label_short) str_wrap(df$race_label_short, width = 15))+
    # scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    
    # vertical line for Total 
    geom_hline(yintercept =subset(df_subgroup, race == "total")$rate,
               linetype = "dotted", color = "black",  size = 1) +
    # size = 0.75) +
    
    # label for vertical Total line
    annotate(geom = "text",
             x = 0.75,
             y = subset(df_subgroup, race == "total")$rate,
             label = paste0("Average for All Youth: ", round(subset(df_subgroup, race == "total")$rate,
                                                             digits = 1),data_unit),
             hjust =0, vjust = -1, fontface = "bold",
             color = "black", size = 4) +
    #axis labels
    xlab("") +
    ylab("") +
    # bar labels
    geom_text(data = df,
              aes(label = paste0(round(rate, digits = 1),data_unit)),
              size = 4,
              stat="identity", colour = "white",
              position = position_dodge(width = 1), 
              vjust = 2.25 , 
              # hjust= 1.15,
              fontface = "bold") +
    labs(
      title = paste(str_wrap(title, whitespace_only = TRUE, width = 60), collapse = "\n"),
      subtitle = paste(str_wrap(subtitle,whitespace_only = TRUE, width = 75), collapse = "\n"),
      caption =  str_wrap(paste0("Indicator: ",caption_indicator_def, 
                                 " Race Note: ", caption_racenote, ".",
                                 " Data Source: ", caption_datasource), 155)) +
    #theme/aesthetics
    theme_minimal() +
    theme(legend.title = element_blank(), # no legend--modify if necessary
          # define style for axis text
          axis.text.y = element_blank(), 
          axis.text.x = element_text(size = 9, 
                                     colour = "black", family= font_axis_label,
                                     face = "bold"),
          axis.title.y = element_text(size = 9, margin = margin(10, 5, 0, 0),
                                      colour = "black", family = font_axis_label),
          
          # define style for title and caption (for now no title bc that might be done by the designer?)
          plot.caption = 
            element_text(hjust = 0.0, size = 9, colour = "black", family = font_caption),
          plot.title = 
            element_text(hjust = 0.0, size = 21, colour = "black", family = font_title,
                         face = "bold"), 
          plot.subtitle = 
            element_text(hjust = 0.0, size = 12, colour = "black", family = font_title), 
          axis.ticks = element_blank(),
          # grid line style
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) 
  
  
  subgroup_visual
  
  ##Saving Visual ##
  ggsave(plot=subgroup_visual, 
         file=paste0("W:/Project/OSI/Bold Vision/BV 2023/Deliverables/", domain, "/",
                     indicator,"_subgroup", ".jpg"),
         units = c("in"),  width = 8, height = 5.5)
  
  ggsave(plot=subgroup_visual, 
         file=paste0("W:/Project/OSI/Bold Vision/BV 2023/Deliverables/", domain, "/",
                     indicator,"_subgroup", ".svg"),
         units = c("in"),  width = 8, height = 5.5)
  
  ggsave(plot=subgroup_visual,
         file=paste0("W:/Project/OSI/Bold Vision/BV 2023/Deliverables/", domain, "/",
                     indicator,"_subgroup", ".pdf"),
         units = c("in"),  width = 8, height = 5.5, device = cairo_pdf)
  
}
