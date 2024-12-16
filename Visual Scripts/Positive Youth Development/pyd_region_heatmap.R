# Create Positive Youth Development Heatmap and Index of Disparity chart for regions

# install packages if not already installed -----
list.of.packages <- c("dplyr","stringr","showtext","ggplot2","ggtext", "extrafont", "RPostgresSQL") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 
if(length(new.packages)) install.packages(new.packages) 

# install.packages("ggchicklet", repos = "https://cinc.rud.is")
# remotes::install_github("tidyverse/ggplot2") to get latest ggplot version

# Prep and Set Up 
#### Loading Libraries ####
library(dplyr)
library(stringr)
library(showtext)
library(ggplot2)
library(ggtext)
library(extrafont)
library(RPostgreSQL)
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

# colors for heatmap (not BV colors)
light_gray <- "#F2F2F2"
light_pink <- "#F46094"
lighter_pink <- "#F776BC"
lighest_pink <- "#FA8EDD"
lightest_pink2 <- "#FDA5F5"
darker_pink <- "#D4453E"
darkest_blue <- "#1a0b6d"
dark_blue <- "#220F90"
dim_blue <- "#4D39BF"
dimmer_blue <- "#7161CC"
dimmest_blue <- "#B8B0E5"
lavender <- "#DCD8F2"


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


#### Loading Database from PgAdmin/Postgres #### 

#Reference: W:/Project/OSI/Bold Vision/BV 2023/R/boldvision_22_23/bv_visuals_functions.R

source("W:\\RDA Team\\R\\credentials_source.R")
conBV <- connect_to_db("bold_vision")

#load in domains
yp_domain <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.yp_region_domain")
si_domain <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.si_region_domain")
pyd_domain <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.pyd_region_domain")
hbe_domain <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.hbe_region_domain")

#disconnect
dbDisconnect(conBV)

#source in function that rounds the corners of the heatmap squares
source("https://github.com/catalystcalifornia/boldvision_2023/blob/main/Visual%20Scripts/rounded_corner_function.R")
# heatmap ----

#remove total and bipoc columns
df_no_total <-pyd_domain 

heatmap <- ggplot(df_no_total, aes(x=spa, y=reorder(category,index_of_disparity), fill= avg_rank, height=0.55, width=0.55)) + #height and width change the size of the squares #originally height=0.6, width = 0.6
  geom_rtile(alpha=0.83, radius = unit(15, "pt")) + #line that makes the chart a heatmap 
  scale_fill_gradientn(colors = c(darkest_blue,blue,dim_blue, dimmer_blue, dimmest_blue, lighest_pink, lighter_pink, light_pink, dark_pink, darker_pink), na.value=light_gray, 
                       name="", labels=c('', '', '', '', '')) + 
  labs(title = "<b>Mapping <i>regional disparities</i> across positive youth development systems in LA County</b>", 
       caption = "The color of the square shows how different youth outcomes are for each indicator. <b><span style='font-size:14pt'>Regions with <span style='color:#1a0b6d;'>darker blue squares</span> have better outcomes and smaller differences from<br>the region with the best rate. Regions with <span style='color:#D4453E;'>darker pink squares</span> have greater disparities from the region with the best rate.</span></b>
       <br><br>See boldvisionla.org for definitions and methodology.")+  
  scale_x_discrete(labels = function(spa) str_wrap(spa, width = 10), position = "top") +            # wrap long labels
  scale_y_discrete(labels = function(category) str_wrap(category, width = 20)) + # wrap long labels
  geom_hline(yintercept = seq(0.5, length(df_no_total$category), by = 1), color=gray, linewidth=.5, alpha=.5)+ # set horizontal lines between groups on x axis
  theme_void()+  
  xlab("") + 
  ylab("") 
heatmap

heatmap <- heatmap + theme(
  plot.background = element_rect(fill = light_gray),
  panel.grid.major = element_blank(),
  panel.grid.minor =  element_blank(),
  panel.border = element_blank(),
  panel.background = element_rect(fill=light_gray, color = light_gray),
  axis.title.x = element_blank(),
  axis.title.y =element_blank(),
  axis.text.x = element_text(size = 20, family= font_axis_label, lineheight = 1, hjust=0.5),
  axis.text.y = element_text(size = 20, family= font_axis_label, lineheight = 1, hjust=0),
  axis.ticks = element_blank(),
  plot.title= element_markdown(family = font_title, size = 26, hjust = 0, lineheight = 0.4, margin=margin(0,0,30,0)),
  plot.title.position = "plot", 
  plot.caption = element_markdown(family = font_title, size = 15, hjust = 0, lineheight = 0.4), text=element_text(family=font_caption),
  plot.caption.position = "plot",
  legend.position = c(-0.12, 1.04), 
  legend.direction = "horizontal",
  legend.key.width = unit(1.5, "cm"),
  legend.text = element_text(size=11),
  legend.title = element_blank(),
  plot.margin = unit(c(1, 1, 1, 1), "inches"))
heatmap 

# ggsave(plot=heatmap,
#        file="W:/Project/OSI/Bold Vision/BV 2023/Deliverables/Positive Youth Development/R Draft Files/pyd_region_heatmap_r.pdf",
#        units = c("in"),  width = 18, height=10, device = cairo_pdf)#originally width = 18, height=10


#index of disparity chart ----

id_data <- pyd_domain %>% select(category, index_of_disparity) %>% distinct() %>% filter(!is.na(index_of_disparity))

id_barchart <- ggplot(id_data, aes(x=reorder(category,index_of_disparity), y=index_of_disparity, height=0.7, width=0.7)) +
  geom_chicklet(radius = grid:: unit(3, "mm"), fill=blue) + #rounded corners bar chart
  labs(title="<b>Youth face the highest regional disparities in asthma rates</b>", 
       subtitle = "Average disparity between regions by indicator",
       caption="The length of the bar shows the average disparity between regions for each indicator. <br>The longer the bar, the greater the disparity.<br>See boldvisionla.org for definitions and methodology.") + 
  geom_text(aes(label = paste0(index_of_disparity,"%")), family = font_bar_label, hjust = 1.5, size = 6.5, color="White") +   # format data labels, adjust hjust to avoid overlap w/ total line # , hjust = 1.7
  scale_x_discrete(labels = function(category) str_wrap(category, width = 20)) +            # wrap long labels
  theme_void()+
  xlab("") +
  ylab("") +
  coord_flip()

id_barchart <- id_barchart + theme(
  plot.background = element_rect(fill = light_gray),
  panel.grid.major = element_blank(),
  panel.grid.minor =  element_blank(),
  panel.border = element_blank(),
  panel.background = element_rect(fill=light_gray, color = light_gray),
  axis.title.x = element_blank(),
  axis.title.y =element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 16, family= font_axis_label, lineheight = 1, hjust=0),
  axis.ticks = element_blank(),
  plot.title= element_markdown(family = font_title, size = 24, hjust = 0),
  plot.caption = element_markdown(family = font_title, size = 11, hjust = 0, lineheight = 0.4), 
  plot.subtitle= element_markdown(family = font_title, size = 18, hjust = 0, lineheight = 0.4, margin=margin(30,0,0,0)), 
  plot.caption.position = "plot",
  plot.title.position = "plot",
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "inches"))
id_barchart

# ggsave(plot=id_barchart,
#        file="W:/Project/OSI/Bold Vision/BV 2023/Deliverables/Positive Youth Development/R Draft Files/pyd_region_id_barchart_r.pdf",
#        units = c("in"),  width = 12, height=8, device = cairo_pdf)