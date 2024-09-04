# Create a summary heatmap visual across all domains visualizing average regional and racial disparities on same table

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
library(data.table)

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
region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.domain_region_id")
region<-region%>%mutate(rank=rank(domain_id)) # rank by ID for region IDs
subgroup<- dbGetQuery(conBV, "SELECT * FROM bv_2023.domain_subgroup_id")
subgroup<-subgroup%>%mutate(rank=rank(domain_id)) # rank by ID for subgroup IDs


#disconnect
dbDisconnect(conBV)

# bind
df<-rbind(region,subgroup)%>%
  mutate(level=ifelse(level=="subgroup","Disparities Between Races","Disparites Between Regions")) # clean up labels for heatmap

#rescale ranks for function to work
rescaled <- df %>% select(level,domain,domain_id) %>% 
  group_by(level) %>% mutate(rank = rank(domain_id)) %>% select(-domain_id)
rescaled <- melt(rescaled)
rescaled <- data.table(rescaled)
rescaled[,avg_rank:=value/mean(value),by=.(level)]
rescaled <- rescaled %>% pivot_wider(names_from = variable, values_from = value) %>% as.data.frame()

df<-rescaled%>%right_join(df)

#source in function that rounds the corners of the heatmap squares
source("W:/Project/OSI/Bold Vision/BV 2023/R/rounded_corner_function.R")

# heatmap ----


heatmap <- ggplot(df, aes(x=domain, y=level, fill=avg_rank, height=0.6, width=0.5)) + #height and width change the size of the squares #originally height=0.6, width = 0.6
  geom_rtile(alpha=083, radius = unit(15, "pt")) + #line that makes the chart a heatmap 
  scale_fill_gradientn(colors = c(darkest_blue,blue,dim_blue, dimmer_blue, dimmest_blue, lighest_pink, lighter_pink, light_pink, dark_pink, darker_pink), na.value=light_gray, 
                       name="", labels=c('', '', '', '','','', '')) + 
  labs(title = "<b>Mapping <i>average racial and regional disparities</i> across Bold Vision Domains</b>", 
       caption = "The color of the square shows how great racial and regional disparities are for each domain. <b><span style='font-size:14pt'>Domains with <span style='color:#1a0b6d;'>darker blue squares</span> on average have lower regional or racial disparities <br>compared to other domains. Domains with <span style='color:#D4453E;'>darker pink squares</span> have greater racial or regional disparities compared to other domains.</span></b>
       <br><br>See boldvisionla.org for definitions and methodology.")+  
  scale_x_discrete(labels = function(domain) str_wrap(domain, width = 10), position = "top") +            # wrap long labels
  scale_y_discrete(labels = function(level) str_wrap(level, width = 20)) + # wrap long labels
  geom_hline(yintercept = seq(0.5, length(df$level), by = 1), color=gray, linewidth=.5, alpha=.5)+ # set horizontal lines between groups on x axis
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
  plot.title= element_markdown(family = font_title,size = 26, hjust = 0, lineheight = 0.4, margin=margin(0,0,30,0)),
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
#        file="W:/Project/OSI/Bold Vision/BV 2023/Deliverables/BV_Domain_Heatmap_r.pdf",
#        units = c("in"),  width = 18, height=10, device = cairo_pdf)#originally width = 18, height=10


