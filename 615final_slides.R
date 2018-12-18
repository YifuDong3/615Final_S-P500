library(officer)
library(magrittr)
library(tidyverse)
library(readxl)

##  pic a template you like
pres1 <- read_pptx("Default.pptx") 


##  get the layout
layout_summary(pres1)

master <- "Default Design"


## make a slide
#SLIDE1
layout_properties(x = pres1, layout = "Title Slide", master = master )

pres1 %<>%  add_slide(layout = "Title Slide", master = master) %>% 
  ph_with_text(type = "ctrTitle", str = "Stock Return and Comapny Value") %>%
  ph_with_text(type="subTitle",str="Focuing on S&P 500 Index
               ")

#SLIDE 2
pres1 %<>%  add_slide(layout = "Two Content", master = master) %>% 
  ph_with_text(type = "title", str = "My task") %>%
  ph_with_ul(type = "body", index = 1, 
             str_list = c("Exploring whether the stock return follows well the Benford's Law"),
             level_list = c(1),
             style = fp_text(font.size = 20))%>%
  ph_with_ul(type = "body", index = 2, 
             str_list = c("Exploring the relationship between stock return and book value of companies"),
             level_list = c(1),
             style = fp_text(font.size = 20))

pres1<-read_pptx("template.pptx")
master <- "Default Design"
layout_properties(x = pres1, layout = "Title Slide", master = master )

#SLIDE 3
pres1 %<>%  add_slide(layout = "Two Content", master = master) %>% 
  ph_with_text(type = "title", str = "Definition") %>%
  ph_with_ul(type = "body", index = 1, 
             str_list = c("Stock Return: Appreciation in the price plus any dividends paid, divided by the original price of the stock."),
             level_list = c(1),
             style = fp_text(font.size = 20))%>%
  ph_with_ul(type = "body", index = 2, 
             str_list = c("Book Value:The total amount a company is worth if all its assets are sold and all the liabilities are paid back."),
             level_list = c(1),
             style = fp_text(font.size = 20))

pres1<-read_pptx("template.pptx")
master <- "Default Design"
layout_properties(x = pres1, layout = "Title Slide", master = master )

  



#SLIDE 4
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Weight of components") %>%
  ph_with_img_at(src = "chart.png", width = 6,height = 4,left =3 ,top = 2,rot = 0)




#slide 5
layout_properties(x = pres1, layout = "Title and Content", master = master )
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Top50 Components weight") %>%
  ph_with_img_at(src = "11.png", width = 8,height = 5,left = 1,top = 2,rot = 0)

#SLIDE 6
layout_properties(x = pres1, layout = "Title and Content", master = master )
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Volume of S&P 500") %>%
  ph_with_img_at(src = "12.png", width = 7,height = 4,left = 1,top = 2,rot = 0)%>%
  ph_with_text(type = "ftr", str = "Key Investment Indices")

layout_properties(x = pres1, layout = "Title and Content", master = master )
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Price of component stocks") %>%
  ph_with_img_at(src = "13.png", width = 7,height = 4,left = 1,top = 2,rot = 0)%>%
  ph_with_text(type = "ftr", str = "Key Investment Indices")


layout_properties(x = pres1, layout = "Title and Content", master = master )
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Index Price and Return") %>%
  ph_with_img_at(src = "14.png", width = 7,height = 4,left = 1,top = 2,rot = 0)%>%
  ph_with_text(type = "ftr", str = "Return and Price")

layout_properties(x = pres1, layout = "Title Slide", master = master )

pres1 %<>%  add_slide(layout = "Title Slide", master = master) %>% 
  ph_with_text(type = "ctrTitle", str = "Analysis ") 

layout_properties(x = pres1, layout = "Title and Content", master = master )
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Benford: the index return follows the Benford's Law") %>%
  ph_with_img_at(src = "15.png", width = 7,height = 4,left = 1,top = 2,rot = 0)

layout_properties(x = pres1, layout = "Title and Content", master = master )
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Benford: the distribution of components return doesn't follows the Benford's Law") %>%
  ph_with_img_at(src = "16.png", width = 7,height = 4,left = 1,top = 2,rot = 0)


layout_properties(x = pres1, layout = "Title and Content", master = master )
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Book Value & Stock Return") %>%
  ph_with_img_at(src = "17.png", width = 7,height = 5,left = 1,top = 2,rot = 0)

pres1 %<>% add_slide(layout = "Title and Content", master = master)%>%
  ph_with_ul(type = "body", 
             index = 1, 
             str_list = c("The plots above shows that  company value are related with stock return. Small companies are more likely to fluctuated wildly.",
                          "But we still need to do more than data visualization to figure out the relation.
"), 
             level_list = c(1,1), style = fp_text(font.size = 32))

pres1 %<>% add_slide(layout = "Title and Content", master = master)%>%
  ph_with_text(type = "title", str = "Conclusion")%>%
  ph_with_ul(type = "body", 
             index = 1, 
             str_list = c(" Companies with small book value are more likely to have higher return, but also more likely to have lower return. In other words, small companies are more likely to fluctuated wildly. ",
                          "From the Benford Analysis, we know that S&P500 Index Return follows well the Benford's Law, which is amazing. But the distribution of the return of every individual stock doesn't follow the Benford's Law." 
                         ), 
             level_list = c(1,1), style = fp_text(font.size = 22))


layout_properties(x = pres1, layout = "Title Slide", master = master )

pres1 %<>%  add_slide(layout = "Title Slide", master = master) %>% 
  ph_with_text(type = "ctrTitle", str = "Thank you! ") 

#########
print(pres1, target = "ppt1111.pptx") 