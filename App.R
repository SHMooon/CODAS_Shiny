
#Put names
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#alle variable in einem Sidebar panel links with scroll bar und Ergebnisse rechts fixed. See Kosten.!! 
#Change color of scroll bar -> sichtbar. 
#https://www.r-bloggers.com/2022/06/scrollbar-for-the-shiny-sidebar/
#put explanation of graphics 
#scroll color 




#change the presented grapha: legend, labels... 


library(shiny)
library(bslib)
library(shinythemes)
library(ggplot2)
library(shinyjs)
library(dplyr)
library(scales)


# Model of the school garden (see Index.RMD for the explaination and posthoc)
library(decisionSupport)

# creat NIFAM folder in Server
# dir.create("user-states/NIFAM/", recursive = T)


# Define UI for application that draws a histogram
#UI####
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* For Chrome, Safari, and Edge */
      ::-webkit-scrollbar {
        width: 10px;
      }
      ::-webkit-scrollbar-track {
        background: #F1F1F1;
      }
      ::-webkit-scrollbar-thumb {
        background: #2d7547;
      }
      ::-webkit-scrollbar-thumb:hover {
        background: darkred;
      }
      /* For Firefox */
      body {
        scrollbar-width: thin;
        scrollbar-color: #2d7547; /* different colors : red #F1F1F1 */
      }
      
      /* close and open Sidebar */
      #toggleSidebar {
        background-color: #2d7547;  /* dark green Default button color */
        color: white;
      } 
      #toggleSidebar.closed {
        background-color: #636af7;  /* Color when sidebar is hidden */
      }
      
      /* Result as a sticky panel so that It shows when participants are answering */
      .main-panel {
       position: sticky;
      top: 0;
      z-index: 1000;
      background-color: #f4f4f4;
      padding: 15px;
      box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1); /* Optional: Adds shadow to give a floating effect */
    }
    }
    .content {
      margin-right: 60px; /* Adjust to give space for the sticky panel */
    }
    .app-header {
      background-color: #ffffff; /* Choose your color */
      padding: 10px; /* Optional: Adds some padding around the title */
      border-radius: 5px; /* Optional: Rounds the corners */
    }
    .tabPanel {
      background-color: #EFF4EF; /* Choose your color */
      padding: 10px; /* Optional: Adds some padding around the title */
      border-radius: 5px; /* Optional: Rounds the corners */
    }
    
    "))
  ),
  
  #UI Start####
  # Application title
  titlePanel(
    div(class = "app-header",
        # Logos column
        fluidRow(column(width = 2,
                        align = "right",
                        tags$a(#href = "https://agroreforest.eu/", # Add URL here
                          tags$img(src = "Logo/NIFAM.png", height = "90px"))),
                 column(width = 7,
                        align = "center",
                        h2(class = "app-title",
                           "Holistic Decision Analysis for NIFAM")),
                 column(width = 3,
                        align = "left",
                        tags$a(href = "https://www.gartenbauwissenschaft.uni-bonn.de/", # Add URL here
                               tags$img(src = "Logo/logo-mix-nobg-en.png", height = "90px"))),
                 
                 windowTitle = "MyPage")
    )),
  #theme = bs_theme(version = 5, bootswatch = "minty"), 
  # tabsetPanel(      
  # tabPanel("Public", h1("Public School"),
  #          h2("HortiPrimed")),
  # tabPanel("Seedling",h1("Tomatenjungepflanzeproduktion")
  # ),
  
  tabsetPanel(
  tabPanel("2.Version", class= "tabPanel",
           
           fluidRow(
           column(12, h5("School garden Version: 14.Mar.25"),
                  h5 ("Authors: Sanghyo Moon, Cory Whitney, & Luu Thi Thu Giang"),
                  align = "right"      )
           ),
           fluidRow(width = 12,
                    uiOutput("login_status"),
                    uiOutput("project_info"),
                    p("updating")),
           fluidRow(
             column(4, textInput("Date_1", "Ngày (tùy chọn)")),  # Each column takes up 4/12 of the width
             column(4, textInput("Organization_1", "Tổ chức của bạn (tùy chọn)")),
             column(4, textInput("Name_1", "Tên của bạn (tùy chọn)"))
           ),
           fluidRow(
             sidebarPanel(width = 12,
                          fluidRow( h4( strong('Cách sử dụng cái này NIFAM-Shiny:')),
                                    h5 (HTML("<b>1.</b> Vui lòng cung cấp 'dữ liệu đầu vào' của bạn")),
                                    h5 (HTML("<b>2.</b> Khi bạn hoàn thành, nhấp vào  <b> Tải xuống</b>.")),
                                    h5 (HTML("<b>3.</b> Gửi tập tin của bạn đến địa chỉ email '<b></b>'.
                                      Nếu bạn có bất kỳ câu hỏi nào, vui lòng liên hệ với chúng tôi qua email."))
                                    
                          ))),
           
           
           fluidRow(
             
             column(7, class = "content",
                    br(),
                    
                    tags$div(id = "0_1", h4(strong('Giới thiệu về mô hình vườn trường'))),
                    p("Mô hình hỗ trợ ra quyết định cho can thiệp dinh dưỡng bằng vườn trường tại khu vực đô thị Hà Nội được xây dựng nhằm mục đích giúp Ban Giám Hiệu 
                    của các trường học ở đưa ra quyết định có nên vận hành vườn trường trong khuôn viên trường học hay không và có tích hợp vườn trường này vào phương 
                    pháp giảng dạy STEM hay không. Theo đó, có 3 phương án đầu tư được đưa ra bao gồm: "),
                  
                    p(HTML("<b>1. Vuờn:</b> Xây dựng vườn trường trên khuôn viên sẵn có của trường. 
                    Vườn được xây dựng thêm một cách không chính thức, sử dụng thụ động cho mục đích giáo dục ")),
                    p(HTML("<b>2.	Vườn-STEM:</b> Vườn trường trở thành một phần chính thức của chương trình giáo dục STEM:
                    Được sử dụng cho mục đích giáo dục, kèm theo các chi phí đào tạo và chi phí khác, nhưng bù đắp bằng việc giảm chi phí giáo dục và hoạt động ngoại khóa.
                    ")),
                    p(HTML("<b>3.	Baseline:</b> Duy trì việc sử dụng đất như hiện tại, không thay đổi gì:
                    Diện tích đất được tiếp tục sử dụng cho các mục đích khác như khu vui chơi thay vì làm vườn trường.")),
                    p ("Mô hình lý thuyết của quyết định này được xây dựng với sự tham gia một nhóm chuyên gia và tổ chức phi chính phủ CODAS. 
                    Sau đó, một mô hình toán học đã được phát triển bởi nhóm nghiên cứu của Đại học Bonn để mô phỏng kết quả của 10 năm hoạt 
                    động của vườn trường dựa vào giá trị của các biến đầu vào được cung cấp bởi chuyên gia. Bạn có thể nhìn xem kết quả tác động của vườn trường ở phía bên phải."),
                    br(),
                    
                    tags$div(h4(strong("Cung cấp số liệu đầu vào cho mô hình"))),
                    p("Mô hình lý thuyết cho thấy tất cả các biến số liên quan và ảnh hưởng đến quyết định và mỗi biến số cần 
                      được đánh giá định lượng bằng cách đưa ra các khoảng giá trị hợp lý. 
                      Vui lòng cung cấp các khoảng giá trị mà anh chị chắc chắn 90% là giá trị chính xác sẽ nằm trong khoảng giá trị này, 
                      giống như khi bạn đưa ra khoảng giá trị khi tham gia tập huấn hiệu chỉnh trong dự án NIFAM."),
                    br(),
                    
                    tags$div(h4(strong("Ai trả lời câu hỏi nào?"))),
                    p ("Mô hình có nhiều biến đầu vào thuộc 15 lĩnh vực khác nhau như trong mục lục. Sẽ rất khó để mỗi chuyên gia trả 
                         lời tất cả các câu hỏi liên quan. Do vậy, chuyên gia có thể trả lời một số câu hỏi khác nhau phù hợp với chuyên môn. 
                         Ví dụ, những câu hỏi chung về mô hình như diện tích vườn thì nên hỏi người ra quyết định, cố vấn của người ra quyết 
                         định. Những câu hỏi về chi phí đầu tư xây dựng vườn thì nên được hỏi các bên cung cấp dịch vụ, các tổ chức có kinh 
                         nghiệm xây dựng vườn trường (như CODAS). Ngoài ra, có một số câu hỏi sẽ cần thông tin đầu vào từ câu hỏi khác mới trả 
                         lời được. Ví dụ như câu hỏi về tác động lên việc giảm chi phí của căng tin mua thực phẩm bên ngoài do có nguồn cung từ 
                         vườn trường. Câu hỏi này yêu cầu người trả lời phải biết được diện tích vườn là bao nhiêu, vườn cung cấp những loại thực 
                         phẩm nào (rau, thịt). Do vậy, người trả lời cần biết thông tin tổng thể về khu vườn trước khi trả lời câu hỏi về tác động. "),
                    br(),
                    tags$div(id = "0", h4(strong("Mục lục"))),
                  
                    tags$a(href = "#1", "1. Các thông số chung của mô hình", style = "display: block; margin-bottom: 5px;"),
                    tags$a(href = "#2", "2. Các thông số về khu vườn", style = "display: block; margin-bottom: 5px;"),
                    tags$a(href = "#3", "3. Các thông số về quản lý vườn", style = "display: block; margin-bottom: 5px;"),
                    tags$a(href = "#4", "4. Chi phí đầu tư xây dựng vườn", style = "display: block; margin-bottom: 5px;"),
                    tags$a(href = "#5", "5. Chi phí duy trì hoạt động vườn trường", style = "display: block; margin-bottom: 5px;"),
                    tags$a(href = "#6", "6. Căng tin", style = "display: block; margin-bottom: 5px;"),
                    tags$a(href = "#7", "7. Tiết kiệm chi phí khác nhờ có vườn trường", style = "display: block; margin-bottom: 5px;"),
                    tags$a(href = "#8", "8. Ước tính tiềm năng thu hút đầu tư", style = "display: block; margin-bottom: 5px;"),
                    tags$a(href = "#9", "9. Tác động của vườn trường tới sức khỏe, học tập và hoạt động xã hội của trẻ ", style = "display: block; margin-bottom: 5px;"),
                    tags$a(href = "#10", "10. Tác động lên sức khỏe tinh thần", style = "display: block; margin-bottom: 5px;"),
                    tags$a(href = "#11", "11. Kịch bản có và không có phương pháp giảng dạy STEM", style = "display: block; margin-bottom: 5px;"),
                    tags$a(href = "#12", "12. Giá trị môi trường ", style = "display: block; margin-bottom: 5px;"),
                    tags$a(href = "#13", "13. Giá trị các sự kiện gắn với vườn trường", style = "display: block; margin-bottom: 5px;"),
                    tags$a(href = "#14", "14. Thông số liên quan đến sử dụng đất", style = "display: block; margin-bottom: 5px;"),
                    tags$a(href = "#15", "15. Các biến số khác", style = "display: block; margin-bottom: 5px;"),
                    tags$a(href = "#16", "Cải thiện mô hình", style = "display: block; margin-bottom: 5px;"),
                    br(),
                    
                    
                    #1. Introduction on data input provision####
                    
                    tags$div(id = "1", h4(strong('1. Các thông số chung của mô hình'))),
                    useShinyjs(),  # Initialize shinyjs
                    
                    sidebarPanel(  id = "sidebar",  # Assign an ID to the sidebar
                                   width = 12,
                                   style = "height: 60vh; overflow-y: auto;", #height in % 
                                   
                                   h5(HTML("<b>1-1. Number of years for garden simulation:</b>")),
                                   p("Vui lòng đưa ra thời gian tính theo năm mà bạn muốn kết quả của dự án vườn trường được mô phỏng",
                                     style = "padding-left: 20px;"),
                                   
                                   sliderInput("number_of_years_c",
                                               NULL,
                                               min = 1,
                                               max = 10,
                                               value = 5,
                                               step = 1),
                                   h5(HTML("<b>1-2. Discounting factor/ base (lending) rate/ prime (lending) rate:</b>")),
                                   p("Khoảng giá trị tối thiểu và tối đa của lãi suất cho vay cơ bản ở Việt Nam mà bạn ước tính trong 10 năm tới"
                                     ,style = "padding-left: 20px;"),
                                   sliderInput("discount_rate",
                                               NULL,
                                               min = 1,
                                               max = 15,
                                               value = c(2, 8),
                                               step = 1), 
                                   h5(HTML("<b>1-3. Coefficient of variation for our school garden intervention (%):</b>")),
                                   sliderInput("CV_value_t",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(40, 60),
                                               step = 1), 
                                   h5(HTML("<b>1-4. Inflation rate (%):</b>")),
                                   p("Hãy ước tính tỷ lệ lạm phát ở Việt Nam trong 10 năm tới, theo % tối thiểu và % tối đa." 
                                     ,style = "padding-left: 20px;"),
                                   sliderInput("inflation_rate",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(3, 15),
                                               step = 1),
                                   tags$a(href = "#0", "Quay lại Mục lục", 
                                          style = "display: inline-block; margin-top: 20px; padding: 5px 10px; 
                                                  background-color: #3283CD; float: right; color: white; 
                                                  border-radius: 5px; text-decoration: none;")
                                   
                    
                    ),
                    
                   
                    #2. General estimation####
                    tags$div(id = "2", h4(strong('2. Các thông số về khu vườn'))),
                    
                    sidebarPanel(width = 12,
                                 style = "height: 60vh; overflow-y: auto;", #height in %
                                 
                                 h5(HTML("<b>2-1. Size of school garden in (m2):</b>")),
                                 p("Hãy ước tính quy mô diện tích (tính bằng m2) mà một trường học có thể phân bổ cho dự án vườn trường."
                                   ,style = "padding-left: 20px;"),
                                 sliderInput("size_of_garden",
                                             NULL,
                                             min = 1,
                                             max = 500,
                                             value = c(5, 100),
                                             step = 1),
                                 h5(HTML("<b>2-2. Cut off value for where the garden becomes more expensive:</b>")),
                                 p("Vui lòng ước tính ngưỡng diện tích mà từ mốc đó chi phí đầu tư cho vườn trường có thể trở nên 
                                   đắt hơn đáng kể vì có thể cần phải có các hình thức đầu tư khác (ví dụ: tăng cường cơ giới hóa 
                                   hoặc tăng cường chăn nuôi (gia súc lớn thay vì gia súc nhỏ và gia cầm)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("expensive_garden_size",
                                             NULL,
                                             min = 1,
                                             max = 200,
                                             value = c(80, 100),
                                             step = 1),
                                 h5(HTML("<b>2-3. Percentage more expensive we expect the garden to be if it is beyond the expensive_garden_size:</b>")),
                                 p("Hãy ước tính chi phí đầu tư sẽ tăng như thế nào khi vượt quá quy mô diện tích này (tăng ...%)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("cost_increase_expensive_garden_size",
                                             NULL,
                                             min = 0.1,
                                             max = 20,
                                             value = c(0.4, 2.6),
                                             step = 0.1),
                                 tags$a(href = "#0", "Quay lại Mục lục", 
                                        style = "display: inline-block; margin-top: 20px; padding: 5px 10px; background-color: #3283CD; 
                           float: right; color: white; border-radius: 5px; text-decoration: none;")
                                 
                    ),
                    
                    p(),
                    #3. General estimation of####
                    tags$div(id = "3", h4(strong('3. Các thông số về quản lý vườn'))),
                   
                    sidebarPanel(width = 12,
                                 style = "height: 60vh; overflow-y: auto;", #height in %
                                 
                                 h5(HTML("<b>3-1. Chance of student engagement (%):</b>")),
                                 p("Theo anh/chị, có bao nhiêu % khả năng là học sinh sẽ thích và tham gia tích cực vào dự án vườn trường?",
                                   style = "padding-left: 20px;"),
                                 sliderInput("if_students_like_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(50, 80),
                                             step = 1),
                                 h5(HTML("<b>3-2. Chance of parents support / effectiveness (%):</b>")),
                                 p("Theo anh/chị, có bao nhiêu % khả năng phụ huynh học sinh sẽ ủng hộ và hỗ trợ cho dự án vườn trường",
                                   style = "padding-left: 20px;"),
                                 sliderInput("if_parents_like_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(50, 80),
                                             step = 1),
                                 h5(HTML("<b>3-3. Chance of community support (%):</b>")),
                                 p("Theo anh/chị, có bao nhiêu % khả năng là cộng đồng, người dân ở khu vực lân cận sẽ có thái độ tích cực với dự án vườn trường?",
                                   style = "padding-left: 20px;"),
                                 sliderInput("if_community_likes_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(50, 80),
                                             step = 1),
                                 h5(HTML("<b>3-4. Chance of effective garden management (%):</b>")),
                                 p("Theo anh/chị, có bao nhiêu % khả năng là vườn trường sẽ được quản lý một cách hiệu quả",
                                   style = "padding-left: 20px;"),
                                 sliderInput("if_effective_manage_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(50, 80),
                                             step = 1),
                                 h5(HTML("<b>3-4. Chance of sufficient yield from garden (%):</b>")),
                                 p("Bạn đánh giá thế nào về khả năng vườn trường cung cấp đủ năng suất cho trường sử dụng (%)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("if_garden_yield_enough_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(50, 80),
                                             step = 1),
                                 h5(HTML("<b>3-5. Chance of healthy food from garden (%):</b>")),
                                 p("Theo anh/chị, có bao nhiêu % khả năng vườn trường sẽ cung cấp được thực phẩm lành (hơn)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("if_garden_healthy_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(50, 80),
                                             step = 1),
                                 h5(HTML("<b>3-6. Chance of teacher engagement (%):</b>")),
                                 p("Bạn đánh giá thế nào về khả năng giáo viên thích dự án vườn trường và tham gia tích cực vào nó (tính bằng %)?",
                                   style = "padding-left: 20px;"),
                                 sliderInput("if_teachers_like_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(50, 80),
                                             step = 1),
                                 h5(HTML("<b>3-7. Chance of high education quality / effectiveness (%):</b>")),
                                 p("Bạn đánh giá như thế nào về khả năng những bài học trong vườn trường của trẻ em có hiệu quả (tính bằng %)?",
                                   style = "padding-left: 20px;"),
                                 sliderInput("if_effective_teaching_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(50, 80),
                                             step = 1),
                                 h5(HTML("<b>3-8. Chance of effective training for teachers (%):</b>")),
                                 p("Bạn đánh giá thế nào về khả năng giáo viên sẽ được đào tạo hiệu quả về làm vườn 
                                   ở trường và có thể truyền tải thông điệp một cách hiệu quả đến trẻ em (%)?",
                                   style = "padding-left: 20px;"),
                                 sliderInput("if_effective_training_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(50, 80),
                                             step = 1),
                                 h5(HTML("<b>3-9. Chance of garden having ecologically valuable green space (%):</b>")),
                                 p("Bạn đánh giá thế nào về khả năng khu vườn sẽ trở thành nơi có giá trị sinh thái, 
                                   nơi cư trú của thực vật và động vật (tính bằng %)?",
                                   style = "padding-left: 20px;"),
                                 sliderInput("if_offer_green_space_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(50, 80),
                                             step = 1),
                                 h5(HTML("<b>3-10. Chance of garden reducing polution (%):</b>")),
                                 p("Bạn đánh giá thế nào về khả năng sự tồn tại của khu vườn sẽ làm giảm 
                                   ô nhiễm so với cách sử dụng không gian hiện tại (tính bằng %)?",
                                   style = "padding-left: 20px;"),
                                 sliderInput("if_reduce_pollution_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(50, 80),
                                             step = 1),
                                 h5(HTML("<b>3-11. Chance of biophysical not damaging (i.e. weather) (%):</b>")),
                                 p("Khả năng môi trường sẽ thuận lợi cho khu vườn, tức là không gây ra bất kỳ thiệt hại nào 
                                   (vườn sẽ không bị ảnh hưởng bởi nắng nóng, sương giá, lũ lụt, hạn hán) (tính %)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("if_biophysical_good_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(50, 80),
                                             step = 1),
                                 tags$a(href = "#0", "Quay lại Mục lục", 
                                        style = "display: inline-block; margin-top: 20px; padding: 5px 10px; background-color: #3283CD; 
                           float: right; color: white; border-radius: 5px; text-decoration: none;")
                                 
                    ),
                    #4.  The investment costs####
                    tags$div(id = "4", h4(strong('4. Chi phí đầu tư xây dựng vườn'))),
                    
                    sidebarPanel(width = 12,
                                 style = "height: 60vh; overflow-y: auto;", #height in %
                                 
                                 h5(HTML("<b>4-1. Costs of design team consultant (million VND):</b>")),
                                 p("Anh/chị vui lòng đưa ra ước tính chi phí cho việc quy hoạch và thiết kế khu vườn 
                                   (bố trí lô đất, lập kế hoạch luân canh cây trồng, v.v.) (triệu đồng).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("garden_designing_costs",
                                             NULL,
                                             min = 1,
                                             max = 50,
                                             value = c(10, 20),
                                             step = 1),
                                 h5(HTML("<b>4-2. Costs of construction for setting up garden (million VND):</b>")),
                                 p("Vui lòng cho biết khoảng chi phí dự kiến ​​cho việc thiết lập khu vườn (đầu tư ban đầu) (triệu đồng).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("construction_cost",
                                             NULL,
                                             min = 1,
                                             max = 200,
                                             value = c(5.5, 50.5),
                                             step = 0.5),
                                 h5(HTML("<b>4-3. Costs of equipment for setting up garden (million VND):</b>")),
                                 p("Anh/chị vui lòng đưa ra ước tính chi phí cho những thiết bị làm vườn cần thiết 
                                   (máy móc và dụng cụ làm vườn) (tính bằng triệu đồng)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("equipment_cost",
                                             NULL,
                                             min = 1,
                                             max = 50,
                                             value = c(5, 20),
                                             step = 1),
                                 h5(HTML("<b>4-4. Costs of training teachers when setting up garden (million VND):</b>")),
                                 p("Anh/chị vui lòng đưa ra ước tính chi phí ban đầu cho việc đào tạo giáo viên trong việc giảng dạy 
                                   những kiến thức về vườn trường (ví dụ: kiến thức liên quan đến làm vườn, sử dụng giáo cụ, dinh dưỡng 
                                   lành mạnh…) (tính bằng triệu đồng)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("teacher_training_cost",
                                             NULL,
                                             min = 1,
                                             max = 50,
                                             value = c(10, 30),
                                             step = 1),
                                 h5(HTML("<b>4-5. Costs of planning meetings (million VND):</b>")),
                                 p("Anh/chị vui lòng đưa ra ước tính chi phí cho hoạt động lập kế hoạch với ban giám 
                                   hiệu  trước khi xây dựng vườn trường (một phần chi phí ban đầu) (tính bằng triệu đồng)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("school_board_planning",
                                             NULL,
                                             min = 1,
                                             max = 50,
                                             value = c(3, 15),
                                             step = 1),
                                 h5(HTML("<b>4-6. Equipment for teaching (million VND):</b>")),
                                 p("Anh/chị vui lòng đưa ra ước tính chi phí cho các giáo cụ phục vụ cho việc giảng dạy 
                                   (ví dụ: kính hiển vi, kính lúp, bảng, biểu đồ, dụng cụ làm vườn cho trẻ em vv) (tính bằng triệu đồng)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("teaching_equipment",
                                             NULL,
                                             min = 1,
                                             max = 50,
                                             value = c(5, 15),
                                             step = 1),
                                 h5(HTML("<b>4-7. Starting compost (million VND):</b>")),
                                 p("Anh/chị vui lòng đưa ra ước tính chi phí cho một đống phân ủ (tương đương cỡ của vườn) 
                                   bao gồm chi phí thiết lập đống ủ và chi phí ban đầu của việc nuôi cấy vi sinh vật cho việc ủ phân 
                                   (tính bằng triệu đồng)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("compost_starting",
                                             NULL,
                                             min = 1,
                                             max = 50,
                                             value = c(3, 15),
                                             step = 1),
                                 h5(HTML("<b>4-8. Starting worms for compost (million VND):</b>")),
                                 p("Anh/chị vui lòng đưa ra ước tính chi phí của trùn quế cho hoạt động nuôi trùn quế (tính bằng triệu đồng)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("worm_starting",
                                             NULL,
                                             min = 1,
                                             max = 50,
                                             value = c(3, 10),
                                             step = 1),
                                 h5(HTML("<b>4-9. Livestock_establishment_costs:</b>")),
                                 p("Please estimate the costs for the desired set of livestock 
                                     that will be farmed in the garden for manure production and use of crop residues  (in million VND).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("livestock_establishment_costs",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(3, 20),
                                             step = 1),
                                 p(),
                                 h5(HTML("<b>4-10. Chance that families donate to establishment (%):</b>")),
                                 p("Theo anh/chị, có bao nhiêu % khả năng phụ huynh học sinh hoặc các nhà tài 
                                   trợ khác sẽ đóng góp vào chi phí xây dựng ban đầu (tính bằng %)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("if_family_pays_establishment_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(20, 50),
                                             step = 1),
                                 h5(HTML("<b>4-11. Portion of establishment costs donated by families (%):</b>")),
                                 p("Nếu có sự tham gia đóng góp của phụ huynh và nhà tài trợ tiềm năng vào chi phí thiết lập, 
                                   anh/chị nghĩ số tiền đóng góp sẽ là bao nhiêu (tính bằng triệu đồng)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("establishment_family_portion_paid_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(20, 80),
                                             step = 1),
                                 tags$a(href = "#0", "Quay lại Mục lục", 
                                        style = "display: inline-block; margin-top: 20px; padding: 5px 10px; background-color: #3283CD; 
                           float: right; color: white; border-radius: 5px; text-decoration: none;")
                                 
                    ),
                    #5. The running costs####
                    tags$div(id = "5", h4(strong('5. Chi phí duy trì hoạt động vườn trường'))),
                    
                    sidebarPanel(width = 12,
                                 style = "height: 60vh; overflow-y: auto;", #height in %
                                 
                                 h5(HTML("<b>5-1. Annual Labor cost to maintain school garden  (million VND/yr):</b>")),
                                 p("Anh/chị vui lòng ước tính chi phí nhân công hàng năm để duy trì khu vườn (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("maintaining_labor",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(20, 40),
                                             step = 1),
                                 h5(HTML("<b>5-2. Additional teacher salary costs (million VND/yr):</b>")),
                                 p("Anh/chị vui lòng ước tính chi phí cho việc tăng lương cho giáo viên tham gia giảng dạy trong dự án vườn trường",
                                   style = "padding-left: 20px;"),
                                 sliderInput("teacher_salary_cost",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(20, 30),
                                             step = 1),
                                 h5(HTML("<b>5-3. Teaching equipment / manitaining microscopes etc. (million VND/yr):</b>")),
                                 p("Anh/chị vui lòng ước tính chi phí hàng năm cho việc bảo trì (và thay mới mới khi cần thiết) 
                                   thiết bị dạy học (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("teaching_equipment_annual",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(1, 50),
                                             step = 1),
                                 h5(HTML("<b>5-4. Teaching tools / paper etc. (million VND/yr):</b>")),
                                 p("Hãy ước tính chi phí hàng năm cho tài liệu giảng dạy như giấy, v.v. (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("teaching_tools",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(2, 20),
                                             step = 1),
                                 h5(HTML("<b>5-5. Please estimate the annual costs for seeds and seedlings  (in million VND/ year).:</b>")),
                                 p("Hãy ước tính chi phí về hạt giống và cây giống hàng năm (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("seed_costs",
                                             NULL,
                                             min = 1,
                                             max = 50,
                                             value = c(2, 30),
                                             step = 1),
                                 h5(HTML("<b>5-6. Fertilizer i.e. EM to add to compost (million VND/yr):</b>")),
                                 p("Anh/chị vui lòng ước tính chi phí cho phân bón hữu cơ cần mua (nếu việc tự sản xuất không đáp ứng được 
                                   nhu cầu), vi sinh vật có lợi để thêm vào phân trộn, giun đất để làm phân trùn quế, nếu cần, v.v. 
                                   (tính bằng triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("fertilizer",
                                             NULL,
                                             min = 1,
                                             max = 30,
                                             value = c(1, 5.5),
                                             step = 0.5),
                                 h5(HTML("<b>5-7. Integrated Pest Managemernt (IPM) (million VND/yr):</b>")),
                                 p("Anh/chị vui lòng ước tính chi phí hàng năm cho các biện pháp quản lý dịch hại tổng hợp (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("plant_protection",
                                             NULL,
                                             min = 1,
                                             max = 30,
                                             value = c(1, 5.5),
                                             step = 0.5),
                                 h5(HTML("<b>5-8. Mainitaining animals (million VND/yr):</b>")),
                                 p("Anh/chị vui lòng ước tính chi phí hàng năm cho vật nuôi trong vườn (thức ăn, chăm sóc thú ý, bảo trì 
                                   chuồng trại và hàng rào, thay mới dụng cụ chăn nuôi nhưng mán thức ăn và máng đựng nước…) (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("livestock_maint",
                                             NULL,
                                             min = 1,
                                             max = 50,
                                             value = c(1, 20),
                                             step = 1),
                                 h5(HTML("<b>5-9. Mainitaining teacher training (million VND/yr):</b>")),
                                 p("Anh/chị vui lòng ước tính chi phí hàng năm (tiếp tục) để duy trì đào tạo giáo viên giảng dạy trong dự án vườn trường (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("annual_teacher_training",
                                             NULL,
                                             min = 1,
                                             max = 500,
                                             value = c(5, 276),
                                             step = 1),
                                 tags$a(href = "#0", "Quay lại Mục lục", 
                                        style = "display: inline-block; margin-top: 20px; padding: 5px 10px; background-color: #3283CD; 
                           float: right; color: white; border-radius: 5px; text-decoration: none;")
                                 
                    ), #close 5. sidebarPanel 
                    #6. Canteen####
                    tags$div(id = "6", h4(strong('6. Căng tin'))),
                    
                    sidebarPanel(width = 12,
                                 style = "height: 60vh; overflow-y: auto;", #height in %
                                 
                                 h5(HTML("<b>6-1. Chance that the school has a canteen (%):</b>")),
                                 p("Theo anh/chị, có bao nhiêu % khả năng là trường học có căng tin",
                                   style = "padding-left: 20px;"),
                                 sliderInput("if_school_has_canteen_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(20, 50),
                                             step = 1),
                                 h5(HTML("<b>6-2. Canteen savings (million VND/yr):</b>")),
                                 p("Nếu trường có căng tin, anh/chị vui lòng đưa ra ước tính chi phí mà căng tin trường sẽ 
                                   tiết kiệm được nhờ các sản phẩm từ vườn trường cung cấp thay vì đi mua (nếu sản phẩm vườn trồng 
                                   thay thế cho trái cây hoặc rau củ do căng tin mua) nếu có (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("canteen_savings",
                                             NULL,
                                             min = 1,
                                             max = 10,
                                             value = c(1, 5.5),
                                             step = 0.5),
                                 h5(HTML("<b>6-3. Sales of garden products (million VND/yr):</b>")),
                                 p("Anh/chị vui lòng đưa ra ước tính doanh thu hàng năm từ các sản phẩm không được 
                                   sử dụng cho căng tin từ vườn trường (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("sale_of_yield",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(10, 30),
                                             step = 1),
                                 tags$a(href = "#0", "Quay lại Mục lục", 
                                        style = "display: inline-block; margin-top: 20px; padding: 5px 10px; background-color: #3283CD; 
                           float: right; color: white; border-radius: 5px; text-decoration: none;")
                                 
                    ),
                    #7. Potential cost savings####
                    tags$div(id = "7", h4(strong('7. Tiết kiệm chi phí khác nhờ có vườn trường'))),
                    
                    sidebarPanel(width = 12,
                                 style = "height: 60vh; overflow-y: auto;", #height in %
                                 
                                 h5(HTML("<b>7-1. Savings from extra-cirriclar activities (million VND/year):</b>")),
                                 p("Anh/chị vui lòng ước tính chi phí hàng năm cho hoạt động ngoại khóa có thể tiết kiệm 
                                   (nếu có) được nhờ có vườn trường (triệu đồng/năm)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("extra_cirricular_savings",
                                             NULL,
                                             min = 1,
                                             max = 200,
                                             value = c(50, 80),
                                             step = 1),
                                 h5(HTML("<b>7-2. Savings on formal education costs (no STEM garden) (million VND/year):</b>")),
                                 p("Hãy ước tính mức tiết kiệm (nếu có) cho chi phí giáo dục chính quy trong trường hợp vườn trường 
                                   được thiết lập mà KHÔNG tích hợp phương pháp giảng dạy STEM (triệu đồng/năm)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("formal_edu_savings",
                                             NULL,
                                             min = 1,
                                             max = 20,
                                             value = c(1, 10.5),
                                             step = 0.5),
                                 h5(HTML("<b>7-3. formal_edu_savings_STEM:</b>")),
                                 p("Hãy ước tính mức tiết kiệm (nếu có) cho chi phí giáo dục chính quy  trong trường hợp vườn 
                                   trường được thiết lập mà CÓ tích hợp phương pháp giảng dạy STEM (triệu đồng/năm)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("formal_edu_savings_STEM",
                                             NULL,
                                             min = 1,
                                             max = 500,
                                             value = c(20, 100),
                                             step = 1),
                                 tags$a(href = "#0", "Quay lại Mục lục", 
                                        style = "display: inline-block; margin-top: 20px; padding: 5px 10px; background-color: #3283CD; 
                           float: right; color: white; border-radius: 5px; text-decoration: none;")
                                 
                                 
                    ),
                    #8. Estimation of potential external investment####
                    tags$div(id = "8", h4(strong('8. Ước tính tiềm năng thu hút đầu tư'))),
                    
                    p("Vận hành chương trình vườn trường có thể làm tăng danh tiếng của trường và do đó, điều này có thể dẫn đến nguồn đầu tư 
                      bổ sung từ bên ngoài trường học (ví dụ: các nhà tài trợ từ các doanh nghiệp địa phương). Tuy nhiên, điều này không chắc chắn."),
                    sidebarPanel(width = 12,
                                 style = "height: 60vh; overflow-y: auto;", #height in %
                                 
                                 h5(HTML("<b>8-1. Value of outside investment due to improved reputation no formal STEM (million VND/year):</b>")),
                                 p(HTML("Vui lòng ước tính giá trị đầu tư từ bên ngoài từ việc nâng cao danh tiếng (nếu có) trong trường hợp vườn được 
                                        thành lập mà KHÔNG tích hợp phương pháp giảng dạy STEM (triệu đồng/năm)"),
                                   style = "padding-left: 20px;"),
                                 sliderInput("outside_investment_value",
                                             NULL,
                                             min = 0.5,
                                             max = 20,
                                             value = c(1, 10.5),
                                             step = 0.5),
                                 h5(HTML("<b>8-2. Value of outside investment due to improved reputation with STEM (million VND/year):</b>")),
                                 p(HTML("Vui lòng ước tính giá trị đầu tư từ bên ngoài từ việc nâng cao danh tiếng (nếu có) trong trường hợp vườn được 
                                        thành lập mà CÓ tích hợp phương pháp giảng dạy STEM (triệu đồng/năm)"),
                                   style = "padding-left: 20px;"),
                                 sliderInput("outside_investment_value_STEM",
                                             NULL,
                                             min = 0.5,
                                             max = 20,
                                             value = c(1, 10.5),
                                             step = 0.5),
                                 h5(HTML("<b>8-3. Increased enrollment and/or tuition due to improved reputation no STEM (million VND/year):</b>")),
                                 p(HTML("Vui lòng ước tính doanh thu tăng hằng năm từ việc tăng số lượng tuyển sinh và/hoặc tăng học phí cho mỗi trẻ 
                                        do danh tiếng được cải thiện trong trường hợp vườn trường KHÔNG tích hợp dạy học STEM (khoa học, công nghệ, 
                                        kỹ thuật và toán) (triệu đồng/năm)."),
                                   style = "padding-left: 20px;"),
                                 sliderInput("increased_enrollment_value",
                                             NULL,
                                             min = 0.1,
                                             max = 10,
                                             value = c(0.1, 5.1),
                                             step = 0.1),
                                 h5(HTML("<b>8-4. Increased enrollment and/or tuition due to improved reputation with STEM (million VND/year):</b>")),
                                 p(HTML("Vui lòng ước tính doanh thu tăng hằng năm từ việc tăng số lượng tuyển sinh và/hoặc tăng học phí cho mỗi trẻ do 
                                        danh tiếng được cải thiện trong trường hợp vườn trường CÓ tích hợp dạy học STEM (khoa học, công nghệ, kỹ thuật 
                                        và toán) (triệu đồng/năm)."),
                                   style = "padding-left: 20px;"),
                                 sliderInput("increased_enrollment_value_STEM",
                                             NULL,
                                             min = 1,
                                             max = 200,
                                             value = c(50, 80),
                                             step = 1),
                                 tags$a(href = "#0", "Quay lại Mục lục", 
                                        style = "display: inline-block; margin-top: 20px; padding: 5px 10px; background-color: #3283CD; 
                           float: right; color: white; border-radius: 5px; text-decoration: none;")
                                 
                                 
                    ),
                    #9. Impacts on health, performance and community engagement:####
                    tags$div(id = "9", h4(strong('9. Tác động của vườn trường tới sức khỏe, học tập và hoạt động xã hội của trẻ
                    '))),
                    
                    p("Children having a garden at their school might or might not eat and snack more fruits and vegetables 
                 than children without a school garden. This could potentially affect their health status, ability to concentrate 
                 and learn as well as to be able to socially engage with their environment. We assume that the quantity children 
                 eat/snack fruits and vegetables from the garden is not affected by whether the garden is included in a STEM teaching approach or not."),
                    sidebarPanel(width = 12,
                                 style = "height: 60vh; overflow-y: auto;", #height in %
                                 
                                 h5(HTML("<b>9-1. Healthcare savings for children having more access to safe vegetables from the garden (million VND/year):</b>")),
                                 p("Vui lòng ước tính chi phí tiết kiệm được hàng năm trong việc chăm sóc sức khỏe của trẻ em học ở những trường có vườn trường và 
                                   ăn nhiều rau quả hơn (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("child_veg_health_care_savings",
                                             NULL,
                                             min = 0.1,
                                             max = 10,
                                             value = c(0.1, 5),
                                             step = 0.1),
                                 h5(HTML("<b>9-2. School performance value for children having more access to safe vegetables from the garden (million VND/year):</b>")),
                                 p("Vui lòng ước tính lợi ích hàng năm bằng tiền nhờ việc cải thiện kết quả học tập ở trẻ em học ở trường có vườn trường và ăn 
                                   nhiều rau quả hơn (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("child_veg_school_performance_value",
                                             NULL,
                                             min = 0.01,
                                             max = 1,
                                             value = c(0.01, 0.2),
                                             step = 0.01),
                                 h5(HTML("<b>9-3. Community engagement value for children having more access to safe vegetables from the garden (million VND/year):</b>")),
                                 p("Vui lòng ước tính lợi ích hàng năm bằng tiền từ việc tham gia tham các hoạt động xã hội tích cực hơn của trẻ em học ở trường có vườn 
                                   và ăn nhiều rau quả hơn (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("child_veg_community_engagement_value",
                                             NULL,
                                             min = 0.01,
                                             max = 1,
                                             value = c(0.01, 0.1),
                                             step = 0.01),
                                 tags$a(href = "#0", "Quay lại Mục lục", 
                                        style = "display: inline-block; margin-top: 20px; padding: 5px 10px; background-color: #3283CD; 
                           float: right; color: white; border-radius: 5px; text-decoration: none;")
                                 
                                 
                    ),
                    #10. mental health####
                    tags$div(id = "10", h4(strong('10. Tác động lên sức khỏe tinh thần'))),
                    
                    p("Nghiên cứu y học đã xác nhận lợi ích về sức khỏe tinh thần của các khoảng không gian xanh tại các thành phố. 
                      Vườn trường được cho là có thể hoặc không có lợi ích về sức khỏe tinh thần"),
                    sidebarPanel(width = 12,
                                 style = "height: 20vh; overflow-y: auto;", #height in %
                                 
                                 h5(HTML("<b>10-1. Mental health value of children having a garden at school (million VND/year):</b>")),
                                 p("Vui lòng ước tính lợi ích đối với sức khỏe tinh thần (bằng tiền, hàng năm) từ không gian xanh của vườn 
                                   trường đối với trẻ em, giáo viên nhà trường và cư dân những khu lân cận trường",
                                   style = "padding-left: 20px;"),
                                 sliderInput("garden_mental_health_value",
                                             NULL,
                                             min = 1,
                                             max = 500,
                                             value = c(4, 300),
                                             step = 1),
                                 tags$a(href = "#0", "Quay lại Mục lục", 
                                        style = "display: inline-block; margin-top: 20px; padding: 5px 10px; background-color: #3283CD; 
                           float: right; color: white; border-radius: 5px; text-decoration: none;")
                                 
                                 
                    ),
                    #11. Estimation with and without a STEM####
                    tags$div(id = "11", h4(strong('11. Kịch bản có và không có phương pháp giảng dạy STEM'))),
                    
                    p("Có khả năng trẻ em học ở những trường có vườn có thể (hoặc không) lựa chọn những thực phẩm tốt cho sức khỏe hơn là những trẻ em học 
                      ở những trường không có vườn. Điều này có khả năng ảnh hưởng đến tình trạng sức khỏe, khả năng tập trung và học hỏi cũng như khả năng 
                      hòa nhập với môi trường xã hội của các em. Chúng tôi cho rằng việc có đưa dự án vườn trường vào chương trình giáo dục STEM hay không sẽ 
                      ảnh hưởng đến sự lựa chọn thực phẩm của các em."),
                    sidebarPanel(width = 12,
                                 style = "height: 60vh; overflow-y: auto;", #height in %
                                 h4(strong("Without a STEM teaching approach")),
                                 h5(HTML("<b>11-1. Healthcare savings from children making better choices about food with a passive (no STEM) garden (million VND/year):</b>")),
                                 p(HTML("Vui lòng ước tính lợi ích hàng năm từ việc tiết kiệm chi phí chăm sóc sức khỏe cho trẻ em học ở những trường có vườn và có sự  lựa chọn 
                                        thực phẩm tốt hơn (không có STEM) (triệu đồng/năm)."),
                                   style = "padding-left: 20px;"),
                                 sliderInput("child_garden_health_care_savings",
                                             NULL,
                                             min = 1,
                                             max = 1000,
                                             value = c(10, 500),
                                             step = 1),
                                 h5(HTML("<b>11-2. School performance value for children making better choices about food with a passive (no STEM) garden (million VND/year):</b>")),
                                 p(HTML("Vui lòng ước tính lợi ích hàng năm của việc cải thiện thành tích học tập của trẻ em học ở những trường có vườn và có sự  lựa chọn thực phẩm tốt hơn (không có STEM) (triệu đồng/năm)."),
                                   style = "padding-left: 20px;"),
                                 sliderInput("child_garden_school_performance_value",
                                             NULL,
                                             min = 1,
                                             max = 500,
                                             value = c(21, 200),
                                             step = 1),
                                 h5(HTML("<b>11-3. Community engagement value for children making better choices about food with a passive (no STEM) garden (million VND/year):</b>")),
                                 p("Vui lòng ước tính lợi ích của việc tham gia xã hội tích cực hơn của trẻ em học ở những trường có vườn và có lựa chọn thực phẩm tốt hơn (không có STEM) (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("child_garden_community_engagement_value",
                                             NULL,
                                             min = 1,
                                             max = 50,
                                             value = c(3, 7),
                                             step = 1),
                                 p(),
                                 p(),
                                 h4(strong("With a STEM teaching approach")),
                                 
                                 h5(HTML("<b>11-4. Healthcare savings from children making better choices about food with STEM garden (million VND/year):</b>")),
                                 p("Vui lòng ước tính lợi ích hàng năm từ việc tiết kiệm chi phí chăm sóc sức khỏe cho trẻ em học ở những trường có vườn và có sự 
                                   lựa chọn thực phẩm tốt hơn (có STEM) (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("child_STEM_health_care_savings",
                                             NULL,
                                             min = 1,
                                             max = 500,
                                             value = c(10, 80),
                                             step = 1),
                                 h5(HTML("<b>11-5. School performance value for children making better choices about food with a STEM garden (million VND/year):</b>")),
                                 p("Vui lòng ước tính lợi ích hàng năm của việc cải thiện thành tích học tập của trẻ em học ở những trường có vườn và có sự  lựa chọn thực 
                                   phẩm tốt hơn (có STEM) (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("child_STEM_school_performance_value",
                                             NULL,
                                             min = 1,
                                             max = 500,
                                             value = c(2, 100),
                                             step = 1),
                                 h5(HTML("<b>11-6. Community engagement value for children making better choices about food with a STEM garden (million VND/year):</b>")),
                                 p("Vui lòng ước tính lợi ích của việc tham gia xã hội tích cực hơn của trẻ em học ở những trường có vườn và có lựa chọn thực phẩm tốt hơn 
                                   (có STEM) (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("child_STEM_community_engagement_value",
                                             NULL,
                                             min = 1,
                                             max = 500,
                                             value = c(10, 250),
                                             step = 1),
                                 tags$a(href = "#0", "Quay lại Mục lục", 
                                        style = "display: inline-block; margin-top: 20px; padding: 5px 10px; background-color: #3283CD; 
                           float: right; color: white; border-radius: 5px; text-decoration: none;")
                                 
                                 
                    ),
                    #12. Giá trị môi trường####
                    tags$div(id = "12", h4(strong('12. Giá trị môi trường  '))),
                    
                    sidebarPanel(width = 12,
                                 style = "height: 40vh; overflow-y: auto;", #height in %
                                 
                                 h5(HTML("<b>12-1. Value of green space (million VND/year):</b>")),
                                 p("Hãy ước tính giá trị hàng năm của việc có được môi trường sống cho thực vật, động vật (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("green_space_eco_value",
                                             NULL,
                                             min = 1,
                                             max = 50,
                                             value = c(1, 10),
                                             step = 1),
                                 h5(HTML("<b>12-2. Value of reduced polution on school garden (million VND/year):</b>")),
                                 p("Hãy ước tính giá trị giảm thiểu ô nhiễm hàng năm (triệu đồng/năm).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("reduce_pollution_value",
                                             NULL,
                                             min = 1,
                                             max = 50,
                                             value = c(1, 3),
                                             step = 1),
                                 tags$a(href = "#0", "Quay lại Mục lục", 
                                        style = "display: inline-block; margin-top: 20px; padding: 5px 10px; background-color: #3283CD; 
                           float: right; color: white; border-radius: 5px; text-decoration: none;")
                                 
                                 
                    ), # close sidebarPanel 12. 
                    #13. Giá trị các sự kiện trường học.####
                    tags$div(id = "13", h4(strong('13. Giá trị các sự kiện gắn với vườn trường'))),
                    
                    sidebarPanel(width = 12,
                                 style = "height: 40vh; overflow-y: auto;", #height in %
                                 
                                 h5(HTML("<b>13-1. Value of garden related school events (million VND/year):</b>")),
                                 p("Hãy ước tính giá trị của một (1) sự kiện liên quan đến vườn trường đối với trường học và cộng đồng (triệu đồng/sự kiện)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("school_event_value",
                                             NULL,
                                             min = 1,
                                             max = 500,
                                             value = c(10, 200),
                                             step = 1),
                                 h5(HTML("<b>13-2. Number of school events per year (days per year):</b>")),
                                 p("Vui lòng ước tính số sự kiện trường học liên quan đến vườn mỗi năm (số ngày trong năm)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("school_event_freq",
                                             NULL,
                                             min = 1,
                                             max = 50,
                                             value = c(3, 10),
                                             step = 1),
                                 tags$a(href = "#0", "Quay lại Mục lục", 
                                        style = "display: inline-block; margin-top: 20px; padding: 5px 10px; background-color: #3283CD; 
                           float: right; color: white; border-radius: 5px; text-decoration: none;")
                                 
                                 
                    ), # close sidebarPanel 13. 
                    #14. Thông số liên quan đến sử dụng đất...####
                    tags$div(id = "14", h4(strong('14. Thông số liên quan đến sử dụng đất'))),
                    
                    sidebarPanel(width = 12,
                                 style = "height: 60vh; overflow-y: auto;", #height in %
                                 
                                 h4(strong("Land Use")),
                                 h5(HTML("<b>14-1. Value of non garden land use, playground etc. (million VND/yr):</b>")),
                                 p("Anh/chị vui lòng ước tình giá trị hàng năm mà mảnh đất mang lại với mục đích sử dụng hiện nay 
                                   (ví dụ: sân chơi cho trẻ em)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("value_of_non_garden_land_use",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(20, 50),
                                             step = 1),
                                 h5(HTML("<b>14-2. Cost of non garden land use (million VND/yr):</b>")),
                                 p("Vui lòng ước tính chi phí hàng năm của mục đích sử dụng khác của mảnh đất thay vì làm vườn 
                                   (ví dụ: chi phí san lấp, làm hàng rào, dọn dẹp?) (triệu đồng/năm)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("costs_of_non_garden_land_use",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(1, 5),
                                             step = 1),
                                 p(),
                                 p(),
                                 h4(strong("Parking")),
                                 h5(HTML("<b>14-3. Chance of including parking on the plot without a garden (%):</b>")),
                                 p("Anh/chị nghĩ có bao nhiêu %  khả năng diện tích đất dự định sử dụng cho vườn trường có 
                                   thể tạo ra thu nhập thay thế, ví dụ: nếu nó được sử dụng làm bãi đậu xe thay vì làm vườn trường (tính bằng %).",
                                   style = "padding-left: 20px;"),
                                 sliderInput("if_parking_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(10, 80),
                                             step = 1),
                                 h5(HTML("<b>14-4. Above table value of parking (million VND/yr):</b>")),
                                 p("Nếu có khả năng tạo ra thu nhập khác thay vì làm vườn trường, giá trị hàng năm của nguồn 
                                   thu nhập này từ mảnh đất là bao nhiêu?  (triệu đồng/năm)",
                                   style = "padding-left: 20px;"),
                                 sliderInput("parking_value",
                                             NULL,
                                             min = 0.1,
                                             max = 10,
                                             value = c(0.2, 3),
                                             step = 0.1),
                                 tags$a(href = "#0", "Quay lại Mục lục", 
                                        style = "display: inline-block; margin-top: 20px; padding: 5px 10px; background-color: #3283CD; 
                           float: right; color: white; border-radius: 5px; text-decoration: none;")
                                 
                                 
                    ), # close sidebarPanel 14. 
                    #15. Các biến số khác ####
                    tags$div(id = "15", h4(strong('15. Các biến số khác'))),
                    
                    sidebarPanel(width = 12,
                                 style = "height: 40vh; overflow-y: auto;", #height in %
                                 
                                 h5(HTML("<b>15-1. Chance of school choosing to integrate animals in garden (%):</b>")),
                                 p("Khả năng trường sẽ đưa động vật vào trong mô hình",
                                   style = "padding-left: 20px;"),
                                 sliderInput("if_animals_in_garden_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(20, 70),
                                             step = 1),
                                 h5(HTML("<b>15-2. Digging a fish pond in the garden (million VND):</b>")),
                                 p("Khả năng trường cho đào ao cá",
                                   style = "padding-left: 20px;"),
                                 sliderInput("fishpond_cost",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(7, 10),
                                             step = 1),
                                 h5(HTML("<b>15-3. Chance that the garden space is fallow green space (%):</b>")),
                                 p("Khả năng khu đất làm vườn là đất hỏ hoang nhưng vẫn có thảm thực vật",
                                   style = "padding-left: 20px;"),
                                 sliderInput("chance_garden_is_fallow_green_space_t",
                                             NULL,
                                             min = 0.1,
                                             max = 100,
                                             value = c(0.2, 5),
                                             step = 0.1),
                                 h5(HTML("<b>15-4. Proportion of value of fallow greenspace compared to garden (%):</b>")),
                                 p("Phần trăm diện tích đất bỏ hoang nhưng vẫn có thảm thực vật so với diện tích cả khu vườn",
                                   style = "padding-left: 20px;"),
                                 sliderInput("fallow_eco_reduction_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(50, 80),
                                             step = 1),
                                 h5(HTML("<b>15-5. Value of non-garden green space for child health (million VND/year):</b>")),
                                 p("Giá trị không gian xanh của phần đất bỏ hoang (nhưng vẫn có thảm thực vật) đối với sức khỏe học sinh",
                                   style = "padding-left: 20px;"),
                                 sliderInput("green_space_health_value",
                                             NULL,
                                             min = 1,
                                             max = 50,
                                             value = c(1, 10),
                                             step = 1),
                                 h5(HTML("<b>15-6. Proportion of value of fallow greenspace for child heatlh compared to garden (%):</b>")),
                                 p("Phần trăm giá trị phần đất bỏ hoang (vẫn có thảm thực vật) đối với sức khỏe học sinh khi so sánh với cả khu vườn",
                                   style = "padding-left: 20px;"),
                                 sliderInput("fallow_health_reduction_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(50, 80),
                                             step = 1),
                                 h5(HTML("<b>15-7. Chance that the school has acess to land (%):</b>")),
                                 p("Khả năng trường có tiếp cận đất",
                                   style = "padding-left: 20px;"),
                                 sliderInput("land_access_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(60, 95),
                                             step = 1),
                                 h5(HTML("<b>15-8. Chance that the land at the school is suitable (%):</b>")),
                                 p("Khả năng khu đất phù hợp làm vườn trường",
                                   style = "padding-left: 20px;"),
                                 sliderInput("suitability_of_land_for_garden_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(60, 95),
                                             step = 1),
                                 h5(HTML("<b>15-9. Chance that beurocratic barriers will inhibit the process (%):</b>")),
                                 p("Khả năng bị các rào cản liên quan đến hành chính, thủ tục gây cản trở quá trình xây dựng vườn trường",
                                   style = "padding-left: 20px;"),
                                 sliderInput("beurocratic_barriers_t",
                                             NULL,
                                             min = 1,
                                             max = 100,
                                             value = c(1, 50),
                                             step = 1),
                                 tags$a(href = "#0", "Quay lại Mục lục", 
                                        style = "display: inline-block; margin-top: 20px; padding: 5px 10px; background-color: #3283CD; 
                           float: right; color: white; border-radius: 5px; text-decoration: none;")
                    ) # close sidebarPanel 15.
             ), #close left colum
             
             
             
             
             #Results####
             column(5, class = "main-panel",
                    style = "height: 100vh; overflow-y: auto;",
                    wellPanel(
                      p("Có thể mất chút thời gian để hiển thị biểu đồ."),
                      h3( strong('Kết quả')),
                      h5(strong("1. Giá trị hiện tại ròng (NPV)")),
                      plotOutput("distPlot1",height = "250px",
                                 width = "95%"),
                      p(strong("Fig.1 Phân bố kết quả xác suất từ mô phỏng Monte Carlo cho đường cơ sở và can thiệp.")),
                      p ("Biểu đồ trên cung cấp so sánh trực quan về kết quả đầu tư tính bằng giá trị hiện tại ròng (NPV) tính bằng triệu 
                         đồng trong khoảng thời gian mô phỏng khi so sánh giải pháp can thiệp, tức là có thiết lập vườn trường so với không 
                         có vườn trường. Trục x là giá trị lợi nhuận tại ròng của vườn trường cho toàn bộ số năm mô phỏng, đã được chiết khấu 
                         về giá trị hiện tại. Trục y là mật độ xác suất xảy ra tức là mức độ tập trung của các giá trị NPV. Các vùng có mật độ 
                         cao hơn (đường cong cao hơn) thì Giá trị NPV ở đó xuất hiện nhiều hơn, có xác suất cao hơn. Các vùng có mật độ thấp hơn 
                         (đường cong thấp hơn) thì giá trị NPV ít xuất hiện hơn. Diện tích dưới đường cong, tức là tổng xác suất của tất cả các giá 
                         trị NPV có thể xảy ra luôn bằng 1."),
                      downloadButton("save_plot1", "Tải về đồ thị"),
                      
                      br(), # blank line
                      br(), # blank line
                    
                      h5(strong("2.“Nên làm” vs. “Không nên làm")),
                      plotOutput("distPlot2",height = "250px",
                                 width = "95%"),
                      h5(strong('Figure 2. Kết quả xác suất của quyết định xét về NPV trong giai đoạn mô phỏng.')),
                      p("Biểu đồ cột minh họa phân bố các kết quả của Giá trị Hiện tại Ròng (NPV), tính bằng triệu đồng, 
                        trong hai kịch bản là kịch bản cơ sở và kịch bản can thiệp, trong suốt giai đoạn mô phỏng. Kịch bản 
                        can thiệp gồm lựa chọn thiết lập vườn trường có và không có tích hợp với STEM tại một trường công lập. 
                        Quyết định được thể hiện trực quan: các cột màu xanh bên phải biểu thị các trường hợp NPV dương (hỗ trợ quyết định 'Nên làm'), 
                        trong khi các cột bên trái biểu thị NPV âm (gợi ý 'Không nên làm')."),
                      
                      downloadButton("save_plot2", "Tải về đồ thị"),
                      
                      br(), # blank line
                      br(), # blank line
                      
                      h5(strong("3-1. Cash flow of public school garden")),
                      
                      plotOutput("distPlot3",height = "250px",
                                 width = "95%"),
                      
                      p('Biểu đồ trên minh họa dòng tiền mặt tính bằng triệu đồng/năm trong những năm mô phỏng.'),
                      downloadButton("save_plot3", "Tải về đồ thị"), 
                      
                      h5(strong("3-2. Cash flow of public school STEM garden")),
                      
                      plotOutput("distPlot4",height = "250px",
                                 width = "100%"),
                      
                      p('Biểu đồ trên minh họa dòng tiền mặt tính bằng triệu đồng/năm trong những năm mô phỏng.'),
                      downloadButton("save_plot4", "Tải về đồ thị"), 
                      
                    )# close wellPanel       
             ) # close the right side column
           ), #close FluidRow
           
           fluidRow(
             
             sidebarPanel(width = 12, 
                          
                          fluidRow(tags$div(id = "16", h4(strong('Cải thiện mô hình'))),
                                    textAreaInput(width = "100%", 
                                                  height ='100px',
                                                  "Improvement_1", 
                                                  "Xin hãy cho chúng tôi thêm ý tưởng để cải thiện mô hình.")),
                          fluidRow(align = "left",
                                   
                                   #save & download file####
                                   # Single button for saving and downloading
                                   h5( strong('Please save and download your file')),      
                                   downloadButton('saveDownload', 'Tải về'))
             ),#close sidebarPanel
             
             fluidRow( width = 10, 
                       div(style = "background-image: url('favri_1.jpg'); 
                        background-size: cover; 
                        height: 100vh; 
                        color: white;")),
             fluidRow(column(width = 2,
                             align = "right",
                             tags$a(href = "https://codas.vn/?lang=en",
                                    tags$img(src = "Logo/CODAS.png", height = "70px"))
             ),
             column(width = 8,  # Center alignment logic with empty space between logos
                    align = "center",
                    ""
             ),      
             column( width = 2,
                     align = "center",
                     tags$a(tags$img(src = "Logo/BMEL.png", height = "90px"))
             ))
             
           )# close last fluidRow
           
           
           
  ), # closed tabPanel
  
  # second tabPanel ####
  tabPanel("Input",
           h3("Input Table"),
           class= "tabPanel",
           fluidRow(
             tableOutput("simple_table")
           )
           
  ),#finish second tabPanel
  
  #third table
  tabPanel("Output X",
           h3("Output Data"),
           class= "tabPanel",
           fluidRow(
             tableOutput("output_table_x")
           )
           
  ),#finish third tabPanel
  #fourth table
  tabPanel("Output Y",
           h3("Output Data"),
           class= "tabPanel",
           fluidRow(
             tableOutput("output_table_y")
           )
           
  ), ##finish fourth tabPanel
  #fifth table
  tabPanel("Output Y/N",
           h3("Output Y/N"),
           class= "tabPanel",
           fluidRow(
             tableOutput("garden_data_long")
           )
           
  )#finish fourth tabPanel
  
  
  
  
  ) # closed tabsetPanel
  
)# UI closed

# Define server logic required to draw a histogram


server <- function(input, output, session) {
  
  
  # observeEvent(input$toggleSidebar, {
  #   toggle("sidebar")  # Toggles the visibility of the sidebar
  # })
  # Track sidebar visibility
  sidebar_visible <- reactiveVal(TRUE)
  
  observeEvent(input$toggleSidebar, {
    toggle("sidebar")
    sidebar_visible(!sidebar_visible())
    
    if (sidebar_visible()) {
      updateActionButton(session, "toggleSidebar", label = "Hide Sidebar",icon = icon("fa-light fa-eye-slash"))
      shinyjs::removeClass(selector = "#toggleSidebar", class = "closed")
    } else {
      updateActionButton(session, "toggleSidebar", label = "Show 1. Introduction",icon = icon("fa-regular fa-face-grin-stars"))
      shinyjs::addClass(selector = "#toggleSidebar", class = "closed")
    }
  })
  
  
  # Reactive expression to calculate lower_values
  input_estimates <- reactive({
    
    # variables: input ID
    variables <- names(input)[!grepl("_1$", names(input))]
    
    
    lower_values <- sapply(variables, function(var) {
      value <- input[[var]]
      
      # Apply logic based on the slider ranges
      if (length(value) == 1) {
        as.numeric(value)
      } else if (grepl("_t$", var)) {  # Check if variable name ends with "_t"
        as.numeric(value[1])  / 100  # Special logic for sliders with names ending in "_t"
      } else if (grepl("_1$", var)) {
        NULL  # Default behavior
      } else {
        as.numeric(value[1])   # Default behavior
      }
    })
    
    
    upper_values <- sapply(variables, function(var) {
      value <- input[[var]]
      
      # Apply logic based on the slider ranges
      if (length(value) == 1) {
        as.numeric(value)
      } else if (grepl("_t$", var)) {  # Check if variable name ends with "_t"
        as.numeric(value[2]) / 100  # Special logic for sliders with names ending in "_t"
      } else if (grepl("_1$", var)) {
        NULL   # Default behavior
      }else {
        as.numeric(value[2])  # Default behavior
      }
    })
    
    # distributions    
    distributions <- sapply(variables, function(var) {
      value <- input[[var]]
      
      if (grepl("_t$", var)) {
        "tnorm_0_1"  # Special logic for variables ending with "_t"
      } else if (grepl("_c$", var)) {  # Check if variable name ends with "_c"
        "const"  # Special logic for sliders with names ending in "_t"
      } else if (grepl("_1$", var)) {
        as.character(value)   # Default behavior
      } else {
        "posnorm"  # Default behavior
      }
    })
    
    
    
    # Create a data frame from the results
    data.frame(
      variable = variables,
      lower = lower_values, 
      upper = upper_values, 
      distribution = distributions, 
      stringsAsFactors = FALSE)
  })
  
  school_garden_function <- function(x, varnames){
    
    # Costs####
    
    # Establishment costs 
    
    # chance_event options: 
    # family contribution? Will they pay a bit? 
    family_pays_establishment_yes_no <- chance_event(if_family_pays_establishment_t) 
    # some above the table (mostly under the table)
    
    garden_construction_cost <- if (family_pays_establishment_yes_no == 1) {
      construction_cost * # labor cost (2-3 people/day) + machine cost to setup garden system
        (1-establishment_family_portion_paid_t) # the family pays a bit
    } else {
      construction_cost = construction_cost
    }
    
    ## Garden establishment ####
    
    garden_establishment_costs <- compost_starting + # getting started with the compost
      worm_starting + # maintaining the compost and breaking down residue
      garden_designing_costs + # garden design costs (hiring a planner) 
      equipment_cost + # this is a high value because we may need a lot of equipment, netting, trellis for plants to climb
      # considering the range, this could be simple or a smart system (full automation)... 
      garden_construction_cost  
    
    # garden establishment cost values based on garden land area 
    # gardens are equally expensive but get more expensive if they are really big
    if (size_of_garden > expensive_garden_size) {
      # of the garden size is large then increase the establishment costs
      # increase by 
      garden_establishment_costs <- garden_establishment_costs * cost_increase_expensive_garden_size
    } else {
      garden_establishment_costs <- garden_establishment_costs 
    }
    
    ## STEM Garden establishment ####
    #costs if with STEM education
    STEM_establishment_costs <- teaching_equipment + # teaching equipment for sciences (science oriented training)
      # includes microscopes and other highly technical education equipment
      # consider 'if else' for aquatic vs. soil vs. rooftop in available space 
      # (not all have soil but all have space)
      teacher_training_cost  # cost for training teacher on gardening
    # this is low because we see it as a benefit partly because of 
    # training for the teachers in STEM and other topics like transdiscipinary and other topics
    # we save time and money on the training, which would otherwise have been spent on other training
    # teacher's cn also save money on other training courses for these topics 
    # that they otherwise would have had to take
    # requires training on 5 or 7 subjects (biology etc.) for 12 days
    
    # cut-off values for number of students
    
    #establishment costs if passive (no STEM) education ####
    establishment_cost_year_one <- school_board_planning + 
      garden_establishment_costs
    #establishment costs if with STEM education ####
    establishment_cost_year_one_STEM <- school_board_planning + 
      garden_establishment_costs + 
      STEM_establishment_costs 
    
    # Maintenance costs ####
    
    # maintenance costs for the garden (with or without STEM)
    garden_maintenance_cost <-
      maintaining_labor + # technical staff etc
      # 2-3 hours per day to manage a garden of this rough size
      seed_costs + # seeds and seedlings each year
      fertilizer + # EM and other helpers for compost
      plant_protection  # IPM for plant protection
    
    # garden maint. cost values based on garden land area 
    # gardens will be equally expensive to maintain (see above)
    if (size_of_garden > expensive_garden_size) {
      # if the garden size is large then increase the establishment costs
      # increase by variable cost_increase_expensive_garden_size
      garden_maintenance_cost <- garden_maintenance_cost * cost_increase_expensive_garden_size
    } else {
      garden_maintenance_cost <- garden_maintenance_cost  
    }
    
    ## maintenance costs if with STEM education
    STEM_maintenance_cost <-
      teacher_salary_cost +  # extra costs for teachers to work on the garden
      annual_teacher_training + # annual teacher training 12 days on 6 subjects
      # low because it is run by the teachers who have already been trained
      teaching_equipment_annual + # reagents, colors, paper, apps
      teaching_tools # children's garden tools, gloves, hoes, basket etc.
    
    # annual maintenance costs if passive (no STEM) education
    maintenance_cost_annual <- garden_maintenance_cost +
      # still need children's garden tools, gloves, hoes, basket etc.
      teaching_tools +
      # annual teacher training just for passive garden activity
      annual_teacher_training * 0.1
    
    ## annual maintenance costs if with STEM education
    maintenance_cost_annual_STEM <- garden_maintenance_cost +
      STEM_maintenance_cost
    
    # Add up all annual maintenance costs garden (no STEM)
    total_cost <- vv(maintenance_cost_annual, 
                     var_CV = CV_value_t, 
                     n = number_of_years_c, 
                     relative_trend = inflation_rate) #percentage of increase each year
    
    # Add up all annual maintenance costs garden with STEM
    total_cost_STEM <- vv(maintenance_cost_annual_STEM, 
                          var_CV = CV_value_t, 
                          n = number_of_years_c, 
                          relative_trend = inflation_rate) #percentage of increase each year
    
    # Calculate management plus establishment costs in the first year
    total_cost[1] <- establishment_cost_year_one + 
      maintenance_cost_annual #make sure the first is establishment_cost_year_one
    
    # Calculate management plus establishment costs in the first year with STEM
    total_cost_STEM[1] <- establishment_cost_year_one_STEM + 
      maintenance_cost_annual_STEM
    
    # if including animals garden prices change a bit ####
    # Circular garden with animals, trees, plants, fish (Bac Tom option)
    annual_livestock_cost  <- vv(livestock_maint, 
                                 var_CV = CV_value_t, 
                                 n = number_of_years_c, 
                                 relative_trend = inflation_rate) #percentage of increase each year
    
    if_animals_included_t <- chance_event(if_animals_in_garden_t)
    
    if (if_animals_included_t == 1){
      # costs of establishing animals in the garden (small birds, rabbits, fish)
      total_cost_STEM[1] <- total_cost_STEM[1] + livestock_establishment_costs + fishpond_cost
      total_cost[1] <- total_cost[1] + livestock_establishment_costs + fishpond_cost
    } else {
      total_cost_STEM = total_cost_STEM
      total_cost = total_cost
    }
    # Risks ####
    
    # These are 'ex-ante' risks, or risks understood when making a decision
    # we use these to multiply the values for the relevant benefits
    # the minimum values are effectively a reduction in the benefits
    # used to multiply benefits (by a number 90% likely)
    # not differentiated by passive and STEM education
    
    garden_function_risk <-  min(if_biophysical_good_t, 
                                 if_students_like_t, # damage garden
                                 if_parents_like_t, #  support
                                 if_community_likes_t, #damage garden
                                 if_effective_manage_t) # well managed garden
    
    garden_nutrition_risk <- min(if_students_like_t, # eat veg/change behavior
                                 if_garden_yield_enough_t, # goes to hh and school canteen
                                 if_parents_like_t, #  support and buy garden product
                                 if_garden_healthy_t, # good food from the garden
                                 if_effective_manage_t) # well managed garden
    # ex-ante education risks
    education_risk <- min(if_students_like_t, # pay attention
                          if_teachers_like_t,# teach effectively
                          if_parents_like_t,# Allow students to attend
                          if_effective_teaching_t, # closely related to the next
                          if_effective_training_t) # but possibly non-correlary
    
    # ex-ante community risks
    community_risk <- min(if_parents_like_t, #  support and promote
                          if_community_likes_t, # support and promote
                          if_effective_manage_t) # well managed garden makes good impression
    
    # ex-ante ecological risks
    ecological_risk <- min(if_offer_green_space_t, # offer green space
                           if_reduce_pollution_t) # offer habitat
    
    # Benefits and Risks ####
    
    canteen_yes_no <- chance_event(if_school_has_canteen_t) 
    # private schools have but others not so much
    # this will change under new decrees and nutrition plans
    
    # parents pay for the canteen food / the school will sell to parents
    if (canteen_yes_no == 1) {
      # sell some and eat rest in canteen 
      harvest_value = vv(canteen_savings + sale_of_yield, CV_value_t, 
                         number_of_years_c,
                         #inflation -> percentage of increase each year
                         relative_trend = inflation_rate) * garden_function_risk 
      # account for risk that the garden is not fully functional
    } else {
      # just sell, never eat in the canteen
      # same for no STEM and STEM
      harvest_value = vv(sale_of_yield, CV_value_t, 
                         number_of_years_c, 
                         relative_trend = inflation_rate) * garden_function_risk
    }
    
    # here we get a bit abstract but we do not want to leave this out
    # learning is worth something to us 
    # and we want to make sure this is part of the recommendation
    # we use things like savings on after school programs, tutor-ships,
    # hiring more teaching staff, organizing more events for kids
    # we call this 'extra_cirricular_savings'
    
    # education quality is correlated (highly) to the learning and to 
    # other values like outside investment (i.e. parents invest)
    # and increased enrollment by 
    # creating a good impression and gaining reputation
    
    # The savings are also in formal education 
    # the school meets some of the KPI for education with the garden
    # such as local enterprise and local economics 
    # Ministry decree of edu. to benefit local economy (35 sessions of 45 min./yr)
    # private school has more time than this 
    # this will be applied in the STEM case only 
    
    education_savings <- formal_edu_savings #(not much savings here with no STEM)
    
    education_savings_STEM <- formal_edu_savings_STEM + 
      extra_cirricular_savings
    
    #savings on learning
    learning_value <- vv(education_savings, 
                         CV_value_t, 
                         number_of_years_c, 
                         relative_trend = inflation_rate) * education_risk
    # Not until the 2nd year is the garden expected to start 'running well'
    # thus providing a learning value
    learning_value[1] <- 0
    #savings on learning with STEM education
    learning_value_STEM <- vv(education_savings_STEM, 
                              CV_value_t, 
                              number_of_years_c, 
                              relative_trend = inflation_rate) * education_risk
    
    # The 3rd year is when the STEM education plan will be fully running
    learning_value_STEM[1:2] <- 0
    
    # Reputation ####
    # Reputation for schools, teachers, school board, students…
    # through community building, green running award, 
    # planting trees, environment ecology groups
    # school events in garden connect community, leads to
    # greater access and awareness, positive change in choices around food
    
    # reputation is not important for any older schools
    # only new schools will care about reputation
    # old schools do not need reputation - they already get too many applications they cannot get more students
    # 
    # They do not get investment
    # the school belongs to big companies
    
    #investments from outside
    # i.e. sponsors from local business 
    outside_investment <- vv(outside_investment_value, # related to networking
                             CV_value_t, 
                             number_of_years_c, 
                             relative_trend = inflation_rate) * community_risk
    
    outside_investment_STEM <- vv(outside_investment_value_STEM, # related to networking
                                  CV_value_t, 
                                  number_of_years_c, 
                                  relative_trend = inflation_rate) * community_risk
    
    # Same for STEM and no STEM
    # the community appreciates the garden 
    # they come to the school and take part in school events
    # the school benefits from the event by selling products
    # maybe products from the garden or increased sales of other school products
    community_value <-  vv(school_event_value * school_event_freq, # i.e. seedlings for sale
                           CV_value_t, 
                           number_of_years_c, 
                           relative_trend = inflation_rate) * community_risk
    
    # Increased enrollment ####
    # earnings from increased enrollment without STEM
    increased_enrollment <-  vv(increased_enrollment_value,
                                CV_value_t, 
                                number_of_years_c, 
                                relative_trend = inflation_rate) * education_risk
    
    # Increased enrollment with STEM
    increased_enrollment_STEM <-  vv(increased_enrollment_value,
                                     CV_value_t, 
                                     number_of_years_c, 
                                     relative_trend = inflation_rate) * education_risk
    
    # It takes time to get a good reputation
    # make the first year (unproductive year) 
    # and the second year (year of gaining reputation) zero
    increased_enrollment[1:2] <- 0 
    # with STEM it takes two years to get the fully functioning garden 
    # and thus to gain reputation
    increased_enrollment_STEM[1:3] <- 0 
    
    # Health related values ####
    # These are critical and extremely important but also somewhat intangible
    # here we determine the value of vegetable access with some proxy values
    child_veg_access <- child_veg_health_care_savings + 
      # access to and consumption of safe food (i.e. vegetables) from the garden
      # can lead to better performance
      child_veg_school_performance_value + 
      # value for children having more access to safe vegetables from the garden
      # as it relates to their engagement with the community
      child_veg_community_engagement_value 
    
    # here we determine the value of healthier choices with some proxy values
    child_healthier_choices <- child_garden_health_care_savings + 
      # children with a garden on campus may do better in school
      # here is the expected effect
      child_garden_school_performance_value + 
      child_garden_community_engagement_value  
    
    # Need to consider these values carefully as they differ between options
    # health benefits from gardens no STEM
    health_value <- child_veg_access + 
      child_healthier_choices  + 
      garden_mental_health_value 
    # can be expanded to include more: children, 
    # but also community, teachers, school staff and neighbors...
    
    health_related_value <-  vv(health_value, 
                                CV_value_t, 
                                number_of_years_c, 
                                relative_trend = inflation_rate) * garden_nutrition_risk
    # health benefits from gardens with STEM
    # here we determine the value of healthier choices with some proxy values
    child_healthier_choices_STEM <- child_STEM_health_care_savings + 
      # making better choices about food leads to lower health care costs
      child_STEM_school_performance_value + 
      # for children making better choices about food 
      child_STEM_community_engagement_value  
    # Assuming more formal STEM education time in the garden leads to 
    # better health choices but does not change access (same garden)
    
    health_value_STEM <- child_veg_access + 
      child_healthier_choices_STEM  + 
      garden_mental_health_value
    
    health_related_value_STEM <-  vv(health_value_STEM, 
                                     CV_value_t, 
                                     number_of_years_c, 
                                     relative_trend = inflation_rate) * garden_nutrition_risk
    
    # Here we also get abstract, we care about green space and pollution reduction
    
    environmental_value <- 
      green_space_eco_value + # we care about the green space
      # citizens pay more to live close to green spaces
      # cities like Hanoi spend money on planting and maintaining parks and trees
      # - improve mental and physical health
      # - provide opportunities for physical activity and social interaction
      # - public green spaces have been linked to lower crime rates
      
      reduce_pollution_value 
    # i.e. improved air quality/ filter pollutants from the air
    # improving air quality and reducing the risk of respiratory problems
    # especially important in urban areas, where air pollution is high 
    #   - (March 5th 2024) Hanoi worst air pollution in world
    # cities are willing to invest in green areas for pollution 
    #   - (i.e. moss wall in Stuttgart)
    # Hanoi plants and maintains parks and trees (maybe partly for pollution?)
    # attract insects, birds, and other wildlife, contributing to local biodiversity.
    # beneficial microorganisms, promoting soil fertility and plant health.
    # attract butterflies and birds, enhancing the ecological value of the garden.
    # attract pollinators such as bees and butterflies, supporting the pollination of nearby plants 
    # improved air quality by absorbing pollutants and releasing oxygen.
    # provides an outdoor classroom for students to learn about ecosystems, plant life cycles, and the interdependence of living organisms.
    # encourages sustainable practices, such as composting, water conservation, and organic gardening methods.
    # composting systems, reduce organic waste and promoting the recycling of nutrient-rich materials back into the soil.
    # raise awareness about waste reduction and resource conservation.
    
    # some discussion of carbon credit values (not included)
    # garden ecological value - possible fallow land ecologcal value 
    # assuming it could be a green space
    # low chance since land is needed in the city
    
    environmental_value_of_fallow_green_space <- (green_space_eco_value + 
                                                    reduce_pollution_value) * 
      # fallow green space is less valuable than managed garden green space by x amount
      fallow_eco_reduction_t
    
    # Fallow land also has value to health (no garden health value)
    
    health_value_of_fallow_play_green_space <- (green_space_health_value + 
                                                  reduce_pollution_value) * 
      # fallow space has lower health value than managed garden space by x amount
      fallow_health_reduction_t
    
    fallow_land <- chance_event(chance = chance_garden_is_fallow_green_space_t, 
                                value_if = 1, 
                                value_if_not = 0)
    
    if(fallow_land == 1){
      environment_related_value <-  vv(environmental_value - 
                                         environmental_value_of_fallow_green_space, 
                                       CV_value_t, 
                                       number_of_years_c, 
                                       relative_trend = inflation_rate) * ecological_risk
    }else{
      environment_related_value <-  vv(environmental_value, 
                                       CV_value_t, 
                                       number_of_years_c, 
                                       relative_trend = inflation_rate) * ecological_risk
      # overwrite for chance / use as 'No garden' results
      environmental_value_of_fallow_green_space <- 0 
      health_value_of_fallow_play_green_space <- 0
    }
    
    # Add up all benefits ####
    total_benefit <- harvest_value + learning_value + 
      outside_investment + increased_enrollment + 
      # health_related_value + environment_related_value + 
      community_value
    
    # Add up all benefits with STEM ####
    total_benefit_STEM <- harvest_value + learning_value_STEM + 
      outside_investment_STEM + increased_enrollment_STEM + 
      # health_related_value_STEM + environment_related_value + 
      community_value
    
    
    ## Risks for Public Schools ####
    # Access to land 
    # unlikely to have access to land
    # many schools that attended CODAS meetings (follow up to the first workshop) 
    # did not have access to land
    stop_garden_no_land <- chance_event(1-land_access_t)
    
    # no benefits if public schools meet these challenges
    
    ## If no land then very little or no costs 
    ## just meetings and other small things
    ## also no benefits
    if (stop_garden_no_land == 1) {
      # no benefits from the garden
      total_benefit_public_school <- rep(0, number_of_years_c)
      total_cost_public_school <- rep(0, number_of_years_c)
      # no benefits from STEM
      total_benefit_STEM_public_school <- rep(0, number_of_years_c)
      total_cost_STEM_public_school <- rep(0, number_of_years_c)
      environment_related_value <- rep(0, number_of_years_c)
      health_related_value <- rep(0, number_of_years_c)
      health_related_value_STEM <- rep(0, number_of_years_c)
    } else {
      # costs and benefits are the same
      total_benefit_public_school <- total_benefit
      total_cost_public_school <- total_cost
      total_benefit_STEM_public_school <- total_benefit_STEM
      total_cost_STEM_public_school <- total_cost_STEM
      environment_related_value <- environment_related_value
      health_related_value <- health_related_value
      health_related_value_STEM <- health_related_value_STEM
    }
    
    # the land they have access to is just cement part of playground
    # or otherwise turns out to be unsuitable (rocks, marsh etc.)
    stop_garden_unsuitable_land <- chance_event(1-suitability_of_land_for_garden_t)
    
    # many of the schools (especially public schools) can be overwhelmed with bureaucracy
    # CODAS was unable to overcome the bureaucracy hurdles 
    # We (CODAS and NIFAM) were unable to partner with public schools
    stop_garden_beurocratic_barriers <- chance_event(beurocratic_barriers_t)
    
    ## if land turns out to be unsuitable after some investment 
    ## or bureaucratic barriers permit the use of the garden 
    ## then establishment costs are incurred
    ## but there are no returns 
    if (stop_garden_unsuitable_land == 1 | 
        stop_garden_beurocratic_barriers == 1) {
      # no benefits from the garden
      total_benefit_public_school <- rep(0, number_of_years_c)
      # costs only in year 1
      total_cost_public_school <- (total_cost[2:number_of_years_c]<-0)
      # no benefits from STEM
      total_benefit_STEM_public_school <- rep(0, number_of_years_c)
      # costs only in year 1
      total_cost_STEM_public_school <- (total_cost_STEM[2:number_of_years_c]<-0)
    } else {
      # costs and benefits are the same
      total_benefit_public_school <- total_benefit
      total_cost_public_school <- total_cost
      total_benefit_STEM_public_school <- total_benefit_STEM
      total_cost_STEM_public_school <- total_cost_STEM
    }
    
    # Final result of the costs and benefits no STEM
    garden_result <- total_benefit - total_cost
    
    # Final result of the costs and benefits STEM
    garden_result_STEM <- total_benefit_STEM - total_cost_STEM
    
    # Final result of the costs and benefits no STEM at public school
    garden_result_public_school <- total_benefit_public_school - total_cost_public_school
    
    # Final result of the costs and benefits STEM at public school
    garden_result_STEM_public_school <- total_benefit_STEM_public_school - total_cost_STEM_public_school
    
    # Alternative use of garden space ####
    ## land-use result = all costs and benefits
    # These are opportunity costs
    # i.e. the value of the next-highest-valued alternative use of the garden space
    
    # the above-board earnings from parking (much will be under the table)
    parking_on_campus <- chance_event(if_parking_t) # some above the table (mostly under the table)
    
    if (parking_on_campus == 1) {
      non_garden_value <- vv(value_of_non_garden_land_use + parking_value, 
                             # this is a contentious issue with a lot of discussion
                             # keeping a low value and low chance for now
                             CV_value_t, 
                             number_of_years_c, 
                             relative_trend = inflation_rate) 
    } else {
      non_garden_value <- vv(value_of_non_garden_land_use, #i.e. cost of other playground
                             CV_value_t, 
                             number_of_years_c, 
                             relative_trend = inflation_rate)
    }
    
    
    total_benefit_no <- vv(non_garden_value + # income loss of playground, parking etc.
                             school_board_planning, # time savings for board
                           var_CV = CV_value_t, 
                           n = number_of_years_c, 
                           relative_trend = inflation_rate)
    
    total_cost_no <- vv(costs_of_non_garden_land_use, # cleaning playground, managing parking etc.
                        var_CV = CV_value_t, 
                        n = number_of_years_c, 
                        relative_trend = inflation_rate)
    
    # subtract costs from benefits
    
    no_garden_result <- total_benefit_no - total_cost_no
    
    # NPV calculations
    # calculate the Net Present Value (NPV) with with the specified discount rate
    # the values include expected inflation so the discount already includes this
    # These represent the expected wins over doing nothing
    
    # NPV no intervention ####
    NPV_no_garden <-
      discount(x = no_garden_result, 
               discount_rate = discount_rate, 
               calculate_NPV = TRUE)
    
    # By including `- no_garden_result` we are calculating the expected gains
    
    NPV_garden <-
      discount(x = garden_result - no_garden_result, 
               discount_rate = discount_rate, 
               calculate_NPV = TRUE)
    
    NPV_garden_STEM <-
      discount(x = garden_result_STEM - no_garden_result, 
               discount_rate = discount_rate, 
               calculate_NPV = TRUE)
    
    NPV_garden_public_school <-
      discount(x = garden_result_public_school - no_garden_result, 
               discount_rate = discount_rate, 
               calculate_NPV = TRUE)
    
    NPV_garden_STEM_public_school <-
      discount(x = garden_result_STEM_public_school - no_garden_result, 
               discount_rate = discount_rate, 
               calculate_NPV = TRUE)
    
    biodiversity <- sum(environment_related_value) #assume the same ecology value
    health <- sum(health_related_value) 
    health_STEM <- sum(health_related_value_STEM)
    
    ### END of garden model script ####
    
    # Beware, if we do not name our outputs (left-hand side of the equal sign) 
    # in the return section, the variables will be called output_1, _2, etc.
    return(list(NPV_garden = NPV_garden,
                # comparative results do - do nothing
                NPV_garden_STEM = NPV_garden_STEM,
                NPV_garden_public_school = NPV_garden_public_school,
                NPV_garden_STEM_public_school = NPV_garden_STEM_public_school,
                biodiversity = biodiversity,
                health = health,
                health_STEM = health_STEM,
                # others
                total_costs = sum(total_cost),
                total_costs_STEM = sum(total_cost_STEM),
                Cashflow_garden = garden_result, 
                Cashflow_garden_STEM = garden_result_STEM, 
                Cashflow_garden_public = garden_result_public_school, 
                Cashflow_garden_STEM_public = garden_result_STEM_public_school))
  }
  
  # simulation ####    
 garden_simulation_results <- reactive({

    mcSimulation(
      estimate = as.estimate(input_estimates()),  # Pass the modified object
      model_function = school_garden_function,
      numberOfModelRuns = 100, # change later to Run 1e4 simulations
      functionSyntax = "plainNames"
    )
  })
 # Basic table output
  output$output_table_x <- renderTable({
    garden_simulation_results()$x
  })
 # Basic table output
  output$output_table_y <- renderTable({
    garden_simulation_results()$y
  })
 output$distPlot1 <- renderPlot({
    
    
    garden_data_long <- garden_simulation_results()$y %>%
      tidyr::pivot_longer(cols = c("NPV_garden_public_school", "NPV_garden_STEM_public_school"),
                          names_to = "name", values_to = "value") %>%
      dplyr::mutate(name = dplyr::recode(name, 
                                         "NPV_garden_public_school" = "public school garden",
                                         "NPV_garden_STEM_public_school" = "public school STEM garden"))
    
    colors <- c("#009999", "#0000FF", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    
    ggplot2::ggplot(garden_data_long, 
                    ggplot2::aes(x = value, group = name, fill = name, color = name)) + 
      ggplot2::geom_density(alpha = 0.1, linewidth = 0.5) +  # Density plot
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.01), labels = scales::comma) + 
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.01), labels = scales::comma) + 
      ggplot2::scale_fill_manual(values = colors) + 
      ggplot2::scale_color_manual(values = colors) + 
      ggplot2::labs(x = "Kết quả NPV so sánh", y = "Mật độ xác suất", fill = "Decision option", color = "Decision option") + 
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = c(0.98, 0.98),  # Legend in top-right corner
        legend.justification = c(1, 1),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
      ) 
    })

  plot2 <- reactive({
    garden_data_long <- garden_simulation_results()$y %>%
      dplyr::mutate(NPV_garden_public_school_group = ifelse(NPV_garden_public_school < 0, "Không nên làm", "Nên làm"),
                    NPV_garden_STEM_public_school_group = ifelse(NPV_garden_STEM_public_school < 0, "Không nên làm", "Nên làm")
      ) %>%
      tidyr::pivot_longer(
        cols = c(NPV_garden_public_school_group, NPV_garden_STEM_public_school_group),
        names_to = "name",
        values_to = "group"
      ) %>%
      dplyr::mutate(name = dplyr::recode(name, 
                                         "NPV_garden_public_school_group" = "vườn trường công \nkhông có STEM",
                                         "NPV_garden_STEM_public_school_group" = "vườn trường công \nvới STEM")
      )%>%
      dplyr::mutate(name = factor(name, levels = c("vườn trường công \nkhông có STEM", "vườn trường công \nvới STEM"))) %>%
      dplyr::group_by(name, group) %>%
      dplyr::summarise(
        count = n(),
        percent = round(100 * n() / nrow(garden_simulation_results()$y), 1),
        .groups = "drop"
      )
    
    # Plot
    ggplot2::ggplot(garden_data_long, aes(x = name, y = percent, fill = group)) + 
      ggplot2::geom_bar(stat = "identity",color = "white", alpha = 0.3,
                        position = position_stack(reverse = TRUE)) +  # Use "stack" instead of "fill"
      ggplot2::geom_text(aes(label = paste0(round(percent, 1), "%")), 
                         position = position_stack(vjust = 0.5,reverse = TRUE), 
                         size = 5, color = "black") +  # Add percentage labels
      ggplot2::scale_fill_manual(values = c("Nên làm" = "blue", "Không nên làm" = "red")) +
    
      ggplot2::coord_flip() +  
      ggplot2::theme_minimal() + 
      ggplot2::theme(
        axis.title.y = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x = element_blank(),
        legend.position = "bottom", 
        legend.justification = "center",
        legend.direction = "horizontal",  # Make the legend horizontal
        legend.box.spacing = unit(0.5, "cm")
      ) +
      ggplot2::labs(fill = "Phán quyết")
  })
  
  # Basic table output
  output$garden_data_long <- renderTable({
    garden_data_long <- garden_simulation_results()$y %>%
      dplyr::mutate(NPV_garden_public_school_group = ifelse(NPV_garden_public_school < 0, "No", "Yes"),
                    NPV_garden_STEM_public_school_group = ifelse(NPV_garden_STEM_public_school < 0, "No", "Yes")
      ) %>%
      tidyr::pivot_longer(
        cols = c(NPV_garden_public_school_group, NPV_garden_STEM_public_school_group),
        names_to = "name",
        values_to = "group"
      ) %>%
      dplyr::mutate(name = dplyr::recode(name, 
                                         "NPV_garden_public_school_group" = "public school \ngarden",
                                         "NPV_garden_STEM_public_school_group" = "public school \nSTEM garden")
      )%>%
      dplyr::mutate(name = factor(name, levels = c("public school \ngarden", "public school \nSTEM garden"))) %>%
      dplyr::group_by(name, group) %>%
      dplyr::summarise(
        count = n(),
        percent = round(100 * n() / nrow(garden_simulation_results()$y), 1),
        .groups = "drop"
      )
  })
  
  
  plot3 <- reactive({
    decisionSupport::plot_cashflow(mcSimulation_object = garden_simulation_results(), 
                                   cashflow_var_name = "Cashflow_garden_public", 
                                   facet_labels = "Public school garden",
                                   y_axis_name = "Dòng tiền (million VND/year)",
                                   x_axis_name = "Dòng thời gian can thiệp",
                                   legend_name = "tứ phân vị (%)",
                                   legend_labels = c("5 to 95 %", "25 to 75 %", "median")) + 
      theme(legend.margin = margin(0.2,1,0.2,1),
            #legend.title = element_blank(), # remove title = remove space between 25-75% and median
            legend.position = c(0.17, 0.98),  # Legend in top-right corner
            legend.justification = c(1, 1),
            legend.box.background = element_rect(fill = "white"),
            legend.spacing.y = unit(0, "pt"), # spacing between two legends
            legend.key.height = unit(0.6, "cm"),  # Reduces the height of legend keys
            legend.text = element_text(size = 8)
            #legend.box.spacing= unit(0, "pt") # space between plot and legend
      )
      
  })
  
  plot4 <- reactive({
    decisionSupport::plot_cashflow(mcSimulation_object = garden_simulation_results(), 
                                   cashflow_var_name = "Cashflow_garden_STEM_public", 
                                   facet_labels = "Public school STEM garden",
                                   y_axis_name = "Dòng tiền (million VND/year)",
                                   x_axis_name = "Dòng thời gian can thiệp",
                                   legend_name = "tứ phân vị (%)",
                                   legend_labels = c("5 to 95 %", "25 to 75 %", "median")) + 
      theme(legend.margin = margin(0.2,1,0.2,1),
            #legend.title = element_blank(), # remove title = remove space between 25-75% and median
            legend.position = c(0.17, 0.98),  # Legend in top-right corner
            legend.justification = c(1, 1),
            legend.box.background = element_rect(fill = "white"),
            legend.spacing.y = unit(0, "pt"), # spacing between two legends
            legend.key.height = unit(0.6, "cm"),  # Reduces the height of legend keys
            legend.text = element_text(size = 8)
            #legend.box.spacing= unit(0, "pt") # space between plot and legend
      )
  })
  
  
  
  ### Render the plots using the reactive expressions

  output$distPlot2 <- renderPlot({
    plot2()
  })
  output$distPlot3 <- renderPlot({
    plot3()
  })
  output$distPlot4 <- renderPlot({
    plot4()
  })
  
  ### Helper function to create download handlers
  createDownloadHandler <- function(plot_reactive, filename_prefix) {
    downloadHandler(
      filename = function() {
        paste(filename_prefix, format( Sys.Date(), "%Y-%m-%d"), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot_reactive(), device = "png")
      }
    )
  }
  
  
  ### Create download handlers for each plot
  output$save_plot1 <- createDownloadHandler(plot1, "Plot_comparison_outcome")
  output$save_plot2 <- createDownloadHandler(plot2, "Plot_comparison_outcome")
  output$save_plot3 <- createDownloadHandler(plot3, "Plot_Cashflow")
  output$save_plot4 <- createDownloadHandler(plot4, "Plot_Cashflow_STEM")
  
  
  # Display data frame 
  # Basic table output
  output$simple_table <- renderTable({
    input_estimates_table()
  })
  
  #Create a data save and download option ####
  
  input_estimates_table <- reactive({
    
    # variables: input ID
    variables <- names(input)
    
    
    lower_values <- sapply(variables, function(var) {
      value <- input[[var]]
      
      # Apply logic based on the slider ranges
      if (length(value) == 1) {
        as.numeric(value)
      } else if (grepl("_t$", var)) {  # Check if variable name ends with "_t"
        as.numeric(value[1])  / 100  # Special logic for sliders with names ending in "_t"
      } else if (grepl("_1$", var)) {
        NULL  # Default behavior
      } else {
        as.numeric(value[1])   # Default behavior
      }
    })
    
    
    upper_values <- sapply(variables, function(var) {
      value <- input[[var]]
      
      # Apply logic based on the slider ranges
      if (length(value) == 1) {
        as.numeric(value)
      } else if (grepl("_t$", var)) {  # Check if variable name ends with "_t"
        as.numeric(value[2]) / 100  # Special logic for sliders with names ending in "_t"
      } else if (grepl("_1$", var)) {
        NULL   # Default behavior
      }else {
        as.numeric(value[2])  # Default behavior
      }
    })
    
    # distributions    
    distributions <- sapply(variables, function(var) {
      value <- input[[var]]
      
      if (grepl("_t$", var)) {
        "tnorm_0_1"  # Special logic for variables ending with "_t"
      } else if (grepl("_c$", var)) {  # Check if variable name ends with "_t"
        "const"  # Special logic for sliders with names ending in "_t"
      } else if (grepl("_1$", var)) {
        as.character(value)   # Default behavior
      } else {
        "posnorm"  # Default behavior
      }
    })
    
    
    
    # Create a data frame from the results
    data.frame(
      variable = variables,
      lower = lower_values, 
      upper = upper_values, 
      distribution = distributions, 
      stringsAsFactors = FALSE)
  })
  
  # Basic table output
  output$simple_table <- renderTable({
    input_estimates_table()
  })
  
  output$saveDownload <- downloadHandler(            # Create the download file name
    filename = function() {
      paste("NIFAM-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
    
      write.csv(input_estimates_table(), file)               
    })                          
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
