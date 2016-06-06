#!/usr/bin/env Rscript
options(scipen = 999)
Rfiles 			<- list.files(pattern = "\\.r", ignore.case = TRUE, full.names = TRUE)

source(grep("common", Rfiles, value = TRUE))



pkg		<- c("XML", "RCurl", "RColorBrewer", "ggplot2", "scales",
			 "jsonlite", "grid", "timeDate", "data.table", "ggthemes",
			 "reshape")	 
Object 			<- new("startUps", pkgs = pkg , Input = c("DATA", "GRAF", "INFO") )

Object$instant_pkgs( )

GRAF		<- file.path(getwd(), "GRAF")
DATA		<- file.path(getwd(), "DATA")
INFO		<- file.path(getwd(), "INFO")

! file.exists("DATA") && dir.create(path = DATA, recursive = TRUE)



DATA			<- file.path(  getwd(), 'Data')
GRAF			<- file.path( getwd(),  "graf")


if( ! file.exists( DATA) ){
	dir.create( DATA, recursive = TRUE, showWarnings = FALSE )
}

if( !file.exists( GRAF) ){
	dir.create( GRAF, recursive = TRUE )
}


    
#' x Fill in the title of legend
GuideCol 	<- function(x, ...)
{
	x1 <- guides(	colour = guide_legend(
					  title = x, 
					   title.position = "top", 
					 title.hjust = 0.5,
					title.theme = element_text(size = 10, angle = 0),
					keywidth 	= unit(15, "mm"), 
					keyheight	= unit(4,"mm"),
					label.position = "bottom",
					label.hjust 	= 0.5,
					label.theme 	=  element_text(size = 9, angle = 0),
								... 
					))
	return(x1 )				
}




url		<- "http://www.modularfinance.se/api/challenges/buy-sell.json"


data1		<- fromJSON(url)
data2		<- data1$data
setDT(data2)

Names		<- colnames(data2)
.char		<- c("quote_date", "paper", "exch")
.num		<- setdiff(Names, .char)



data2		<- cbind(	data2[, lapply(.SD, as.character), .SDcols = .char],   
						data2[, lapply(.SD, as.numeric), .SDcols = .num])

data2[, quote_date := gsub("(\\w{4})(\\w{2})(\\w{2})", "\\1-\\2-\\3", quote_date)]
data2[, quote_date := as.Date(quote_date)]
data2[, ':=' (	Month 	= format(quote_date, "%B"),
				Day		= format(quote_date, "%a")
)]
data2[, ':=' ( 	Dopen = shift(open, n = 1, type = 'lag')/open -1 ,
				Dclose = shift(close, n = 1, type = 'lag')/close -1,
				Daily = close/open -1 )]
data2[is.na(Dopen), Dopen := 0]
data2[is.na(Dclose), Dclose := 0]
data2[order(quote_date, decreasing = FALSE), ':='
	 ( CumDopen = cumprod(1+Dopen), 
		CumDclose = cumprod(1+Dclose),
		CumDaily = cumprod(1+Daily)
	)]


seq1			<- 2:NROW(data2)

seq2			<- data.table( )

for( i in seq1 )
{
	data3			<- copy(data2)
	data3			<- data3[order(quote_date, decreasing = FALSE), ]
	data3[,':='  (d = rollsum(Dopen, i), noDays = .I)]
	rmNr		<- NROW(data3) - (i + 1)
	data3[noDays > rmNr, d := 0]
	nr		<- data3[, which.max(d)]
	Step1	<- data3[nr:(nr + i)][order(quote_date), .SD[c(1,.N), .(TimeFrame = i, quote_date, open)]]
 	Step1[, Return := open[.N]/open[1] - 1]
	Step2	<- data.table(
			TimeFrame = i, 
			From = Step1[, quote_date[1]], 
			To = Step1[, quote_date[.N]], 
			Return = Step1[, Return[.N]]
		)
	

	seq2	<- rbind(seq2, Step2)
}
rm(data3); gc(reset = TRUE)

Date1		<- seq2[which.max(Return),]

FileName	<- file.path( DATA,"Info.RData")		
file.exists(FileName)	 && file.remove(FileName)
			
save( Date1 , file = FileName)		

step1		<- melt.data.table(data2[, .(quote_date, Dopen, Dclose, Daily)], id.vars = 'quote_date')
step2		<- melt.data.table(data2[, .(quote_date, CumDopen, CumDclose, CumDaily)], id.vars = 'quote_date')


Line	<-	ggplot(step1, aes(x = quote_date, y = value, colour = variable, group = variable)) + 
			geom_line(size = 0.9)  +
  			scale_y_continuous( breaks = pretty_breaks(8), labels = percent) +	
  			scale_x_date(	expand = c(0.01,0.01),
							breaks = date_breaks("1 months"),
  							labels = date_format("%Y\n%b")) +
			theme_dark() + 
			theme( legend.position = "bottom", 
					strip.text = element_text(size = 12, face = "bold"),
					axis.text.y = element_text(angle = 0),
					title = element_text(size = 11,vjust = 1)) + 
			facet_wrap( ~ variable, scales = 'free_y', ncol = 2) + 
			GuideCol('Returns')
			
	pdf( file = file.path(GRAF, paste0("Return.pdf")) ,
	     height = unit(7,"cm"), width = unit(11,"cm"),
	     pointsize = 10, colormodel = "rgb", bg = "white")  
		print(Line)		
	dev.off()				
					
			
Line1	<-	ggplot(step2, aes(x = quote_date, y = value*100, colour = variable, group = variable)) + 
			geom_line(size = 0.9)  +
  			scale_y_continuous( breaks = pretty_breaks(8)) +	
  			scale_x_date(	expand = c(0.01,0.01),
							breaks = date_breaks("1 months"),
  							labels = date_format("%Y\n%b")) +
			theme_dark() + 
			theme( legend.position = "bottom", 
					strip.text = element_text(size = 12, face = "bold"),
					axis.text.y = element_text(angle = 0),
					title = element_text(size = 11,vjust = 1)) + 
			facet_wrap( ~ variable, scales = 'free_y', ncol = 2) + 
			GuideCol('Cumulative returns')
			
	pdf( file = file.path(GRAF, paste0("CumReturn.pdf")) ,
	     height = unit(7,"cm"), width = unit(11,"cm"),
	     pointsize = 10, colormodel = "rgb", bg = "white")  
		print(Line1)		
	dev.off()				
	
Bar <- 		ggplot(data2 , aes(  Dopen) )+	
			geom_density(alpha = 0.1, fill = 'red') +		
			scale_x_continuous( expand = c(0.01,0.01),
					 breaks = pretty_breaks(8) ) +
			theme_dark() +	
			ggtitle("Probabilty distribution of open returns")
	
	pdf( file = file.path(GRAF, paste0("dist1.pdf")) ,
	     height = unit(7,"cm"), width = unit(11,"cm"),
	     pointsize = 10, colormodel = "rgb", bg = "white")  
		print(Bar)		
	dev.off()		
	
	
nrMax		<- data2[,which.max(Dopen)]	
data2[(nrMax -1 ):(nrMax +1)]
					
			pdf( file = file.path( GRAF,"zz.pdf") ,
	    	 height = unit(0.15*NROW(Step1),"cm"), width = unit(4.8,"cm"),
	     		 pointsize = 10, colormodel = "rgb", bg = "white")  
			tabelFun(, plot = TRUE )	
		dev.off()			