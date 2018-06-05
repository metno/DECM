library(shiny)
library(DECM)
library(fields)
source("helpers.R")

## Load statistics calculated with script 'calculate_statistics.R'
stats <- NULL
data("statistics.cmip.era.tas.1981-2010")
stats$tas$present <- store
data("statistics.cmip.tas.2021-2050")
stats$tas$nf <- store
data("statistics.cmip.tas.2071-2100")
stats$tas$ff <- store
data("statistics.cmip.era.pr.1981-2010")
stats$pr$present <- store
data("statistics.cmip.pr.2021-2050")
stats$pr$nf <- store
data("statistics.cmip.pr.2071-2100")
stats$pr$ff <- store

## Load metadata
data("metaextract")
im <- meta$project_id=="CMIP5" & meta$var=="tas"
gcmnames <- paste(seq(sum(im)),": ",meta$gcm[im],".",meta$gcm_rip[im],sep="")

## Function 'regions' is defined in helpers.R
srex <- regions("srex")

## Load geographical data for map
data("geoborders",envir=environment())

## Define a server for the Shiny app
shinyServer(function(input, output) {
		      
		      ##Weighted metrics calculator
		      #Maybe: add check box to use same weights as in spread calculator
		      output$MetricText  <- renderText({
			          
			          #gcm names
			          gcms <- names(stats$tas$ff)
				      
				      #reference data
				      tasref <- names(stats$tas$present)[!grepl("gcm",names(stats$tas$present))]
				      prref <- names(stats$pr$present)[!grepl("gcm",names(stats$pr$present))]
				          
				          #selected gcms
				          im <- as.numeric(gsub(":.*","",input$gcms))
				          
				          #user selected focus regions
				          Regionlist <- list(input$regionwm1,input$regionwm2)
					      Regions <- Regionlist[which(Regionlist != "---")]#Only those different from "---"
					      
					      #model ranks for seasons, metrics and selected focus regions
					      tasRanks <- array(NA,c(length(gcms),5,4,length(Regions)))
					          prRanks <- array(NA,c(length(gcms),5,4,length(Regions)))
					          
					          Seasons <- list("ann",c('dec','jan','feb'),c('mar','apr','may'),c('jun','jul','aug'),c('sep','oct','nov'))
						      Periods <- list("nf","ff")
						      
						      for (si in 1:5)
							          {
									        for (ri in 1:length(Regions))
											      {
												              if(tolower(Regions[[ri]])=="global") {
														                region <- "global"
						              } else {
								                i.srex <- which(srex$name==Regions[[ri]])
							                region <- srex$label[i.srex]
									        }
						              
						              #bias
						              tasRanks[,si,1,ri] <- rank(abs(sapply(gcms, function(gcm) mean(sapply(Seasons[[si]], function(s)
																              stats$tas$present[[gcm]][[region]][["mean"]][[s]]-stats$tas$present[[tasref]][[region]][["mean"]][[s]])))))
							              prRanks[,si,1,ri] <- rank(abs(sapply(gcms, function(gcm) mean(sapply(Seasons[[si]], function(s)
																	             stats$pr$present[[gcm]][[region]][["mean"]][[s]]*60*60*24-stats$pr$present[[prref]][[region]][["mean"]][[s]]*1E3)))))
							              
							              #sd.ration
							              tasRanks[,si,2,ri] <- rank(abs(1-sapply(gcms, function(gcm) mean(sapply(Seasons[[si]], function(s)
																	                stats$tas$present[[gcm]][[region]][["spatial.sd"]][[s]]/stats$tas$present[[tasref]][[region]][["spatial.sd"]][[s]])))))
								              prRanks[,si,2,ri] <- rank(abs(1-sapply(gcms, function(gcm) mean(sapply(Seasons[[si]], function(s)
																		               stats$pr$present[[gcm]][[region]][["spatial.sd"]][[s]]*60*60*24/stats$pr$present[[prref]][[region]][["spatial.sd"]][[s]]/1E3)))))
								              
								              #corr
								              tasRanks[,si,3,ri] <- rank(1-sapply(gcms, function(gcm) mean(sapply(Seasons[[si]], function(s)
																		            stats$tas$present[[gcm]][[region]][["corr"]][[s]]))))
									              prRanks[,si,3,ri] <- rank(1-sapply(gcms, function(gcm) mean(sapply(Seasons[[si]], function(s)
																			           stats$pr$present[[gcm]][[region]][["corr"]][[s]]))))
									              
									              #CMPI (combined model performance index)
									              tasRanks[,si,4,ri] <- rank(-1*sapply(gcms, function(gcm) mean(sapply(Seasons[[si]], function(s)
																			             stats$tas$present[[gcm]][[region]][["e"]]))))
										              prRanks[,si,4,ri] <- rank(-1*sapply(gcms, function(gcm) mean(sapply(Seasons[[si]], function(s)
																				            stats$pr$present[[gcm]][[region]][["e"]]))))
										              
										            }
						          }
						          
						          
						          #seasonal ranks, weighted for temp and precip
						          seas_varweightedranks <- as.numeric(input$wmdt)*tasRanks+as.numeric(input$wmdp)*prRanks
						          
						          #seasonal weights
						          seasweightvec <- as.numeric(c(input$wmann,input$wmdjf,input$wmmam,input$wmjja,input$wmson))
							      
							      #region weights
							      regweightvecin <- as.numeric(c(input$wmreg1,input$wmreg2))
							      regweightvec <- regweightvecin[which(Regionlist != "---")]#Only those where region different from "---"
							          
							          #calculate weighted ranks for selection
							          weightedranks_all <- array(NA,c(length(gcms),4))
							          
							          for (mod in 1:length(gcms))
									      {
										            for (score in 1:4)
												          {
														          weightedranks_all[mod,score] <- (seasweightvec %*% seas_varweightedranks[mod,,score,] %*% regweightvec)
								        }
								      }
								      weightedranks_all <- apply(weightedranks_all,2,rank)
								      
								      
								      #weight metrics
								      metweightvec <- as.numeric(c(input$wmbias,input$wmsd,input$wmsc,input$wmcmpi))
								          weightedrank_all <- rank(weightedranks_all %*% metweightvec)
								          weightedrank <- weightedrank_all[im]
									      
									      mean_weightedrank <- mean(weightedrank,na.rm=T)
									      
									      ###    Weighted spread calculations
									      #total ensemble and model selection spread for seasons, periods and selected focus regions
									      dtasSpread <- array(NA,c(5,2,length(Regions)))
									          dprSpread <- array(NA,c(5,2,length(Regions)))
									          dtasSelSpread <- array(NA,c(5,2,length(Regions)))
										      dprSelSpread <- array(NA,c(5,2,length(Regions)))
										      
										      Seasons <- list("ann",c('dec','jan','feb'),c('mar','apr','may'),c('jun','jul','aug'),c('sep','oct','nov'))
										          Periods <- list("nf","ff")
										          
										          for (si in 1:5)
												      {
													            for (peri in 1:2)
															          {
																	          for (ri in 1:length(Regions))
																			          {
																					            if(tolower(Regions[[ri]])=="global") {
																							                region <- "global"
											            } else {
													                i.srex <- which(srex$name==Regions[[ri]])
												                region <- srex$label[i.srex]
														          }
											            
											            dtasSpread[si,peri,ri] <- diff(range(sapply(gcms, function(gcm) mean(sapply(Seasons[[si]], function(s)
																						            stats$tas[[Periods[[peri]]]][[gcm]][[region]][["mean"]][[s]])) - 
																		              mean(sapply(Seasons[[si]], function(s) 
																					                  stats$tas$present[[gcm]][[region]][["mean"]][[s]]))),na.rm=T))
												              dprSpread[si,peri,ri] <- diff(range(sapply(gcms, function(gcm) mean(sapply(Seasons[[si]], function(s)
																							             stats$pr[[Periods[[peri]]]][[gcm]][[region]][["mean"]][[s]])) - 
																			               mean(sapply(Seasons[[si]], function(s) 
																						                   stats$pr$present[[gcm]][[region]][["mean"]][[s]]))),na.rm=T))*60*60*24
												              
												              
												              dtasSelSpread[si,peri,ri] <- diff(range(sapply(gcms, function(gcm) mean(sapply(Seasons[[si]], function(s)
																							                 stats$tas[[Periods[[peri]]]][[gcm]][[region]][["mean"]][[s]])) - 
																			                   mean(sapply(Seasons[[si]], function(s) 
																						                       stats$tas$present[[gcm]][[region]][["mean"]][[s]])))[im],na.rm=T))
													                dprSelSpread[si,peri,ri] <- diff(range(sapply(gcms, function(gcm) mean(sapply(Seasons[[si]], function(s)
																								                  stats$pr[[Periods[[peri]]]][[gcm]][[region]][["mean"]][[s]])) - 
																				                    mean(sapply(Seasons[[si]], function(s) 
																								                stats$pr$present[[gcm]][[region]][["mean"]][[s]])))[im],na.rm=T))*60*60*24
													              }
											        }
											      }
											      
											      #seasonal, relative model selection spread
											      dtasRelSpread <- dtasSelSpread/dtasSpread
											      dprRelSpread <- dprSelSpread/dprSpread
											          
											          #seasonal relative spread, weighted for temp and precip
											          seas_varweightedspread <- (as.numeric(input$wmdt)*dtasRelSpread+as.numeric(input$wmdp)*dprRelSpread)/(as.numeric(input$wmdt)+as.numeric(input$wmdp))
											          
											          #seasonal weights
											          seasweightvec <- as.numeric(c(input$wmann,input$wmdjf,input$wmmam,input$wmjja,input$wmson))
												      
												      #region weights
												      regweightvecin <- as.numeric(c(input$wmreg1,input$wmreg2))
												      regweightvec <- regweightvecin[which(Regionlist != "---")]#Only those where region different from "---"
												          
												          #calculate weighted spread
												          weightedspread_nf <- (seasweightvec %*% seas_varweightedspread[,1,] %*% regweightvec)/sum(seasweightvec)/sum(regweightvec)
												          weightedspread_ff <- (seasweightvec %*% seas_varweightedspread[,2,] %*% regweightvec)/sum(seasweightvec)/sum(regweightvec)
													      
													      #output
													      legcolsrank=two.colors(n=107,start="green",end="red",middle = "orange") #colors for ranks
													      meanRelMetricsIndx <- floor(mean_weightedrank) #color index based on weighted rank
													          
													          legcols=two.colors(n=11,start="red",end="green",middle = "orange") #colors for percentage number
													          meanRelSpreadIndx_nf <- floor(weightedspread_nf*10)+1 #color index based on weighted mean rel. spread for near future
														      meanRelSpreadIndx_ff <- floor(weightedspread_ff*10)+1 #color index based on weighted mean rel. spread for far future
														      
														      paste("The mean weighted model performance rank of the selected models is ","<font size = +1, font color=\"",legcolsrank[meanRelMetricsIndx],"\"><b> Rank", round(mean_weightedrank), "of",length(gcms),"</b></font>.
															              <br> (Ranks ",paste(round(weightedrank),collapse="/")," for the single models.)
																                <br> The best performing 11 models would be:",paste(order(weightedrank_all)[1:11],collapse="/"),
																                "<br><br><br> For the far future, the weighted mean spread of the selected models is","<font size = +1, font color=\"",legcols[meanRelSpreadIndx_ff],"\"><b>", round(weightedspread_ff*100), "%</b>
																		          </font> of the spread coverd by the whole model ensemble.
																		          For the near future it is <font size = +1, font color=\"",legcols[meanRelSpreadIndx_nf],"\"><b>", round(weightedspread_nf*100), "%</b></font>."
																			            ,"<br> Use the scatterplot tool below to check the projected changes within the whole ensemble for a specific region, season and time-line. Add more models at the edges to increase the realtive spread.")
														          
														        })
		        
		        
		        ##map
		        output$mapm1 <- renderPlot({
				    if(tolower(input$regionwm1)=="global") {
					          region <- list(lon=c(-180,-180,180,180,-180),lat=c(-90,90,90,-90,-90))
				        } else {
						      i.srex <- which(srex$name==input$regionwm1)
					      region <- list(lon=srex$coord[[i.srex]][1,],
							                          lat=srex$coord[[i.srex]][2,])
					          }
				        par(mgp=c(1,0.5,0),mar=c(0.2,0.2,0.2,0.2))
				        plot(geoborders$x,geoborders$y,col="grey30",type="l",lwd=0.5,
					              xlim=c(-180,180),ylim=c(-90,90),
						               xlab="Longitude",ylab="Latitude",xaxt="n",yaxt="n")
					    par(xaxt="s",yaxt="s",las=1,col.axis='grey',col.lab='grey20',
						        cex.lab=0.7,cex.axis=0.7)
					    axis(3,at=pretty(par("xaxp")[1:2],n=5),col='grey50')
					        axis(2,at=pretty(par("yaxp")[1:2],n=5),col='grey50')
					        grid()
						    lines(region$lon,region$lat,col="blue",lwd=1.5,lty=1)
						  }, width=200,height=200*0.6)#width=250, height=175)
		        
		        ##map
		        output$mapm2 <- renderPlot({
				    if(input$regionwm2 != "---"){
					          if(tolower(input$regionwm2)=="global") {
							          region <- list(lon=c(-180,-180,180,180,-180),lat=c(-90,90,90,-90,-90))
				          } else {
						          i.srex <- which(srex$name==input$regionwm2)
					          region <- list(lon=srex$coord[[i.srex]][1,],
								                        lat=srex$coord[[i.srex]][2,])
						        }
				          par(mgp=c(1,0.5,0),mar=c(0.2,0.2,0.2,0.2))
					        plot(geoborders$x,geoborders$y,col="grey30",type="l",lwd=0.5,
						                xlim=c(-180,180),ylim=c(-90,90),
								           xlab="Longitude",ylab="Latitude",xaxt="n",yaxt="n")
					        par(xaxt="s",yaxt="s",las=1,col.axis='grey',col.lab='grey20',
						              cex.lab=0.7,cex.axis=0.7)
						      axis(3,at=pretty(par("xaxp")[1:2],n=5),col='grey50')
						      axis(2,at=pretty(par("yaxp")[1:2],n=5),col='grey50')
						            grid()
						            lines(region$lon,region$lat,col="blue",lwd=1.5,lty=1)
							        }}, width=200,height=200*0.6)#width=250, height=175)
			  
			  ##map
			  output$map <- renderPlot({
				      if(tolower(input$region)=="global") {
					            region <- list(lon=c(-180,-180,180,180,-180),lat=c(-90,90,90,-90,-90))
				          } else {
						        i.srex <- which(srex$name==input$region)
					        region <- list(lon=srex$coord[[i.srex]][1,],
							                            lat=srex$coord[[i.srex]][2,])
						    }
				          par(mgp=c(1,0.5,0),mar=c(0.2,0.2,0.2,0.2))
				          plot(geoborders$x,geoborders$y,col="grey30",type="l",lwd=0.5,
					                xlim=c(-180,180),ylim=c(-90,90),
							         xlab="Longitude",ylab="Latitude",xaxt="n",yaxt="n")
					      par(xaxt="s",yaxt="s",las=1,col.axis='grey',col.lab='grey20',
						          cex.lab=0.7,cex.axis=0.7)
					      axis(3,at=pretty(par("xaxp")[1:2],n=5),col='grey50')
					          axis(2,at=pretty(par("yaxp")[1:2],n=5),col='grey50')
					          grid()
						      lines(region$lon,region$lat,col="blue",lwd=1.5,lty=1)
						    }, width=200,height=200*0.6)#width=250, height=175)
			  
			  ##Scatterplot of temperature and precip. change 
			  output$dtdpr <- renderPlot({
				      season <- switch(tolower(as.character(input$season)),
						                            'annual mean'='ann',
									                         'winter'=c('dec','jan','feb'),
									                         'spring'=c('mar','apr','may'),
												                      'summer'=c('jun','jul','aug'),
												                      'autumn'=c('sep','oct','nov'))
				          #'annual mean'='ann','winter'='djf','spring'='mam',
				          #'summer'='jja','autumn'='son')
				          #'
				          period <- switch(tolower(as.character(input$period)),
							                        "far future (2071-2100)"='ff',
										                     "near future (2021-2050)"='nf')
				          
				          gcms <- names(stats$tas$ff)
					      
					      if(tolower(input$region)=="global") {
						            region <- "global"
					      } else {
						            i.srex <- which(srex$name==input$region)
					            region <- srex$label[i.srex]
						        }
					      
					      #Temperature and precip. spread
					      dtas <- sapply(gcms, function(gcm) mean(sapply(season, function(s)
											           stats$tas[[period]][[gcm]][[region]][["mean"]][[s]])) - 
							             mean(sapply(season, function(s) 
										           stats$tas$present[[gcm]][[region]][["mean"]][[s]])))
					          dpr <- sapply(gcms, function(gcm) mean(sapply(season, function(s)
												      stats$pr[[period]][[gcm]][[region]][["mean"]][[s]])) - 
								        mean(sapply(season, function(s) 
										              stats$pr$present[[gcm]][[region]][["mean"]][[s]])))
					          
					          #selected gcms
					          im <- as.numeric(gsub(":.*","",input$gcms))
						      
						      #make scatterplot
						      scatterplot(dtas,dpr*(60*60*24),ix=NULL,xlim=input$tlim,ylim=input$plim,
								                  xlab="Temperature change (deg C)",ylab="Precipitation change (mm/day)",
										                  main=paste("Climate change present day (1981-2010) to",tolower(input$period)),
										                  show.legend=FALSE,im=im,legend=seq(length(dtas)),pal=NULL,#"rainbow",
												                  pch=seq(length(dtas)),cex=1.4,lwd=1.5,new=FALSE)
						      
						      #total ensemble spread
						      dprSpread <- diff(range(dpr*(60*60*24),na.rm=T))
						          dtasSpread <- diff(range(dtas,na.rm=T))
						          
						          #model selection spread
						          dprSelSpread <- diff(range(dpr[im]*(60*60*24),na.rm=T))
							      dtasSelSpread <- diff(range(dtas[im],na.rm=T))
							      
							      #add colored legend
							      legcols=two.colors(n=11,start="red",end="green",middle = "orange") #colors for background of legend
							          meanRelSpreadIndx <- floor((dtasSelSpread/dtasSpread+dprSelSpread/dprSpread)/2*10)+1 #color index based on mean rel. spread
							          legend("bottomright",bg=legcols[meanRelSpreadIndx], legend=c("Selection spread:",paste("dT: ",round(dtasSelSpread/dtasSpread*100),"% (",round(dtasSelSpread,1),"°C of total ",round(dtasSpread,1),"°C).",sep=""), paste("dP: ",round(dprSelSpread/dprSpread*100),"% (",round(dprSelSpread,2)," mm/day of total ",round(dprSpread,2)," mm/day).",sep="")),cex=1)
								      
								    }, width=500, height=500)


})

