## Author: Kehao Zhu
## Contributor: Michael Flanagin
## Date Created: 11 April 2019
## Objective: Download most recent AACT database snapshot,
##              Create visuals for SCT 2019 conference talk
## Output:
## Modification Log:
##  04/16/19  MF  First pass comments
##  

install.packages(c("RPostgreSQL", "tidyverse", "lubridate", "survival", "data.table"));

library(RPostgreSQL)
library(tidyverse)
library(lubridate)
library(survival)
library(data.table)
#connecting to the AACT database
drv <-dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",
                 host="aact-db.ctti-clinicaltrials.org", 
                 port=5432, user="kehaoz", password="axio")

#Select relevent variables and creating some trials level summaries from trials that completed.
#Key variable ACT: likely ACT status, which is adopted and modified from Anderson et al. NEJM 2015
Dat<-dbGetQuery(con, 
                             "SELECT  a.nct_id,a.results_first_submitted_date,a.is_fda_regulated_drug, a.is_fda_regulated_device,a.overall_status,
                                      a.primary_completion_date,a.completion_date,a.completion_date_type, a.study_type,a.verification_date,a.phase,
                                      a.plan_to_share_ipd,a.plan_to_share_ipd_description,a.has_dmc,a.disposition_first_submitted_date,
                                      a.is_us_export,d.first_results_pending,dthaeYN,AEasYN,AEtfYN,baseYN,outcomeYN,aeYN,analysisYN,POcount,SOcount,oPOPYN,oPOPcount,
                                      raceYN,basecount,
                                      CASE WHEN (b.us_site=1
                                                AND a.study_type='Interventional' AND riskyInternvention=1 AND
                                                ((a.phase NOT IN ('Early Phase 1','Phase 1','N/A'))
                                                OR (c.primary_purpose != 'Device Feasibility'))) THEN 1 ELSE 0 END AS ACT
                                      FROM studies a
                                      LEFT JOIN (SELECT DISTINCT nct_id, 1 AS us_site 
                                                  FROM Countries WHERE 
                                                  name IN ('United States','American Samoa','Guam','Northern Mariana Islands', 'Puerto Rico','U.S. Virgin Islands') AND removed IS NULL )  b 
                                                  ON a.nct_id=b.nct_id
                                      LEFT JOIN Designs c ON a.nct_id=c.nct_id 
                                      LEFT JOIN (SELECT nct_id,min(event_date) AS first_results_pending 
                                                    FROM Pending_Results GROUP BY nct_id) d ON a.nct_id=d.nct_id
                                      LEFT JOIN (SELECT DISTINCT nct_id, 1 AS riskyInternvention 
                                                  FROM Interventions
                                                  WHERE intervention_type in ('Drug','Diagnostic Test',
                                                  'Device','Biological','Combination Product','Genetic','Radiation')) e ON a.nct_id=e.nct_id
                                      LEFT JOIN (SELECT DISTINCT nct_id, 1 AS dthaeYN FROM Reported_Events 
                                                  WHERE adverse_event_term in ('Total, all-cause mortality','Death','death')) f ON a.nct_id=f.nct_id
                                      LEFT JOIN (SELECT DISTINCT nct_id, 1 AS AEasYN FROM Reported_Events 
                                                  WHERE default_assessment!='') g ON a.nct_id=g.nct_id
                                      LEFT JOIN (SELECT DISTINCT nct_id, 1 AS AEtfYN FROM Reported_Events 
                                                  WHERE time_frame!='') ga ON a.nct_id=ga.nct_id
                                      LEFT JOIN (SELECT DISTINCT nct_id, 1 AS baseYN FROM Baseline_Measurements) h ON a.nct_id=h.nct_id
                                      LEFT JOIN (SELECT nct_id, COUNT(DISTINCT title) AS baseCount FROM Baseline_Measurements GROUP BY nct_id) ha ON a.nct_id=ha.nct_id
                                      LEFT JOIN (SELECT DISTINCT nct_id, 1 AS raceYN FROM Baseline_Measurements 
                                                WHERE title in ('Race/Ethnicity, Customized','Ethnicity (NIH/OMB)','Race (NIH/OMB)')) hb ON a.nct_id=hb.nct_id
                                      LEFT JOIN (SELECT DISTINCT nct_id, 1 AS outcomeYN FROM Outcomes) i ON a.nct_id=i.nct_id
                                      LEFT JOIN (SELECT DISTINCT nct_id, 1 AS oPOPYN FROM Outcomes WHERE population!='') ia ON a.nct_id=ia.nct_id
                                      LEFT JOIN (SELECT nct_id, COUNT(DISTINCT population) AS oPOPcount FROM Outcomes GROUP BY nct_id) ib ON a.nct_id=ib.nct_id
                                      LEFT JOIN (SELECT DISTINCT nct_id, 1 AS aeYN FROM Reported_Events) j ON a.nct_id=j.nct_id
                                      LEFT JOIN (SELECT DISTINCT nct_id, 1 AS analysisYN FROM Outcome_Analyses) k ON a.nct_id=k.nct_id
                                      LEFT JOIN (SELECT nct_id, count(id) AS POcount FROM Outcomes WHERE outcome_type='Primary' GROUP BY nct_id) l ON a.nct_id=l.nct_id
                                      LEFT JOIN (SELECT nct_id, count(id) AS SOcount FROM Outcomes WHERE outcome_type='Secondary' GROUP BY nct_id) m ON a.nct_id=m.nct_id
                                      WHERE a.overall_status IN ('Completed','Terminated')")

#probable funding source adopted from Califf et al. JAMA 2012
funding<-dbGetQuery(con,"SELECT a.nct_id,/*a.agency_class AS lead_class, a.name AS lead_name, b.NIHcol,c.Industrycol, */
                      CASE WHEN (a.agency_class='NIH' OR b.NIHcol=1) THEN 'NIH' 
                           WHEN (a.agency_class='Industry' OR c.Industrycol=1) THEN 'Industry'
                           ELSE 'Other' END AS funding_class
                      FROM Sponsors a
                      LEFT JOIN (select DISTINCT nct_id, 1 AS NIHcol FROM Sponsors WHERE agency_class='NIH') b ON a.nct_id=b.nct_id
                      LEFT JOIN (select DISTINCT nct_id, 1 AS Industrycol FROM Sponsors WHERE agency_class='Industry') c ON a.nct_id=c.nct_id
                      WHERE a.lead_or_collaborator='lead'")

ACT<-as_data_frame(Dat)

## Original data as data.table
TCA <- data.table( Dat )

#derive more variables, including whether result is submitted and time from completion to submission
ACT2<-mutate(ACT,
             were_results_reported=if_else(is.na(first_results_pending)==F,T,is.na(results_first_submitted_date)==F),
             resultdt=if_else(is.na(results_first_submitted_date)==F,results_first_submitted_date,
                             if_else(is.na(first_results_pending)==F,first_results_pending,Sys.Date())),
             completedt=if_else(is.na(primary_completion_date)==F,primary_completion_date,
                               if_else(is.na(completion_date)==F,completion_date,verification_date)),
             completion_yr=format(as.Date(completedt),"%Y"),
             year1_due=as.factor(as.numeric(completion_yr)+1),
             t2result=as.duration(completedt %--% resultdt)/dyears(1),
             t2result_m=ceiling(as.duration(completedt %--% resultdt)/dyears(1)*12),
             socount=if_else(is.na(socount),0,socount),
             pocount=if_else(is.na(pocount),0,pocount)) %>%
  filter(completedt<ymd(Sys.Date()-years(1))) %>% left_join(as_data_frame(funding))

#some summaries
ACT2 %>% filter(completion_yr %in% c(2008:2017)) %>% 
  summarise(totn=n(),actp=sum(act)*100/n(),nactp=sum(act==0)*100/n()) 
ACT2 %>% filter(completion_yr %in% c(2008:2017)) %>% group_by(act) %>%
  summarise(resultp=sum(were_results_reported)*100/n()) 
ACT2 %>% filter(completion_yr %in% c(2008:2017)) %>% group_by(overall_status) %>%
  summarise(n()) 

#summaries by year for trials completed at least one year ago. 
year1_<-ACT2 %>% filter(completedt<ymd(Sys.Date()-years(1)) & completion_yr>=2008) %>%
  group_by(act,completion_yr) %>% 
  summarise(totn=n(),
            resultn=sum(were_results_reported==T),
            yr1result=sum(t2result<=1&were_results_reported==T)/totn * 100,
            m12result=sum(t2result_m<=12&were_results_reported==T)/totn * 100,
            t2result_sum=mean(t2result,na.rm=T),
            dthaep=sum(dthaeyn,na.rm=T)*100/ resultn,
            fda.reg=sum(is_fda_regulated_drug==T|is_fda_regulated_drug==T,na.rm=T),
            fda.reg_NA=sum(is.na(is_fda_regulated_drug)&is.na(is_fda_regulated_drug)),
            export=sum(is_us_export==T, na.rm=T),
            ipd=sum(plan_to_share_ipd=='Yes',na.rm=T),
            ipd_p=sum(plan_to_share_ipd=='Yes',na.rm=T)/totn)

#plot 12 month submission rate by year
ggplot(year1_,aes(y=m12result, x= completion_yr,group=act))+
  geom_point(aes(colour = factor(act),shape =factor(act)))+
  labs(y='results submitted to \nCT.gov within 1 yr (%)',x='year completed')+
  scale_colour_manual(name = "Likely ACT",
                      labels = c("No",'Yes'),
                      values = c("blue", "red")) +   
  scale_shape_manual(name = "Likely ACT",
                     labels = c("No","Yes"),
                     values = c(19, 17))

#cumulative rate by ACT status
ACT_m<-filter(ACT2,completion_yr %in% c(2008:2018))
km1<-survfit(Surv(t2result_m,were_results_reported)~act, data=ACT_m)
plot(km1,xlim=c(0,60),ylim=c(0,60),col=c('blue','red'),fun = function(x) (1-x)*100,
     xlab='Months after Primary Completion Date',ylab='% Trials',
     main='Cumulative Percentage of Clinical Trials \nThat Reported Results to CT.gov',
     lwd=3,axes = FALSE)
axis(side = 1, at = 12*c(0,1,2,3,4,5))
axis(side = 2, at = 10*c(0,1:6))
abline(v=12,lty=3)
legend("topleft", inset=0.01,legend=c("Yes", "No"),
       col=c("red", "blue"),title = 'Likely ACT',lty=c(1,1),lwd=c(3,3))


##among trials with results, were key results summarized  
ACT2 %>% filter(is.na(results_first_submitted_date)==F & completion_yr %in% c(2008:2017)) %>% 
        group_by(act,completion_yr) %>% 
        summarise(resultn=n(),
                  dthaeYNp=sum(dthaeyn,na.rm=T)/n(),
                  AEasYNp=sum(aeasyn,na.rm=T)/n(),
                  baseYNp=sum(baseyn,na.rm=T)/n(),
                  basecount50=quantile(basecount,probs=0.10,na.rm=T),
                  outcomeYNp=sum(outcomeyn,na.rm=T)/n(),
                  aeYNp=sum(aeyn,na.rm=T)/n(),
                  analysisYNp=sum(analysisyn,na.rm=T)/n(),
                  raceYNp=sum(raceyn,na.rm=T)/n(),
                  oPopYNp=sum(opopyn,na.rm=T)/n(),
                  analysisYNp=sum(analysisyn,na.rm=T)/n(),
                  OPopcount50=quantile(opopcount,probs=0.50,na.rm=T),
                  pocount90=quantile(pocount,probs=0.90,na.rm=T)
                  ) 


