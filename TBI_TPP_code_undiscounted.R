#Read in dataset
#install.packages("readxl")
library(readxl)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(tidyverse)
library(gridExtra)
library(plyr)
library(dplyr)
library(reshape2)
library(data.table)
library(scales)
library(mosaic)
library(grid)
library(gganimate)
library(av)
library(xlsx)
#####################################################################################################################
#                                               LOAD THE DATA                                                       #
#####################################################################################################################
#Load the micro costing summaries
SA_costs <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Template_SA", col_types = "numeric", range = "A1:T16001")
Brazil_costs <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Template_Brazil", col_types = "numeric", range = "A1:T16001")

#Load South African mathematical model epidemiological outputs
SA_baseline <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "SA_baseline_data", col_types = "numeric", range = "A1:R16001")
SA_scaleup <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "SA_scaleup_data", col_types = "numeric", range = "A1:R16001")
SA_minimal <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "SA_minimal_data", col_types = "numeric", range = "A1:R16001")
SA_optimal <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "SA_optimal_data", col_types = "numeric", range = "A1:R16001")


#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

                                      ##### South Africa #######
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#####################################################################################################################
#                                       baseline ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for baseline dataset
# PT
SA_baseline$C_PT_all<-(SA_baseline$New_PT_all*(SA_costs$LTBI_baseline+SA_costs$AE_LTBI)) # Cost: PT all
SA_baseline$C_PT_hiv_neg<-(SA_baseline$New_PT_all-SA_baseline$New_PT_HIV)*(SA_costs$LTBI_baseline+SA_costs$AE_LTBI) # Cost: PT (HIV negative)
SA_baseline$C_PT_hiv<-(SA_baseline$New_PT_HIV*(SA_costs$LTBI_baseline+SA_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
SA_baseline$C_DSTB_all_DOT<-(SA_baseline$TB_cases_all-SA_baseline$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_baseline$C_DSTB_hiv_neg_DOT<-((SA_baseline$TB_cases_all-SA_baseline$TB_cases_MDR)-(SA_baseline$TB_cases_HIV-SA_baseline$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_baseline$C_DSTB_hiv_DOT<-(SA_baseline$TB_cases_HIV-SA_baseline$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
SA_baseline$C_MDRTB_all_DOT<-(SA_baseline$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_baseline$C_MDRTB_hiv_neg_DOT<-(SA_baseline$TB_cases_MDR-SA_baseline$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_baseline$C_MDRTB_hiv_DOT<-SA_baseline$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
SA_baseline$C_TB_all_DOT<-SA_baseline$C_DSTB_all_DOT+SA_baseline$C_MDRTB_all_DOT # Cost: TB (DS-TB & MRR-TB)
SA_baseline$C_TB_hiv_neg_DOT<-SA_baseline$C_DSTB_hiv_neg_DOT+SA_baseline$C_MDRTB_hiv_neg_DOT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_baseline$C_TB_hiv_DOT<-SA_baseline$C_DSTB_hiv_DOT+SA_baseline$C_MDRTB_hiv_DOT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_baseline$C_TB_all_DOT)
#sum(SA_baseline$C_TB_hiv_neg_DOT)
#sum(SA_baseline$C_TB_hiv_DOT)

#baseline STRATEGY COST: PT & TB (DOT)
SA_baseline$C_baseline_all_DOT<-SA_baseline$C_PT_all+SA_baseline$C_TB_all_DOT #Cost: strategy all (PT+TB) DOT
SA_baseline$C_baseline_hiv_neg_DOT<-SA_baseline$C_PT_hiv_neg+SA_baseline$C_TB_hiv_neg_DOT #Cost: strategy HIV negative (PT+TB) DOT
SA_baseline$C_baseline_hiv_DOT<-SA_baseline$C_PT_hiv+SA_baseline$C_TB_hiv_DOT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       baseline ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
SA_baseline$C_DSTB_all_DOT_SAT<-(SA_baseline$TB_cases_all-SA_baseline$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_baseline$C_DSTB_hiv_neg_DOT_SAT<-((SA_baseline$TB_cases_all-SA_baseline$TB_cases_MDR)-(SA_baseline$TB_cases_HIV-SA_baseline$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_baseline$C_DSTB_hiv_DOT_SAT<-(SA_baseline$TB_cases_HIV-SA_baseline$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
SA_baseline$C_MDRTB_all_DOT_SAT<-(SA_baseline$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_baseline$C_MDRTB_hiv_neg_DOT_SAT<-(SA_baseline$TB_cases_MDR-SA_baseline$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_baseline$C_MDRTB_hiv_DOT_SAT<-SA_baseline$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
SA_baseline$C_TB_all_DOT_SAT<-SA_baseline$C_DSTB_all_DOT_SAT+SA_baseline$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
SA_baseline$C_TB_hiv_neg_DOT_SAT<-SA_baseline$C_DSTB_hiv_neg_DOT_SAT+SA_baseline$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_baseline$C_TB_hiv_DOT_SAT<-SA_baseline$C_DSTB_hiv_DOT_SAT+SA_baseline$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_baseline$C_TB_all_DOT_SAT)
#sum(SA_baseline$C_TB_hiv_neg_DOT_SAT)
#sum(SA_baseline$C_TB_hiv_DOT_SAT)

#baseline STRATEGY COST: PT & TB (DOT_SAT)
SA_baseline$C_baseline_all_DOT_SAT<-SA_baseline$C_PT_all+SA_baseline$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT_SAT
SA_baseline$C_baseline_hiv_neg_DOT_SAT<-SA_baseline$C_PT_hiv_neg+SA_baseline$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
SA_baseline$C_baseline_hiv_DOT_SAT<-SA_baseline$C_PT_hiv+SA_baseline$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT



#####################################################################################################################
#                                       scaleup ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for scaleup dataset
# PT
SA_scaleup$C_PT_all<-(SA_scaleup$New_PT_all*(SA_costs$LTBI_scaleup+SA_costs$AE_LTBI)) # Cost: PT all
SA_scaleup$C_PT_hiv_neg<-(SA_scaleup$New_PT_all-SA_scaleup$New_PT_HIV)*(SA_costs$LTBI_scaleup+SA_costs$AE_LTBI) # Cost: PT (HIV negative)
SA_scaleup$C_PT_hiv<-(SA_scaleup$New_PT_HIV*(SA_costs$LTBI_scaleup+SA_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
SA_scaleup$C_DSTB_all_DOT<-(SA_scaleup$TB_cases_all-SA_scaleup$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_scaleup$C_DSTB_hiv_neg_DOT<-((SA_scaleup$TB_cases_all-SA_scaleup$TB_cases_MDR)-(SA_scaleup$TB_cases_HIV-SA_scaleup$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_scaleup$C_DSTB_hiv_DOT<-(SA_scaleup$TB_cases_HIV-SA_scaleup$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
SA_scaleup$C_MDRTB_all_DOT<-(SA_scaleup$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_scaleup$C_MDRTB_hiv_neg_DOT<-(SA_scaleup$TB_cases_MDR-SA_scaleup$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_scaleup$C_MDRTB_hiv_DOT<-SA_scaleup$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
SA_scaleup$C_TB_all_DOT<-SA_scaleup$C_DSTB_all_DOT+SA_scaleup$C_MDRTB_all_DOT # Cost: TB (DS-TB & MRR-TB)
SA_scaleup$C_TB_hiv_neg_DOT<-SA_scaleup$C_DSTB_hiv_neg_DOT+SA_scaleup$C_MDRTB_hiv_neg_DOT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_scaleup$C_TB_hiv_DOT<-SA_scaleup$C_DSTB_hiv_DOT+SA_scaleup$C_MDRTB_hiv_DOT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_scaleup$C_TB_all_DOT)
#sum(SA_scaleup$C_TB_hiv_neg_DOT)
#sum(SA_scaleup$C_TB_hiv_DOT)

#scaleup STRATEGY COST: PT & TB (DOT)
SA_scaleup$C_scaleup_all_DOT<-SA_scaleup$C_PT_all+SA_scaleup$C_TB_all_DOT #Cost: strategy all (PT+TB) DOT
SA_scaleup$C_scaleup_hiv_neg_DOT<-SA_scaleup$C_PT_hiv_neg+SA_scaleup$C_TB_hiv_neg_DOT #Cost: strategy HIV negative (PT+TB) DOT
SA_scaleup$C_scaleup_hiv_DOT<-SA_scaleup$C_PT_hiv+SA_scaleup$C_TB_hiv_DOT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       scaleup ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
SA_scaleup$C_DSTB_all_DOT_SAT<-(SA_scaleup$TB_cases_all-SA_scaleup$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_scaleup$C_DSTB_hiv_neg_DOT_SAT<-((SA_scaleup$TB_cases_all-SA_scaleup$TB_cases_MDR)-(SA_scaleup$TB_cases_HIV-SA_scaleup$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_scaleup$C_DSTB_hiv_DOT_SAT<-(SA_scaleup$TB_cases_HIV-SA_scaleup$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
SA_scaleup$C_MDRTB_all_DOT_SAT<-(SA_scaleup$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_scaleup$C_MDRTB_hiv_neg_DOT_SAT<-(SA_scaleup$TB_cases_MDR-SA_scaleup$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_scaleup$C_MDRTB_hiv_DOT_SAT<-SA_scaleup$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
SA_scaleup$C_TB_all_DOT_SAT<-SA_scaleup$C_DSTB_all_DOT_SAT+SA_scaleup$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
SA_scaleup$C_TB_hiv_neg_DOT_SAT<-SA_scaleup$C_DSTB_hiv_neg_DOT_SAT+SA_scaleup$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_scaleup$C_TB_hiv_DOT_SAT<-SA_scaleup$C_DSTB_hiv_DOT_SAT+SA_scaleup$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_scaleup$C_TB_all_DOT_SAT)
#sum(SA_scaleup$C_TB_hiv_neg_DOT_SAT)
#sum(SA_scaleup$C_TB_hiv_DOT_SAT)

#scaleup STRATEGY COST: PT & TB (DOT_SAT)
SA_scaleup$C_scaleup_all_DOT_SAT<-SA_scaleup$C_PT_all+SA_scaleup$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT_SAT
SA_scaleup$C_scaleup_hiv_neg_DOT_SAT<-SA_scaleup$C_PT_hiv_neg+SA_scaleup$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT

SA_scaleup$C_scaleup_hiv_DOT_SAT<-SA_scaleup$C_PT_hiv+SA_scaleup$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT



#####################################################################################################################
#                                       minimal ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for minimal dataset
# PT
SA_minimal$C_PT_all<-(SA_minimal$New_PT_all*(SA_costs$LTBI_minimal+SA_costs$AE_LTBI)) # Cost: PT all
SA_minimal$C_PT_hiv_neg<-(SA_minimal$New_PT_all-SA_minimal$New_PT_HIV)*(SA_costs$LTBI_minimal+SA_costs$AE_LTBI) # Cost: PT (HIV negative)
SA_minimal$C_PT_hiv<-(SA_minimal$New_PT_HIV*(SA_costs$LTBI_minimal+SA_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
SA_minimal$C_DSTB_all_DOT<-(SA_minimal$TB_cases_all-SA_minimal$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_minimal$C_DSTB_hiv_neg_DOT<-((SA_minimal$TB_cases_all-SA_minimal$TB_cases_MDR)-(SA_minimal$TB_cases_HIV-SA_minimal$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_minimal$C_DSTB_hiv_DOT<-(SA_minimal$TB_cases_HIV-SA_minimal$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
SA_minimal$C_MDRTB_all_DOT<-(SA_minimal$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_minimal$C_MDRTB_hiv_neg_DOT<-(SA_minimal$TB_cases_MDR-SA_minimal$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_minimal$C_MDRTB_hiv_DOT<-SA_minimal$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
SA_minimal$C_TB_all_DOT<-SA_minimal$C_DSTB_all_DOT+SA_minimal$C_MDRTB_all_DOT # Cost: TB (DS-TB & MRR-TB)
SA_minimal$C_TB_hiv_neg_DOT<-SA_minimal$C_DSTB_hiv_neg_DOT+SA_minimal$C_MDRTB_hiv_neg_DOT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_minimal$C_TB_hiv_DOT<-SA_minimal$C_DSTB_hiv_DOT+SA_minimal$C_MDRTB_hiv_DOT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_minimal$C_TB_all_DOT)
#sum(SA_minimal$C_TB_hiv_neg_DOT)
#sum(SA_minimal$C_TB_hiv_DOT)

#minimal STRATEGY COST: PT & TB (DOT)
SA_minimal$C_minimal_all_DOT<-SA_minimal$C_PT_all+SA_minimal$C_TB_all_DOT #Cost: strategy all (PT+TB) DOT
SA_minimal$C_minimal_hiv_neg_DOT<-SA_minimal$C_PT_hiv_neg+SA_minimal$C_TB_hiv_neg_DOT #Cost: strategy HIV negative (PT+TB) DOT
SA_minimal$C_minimal_hiv_DOT<-SA_minimal$C_PT_hiv+SA_minimal$C_TB_hiv_DOT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       minimal ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
SA_minimal$C_DSTB_all_DOT_SAT<-(SA_minimal$TB_cases_all-SA_minimal$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_minimal$C_DSTB_hiv_neg_DOT_SAT<-((SA_minimal$TB_cases_all-SA_minimal$TB_cases_MDR)-(SA_minimal$TB_cases_HIV-SA_minimal$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_minimal$C_DSTB_hiv_DOT_SAT<-(SA_minimal$TB_cases_HIV-SA_minimal$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
SA_minimal$C_MDRTB_all_DOT_SAT<-(SA_minimal$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_minimal$C_MDRTB_hiv_neg_DOT_SAT<-(SA_minimal$TB_cases_MDR-SA_minimal$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_minimal$C_MDRTB_hiv_DOT_SAT<-SA_minimal$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
SA_minimal$C_TB_all_DOT_SAT<-SA_minimal$C_DSTB_all_DOT_SAT+SA_minimal$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
SA_minimal$C_TB_hiv_neg_DOT_SAT<-SA_minimal$C_DSTB_hiv_neg_DOT_SAT+SA_minimal$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_minimal$C_TB_hiv_DOT_SAT<-SA_minimal$C_DSTB_hiv_DOT_SAT+SA_minimal$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_minimal$C_TB_all_DOT_SAT)
#sum(SA_minimal$C_TB_hiv_neg_DOT_SAT)
#sum(SA_minimal$C_TB_hiv_DOT_SAT)

#minimal STRATEGY COST: PT & TB (DOT_SAT)
SA_minimal$C_minimal_all_DOT_SAT<-SA_minimal$C_PT_all+SA_minimal$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT_SAT
SA_minimal$C_minimal_hiv_neg_DOT_SAT<-SA_minimal$C_PT_hiv_neg+SA_minimal$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
SA_minimal$C_minimal_hiv_DOT_SAT<-SA_minimal$C_PT_hiv+SA_minimal$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT



#####################################################################################################################
#                                       optimal ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for optimal dataset
# PT
SA_optimal$C_PT_all<-(SA_optimal$New_PT_all*(SA_costs$LTBI_optimal+SA_costs$AE_LTBI)) # Cost: PT all
SA_optimal$C_PT_hiv_neg<-(SA_optimal$New_PT_all-SA_optimal$New_PT_HIV)*(SA_costs$LTBI_optimal+SA_costs$AE_LTBI) # Cost: PT (HIV negative)
SA_optimal$C_PT_hiv<-(SA_optimal$New_PT_HIV*(SA_costs$LTBI_optimal+SA_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
SA_optimal$C_DSTB_all_DOT<-(SA_optimal$TB_cases_all-SA_optimal$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_optimal$C_DSTB_hiv_neg_DOT<-((SA_optimal$TB_cases_all-SA_optimal$TB_cases_MDR)-(SA_optimal$TB_cases_HIV-SA_optimal$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_optimal$C_DSTB_hiv_DOT<-(SA_optimal$TB_cases_HIV-SA_optimal$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
SA_optimal$C_MDRTB_all_DOT<-(SA_optimal$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_optimal$C_MDRTB_hiv_neg_DOT<-(SA_optimal$TB_cases_MDR-SA_optimal$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_optimal$C_MDRTB_hiv_DOT<-SA_optimal$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
SA_optimal$C_TB_all_DOT<-SA_optimal$C_DSTB_all_DOT+SA_optimal$C_MDRTB_all_DOT # Cost: TB (DS-TB & MRR-TB)
SA_optimal$C_TB_hiv_neg_DOT<-SA_optimal$C_DSTB_hiv_neg_DOT+SA_optimal$C_MDRTB_hiv_neg_DOT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_optimal$C_TB_hiv_DOT<-SA_optimal$C_DSTB_hiv_DOT+SA_optimal$C_MDRTB_hiv_DOT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_optimal$C_TB_all_DOT)
#sum(SA_optimal$C_TB_hiv_neg_DOT)
#sum(SA_optimal$C_TB_hiv_DOT)

#optimal STRATEGY COST: PT & TB (DOT)
SA_optimal$C_optimal_all_DOT<-SA_optimal$C_PT_all+SA_optimal$C_TB_all_DOT #Cost: strategy all (PT+TB) DOT
SA_optimal$C_optimal_hiv_neg_DOT<-SA_optimal$C_PT_hiv_neg+SA_optimal$C_TB_hiv_neg_DOT #Cost: strategy HIV negative (PT+TB) DOT
SA_optimal$C_optimal_hiv_DOT<-SA_optimal$C_PT_hiv+SA_optimal$C_TB_hiv_DOT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       optimal ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
SA_optimal$C_DSTB_all_DOT_SAT<-(SA_optimal$TB_cases_all-SA_optimal$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_optimal$C_DSTB_hiv_neg_DOT_SAT<-((SA_optimal$TB_cases_all-SA_optimal$TB_cases_MDR)-(SA_optimal$TB_cases_HIV-SA_optimal$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_optimal$C_DSTB_hiv_DOT_SAT<-(SA_optimal$TB_cases_HIV-SA_optimal$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
SA_optimal$C_MDRTB_all_DOT_SAT<-(SA_optimal$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_optimal$C_MDRTB_hiv_neg_DOT_SAT<-(SA_optimal$TB_cases_MDR-SA_optimal$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_optimal$C_MDRTB_hiv_DOT_SAT<-SA_optimal$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
SA_optimal$C_TB_all_DOT_SAT<-SA_optimal$C_DSTB_all_DOT_SAT+SA_optimal$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
SA_optimal$C_TB_hiv_neg_DOT_SAT<-SA_optimal$C_DSTB_hiv_neg_DOT_SAT+SA_optimal$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_optimal$C_TB_hiv_DOT_SAT<-SA_optimal$C_DSTB_hiv_DOT_SAT+SA_optimal$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_optimal$C_TB_all_DOT_SAT)
#sum(SA_optimal$C_TB_hiv_neg_DOT_SAT)
#sum(SA_optimal$C_TB_hiv_DOT_SAT)

#optimal STRATEGY COST: PT & TB (DOT_SAT)
SA_optimal$C_optimal_all_DOT_SAT<-SA_optimal$C_PT_all+SA_optimal$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT_SAT
SA_optimal$C_optimal_hiv_neg_DOT_SAT<-SA_optimal$C_PT_hiv_neg+SA_optimal$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
SA_optimal$C_optimal_hiv_DOT_SAT<-SA_optimal$C_PT_hiv+SA_optimal$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT




#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

                                        ##### PREP DATA FOR ANALYSIS/PLOTTING #######
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#Melt data in key packets for plotting
SA_strategy_cost<-data.frame(
  Year=SA_baseline$Year,
  Baseline=SA_baseline$C_baseline_all_DOT_SAT,
  Baseline_HIV_neg=SA_baseline$C_baseline_hiv_neg_DOT_SAT,
  Baseline_PLHIV=SA_baseline$C_baseline_hiv_DOT_SAT,
  "Expanded 6H"=SA_scaleup$C_scaleup_all_DOT_SAT,
  "Expanded 6H_neg"=SA_scaleup$C_scaleup_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=SA_scaleup$C_scaleup_hiv_DOT_SAT,
  "Minimal regimen"=SA_minimal$C_minimal_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=SA_minimal$C_minimal_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=SA_minimal$C_minimal_hiv_DOT_SAT,
  "Optimal regimen"=SA_optimal$C_optimal_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=SA_optimal$C_optimal_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=SA_optimal$C_optimal_hiv_DOT_SAT)
SA_cost_pop_graph<-melt(SA_strategy_cost, id.vars =c("Year"), variable.name = "cost_pop")
SA_cost_pop_graph$cum_cost<-ave(SA_cost_pop_graph$value, SA_cost_pop_graph$cost_pop, FUN=cumsum)
SA_cost_pop_graph$Year<- as.Date(ISOdate(SA_cost_pop_graph$Year,1,1) ) #set up date variable
SA_cost_pop_graph$strategy = ifelse(grepl("Baseline", SA_cost_pop_graph$cost_pop), "_baseline",
                                      ifelse(grepl("6H", SA_cost_pop_graph$cost_pop),"6H",ifelse(grepl("Minimal", SA_cost_pop_graph$cost_pop), "minimal","Optimal")))
SA_cost_pop_graph$population = ifelse(!grepl("_", SA_cost_pop_graph$cost_pop), "All treatment candidates",
                                ifelse(grepl("PLHIV", SA_cost_pop_graph$cost_pop),"PLHIV","HHC (HIV negatives)"))
SA_cost_pop_graph$cost_pop = chartr("."," ",SA_cost_pop_graph$cost_pop)

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

                                      ##### PLOTS #######
#####################################################################################################################
#####################################################################################################################


#####################################################################################################################
                                        #Annual cost per population group#
#####################################################################################################################


####################
SA_graph_annual_costs <- ggplot(SA_cost_pop_graph,aes(Year,value))+
  scale_color_brewer(palette = "Set1")+
  theme_classic()
# annual costs graph (not smoothed out)
SA_graph_annual_costs+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_summary(fun = mean,geom = "line",size = 1,aes(colour = strategy))+
  labs(x = "Years",y = expression(paste("Annual cost (US$)")),colour = "")
  ggsave("SA_graph_annual_cost.jpeg", 
       width = 25, height = 16, units = "cm")

# smoothed annual costs graph
  
  SA_graph_annual_costs+
    #  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
    scale_x_date(date_breaks="4 years",date_labels="%Y")+
    scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
    facet_wrap(~population, scales = "free") +
    guides(fill = "none")+
    stat_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE) +
    # geom_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE)+
    labs(x = "Years",y = expression(paste("Annual cost (US$)")),colour = "")
  ggsave("SA_graph_annual_costs(smoothed).jpeg", 
         width = 25, height = 16, units = "cm")
  
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  #                                   Cumulative epi & cost  per population group#
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  
  #####################################################################################################################
  #                                   1A. Cumulative PT & TB cost per population group#
  #####################################################################################################################
  #Data wrangling: Melt data in key packets for plotting
  SA_strategy_cost_cumulative<-data.frame(
    Runs=SA_baseline$Run,
    Baseline=SA_baseline$C_baseline_all_DOT_SAT,
    Baseline_HIV_neg=SA_baseline$C_baseline_hiv_neg_DOT_SAT,
    Baseline_PLHIV=SA_baseline$C_baseline_hiv_DOT_SAT,
    "Expanded 6H"=SA_scaleup$C_scaleup_all_DOT_SAT,
    "Expanded 6H_neg"=SA_scaleup$C_scaleup_hiv_neg_DOT_SAT,
    "Expanded 6H_PLHIV"=SA_scaleup$C_scaleup_hiv_DOT_SAT,
    "Minimal regimen"=SA_minimal$C_minimal_all_DOT_SAT,
    "Minimal regimen_HIV_neg"=SA_minimal$C_minimal_hiv_neg_DOT_SAT,
    "Minimal regimen_PLHIV"=SA_minimal$C_minimal_hiv_DOT_SAT,
    "Optimal regimen"=SA_optimal$C_optimal_all_DOT_SAT,
    "Optimal regimen_HIV_neg"=SA_optimal$C_optimal_hiv_neg_DOT_SAT,
    "Optimal regimen_PLHIV"=SA_optimal$C_optimal_hiv_DOT_SAT)
  
  SA_cost_pop_graph_cumulative<-melt(SA_strategy_cost_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
  SA_cost_pop_graph_cumulative$cum_cost<-ave(SA_cost_pop_graph_cumulative$value, SA_cost_pop_graph_cumulative$cost_pop, FUN=cumsum)
  # SA_cost_pop_graph_cumulative$Year<- as.Date(ISOdate(SA_cost_pop_graph_cumulative$Year,1,1) ) #set up date variable
  SA_cost_pop_graph_cumulative$strategy = ifelse(grepl("Baseline", SA_cost_pop_graph_cumulative$cost_pop), "_baseline",
                                                 ifelse(grepl("6H", SA_cost_pop_graph_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", SA_cost_pop_graph_cumulative$cost_pop), "minimal","Optimal")))
  SA_cost_pop_graph_cumulative$population = ifelse(!grepl("_", SA_cost_pop_graph_cumulative$cost_pop), "All treatment candidates",
                                                   ifelse(grepl("PLHIV", SA_cost_pop_graph_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
  SA_cost_pop_graph_cumulative$cost_pop = chartr("."," ",SA_cost_pop_graph_cumulative$cost_pop)
  
  #####################################################################################################################
  
  SA_cost_cumulat<-aggregate(x=SA_cost_pop_graph_cumulative$value, by=list(Runs=SA_cost_pop_graph_cumulative$Runs,cost_pop=SA_cost_pop_graph_cumulative$cost_pop,strategy=SA_cost_pop_graph_cumulative$strategy,population=SA_cost_pop_graph_cumulative$population), FUN=sum)
  #cumulative cost graph
  SA_cost_cumulat_graph <- ggplot(data=SA_cost_cumulat, aes(cost_pop,x,color=strategy)) + 
    geom_boxplot(notch = TRUE) +
    scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
    facet_wrap(~population, dir = "h", scales = "free") +
    labs(x = "population group",y = expression(paste("cumulative cost (US$ Billions)")),colour = "",subtitle="",caption="")+
    guides(fill = "none")+
    stat_boxplot(geom = "errorbar", width = 0.5) +  
    geom_boxplot()+
    scale_color_brewer(palette = "Set1")+
    theme_classic()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  ggsave("1A. SA_graph_cumulative_costs.jpeg", 
         width = 25, height = 16, units = "cm")
  
  
  
  SA_overall_cost_table<-SA_cost_cumulat %>% 
    group_by(cost_pop, strategy,population) %>% 
    summarise_at(vars(x),
                 list(median=median,
                      mean=mean,
                      quantile_lower = ~quantile(., probs = 0.025),
                      quantile_upper = ~quantile(., probs = 0.975)))
  write.csv(SA_overall_cost_table, file = "1A. South African cumulative cost output tables.csv")
  
  
  
  #####################################################################################################################
  #                                   1B. Cumulative TB cost per population group#
  #####################################################################################################################
  #Data wrangling: Melt data in key packets for plotting
  SA_strategy_cost_TB_cumulative<-data.frame(
    Runs=SA_baseline$Run,
    Baseline=SA_baseline$C_TB_all_DOT_SAT,
    Baseline_HIV_neg=SA_baseline$C_TB_hiv_neg_DOT_SAT,
    Baseline_PLHIV=SA_baseline$C_TB_hiv_DOT_SAT,
    "Expanded 6H"=SA_scaleup$C_TB_all_DOT_SAT,
    "Expanded 6H_neg"=SA_scaleup$C_TB_hiv_neg_DOT_SAT,
    "Expanded 6H_PLHIV"=SA_scaleup$C_TB_hiv_DOT_SAT,
    "Minimal regimen"=SA_minimal$C_TB_all_DOT_SAT,
    "Minimal regimen_HIV_neg"=SA_minimal$C_TB_hiv_neg_DOT_SAT,
    "Minimal regimen_PLHIV"=SA_minimal$C_TB_hiv_DOT_SAT,
    "Optimal regimen"=SA_optimal$C_TB_all_DOT_SAT,
    "Optimal regimen_HIV_neg"=SA_optimal$C_TB_hiv_neg_DOT_SAT,
    "Optimal regimen_PLHIV"=SA_optimal$C_TB_hiv_DOT_SAT)
  
  SA_cost_pop_graph_TB_cumulative<-melt(SA_strategy_cost_TB_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
  SA_cost_pop_graph_TB_cumulative$cum_cost<-ave(SA_cost_pop_graph_TB_cumulative$value, SA_cost_pop_graph_TB_cumulative$cost_pop, FUN=cumsum)
  # SA_cost_pop_graph_TB_cumulative$Year<- as.Date(ISOdate(SA_cost_pop_graph_TB_cumulative$Year,1,1) ) #set up date variable
  SA_cost_pop_graph_TB_cumulative$strategy = ifelse(grepl("Baseline", SA_cost_pop_graph_TB_cumulative$cost_pop), "_baseline",
                                                 ifelse(grepl("6H", SA_cost_pop_graph_TB_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", SA_cost_pop_graph_TB_cumulative$cost_pop), "minimal","Optimal")))
  SA_cost_pop_graph_TB_cumulative$population = ifelse(!grepl("_", SA_cost_pop_graph_TB_cumulative$cost_pop), "All treatment candidates",
                                                   ifelse(grepl("PLHIV", SA_cost_pop_graph_TB_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
  SA_cost_pop_graph_TB_cumulative$cost_pop = chartr("."," ",SA_cost_pop_graph_TB_cumulative$cost_pop)
  
  #####################################################################################################################
  
  SA_cost_TB_cumulat<-aggregate(x=SA_cost_pop_graph_TB_cumulative$value, by=list(Runs=SA_cost_pop_graph_TB_cumulative$Runs,cost_pop=SA_cost_pop_graph_TB_cumulative$cost_pop,strategy=SA_cost_pop_graph_TB_cumulative$strategy,population=SA_cost_pop_graph_TB_cumulative$population), FUN=sum)
  #cumulative cost graph
  SA_cost_TB_cumulat_graph <- ggplot(data=SA_cost_TB_cumulat, aes(cost_pop,x,color=strategy)) + 
    geom_boxplot(notch = TRUE) +
    scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
    facet_wrap(~population, dir = "h", scales = "free") +
    labs(x = "population group",y = expression(paste("cost of diagnosis and treatment of TB (US$ Billions)")),colour = "",subtitle="",caption="")+
    guides(fill = "none")+
    stat_boxplot(geom = "errorbar", width = 0.5) +  
    geom_boxplot()+
    scale_color_brewer(palette = "Set1")+
    theme_classic()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  ggsave("1B. SA_graph_TB_cumulative_costs.jpeg", 
         width = 25, height = 16, units = "cm")
  
  
  
  SA_overall_cost_table<-SA_cost_TB_cumulat %>% 
    group_by(cost_pop, strategy,population) %>% 
    summarise_at(vars(x),
                 list(median=median,
                      mean=mean,
                      quantile_lower = ~quantile(., probs = 0.025),
                      quantile_upper = ~quantile(., probs = 0.975)))
  write.csv(SA_overall_cost_table, file = "1B. South African cumulative cost TB dig&tx output tables.csv")
  

  
  #####################################################################################################################
  #                                   1C. Cumulative RR-TB cost per population group#
  #####################################################################################################################
  #Data wrangling: Melt data in key packets for plotting
  SA_strategy_cost_MDRTB_cumulative<-data.frame(
    Runs=SA_baseline$Run,
    Baseline=SA_baseline$C_MDRTB_all_DOT_SAT,
    Baseline_HIV_neg=SA_baseline$C_MDRTB_hiv_neg_DOT_SAT,
    Baseline_PLHIV=SA_baseline$C_MDRTB_hiv_DOT_SAT,
    "Expanded 6H"=SA_scaleup$C_MDRTB_all_DOT_SAT,
    "Expanded 6H_neg"=SA_scaleup$C_MDRTB_hiv_neg_DOT_SAT,
    "Expanded 6H_PLHIV"=SA_scaleup$C_MDRTB_hiv_DOT_SAT,
    "Minimal regimen"=SA_minimal$C_MDRTB_all_DOT_SAT,
    "Minimal regimen_HIV_neg"=SA_minimal$C_MDRTB_hiv_neg_DOT_SAT,
    "Minimal regimen_PLHIV"=SA_minimal$C_MDRTB_hiv_DOT_SAT,
    "Optimal regimen"=SA_optimal$C_MDRTB_all_DOT_SAT,
    "Optimal regimen_HIV_neg"=SA_optimal$C_MDRTB_hiv_neg_DOT_SAT,
    "Optimal regimen_PLHIV"=SA_optimal$C_MDRTB_hiv_DOT_SAT)
  
  SA_cost_pop_graph_MDRTB_cumulative<-melt(SA_strategy_cost_MDRTB_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
  SA_cost_pop_graph_MDRTB_cumulative$cum_cost<-ave(SA_cost_pop_graph_MDRTB_cumulative$value, SA_cost_pop_graph_MDRTB_cumulative$cost_pop, FUN=cumsum)
  # SA_cost_pop_graph_MDRTB_cumulative$Year<- as.Date(ISOdate(SA_cost_pop_graph_MDRTB_cumulative$Year,1,1) ) #set up date variable
  SA_cost_pop_graph_MDRTB_cumulative$strategy = ifelse(grepl("Baseline", SA_cost_pop_graph_MDRTB_cumulative$cost_pop), "_baseline",
                                                    ifelse(grepl("6H", SA_cost_pop_graph_MDRTB_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", SA_cost_pop_graph_MDRTB_cumulative$cost_pop), "minimal","Optimal")))
  SA_cost_pop_graph_MDRTB_cumulative$population = ifelse(!grepl("_", SA_cost_pop_graph_MDRTB_cumulative$cost_pop), "All treatment candidates",
                                                      ifelse(grepl("PLHIV", SA_cost_pop_graph_MDRTB_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
  SA_cost_pop_graph_MDRTB_cumulative$cost_pop = chartr("."," ",SA_cost_pop_graph_MDRTB_cumulative$cost_pop)
  
  #####################################################################################################################
  
  SA_cost_MDRTB_cumulat<-aggregate(x=SA_cost_pop_graph_MDRTB_cumulative$value, by=list(Runs=SA_cost_pop_graph_MDRTB_cumulative$Runs,cost_pop=SA_cost_pop_graph_MDRTB_cumulative$cost_pop,strategy=SA_cost_pop_graph_MDRTB_cumulative$strategy,population=SA_cost_pop_graph_MDRTB_cumulative$population), FUN=sum)
  #cumulative cost graph
  SA_cost_MDRTB_cumulat_graph <- ggplot(data=SA_cost_MDRTB_cumulat, aes(cost_pop,x,color=strategy)) + 
    geom_boxplot(notch = TRUE) +
    scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
    facet_wrap(~population, dir = "h", scales = "free") +
    labs(x = "population group",y = expression(paste("cost of diagnosis and treatment of  RR-TB (US$ Billions)")),colour = "",subtitle="",caption="")+
    guides(fill = "none")+
    stat_boxplot(geom = "errorbar", width = 0.5) +  
    geom_boxplot()+
    scale_color_brewer(palette = "Set1")+
    theme_classic()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  ggsave("1C. SA_graph_RR-TB_cumulative_costs.jpeg", 
         width = 25, height = 16, units = "cm")
  
  
  
  SA_overall_cost_table<-SA_cost_MDRTB_cumulat %>% 
    group_by(cost_pop, strategy,population) %>% 
    summarise_at(vars(x),
                 list(median=median,
                      mean=mean,
                      quantile_lower = ~quantile(., probs = 0.025),
                      quantile_upper = ~quantile(., probs = 0.975)))
  write.csv(SA_overall_cost_table, file = "1C. South African cumulative cost RR-TB dig&tx output tables.csv")
  
  
  
  
  #####################################################################################################################
  #                                   1D. Cumulative PT - cost per population group#
  #####################################################################################################################
  #Data wrangling: Melt data in key packets for plotting
  SA_strategy_PT_cost_cumulative<-data.frame(
    Runs=SA_baseline$Run,
    Baseline=SA_baseline$C_PT_all,
    Baseline_HIV_neg=SA_baseline$C_PT_hiv_neg,
    Baseline_PLHIV=SA_baseline$C_PT_hiv,
    "Expanded 6H"=SA_scaleup$C_PT_all,
    "Expanded 6H_neg"=SA_scaleup$C_PT_hiv_neg,
    "Expanded 6H_PLHIV"=SA_scaleup$C_PT_hiv,
    "Minimal regimen"=SA_minimal$C_PT_all,
    "Minimal regimen_HIV_neg"=SA_minimal$C_PT_hiv_neg,
    "Minimal regimen_PLHIV"=SA_minimal$C_PT_hiv,
    "Optimal regimen"=SA_optimal$C_PT_all,
    "Optimal regimen_HIV_neg"=SA_optimal$C_PT_hiv_neg,
    "Optimal regimen_PLHIV"=SA_optimal$C_PT_hiv)
  
  SA_PT_cost_pop_graph_cumulative<-melt(SA_strategy_PT_cost_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
  SA_PT_cost_pop_graph_cumulative$strategy = ifelse(grepl("Baseline", SA_PT_cost_pop_graph_cumulative$cost_pop), "_baseline",
                                                    ifelse(grepl("6H", SA_PT_cost_pop_graph_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", SA_PT_cost_pop_graph_cumulative$cost_pop), "minimal","Optimal")))
  SA_PT_cost_pop_graph_cumulative$population = ifelse(!grepl("_", SA_PT_cost_pop_graph_cumulative$cost_pop), "All treatment candidates",
                                                      ifelse(grepl("PLHIV", SA_PT_cost_pop_graph_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
  SA_PT_cost_pop_graph_cumulative$cost_pop = chartr("."," ",SA_PT_cost_pop_graph_cumulative$cost_pop)
  
  #####################################################################################################################
  
  SA_PT_cost_cumulat<-aggregate(x=SA_PT_cost_pop_graph_cumulative$value, by=list(Runs=SA_PT_cost_pop_graph_cumulative$Runs,cost_pop=SA_PT_cost_pop_graph_cumulative$cost_pop,strategy=SA_PT_cost_pop_graph_cumulative$strategy,population=SA_PT_cost_pop_graph_cumulative$population), FUN=sum)
  #cumulative cost graph
  SA_PT_cost_cumulat_graph <- ggplot(data=SA_PT_cost_cumulat, aes(cost_pop,x,color=strategy)) + 
    geom_boxplot(notch = TRUE) +
    scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
    facet_wrap(~population, dir = "h", scales = "free") +
    labs(x = "population group",y = expression(paste("cumulative PT cost (US$ Millions)")),colour = "",subtitle="",caption="")+
    guides(fill = "none")+
    stat_boxplot(geom = "errorbar", width = 0.5) +  
    geom_boxplot()+
    scale_color_brewer(palette = "Set1")+
    theme_classic()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  ggsave("1D. SA_PT_graph_cumulative_costs.jpeg", 
         width = 25, height = 16, units = "cm")
  
  
  
  SA_overall_cost_table<-SA_PT_cost_cumulat %>% 
    group_by(cost_pop, strategy,population) %>% 
    summarise_at(vars(x),
                 list(median=median,
                      mean=mean,
                      quantile_lower = ~quantile(., probs = 0.025),
                      quantile_upper = ~quantile(., probs = 0.975)))
  write.csv(SA_overall_cost_table, file = "1D. South African cumulative PT cost output tables.csv")
  

  #####################################################################################################################
  #                                   1E. Cumulative DS-TB cost per population group#
  #####################################################################################################################
  #Data wrangling: Melt data in key packets for plotting
  SA_strategy_cost_DSTB_cumulative<-data.frame(
    Runs=SA_baseline$Run,
    Baseline=SA_baseline$C_DSTB_all_DOT_SAT,
    Baseline_HIV_neg=SA_baseline$C_DSTB_hiv_neg_DOT_SAT,
    Baseline_PLHIV=SA_baseline$C_DSTB_hiv_DOT_SAT,
    "Expanded 6H"=SA_scaleup$C_DSTB_all_DOT_SAT,
    "Expanded 6H_neg"=SA_scaleup$C_DSTB_hiv_neg_DOT_SAT,
    "Expanded 6H_PLHIV"=SA_scaleup$C_DSTB_hiv_DOT_SAT,
    "Minimal regimen"=SA_minimal$C_DSTB_all_DOT_SAT,
    "Minimal regimen_HIV_neg"=SA_minimal$C_DSTB_hiv_neg_DOT_SAT,
    "Minimal regimen_PLHIV"=SA_minimal$C_DSTB_hiv_DOT_SAT,
    "Optimal regimen"=SA_optimal$C_DSTB_all_DOT_SAT,
    "Optimal regimen_HIV_neg"=SA_optimal$C_DSTB_hiv_neg_DOT_SAT,
    "Optimal regimen_PLHIV"=SA_optimal$C_DSTB_hiv_DOT_SAT)
  
  SA_cost_pop_graph_DSTB_cumulative<-melt(SA_strategy_cost_DSTB_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
  SA_cost_pop_graph_DSTB_cumulative$cum_cost<-ave(SA_cost_pop_graph_DSTB_cumulative$value, SA_cost_pop_graph_DSTB_cumulative$cost_pop, FUN=cumsum)
  # SA_cost_pop_graph_DSTB_cumulative$Year<- as.Date(ISOdate(SA_cost_pop_graph_DSTB_cumulative$Year,1,1) ) #set up date variable
  SA_cost_pop_graph_DSTB_cumulative$strategy = ifelse(grepl("Baseline", SA_cost_pop_graph_DSTB_cumulative$cost_pop), "_baseline",
                                                          ifelse(grepl("6H", SA_cost_pop_graph_DSTB_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", SA_cost_pop_graph_DSTB_cumulative$cost_pop), "minimal","Optimal")))
  SA_cost_pop_graph_DSTB_cumulative$population = ifelse(!grepl("_", SA_cost_pop_graph_DSTB_cumulative$cost_pop), "All treatment candidates",
                                                            ifelse(grepl("PLHIV", SA_cost_pop_graph_DSTB_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
  SA_cost_pop_graph_DSTB_cumulative$cost_pop = chartr("."," ",SA_cost_pop_graph_DSTB_cumulative$cost_pop)
  
  #####################################################################################################################
  
  SA_cost_DSTB_cumulat<-aggregate(x=SA_cost_pop_graph_DSTB_cumulative$value, by=list(Runs=SA_cost_pop_graph_DSTB_cumulative$Runs,cost_pop=SA_cost_pop_graph_DSTB_cumulative$cost_pop,strategy=SA_cost_pop_graph_DSTB_cumulative$strategy,population=SA_cost_pop_graph_DSTB_cumulative$population), FUN=sum)
  #cumulative cost graph
  SA_cost_DSTB_cumulat_graph <- ggplot(data=SA_cost_DSTB_cumulat, aes(cost_pop,x,color=strategy)) + 
    geom_boxplot(notch = TRUE) +
    scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
    facet_wrap(~population, dir = "h", scales = "free") +
    labs(x = "population group",y = expression(paste("cost of diagnosis and treatment of  DS-TB (US$ Billions)")),colour = "",subtitle="",caption="")+
    guides(fill = "none")+
    stat_boxplot(geom = "errorbar", width = 0.5) +  
    geom_boxplot()+
    scale_color_brewer(palette = "Set1")+
    theme_classic()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  ggsave("1E. SA_graph_DS-TB_cumulative_costs.jpeg", 
         width = 25, height = 16, units = "cm")
  
  
  
  SA_DS_cost_table<-SA_cost_DSTB_cumulat %>% 
    group_by(cost_pop, strategy,population) %>% 
    summarise_at(vars(x),
                 list(median=median,
                      mean=mean,
                      quantile_lower = ~quantile(., probs = 0.025),
                      quantile_upper = ~quantile(., probs = 0.975)))
  write.csv(SA_DS_cost_table, file = "1E. SA cumulative cost DS-TB dig&tx output tables.csv")
  
  
  
  
  
  #####################################################################################################################
  #                                  2A. Cumulative overall DS-TB per population group#
  #####################################################################################################################
  #Data wrangling: Melt data in key packets for plotting
  SA_epi_cumulative_DSTB<-data.frame(
    Runs=SA_baseline$Run,
    Baseline=SA_baseline$TB_cases_all-SA_baseline$TB_cases_MDR,
    Baseline_HIV_neg=(SA_baseline$TB_cases_all-SA_baseline$TB_cases_MDR)-(SA_baseline$TB_cases_HIV-SA_baseline$TB_cases_MDR_HIV),
    Baseline_PLHIV=SA_baseline$TB_cases_HIV-SA_baseline$TB_cases_MDR_HIV,
    "Expanded 6H"=SA_scaleup$TB_cases_all-SA_scaleup$TB_cases_MDR,
    "Expanded 6H_neg"=(SA_scaleup$TB_cases_all-SA_scaleup$TB_cases_MDR)-(SA_scaleup$TB_cases_HIV-SA_scaleup$TB_cases_MDR_HIV),
    "Expanded 6H_PLHIV"=SA_scaleup$TB_cases_HIV-SA_scaleup$TB_cases_MDR_HIV,
    "Minimal regimen"=SA_minimal$TB_cases_all-SA_minimal$TB_cases_MDR,
    "Minimal regimen_HIV_neg"=(SA_minimal$TB_cases_all-SA_minimal$TB_cases_MDR)-(SA_minimal$TB_cases_HIV-SA_minimal$TB_cases_MDR_HIV),
    "Minimal regimen_PLHIV"=SA_minimal$TB_cases_HIV-SA_minimal$TB_cases_MDR_HIV,
    "Optimal regimen"=SA_optimal$TB_cases_all-SA_optimal$TB_cases_MDR,
    "Optimal regimen_HIV_neg"=(SA_optimal$TB_cases_all-SA_optimal$TB_cases_MDR)-(SA_optimal$TB_cases_HIV-SA_optimal$TB_cases_MDR_HIV),
    "Optimal regimen_PLHIV"=SA_optimal$TB_cases_HIV-SA_optimal$TB_cases_MDR_HIV)
  
  SA_epi_cumulative_DSTB_graph<-melt(SA_epi_cumulative_DSTB, id.vars =c("Runs"), variable.name = "epi_pop")
  SA_epi_cumulative_DSTB_graph$strategy = ifelse(grepl("Baseline", SA_epi_cumulative_DSTB_graph$epi_pop), "_baseline",
                                                  ifelse(grepl("6H", SA_epi_cumulative_DSTB_graph$epi_pop),"6H",ifelse(grepl("Minimal", SA_epi_cumulative_DSTB_graph$epi_pop), "minimal","Optimal")))
  SA_epi_cumulative_DSTB_graph$population = ifelse(!grepl("_", SA_epi_cumulative_DSTB_graph$epi_pop), "All treatment candidates",
                                                    ifelse(grepl("PLHIV", SA_epi_cumulative_DSTB_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
  SA_epi_cumulative_DSTB_graph$epi_pop = chartr("."," ",SA_epi_cumulative_DSTB_graph$epi_pop)
  
  #####################################################################################################################
  
  SA_epi_DSTB_cumulat<-aggregate(x=SA_epi_cumulative_DSTB_graph$value, by=list(Runs=SA_epi_cumulative_DSTB_graph$Runs,epi_pop=SA_epi_cumulative_DSTB_graph$epi_pop,strategy=SA_epi_cumulative_DSTB_graph$strategy,population=SA_epi_cumulative_DSTB_graph$population), FUN=sum)
  #cumulative epi graph
  SA_epi_DSTB_cumulat_graph <- ggplot(data=SA_epi_DSTB_cumulat, aes(epi_pop,x,color=strategy)) + 
    geom_boxplot(notch = TRUE) +
    scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
    facet_wrap(~population, dir = "h", scales = "free") +
    labs(x = "population group",y = expression(paste("cumulative TB cases  (DS-TB)")),colour = "",subtitle="",caption="")+
    guides(fill = "none")+
    stat_boxplot(geom = "errorbar", width = 0.5) +  
    geom_boxplot()+
    scale_color_brewer(palette = "Set1")+
    theme_classic()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  ggsave("2A. Epidemiological projections - DS-TB cases, South Africa, 2020–2035.jpeg", 
         width = 25, height = 16, units = "cm")
  
  
  
  SA_overall_epi_DSTB_table<-SA_epi_DSTB_cumulat %>% 
    group_by(strategy,population) %>% 
    summarise_at(vars(x),
                 list(median=median,
                      mean=mean,
                      quantile_lower = ~quantile(., probs = 0.025),
                      quantile_upper = ~quantile(., probs = 0.975)))
  write.csv(SA_overall_epi_DSTB_table, file = "2A. South African - cumulative DS-TB cases tables.csv")
  
  
  
  
  
  
  #####################################################################################################################
  #                                  2B. Cumulative overall TB per population group#
  #####################################################################################################################
  #Data wrangling: Melt data in key packets for plotting
  SA_epi_cumulative_TBall<-data.frame(
    Runs=SA_baseline$Run,
    Baseline=SA_baseline$TB_cases_all,
    Baseline_HIV_neg=SA_baseline$TB_cases_all-SA_baseline$TB_cases_HIV,
    Baseline_PLHIV=SA_baseline$TB_cases_HIV,
    "Expanded 6H"=SA_scaleup$TB_cases_all,
    "Expanded 6H_neg"=SA_scaleup$TB_cases_all-SA_scaleup$TB_cases_HIV,
    "Expanded 6H_PLHIV"=SA_scaleup$TB_cases_HIV,
    "Minimal regimen"=SA_minimal$TB_cases_all,
    "Minimal regimen_HIV_neg"=SA_minimal$TB_cases_all-SA_minimal$TB_cases_HIV,
    "Minimal regimen_PLHIV"=SA_minimal$TB_cases_HIV,
    "Optimal regimen"=SA_optimal$TB_cases_all,
    "Optimal regimen_HIV_neg"=SA_optimal$TB_cases_all-SA_optimal$TB_cases_HIV,
    "Optimal regimen_PLHIV"=SA_optimal$TB_cases_HIV)
  
  SA_epi_cumulative_TBall_graph<-melt(SA_epi_cumulative_TBall, id.vars =c("Runs"), variable.name = "epi_pop")
  SA_epi_cumulative_TBall_graph$strategy = ifelse(grepl("Baseline", SA_epi_cumulative_TBall_graph$epi_pop), "_baseline",
                                                 ifelse(grepl("6H", SA_epi_cumulative_TBall_graph$epi_pop),"6H",ifelse(grepl("Minimal", SA_epi_cumulative_TBall_graph$epi_pop), "minimal","Optimal")))
  SA_epi_cumulative_TBall_graph$population = ifelse(!grepl("_", SA_epi_cumulative_TBall_graph$epi_pop), "All treatment candidates",
                                                   ifelse(grepl("PLHIV", SA_epi_cumulative_TBall_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
  SA_epi_cumulative_TBall_graph$epi_pop = chartr("."," ",SA_epi_cumulative_TBall_graph$epi_pop)
  
  #####################################################################################################################
  
  SA_epi_cumulat<-aggregate(x=SA_epi_cumulative_TBall_graph$value, by=list(Runs=SA_epi_cumulative_TBall_graph$Runs,epi_pop=SA_epi_cumulative_TBall_graph$epi_pop,strategy=SA_epi_cumulative_TBall_graph$strategy,population=SA_epi_cumulative_TBall_graph$population), FUN=sum)
  #cumulative epi graph
  SA_epi_cumulat_graph <- ggplot(data=SA_epi_cumulat, aes(epi_pop,x,color=strategy)) + 
    geom_boxplot(notch = TRUE) +
    scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
    facet_wrap(~population, dir = "h", scales = "free") +
    labs(x = "population group",y = expression(paste("cumulative TB cases  (DS-TB & RR-TB)")),colour = "",subtitle="",caption="")+
    guides(fill = "none")+
    stat_boxplot(geom = "errorbar", width = 0.5) +  
    geom_boxplot()+
    scale_color_brewer(palette = "Set1")+
    theme_classic()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  ggsave("2B. Epidemiological projections, South Africa, 2020–2035.jpeg", 
         width = 25, height = 16, units = "cm")
  
  
  
  SA_overall_epi_table<-SA_epi_cumulat %>% 
    group_by(strategy,population) %>% 
    summarise_at(vars(x),
                 list(median=median,
                      mean=mean,
                      quantile_lower = ~quantile(., probs = 0.025),
                      quantile_upper = ~quantile(., probs = 0.975)))
  write.csv(SA_overall_epi_table, file = "2B. South African - cumulative TB cases (DS-TB & RR-TB) tables.csv")
  
  
  

  
  #####################################################################################################################
  #                                  2C. Cumulative RR-TB per population group#
  #####################################################################################################################
  #Data wrangling: Melt data in key packets for plotting
  SA_epi_cumulative_DRTB<-data.frame(
    Runs=SA_baseline$Run,
    Baseline=SA_baseline$TB_cases_MDR,
    Baseline_HIV_neg=SA_baseline$TB_cases_MDR-SA_baseline$TB_cases_MDR_HIV,
    Baseline_PLHIV=SA_baseline$TB_cases_MDR_HIV,
    "Expanded 6H"=SA_scaleup$TB_cases_MDR,
    "Expanded 6H_neg"=SA_scaleup$TB_cases_MDR-SA_scaleup$TB_cases_MDR_HIV,
    "Expanded 6H_PLHIV"=SA_scaleup$TB_cases_MDR_HIV,
    "Minimal regimen"=SA_minimal$TB_cases_MDR,
    "Minimal regimen_HIV_neg"=SA_minimal$TB_cases_MDR-SA_minimal$TB_cases_MDR_HIV,
    "Minimal regimen_PLHIV"=SA_minimal$TB_cases_MDR_HIV,
    "Optimal regimen"=SA_optimal$TB_cases_MDR,
    "Optimal regimen_HIV_neg"=SA_optimal$TB_cases_MDR-SA_optimal$TB_cases_MDR_HIV,
    "Optimal regimen_PLHIV"=SA_optimal$TB_cases_MDR_HIV)
  
  SA_epi_cumulative_DRTB_graph<-melt(SA_epi_cumulative_DRTB, id.vars =c("Runs"), variable.name = "epi_pop")
  SA_epi_cumulative_DRTB_graph$strategy = ifelse(grepl("Baseline", SA_epi_cumulative_DRTB_graph$epi_pop), "_baseline",
                                                  ifelse(grepl("6H", SA_epi_cumulative_DRTB_graph$epi_pop),"6H",ifelse(grepl("Minimal", SA_epi_cumulative_DRTB_graph$epi_pop), "minimal","Optimal")))
  SA_epi_cumulative_DRTB_graph$population = ifelse(!grepl("_", SA_epi_cumulative_DRTB_graph$epi_pop), "All treatment candidates",
                                                    ifelse(grepl("PLHIV", SA_epi_cumulative_DRTB_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
  SA_epi_cumulative_DRTB_graph$epi_pop = chartr("."," ",SA_epi_cumulative_DRTB_graph$epi_pop)
  
  #####################################################################################################################
  
  SA_epi_cumulat_DRTB<-aggregate(x=SA_epi_cumulative_DRTB_graph$value, by=list(Runs=SA_epi_cumulative_DRTB_graph$Runs,epi_pop=SA_epi_cumulative_DRTB_graph$epi_pop,strategy=SA_epi_cumulative_DRTB_graph$strategy,population=SA_epi_cumulative_DRTB_graph$population), FUN=sum)
  #cumulative epi graph
  SA_epi_cumulat_graph_DRTB <- ggplot(data=SA_epi_cumulat_DRTB, aes(epi_pop,x,color=strategy)) + 
    geom_boxplot(notch = TRUE) +
    scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
    facet_wrap(~population, dir = "h", scales = "free") +
    labs(x = "population group",y = expression(paste("cumulative RR-TB cases")),colour = "",subtitle="",caption="")+
    guides(fill = "none")+
    stat_boxplot(geom = "errorbar", width = 0.5) +  
    geom_boxplot()+
    scale_color_brewer(palette = "Set1")+
    theme_classic()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  ggsave("2C. RR-TB Epidemiological projections, South Africa, 2020–2035.jpeg", 
         width = 25, height = 16, units = "cm")
  
  
  
  SA_overall_DRTB_epi_table<-SA_epi_cumulat_DRTB %>% 
    group_by(strategy,population) %>% 
    summarise_at(vars(x),
                 list(median=median,
                      mean=mean,
                      quantile_lower = ~quantile(., probs = 0.025),
                      quantile_upper = ~quantile(., probs = 0.975)))
  write.csv(SA_overall_DRTB_epi_table, file = "2C. South African - cumulative RR-TB cases tables.csv")
  
  
  
  
  
  #####################################################################################################################
  #                                  2D. Cumulative overall PT per population group#
  #####################################################################################################################
  #Data wrangling: Melt data in key packets for plotting
  SA_epi_cumulative_PT<-data.frame(
    Runs=SA_baseline$Run,
    Baseline=SA_baseline$New_PT_all,
    Baseline_HIV_neg=SA_baseline$New_PT_all-SA_baseline$New_PT_HIV,
    Baseline_PLHIV=SA_baseline$New_PT_HIV,
    "Expanded 6H"=SA_scaleup$New_PT_all,
    "Expanded 6H_neg"=SA_scaleup$New_PT_all-SA_scaleup$New_PT_HIV,
    "Expanded 6H_PLHIV"=SA_scaleup$New_PT_HIV,
    "Minimal regimen"=SA_minimal$New_PT_all,
    "Minimal regimen_HIV_neg"=SA_minimal$New_PT_all-SA_minimal$New_PT_HIV,
    "Minimal regimen_PLHIV"=SA_minimal$New_PT_HIV,
    "Optimal regimen"=SA_optimal$New_PT_all,
    "Optimal regimen_HIV_neg"=SA_optimal$New_PT_all-SA_optimal$New_PT_HIV,
    "Optimal regimen_PLHIV"=SA_optimal$New_PT_HIV)
  
  SA_epi_cumulative_PT_graph<-melt(SA_epi_cumulative_PT, id.vars =c("Runs"), variable.name = "epi_pop")
  SA_epi_cumulative_PT_graph$strategy = ifelse(grepl("Baseline", SA_epi_cumulative_PT_graph$epi_pop), "_baseline",
                                                  ifelse(grepl("6H", SA_epi_cumulative_PT_graph$epi_pop),"6H",ifelse(grepl("Minimal", SA_epi_cumulative_PT_graph$epi_pop), "minimal","Optimal")))
  SA_epi_cumulative_PT_graph$population = ifelse(!grepl("_", SA_epi_cumulative_PT_graph$epi_pop), "All treatment candidates",
                                                    ifelse(grepl("PLHIV", SA_epi_cumulative_PT_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
  SA_epi_cumulative_PT_graph$epi_pop = chartr("."," ",SA_epi_cumulative_PT_graph$epi_pop)
  
  #####################################################################################################################
  
  SA_PT_epi_cumulat<-aggregate(x=SA_epi_cumulative_PT_graph$value, by=list(Runs=SA_epi_cumulative_PT_graph$Runs,epi_pop=SA_epi_cumulative_PT_graph$epi_pop,strategy=SA_epi_cumulative_PT_graph$strategy,population=SA_epi_cumulative_PT_graph$population), FUN=sum)
  #cumulative epi graph
  SA_PT_epi_cumulat_graph <- ggplot(data=SA_PT_epi_cumulat, aes(epi_pop,x,color=strategy)) + 
    geom_boxplot(notch = TRUE) +
    scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
    facet_wrap(~population, dir = "h", scales = "free") +
    labs(x = "population group",y = expression(paste("Individuals on preventive therapy, 2020–2035")),colour = "",subtitle="",caption="")+
    guides(fill = "none")+
    stat_boxplot(geom = "errorbar", width = 0.5) +  
    geom_boxplot()+
    scale_color_brewer(palette = "Set1")+
    theme_classic()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  ggsave("2D. Epidemiological PT projections, South Africa, 2020–2035.jpeg", 
         width = 25, height = 16, units = "cm")
  
  
  
  SA_overall_PT_epi_table<-SA_PT_epi_cumulat %>% 
    group_by(strategy,population) %>% 
    summarise_at(vars(x),
                 list(median=median,
                      mean=mean,
                      quantile_lower = ~quantile(., probs = 0.025),
                      quantile_upper = ~quantile(., probs = 0.975)))
  write.csv(SA_overall_PT_epi_table, file = "2D. South African - Total individuals on preventive therapy.csv")
  
  
  
  
  
  
  #####################################################################################################################
  #                                  3. Cumulative TB deaths per population group#
  #####################################################################################################################
  #Data wrangling: Melt data in key packets for plotting
  SA_epi_cumulative_TBdeaths<-data.frame(
    Runs=SA_baseline$Run,
    Baseline=SA_baseline$TB_deaths_all,
    Baseline_HIV_neg=SA_baseline$TB_deaths_all-SA_baseline$TB_deaths_HIV,
    Baseline_PLHIV=SA_baseline$TB_deaths_HIV,
    "Expanded 6H"=SA_scaleup$TB_deaths_all,
    "Expanded 6H_neg"=SA_scaleup$TB_deaths_all-SA_scaleup$TB_deaths_HIV,
    "Expanded 6H_PLHIV"=SA_scaleup$TB_deaths_HIV,
    "Minimal regimen"=SA_minimal$TB_deaths_all,
    "Minimal regimen_HIV_neg"=SA_minimal$TB_deaths_all-SA_minimal$TB_deaths_HIV,
    "Minimal regimen_PLHIV"=SA_minimal$TB_deaths_HIV,
    "Optimal regimen"=SA_optimal$TB_deaths_all,
    "Optimal regimen_HIV_neg"=SA_optimal$TB_deaths_all-SA_optimal$TB_deaths_HIV,
    "Optimal regimen_PLHIV"=SA_optimal$TB_deaths_HIV)
  
  SA_epi_cumulative_TBdeaths_graph<-melt(SA_epi_cumulative_TBdeaths, id.vars =c("Runs"), variable.name = "epi_pop")
  SA_epi_cumulative_TBdeaths_graph$strategy = ifelse(grepl("Baseline", SA_epi_cumulative_TBdeaths_graph$epi_pop), "_baseline",
                                                 ifelse(grepl("6H", SA_epi_cumulative_TBdeaths_graph$epi_pop),"6H",ifelse(grepl("Minimal", SA_epi_cumulative_TBdeaths_graph$epi_pop), "minimal","Optimal")))
  SA_epi_cumulative_TBdeaths_graph$population = ifelse(!grepl("_", SA_epi_cumulative_TBdeaths_graph$epi_pop), "All treatment candidates",
                                                   ifelse(grepl("PLHIV", SA_epi_cumulative_TBdeaths_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
  SA_epi_cumulative_TBdeaths_graph$epi_pop = chartr("."," ",SA_epi_cumulative_TBdeaths_graph$epi_pop)
  
  #####################################################################################################################
  
  SA_epi_cumulat_TBdeaths<-aggregate(x=SA_epi_cumulative_TBdeaths_graph$value, by=list(Runs=SA_epi_cumulative_TBdeaths_graph$Runs,epi_pop=SA_epi_cumulative_TBdeaths_graph$epi_pop,strategy=SA_epi_cumulative_TBdeaths_graph$strategy,population=SA_epi_cumulative_TBdeaths_graph$population), FUN=sum)
  #cumulative epi graph
  SA_epi_cumulat_graph_TBdeaths <- ggplot(data=SA_epi_cumulat_TBdeaths, aes(epi_pop,x,color=strategy)) + 
    geom_boxplot(notch = TRUE) +
    scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
    facet_wrap(~population, dir = "h", scales = "free") +
    labs(x = "population group",y = expression(paste("TB deaths")),colour = "",subtitle="",caption="")+
    guides(fill = "none")+
    stat_boxplot(geom = "errorbar", width = 0.5) +  
    geom_boxplot()+
    scale_color_brewer(palette = "Set1")+
    theme_classic()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  ggsave("3. TB deaths Epidemiological projections, South Africa, 2020–2035.jpeg", 
         width = 25, height = 16, units = "cm")
  
  
  
  SA_overall_TBdeaths_epi_table<-SA_epi_cumulat_TBdeaths %>% 
    group_by(strategy,population) %>% 
    summarise_at(vars(x),
                 list(median=median,
                      mean=mean,
                      quantile_lower = ~quantile(., probs = 0.025),
                      quantile_upper = ~quantile(., probs = 0.975)))
  write.csv(SA_overall_TBdeaths_epi_table, file = "3. South African - cumulative TB deaths tables.csv")
  
  
  
  
  
  
  
  
  #####################################################################################################################
  #                                  4. Cumulative DALYs per population group#
  #####################################################################################################################
  #Data wrangling: Melt data in key packets for plotting
  SA_epi_cumulative_DALYs<-data.frame(
    Runs=SA_baseline$Run,
    Baseline=SA_baseline$YLD+SA_baseline$YLL,
    Baseline_HIV_neg=(SA_baseline$YLD+SA_baseline$YLL)-(SA_baseline$YLD_HIV+SA_baseline$YLL_HIV),
    Baseline_PLHIV=SA_baseline$YLD_HIV+SA_baseline$YLL_HIV,
    "Expanded 6H"=SA_scaleup$YLD+SA_scaleup$YLL,
    "Expanded 6H_neg"=(SA_scaleup$YLD+SA_scaleup$YLL)-(SA_scaleup$YLD_HIV+SA_scaleup$YLL_HIV),
    "Expanded 6H_PLHIV"=SA_scaleup$YLD_HIV+SA_scaleup$YLL_HIV,
    "Minimal regimen"=SA_minimal$YLD+SA_minimal$YLL,
    "Minimal regimen_HIV_neg"=(SA_minimal$YLD+SA_minimal$YLL)-(SA_minimal$YLD_HIV+SA_minimal$YLL_HIV),
    "Minimal regimen_PLHIV"=SA_minimal$YLD_HIV+SA_minimal$YLL_HIV,
    "Optimal regimen"=SA_optimal$YLD+SA_optimal$YLL,
    "Optimal regimen_HIV_neg"=(SA_optimal$YLD+SA_optimal$YLL)-(SA_optimal$YLD_HIV+SA_optimal$YLL_HIV),
    "Optimal regimen_PLHIV"=SA_optimal$YLD_HIV+SA_optimal$YLL_HIV)
  
  SA_epi_cumulative_DALYs_graph<-melt(SA_epi_cumulative_DALYs, id.vars =c("Runs"), variable.name = "epi_pop")
  SA_epi_cumulative_DALYs_graph$strategy = ifelse(grepl("Baseline", SA_epi_cumulative_DALYs_graph$epi_pop), "_baseline",
                                                     ifelse(grepl("6H", SA_epi_cumulative_DALYs_graph$epi_pop),"6H",ifelse(grepl("Minimal", SA_epi_cumulative_DALYs_graph$epi_pop), "minimal","Optimal")))
  SA_epi_cumulative_DALYs_graph$population = ifelse(!grepl("_", SA_epi_cumulative_DALYs_graph$epi_pop), "All treatment candidates",
                                                       ifelse(grepl("PLHIV", SA_epi_cumulative_DALYs_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
  SA_epi_cumulative_DALYs_graph$epi_pop = chartr("."," ",SA_epi_cumulative_DALYs_graph$epi_pop)
  
  #####################################################################################################################
  
  SA_epi_cumulat_DALYs<-aggregate(x=SA_epi_cumulative_DALYs_graph$value, by=list(Runs=SA_epi_cumulative_DALYs_graph$Runs,epi_pop=SA_epi_cumulative_DALYs_graph$epi_pop,strategy=SA_epi_cumulative_DALYs_graph$strategy,population=SA_epi_cumulative_DALYs_graph$population), FUN=sum)
  #cumulative epi graph
  SA_epi_cumulat_graph_DALYs <- ggplot(data=SA_epi_cumulat_DALYs, aes(epi_pop,x,color=strategy)) + 
    geom_boxplot(notch = TRUE) +
    scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
    facet_wrap(~population, dir = "h", scales = "free") +
    labs(x = "population group",y = expression(paste("DALYs")),colour = "",subtitle="",caption="")+
    guides(fill = "none")+
    stat_boxplot(geom = "errorbar", width = 0.5) +  
    geom_boxplot()+
    scale_color_brewer(palette = "Set1")+
    theme_classic()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  ggsave("4. DALYs - Epidemiological projections, South Africa, 2020–2035.jpeg", 
         width = 25, height = 16, units = "cm")
  
  
  
  SA_overall_DALYs_epi_table<-SA_epi_cumulat_DALYs %>% 
    group_by(strategy,population) %>% 
    summarise_at(vars(x),
                 list(median=median,
                      mean=mean,
                      quantile_lower = ~quantile(., probs = 0.025),
                      quantile_upper = ~quantile(., probs = 0.975)))
  write.csv(SA_overall_DALYs_epi_table, file = "4. South African - cumulative DALYs tables.csv")
  
  
  
  
  
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  #                                     Incremental cost                                                              #
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  SA_strategy_cost_cumulative<-data.frame(
    Runs=SA_baseline$Run,
    Baseline=SA_baseline$C_baseline_all_DOT_SAT,
    Baseline_HIV_neg=SA_baseline$C_baseline_hiv_neg_DOT_SAT,
    Baseline_PLHIV=SA_baseline$C_baseline_hiv_DOT_SAT,
    "Expanded 6H"=SA_scaleup$C_scaleup_all_DOT_SAT,
    "Expanded 6H_neg"=SA_scaleup$C_scaleup_hiv_neg_DOT_SAT,
    "Expanded 6H_PLHIV"=SA_scaleup$C_scaleup_hiv_DOT_SAT,
    "Minimal regimen"=SA_minimal$C_minimal_all_DOT_SAT,
    "Minimal regimen_HIV_neg"=SA_minimal$C_minimal_hiv_neg_DOT_SAT,
    "Minimal regimen_PLHIV"=SA_minimal$C_minimal_hiv_DOT_SAT,
    "Optimal regimen"=SA_optimal$C_optimal_all_DOT_SAT,
    "Optimal regimen_HIV_neg"=SA_optimal$C_optimal_hiv_neg_DOT_SAT,
    "Optimal regimen_PLHIV"=SA_optimal$C_optimal_hiv_DOT_SAT)
  
  SA_cost_pop_graph_cumulative<-melt(SA_strategy_cost_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
  SA_cost_pop_graph_cumulative$cum_cost<-ave(SA_cost_pop_graph_cumulative$value, SA_cost_pop_graph_cumulative$cost_pop, FUN=cumsum)
  # SA_cost_pop_graph_cumulative$Year<- as.Date(ISOdate(SA_cost_pop_graph_cumulative$Year,1,1) ) #set up date variable
  SA_cost_pop_graph_cumulative$strategy = ifelse(grepl("Baseline", SA_cost_pop_graph_cumulative$cost_pop), "_baseline",
                                                 ifelse(grepl("6H", SA_cost_pop_graph_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", SA_cost_pop_graph_cumulative$cost_pop), "minimal","Optimal")))
  SA_cost_pop_graph_cumulative$population = ifelse(!grepl("_", SA_cost_pop_graph_cumulative$cost_pop), "All treatment candidates",
                                                   ifelse(grepl("PLHIV", SA_cost_pop_graph_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
  SA_cost_pop_graph_cumulative$cost_pop = chartr("."," ",SA_cost_pop_graph_cumulative$cost_pop)
  
  #####################################################################################################################
  
  SA_cost_cumulat<-aggregate(x=SA_cost_pop_graph_cumulative$value, by=list(Runs=SA_cost_pop_graph_cumulative$Runs,cost_pop=SA_cost_pop_graph_cumulative$cost_pop,strategy=SA_cost_pop_graph_cumulative$strategy,population=SA_cost_pop_graph_cumulative$population), FUN=sum)
  #cumulative cost graph
  SA_cost_cumulat_graph <- ggplot(data=SA_cost_cumulat, aes(cost_pop,x,color=strategy)) + 
    geom_boxplot(notch = TRUE) +
    scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
    facet_wrap(~population, dir = "h", scales = "free") +
    labs(x = "population group",y = expression(paste("cumulative cost (US$ Billions)")),colour = "",subtitle="",caption="")+
    guides(fill = "none")+
    stat_boxplot(geom = "errorbar", width = 0.5) +  
    geom_boxplot()+
    scale_color_brewer(palette = "Set1")+
    theme_classic()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  ggsave("1A. SA_graph_cumulative_costs.jpeg", 
         width = 25, height = 16, units = "cm")
  
  
  
  SA_overall_cost_table<-SA_cost_cumulat %>% 
    group_by(cost_pop, strategy,population) %>% 
    summarise_at(vars(x),
                 list(median=median,
                      mean=mean,
                      quantile_lower = ~quantile(., probs = 0.025),
                      quantile_upper = ~quantile(., probs = 0.975)))
  write.csv(SA_overall_cost_table, file = "1A. South African cumulative cost output tables.csv")
  
  
  
  
  #Incremental cost - Overall (PT + TB)
  SA_c_baseline_vs_6h_overall <- data.frame(Runs=SA_baseline$Run,SA_strategy_cost$Expanded.6H-SA_strategy_cost$Baseline)
  SA_c_baseline_vs_minimal_overall <- data.frame(SA_strategy_cost$Minimal.regimen-SA_strategy_cost$Baseline)
  SA_c_baseline_vs_optimal_overall <- data.frame(SA_strategy_cost$Optimal.regimen-SA_strategy_cost$Baseline)
  SA_c_6h_vs_minimal_overall <- data.frame(SA_strategy_cost$Minimal.regimen-SA_strategy_cost$Expanded.6H)
  SA_c_6h_vs_optimal_overall <- data.frame(SA_strategy_cost$Optimal.regimen-SA_strategy_cost$Expanded.6H)
  SA_c_minimal_vs_optimal_overall <- data.frame(SA_strategy_cost$Optimal.regimen-SA_strategy_cost$Minimal.regimen)
  #####################################################################################################################################
  
  #Incremental cost - Overall (PT)
  SA_cPT_baseline_vs_6h_overall <- data.frame(SA_scaleup$C_PT_all - SA_baseline$C_PT_all)
  SA_cPT_baseline_vs_minimal_overall <- data.frame(SA_minimal$C_PT_all - SA_baseline$C_PT_all)
  SA_cPT_baseline_vs_optimal_overall <- data.frame(SA_optimal$C_PT_all - SA_baseline$C_PT_all)
  SA_cPT_6h_vs_minimal_overall <- data.frame(SA_minimal$C_PT_all - SA_scaleup$C_PT_all)
  SA_cPT_6h_vs_optimal_overall <- data.frame(SA_optimal$C_PT_all - SA_scaleup$C_PT_all)
  SA_cPT_minimal_vs_optimal_overall <- data.frame(SA_optimal$C_PT_all - SA_minimal$C_PT_all)
  
  #####################################################################################################################################
  #Incremental cost - Overall (TB)
  SA_cTB_baseline_vs_6h_overall <- data.frame(SA_scaleup$C_TB_all_DOT_SAT - SA_baseline$C_TB_all_DOT_SAT)
  SA_cTB_baseline_vs_minimal_overall <- data.frame(SA_minimal$C_TB_all_DOT_SAT - SA_baseline$C_TB_all_DOT_SAT)
  SA_cTB_baseline_vs_optimal_overall <- data.frame(SA_optimal$C_TB_all_DOT_SAT - SA_baseline$C_TB_all_DOT_SAT)
  SA_cTB_6h_vs_minimal_overall <- data.frame(SA_minimal$C_TB_all_DOT_SAT - SA_scaleup$C_TB_all_DOT_SAT)
  SA_cTB_6h_vs_optimal_overall <- data.frame(SA_optimal$C_TB_all_DOT_SAT - SA_scaleup$C_TB_all_DOT_SAT)
  SA_cTB_minimal_vs_optimal_overall <- data.frame(SA_optimal$C_TB_all_DOT_SAT - SA_minimal$C_TB_all_DOT_SAT)
  #####################################################################################################################################
  
  
  #Incremental cost - HIV negative (PT + TB)
  SA_c_baseline_vs_6h_HIV_neg <- data.frame(SA_strategy_cost$Expanded.6H_neg-SA_strategy_cost$Baseline_HIV_neg)
  SA_c_baseline_vs_minimal_HIV_neg <- data.frame(SA_strategy_cost$Minimal.regimen_HIV_neg-SA_strategy_cost$Baseline_HIV_neg)
  SA_c_baseline_vs_optimal_HIV_neg <- data.frame(SA_strategy_cost$Optimal.regimen_HIV_neg-SA_strategy_cost$Baseline_HIV_neg)
  SA_c_6h_vs_minimal_HIV_neg <- data.frame(SA_strategy_cost$Minimal.regimen_HIV_neg-SA_strategy_cost$Expanded.6H_neg)
  SA_c_6h_vs_optimal_HIV_neg <- data.frame(SA_strategy_cost$Optimal.regimen_HIV_neg-SA_strategy_cost$Expanded.6H_neg)
  SA_c_minimal_vs_optimal_HIV_neg <- data.frame(SA_strategy_cost$Optimal.regimen_HIV_neg-SA_strategy_cost$Minimal.regimen_HIV_neg)
  
  #####################################################################################################################################
  
  #Incremental cost - HIV negative (PT)
  SA_cPT_baseline_vs_6h_HIV_neg <- data.frame(SA_scaleup$C_PT_hiv_neg - SA_baseline$C_PT_hiv_neg)
  SA_cPT_baseline_vs_minimal_HIV_neg <- data.frame(SA_minimal$C_PT_hiv_neg - SA_baseline$C_PT_hiv_neg)
  SA_cPT_baseline_vs_optimal_HIV_neg <- data.frame(SA_optimal$C_PT_hiv_neg - SA_baseline$C_PT_hiv_neg)
  SA_cPT_6h_vs_minimal_HIV_neg <- data.frame(SA_minimal$C_PT_hiv_neg - SA_scaleup$C_PT_hiv_neg)
  SA_cPT_6h_vs_optimal_HIV_neg <- data.frame(SA_optimal$C_PT_hiv_neg - SA_scaleup$C_PT_hiv_neg)
  SA_cPT_minimal_vs_optimal_HIV_neg <- data.frame(SA_optimal$C_PT_hiv_neg - SA_minimal$C_PT_hiv_neg)
  
  #####################################################################################################################################
  
  
  #Incremental cost - HIV negative (TB)
  SA_cTB_baseline_vs_6h_HIV_neg <- data.frame(SA_scaleup$C_TB_hiv_neg_DOT_SAT - SA_baseline$C_TB_hiv_neg_DOT_SAT)
  SA_cTB_baseline_vs_minimal_HIV_neg <- data.frame(SA_minimal$C_TB_hiv_neg_DOT_SAT - SA_baseline$C_TB_hiv_neg_DOT_SAT)
  SA_cTB_baseline_vs_optimal_HIV_neg <- data.frame(SA_optimal$C_TB_hiv_neg_DOT_SAT - SA_baseline$C_TB_hiv_neg_DOT_SAT)
  SA_cTB_6h_vs_minimal_HIV_neg <- data.frame(SA_minimal$C_TB_hiv_neg_DOT_SAT - SA_scaleup$C_TB_hiv_neg_DOT_SAT)
  SA_cTB_6h_vs_optimal_HIV_neg <- data.frame(SA_optimal$C_TB_hiv_neg_DOT_SAT - SA_scaleup$C_TB_hiv_neg_DOT_SAT)
  SA_cTB_minimal_vs_optimal_HIV_neg <- data.frame(SA_optimal$C_TB_hiv_neg_DOT_SAT - SA_minimal$C_TB_hiv_neg_DOT_SAT)
  
  #####################################################################################################################################
  
  
  #Incremental cost - PLHIV (PT+TB)
  SA_c_baseline_vs_6h_PLHIV <- data.frame(SA_strategy_cost$Expanded.6H_PLHIV-SA_strategy_cost$Baseline_PLHIV)
  SA_c_baseline_vs_minimal_PLHIV <- data.frame(SA_strategy_cost$Minimal.regimen_PLHIV-SA_strategy_cost$Baseline_PLHIV)
  SA_c_baseline_vs_optimal_PLHIV <- data.frame(SA_strategy_cost$Optimal.regimen_PLHIV-SA_strategy_cost$Baseline_PLHIV)
  SA_c_6h_vs_minimal_PLHIV <- data.frame(SA_strategy_cost$Minimal.regimen_PLHIV-SA_strategy_cost$Expanded.6H_PLHIV)
  SA_c_6h_vs_optimal_PLHIV <- data.frame(SA_strategy_cost$Optimal.regimen_PLHIV-SA_strategy_cost$Expanded.6H_PLHIV)
  SA_c_minimal_vs_optimal_PLHIV <- data.frame(SA_strategy_cost$Optimal.regimen_PLHIV-SA_strategy_cost$Minimal.regimen_PLHIV)
  
  #####################################################################################################################################
  
  
  #Incremental cost - PLHIV (PT)
  SA_cPT_baseline_vs_6h_PLHIV <- data.frame(SA_scaleup$C_PT_hiv - SA_baseline$C_PT_hiv)
  SA_cPT_baseline_vs_minimal_PLHIV <- data.frame(SA_minimal$C_PT_hiv - SA_baseline$C_PT_hiv)
  SA_cPT_baseline_vs_optimal_PLHIV <- data.frame(SA_optimal$C_PT_hiv - SA_baseline$C_PT_hiv)
  SA_cPT_6h_vs_minimal_PLHIV <- data.frame(SA_minimal$C_PT_hiv - SA_scaleup$C_PT_hiv)
  SA_cPT_6h_vs_optimal_PLHIV <- data.frame(SA_optimal$C_PT_hiv - SA_scaleup$C_PT_hiv)
  SA_cPT_minimal_vs_optimal_PLHIV <- data.frame(SA_optimal$C_PT_hiv - SA_minimal$C_PT_hiv)
  
  #####################################################################################################################################
  
  
  #Incremental cost - PLHIV (TB)
  SA_cTB_baseline_vs_6h_PLHIV <- data.frame(SA_scaleup$C_TB_hiv_DOT_SAT - SA_baseline$C_TB_hiv_DOT_SAT)
  SA_cTB_baseline_vs_minimal_PLHIV <- data.frame(SA_minimal$C_TB_hiv_DOT_SAT - SA_baseline$C_TB_hiv_DOT_SAT)
  SA_cTB_baseline_vs_optimal_PLHIV <- data.frame(SA_optimal$C_TB_hiv_DOT_SAT - SA_baseline$C_TB_hiv_DOT_SAT)
  SA_cTB_6h_vs_minimal_PLHIV <- data.frame(SA_minimal$C_TB_hiv_DOT_SAT - SA_scaleup$C_TB_hiv_DOT_SAT)
  SA_cTB_6h_vs_optimal_PLHIV <- data.frame(SA_optimal$C_TB_hiv_DOT_SAT - SA_scaleup$C_TB_hiv_DOT_SAT)
  SA_cTB_minimal_vs_optimal_PLHIV <- data.frame(SA_optimal$C_TB_hiv_DOT_SAT - SA_minimal$C_TB_hiv_DOT_SAT)
  
  #####################################################################################################################################
  #####################################################################################################################################

  #combine columns
  SA_incr_cost<- data.frame(SA_c_baseline_vs_6h_overall,SA_c_baseline_vs_minimal_overall,SA_c_baseline_vs_optimal_overall,SA_c_6h_vs_minimal_overall,SA_c_6h_vs_optimal_overall,SA_c_minimal_vs_optimal_overall,  #Incremental cost - Overall (PT + TB)
                            SA_cPT_baseline_vs_6h_overall,SA_cPT_baseline_vs_minimal_overall,SA_cPT_baseline_vs_optimal_overall,SA_cPT_6h_vs_minimal_overall,SA_cPT_6h_vs_optimal_overall,SA_cPT_minimal_vs_optimal_overall,  #Incremental cost - Overall (PT)
                            SA_cTB_baseline_vs_6h_overall,SA_cTB_baseline_vs_minimal_overall,SA_cTB_baseline_vs_optimal_overall,SA_cTB_6h_vs_minimal_overall,SA_cTB_6h_vs_optimal_overall,SA_cTB_minimal_vs_optimal_overall,  #Incremental cost - Overall (TB)
                            SA_c_baseline_vs_6h_HIV_neg,SA_c_baseline_vs_minimal_HIV_neg,SA_c_baseline_vs_optimal_HIV_neg,SA_c_6h_vs_minimal_HIV_neg,SA_c_6h_vs_optimal_HIV_neg,SA_c_minimal_vs_optimal_HIV_neg,  #Incremental cost - HIV negative (PT + TB)
                            SA_cPT_baseline_vs_6h_HIV_neg,SA_cPT_baseline_vs_minimal_HIV_neg,SA_cPT_baseline_vs_optimal_HIV_neg,SA_cPT_6h_vs_minimal_HIV_neg,SA_cPT_6h_vs_optimal_HIV_neg,SA_cPT_minimal_vs_optimal_HIV_neg, #Incremental cost - HIV negative (PT)
                            SA_cTB_baseline_vs_6h_HIV_neg,SA_cTB_baseline_vs_minimal_HIV_neg,SA_cTB_baseline_vs_optimal_HIV_neg,SA_cTB_6h_vs_minimal_HIV_neg,SA_cTB_6h_vs_optimal_HIV_neg,SA_cTB_minimal_vs_optimal_HIV_neg,  #Incremental cost - HIV negative (TB)
                            SA_c_baseline_vs_6h_PLHIV,SA_c_baseline_vs_minimal_PLHIV,SA_c_baseline_vs_optimal_PLHIV,SA_c_6h_vs_minimal_PLHIV,SA_c_6h_vs_optimal_PLHIV,SA_c_minimal_vs_optimal_PLHIV,  #Incremental cost - PLHIV (PT+TB)
                            SA_cPT_baseline_vs_6h_PLHIV,SA_cPT_baseline_vs_minimal_PLHIV,SA_cPT_baseline_vs_optimal_PLHIV,SA_cPT_6h_vs_minimal_PLHIV,SA_cPT_6h_vs_optimal_PLHIV,SA_cPT_minimal_vs_optimal_PLHIV,  #Incremental cost - PLHIV (PT)
                            SA_cTB_baseline_vs_6h_PLHIV,SA_cTB_baseline_vs_minimal_PLHIV,SA_cTB_baseline_vs_optimal_PLHIV,SA_cTB_6h_vs_minimal_PLHIV,SA_cTB_6h_vs_optimal_PLHIV,SA_cTB_minimal_vs_optimal_PLHIV   #Incremental cost - PLHIV (TB)
                            )
  colnames(SA_incr_cost) <- c("Runs","SA_c_baseline_vs_6h_overall","SA_c_baseline_vs_minimal_overall","SA_c_baseline_vs_optimal_overall","SA_c_6h_vs_minimal_overall","SA_c_6h_vs_optimal_overall","SA_c_minimal_vs_optimal_overall",  #Incremental cost - Overall (PT + TB)
                            "SA_cPT_baseline_vs_6h_overall","SA_cPT_baseline_vs_minimal_overall","SA_cPT_baseline_vs_optimal_overall","SA_cPT_6h_vs_minimal_overall","SA_cPT_6h_vs_optimal_overall","SA_cPT_minimal_vs_optimal_overall",  #Incremental cost - Overall (PT)
                            "SA_cTB_baseline_vs_6h_overall","SA_cTB_baseline_vs_minimal_overall","SA_cTB_baseline_vs_optimal_overall","SA_cTB_6h_vs_minimal_overall","SA_cTB_6h_vs_optimal_overall","SA_cTB_minimal_vs_optimal_overall",  #Incremental cost - Overall (TB)
                            "SA_c_baseline_vs_6h_HIV_neg","SA_c_baseline_vs_minimal_HIV_neg","SA_c_baseline_vs_optimal_HIV_neg","SA_c_6h_vs_minimal_HIV_neg","SA_c_6h_vs_optimal_HIV_neg","SA_c_minimal_vs_optimal_HIV_neg",  #Incremental cost - HIV negative (PT + TB)
                            "SA_cPT_baseline_vs_6h_HIV_neg","SA_cPT_baseline_vs_minimal_HIV_neg","SA_cPT_baseline_vs_optimal_HIV_neg","SA_cPT_6h_vs_minimal_HIV_neg","SA_cPT_6h_vs_optimal_HIV_neg","SA_cPT_minimal_vs_optimal_HIV_neg", #Incremental cost - HIV negative (PT)
                            "SA_cTB_baseline_vs_6h_HIV_neg","SA_cTB_baseline_vs_minimal_HIV_neg","SA_cTB_baseline_vs_optimal_HIV_neg","SA_cTB_6h_vs_minimal_HIV_neg","SA_cTB_6h_vs_optimal_HIV_neg","SA_cTB_minimal_vs_optimal_HIV_neg",  #Incremental cost - HIV negative (TB)
                            "SA_c_baseline_vs_6h_PLHIV","SA_c_baseline_vs_minimal_PLHIV","SA_c_baseline_vs_optimal_PLHIV","SA_c_6h_vs_minimal_PLHIV","SA_c_6h_vs_optimal_PLHIV","SA_c_minimal_vs_optimal_PLHIV",  #Incremental cost - PLHIV (PT+TB)
                            "SA_cPT_baseline_vs_6h_PLHIV","SA_cPT_baseline_vs_minimal_PLHIV","SA_cPT_baseline_vs_optimal_PLHIV","SA_cPT_6h_vs_minimal_PLHIV","SA_cPT_6h_vs_optimal_PLHIV","SA_cPT_minimal_vs_optimal_PLHIV",  #Incremental cost - PLHIV (PT)
                            "SA_cTB_baseline_vs_6h_PLHIV","SA_cTB_baseline_vs_minimal_PLHIV","SA_cTB_baseline_vs_optimal_PLHIV","SA_cTB_6h_vs_minimal_PLHIV","SA_cTB_6h_vs_optimal_PLHIV","SA_cTB_minimal_vs_optimal_PLHIV"   #Incremental cost - PLHIV (TB)
  )
  
    #####################################################################################################################################
  #####################################################################################################################################
  #colnames(SA_incr_cost) <- c("Runs","All_TBPT_baseline vs 6H","All_TBPT_baseline vs minimal","All_TBPT_baseline vs optimal","All_TBPT_6H vs minimal", "All_TBPT_6H vs optimal", "All_TBPT_minimal vs optimal",
  #                                    "All_PT_baseline vs 6H","All_PT_baseline vs minimal","All_PT_baseline vs optimal","All_PT_6H vs minimal", "All_PT_6H vs optimal", "All_PT_minimal vs optimal",
  #                                   "All_TB_baseline vs 6H","All_TB_baseline vs minimal","All_TB_baseline vs optimal","All_TB_6H vs minimal", "All_TB_6H vs optimal", "All_TB_minimal vs optimal",
  #                                    "HHC_TBPT_baseline vs 6H","HHC_TBPT_baseline vs minimal","HHC_TBPT_baseline vs optimal","HHC_TBPT_6H vs minimal", "HHC_TBPT_6H vs optimal", "HHC_TBPT_minimal vs optimal",
  #                                   "HHC_PT_baseline vs 6H","HHC_PT_baseline vs minimal","HHC_PT_baseline vs optimal","HHC_PT_6H vs minimal", "HHC_PT_6H vs optimal", "HHC_PT_minimal vs optimal",
  #                                   "HHC_TB_baseline vs 6H","HHC_TB_baseline vs minimal","HHC_TB_baseline vs optimal","HHC_TB_6H vs minimal", "HHC_TB_6H vs optimal", "HHC_TB_minimal vs optimal",
  #                                   "HIV_TBPT_baseline vs 6H","HIV_TBPT_baseline vs minimal","HIV_TBPT_baseline vs optimal","HIV_TBPT_6H vs minimal", "HIV_TBPT_6H vs optimal", "HIV_TBPT_minimal vs optimal",
  #                                   "HIV_PT_baseline vs 6H","HIV_PT_baseline vs minimal","HIV_PT_baseline vs optimal","HIV_PT_6H vs minimal", "HIV_PT_6H vs optimal", "HIV_PT_minimal vs optimal",
  #                                   "HIV_TB_baseline vs 6H","HIV_TB_baseline vs minimal","HIV_TB_baseline vs optimal","HIV_TB_6H vs minimal", "HIV_TB_6H vs optimal", "HIV_TB_minimal vs optimal")
  
  
  SA_incr_cost_graph<-melt(SA_incr_cost, id.vars =c("Runs"), variable.name = "cost_pop")
  SA_incr_cost_graph$cum_cost<-ave(SA_incr_cost_graph$value, SA_incr_cost_graph$cost_pop, FUN=cumsum)
  SA_incr_cost_graph$comparison = ifelse(grepl("baseline_vs_6h", SA_incr_cost_graph$cost_pop), "1a. 6H vs baseline",
                                  ifelse(grepl("baseline_vs_minimal", SA_incr_cost_graph$cost_pop),"1b. Minimal vs baseline",
                                  ifelse(grepl("baseline_vs_optimal", SA_incr_cost_graph$cost_pop),"1c. Optimal vs baseline",
                                  ifelse(grepl("6h_vs_minimal", SA_incr_cost_graph$cost_pop), "2a. Minimal vs 6H",
                                  ifelse(grepl("6h_vs_optimal", SA_incr_cost_graph$cost_pop),"2b. Optimal vs 6H","3. Optimal vs minimal")))))
  SA_incr_cost_graph$outcome = ifelse(grepl("cPT", SA_incr_cost_graph$cost_pop), "1.PT",
                                         ifelse(grepl("cTB", SA_incr_cost_graph$cost_pop),"2.TB","3.PT & TB"))
  SA_incr_cost_graph$population = ifelse(grepl("overall", SA_incr_cost_graph$cost_pop), "All treatment candidates",
                                                  ifelse(grepl("PLHIV", SA_incr_cost_graph$cost_pop),"PLHIV","HHC (HIV negatives)"))
  SA_incr_cost_graph$cost_pop = chartr("."," ",SA_incr_cost_graph$cost_pop)
  
  #####################################################################################################################
  
  SA_incr_cost<-aggregate(x=SA_incr_cost_graph$value, by=list(Runs=SA_incr_cost_graph$Runs, comparison=SA_incr_cost_graph$comparison,outcome=SA_incr_cost_graph$outcome,population=SA_incr_cost_graph$population), FUN=sum)
  #Incremental cost graph
  #Incremental cost graph
  SA_incr_cost_limited<-SA_incr_cost %>%
    filter(comparison !="1c. Optimal vs baseline", comparison!="2b. Optimal vs 6H")
  
  
  SA_incr_cost_graph <- ggplot(data=SA_incr_cost_limited, aes(comparison,x,color=comparison)) + 
    geom_boxplot(notch = TRUE) +
    scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
    facet_wrap(~population + outcome, dir = "h", scales = "free") +
    labs(x = "population group", y = expression(paste("Incremental cost (US$ Billions)")),colour = "",subtitle="",caption="")+
    guides(fill = "none")+
    stat_boxplot(geom = "errorbar", width = 0.5) +  
    geom_boxplot()+
    scale_color_brewer(palette = "Set1")+
    theme_classic()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  ggsave("5. SA_incr_cost_graph.jpeg", 
         width = 25, height = 16, units = "cm")
  
  
  #Incremental cost table with ranges  
  SA_incr_cost_table<-SA_incr_cost %>% 
    group_by(population,outcome, comparison) %>% 
    summarise_at(vars(x),
                 list(median=median,
                      mean=mean,
                      quantile_lower = ~quantile(., probs = 0.025),
                      quantile_upper = ~quantile(., probs = 0.975)))
  write.csv(SA_incr_cost_table, file = "5. South African incremental cost output tables.csv")
  
  
  
  
  #incremental graph option 2
  SA_incr_cost_table$fill <- ifelse(SA_incr_cost_table$median > 0, "#0408e0", "#c43b00")
  
  SA_incremental_cost_plot2 <- ggplot(SA_incr_cost_table, aes(x = comparison, y = median, fill = fill)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label=paste("$",round(median/1000000,0),"M")), position=position_dodge(width=0.9), vjust=-0.25, size=3, fontface="bold")+
    facet_wrap(~population + outcome, scales="fixed") +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
    scale_fill_discrete(name="Incremental costs",
                        breaks=c("#0408e0", "#c43b00"),
                        labels=c("Additional cost","Savings"))+
    #scale_color_hue(labels = c("Savings","Additional cost"))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none")+
    xlab("Comparison") +
    ylab("Incremental cost (in USD)") +
    labs(title="",
         subtitle="",
         caption="")+
    
    
    ggtitle("Incremental cost of TPT strategies for South Africa (2020-2035)")
  ggsave("5. SA_incremental cost2.png", 
         width = 30, height = 20, units = "cm")
  
  
  SA_incremental_cost_plot2
  
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

  

  
  
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  #                                     Incremental effectiveness                                                     #
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
 
  
  #####################################################################################################################
  #incremental effectiveness - all treatment candidates
  #####################################################################################################################
  
  #Incremental effectiveness - Overall (PT)
  SA_PT_baseline_vs_6h_overall <- data.frame(Runs=SA_baseline$Run,SA_epi_cumulative_PT$Expanded.6H-SA_epi_cumulative_PT$Baseline)
  SA_PT_baseline_vs_minimal_overall <- data.frame(SA_epi_cumulative_PT$Minimal.regimen-SA_epi_cumulative_PT$Baseline)
  SA_PT_baseline_vs_optimal_overall <- data.frame(SA_epi_cumulative_PT$Optimal.regimen-SA_epi_cumulative_PT$Baseline)
  SA_PT_6h_vs_minimal_overall <- data.frame(SA_epi_cumulative_PT$Minimal.regimen-SA_epi_cumulative_PT$Expanded.6H)
  SA_PT_6h_vs_optimal_overall <- data.frame(SA_epi_cumulative_PT$Optimal.regimen-SA_epi_cumulative_PT$Expanded.6H)
  SA_PT_minimal_vs_optimal_overall <- data.frame(SA_epi_cumulative_PT$Optimal.regimen-SA_epi_cumulative_PT$Minimal.regimen)
  #####################################################################################################################################

  #Incremental effectiveness - Overall (DS-TB)
  SA_DSTB_baseline_vs_6h_overall <- data.frame(SA_epi_cumulative_DSTB$Expanded.6H-SA_epi_cumulative_DSTB$Baseline)
  SA_DSTB_baseline_vs_minimal_overall <- data.frame(SA_epi_cumulative_DSTB$Minimal.regimen-SA_epi_cumulative_DSTB$Baseline)
  SA_DSTB_baseline_vs_optimal_overall <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen-SA_epi_cumulative_DSTB$Baseline)
  SA_DSTB_6h_vs_minimal_overall <- data.frame(SA_epi_cumulative_DSTB$Minimal.regimen-SA_epi_cumulative_DSTB$Expanded.6H)
  SA_DSTB_6h_vs_optimal_overall <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen-SA_epi_cumulative_DSTB$Expanded.6H)
  SA_DSTB_minimal_vs_optimal_overall <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen-SA_epi_cumulative_DSTB$Minimal.regimen)
  
  #####################################################################################################################################
  #Incremental effectiveness - Overall (RR-TB)
  SA_DRTB_baseline_vs_6h_overall <- data.frame(SA_epi_cumulative_DRTB$Expanded.6H-SA_epi_cumulative_DRTB$Baseline)
  SA_DRTB_baseline_vs_minimal_overall <- data.frame(SA_epi_cumulative_DRTB$Minimal.regimen-SA_epi_cumulative_DRTB$Baseline)
  SA_DRTB_baseline_vs_optimal_overall <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen-SA_epi_cumulative_DRTB$Baseline)
  SA_DRTB_6h_vs_minimal_overall <- data.frame(SA_epi_cumulative_DRTB$Minimal.regimen-SA_epi_cumulative_DRTB$Expanded.6H)
  SA_DRTB_6h_vs_optimal_overall <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen-SA_epi_cumulative_DRTB$Expanded.6H)
  SA_DRTB_minimal_vs_optimal_overall <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen-SA_epi_cumulative_DRTB$Minimal.regimen)
  
  #####################################################################################################################################
  #Incremental effectiveness - Overall (TB cases (DS-TB & RR-TB))
  SA_TBall_baseline_vs_6h_overall <- data.frame(SA_epi_cumulative_TBall$Expanded.6H-SA_epi_cumulative_TBall$Baseline)
  SA_TBall_baseline_vs_minimal_overall <- data.frame(SA_epi_cumulative_TBall$Minimal.regimen-SA_epi_cumulative_TBall$Baseline)
  SA_TBall_baseline_vs_optimal_overall <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen-SA_epi_cumulative_TBall$Baseline)
  SA_TBall_6h_vs_minimal_overall <- data.frame(SA_epi_cumulative_TBall$Minimal.regimen-SA_epi_cumulative_TBall$Expanded.6H)
  SA_TBall_6h_vs_optimal_overall <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen-SA_epi_cumulative_TBall$Expanded.6H)
  SA_TBall_minimal_vs_optimal_overall <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen-SA_epi_cumulative_TBall$Minimal.regimen)
  
  #####################################################################################################################################
  #Incremental effectiveness - Overall (TB deaths)
  SA_TBdeaths_baseline_vs_6h_overall <- data.frame(SA_epi_cumulative_TBdeaths$Expanded.6H-SA_epi_cumulative_TBdeaths$Baseline)
  SA_TBdeaths_baseline_vs_minimal_overall <- data.frame(SA_epi_cumulative_TBdeaths$Minimal.regimen-SA_epi_cumulative_TBdeaths$Baseline)
  SA_TBdeaths_baseline_vs_optimal_overall <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen-SA_epi_cumulative_TBdeaths$Baseline)
  SA_TBdeaths_6h_vs_minimal_overall <- data.frame(SA_epi_cumulative_TBdeaths$Minimal.regimen-SA_epi_cumulative_TBdeaths$Expanded.6H)
  SA_TBdeaths_6h_vs_optimal_overall <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen-SA_epi_cumulative_TBdeaths$Expanded.6H)
  SA_TBdeaths_minimal_vs_optimal_overall <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen-SA_epi_cumulative_TBdeaths$Minimal.regimen)
  
  #####################################################################################################################################
  #Incremental effectiveness - Overall (DALYs)
  SA_DALYs_baseline_vs_6h_overall <- data.frame(SA_epi_cumulative_DALYs$Expanded.6H-SA_epi_cumulative_DALYs$Baseline)
  SA_DALYs_baseline_vs_minimal_overall <- data.frame(SA_epi_cumulative_DALYs$Minimal.regimen-SA_epi_cumulative_DALYs$Baseline)
  SA_DALYs_baseline_vs_optimal_overall <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen-SA_epi_cumulative_DALYs$Baseline)
  SA_DALYs_6h_vs_minimal_overall <- data.frame(SA_epi_cumulative_DALYs$Minimal.regimen-SA_epi_cumulative_DALYs$Expanded.6H)
  SA_DALYs_6h_vs_optimal_overall <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen-SA_epi_cumulative_DALYs$Expanded.6H)
  SA_DALYs_minimal_vs_optimal_overall <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen-SA_epi_cumulative_DALYs$Minimal.regimen)
  

  #####################################################################################################################
  #incremental effectiveness - Household contacts (HIV-negative)
  #####################################################################################################################
  
  #Incremental effectiveness - HIV negative (PT)
  SA_PT_baseline_vs_6h_HIV_neg <- data.frame(SA_epi_cumulative_PT$Expanded.6H_neg-SA_epi_cumulative_PT$Baseline_HIV_neg)
  SA_PT_baseline_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_PT$Minimal.regimen_HIV_neg-SA_epi_cumulative_PT$Baseline_HIV_neg)
  SA_PT_baseline_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_PT$Optimal.regimen_HIV_neg-SA_epi_cumulative_PT$Baseline_HIV_neg)
  SA_PT_6h_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_PT$Minimal.regimen_HIV_neg-SA_epi_cumulative_PT$Expanded.6H_neg)
  SA_PT_6h_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_PT$Optimal.regimen_HIV_neg-SA_epi_cumulative_PT$Expanded.6H_neg)
  SA_PT_minimal_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_PT$Optimal.regimen_HIV_neg-SA_epi_cumulative_PT$Minimal.regimen_HIV_neg)
  #####################################################################################################################################
  
  #Incremental effectiveness - HIV negative (DS-TB)
  SA_DSTB_baseline_vs_6h_HIV_neg <- data.frame(SA_epi_cumulative_DSTB$Expanded.6H_neg-SA_epi_cumulative_DSTB$Baseline_HIV_neg)
  SA_DSTB_baseline_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_DSTB$Minimal.regimen_HIV_neg-SA_epi_cumulative_DSTB$Baseline_HIV_neg)
  SA_DSTB_baseline_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen_HIV_neg-SA_epi_cumulative_DSTB$Baseline_HIV_neg)
  SA_DSTB_6h_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_DSTB$Minimal.regimen_HIV_neg-SA_epi_cumulative_DSTB$Expanded.6H_neg)
  SA_DSTB_6h_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen_HIV_neg-SA_epi_cumulative_DSTB$Expanded.6H_neg)
  SA_DSTB_minimal_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen_HIV_neg-SA_epi_cumulative_DSTB$Minimal.regimen_HIV_neg)
  
  #####################################################################################################################################
  #Incremental effectiveness - HIV negative (RR-TB)
  SA_DRTB_baseline_vs_6h_HIV_neg <- data.frame(SA_epi_cumulative_DRTB$Expanded.6H_neg-SA_epi_cumulative_DRTB$Baseline_HIV_neg)
  SA_DRTB_baseline_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_DRTB$Minimal.regimen_HIV_neg-SA_epi_cumulative_DRTB$Baseline_HIV_neg)
  SA_DRTB_baseline_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen_HIV_neg-SA_epi_cumulative_DRTB$Baseline_HIV_neg)
  SA_DRTB_6h_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_DRTB$Minimal.regimen_HIV_neg-SA_epi_cumulative_DRTB$Expanded.6H_neg)
  SA_DRTB_6h_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen_HIV_neg-SA_epi_cumulative_DRTB$Expanded.6H_neg)
  SA_DRTB_minimal_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen_HIV_neg-SA_epi_cumulative_DRTB$Minimal.regimen_HIV_neg)
  
  #####################################################################################################################################
  #Incremental effectiveness - HIV negative (TB cases (DS-TB & RR-TB))
  SA_TBall_baseline_vs_6h_HIV_neg <- data.frame(SA_epi_cumulative_TBall$Expanded.6H_neg-SA_epi_cumulative_TBall$Baseline_HIV_neg)
  SA_TBall_baseline_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_TBall$Minimal.regimen_HIV_neg-SA_epi_cumulative_TBall$Baseline_HIV_neg)
  SA_TBall_baseline_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen_HIV_neg-SA_epi_cumulative_TBall$Baseline_HIV_neg)
  SA_TBall_6h_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_TBall$Minimal.regimen_HIV_neg-SA_epi_cumulative_TBall$Expanded.6H_neg)
  SA_TBall_6h_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen_HIV_neg-SA_epi_cumulative_TBall$Expanded.6H_neg)
  SA_TBall_minimal_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen_HIV_neg-SA_epi_cumulative_TBall$Minimal.regimen_HIV_neg)
  
  #####################################################################################################################################
  #Incremental effectiveness - HIV negative (TB deaths)
  SA_TBdeaths_baseline_vs_6h_HIV_neg <- data.frame(SA_epi_cumulative_TBdeaths$Expanded.6H_neg-SA_epi_cumulative_TBdeaths$Baseline_HIV_neg)
  SA_TBdeaths_baseline_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_TBdeaths$Minimal.regimen_HIV_neg-SA_epi_cumulative_TBdeaths$Baseline_HIV_neg)
  SA_TBdeaths_baseline_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen_HIV_neg-SA_epi_cumulative_TBdeaths$Baseline_HIV_neg)
  SA_TBdeaths_6h_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_TBdeaths$Minimal.regimen_HIV_neg-SA_epi_cumulative_TBdeaths$Expanded.6H_neg)
  SA_TBdeaths_6h_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen_HIV_neg-SA_epi_cumulative_TBdeaths$Expanded.6H_neg)
  SA_TBdeaths_minimal_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen_HIV_neg-SA_epi_cumulative_TBdeaths$Minimal.regimen_HIV_neg)
  
  #####################################################################################################################################
  #Incremental effectiveness - HIV negative (DALYs)
  SA_DALYs_baseline_vs_6h_HIV_neg <- data.frame(SA_epi_cumulative_DALYs$Expanded.6H_neg-SA_epi_cumulative_DALYs$Baseline_HIV_neg)
  SA_DALYs_baseline_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_DALYs$Minimal.regimen_HIV_neg-SA_epi_cumulative_DALYs$Baseline_HIV_neg)
  SA_DALYs_baseline_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen_HIV_neg-SA_epi_cumulative_DALYs$Baseline_HIV_neg)
  SA_DALYs_6h_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_DALYs$Minimal.regimen_HIV_neg-SA_epi_cumulative_DALYs$Expanded.6H_neg)
  SA_DALYs_6h_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen_HIV_neg-SA_epi_cumulative_DALYs$Expanded.6H_neg)
  SA_DALYs_minimal_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen_HIV_neg-SA_epi_cumulative_DALYs$Minimal.regimen_HIV_neg)
  

  
  #####################################################################################################################
  #incremental effectiveness - PLHIV
  #####################################################################################################################
  
  #Incremental effectiveness - HIV negative (PT)
  SA_PT_baseline_vs_6h_PLHIV <- data.frame(SA_epi_cumulative_PT$Expanded.6H_PLHIV-SA_epi_cumulative_PT$Baseline_PLHIV)
  SA_PT_baseline_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_PT$Minimal.regimen_PLHIV-SA_epi_cumulative_PT$Baseline_PLHIV)
  SA_PT_baseline_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_PT$Optimal.regimen_PLHIV-SA_epi_cumulative_PT$Baseline_PLHIV)
  SA_PT_6h_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_PT$Minimal.regimen_PLHIV-SA_epi_cumulative_PT$Expanded.6H_PLHIV)
  SA_PT_6h_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_PT$Optimal.regimen_PLHIV-SA_epi_cumulative_PT$Expanded.6H_PLHIV)
  SA_PT_minimal_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_PT$Optimal.regimen_PLHIV-SA_epi_cumulative_PT$Minimal.regimen_PLHIV)
  #####################################################################################################################################
  
  #Incremental effectiveness - HIV negative (DS-TB)
  SA_DSTB_baseline_vs_6h_PLHIV <- data.frame(SA_epi_cumulative_DSTB$Expanded.6H_PLHIV-SA_epi_cumulative_DSTB$Baseline_PLHIV)
  SA_DSTB_baseline_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_DSTB$Minimal.regimen_PLHIV-SA_epi_cumulative_DSTB$Baseline_PLHIV)
  SA_DSTB_baseline_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen_PLHIV-SA_epi_cumulative_DSTB$Baseline_PLHIV)
  SA_DSTB_6h_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_DSTB$Minimal.regimen_PLHIV-SA_epi_cumulative_DSTB$Expanded.6H_PLHIV)
  SA_DSTB_6h_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen_PLHIV-SA_epi_cumulative_DSTB$Expanded.6H_PLHIV)
  SA_DSTB_minimal_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen_PLHIV-SA_epi_cumulative_DSTB$Minimal.regimen_PLHIV)
  
  #####################################################################################################################################
  #Incremental effectiveness - HIV negative (RR-TB)
  SA_DRTB_baseline_vs_6h_PLHIV <- data.frame(SA_epi_cumulative_DRTB$Expanded.6H_PLHIV-SA_epi_cumulative_DRTB$Baseline_PLHIV)
  SA_DRTB_baseline_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_DRTB$Minimal.regimen_PLHIV-SA_epi_cumulative_DRTB$Baseline_PLHIV)
  SA_DRTB_baseline_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen_PLHIV-SA_epi_cumulative_DRTB$Baseline_PLHIV)
  SA_DRTB_6h_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_DRTB$Minimal.regimen_PLHIV-SA_epi_cumulative_DRTB$Expanded.6H_PLHIV)
  SA_DRTB_6h_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen_PLHIV-SA_epi_cumulative_DRTB$Expanded.6H_PLHIV)
  SA_DRTB_minimal_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen_PLHIV-SA_epi_cumulative_DRTB$Minimal.regimen_PLHIV)
  
  #####################################################################################################################################
  #Incremental effectiveness - HIV negative (TB cases (DS-TB & RR-TB))
  SA_TBall_baseline_vs_6h_PLHIV <- data.frame(SA_epi_cumulative_TBall$Expanded.6H_PLHIV-SA_epi_cumulative_TBall$Baseline_PLHIV)
  SA_TBall_baseline_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_TBall$Minimal.regimen_PLHIV-SA_epi_cumulative_TBall$Baseline_PLHIV)
  SA_TBall_baseline_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen_PLHIV-SA_epi_cumulative_TBall$Baseline_PLHIV)
  SA_TBall_6h_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_TBall$Minimal.regimen_PLHIV-SA_epi_cumulative_TBall$Expanded.6H_PLHIV)
  SA_TBall_6h_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen_PLHIV-SA_epi_cumulative_TBall$Expanded.6H_PLHIV)
  SA_TBall_minimal_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen_PLHIV-SA_epi_cumulative_TBall$Minimal.regimen_PLHIV)
  
  #####################################################################################################################################
  #Incremental effectiveness - HIV negative (TB deaths)
  SA_TBdeaths_baseline_vs_6h_PLHIV <- data.frame(SA_epi_cumulative_TBdeaths$Expanded.6H_PLHIV-SA_epi_cumulative_TBdeaths$Baseline_PLHIV)
  SA_TBdeaths_baseline_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_TBdeaths$Minimal.regimen_PLHIV-SA_epi_cumulative_TBdeaths$Baseline_PLHIV)
  SA_TBdeaths_baseline_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen_PLHIV-SA_epi_cumulative_TBdeaths$Baseline_PLHIV)
  SA_TBdeaths_6h_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_TBdeaths$Minimal.regimen_PLHIV-SA_epi_cumulative_TBdeaths$Expanded.6H_PLHIV)
  SA_TBdeaths_6h_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen_PLHIV-SA_epi_cumulative_TBdeaths$Expanded.6H_PLHIV)
  SA_TBdeaths_minimal_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen_PLHIV-SA_epi_cumulative_TBdeaths$Minimal.regimen_PLHIV)
  
  #####################################################################################################################################
  #Incremental effectiveness - HIV negative (DALYs)
  SA_DALYs_baseline_vs_6h_PLHIV <- data.frame(SA_epi_cumulative_DALYs$Expanded.6H_PLHIV-SA_epi_cumulative_DALYs$Baseline_PLHIV)
  SA_DALYs_baseline_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_DALYs$Minimal.regimen_PLHIV-SA_epi_cumulative_DALYs$Baseline_PLHIV)
  SA_DALYs_baseline_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen_PLHIV-SA_epi_cumulative_DALYs$Baseline_PLHIV)
  SA_DALYs_6h_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_DALYs$Minimal.regimen_PLHIV-SA_epi_cumulative_DALYs$Expanded.6H_PLHIV)
  SA_DALYs_6h_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen_PLHIV-SA_epi_cumulative_DALYs$Expanded.6H_PLHIV)
  SA_DALYs_minimal_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen_PLHIV-SA_epi_cumulative_DALYs$Minimal.regimen_PLHIV)
  
  
  
  #####################################################################################################################################
  
  #####################################################################################################################################
  #####################################################################################################################################
  
  #combine columns
SA_incr_effectiveness<-data.frame(SA_PT_baseline_vs_6h_overall,SA_PT_baseline_vs_minimal_overall,SA_PT_baseline_vs_optimal_overall,SA_PT_6h_vs_minimal_overall,SA_PT_6h_vs_optimal_overall,SA_PT_minimal_vs_optimal_overall,
                         SA_PT_baseline_vs_6h_HIV_neg,SA_PT_baseline_vs_minimal_HIV_neg,SA_PT_baseline_vs_optimal_HIV_neg,SA_PT_6h_vs_minimal_HIV_neg,SA_PT_6h_vs_optimal_HIV_neg,SA_PT_minimal_vs_optimal_HIV_neg,
                         SA_PT_baseline_vs_6h_PLHIV,
                         SA_PT_baseline_vs_minimal_PLHIV,
                         SA_PT_baseline_vs_optimal_PLHIV,
                         SA_PT_6h_vs_minimal_PLHIV,
                         SA_PT_6h_vs_optimal_PLHIV,
                         SA_PT_minimal_vs_optimal_PLHIV,
                         SA_DSTB_baseline_vs_6h_overall,
                         SA_DSTB_baseline_vs_minimal_overall,
                         SA_DSTB_baseline_vs_optimal_overall,
                         SA_DSTB_6h_vs_minimal_overall,
                         SA_DSTB_6h_vs_optimal_overall,
                         SA_DSTB_minimal_vs_optimal_overall,
                         SA_DSTB_baseline_vs_6h_HIV_neg,
                         SA_DSTB_baseline_vs_minimal_HIV_neg,
                         SA_DSTB_baseline_vs_optimal_HIV_neg,
                         SA_DSTB_6h_vs_minimal_HIV_neg,
                         SA_DSTB_6h_vs_optimal_HIV_neg,
                         SA_DSTB_minimal_vs_optimal_HIV_neg,
                         SA_DSTB_baseline_vs_6h_PLHIV,
                         SA_DSTB_baseline_vs_minimal_PLHIV,
                         SA_DSTB_baseline_vs_optimal_PLHIV,
                         SA_DSTB_6h_vs_minimal_PLHIV,
                         SA_DSTB_6h_vs_optimal_PLHIV,
                         SA_DSTB_minimal_vs_optimal_PLHIV,
                         SA_DRTB_baseline_vs_6h_overall,
                         SA_DRTB_baseline_vs_minimal_overall,
                         SA_DRTB_baseline_vs_optimal_overall,
                         SA_DRTB_6h_vs_minimal_overall,
                         SA_DRTB_6h_vs_optimal_overall,
                         SA_DRTB_minimal_vs_optimal_overall,
                         SA_DRTB_baseline_vs_6h_HIV_neg,
                         SA_DRTB_baseline_vs_minimal_HIV_neg,
                         SA_DRTB_baseline_vs_optimal_HIV_neg,
                         SA_DRTB_6h_vs_minimal_HIV_neg,
                         SA_DRTB_6h_vs_optimal_HIV_neg,
                         SA_DRTB_minimal_vs_optimal_HIV_neg,
                         SA_DRTB_baseline_vs_6h_PLHIV,
                         SA_DRTB_baseline_vs_minimal_PLHIV,
                         SA_DRTB_baseline_vs_optimal_PLHIV,
                         SA_DRTB_6h_vs_minimal_PLHIV,
                         SA_DRTB_6h_vs_optimal_PLHIV,
                         SA_DRTB_minimal_vs_optimal_PLHIV,
                         SA_TBall_baseline_vs_6h_overall,
                         SA_TBall_baseline_vs_minimal_overall,
                         SA_TBall_baseline_vs_optimal_overall,
                         SA_TBall_6h_vs_minimal_overall,
                         SA_TBall_6h_vs_optimal_overall,
                         SA_TBall_minimal_vs_optimal_overall,
                         SA_TBall_baseline_vs_6h_HIV_neg,
                         SA_TBall_baseline_vs_minimal_HIV_neg,
                         SA_TBall_baseline_vs_optimal_HIV_neg,
                         SA_TBall_6h_vs_minimal_HIV_neg,
                         SA_TBall_6h_vs_optimal_HIV_neg,
                         SA_TBall_minimal_vs_optimal_HIV_neg,
                         SA_TBall_baseline_vs_6h_PLHIV,
                         SA_TBall_baseline_vs_minimal_PLHIV,
                         SA_TBall_baseline_vs_optimal_PLHIV,
                         SA_TBall_6h_vs_minimal_PLHIV,
                         SA_TBall_6h_vs_optimal_PLHIV,
                         SA_TBall_minimal_vs_optimal_PLHIV,
                         SA_TBdeaths_baseline_vs_6h_overall,
                         SA_TBdeaths_baseline_vs_minimal_overall,
                         SA_TBdeaths_baseline_vs_optimal_overall,
                         SA_TBdeaths_6h_vs_minimal_overall,
                         SA_TBdeaths_6h_vs_optimal_overall,
                         SA_TBdeaths_minimal_vs_optimal_overall,
                         SA_TBdeaths_baseline_vs_6h_HIV_neg,
                         SA_TBdeaths_baseline_vs_minimal_HIV_neg,
                         SA_TBdeaths_baseline_vs_optimal_HIV_neg,
                         SA_TBdeaths_6h_vs_minimal_HIV_neg,
                         SA_TBdeaths_6h_vs_optimal_HIV_neg,
                         SA_TBdeaths_minimal_vs_optimal_HIV_neg,
                         SA_TBdeaths_baseline_vs_6h_PLHIV,
                         SA_TBdeaths_baseline_vs_minimal_PLHIV,
                         SA_TBdeaths_baseline_vs_optimal_PLHIV,
                         SA_TBdeaths_6h_vs_minimal_PLHIV,
                         SA_TBdeaths_6h_vs_optimal_PLHIV,
                         SA_TBdeaths_minimal_vs_optimal_PLHIV,
                         SA_DALYs_baseline_vs_6h_overall,
                         SA_DALYs_baseline_vs_minimal_overall,
                         SA_DALYs_baseline_vs_optimal_overall,
                         SA_DALYs_6h_vs_minimal_overall,
                         SA_DALYs_6h_vs_optimal_overall,
                         SA_DALYs_minimal_vs_optimal_overall,
                         SA_DALYs_baseline_vs_6h_HIV_neg,
                         SA_DALYs_baseline_vs_minimal_HIV_neg,
                         SA_DALYs_baseline_vs_optimal_HIV_neg,
                         SA_DALYs_6h_vs_minimal_HIV_neg,
                         SA_DALYs_6h_vs_optimal_HIV_neg,
                         SA_DALYs_minimal_vs_optimal_HIV_neg,
                         SA_DALYs_baseline_vs_6h_PLHIV,
                         SA_DALYs_baseline_vs_minimal_PLHIV,
                         SA_DALYs_baseline_vs_optimal_PLHIV,
                         SA_DALYs_6h_vs_minimal_PLHIV,
                         SA_DALYs_6h_vs_optimal_PLHIV,
                         SA_DALYs_minimal_vs_optimal_PLHIV
                         )
  colnames(SA_incr_effectiveness) <- c("Runs","SA_PT_baseline_vs_6h_overall",
                                       "SA_PT_baseline_vs_minimal_overall",
                                       "SA_PT_baseline_vs_optimal_overall",
                                       "SA_PT_6h_vs_minimal_overall",
                                       "SA_PT_6h_vs_optimal_overall",
                                       "SA_PT_minimal_vs_optimal_overall",
                                       "SA_PT_baseline_vs_6h_HIV_neg",
                                       "SA_PT_baseline_vs_minimal_HIV_neg",
                                       "SA_PT_baseline_vs_optimal_HIV_neg",
                                       "SA_PT_6h_vs_minimal_HIV_neg",
                                       "SA_PT_6h_vs_optimal_HIV_neg",
                                       "SA_PT_minimal_vs_optimal_HIV_neg",
                                       "SA_PT_baseline_vs_6h_PLHIV",
                                       "SA_PT_baseline_vs_minimal_PLHIV",
                                       "SA_PT_baseline_vs_optimal_PLHIV",
                                       "SA_PT_6h_vs_minimal_PLHIV",
                                       "SA_PT_6h_vs_optimal_PLHIV",
                                       "SA_PT_minimal_vs_optimal_PLHIV",
                                       "SA_DSTB_baseline_vs_6h_overall",
                                       "SA_DSTB_baseline_vs_minimal_overall",
                                       "SA_DSTB_baseline_vs_optimal_overall",
                                       "SA_DSTB_6h_vs_minimal_overall",
                                       "SA_DSTB_6h_vs_optimal_overall",
                                       "SA_DSTB_minimal_vs_optimal_overall",
                                       "SA_DSTB_baseline_vs_6h_HIV_neg",
                                       "SA_DSTB_baseline_vs_minimal_HIV_neg",
                                       "SA_DSTB_baseline_vs_optimal_HIV_neg",
                                       "SA_DSTB_6h_vs_minimal_HIV_neg",
                                       "SA_DSTB_6h_vs_optimal_HIV_neg",
                                       "SA_DSTB_minimal_vs_optimal_HIV_neg",
                                       "SA_DSTB_baseline_vs_6h_PLHIV",
                                       "SA_DSTB_baseline_vs_minimal_PLHIV",
                                       "SA_DSTB_baseline_vs_optimal_PLHIV",
                                       "SA_DSTB_6h_vs_minimal_PLHIV",
                                       "SA_DSTB_6h_vs_optimal_PLHIV",
                                       "SA_DSTB_minimal_vs_optimal_PLHIV",
                                       "SA_DRTB_baseline_vs_6h_overall",
                                       "SA_DRTB_baseline_vs_minimal_overall",
                                       "SA_DRTB_baseline_vs_optimal_overall",
                                       "SA_DRTB_6h_vs_minimal_overall",
                                       "SA_DRTB_6h_vs_optimal_overall",
                                       "SA_DRTB_minimal_vs_optimal_overall",
                                       "SA_DRTB_baseline_vs_6h_HIV_neg",
                                       "SA_DRTB_baseline_vs_minimal_HIV_neg",
                                       "SA_DRTB_baseline_vs_optimal_HIV_neg",
                                       "SA_DRTB_6h_vs_minimal_HIV_neg",
                                       "SA_DRTB_6h_vs_optimal_HIV_neg",
                                       "SA_DRTB_minimal_vs_optimal_HIV_neg",
                                       "SA_DRTB_baseline_vs_6h_PLHIV",
                                       "SA_DRTB_baseline_vs_minimal_PLHIV",
                                       "SA_DRTB_baseline_vs_optimal_PLHIV",
                                       "SA_DRTB_6h_vs_minimal_PLHIV",
                                       "SA_DRTB_6h_vs_optimal_PLHIV",
                                       "SA_DRTB_minimal_vs_optimal_PLHIV",
                                       "SA_TBall_baseline_vs_6h_overall",
                                       "SA_TBall_baseline_vs_minimal_overall",
                                       "SA_TBall_baseline_vs_optimal_overall",
                                       "SA_TBall_6h_vs_minimal_overall",
                                       "SA_TBall_6h_vs_optimal_overall",
                                       "SA_TBall_minimal_vs_optimal_overall",
                                       "SA_TBall_baseline_vs_6h_HIV_neg",
                                       "SA_TBall_baseline_vs_minimal_HIV_neg",
                                       "SA_TBall_baseline_vs_optimal_HIV_neg",
                                       "SA_TBall_6h_vs_minimal_HIV_neg",
                                       "SA_TBall_6h_vs_optimal_HIV_neg",
                                       "SA_TBall_minimal_vs_optimal_HIV_neg",
                                       "SA_TBall_baseline_vs_6h_PLHIV",
                                       "SA_TBall_baseline_vs_minimal_PLHIV",
                                       "SA_TBall_baseline_vs_optimal_PLHIV",
                                       "SA_TBall_6h_vs_minimal_PLHIV",
                                       "SA_TBall_6h_vs_optimal_PLHIV",
                                       "SA_TBall_minimal_vs_optimal_PLHIV",
                                       "SA_TBdeaths_baseline_vs_6h_overall",
                                       "SA_TBdeaths_baseline_vs_minimal_overall",
                                       "SA_TBdeaths_baseline_vs_optimal_overall",
                                       "SA_TBdeaths_6h_vs_minimal_overall",
                                       "SA_TBdeaths_6h_vs_optimal_overall",
                                       "SA_TBdeaths_minimal_vs_optimal_overall",
                                       "SA_TBdeaths_baseline_vs_6h_HIV_neg",
                                       "SA_TBdeaths_baseline_vs_minimal_HIV_neg",
                                       "SA_TBdeaths_baseline_vs_optimal_HIV_neg",
                                       "SA_TBdeaths_6h_vs_minimal_HIV_neg",
                                       "SA_TBdeaths_6h_vs_optimal_HIV_neg",
                                       "SA_TBdeaths_minimal_vs_optimal_HIV_neg",
                                       "SA_TBdeaths_baseline_vs_6h_PLHIV",
                                       "SA_TBdeaths_baseline_vs_minimal_PLHIV",
                                       "SA_TBdeaths_baseline_vs_optimal_PLHIV",
                                       "SA_TBdeaths_6h_vs_minimal_PLHIV",
                                       "SA_TBdeaths_6h_vs_optimal_PLHIV",
                                       "SA_TBdeaths_minimal_vs_optimal_PLHIV",
                                       "SA_DALYs_baseline_vs_6h_overall",
                                       "SA_DALYs_baseline_vs_minimal_overall",
                                       "SA_DALYs_baseline_vs_optimal_overall",
                                       "SA_DALYs_6h_vs_minimal_overall",
                                       "SA_DALYs_6h_vs_optimal_overall",
                                       "SA_DALYs_minimal_vs_optimal_overall",
                                       "SA_DALYs_baseline_vs_6h_HIV_neg",
                                       "SA_DALYs_baseline_vs_minimal_HIV_neg",
                                       "SA_DALYs_baseline_vs_optimal_HIV_neg",
                                       "SA_DALYs_6h_vs_minimal_HIV_neg",
                                       "SA_DALYs_6h_vs_optimal_HIV_neg",
                                       "SA_DALYs_minimal_vs_optimal_HIV_neg",
                                       "SA_DALYs_baseline_vs_6h_PLHIV",
                                       "SA_DALYs_baseline_vs_minimal_PLHIV",
                                       "SA_DALYs_baseline_vs_optimal_PLHIV",
                                       "SA_DALYs_6h_vs_minimal_PLHIV",
                                       "SA_DALYs_6h_vs_optimal_PLHIV",
                                       "SA_DALYs_minimal_vs_optimal_PLHIV")
  
  #####################################################################################################################################
  #####################################################################################################################################
  #colnames(SA_incr_cost) <- c("Runs","All_TBPT_baseline vs 6H","All_TBPT_baseline vs minimal","All_TBPT_baseline vs optimal","All_TBPT_6H vs minimal", "All_TBPT_6H vs optimal", "All_TBPT_minimal vs optimal",
  #                                    "All_PT_baseline vs 6H","All_PT_baseline vs minimal","All_PT_baseline vs optimal","All_PT_6H vs minimal", "All_PT_6H vs optimal", "All_PT_minimal vs optimal",
  #                                   "All_TB_baseline vs 6H","All_TB_baseline vs minimal","All_TB_baseline vs optimal","All_TB_6H vs minimal", "All_TB_6H vs optimal", "All_TB_minimal vs optimal",
  #                                    "HHC_TBPT_baseline vs 6H","HHC_TBPT_baseline vs minimal","HHC_TBPT_baseline vs optimal","HHC_TBPT_6H vs minimal", "HHC_TBPT_6H vs optimal", "HHC_TBPT_minimal vs optimal",
  #                                   "HHC_PT_baseline vs 6H","HHC_PT_baseline vs minimal","HHC_PT_baseline vs optimal","HHC_PT_6H vs minimal", "HHC_PT_6H vs optimal", "HHC_PT_minimal vs optimal",
  #                                   "HHC_TB_baseline vs 6H","HHC_TB_baseline vs minimal","HHC_TB_baseline vs optimal","HHC_TB_6H vs minimal", "HHC_TB_6H vs optimal", "HHC_TB_minimal vs optimal",
  #                                   "HIV_TBPT_baseline vs 6H","HIV_TBPT_baseline vs minimal","HIV_TBPT_baseline vs optimal","HIV_TBPT_6H vs minimal", "HIV_TBPT_6H vs optimal", "HIV_TBPT_minimal vs optimal",
  #                                   "HIV_PT_baseline vs 6H","HIV_PT_baseline vs minimal","HIV_PT_baseline vs optimal","HIV_PT_6H vs minimal", "HIV_PT_6H vs optimal", "HIV_PT_minimal vs optimal",
  #                                   "HIV_TB_baseline vs 6H","HIV_TB_baseline vs minimal","HIV_TB_baseline vs optimal","HIV_TB_6H vs minimal", "HIV_TB_6H vs optimal", "HIV_TB_minimal vs optimal")
  
  
  SA_incr_effectiveness_graph<-melt(SA_incr_effectiveness, id.vars =c("Runs"), variable.name = "effectiveness_pop")
  SA_incr_effectiveness_graph$cum_effectiveness<-ave(SA_incr_effectiveness_graph$value, SA_incr_effectiveness_graph$effectiveness_pop, FUN=cumsum)
  SA_incr_effectiveness_graph$comparison = ifelse(grepl("baseline_vs_6h", SA_incr_effectiveness_graph$effectiveness_pop), "1a. 6H vs baseline",
                                         ifelse(grepl("baseline_vs_minimal", SA_incr_effectiveness_graph$effectiveness_pop),"1b. Minimal vs baseline",
                                                ifelse(grepl("baseline_vs_optimal", SA_incr_effectiveness_graph$effectiveness_pop),"1c. Optimal vs baseline",
                                                       ifelse(grepl("6h_vs_minimal", SA_incr_effectiveness_graph$effectiveness_pop), "2a. Minimal vs 6H",
                                                              ifelse(grepl("6h_vs_optimal", SA_incr_effectiveness_graph$effectiveness_pop),"2b. Optimal vs 6H","3. Optimal vs minimal")))))
  SA_incr_effectiveness_graph$outcome = ifelse(grepl("PT", SA_incr_effectiveness_graph$effectiveness_pop), "1. PT",
                                        ifelse(grepl("DSTB", SA_incr_effectiveness_graph$effectiveness_pop),"2. DS-TB cases",
                                        ifelse(grepl("DRTB", SA_incr_effectiveness_graph$effectiveness_pop),"3. RR-TB cases",
                                        ifelse(grepl("TBall", SA_incr_effectiveness_graph$effectiveness_pop),"4. All TB cases (DS-TB & RR-TB)",
                                        ifelse(grepl("TBdeaths", SA_incr_effectiveness_graph$effectiveness_pop),"5. TB deaths",
                                                      "6. DALYs")))))
  SA_incr_effectiveness_graph$population = ifelse(grepl("overall", SA_incr_effectiveness_graph$effectiveness_pop), "All treatment candidates",
                                         ifelse(grepl("PLHIV", SA_incr_effectiveness_graph$effectiveness_pop),"PLHIV","HHC (HIV negatives)"))
  SA_incr_effectiveness_graph$effectiveness_pop = chartr("."," ",SA_incr_effectiveness_graph$effectiveness_pop)
  
  
  
  SA_incr_effectiveness<-aggregate(x=SA_incr_effectiveness_graph$value, by=list(Runs=SA_incr_effectiveness_graph$Runs, comparison=SA_incr_effectiveness_graph$comparison,outcome=SA_incr_effectiveness_graph$outcome,population=SA_incr_effectiveness_graph$population), FUN=sum)
  
  #####################################################################################################################
  #Incremental effectiveness table with ranges  
  #####################################################################################################################
  SA_incr_effectiveness_table<-SA_incr_effectiveness %>% 
    group_by(population,outcome, comparison) %>% 
    summarise_at(vars(x),
                 list(median=median,
                      mean=mean,
                      quantile_lower = ~quantile(., probs = 0.025),
                      quantile_upper = ~quantile(., probs = 0.975)))
  write.csv(SA_incr_effectiveness_table, file = "6. South African incremental effectiveness output tables.csv")
  
  
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  
  SA_incr_effectiveness<-aggregate(x=SA_incr_effectiveness_graph$value, by=list(Runs=SA_incr_effectiveness_graph$Runs, comparison=SA_incr_effectiveness_graph$comparison,outcome=SA_incr_effectiveness_graph$outcome,population=SA_incr_effectiveness_graph$population), FUN=sum)
  #Incremental cost graph
  SA_incr_effectiveness_graph <- ggplot(data=SA_incr_effectiveness, aes(comparison,x,color=comparison)) + 
    geom_boxplot(notch = TRUE) +
    scale_y_continuous(labels = unit_format( unit = "K", scale = 1e-3))+
    facet_wrap(~population + outcome, dir = "h", scales = "free") +
    labs(x = "population group",y = expression(paste("Incremental effectiveness")),colour = "",subtitle="",caption="")+
    guides(fill = "none")+
    stat_boxplot(geom = "errorbar", width = 0.5) +  
    geom_boxplot()+
    scale_color_brewer(palette = "Set1")+
    theme_classic()+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  ggsave("6. SA_incr_effectiveness_graph.jpeg", 
         width = 25, height = 16, units = "cm")
  
  
  #incremental graph option 2 - OVERALL
  
  SA_incr_effectiveness_table_overall<-SA_incr_effectiveness_table %>%
    filter(population =="All treatment candidates")
#    filter(data_type !="Epi data" | state!="TPT")
    
  SA_incr_effectiveness_table_overall$fill <- ifelse(SA_incr_effectiveness_table_overall$median > 0, "#0408e0", "#c43b00")
  
  SA_incremental_effectiveness_overall_plot <- ggplot(SA_incr_effectiveness_table_overall, aes(x = comparison, y = median, fill = fill)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label=paste("",round(median/1000,0),"K")), position=position_dodge(width=0.9), vjust=-0.25, size=3, fontface="bold")+
    facet_wrap(~outcome, scales="fixed") +
    scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
    scale_fill_discrete(name="Incremental effectiveness",
                        breaks=c("#0408e0", "#c43b00"),
                        labels=c("Increases","Averted"))+
    #scale_color_hue(labels = c("Savings","Additional cost"))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none",)+
    xlab("Comparison") +
    ylab("Incremental effectiveness") +
    labs(title="",
         subtitle="",
         caption="")+
    
    
    ggtitle("Incremental effectiveness of TPT strategies for South Africa (All treatment candidates 2020-2035)")
  ggsave("6a. SA_incremental effectiveness (All treatment candidates).png", 
         width = 30, height = 20, units = "cm")
  
  
  SA_incremental_effectiveness_overall_plot
  
  
    
  #incremental graph option 2 - HHC (HIV negatives)
  
  SA_incr_effectiveness_table_HIVneg<-SA_incr_effectiveness_table %>%
    filter(population =="HHC (HIV negatives)")

  SA_incr_effectiveness_table_HIVneg$fill <- ifelse(SA_incr_effectiveness_table_HIVneg$median > 0, "#0408e0", "#c43b00")
  
  SA_incremental_effectiveness_HIVneg_plot <- ggplot(SA_incr_effectiveness_table_HIVneg, aes(x = comparison, y = median, fill = fill)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label=paste("",round(median/1000,0),"K")), position=position_dodge(width=0.9), vjust=-0.25, size=3, fontface="bold")+
    facet_wrap(~outcome, scales="fixed") +
    scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
    scale_fill_discrete(name="Incremental effectiveness",
                        breaks=c("#0408e0", "#c43b00"),
                        labels=c("Increases","Averted"))+
    #scale_color_hue(labels = c("Savings","Additional cost"))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none",)+
    xlab("Comparison") +
    ylab("Incremental effectiveness") +
    labs(title="",
         subtitle="",
         caption="")+
    
    
    ggtitle("Incremental effectiveness of TPT strategies for South Africa (HHC (HIV negatives) 2020-2035)")
  ggsave("6b. SA_incremental effectiveness (HHC (HIV negatives)).png", 
         width = 30, height = 20, units = "cm")
  
  
  SA_incremental_effectiveness_HIVneg_plot
  
  
  
  
  #incremental graph option 2 - PLHIV
  
  SA_incr_effectiveness_table_PLHIV<-SA_incr_effectiveness_table %>%
    filter(population =="PLHIV")
  
  SA_incr_effectiveness_table_PLHIV$fill <- ifelse(SA_incr_effectiveness_table_PLHIV$median > 0, "#0408e0", "#c43b00")
  
  SA_incremental_effectiveness_PLHIV_plot <- ggplot(SA_incr_effectiveness_table_PLHIV, aes(x = comparison, y = median, fill = fill)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label=paste("",round(median/1000,0),"K")), position=position_dodge(width=0.9), vjust=-0.25, size=3, fontface="bold")+
    facet_wrap(~outcome, scales="fixed") +
    scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
    scale_fill_discrete(name="Incremental effectiveness",
                        breaks=c("#0408e0", "#c43b00"),
                        labels=c("Increases","Averted"))+
    #scale_color_hue(labels = c("Savings","Additional cost"))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none",)+
    xlab("Comparison") +
    ylab("Incremental effectiveness") +
    labs(title="",
         subtitle="",
         caption="")+
    
    
    ggtitle("Incremental effectiveness of TPT strategies for South Africa (PLHIV 2020-2035)")
  ggsave("6c. SA_incremental effectiveness (PLHIV).png", 
         width = 30, height = 20, units = "cm")
  
  
  SA_incremental_effectiveness_PLHIV_plot
  
  
  
  # combine cumulative cost plots multiple
  SA_cumulative_combine<-ggarrange(SA_PT_cost_cumulat_graph, SA_cost_TB_cumulat_graph, SA_cost_MDRTB_cumulat_graph, SA_cost_cumulat_graph, labels = c("PT", "TB", "RR-TB", "PT&TB"),
                                   common.legend = TRUE, legend = "bottom")
  SA_cumulative_combine
  ggsave("7. SA_combined_graphs_cumulative.png", 
         width = 30, height = 20, units = "cm")
  


  
  
  
  
  
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### INCREMENT COST EFFECTIVENESS RATIO #######
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
  
  
  
  #####################################################################################################################################
  #ICER -  Overall (TB cases (DS-TB & RR-TB))
  SA_ICER_TB_baseline_vs_6h_overall <- data.frame(Runs=SA_baseline$Run,(SA_strategy_cost$Expanded.6H-SA_strategy_cost$Baseline)/(SA_epi_cumulative_TBall$Baseline-SA_epi_cumulative_TBall$Expanded.6H))
  SA_ICER_TB_baseline_vs_minimal_overall <- data.frame((SA_strategy_cost$Minimal.regimen-SA_strategy_cost$Baseline)/(SA_epi_cumulative_TBall$Baseline-SA_epi_cumulative_TBall$Minimal.regimen))

  #####################################################################################################################################
  #ICER -  Overall (TB deaths)
  SA_ICER_TBdeaths_baseline_vs_6h_overall <- data.frame((SA_strategy_cost$Expanded.6H-SA_strategy_cost$Baseline)/(SA_epi_cumulative_TBdeaths$Baseline-SA_epi_cumulative_TBdeaths$Expanded.6H))
  SA_ICER_TBdeaths_baseline_vs_minimal_overall <- data.frame((SA_strategy_cost$Minimal.regimen-SA_strategy_cost$Baseline)/(SA_epi_cumulative_TBdeaths$Baseline-SA_epi_cumulative_TBdeaths$Minimal.regimen))
  
  #####################################################################################################################################
  #ICER -  Overall (DALYs)
  SA_ICER_DALYs_baseline_vs_6h_overall <- data.frame((SA_strategy_cost$Expanded.6H-SA_strategy_cost$Baseline)/(SA_epi_cumulative_DALYs$Baseline-SA_epi_cumulative_DALYs$Expanded.6H))
  SA_ICER_DALYs_baseline_vs_minimal_overall <- data.frame((SA_strategy_cost$Minimal.regimen-SA_strategy_cost$Baseline)/(SA_epi_cumulative_DALYs$Baseline-SA_epi_cumulative_DALYs$Minimal.regimen))
  
  
  
  
  #combine columns
  SA_ICER<-data.frame(SA_ICER_TB_baseline_vs_6h_overall,
                      SA_ICER_TB_baseline_vs_minimal_overall,
                      SA_ICER_TBdeaths_baseline_vs_6h_overall,
                      SA_ICER_TBdeaths_baseline_vs_minimal_overall,
                      SA_ICER_DALYs_baseline_vs_6h_overall,
                      SA_ICER_DALYs_baseline_vs_minimal_overall)
  colnames(SA_ICER) <- c("Runs","SA_ICER_TB_baseline_vs_6h_overall",
                         "SA_ICER_TB_baseline_vs_minimal_overall",
                         "SA_ICER_TBdeaths_baseline_vs_6h_overall",
                         "SA_ICER_TBdeaths_baseline_vs_minimal_overall",
                         "SA_ICER_DALYs_baseline_vs_6h_overall",
                         "SA_ICER_DALYs_baseline_vs_minimal_overall")
  SA_ICER_graph<-melt(SA_ICER, id.vars =c("Runs"), variable.name = "ICER_pop")
  SA_ICER_graph$cum_ICER<-ave(SA_ICER_graph$value, SA_ICER_graph$ICER_pop, FUN=cumsum)
  SA_ICER_graph$comparison = ifelse(grepl("baseline_vs_6h", SA_ICER_graph$ICER_pop), "1a. 6H vs baseline",
                                                  "1b. Minimal vs baseline")
  SA_ICER_graph$outcome = ifelse(grepl("DALYs", SA_ICER_graph$ICER_pop),"3. incr. cost per DALY averted",
                                 ifelse(grepl("TBdeaths", SA_ICER_graph$ICER_pop),"2. incr. cost per TB death averted",
                                                                           "1. incr. cost per TB case averted"))
  SA_ICER_graph$ICER_pop = chartr("."," ",SA_ICER_graph$ICER_pop)
  

  SA_ICER<-aggregate(x=SA_ICER_graph$value, by=list(Runs=SA_ICER_graph$Runs, comparison=SA_ICER_graph$comparison,outcome=SA_ICER_graph$outcome), FUN=sum)
  
  #####################################################################################################################
  #Incremental effectiveness table with ranges  
  #####################################################################################################################
  SA_ICER_table<-SA_ICER %>% 
    group_by(outcome, comparison) %>% 
    summarise_at(vars(x),
                 list(median=median,
                      mean=mean,
                      quantile_lower = ~quantile(., probs = 0.025),
                      quantile_upper = ~quantile(., probs = 0.975)))
  write.csv(SA_ICER_table, file = "8. South African ICERs output tables.csv")
  
  
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

                                                              ##### BRAZIL #######
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#####################################################################################################################
#                                               LOAD THE DATA                                                       #
#####################################################################################################################
#Load the micro costing summaries
Brazil_costs <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Template_Brazil", col_types = "numeric", range = "A1:T16001")

#Load Brazil mathematical model epidemiological outputs
Brazil_baseline <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Brazil_baseline_data", col_types = "numeric", range = "A1:R16001")
Brazil_scaleup <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Brazil_scaleup_data", col_types = "numeric", range = "A1:R16001")
Brazil_minimal <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Brazil_minimal_data", col_types = "numeric", range = "A1:R16001")
Brazil_optimal <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Brazil_optimal_data", col_types = "numeric", range = "A1:R16001")


#####################################################################################################################
#                                       baseline ANNUAL COSTS - DOT                                                 #
#####################################################################################################################


#Annual costs for baseline dataset
# PT
Brazil_baseline$C_PT_all<-(Brazil_baseline$New_PT_all*(Brazil_costs$LTBI_baseline+Brazil_costs$AE_LTBI)) # Cost: PT all
Brazil_baseline$C_PT_hiv_neg<-(Brazil_baseline$New_PT_all-Brazil_baseline$New_PT_HIV)*(Brazil_costs$LTBI_baseline+Brazil_costs$AE_LTBI) # Cost: PT (HIV negative)
Brazil_baseline$C_PT_hiv<-(Brazil_baseline$New_PT_HIV*(Brazil_costs$LTBI_baseline+Brazil_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
Brazil_baseline$C_DSTB_all_DOT_SAT<-(Brazil_baseline$TB_cases_all-Brazil_baseline$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_baseline$C_DSTB_hiv_neg_DOT_SAT<-((Brazil_baseline$TB_cases_all-Brazil_baseline$TB_cases_MDR)-(Brazil_baseline$TB_cases_HIV-Brazil_baseline$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_baseline$C_DSTB_hiv_DOT_SAT<-(Brazil_baseline$TB_cases_HIV-Brazil_baseline$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
Brazil_baseline$C_MDRTB_all_DOT_SAT<-(Brazil_baseline$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_baseline$C_MDRTB_hiv_neg_DOT_SAT<-(Brazil_baseline$TB_cases_MDR-Brazil_baseline$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_baseline$C_MDRTB_hiv_DOT_SAT<-Brazil_baseline$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
Brazil_baseline$C_TB_all_DOT_SAT<-Brazil_baseline$C_DSTB_all_DOT_SAT+Brazil_baseline$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_baseline$C_TB_hiv_neg_DOT_SAT<-Brazil_baseline$C_DSTB_hiv_neg_DOT_SAT+Brazil_baseline$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_baseline$C_TB_hiv_DOT_SAT<-Brazil_baseline$C_DSTB_hiv_DOT_SAT+Brazil_baseline$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_baseline$C_TB_all_DOT_SAT)
#sum(Brazil_baseline$C_TB_hiv_neg_DOT_SAT)
#sum(Brazil_baseline$C_TB_hiv_DOT_SAT)

#baseline STRATEGY COST: PT & TB (DOT)
Brazil_baseline$C_baseline_all_DOT_SAT<-Brazil_baseline$C_PT_all+Brazil_baseline$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT
Brazil_baseline$C_baseline_hiv_neg_DOT_SAT<-Brazil_baseline$C_PT_hiv_neg+Brazil_baseline$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT
Brazil_baseline$C_baseline_hiv_DOT_SAT<-Brazil_baseline$C_PT_hiv+Brazil_baseline$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       baseline ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
Brazil_baseline$C_DSTB_all_DOT_SAT_SAT<-(Brazil_baseline$TB_cases_all-Brazil_baseline$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_baseline$C_DSTB_hiv_neg_DOT_SAT_SAT<-((Brazil_baseline$TB_cases_all-Brazil_baseline$TB_cases_MDR)-(Brazil_baseline$TB_cases_HIV-Brazil_baseline$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_baseline$C_DSTB_hiv_DOT_SAT_SAT<-(Brazil_baseline$TB_cases_HIV-Brazil_baseline$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
Brazil_baseline$C_MDRTB_all_DOT_SAT_SAT<-(Brazil_baseline$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_baseline$C_MDRTB_hiv_neg_DOT_SAT_SAT<-(Brazil_baseline$TB_cases_MDR-Brazil_baseline$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_baseline$C_MDRTB_hiv_DOT_SAT_SAT<-Brazil_baseline$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
Brazil_baseline$C_TB_all_DOT_SAT_SAT<-Brazil_baseline$C_DSTB_all_DOT_SAT_SAT+Brazil_baseline$C_MDRTB_all_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_baseline$C_TB_hiv_neg_DOT_SAT_SAT<-Brazil_baseline$C_DSTB_hiv_neg_DOT_SAT_SAT+Brazil_baseline$C_MDRTB_hiv_neg_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_baseline$C_TB_hiv_DOT_SAT_SAT<-Brazil_baseline$C_DSTB_hiv_DOT_SAT_SAT+Brazil_baseline$C_MDRTB_hiv_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_baseline$C_TB_all_DOT_SAT_SAT)
#sum(Brazil_baseline$C_TB_hiv_neg_DOT_SAT_SAT)
#sum(Brazil_baseline$C_TB_hiv_DOT_SAT_SAT)

#baseline STRATEGY COST: PT & TB (DOT_SAT)
Brazil_baseline$C_baseline_all_DOT_SAT_SAT<-Brazil_baseline$C_PT_all+Brazil_baseline$C_TB_all_DOT_SAT_SAT #Cost: strategy all (PT+TB) DOT_SAT
Brazil_baseline$C_baseline_hiv_neg_DOT_SAT_SAT<-Brazil_baseline$C_PT_hiv_neg+Brazil_baseline$C_TB_hiv_neg_DOT_SAT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
Brazil_baseline$C_baseline_hiv_DOT_SAT_SAT<-Brazil_baseline$C_PT_hiv+Brazil_baseline$C_TB_hiv_DOT_SAT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT





#####################################################################################################################
#                                       scaleup ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for scaleup dataset
# PT
Brazil_scaleup$C_PT_all<-(Brazil_scaleup$New_PT_all*(Brazil_costs$LTBI_scaleup+Brazil_costs$AE_LTBI)) # Cost: PT all
Brazil_scaleup$C_PT_hiv_neg<-(Brazil_scaleup$New_PT_all-Brazil_scaleup$New_PT_HIV)*(Brazil_costs$LTBI_scaleup+Brazil_costs$AE_LTBI) # Cost: PT (HIV negative)
Brazil_scaleup$C_PT_hiv<-(Brazil_scaleup$New_PT_HIV*(Brazil_costs$LTBI_scaleup+Brazil_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
Brazil_scaleup$C_DSTB_all_DOT_SAT<-(Brazil_scaleup$TB_cases_all-Brazil_scaleup$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_scaleup$C_DSTB_hiv_neg_DOT_SAT<-((Brazil_scaleup$TB_cases_all-Brazil_scaleup$TB_cases_MDR)-(Brazil_scaleup$TB_cases_HIV-Brazil_scaleup$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_scaleup$C_DSTB_hiv_DOT_SAT<-(Brazil_scaleup$TB_cases_HIV-Brazil_scaleup$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
Brazil_scaleup$C_MDRTB_all_DOT_SAT<-(Brazil_scaleup$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_scaleup$C_MDRTB_hiv_neg_DOT_SAT<-(Brazil_scaleup$TB_cases_MDR-Brazil_scaleup$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_scaleup$C_MDRTB_hiv_DOT_SAT<-Brazil_scaleup$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
Brazil_scaleup$C_TB_all_DOT_SAT<-Brazil_scaleup$C_DSTB_all_DOT_SAT+Brazil_scaleup$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_scaleup$C_TB_hiv_neg_DOT_SAT<-Brazil_scaleup$C_DSTB_hiv_neg_DOT_SAT+Brazil_scaleup$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_scaleup$C_TB_hiv_DOT_SAT<-Brazil_scaleup$C_DSTB_hiv_DOT_SAT+Brazil_scaleup$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_scaleup$C_TB_all_DOT_SAT)
#sum(Brazil_scaleup$C_TB_hiv_neg_DOT_SAT)
#sum(Brazil_scaleup$C_TB_hiv_DOT_SAT)

#scaleup STRATEGY COST: PT & TB (DOT)
Brazil_scaleup$C_scaleup_all_DOT_SAT<-Brazil_scaleup$C_PT_all+Brazil_scaleup$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT
Brazil_scaleup$C_scaleup_hiv_neg_DOT_SAT<-Brazil_scaleup$C_PT_hiv_neg+Brazil_scaleup$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT
Brazil_scaleup$C_scaleup_hiv_DOT_SAT<-Brazil_scaleup$C_PT_hiv+Brazil_scaleup$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       scaleup ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
Brazil_scaleup$C_DSTB_all_DOT_SAT_SAT<-(Brazil_scaleup$TB_cases_all-Brazil_scaleup$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_scaleup$C_DSTB_hiv_neg_DOT_SAT_SAT<-((Brazil_scaleup$TB_cases_all-Brazil_scaleup$TB_cases_MDR)-(Brazil_scaleup$TB_cases_HIV-Brazil_scaleup$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_scaleup$C_DSTB_hiv_DOT_SAT_SAT<-(Brazil_scaleup$TB_cases_HIV-Brazil_scaleup$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
Brazil_scaleup$C_MDRTB_all_DOT_SAT_SAT<-(Brazil_scaleup$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_scaleup$C_MDRTB_hiv_neg_DOT_SAT_SAT<-(Brazil_scaleup$TB_cases_MDR-Brazil_scaleup$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_scaleup$C_MDRTB_hiv_DOT_SAT_SAT<-Brazil_scaleup$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
Brazil_scaleup$C_TB_all_DOT_SAT_SAT<-Brazil_scaleup$C_DSTB_all_DOT_SAT_SAT+Brazil_scaleup$C_MDRTB_all_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_scaleup$C_TB_hiv_neg_DOT_SAT_SAT<-Brazil_scaleup$C_DSTB_hiv_neg_DOT_SAT_SAT+Brazil_scaleup$C_MDRTB_hiv_neg_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_scaleup$C_TB_hiv_DOT_SAT_SAT<-Brazil_scaleup$C_DSTB_hiv_DOT_SAT_SAT+Brazil_scaleup$C_MDRTB_hiv_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_scaleup$C_TB_all_DOT_SAT_SAT)
#sum(Brazil_scaleup$C_TB_hiv_neg_DOT_SAT_SAT)
#sum(Brazil_scaleup$C_TB_hiv_DOT_SAT_SAT)

#scaleup STRATEGY COST: PT & TB (DOT_SAT)
Brazil_scaleup$C_scaleup_all_DOT_SAT_SAT<-Brazil_scaleup$C_PT_all+Brazil_scaleup$C_TB_all_DOT_SAT_SAT #Cost: strategy all (PT+TB) DOT_SAT
Brazil_scaleup$C_scaleup_hiv_neg_DOT_SAT_SAT<-Brazil_scaleup$C_PT_hiv_neg+Brazil_scaleup$C_TB_hiv_neg_DOT_SAT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
Brazil_scaleup$C_scaleup_hiv_DOT_SAT_SAT<-Brazil_scaleup$C_PT_hiv+Brazil_scaleup$C_TB_hiv_DOT_SAT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT




#####################################################################################################################
#                                       minimal ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for minimal dataset
# PT
Brazil_minimal$C_PT_all<-(Brazil_minimal$New_PT_all*(Brazil_costs$LTBI_minimal+Brazil_costs$AE_LTBI)) # Cost: PT all
Brazil_minimal$C_PT_hiv_neg<-(Brazil_minimal$New_PT_all-Brazil_minimal$New_PT_HIV)*(Brazil_costs$LTBI_minimal+Brazil_costs$AE_LTBI) # Cost: PT (HIV negative)
Brazil_minimal$C_PT_hiv<-(Brazil_minimal$New_PT_HIV*(Brazil_costs$LTBI_minimal+Brazil_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
Brazil_minimal$C_DSTB_all_DOT_SAT<-(Brazil_minimal$TB_cases_all-Brazil_minimal$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_minimal$C_DSTB_hiv_neg_DOT_SAT<-((Brazil_minimal$TB_cases_all-Brazil_minimal$TB_cases_MDR)-(Brazil_minimal$TB_cases_HIV-Brazil_minimal$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_minimal$C_DSTB_hiv_DOT_SAT<-(Brazil_minimal$TB_cases_HIV-Brazil_minimal$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
Brazil_minimal$C_MDRTB_all_DOT_SAT<-(Brazil_minimal$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_minimal$C_MDRTB_hiv_neg_DOT_SAT<-(Brazil_minimal$TB_cases_MDR-Brazil_minimal$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_minimal$C_MDRTB_hiv_DOT_SAT<-Brazil_minimal$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
Brazil_minimal$C_TB_all_DOT_SAT<-Brazil_minimal$C_DSTB_all_DOT_SAT+Brazil_minimal$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_minimal$C_TB_hiv_neg_DOT_SAT<-Brazil_minimal$C_DSTB_hiv_neg_DOT_SAT+Brazil_minimal$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_minimal$C_TB_hiv_DOT_SAT<-Brazil_minimal$C_DSTB_hiv_DOT_SAT+Brazil_minimal$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_minimal$C_TB_all_DOT_SAT)
#sum(Brazil_minimal$C_TB_hiv_neg_DOT_SAT)
#sum(Brazil_minimal$C_TB_hiv_DOT_SAT)

#minimal STRATEGY COST: PT & TB (DOT)
Brazil_minimal$C_minimal_all_DOT_SAT<-Brazil_minimal$C_PT_all+Brazil_minimal$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT
Brazil_minimal$C_minimal_hiv_neg_DOT_SAT<-Brazil_minimal$C_PT_hiv_neg+Brazil_minimal$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT
Brazil_minimal$C_minimal_hiv_DOT_SAT<-Brazil_minimal$C_PT_hiv+Brazil_minimal$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       minimal ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
Brazil_minimal$C_DSTB_all_DOT_SAT_SAT<-(Brazil_minimal$TB_cases_all-Brazil_minimal$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_minimal$C_DSTB_hiv_neg_DOT_SAT_SAT<-((Brazil_minimal$TB_cases_all-Brazil_minimal$TB_cases_MDR)-(Brazil_minimal$TB_cases_HIV-Brazil_minimal$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_minimal$C_DSTB_hiv_DOT_SAT_SAT<-(Brazil_minimal$TB_cases_HIV-Brazil_minimal$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
Brazil_minimal$C_MDRTB_all_DOT_SAT_SAT<-(Brazil_minimal$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_minimal$C_MDRTB_hiv_neg_DOT_SAT_SAT<-(Brazil_minimal$TB_cases_MDR-Brazil_minimal$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_minimal$C_MDRTB_hiv_DOT_SAT_SAT<-Brazil_minimal$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
Brazil_minimal$C_TB_all_DOT_SAT_SAT<-Brazil_minimal$C_DSTB_all_DOT_SAT_SAT+Brazil_minimal$C_MDRTB_all_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_minimal$C_TB_hiv_neg_DOT_SAT_SAT<-Brazil_minimal$C_DSTB_hiv_neg_DOT_SAT_SAT+Brazil_minimal$C_MDRTB_hiv_neg_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_minimal$C_TB_hiv_DOT_SAT_SAT<-Brazil_minimal$C_DSTB_hiv_DOT_SAT_SAT+Brazil_minimal$C_MDRTB_hiv_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_minimal$C_TB_all_DOT_SAT_SAT)
#sum(Brazil_minimal$C_TB_hiv_neg_DOT_SAT_SAT)
#sum(Brazil_minimal$C_TB_hiv_DOT_SAT_SAT)

#minimal STRATEGY COST: PT & TB (DOT_SAT)
Brazil_minimal$C_minimal_all_DOT_SAT_SAT<-Brazil_minimal$C_PT_all+Brazil_minimal$C_TB_all_DOT_SAT_SAT #Cost: strategy all (PT+TB) DOT_SAT
Brazil_minimal$C_minimal_hiv_neg_DOT_SAT_SAT<-Brazil_minimal$C_PT_hiv_neg+Brazil_minimal$C_TB_hiv_neg_DOT_SAT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
Brazil_minimal$C_minimal_hiv_DOT_SAT_SAT<-Brazil_minimal$C_PT_hiv+Brazil_minimal$C_TB_hiv_DOT_SAT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT




#####################################################################################################################
#                                       optimal ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for optimal dataset
# PT
Brazil_optimal$C_PT_all<-(Brazil_optimal$New_PT_all*(Brazil_costs$LTBI_optimal+Brazil_costs$AE_LTBI)) # Cost: PT all
Brazil_optimal$C_PT_hiv_neg<-(Brazil_optimal$New_PT_all-Brazil_optimal$New_PT_HIV)*(Brazil_costs$LTBI_optimal+Brazil_costs$AE_LTBI) # Cost: PT (HIV negative)
Brazil_optimal$C_PT_hiv<-(Brazil_optimal$New_PT_HIV*(Brazil_costs$LTBI_optimal+Brazil_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
Brazil_optimal$C_DSTB_all_DOT_SAT<-(Brazil_optimal$TB_cases_all-Brazil_optimal$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_optimal$C_DSTB_hiv_neg_DOT_SAT<-((Brazil_optimal$TB_cases_all-Brazil_optimal$TB_cases_MDR)-(Brazil_optimal$TB_cases_HIV-Brazil_optimal$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_optimal$C_DSTB_hiv_DOT_SAT<-(Brazil_optimal$TB_cases_HIV-Brazil_optimal$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
Brazil_optimal$C_MDRTB_all_DOT_SAT<-(Brazil_optimal$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_optimal$C_MDRTB_hiv_neg_DOT_SAT<-(Brazil_optimal$TB_cases_MDR-Brazil_optimal$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_optimal$C_MDRTB_hiv_DOT_SAT<-Brazil_optimal$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
Brazil_optimal$C_TB_all_DOT_SAT<-Brazil_optimal$C_DSTB_all_DOT_SAT+Brazil_optimal$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_optimal$C_TB_hiv_neg_DOT_SAT<-Brazil_optimal$C_DSTB_hiv_neg_DOT_SAT+Brazil_optimal$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_optimal$C_TB_hiv_DOT_SAT<-Brazil_optimal$C_DSTB_hiv_DOT_SAT+Brazil_optimal$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_optimal$C_TB_all_DOT_SAT)
#sum(Brazil_optimal$C_TB_hiv_neg_DOT_SAT)
#sum(Brazil_optimal$C_TB_hiv_DOT_SAT)

#optimal STRATEGY COST: PT & TB (DOT)
Brazil_optimal$C_optimal_all_DOT_SAT<-Brazil_optimal$C_PT_all+Brazil_optimal$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT
Brazil_optimal$C_optimal_hiv_neg_DOT_SAT<-Brazil_optimal$C_PT_hiv_neg+Brazil_optimal$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT
Brazil_optimal$C_optimal_hiv_DOT_SAT<-Brazil_optimal$C_PT_hiv+Brazil_optimal$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       optimal ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
Brazil_optimal$C_DSTB_all_DOT_SAT_SAT<-(Brazil_optimal$TB_cases_all-Brazil_optimal$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_optimal$C_DSTB_hiv_neg_DOT_SAT_SAT<-((Brazil_optimal$TB_cases_all-Brazil_optimal$TB_cases_MDR)-(Brazil_optimal$TB_cases_HIV-Brazil_optimal$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_optimal$C_DSTB_hiv_DOT_SAT_SAT<-(Brazil_optimal$TB_cases_HIV-Brazil_optimal$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
Brazil_optimal$C_MDRTB_all_DOT_SAT_SAT<-(Brazil_optimal$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_optimal$C_MDRTB_hiv_neg_DOT_SAT_SAT<-(Brazil_optimal$TB_cases_MDR-Brazil_optimal$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_optimal$C_MDRTB_hiv_DOT_SAT_SAT<-Brazil_optimal$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
Brazil_optimal$C_TB_all_DOT_SAT_SAT<-Brazil_optimal$C_DSTB_all_DOT_SAT_SAT+Brazil_optimal$C_MDRTB_all_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_optimal$C_TB_hiv_neg_DOT_SAT_SAT<-Brazil_optimal$C_DSTB_hiv_neg_DOT_SAT_SAT+Brazil_optimal$C_MDRTB_hiv_neg_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_optimal$C_TB_hiv_DOT_SAT_SAT<-Brazil_optimal$C_DSTB_hiv_DOT_SAT_SAT+Brazil_optimal$C_MDRTB_hiv_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_optimal$C_TB_all_DOT_SAT_SAT)
#sum(Brazil_optimal$C_TB_hiv_neg_DOT_SAT_SAT)
#sum(Brazil_optimal$C_TB_hiv_DOT_SAT_SAT)

#optimal STRATEGY COST: PT & TB (DOT_SAT)
Brazil_optimal$C_optimal_all_DOT_SAT_SAT<-Brazil_optimal$C_PT_all+Brazil_optimal$C_TB_all_DOT_SAT_SAT #Cost: strategy all (PT+TB) DOT_SAT
Brazil_optimal$C_optimal_hiv_neg_DOT_SAT_SAT<-Brazil_optimal$C_PT_hiv_neg+Brazil_optimal$C_TB_hiv_neg_DOT_SAT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
Brazil_optimal$C_optimal_hiv_DOT_SAT_SAT<-Brazil_optimal$C_PT_hiv+Brazil_optimal$C_TB_hiv_DOT_SAT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT






#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### PREP DATA FOR ANALYSIS/PLOTTING #######
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#Melt data in key packets for plotting
Brazil_strategy_cost<-data.frame(
  Year=Brazil_baseline$Year,
  Baseline=Brazil_baseline$C_baseline_all_DOT_SAT,
  Baseline_HIV_neg=Brazil_baseline$C_baseline_hiv_neg_DOT_SAT,
  Baseline_PLHIV=Brazil_baseline$C_baseline_hiv_DOT_SAT,
  "Expanded 6H"=Brazil_scaleup$C_scaleup_all_DOT_SAT,
  "Expanded 6H_neg"=Brazil_scaleup$C_scaleup_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=Brazil_scaleup$C_scaleup_hiv_DOT_SAT,
  "Minimal regimen"=Brazil_minimal$C_minimal_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=Brazil_minimal$C_minimal_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=Brazil_minimal$C_minimal_hiv_DOT_SAT,
  "Optimal regimen"=Brazil_optimal$C_optimal_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=Brazil_optimal$C_optimal_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=Brazil_optimal$C_optimal_hiv_DOT_SAT)
Brazil_cost_pop_graph<-melt(Brazil_strategy_cost, id.vars =c("Year"), variable.name = "cost_pop")
Brazil_cost_pop_graph$cum_cost<-ave(Brazil_cost_pop_graph$value, Brazil_cost_pop_graph$cost_pop, FUN=cumsum)
Brazil_cost_pop_graph$Year<- as.Date(ISOdate(Brazil_cost_pop_graph$Year,1,1) ) #set up date variable
Brazil_cost_pop_graph$strategy = ifelse(grepl("Baseline", Brazil_cost_pop_graph$cost_pop), "_baseline",
                                    ifelse(grepl("6H", Brazil_cost_pop_graph$cost_pop),"6H",ifelse(grepl("Minimal", Brazil_cost_pop_graph$cost_pop), "minimal","Optimal")))
Brazil_cost_pop_graph$population = ifelse(!grepl("_", Brazil_cost_pop_graph$cost_pop), "All treatment candidates",
                                      ifelse(grepl("PLHIV", Brazil_cost_pop_graph$cost_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_cost_pop_graph$cost_pop = chartr("."," ",Brazil_cost_pop_graph$cost_pop)

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### PLOTS #######
#####################################################################################################################
#####################################################################################################################


#####################################################################################################################
#Annual cost per population group#
#####################################################################################################################


####################
Brazil_graph_annual_costs <- ggplot(Brazil_cost_pop_graph,aes(Year,value))+
  scale_color_brewer(palette = "Set1")+
  theme_classic()
# annual costs graph (not smoothed out)
Brazil_graph_annual_costs+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_summary(fun = mean,geom = "line",size = 1,aes(colour = strategy))+
  labs(x = "Years",y = expression(paste("Annual cost (US$)")),colour = "")
ggsave("Brazil_graph_annual_cost.jpeg", 
       width = 25, height = 16, units = "cm")

# smoothed annual costs graph

Brazil_graph_annual_costs+
  #  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE) +
  # geom_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE)+
  labs(x = "Years",y = expression(paste("Annual cost (US$)")),colour = "")
ggsave("Brazil_graph_annual_costs(smoothed).jpeg", 
       width = 25, height = 16, units = "cm")




















#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#                                   Cumulative epi & cost  per population group#
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#####################################################################################################################
#                                   1A. Cumulative PT & TB cost per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_strategy_cost_cumulative<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$C_baseline_all_DOT_SAT,
  Baseline_HIV_neg=Brazil_baseline$C_baseline_hiv_neg_DOT_SAT,
  Baseline_PLHIV=Brazil_baseline$C_baseline_hiv_DOT_SAT,
  "Expanded 6H"=Brazil_scaleup$C_scaleup_all_DOT_SAT,
  "Expanded 6H_neg"=Brazil_scaleup$C_scaleup_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=Brazil_scaleup$C_scaleup_hiv_DOT_SAT,
  "Minimal regimen"=Brazil_minimal$C_minimal_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=Brazil_minimal$C_minimal_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=Brazil_minimal$C_minimal_hiv_DOT_SAT,
  "Optimal regimen"=Brazil_optimal$C_optimal_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=Brazil_optimal$C_optimal_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=Brazil_optimal$C_optimal_hiv_DOT_SAT)

Brazil_cost_pop_graph_cumulative<-melt(Brazil_strategy_cost_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
Brazil_cost_pop_graph_cumulative$cum_cost<-ave(Brazil_cost_pop_graph_cumulative$value, Brazil_cost_pop_graph_cumulative$cost_pop, FUN=cumsum)
# Brazil_cost_pop_graph_cumulative$Year<- as.Date(ISOdate(Brazil_cost_pop_graph_cumulative$Year,1,1) ) #set up date variable
Brazil_cost_pop_graph_cumulative$strategy = ifelse(grepl("Baseline", Brazil_cost_pop_graph_cumulative$cost_pop), "_baseline",
                                               ifelse(grepl("6H", Brazil_cost_pop_graph_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", Brazil_cost_pop_graph_cumulative$cost_pop), "minimal","Optimal")))
Brazil_cost_pop_graph_cumulative$population = ifelse(!grepl("_", Brazil_cost_pop_graph_cumulative$cost_pop), "All treatment candidates",
                                                 ifelse(grepl("PLHIV", Brazil_cost_pop_graph_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_cost_pop_graph_cumulative$cost_pop = chartr("."," ",Brazil_cost_pop_graph_cumulative$cost_pop)

#####################################################################################################################

Brazil_cost_cumulat<-aggregate(x=Brazil_cost_pop_graph_cumulative$value, by=list(Runs=Brazil_cost_pop_graph_cumulative$Runs,cost_pop=Brazil_cost_pop_graph_cumulative$cost_pop,strategy=Brazil_cost_pop_graph_cumulative$strategy,population=Brazil_cost_pop_graph_cumulative$population), FUN=sum)
#cumulative cost graph
Brazil_cost_cumulat_graph <- ggplot(data=Brazil_cost_cumulat, aes(cost_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative cost (US$ Billions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("1A. Brazil_graph_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_cost_table<-Brazil_cost_cumulat %>% 
  group_by(cost_pop, strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_cost_table, file = "1A. Brazil cumulative cost output tables.csv")



#####################################################################################################################
#                                   1B. Cumulative TB cost per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_strategy_cost_TB_cumulative<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$C_TB_all_DOT_SAT,
  Baseline_HIV_neg=Brazil_baseline$C_TB_hiv_neg_DOT_SAT,
  Baseline_PLHIV=Brazil_baseline$C_TB_hiv_DOT_SAT,
  "Expanded 6H"=Brazil_scaleup$C_TB_all_DOT_SAT,
  "Expanded 6H_neg"=Brazil_scaleup$C_TB_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=Brazil_scaleup$C_TB_hiv_DOT_SAT,
  "Minimal regimen"=Brazil_minimal$C_TB_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=Brazil_minimal$C_TB_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=Brazil_minimal$C_TB_hiv_DOT_SAT,
  "Optimal regimen"=Brazil_optimal$C_TB_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=Brazil_optimal$C_TB_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=Brazil_optimal$C_TB_hiv_DOT_SAT)

Brazil_cost_pop_graph_TB_cumulative<-melt(Brazil_strategy_cost_TB_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
Brazil_cost_pop_graph_TB_cumulative$cum_cost<-ave(Brazil_cost_pop_graph_TB_cumulative$value, Brazil_cost_pop_graph_TB_cumulative$cost_pop, FUN=cumsum)
# Brazil_cost_pop_graph_TB_cumulative$Year<- as.Date(ISOdate(Brazil_cost_pop_graph_TB_cumulative$Year,1,1) ) #set up date variable
Brazil_cost_pop_graph_TB_cumulative$strategy = ifelse(grepl("Baseline", Brazil_cost_pop_graph_TB_cumulative$cost_pop), "_baseline",
                                                  ifelse(grepl("6H", Brazil_cost_pop_graph_TB_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", Brazil_cost_pop_graph_TB_cumulative$cost_pop), "minimal","Optimal")))
Brazil_cost_pop_graph_TB_cumulative$population = ifelse(!grepl("_", Brazil_cost_pop_graph_TB_cumulative$cost_pop), "All treatment candidates",
                                                    ifelse(grepl("PLHIV", Brazil_cost_pop_graph_TB_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_cost_pop_graph_TB_cumulative$cost_pop = chartr("."," ",Brazil_cost_pop_graph_TB_cumulative$cost_pop)

#####################################################################################################################

Brazil_cost_TB_cumulat<-aggregate(x=Brazil_cost_pop_graph_TB_cumulative$value, by=list(Runs=Brazil_cost_pop_graph_TB_cumulative$Runs,cost_pop=Brazil_cost_pop_graph_TB_cumulative$cost_pop,strategy=Brazil_cost_pop_graph_TB_cumulative$strategy,population=Brazil_cost_pop_graph_TB_cumulative$population), FUN=sum)
#cumulative cost graph
Brazil_cost_TB_cumulat_graph <- ggplot(data=Brazil_cost_TB_cumulat, aes(cost_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cost of diagnosis and treatment of TB (US$ Billions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("1B. Brazil_graph_TB_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_cost_table<-Brazil_cost_TB_cumulat %>% 
  group_by(cost_pop, strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_cost_table, file = "1B. Brazil cumulative cost TB dig&tx output tables.csv")



#####################################################################################################################
#                                   1C. Cumulative RR-TB cost per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_strategy_cost_MDRTB_cumulative<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$C_MDRTB_all_DOT_SAT,
  Baseline_HIV_neg=Brazil_baseline$C_MDRTB_hiv_neg_DOT_SAT,
  Baseline_PLHIV=Brazil_baseline$C_MDRTB_hiv_DOT_SAT,
  "Expanded 6H"=Brazil_scaleup$C_MDRTB_all_DOT_SAT,
  "Expanded 6H_neg"=Brazil_scaleup$C_MDRTB_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=Brazil_scaleup$C_MDRTB_hiv_DOT_SAT,
  "Minimal regimen"=Brazil_minimal$C_MDRTB_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=Brazil_minimal$C_MDRTB_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=Brazil_minimal$C_MDRTB_hiv_DOT_SAT,
  "Optimal regimen"=Brazil_optimal$C_MDRTB_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=Brazil_optimal$C_MDRTB_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=Brazil_optimal$C_MDRTB_hiv_DOT_SAT)

Brazil_cost_pop_graph_MDRTB_cumulative<-melt(Brazil_strategy_cost_MDRTB_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
Brazil_cost_pop_graph_MDRTB_cumulative$cum_cost<-ave(Brazil_cost_pop_graph_MDRTB_cumulative$value, Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop, FUN=cumsum)
# Brazil_cost_pop_graph_MDRTB_cumulative$Year<- as.Date(ISOdate(Brazil_cost_pop_graph_MDRTB_cumulative$Year,1,1) ) #set up date variable
Brazil_cost_pop_graph_MDRTB_cumulative$strategy = ifelse(grepl("Baseline", Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop), "_baseline",
                                                     ifelse(grepl("6H", Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop), "minimal","Optimal")))
Brazil_cost_pop_graph_MDRTB_cumulative$population = ifelse(!grepl("_", Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop), "All treatment candidates",
                                                       ifelse(grepl("PLHIV", Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop = chartr("."," ",Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop)

#####################################################################################################################

Brazil_cost_MDRTB_cumulat<-aggregate(x=Brazil_cost_pop_graph_MDRTB_cumulative$value, by=list(Runs=Brazil_cost_pop_graph_MDRTB_cumulative$Runs,cost_pop=Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop,strategy=Brazil_cost_pop_graph_MDRTB_cumulative$strategy,population=Brazil_cost_pop_graph_MDRTB_cumulative$population), FUN=sum)
#cumulative cost graph
Brazil_cost_MDRTB_cumulat_graph <- ggplot(data=Brazil_cost_MDRTB_cumulat, aes(cost_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cost of diagnosis and treatment of  RR-TB (US$ Billions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("1C. Brazil_graph_RR-TB_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_rr_cost_table<-Brazil_cost_MDRTB_cumulat %>% 
  group_by(cost_pop, strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_rr_cost_table, file = "1C. Brazil cumulative cost RR-TB dig&tx output tables.csv")




#####################################################################################################################
#                                   1D. Cumulative PT - cost per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_strategy_PT_cost_cumulative<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$C_PT_all,
  Baseline_HIV_neg=Brazil_baseline$C_PT_hiv_neg,
  Baseline_PLHIV=Brazil_baseline$C_PT_hiv,
  "Expanded 6H"=Brazil_scaleup$C_PT_all,
  "Expanded 6H_neg"=Brazil_scaleup$C_PT_hiv_neg,
  "Expanded 6H_PLHIV"=Brazil_scaleup$C_PT_hiv,
  "Minimal regimen"=Brazil_minimal$C_PT_all,
  "Minimal regimen_HIV_neg"=Brazil_minimal$C_PT_hiv_neg,
  "Minimal regimen_PLHIV"=Brazil_minimal$C_PT_hiv,
  "Optimal regimen"=Brazil_optimal$C_PT_all,
  "Optimal regimen_HIV_neg"=Brazil_optimal$C_PT_hiv_neg,
  "Optimal regimen_PLHIV"=Brazil_optimal$C_PT_hiv)

Brazil_PT_cost_pop_graph_cumulative<-melt(Brazil_strategy_PT_cost_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
Brazil_PT_cost_pop_graph_cumulative$strategy = ifelse(grepl("Baseline", Brazil_PT_cost_pop_graph_cumulative$cost_pop), "_baseline",
                                                  ifelse(grepl("6H", Brazil_PT_cost_pop_graph_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", Brazil_PT_cost_pop_graph_cumulative$cost_pop), "minimal","Optimal")))
Brazil_PT_cost_pop_graph_cumulative$population = ifelse(!grepl("_", Brazil_PT_cost_pop_graph_cumulative$cost_pop), "All treatment candidates",
                                                    ifelse(grepl("PLHIV", Brazil_PT_cost_pop_graph_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_PT_cost_pop_graph_cumulative$cost_pop = chartr("."," ",Brazil_PT_cost_pop_graph_cumulative$cost_pop)

#####################################################################################################################

Brazil_PT_cost_cumulat<-aggregate(x=Brazil_PT_cost_pop_graph_cumulative$value, by=list(Runs=Brazil_PT_cost_pop_graph_cumulative$Runs,cost_pop=Brazil_PT_cost_pop_graph_cumulative$cost_pop,strategy=Brazil_PT_cost_pop_graph_cumulative$strategy,population=Brazil_PT_cost_pop_graph_cumulative$population), FUN=sum)
#cumulative cost graph
Brazil_PT_cost_cumulat_graph <- ggplot(data=Brazil_PT_cost_cumulat, aes(cost_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative PT cost (US$ Millions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("1D. Brazil_PT_graph_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_cost_table<-Brazil_PT_cost_cumulat %>% 
  group_by(cost_pop, strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_cost_table, file = "1D. Brazil cumulative PT cost output tables.csv")




# combine cumulative cost plots multiple
Brazil_cumulative_combine<-ggarrange(Brazil_PT_cost_cumulat_graph, Brazil_cost_TB_cumulat_graph, Brazil_cost_MDRTB_cumulat_graph, Brazil_cost_cumulat_graph, labels = c("PT", "TB", "RR-TB", "PT&TB"),
                                     common.legend = TRUE, legend = "bottom")
Brazil_cumulative_combine
ggsave("7. Brazil_combined_graphs_cumulative.png", 
       width = 30, height = 20, units = "cm")

##

#####################################################################################################################
#                                   1E. Cumulative DS-TB cost per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_strategy_cost_DSTB_cumulative<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$C_DSTB_all_DOT_SAT,
  Baseline_HIV_neg=Brazil_baseline$C_DSTB_hiv_neg_DOT_SAT,
  Baseline_PLHIV=Brazil_baseline$C_DSTB_hiv_DOT_SAT,
  "Expanded 6H"=Brazil_scaleup$C_DSTB_all_DOT_SAT,
  "Expanded 6H_neg"=Brazil_scaleup$C_DSTB_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=Brazil_scaleup$C_DSTB_hiv_DOT_SAT,
  "Minimal regimen"=Brazil_minimal$C_DSTB_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=Brazil_minimal$C_DSTB_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=Brazil_minimal$C_DSTB_hiv_DOT_SAT,
  "Optimal regimen"=Brazil_optimal$C_DSTB_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=Brazil_optimal$C_DSTB_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=Brazil_optimal$C_DSTB_hiv_DOT_SAT)

Brazil_cost_pop_graph_DSTB_cumulative<-melt(Brazil_strategy_cost_DSTB_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
Brazil_cost_pop_graph_DSTB_cumulative$cum_cost<-ave(Brazil_cost_pop_graph_DSTB_cumulative$value, Brazil_cost_pop_graph_DSTB_cumulative$cost_pop, FUN=cumsum)
# Brazil_cost_pop_graph_DSTB_cumulative$Year<- as.Date(ISOdate(Brazil_cost_pop_graph_DSTB_cumulative$Year,1,1) ) #set up date variable
Brazil_cost_pop_graph_DSTB_cumulative$strategy = ifelse(grepl("Baseline", Brazil_cost_pop_graph_DSTB_cumulative$cost_pop), "_baseline",
                                                         ifelse(grepl("6H", Brazil_cost_pop_graph_DSTB_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", Brazil_cost_pop_graph_DSTB_cumulative$cost_pop), "minimal","Optimal")))
Brazil_cost_pop_graph_DSTB_cumulative$population = ifelse(!grepl("_", Brazil_cost_pop_graph_DSTB_cumulative$cost_pop), "All treatment candidates",
                                                           ifelse(grepl("PLHIV", Brazil_cost_pop_graph_DSTB_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_cost_pop_graph_DSTB_cumulative$cost_pop = chartr("."," ",Brazil_cost_pop_graph_DSTB_cumulative$cost_pop)

#####################################################################################################################

Brazil_cost_DSTB_cumulat<-aggregate(x=Brazil_cost_pop_graph_DSTB_cumulative$value, by=list(Runs=Brazil_cost_pop_graph_DSTB_cumulative$Runs,cost_pop=Brazil_cost_pop_graph_DSTB_cumulative$cost_pop,strategy=Brazil_cost_pop_graph_DSTB_cumulative$strategy,population=Brazil_cost_pop_graph_DSTB_cumulative$population), FUN=sum)
#cumulative cost graph
Brazil_cost_DSTB_cumulat_graph <- ggplot(data=Brazil_cost_DSTB_cumulat, aes(cost_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cost of diagnosis and treatment of  DS-TB (US$ Billions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("1E. Brazil_graph_DS-TB_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_DS_cost_table<-Brazil_cost_DSTB_cumulat %>% 
  group_by(cost_pop, strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_DS_cost_table, file = "1E. Brazil cumulative cost DS-TB dig&tx output tables.csv")







#####################################################################################################################
#                                  2A. Cumulative overall DS-TB per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_epi_cumulative_DSTB<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$TB_cases_all-Brazil_baseline$TB_cases_MDR,
  Baseline_HIV_neg=(Brazil_baseline$TB_cases_all-Brazil_baseline$TB_cases_MDR)-(Brazil_baseline$TB_cases_HIV-Brazil_baseline$TB_cases_MDR_HIV),
  Baseline_PLHIV=Brazil_baseline$TB_cases_HIV-Brazil_baseline$TB_cases_MDR_HIV,
  "Expanded 6H"=Brazil_scaleup$TB_cases_all-Brazil_scaleup$TB_cases_MDR,
  "Expanded 6H_neg"=(Brazil_scaleup$TB_cases_all-Brazil_scaleup$TB_cases_MDR)-(Brazil_scaleup$TB_cases_HIV-Brazil_scaleup$TB_cases_MDR_HIV),
  "Expanded 6H_PLHIV"=Brazil_scaleup$TB_cases_HIV-Brazil_scaleup$TB_cases_MDR_HIV,
  "Minimal regimen"=Brazil_minimal$TB_cases_all-Brazil_minimal$TB_cases_MDR,
  "Minimal regimen_HIV_neg"=(Brazil_minimal$TB_cases_all-Brazil_minimal$TB_cases_MDR)-(Brazil_minimal$TB_cases_HIV-Brazil_minimal$TB_cases_MDR_HIV),
  "Minimal regimen_PLHIV"=Brazil_minimal$TB_cases_HIV-Brazil_minimal$TB_cases_MDR_HIV,
  "Optimal regimen"=Brazil_optimal$TB_cases_all-Brazil_optimal$TB_cases_MDR,
  "Optimal regimen_HIV_neg"=(Brazil_optimal$TB_cases_all-Brazil_optimal$TB_cases_MDR)-(Brazil_optimal$TB_cases_HIV-Brazil_optimal$TB_cases_MDR_HIV),
  "Optimal regimen_PLHIV"=Brazil_optimal$TB_cases_HIV-Brazil_optimal$TB_cases_MDR_HIV)

Brazil_epi_cumulative_DSTB_graph<-melt(Brazil_epi_cumulative_DSTB, id.vars =c("Runs"), variable.name = "epi_pop")
Brazil_epi_cumulative_DSTB_graph$strategy = ifelse(grepl("Baseline", Brazil_epi_cumulative_DSTB_graph$epi_pop), "_baseline",
                                               ifelse(grepl("6H", Brazil_epi_cumulative_DSTB_graph$epi_pop),"6H",ifelse(grepl("Minimal", Brazil_epi_cumulative_DSTB_graph$epi_pop), "minimal","Optimal")))
Brazil_epi_cumulative_DSTB_graph$population = ifelse(!grepl("_", Brazil_epi_cumulative_DSTB_graph$epi_pop), "All treatment candidates",
                                                 ifelse(grepl("PLHIV", Brazil_epi_cumulative_DSTB_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_epi_cumulative_DSTB_graph$epi_pop = chartr("."," ",Brazil_epi_cumulative_DSTB_graph$epi_pop)

#####################################################################################################################

Brazil_epi_DSTB_cumulat<-aggregate(x=Brazil_epi_cumulative_DSTB_graph$value, by=list(Runs=Brazil_epi_cumulative_DSTB_graph$Runs,epi_pop=Brazil_epi_cumulative_DSTB_graph$epi_pop,strategy=Brazil_epi_cumulative_DSTB_graph$strategy,population=Brazil_epi_cumulative_DSTB_graph$population), FUN=sum)
#cumulative epi graph
Brazil_epi_DSTB_cumulat_graph <- ggplot(data=Brazil_epi_DSTB_cumulat, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative TB cases  (DS-TB)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("2A. Epidemiological projections - DS-TB cases, Brazil, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_epi_DSTB_table<-Brazil_epi_DSTB_cumulat %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_epi_DSTB_table, file = "2A. Brazil - cumulative DS-TB cases tables.csv")






#####################################################################################################################
#                                  2B. Cumulative overall TB per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_epi_cumulative_TBall<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$TB_cases_all,
  Baseline_HIV_neg=Brazil_baseline$TB_cases_all-Brazil_baseline$TB_cases_HIV,
  Baseline_PLHIV=Brazil_baseline$TB_cases_HIV,
  "Expanded 6H"=Brazil_scaleup$TB_cases_all,
  "Expanded 6H_neg"=Brazil_scaleup$TB_cases_all-Brazil_scaleup$TB_cases_HIV,
  "Expanded 6H_PLHIV"=Brazil_scaleup$TB_cases_HIV,
  "Minimal regimen"=Brazil_minimal$TB_cases_all,
  "Minimal regimen_HIV_neg"=Brazil_minimal$TB_cases_all-Brazil_minimal$TB_cases_HIV,
  "Minimal regimen_PLHIV"=Brazil_minimal$TB_cases_HIV,
  "Optimal regimen"=Brazil_optimal$TB_cases_all,
  "Optimal regimen_HIV_neg"=Brazil_optimal$TB_cases_all-Brazil_optimal$TB_cases_HIV,
  "Optimal regimen_PLHIV"=Brazil_optimal$TB_cases_HIV)

Brazil_epi_cumulative_TBall_graph<-melt(Brazil_epi_cumulative_TBall, id.vars =c("Runs"), variable.name = "epi_pop")
Brazil_epi_cumulative_TBall_graph$strategy = ifelse(grepl("Baseline", Brazil_epi_cumulative_TBall_graph$epi_pop), "_baseline",
                                                ifelse(grepl("6H", Brazil_epi_cumulative_TBall_graph$epi_pop),"6H",ifelse(grepl("Minimal", Brazil_epi_cumulative_TBall_graph$epi_pop), "minimal","Optimal")))
Brazil_epi_cumulative_TBall_graph$population = ifelse(!grepl("_", Brazil_epi_cumulative_TBall_graph$epi_pop), "All treatment candidates",
                                                  ifelse(grepl("PLHIV", Brazil_epi_cumulative_TBall_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_epi_cumulative_TBall_graph$epi_pop = chartr("."," ",Brazil_epi_cumulative_TBall_graph$epi_pop)

#####################################################################################################################

Brazil_epi_cumulat<-aggregate(x=Brazil_epi_cumulative_TBall_graph$value, by=list(Runs=Brazil_epi_cumulative_TBall_graph$Runs,epi_pop=Brazil_epi_cumulative_TBall_graph$epi_pop,strategy=Brazil_epi_cumulative_TBall_graph$strategy,population=Brazil_epi_cumulative_TBall_graph$population), FUN=sum)
#cumulative epi graph
Brazil_epi_cumulat_graph <- ggplot(data=Brazil_epi_cumulat, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative TB cases  (DS-TB & RR-TB)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("2B. Epidemiological projections, Brazil, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_epi_table<-Brazil_epi_cumulat %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_epi_table, file = "2B. Brazil - cumulative TB cases (DS-TB & RR-TB) tables.csv")





#####################################################################################################################
#                                  2C. Cumulative RR-TB per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_epi_cumulative_DRTB<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$TB_cases_MDR,
  Baseline_HIV_neg=Brazil_baseline$TB_cases_MDR-Brazil_baseline$TB_cases_MDR_HIV,
  Baseline_PLHIV=Brazil_baseline$TB_cases_MDR_HIV,
  "Expanded 6H"=Brazil_scaleup$TB_cases_MDR,
  "Expanded 6H_neg"=Brazil_scaleup$TB_cases_MDR-Brazil_scaleup$TB_cases_MDR_HIV,
  "Expanded 6H_PLHIV"=Brazil_scaleup$TB_cases_MDR_HIV,
  "Minimal regimen"=Brazil_minimal$TB_cases_MDR,
  "Minimal regimen_HIV_neg"=Brazil_minimal$TB_cases_MDR-Brazil_minimal$TB_cases_MDR_HIV,
  "Minimal regimen_PLHIV"=Brazil_minimal$TB_cases_MDR_HIV,
  "Optimal regimen"=Brazil_optimal$TB_cases_MDR,
  "Optimal regimen_HIV_neg"=Brazil_optimal$TB_cases_MDR-Brazil_optimal$TB_cases_MDR_HIV,
  "Optimal regimen_PLHIV"=Brazil_optimal$TB_cases_MDR_HIV)

Brazil_epi_cumulative_DRTB_graph<-melt(Brazil_epi_cumulative_DRTB, id.vars =c("Runs"), variable.name = "epi_pop")
Brazil_epi_cumulative_DRTB_graph$strategy = ifelse(grepl("Baseline", Brazil_epi_cumulative_DRTB_graph$epi_pop), "_baseline",
                                               ifelse(grepl("6H", Brazil_epi_cumulative_DRTB_graph$epi_pop),"6H",ifelse(grepl("Minimal", Brazil_epi_cumulative_DRTB_graph$epi_pop), "minimal","Optimal")))
Brazil_epi_cumulative_DRTB_graph$population = ifelse(!grepl("_", Brazil_epi_cumulative_DRTB_graph$epi_pop), "All treatment candidates",
                                                 ifelse(grepl("PLHIV", Brazil_epi_cumulative_DRTB_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_epi_cumulative_DRTB_graph$epi_pop = chartr("."," ",Brazil_epi_cumulative_DRTB_graph$epi_pop)

#####################################################################################################################

Brazil_epi_cumulat_DRTB<-aggregate(x=Brazil_epi_cumulative_DRTB_graph$value, by=list(Runs=Brazil_epi_cumulative_DRTB_graph$Runs,epi_pop=Brazil_epi_cumulative_DRTB_graph$epi_pop,strategy=Brazil_epi_cumulative_DRTB_graph$strategy,population=Brazil_epi_cumulative_DRTB_graph$population), FUN=sum)
#cumulative epi graph
Brazil_epi_cumulat_graph_DRTB <- ggplot(data=Brazil_epi_cumulat_DRTB, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative RR-TB cases")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("2C. RR-TB Epidemiological projections, Brazil, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_DRTB_epi_table<-Brazil_epi_cumulat_DRTB %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_DRTB_epi_table, file = "2C. Brazil - cumulative RR-TB cases tables.csv")





#####################################################################################################################
#                                  2D. Cumulative overall PT per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_epi_cumulative_PT<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$New_PT_all,
  Baseline_HIV_neg=Brazil_baseline$New_PT_all-Brazil_baseline$New_PT_HIV,
  Baseline_PLHIV=Brazil_baseline$New_PT_HIV,
  "Expanded 6H"=Brazil_scaleup$New_PT_all,
  "Expanded 6H_neg"=Brazil_scaleup$New_PT_all-Brazil_scaleup$New_PT_HIV,
  "Expanded 6H_PLHIV"=Brazil_scaleup$New_PT_HIV,
  "Minimal regimen"=Brazil_minimal$New_PT_all,
  "Minimal regimen_HIV_neg"=Brazil_minimal$New_PT_all-Brazil_minimal$New_PT_HIV,
  "Minimal regimen_PLHIV"=Brazil_minimal$New_PT_HIV,
  "Optimal regimen"=Brazil_optimal$New_PT_all,
  "Optimal regimen_HIV_neg"=Brazil_optimal$New_PT_all-Brazil_optimal$New_PT_HIV,
  "Optimal regimen_PLHIV"=Brazil_optimal$New_PT_HIV)

Brazil_epi_cumulative_PT_graph<-melt(Brazil_epi_cumulative_PT, id.vars =c("Runs"), variable.name = "epi_pop")
Brazil_epi_cumulative_PT_graph$strategy = ifelse(grepl("Baseline", Brazil_epi_cumulative_PT_graph$epi_pop), "_baseline",
                                             ifelse(grepl("6H", Brazil_epi_cumulative_PT_graph$epi_pop),"6H",ifelse(grepl("Minimal", Brazil_epi_cumulative_PT_graph$epi_pop), "minimal","Optimal")))
Brazil_epi_cumulative_PT_graph$population = ifelse(!grepl("_", Brazil_epi_cumulative_PT_graph$epi_pop), "All treatment candidates",
                                               ifelse(grepl("PLHIV", Brazil_epi_cumulative_PT_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_epi_cumulative_PT_graph$epi_pop = chartr("."," ",Brazil_epi_cumulative_PT_graph$epi_pop)

#####################################################################################################################

Brazil_PT_epi_cumulat<-aggregate(x=Brazil_epi_cumulative_PT_graph$value, by=list(Runs=Brazil_epi_cumulative_PT_graph$Runs,epi_pop=Brazil_epi_cumulative_PT_graph$epi_pop,strategy=Brazil_epi_cumulative_PT_graph$strategy,population=Brazil_epi_cumulative_PT_graph$population), FUN=sum)
#cumulative epi graph
Brazil_PT_epi_cumulat_graph <- ggplot(data=Brazil_PT_epi_cumulat, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("Individuals on preventive therapy, 2020–2035")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("2D. Epidemiological PT projections, Brazil, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_PT_epi_table<-Brazil_PT_epi_cumulat %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_PT_epi_table, file = "2D. Brazil - Total individuals on preventive therapy.csv")






#####################################################################################################################
#                                  3. Cumulative TB deaths per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_epi_cumulative_TBdeaths<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$TB_deaths_all,
  Baseline_HIV_neg=Brazil_baseline$TB_deaths_all-Brazil_baseline$TB_deaths_HIV,
  Baseline_PLHIV=Brazil_baseline$TB_deaths_HIV,
  "Expanded 6H"=Brazil_scaleup$TB_deaths_all,
  "Expanded 6H_neg"=Brazil_scaleup$TB_deaths_all-Brazil_scaleup$TB_deaths_HIV,
  "Expanded 6H_PLHIV"=Brazil_scaleup$TB_deaths_HIV,
  "Minimal regimen"=Brazil_minimal$TB_deaths_all,
  "Minimal regimen_HIV_neg"=Brazil_minimal$TB_deaths_all-Brazil_minimal$TB_deaths_HIV,
  "Minimal regimen_PLHIV"=Brazil_minimal$TB_deaths_HIV,
  "Optimal regimen"=Brazil_optimal$TB_deaths_all,
  "Optimal regimen_HIV_neg"=Brazil_optimal$TB_deaths_all-Brazil_optimal$TB_deaths_HIV,
  "Optimal regimen_PLHIV"=Brazil_optimal$TB_deaths_HIV)

Brazil_epi_cumulative_TBdeaths_graph<-melt(Brazil_epi_cumulative_TBdeaths, id.vars =c("Runs"), variable.name = "epi_pop")
Brazil_epi_cumulative_TBdeaths_graph$strategy = ifelse(grepl("Baseline", Brazil_epi_cumulative_TBdeaths_graph$epi_pop), "_baseline",
                                                   ifelse(grepl("6H", Brazil_epi_cumulative_TBdeaths_graph$epi_pop),"6H",ifelse(grepl("Minimal", Brazil_epi_cumulative_TBdeaths_graph$epi_pop), "minimal","Optimal")))
Brazil_epi_cumulative_TBdeaths_graph$population = ifelse(!grepl("_", Brazil_epi_cumulative_TBdeaths_graph$epi_pop), "All treatment candidates",
                                                     ifelse(grepl("PLHIV", Brazil_epi_cumulative_TBdeaths_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_epi_cumulative_TBdeaths_graph$epi_pop = chartr("."," ",Brazil_epi_cumulative_TBdeaths_graph$epi_pop)

#####################################################################################################################

Brazil_epi_cumulat_TBdeaths<-aggregate(x=Brazil_epi_cumulative_TBdeaths_graph$value, by=list(Runs=Brazil_epi_cumulative_TBdeaths_graph$Runs,epi_pop=Brazil_epi_cumulative_TBdeaths_graph$epi_pop,strategy=Brazil_epi_cumulative_TBdeaths_graph$strategy,population=Brazil_epi_cumulative_TBdeaths_graph$population), FUN=sum)
#cumulative epi graph
Brazil_epi_cumulat_graph_TBdeaths <- ggplot(data=Brazil_epi_cumulat_TBdeaths, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("TB deaths")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("3. TB deaths Epidemiological projections, Brazil, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_TBdeaths_epi_table<-Brazil_epi_cumulat_TBdeaths %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_TBdeaths_epi_table, file = "3. Brazil - cumulative TB deaths tables.csv")








#####################################################################################################################
#                                  4. Cumulative DALYs per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_epi_cumulative_DALYs<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$YLD+Brazil_baseline$YLL,
  Baseline_HIV_neg=(Brazil_baseline$YLD+Brazil_baseline$YLL)-(Brazil_baseline$YLD_HIV+Brazil_baseline$YLL_HIV),
  Baseline_PLHIV=Brazil_baseline$YLD_HIV+Brazil_baseline$YLL_HIV,
  "Expanded 6H"=Brazil_scaleup$YLD+Brazil_scaleup$YLL,
  "Expanded 6H_neg"=(Brazil_scaleup$YLD+Brazil_scaleup$YLL)-(Brazil_scaleup$YLD_HIV+Brazil_scaleup$YLL_HIV),
  "Expanded 6H_PLHIV"=Brazil_scaleup$YLD_HIV+Brazil_scaleup$YLL_HIV,
  "Minimal regimen"=Brazil_minimal$YLD+Brazil_minimal$YLL,
  "Minimal regimen_HIV_neg"=(Brazil_minimal$YLD+Brazil_minimal$YLL)-(Brazil_minimal$YLD_HIV+Brazil_minimal$YLL_HIV),
  "Minimal regimen_PLHIV"=Brazil_minimal$YLD_HIV+Brazil_minimal$YLL_HIV,
  "Optimal regimen"=Brazil_optimal$YLD+Brazil_optimal$YLL,
  "Optimal regimen_HIV_neg"=(Brazil_optimal$YLD+Brazil_optimal$YLL)-(Brazil_optimal$YLD_HIV+Brazil_optimal$YLL_HIV),
  "Optimal regimen_PLHIV"=Brazil_optimal$YLD_HIV+Brazil_optimal$YLL_HIV)

Brazil_epi_cumulative_DALYs_graph<-melt(Brazil_epi_cumulative_DALYs, id.vars =c("Runs"), variable.name = "epi_pop")
Brazil_epi_cumulative_DALYs_graph$strategy = ifelse(grepl("Baseline", Brazil_epi_cumulative_DALYs_graph$epi_pop), "_baseline",
                                                ifelse(grepl("6H", Brazil_epi_cumulative_DALYs_graph$epi_pop),"6H",ifelse(grepl("Minimal", Brazil_epi_cumulative_DALYs_graph$epi_pop), "minimal","Optimal")))
Brazil_epi_cumulative_DALYs_graph$population = ifelse(!grepl("_", Brazil_epi_cumulative_DALYs_graph$epi_pop), "All treatment candidates",
                                                  ifelse(grepl("PLHIV", Brazil_epi_cumulative_DALYs_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_epi_cumulative_DALYs_graph$epi_pop = chartr("."," ",Brazil_epi_cumulative_DALYs_graph$epi_pop)

#####################################################################################################################

Brazil_epi_cumulat_DALYs<-aggregate(x=Brazil_epi_cumulative_DALYs_graph$value, by=list(Runs=Brazil_epi_cumulative_DALYs_graph$Runs,epi_pop=Brazil_epi_cumulative_DALYs_graph$epi_pop,strategy=Brazil_epi_cumulative_DALYs_graph$strategy,population=Brazil_epi_cumulative_DALYs_graph$population), FUN=sum)
#cumulative epi graph
Brazil_epi_cumulat_graph_DALYs <- ggplot(data=Brazil_epi_cumulat_DALYs, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("DALYs")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("4. DALYs - Epidemiological projections, Brazil, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_DALYs_epi_table<-Brazil_epi_cumulat_DALYs %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_DALYs_epi_table, file = "4. Brazil - cumulative DALYs tables.csv")





#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#                                     Incremental cost                                                              #
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
Brazil_strategy_cost_cumulative<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$C_baseline_all_DOT_SAT,
  Baseline_HIV_neg=Brazil_baseline$C_baseline_hiv_neg_DOT_SAT,
  Baseline_PLHIV=Brazil_baseline$C_baseline_hiv_DOT_SAT,
  "Expanded 6H"=Brazil_scaleup$C_scaleup_all_DOT_SAT,
  "Expanded 6H_neg"=Brazil_scaleup$C_scaleup_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=Brazil_scaleup$C_scaleup_hiv_DOT_SAT,
  "Minimal regimen"=Brazil_minimal$C_minimal_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=Brazil_minimal$C_minimal_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=Brazil_minimal$C_minimal_hiv_DOT_SAT,
  "Optimal regimen"=Brazil_optimal$C_optimal_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=Brazil_optimal$C_optimal_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=Brazil_optimal$C_optimal_hiv_DOT_SAT)

Brazil_cost_pop_graph_cumulative<-melt(Brazil_strategy_cost_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
Brazil_cost_pop_graph_cumulative$cum_cost<-ave(Brazil_cost_pop_graph_cumulative$value, Brazil_cost_pop_graph_cumulative$cost_pop, FUN=cumsum)
# Brazil_cost_pop_graph_cumulative$Year<- as.Date(ISOdate(Brazil_cost_pop_graph_cumulative$Year,1,1) ) #set up date variable
Brazil_cost_pop_graph_cumulative$strategy = ifelse(grepl("Baseline", Brazil_cost_pop_graph_cumulative$cost_pop), "_baseline",
                                               ifelse(grepl("6H", Brazil_cost_pop_graph_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", Brazil_cost_pop_graph_cumulative$cost_pop), "minimal","Optimal")))
Brazil_cost_pop_graph_cumulative$population = ifelse(!grepl("_", Brazil_cost_pop_graph_cumulative$cost_pop), "All treatment candidates",
                                                 ifelse(grepl("PLHIV", Brazil_cost_pop_graph_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_cost_pop_graph_cumulative$cost_pop = chartr("."," ",Brazil_cost_pop_graph_cumulative$cost_pop)

#####################################################################################################################

Brazil_cost_cumulat<-aggregate(x=Brazil_cost_pop_graph_cumulative$value, by=list(Runs=Brazil_cost_pop_graph_cumulative$Runs,cost_pop=Brazil_cost_pop_graph_cumulative$cost_pop,strategy=Brazil_cost_pop_graph_cumulative$strategy,population=Brazil_cost_pop_graph_cumulative$population), FUN=sum)
#cumulative cost graph
Brazil_cost_cumulat_graph <- ggplot(data=Brazil_cost_cumulat, aes(cost_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative cost (US$ Billions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("1A. Brazil_graph_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_cost_table<-Brazil_cost_cumulat %>% 
  group_by(cost_pop, strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_cost_table, file = "1A. Brazil cumulative cost output tables.csv")




#Incremental cost - Overall (PT + TB)
Brazil_c_baseline_vs_6h_overall <- data.frame(Runs=Brazil_baseline$Run,Brazil_strategy_cost$Expanded.6H-Brazil_strategy_cost$Baseline)
Brazil_c_baseline_vs_minimal_overall <- data.frame(Brazil_strategy_cost$Minimal.regimen-Brazil_strategy_cost$Baseline)
Brazil_c_baseline_vs_optimal_overall <- data.frame(Brazil_strategy_cost$Optimal.regimen-Brazil_strategy_cost$Baseline)
Brazil_c_6h_vs_minimal_overall <- data.frame(Brazil_strategy_cost$Minimal.regimen-Brazil_strategy_cost$Expanded.6H)
Brazil_c_6h_vs_optimal_overall <- data.frame(Brazil_strategy_cost$Optimal.regimen-Brazil_strategy_cost$Expanded.6H)
Brazil_c_minimal_vs_optimal_overall <- data.frame(Brazil_strategy_cost$Optimal.regimen-Brazil_strategy_cost$Minimal.regimen)
#####################################################################################################################################

#Incremental cost - Overall (PT)
Brazil_cPT_baseline_vs_6h_overall <- data.frame(Brazil_scaleup$C_PT_all - Brazil_baseline$C_PT_all)
Brazil_cPT_baseline_vs_minimal_overall <- data.frame(Brazil_minimal$C_PT_all - Brazil_baseline$C_PT_all)
Brazil_cPT_baseline_vs_optimal_overall <- data.frame(Brazil_optimal$C_PT_all - Brazil_baseline$C_PT_all)
Brazil_cPT_6h_vs_minimal_overall <- data.frame(Brazil_minimal$C_PT_all - Brazil_scaleup$C_PT_all)
Brazil_cPT_6h_vs_optimal_overall <- data.frame(Brazil_optimal$C_PT_all - Brazil_scaleup$C_PT_all)
Brazil_cPT_minimal_vs_optimal_overall <- data.frame(Brazil_optimal$C_PT_all - Brazil_minimal$C_PT_all)

#####################################################################################################################################
#Incremental cost - Overall (TB)
Brazil_cTB_baseline_vs_6h_overall <- data.frame(Brazil_scaleup$C_TB_all_DOT_SAT - Brazil_baseline$C_TB_all_DOT_SAT)
Brazil_cTB_baseline_vs_minimal_overall <- data.frame(Brazil_minimal$C_TB_all_DOT_SAT - Brazil_baseline$C_TB_all_DOT_SAT)
Brazil_cTB_baseline_vs_optimal_overall <- data.frame(Brazil_optimal$C_TB_all_DOT_SAT - Brazil_baseline$C_TB_all_DOT_SAT)
Brazil_cTB_6h_vs_minimal_overall <- data.frame(Brazil_minimal$C_TB_all_DOT_SAT - Brazil_scaleup$C_TB_all_DOT_SAT)
Brazil_cTB_6h_vs_optimal_overall <- data.frame(Brazil_optimal$C_TB_all_DOT_SAT - Brazil_scaleup$C_TB_all_DOT_SAT)
Brazil_cTB_minimal_vs_optimal_overall <- data.frame(Brazil_optimal$C_TB_all_DOT_SAT - Brazil_minimal$C_TB_all_DOT_SAT)
#####################################################################################################################################


#Incremental cost - HIV negative (PT + TB)
Brazil_c_baseline_vs_6h_HIV_neg <- data.frame(Brazil_strategy_cost$Expanded.6H_neg-Brazil_strategy_cost$Baseline_HIV_neg)
Brazil_c_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_strategy_cost$Minimal.regimen_HIV_neg-Brazil_strategy_cost$Baseline_HIV_neg)
Brazil_c_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_strategy_cost$Optimal.regimen_HIV_neg-Brazil_strategy_cost$Baseline_HIV_neg)
Brazil_c_6h_vs_minimal_HIV_neg <- data.frame(Brazil_strategy_cost$Minimal.regimen_HIV_neg-Brazil_strategy_cost$Expanded.6H_neg)
Brazil_c_6h_vs_optimal_HIV_neg <- data.frame(Brazil_strategy_cost$Optimal.regimen_HIV_neg-Brazil_strategy_cost$Expanded.6H_neg)
Brazil_c_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_strategy_cost$Optimal.regimen_HIV_neg-Brazil_strategy_cost$Minimal.regimen_HIV_neg)

#####################################################################################################################################

#Incremental cost - HIV negative (PT)
Brazil_cPT_baseline_vs_6h_HIV_neg <- data.frame(Brazil_scaleup$C_PT_hiv_neg - Brazil_baseline$C_PT_hiv_neg)
Brazil_cPT_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_minimal$C_PT_hiv_neg - Brazil_baseline$C_PT_hiv_neg)
Brazil_cPT_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_optimal$C_PT_hiv_neg - Brazil_baseline$C_PT_hiv_neg)
Brazil_cPT_6h_vs_minimal_HIV_neg <- data.frame(Brazil_minimal$C_PT_hiv_neg - Brazil_scaleup$C_PT_hiv_neg)
Brazil_cPT_6h_vs_optimal_HIV_neg <- data.frame(Brazil_optimal$C_PT_hiv_neg - Brazil_scaleup$C_PT_hiv_neg)
Brazil_cPT_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_optimal$C_PT_hiv_neg - Brazil_minimal$C_PT_hiv_neg)

#####################################################################################################################################


#Incremental cost - HIV negative (TB)
Brazil_cTB_baseline_vs_6h_HIV_neg <- data.frame(Brazil_scaleup$C_TB_hiv_neg_DOT_SAT - Brazil_baseline$C_TB_hiv_neg_DOT_SAT)
Brazil_cTB_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_minimal$C_TB_hiv_neg_DOT_SAT - Brazil_baseline$C_TB_hiv_neg_DOT_SAT)
Brazil_cTB_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_optimal$C_TB_hiv_neg_DOT_SAT - Brazil_baseline$C_TB_hiv_neg_DOT_SAT)
Brazil_cTB_6h_vs_minimal_HIV_neg <- data.frame(Brazil_minimal$C_TB_hiv_neg_DOT_SAT - Brazil_scaleup$C_TB_hiv_neg_DOT_SAT)
Brazil_cTB_6h_vs_optimal_HIV_neg <- data.frame(Brazil_optimal$C_TB_hiv_neg_DOT_SAT - Brazil_scaleup$C_TB_hiv_neg_DOT_SAT)
Brazil_cTB_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_optimal$C_TB_hiv_neg_DOT_SAT - Brazil_minimal$C_TB_hiv_neg_DOT_SAT)

#####################################################################################################################################


#Incremental cost - PLHIV (PT+TB)
Brazil_c_baseline_vs_6h_PLHIV <- data.frame(Brazil_strategy_cost$Expanded.6H_PLHIV-Brazil_strategy_cost$Baseline_PLHIV)
Brazil_c_baseline_vs_minimal_PLHIV <- data.frame(Brazil_strategy_cost$Minimal.regimen_PLHIV-Brazil_strategy_cost$Baseline_PLHIV)
Brazil_c_baseline_vs_optimal_PLHIV <- data.frame(Brazil_strategy_cost$Optimal.regimen_PLHIV-Brazil_strategy_cost$Baseline_PLHIV)
Brazil_c_6h_vs_minimal_PLHIV <- data.frame(Brazil_strategy_cost$Minimal.regimen_PLHIV-Brazil_strategy_cost$Expanded.6H_PLHIV)
Brazil_c_6h_vs_optimal_PLHIV <- data.frame(Brazil_strategy_cost$Optimal.regimen_PLHIV-Brazil_strategy_cost$Expanded.6H_PLHIV)
Brazil_c_minimal_vs_optimal_PLHIV <- data.frame(Brazil_strategy_cost$Optimal.regimen_PLHIV-Brazil_strategy_cost$Minimal.regimen_PLHIV)

#####################################################################################################################################


#Incremental cost - PLHIV (PT)
Brazil_cPT_baseline_vs_6h_PLHIV <- data.frame(Brazil_scaleup$C_PT_hiv - Brazil_baseline$C_PT_hiv)
Brazil_cPT_baseline_vs_minimal_PLHIV <- data.frame(Brazil_minimal$C_PT_hiv - Brazil_baseline$C_PT_hiv)
Brazil_cPT_baseline_vs_optimal_PLHIV <- data.frame(Brazil_optimal$C_PT_hiv - Brazil_baseline$C_PT_hiv)
Brazil_cPT_6h_vs_minimal_PLHIV <- data.frame(Brazil_minimal$C_PT_hiv - Brazil_scaleup$C_PT_hiv)
Brazil_cPT_6h_vs_optimal_PLHIV <- data.frame(Brazil_optimal$C_PT_hiv - Brazil_scaleup$C_PT_hiv)
Brazil_cPT_minimal_vs_optimal_PLHIV <- data.frame(Brazil_optimal$C_PT_hiv - Brazil_minimal$C_PT_hiv)

#####################################################################################################################################


#Incremental cost - PLHIV (TB)
Brazil_cTB_baseline_vs_6h_PLHIV <- data.frame(Brazil_scaleup$C_TB_hiv_DOT_SAT - Brazil_baseline$C_TB_hiv_DOT_SAT)
Brazil_cTB_baseline_vs_minimal_PLHIV <- data.frame(Brazil_minimal$C_TB_hiv_DOT_SAT - Brazil_baseline$C_TB_hiv_DOT_SAT)
Brazil_cTB_baseline_vs_optimal_PLHIV <- data.frame(Brazil_optimal$C_TB_hiv_DOT_SAT - Brazil_baseline$C_TB_hiv_DOT_SAT)
Brazil_cTB_6h_vs_minimal_PLHIV <- data.frame(Brazil_minimal$C_TB_hiv_DOT_SAT - Brazil_scaleup$C_TB_hiv_DOT_SAT)
Brazil_cTB_6h_vs_optimal_PLHIV <- data.frame(Brazil_optimal$C_TB_hiv_DOT_SAT - Brazil_scaleup$C_TB_hiv_DOT_SAT)
Brazil_cTB_minimal_vs_optimal_PLHIV <- data.frame(Brazil_optimal$C_TB_hiv_DOT_SAT - Brazil_minimal$C_TB_hiv_DOT_SAT)

#####################################################################################################################################
#####################################################################################################################################

#combine columns
Brazil_incr_cost<- data.frame(Brazil_c_baseline_vs_6h_overall,Brazil_c_baseline_vs_minimal_overall,Brazil_c_baseline_vs_optimal_overall,Brazil_c_6h_vs_minimal_overall,Brazil_c_6h_vs_optimal_overall,Brazil_c_minimal_vs_optimal_overall,  #Incremental cost - Overall (PT + TB)
                          Brazil_cPT_baseline_vs_6h_overall,Brazil_cPT_baseline_vs_minimal_overall,Brazil_cPT_baseline_vs_optimal_overall,Brazil_cPT_6h_vs_minimal_overall,Brazil_cPT_6h_vs_optimal_overall,Brazil_cPT_minimal_vs_optimal_overall,  #Incremental cost - Overall (PT)
                          Brazil_cTB_baseline_vs_6h_overall,Brazil_cTB_baseline_vs_minimal_overall,Brazil_cTB_baseline_vs_optimal_overall,Brazil_cTB_6h_vs_minimal_overall,Brazil_cTB_6h_vs_optimal_overall,Brazil_cTB_minimal_vs_optimal_overall,  #Incremental cost - Overall (TB)
                          Brazil_c_baseline_vs_6h_HIV_neg,Brazil_c_baseline_vs_minimal_HIV_neg,Brazil_c_baseline_vs_optimal_HIV_neg,Brazil_c_6h_vs_minimal_HIV_neg,Brazil_c_6h_vs_optimal_HIV_neg,Brazil_c_minimal_vs_optimal_HIV_neg,  #Incremental cost - HIV negative (PT + TB)
                          Brazil_cPT_baseline_vs_6h_HIV_neg,Brazil_cPT_baseline_vs_minimal_HIV_neg,Brazil_cPT_baseline_vs_optimal_HIV_neg,Brazil_cPT_6h_vs_minimal_HIV_neg,Brazil_cPT_6h_vs_optimal_HIV_neg,Brazil_cPT_minimal_vs_optimal_HIV_neg, #Incremental cost - HIV negative (PT)
                          Brazil_cTB_baseline_vs_6h_HIV_neg,Brazil_cTB_baseline_vs_minimal_HIV_neg,Brazil_cTB_baseline_vs_optimal_HIV_neg,Brazil_cTB_6h_vs_minimal_HIV_neg,Brazil_cTB_6h_vs_optimal_HIV_neg,Brazil_cTB_minimal_vs_optimal_HIV_neg,  #Incremental cost - HIV negative (TB)
                          Brazil_c_baseline_vs_6h_PLHIV,Brazil_c_baseline_vs_minimal_PLHIV,Brazil_c_baseline_vs_optimal_PLHIV,Brazil_c_6h_vs_minimal_PLHIV,Brazil_c_6h_vs_optimal_PLHIV,Brazil_c_minimal_vs_optimal_PLHIV,  #Incremental cost - PLHIV (PT+TB)
                          Brazil_cPT_baseline_vs_6h_PLHIV,Brazil_cPT_baseline_vs_minimal_PLHIV,Brazil_cPT_baseline_vs_optimal_PLHIV,Brazil_cPT_6h_vs_minimal_PLHIV,Brazil_cPT_6h_vs_optimal_PLHIV,Brazil_cPT_minimal_vs_optimal_PLHIV,  #Incremental cost - PLHIV (PT)
                          Brazil_cTB_baseline_vs_6h_PLHIV,Brazil_cTB_baseline_vs_minimal_PLHIV,Brazil_cTB_baseline_vs_optimal_PLHIV,Brazil_cTB_6h_vs_minimal_PLHIV,Brazil_cTB_6h_vs_optimal_PLHIV,Brazil_cTB_minimal_vs_optimal_PLHIV   #Incremental cost - PLHIV (TB)
)
colnames(Brazil_incr_cost) <- c("Runs","Brazil_c_baseline_vs_6h_overall","Brazil_c_baseline_vs_minimal_overall","Brazil_c_baseline_vs_optimal_overall","Brazil_c_6h_vs_minimal_overall","Brazil_c_6h_vs_optimal_overall","Brazil_c_minimal_vs_optimal_overall",  #Incremental cost - Overall (PT + TB)
                            "Brazil_cPT_baseline_vs_6h_overall","Brazil_cPT_baseline_vs_minimal_overall","Brazil_cPT_baseline_vs_optimal_overall","Brazil_cPT_6h_vs_minimal_overall","Brazil_cPT_6h_vs_optimal_overall","Brazil_cPT_minimal_vs_optimal_overall",  #Incremental cost - Overall (PT)
                            "Brazil_cTB_baseline_vs_6h_overall","Brazil_cTB_baseline_vs_minimal_overall","Brazil_cTB_baseline_vs_optimal_overall","Brazil_cTB_6h_vs_minimal_overall","Brazil_cTB_6h_vs_optimal_overall","Brazil_cTB_minimal_vs_optimal_overall",  #Incremental cost - Overall (TB)
                            "Brazil_c_baseline_vs_6h_HIV_neg","Brazil_c_baseline_vs_minimal_HIV_neg","Brazil_c_baseline_vs_optimal_HIV_neg","Brazil_c_6h_vs_minimal_HIV_neg","Brazil_c_6h_vs_optimal_HIV_neg","Brazil_c_minimal_vs_optimal_HIV_neg",  #Incremental cost - HIV negative (PT + TB)
                            "Brazil_cPT_baseline_vs_6h_HIV_neg","Brazil_cPT_baseline_vs_minimal_HIV_neg","Brazil_cPT_baseline_vs_optimal_HIV_neg","Brazil_cPT_6h_vs_minimal_HIV_neg","Brazil_cPT_6h_vs_optimal_HIV_neg","Brazil_cPT_minimal_vs_optimal_HIV_neg", #Incremental cost - HIV negative (PT)
                            "Brazil_cTB_baseline_vs_6h_HIV_neg","Brazil_cTB_baseline_vs_minimal_HIV_neg","Brazil_cTB_baseline_vs_optimal_HIV_neg","Brazil_cTB_6h_vs_minimal_HIV_neg","Brazil_cTB_6h_vs_optimal_HIV_neg","Brazil_cTB_minimal_vs_optimal_HIV_neg",  #Incremental cost - HIV negative (TB)
                            "Brazil_c_baseline_vs_6h_PLHIV","Brazil_c_baseline_vs_minimal_PLHIV","Brazil_c_baseline_vs_optimal_PLHIV","Brazil_c_6h_vs_minimal_PLHIV","Brazil_c_6h_vs_optimal_PLHIV","Brazil_c_minimal_vs_optimal_PLHIV",  #Incremental cost - PLHIV (PT+TB)
                            "Brazil_cPT_baseline_vs_6h_PLHIV","Brazil_cPT_baseline_vs_minimal_PLHIV","Brazil_cPT_baseline_vs_optimal_PLHIV","Brazil_cPT_6h_vs_minimal_PLHIV","Brazil_cPT_6h_vs_optimal_PLHIV","Brazil_cPT_minimal_vs_optimal_PLHIV",  #Incremental cost - PLHIV (PT)
                            "Brazil_cTB_baseline_vs_6h_PLHIV","Brazil_cTB_baseline_vs_minimal_PLHIV","Brazil_cTB_baseline_vs_optimal_PLHIV","Brazil_cTB_6h_vs_minimal_PLHIV","Brazil_cTB_6h_vs_optimal_PLHIV","Brazil_cTB_minimal_vs_optimal_PLHIV"   #Incremental cost - PLHIV (TB)
)

#####################################################################################################################################
#####################################################################################################################################
#colnames(Brazil_incr_cost) <- c("Runs","All_TBPT_baseline vs 6H","All_TBPT_baseline vs minimal","All_TBPT_baseline vs optimal","All_TBPT_6H vs minimal", "All_TBPT_6H vs optimal", "All_TBPT_minimal vs optimal",
#                                    "All_PT_baseline vs 6H","All_PT_baseline vs minimal","All_PT_baseline vs optimal","All_PT_6H vs minimal", "All_PT_6H vs optimal", "All_PT_minimal vs optimal",
#                                   "All_TB_baseline vs 6H","All_TB_baseline vs minimal","All_TB_baseline vs optimal","All_TB_6H vs minimal", "All_TB_6H vs optimal", "All_TB_minimal vs optimal",
#                                    "HHC_TBPT_baseline vs 6H","HHC_TBPT_baseline vs minimal","HHC_TBPT_baseline vs optimal","HHC_TBPT_6H vs minimal", "HHC_TBPT_6H vs optimal", "HHC_TBPT_minimal vs optimal",
#                                   "HHC_PT_baseline vs 6H","HHC_PT_baseline vs minimal","HHC_PT_baseline vs optimal","HHC_PT_6H vs minimal", "HHC_PT_6H vs optimal", "HHC_PT_minimal vs optimal",
#                                   "HHC_TB_baseline vs 6H","HHC_TB_baseline vs minimal","HHC_TB_baseline vs optimal","HHC_TB_6H vs minimal", "HHC_TB_6H vs optimal", "HHC_TB_minimal vs optimal",
#                                   "HIV_TBPT_baseline vs 6H","HIV_TBPT_baseline vs minimal","HIV_TBPT_baseline vs optimal","HIV_TBPT_6H vs minimal", "HIV_TBPT_6H vs optimal", "HIV_TBPT_minimal vs optimal",
#                                   "HIV_PT_baseline vs 6H","HIV_PT_baseline vs minimal","HIV_PT_baseline vs optimal","HIV_PT_6H vs minimal", "HIV_PT_6H vs optimal", "HIV_PT_minimal vs optimal",
#                                   "HIV_TB_baseline vs 6H","HIV_TB_baseline vs minimal","HIV_TB_baseline vs optimal","HIV_TB_6H vs minimal", "HIV_TB_6H vs optimal", "HIV_TB_minimal vs optimal")


Brazil_incr_cost_graph<-melt(Brazil_incr_cost, id.vars =c("Runs"), variable.name = "cost_pop")
Brazil_incr_cost_graph$cum_cost<-ave(Brazil_incr_cost_graph$value, Brazil_incr_cost_graph$cost_pop, FUN=cumsum)
Brazil_incr_cost_graph$comparison = ifelse(grepl("baseline_vs_6h", Brazil_incr_cost_graph$cost_pop), "1a. 6H vs baseline",
                                       ifelse(grepl("baseline_vs_minimal", Brazil_incr_cost_graph$cost_pop),"1b. Minimal vs baseline",
                                              ifelse(grepl("baseline_vs_optimal", Brazil_incr_cost_graph$cost_pop),"1c. Optimal vs baseline",
                                                     ifelse(grepl("6h_vs_minimal", Brazil_incr_cost_graph$cost_pop), "2a. Minimal vs 6H",
                                                            ifelse(grepl("6h_vs_optimal", Brazil_incr_cost_graph$cost_pop),"2b. Optimal vs 6H","3. Optimal vs minimal")))))
Brazil_incr_cost_graph$outcome = ifelse(grepl("cPT", Brazil_incr_cost_graph$cost_pop), "1.PT",
                                    ifelse(grepl("cTB", Brazil_incr_cost_graph$cost_pop),"2.TB","3.PT & TB"))
Brazil_incr_cost_graph$population = ifelse(grepl("overall", Brazil_incr_cost_graph$cost_pop), "All treatment candidates",
                                       ifelse(grepl("PLHIV", Brazil_incr_cost_graph$cost_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_incr_cost_graph$cost_pop = chartr("."," ",Brazil_incr_cost_graph$cost_pop)

#####################################################################################################################

Brazil_incr_cost<-aggregate(x=Brazil_incr_cost_graph$value, by=list(Runs=Brazil_incr_cost_graph$Runs, comparison=Brazil_incr_cost_graph$comparison,outcome=Brazil_incr_cost_graph$outcome,population=Brazil_incr_cost_graph$population), FUN=sum)
#Incremental cost graph
#Incremental cost graph
Brazil_incr_cost_limited<-Brazil_incr_cost %>%
  filter(comparison !="1c. Optimal vs baseline", comparison!="2b. Optimal vs 6H")


Brazil_incr_cost_graph <- ggplot(data=Brazil_incr_cost_limited, aes(comparison,x,color=comparison)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
  facet_wrap(~population + outcome, dir = "h", scales = "free") +
  labs(x = "population group", y = expression(paste("Incremental cost (US$ Billions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("5. Brazil_incr_cost_graph.jpeg", 
       width = 25, height = 16, units = "cm")


#Incremental cost table with ranges  
Brazil_incr_cost_table<-Brazil_incr_cost %>% 
  group_by(population,outcome, comparison) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_incr_cost_table, file = "5. Brazil incremental cost output tables.csv")




#incremental graph option 2
Brazil_incr_cost_table$fill <- ifelse(Brazil_incr_cost_table$median > 0, "#0408e0", "#c43b00")

Brazil_incremental_cost_plot2 <- ggplot(Brazil_incr_cost_table, aes(x = comparison, y = median, fill = fill)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=paste("$",round(median/1000000,0),"M")), position=position_dodge(width=0.9), vjust=-0.25, size=3, fontface="bold")+
  facet_wrap(~population + outcome, scales="fixed") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_fill_discrete(name="Incremental costs",
                      breaks=c("#0408e0", "#c43b00"),
                      labels=c("Additional cost","Savings"))+
  #scale_color_hue(labels = c("Savings","Additional cost"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none")+
  xlab("Comparison") +
  ylab("Incremental cost (in USD)") +
  labs(title="",
       subtitle="",
       caption="")+
  
  
  ggtitle("Incremental cost of TPT strategies for Brazil (2020-2035)")
ggsave("5. Brazil_incremental cost2.png", 
       width = 30, height = 20, units = "cm")


Brazil_incremental_cost_plot2

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################





#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#                                     Incremental effectiveness                                                     #
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################


#####################################################################################################################
#incremental effectiveness - all treatment candidates
#####################################################################################################################

#Incremental effectiveness - Overall (PT)
Brazil_PT_baseline_vs_6h_overall <- data.frame(Runs=Brazil_baseline$Run,Brazil_epi_cumulative_PT$Expanded.6H-Brazil_epi_cumulative_PT$Baseline)
Brazil_PT_baseline_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_PT$Minimal.regimen-Brazil_epi_cumulative_PT$Baseline)
Brazil_PT_baseline_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen-Brazil_epi_cumulative_PT$Baseline)
Brazil_PT_6h_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_PT$Minimal.regimen-Brazil_epi_cumulative_PT$Expanded.6H)
Brazil_PT_6h_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen-Brazil_epi_cumulative_PT$Expanded.6H)
Brazil_PT_minimal_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen-Brazil_epi_cumulative_PT$Minimal.regimen)
#####################################################################################################################################

#Incremental effectiveness - Overall (DS-TB)
Brazil_DSTB_baseline_vs_6h_overall <- data.frame(Brazil_epi_cumulative_DSTB$Expanded.6H-Brazil_epi_cumulative_DSTB$Baseline)
Brazil_DSTB_baseline_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_DSTB$Minimal.regimen-Brazil_epi_cumulative_DSTB$Baseline)
Brazil_DSTB_baseline_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen-Brazil_epi_cumulative_DSTB$Baseline)
Brazil_DSTB_6h_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_DSTB$Minimal.regimen-Brazil_epi_cumulative_DSTB$Expanded.6H)
Brazil_DSTB_6h_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen-Brazil_epi_cumulative_DSTB$Expanded.6H)
Brazil_DSTB_minimal_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen-Brazil_epi_cumulative_DSTB$Minimal.regimen)

#####################################################################################################################################
#Incremental effectiveness - Overall (RR-TB)
Brazil_DRTB_baseline_vs_6h_overall <- data.frame(Brazil_epi_cumulative_DRTB$Expanded.6H-Brazil_epi_cumulative_DRTB$Baseline)
Brazil_DRTB_baseline_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_DRTB$Minimal.regimen-Brazil_epi_cumulative_DRTB$Baseline)
Brazil_DRTB_baseline_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen-Brazil_epi_cumulative_DRTB$Baseline)
Brazil_DRTB_6h_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_DRTB$Minimal.regimen-Brazil_epi_cumulative_DRTB$Expanded.6H)
Brazil_DRTB_6h_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen-Brazil_epi_cumulative_DRTB$Expanded.6H)
Brazil_DRTB_minimal_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen-Brazil_epi_cumulative_DRTB$Minimal.regimen)

#####################################################################################################################################
#Incremental effectiveness - Overall (TB cases (DS-TB & RR-TB))
Brazil_TBall_baseline_vs_6h_overall <- data.frame(Brazil_epi_cumulative_TBall$Expanded.6H-Brazil_epi_cumulative_TBall$Baseline)
Brazil_TBall_baseline_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_TBall$Minimal.regimen-Brazil_epi_cumulative_TBall$Baseline)
Brazil_TBall_baseline_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen-Brazil_epi_cumulative_TBall$Baseline)
Brazil_TBall_6h_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_TBall$Minimal.regimen-Brazil_epi_cumulative_TBall$Expanded.6H)
Brazil_TBall_6h_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen-Brazil_epi_cumulative_TBall$Expanded.6H)
Brazil_TBall_minimal_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen-Brazil_epi_cumulative_TBall$Minimal.regimen)

#####################################################################################################################################
#Incremental effectiveness - Overall (TB deaths)
Brazil_TBdeaths_baseline_vs_6h_overall <- data.frame(Brazil_epi_cumulative_TBdeaths$Expanded.6H-Brazil_epi_cumulative_TBdeaths$Baseline)
Brazil_TBdeaths_baseline_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_TBdeaths$Minimal.regimen-Brazil_epi_cumulative_TBdeaths$Baseline)
Brazil_TBdeaths_baseline_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen-Brazil_epi_cumulative_TBdeaths$Baseline)
Brazil_TBdeaths_6h_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_TBdeaths$Minimal.regimen-Brazil_epi_cumulative_TBdeaths$Expanded.6H)
Brazil_TBdeaths_6h_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen-Brazil_epi_cumulative_TBdeaths$Expanded.6H)
Brazil_TBdeaths_minimal_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen-Brazil_epi_cumulative_TBdeaths$Minimal.regimen)

#####################################################################################################################################
#Incremental effectiveness - Overall (DALYs)
Brazil_DALYs_baseline_vs_6h_overall <- data.frame(Brazil_epi_cumulative_DALYs$Expanded.6H-Brazil_epi_cumulative_DALYs$Baseline)
Brazil_DALYs_baseline_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_DALYs$Minimal.regimen-Brazil_epi_cumulative_DALYs$Baseline)
Brazil_DALYs_baseline_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen-Brazil_epi_cumulative_DALYs$Baseline)
Brazil_DALYs_6h_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_DALYs$Minimal.regimen-Brazil_epi_cumulative_DALYs$Expanded.6H)
Brazil_DALYs_6h_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen-Brazil_epi_cumulative_DALYs$Expanded.6H)
Brazil_DALYs_minimal_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen-Brazil_epi_cumulative_DALYs$Minimal.regimen)


#####################################################################################################################
#incremental effectiveness - Household contacts (HIV-negative)
#####################################################################################################################

#Incremental effectiveness - HIV negative (PT)
Brazil_PT_baseline_vs_6h_HIV_neg <- data.frame(Brazil_epi_cumulative_PT$Expanded.6H_neg-Brazil_epi_cumulative_PT$Baseline_HIV_neg)
Brazil_PT_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_PT$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_PT$Baseline_HIV_neg)
Brazil_PT_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_PT$Baseline_HIV_neg)
Brazil_PT_6h_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_PT$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_PT$Expanded.6H_neg)
Brazil_PT_6h_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_PT$Expanded.6H_neg)
Brazil_PT_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_PT$Minimal.regimen_HIV_neg)
#####################################################################################################################################

#Incremental effectiveness - HIV negative (DS-TB)
Brazil_DSTB_baseline_vs_6h_HIV_neg <- data.frame(Brazil_epi_cumulative_DSTB$Expanded.6H_neg-Brazil_epi_cumulative_DSTB$Baseline_HIV_neg)
Brazil_DSTB_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DSTB$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_DSTB$Baseline_HIV_neg)
Brazil_DSTB_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DSTB$Baseline_HIV_neg)
Brazil_DSTB_6h_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DSTB$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_DSTB$Expanded.6H_neg)
Brazil_DSTB_6h_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DSTB$Expanded.6H_neg)
Brazil_DSTB_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DSTB$Minimal.regimen_HIV_neg)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (RR-TB)
Brazil_DRTB_baseline_vs_6h_HIV_neg <- data.frame(Brazil_epi_cumulative_DRTB$Expanded.6H_neg-Brazil_epi_cumulative_DRTB$Baseline_HIV_neg)
Brazil_DRTB_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DRTB$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_DRTB$Baseline_HIV_neg)
Brazil_DRTB_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DRTB$Baseline_HIV_neg)
Brazil_DRTB_6h_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DRTB$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_DRTB$Expanded.6H_neg)
Brazil_DRTB_6h_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DRTB$Expanded.6H_neg)
Brazil_DRTB_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DRTB$Minimal.regimen_HIV_neg)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (TB cases (DS-TB & RR-TB))
Brazil_TBall_baseline_vs_6h_HIV_neg <- data.frame(Brazil_epi_cumulative_TBall$Expanded.6H_neg-Brazil_epi_cumulative_TBall$Baseline_HIV_neg)
Brazil_TBall_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBall$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_TBall$Baseline_HIV_neg)
Brazil_TBall_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_TBall$Baseline_HIV_neg)
Brazil_TBall_6h_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBall$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_TBall$Expanded.6H_neg)
Brazil_TBall_6h_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_TBall$Expanded.6H_neg)
Brazil_TBall_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_TBall$Minimal.regimen_HIV_neg)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (TB deaths)
Brazil_TBdeaths_baseline_vs_6h_HIV_neg <- data.frame(Brazil_epi_cumulative_TBdeaths$Expanded.6H_neg-Brazil_epi_cumulative_TBdeaths$Baseline_HIV_neg)
Brazil_TBdeaths_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBdeaths$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_TBdeaths$Baseline_HIV_neg)
Brazil_TBdeaths_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_TBdeaths$Baseline_HIV_neg)
Brazil_TBdeaths_6h_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBdeaths$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_TBdeaths$Expanded.6H_neg)
Brazil_TBdeaths_6h_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_TBdeaths$Expanded.6H_neg)
Brazil_TBdeaths_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_TBdeaths$Minimal.regimen_HIV_neg)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (DALYs)
Brazil_DALYs_baseline_vs_6h_HIV_neg <- data.frame(Brazil_epi_cumulative_DALYs$Expanded.6H_neg-Brazil_epi_cumulative_DALYs$Baseline_HIV_neg)
Brazil_DALYs_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DALYs$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_DALYs$Baseline_HIV_neg)
Brazil_DALYs_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DALYs$Baseline_HIV_neg)
Brazil_DALYs_6h_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DALYs$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_DALYs$Expanded.6H_neg)
Brazil_DALYs_6h_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DALYs$Expanded.6H_neg)
Brazil_DALYs_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DALYs$Minimal.regimen_HIV_neg)



#####################################################################################################################
#incremental effectiveness - PLHIV
#####################################################################################################################

#Incremental effectiveness - HIV negative (PT)
Brazil_PT_baseline_vs_6h_PLHIV <- data.frame(Brazil_epi_cumulative_PT$Expanded.6H_PLHIV-Brazil_epi_cumulative_PT$Baseline_PLHIV)
Brazil_PT_baseline_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_PT$Minimal.regimen_PLHIV-Brazil_epi_cumulative_PT$Baseline_PLHIV)
Brazil_PT_baseline_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen_PLHIV-Brazil_epi_cumulative_PT$Baseline_PLHIV)
Brazil_PT_6h_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_PT$Minimal.regimen_PLHIV-Brazil_epi_cumulative_PT$Expanded.6H_PLHIV)
Brazil_PT_6h_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen_PLHIV-Brazil_epi_cumulative_PT$Expanded.6H_PLHIV)
Brazil_PT_minimal_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen_PLHIV-Brazil_epi_cumulative_PT$Minimal.regimen_PLHIV)
#####################################################################################################################################

#Incremental effectiveness - HIV negative (DS-TB)
Brazil_DSTB_baseline_vs_6h_PLHIV <- data.frame(Brazil_epi_cumulative_DSTB$Expanded.6H_PLHIV-Brazil_epi_cumulative_DSTB$Baseline_PLHIV)
Brazil_DSTB_baseline_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_DSTB$Minimal.regimen_PLHIV-Brazil_epi_cumulative_DSTB$Baseline_PLHIV)
Brazil_DSTB_baseline_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DSTB$Baseline_PLHIV)
Brazil_DSTB_6h_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_DSTB$Minimal.regimen_PLHIV-Brazil_epi_cumulative_DSTB$Expanded.6H_PLHIV)
Brazil_DSTB_6h_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DSTB$Expanded.6H_PLHIV)
Brazil_DSTB_minimal_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DSTB$Minimal.regimen_PLHIV)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (RR-TB)
Brazil_DRTB_baseline_vs_6h_PLHIV <- data.frame(Brazil_epi_cumulative_DRTB$Expanded.6H_PLHIV-Brazil_epi_cumulative_DRTB$Baseline_PLHIV)
Brazil_DRTB_baseline_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_DRTB$Minimal.regimen_PLHIV-Brazil_epi_cumulative_DRTB$Baseline_PLHIV)
Brazil_DRTB_baseline_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DRTB$Baseline_PLHIV)
Brazil_DRTB_6h_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_DRTB$Minimal.regimen_PLHIV-Brazil_epi_cumulative_DRTB$Expanded.6H_PLHIV)
Brazil_DRTB_6h_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DRTB$Expanded.6H_PLHIV)
Brazil_DRTB_minimal_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DRTB$Minimal.regimen_PLHIV)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (TB cases (DS-TB & RR-TB))
Brazil_TBall_baseline_vs_6h_PLHIV <- data.frame(Brazil_epi_cumulative_TBall$Expanded.6H_PLHIV-Brazil_epi_cumulative_TBall$Baseline_PLHIV)
Brazil_TBall_baseline_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBall$Minimal.regimen_PLHIV-Brazil_epi_cumulative_TBall$Baseline_PLHIV)
Brazil_TBall_baseline_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen_PLHIV-Brazil_epi_cumulative_TBall$Baseline_PLHIV)
Brazil_TBall_6h_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBall$Minimal.regimen_PLHIV-Brazil_epi_cumulative_TBall$Expanded.6H_PLHIV)
Brazil_TBall_6h_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen_PLHIV-Brazil_epi_cumulative_TBall$Expanded.6H_PLHIV)
Brazil_TBall_minimal_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen_PLHIV-Brazil_epi_cumulative_TBall$Minimal.regimen_PLHIV)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (TB deaths)
Brazil_TBdeaths_baseline_vs_6h_PLHIV <- data.frame(Brazil_epi_cumulative_TBdeaths$Expanded.6H_PLHIV-Brazil_epi_cumulative_TBdeaths$Baseline_PLHIV)
Brazil_TBdeaths_baseline_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBdeaths$Minimal.regimen_PLHIV-Brazil_epi_cumulative_TBdeaths$Baseline_PLHIV)
Brazil_TBdeaths_baseline_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen_PLHIV-Brazil_epi_cumulative_TBdeaths$Baseline_PLHIV)
Brazil_TBdeaths_6h_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBdeaths$Minimal.regimen_PLHIV-Brazil_epi_cumulative_TBdeaths$Expanded.6H_PLHIV)
Brazil_TBdeaths_6h_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen_PLHIV-Brazil_epi_cumulative_TBdeaths$Expanded.6H_PLHIV)
Brazil_TBdeaths_minimal_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen_PLHIV-Brazil_epi_cumulative_TBdeaths$Minimal.regimen_PLHIV)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (DALYs)
Brazil_DALYs_baseline_vs_6h_PLHIV <- data.frame(Brazil_epi_cumulative_DALYs$Expanded.6H_PLHIV-Brazil_epi_cumulative_DALYs$Baseline_PLHIV)
Brazil_DALYs_baseline_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_DALYs$Minimal.regimen_PLHIV-Brazil_epi_cumulative_DALYs$Baseline_PLHIV)
Brazil_DALYs_baseline_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DALYs$Baseline_PLHIV)
Brazil_DALYs_6h_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_DALYs$Minimal.regimen_PLHIV-Brazil_epi_cumulative_DALYs$Expanded.6H_PLHIV)
Brazil_DALYs_6h_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DALYs$Expanded.6H_PLHIV)
Brazil_DALYs_minimal_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DALYs$Minimal.regimen_PLHIV)



#####################################################################################################################################

#####################################################################################################################################
#####################################################################################################################################

#combine columns
Brazil_incr_effectiveness<-data.frame(Brazil_PT_baseline_vs_6h_overall,Brazil_PT_baseline_vs_minimal_overall,Brazil_PT_baseline_vs_optimal_overall,Brazil_PT_6h_vs_minimal_overall,Brazil_PT_6h_vs_optimal_overall,Brazil_PT_minimal_vs_optimal_overall,
                                  Brazil_PT_baseline_vs_6h_HIV_neg,Brazil_PT_baseline_vs_minimal_HIV_neg,Brazil_PT_baseline_vs_optimal_HIV_neg,Brazil_PT_6h_vs_minimal_HIV_neg,Brazil_PT_6h_vs_optimal_HIV_neg,Brazil_PT_minimal_vs_optimal_HIV_neg,
                                  Brazil_PT_baseline_vs_6h_PLHIV,
                                  Brazil_PT_baseline_vs_minimal_PLHIV,
                                  Brazil_PT_baseline_vs_optimal_PLHIV,
                                  Brazil_PT_6h_vs_minimal_PLHIV,
                                  Brazil_PT_6h_vs_optimal_PLHIV,
                                  Brazil_PT_minimal_vs_optimal_PLHIV,
                                  Brazil_DSTB_baseline_vs_6h_overall,
                                  Brazil_DSTB_baseline_vs_minimal_overall,
                                  Brazil_DSTB_baseline_vs_optimal_overall,
                                  Brazil_DSTB_6h_vs_minimal_overall,
                                  Brazil_DSTB_6h_vs_optimal_overall,
                                  Brazil_DSTB_minimal_vs_optimal_overall,
                                  Brazil_DSTB_baseline_vs_6h_HIV_neg,
                                  Brazil_DSTB_baseline_vs_minimal_HIV_neg,
                                  Brazil_DSTB_baseline_vs_optimal_HIV_neg,
                                  Brazil_DSTB_6h_vs_minimal_HIV_neg,
                                  Brazil_DSTB_6h_vs_optimal_HIV_neg,
                                  Brazil_DSTB_minimal_vs_optimal_HIV_neg,
                                  Brazil_DSTB_baseline_vs_6h_PLHIV,
                                  Brazil_DSTB_baseline_vs_minimal_PLHIV,
                                  Brazil_DSTB_baseline_vs_optimal_PLHIV,
                                  Brazil_DSTB_6h_vs_minimal_PLHIV,
                                  Brazil_DSTB_6h_vs_optimal_PLHIV,
                                  Brazil_DSTB_minimal_vs_optimal_PLHIV,
                                  Brazil_DRTB_baseline_vs_6h_overall,
                                  Brazil_DRTB_baseline_vs_minimal_overall,
                                  Brazil_DRTB_baseline_vs_optimal_overall,
                                  Brazil_DRTB_6h_vs_minimal_overall,
                                  Brazil_DRTB_6h_vs_optimal_overall,
                                  Brazil_DRTB_minimal_vs_optimal_overall,
                                  Brazil_DRTB_baseline_vs_6h_HIV_neg,
                                  Brazil_DRTB_baseline_vs_minimal_HIV_neg,
                                  Brazil_DRTB_baseline_vs_optimal_HIV_neg,
                                  Brazil_DRTB_6h_vs_minimal_HIV_neg,
                                  Brazil_DRTB_6h_vs_optimal_HIV_neg,
                                  Brazil_DRTB_minimal_vs_optimal_HIV_neg,
                                  Brazil_DRTB_baseline_vs_6h_PLHIV,
                                  Brazil_DRTB_baseline_vs_minimal_PLHIV,
                                  Brazil_DRTB_baseline_vs_optimal_PLHIV,
                                  Brazil_DRTB_6h_vs_minimal_PLHIV,
                                  Brazil_DRTB_6h_vs_optimal_PLHIV,
                                  Brazil_DRTB_minimal_vs_optimal_PLHIV,
                                  Brazil_TBall_baseline_vs_6h_overall,
                                  Brazil_TBall_baseline_vs_minimal_overall,
                                  Brazil_TBall_baseline_vs_optimal_overall,
                                  Brazil_TBall_6h_vs_minimal_overall,
                                  Brazil_TBall_6h_vs_optimal_overall,
                                  Brazil_TBall_minimal_vs_optimal_overall,
                                  Brazil_TBall_baseline_vs_6h_HIV_neg,
                                  Brazil_TBall_baseline_vs_minimal_HIV_neg,
                                  Brazil_TBall_baseline_vs_optimal_HIV_neg,
                                  Brazil_TBall_6h_vs_minimal_HIV_neg,
                                  Brazil_TBall_6h_vs_optimal_HIV_neg,
                                  Brazil_TBall_minimal_vs_optimal_HIV_neg,
                                  Brazil_TBall_baseline_vs_6h_PLHIV,
                                  Brazil_TBall_baseline_vs_minimal_PLHIV,
                                  Brazil_TBall_baseline_vs_optimal_PLHIV,
                                  Brazil_TBall_6h_vs_minimal_PLHIV,
                                  Brazil_TBall_6h_vs_optimal_PLHIV,
                                  Brazil_TBall_minimal_vs_optimal_PLHIV,
                                  Brazil_TBdeaths_baseline_vs_6h_overall,
                                  Brazil_TBdeaths_baseline_vs_minimal_overall,
                                  Brazil_TBdeaths_baseline_vs_optimal_overall,
                                  Brazil_TBdeaths_6h_vs_minimal_overall,
                                  Brazil_TBdeaths_6h_vs_optimal_overall,
                                  Brazil_TBdeaths_minimal_vs_optimal_overall,
                                  Brazil_TBdeaths_baseline_vs_6h_HIV_neg,
                                  Brazil_TBdeaths_baseline_vs_minimal_HIV_neg,
                                  Brazil_TBdeaths_baseline_vs_optimal_HIV_neg,
                                  Brazil_TBdeaths_6h_vs_minimal_HIV_neg,
                                  Brazil_TBdeaths_6h_vs_optimal_HIV_neg,
                                  Brazil_TBdeaths_minimal_vs_optimal_HIV_neg,
                                  Brazil_TBdeaths_baseline_vs_6h_PLHIV,
                                  Brazil_TBdeaths_baseline_vs_minimal_PLHIV,
                                  Brazil_TBdeaths_baseline_vs_optimal_PLHIV,
                                  Brazil_TBdeaths_6h_vs_minimal_PLHIV,
                                  Brazil_TBdeaths_6h_vs_optimal_PLHIV,
                                  Brazil_TBdeaths_minimal_vs_optimal_PLHIV,
                                  Brazil_DALYs_baseline_vs_6h_overall,
                                  Brazil_DALYs_baseline_vs_minimal_overall,
                                  Brazil_DALYs_baseline_vs_optimal_overall,
                                  Brazil_DALYs_6h_vs_minimal_overall,
                                  Brazil_DALYs_6h_vs_optimal_overall,
                                  Brazil_DALYs_minimal_vs_optimal_overall,
                                  Brazil_DALYs_baseline_vs_6h_HIV_neg,
                                  Brazil_DALYs_baseline_vs_minimal_HIV_neg,
                                  Brazil_DALYs_baseline_vs_optimal_HIV_neg,
                                  Brazil_DALYs_6h_vs_minimal_HIV_neg,
                                  Brazil_DALYs_6h_vs_optimal_HIV_neg,
                                  Brazil_DALYs_minimal_vs_optimal_HIV_neg,
                                  Brazil_DALYs_baseline_vs_6h_PLHIV,
                                  Brazil_DALYs_baseline_vs_minimal_PLHIV,
                                  Brazil_DALYs_baseline_vs_optimal_PLHIV,
                                  Brazil_DALYs_6h_vs_minimal_PLHIV,
                                  Brazil_DALYs_6h_vs_optimal_PLHIV,
                                  Brazil_DALYs_minimal_vs_optimal_PLHIV
)
colnames(Brazil_incr_effectiveness) <- c("Runs","Brazil_PT_baseline_vs_6h_overall",
                                     "Brazil_PT_baseline_vs_minimal_overall",
                                     "Brazil_PT_baseline_vs_optimal_overall",
                                     "Brazil_PT_6h_vs_minimal_overall",
                                     "Brazil_PT_6h_vs_optimal_overall",
                                     "Brazil_PT_minimal_vs_optimal_overall",
                                     "Brazil_PT_baseline_vs_6h_HIV_neg",
                                     "Brazil_PT_baseline_vs_minimal_HIV_neg",
                                     "Brazil_PT_baseline_vs_optimal_HIV_neg",
                                     "Brazil_PT_6h_vs_minimal_HIV_neg",
                                     "Brazil_PT_6h_vs_optimal_HIV_neg",
                                     "Brazil_PT_minimal_vs_optimal_HIV_neg",
                                     "Brazil_PT_baseline_vs_6h_PLHIV",
                                     "Brazil_PT_baseline_vs_minimal_PLHIV",
                                     "Brazil_PT_baseline_vs_optimal_PLHIV",
                                     "Brazil_PT_6h_vs_minimal_PLHIV",
                                     "Brazil_PT_6h_vs_optimal_PLHIV",
                                     "Brazil_PT_minimal_vs_optimal_PLHIV",
                                     "Brazil_DSTB_baseline_vs_6h_overall",
                                     "Brazil_DSTB_baseline_vs_minimal_overall",
                                     "Brazil_DSTB_baseline_vs_optimal_overall",
                                     "Brazil_DSTB_6h_vs_minimal_overall",
                                     "Brazil_DSTB_6h_vs_optimal_overall",
                                     "Brazil_DSTB_minimal_vs_optimal_overall",
                                     "Brazil_DSTB_baseline_vs_6h_HIV_neg",
                                     "Brazil_DSTB_baseline_vs_minimal_HIV_neg",
                                     "Brazil_DSTB_baseline_vs_optimal_HIV_neg",
                                     "Brazil_DSTB_6h_vs_minimal_HIV_neg",
                                     "Brazil_DSTB_6h_vs_optimal_HIV_neg",
                                     "Brazil_DSTB_minimal_vs_optimal_HIV_neg",
                                     "Brazil_DSTB_baseline_vs_6h_PLHIV",
                                     "Brazil_DSTB_baseline_vs_minimal_PLHIV",
                                     "Brazil_DSTB_baseline_vs_optimal_PLHIV",
                                     "Brazil_DSTB_6h_vs_minimal_PLHIV",
                                     "Brazil_DSTB_6h_vs_optimal_PLHIV",
                                     "Brazil_DSTB_minimal_vs_optimal_PLHIV",
                                     "Brazil_DRTB_baseline_vs_6h_overall",
                                     "Brazil_DRTB_baseline_vs_minimal_overall",
                                     "Brazil_DRTB_baseline_vs_optimal_overall",
                                     "Brazil_DRTB_6h_vs_minimal_overall",
                                     "Brazil_DRTB_6h_vs_optimal_overall",
                                     "Brazil_DRTB_minimal_vs_optimal_overall",
                                     "Brazil_DRTB_baseline_vs_6h_HIV_neg",
                                     "Brazil_DRTB_baseline_vs_minimal_HIV_neg",
                                     "Brazil_DRTB_baseline_vs_optimal_HIV_neg",
                                     "Brazil_DRTB_6h_vs_minimal_HIV_neg",
                                     "Brazil_DRTB_6h_vs_optimal_HIV_neg",
                                     "Brazil_DRTB_minimal_vs_optimal_HIV_neg",
                                     "Brazil_DRTB_baseline_vs_6h_PLHIV",
                                     "Brazil_DRTB_baseline_vs_minimal_PLHIV",
                                     "Brazil_DRTB_baseline_vs_optimal_PLHIV",
                                     "Brazil_DRTB_6h_vs_minimal_PLHIV",
                                     "Brazil_DRTB_6h_vs_optimal_PLHIV",
                                     "Brazil_DRTB_minimal_vs_optimal_PLHIV",
                                     "Brazil_TBall_baseline_vs_6h_overall",
                                     "Brazil_TBall_baseline_vs_minimal_overall",
                                     "Brazil_TBall_baseline_vs_optimal_overall",
                                     "Brazil_TBall_6h_vs_minimal_overall",
                                     "Brazil_TBall_6h_vs_optimal_overall",
                                     "Brazil_TBall_minimal_vs_optimal_overall",
                                     "Brazil_TBall_baseline_vs_6h_HIV_neg",
                                     "Brazil_TBall_baseline_vs_minimal_HIV_neg",
                                     "Brazil_TBall_baseline_vs_optimal_HIV_neg",
                                     "Brazil_TBall_6h_vs_minimal_HIV_neg",
                                     "Brazil_TBall_6h_vs_optimal_HIV_neg",
                                     "Brazil_TBall_minimal_vs_optimal_HIV_neg",
                                     "Brazil_TBall_baseline_vs_6h_PLHIV",
                                     "Brazil_TBall_baseline_vs_minimal_PLHIV",
                                     "Brazil_TBall_baseline_vs_optimal_PLHIV",
                                     "Brazil_TBall_6h_vs_minimal_PLHIV",
                                     "Brazil_TBall_6h_vs_optimal_PLHIV",
                                     "Brazil_TBall_minimal_vs_optimal_PLHIV",
                                     "Brazil_TBdeaths_baseline_vs_6h_overall",
                                     "Brazil_TBdeaths_baseline_vs_minimal_overall",
                                     "Brazil_TBdeaths_baseline_vs_optimal_overall",
                                     "Brazil_TBdeaths_6h_vs_minimal_overall",
                                     "Brazil_TBdeaths_6h_vs_optimal_overall",
                                     "Brazil_TBdeaths_minimal_vs_optimal_overall",
                                     "Brazil_TBdeaths_baseline_vs_6h_HIV_neg",
                                     "Brazil_TBdeaths_baseline_vs_minimal_HIV_neg",
                                     "Brazil_TBdeaths_baseline_vs_optimal_HIV_neg",
                                     "Brazil_TBdeaths_6h_vs_minimal_HIV_neg",
                                     "Brazil_TBdeaths_6h_vs_optimal_HIV_neg",
                                     "Brazil_TBdeaths_minimal_vs_optimal_HIV_neg",
                                     "Brazil_TBdeaths_baseline_vs_6h_PLHIV",
                                     "Brazil_TBdeaths_baseline_vs_minimal_PLHIV",
                                     "Brazil_TBdeaths_baseline_vs_optimal_PLHIV",
                                     "Brazil_TBdeaths_6h_vs_minimal_PLHIV",
                                     "Brazil_TBdeaths_6h_vs_optimal_PLHIV",
                                     "Brazil_TBdeaths_minimal_vs_optimal_PLHIV",
                                     "Brazil_DALYs_baseline_vs_6h_overall",
                                     "Brazil_DALYs_baseline_vs_minimal_overall",
                                     "Brazil_DALYs_baseline_vs_optimal_overall",
                                     "Brazil_DALYs_6h_vs_minimal_overall",
                                     "Brazil_DALYs_6h_vs_optimal_overall",
                                     "Brazil_DALYs_minimal_vs_optimal_overall",
                                     "Brazil_DALYs_baseline_vs_6h_HIV_neg",
                                     "Brazil_DALYs_baseline_vs_minimal_HIV_neg",
                                     "Brazil_DALYs_baseline_vs_optimal_HIV_neg",
                                     "Brazil_DALYs_6h_vs_minimal_HIV_neg",
                                     "Brazil_DALYs_6h_vs_optimal_HIV_neg",
                                     "Brazil_DALYs_minimal_vs_optimal_HIV_neg",
                                     "Brazil_DALYs_baseline_vs_6h_PLHIV",
                                     "Brazil_DALYs_baseline_vs_minimal_PLHIV",
                                     "Brazil_DALYs_baseline_vs_optimal_PLHIV",
                                     "Brazil_DALYs_6h_vs_minimal_PLHIV",
                                     "Brazil_DALYs_6h_vs_optimal_PLHIV",
                                     "Brazil_DALYs_minimal_vs_optimal_PLHIV")

#####################################################################################################################################
#####################################################################################################################################
#colnames(Brazil_incr_cost) <- c("Runs","All_TBPT_baseline vs 6H","All_TBPT_baseline vs minimal","All_TBPT_baseline vs optimal","All_TBPT_6H vs minimal", "All_TBPT_6H vs optimal", "All_TBPT_minimal vs optimal",
#                                    "All_PT_baseline vs 6H","All_PT_baseline vs minimal","All_PT_baseline vs optimal","All_PT_6H vs minimal", "All_PT_6H vs optimal", "All_PT_minimal vs optimal",
#                                   "All_TB_baseline vs 6H","All_TB_baseline vs minimal","All_TB_baseline vs optimal","All_TB_6H vs minimal", "All_TB_6H vs optimal", "All_TB_minimal vs optimal",
#                                    "HHC_TBPT_baseline vs 6H","HHC_TBPT_baseline vs minimal","HHC_TBPT_baseline vs optimal","HHC_TBPT_6H vs minimal", "HHC_TBPT_6H vs optimal", "HHC_TBPT_minimal vs optimal",
#                                   "HHC_PT_baseline vs 6H","HHC_PT_baseline vs minimal","HHC_PT_baseline vs optimal","HHC_PT_6H vs minimal", "HHC_PT_6H vs optimal", "HHC_PT_minimal vs optimal",
#                                   "HHC_TB_baseline vs 6H","HHC_TB_baseline vs minimal","HHC_TB_baseline vs optimal","HHC_TB_6H vs minimal", "HHC_TB_6H vs optimal", "HHC_TB_minimal vs optimal",
#                                   "HIV_TBPT_baseline vs 6H","HIV_TBPT_baseline vs minimal","HIV_TBPT_baseline vs optimal","HIV_TBPT_6H vs minimal", "HIV_TBPT_6H vs optimal", "HIV_TBPT_minimal vs optimal",
#                                   "HIV_PT_baseline vs 6H","HIV_PT_baseline vs minimal","HIV_PT_baseline vs optimal","HIV_PT_6H vs minimal", "HIV_PT_6H vs optimal", "HIV_PT_minimal vs optimal",
#                                   "HIV_TB_baseline vs 6H","HIV_TB_baseline vs minimal","HIV_TB_baseline vs optimal","HIV_TB_6H vs minimal", "HIV_TB_6H vs optimal", "HIV_TB_minimal vs optimal")


Brazil_incr_effectiveness_graph<-melt(Brazil_incr_effectiveness, id.vars =c("Runs"), variable.name = "effectiveness_pop")
Brazil_incr_effectiveness_graph$cum_effectiveness<-ave(Brazil_incr_effectiveness_graph$value, Brazil_incr_effectiveness_graph$effectiveness_pop, FUN=cumsum)
Brazil_incr_effectiveness_graph$comparison = ifelse(grepl("baseline_vs_6h", Brazil_incr_effectiveness_graph$effectiveness_pop), "1a. 6H vs baseline",
                                                ifelse(grepl("baseline_vs_minimal", Brazil_incr_effectiveness_graph$effectiveness_pop),"1b. Minimal vs baseline",
                                                       ifelse(grepl("baseline_vs_optimal", Brazil_incr_effectiveness_graph$effectiveness_pop),"1c. Optimal vs baseline",
                                                              ifelse(grepl("6h_vs_minimal", Brazil_incr_effectiveness_graph$effectiveness_pop), "2a. Minimal vs 6H",
                                                                     ifelse(grepl("6h_vs_optimal", Brazil_incr_effectiveness_graph$effectiveness_pop),"2b. Optimal vs 6H","3. Optimal vs minimal")))))
Brazil_incr_effectiveness_graph$outcome = ifelse(grepl("PT", Brazil_incr_effectiveness_graph$effectiveness_pop), "1. PT",
                                             ifelse(grepl("DSTB", Brazil_incr_effectiveness_graph$effectiveness_pop),"2. DS-TB cases",
                                                    ifelse(grepl("DRTB", Brazil_incr_effectiveness_graph$effectiveness_pop),"3. RR-TB cases",
                                                           ifelse(grepl("TBall", Brazil_incr_effectiveness_graph$effectiveness_pop),"4. All TB cases (DS-TB & RR-TB)",
                                                                  ifelse(grepl("TBdeaths", Brazil_incr_effectiveness_graph$effectiveness_pop),"5. TB deaths",
                                                                         "6. DALYs")))))
Brazil_incr_effectiveness_graph$population = ifelse(grepl("overall", Brazil_incr_effectiveness_graph$effectiveness_pop), "All treatment candidates",
                                                ifelse(grepl("PLHIV", Brazil_incr_effectiveness_graph$effectiveness_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_incr_effectiveness_graph$effectiveness_pop = chartr("."," ",Brazil_incr_effectiveness_graph$effectiveness_pop)



Brazil_incr_effectiveness<-aggregate(x=Brazil_incr_effectiveness_graph$value, by=list(Runs=Brazil_incr_effectiveness_graph$Runs, comparison=Brazil_incr_effectiveness_graph$comparison,outcome=Brazil_incr_effectiveness_graph$outcome,population=Brazil_incr_effectiveness_graph$population), FUN=sum)

#####################################################################################################################
#Incremental effectiveness table with ranges  
#####################################################################################################################
Brazil_incr_effectiveness_table<-Brazil_incr_effectiveness %>% 
  group_by(population,outcome, comparison) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_incr_effectiveness_table, file = "6. Brazil incremental effectiveness output tables.csv")


#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

Brazil_incr_effectiveness<-aggregate(x=Brazil_incr_effectiveness_graph$value, by=list(Runs=Brazil_incr_effectiveness_graph$Runs, comparison=Brazil_incr_effectiveness_graph$comparison,outcome=Brazil_incr_effectiveness_graph$outcome,population=Brazil_incr_effectiveness_graph$population), FUN=sum)
#Incremental cost graph
Brazil_incr_effectiveness_graph <- ggplot(data=Brazil_incr_effectiveness, aes(comparison,x,color=comparison)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "K", scale = 1e-3))+
  facet_wrap(~population + outcome, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("Incremental effectiveness")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("6. Brazil_incr_effectiveness_graph.jpeg", 
       width = 25, height = 16, units = "cm")


#incremental graph option 2 - OVERALL

Brazil_incr_effectiveness_table_overall<-Brazil_incr_effectiveness_table %>%
  filter(population =="All treatment candidates")
#    filter(data_type !="Epi data" | state!="TPT")

Brazil_incr_effectiveness_table_overall$fill <- ifelse(Brazil_incr_effectiveness_table_overall$median > 0, "#0408e0", "#c43b00")

Brazil_incremental_effectiveness_overall_plot <- ggplot(Brazil_incr_effectiveness_table_overall, aes(x = comparison, y = median, fill = fill)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=paste("",round(median/1000,0),"K")), position=position_dodge(width=0.9), vjust=-0.25, size=3, fontface="bold")+
  facet_wrap(~outcome, scales="fixed") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_fill_discrete(name="Incremental effectiveness",
                      breaks=c("#0408e0", "#c43b00"),
                      labels=c("Increases","Averted"))+
  #scale_color_hue(labels = c("Savings","Additional cost"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none",)+
  xlab("Comparison") +
  ylab("Incremental effectiveness") +
  labs(title="",
       subtitle="",
       caption="")+
  
  
  ggtitle("Incremental effectiveness of TPT strategies for Brazil (All treatment candidates 2020-2035)")
ggsave("6a. Brazil_incremental effectiveness (All treatment candidates).png", 
       width = 30, height = 20, units = "cm")


Brazil_incremental_effectiveness_overall_plot



#incremental graph option 2 - HHC (HIV negatives)

Brazil_incr_effectiveness_table_HIVneg<-Brazil_incr_effectiveness_table %>%
  filter(population =="HHC (HIV negatives)")

Brazil_incr_effectiveness_table_HIVneg$fill <- ifelse(Brazil_incr_effectiveness_table_HIVneg$median > 0, "#0408e0", "#c43b00")

Brazil_incremental_effectiveness_HIVneg_plot <- ggplot(Brazil_incr_effectiveness_table_HIVneg, aes(x = comparison, y = median, fill = fill)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=paste("",round(median/1000,0),"K")), position=position_dodge(width=0.9), vjust=-0.25, size=3, fontface="bold")+
  facet_wrap(~outcome, scales="fixed") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_fill_discrete(name="Incremental effectiveness",
                      breaks=c("#0408e0", "#c43b00"),
                      labels=c("Increases","Averted"))+
  #scale_color_hue(labels = c("Savings","Additional cost"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none",)+
  xlab("Comparison") +
  ylab("Incremental effectiveness") +
  labs(title="",
       subtitle="",
       caption="")+
  
  
  ggtitle("Incremental effectiveness of TPT strategies for Brazil (HHC (HIV negatives) 2020-2035)")
ggsave("6b. Brazil_incremental effectiveness (HHC (HIV negatives)).png", 
       width = 30, height = 20, units = "cm")


Brazil_incremental_effectiveness_HIVneg_plot




#incremental graph option 2 - PLHIV

Brazil_incr_effectiveness_table_PLHIV<-Brazil_incr_effectiveness_table %>%
  filter(population =="PLHIV")

Brazil_incr_effectiveness_table_PLHIV$fill <- ifelse(Brazil_incr_effectiveness_table_PLHIV$median > 0, "#0408e0", "#c43b00")

Brazil_incremental_effectiveness_PLHIV_plot <- ggplot(Brazil_incr_effectiveness_table_PLHIV, aes(x = comparison, y = median, fill = fill)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=paste("",round(median/1000,0),"K")), position=position_dodge(width=0.9), vjust=-0.25, size=3, fontface="bold")+
  facet_wrap(~outcome, scales="fixed") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_fill_discrete(name="Incremental effectiveness",
                      breaks=c("#0408e0", "#c43b00"),
                      labels=c("Increases","Averted"))+
  #scale_color_hue(labels = c("Savings","Additional cost"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none",)+
  xlab("Comparison") +
  ylab("Incremental effectiveness") +
  labs(title="",
       subtitle="",
       caption="")+
  
  
  ggtitle("Incremental effectiveness of TPT strategies for Brazil (PLHIV 2020-2035)")
ggsave("6c. Brazil_incremental effectiveness (PLHIV).png", 
       width = 30, height = 20, units = "cm")


Brazil_incremental_effectiveness_PLHIV_plot




#####################################################################################################################
#####################################################################################################################

#####################################################################################################################

#####                                 TROUBLESHOOTING                                                        #######
#####################################################################################################################
#####################################################################################################################



##### TROUBLESHOOT EPI DATA PREP DATA FOR ANALYSIS #######
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#Melt data in key packets for plotting
Brazil_strategy<-data.frame(
  Year=Brazil_baseline$Year,
  Baseline_PT=Brazil_baseline$New_PT_all,
  Baseline_TB=Brazil_baseline$TB_cases_all,
  Baseline_MDR=Brazil_baseline$TB_cases_MDR,
  "Expanded 6H_PT"=Brazil_scaleup$New_PT_all,
  "Expanded 6H_TB"=Brazil_scaleup$TB_cases_all,
  "Expanded 6H_MDR"=Brazil_scaleup$TB_cases_MDR,
  "Minimal regimen_PT"=Brazil_minimal$New_PT_all,
  "Minimal regimen_TB"=Brazil_minimal$TB_cases_all,
  "Minimal regimen_MDR"=Brazil_minimal$TB_cases_MDR,
  "Optimal regimen_PT"=Brazil_optimal$New_PT_all,
  "Optimal regimen_TB"=Brazil_optimal$TB_cases_all,
  "Optimal regimen_MDR"=Brazil_optimal$TB_cases_MDR)
Brazil_pop_graph<-melt(Brazil_strategy, id.vars =c("Year"), variable.name = "pop")
Brazil_pop_graph$cum_pop<-ave(Brazil_pop_graph$value, Brazil_pop_graph$pop, FUN=cumsum)
Brazil_pop_graph$Year<- as.Date(ISOdate(Brazil_pop_graph$Year,1,1) ) #set up date variable
Brazil_pop_graph$strategy = ifelse(grepl("Baseline", Brazil_pop_graph$pop), "_baseline",
                               ifelse(grepl("6H", Brazil_pop_graph$pop),"6H",ifelse(grepl("Minimal", Brazil_pop_graph$pop), "minimal","Optimal")))
Brazil_pop_graph$population = ifelse(grepl("_PT", Brazil_pop_graph$pop), "_Preventive therapy",
                                 ifelse(grepl("_TB", Brazil_pop_graph$pop),"TB (All)","TB(RR-TB)"))
Brazil_pop_graph$pop = chartr("."," ",Brazil_pop_graph$pop)

#####################################################################################################################
#Annual Epi outputs per strategy#
#####################################################################################################################


####################
Brazil_graph_annual <- ggplot(Brazil_pop_graph,aes(Year,value))+
  scale_color_brewer(palette = "Set1")+
  theme_classic()
# annual costs graph (not smoothed out)
Brazil_graph_annual+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_summary(fun = mean,geom = "line",size = 1,aes(colour = strategy))+
  labs(x = "Years",y = expression(paste("Individuals (100K)")),colour = "")
ggsave("Brazil_graph_annual_Epi_output.jpeg", 
       width = 25, height = 16, units = "cm")

# smoothed annual costs graph

Brazil_graph_annual+
  #  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE) +
  # geom_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE)+
  labs(x = "Years",y = expression(paste("Individuals (100K)")),colour = "")
ggsave("Brazil_graph_annual_Epi_output(smoothed).jpeg", 
       width = 25, height = 16, units = "cm")






#####################################################################################################################

##### PLOTS - South Africa #######
#####################################################################################################################
#####################################################################################################################


##### TROUBLESHOOT EPI DATA PREP DATA FOR ANALYSIS #######
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#Melt data in key packets for plotting
SA_strategy<-data.frame(
  Year=SA_baseline$Year,
  Baseline_PT=SA_baseline$New_PT_all,
  Baseline_TB=SA_baseline$TB_cases_all,
  Baseline_MDR=SA_baseline$TB_cases_MDR,
  "Expanded 6H_PT"=SA_scaleup$New_PT_all,
  "Expanded 6H_TB"=SA_scaleup$TB_cases_all,
  "Expanded 6H_MDR"=SA_scaleup$TB_cases_MDR,
  "Minimal regimen_PT"=SA_minimal$New_PT_all,
  "Minimal regimen_TB"=SA_minimal$TB_cases_all,
  "Minimal regimen_MDR"=SA_minimal$TB_cases_MDR,
  "Optimal regimen_PT"=SA_optimal$New_PT_all,
  "Optimal regimen_TB"=SA_optimal$TB_cases_all,
  "Optimal regimen_MDR"=SA_optimal$TB_cases_MDR)
SA_pop_graph<-melt(SA_strategy, id.vars =c("Year"), variable.name = "pop")
SA_pop_graph$cum_pop<-ave(SA_pop_graph$value, SA_pop_graph$pop, FUN=cumsum)
SA_pop_graph$Year<- as.Date(ISOdate(SA_pop_graph$Year,1,1) ) #set up date variable
SA_pop_graph$strategy = ifelse(grepl("Baseline", SA_pop_graph$pop), "_baseline",
                               ifelse(grepl("6H", SA_pop_graph$pop),"6H",ifelse(grepl("Minimal", SA_pop_graph$pop), "minimal","Optimal")))
SA_pop_graph$population = ifelse(grepl("_PT", SA_pop_graph$pop), "_Preventive therapy",
                                 ifelse(grepl("_TB", SA_pop_graph$pop),"TB (All)","TB(RR-TB)"))
SA_pop_graph$pop = chartr("."," ",SA_pop_graph$pop)


#####################################################################################################################
#Annual Epi outputs per strategy#
#####################################################################################################################


####################
SA_graph_annual <- ggplot(SA_pop_graph,aes(Year,value))+
  scale_color_brewer(palette = "Set1")+
  theme_classic()
# annual costs graph (not smoothed out)
SA_graph_annual+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_summary(fun = mean,geom = "line",size = 1,aes(colour = strategy))+
  labs(x = "Years",y = expression(paste("individuals (100K)")),colour = "")
ggsave("SA_graph_annual_Epi_output.jpeg", 
       width = 25, height = 16, units = "cm")

# smoothed annual costs graph

SA_graph_annual+
  #  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE) +
  # geom_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE)+
  labs(x = "Years",y = expression(paste("Individuals (100K)")),colour = "")
ggsave("SA_graph_annual_Epi_output(smoothed).jpeg", 
       width = 25, height = 16, units = "cm")







#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### TROUBLESHOOT 2 - PREP DATA FOR ANALYSIS - RR-TB EPI OUTPUT BY POP GROUP#######
#####################################################################################################################
#####################################################################################################################
############### South Africa#######
#####################################################################################################################

#Melt data in key packets for plotting
SA_strategy_DRTB<-data.frame(
  Year=SA_baseline$Year,
  Baseline_all=SA_baseline$TB_cases_MDR,
  Baseline_HIV_neg=SA_baseline$TB_cases_MDR-SA_baseline$TB_cases_MDR_HIV,
  Baseline_PLHIV=SA_baseline$TB_cases_MDR_HIV,
  "Expanded 6H_all"=SA_scaleup$TB_cases_MDR,
  "Expanded 6H_neg"=SA_scaleup$TB_cases_MDR-SA_scaleup$TB_cases_MDR_HIV,
  "Expanded 6H_PLHIV"=SA_scaleup$TB_cases_MDR_HIV,
  "Minimal regimen_all"=SA_minimal$TB_cases_MDR,
  "Minimal regimen_HIV_neg"=SA_minimal$TB_cases_MDR-SA_minimal$TB_cases_MDR_HIV,
  "Minimal regimen_PLHIV"=SA_minimal$TB_cases_MDR_HIV,
  "Optimal regimen_all"=SA_optimal$TB_cases_MDR,
  "Optimal regimen_HIV_neg"=SA_optimal$TB_cases_MDR-SA_optimal$TB_cases_MDR_HIV,
  "Optimal regimen_PLHIV"=SA_optimal$TB_cases_MDR_HIV)
SA_pop_graph_DRTB<-melt(SA_strategy_DRTB, id.vars =c("Year"), variable.name = "pop_DRTB")
SA_pop_graph_DRTB$cum_DRTB<-ave(SA_pop_graph_DRTB$value, SA_pop_graph_DRTB$pop_DRTB, FUN=cumsum)
SA_pop_graph_DRTB$Year<- as.Date(ISOdate(SA_pop_graph_DRTB$Year,1,1) ) #set up date variable
SA_pop_graph_DRTB$strategy = ifelse(grepl("Baseline", SA_pop_graph_DRTB$pop_DRTB), "_baseline",
                                    ifelse(grepl("6H", SA_pop_graph_DRTB$pop_DRTB),"6H",ifelse(grepl("Minimal", SA_pop_graph_DRTB$pop_DRTB), "minimal","Optimal")))
SA_pop_graph_DRTB$population = ifelse(grepl("_all", SA_pop_graph_DRTB$pop_DRTB), "All treatment candidates",
                                      ifelse(grepl("PLHIV", SA_pop_graph_DRTB$pop_DRTB),"PLHIV","HHC (HIV negatives)"))
SA_pop_graph_DRTB$pop_DRTB = chartr("."," ",SA_pop_graph_DRTB$pop_DRTB)

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### PLOTS #######
#####################################################################################################################
#####################################################################################################################


#####################################################################################################################
#Annual RR-TB cases per population group#
#####################################################################################################################


####################
SA_graph_annual_DRTB <- ggplot(SA_pop_graph_DRTB,aes(Year,value))+
  scale_color_brewer(palette = "Set1")+
  theme_classic()
# annual DR0TB graph (not smoothed out)
SA_graph_annual_DRTB+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_summary(fun = mean,geom = "line",size = 1,aes(colour = strategy))+
  labs(x = "Years",y = expression(paste("individuals (100K)")),colour = "")
ggsave("SA_graph_annual_DR_TB.jpeg", 
       width = 25, height = 16, units = "cm")

# smoothed annual RR-TB cases graph

SA_graph_annual_DRTB+
  #  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE) +
  # geom_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE)+
  labs(x = "Years",y = expression(paste("Individuals (100K)")),colour = "")
ggsave("SA_graph_annual_DR_TB(smoothed).jpeg", 
       width = 25, height = 16, units = "cm")



#####################################################################################################################
############### Brazil#######
#####################################################################################################################

#Melt data in key packets for plotting
Brazil_strategy_DRTB<-data.frame(
  Year=Brazil_baseline$Year,
  Baseline_all=Brazil_baseline$TB_cases_MDR,
  Baseline_HIV_neg=Brazil_baseline$TB_cases_MDR-Brazil_baseline$TB_cases_MDR_HIV,
  Baseline_PLHIV=Brazil_baseline$TB_cases_MDR_HIV,
  "Expanded 6H_all"=Brazil_scaleup$TB_cases_MDR,
  "Expanded 6H_neg"=Brazil_scaleup$TB_cases_MDR-Brazil_scaleup$TB_cases_MDR_HIV,
  "Expanded 6H_PLHIV"=Brazil_scaleup$TB_cases_MDR_HIV,
  "Minimal regimen_all"=Brazil_minimal$TB_cases_MDR,
  "Minimal regimen_HIV_neg"=Brazil_minimal$TB_cases_MDR-Brazil_minimal$TB_cases_MDR_HIV,
  "Minimal regimen_PLHIV"=Brazil_minimal$TB_cases_MDR_HIV,
  "Optimal regimen_all"=Brazil_optimal$TB_cases_MDR,
  "Optimal regimen_HIV_neg"=Brazil_optimal$TB_cases_MDR-Brazil_optimal$TB_cases_MDR_HIV,
  "Optimal regimen_PLHIV"=Brazil_optimal$TB_cases_MDR_HIV)
Brazil_pop_graph_DRTB<-melt(Brazil_strategy_DRTB, id.vars =c("Year"), variable.name = "pop_DRTB")
Brazil_pop_graph_DRTB$cum_DRTB<-ave(Brazil_pop_graph_DRTB$value, Brazil_pop_graph_DRTB$pop_DRTB, FUN=cumsum)
Brazil_pop_graph_DRTB$Year<- as.Date(ISOdate(Brazil_pop_graph_DRTB$Year,1,1) ) #set up date variable
Brazil_pop_graph_DRTB$strategy = ifelse(grepl("Baseline", Brazil_pop_graph_DRTB$pop_DRTB), "_baseline",
                                    ifelse(grepl("6H", Brazil_pop_graph_DRTB$pop_DRTB),"6H",ifelse(grepl("Minimal", Brazil_pop_graph_DRTB$pop_DRTB), "minimal","Optimal")))
Brazil_pop_graph_DRTB$population = ifelse(grepl("_all", Brazil_pop_graph_DRTB$pop_DRTB), "All treatment candidates",
                                      ifelse(grepl("PLHIV", Brazil_pop_graph_DRTB$pop_DRTB),"PLHIV","HHC (HIV negatives)"))
Brazil_pop_graph_DRTB$pop_DRTB = chartr("."," ",Brazil_pop_graph_DRTB$pop_DRTB)

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### PLOTS #######
#####################################################################################################################
#####################################################################################################################


#####################################################################################################################
#Annual RR-TB cases per population group#
#####################################################################################################################


####################
Brazil_graph_annual_DRTB <- ggplot(Brazil_pop_graph_DRTB,aes(Year,value))+
  scale_color_brewer(palette = "Set1")+
  theme_classic()
# annual DR0TB graph (not smoothed out)
Brazil_graph_annual_DRTB+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_summary(fun = mean,geom = "line",size = 1,aes(colour = strategy))+
  labs(x = "Years",y = expression(paste("individuals (100K)")),colour = "")
ggsave("Brazil_graph_annual_DR_TB.jpeg", 
       width = 25, height = 16, units = "cm")

# smoothed annual RR-TB cases graph

Brazil_graph_annual_DRTB+
  #  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE) +
  # geom_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE)+
  labs(x = "Years",y = expression(paste("Individuals (100K)")),colour = "")
ggsave("Brazil_graph_annual_DR_TB(smoothed).jpeg", 
       width = 25, height = 16, units = "cm")










#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### TROUBLESHOOT 3 - PREP DATA FOR ANALYSIS - TB EPI OUTPUT BY POP GROUP#######
#####################################################################################################################
#####################################################################################################################
############### South Africa#######
#####################################################################################################################

#Melt data in key packets for plotting
SA_strategy_TB<-data.frame(
  Year=SA_baseline$Year,
  Baseline_all=SA_baseline$TB_cases_all,
  Baseline_HIV_neg=SA_baseline$TB_cases_all-SA_baseline$TB_cases_HIV,
  Baseline_PLHIV=SA_baseline$TB_cases_HIV,
  "Expanded 6H_all"=SA_scaleup$TB_cases_all,
  "Expanded 6H_neg"=SA_scaleup$TB_cases_all-SA_scaleup$TB_cases_HIV,
  "Expanded 6H_PLHIV"=SA_scaleup$TB_cases_HIV,
  "Minimal regimen_all"=SA_minimal$TB_cases_all,
  "Minimal regimen_HIV_neg"=SA_minimal$TB_cases_all-SA_minimal$TB_cases_HIV,
  "Minimal regimen_PLHIV"=SA_minimal$TB_cases_HIV,
  "Optimal regimen_all"=SA_optimal$TB_cases_all,
  "Optimal regimen_HIV_neg"=SA_optimal$TB_cases_all-SA_optimal$TB_cases_HIV,
  "Optimal regimen_PLHIV"=SA_optimal$TB_cases_HIV)
SA_pop_graph_TB<-melt(SA_strategy_TB, id.vars =c("Year"), variable.name = "pop_TB")
SA_pop_graph_TB$cum_TB<-ave(SA_pop_graph_TB$value, SA_pop_graph_TB$pop_TB, FUN=cumsum)
SA_pop_graph_TB$Year<- as.Date(ISOdate(SA_pop_graph_TB$Year,1,1) ) #set up date variable
SA_pop_graph_TB$strategy = ifelse(grepl("Baseline", SA_pop_graph_TB$pop_TB), "_baseline",
                                    ifelse(grepl("6H", SA_pop_graph_TB$pop_TB),"6H",ifelse(grepl("Minimal", SA_pop_graph_TB$pop_TB), "minimal","Optimal")))
SA_pop_graph_TB$population = ifelse(grepl("_all", SA_pop_graph_TB$pop_TB), "All treatment candidates",
                                      ifelse(grepl("PLHIV", SA_pop_graph_TB$pop_TB),"PLHIV","HHC (HIV negatives)"))
SA_pop_graph_TB$pop_TB = chartr("."," ",SA_pop_graph_TB$pop_TB)

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### PLOTS #######
#####################################################################################################################
#####################################################################################################################


#####################################################################################################################
#Annual RR-TB cases per population group#
#####################################################################################################################


####################
SA_graph_annual_TB <- ggplot(SA_pop_graph_TB,aes(Year,value))+
  scale_color_brewer(palette = "Set1")+
  theme_classic()
# annual DR0TB graph (not smoothed out)
SA_graph_annual_TB+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_summary(fun = mean,geom = "line",size = 1,aes(colour = strategy))+
  labs(x = "Years",y = expression(paste("individuals (100K)")),colour = "")
ggsave("SA_graph_annual_TB_all.jpeg", 
       width = 25, height = 16, units = "cm")

# smoothed annual RR-TB cases graph

SA_graph_annual_TB+
  #  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE) +
  # geom_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE)+
  labs(x = "Years",y = expression(paste("Individuals (100K)")),colour = "")
ggsave("SA_graph_annual_TB_all(smoothed).jpeg", 
       width = 25, height = 16, units = "cm")



#####################################################################################################################
############### Brazil#######
#####################################################################################################################

#Melt data in key packets for plotting
Brazil_strategy_TB<-data.frame(
  Year=Brazil_baseline$Year,
  Baseline_all=Brazil_baseline$TB_cases_all,
  Baseline_HIV_neg=Brazil_baseline$TB_cases_all-Brazil_baseline$TB_cases_HIV,
  Baseline_PLHIV=Brazil_baseline$TB_cases_HIV,
  "Expanded 6H_all"=Brazil_scaleup$TB_cases_all,
  "Expanded 6H_neg"=Brazil_scaleup$TB_cases_all-Brazil_scaleup$TB_cases_HIV,
  "Expanded 6H_PLHIV"=Brazil_scaleup$TB_cases_HIV,
  "Minimal regimen_all"=Brazil_minimal$TB_cases_all,
  "Minimal regimen_HIV_neg"=Brazil_minimal$TB_cases_all-Brazil_minimal$TB_cases_HIV,
  "Minimal regimen_PLHIV"=Brazil_minimal$TB_cases_HIV,
  "Optimal regimen_all"=Brazil_optimal$TB_cases_all,
  "Optimal regimen_HIV_neg"=Brazil_optimal$TB_cases_all-Brazil_optimal$TB_cases_HIV,
  "Optimal regimen_PLHIV"=Brazil_optimal$TB_cases_HIV)
Brazil_pop_graph_TB<-melt(Brazil_strategy_TB, id.vars =c("Year"), variable.name = "pop_TB")
Brazil_pop_graph_TB$cum_TB<-ave(Brazil_pop_graph_TB$value, Brazil_pop_graph_TB$pop_TB, FUN=cumsum)
Brazil_pop_graph_TB$Year<- as.Date(ISOdate(Brazil_pop_graph_TB$Year,1,1) ) #set up date variable
Brazil_pop_graph_TB$strategy = ifelse(grepl("Baseline", Brazil_pop_graph_TB$pop_TB), "_baseline",
                                        ifelse(grepl("6H", Brazil_pop_graph_TB$pop_TB),"6H",ifelse(grepl("Minimal", Brazil_pop_graph_TB$pop_TB), "minimal","Optimal")))
Brazil_pop_graph_TB$population = ifelse(grepl("_all", Brazil_pop_graph_TB$pop_TB), "All treatment candidates",
                                          ifelse(grepl("PLHIV", Brazil_pop_graph_TB$pop_TB),"PLHIV","HHC (HIV negatives)"))
Brazil_pop_graph_TB$pop_TB = chartr("."," ",Brazil_pop_graph_TB$pop_TB)

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### PLOTS #######
#####################################################################################################################
#####################################################################################################################


#####################################################################################################################
#Annual RR-TB cases per population group#
#####################################################################################################################


####################
Brazil_graph_annual_TB <- ggplot(Brazil_pop_graph_TB,aes(Year,value))+
  scale_color_brewer(palette = "Set1")+
  theme_classic()
# annual DR0TB graph (not smoothed out)
Brazil_graph_annual_TB+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_summary(fun = mean,geom = "line",size = 1,aes(colour = strategy))+
  labs(x = "Years",y = expression(paste("individuals (100K)")),colour = "")
ggsave("Brazil_graph_annual_TB_all.jpeg", 
       width = 25, height = 16, units = "cm")

# smoothed annual RR-TB cases graph

Brazil_graph_annual_TB+
  #  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE) +
  # geom_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE)+
  labs(x = "Years",y = expression(paste("Individuals (100K)")),colour = "")
ggsave("Brazil_graph_annual_TB_all(smoothed).jpeg", 
       width = 25, height = 16, units = "cm")








#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
##########################################                               ####################################################################
########################################## DR-BARRIER 96, 97, 98 AND 99  ####################################################################
##########################################                               ####################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################



#####################################################################################################################
#                                               LOAD THE DATA                                                       #
#####################################################################################################################
#Load South African mathematical model epidemiological outputs - DR barriers
SA_DR96 <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "SA_DR96_data", col_types = "numeric", range = "A1:R16001")
SA_DR97 <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "SA_DR97_data", col_types = "numeric", range = "A1:R16001")
SA_DR98 <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "SA_DR98_data", col_types = "numeric", range = "A1:R16001")
SA_DR99 <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "SA_DR99_data", col_types = "numeric", range = "A1:R16001")
SA_DR25 <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "SA_DR25_data", col_types = "numeric", range = "A1:R16001")
SA_DR75 <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "SA_DR75_data", col_types = "numeric", range = "A1:R16001")

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### South Africa #######
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#####################################################################################################################
#                                       DR96 ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for DR96 dataset
# PT
SA_DR96$C_PT_all<-(SA_DR96$New_PT_all*(SA_costs$LTBI_DR96+SA_costs$AE_LTBI)) # Cost: PT all
SA_DR96$C_PT_hiv_neg<-(SA_DR96$New_PT_all-SA_DR96$New_PT_HIV)*(SA_costs$LTBI_DR96+SA_costs$AE_LTBI) # Cost: PT (HIV negative)
SA_DR96$C_PT_hiv<-(SA_DR96$New_PT_HIV*(SA_costs$LTBI_DR96+SA_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
SA_DR96$C_DSTB_all_DOT<-(SA_DR96$TB_cases_all-SA_DR96$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_DR96$C_DSTB_hiv_neg_DOT<-((SA_DR96$TB_cases_all-SA_DR96$TB_cases_MDR)-(SA_DR96$TB_cases_HIV-SA_DR96$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_DR96$C_DSTB_hiv_DOT<-(SA_DR96$TB_cases_HIV-SA_DR96$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
SA_DR96$C_MDRTB_all_DOT<-(SA_DR96$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_DR96$C_MDRTB_hiv_neg_DOT<-(SA_DR96$TB_cases_MDR-SA_DR96$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_DR96$C_MDRTB_hiv_DOT<-SA_DR96$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
SA_DR96$C_TB_all_DOT<-SA_DR96$C_DSTB_all_DOT+SA_DR96$C_MDRTB_all_DOT # Cost: TB (DS-TB & MRR-TB)
SA_DR96$C_TB_hiv_neg_DOT<-SA_DR96$C_DSTB_hiv_neg_DOT+SA_DR96$C_MDRTB_hiv_neg_DOT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_DR96$C_TB_hiv_DOT<-SA_DR96$C_DSTB_hiv_DOT+SA_DR96$C_MDRTB_hiv_DOT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_DR96$C_TB_all_DOT)
#sum(SA_DR96$C_TB_hiv_neg_DOT)
#sum(SA_DR96$C_TB_hiv_DOT)

#DR96 STRATEGY COST: PT & TB (DOT)
SA_DR96$C_DR96_all_DOT<-SA_DR96$C_PT_all+SA_DR96$C_TB_all_DOT #Cost: strategy all (PT+TB) DOT
SA_DR96$C_DR96_hiv_neg_DOT<-SA_DR96$C_PT_hiv_neg+SA_DR96$C_TB_hiv_neg_DOT #Cost: strategy HIV negative (PT+TB) DOT
SA_DR96$C_DR96_hiv_DOT<-SA_DR96$C_PT_hiv+SA_DR96$C_TB_hiv_DOT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       DR96 ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
SA_DR96$C_DSTB_all_DOT_SAT<-(SA_DR96$TB_cases_all-SA_DR96$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_DR96$C_DSTB_hiv_neg_DOT_SAT<-((SA_DR96$TB_cases_all-SA_DR96$TB_cases_MDR)-(SA_DR96$TB_cases_HIV-SA_DR96$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_DR96$C_DSTB_hiv_DOT_SAT<-(SA_DR96$TB_cases_HIV-SA_DR96$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
SA_DR96$C_MDRTB_all_DOT_SAT<-(SA_DR96$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_DR96$C_MDRTB_hiv_neg_DOT_SAT<-(SA_DR96$TB_cases_MDR-SA_DR96$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_DR96$C_MDRTB_hiv_DOT_SAT<-SA_DR96$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
SA_DR96$C_TB_all_DOT_SAT<-SA_DR96$C_DSTB_all_DOT_SAT+SA_DR96$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
SA_DR96$C_TB_hiv_neg_DOT_SAT<-SA_DR96$C_DSTB_hiv_neg_DOT_SAT+SA_DR96$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_DR96$C_TB_hiv_DOT_SAT<-SA_DR96$C_DSTB_hiv_DOT_SAT+SA_DR96$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_DR96$C_TB_all_DOT_SAT)
#sum(SA_DR96$C_TB_hiv_neg_DOT_SAT)
#sum(SA_DR96$C_TB_hiv_DOT_SAT)

#DR96 STRATEGY COST: PT & TB (DOT_SAT)
SA_DR96$C_DR96_all_DOT_SAT<-SA_DR96$C_PT_all+SA_DR96$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT_SAT
SA_DR96$C_DR96_hiv_neg_DOT_SAT<-SA_DR96$C_PT_hiv_neg+SA_DR96$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
SA_DR96$C_DR96_hiv_DOT_SAT<-SA_DR96$C_PT_hiv+SA_DR96$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT



#####################################################################################################################
#                                       DR97 ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for DR97 dataset
# PT
SA_DR97$C_PT_all<-(SA_DR97$New_PT_all*(SA_costs$LTBI_DR97+SA_costs$AE_LTBI)) # Cost: PT all
SA_DR97$C_PT_hiv_neg<-(SA_DR97$New_PT_all-SA_DR97$New_PT_HIV)*(SA_costs$LTBI_DR97+SA_costs$AE_LTBI) # Cost: PT (HIV negative)
SA_DR97$C_PT_hiv<-(SA_DR97$New_PT_HIV*(SA_costs$LTBI_DR97+SA_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
SA_DR97$C_DSTB_all_DOT<-(SA_DR97$TB_cases_all-SA_DR97$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_DR97$C_DSTB_hiv_neg_DOT<-((SA_DR97$TB_cases_all-SA_DR97$TB_cases_MDR)-(SA_DR97$TB_cases_HIV-SA_DR97$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_DR97$C_DSTB_hiv_DOT<-(SA_DR97$TB_cases_HIV-SA_DR97$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
SA_DR97$C_MDRTB_all_DOT<-(SA_DR97$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_DR97$C_MDRTB_hiv_neg_DOT<-(SA_DR97$TB_cases_MDR-SA_DR97$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_DR97$C_MDRTB_hiv_DOT<-SA_DR97$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
SA_DR97$C_TB_all_DOT<-SA_DR97$C_DSTB_all_DOT+SA_DR97$C_MDRTB_all_DOT # Cost: TB (DS-TB & MRR-TB)
SA_DR97$C_TB_hiv_neg_DOT<-SA_DR97$C_DSTB_hiv_neg_DOT+SA_DR97$C_MDRTB_hiv_neg_DOT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_DR97$C_TB_hiv_DOT<-SA_DR97$C_DSTB_hiv_DOT+SA_DR97$C_MDRTB_hiv_DOT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_DR97$C_TB_all_DOT)
#sum(SA_DR97$C_TB_hiv_neg_DOT)
#sum(SA_DR97$C_TB_hiv_DOT)

#DR97 STRATEGY COST: PT & TB (DOT)
SA_DR97$C_DR97_all_DOT<-SA_DR97$C_PT_all+SA_DR97$C_TB_all_DOT #Cost: strategy all (PT+TB) DOT
SA_DR97$C_DR97_hiv_neg_DOT<-SA_DR97$C_PT_hiv_neg+SA_DR97$C_TB_hiv_neg_DOT #Cost: strategy HIV negative (PT+TB) DOT
SA_DR97$C_DR97_hiv_DOT<-SA_DR97$C_PT_hiv+SA_DR97$C_TB_hiv_DOT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       DR97 ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
SA_DR97$C_DSTB_all_DOT_SAT<-(SA_DR97$TB_cases_all-SA_DR97$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_DR97$C_DSTB_hiv_neg_DOT_SAT<-((SA_DR97$TB_cases_all-SA_DR97$TB_cases_MDR)-(SA_DR97$TB_cases_HIV-SA_DR97$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_DR97$C_DSTB_hiv_DOT_SAT<-(SA_DR97$TB_cases_HIV-SA_DR97$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
SA_DR97$C_MDRTB_all_DOT_SAT<-(SA_DR97$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_DR97$C_MDRTB_hiv_neg_DOT_SAT<-(SA_DR97$TB_cases_MDR-SA_DR97$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_DR97$C_MDRTB_hiv_DOT_SAT<-SA_DR97$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
SA_DR97$C_TB_all_DOT_SAT<-SA_DR97$C_DSTB_all_DOT_SAT+SA_DR97$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
SA_DR97$C_TB_hiv_neg_DOT_SAT<-SA_DR97$C_DSTB_hiv_neg_DOT_SAT+SA_DR97$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_DR97$C_TB_hiv_DOT_SAT<-SA_DR97$C_DSTB_hiv_DOT_SAT+SA_DR97$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_DR97$C_TB_all_DOT_SAT)
#sum(SA_DR97$C_TB_hiv_neg_DOT_SAT)
#sum(SA_DR97$C_TB_hiv_DOT_SAT)

#DR97 STRATEGY COST: PT & TB (DOT_SAT)
SA_DR97$C_DR97_all_DOT_SAT<-SA_DR97$C_PT_all+SA_DR97$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT_SAT
SA_DR97$C_DR97_hiv_neg_DOT_SAT<-SA_DR97$C_PT_hiv_neg+SA_DR97$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT

SA_DR97$C_DR97_hiv_DOT_SAT<-SA_DR97$C_PT_hiv+SA_DR97$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT



#####################################################################################################################
#                                       DR98 ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for DR98 dataset
# PT
SA_DR98$C_PT_all<-(SA_DR98$New_PT_all*(SA_costs$LTBI_DR98+SA_costs$AE_LTBI)) # Cost: PT all
SA_DR98$C_PT_hiv_neg<-(SA_DR98$New_PT_all-SA_DR98$New_PT_HIV)*(SA_costs$LTBI_DR98+SA_costs$AE_LTBI) # Cost: PT (HIV negative)
SA_DR98$C_PT_hiv<-(SA_DR98$New_PT_HIV*(SA_costs$LTBI_DR98+SA_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
SA_DR98$C_DSTB_all_DOT<-(SA_DR98$TB_cases_all-SA_DR98$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_DR98$C_DSTB_hiv_neg_DOT<-((SA_DR98$TB_cases_all-SA_DR98$TB_cases_MDR)-(SA_DR98$TB_cases_HIV-SA_DR98$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_DR98$C_DSTB_hiv_DOT<-(SA_DR98$TB_cases_HIV-SA_DR98$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
SA_DR98$C_MDRTB_all_DOT<-(SA_DR98$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_DR98$C_MDRTB_hiv_neg_DOT<-(SA_DR98$TB_cases_MDR-SA_DR98$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_DR98$C_MDRTB_hiv_DOT<-SA_DR98$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
SA_DR98$C_TB_all_DOT<-SA_DR98$C_DSTB_all_DOT+SA_DR98$C_MDRTB_all_DOT # Cost: TB (DS-TB & MRR-TB)
SA_DR98$C_TB_hiv_neg_DOT<-SA_DR98$C_DSTB_hiv_neg_DOT+SA_DR98$C_MDRTB_hiv_neg_DOT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_DR98$C_TB_hiv_DOT<-SA_DR98$C_DSTB_hiv_DOT+SA_DR98$C_MDRTB_hiv_DOT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_DR98$C_TB_all_DOT)
#sum(SA_DR98$C_TB_hiv_neg_DOT)
#sum(SA_DR98$C_TB_hiv_DOT)

#DR98 STRATEGY COST: PT & TB (DOT)
SA_DR98$C_DR98_all_DOT<-SA_DR98$C_PT_all+SA_DR98$C_TB_all_DOT #Cost: strategy all (PT+TB) DOT
SA_DR98$C_DR98_hiv_neg_DOT<-SA_DR98$C_PT_hiv_neg+SA_DR98$C_TB_hiv_neg_DOT #Cost: strategy HIV negative (PT+TB) DOT
SA_DR98$C_DR98_hiv_DOT<-SA_DR98$C_PT_hiv+SA_DR98$C_TB_hiv_DOT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       DR98 ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
SA_DR98$C_DSTB_all_DOT_SAT<-(SA_DR98$TB_cases_all-SA_DR98$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_DR98$C_DSTB_hiv_neg_DOT_SAT<-((SA_DR98$TB_cases_all-SA_DR98$TB_cases_MDR)-(SA_DR98$TB_cases_HIV-SA_DR98$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_DR98$C_DSTB_hiv_DOT_SAT<-(SA_DR98$TB_cases_HIV-SA_DR98$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
SA_DR98$C_MDRTB_all_DOT_SAT<-(SA_DR98$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_DR98$C_MDRTB_hiv_neg_DOT_SAT<-(SA_DR98$TB_cases_MDR-SA_DR98$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_DR98$C_MDRTB_hiv_DOT_SAT<-SA_DR98$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
SA_DR98$C_TB_all_DOT_SAT<-SA_DR98$C_DSTB_all_DOT_SAT+SA_DR98$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
SA_DR98$C_TB_hiv_neg_DOT_SAT<-SA_DR98$C_DSTB_hiv_neg_DOT_SAT+SA_DR98$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_DR98$C_TB_hiv_DOT_SAT<-SA_DR98$C_DSTB_hiv_DOT_SAT+SA_DR98$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_DR98$C_TB_all_DOT_SAT)
#sum(SA_DR98$C_TB_hiv_neg_DOT_SAT)
#sum(SA_DR98$C_TB_hiv_DOT_SAT)

#DR98 STRATEGY COST: PT & TB (DOT_SAT)
SA_DR98$C_DR98_all_DOT_SAT<-SA_DR98$C_PT_all+SA_DR98$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT_SAT
SA_DR98$C_DR98_hiv_neg_DOT_SAT<-SA_DR98$C_PT_hiv_neg+SA_DR98$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
SA_DR98$C_DR98_hiv_DOT_SAT<-SA_DR98$C_PT_hiv+SA_DR98$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT



#####################################################################################################################
#                                       DR99 ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for DR99 dataset
# PT
SA_DR99$C_PT_all<-(SA_DR99$New_PT_all*(SA_costs$LTBI_DR99+SA_costs$AE_LTBI)) # Cost: PT all
SA_DR99$C_PT_hiv_neg<-(SA_DR99$New_PT_all-SA_DR99$New_PT_HIV)*(SA_costs$LTBI_DR99+SA_costs$AE_LTBI) # Cost: PT (HIV negative)
SA_DR99$C_PT_hiv<-(SA_DR99$New_PT_HIV*(SA_costs$LTBI_DR99+SA_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
SA_DR99$C_DSTB_all_DOT<-(SA_DR99$TB_cases_all-SA_DR99$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_DR99$C_DSTB_hiv_neg_DOT<-((SA_DR99$TB_cases_all-SA_DR99$TB_cases_MDR)-(SA_DR99$TB_cases_HIV-SA_DR99$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_DR99$C_DSTB_hiv_DOT<-(SA_DR99$TB_cases_HIV-SA_DR99$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
SA_DR99$C_MDRTB_all_DOT<-(SA_DR99$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_DR99$C_MDRTB_hiv_neg_DOT<-(SA_DR99$TB_cases_MDR-SA_DR99$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_DR99$C_MDRTB_hiv_DOT<-SA_DR99$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
SA_DR99$C_TB_all_DOT<-SA_DR99$C_DSTB_all_DOT+SA_DR99$C_MDRTB_all_DOT # Cost: TB (DS-TB & MRR-TB)
SA_DR99$C_TB_hiv_neg_DOT<-SA_DR99$C_DSTB_hiv_neg_DOT+SA_DR99$C_MDRTB_hiv_neg_DOT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_DR99$C_TB_hiv_DOT<-SA_DR99$C_DSTB_hiv_DOT+SA_DR99$C_MDRTB_hiv_DOT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_DR99$C_TB_all_DOT)
#sum(SA_DR99$C_TB_hiv_neg_DOT)
#sum(SA_DR99$C_TB_hiv_DOT)

#DR99 STRATEGY COST: PT & TB (DOT)
SA_DR99$C_DR99_all_DOT<-SA_DR99$C_PT_all+SA_DR99$C_TB_all_DOT #Cost: strategy all (PT+TB) DOT
SA_DR99$C_DR99_hiv_neg_DOT<-SA_DR99$C_PT_hiv_neg+SA_DR99$C_TB_hiv_neg_DOT #Cost: strategy HIV negative (PT+TB) DOT
SA_DR99$C_DR99_hiv_DOT<-SA_DR99$C_PT_hiv+SA_DR99$C_TB_hiv_DOT #Cost: strategy HIV negative (PT+TB) DOT


#####################################################################################################################
#                                       DR99 ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
SA_DR99$C_DSTB_all_DOT_SAT<-(SA_DR99$TB_cases_all-SA_DR99$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_DR99$C_DSTB_hiv_neg_DOT_SAT<-((SA_DR99$TB_cases_all-SA_DR99$TB_cases_MDR)-(SA_DR99$TB_cases_HIV-SA_DR99$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_DR99$C_DSTB_hiv_DOT_SAT<-(SA_DR99$TB_cases_HIV-SA_DR99$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
SA_DR99$C_MDRTB_all_DOT_SAT<-(SA_DR99$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_DR99$C_MDRTB_hiv_neg_DOT_SAT<-(SA_DR99$TB_cases_MDR-SA_DR99$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_DR99$C_MDRTB_hiv_DOT_SAT<-SA_DR99$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
SA_DR99$C_TB_all_DOT_SAT<-SA_DR99$C_DSTB_all_DOT_SAT+SA_DR99$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
SA_DR99$C_TB_hiv_neg_DOT_SAT<-SA_DR99$C_DSTB_hiv_neg_DOT_SAT+SA_DR99$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_DR99$C_TB_hiv_DOT_SAT<-SA_DR99$C_DSTB_hiv_DOT_SAT+SA_DR99$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_DR99$C_TB_all_DOT_SAT)
#sum(SA_DR99$C_TB_hiv_neg_DOT_SAT)
#sum(SA_DR99$C_TB_hiv_DOT_SAT)

#DR99 STRATEGY COST: PT & TB (DOT_SAT)
SA_DR99$C_DR99_all_DOT_SAT<-SA_DR99$C_PT_all+SA_DR99$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT_SAT
SA_DR99$C_DR99_hiv_neg_DOT_SAT<-SA_DR99$C_PT_hiv_neg+SA_DR99$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
SA_DR99$C_DR99_hiv_DOT_SAT<-SA_DR99$C_PT_hiv+SA_DR99$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT



#####################################################################################################################
#                                       DR25 ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for DR25 dataset
# PT
SA_DR25$C_PT_all<-(SA_DR25$New_PT_all*(SA_costs$LTBI_DR25+SA_costs$AE_LTBI)) # Cost: PT all
SA_DR25$C_PT_hiv_neg<-(SA_DR25$New_PT_all-SA_DR25$New_PT_HIV)*(SA_costs$LTBI_DR25+SA_costs$AE_LTBI) # Cost: PT (HIV negative)
SA_DR25$C_PT_hiv<-(SA_DR25$New_PT_HIV*(SA_costs$LTBI_DR25+SA_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
SA_DR25$C_DSTB_all_DOT<-(SA_DR25$TB_cases_all-SA_DR25$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_DR25$C_DSTB_hiv_neg_DOT<-((SA_DR25$TB_cases_all-SA_DR25$TB_cases_MDR)-(SA_DR25$TB_cases_HIV-SA_DR25$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_DR25$C_DSTB_hiv_DOT<-(SA_DR25$TB_cases_HIV-SA_DR25$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
SA_DR25$C_MDRTB_all_DOT<-(SA_DR25$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_DR25$C_MDRTB_hiv_neg_DOT<-(SA_DR25$TB_cases_MDR-SA_DR25$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_DR25$C_MDRTB_hiv_DOT<-SA_DR25$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
SA_DR25$C_TB_all_DOT<-SA_DR25$C_DSTB_all_DOT+SA_DR25$C_MDRTB_all_DOT # Cost: TB (DS-TB & MRR-TB)
SA_DR25$C_TB_hiv_neg_DOT<-SA_DR25$C_DSTB_hiv_neg_DOT+SA_DR25$C_MDRTB_hiv_neg_DOT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_DR25$C_TB_hiv_DOT<-SA_DR25$C_DSTB_hiv_DOT+SA_DR25$C_MDRTB_hiv_DOT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_DR25$C_TB_all_DOT)
#sum(SA_DR25$C_TB_hiv_neg_DOT)
#sum(SA_DR25$C_TB_hiv_DOT)

#DR25 STRATEGY COST: PT & TB (DOT)
SA_DR25$C_DR25_all_DOT<-SA_DR25$C_PT_all+SA_DR25$C_TB_all_DOT #Cost: strategy all (PT+TB) DOT
SA_DR25$C_DR25_hiv_neg_DOT<-SA_DR25$C_PT_hiv_neg+SA_DR25$C_TB_hiv_neg_DOT #Cost: strategy HIV negative (PT+TB) DOT
SA_DR25$C_DR25_hiv_DOT<-SA_DR25$C_PT_hiv+SA_DR25$C_TB_hiv_DOT #Cost: strategy HIV negative (PT+TB) DOT


#####################################################################################################################
#                                       DR25 ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
SA_DR25$C_DSTB_all_DOT_SAT<-(SA_DR25$TB_cases_all-SA_DR25$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_DR25$C_DSTB_hiv_neg_DOT_SAT<-((SA_DR25$TB_cases_all-SA_DR25$TB_cases_MDR)-(SA_DR25$TB_cases_HIV-SA_DR25$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_DR25$C_DSTB_hiv_DOT_SAT<-(SA_DR25$TB_cases_HIV-SA_DR25$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
SA_DR25$C_MDRTB_all_DOT_SAT<-(SA_DR25$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_DR25$C_MDRTB_hiv_neg_DOT_SAT<-(SA_DR25$TB_cases_MDR-SA_DR25$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_DR25$C_MDRTB_hiv_DOT_SAT<-SA_DR25$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
SA_DR25$C_TB_all_DOT_SAT<-SA_DR25$C_DSTB_all_DOT_SAT+SA_DR25$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
SA_DR25$C_TB_hiv_neg_DOT_SAT<-SA_DR25$C_DSTB_hiv_neg_DOT_SAT+SA_DR25$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_DR25$C_TB_hiv_DOT_SAT<-SA_DR25$C_DSTB_hiv_DOT_SAT+SA_DR25$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_DR25$C_TB_all_DOT_SAT)
#sum(SA_DR25$C_TB_hiv_neg_DOT_SAT)
#sum(SA_DR25$C_TB_hiv_DOT_SAT)

#DR25 STRATEGY COST: PT & TB (DOT_SAT)
SA_DR25$C_DR25_all_DOT_SAT<-SA_DR25$C_PT_all+SA_DR25$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT_SAT
SA_DR25$C_DR25_hiv_neg_DOT_SAT<-SA_DR25$C_PT_hiv_neg+SA_DR25$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
SA_DR25$C_DR25_hiv_DOT_SAT<-SA_DR25$C_PT_hiv+SA_DR25$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT





#####################################################################################################################
#                                       DR75 ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for DR75 dataset
# PT
SA_DR75$C_PT_all<-(SA_DR75$New_PT_all*(SA_costs$LTBI_DR75+SA_costs$AE_LTBI)) # Cost: PT all
SA_DR75$C_PT_hiv_neg<-(SA_DR75$New_PT_all-SA_DR75$New_PT_HIV)*(SA_costs$LTBI_DR75+SA_costs$AE_LTBI) # Cost: PT (HIV negative)
SA_DR75$C_PT_hiv<-(SA_DR75$New_PT_HIV*(SA_costs$LTBI_DR75+SA_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
SA_DR75$C_DSTB_all_DOT<-(SA_DR75$TB_cases_all-SA_DR75$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_DR75$C_DSTB_hiv_neg_DOT<-((SA_DR75$TB_cases_all-SA_DR75$TB_cases_MDR)-(SA_DR75$TB_cases_HIV-SA_DR75$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_DR75$C_DSTB_hiv_DOT<-(SA_DR75$TB_cases_HIV-SA_DR75$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
SA_DR75$C_MDRTB_all_DOT<-(SA_DR75$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_DR75$C_MDRTB_hiv_neg_DOT<-(SA_DR75$TB_cases_MDR-SA_DR75$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_DR75$C_MDRTB_hiv_DOT<-SA_DR75$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
SA_DR75$C_TB_all_DOT<-SA_DR75$C_DSTB_all_DOT+SA_DR75$C_MDRTB_all_DOT # Cost: TB (DS-TB & MRR-TB)
SA_DR75$C_TB_hiv_neg_DOT<-SA_DR75$C_DSTB_hiv_neg_DOT+SA_DR75$C_MDRTB_hiv_neg_DOT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_DR75$C_TB_hiv_DOT<-SA_DR75$C_DSTB_hiv_DOT+SA_DR75$C_MDRTB_hiv_DOT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_DR75$C_TB_all_DOT)
#sum(SA_DR75$C_TB_hiv_neg_DOT)
#sum(SA_DR75$C_TB_hiv_DOT)

#DR75 STRATEGY COST: PT & TB (DOT)
SA_DR75$C_DR75_all_DOT<-SA_DR75$C_PT_all+SA_DR75$C_TB_all_DOT #Cost: strategy all (PT+TB) DOT
SA_DR75$C_DR75_hiv_neg_DOT<-SA_DR75$C_PT_hiv_neg+SA_DR75$C_TB_hiv_neg_DOT #Cost: strategy HIV negative (PT+TB) DOT
SA_DR75$C_DR75_hiv_DOT<-SA_DR75$C_PT_hiv+SA_DR75$C_TB_hiv_DOT #Cost: strategy HIV negative (PT+TB) DOT


#####################################################################################################################
#                                       DR75 ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
SA_DR75$C_DSTB_all_DOT_SAT<-(SA_DR75$TB_cases_all-SA_DR75$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB
SA_DR75$C_DSTB_hiv_neg_DOT_SAT<-((SA_DR75$TB_cases_all-SA_DR75$TB_cases_MDR)-(SA_DR75$TB_cases_HIV-SA_DR75$TB_cases_MDR_HIV))*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
SA_DR75$C_DSTB_hiv_DOT_SAT<-(SA_DR75$TB_cases_HIV-SA_DR75$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$DSTB_DOT_SAT+SA_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
SA_DR75$C_MDRTB_all_DOT_SAT<-(SA_DR75$TB_cases_MDR)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB
SA_DR75$C_MDRTB_hiv_neg_DOT_SAT<-(SA_DR75$TB_cases_MDR-SA_DR75$TB_cases_MDR_HIV)*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
SA_DR75$C_MDRTB_hiv_DOT_SAT<-SA_DR75$TB_cases_MDR_HIV*(SA_costs$TB_PreDiagnosis+SA_costs$TB_Diagnosis+SA_costs$MDRTB_DOT_SAT+SA_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
SA_DR75$C_TB_all_DOT_SAT<-SA_DR75$C_DSTB_all_DOT_SAT+SA_DR75$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
SA_DR75$C_TB_hiv_neg_DOT_SAT<-SA_DR75$C_DSTB_hiv_neg_DOT_SAT+SA_DR75$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
SA_DR75$C_TB_hiv_DOT_SAT<-SA_DR75$C_DSTB_hiv_DOT_SAT+SA_DR75$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(SA_DR75$C_TB_all_DOT_SAT)
#sum(SA_DR75$C_TB_hiv_neg_DOT_SAT)
#sum(SA_DR75$C_TB_hiv_DOT_SAT)

#DR75 STRATEGY COST: PT & TB (DOT_SAT)
SA_DR75$C_DR75_all_DOT_SAT<-SA_DR75$C_PT_all+SA_DR75$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT_SAT
SA_DR75$C_DR75_hiv_neg_DOT_SAT<-SA_DR75$C_PT_hiv_neg+SA_DR75$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
SA_DR75$C_DR75_hiv_DOT_SAT<-SA_DR75$C_PT_hiv+SA_DR75$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT



#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#                                   DR-barrier Cumulative RR-TB & total cost (PT & TB)  per population group        #
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#####################################################################################################################
#                                   1DR. SA Cumulative cost & RR-TB#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
SA_strategy_cost_cumulative_DRbarrier<-data.frame(
  Runs=SA_baseline$Run,
  "Minimal_c"=SA_minimal$C_minimal_all_DOT_SAT,
  "DR96_c"=SA_DR96$C_DR96_all_DOT_SAT,
  "DR97_c"=SA_DR97$C_DR97_all_DOT_SAT,
  "DR98_c"=SA_DR98$C_DR98_all_DOT_SAT,
  "DR99_c"=SA_DR99$C_DR99_all_DOT_SAT,
  "Optimal_c"=SA_optimal$C_optimal_all_DOT_SAT,
  "Minimal_e"=SA_minimal$TB_cases_MDR,
  "DR96_e"=SA_DR96$TB_cases_MDR,
  "DR97_e"=SA_DR97$TB_cases_MDR,
  "DR98_e"=SA_DR98$TB_cases_MDR,
  "DR99_e"=SA_DR99$TB_cases_MDR,
  "Optimal_e"=SA_optimal$TB_cases_MDR)

SA_cost_pop_graph_cumulative_DRbarrier<-melt(SA_strategy_cost_cumulative_DRbarrier, id.vars =c("Runs"), variable.name = "cost_epi_pop")
SA_cost_pop_graph_cumulative_DRbarrier$strategy = ifelse(grepl("Minimal", SA_cost_pop_graph_cumulative_DRbarrier$cost_epi_pop), "Minimal",
                                                         ifelse(grepl("DR96", SA_cost_pop_graph_cumulative_DRbarrier$cost_epi_pop),"RR-96",
                                                                ifelse(grepl("DR97", SA_cost_pop_graph_cumulative_DRbarrier$cost_epi_pop),"RR-97",
                                                                       ifelse(grepl("DR98", SA_cost_pop_graph_cumulative_DRbarrier$cost_epi_pop),"RR-98",
                                                                              ifelse(grepl("DR99", SA_cost_pop_graph_cumulative_DRbarrier$cost_epi_pop),"RR-99",
                                                                                     "The optimal")))))
SA_cost_pop_graph_cumulative_DRbarrier$outcome = ifelse(grepl("_c", SA_cost_pop_graph_cumulative_DRbarrier$cost_epi_pop), "Cost","RR-TB")

SA_cost_pop_graph_cumulative_DRbarrier$cost_epi_pop = chartr("."," ",SA_cost_pop_graph_cumulative_DRbarrier$cost_epi_pop)

#####################################################################################################################

SA_cost_cumulat_DRbarrier<-aggregate(x=SA_cost_pop_graph_cumulative_DRbarrier$value, by=list(Runs=SA_cost_pop_graph_cumulative_DRbarrier$Runs,outcome=SA_cost_pop_graph_cumulative_DRbarrier$outcome,strategy=SA_cost_pop_graph_cumulative_DRbarrier$strategy), FUN=sum)

SA_overall_cost_table_DRbarrier<-SA_cost_cumulat_DRbarrier %>%
  group_by(strategy,outcome) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))

write.csv(SA_overall_cost_table_DRbarrier, file = "1.RR. South African DR barrier cumulative cost output tables.csv")

SA_overall_cost_table_DRbarrier2<-SA_cost_cumulat_DRbarrier %>%
  group_by(strategy,outcome) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.25),
                    quantile_upper = ~quantile(., probs = 0.75)))

SA_table_DRbarrier <- SA_overall_cost_table_DRbarrier2 %>% mutate( median = ifelse (outcome=="Cost", median/1000000000,
                                                                                    median/100000),
                                                                   quantile_lower = ifelse (outcome=="Cost", quantile_lower/1000000000,
                                                                                            quantile_lower/100000),
                                                                   quantile_upper = ifelse (outcome=="Cost", quantile_upper/1000000000,
                                                                                            quantile_upper/100000))


#table option 2

SA_cost_cumulat_graph_DRbarrier2 <- ggplot(SA_table_DRbarrier2, aes(y = median,x = strategy)) + 
  geom_line(aes())  +
  geom_errorbar(aes(ymin = quantile_lower, ymax = quantile_upper), width=.1) +
  geom_point(size=3)
SA_table_DRbarrier2
ggsave("1RR2. SA_graph_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



SA_cost_cumulat_graph_DRbarrier3 <- ggplot(SA_table_DRbarrier, aes(y = median,x = strategy)) +
  geom_jitter(
    aes(color = outcome),
    position = position_jitter(0)
  ) + 
  geom_line(
    aes(group = outcome, color = outcome),
    data = SA_table_DRbarrier
  ) +
  geom_errorbar(
    aes(ymin = quantile_lower, ymax = quantile_upper, color = outcome),
    data = SA_table_DRbarrier, width = 0.1
  )+
  scale_y_continuous(
    limits = c(0,5),
    labels = unit_format( unit = "B", scale = 1),
    
    # Features of the first axis
    name = "Cumulative cost (US$ billions)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~./1, name="RR-TB (100,000 cases)"))+
    
  theme_classic()+
  scale_color_manual(values = c("red", "deepskyblue"))
SA_cost_cumulat_graph_DRbarrier3

ggsave("1RR3. SA_graph_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")




#####################################################################################################################
#                                   2DR. SA Cumulative cost & RR-TB (minimal and optimal vs 25 and 75 DR barrier                   #
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
SA_strategy_cost_cumulative_2575DRbarrier<-data.frame(
  Runs=SA_baseline$Run,
  "Minimal_c"=SA_minimal$C_minimal_all_DOT_SAT,
  "DR25_c"=SA_DR25$C_DR25_all_DOT_SAT,
  "DR75_c"=SA_DR75$C_DR75_all_DOT_SAT,
  "Optimal_c"=SA_optimal$C_optimal_all_DOT_SAT,
  "Minimal_e"=SA_minimal$TB_cases_MDR,
  "DR25_e"=SA_DR25$TB_cases_MDR,
  "DR75_e"=SA_DR75$TB_cases_MDR,
  "Optimal_e"=SA_optimal$TB_cases_MDR)

SA_cost_pop_graph_cumulative_2575DRbarrier<-melt(SA_strategy_cost_cumulative_2575DRbarrier, id.vars =c("Runs"), variable.name = "cost_epi_pop")
SA_cost_pop_graph_cumulative_2575DRbarrier$strategy = ifelse(grepl("Minimal", SA_cost_pop_graph_cumulative_2575DRbarrier$cost_epi_pop), "RR-50 (minimal)",
                                                         ifelse(grepl("DR25", SA_cost_pop_graph_cumulative_2575DRbarrier$cost_epi_pop),"RR-25",
                                                                ifelse(grepl("DR75", SA_cost_pop_graph_cumulative_2575DRbarrier$cost_epi_pop),"RR-75",
                                                                                     "The optimal")))
SA_cost_pop_graph_cumulative_2575DRbarrier$outcome = ifelse(grepl("_c", SA_cost_pop_graph_cumulative_2575DRbarrier$cost_epi_pop), "Cost","RR-TB")

SA_cost_pop_graph_cumulative_2575DRbarrier$cost_epi_pop = chartr("."," ",SA_cost_pop_graph_cumulative_2575DRbarrier$cost_epi_pop)

#####################################################################################################################

SA_cost_cumulat_2575DRbarrier<-aggregate(x=SA_cost_pop_graph_cumulative_2575DRbarrier$value, by=list(Runs=SA_cost_pop_graph_cumulative_2575DRbarrier$Runs,outcome=SA_cost_pop_graph_cumulative_2575DRbarrier$outcome,strategy=SA_cost_pop_graph_cumulative_2575DRbarrier$strategy), FUN=sum)

SA_overall_cost_table_2575DRbarrier<-SA_cost_cumulat_2575DRbarrier %>%
  group_by(strategy,outcome) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.755)))

write.csv(SA_overall_cost_table_2575DRbarrier, file = "2.RR. South African 25 and 75 DR barrier cumulative cost output tables.csv")

SA_overall_cost_table_2575DRbarrier2<-SA_cost_cumulat_2575DRbarrier %>%
  group_by(strategy,outcome) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.25),
                    quantile_upper = ~quantile(., probs = 0.75)))

SA_table_2575DRbarrier <- SA_overall_cost_table_2575DRbarrier2 %>% mutate( median = ifelse (outcome=="Cost", median/1000000000,
                                                                                    median/100000),
                                                                   quantile_lower = ifelse (outcome=="Cost", quantile_lower/1000000000,
                                                                                            quantile_lower/100000),
                                                                   quantile_upper = ifelse (outcome=="Cost", quantile_upper/1000000000,
                                                                                            quantile_upper/100000))

#table option 2


SA_cost_cumulat_graph_2575DRbarrier3 <- ggplot(SA_table_2575DRbarrier, aes(y = median,x = strategy)) +
  geom_jitter(
    aes(color = outcome),
    position = position_jitter(0)
  ) + 
  geom_line(
    aes(group = outcome, color = outcome),
    data = SA_table_2575DRbarrier
  ) +
  geom_errorbar(
    aes(ymin = quantile_lower, ymax = quantile_upper, color = outcome),
    data = SA_table_2575DRbarrier, width = 0.1
  )+
  scale_y_continuous(
    limits = c(0,5),
    labels = unit_format( unit = "B", scale = 1),
    
    # Features of the first axis
    name = "Cumulative cost (US$ billions)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~./1, name="RR-TB (100,000 cases)"))+
  
  theme_classic()+
  scale_color_manual(values = c("red", "deepskyblue"))
SA_cost_cumulat_graph_2575DRbarrier3

ggsave("2RR3. SA_graph_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



# Combine RR-TB sensitivity analysis graphs
SA_DRTB_combine<-ggarrange(SA_cost_cumulat_graph_DRbarrier3, SA_cost_cumulat_graph_2575DRbarrier3, labels = c("RR barrier of 96-99%", "RR barrier of 25 and 75%"),
                                 common.legend = TRUE, legend = "bottom")
SA_DRTB_combine
ggsave("3RR. SA_combined_graphs_DRTB.png", 
       width = 30, height = 20, units = "cm")








#####################################################################################################################
#                                               LOAD BRAZIL DATA                                                       #
#####################################################################################################################
#Load South African mathematical model epidemiological outputs - DR barriers
Brazil_DR96 <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Brazil_DR96_data", col_types = "numeric", range = "A1:R16001")
Brazil_DR97 <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Brazil_DR97_data", col_types = "numeric", range = "A1:R16001")
Brazil_DR98 <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Brazil_DR98_data", col_types = "numeric", range = "A1:R16001")
Brazil_DR99 <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Brazil_DR99_data", col_types = "numeric", range = "A1:R16001")
Brazil_DR25 <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Brazil_DR25_data", col_types = "numeric", range = "A1:R16001")
Brazil_DR75 <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Brazil_DR75_data", col_types = "numeric", range = "A1:R16001")

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### South Africa #######
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#####################################################################################################################
#                                       DR96 ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for DR96 dataset
# PT
Brazil_DR96$C_PT_all<-(Brazil_DR96$New_PT_all*(Brazil_costs$LTBI_DR96+Brazil_costs$AE_LTBI)) # Cost: PT all
Brazil_DR96$C_PT_hiv_neg<-(Brazil_DR96$New_PT_all-Brazil_DR96$New_PT_HIV)*(Brazil_costs$LTBI_DR96+Brazil_costs$AE_LTBI) # Cost: PT (HIV negative)
Brazil_DR96$C_PT_hiv<-(Brazil_DR96$New_PT_HIV*(Brazil_costs$LTBI_DR96+Brazil_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
Brazil_DR96$C_DSTB_all_DOT<-(Brazil_DR96$TB_cases_all-Brazil_DR96$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_DR96$C_DSTB_hiv_neg_DOT<-((Brazil_DR96$TB_cases_all-Brazil_DR96$TB_cases_MDR)-(Brazil_DR96$TB_cases_HIV-Brazil_DR96$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_DR96$C_DSTB_hiv_DOT<-(Brazil_DR96$TB_cases_HIV-Brazil_DR96$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
Brazil_DR96$C_MDRTB_all_DOT<-(Brazil_DR96$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_DR96$C_MDRTB_hiv_neg_DOT<-(Brazil_DR96$TB_cases_MDR-Brazil_DR96$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_DR96$C_MDRTB_hiv_DOT<-Brazil_DR96$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
Brazil_DR96$C_TB_all_DOT<-Brazil_DR96$C_DSTB_all_DOT+Brazil_DR96$C_MDRTB_all_DOT # Cost: TB (DS-TB & MRR-TB)
Brazil_DR96$C_TB_hiv_neg_DOT<-Brazil_DR96$C_DSTB_hiv_neg_DOT+Brazil_DR96$C_MDRTB_hiv_neg_DOT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_DR96$C_TB_hiv_DOT<-Brazil_DR96$C_DSTB_hiv_DOT+Brazil_DR96$C_MDRTB_hiv_DOT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_DR96$C_TB_all_DOT)
#sum(Brazil_DR96$C_TB_hiv_neg_DOT)
#sum(Brazil_DR96$C_TB_hiv_DOT)

#DR96 STRATEGY COST: PT & TB (DOT)
Brazil_DR96$C_DR96_all_DOT<-Brazil_DR96$C_PT_all+Brazil_DR96$C_TB_all_DOT #Cost: strategy all (PT+TB) DOT
Brazil_DR96$C_DR96_hiv_neg_DOT<-Brazil_DR96$C_PT_hiv_neg+Brazil_DR96$C_TB_hiv_neg_DOT #Cost: strategy HIV negative (PT+TB) DOT
Brazil_DR96$C_DR96_hiv_DOT<-Brazil_DR96$C_PT_hiv+Brazil_DR96$C_TB_hiv_DOT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       DR96 ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
Brazil_DR96$C_DSTB_all_DOT_SAT<-(Brazil_DR96$TB_cases_all-Brazil_DR96$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_DR96$C_DSTB_hiv_neg_DOT_SAT<-((Brazil_DR96$TB_cases_all-Brazil_DR96$TB_cases_MDR)-(Brazil_DR96$TB_cases_HIV-Brazil_DR96$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_DR96$C_DSTB_hiv_DOT_SAT<-(Brazil_DR96$TB_cases_HIV-Brazil_DR96$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
Brazil_DR96$C_MDRTB_all_DOT_SAT<-(Brazil_DR96$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_DR96$C_MDRTB_hiv_neg_DOT_SAT<-(Brazil_DR96$TB_cases_MDR-Brazil_DR96$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_DR96$C_MDRTB_hiv_DOT_SAT<-Brazil_DR96$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
Brazil_DR96$C_TB_all_DOT_SAT<-Brazil_DR96$C_DSTB_all_DOT_SAT+Brazil_DR96$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_DR96$C_TB_hiv_neg_DOT_SAT<-Brazil_DR96$C_DSTB_hiv_neg_DOT_SAT+Brazil_DR96$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_DR96$C_TB_hiv_DOT_SAT<-Brazil_DR96$C_DSTB_hiv_DOT_SAT+Brazil_DR96$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_DR96$C_TB_all_DOT_SAT)
#sum(Brazil_DR96$C_TB_hiv_neg_DOT_SAT)
#sum(Brazil_DR96$C_TB_hiv_DOT_SAT)

#DR96 STRATEGY COST: PT & TB (DOT_SAT)
Brazil_DR96$C_DR96_all_DOT_SAT<-Brazil_DR96$C_PT_all+Brazil_DR96$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT_SAT
Brazil_DR96$C_DR96_hiv_neg_DOT_SAT<-Brazil_DR96$C_PT_hiv_neg+Brazil_DR96$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
Brazil_DR96$C_DR96_hiv_DOT_SAT<-Brazil_DR96$C_PT_hiv+Brazil_DR96$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT



#####################################################################################################################
#                                       DR97 ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for DR97 dataset
# PT
Brazil_DR97$C_PT_all<-(Brazil_DR97$New_PT_all*(Brazil_costs$LTBI_DR97+Brazil_costs$AE_LTBI)) # Cost: PT all
Brazil_DR97$C_PT_hiv_neg<-(Brazil_DR97$New_PT_all-Brazil_DR97$New_PT_HIV)*(Brazil_costs$LTBI_DR97+Brazil_costs$AE_LTBI) # Cost: PT (HIV negative)
Brazil_DR97$C_PT_hiv<-(Brazil_DR97$New_PT_HIV*(Brazil_costs$LTBI_DR97+Brazil_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
Brazil_DR97$C_DSTB_all_DOT<-(Brazil_DR97$TB_cases_all-Brazil_DR97$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_DR97$C_DSTB_hiv_neg_DOT<-((Brazil_DR97$TB_cases_all-Brazil_DR97$TB_cases_MDR)-(Brazil_DR97$TB_cases_HIV-Brazil_DR97$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_DR97$C_DSTB_hiv_DOT<-(Brazil_DR97$TB_cases_HIV-Brazil_DR97$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
Brazil_DR97$C_MDRTB_all_DOT<-(Brazil_DR97$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_DR97$C_MDRTB_hiv_neg_DOT<-(Brazil_DR97$TB_cases_MDR-Brazil_DR97$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_DR97$C_MDRTB_hiv_DOT<-Brazil_DR97$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
Brazil_DR97$C_TB_all_DOT<-Brazil_DR97$C_DSTB_all_DOT+Brazil_DR97$C_MDRTB_all_DOT # Cost: TB (DS-TB & MRR-TB)
Brazil_DR97$C_TB_hiv_neg_DOT<-Brazil_DR97$C_DSTB_hiv_neg_DOT+Brazil_DR97$C_MDRTB_hiv_neg_DOT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_DR97$C_TB_hiv_DOT<-Brazil_DR97$C_DSTB_hiv_DOT+Brazil_DR97$C_MDRTB_hiv_DOT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_DR97$C_TB_all_DOT)
#sum(Brazil_DR97$C_TB_hiv_neg_DOT)
#sum(Brazil_DR97$C_TB_hiv_DOT)

#DR97 STRATEGY COST: PT & TB (DOT)
Brazil_DR97$C_DR97_all_DOT<-Brazil_DR97$C_PT_all+Brazil_DR97$C_TB_all_DOT #Cost: strategy all (PT+TB) DOT
Brazil_DR97$C_DR97_hiv_neg_DOT<-Brazil_DR97$C_PT_hiv_neg+Brazil_DR97$C_TB_hiv_neg_DOT #Cost: strategy HIV negative (PT+TB) DOT
Brazil_DR97$C_DR97_hiv_DOT<-Brazil_DR97$C_PT_hiv+Brazil_DR97$C_TB_hiv_DOT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       DR97 ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
Brazil_DR97$C_DSTB_all_DOT_SAT<-(Brazil_DR97$TB_cases_all-Brazil_DR97$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_DR97$C_DSTB_hiv_neg_DOT_SAT<-((Brazil_DR97$TB_cases_all-Brazil_DR97$TB_cases_MDR)-(Brazil_DR97$TB_cases_HIV-Brazil_DR97$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_DR97$C_DSTB_hiv_DOT_SAT<-(Brazil_DR97$TB_cases_HIV-Brazil_DR97$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
Brazil_DR97$C_MDRTB_all_DOT_SAT<-(Brazil_DR97$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_DR97$C_MDRTB_hiv_neg_DOT_SAT<-(Brazil_DR97$TB_cases_MDR-Brazil_DR97$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_DR97$C_MDRTB_hiv_DOT_SAT<-Brazil_DR97$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
Brazil_DR97$C_TB_all_DOT_SAT<-Brazil_DR97$C_DSTB_all_DOT_SAT+Brazil_DR97$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_DR97$C_TB_hiv_neg_DOT_SAT<-Brazil_DR97$C_DSTB_hiv_neg_DOT_SAT+Brazil_DR97$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_DR97$C_TB_hiv_DOT_SAT<-Brazil_DR97$C_DSTB_hiv_DOT_SAT+Brazil_DR97$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_DR97$C_TB_all_DOT_SAT)
#sum(Brazil_DR97$C_TB_hiv_neg_DOT_SAT)
#sum(Brazil_DR97$C_TB_hiv_DOT_SAT)

#DR97 STRATEGY COST: PT & TB (DOT_SAT)
Brazil_DR97$C_DR97_all_DOT_SAT<-Brazil_DR97$C_PT_all+Brazil_DR97$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT_SAT
Brazil_DR97$C_DR97_hiv_neg_DOT_SAT<-Brazil_DR97$C_PT_hiv_neg+Brazil_DR97$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT

Brazil_DR97$C_DR97_hiv_DOT_SAT<-Brazil_DR97$C_PT_hiv+Brazil_DR97$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT



#####################################################################################################################
#                                       DR98 ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for DR98 dataset
# PT
Brazil_DR98$C_PT_all<-(Brazil_DR98$New_PT_all*(Brazil_costs$LTBI_DR98+Brazil_costs$AE_LTBI)) # Cost: PT all
Brazil_DR98$C_PT_hiv_neg<-(Brazil_DR98$New_PT_all-Brazil_DR98$New_PT_HIV)*(Brazil_costs$LTBI_DR98+Brazil_costs$AE_LTBI) # Cost: PT (HIV negative)
Brazil_DR98$C_PT_hiv<-(Brazil_DR98$New_PT_HIV*(Brazil_costs$LTBI_DR98+Brazil_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
Brazil_DR98$C_DSTB_all_DOT<-(Brazil_DR98$TB_cases_all-Brazil_DR98$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_DR98$C_DSTB_hiv_neg_DOT<-((Brazil_DR98$TB_cases_all-Brazil_DR98$TB_cases_MDR)-(Brazil_DR98$TB_cases_HIV-Brazil_DR98$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_DR98$C_DSTB_hiv_DOT<-(Brazil_DR98$TB_cases_HIV-Brazil_DR98$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
Brazil_DR98$C_MDRTB_all_DOT<-(Brazil_DR98$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_DR98$C_MDRTB_hiv_neg_DOT<-(Brazil_DR98$TB_cases_MDR-Brazil_DR98$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_DR98$C_MDRTB_hiv_DOT<-Brazil_DR98$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
Brazil_DR98$C_TB_all_DOT<-Brazil_DR98$C_DSTB_all_DOT+Brazil_DR98$C_MDRTB_all_DOT # Cost: TB (DS-TB & MRR-TB)
Brazil_DR98$C_TB_hiv_neg_DOT<-Brazil_DR98$C_DSTB_hiv_neg_DOT+Brazil_DR98$C_MDRTB_hiv_neg_DOT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_DR98$C_TB_hiv_DOT<-Brazil_DR98$C_DSTB_hiv_DOT+Brazil_DR98$C_MDRTB_hiv_DOT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_DR98$C_TB_all_DOT)
#sum(Brazil_DR98$C_TB_hiv_neg_DOT)
#sum(Brazil_DR98$C_TB_hiv_DOT)

#DR98 STRATEGY COST: PT & TB (DOT)
Brazil_DR98$C_DR98_all_DOT<-Brazil_DR98$C_PT_all+Brazil_DR98$C_TB_all_DOT #Cost: strategy all (PT+TB) DOT
Brazil_DR98$C_DR98_hiv_neg_DOT<-Brazil_DR98$C_PT_hiv_neg+Brazil_DR98$C_TB_hiv_neg_DOT #Cost: strategy HIV negative (PT+TB) DOT
Brazil_DR98$C_DR98_hiv_DOT<-Brazil_DR98$C_PT_hiv+Brazil_DR98$C_TB_hiv_DOT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       DR98 ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
Brazil_DR98$C_DSTB_all_DOT_SAT<-(Brazil_DR98$TB_cases_all-Brazil_DR98$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_DR98$C_DSTB_hiv_neg_DOT_SAT<-((Brazil_DR98$TB_cases_all-Brazil_DR98$TB_cases_MDR)-(Brazil_DR98$TB_cases_HIV-Brazil_DR98$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_DR98$C_DSTB_hiv_DOT_SAT<-(Brazil_DR98$TB_cases_HIV-Brazil_DR98$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
Brazil_DR98$C_MDRTB_all_DOT_SAT<-(Brazil_DR98$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_DR98$C_MDRTB_hiv_neg_DOT_SAT<-(Brazil_DR98$TB_cases_MDR-Brazil_DR98$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_DR98$C_MDRTB_hiv_DOT_SAT<-Brazil_DR98$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
Brazil_DR98$C_TB_all_DOT_SAT<-Brazil_DR98$C_DSTB_all_DOT_SAT+Brazil_DR98$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_DR98$C_TB_hiv_neg_DOT_SAT<-Brazil_DR98$C_DSTB_hiv_neg_DOT_SAT+Brazil_DR98$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_DR98$C_TB_hiv_DOT_SAT<-Brazil_DR98$C_DSTB_hiv_DOT_SAT+Brazil_DR98$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_DR98$C_TB_all_DOT_SAT)
#sum(Brazil_DR98$C_TB_hiv_neg_DOT_SAT)
#sum(Brazil_DR98$C_TB_hiv_DOT_SAT)

#DR98 STRATEGY COST: PT & TB (DOT_SAT)
Brazil_DR98$C_DR98_all_DOT_SAT<-Brazil_DR98$C_PT_all+Brazil_DR98$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT_SAT
Brazil_DR98$C_DR98_hiv_neg_DOT_SAT<-Brazil_DR98$C_PT_hiv_neg+Brazil_DR98$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
Brazil_DR98$C_DR98_hiv_DOT_SAT<-Brazil_DR98$C_PT_hiv+Brazil_DR98$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT



#####################################################################################################################
#                                       DR99 ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for DR99 dataset
# PT
Brazil_DR99$C_PT_all<-(Brazil_DR99$New_PT_all*(Brazil_costs$LTBI_DR99+Brazil_costs$AE_LTBI)) # Cost: PT all
Brazil_DR99$C_PT_hiv_neg<-(Brazil_DR99$New_PT_all-Brazil_DR99$New_PT_HIV)*(Brazil_costs$LTBI_DR99+Brazil_costs$AE_LTBI) # Cost: PT (HIV negative)
Brazil_DR99$C_PT_hiv<-(Brazil_DR99$New_PT_HIV*(Brazil_costs$LTBI_DR99+Brazil_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
Brazil_DR99$C_DSTB_all_DOT<-(Brazil_DR99$TB_cases_all-Brazil_DR99$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_DR99$C_DSTB_hiv_neg_DOT<-((Brazil_DR99$TB_cases_all-Brazil_DR99$TB_cases_MDR)-(Brazil_DR99$TB_cases_HIV-Brazil_DR99$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_DR99$C_DSTB_hiv_DOT<-(Brazil_DR99$TB_cases_HIV-Brazil_DR99$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
Brazil_DR99$C_MDRTB_all_DOT<-(Brazil_DR99$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_DR99$C_MDRTB_hiv_neg_DOT<-(Brazil_DR99$TB_cases_MDR-Brazil_DR99$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_DR99$C_MDRTB_hiv_DOT<-Brazil_DR99$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
Brazil_DR99$C_TB_all_DOT<-Brazil_DR99$C_DSTB_all_DOT+Brazil_DR99$C_MDRTB_all_DOT # Cost: TB (DS-TB & MRR-TB)
Brazil_DR99$C_TB_hiv_neg_DOT<-Brazil_DR99$C_DSTB_hiv_neg_DOT+Brazil_DR99$C_MDRTB_hiv_neg_DOT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_DR99$C_TB_hiv_DOT<-Brazil_DR99$C_DSTB_hiv_DOT+Brazil_DR99$C_MDRTB_hiv_DOT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_DR99$C_TB_all_DOT)
#sum(Brazil_DR99$C_TB_hiv_neg_DOT)
#sum(Brazil_DR99$C_TB_hiv_DOT)

#DR99 STRATEGY COST: PT & TB (DOT)
Brazil_DR99$C_DR99_all_DOT<-Brazil_DR99$C_PT_all+Brazil_DR99$C_TB_all_DOT #Cost: strategy all (PT+TB) DOT
Brazil_DR99$C_DR99_hiv_neg_DOT<-Brazil_DR99$C_PT_hiv_neg+Brazil_DR99$C_TB_hiv_neg_DOT #Cost: strategy HIV negative (PT+TB) DOT
Brazil_DR99$C_DR99_hiv_DOT<-Brazil_DR99$C_PT_hiv+Brazil_DR99$C_TB_hiv_DOT #Cost: strategy HIV negative (PT+TB) DOT


#####################################################################################################################
#                                       DR99 ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
Brazil_DR99$C_DSTB_all_DOT_SAT<-(Brazil_DR99$TB_cases_all-Brazil_DR99$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_DR99$C_DSTB_hiv_neg_DOT_SAT<-((Brazil_DR99$TB_cases_all-Brazil_DR99$TB_cases_MDR)-(Brazil_DR99$TB_cases_HIV-Brazil_DR99$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_DR99$C_DSTB_hiv_DOT_SAT<-(Brazil_DR99$TB_cases_HIV-Brazil_DR99$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
Brazil_DR99$C_MDRTB_all_DOT_SAT<-(Brazil_DR99$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_DR99$C_MDRTB_hiv_neg_DOT_SAT<-(Brazil_DR99$TB_cases_MDR-Brazil_DR99$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_DR99$C_MDRTB_hiv_DOT_SAT<-Brazil_DR99$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
Brazil_DR99$C_TB_all_DOT_SAT<-Brazil_DR99$C_DSTB_all_DOT_SAT+Brazil_DR99$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_DR99$C_TB_hiv_neg_DOT_SAT<-Brazil_DR99$C_DSTB_hiv_neg_DOT_SAT+Brazil_DR99$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_DR99$C_TB_hiv_DOT_SAT<-Brazil_DR99$C_DSTB_hiv_DOT_SAT+Brazil_DR99$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_DR99$C_TB_all_DOT_SAT)
#sum(Brazil_DR99$C_TB_hiv_neg_DOT_SAT)
#sum(Brazil_DR99$C_TB_hiv_DOT_SAT)

#DR99 STRATEGY COST: PT & TB (DOT_SAT)
Brazil_DR99$C_DR99_all_DOT_SAT<-Brazil_DR99$C_PT_all+Brazil_DR99$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT_SAT
Brazil_DR99$C_DR99_hiv_neg_DOT_SAT<-Brazil_DR99$C_PT_hiv_neg+Brazil_DR99$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
Brazil_DR99$C_DR99_hiv_DOT_SAT<-Brazil_DR99$C_PT_hiv+Brazil_DR99$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT



#####################################################################################################################
#                                       DR25 ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for DR25 dataset
# PT
Brazil_DR25$C_PT_all<-(Brazil_DR25$New_PT_all*(Brazil_costs$LTBI_DR25+Brazil_costs$AE_LTBI)) # Cost: PT all
Brazil_DR25$C_PT_hiv_neg<-(Brazil_DR25$New_PT_all-Brazil_DR25$New_PT_HIV)*(Brazil_costs$LTBI_DR25+Brazil_costs$AE_LTBI) # Cost: PT (HIV negative)
Brazil_DR25$C_PT_hiv<-(Brazil_DR25$New_PT_HIV*(Brazil_costs$LTBI_DR25+Brazil_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
Brazil_DR25$C_DSTB_all_DOT<-(Brazil_DR25$TB_cases_all-Brazil_DR25$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_DR25$C_DSTB_hiv_neg_DOT<-((Brazil_DR25$TB_cases_all-Brazil_DR25$TB_cases_MDR)-(Brazil_DR25$TB_cases_HIV-Brazil_DR25$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_DR25$C_DSTB_hiv_DOT<-(Brazil_DR25$TB_cases_HIV-Brazil_DR25$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
Brazil_DR25$C_MDRTB_all_DOT<-(Brazil_DR25$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_DR25$C_MDRTB_hiv_neg_DOT<-(Brazil_DR25$TB_cases_MDR-Brazil_DR25$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_DR25$C_MDRTB_hiv_DOT<-Brazil_DR25$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
Brazil_DR25$C_TB_all_DOT<-Brazil_DR25$C_DSTB_all_DOT+Brazil_DR25$C_MDRTB_all_DOT # Cost: TB (DS-TB & MRR-TB)
Brazil_DR25$C_TB_hiv_neg_DOT<-Brazil_DR25$C_DSTB_hiv_neg_DOT+Brazil_DR25$C_MDRTB_hiv_neg_DOT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_DR25$C_TB_hiv_DOT<-Brazil_DR25$C_DSTB_hiv_DOT+Brazil_DR25$C_MDRTB_hiv_DOT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_DR25$C_TB_all_DOT)
#sum(Brazil_DR25$C_TB_hiv_neg_DOT)
#sum(Brazil_DR25$C_TB_hiv_DOT)

#DR25 STRATEGY COST: PT & TB (DOT)
Brazil_DR25$C_DR25_all_DOT<-Brazil_DR25$C_PT_all+Brazil_DR25$C_TB_all_DOT #Cost: strategy all (PT+TB) DOT
Brazil_DR25$C_DR25_hiv_neg_DOT<-Brazil_DR25$C_PT_hiv_neg+Brazil_DR25$C_TB_hiv_neg_DOT #Cost: strategy HIV negative (PT+TB) DOT
Brazil_DR25$C_DR25_hiv_DOT<-Brazil_DR25$C_PT_hiv+Brazil_DR25$C_TB_hiv_DOT #Cost: strategy HIV negative (PT+TB) DOT


#####################################################################################################################
#                                       DR25 ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
Brazil_DR25$C_DSTB_all_DOT_SAT<-(Brazil_DR25$TB_cases_all-Brazil_DR25$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_DR25$C_DSTB_hiv_neg_DOT_SAT<-((Brazil_DR25$TB_cases_all-Brazil_DR25$TB_cases_MDR)-(Brazil_DR25$TB_cases_HIV-Brazil_DR25$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_DR25$C_DSTB_hiv_DOT_SAT<-(Brazil_DR25$TB_cases_HIV-Brazil_DR25$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
Brazil_DR25$C_MDRTB_all_DOT_SAT<-(Brazil_DR25$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_DR25$C_MDRTB_hiv_neg_DOT_SAT<-(Brazil_DR25$TB_cases_MDR-Brazil_DR25$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_DR25$C_MDRTB_hiv_DOT_SAT<-Brazil_DR25$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
Brazil_DR25$C_TB_all_DOT_SAT<-Brazil_DR25$C_DSTB_all_DOT_SAT+Brazil_DR25$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_DR25$C_TB_hiv_neg_DOT_SAT<-Brazil_DR25$C_DSTB_hiv_neg_DOT_SAT+Brazil_DR25$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_DR25$C_TB_hiv_DOT_SAT<-Brazil_DR25$C_DSTB_hiv_DOT_SAT+Brazil_DR25$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_DR25$C_TB_all_DOT_SAT)
#sum(Brazil_DR25$C_TB_hiv_neg_DOT_SAT)
#sum(Brazil_DR25$C_TB_hiv_DOT_SAT)

#DR25 STRATEGY COST: PT & TB (DOT_SAT)
Brazil_DR25$C_DR25_all_DOT_SAT<-Brazil_DR25$C_PT_all+Brazil_DR25$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT_SAT
Brazil_DR25$C_DR25_hiv_neg_DOT_SAT<-Brazil_DR25$C_PT_hiv_neg+Brazil_DR25$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
Brazil_DR25$C_DR25_hiv_DOT_SAT<-Brazil_DR25$C_PT_hiv+Brazil_DR25$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT





#####################################################################################################################
#                                       DR75 ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for DR75 dataset
# PT
Brazil_DR75$C_PT_all<-(Brazil_DR75$New_PT_all*(Brazil_costs$LTBI_DR75+Brazil_costs$AE_LTBI)) # Cost: PT all
Brazil_DR75$C_PT_hiv_neg<-(Brazil_DR75$New_PT_all-Brazil_DR75$New_PT_HIV)*(Brazil_costs$LTBI_DR75+Brazil_costs$AE_LTBI) # Cost: PT (HIV negative)
Brazil_DR75$C_PT_hiv<-(Brazil_DR75$New_PT_HIV*(Brazil_costs$LTBI_DR75+Brazil_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
Brazil_DR75$C_DSTB_all_DOT<-(Brazil_DR75$TB_cases_all-Brazil_DR75$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_DR75$C_DSTB_hiv_neg_DOT<-((Brazil_DR75$TB_cases_all-Brazil_DR75$TB_cases_MDR)-(Brazil_DR75$TB_cases_HIV-Brazil_DR75$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_DR75$C_DSTB_hiv_DOT<-(Brazil_DR75$TB_cases_HIV-Brazil_DR75$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
Brazil_DR75$C_MDRTB_all_DOT<-(Brazil_DR75$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_DR75$C_MDRTB_hiv_neg_DOT<-(Brazil_DR75$TB_cases_MDR-Brazil_DR75$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_DR75$C_MDRTB_hiv_DOT<-Brazil_DR75$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
Brazil_DR75$C_TB_all_DOT<-Brazil_DR75$C_DSTB_all_DOT+Brazil_DR75$C_MDRTB_all_DOT # Cost: TB (DS-TB & MRR-TB)
Brazil_DR75$C_TB_hiv_neg_DOT<-Brazil_DR75$C_DSTB_hiv_neg_DOT+Brazil_DR75$C_MDRTB_hiv_neg_DOT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_DR75$C_TB_hiv_DOT<-Brazil_DR75$C_DSTB_hiv_DOT+Brazil_DR75$C_MDRTB_hiv_DOT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_DR75$C_TB_all_DOT)
#sum(Brazil_DR75$C_TB_hiv_neg_DOT)
#sum(Brazil_DR75$C_TB_hiv_DOT)

#DR75 STRATEGY COST: PT & TB (DOT)
Brazil_DR75$C_DR75_all_DOT<-Brazil_DR75$C_PT_all+Brazil_DR75$C_TB_all_DOT #Cost: strategy all (PT+TB) DOT
Brazil_DR75$C_DR75_hiv_neg_DOT<-Brazil_DR75$C_PT_hiv_neg+Brazil_DR75$C_TB_hiv_neg_DOT #Cost: strategy HIV negative (PT+TB) DOT
Brazil_DR75$C_DR75_hiv_DOT<-Brazil_DR75$C_PT_hiv+Brazil_DR75$C_TB_hiv_DOT #Cost: strategy HIV negative (PT+TB) DOT


#####################################################################################################################
#                                       DR75 ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
Brazil_DR75$C_DSTB_all_DOT_SAT<-(Brazil_DR75$TB_cases_all-Brazil_DR75$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_DR75$C_DSTB_hiv_neg_DOT_SAT<-((Brazil_DR75$TB_cases_all-Brazil_DR75$TB_cases_MDR)-(Brazil_DR75$TB_cases_HIV-Brazil_DR75$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_DR75$C_DSTB_hiv_DOT_SAT<-(Brazil_DR75$TB_cases_HIV-Brazil_DR75$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
Brazil_DR75$C_MDRTB_all_DOT_SAT<-(Brazil_DR75$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_DR75$C_MDRTB_hiv_neg_DOT_SAT<-(Brazil_DR75$TB_cases_MDR-Brazil_DR75$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_DR75$C_MDRTB_hiv_DOT_SAT<-Brazil_DR75$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
Brazil_DR75$C_TB_all_DOT_SAT<-Brazil_DR75$C_DSTB_all_DOT_SAT+Brazil_DR75$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_DR75$C_TB_hiv_neg_DOT_SAT<-Brazil_DR75$C_DSTB_hiv_neg_DOT_SAT+Brazil_DR75$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_DR75$C_TB_hiv_DOT_SAT<-Brazil_DR75$C_DSTB_hiv_DOT_SAT+Brazil_DR75$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_DR75$C_TB_all_DOT_SAT)
#sum(Brazil_DR75$C_TB_hiv_neg_DOT_SAT)
#sum(Brazil_DR75$C_TB_hiv_DOT_SAT)

#DR75 STRATEGY COST: PT & TB (DOT_SAT)
Brazil_DR75$C_DR75_all_DOT_SAT<-Brazil_DR75$C_PT_all+Brazil_DR75$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT_SAT
Brazil_DR75$C_DR75_hiv_neg_DOT_SAT<-Brazil_DR75$C_PT_hiv_neg+Brazil_DR75$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
Brazil_DR75$C_DR75_hiv_DOT_SAT<-Brazil_DR75$C_PT_hiv+Brazil_DR75$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT



#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#                                   DR-barrier Cumulative RR-TB & total cost (PT & TB)  per population group        #
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#####################################################################################################################
#####################################################################################################################
#                                   1DR. Brazil Cumulative cost & RR-TB#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_strategy_cost_cumulative_DRbarrier<-data.frame(
  Runs=Brazil_baseline$Run,
  "Minimal_c"=Brazil_minimal$C_minimal_all_DOT_SAT,
  "DR96_c"=Brazil_DR96$C_DR96_all_DOT_SAT,
  "DR97_c"=Brazil_DR97$C_DR97_all_DOT_SAT,
  "DR98_c"=Brazil_DR98$C_DR98_all_DOT_SAT,
  "DR99_c"=Brazil_DR99$C_DR99_all_DOT_SAT,
  "Optimal_c"=Brazil_optimal$C_optimal_all_DOT_SAT,
  "Minimal_e"=Brazil_minimal$TB_cases_MDR,
  "DR96_e"=Brazil_DR96$TB_cases_MDR,
  "DR97_e"=Brazil_DR97$TB_cases_MDR,
  "DR98_e"=Brazil_DR98$TB_cases_MDR,
  "DR99_e"=Brazil_DR99$TB_cases_MDR,
  "Optimal_e"=Brazil_optimal$TB_cases_MDR)

Brazil_cost_pop_graph_cumulative_DRbarrier<-melt(Brazil_strategy_cost_cumulative_DRbarrier, id.vars =c("Runs"), variable.name = "cost_epi_pop")
Brazil_cost_pop_graph_cumulative_DRbarrier$strategy = ifelse(grepl("Minimal", Brazil_cost_pop_graph_cumulative_DRbarrier$cost_epi_pop), "Minimal",
                                                         ifelse(grepl("DR96", Brazil_cost_pop_graph_cumulative_DRbarrier$cost_epi_pop),"RR-96",
                                                                ifelse(grepl("DR97", Brazil_cost_pop_graph_cumulative_DRbarrier$cost_epi_pop),"RR-97",
                                                                       ifelse(grepl("DR98", Brazil_cost_pop_graph_cumulative_DRbarrier$cost_epi_pop),"RR-98",
                                                                              ifelse(grepl("DR99", Brazil_cost_pop_graph_cumulative_DRbarrier$cost_epi_pop),"RR-99",
                                                                                     "The optimal")))))
Brazil_cost_pop_graph_cumulative_DRbarrier$outcome = ifelse(grepl("_c", Brazil_cost_pop_graph_cumulative_DRbarrier$cost_epi_pop), "Cost","RR-TB")

Brazil_cost_pop_graph_cumulative_DRbarrier$cost_epi_pop = chartr("."," ",Brazil_cost_pop_graph_cumulative_DRbarrier$cost_epi_pop)

#####################################################################################################################

Brazil_cost_cumulat_DRbarrier<-aggregate(x=Brazil_cost_pop_graph_cumulative_DRbarrier$value, by=list(Runs=Brazil_cost_pop_graph_cumulative_DRbarrier$Runs,outcome=Brazil_cost_pop_graph_cumulative_DRbarrier$outcome,strategy=Brazil_cost_pop_graph_cumulative_DRbarrier$strategy), FUN=sum)

Brazil_overall_cost_table_DRbarrier<-Brazil_cost_cumulat_DRbarrier %>%
  group_by(strategy,outcome) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))

write.csv(Brazil_overall_cost_table_DRbarrier, file = "1.RR. Brazil RR barrier cumulative cost output tables.csv")

Brazil_overall_cost_table_DRbarrier2<-Brazil_cost_cumulat_DRbarrier %>%
  group_by(strategy,outcome) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.25),
                    quantile_upper = ~quantile(., probs = 0.75)))

Brazil_table_DRbarrier <- Brazil_overall_cost_table_DRbarrier2 %>% mutate( median = ifelse (outcome=="Cost", median/1000000000,
                                                                                    median/100000),
                                                                   quantile_lower = ifelse (outcome=="Cost", quantile_lower/1000000000,
                                                                                            quantile_lower/100000),
                                                                   quantile_upper = ifelse (outcome=="Cost", quantile_upper/1000000000,
                                                                                            quantile_upper/100000))


#table option 2

Brazil_cost_cumulat_graph_DRbarrier2 <- ggplot(Brazil_table_DRbarrier2, aes(y = median,x = strategy)) + 
  geom_line(aes())  +
  geom_errorbar(aes(ymin = quantile_lower, ymax = quantile_upper), width=.1) +
  geom_point(size=3)
Brazil_table_DRbarrier2
ggsave("1RR2. Brazil_graph_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_cost_cumulat_graph_DRbarrier3 <- ggplot(Brazil_table_DRbarrier, aes(y = median,x = strategy)) +
  geom_jitter(
    aes(color = outcome),
    position = position_jitter(0)
  ) + 
  geom_line(
    aes(group = outcome, color = outcome),
    data = Brazil_table_DRbarrier
  ) +
  geom_errorbar(
    aes(ymin = quantile_lower, ymax = quantile_upper, color = outcome),
    data = Brazil_table_DRbarrier, width = 0.1
  )+
  scale_y_continuous(
    limits = c(0,2),
    labels = unit_format( unit = "B", scale = 1),
    
    # Features of the first axis
    name = "Cumulative cost (US$ billions)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~./1, name="RR-TB (100,000 cases)"))+
  
  theme_classic()+
  scale_color_manual(values = c("red", "deepskyblue"))
Brazil_cost_cumulat_graph_DRbarrier3

ggsave("1RR3. Brazil_graph_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")




#####################################################################################################################
#                                   2DR. Brazil Cumulative cost & RR-TB (minimal and optimal vs 25 and 75 DR barrier                   #
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_strategy_cost_cumulative_2575DRbarrier<-data.frame(
  Runs=Brazil_baseline$Run,
  "Minimal_c"=Brazil_minimal$C_minimal_all_DOT_SAT,
  "DR25_c"=Brazil_DR25$C_DR25_all_DOT_SAT,
  "DR75_c"=Brazil_DR75$C_DR75_all_DOT_SAT,
  "Optimal_c"=Brazil_optimal$C_optimal_all_DOT_SAT,
  "Minimal_e"=Brazil_minimal$TB_cases_MDR,
  "DR25_e"=Brazil_DR25$TB_cases_MDR,
  "DR75_e"=Brazil_DR75$TB_cases_MDR,
  "Optimal_e"=Brazil_optimal$TB_cases_MDR)

Brazil_cost_pop_graph_cumulative_2575DRbarrier<-melt(Brazil_strategy_cost_cumulative_2575DRbarrier, id.vars =c("Runs"), variable.name = "cost_epi_pop")
Brazil_cost_pop_graph_cumulative_2575DRbarrier$strategy = ifelse(grepl("Minimal", Brazil_cost_pop_graph_cumulative_2575DRbarrier$cost_epi_pop), "RR-50 (minimal)",
                                                             ifelse(grepl("DR25", Brazil_cost_pop_graph_cumulative_2575DRbarrier$cost_epi_pop),"RR-25",
                                                                    ifelse(grepl("DR75", Brazil_cost_pop_graph_cumulative_2575DRbarrier$cost_epi_pop),"RR-75",
                                                                           "The Optimal")))
Brazil_cost_pop_graph_cumulative_2575DRbarrier$outcome = ifelse(grepl("_c", Brazil_cost_pop_graph_cumulative_2575DRbarrier$cost_epi_pop), "Cost","RR-TB")

Brazil_cost_pop_graph_cumulative_2575DRbarrier$cost_epi_pop = chartr("."," ",Brazil_cost_pop_graph_cumulative_2575DRbarrier$cost_epi_pop)

#####################################################################################################################

Brazil_cost_cumulat_2575DRbarrier<-aggregate(x=Brazil_cost_pop_graph_cumulative_2575DRbarrier$value, by=list(Runs=Brazil_cost_pop_graph_cumulative_2575DRbarrier$Runs,outcome=Brazil_cost_pop_graph_cumulative_2575DRbarrier$outcome,strategy=Brazil_cost_pop_graph_cumulative_2575DRbarrier$strategy), FUN=sum)

Brazil_overall_cost_table_2575DRbarrier<-Brazil_cost_cumulat_2575DRbarrier %>%
  group_by(strategy,outcome) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.755)))

write.csv(Brazil_overall_cost_table_2575DRbarrier, file = "2.RR. Brazil 25 and 75 DR barrier cumulative cost output tables.csv")

Brazil_overall_cost_table_2575DRbarrier2<-Brazil_cost_cumulat_2575DRbarrier %>%
  group_by(strategy,outcome) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.25),
                    quantile_upper = ~quantile(., probs = 0.75)))

Brazil_table_2575DRbarrier <- Brazil_overall_cost_table_2575DRbarrier2 %>% mutate( median = ifelse (outcome=="Cost", median/1000000000,
                                                                                            median/100000),
                                                                           quantile_lower = ifelse (outcome=="Cost", quantile_lower/1000000000,
                                                                                                    quantile_lower/100000),
                                                                           quantile_upper = ifelse (outcome=="Cost", quantile_upper/1000000000,
                                                                                                    quantile_upper/100000))

#table option 2


Brazil_cost_cumulat_graph_2575DRbarrier3 <- ggplot(Brazil_table_2575DRbarrier, aes(y = median,x = strategy)) +
  geom_jitter(
    aes(color = outcome),
    position = position_jitter(0)
  ) + 
  geom_line(
    aes(group = outcome, color = outcome),
    data = Brazil_table_2575DRbarrier
  ) +
  geom_errorbar(
    aes(ymin = quantile_lower, ymax = quantile_upper, color = outcome),
    data = Brazil_table_2575DRbarrier, width = 0.1
  )+
  scale_y_continuous(
    limits = c(0,2),
    labels = unit_format( unit = "B", scale = 1),
    
    # Features of the first axis
    name = "Cumulative cost (US$ billions)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~./1, name="RR-TB (100,000 cases)"))+
  
  theme_classic()+
  scale_color_manual(values = c("red", "deepskyblue"))
Brazil_cost_cumulat_graph_2575DRbarrier3

ggsave("2RR3. Brazil_graph_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



# Combine RR-TB sensitivity analysis graphs
Brazil_DRTB_combine<-ggarrange(Brazil_cost_cumulat_graph_DRbarrier3, Brazil_cost_cumulat_graph_2575DRbarrier3, labels = c("RR barrier of 96-99%", "RR barrier of 25 and 75%"),
                           common.legend = TRUE, legend = "bottom")
Brazil_DRTB_combine
ggsave("3RR. Brazil_combined_graphs_DRTB.png", 
       width = 30, height = 20, units = "cm")

#####################################################################################################################

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### PREP DATA FOR ANALYSIS/PLOTTING #######
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#Melt data in key packets for plotting
SA_strategy_cost<-data.frame(
  Year=SA_baseline$Year,
  Baseline=SA_baseline$C_baseline_all_DOT_SAT,
  Baseline_HIV_neg=SA_baseline$C_baseline_hiv_neg_DOT_SAT,
  Baseline_PLHIV=SA_baseline$C_baseline_hiv_DOT_SAT,
  "Expanded 6H"=SA_scaleup$C_scaleup_all_DOT_SAT,
  "Expanded 6H_neg"=SA_scaleup$C_scaleup_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=SA_scaleup$C_scaleup_hiv_DOT_SAT,
  "Minimal regimen"=SA_minimal$C_minimal_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=SA_minimal$C_minimal_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=SA_minimal$C_minimal_hiv_DOT_SAT,
  "Optimal regimen"=SA_optimal$C_optimal_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=SA_optimal$C_optimal_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=SA_optimal$C_optimal_hiv_DOT_SAT)
SA_cost_pop_graph<-melt(SA_strategy_cost, id.vars =c("Year"), variable.name = "cost_pop")
SA_cost_pop_graph$cum_cost<-ave(SA_cost_pop_graph$value, SA_cost_pop_graph$cost_pop, FUN=cumsum)
SA_cost_pop_graph$Year<- as.Date(ISOdate(SA_cost_pop_graph$Year,1,1) ) #set up date variable
SA_cost_pop_graph$strategy = ifelse(grepl("Baseline", SA_cost_pop_graph$cost_pop), "_baseline",
                                    ifelse(grepl("6H", SA_cost_pop_graph$cost_pop),"6H",ifelse(grepl("Minimal", SA_cost_pop_graph$cost_pop), "minimal","Optimal")))
SA_cost_pop_graph$population = ifelse(!grepl("_", SA_cost_pop_graph$cost_pop), "All treatment candidates",
                                      ifelse(grepl("PLHIV", SA_cost_pop_graph$cost_pop),"PLHIV","HHC (HIV negatives)"))
SA_cost_pop_graph$cost_pop = chartr("."," ",SA_cost_pop_graph$cost_pop)

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### PLOTS #######
#####################################################################################################################
#####################################################################################################################


#####################################################################################################################
#Annual cost per population group#
#####################################################################################################################


####################
SA_graph_annual_costs <- ggplot(SA_cost_pop_graph,aes(Year,value))+
  scale_color_brewer(palette = "Set1")+
  theme_classic()
# annual costs graph (not smoothed out)
SA_graph_annual_costs+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_summary(fun = mean,geom = "line",size = 1,aes(colour = strategy))+
  labs(x = "Years",y = expression(paste("Annual cost (US$)")),colour = "")
ggsave("SA_graph_annual_cost.jpeg", 
       width = 25, height = 16, units = "cm")

# smoothed annual costs graph

SA_graph_annual_costs+
  #  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE) +
  # geom_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE)+
  labs(x = "Years",y = expression(paste("Annual cost (US$)")),colour = "")
ggsave("SA_graph_annual_costs(smoothed).jpeg", 
       width = 25, height = 16, units = "cm")




















#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#                                   Cumulative epi & cost  per population group#
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#####################################################################################################################
#                                   1A. Cumulative PT & TB cost per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
SA_strategy_cost_cumulative<-data.frame(
  Runs=SA_baseline$Run,
  Baseline=SA_baseline$C_baseline_all_DOT_SAT,
  Baseline_HIV_neg=SA_baseline$C_baseline_hiv_neg_DOT_SAT,
  Baseline_PLHIV=SA_baseline$C_baseline_hiv_DOT_SAT,
  "Expanded 6H"=SA_scaleup$C_scaleup_all_DOT_SAT,
  "Expanded 6H_neg"=SA_scaleup$C_scaleup_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=SA_scaleup$C_scaleup_hiv_DOT_SAT,
  "Minimal regimen"=SA_minimal$C_minimal_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=SA_minimal$C_minimal_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=SA_minimal$C_minimal_hiv_DOT_SAT,
  "Optimal regimen"=SA_optimal$C_optimal_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=SA_optimal$C_optimal_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=SA_optimal$C_optimal_hiv_DOT_SAT)

SA_cost_pop_graph_cumulative<-melt(SA_strategy_cost_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
SA_cost_pop_graph_cumulative$cum_cost<-ave(SA_cost_pop_graph_cumulative$value, SA_cost_pop_graph_cumulative$cost_pop, FUN=cumsum)
# SA_cost_pop_graph_cumulative$Year<- as.Date(ISOdate(SA_cost_pop_graph_cumulative$Year,1,1) ) #set up date variable
SA_cost_pop_graph_cumulative$strategy = ifelse(grepl("Baseline", SA_cost_pop_graph_cumulative$cost_pop), "_baseline",
                                               ifelse(grepl("6H", SA_cost_pop_graph_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", SA_cost_pop_graph_cumulative$cost_pop), "minimal","Optimal")))
SA_cost_pop_graph_cumulative$population = ifelse(!grepl("_", SA_cost_pop_graph_cumulative$cost_pop), "All treatment candidates",
                                                 ifelse(grepl("PLHIV", SA_cost_pop_graph_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
SA_cost_pop_graph_cumulative$cost_pop = chartr("."," ",SA_cost_pop_graph_cumulative$cost_pop)

#####################################################################################################################

SA_cost_cumulat<-aggregate(x=SA_cost_pop_graph_cumulative$value, by=list(Runs=SA_cost_pop_graph_cumulative$Runs,cost_pop=SA_cost_pop_graph_cumulative$cost_pop,strategy=SA_cost_pop_graph_cumulative$strategy,population=SA_cost_pop_graph_cumulative$population), FUN=sum)
#cumulative cost graph
SA_cost_cumulat_graph <- ggplot(data=SA_cost_cumulat, aes(cost_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative cost (US$ Billions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("1A. SA_graph_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



SA_overall_cost_table<-SA_cost_cumulat %>% 
  group_by(cost_pop, strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(SA_overall_cost_table, file = "1A. South African cumulative cost output tables.csv")



#####################################################################################################################
#                                   1B. Cumulative TB cost per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
SA_strategy_cost_TB_cumulative<-data.frame(
  Runs=SA_baseline$Run,
  Baseline=SA_baseline$C_TB_all_DOT_SAT,
  Baseline_HIV_neg=SA_baseline$C_TB_hiv_neg_DOT_SAT,
  Baseline_PLHIV=SA_baseline$C_TB_hiv_DOT_SAT,
  "Expanded 6H"=SA_scaleup$C_TB_all_DOT_SAT,
  "Expanded 6H_neg"=SA_scaleup$C_TB_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=SA_scaleup$C_TB_hiv_DOT_SAT,
  "Minimal regimen"=SA_minimal$C_TB_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=SA_minimal$C_TB_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=SA_minimal$C_TB_hiv_DOT_SAT,
  "Optimal regimen"=SA_optimal$C_TB_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=SA_optimal$C_TB_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=SA_optimal$C_TB_hiv_DOT_SAT)

SA_cost_pop_graph_TB_cumulative<-melt(SA_strategy_cost_TB_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
SA_cost_pop_graph_TB_cumulative$cum_cost<-ave(SA_cost_pop_graph_TB_cumulative$value, SA_cost_pop_graph_TB_cumulative$cost_pop, FUN=cumsum)
# SA_cost_pop_graph_TB_cumulative$Year<- as.Date(ISOdate(SA_cost_pop_graph_TB_cumulative$Year,1,1) ) #set up date variable
SA_cost_pop_graph_TB_cumulative$strategy = ifelse(grepl("Baseline", SA_cost_pop_graph_TB_cumulative$cost_pop), "_baseline",
                                                  ifelse(grepl("6H", SA_cost_pop_graph_TB_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", SA_cost_pop_graph_TB_cumulative$cost_pop), "minimal","Optimal")))
SA_cost_pop_graph_TB_cumulative$population = ifelse(!grepl("_", SA_cost_pop_graph_TB_cumulative$cost_pop), "All treatment candidates",
                                                    ifelse(grepl("PLHIV", SA_cost_pop_graph_TB_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
SA_cost_pop_graph_TB_cumulative$cost_pop = chartr("."," ",SA_cost_pop_graph_TB_cumulative$cost_pop)

#####################################################################################################################

SA_cost_TB_cumulat<-aggregate(x=SA_cost_pop_graph_TB_cumulative$value, by=list(Runs=SA_cost_pop_graph_TB_cumulative$Runs,cost_pop=SA_cost_pop_graph_TB_cumulative$cost_pop,strategy=SA_cost_pop_graph_TB_cumulative$strategy,population=SA_cost_pop_graph_TB_cumulative$population), FUN=sum)
#cumulative cost graph
SA_cost_TB_cumulat_graph <- ggplot(data=SA_cost_TB_cumulat, aes(cost_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cost of diagnosis and treatment of TB (US$ Billions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("1B. SA_graph_TB_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



SA_overall_cost_table<-SA_cost_TB_cumulat %>% 
  group_by(cost_pop, strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(SA_overall_cost_table, file = "1B. South African cumulative cost TB dig&tx output tables.csv")



#####################################################################################################################
#                                   1C. Cumulative RR-TB cost per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
SA_strategy_cost_MDRTB_cumulative<-data.frame(
  Runs=SA_baseline$Run,
  Baseline=SA_baseline$C_MDRTB_all_DOT_SAT,
  Baseline_HIV_neg=SA_baseline$C_MDRTB_hiv_neg_DOT_SAT,
  Baseline_PLHIV=SA_baseline$C_MDRTB_hiv_DOT_SAT,
  "Expanded 6H"=SA_scaleup$C_MDRTB_all_DOT_SAT,
  "Expanded 6H_neg"=SA_scaleup$C_MDRTB_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=SA_scaleup$C_MDRTB_hiv_DOT_SAT,
  "Minimal regimen"=SA_minimal$C_MDRTB_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=SA_minimal$C_MDRTB_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=SA_minimal$C_MDRTB_hiv_DOT_SAT,
  "Optimal regimen"=SA_optimal$C_MDRTB_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=SA_optimal$C_MDRTB_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=SA_optimal$C_MDRTB_hiv_DOT_SAT)

SA_cost_pop_graph_MDRTB_cumulative<-melt(SA_strategy_cost_MDRTB_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
SA_cost_pop_graph_MDRTB_cumulative$cum_cost<-ave(SA_cost_pop_graph_MDRTB_cumulative$value, SA_cost_pop_graph_MDRTB_cumulative$cost_pop, FUN=cumsum)
# SA_cost_pop_graph_MDRTB_cumulative$Year<- as.Date(ISOdate(SA_cost_pop_graph_MDRTB_cumulative$Year,1,1) ) #set up date variable
SA_cost_pop_graph_MDRTB_cumulative$strategy = ifelse(grepl("Baseline", SA_cost_pop_graph_MDRTB_cumulative$cost_pop), "_baseline",
                                                     ifelse(grepl("6H", SA_cost_pop_graph_MDRTB_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", SA_cost_pop_graph_MDRTB_cumulative$cost_pop), "minimal","Optimal")))
SA_cost_pop_graph_MDRTB_cumulative$population = ifelse(!grepl("_", SA_cost_pop_graph_MDRTB_cumulative$cost_pop), "All treatment candidates",
                                                       ifelse(grepl("PLHIV", SA_cost_pop_graph_MDRTB_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
SA_cost_pop_graph_MDRTB_cumulative$cost_pop = chartr("."," ",SA_cost_pop_graph_MDRTB_cumulative$cost_pop)

#####################################################################################################################

SA_cost_MDRTB_cumulat<-aggregate(x=SA_cost_pop_graph_MDRTB_cumulative$value, by=list(Runs=SA_cost_pop_graph_MDRTB_cumulative$Runs,cost_pop=SA_cost_pop_graph_MDRTB_cumulative$cost_pop,strategy=SA_cost_pop_graph_MDRTB_cumulative$strategy,population=SA_cost_pop_graph_MDRTB_cumulative$population), FUN=sum)
#cumulative cost graph
SA_cost_MDRTB_cumulat_graph <- ggplot(data=SA_cost_MDRTB_cumulat, aes(cost_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cost of diagnosis and treatment of  RR-TB (US$ Billions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("1C. SA_graph_RR-TB_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



SA_overall_cost_table<-SA_cost_MDRTB_cumulat %>% 
  group_by(cost_pop, strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(SA_overall_cost_table, file = "1C. South African cumulative cost RR-TB dig&tx output tables.csv")




#####################################################################################################################
#                                   1D. Cumulative PT - cost per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
SA_strategy_PT_cost_cumulative<-data.frame(
  Runs=SA_baseline$Run,
  Baseline=SA_baseline$C_PT_all,
  Baseline_HIV_neg=SA_baseline$C_PT_hiv_neg,
  Baseline_PLHIV=SA_baseline$C_PT_hiv,
  "Expanded 6H"=SA_scaleup$C_PT_all,
  "Expanded 6H_neg"=SA_scaleup$C_PT_hiv_neg,
  "Expanded 6H_PLHIV"=SA_scaleup$C_PT_hiv,
  "Minimal regimen"=SA_minimal$C_PT_all,
  "Minimal regimen_HIV_neg"=SA_minimal$C_PT_hiv_neg,
  "Minimal regimen_PLHIV"=SA_minimal$C_PT_hiv,
  "Optimal regimen"=SA_optimal$C_PT_all,
  "Optimal regimen_HIV_neg"=SA_optimal$C_PT_hiv_neg,
  "Optimal regimen_PLHIV"=SA_optimal$C_PT_hiv)

SA_PT_cost_pop_graph_cumulative<-melt(SA_strategy_PT_cost_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
SA_PT_cost_pop_graph_cumulative$strategy = ifelse(grepl("Baseline", SA_PT_cost_pop_graph_cumulative$cost_pop), "_baseline",
                                                  ifelse(grepl("6H", SA_PT_cost_pop_graph_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", SA_PT_cost_pop_graph_cumulative$cost_pop), "minimal","Optimal")))
SA_PT_cost_pop_graph_cumulative$population = ifelse(!grepl("_", SA_PT_cost_pop_graph_cumulative$cost_pop), "All treatment candidates",
                                                    ifelse(grepl("PLHIV", SA_PT_cost_pop_graph_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
SA_PT_cost_pop_graph_cumulative$cost_pop = chartr("."," ",SA_PT_cost_pop_graph_cumulative$cost_pop)

#####################################################################################################################

SA_PT_cost_cumulat<-aggregate(x=SA_PT_cost_pop_graph_cumulative$value, by=list(Runs=SA_PT_cost_pop_graph_cumulative$Runs,cost_pop=SA_PT_cost_pop_graph_cumulative$cost_pop,strategy=SA_PT_cost_pop_graph_cumulative$strategy,population=SA_PT_cost_pop_graph_cumulative$population), FUN=sum)
#cumulative cost graph
SA_PT_cost_cumulat_graph <- ggplot(data=SA_PT_cost_cumulat, aes(cost_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative PT cost (US$ Millions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("1D. SA_PT_graph_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



SA_overall_cost_table<-SA_PT_cost_cumulat %>% 
  group_by(cost_pop, strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(SA_overall_cost_table, file = "1D. South African cumulative PT cost output tables.csv")



#####################################################################################################################
#                                  2A. Cumulative overall DS-TB per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
SA_epi_cumulative_DSTB<-data.frame(
  Runs=SA_baseline$Run,
  Baseline=SA_baseline$TB_cases_all-SA_baseline$TB_cases_MDR,
  Baseline_HIV_neg=(SA_baseline$TB_cases_all-SA_baseline$TB_cases_MDR)-(SA_baseline$TB_cases_HIV-SA_baseline$TB_cases_MDR_HIV),
  Baseline_PLHIV=SA_baseline$TB_cases_HIV-SA_baseline$TB_cases_MDR_HIV,
  "Expanded 6H"=SA_scaleup$TB_cases_all-SA_scaleup$TB_cases_MDR,
  "Expanded 6H_neg"=(SA_scaleup$TB_cases_all-SA_scaleup$TB_cases_MDR)-(SA_scaleup$TB_cases_HIV-SA_scaleup$TB_cases_MDR_HIV),
  "Expanded 6H_PLHIV"=SA_scaleup$TB_cases_HIV-SA_scaleup$TB_cases_MDR_HIV,
  "Minimal regimen"=SA_minimal$TB_cases_all-SA_minimal$TB_cases_MDR,
  "Minimal regimen_HIV_neg"=(SA_minimal$TB_cases_all-SA_minimal$TB_cases_MDR)-(SA_minimal$TB_cases_HIV-SA_minimal$TB_cases_MDR_HIV),
  "Minimal regimen_PLHIV"=SA_minimal$TB_cases_HIV-SA_minimal$TB_cases_MDR_HIV,
  "Optimal regimen"=SA_optimal$TB_cases_all-SA_optimal$TB_cases_MDR,
  "Optimal regimen_HIV_neg"=(SA_optimal$TB_cases_all-SA_optimal$TB_cases_MDR)-(SA_optimal$TB_cases_HIV-SA_optimal$TB_cases_MDR_HIV),
  "Optimal regimen_PLHIV"=SA_optimal$TB_cases_HIV-SA_optimal$TB_cases_MDR_HIV)

SA_epi_cumulative_DSTB_graph<-melt(SA_epi_cumulative_DSTB, id.vars =c("Runs"), variable.name = "epi_pop")
SA_epi_cumulative_DSTB_graph$strategy = ifelse(grepl("Baseline", SA_epi_cumulative_DSTB_graph$epi_pop), "_baseline",
                                               ifelse(grepl("6H", SA_epi_cumulative_DSTB_graph$epi_pop),"6H",ifelse(grepl("Minimal", SA_epi_cumulative_DSTB_graph$epi_pop), "minimal","Optimal")))
SA_epi_cumulative_DSTB_graph$population = ifelse(!grepl("_", SA_epi_cumulative_DSTB_graph$epi_pop), "All treatment candidates",
                                                 ifelse(grepl("PLHIV", SA_epi_cumulative_DSTB_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
SA_epi_cumulative_DSTB_graph$epi_pop = chartr("."," ",SA_epi_cumulative_DSTB_graph$epi_pop)

#####################################################################################################################

SA_epi_DSTB_cumulat<-aggregate(x=SA_epi_cumulative_DSTB_graph$value, by=list(Runs=SA_epi_cumulative_DSTB_graph$Runs,epi_pop=SA_epi_cumulative_DSTB_graph$epi_pop,strategy=SA_epi_cumulative_DSTB_graph$strategy,population=SA_epi_cumulative_DSTB_graph$population), FUN=sum)
#cumulative epi graph
SA_epi_DSTB_cumulat_graph <- ggplot(data=SA_epi_DSTB_cumulat, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative TB cases  (DS-TB)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("2A. Epidemiological projections - DS-TB cases, South Africa, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



SA_overall_epi_DSTB_table<-SA_epi_DSTB_cumulat %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(SA_overall_epi_DSTB_table, file = "2A. South African - cumulative DS-TB cases tables.csv")






#####################################################################################################################
#                                  2B. Cumulative overall TB per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
SA_epi_cumulative_TBall<-data.frame(
  Runs=SA_baseline$Run,
  Baseline=SA_baseline$TB_cases_all,
  Baseline_HIV_neg=SA_baseline$TB_cases_all-SA_baseline$TB_cases_HIV,
  Baseline_PLHIV=SA_baseline$TB_cases_HIV,
  "Expanded 6H"=SA_scaleup$TB_cases_all,
  "Expanded 6H_neg"=SA_scaleup$TB_cases_all-SA_scaleup$TB_cases_HIV,
  "Expanded 6H_PLHIV"=SA_scaleup$TB_cases_HIV,
  "Minimal regimen"=SA_minimal$TB_cases_all,
  "Minimal regimen_HIV_neg"=SA_minimal$TB_cases_all-SA_minimal$TB_cases_HIV,
  "Minimal regimen_PLHIV"=SA_minimal$TB_cases_HIV,
  "Optimal regimen"=SA_optimal$TB_cases_all,
  "Optimal regimen_HIV_neg"=SA_optimal$TB_cases_all-SA_optimal$TB_cases_HIV,
  "Optimal regimen_PLHIV"=SA_optimal$TB_cases_HIV)

SA_epi_cumulative_TBall_graph<-melt(SA_epi_cumulative_TBall, id.vars =c("Runs"), variable.name = "epi_pop")
SA_epi_cumulative_TBall_graph$strategy = ifelse(grepl("Baseline", SA_epi_cumulative_TBall_graph$epi_pop), "_baseline",
                                                ifelse(grepl("6H", SA_epi_cumulative_TBall_graph$epi_pop),"6H",ifelse(grepl("Minimal", SA_epi_cumulative_TBall_graph$epi_pop), "minimal","Optimal")))
SA_epi_cumulative_TBall_graph$population = ifelse(!grepl("_", SA_epi_cumulative_TBall_graph$epi_pop), "All treatment candidates",
                                                  ifelse(grepl("PLHIV", SA_epi_cumulative_TBall_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
SA_epi_cumulative_TBall_graph$epi_pop = chartr("."," ",SA_epi_cumulative_TBall_graph$epi_pop)

#####################################################################################################################

SA_epi_cumulat<-aggregate(x=SA_epi_cumulative_TBall_graph$value, by=list(Runs=SA_epi_cumulative_TBall_graph$Runs,epi_pop=SA_epi_cumulative_TBall_graph$epi_pop,strategy=SA_epi_cumulative_TBall_graph$strategy,population=SA_epi_cumulative_TBall_graph$population), FUN=sum)
#cumulative epi graph
SA_epi_cumulat_graph <- ggplot(data=SA_epi_cumulat, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative TB cases  (DS-TB & RR-TB)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("2B. Epidemiological projections, South Africa, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



SA_overall_epi_table<-SA_epi_cumulat %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(SA_overall_epi_table, file = "2B. South African - cumulative TB cases (DS-TB & RR-TB) tables.csv")





#####################################################################################################################
#                                  2C. Cumulative RR-TB per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
SA_epi_cumulative_DRTB<-data.frame(
  Runs=SA_baseline$Run,
  Baseline=SA_baseline$TB_cases_MDR,
  Baseline_HIV_neg=SA_baseline$TB_cases_MDR-SA_baseline$TB_cases_MDR_HIV,
  Baseline_PLHIV=SA_baseline$TB_cases_MDR_HIV,
  "Expanded 6H"=SA_scaleup$TB_cases_MDR,
  "Expanded 6H_neg"=SA_scaleup$TB_cases_MDR-SA_scaleup$TB_cases_MDR_HIV,
  "Expanded 6H_PLHIV"=SA_scaleup$TB_cases_MDR_HIV,
  "Minimal regimen"=SA_minimal$TB_cases_MDR,
  "Minimal regimen_HIV_neg"=SA_minimal$TB_cases_MDR-SA_minimal$TB_cases_MDR_HIV,
  "Minimal regimen_PLHIV"=SA_minimal$TB_cases_MDR_HIV,
  "Optimal regimen"=SA_optimal$TB_cases_MDR,
  "Optimal regimen_HIV_neg"=SA_optimal$TB_cases_MDR-SA_optimal$TB_cases_MDR_HIV,
  "Optimal regimen_PLHIV"=SA_optimal$TB_cases_MDR_HIV)

SA_epi_cumulative_DRTB_graph<-melt(SA_epi_cumulative_DRTB, id.vars =c("Runs"), variable.name = "epi_pop")
SA_epi_cumulative_DRTB_graph$strategy = ifelse(grepl("Baseline", SA_epi_cumulative_DRTB_graph$epi_pop), "_baseline",
                                               ifelse(grepl("6H", SA_epi_cumulative_DRTB_graph$epi_pop),"6H",ifelse(grepl("Minimal", SA_epi_cumulative_DRTB_graph$epi_pop), "minimal","Optimal")))
SA_epi_cumulative_DRTB_graph$population = ifelse(!grepl("_", SA_epi_cumulative_DRTB_graph$epi_pop), "All treatment candidates",
                                                 ifelse(grepl("PLHIV", SA_epi_cumulative_DRTB_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
SA_epi_cumulative_DRTB_graph$epi_pop = chartr("."," ",SA_epi_cumulative_DRTB_graph$epi_pop)

#####################################################################################################################

SA_epi_cumulat_DRTB<-aggregate(x=SA_epi_cumulative_DRTB_graph$value, by=list(Runs=SA_epi_cumulative_DRTB_graph$Runs,epi_pop=SA_epi_cumulative_DRTB_graph$epi_pop,strategy=SA_epi_cumulative_DRTB_graph$strategy,population=SA_epi_cumulative_DRTB_graph$population), FUN=sum)
#cumulative epi graph
SA_epi_cumulat_graph_DRTB <- ggplot(data=SA_epi_cumulat_DRTB, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative RR-TB cases")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("2C. RR-TB Epidemiological projections, South Africa, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



SA_overall_DRTB_epi_table<-SA_epi_cumulat_DRTB %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(SA_overall_DRTB_epi_table, file = "2C. South African - cumulative RR-TB cases tables.csv")





#####################################################################################################################
#                                  2D. Cumulative overall PT per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
SA_epi_cumulative_PT<-data.frame(
  Runs=SA_baseline$Run,
  Baseline=SA_baseline$New_PT_all,
  Baseline_HIV_neg=SA_baseline$New_PT_all-SA_baseline$New_PT_HIV,
  Baseline_PLHIV=SA_baseline$New_PT_HIV,
  "Expanded 6H"=SA_scaleup$New_PT_all,
  "Expanded 6H_neg"=SA_scaleup$New_PT_all-SA_scaleup$New_PT_HIV,
  "Expanded 6H_PLHIV"=SA_scaleup$New_PT_HIV,
  "Minimal regimen"=SA_minimal$New_PT_all,
  "Minimal regimen_HIV_neg"=SA_minimal$New_PT_all-SA_minimal$New_PT_HIV,
  "Minimal regimen_PLHIV"=SA_minimal$New_PT_HIV,
  "Optimal regimen"=SA_optimal$New_PT_all,
  "Optimal regimen_HIV_neg"=SA_optimal$New_PT_all-SA_optimal$New_PT_HIV,
  "Optimal regimen_PLHIV"=SA_optimal$New_PT_HIV)

SA_epi_cumulative_PT_graph<-melt(SA_epi_cumulative_PT, id.vars =c("Runs"), variable.name = "epi_pop")
SA_epi_cumulative_PT_graph$strategy = ifelse(grepl("Baseline", SA_epi_cumulative_PT_graph$epi_pop), "_baseline",
                                             ifelse(grepl("6H", SA_epi_cumulative_PT_graph$epi_pop),"6H",ifelse(grepl("Minimal", SA_epi_cumulative_PT_graph$epi_pop), "minimal","Optimal")))
SA_epi_cumulative_PT_graph$population = ifelse(!grepl("_", SA_epi_cumulative_PT_graph$epi_pop), "All treatment candidates",
                                               ifelse(grepl("PLHIV", SA_epi_cumulative_PT_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
SA_epi_cumulative_PT_graph$epi_pop = chartr("."," ",SA_epi_cumulative_PT_graph$epi_pop)

#####################################################################################################################

SA_PT_epi_cumulat<-aggregate(x=SA_epi_cumulative_PT_graph$value, by=list(Runs=SA_epi_cumulative_PT_graph$Runs,epi_pop=SA_epi_cumulative_PT_graph$epi_pop,strategy=SA_epi_cumulative_PT_graph$strategy,population=SA_epi_cumulative_PT_graph$population), FUN=sum)
#cumulative epi graph
SA_PT_epi_cumulat_graph <- ggplot(data=SA_PT_epi_cumulat, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("Individuals on preventive therapy, 2020–2035")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("2D. Epidemiological PT projections, South Africa, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



SA_overall_PT_epi_table<-SA_PT_epi_cumulat %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(SA_overall_PT_epi_table, file = "2D. South African - Total individuals on preventive therapy.csv")






#####################################################################################################################
#                                  3. Cumulative TB deaths per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
SA_epi_cumulative_TBdeaths<-data.frame(
  Runs=SA_baseline$Run,
  Baseline=SA_baseline$TB_deaths_all,
  Baseline_HIV_neg=SA_baseline$TB_deaths_all-SA_baseline$TB_deaths_HIV,
  Baseline_PLHIV=SA_baseline$TB_deaths_HIV,
  "Expanded 6H"=SA_scaleup$TB_deaths_all,
  "Expanded 6H_neg"=SA_scaleup$TB_deaths_all-SA_scaleup$TB_deaths_HIV,
  "Expanded 6H_PLHIV"=SA_scaleup$TB_deaths_HIV,
  "Minimal regimen"=SA_minimal$TB_deaths_all,
  "Minimal regimen_HIV_neg"=SA_minimal$TB_deaths_all-SA_minimal$TB_deaths_HIV,
  "Minimal regimen_PLHIV"=SA_minimal$TB_deaths_HIV,
  "Optimal regimen"=SA_optimal$TB_deaths_all,
  "Optimal regimen_HIV_neg"=SA_optimal$TB_deaths_all-SA_optimal$TB_deaths_HIV,
  "Optimal regimen_PLHIV"=SA_optimal$TB_deaths_HIV)

SA_epi_cumulative_TBdeaths_graph<-melt(SA_epi_cumulative_TBdeaths, id.vars =c("Runs"), variable.name = "epi_pop")
SA_epi_cumulative_TBdeaths_graph$strategy = ifelse(grepl("Baseline", SA_epi_cumulative_TBdeaths_graph$epi_pop), "_baseline",
                                                   ifelse(grepl("6H", SA_epi_cumulative_TBdeaths_graph$epi_pop),"6H",ifelse(grepl("Minimal", SA_epi_cumulative_TBdeaths_graph$epi_pop), "minimal","Optimal")))
SA_epi_cumulative_TBdeaths_graph$population = ifelse(!grepl("_", SA_epi_cumulative_TBdeaths_graph$epi_pop), "All treatment candidates",
                                                     ifelse(grepl("PLHIV", SA_epi_cumulative_TBdeaths_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
SA_epi_cumulative_TBdeaths_graph$epi_pop = chartr("."," ",SA_epi_cumulative_TBdeaths_graph$epi_pop)

#####################################################################################################################

SA_epi_cumulat_TBdeaths<-aggregate(x=SA_epi_cumulative_TBdeaths_graph$value, by=list(Runs=SA_epi_cumulative_TBdeaths_graph$Runs,epi_pop=SA_epi_cumulative_TBdeaths_graph$epi_pop,strategy=SA_epi_cumulative_TBdeaths_graph$strategy,population=SA_epi_cumulative_TBdeaths_graph$population), FUN=sum)
#cumulative epi graph
SA_epi_cumulat_graph_TBdeaths <- ggplot(data=SA_epi_cumulat_TBdeaths, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("TB deaths")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("3. TB deaths Epidemiological projections, South Africa, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



SA_overall_TBdeaths_epi_table<-SA_epi_cumulat_TBdeaths %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(SA_overall_TBdeaths_epi_table, file = "3. South African - cumulative TB deaths tables.csv")








#####################################################################################################################
#                                  4. Cumulative DALYs per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
SA_epi_cumulative_DALYs<-data.frame(
  Runs=SA_baseline$Run,
  Baseline=SA_baseline$YLD+SA_baseline$YLL,
  Baseline_HIV_neg=(SA_baseline$YLD+SA_baseline$YLL)-(SA_baseline$YLD_HIV+SA_baseline$YLL_HIV),
  Baseline_PLHIV=SA_baseline$YLD_HIV+SA_baseline$YLL_HIV,
  "Expanded 6H"=SA_scaleup$YLD+SA_scaleup$YLL,
  "Expanded 6H_neg"=(SA_scaleup$YLD+SA_scaleup$YLL)-(SA_scaleup$YLD_HIV+SA_scaleup$YLL_HIV),
  "Expanded 6H_PLHIV"=SA_scaleup$YLD_HIV+SA_scaleup$YLL_HIV,
  "Minimal regimen"=SA_minimal$YLD+SA_minimal$YLL,
  "Minimal regimen_HIV_neg"=(SA_minimal$YLD+SA_minimal$YLL)-(SA_minimal$YLD_HIV+SA_minimal$YLL_HIV),
  "Minimal regimen_PLHIV"=SA_minimal$YLD_HIV+SA_minimal$YLL_HIV,
  "Optimal regimen"=SA_optimal$YLD+SA_optimal$YLL,
  "Optimal regimen_HIV_neg"=(SA_optimal$YLD+SA_optimal$YLL)-(SA_optimal$YLD_HIV+SA_optimal$YLL_HIV),
  "Optimal regimen_PLHIV"=SA_optimal$YLD_HIV+SA_optimal$YLL_HIV)

SA_epi_cumulative_DALYs_graph<-melt(SA_epi_cumulative_DALYs, id.vars =c("Runs"), variable.name = "epi_pop")
SA_epi_cumulative_DALYs_graph$strategy = ifelse(grepl("Baseline", SA_epi_cumulative_DALYs_graph$epi_pop), "_baseline",
                                                ifelse(grepl("6H", SA_epi_cumulative_DALYs_graph$epi_pop),"6H",ifelse(grepl("Minimal", SA_epi_cumulative_DALYs_graph$epi_pop), "minimal","Optimal")))
SA_epi_cumulative_DALYs_graph$population = ifelse(!grepl("_", SA_epi_cumulative_DALYs_graph$epi_pop), "All treatment candidates",
                                                  ifelse(grepl("PLHIV", SA_epi_cumulative_DALYs_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
SA_epi_cumulative_DALYs_graph$epi_pop = chartr("."," ",SA_epi_cumulative_DALYs_graph$epi_pop)

#####################################################################################################################

SA_epi_cumulat_DALYs<-aggregate(x=SA_epi_cumulative_DALYs_graph$value, by=list(Runs=SA_epi_cumulative_DALYs_graph$Runs,epi_pop=SA_epi_cumulative_DALYs_graph$epi_pop,strategy=SA_epi_cumulative_DALYs_graph$strategy,population=SA_epi_cumulative_DALYs_graph$population), FUN=sum)
#cumulative epi graph
SA_epi_cumulat_graph_DALYs <- ggplot(data=SA_epi_cumulat_DALYs, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("DALYs")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("4. DALYs - Epidemiological projections, South Africa, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



SA_overall_DALYs_epi_table<-SA_epi_cumulat_DALYs %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(SA_overall_DALYs_epi_table, file = "4. South African - cumulative DALYs tables.csv")





#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#                                     Incremental cost                                                              #
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
SA_strategy_cost_cumulative<-data.frame(
  Runs=SA_baseline$Run,
  Baseline=SA_baseline$C_baseline_all_DOT_SAT,
  Baseline_HIV_neg=SA_baseline$C_baseline_hiv_neg_DOT_SAT,
  Baseline_PLHIV=SA_baseline$C_baseline_hiv_DOT_SAT,
  "Expanded 6H"=SA_scaleup$C_scaleup_all_DOT_SAT,
  "Expanded 6H_neg"=SA_scaleup$C_scaleup_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=SA_scaleup$C_scaleup_hiv_DOT_SAT,
  "Minimal regimen"=SA_minimal$C_minimal_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=SA_minimal$C_minimal_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=SA_minimal$C_minimal_hiv_DOT_SAT,
  "Optimal regimen"=SA_optimal$C_optimal_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=SA_optimal$C_optimal_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=SA_optimal$C_optimal_hiv_DOT_SAT)

SA_cost_pop_graph_cumulative<-melt(SA_strategy_cost_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
SA_cost_pop_graph_cumulative$cum_cost<-ave(SA_cost_pop_graph_cumulative$value, SA_cost_pop_graph_cumulative$cost_pop, FUN=cumsum)
# SA_cost_pop_graph_cumulative$Year<- as.Date(ISOdate(SA_cost_pop_graph_cumulative$Year,1,1) ) #set up date variable
SA_cost_pop_graph_cumulative$strategy = ifelse(grepl("Baseline", SA_cost_pop_graph_cumulative$cost_pop), "_baseline",
                                               ifelse(grepl("6H", SA_cost_pop_graph_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", SA_cost_pop_graph_cumulative$cost_pop), "minimal","Optimal")))
SA_cost_pop_graph_cumulative$population = ifelse(!grepl("_", SA_cost_pop_graph_cumulative$cost_pop), "All treatment candidates",
                                                 ifelse(grepl("PLHIV", SA_cost_pop_graph_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
SA_cost_pop_graph_cumulative$cost_pop = chartr("."," ",SA_cost_pop_graph_cumulative$cost_pop)

#####################################################################################################################

SA_cost_cumulat<-aggregate(x=SA_cost_pop_graph_cumulative$value, by=list(Runs=SA_cost_pop_graph_cumulative$Runs,cost_pop=SA_cost_pop_graph_cumulative$cost_pop,strategy=SA_cost_pop_graph_cumulative$strategy,population=SA_cost_pop_graph_cumulative$population), FUN=sum)
#cumulative cost graph
SA_cost_cumulat_graph <- ggplot(data=SA_cost_cumulat, aes(cost_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative cost (US$ Billions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("1A. SA_graph_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



SA_overall_cost_table<-SA_cost_cumulat %>% 
  group_by(cost_pop, strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(SA_overall_cost_table, file = "1A. South African cumulative cost output tables.csv")




#Incremental cost - Overall (PT + TB)
SA_c_baseline_vs_6h_overall <- data.frame(Runs=SA_baseline$Run,SA_strategy_cost$Expanded.6H-SA_strategy_cost$Baseline)
SA_c_baseline_vs_minimal_overall <- data.frame(SA_strategy_cost$Minimal.regimen-SA_strategy_cost$Baseline)
SA_c_baseline_vs_optimal_overall <- data.frame(SA_strategy_cost$Optimal.regimen-SA_strategy_cost$Baseline)
SA_c_6h_vs_minimal_overall <- data.frame(SA_strategy_cost$Minimal.regimen-SA_strategy_cost$Expanded.6H)
SA_c_6h_vs_optimal_overall <- data.frame(SA_strategy_cost$Optimal.regimen-SA_strategy_cost$Expanded.6H)
SA_c_minimal_vs_optimal_overall <- data.frame(SA_strategy_cost$Optimal.regimen-SA_strategy_cost$Minimal.regimen)
#####################################################################################################################################

#Incremental cost - Overall (PT)
SA_cPT_baseline_vs_6h_overall <- data.frame(SA_scaleup$C_PT_all - SA_baseline$C_PT_all)
SA_cPT_baseline_vs_minimal_overall <- data.frame(SA_minimal$C_PT_all - SA_baseline$C_PT_all)
SA_cPT_baseline_vs_optimal_overall <- data.frame(SA_optimal$C_PT_all - SA_baseline$C_PT_all)
SA_cPT_6h_vs_minimal_overall <- data.frame(SA_minimal$C_PT_all - SA_scaleup$C_PT_all)
SA_cPT_6h_vs_optimal_overall <- data.frame(SA_optimal$C_PT_all - SA_scaleup$C_PT_all)
SA_cPT_minimal_vs_optimal_overall <- data.frame(SA_optimal$C_PT_all - SA_minimal$C_PT_all)

#####################################################################################################################################
#Incremental cost - Overall (TB)
SA_cTB_baseline_vs_6h_overall <- data.frame(SA_scaleup$C_TB_all_DOT_SAT - SA_baseline$C_TB_all_DOT_SAT)
SA_cTB_baseline_vs_minimal_overall <- data.frame(SA_minimal$C_TB_all_DOT_SAT - SA_baseline$C_TB_all_DOT_SAT)
SA_cTB_baseline_vs_optimal_overall <- data.frame(SA_optimal$C_TB_all_DOT_SAT - SA_baseline$C_TB_all_DOT_SAT)
SA_cTB_6h_vs_minimal_overall <- data.frame(SA_minimal$C_TB_all_DOT_SAT - SA_scaleup$C_TB_all_DOT_SAT)
SA_cTB_6h_vs_optimal_overall <- data.frame(SA_optimal$C_TB_all_DOT_SAT - SA_scaleup$C_TB_all_DOT_SAT)
SA_cTB_minimal_vs_optimal_overall <- data.frame(SA_optimal$C_TB_all_DOT_SAT - SA_minimal$C_TB_all_DOT_SAT)
#####################################################################################################################################


#Incremental cost - HIV negative (PT + TB)
SA_c_baseline_vs_6h_HIV_neg <- data.frame(SA_strategy_cost$Expanded.6H_neg-SA_strategy_cost$Baseline_HIV_neg)
SA_c_baseline_vs_minimal_HIV_neg <- data.frame(SA_strategy_cost$Minimal.regimen_HIV_neg-SA_strategy_cost$Baseline_HIV_neg)
SA_c_baseline_vs_optimal_HIV_neg <- data.frame(SA_strategy_cost$Optimal.regimen_HIV_neg-SA_strategy_cost$Baseline_HIV_neg)
SA_c_6h_vs_minimal_HIV_neg <- data.frame(SA_strategy_cost$Minimal.regimen_HIV_neg-SA_strategy_cost$Expanded.6H_neg)
SA_c_6h_vs_optimal_HIV_neg <- data.frame(SA_strategy_cost$Optimal.regimen_HIV_neg-SA_strategy_cost$Expanded.6H_neg)
SA_c_minimal_vs_optimal_HIV_neg <- data.frame(SA_strategy_cost$Optimal.regimen_HIV_neg-SA_strategy_cost$Minimal.regimen_HIV_neg)

#####################################################################################################################################

#Incremental cost - HIV negative (PT)
SA_cPT_baseline_vs_6h_HIV_neg <- data.frame(SA_scaleup$C_PT_hiv_neg - SA_baseline$C_PT_hiv_neg)
SA_cPT_baseline_vs_minimal_HIV_neg <- data.frame(SA_minimal$C_PT_hiv_neg - SA_baseline$C_PT_hiv_neg)
SA_cPT_baseline_vs_optimal_HIV_neg <- data.frame(SA_optimal$C_PT_hiv_neg - SA_baseline$C_PT_hiv_neg)
SA_cPT_6h_vs_minimal_HIV_neg <- data.frame(SA_minimal$C_PT_hiv_neg - SA_scaleup$C_PT_hiv_neg)
SA_cPT_6h_vs_optimal_HIV_neg <- data.frame(SA_optimal$C_PT_hiv_neg - SA_scaleup$C_PT_hiv_neg)
SA_cPT_minimal_vs_optimal_HIV_neg <- data.frame(SA_optimal$C_PT_hiv_neg - SA_minimal$C_PT_hiv_neg)

#####################################################################################################################################


#Incremental cost - HIV negative (TB)
SA_cTB_baseline_vs_6h_HIV_neg <- data.frame(SA_scaleup$C_TB_hiv_neg_DOT_SAT - SA_baseline$C_TB_hiv_neg_DOT_SAT)
SA_cTB_baseline_vs_minimal_HIV_neg <- data.frame(SA_minimal$C_TB_hiv_neg_DOT_SAT - SA_baseline$C_TB_hiv_neg_DOT_SAT)
SA_cTB_baseline_vs_optimal_HIV_neg <- data.frame(SA_optimal$C_TB_hiv_neg_DOT_SAT - SA_baseline$C_TB_hiv_neg_DOT_SAT)
SA_cTB_6h_vs_minimal_HIV_neg <- data.frame(SA_minimal$C_TB_hiv_neg_DOT_SAT - SA_scaleup$C_TB_hiv_neg_DOT_SAT)
SA_cTB_6h_vs_optimal_HIV_neg <- data.frame(SA_optimal$C_TB_hiv_neg_DOT_SAT - SA_scaleup$C_TB_hiv_neg_DOT_SAT)
SA_cTB_minimal_vs_optimal_HIV_neg <- data.frame(SA_optimal$C_TB_hiv_neg_DOT_SAT - SA_minimal$C_TB_hiv_neg_DOT_SAT)

#####################################################################################################################################


#Incremental cost - PLHIV (PT+TB)
SA_c_baseline_vs_6h_PLHIV <- data.frame(SA_strategy_cost$Expanded.6H_PLHIV-SA_strategy_cost$Baseline_PLHIV)
SA_c_baseline_vs_minimal_PLHIV <- data.frame(SA_strategy_cost$Minimal.regimen_PLHIV-SA_strategy_cost$Baseline_PLHIV)
SA_c_baseline_vs_optimal_PLHIV <- data.frame(SA_strategy_cost$Optimal.regimen_PLHIV-SA_strategy_cost$Baseline_PLHIV)
SA_c_6h_vs_minimal_PLHIV <- data.frame(SA_strategy_cost$Minimal.regimen_PLHIV-SA_strategy_cost$Expanded.6H_PLHIV)
SA_c_6h_vs_optimal_PLHIV <- data.frame(SA_strategy_cost$Optimal.regimen_PLHIV-SA_strategy_cost$Expanded.6H_PLHIV)
SA_c_minimal_vs_optimal_PLHIV <- data.frame(SA_strategy_cost$Optimal.regimen_PLHIV-SA_strategy_cost$Minimal.regimen_PLHIV)

#####################################################################################################################################


#Incremental cost - PLHIV (PT)
SA_cPT_baseline_vs_6h_PLHIV <- data.frame(SA_scaleup$C_PT_hiv - SA_baseline$C_PT_hiv)
SA_cPT_baseline_vs_minimal_PLHIV <- data.frame(SA_minimal$C_PT_hiv - SA_baseline$C_PT_hiv)
SA_cPT_baseline_vs_optimal_PLHIV <- data.frame(SA_optimal$C_PT_hiv - SA_baseline$C_PT_hiv)
SA_cPT_6h_vs_minimal_PLHIV <- data.frame(SA_minimal$C_PT_hiv - SA_scaleup$C_PT_hiv)
SA_cPT_6h_vs_optimal_PLHIV <- data.frame(SA_optimal$C_PT_hiv - SA_scaleup$C_PT_hiv)
SA_cPT_minimal_vs_optimal_PLHIV <- data.frame(SA_optimal$C_PT_hiv - SA_minimal$C_PT_hiv)

#####################################################################################################################################


#Incremental cost - PLHIV (TB)
SA_cTB_baseline_vs_6h_PLHIV <- data.frame(SA_scaleup$C_TB_hiv_DOT_SAT - SA_baseline$C_TB_hiv_DOT_SAT)
SA_cTB_baseline_vs_minimal_PLHIV <- data.frame(SA_minimal$C_TB_hiv_DOT_SAT - SA_baseline$C_TB_hiv_DOT_SAT)
SA_cTB_baseline_vs_optimal_PLHIV <- data.frame(SA_optimal$C_TB_hiv_DOT_SAT - SA_baseline$C_TB_hiv_DOT_SAT)
SA_cTB_6h_vs_minimal_PLHIV <- data.frame(SA_minimal$C_TB_hiv_DOT_SAT - SA_scaleup$C_TB_hiv_DOT_SAT)
SA_cTB_6h_vs_optimal_PLHIV <- data.frame(SA_optimal$C_TB_hiv_DOT_SAT - SA_scaleup$C_TB_hiv_DOT_SAT)
SA_cTB_minimal_vs_optimal_PLHIV <- data.frame(SA_optimal$C_TB_hiv_DOT_SAT - SA_minimal$C_TB_hiv_DOT_SAT)

#####################################################################################################################################
#####################################################################################################################################

#combine columns
SA_incr_cost<- data.frame(SA_c_baseline_vs_6h_overall,SA_c_baseline_vs_minimal_overall,SA_c_baseline_vs_optimal_overall,SA_c_6h_vs_minimal_overall,SA_c_6h_vs_optimal_overall,SA_c_minimal_vs_optimal_overall,  #Incremental cost - Overall (PT + TB)
                          SA_cPT_baseline_vs_6h_overall,SA_cPT_baseline_vs_minimal_overall,SA_cPT_baseline_vs_optimal_overall,SA_cPT_6h_vs_minimal_overall,SA_cPT_6h_vs_optimal_overall,SA_cPT_minimal_vs_optimal_overall,  #Incremental cost - Overall (PT)
                          SA_cTB_baseline_vs_6h_overall,SA_cTB_baseline_vs_minimal_overall,SA_cTB_baseline_vs_optimal_overall,SA_cTB_6h_vs_minimal_overall,SA_cTB_6h_vs_optimal_overall,SA_cTB_minimal_vs_optimal_overall,  #Incremental cost - Overall (TB)
                          SA_c_baseline_vs_6h_HIV_neg,SA_c_baseline_vs_minimal_HIV_neg,SA_c_baseline_vs_optimal_HIV_neg,SA_c_6h_vs_minimal_HIV_neg,SA_c_6h_vs_optimal_HIV_neg,SA_c_minimal_vs_optimal_HIV_neg,  #Incremental cost - HIV negative (PT + TB)
                          SA_cPT_baseline_vs_6h_HIV_neg,SA_cPT_baseline_vs_minimal_HIV_neg,SA_cPT_baseline_vs_optimal_HIV_neg,SA_cPT_6h_vs_minimal_HIV_neg,SA_cPT_6h_vs_optimal_HIV_neg,SA_cPT_minimal_vs_optimal_HIV_neg, #Incremental cost - HIV negative (PT)
                          SA_cTB_baseline_vs_6h_HIV_neg,SA_cTB_baseline_vs_minimal_HIV_neg,SA_cTB_baseline_vs_optimal_HIV_neg,SA_cTB_6h_vs_minimal_HIV_neg,SA_cTB_6h_vs_optimal_HIV_neg,SA_cTB_minimal_vs_optimal_HIV_neg,  #Incremental cost - HIV negative (TB)
                          SA_c_baseline_vs_6h_PLHIV,SA_c_baseline_vs_minimal_PLHIV,SA_c_baseline_vs_optimal_PLHIV,SA_c_6h_vs_minimal_PLHIV,SA_c_6h_vs_optimal_PLHIV,SA_c_minimal_vs_optimal_PLHIV,  #Incremental cost - PLHIV (PT+TB)
                          SA_cPT_baseline_vs_6h_PLHIV,SA_cPT_baseline_vs_minimal_PLHIV,SA_cPT_baseline_vs_optimal_PLHIV,SA_cPT_6h_vs_minimal_PLHIV,SA_cPT_6h_vs_optimal_PLHIV,SA_cPT_minimal_vs_optimal_PLHIV,  #Incremental cost - PLHIV (PT)
                          SA_cTB_baseline_vs_6h_PLHIV,SA_cTB_baseline_vs_minimal_PLHIV,SA_cTB_baseline_vs_optimal_PLHIV,SA_cTB_6h_vs_minimal_PLHIV,SA_cTB_6h_vs_optimal_PLHIV,SA_cTB_minimal_vs_optimal_PLHIV   #Incremental cost - PLHIV (TB)
)
colnames(SA_incr_cost) <- c("Runs","SA_c_baseline_vs_6h_overall","SA_c_baseline_vs_minimal_overall","SA_c_baseline_vs_optimal_overall","SA_c_6h_vs_minimal_overall","SA_c_6h_vs_optimal_overall","SA_c_minimal_vs_optimal_overall",  #Incremental cost - Overall (PT + TB)
                            "SA_cPT_baseline_vs_6h_overall","SA_cPT_baseline_vs_minimal_overall","SA_cPT_baseline_vs_optimal_overall","SA_cPT_6h_vs_minimal_overall","SA_cPT_6h_vs_optimal_overall","SA_cPT_minimal_vs_optimal_overall",  #Incremental cost - Overall (PT)
                            "SA_cTB_baseline_vs_6h_overall","SA_cTB_baseline_vs_minimal_overall","SA_cTB_baseline_vs_optimal_overall","SA_cTB_6h_vs_minimal_overall","SA_cTB_6h_vs_optimal_overall","SA_cTB_minimal_vs_optimal_overall",  #Incremental cost - Overall (TB)
                            "SA_c_baseline_vs_6h_HIV_neg","SA_c_baseline_vs_minimal_HIV_neg","SA_c_baseline_vs_optimal_HIV_neg","SA_c_6h_vs_minimal_HIV_neg","SA_c_6h_vs_optimal_HIV_neg","SA_c_minimal_vs_optimal_HIV_neg",  #Incremental cost - HIV negative (PT + TB)
                            "SA_cPT_baseline_vs_6h_HIV_neg","SA_cPT_baseline_vs_minimal_HIV_neg","SA_cPT_baseline_vs_optimal_HIV_neg","SA_cPT_6h_vs_minimal_HIV_neg","SA_cPT_6h_vs_optimal_HIV_neg","SA_cPT_minimal_vs_optimal_HIV_neg", #Incremental cost - HIV negative (PT)
                            "SA_cTB_baseline_vs_6h_HIV_neg","SA_cTB_baseline_vs_minimal_HIV_neg","SA_cTB_baseline_vs_optimal_HIV_neg","SA_cTB_6h_vs_minimal_HIV_neg","SA_cTB_6h_vs_optimal_HIV_neg","SA_cTB_minimal_vs_optimal_HIV_neg",  #Incremental cost - HIV negative (TB)
                            "SA_c_baseline_vs_6h_PLHIV","SA_c_baseline_vs_minimal_PLHIV","SA_c_baseline_vs_optimal_PLHIV","SA_c_6h_vs_minimal_PLHIV","SA_c_6h_vs_optimal_PLHIV","SA_c_minimal_vs_optimal_PLHIV",  #Incremental cost - PLHIV (PT+TB)
                            "SA_cPT_baseline_vs_6h_PLHIV","SA_cPT_baseline_vs_minimal_PLHIV","SA_cPT_baseline_vs_optimal_PLHIV","SA_cPT_6h_vs_minimal_PLHIV","SA_cPT_6h_vs_optimal_PLHIV","SA_cPT_minimal_vs_optimal_PLHIV",  #Incremental cost - PLHIV (PT)
                            "SA_cTB_baseline_vs_6h_PLHIV","SA_cTB_baseline_vs_minimal_PLHIV","SA_cTB_baseline_vs_optimal_PLHIV","SA_cTB_6h_vs_minimal_PLHIV","SA_cTB_6h_vs_optimal_PLHIV","SA_cTB_minimal_vs_optimal_PLHIV"   #Incremental cost - PLHIV (TB)
)

#####################################################################################################################################
#####################################################################################################################################
#colnames(SA_incr_cost) <- c("Runs","All_TBPT_baseline vs 6H","All_TBPT_baseline vs minimal","All_TBPT_baseline vs optimal","All_TBPT_6H vs minimal", "All_TBPT_6H vs optimal", "All_TBPT_minimal vs optimal",
#                                    "All_PT_baseline vs 6H","All_PT_baseline vs minimal","All_PT_baseline vs optimal","All_PT_6H vs minimal", "All_PT_6H vs optimal", "All_PT_minimal vs optimal",
#                                   "All_TB_baseline vs 6H","All_TB_baseline vs minimal","All_TB_baseline vs optimal","All_TB_6H vs minimal", "All_TB_6H vs optimal", "All_TB_minimal vs optimal",
#                                    "HHC_TBPT_baseline vs 6H","HHC_TBPT_baseline vs minimal","HHC_TBPT_baseline vs optimal","HHC_TBPT_6H vs minimal", "HHC_TBPT_6H vs optimal", "HHC_TBPT_minimal vs optimal",
#                                   "HHC_PT_baseline vs 6H","HHC_PT_baseline vs minimal","HHC_PT_baseline vs optimal","HHC_PT_6H vs minimal", "HHC_PT_6H vs optimal", "HHC_PT_minimal vs optimal",
#                                   "HHC_TB_baseline vs 6H","HHC_TB_baseline vs minimal","HHC_TB_baseline vs optimal","HHC_TB_6H vs minimal", "HHC_TB_6H vs optimal", "HHC_TB_minimal vs optimal",
#                                   "HIV_TBPT_baseline vs 6H","HIV_TBPT_baseline vs minimal","HIV_TBPT_baseline vs optimal","HIV_TBPT_6H vs minimal", "HIV_TBPT_6H vs optimal", "HIV_TBPT_minimal vs optimal",
#                                   "HIV_PT_baseline vs 6H","HIV_PT_baseline vs minimal","HIV_PT_baseline vs optimal","HIV_PT_6H vs minimal", "HIV_PT_6H vs optimal", "HIV_PT_minimal vs optimal",
#                                   "HIV_TB_baseline vs 6H","HIV_TB_baseline vs minimal","HIV_TB_baseline vs optimal","HIV_TB_6H vs minimal", "HIV_TB_6H vs optimal", "HIV_TB_minimal vs optimal")


SA_incr_cost_graph<-melt(SA_incr_cost, id.vars =c("Runs"), variable.name = "cost_pop")
SA_incr_cost_graph$cum_cost<-ave(SA_incr_cost_graph$value, SA_incr_cost_graph$cost_pop, FUN=cumsum)
SA_incr_cost_graph$comparison = ifelse(grepl("baseline_vs_6h", SA_incr_cost_graph$cost_pop), "1a. 6H vs baseline",
                                       ifelse(grepl("baseline_vs_minimal", SA_incr_cost_graph$cost_pop),"1b. Minimal vs baseline",
                                              ifelse(grepl("baseline_vs_optimal", SA_incr_cost_graph$cost_pop),"1c. Optimal vs baseline",
                                                     ifelse(grepl("6h_vs_minimal", SA_incr_cost_graph$cost_pop), "2a. Minimal vs 6H",
                                                            ifelse(grepl("6h_vs_optimal", SA_incr_cost_graph$cost_pop),"2b. Optimal vs 6H","3. Optimal vs minimal")))))
SA_incr_cost_graph$outcome = ifelse(grepl("cPT", SA_incr_cost_graph$cost_pop), "1.PT",
                                    ifelse(grepl("cTB", SA_incr_cost_graph$cost_pop),"2.TB","3.PT & TB"))
SA_incr_cost_graph$population = ifelse(grepl("overall", SA_incr_cost_graph$cost_pop), "All treatment candidates",
                                       ifelse(grepl("PLHIV", SA_incr_cost_graph$cost_pop),"PLHIV","HHC (HIV negatives)"))
SA_incr_cost_graph$cost_pop = chartr("."," ",SA_incr_cost_graph$cost_pop)

#####################################################################################################################

SA_incr_cost<-aggregate(x=SA_incr_cost_graph$value, by=list(Runs=SA_incr_cost_graph$Runs, comparison=SA_incr_cost_graph$comparison,outcome=SA_incr_cost_graph$outcome,population=SA_incr_cost_graph$population), FUN=sum)
#Incremental cost graph
#Incremental cost graph
SA_incr_cost_limited<-SA_incr_cost %>%
  filter(comparison !="1c. Optimal vs baseline", comparison!="2b. Optimal vs 6H")


SA_incr_cost_graph <- ggplot(data=SA_incr_cost_limited, aes(comparison,x,color=comparison)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
  facet_wrap(~population + outcome, dir = "h", scales = "free") +
  labs(x = "population group", y = expression(paste("Incremental cost (US$ Billions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("5. SA_incr_cost_graph.jpeg", 
       width = 25, height = 16, units = "cm")


#Incremental cost table with ranges  
SA_incr_cost_table<-SA_incr_cost %>% 
  group_by(population,outcome, comparison) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(SA_incr_cost_table, file = "5. South African incremental cost output tables.csv")




#incremental graph option 2
SA_incr_cost_table$fill <- ifelse(SA_incr_cost_table$median > 0, "#0408e0", "#c43b00")

SA_incremental_cost_plot2 <- ggplot(SA_incr_cost_table, aes(x = comparison, y = median, fill = fill)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=paste("$",round(median/1000000,0),"M")), position=position_dodge(width=0.9), vjust=-0.25, size=3, fontface="bold")+
  facet_wrap(~population + outcome, scales="fixed") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_fill_discrete(name="Incremental costs",
                      breaks=c("#0408e0", "#c43b00"),
                      labels=c("Additional cost","Savings"))+
  #scale_color_hue(labels = c("Savings","Additional cost"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none")+
  xlab("Comparison") +
  ylab("Incremental cost (in USD)") +
  labs(title="",
       subtitle="",
       caption="")+
  
  
  ggtitle("Incremental cost of TPT strategies for South Africa (2020-2035)")
ggsave("5. SA_incremental cost2.png", 
       width = 30, height = 20, units = "cm")


SA_incremental_cost_plot2

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################





#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#                                     Incremental effectiveness                                                     #
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################


#####################################################################################################################
#incremental effectiveness - all treatment candidates
#####################################################################################################################

#Incremental effectiveness - Overall (PT)
SA_PT_baseline_vs_6h_overall <- data.frame(Runs=SA_baseline$Run,SA_epi_cumulative_PT$Expanded.6H-SA_epi_cumulative_PT$Baseline)
SA_PT_baseline_vs_minimal_overall <- data.frame(SA_epi_cumulative_PT$Minimal.regimen-SA_epi_cumulative_PT$Baseline)
SA_PT_baseline_vs_optimal_overall <- data.frame(SA_epi_cumulative_PT$Optimal.regimen-SA_epi_cumulative_PT$Baseline)
SA_PT_6h_vs_minimal_overall <- data.frame(SA_epi_cumulative_PT$Minimal.regimen-SA_epi_cumulative_PT$Expanded.6H)
SA_PT_6h_vs_optimal_overall <- data.frame(SA_epi_cumulative_PT$Optimal.regimen-SA_epi_cumulative_PT$Expanded.6H)
SA_PT_minimal_vs_optimal_overall <- data.frame(SA_epi_cumulative_PT$Optimal.regimen-SA_epi_cumulative_PT$Minimal.regimen)
#####################################################################################################################################

#Incremental effectiveness - Overall (DS-TB)
SA_DSTB_baseline_vs_6h_overall <- data.frame(SA_epi_cumulative_DSTB$Expanded.6H-SA_epi_cumulative_DSTB$Baseline)
SA_DSTB_baseline_vs_minimal_overall <- data.frame(SA_epi_cumulative_DSTB$Minimal.regimen-SA_epi_cumulative_DSTB$Baseline)
SA_DSTB_baseline_vs_optimal_overall <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen-SA_epi_cumulative_DSTB$Baseline)
SA_DSTB_6h_vs_minimal_overall <- data.frame(SA_epi_cumulative_DSTB$Minimal.regimen-SA_epi_cumulative_DSTB$Expanded.6H)
SA_DSTB_6h_vs_optimal_overall <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen-SA_epi_cumulative_DSTB$Expanded.6H)
SA_DSTB_minimal_vs_optimal_overall <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen-SA_epi_cumulative_DSTB$Minimal.regimen)

#####################################################################################################################################
#Incremental effectiveness - Overall (RR-TB)
SA_DRTB_baseline_vs_6h_overall <- data.frame(SA_epi_cumulative_DRTB$Expanded.6H-SA_epi_cumulative_DRTB$Baseline)
SA_DRTB_baseline_vs_minimal_overall <- data.frame(SA_epi_cumulative_DRTB$Minimal.regimen-SA_epi_cumulative_DRTB$Baseline)
SA_DRTB_baseline_vs_optimal_overall <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen-SA_epi_cumulative_DRTB$Baseline)
SA_DRTB_6h_vs_minimal_overall <- data.frame(SA_epi_cumulative_DRTB$Minimal.regimen-SA_epi_cumulative_DRTB$Expanded.6H)
SA_DRTB_6h_vs_optimal_overall <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen-SA_epi_cumulative_DRTB$Expanded.6H)
SA_DRTB_minimal_vs_optimal_overall <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen-SA_epi_cumulative_DRTB$Minimal.regimen)

#####################################################################################################################################
#Incremental effectiveness - Overall (TB cases (DS-TB & RR-TB))
SA_TBall_baseline_vs_6h_overall <- data.frame(SA_epi_cumulative_TBall$Expanded.6H-SA_epi_cumulative_TBall$Baseline)
SA_TBall_baseline_vs_minimal_overall <- data.frame(SA_epi_cumulative_TBall$Minimal.regimen-SA_epi_cumulative_TBall$Baseline)
SA_TBall_baseline_vs_optimal_overall <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen-SA_epi_cumulative_TBall$Baseline)
SA_TBall_6h_vs_minimal_overall <- data.frame(SA_epi_cumulative_TBall$Minimal.regimen-SA_epi_cumulative_TBall$Expanded.6H)
SA_TBall_6h_vs_optimal_overall <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen-SA_epi_cumulative_TBall$Expanded.6H)
SA_TBall_minimal_vs_optimal_overall <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen-SA_epi_cumulative_TBall$Minimal.regimen)

#####################################################################################################################################
#Incremental effectiveness - Overall (TB deaths)
SA_TBdeaths_baseline_vs_6h_overall <- data.frame(SA_epi_cumulative_TBdeaths$Expanded.6H-SA_epi_cumulative_TBdeaths$Baseline)
SA_TBdeaths_baseline_vs_minimal_overall <- data.frame(SA_epi_cumulative_TBdeaths$Minimal.regimen-SA_epi_cumulative_TBdeaths$Baseline)
SA_TBdeaths_baseline_vs_optimal_overall <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen-SA_epi_cumulative_TBdeaths$Baseline)
SA_TBdeaths_6h_vs_minimal_overall <- data.frame(SA_epi_cumulative_TBdeaths$Minimal.regimen-SA_epi_cumulative_TBdeaths$Expanded.6H)
SA_TBdeaths_6h_vs_optimal_overall <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen-SA_epi_cumulative_TBdeaths$Expanded.6H)
SA_TBdeaths_minimal_vs_optimal_overall <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen-SA_epi_cumulative_TBdeaths$Minimal.regimen)

#####################################################################################################################################
#Incremental effectiveness - Overall (DALYs)
SA_DALYs_baseline_vs_6h_overall <- data.frame(SA_epi_cumulative_DALYs$Expanded.6H-SA_epi_cumulative_DALYs$Baseline)
SA_DALYs_baseline_vs_minimal_overall <- data.frame(SA_epi_cumulative_DALYs$Minimal.regimen-SA_epi_cumulative_DALYs$Baseline)
SA_DALYs_baseline_vs_optimal_overall <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen-SA_epi_cumulative_DALYs$Baseline)
SA_DALYs_6h_vs_minimal_overall <- data.frame(SA_epi_cumulative_DALYs$Minimal.regimen-SA_epi_cumulative_DALYs$Expanded.6H)
SA_DALYs_6h_vs_optimal_overall <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen-SA_epi_cumulative_DALYs$Expanded.6H)
SA_DALYs_minimal_vs_optimal_overall <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen-SA_epi_cumulative_DALYs$Minimal.regimen)


#####################################################################################################################
#incremental effectiveness - Household contacts (HIV-negative)
#####################################################################################################################

#Incremental effectiveness - HIV negative (PT)
SA_PT_baseline_vs_6h_HIV_neg <- data.frame(SA_epi_cumulative_PT$Expanded.6H_neg-SA_epi_cumulative_PT$Baseline_HIV_neg)
SA_PT_baseline_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_PT$Minimal.regimen_HIV_neg-SA_epi_cumulative_PT$Baseline_HIV_neg)
SA_PT_baseline_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_PT$Optimal.regimen_HIV_neg-SA_epi_cumulative_PT$Baseline_HIV_neg)
SA_PT_6h_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_PT$Minimal.regimen_HIV_neg-SA_epi_cumulative_PT$Expanded.6H_neg)
SA_PT_6h_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_PT$Optimal.regimen_HIV_neg-SA_epi_cumulative_PT$Expanded.6H_neg)
SA_PT_minimal_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_PT$Optimal.regimen_HIV_neg-SA_epi_cumulative_PT$Minimal.regimen_HIV_neg)
#####################################################################################################################################

#Incremental effectiveness - HIV negative (DS-TB)
SA_DSTB_baseline_vs_6h_HIV_neg <- data.frame(SA_epi_cumulative_DSTB$Expanded.6H_neg-SA_epi_cumulative_DSTB$Baseline_HIV_neg)
SA_DSTB_baseline_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_DSTB$Minimal.regimen_HIV_neg-SA_epi_cumulative_DSTB$Baseline_HIV_neg)
SA_DSTB_baseline_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen_HIV_neg-SA_epi_cumulative_DSTB$Baseline_HIV_neg)
SA_DSTB_6h_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_DSTB$Minimal.regimen_HIV_neg-SA_epi_cumulative_DSTB$Expanded.6H_neg)
SA_DSTB_6h_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen_HIV_neg-SA_epi_cumulative_DSTB$Expanded.6H_neg)
SA_DSTB_minimal_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen_HIV_neg-SA_epi_cumulative_DSTB$Minimal.regimen_HIV_neg)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (RR-TB)
SA_DRTB_baseline_vs_6h_HIV_neg <- data.frame(SA_epi_cumulative_DRTB$Expanded.6H_neg-SA_epi_cumulative_DRTB$Baseline_HIV_neg)
SA_DRTB_baseline_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_DRTB$Minimal.regimen_HIV_neg-SA_epi_cumulative_DRTB$Baseline_HIV_neg)
SA_DRTB_baseline_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen_HIV_neg-SA_epi_cumulative_DRTB$Baseline_HIV_neg)
SA_DRTB_6h_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_DRTB$Minimal.regimen_HIV_neg-SA_epi_cumulative_DRTB$Expanded.6H_neg)
SA_DRTB_6h_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen_HIV_neg-SA_epi_cumulative_DRTB$Expanded.6H_neg)
SA_DRTB_minimal_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen_HIV_neg-SA_epi_cumulative_DRTB$Minimal.regimen_HIV_neg)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (TB cases (DS-TB & RR-TB))
SA_TBall_baseline_vs_6h_HIV_neg <- data.frame(SA_epi_cumulative_TBall$Expanded.6H_neg-SA_epi_cumulative_TBall$Baseline_HIV_neg)
SA_TBall_baseline_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_TBall$Minimal.regimen_HIV_neg-SA_epi_cumulative_TBall$Baseline_HIV_neg)
SA_TBall_baseline_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen_HIV_neg-SA_epi_cumulative_TBall$Baseline_HIV_neg)
SA_TBall_6h_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_TBall$Minimal.regimen_HIV_neg-SA_epi_cumulative_TBall$Expanded.6H_neg)
SA_TBall_6h_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen_HIV_neg-SA_epi_cumulative_TBall$Expanded.6H_neg)
SA_TBall_minimal_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen_HIV_neg-SA_epi_cumulative_TBall$Minimal.regimen_HIV_neg)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (TB deaths)
SA_TBdeaths_baseline_vs_6h_HIV_neg <- data.frame(SA_epi_cumulative_TBdeaths$Expanded.6H_neg-SA_epi_cumulative_TBdeaths$Baseline_HIV_neg)
SA_TBdeaths_baseline_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_TBdeaths$Minimal.regimen_HIV_neg-SA_epi_cumulative_TBdeaths$Baseline_HIV_neg)
SA_TBdeaths_baseline_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen_HIV_neg-SA_epi_cumulative_TBdeaths$Baseline_HIV_neg)
SA_TBdeaths_6h_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_TBdeaths$Minimal.regimen_HIV_neg-SA_epi_cumulative_TBdeaths$Expanded.6H_neg)
SA_TBdeaths_6h_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen_HIV_neg-SA_epi_cumulative_TBdeaths$Expanded.6H_neg)
SA_TBdeaths_minimal_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen_HIV_neg-SA_epi_cumulative_TBdeaths$Minimal.regimen_HIV_neg)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (DALYs)
SA_DALYs_baseline_vs_6h_HIV_neg <- data.frame(SA_epi_cumulative_DALYs$Expanded.6H_neg-SA_epi_cumulative_DALYs$Baseline_HIV_neg)
SA_DALYs_baseline_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_DALYs$Minimal.regimen_HIV_neg-SA_epi_cumulative_DALYs$Baseline_HIV_neg)
SA_DALYs_baseline_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen_HIV_neg-SA_epi_cumulative_DALYs$Baseline_HIV_neg)
SA_DALYs_6h_vs_minimal_HIV_neg <- data.frame(SA_epi_cumulative_DALYs$Minimal.regimen_HIV_neg-SA_epi_cumulative_DALYs$Expanded.6H_neg)
SA_DALYs_6h_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen_HIV_neg-SA_epi_cumulative_DALYs$Expanded.6H_neg)
SA_DALYs_minimal_vs_optimal_HIV_neg <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen_HIV_neg-SA_epi_cumulative_DALYs$Minimal.regimen_HIV_neg)



#####################################################################################################################
#incremental effectiveness - PLHIV
#####################################################################################################################

#Incremental effectiveness - HIV negative (PT)
SA_PT_baseline_vs_6h_PLHIV <- data.frame(SA_epi_cumulative_PT$Expanded.6H_PLHIV-SA_epi_cumulative_PT$Baseline_PLHIV)
SA_PT_baseline_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_PT$Minimal.regimen_PLHIV-SA_epi_cumulative_PT$Baseline_PLHIV)
SA_PT_baseline_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_PT$Optimal.regimen_PLHIV-SA_epi_cumulative_PT$Baseline_PLHIV)
SA_PT_6h_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_PT$Minimal.regimen_PLHIV-SA_epi_cumulative_PT$Expanded.6H_PLHIV)
SA_PT_6h_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_PT$Optimal.regimen_PLHIV-SA_epi_cumulative_PT$Expanded.6H_PLHIV)
SA_PT_minimal_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_PT$Optimal.regimen_PLHIV-SA_epi_cumulative_PT$Minimal.regimen_PLHIV)
#####################################################################################################################################

#Incremental effectiveness - HIV negative (DS-TB)
SA_DSTB_baseline_vs_6h_PLHIV <- data.frame(SA_epi_cumulative_DSTB$Expanded.6H_PLHIV-SA_epi_cumulative_DSTB$Baseline_PLHIV)
SA_DSTB_baseline_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_DSTB$Minimal.regimen_PLHIV-SA_epi_cumulative_DSTB$Baseline_PLHIV)
SA_DSTB_baseline_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen_PLHIV-SA_epi_cumulative_DSTB$Baseline_PLHIV)
SA_DSTB_6h_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_DSTB$Minimal.regimen_PLHIV-SA_epi_cumulative_DSTB$Expanded.6H_PLHIV)
SA_DSTB_6h_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen_PLHIV-SA_epi_cumulative_DSTB$Expanded.6H_PLHIV)
SA_DSTB_minimal_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DSTB$Optimal.regimen_PLHIV-SA_epi_cumulative_DSTB$Minimal.regimen_PLHIV)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (RR-TB)
SA_DRTB_baseline_vs_6h_PLHIV <- data.frame(SA_epi_cumulative_DRTB$Expanded.6H_PLHIV-SA_epi_cumulative_DRTB$Baseline_PLHIV)
SA_DRTB_baseline_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_DRTB$Minimal.regimen_PLHIV-SA_epi_cumulative_DRTB$Baseline_PLHIV)
SA_DRTB_baseline_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen_PLHIV-SA_epi_cumulative_DRTB$Baseline_PLHIV)
SA_DRTB_6h_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_DRTB$Minimal.regimen_PLHIV-SA_epi_cumulative_DRTB$Expanded.6H_PLHIV)
SA_DRTB_6h_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen_PLHIV-SA_epi_cumulative_DRTB$Expanded.6H_PLHIV)
SA_DRTB_minimal_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DRTB$Optimal.regimen_PLHIV-SA_epi_cumulative_DRTB$Minimal.regimen_PLHIV)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (TB cases (DS-TB & RR-TB))
SA_TBall_baseline_vs_6h_PLHIV <- data.frame(SA_epi_cumulative_TBall$Expanded.6H_PLHIV-SA_epi_cumulative_TBall$Baseline_PLHIV)
SA_TBall_baseline_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_TBall$Minimal.regimen_PLHIV-SA_epi_cumulative_TBall$Baseline_PLHIV)
SA_TBall_baseline_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen_PLHIV-SA_epi_cumulative_TBall$Baseline_PLHIV)
SA_TBall_6h_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_TBall$Minimal.regimen_PLHIV-SA_epi_cumulative_TBall$Expanded.6H_PLHIV)
SA_TBall_6h_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen_PLHIV-SA_epi_cumulative_TBall$Expanded.6H_PLHIV)
SA_TBall_minimal_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_TBall$Optimal.regimen_PLHIV-SA_epi_cumulative_TBall$Minimal.regimen_PLHIV)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (TB deaths)
SA_TBdeaths_baseline_vs_6h_PLHIV <- data.frame(SA_epi_cumulative_TBdeaths$Expanded.6H_PLHIV-SA_epi_cumulative_TBdeaths$Baseline_PLHIV)
SA_TBdeaths_baseline_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_TBdeaths$Minimal.regimen_PLHIV-SA_epi_cumulative_TBdeaths$Baseline_PLHIV)
SA_TBdeaths_baseline_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen_PLHIV-SA_epi_cumulative_TBdeaths$Baseline_PLHIV)
SA_TBdeaths_6h_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_TBdeaths$Minimal.regimen_PLHIV-SA_epi_cumulative_TBdeaths$Expanded.6H_PLHIV)
SA_TBdeaths_6h_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen_PLHIV-SA_epi_cumulative_TBdeaths$Expanded.6H_PLHIV)
SA_TBdeaths_minimal_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_TBdeaths$Optimal.regimen_PLHIV-SA_epi_cumulative_TBdeaths$Minimal.regimen_PLHIV)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (DALYs)
SA_DALYs_baseline_vs_6h_PLHIV <- data.frame(SA_epi_cumulative_DALYs$Expanded.6H_PLHIV-SA_epi_cumulative_DALYs$Baseline_PLHIV)
SA_DALYs_baseline_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_DALYs$Minimal.regimen_PLHIV-SA_epi_cumulative_DALYs$Baseline_PLHIV)
SA_DALYs_baseline_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen_PLHIV-SA_epi_cumulative_DALYs$Baseline_PLHIV)
SA_DALYs_6h_vs_minimal_PLHIV <- data.frame(SA_epi_cumulative_DALYs$Minimal.regimen_PLHIV-SA_epi_cumulative_DALYs$Expanded.6H_PLHIV)
SA_DALYs_6h_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen_PLHIV-SA_epi_cumulative_DALYs$Expanded.6H_PLHIV)
SA_DALYs_minimal_vs_optimal_PLHIV <- data.frame(SA_epi_cumulative_DALYs$Optimal.regimen_PLHIV-SA_epi_cumulative_DALYs$Minimal.regimen_PLHIV)



#####################################################################################################################################

#####################################################################################################################################
#####################################################################################################################################

#combine columns
SA_incr_effectiveness<-data.frame(SA_PT_baseline_vs_6h_overall,SA_PT_baseline_vs_minimal_overall,SA_PT_baseline_vs_optimal_overall,SA_PT_6h_vs_minimal_overall,SA_PT_6h_vs_optimal_overall,SA_PT_minimal_vs_optimal_overall,
                                  SA_PT_baseline_vs_6h_HIV_neg,SA_PT_baseline_vs_minimal_HIV_neg,SA_PT_baseline_vs_optimal_HIV_neg,SA_PT_6h_vs_minimal_HIV_neg,SA_PT_6h_vs_optimal_HIV_neg,SA_PT_minimal_vs_optimal_HIV_neg,
                                  SA_PT_baseline_vs_6h_PLHIV,
                                  SA_PT_baseline_vs_minimal_PLHIV,
                                  SA_PT_baseline_vs_optimal_PLHIV,
                                  SA_PT_6h_vs_minimal_PLHIV,
                                  SA_PT_6h_vs_optimal_PLHIV,
                                  SA_PT_minimal_vs_optimal_PLHIV,
                                  SA_DSTB_baseline_vs_6h_overall,
                                  SA_DSTB_baseline_vs_minimal_overall,
                                  SA_DSTB_baseline_vs_optimal_overall,
                                  SA_DSTB_6h_vs_minimal_overall,
                                  SA_DSTB_6h_vs_optimal_overall,
                                  SA_DSTB_minimal_vs_optimal_overall,
                                  SA_DSTB_baseline_vs_6h_HIV_neg,
                                  SA_DSTB_baseline_vs_minimal_HIV_neg,
                                  SA_DSTB_baseline_vs_optimal_HIV_neg,
                                  SA_DSTB_6h_vs_minimal_HIV_neg,
                                  SA_DSTB_6h_vs_optimal_HIV_neg,
                                  SA_DSTB_minimal_vs_optimal_HIV_neg,
                                  SA_DSTB_baseline_vs_6h_PLHIV,
                                  SA_DSTB_baseline_vs_minimal_PLHIV,
                                  SA_DSTB_baseline_vs_optimal_PLHIV,
                                  SA_DSTB_6h_vs_minimal_PLHIV,
                                  SA_DSTB_6h_vs_optimal_PLHIV,
                                  SA_DSTB_minimal_vs_optimal_PLHIV,
                                  SA_DRTB_baseline_vs_6h_overall,
                                  SA_DRTB_baseline_vs_minimal_overall,
                                  SA_DRTB_baseline_vs_optimal_overall,
                                  SA_DRTB_6h_vs_minimal_overall,
                                  SA_DRTB_6h_vs_optimal_overall,
                                  SA_DRTB_minimal_vs_optimal_overall,
                                  SA_DRTB_baseline_vs_6h_HIV_neg,
                                  SA_DRTB_baseline_vs_minimal_HIV_neg,
                                  SA_DRTB_baseline_vs_optimal_HIV_neg,
                                  SA_DRTB_6h_vs_minimal_HIV_neg,
                                  SA_DRTB_6h_vs_optimal_HIV_neg,
                                  SA_DRTB_minimal_vs_optimal_HIV_neg,
                                  SA_DRTB_baseline_vs_6h_PLHIV,
                                  SA_DRTB_baseline_vs_minimal_PLHIV,
                                  SA_DRTB_baseline_vs_optimal_PLHIV,
                                  SA_DRTB_6h_vs_minimal_PLHIV,
                                  SA_DRTB_6h_vs_optimal_PLHIV,
                                  SA_DRTB_minimal_vs_optimal_PLHIV,
                                  SA_TBall_baseline_vs_6h_overall,
                                  SA_TBall_baseline_vs_minimal_overall,
                                  SA_TBall_baseline_vs_optimal_overall,
                                  SA_TBall_6h_vs_minimal_overall,
                                  SA_TBall_6h_vs_optimal_overall,
                                  SA_TBall_minimal_vs_optimal_overall,
                                  SA_TBall_baseline_vs_6h_HIV_neg,
                                  SA_TBall_baseline_vs_minimal_HIV_neg,
                                  SA_TBall_baseline_vs_optimal_HIV_neg,
                                  SA_TBall_6h_vs_minimal_HIV_neg,
                                  SA_TBall_6h_vs_optimal_HIV_neg,
                                  SA_TBall_minimal_vs_optimal_HIV_neg,
                                  SA_TBall_baseline_vs_6h_PLHIV,
                                  SA_TBall_baseline_vs_minimal_PLHIV,
                                  SA_TBall_baseline_vs_optimal_PLHIV,
                                  SA_TBall_6h_vs_minimal_PLHIV,
                                  SA_TBall_6h_vs_optimal_PLHIV,
                                  SA_TBall_minimal_vs_optimal_PLHIV,
                                  SA_TBdeaths_baseline_vs_6h_overall,
                                  SA_TBdeaths_baseline_vs_minimal_overall,
                                  SA_TBdeaths_baseline_vs_optimal_overall,
                                  SA_TBdeaths_6h_vs_minimal_overall,
                                  SA_TBdeaths_6h_vs_optimal_overall,
                                  SA_TBdeaths_minimal_vs_optimal_overall,
                                  SA_TBdeaths_baseline_vs_6h_HIV_neg,
                                  SA_TBdeaths_baseline_vs_minimal_HIV_neg,
                                  SA_TBdeaths_baseline_vs_optimal_HIV_neg,
                                  SA_TBdeaths_6h_vs_minimal_HIV_neg,
                                  SA_TBdeaths_6h_vs_optimal_HIV_neg,
                                  SA_TBdeaths_minimal_vs_optimal_HIV_neg,
                                  SA_TBdeaths_baseline_vs_6h_PLHIV,
                                  SA_TBdeaths_baseline_vs_minimal_PLHIV,
                                  SA_TBdeaths_baseline_vs_optimal_PLHIV,
                                  SA_TBdeaths_6h_vs_minimal_PLHIV,
                                  SA_TBdeaths_6h_vs_optimal_PLHIV,
                                  SA_TBdeaths_minimal_vs_optimal_PLHIV,
                                  SA_DALYs_baseline_vs_6h_overall,
                                  SA_DALYs_baseline_vs_minimal_overall,
                                  SA_DALYs_baseline_vs_optimal_overall,
                                  SA_DALYs_6h_vs_minimal_overall,
                                  SA_DALYs_6h_vs_optimal_overall,
                                  SA_DALYs_minimal_vs_optimal_overall,
                                  SA_DALYs_baseline_vs_6h_HIV_neg,
                                  SA_DALYs_baseline_vs_minimal_HIV_neg,
                                  SA_DALYs_baseline_vs_optimal_HIV_neg,
                                  SA_DALYs_6h_vs_minimal_HIV_neg,
                                  SA_DALYs_6h_vs_optimal_HIV_neg,
                                  SA_DALYs_minimal_vs_optimal_HIV_neg,
                                  SA_DALYs_baseline_vs_6h_PLHIV,
                                  SA_DALYs_baseline_vs_minimal_PLHIV,
                                  SA_DALYs_baseline_vs_optimal_PLHIV,
                                  SA_DALYs_6h_vs_minimal_PLHIV,
                                  SA_DALYs_6h_vs_optimal_PLHIV,
                                  SA_DALYs_minimal_vs_optimal_PLHIV
)
colnames(SA_incr_effectiveness) <- c("Runs","SA_PT_baseline_vs_6h_overall",
                                     "SA_PT_baseline_vs_minimal_overall",
                                     "SA_PT_baseline_vs_optimal_overall",
                                     "SA_PT_6h_vs_minimal_overall",
                                     "SA_PT_6h_vs_optimal_overall",
                                     "SA_PT_minimal_vs_optimal_overall",
                                     "SA_PT_baseline_vs_6h_HIV_neg",
                                     "SA_PT_baseline_vs_minimal_HIV_neg",
                                     "SA_PT_baseline_vs_optimal_HIV_neg",
                                     "SA_PT_6h_vs_minimal_HIV_neg",
                                     "SA_PT_6h_vs_optimal_HIV_neg",
                                     "SA_PT_minimal_vs_optimal_HIV_neg",
                                     "SA_PT_baseline_vs_6h_PLHIV",
                                     "SA_PT_baseline_vs_minimal_PLHIV",
                                     "SA_PT_baseline_vs_optimal_PLHIV",
                                     "SA_PT_6h_vs_minimal_PLHIV",
                                     "SA_PT_6h_vs_optimal_PLHIV",
                                     "SA_PT_minimal_vs_optimal_PLHIV",
                                     "SA_DSTB_baseline_vs_6h_overall",
                                     "SA_DSTB_baseline_vs_minimal_overall",
                                     "SA_DSTB_baseline_vs_optimal_overall",
                                     "SA_DSTB_6h_vs_minimal_overall",
                                     "SA_DSTB_6h_vs_optimal_overall",
                                     "SA_DSTB_minimal_vs_optimal_overall",
                                     "SA_DSTB_baseline_vs_6h_HIV_neg",
                                     "SA_DSTB_baseline_vs_minimal_HIV_neg",
                                     "SA_DSTB_baseline_vs_optimal_HIV_neg",
                                     "SA_DSTB_6h_vs_minimal_HIV_neg",
                                     "SA_DSTB_6h_vs_optimal_HIV_neg",
                                     "SA_DSTB_minimal_vs_optimal_HIV_neg",
                                     "SA_DSTB_baseline_vs_6h_PLHIV",
                                     "SA_DSTB_baseline_vs_minimal_PLHIV",
                                     "SA_DSTB_baseline_vs_optimal_PLHIV",
                                     "SA_DSTB_6h_vs_minimal_PLHIV",
                                     "SA_DSTB_6h_vs_optimal_PLHIV",
                                     "SA_DSTB_minimal_vs_optimal_PLHIV",
                                     "SA_DRTB_baseline_vs_6h_overall",
                                     "SA_DRTB_baseline_vs_minimal_overall",
                                     "SA_DRTB_baseline_vs_optimal_overall",
                                     "SA_DRTB_6h_vs_minimal_overall",
                                     "SA_DRTB_6h_vs_optimal_overall",
                                     "SA_DRTB_minimal_vs_optimal_overall",
                                     "SA_DRTB_baseline_vs_6h_HIV_neg",
                                     "SA_DRTB_baseline_vs_minimal_HIV_neg",
                                     "SA_DRTB_baseline_vs_optimal_HIV_neg",
                                     "SA_DRTB_6h_vs_minimal_HIV_neg",
                                     "SA_DRTB_6h_vs_optimal_HIV_neg",
                                     "SA_DRTB_minimal_vs_optimal_HIV_neg",
                                     "SA_DRTB_baseline_vs_6h_PLHIV",
                                     "SA_DRTB_baseline_vs_minimal_PLHIV",
                                     "SA_DRTB_baseline_vs_optimal_PLHIV",
                                     "SA_DRTB_6h_vs_minimal_PLHIV",
                                     "SA_DRTB_6h_vs_optimal_PLHIV",
                                     "SA_DRTB_minimal_vs_optimal_PLHIV",
                                     "SA_TBall_baseline_vs_6h_overall",
                                     "SA_TBall_baseline_vs_minimal_overall",
                                     "SA_TBall_baseline_vs_optimal_overall",
                                     "SA_TBall_6h_vs_minimal_overall",
                                     "SA_TBall_6h_vs_optimal_overall",
                                     "SA_TBall_minimal_vs_optimal_overall",
                                     "SA_TBall_baseline_vs_6h_HIV_neg",
                                     "SA_TBall_baseline_vs_minimal_HIV_neg",
                                     "SA_TBall_baseline_vs_optimal_HIV_neg",
                                     "SA_TBall_6h_vs_minimal_HIV_neg",
                                     "SA_TBall_6h_vs_optimal_HIV_neg",
                                     "SA_TBall_minimal_vs_optimal_HIV_neg",
                                     "SA_TBall_baseline_vs_6h_PLHIV",
                                     "SA_TBall_baseline_vs_minimal_PLHIV",
                                     "SA_TBall_baseline_vs_optimal_PLHIV",
                                     "SA_TBall_6h_vs_minimal_PLHIV",
                                     "SA_TBall_6h_vs_optimal_PLHIV",
                                     "SA_TBall_minimal_vs_optimal_PLHIV",
                                     "SA_TBdeaths_baseline_vs_6h_overall",
                                     "SA_TBdeaths_baseline_vs_minimal_overall",
                                     "SA_TBdeaths_baseline_vs_optimal_overall",
                                     "SA_TBdeaths_6h_vs_minimal_overall",
                                     "SA_TBdeaths_6h_vs_optimal_overall",
                                     "SA_TBdeaths_minimal_vs_optimal_overall",
                                     "SA_TBdeaths_baseline_vs_6h_HIV_neg",
                                     "SA_TBdeaths_baseline_vs_minimal_HIV_neg",
                                     "SA_TBdeaths_baseline_vs_optimal_HIV_neg",
                                     "SA_TBdeaths_6h_vs_minimal_HIV_neg",
                                     "SA_TBdeaths_6h_vs_optimal_HIV_neg",
                                     "SA_TBdeaths_minimal_vs_optimal_HIV_neg",
                                     "SA_TBdeaths_baseline_vs_6h_PLHIV",
                                     "SA_TBdeaths_baseline_vs_minimal_PLHIV",
                                     "SA_TBdeaths_baseline_vs_optimal_PLHIV",
                                     "SA_TBdeaths_6h_vs_minimal_PLHIV",
                                     "SA_TBdeaths_6h_vs_optimal_PLHIV",
                                     "SA_TBdeaths_minimal_vs_optimal_PLHIV",
                                     "SA_DALYs_baseline_vs_6h_overall",
                                     "SA_DALYs_baseline_vs_minimal_overall",
                                     "SA_DALYs_baseline_vs_optimal_overall",
                                     "SA_DALYs_6h_vs_minimal_overall",
                                     "SA_DALYs_6h_vs_optimal_overall",
                                     "SA_DALYs_minimal_vs_optimal_overall",
                                     "SA_DALYs_baseline_vs_6h_HIV_neg",
                                     "SA_DALYs_baseline_vs_minimal_HIV_neg",
                                     "SA_DALYs_baseline_vs_optimal_HIV_neg",
                                     "SA_DALYs_6h_vs_minimal_HIV_neg",
                                     "SA_DALYs_6h_vs_optimal_HIV_neg",
                                     "SA_DALYs_minimal_vs_optimal_HIV_neg",
                                     "SA_DALYs_baseline_vs_6h_PLHIV",
                                     "SA_DALYs_baseline_vs_minimal_PLHIV",
                                     "SA_DALYs_baseline_vs_optimal_PLHIV",
                                     "SA_DALYs_6h_vs_minimal_PLHIV",
                                     "SA_DALYs_6h_vs_optimal_PLHIV",
                                     "SA_DALYs_minimal_vs_optimal_PLHIV")

#####################################################################################################################################
#####################################################################################################################################
#colnames(SA_incr_cost) <- c("Runs","All_TBPT_baseline vs 6H","All_TBPT_baseline vs minimal","All_TBPT_baseline vs optimal","All_TBPT_6H vs minimal", "All_TBPT_6H vs optimal", "All_TBPT_minimal vs optimal",
#                                    "All_PT_baseline vs 6H","All_PT_baseline vs minimal","All_PT_baseline vs optimal","All_PT_6H vs minimal", "All_PT_6H vs optimal", "All_PT_minimal vs optimal",
#                                   "All_TB_baseline vs 6H","All_TB_baseline vs minimal","All_TB_baseline vs optimal","All_TB_6H vs minimal", "All_TB_6H vs optimal", "All_TB_minimal vs optimal",
#                                    "HHC_TBPT_baseline vs 6H","HHC_TBPT_baseline vs minimal","HHC_TBPT_baseline vs optimal","HHC_TBPT_6H vs minimal", "HHC_TBPT_6H vs optimal", "HHC_TBPT_minimal vs optimal",
#                                   "HHC_PT_baseline vs 6H","HHC_PT_baseline vs minimal","HHC_PT_baseline vs optimal","HHC_PT_6H vs minimal", "HHC_PT_6H vs optimal", "HHC_PT_minimal vs optimal",
#                                   "HHC_TB_baseline vs 6H","HHC_TB_baseline vs minimal","HHC_TB_baseline vs optimal","HHC_TB_6H vs minimal", "HHC_TB_6H vs optimal", "HHC_TB_minimal vs optimal",
#                                   "HIV_TBPT_baseline vs 6H","HIV_TBPT_baseline vs minimal","HIV_TBPT_baseline vs optimal","HIV_TBPT_6H vs minimal", "HIV_TBPT_6H vs optimal", "HIV_TBPT_minimal vs optimal",
#                                   "HIV_PT_baseline vs 6H","HIV_PT_baseline vs minimal","HIV_PT_baseline vs optimal","HIV_PT_6H vs minimal", "HIV_PT_6H vs optimal", "HIV_PT_minimal vs optimal",
#                                   "HIV_TB_baseline vs 6H","HIV_TB_baseline vs minimal","HIV_TB_baseline vs optimal","HIV_TB_6H vs minimal", "HIV_TB_6H vs optimal", "HIV_TB_minimal vs optimal")


SA_incr_effectiveness_graph<-melt(SA_incr_effectiveness, id.vars =c("Runs"), variable.name = "effectiveness_pop")
SA_incr_effectiveness_graph$cum_effectiveness<-ave(SA_incr_effectiveness_graph$value, SA_incr_effectiveness_graph$effectiveness_pop, FUN=cumsum)
SA_incr_effectiveness_graph$comparison = ifelse(grepl("baseline_vs_6h", SA_incr_effectiveness_graph$effectiveness_pop), "1a. 6H vs baseline",
                                                ifelse(grepl("baseline_vs_minimal", SA_incr_effectiveness_graph$effectiveness_pop),"1b. Minimal vs baseline",
                                                       ifelse(grepl("baseline_vs_optimal", SA_incr_effectiveness_graph$effectiveness_pop),"1c. Optimal vs baseline",
                                                              ifelse(grepl("6h_vs_minimal", SA_incr_effectiveness_graph$effectiveness_pop), "2a. Minimal vs 6H",
                                                                     ifelse(grepl("6h_vs_optimal", SA_incr_effectiveness_graph$effectiveness_pop),"2b. Optimal vs 6H","3. Optimal vs minimal")))))
SA_incr_effectiveness_graph$outcome = ifelse(grepl("PT", SA_incr_effectiveness_graph$effectiveness_pop), "1. PT",
                                             ifelse(grepl("DSTB", SA_incr_effectiveness_graph$effectiveness_pop),"2. DS-TB cases",
                                                    ifelse(grepl("DRTB", SA_incr_effectiveness_graph$effectiveness_pop),"3. RR-TB cases",
                                                           ifelse(grepl("TBall", SA_incr_effectiveness_graph$effectiveness_pop),"4. All TB cases (DS-TB & RR-TB)",
                                                                  ifelse(grepl("TBdeaths", SA_incr_effectiveness_graph$effectiveness_pop),"5. TB deaths",
                                                                         "6. DALYs")))))
SA_incr_effectiveness_graph$population = ifelse(grepl("overall", SA_incr_effectiveness_graph$effectiveness_pop), "All treatment candidates",
                                                ifelse(grepl("PLHIV", SA_incr_effectiveness_graph$effectiveness_pop),"PLHIV","HHC (HIV negatives)"))
SA_incr_effectiveness_graph$effectiveness_pop = chartr("."," ",SA_incr_effectiveness_graph$effectiveness_pop)



SA_incr_effectiveness<-aggregate(x=SA_incr_effectiveness_graph$value, by=list(Runs=SA_incr_effectiveness_graph$Runs, comparison=SA_incr_effectiveness_graph$comparison,outcome=SA_incr_effectiveness_graph$outcome,population=SA_incr_effectiveness_graph$population), FUN=sum)

#####################################################################################################################
#Incremental effectiveness table with ranges  
#####################################################################################################################
SA_incr_effectiveness_table<-SA_incr_effectiveness %>% 
  group_by(population,outcome, comparison) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(SA_incr_effectiveness_table, file = "6. South African incremental effectiveness output tables.csv")


#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

SA_incr_effectiveness<-aggregate(x=SA_incr_effectiveness_graph$value, by=list(Runs=SA_incr_effectiveness_graph$Runs, comparison=SA_incr_effectiveness_graph$comparison,outcome=SA_incr_effectiveness_graph$outcome,population=SA_incr_effectiveness_graph$population), FUN=sum)
#Incremental cost graph
SA_incr_effectiveness_graph <- ggplot(data=SA_incr_effectiveness, aes(comparison,x,color=comparison)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "K", scale = 1e-3))+
  facet_wrap(~population + outcome, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("Incremental effectiveness")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("6. SA_incr_effectiveness_graph.jpeg", 
       width = 25, height = 16, units = "cm")


#incremental graph option 2 - OVERALL

SA_incr_effectiveness_table_overall<-SA_incr_effectiveness_table %>%
  filter(population =="All treatment candidates")
#    filter(data_type !="Epi data" | state!="TPT")

SA_incr_effectiveness_table_overall$fill <- ifelse(SA_incr_effectiveness_table_overall$median > 0, "#0408e0", "#c43b00")

SA_incremental_effectiveness_overall_plot <- ggplot(SA_incr_effectiveness_table_overall, aes(x = comparison, y = median, fill = fill)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=paste("",round(median/1000,0),"K")), position=position_dodge(width=0.9), vjust=-0.25, size=3, fontface="bold")+
  facet_wrap(~outcome, scales="fixed") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_fill_discrete(name="Incremental effectiveness",
                      breaks=c("#0408e0", "#c43b00"),
                      labels=c("Increases","Averted"))+
  #scale_color_hue(labels = c("Savings","Additional cost"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none",)+
  xlab("Comparison") +
  ylab("Incremental effectiveness") +
  labs(title="",
       subtitle="",
       caption="")+
  
  
  ggtitle("Incremental effectiveness of TPT strategies for South Africa (All treatment candidates 2020-2035)")
ggsave("6a. SA_incremental effectiveness (All treatment candidates).png", 
       width = 30, height = 20, units = "cm")


SA_incremental_effectiveness_overall_plot



#incremental graph option 2 - HHC (HIV negatives)

SA_incr_effectiveness_table_HIVneg<-SA_incr_effectiveness_table %>%
  filter(population =="HHC (HIV negatives)")

SA_incr_effectiveness_table_HIVneg$fill <- ifelse(SA_incr_effectiveness_table_HIVneg$median > 0, "#0408e0", "#c43b00")

SA_incremental_effectiveness_HIVneg_plot <- ggplot(SA_incr_effectiveness_table_HIVneg, aes(x = comparison, y = median, fill = fill)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=paste("",round(median/1000,0),"K")), position=position_dodge(width=0.9), vjust=-0.25, size=3, fontface="bold")+
  facet_wrap(~outcome, scales="fixed") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_fill_discrete(name="Incremental effectiveness",
                      breaks=c("#0408e0", "#c43b00"),
                      labels=c("Increases","Averted"))+
  #scale_color_hue(labels = c("Savings","Additional cost"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none",)+
  xlab("Comparison") +
  ylab("Incremental effectiveness") +
  labs(title="",
       subtitle="",
       caption="")+
  
  
  ggtitle("Incremental effectiveness of TPT strategies for South Africa (HHC (HIV negatives) 2020-2035)")
ggsave("6b. SA_incremental effectiveness (HHC (HIV negatives)).png", 
       width = 30, height = 20, units = "cm")


SA_incremental_effectiveness_HIVneg_plot




#incremental graph option 2 - PLHIV

SA_incr_effectiveness_table_PLHIV<-SA_incr_effectiveness_table %>%
  filter(population =="PLHIV")

SA_incr_effectiveness_table_PLHIV$fill <- ifelse(SA_incr_effectiveness_table_PLHIV$median > 0, "#0408e0", "#c43b00")

SA_incremental_effectiveness_PLHIV_plot <- ggplot(SA_incr_effectiveness_table_PLHIV, aes(x = comparison, y = median, fill = fill)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=paste("",round(median/1000,0),"K")), position=position_dodge(width=0.9), vjust=-0.25, size=3, fontface="bold")+
  facet_wrap(~outcome, scales="fixed") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_fill_discrete(name="Incremental effectiveness",
                      breaks=c("#0408e0", "#c43b00"),
                      labels=c("Increases","Averted"))+
  #scale_color_hue(labels = c("Savings","Additional cost"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none",)+
  xlab("Comparison") +
  ylab("Incremental effectiveness") +
  labs(title="",
       subtitle="",
       caption="")+
  
  
  ggtitle("Incremental effectiveness of TPT strategies for South Africa (PLHIV 2020-2035)")
ggsave("6c. SA_incremental effectiveness (PLHIV).png", 
       width = 30, height = 20, units = "cm")


SA_incremental_effectiveness_PLHIV_plot



# combine cumulative cost plots multiple
SA_cumulative_combine<-ggarrange(SA_PT_cost_cumulat_graph, SA_cost_TB_cumulat_graph, SA_cost_MDRTB_cumulat_graph, SA_cost_cumulat_graph, labels = c("PT", "TB", "RR-TB", "PT&TB"),
                                 common.legend = TRUE, legend = "bottom")
SA_cumulative_combine
ggsave("7. SA_combined_graphs_cumulative.png", 
       width = 30, height = 20, units = "cm")





#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### INCREMENT COST EFFECTIVENESS RATIO #######
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

SA_ICER_calcul<-data.frame(Runs=SA_incr_cost$Runs,comparison=SA_incr_effectiveness_graph$comparison,outcome=SA_incr_effectiveness_graph$outcome,population=SA_incr_effectiveness_graph$population,(SA_incr_cost$x/SA_incr_effectiveness))

SA_ICER_allTB<-SA_incr_effectiveness %>%
  filter(outcome =="4. All TB cases (DS-TB & RR-TB)" & population == "All treatment candidates")
SA_ICER_allcost<-SA_incr_cost %>%
  filter(outcome =="3.PT & TB" & population == "All treatment candidates")
SA_ICER_allTBdeaths<-SA_incr_effectiveness %>%
  filter(outcome =="5. TB deaths" & population == "All treatment candidates")
SA_ICER_allDALYs<-SA_incr_effectiveness %>%
  filter(outcome =="6. DALYs" & population == "All treatment candidates")

SA_ICER_TB<-data.frame(Runs=SA_ICER_allcost$Runs, comparison=SA_ICER_allcost$comparison,TB=(SA_ICER_allcost$x/-SA_ICER_allTB$x))
SA_ICER_TBdeaths<-data.frame(TBdeaths=(SA_ICER_allcost$x/-SA_ICER_allTBdeaths$x))
SA_ICER_DALYs<-data.frame(DALYs=(SA_ICER_allcost$x/-SA_ICER_allDALYs$x))
#combine
SA_ICER<-data.frame(SA_ICER_TB,
                        SA_ICER_TBdeaths,
                        SA_ICER_DALYs)

# quadrant plots for iCERs
SA_quadrant_TB_cost<-data.frame(SA_ICER_allTB,
                                    SA_ICER_allcost,
                                    SA_ICER_allTBdeaths,
                                    SA_ICER_allDALYs)
SA_quadrant_TB_cost<-filter(SA_quadrant_TB_cost)
                            # !comparison %in% c('1c. Optimal vs baseline','2b. Optimal vs 6H'))


SA_quadrant_TB_cost$neg<--1                                                 

SA_quadrant_TB_cost$incremental_cost<-SA_quadrant_TB_cost$x.1
SA_quadrant_TB_cost$TB_case_averted<-SA_quadrant_TB_cost$x*SA_quadrant_TB_cost$neg
SA_quadrant_TB_cost$TB_deaths_averted<-SA_quadrant_TB_cost$x.2*SA_quadrant_TB_cost$neg
SA_quadrant_TB_cost$DALYs_averted<-SA_quadrant_TB_cost$x.3*SA_quadrant_TB_cost$neg


write.csv(SA_quadrant_TB_cost, file = "10. SA Incremental cost and effectiveness.csv")

SA_quadrant<-SA_quadrant_TB_cost 
#%>%
#  filter(comparison=="1a. 6H vs baseline" | comparison=="1b. Minimal vs baseline")

#.quadrants TB-Cost 
SA_CE_TB<-SA_quadrant %>% 
  mutate(quadrant = case_when(TB_case_averted > 0 & incremental_cost > 0   ~ "More expensive and more effective",
                              TB_case_averted <= 0 & incremental_cost > 0  ~ "More expensive and less effective",
                              TB_case_averted <= 0 & incremental_cost <= 0 ~ "Less costly and less effective",
                              TRUE
                              ~ "Less costly and more effective")) %>% 
  ggplot(aes(x = TB_case_averted,comparison, y = incremental_cost, color = 
               ifelse(quadrant=="More expensive and more effective","deepskyblue",
                      ifelse(quadrant=="More expensive and less effective","orange",
                             ifelse(quadrant=="Less costly and less effective","red","darkgreen"))))) +
  scale_colour_manual(values = c("orange"="orange", "red" = "red","deepskyblue" = "deepskyblue","darkgreen" = "darkgreen"))+
  geom_vline(xintercept = 0, linetype="dotted") + # plot vertical line
  geom_hline(yintercept = 0, linetype="dotted") + # plot horizontal line
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6), limits=c(-1600000000,1600000000))+
  scale_x_continuous(labels = unit_format( unit = "", scale = 1e-3), limits=c(0,2000000))+
  facet_wrap(~comparison, dir="h", scales="fixed", ncol = 4) +
  theme_classic()+
  geom_abline(slope=6040, intercept=0, col='red') +
  geom_abline(slope=18120, intercept=0, col='purple') +
  annotate("text", x = 200000, y = 800000000, label = "WTP - GDP", size=2, col="red")+
  annotate("text", x = 200000, y = 1600000000, label = "WTP - GDP*3", size=2, col="purple")+
  xlab("TB cases averted (In Thousands)") +
  ylab("") +
  labs(color='')  +
  theme(legend.position="")+
  geom_point()+
  ggsave("10a. SA_quadrant_TB_cost.jpeg", 
         width = 25, height = 8, units = "cm")

#Quadrants TB deaths -Cost 
SA_CE_TBdeath<-SA_quadrant %>% 
  mutate(quadrant = case_when(TB_deaths_averted > 0 & incremental_cost > 0   ~ "More expensive and more effective",
                              TB_deaths_averted <= 0 & incremental_cost > 0  ~ "More expensive and less effective",
                              TB_deaths_averted <= 0 & incremental_cost <= 0 ~ "Less costly and less effective",
                              TRUE
                              ~ "Less costly and more effective")) %>% 
  ggplot(aes(x = TB_deaths_averted,comparison, y = incremental_cost, color = 
               ifelse(quadrant=="More expensive and more effective","deepskyblue",
                      ifelse(quadrant=="More expensive and less effective","orange",
                             ifelse(quadrant=="Less costly and less effective","red","darkgreen"))))) +
  scale_colour_manual(values = c("orange"="orange", "red" = "red","deepskyblue" = "deepskyblue","darkgreen" = "darkgreen"))+
  geom_vline(xintercept = 0, linetype="dotted") + # plot vertical line
  geom_hline(yintercept = 0, linetype="dotted") + # plot horizontal line
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6), limits=c(-1600000000,1600000000))+
  scale_x_continuous(labels = unit_format( unit = "", scale = 1e-3), limits=c(0,800000))+
  facet_wrap(~comparison, dir="h", scales="fixed", ncol = 4) +
  theme_classic()+
  geom_abline(slope=6040, intercept=0, col='red') +
  geom_abline(slope=18120, intercept=0, col='purple') +
  annotate("text", x = 200000, y = 800000000, label = "WTP - GDP", size=2, col="red")+
  annotate("text", x = 200000, y = 1600000000, label = "WTP - GDP*3", size=2, col="purple")+
  xlab("TB deaths averted  (In Thousands)") +
  ylab("Incremental cost (In millions US$)") +
  labs(color='')  +
  theme(legend.position="")+
  geom_point()+
  ggsave("10b. SA_quadrant_TB_deaths_cost.jpeg", 
         width = 25, height = 8, units = "cm")






#Quadrants DALYs -Cost 
SA_CE_DALYs<-SA_quadrant %>% 
  mutate(quadrant = case_when(DALYs_averted > 0 & incremental_cost > 0   ~ "More expensive and more effective",
                              DALYs_averted <= 0 & incremental_cost > 0  ~ "QMore expensive and less effective",
                              DALYs_averted <= 0 & incremental_cost <= 0 ~ "Less costly and less effective",
                              TRUE
                              ~ "Less costly and more effective")) %>% 
  ggplot(aes(x = DALYs_averted,comparison, y = incremental_cost,  color = 
               ifelse(quadrant=="More expensive and more effective","deepskyblue",
                      ifelse(quadrant=="More expensive and less effective","orange",
                             ifelse(quadrant=="Less costly and less effective","red","darkgreen"))))) +
  scale_colour_manual(values = c("orange"="orange", "red" = "red","deepskyblue" = "deepskyblue","darkgreen" = "darkgreen"))+
  geom_vline(xintercept = 0, linetype="dotted") + # plot vertical line
  geom_hline(yintercept = 0, linetype="dotted") + # plot horizontal line
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6), limits=c(-1600000000,1600000000))+
  scale_x_continuous(labels = unit_format( unit = "", scale = 1e-3), limits=c(0,25000000))+
  facet_wrap(~comparison, dir="h", scales="fixed", ncol = 4) +
  theme_classic()+
  geom_abline(slope=6040, intercept=0, col='red') +
  geom_abline(slope=18120, intercept=0, col='purple') +
  annotate("text", x = 3000000, y = 800000000, label = "WTP - GDP", size=2, col="red")+
  annotate("text", x = 3000000, y = 1600000000, label = "WTP - GDP*3", size=2, col="purple")+
  xlab("DALYs averted (In Thousands)") +
  ylab("") +
  labs(color='')  +
  theme(legend.position="bottom")+
  geom_point()+
  ggsave("10c. SA_quadrant_DALYs_cost.jpeg", 
         width = 25, height = 8, units = "cm")





# combine ICER cost plots multiple
SA_CE_combine<-ggarrange(SA_CE_TB, SA_CE_TBdeath, SA_CE_DALYs, labels = c("TB", "Deaths", "DALYs"),nrow=3,
                                 common.legend = TRUE, legend = "bottom")
SA_CE_combine
ggsave("10. SA_combined_graphs_CE.png", 
       width = 30, height = 20, units = "cm")




#####################################################################################################################
#Incremental effectiveness table with ranges  
#####################################################################################################################
#ICER TB
SA_ICER_table_TB<-SA_ICER %>% 
  group_by(comparison) %>% 
  summarise_at(vars(TB),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(SA_ICER_table_TB, file = "8a. SA ICERs TB tables.csv")

#ICER - TB deaths
SA_ICER_table_TBdeath<-SA_ICER %>% 
  group_by(comparison) %>% 
  summarise_at(vars(TBdeaths),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(SA_ICER_table_TBdeath, file = "8b. SA ICERs TB deaths tables.csv")

#ICER - DALYs
SA_ICER_table_DALYs<-SA_ICER %>% 
  group_by(comparison) %>% 
  summarise_at(vars(DALYs),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(SA_ICER_table_DALYs, file = "8c. SA ICERs DALYs tables.csv")

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### BRAZIL #######
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#####################################################################################################################
#                                               LOAD THE DATA                                                       #
#####################################################################################################################
#Load the micro costing summaries
Brazil_costs <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Template_Brazil", col_types = "numeric", range = "A1:T16001")

#Load Brazil mathematical model epidemiological outputs
Brazil_baseline <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Brazil_baseline_data", col_types = "numeric", range = "A1:R16001")
Brazil_scaleup <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Brazil_scaleup_data", col_types = "numeric", range = "A1:R16001")
Brazil_minimal <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Brazil_minimal_data", col_types = "numeric", range = "A1:R16001")
Brazil_optimal <- readxl::read_xlsx("Raw_Datasets_LTBI_TPP.xlsx", sheet = "Brazil_optimal_data", col_types = "numeric", range = "A1:R16001")


#####################################################################################################################
#                                       baseline ANNUAL COSTS - DOT                                                 #
#####################################################################################################################


#Annual costs for baseline dataset
# PT
Brazil_baseline$C_PT_all<-(Brazil_baseline$New_PT_all*(Brazil_costs$LTBI_baseline+Brazil_costs$AE_LTBI)) # Cost: PT all
Brazil_baseline$C_PT_hiv_neg<-(Brazil_baseline$New_PT_all-Brazil_baseline$New_PT_HIV)*(Brazil_costs$LTBI_baseline+Brazil_costs$AE_LTBI) # Cost: PT (HIV negative)
Brazil_baseline$C_PT_hiv<-(Brazil_baseline$New_PT_HIV*(Brazil_costs$LTBI_baseline+Brazil_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
Brazil_baseline$C_DSTB_all_DOT_SAT<-(Brazil_baseline$TB_cases_all-Brazil_baseline$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_baseline$C_DSTB_hiv_neg_DOT_SAT<-((Brazil_baseline$TB_cases_all-Brazil_baseline$TB_cases_MDR)-(Brazil_baseline$TB_cases_HIV-Brazil_baseline$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_baseline$C_DSTB_hiv_DOT_SAT<-(Brazil_baseline$TB_cases_HIV-Brazil_baseline$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
Brazil_baseline$C_MDRTB_all_DOT_SAT<-(Brazil_baseline$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_baseline$C_MDRTB_hiv_neg_DOT_SAT<-(Brazil_baseline$TB_cases_MDR-Brazil_baseline$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_baseline$C_MDRTB_hiv_DOT_SAT<-Brazil_baseline$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
Brazil_baseline$C_TB_all_DOT_SAT<-Brazil_baseline$C_DSTB_all_DOT_SAT+Brazil_baseline$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_baseline$C_TB_hiv_neg_DOT_SAT<-Brazil_baseline$C_DSTB_hiv_neg_DOT_SAT+Brazil_baseline$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_baseline$C_TB_hiv_DOT_SAT<-Brazil_baseline$C_DSTB_hiv_DOT_SAT+Brazil_baseline$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_baseline$C_TB_all_DOT_SAT)
#sum(Brazil_baseline$C_TB_hiv_neg_DOT_SAT)
#sum(Brazil_baseline$C_TB_hiv_DOT_SAT)

#baseline STRATEGY COST: PT & TB (DOT)
Brazil_baseline$C_baseline_all_DOT_SAT<-Brazil_baseline$C_PT_all+Brazil_baseline$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT
Brazil_baseline$C_baseline_hiv_neg_DOT_SAT<-Brazil_baseline$C_PT_hiv_neg+Brazil_baseline$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT
Brazil_baseline$C_baseline_hiv_DOT_SAT<-Brazil_baseline$C_PT_hiv+Brazil_baseline$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       baseline ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
Brazil_baseline$C_DSTB_all_DOT_SAT_SAT<-(Brazil_baseline$TB_cases_all-Brazil_baseline$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_baseline$C_DSTB_hiv_neg_DOT_SAT_SAT<-((Brazil_baseline$TB_cases_all-Brazil_baseline$TB_cases_MDR)-(Brazil_baseline$TB_cases_HIV-Brazil_baseline$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_baseline$C_DSTB_hiv_DOT_SAT_SAT<-(Brazil_baseline$TB_cases_HIV-Brazil_baseline$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
Brazil_baseline$C_MDRTB_all_DOT_SAT_SAT<-(Brazil_baseline$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_baseline$C_MDRTB_hiv_neg_DOT_SAT_SAT<-(Brazil_baseline$TB_cases_MDR-Brazil_baseline$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_baseline$C_MDRTB_hiv_DOT_SAT_SAT<-Brazil_baseline$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
Brazil_baseline$C_TB_all_DOT_SAT_SAT<-Brazil_baseline$C_DSTB_all_DOT_SAT_SAT+Brazil_baseline$C_MDRTB_all_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_baseline$C_TB_hiv_neg_DOT_SAT_SAT<-Brazil_baseline$C_DSTB_hiv_neg_DOT_SAT_SAT+Brazil_baseline$C_MDRTB_hiv_neg_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_baseline$C_TB_hiv_DOT_SAT_SAT<-Brazil_baseline$C_DSTB_hiv_DOT_SAT_SAT+Brazil_baseline$C_MDRTB_hiv_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_baseline$C_TB_all_DOT_SAT_SAT)
#sum(Brazil_baseline$C_TB_hiv_neg_DOT_SAT_SAT)
#sum(Brazil_baseline$C_TB_hiv_DOT_SAT_SAT)

#baseline STRATEGY COST: PT & TB (DOT_SAT)
Brazil_baseline$C_baseline_all_DOT_SAT_SAT<-Brazil_baseline$C_PT_all+Brazil_baseline$C_TB_all_DOT_SAT_SAT #Cost: strategy all (PT+TB) DOT_SAT
Brazil_baseline$C_baseline_hiv_neg_DOT_SAT_SAT<-Brazil_baseline$C_PT_hiv_neg+Brazil_baseline$C_TB_hiv_neg_DOT_SAT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
Brazil_baseline$C_baseline_hiv_DOT_SAT_SAT<-Brazil_baseline$C_PT_hiv+Brazil_baseline$C_TB_hiv_DOT_SAT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT





#####################################################################################################################
#                                       scaleup ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for scaleup dataset
# PT
Brazil_scaleup$C_PT_all<-(Brazil_scaleup$New_PT_all*(Brazil_costs$LTBI_scaleup+Brazil_costs$AE_LTBI)) # Cost: PT all
Brazil_scaleup$C_PT_hiv_neg<-(Brazil_scaleup$New_PT_all-Brazil_scaleup$New_PT_HIV)*(Brazil_costs$LTBI_scaleup+Brazil_costs$AE_LTBI) # Cost: PT (HIV negative)
Brazil_scaleup$C_PT_hiv<-(Brazil_scaleup$New_PT_HIV*(Brazil_costs$LTBI_scaleup+Brazil_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
Brazil_scaleup$C_DSTB_all_DOT_SAT<-(Brazil_scaleup$TB_cases_all-Brazil_scaleup$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_scaleup$C_DSTB_hiv_neg_DOT_SAT<-((Brazil_scaleup$TB_cases_all-Brazil_scaleup$TB_cases_MDR)-(Brazil_scaleup$TB_cases_HIV-Brazil_scaleup$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_scaleup$C_DSTB_hiv_DOT_SAT<-(Brazil_scaleup$TB_cases_HIV-Brazil_scaleup$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
Brazil_scaleup$C_MDRTB_all_DOT_SAT<-(Brazil_scaleup$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_scaleup$C_MDRTB_hiv_neg_DOT_SAT<-(Brazil_scaleup$TB_cases_MDR-Brazil_scaleup$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_scaleup$C_MDRTB_hiv_DOT_SAT<-Brazil_scaleup$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
Brazil_scaleup$C_TB_all_DOT_SAT<-Brazil_scaleup$C_DSTB_all_DOT_SAT+Brazil_scaleup$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_scaleup$C_TB_hiv_neg_DOT_SAT<-Brazil_scaleup$C_DSTB_hiv_neg_DOT_SAT+Brazil_scaleup$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_scaleup$C_TB_hiv_DOT_SAT<-Brazil_scaleup$C_DSTB_hiv_DOT_SAT+Brazil_scaleup$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_scaleup$C_TB_all_DOT_SAT)
#sum(Brazil_scaleup$C_TB_hiv_neg_DOT_SAT)
#sum(Brazil_scaleup$C_TB_hiv_DOT_SAT)

#scaleup STRATEGY COST: PT & TB (DOT)
Brazil_scaleup$C_scaleup_all_DOT_SAT<-Brazil_scaleup$C_PT_all+Brazil_scaleup$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT
Brazil_scaleup$C_scaleup_hiv_neg_DOT_SAT<-Brazil_scaleup$C_PT_hiv_neg+Brazil_scaleup$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT
Brazil_scaleup$C_scaleup_hiv_DOT_SAT<-Brazil_scaleup$C_PT_hiv+Brazil_scaleup$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       scaleup ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
Brazil_scaleup$C_DSTB_all_DOT_SAT_SAT<-(Brazil_scaleup$TB_cases_all-Brazil_scaleup$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_scaleup$C_DSTB_hiv_neg_DOT_SAT_SAT<-((Brazil_scaleup$TB_cases_all-Brazil_scaleup$TB_cases_MDR)-(Brazil_scaleup$TB_cases_HIV-Brazil_scaleup$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_scaleup$C_DSTB_hiv_DOT_SAT_SAT<-(Brazil_scaleup$TB_cases_HIV-Brazil_scaleup$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
Brazil_scaleup$C_MDRTB_all_DOT_SAT_SAT<-(Brazil_scaleup$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_scaleup$C_MDRTB_hiv_neg_DOT_SAT_SAT<-(Brazil_scaleup$TB_cases_MDR-Brazil_scaleup$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_scaleup$C_MDRTB_hiv_DOT_SAT_SAT<-Brazil_scaleup$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
Brazil_scaleup$C_TB_all_DOT_SAT_SAT<-Brazil_scaleup$C_DSTB_all_DOT_SAT_SAT+Brazil_scaleup$C_MDRTB_all_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_scaleup$C_TB_hiv_neg_DOT_SAT_SAT<-Brazil_scaleup$C_DSTB_hiv_neg_DOT_SAT_SAT+Brazil_scaleup$C_MDRTB_hiv_neg_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_scaleup$C_TB_hiv_DOT_SAT_SAT<-Brazil_scaleup$C_DSTB_hiv_DOT_SAT_SAT+Brazil_scaleup$C_MDRTB_hiv_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_scaleup$C_TB_all_DOT_SAT_SAT)
#sum(Brazil_scaleup$C_TB_hiv_neg_DOT_SAT_SAT)
#sum(Brazil_scaleup$C_TB_hiv_DOT_SAT_SAT)

#scaleup STRATEGY COST: PT & TB (DOT_SAT)
Brazil_scaleup$C_scaleup_all_DOT_SAT_SAT<-Brazil_scaleup$C_PT_all+Brazil_scaleup$C_TB_all_DOT_SAT_SAT #Cost: strategy all (PT+TB) DOT_SAT
Brazil_scaleup$C_scaleup_hiv_neg_DOT_SAT_SAT<-Brazil_scaleup$C_PT_hiv_neg+Brazil_scaleup$C_TB_hiv_neg_DOT_SAT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
Brazil_scaleup$C_scaleup_hiv_DOT_SAT_SAT<-Brazil_scaleup$C_PT_hiv+Brazil_scaleup$C_TB_hiv_DOT_SAT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT




#####################################################################################################################
#                                       minimal ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for minimal dataset
# PT
Brazil_minimal$C_PT_all<-(Brazil_minimal$New_PT_all*(Brazil_costs$LTBI_minimal+Brazil_costs$AE_LTBI)) # Cost: PT all
Brazil_minimal$C_PT_hiv_neg<-(Brazil_minimal$New_PT_all-Brazil_minimal$New_PT_HIV)*(Brazil_costs$LTBI_minimal+Brazil_costs$AE_LTBI) # Cost: PT (HIV negative)
Brazil_minimal$C_PT_hiv<-(Brazil_minimal$New_PT_HIV*(Brazil_costs$LTBI_minimal+Brazil_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
Brazil_minimal$C_DSTB_all_DOT_SAT<-(Brazil_minimal$TB_cases_all-Brazil_minimal$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_minimal$C_DSTB_hiv_neg_DOT_SAT<-((Brazil_minimal$TB_cases_all-Brazil_minimal$TB_cases_MDR)-(Brazil_minimal$TB_cases_HIV-Brazil_minimal$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_minimal$C_DSTB_hiv_DOT_SAT<-(Brazil_minimal$TB_cases_HIV-Brazil_minimal$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
Brazil_minimal$C_MDRTB_all_DOT_SAT<-(Brazil_minimal$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_minimal$C_MDRTB_hiv_neg_DOT_SAT<-(Brazil_minimal$TB_cases_MDR-Brazil_minimal$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_minimal$C_MDRTB_hiv_DOT_SAT<-Brazil_minimal$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
Brazil_minimal$C_TB_all_DOT_SAT<-Brazil_minimal$C_DSTB_all_DOT_SAT+Brazil_minimal$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_minimal$C_TB_hiv_neg_DOT_SAT<-Brazil_minimal$C_DSTB_hiv_neg_DOT_SAT+Brazil_minimal$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_minimal$C_TB_hiv_DOT_SAT<-Brazil_minimal$C_DSTB_hiv_DOT_SAT+Brazil_minimal$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_minimal$C_TB_all_DOT_SAT)
#sum(Brazil_minimal$C_TB_hiv_neg_DOT_SAT)
#sum(Brazil_minimal$C_TB_hiv_DOT_SAT)

#minimal STRATEGY COST: PT & TB (DOT)
Brazil_minimal$C_minimal_all_DOT_SAT<-Brazil_minimal$C_PT_all+Brazil_minimal$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT
Brazil_minimal$C_minimal_hiv_neg_DOT_SAT<-Brazil_minimal$C_PT_hiv_neg+Brazil_minimal$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT
Brazil_minimal$C_minimal_hiv_DOT_SAT<-Brazil_minimal$C_PT_hiv+Brazil_minimal$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       minimal ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
Brazil_minimal$C_DSTB_all_DOT_SAT_SAT<-(Brazil_minimal$TB_cases_all-Brazil_minimal$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_minimal$C_DSTB_hiv_neg_DOT_SAT_SAT<-((Brazil_minimal$TB_cases_all-Brazil_minimal$TB_cases_MDR)-(Brazil_minimal$TB_cases_HIV-Brazil_minimal$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_minimal$C_DSTB_hiv_DOT_SAT_SAT<-(Brazil_minimal$TB_cases_HIV-Brazil_minimal$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
Brazil_minimal$C_MDRTB_all_DOT_SAT_SAT<-(Brazil_minimal$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_minimal$C_MDRTB_hiv_neg_DOT_SAT_SAT<-(Brazil_minimal$TB_cases_MDR-Brazil_minimal$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_minimal$C_MDRTB_hiv_DOT_SAT_SAT<-Brazil_minimal$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
Brazil_minimal$C_TB_all_DOT_SAT_SAT<-Brazil_minimal$C_DSTB_all_DOT_SAT_SAT+Brazil_minimal$C_MDRTB_all_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_minimal$C_TB_hiv_neg_DOT_SAT_SAT<-Brazil_minimal$C_DSTB_hiv_neg_DOT_SAT_SAT+Brazil_minimal$C_MDRTB_hiv_neg_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_minimal$C_TB_hiv_DOT_SAT_SAT<-Brazil_minimal$C_DSTB_hiv_DOT_SAT_SAT+Brazil_minimal$C_MDRTB_hiv_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_minimal$C_TB_all_DOT_SAT_SAT)
#sum(Brazil_minimal$C_TB_hiv_neg_DOT_SAT_SAT)
#sum(Brazil_minimal$C_TB_hiv_DOT_SAT_SAT)

#minimal STRATEGY COST: PT & TB (DOT_SAT)
Brazil_minimal$C_minimal_all_DOT_SAT_SAT<-Brazil_minimal$C_PT_all+Brazil_minimal$C_TB_all_DOT_SAT_SAT #Cost: strategy all (PT+TB) DOT_SAT
Brazil_minimal$C_minimal_hiv_neg_DOT_SAT_SAT<-Brazil_minimal$C_PT_hiv_neg+Brazil_minimal$C_TB_hiv_neg_DOT_SAT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
Brazil_minimal$C_minimal_hiv_DOT_SAT_SAT<-Brazil_minimal$C_PT_hiv+Brazil_minimal$C_TB_hiv_DOT_SAT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT




#####################################################################################################################
#                                       optimal ANNUAL COSTS - DOT                                                 #
#####################################################################################################################

#Annual costs for optimal dataset
# PT
Brazil_optimal$C_PT_all<-(Brazil_optimal$New_PT_all*(Brazil_costs$LTBI_optimal+Brazil_costs$AE_LTBI)) # Cost: PT all
Brazil_optimal$C_PT_hiv_neg<-(Brazil_optimal$New_PT_all-Brazil_optimal$New_PT_HIV)*(Brazil_costs$LTBI_optimal+Brazil_costs$AE_LTBI) # Cost: PT (HIV negative)
Brazil_optimal$C_PT_hiv<-(Brazil_optimal$New_PT_HIV*(Brazil_costs$LTBI_optimal+Brazil_costs$AE_LTBI)) # Cost: PT (HIV+ only)
# TB (DS-TB) DOT
Brazil_optimal$C_DSTB_all_DOT_SAT<-(Brazil_optimal$TB_cases_all-Brazil_optimal$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_optimal$C_DSTB_hiv_neg_DOT_SAT<-((Brazil_optimal$TB_cases_all-Brazil_optimal$TB_cases_MDR)-(Brazil_optimal$TB_cases_HIV-Brazil_optimal$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_optimal$C_DSTB_hiv_DOT_SAT<-(Brazil_optimal$TB_cases_HIV-Brazil_optimal$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT
Brazil_optimal$C_MDRTB_all_DOT_SAT<-(Brazil_optimal$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_optimal$C_MDRTB_hiv_neg_DOT_SAT<-(Brazil_optimal$TB_cases_MDR-Brazil_optimal$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_optimal$C_MDRTB_hiv_DOT_SAT<-Brazil_optimal$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT
Brazil_optimal$C_TB_all_DOT_SAT<-Brazil_optimal$C_DSTB_all_DOT_SAT+Brazil_optimal$C_MDRTB_all_DOT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_optimal$C_TB_hiv_neg_DOT_SAT<-Brazil_optimal$C_DSTB_hiv_neg_DOT_SAT+Brazil_optimal$C_MDRTB_hiv_neg_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_optimal$C_TB_hiv_DOT_SAT<-Brazil_optimal$C_DSTB_hiv_DOT_SAT+Brazil_optimal$C_MDRTB_hiv_DOT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_optimal$C_TB_all_DOT_SAT)
#sum(Brazil_optimal$C_TB_hiv_neg_DOT_SAT)
#sum(Brazil_optimal$C_TB_hiv_DOT_SAT)

#optimal STRATEGY COST: PT & TB (DOT)
Brazil_optimal$C_optimal_all_DOT_SAT<-Brazil_optimal$C_PT_all+Brazil_optimal$C_TB_all_DOT_SAT #Cost: strategy all (PT+TB) DOT
Brazil_optimal$C_optimal_hiv_neg_DOT_SAT<-Brazil_optimal$C_PT_hiv_neg+Brazil_optimal$C_TB_hiv_neg_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT
Brazil_optimal$C_optimal_hiv_DOT_SAT<-Brazil_optimal$C_PT_hiv+Brazil_optimal$C_TB_hiv_DOT_SAT #Cost: strategy HIV negative (PT+TB) DOT

#####################################################################################################################
#                                       optimal ANNUAL COSTS - DOT/SAT                                             #
#####################################################################################################################

# TB (DS-TB) DOT_SAT
Brazil_optimal$C_DSTB_all_DOT_SAT_SAT<-(Brazil_optimal$TB_cases_all-Brazil_optimal$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB
Brazil_optimal$C_DSTB_hiv_neg_DOT_SAT_SAT<-((Brazil_optimal$TB_cases_all-Brazil_optimal$TB_cases_MDR)-(Brazil_optimal$TB_cases_HIV-Brazil_optimal$TB_cases_MDR_HIV))*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV negative)
Brazil_optimal$C_DSTB_hiv_DOT_SAT_SAT<-(Brazil_optimal$TB_cases_HIV-Brazil_optimal$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$DSTB_DOT_SAT_SAT+Brazil_costs$AE_DSTB) # Cost: DS-TB (HIV+)
# TB (MRR-TB) DOT_SAT
Brazil_optimal$C_MDRTB_all_DOT_SAT_SAT<-(Brazil_optimal$TB_cases_MDR)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB
Brazil_optimal$C_MDRTB_hiv_neg_DOT_SAT_SAT<-(Brazil_optimal$TB_cases_MDR-Brazil_optimal$TB_cases_MDR_HIV)*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV negative)
Brazil_optimal$C_MDRTB_hiv_DOT_SAT_SAT<-Brazil_optimal$TB_cases_MDR_HIV*(Brazil_costs$TB_PreDiagnosis+Brazil_costs$TB_Diagnosis+Brazil_costs$MDRTB_DOT_SAT_SAT+Brazil_costs$AE_MDRTB) # Cost: MRR-TB (HIV+)
# TB (DS-TB & MRR-TB) DOT_SAT
Brazil_optimal$C_TB_all_DOT_SAT_SAT<-Brazil_optimal$C_DSTB_all_DOT_SAT_SAT+Brazil_optimal$C_MDRTB_all_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB)
Brazil_optimal$C_TB_hiv_neg_DOT_SAT_SAT<-Brazil_optimal$C_DSTB_hiv_neg_DOT_SAT_SAT+Brazil_optimal$C_MDRTB_hiv_neg_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV negative)
Brazil_optimal$C_TB_hiv_DOT_SAT_SAT<-Brazil_optimal$C_DSTB_hiv_DOT_SAT_SAT+Brazil_optimal$C_MDRTB_hiv_DOT_SAT_SAT # Cost: TB (DS-TB & MRR-TB) (HIV+)
#sum(Brazil_optimal$C_TB_all_DOT_SAT_SAT)
#sum(Brazil_optimal$C_TB_hiv_neg_DOT_SAT_SAT)
#sum(Brazil_optimal$C_TB_hiv_DOT_SAT_SAT)

#optimal STRATEGY COST: PT & TB (DOT_SAT)
Brazil_optimal$C_optimal_all_DOT_SAT_SAT<-Brazil_optimal$C_PT_all+Brazil_optimal$C_TB_all_DOT_SAT_SAT #Cost: strategy all (PT+TB) DOT_SAT
Brazil_optimal$C_optimal_hiv_neg_DOT_SAT_SAT<-Brazil_optimal$C_PT_hiv_neg+Brazil_optimal$C_TB_hiv_neg_DOT_SAT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT
Brazil_optimal$C_optimal_hiv_DOT_SAT_SAT<-Brazil_optimal$C_PT_hiv+Brazil_optimal$C_TB_hiv_DOT_SAT_SAT #Cost: strategy HIV negative (PT+TB) DOT_SAT






#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### PREP DATA FOR ANALYSIS/PLOTTING #######
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#Melt data in key packets for plotting
Brazil_strategy_cost<-data.frame(
  Year=Brazil_baseline$Year,
  Baseline=Brazil_baseline$C_baseline_all_DOT_SAT,
  Baseline_HIV_neg=Brazil_baseline$C_baseline_hiv_neg_DOT_SAT,
  Baseline_PLHIV=Brazil_baseline$C_baseline_hiv_DOT_SAT,
  "Expanded 6H"=Brazil_scaleup$C_scaleup_all_DOT_SAT,
  "Expanded 6H_neg"=Brazil_scaleup$C_scaleup_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=Brazil_scaleup$C_scaleup_hiv_DOT_SAT,
  "Minimal regimen"=Brazil_minimal$C_minimal_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=Brazil_minimal$C_minimal_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=Brazil_minimal$C_minimal_hiv_DOT_SAT,
  "Optimal regimen"=Brazil_optimal$C_optimal_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=Brazil_optimal$C_optimal_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=Brazil_optimal$C_optimal_hiv_DOT_SAT)
Brazil_cost_pop_graph<-melt(Brazil_strategy_cost, id.vars =c("Year"), variable.name = "cost_pop")
Brazil_cost_pop_graph$cum_cost<-ave(Brazil_cost_pop_graph$value, Brazil_cost_pop_graph$cost_pop, FUN=cumsum)
Brazil_cost_pop_graph$Year<- as.Date(ISOdate(Brazil_cost_pop_graph$Year,1,1) ) #set up date variable
Brazil_cost_pop_graph$strategy = ifelse(grepl("Baseline", Brazil_cost_pop_graph$cost_pop), "_baseline",
                                        ifelse(grepl("6H", Brazil_cost_pop_graph$cost_pop),"6H",ifelse(grepl("Minimal", Brazil_cost_pop_graph$cost_pop), "minimal","Optimal")))
Brazil_cost_pop_graph$population = ifelse(!grepl("_", Brazil_cost_pop_graph$cost_pop), "All treatment candidates",
                                          ifelse(grepl("PLHIV", Brazil_cost_pop_graph$cost_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_cost_pop_graph$cost_pop = chartr("."," ",Brazil_cost_pop_graph$cost_pop)

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### PLOTS #######
#####################################################################################################################
#####################################################################################################################


#####################################################################################################################
#Annual cost per population group#
#####################################################################################################################


####################
Brazil_graph_annual_costs <- ggplot(Brazil_cost_pop_graph,aes(Year,value))+
  scale_color_brewer(palette = "Set1")+
  theme_classic()
# annual costs graph (not smoothed out)
Brazil_graph_annual_costs+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_summary(fun = mean,geom = "line",size = 1,aes(colour = strategy))+
  labs(x = "Years",y = expression(paste("Annual cost (US$)")),colour = "")
ggsave("Brazil_graph_annual_cost.jpeg", 
       width = 25, height = 16, units = "cm")

# smoothed annual costs graph

Brazil_graph_annual_costs+
  #  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",size = 1,aes(fill = strategy),alpha = 0.3)+
  scale_x_date(date_breaks="4 years",date_labels="%Y")+
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, scales = "free") +
  guides(fill = "none")+
  stat_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE) +
  # geom_smooth(size = 1,aes(colour = strategy, fill=strategy), method = "loess", span = 0.8, formula = y~x, se=TRUE)+
  labs(x = "Years",y = expression(paste("Annual cost (US$)")),colour = "")
ggsave("Brazil_graph_annual_costs(smoothed).jpeg", 
       width = 25, height = 16, units = "cm")




















#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#                                   Cumulative epi & cost  per population group#
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

#####################################################################################################################
#                                   1A. Cumulative PT & TB cost per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_strategy_cost_cumulative<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$C_baseline_all_DOT_SAT,
  Baseline_HIV_neg=Brazil_baseline$C_baseline_hiv_neg_DOT_SAT,
  Baseline_PLHIV=Brazil_baseline$C_baseline_hiv_DOT_SAT,
  "Expanded 6H"=Brazil_scaleup$C_scaleup_all_DOT_SAT,
  "Expanded 6H_neg"=Brazil_scaleup$C_scaleup_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=Brazil_scaleup$C_scaleup_hiv_DOT_SAT,
  "Minimal regimen"=Brazil_minimal$C_minimal_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=Brazil_minimal$C_minimal_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=Brazil_minimal$C_minimal_hiv_DOT_SAT,
  "Optimal regimen"=Brazil_optimal$C_optimal_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=Brazil_optimal$C_optimal_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=Brazil_optimal$C_optimal_hiv_DOT_SAT)

Brazil_cost_pop_graph_cumulative<-melt(Brazil_strategy_cost_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
Brazil_cost_pop_graph_cumulative$cum_cost<-ave(Brazil_cost_pop_graph_cumulative$value, Brazil_cost_pop_graph_cumulative$cost_pop, FUN=cumsum)
# Brazil_cost_pop_graph_cumulative$Year<- as.Date(ISOdate(Brazil_cost_pop_graph_cumulative$Year,1,1) ) #set up date variable
Brazil_cost_pop_graph_cumulative$strategy = ifelse(grepl("Baseline", Brazil_cost_pop_graph_cumulative$cost_pop), "_baseline",
                                                   ifelse(grepl("6H", Brazil_cost_pop_graph_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", Brazil_cost_pop_graph_cumulative$cost_pop), "minimal","Optimal")))
Brazil_cost_pop_graph_cumulative$population = ifelse(!grepl("_", Brazil_cost_pop_graph_cumulative$cost_pop), "All treatment candidates",
                                                     ifelse(grepl("PLHIV", Brazil_cost_pop_graph_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_cost_pop_graph_cumulative$cost_pop = chartr("."," ",Brazil_cost_pop_graph_cumulative$cost_pop)

#####################################################################################################################

Brazil_cost_cumulat<-aggregate(x=Brazil_cost_pop_graph_cumulative$value, by=list(Runs=Brazil_cost_pop_graph_cumulative$Runs,cost_pop=Brazil_cost_pop_graph_cumulative$cost_pop,strategy=Brazil_cost_pop_graph_cumulative$strategy,population=Brazil_cost_pop_graph_cumulative$population), FUN=sum)
#cumulative cost graph
Brazil_cost_cumulat_graph <- ggplot(data=Brazil_cost_cumulat, aes(cost_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative cost (US$ Billions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("1A. Brazil_graph_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_cost_table<-Brazil_cost_cumulat %>% 
  group_by(cost_pop, strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_cost_table, file = "1A. Brazil cumulative cost output tables.csv")



#####################################################################################################################
#                                   1B. Cumulative TB cost per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_strategy_cost_TB_cumulative<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$C_TB_all_DOT_SAT,
  Baseline_HIV_neg=Brazil_baseline$C_TB_hiv_neg_DOT_SAT,
  Baseline_PLHIV=Brazil_baseline$C_TB_hiv_DOT_SAT,
  "Expanded 6H"=Brazil_scaleup$C_TB_all_DOT_SAT,
  "Expanded 6H_neg"=Brazil_scaleup$C_TB_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=Brazil_scaleup$C_TB_hiv_DOT_SAT,
  "Minimal regimen"=Brazil_minimal$C_TB_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=Brazil_minimal$C_TB_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=Brazil_minimal$C_TB_hiv_DOT_SAT,
  "Optimal regimen"=Brazil_optimal$C_TB_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=Brazil_optimal$C_TB_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=Brazil_optimal$C_TB_hiv_DOT_SAT)

Brazil_cost_pop_graph_TB_cumulative<-melt(Brazil_strategy_cost_TB_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
Brazil_cost_pop_graph_TB_cumulative$cum_cost<-ave(Brazil_cost_pop_graph_TB_cumulative$value, Brazil_cost_pop_graph_TB_cumulative$cost_pop, FUN=cumsum)
# Brazil_cost_pop_graph_TB_cumulative$Year<- as.Date(ISOdate(Brazil_cost_pop_graph_TB_cumulative$Year,1,1) ) #set up date variable
Brazil_cost_pop_graph_TB_cumulative$strategy = ifelse(grepl("Baseline", Brazil_cost_pop_graph_TB_cumulative$cost_pop), "_baseline",
                                                      ifelse(grepl("6H", Brazil_cost_pop_graph_TB_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", Brazil_cost_pop_graph_TB_cumulative$cost_pop), "minimal","Optimal")))
Brazil_cost_pop_graph_TB_cumulative$population = ifelse(!grepl("_", Brazil_cost_pop_graph_TB_cumulative$cost_pop), "All treatment candidates",
                                                        ifelse(grepl("PLHIV", Brazil_cost_pop_graph_TB_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_cost_pop_graph_TB_cumulative$cost_pop = chartr("."," ",Brazil_cost_pop_graph_TB_cumulative$cost_pop)

#####################################################################################################################

Brazil_cost_TB_cumulat<-aggregate(x=Brazil_cost_pop_graph_TB_cumulative$value, by=list(Runs=Brazil_cost_pop_graph_TB_cumulative$Runs,cost_pop=Brazil_cost_pop_graph_TB_cumulative$cost_pop,strategy=Brazil_cost_pop_graph_TB_cumulative$strategy,population=Brazil_cost_pop_graph_TB_cumulative$population), FUN=sum)
#cumulative cost graph
Brazil_cost_TB_cumulat_graph <- ggplot(data=Brazil_cost_TB_cumulat, aes(cost_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cost of diagnosis and treatment of TB (US$ Billions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("1B. Brazil_graph_TB_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_cost_table<-Brazil_cost_TB_cumulat %>% 
  group_by(cost_pop, strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_cost_table, file = "1B. Brazil cumulative cost TB dig&tx output tables.csv")



#####################################################################################################################
#                                   1C. Cumulative RR-TB cost per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_strategy_cost_MDRTB_cumulative<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$C_MDRTB_all_DOT_SAT,
  Baseline_HIV_neg=Brazil_baseline$C_MDRTB_hiv_neg_DOT_SAT,
  Baseline_PLHIV=Brazil_baseline$C_MDRTB_hiv_DOT_SAT,
  "Expanded 6H"=Brazil_scaleup$C_MDRTB_all_DOT_SAT,
  "Expanded 6H_neg"=Brazil_scaleup$C_MDRTB_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=Brazil_scaleup$C_MDRTB_hiv_DOT_SAT,
  "Minimal regimen"=Brazil_minimal$C_MDRTB_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=Brazil_minimal$C_MDRTB_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=Brazil_minimal$C_MDRTB_hiv_DOT_SAT,
  "Optimal regimen"=Brazil_optimal$C_MDRTB_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=Brazil_optimal$C_MDRTB_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=Brazil_optimal$C_MDRTB_hiv_DOT_SAT)

Brazil_cost_pop_graph_MDRTB_cumulative<-melt(Brazil_strategy_cost_MDRTB_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
Brazil_cost_pop_graph_MDRTB_cumulative$cum_cost<-ave(Brazil_cost_pop_graph_MDRTB_cumulative$value, Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop, FUN=cumsum)
# Brazil_cost_pop_graph_MDRTB_cumulative$Year<- as.Date(ISOdate(Brazil_cost_pop_graph_MDRTB_cumulative$Year,1,1) ) #set up date variable
Brazil_cost_pop_graph_MDRTB_cumulative$strategy = ifelse(grepl("Baseline", Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop), "_baseline",
                                                         ifelse(grepl("6H", Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop), "minimal","Optimal")))
Brazil_cost_pop_graph_MDRTB_cumulative$population = ifelse(!grepl("_", Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop), "All treatment candidates",
                                                           ifelse(grepl("PLHIV", Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop = chartr("."," ",Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop)

#####################################################################################################################

Brazil_cost_MDRTB_cumulat<-aggregate(x=Brazil_cost_pop_graph_MDRTB_cumulative$value, by=list(Runs=Brazil_cost_pop_graph_MDRTB_cumulative$Runs,cost_pop=Brazil_cost_pop_graph_MDRTB_cumulative$cost_pop,strategy=Brazil_cost_pop_graph_MDRTB_cumulative$strategy,population=Brazil_cost_pop_graph_MDRTB_cumulative$population), FUN=sum)
#cumulative cost graph
Brazil_cost_MDRTB_cumulat_graph <- ggplot(data=Brazil_cost_MDRTB_cumulat, aes(cost_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cost of diagnosis and treatment of  RR-TB (US$ Billions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("1C. Brazil_graph_RR-TB_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_rr_cost_table<-Brazil_cost_MDRTB_cumulat %>% 
  group_by(cost_pop, strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_rr_cost_table, file = "1C. Brazil cumulative cost RR-TB dig&tx output tables.csv")




#####################################################################################################################
#                                   1D. Cumulative PT - cost per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_strategy_PT_cost_cumulative<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$C_PT_all,
  Baseline_HIV_neg=Brazil_baseline$C_PT_hiv_neg,
  Baseline_PLHIV=Brazil_baseline$C_PT_hiv,
  "Expanded 6H"=Brazil_scaleup$C_PT_all,
  "Expanded 6H_neg"=Brazil_scaleup$C_PT_hiv_neg,
  "Expanded 6H_PLHIV"=Brazil_scaleup$C_PT_hiv,
  "Minimal regimen"=Brazil_minimal$C_PT_all,
  "Minimal regimen_HIV_neg"=Brazil_minimal$C_PT_hiv_neg,
  "Minimal regimen_PLHIV"=Brazil_minimal$C_PT_hiv,
  "Optimal regimen"=Brazil_optimal$C_PT_all,
  "Optimal regimen_HIV_neg"=Brazil_optimal$C_PT_hiv_neg,
  "Optimal regimen_PLHIV"=Brazil_optimal$C_PT_hiv)

Brazil_PT_cost_pop_graph_cumulative<-melt(Brazil_strategy_PT_cost_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
Brazil_PT_cost_pop_graph_cumulative$strategy = ifelse(grepl("Baseline", Brazil_PT_cost_pop_graph_cumulative$cost_pop), "_baseline",
                                                      ifelse(grepl("6H", Brazil_PT_cost_pop_graph_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", Brazil_PT_cost_pop_graph_cumulative$cost_pop), "minimal","Optimal")))
Brazil_PT_cost_pop_graph_cumulative$population = ifelse(!grepl("_", Brazil_PT_cost_pop_graph_cumulative$cost_pop), "All treatment candidates",
                                                        ifelse(grepl("PLHIV", Brazil_PT_cost_pop_graph_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_PT_cost_pop_graph_cumulative$cost_pop = chartr("."," ",Brazil_PT_cost_pop_graph_cumulative$cost_pop)

#####################################################################################################################

Brazil_PT_cost_cumulat<-aggregate(x=Brazil_PT_cost_pop_graph_cumulative$value, by=list(Runs=Brazil_PT_cost_pop_graph_cumulative$Runs,cost_pop=Brazil_PT_cost_pop_graph_cumulative$cost_pop,strategy=Brazil_PT_cost_pop_graph_cumulative$strategy,population=Brazil_PT_cost_pop_graph_cumulative$population), FUN=sum)
#cumulative cost graph
Brazil_PT_cost_cumulat_graph <- ggplot(data=Brazil_PT_cost_cumulat, aes(cost_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative PT cost (US$ Millions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("1D. Brazil_PT_graph_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_cost_table<-Brazil_PT_cost_cumulat %>% 
  group_by(cost_pop, strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_cost_table, file = "1D. Brazil cumulative PT cost output tables.csv")




# combine cumulative cost plots multiple
Brazil_cumulative_combine<-ggarrange(Brazil_PT_cost_cumulat_graph, Brazil_cost_TB_cumulat_graph, Brazil_cost_MDRTB_cumulat_graph, Brazil_cost_cumulat_graph, labels = c("PT", "TB", "RR-TB", "PT&TB"),
                                     common.legend = TRUE, legend = "bottom")
Brazil_cumulative_combine
ggsave("7. Brazil_combined_graphs_cumulative.png", 
       width = 30, height = 20, units = "cm")

##
#####################################################################################################################
#                                  2A. Cumulative overall DS-TB per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_epi_cumulative_DSTB<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$TB_cases_all-Brazil_baseline$TB_cases_MDR,
  Baseline_HIV_neg=(Brazil_baseline$TB_cases_all-Brazil_baseline$TB_cases_MDR)-(Brazil_baseline$TB_cases_HIV-Brazil_baseline$TB_cases_MDR_HIV),
  Baseline_PLHIV=Brazil_baseline$TB_cases_HIV-Brazil_baseline$TB_cases_MDR_HIV,
  "Expanded 6H"=Brazil_scaleup$TB_cases_all-Brazil_scaleup$TB_cases_MDR,
  "Expanded 6H_neg"=(Brazil_scaleup$TB_cases_all-Brazil_scaleup$TB_cases_MDR)-(Brazil_scaleup$TB_cases_HIV-Brazil_scaleup$TB_cases_MDR_HIV),
  "Expanded 6H_PLHIV"=Brazil_scaleup$TB_cases_HIV-Brazil_scaleup$TB_cases_MDR_HIV,
  "Minimal regimen"=Brazil_minimal$TB_cases_all-Brazil_minimal$TB_cases_MDR,
  "Minimal regimen_HIV_neg"=(Brazil_minimal$TB_cases_all-Brazil_minimal$TB_cases_MDR)-(Brazil_minimal$TB_cases_HIV-Brazil_minimal$TB_cases_MDR_HIV),
  "Minimal regimen_PLHIV"=Brazil_minimal$TB_cases_HIV-Brazil_minimal$TB_cases_MDR_HIV,
  "Optimal regimen"=Brazil_optimal$TB_cases_all-Brazil_optimal$TB_cases_MDR,
  "Optimal regimen_HIV_neg"=(Brazil_optimal$TB_cases_all-Brazil_optimal$TB_cases_MDR)-(Brazil_optimal$TB_cases_HIV-Brazil_optimal$TB_cases_MDR_HIV),
  "Optimal regimen_PLHIV"=Brazil_optimal$TB_cases_HIV-Brazil_optimal$TB_cases_MDR_HIV)

Brazil_epi_cumulative_DSTB_graph<-melt(Brazil_epi_cumulative_DSTB, id.vars =c("Runs"), variable.name = "epi_pop")
Brazil_epi_cumulative_DSTB_graph$strategy = ifelse(grepl("Baseline", Brazil_epi_cumulative_DSTB_graph$epi_pop), "_baseline",
                                                   ifelse(grepl("6H", Brazil_epi_cumulative_DSTB_graph$epi_pop),"6H",ifelse(grepl("Minimal", Brazil_epi_cumulative_DSTB_graph$epi_pop), "minimal","Optimal")))
Brazil_epi_cumulative_DSTB_graph$population = ifelse(!grepl("_", Brazil_epi_cumulative_DSTB_graph$epi_pop), "All treatment candidates",
                                                     ifelse(grepl("PLHIV", Brazil_epi_cumulative_DSTB_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_epi_cumulative_DSTB_graph$epi_pop = chartr("."," ",Brazil_epi_cumulative_DSTB_graph$epi_pop)

#####################################################################################################################

Brazil_epi_DSTB_cumulat<-aggregate(x=Brazil_epi_cumulative_DSTB_graph$value, by=list(Runs=Brazil_epi_cumulative_DSTB_graph$Runs,epi_pop=Brazil_epi_cumulative_DSTB_graph$epi_pop,strategy=Brazil_epi_cumulative_DSTB_graph$strategy,population=Brazil_epi_cumulative_DSTB_graph$population), FUN=sum)
#cumulative epi graph
Brazil_epi_DSTB_cumulat_graph <- ggplot(data=Brazil_epi_DSTB_cumulat, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative TB cases  (DS-TB)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("2A. Epidemiological projections - DS-TB cases, Brazil, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_epi_DSTB_table<-Brazil_epi_DSTB_cumulat %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_epi_table, file = "2A. Brazil - cumulative DS-TB cases tables.csv")






#####################################################################################################################
#                                  2B. Cumulative overall TB per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_epi_cumulative_TBall<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$TB_cases_all,
  Baseline_HIV_neg=Brazil_baseline$TB_cases_all-Brazil_baseline$TB_cases_HIV,
  Baseline_PLHIV=Brazil_baseline$TB_cases_HIV,
  "Expanded 6H"=Brazil_scaleup$TB_cases_all,
  "Expanded 6H_neg"=Brazil_scaleup$TB_cases_all-Brazil_scaleup$TB_cases_HIV,
  "Expanded 6H_PLHIV"=Brazil_scaleup$TB_cases_HIV,
  "Minimal regimen"=Brazil_minimal$TB_cases_all,
  "Minimal regimen_HIV_neg"=Brazil_minimal$TB_cases_all-Brazil_minimal$TB_cases_HIV,
  "Minimal regimen_PLHIV"=Brazil_minimal$TB_cases_HIV,
  "Optimal regimen"=Brazil_optimal$TB_cases_all,
  "Optimal regimen_HIV_neg"=Brazil_optimal$TB_cases_all-Brazil_optimal$TB_cases_HIV,
  "Optimal regimen_PLHIV"=Brazil_optimal$TB_cases_HIV)

Brazil_epi_cumulative_TBall_graph<-melt(Brazil_epi_cumulative_TBall, id.vars =c("Runs"), variable.name = "epi_pop")
Brazil_epi_cumulative_TBall_graph$strategy = ifelse(grepl("Baseline", Brazil_epi_cumulative_TBall_graph$epi_pop), "_baseline",
                                                    ifelse(grepl("6H", Brazil_epi_cumulative_TBall_graph$epi_pop),"6H",ifelse(grepl("Minimal", Brazil_epi_cumulative_TBall_graph$epi_pop), "minimal","Optimal")))
Brazil_epi_cumulative_TBall_graph$population = ifelse(!grepl("_", Brazil_epi_cumulative_TBall_graph$epi_pop), "All treatment candidates",
                                                      ifelse(grepl("PLHIV", Brazil_epi_cumulative_TBall_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_epi_cumulative_TBall_graph$epi_pop = chartr("."," ",Brazil_epi_cumulative_TBall_graph$epi_pop)

#####################################################################################################################

Brazil_epi_cumulat<-aggregate(x=Brazil_epi_cumulative_TBall_graph$value, by=list(Runs=Brazil_epi_cumulative_TBall_graph$Runs,epi_pop=Brazil_epi_cumulative_TBall_graph$epi_pop,strategy=Brazil_epi_cumulative_TBall_graph$strategy,population=Brazil_epi_cumulative_TBall_graph$population), FUN=sum)
#cumulative epi graph
Brazil_epi_cumulat_graph <- ggplot(data=Brazil_epi_cumulat, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative TB cases  (DS-TB & RR-TB)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("2B. Epidemiological projections, Brazil, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_epi_table<-Brazil_epi_cumulat %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_epi_table, file = "2B. Brazil - cumulative TB cases (DS-TB & RR-TB) tables.csv")





#####################################################################################################################
#                                  2C. Cumulative RR-TB per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_epi_cumulative_DRTB<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$TB_cases_MDR,
  Baseline_HIV_neg=Brazil_baseline$TB_cases_MDR-Brazil_baseline$TB_cases_MDR_HIV,
  Baseline_PLHIV=Brazil_baseline$TB_cases_MDR_HIV,
  "Expanded 6H"=Brazil_scaleup$TB_cases_MDR,
  "Expanded 6H_neg"=Brazil_scaleup$TB_cases_MDR-Brazil_scaleup$TB_cases_MDR_HIV,
  "Expanded 6H_PLHIV"=Brazil_scaleup$TB_cases_MDR_HIV,
  "Minimal regimen"=Brazil_minimal$TB_cases_MDR,
  "Minimal regimen_HIV_neg"=Brazil_minimal$TB_cases_MDR-Brazil_minimal$TB_cases_MDR_HIV,
  "Minimal regimen_PLHIV"=Brazil_minimal$TB_cases_MDR_HIV,
  "Optimal regimen"=Brazil_optimal$TB_cases_MDR,
  "Optimal regimen_HIV_neg"=Brazil_optimal$TB_cases_MDR-Brazil_optimal$TB_cases_MDR_HIV,
  "Optimal regimen_PLHIV"=Brazil_optimal$TB_cases_MDR_HIV)

Brazil_epi_cumulative_DRTB_graph<-melt(Brazil_epi_cumulative_DRTB, id.vars =c("Runs"), variable.name = "epi_pop")
Brazil_epi_cumulative_DRTB_graph$strategy = ifelse(grepl("Baseline", Brazil_epi_cumulative_DRTB_graph$epi_pop), "_baseline",
                                                   ifelse(grepl("6H", Brazil_epi_cumulative_DRTB_graph$epi_pop),"6H",ifelse(grepl("Minimal", Brazil_epi_cumulative_DRTB_graph$epi_pop), "minimal","Optimal")))
Brazil_epi_cumulative_DRTB_graph$population = ifelse(!grepl("_", Brazil_epi_cumulative_DRTB_graph$epi_pop), "All treatment candidates",
                                                     ifelse(grepl("PLHIV", Brazil_epi_cumulative_DRTB_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_epi_cumulative_DRTB_graph$epi_pop = chartr("."," ",Brazil_epi_cumulative_DRTB_graph$epi_pop)

#####################################################################################################################

Brazil_epi_cumulat_DRTB<-aggregate(x=Brazil_epi_cumulative_DRTB_graph$value, by=list(Runs=Brazil_epi_cumulative_DRTB_graph$Runs,epi_pop=Brazil_epi_cumulative_DRTB_graph$epi_pop,strategy=Brazil_epi_cumulative_DRTB_graph$strategy,population=Brazil_epi_cumulative_DRTB_graph$population), FUN=sum)
#cumulative epi graph
Brazil_epi_cumulat_graph_DRTB <- ggplot(data=Brazil_epi_cumulat_DRTB, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative RR-TB cases")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("2C. RR-TB Epidemiological projections, Brazil, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_DRTB_epi_table<-Brazil_epi_cumulat_DRTB %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_DRTB_epi_table, file = "2C. Brazil - cumulative RR-TB cases tables.csv")





#####################################################################################################################
#                                  2D. Cumulative overall PT per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_epi_cumulative_PT<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$New_PT_all,
  Baseline_HIV_neg=Brazil_baseline$New_PT_all-Brazil_baseline$New_PT_HIV,
  Baseline_PLHIV=Brazil_baseline$New_PT_HIV,
  "Expanded 6H"=Brazil_scaleup$New_PT_all,
  "Expanded 6H_neg"=Brazil_scaleup$New_PT_all-Brazil_scaleup$New_PT_HIV,
  "Expanded 6H_PLHIV"=Brazil_scaleup$New_PT_HIV,
  "Minimal regimen"=Brazil_minimal$New_PT_all,
  "Minimal regimen_HIV_neg"=Brazil_minimal$New_PT_all-Brazil_minimal$New_PT_HIV,
  "Minimal regimen_PLHIV"=Brazil_minimal$New_PT_HIV,
  "Optimal regimen"=Brazil_optimal$New_PT_all,
  "Optimal regimen_HIV_neg"=Brazil_optimal$New_PT_all-Brazil_optimal$New_PT_HIV,
  "Optimal regimen_PLHIV"=Brazil_optimal$New_PT_HIV)

Brazil_epi_cumulative_PT_graph<-melt(Brazil_epi_cumulative_PT, id.vars =c("Runs"), variable.name = "epi_pop")
Brazil_epi_cumulative_PT_graph$strategy = ifelse(grepl("Baseline", Brazil_epi_cumulative_PT_graph$epi_pop), "_baseline",
                                                 ifelse(grepl("6H", Brazil_epi_cumulative_PT_graph$epi_pop),"6H",ifelse(grepl("Minimal", Brazil_epi_cumulative_PT_graph$epi_pop), "minimal","Optimal")))
Brazil_epi_cumulative_PT_graph$population = ifelse(!grepl("_", Brazil_epi_cumulative_PT_graph$epi_pop), "All treatment candidates",
                                                   ifelse(grepl("PLHIV", Brazil_epi_cumulative_PT_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_epi_cumulative_PT_graph$epi_pop = chartr("."," ",Brazil_epi_cumulative_PT_graph$epi_pop)

#####################################################################################################################

Brazil_PT_epi_cumulat<-aggregate(x=Brazil_epi_cumulative_PT_graph$value, by=list(Runs=Brazil_epi_cumulative_PT_graph$Runs,epi_pop=Brazil_epi_cumulative_PT_graph$epi_pop,strategy=Brazil_epi_cumulative_PT_graph$strategy,population=Brazil_epi_cumulative_PT_graph$population), FUN=sum)
#cumulative epi graph
Brazil_PT_epi_cumulat_graph <- ggplot(data=Brazil_PT_epi_cumulat, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("Individuals on preventive therapy, 2020–2035")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("2D. Epidemiological PT projections, Brazil, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_PT_epi_table<-Brazil_PT_epi_cumulat %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_PT_epi_table, file = "2D. Brazil - Total individuals on preventive therapy.csv")






#####################################################################################################################
#                                  3. Cumulative TB deaths per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_epi_cumulative_TBdeaths<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$TB_deaths_all,
  Baseline_HIV_neg=Brazil_baseline$TB_deaths_all-Brazil_baseline$TB_deaths_HIV,
  Baseline_PLHIV=Brazil_baseline$TB_deaths_HIV,
  "Expanded 6H"=Brazil_scaleup$TB_deaths_all,
  "Expanded 6H_neg"=Brazil_scaleup$TB_deaths_all-Brazil_scaleup$TB_deaths_HIV,
  "Expanded 6H_PLHIV"=Brazil_scaleup$TB_deaths_HIV,
  "Minimal regimen"=Brazil_minimal$TB_deaths_all,
  "Minimal regimen_HIV_neg"=Brazil_minimal$TB_deaths_all-Brazil_minimal$TB_deaths_HIV,
  "Minimal regimen_PLHIV"=Brazil_minimal$TB_deaths_HIV,
  "Optimal regimen"=Brazil_optimal$TB_deaths_all,
  "Optimal regimen_HIV_neg"=Brazil_optimal$TB_deaths_all-Brazil_optimal$TB_deaths_HIV,
  "Optimal regimen_PLHIV"=Brazil_optimal$TB_deaths_HIV)

Brazil_epi_cumulative_TBdeaths_graph<-melt(Brazil_epi_cumulative_TBdeaths, id.vars =c("Runs"), variable.name = "epi_pop")
Brazil_epi_cumulative_TBdeaths_graph$strategy = ifelse(grepl("Baseline", Brazil_epi_cumulative_TBdeaths_graph$epi_pop), "_baseline",
                                                       ifelse(grepl("6H", Brazil_epi_cumulative_TBdeaths_graph$epi_pop),"6H",ifelse(grepl("Minimal", Brazil_epi_cumulative_TBdeaths_graph$epi_pop), "minimal","Optimal")))
Brazil_epi_cumulative_TBdeaths_graph$population = ifelse(!grepl("_", Brazil_epi_cumulative_TBdeaths_graph$epi_pop), "All treatment candidates",
                                                         ifelse(grepl("PLHIV", Brazil_epi_cumulative_TBdeaths_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_epi_cumulative_TBdeaths_graph$epi_pop = chartr("."," ",Brazil_epi_cumulative_TBdeaths_graph$epi_pop)

#####################################################################################################################

Brazil_epi_cumulat_TBdeaths<-aggregate(x=Brazil_epi_cumulative_TBdeaths_graph$value, by=list(Runs=Brazil_epi_cumulative_TBdeaths_graph$Runs,epi_pop=Brazil_epi_cumulative_TBdeaths_graph$epi_pop,strategy=Brazil_epi_cumulative_TBdeaths_graph$strategy,population=Brazil_epi_cumulative_TBdeaths_graph$population), FUN=sum)
#cumulative epi graph
Brazil_epi_cumulat_graph_TBdeaths <- ggplot(data=Brazil_epi_cumulat_TBdeaths, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("TB deaths")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("3. TB deaths Epidemiological projections, Brazil, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_TBdeaths_epi_table<-Brazil_epi_cumulat_TBdeaths %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_TBdeaths_epi_table, file = "3. Brazil - cumulative TB deaths tables.csv")








#####################################################################################################################
#                                  4. Cumulative DALYs per population group#
#####################################################################################################################
#Data wrangling: Melt data in key packets for plotting
Brazil_epi_cumulative_DALYs<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$YLD+Brazil_baseline$YLL,
  Baseline_HIV_neg=(Brazil_baseline$YLD+Brazil_baseline$YLL)-(Brazil_baseline$YLD_HIV+Brazil_baseline$YLL_HIV),
  Baseline_PLHIV=Brazil_baseline$YLD_HIV+Brazil_baseline$YLL_HIV,
  "Expanded 6H"=Brazil_scaleup$YLD+Brazil_scaleup$YLL,
  "Expanded 6H_neg"=(Brazil_scaleup$YLD+Brazil_scaleup$YLL)-(Brazil_scaleup$YLD_HIV+Brazil_scaleup$YLL_HIV),
  "Expanded 6H_PLHIV"=Brazil_scaleup$YLD_HIV+Brazil_scaleup$YLL_HIV,
  "Minimal regimen"=Brazil_minimal$YLD+Brazil_minimal$YLL,
  "Minimal regimen_HIV_neg"=(Brazil_minimal$YLD+Brazil_minimal$YLL)-(Brazil_minimal$YLD_HIV+Brazil_minimal$YLL_HIV),
  "Minimal regimen_PLHIV"=Brazil_minimal$YLD_HIV+Brazil_minimal$YLL_HIV,
  "Optimal regimen"=Brazil_optimal$YLD+Brazil_optimal$YLL,
  "Optimal regimen_HIV_neg"=(Brazil_optimal$YLD+Brazil_optimal$YLL)-(Brazil_optimal$YLD_HIV+Brazil_optimal$YLL_HIV),
  "Optimal regimen_PLHIV"=Brazil_optimal$YLD_HIV+Brazil_optimal$YLL_HIV)

Brazil_epi_cumulative_DALYs_graph<-melt(Brazil_epi_cumulative_DALYs, id.vars =c("Runs"), variable.name = "epi_pop")
Brazil_epi_cumulative_DALYs_graph$strategy = ifelse(grepl("Baseline", Brazil_epi_cumulative_DALYs_graph$epi_pop), "_baseline",
                                                    ifelse(grepl("6H", Brazil_epi_cumulative_DALYs_graph$epi_pop),"6H",ifelse(grepl("Minimal", Brazil_epi_cumulative_DALYs_graph$epi_pop), "minimal","Optimal")))
Brazil_epi_cumulative_DALYs_graph$population = ifelse(!grepl("_", Brazil_epi_cumulative_DALYs_graph$epi_pop), "All treatment candidates",
                                                      ifelse(grepl("PLHIV", Brazil_epi_cumulative_DALYs_graph$epi_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_epi_cumulative_DALYs_graph$epi_pop = chartr("."," ",Brazil_epi_cumulative_DALYs_graph$epi_pop)

#####################################################################################################################

Brazil_epi_cumulat_DALYs<-aggregate(x=Brazil_epi_cumulative_DALYs_graph$value, by=list(Runs=Brazil_epi_cumulative_DALYs_graph$Runs,epi_pop=Brazil_epi_cumulative_DALYs_graph$epi_pop,strategy=Brazil_epi_cumulative_DALYs_graph$strategy,population=Brazil_epi_cumulative_DALYs_graph$population), FUN=sum)
#cumulative epi graph
Brazil_epi_cumulat_graph_DALYs <- ggplot(data=Brazil_epi_cumulat_DALYs, aes(epi_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("DALYs")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("4. DALYs - Epidemiological projections, Brazil, 2020–2035.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_DALYs_epi_table<-Brazil_epi_cumulat_DALYs %>% 
  group_by(strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_DALYs_epi_table, file = "4. Brazil - cumulative DALYs tables.csv")





#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#                                     Incremental cost                                                              #
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
Brazil_strategy_cost_cumulative<-data.frame(
  Runs=Brazil_baseline$Run,
  Baseline=Brazil_baseline$C_baseline_all_DOT_SAT,
  Baseline_HIV_neg=Brazil_baseline$C_baseline_hiv_neg_DOT_SAT,
  Baseline_PLHIV=Brazil_baseline$C_baseline_hiv_DOT_SAT,
  "Expanded 6H"=Brazil_scaleup$C_scaleup_all_DOT_SAT,
  "Expanded 6H_neg"=Brazil_scaleup$C_scaleup_hiv_neg_DOT_SAT,
  "Expanded 6H_PLHIV"=Brazil_scaleup$C_scaleup_hiv_DOT_SAT,
  "Minimal regimen"=Brazil_minimal$C_minimal_all_DOT_SAT,
  "Minimal regimen_HIV_neg"=Brazil_minimal$C_minimal_hiv_neg_DOT_SAT,
  "Minimal regimen_PLHIV"=Brazil_minimal$C_minimal_hiv_DOT_SAT,
  "Optimal regimen"=Brazil_optimal$C_optimal_all_DOT_SAT,
  "Optimal regimen_HIV_neg"=Brazil_optimal$C_optimal_hiv_neg_DOT_SAT,
  "Optimal regimen_PLHIV"=Brazil_optimal$C_optimal_hiv_DOT_SAT)

Brazil_cost_pop_graph_cumulative<-melt(Brazil_strategy_cost_cumulative, id.vars =c("Runs"), variable.name = "cost_pop")
Brazil_cost_pop_graph_cumulative$cum_cost<-ave(Brazil_cost_pop_graph_cumulative$value, Brazil_cost_pop_graph_cumulative$cost_pop, FUN=cumsum)
# Brazil_cost_pop_graph_cumulative$Year<- as.Date(ISOdate(Brazil_cost_pop_graph_cumulative$Year,1,1) ) #set up date variable
Brazil_cost_pop_graph_cumulative$strategy = ifelse(grepl("Baseline", Brazil_cost_pop_graph_cumulative$cost_pop), "_baseline",
                                                   ifelse(grepl("6H", Brazil_cost_pop_graph_cumulative$cost_pop),"6H",ifelse(grepl("Minimal", Brazil_cost_pop_graph_cumulative$cost_pop), "minimal","Optimal")))
Brazil_cost_pop_graph_cumulative$population = ifelse(!grepl("_", Brazil_cost_pop_graph_cumulative$cost_pop), "All treatment candidates",
                                                     ifelse(grepl("PLHIV", Brazil_cost_pop_graph_cumulative$cost_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_cost_pop_graph_cumulative$cost_pop = chartr("."," ",Brazil_cost_pop_graph_cumulative$cost_pop)

#####################################################################################################################

Brazil_cost_cumulat<-aggregate(x=Brazil_cost_pop_graph_cumulative$value, by=list(Runs=Brazil_cost_pop_graph_cumulative$Runs,cost_pop=Brazil_cost_pop_graph_cumulative$cost_pop,strategy=Brazil_cost_pop_graph_cumulative$strategy,population=Brazil_cost_pop_graph_cumulative$population), FUN=sum)
#cumulative cost graph
Brazil_cost_cumulat_graph <- ggplot(data=Brazil_cost_cumulat, aes(cost_pop,x,color=strategy)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
  facet_wrap(~population, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("cumulative cost (US$ Billions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("1A. Brazil_graph_cumulative_costs.jpeg", 
       width = 25, height = 16, units = "cm")



Brazil_overall_cost_table<-Brazil_cost_cumulat %>% 
  group_by(cost_pop, strategy,population) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_overall_cost_table, file = "1A. Brazil cumulative cost output tables.csv")




#Incremental cost - Overall (PT + TB)
Brazil_c_baseline_vs_6h_overall <- data.frame(Runs=Brazil_baseline$Run,Brazil_strategy_cost$Expanded.6H-Brazil_strategy_cost$Baseline)
Brazil_c_baseline_vs_minimal_overall <- data.frame(Brazil_strategy_cost$Minimal.regimen-Brazil_strategy_cost$Baseline)
Brazil_c_baseline_vs_optimal_overall <- data.frame(Brazil_strategy_cost$Optimal.regimen-Brazil_strategy_cost$Baseline)
Brazil_c_6h_vs_minimal_overall <- data.frame(Brazil_strategy_cost$Minimal.regimen-Brazil_strategy_cost$Expanded.6H)
Brazil_c_6h_vs_optimal_overall <- data.frame(Brazil_strategy_cost$Optimal.regimen-Brazil_strategy_cost$Expanded.6H)
Brazil_c_minimal_vs_optimal_overall <- data.frame(Brazil_strategy_cost$Optimal.regimen-Brazil_strategy_cost$Minimal.regimen)
#####################################################################################################################################

#Incremental cost - Overall (PT)
Brazil_cPT_baseline_vs_6h_overall <- data.frame(Brazil_scaleup$C_PT_all - Brazil_baseline$C_PT_all)
Brazil_cPT_baseline_vs_minimal_overall <- data.frame(Brazil_minimal$C_PT_all - Brazil_baseline$C_PT_all)
Brazil_cPT_baseline_vs_optimal_overall <- data.frame(Brazil_optimal$C_PT_all - Brazil_baseline$C_PT_all)
Brazil_cPT_6h_vs_minimal_overall <- data.frame(Brazil_minimal$C_PT_all - Brazil_scaleup$C_PT_all)
Brazil_cPT_6h_vs_optimal_overall <- data.frame(Brazil_optimal$C_PT_all - Brazil_scaleup$C_PT_all)
Brazil_cPT_minimal_vs_optimal_overall <- data.frame(Brazil_optimal$C_PT_all - Brazil_minimal$C_PT_all)

#####################################################################################################################################
#Incremental cost - Overall (TB)
Brazil_cTB_baseline_vs_6h_overall <- data.frame(Brazil_scaleup$C_TB_all_DOT_SAT - Brazil_baseline$C_TB_all_DOT_SAT)
Brazil_cTB_baseline_vs_minimal_overall <- data.frame(Brazil_minimal$C_TB_all_DOT_SAT - Brazil_baseline$C_TB_all_DOT_SAT)
Brazil_cTB_baseline_vs_optimal_overall <- data.frame(Brazil_optimal$C_TB_all_DOT_SAT - Brazil_baseline$C_TB_all_DOT_SAT)
Brazil_cTB_6h_vs_minimal_overall <- data.frame(Brazil_minimal$C_TB_all_DOT_SAT - Brazil_scaleup$C_TB_all_DOT_SAT)
Brazil_cTB_6h_vs_optimal_overall <- data.frame(Brazil_optimal$C_TB_all_DOT_SAT - Brazil_scaleup$C_TB_all_DOT_SAT)
Brazil_cTB_minimal_vs_optimal_overall <- data.frame(Brazil_optimal$C_TB_all_DOT_SAT - Brazil_minimal$C_TB_all_DOT_SAT)
#####################################################################################################################################


#Incremental cost - HIV negative (PT + TB)
Brazil_c_baseline_vs_6h_HIV_neg <- data.frame(Brazil_strategy_cost$Expanded.6H_neg-Brazil_strategy_cost$Baseline_HIV_neg)
Brazil_c_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_strategy_cost$Minimal.regimen_HIV_neg-Brazil_strategy_cost$Baseline_HIV_neg)
Brazil_c_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_strategy_cost$Optimal.regimen_HIV_neg-Brazil_strategy_cost$Baseline_HIV_neg)
Brazil_c_6h_vs_minimal_HIV_neg <- data.frame(Brazil_strategy_cost$Minimal.regimen_HIV_neg-Brazil_strategy_cost$Expanded.6H_neg)
Brazil_c_6h_vs_optimal_HIV_neg <- data.frame(Brazil_strategy_cost$Optimal.regimen_HIV_neg-Brazil_strategy_cost$Expanded.6H_neg)
Brazil_c_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_strategy_cost$Optimal.regimen_HIV_neg-Brazil_strategy_cost$Minimal.regimen_HIV_neg)

#####################################################################################################################################

#Incremental cost - HIV negative (PT)
Brazil_cPT_baseline_vs_6h_HIV_neg <- data.frame(Brazil_scaleup$C_PT_hiv_neg - Brazil_baseline$C_PT_hiv_neg)
Brazil_cPT_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_minimal$C_PT_hiv_neg - Brazil_baseline$C_PT_hiv_neg)
Brazil_cPT_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_optimal$C_PT_hiv_neg - Brazil_baseline$C_PT_hiv_neg)
Brazil_cPT_6h_vs_minimal_HIV_neg <- data.frame(Brazil_minimal$C_PT_hiv_neg - Brazil_scaleup$C_PT_hiv_neg)
Brazil_cPT_6h_vs_optimal_HIV_neg <- data.frame(Brazil_optimal$C_PT_hiv_neg - Brazil_scaleup$C_PT_hiv_neg)
Brazil_cPT_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_optimal$C_PT_hiv_neg - Brazil_minimal$C_PT_hiv_neg)

#####################################################################################################################################


#Incremental cost - HIV negative (TB)
Brazil_cTB_baseline_vs_6h_HIV_neg <- data.frame(Brazil_scaleup$C_TB_hiv_neg_DOT_SAT - Brazil_baseline$C_TB_hiv_neg_DOT_SAT)
Brazil_cTB_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_minimal$C_TB_hiv_neg_DOT_SAT - Brazil_baseline$C_TB_hiv_neg_DOT_SAT)
Brazil_cTB_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_optimal$C_TB_hiv_neg_DOT_SAT - Brazil_baseline$C_TB_hiv_neg_DOT_SAT)
Brazil_cTB_6h_vs_minimal_HIV_neg <- data.frame(Brazil_minimal$C_TB_hiv_neg_DOT_SAT - Brazil_scaleup$C_TB_hiv_neg_DOT_SAT)
Brazil_cTB_6h_vs_optimal_HIV_neg <- data.frame(Brazil_optimal$C_TB_hiv_neg_DOT_SAT - Brazil_scaleup$C_TB_hiv_neg_DOT_SAT)
Brazil_cTB_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_optimal$C_TB_hiv_neg_DOT_SAT - Brazil_minimal$C_TB_hiv_neg_DOT_SAT)

#####################################################################################################################################


#Incremental cost - PLHIV (PT+TB)
Brazil_c_baseline_vs_6h_PLHIV <- data.frame(Brazil_strategy_cost$Expanded.6H_PLHIV-Brazil_strategy_cost$Baseline_PLHIV)
Brazil_c_baseline_vs_minimal_PLHIV <- data.frame(Brazil_strategy_cost$Minimal.regimen_PLHIV-Brazil_strategy_cost$Baseline_PLHIV)
Brazil_c_baseline_vs_optimal_PLHIV <- data.frame(Brazil_strategy_cost$Optimal.regimen_PLHIV-Brazil_strategy_cost$Baseline_PLHIV)
Brazil_c_6h_vs_minimal_PLHIV <- data.frame(Brazil_strategy_cost$Minimal.regimen_PLHIV-Brazil_strategy_cost$Expanded.6H_PLHIV)
Brazil_c_6h_vs_optimal_PLHIV <- data.frame(Brazil_strategy_cost$Optimal.regimen_PLHIV-Brazil_strategy_cost$Expanded.6H_PLHIV)
Brazil_c_minimal_vs_optimal_PLHIV <- data.frame(Brazil_strategy_cost$Optimal.regimen_PLHIV-Brazil_strategy_cost$Minimal.regimen_PLHIV)

#####################################################################################################################################


#Incremental cost - PLHIV (PT)
Brazil_cPT_baseline_vs_6h_PLHIV <- data.frame(Brazil_scaleup$C_PT_hiv - Brazil_baseline$C_PT_hiv)
Brazil_cPT_baseline_vs_minimal_PLHIV <- data.frame(Brazil_minimal$C_PT_hiv - Brazil_baseline$C_PT_hiv)
Brazil_cPT_baseline_vs_optimal_PLHIV <- data.frame(Brazil_optimal$C_PT_hiv - Brazil_baseline$C_PT_hiv)
Brazil_cPT_6h_vs_minimal_PLHIV <- data.frame(Brazil_minimal$C_PT_hiv - Brazil_scaleup$C_PT_hiv)
Brazil_cPT_6h_vs_optimal_PLHIV <- data.frame(Brazil_optimal$C_PT_hiv - Brazil_scaleup$C_PT_hiv)
Brazil_cPT_minimal_vs_optimal_PLHIV <- data.frame(Brazil_optimal$C_PT_hiv - Brazil_minimal$C_PT_hiv)

#####################################################################################################################################


#Incremental cost - PLHIV (TB)
Brazil_cTB_baseline_vs_6h_PLHIV <- data.frame(Brazil_scaleup$C_TB_hiv_DOT_SAT - Brazil_baseline$C_TB_hiv_DOT_SAT)
Brazil_cTB_baseline_vs_minimal_PLHIV <- data.frame(Brazil_minimal$C_TB_hiv_DOT_SAT - Brazil_baseline$C_TB_hiv_DOT_SAT)
Brazil_cTB_baseline_vs_optimal_PLHIV <- data.frame(Brazil_optimal$C_TB_hiv_DOT_SAT - Brazil_baseline$C_TB_hiv_DOT_SAT)
Brazil_cTB_6h_vs_minimal_PLHIV <- data.frame(Brazil_minimal$C_TB_hiv_DOT_SAT - Brazil_scaleup$C_TB_hiv_DOT_SAT)
Brazil_cTB_6h_vs_optimal_PLHIV <- data.frame(Brazil_optimal$C_TB_hiv_DOT_SAT - Brazil_scaleup$C_TB_hiv_DOT_SAT)
Brazil_cTB_minimal_vs_optimal_PLHIV <- data.frame(Brazil_optimal$C_TB_hiv_DOT_SAT - Brazil_minimal$C_TB_hiv_DOT_SAT)

#####################################################################################################################################
#####################################################################################################################################

#combine columns
Brazil_incr_cost<- data.frame(Brazil_c_baseline_vs_6h_overall,Brazil_c_baseline_vs_minimal_overall,Brazil_c_baseline_vs_optimal_overall,Brazil_c_6h_vs_minimal_overall,Brazil_c_6h_vs_optimal_overall,Brazil_c_minimal_vs_optimal_overall,  #Incremental cost - Overall (PT + TB)
                              Brazil_cPT_baseline_vs_6h_overall,Brazil_cPT_baseline_vs_minimal_overall,Brazil_cPT_baseline_vs_optimal_overall,Brazil_cPT_6h_vs_minimal_overall,Brazil_cPT_6h_vs_optimal_overall,Brazil_cPT_minimal_vs_optimal_overall,  #Incremental cost - Overall (PT)
                              Brazil_cTB_baseline_vs_6h_overall,Brazil_cTB_baseline_vs_minimal_overall,Brazil_cTB_baseline_vs_optimal_overall,Brazil_cTB_6h_vs_minimal_overall,Brazil_cTB_6h_vs_optimal_overall,Brazil_cTB_minimal_vs_optimal_overall,  #Incremental cost - Overall (TB)
                              Brazil_c_baseline_vs_6h_HIV_neg,Brazil_c_baseline_vs_minimal_HIV_neg,Brazil_c_baseline_vs_optimal_HIV_neg,Brazil_c_6h_vs_minimal_HIV_neg,Brazil_c_6h_vs_optimal_HIV_neg,Brazil_c_minimal_vs_optimal_HIV_neg,  #Incremental cost - HIV negative (PT + TB)
                              Brazil_cPT_baseline_vs_6h_HIV_neg,Brazil_cPT_baseline_vs_minimal_HIV_neg,Brazil_cPT_baseline_vs_optimal_HIV_neg,Brazil_cPT_6h_vs_minimal_HIV_neg,Brazil_cPT_6h_vs_optimal_HIV_neg,Brazil_cPT_minimal_vs_optimal_HIV_neg, #Incremental cost - HIV negative (PT)
                              Brazil_cTB_baseline_vs_6h_HIV_neg,Brazil_cTB_baseline_vs_minimal_HIV_neg,Brazil_cTB_baseline_vs_optimal_HIV_neg,Brazil_cTB_6h_vs_minimal_HIV_neg,Brazil_cTB_6h_vs_optimal_HIV_neg,Brazil_cTB_minimal_vs_optimal_HIV_neg,  #Incremental cost - HIV negative (TB)
                              Brazil_c_baseline_vs_6h_PLHIV,Brazil_c_baseline_vs_minimal_PLHIV,Brazil_c_baseline_vs_optimal_PLHIV,Brazil_c_6h_vs_minimal_PLHIV,Brazil_c_6h_vs_optimal_PLHIV,Brazil_c_minimal_vs_optimal_PLHIV,  #Incremental cost - PLHIV (PT+TB)
                              Brazil_cPT_baseline_vs_6h_PLHIV,Brazil_cPT_baseline_vs_minimal_PLHIV,Brazil_cPT_baseline_vs_optimal_PLHIV,Brazil_cPT_6h_vs_minimal_PLHIV,Brazil_cPT_6h_vs_optimal_PLHIV,Brazil_cPT_minimal_vs_optimal_PLHIV,  #Incremental cost - PLHIV (PT)
                              Brazil_cTB_baseline_vs_6h_PLHIV,Brazil_cTB_baseline_vs_minimal_PLHIV,Brazil_cTB_baseline_vs_optimal_PLHIV,Brazil_cTB_6h_vs_minimal_PLHIV,Brazil_cTB_6h_vs_optimal_PLHIV,Brazil_cTB_minimal_vs_optimal_PLHIV   #Incremental cost - PLHIV (TB)
)
colnames(Brazil_incr_cost) <- c("Runs","Brazil_c_baseline_vs_6h_overall","Brazil_c_baseline_vs_minimal_overall","Brazil_c_baseline_vs_optimal_overall","Brazil_c_6h_vs_minimal_overall","Brazil_c_6h_vs_optimal_overall","Brazil_c_minimal_vs_optimal_overall",  #Incremental cost - Overall (PT + TB)
                                "Brazil_cPT_baseline_vs_6h_overall","Brazil_cPT_baseline_vs_minimal_overall","Brazil_cPT_baseline_vs_optimal_overall","Brazil_cPT_6h_vs_minimal_overall","Brazil_cPT_6h_vs_optimal_overall","Brazil_cPT_minimal_vs_optimal_overall",  #Incremental cost - Overall (PT)
                                "Brazil_cTB_baseline_vs_6h_overall","Brazil_cTB_baseline_vs_minimal_overall","Brazil_cTB_baseline_vs_optimal_overall","Brazil_cTB_6h_vs_minimal_overall","Brazil_cTB_6h_vs_optimal_overall","Brazil_cTB_minimal_vs_optimal_overall",  #Incremental cost - Overall (TB)
                                "Brazil_c_baseline_vs_6h_HIV_neg","Brazil_c_baseline_vs_minimal_HIV_neg","Brazil_c_baseline_vs_optimal_HIV_neg","Brazil_c_6h_vs_minimal_HIV_neg","Brazil_c_6h_vs_optimal_HIV_neg","Brazil_c_minimal_vs_optimal_HIV_neg",  #Incremental cost - HIV negative (PT + TB)
                                "Brazil_cPT_baseline_vs_6h_HIV_neg","Brazil_cPT_baseline_vs_minimal_HIV_neg","Brazil_cPT_baseline_vs_optimal_HIV_neg","Brazil_cPT_6h_vs_minimal_HIV_neg","Brazil_cPT_6h_vs_optimal_HIV_neg","Brazil_cPT_minimal_vs_optimal_HIV_neg", #Incremental cost - HIV negative (PT)
                                "Brazil_cTB_baseline_vs_6h_HIV_neg","Brazil_cTB_baseline_vs_minimal_HIV_neg","Brazil_cTB_baseline_vs_optimal_HIV_neg","Brazil_cTB_6h_vs_minimal_HIV_neg","Brazil_cTB_6h_vs_optimal_HIV_neg","Brazil_cTB_minimal_vs_optimal_HIV_neg",  #Incremental cost - HIV negative (TB)
                                "Brazil_c_baseline_vs_6h_PLHIV","Brazil_c_baseline_vs_minimal_PLHIV","Brazil_c_baseline_vs_optimal_PLHIV","Brazil_c_6h_vs_minimal_PLHIV","Brazil_c_6h_vs_optimal_PLHIV","Brazil_c_minimal_vs_optimal_PLHIV",  #Incremental cost - PLHIV (PT+TB)
                                "Brazil_cPT_baseline_vs_6h_PLHIV","Brazil_cPT_baseline_vs_minimal_PLHIV","Brazil_cPT_baseline_vs_optimal_PLHIV","Brazil_cPT_6h_vs_minimal_PLHIV","Brazil_cPT_6h_vs_optimal_PLHIV","Brazil_cPT_minimal_vs_optimal_PLHIV",  #Incremental cost - PLHIV (PT)
                                "Brazil_cTB_baseline_vs_6h_PLHIV","Brazil_cTB_baseline_vs_minimal_PLHIV","Brazil_cTB_baseline_vs_optimal_PLHIV","Brazil_cTB_6h_vs_minimal_PLHIV","Brazil_cTB_6h_vs_optimal_PLHIV","Brazil_cTB_minimal_vs_optimal_PLHIV"   #Incremental cost - PLHIV (TB)
)

#####################################################################################################################################
#####################################################################################################################################
#colnames(Brazil_incr_cost) <- c("Runs","All_TBPT_baseline vs 6H","All_TBPT_baseline vs minimal","All_TBPT_baseline vs optimal","All_TBPT_6H vs minimal", "All_TBPT_6H vs optimal", "All_TBPT_minimal vs optimal",
#                                    "All_PT_baseline vs 6H","All_PT_baseline vs minimal","All_PT_baseline vs optimal","All_PT_6H vs minimal", "All_PT_6H vs optimal", "All_PT_minimal vs optimal",
#                                   "All_TB_baseline vs 6H","All_TB_baseline vs minimal","All_TB_baseline vs optimal","All_TB_6H vs minimal", "All_TB_6H vs optimal", "All_TB_minimal vs optimal",
#                                    "HHC_TBPT_baseline vs 6H","HHC_TBPT_baseline vs minimal","HHC_TBPT_baseline vs optimal","HHC_TBPT_6H vs minimal", "HHC_TBPT_6H vs optimal", "HHC_TBPT_minimal vs optimal",
#                                   "HHC_PT_baseline vs 6H","HHC_PT_baseline vs minimal","HHC_PT_baseline vs optimal","HHC_PT_6H vs minimal", "HHC_PT_6H vs optimal", "HHC_PT_minimal vs optimal",
#                                   "HHC_TB_baseline vs 6H","HHC_TB_baseline vs minimal","HHC_TB_baseline vs optimal","HHC_TB_6H vs minimal", "HHC_TB_6H vs optimal", "HHC_TB_minimal vs optimal",
#                                   "HIV_TBPT_baseline vs 6H","HIV_TBPT_baseline vs minimal","HIV_TBPT_baseline vs optimal","HIV_TBPT_6H vs minimal", "HIV_TBPT_6H vs optimal", "HIV_TBPT_minimal vs optimal",
#                                   "HIV_PT_baseline vs 6H","HIV_PT_baseline vs minimal","HIV_PT_baseline vs optimal","HIV_PT_6H vs minimal", "HIV_PT_6H vs optimal", "HIV_PT_minimal vs optimal",
#                                   "HIV_TB_baseline vs 6H","HIV_TB_baseline vs minimal","HIV_TB_baseline vs optimal","HIV_TB_6H vs minimal", "HIV_TB_6H vs optimal", "HIV_TB_minimal vs optimal")


Brazil_incr_cost_graph<-melt(Brazil_incr_cost, id.vars =c("Runs"), variable.name = "cost_pop")
Brazil_incr_cost_graph$cum_cost<-ave(Brazil_incr_cost_graph$value, Brazil_incr_cost_graph$cost_pop, FUN=cumsum)
Brazil_incr_cost_graph$comparison = ifelse(grepl("baseline_vs_6h", Brazil_incr_cost_graph$cost_pop), "1a. 6H vs baseline",
                                           ifelse(grepl("baseline_vs_minimal", Brazil_incr_cost_graph$cost_pop),"1b. Minimal vs baseline",
                                                  ifelse(grepl("baseline_vs_optimal", Brazil_incr_cost_graph$cost_pop),"1c. Optimal vs baseline",
                                                         ifelse(grepl("6h_vs_minimal", Brazil_incr_cost_graph$cost_pop), "2a. Minimal vs 6H",
                                                                ifelse(grepl("6h_vs_optimal", Brazil_incr_cost_graph$cost_pop),"2b. Optimal vs 6H","3. Optimal vs minimal")))))
Brazil_incr_cost_graph$outcome = ifelse(grepl("cPT", Brazil_incr_cost_graph$cost_pop), "1.PT",
                                        ifelse(grepl("cTB", Brazil_incr_cost_graph$cost_pop),"2.TB","3.PT & TB"))
Brazil_incr_cost_graph$population = ifelse(grepl("overall", Brazil_incr_cost_graph$cost_pop), "All treatment candidates",
                                           ifelse(grepl("PLHIV", Brazil_incr_cost_graph$cost_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_incr_cost_graph$cost_pop = chartr("."," ",Brazil_incr_cost_graph$cost_pop)

#####################################################################################################################

Brazil_incr_cost<-aggregate(x=Brazil_incr_cost_graph$value, by=list(Runs=Brazil_incr_cost_graph$Runs, comparison=Brazil_incr_cost_graph$comparison,outcome=Brazil_incr_cost_graph$outcome,population=Brazil_incr_cost_graph$population), FUN=sum)
#Incremental cost graph
#Incremental cost graph
Brazil_incr_cost_limited<-Brazil_incr_cost %>%
  filter(comparison !="1c. Optimal vs baseline", comparison!="2b. Optimal vs 6H")


Brazil_incr_cost_graph <- ggplot(data=Brazil_incr_cost_limited, aes(comparison,x,color=comparison)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "B", scale = 1e-9))+
  facet_wrap(~population + outcome, dir = "h", scales = "free") +
  labs(x = "population group", y = expression(paste("Incremental cost (US$ Billions)")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("5. Brazil_incr_cost_graph.jpeg", 
       width = 25, height = 16, units = "cm")


#Incremental cost table with ranges  
Brazil_incr_cost_table<-Brazil_incr_cost %>% 
  group_by(population,outcome, comparison) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_incr_cost_table, file = "5. Brazil incremental cost output tables.csv")




#incremental graph option 2
Brazil_incr_cost_table$fill <- ifelse(Brazil_incr_cost_table$median > 0, "#0408e0", "#c43b00")

Brazil_incremental_cost_plot2 <- ggplot(Brazil_incr_cost_table, aes(x = comparison, y = median, fill = fill)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=paste("$",round(median/1000000,0),"M")), position=position_dodge(width=0.9), vjust=-0.25, size=3, fontface="bold")+
  facet_wrap(~population + outcome, scales="fixed") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_fill_discrete(name="Incremental costs",
                      breaks=c("#0408e0", "#c43b00"),
                      labels=c("Additional cost","Savings"))+
  #scale_color_hue(labels = c("Savings","Additional cost"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none")+
  xlab("Comparison") +
  ylab("Incremental cost (in USD)") +
  labs(title="",
       subtitle="",
       caption="")+
  
  
  ggtitle("Incremental cost of TPT strategies for Brazil (2020-2035)")
ggsave("5. Brazil_incremental cost2.png", 
       width = 30, height = 20, units = "cm")


Brazil_incremental_cost_plot2

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################





#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#                                     Incremental effectiveness                                                     #
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################


#####################################################################################################################
#incremental effectiveness - all treatment candidates
#####################################################################################################################

#Incremental effectiveness - Overall (PT)
Brazil_PT_baseline_vs_6h_overall <- data.frame(Runs=Brazil_baseline$Run,Brazil_epi_cumulative_PT$Expanded.6H-Brazil_epi_cumulative_PT$Baseline)
Brazil_PT_baseline_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_PT$Minimal.regimen-Brazil_epi_cumulative_PT$Baseline)
Brazil_PT_baseline_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen-Brazil_epi_cumulative_PT$Baseline)
Brazil_PT_6h_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_PT$Minimal.regimen-Brazil_epi_cumulative_PT$Expanded.6H)
Brazil_PT_6h_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen-Brazil_epi_cumulative_PT$Expanded.6H)
Brazil_PT_minimal_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen-Brazil_epi_cumulative_PT$Minimal.regimen)
#####################################################################################################################################

#Incremental effectiveness - Overall (DS-TB)
Brazil_DSTB_baseline_vs_6h_overall <- data.frame(Brazil_epi_cumulative_DSTB$Expanded.6H-Brazil_epi_cumulative_DSTB$Baseline)
Brazil_DSTB_baseline_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_DSTB$Minimal.regimen-Brazil_epi_cumulative_DSTB$Baseline)
Brazil_DSTB_baseline_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen-Brazil_epi_cumulative_DSTB$Baseline)
Brazil_DSTB_6h_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_DSTB$Minimal.regimen-Brazil_epi_cumulative_DSTB$Expanded.6H)
Brazil_DSTB_6h_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen-Brazil_epi_cumulative_DSTB$Expanded.6H)
Brazil_DSTB_minimal_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen-Brazil_epi_cumulative_DSTB$Minimal.regimen)

#####################################################################################################################################
#Incremental effectiveness - Overall (RR-TB)
Brazil_DRTB_baseline_vs_6h_overall <- data.frame(Brazil_epi_cumulative_DRTB$Expanded.6H-Brazil_epi_cumulative_DRTB$Baseline)
Brazil_DRTB_baseline_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_DRTB$Minimal.regimen-Brazil_epi_cumulative_DRTB$Baseline)
Brazil_DRTB_baseline_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen-Brazil_epi_cumulative_DRTB$Baseline)
Brazil_DRTB_6h_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_DRTB$Minimal.regimen-Brazil_epi_cumulative_DRTB$Expanded.6H)
Brazil_DRTB_6h_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen-Brazil_epi_cumulative_DRTB$Expanded.6H)
Brazil_DRTB_minimal_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen-Brazil_epi_cumulative_DRTB$Minimal.regimen)

#####################################################################################################################################
#Incremental effectiveness - Overall (TB cases (DS-TB & RR-TB))
Brazil_TBall_baseline_vs_6h_overall <- data.frame(Brazil_epi_cumulative_TBall$Expanded.6H-Brazil_epi_cumulative_TBall$Baseline)
Brazil_TBall_baseline_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_TBall$Minimal.regimen-Brazil_epi_cumulative_TBall$Baseline)
Brazil_TBall_baseline_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen-Brazil_epi_cumulative_TBall$Baseline)
Brazil_TBall_6h_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_TBall$Minimal.regimen-Brazil_epi_cumulative_TBall$Expanded.6H)
Brazil_TBall_6h_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen-Brazil_epi_cumulative_TBall$Expanded.6H)
Brazil_TBall_minimal_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen-Brazil_epi_cumulative_TBall$Minimal.regimen)

#####################################################################################################################################
#Incremental effectiveness - Overall (TB deaths)
Brazil_TBdeaths_baseline_vs_6h_overall <- data.frame(Brazil_epi_cumulative_TBdeaths$Expanded.6H-Brazil_epi_cumulative_TBdeaths$Baseline)
Brazil_TBdeaths_baseline_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_TBdeaths$Minimal.regimen-Brazil_epi_cumulative_TBdeaths$Baseline)
Brazil_TBdeaths_baseline_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen-Brazil_epi_cumulative_TBdeaths$Baseline)
Brazil_TBdeaths_6h_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_TBdeaths$Minimal.regimen-Brazil_epi_cumulative_TBdeaths$Expanded.6H)
Brazil_TBdeaths_6h_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen-Brazil_epi_cumulative_TBdeaths$Expanded.6H)
Brazil_TBdeaths_minimal_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen-Brazil_epi_cumulative_TBdeaths$Minimal.regimen)

#####################################################################################################################################
#Incremental effectiveness - Overall (DALYs)
Brazil_DALYs_baseline_vs_6h_overall <- data.frame(Brazil_epi_cumulative_DALYs$Expanded.6H-Brazil_epi_cumulative_DALYs$Baseline)
Brazil_DALYs_baseline_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_DALYs$Minimal.regimen-Brazil_epi_cumulative_DALYs$Baseline)
Brazil_DALYs_baseline_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen-Brazil_epi_cumulative_DALYs$Baseline)
Brazil_DALYs_6h_vs_minimal_overall <- data.frame(Brazil_epi_cumulative_DALYs$Minimal.regimen-Brazil_epi_cumulative_DALYs$Expanded.6H)
Brazil_DALYs_6h_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen-Brazil_epi_cumulative_DALYs$Expanded.6H)
Brazil_DALYs_minimal_vs_optimal_overall <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen-Brazil_epi_cumulative_DALYs$Minimal.regimen)


#####################################################################################################################
#incremental effectiveness - Household contacts (HIV-negative)
#####################################################################################################################

#Incremental effectiveness - HIV negative (PT)
Brazil_PT_baseline_vs_6h_HIV_neg <- data.frame(Brazil_epi_cumulative_PT$Expanded.6H_neg-Brazil_epi_cumulative_PT$Baseline_HIV_neg)
Brazil_PT_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_PT$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_PT$Baseline_HIV_neg)
Brazil_PT_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_PT$Baseline_HIV_neg)
Brazil_PT_6h_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_PT$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_PT$Expanded.6H_neg)
Brazil_PT_6h_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_PT$Expanded.6H_neg)
Brazil_PT_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_PT$Minimal.regimen_HIV_neg)
#####################################################################################################################################

#Incremental effectiveness - HIV negative (DS-TB)
Brazil_DSTB_baseline_vs_6h_HIV_neg <- data.frame(Brazil_epi_cumulative_DSTB$Expanded.6H_neg-Brazil_epi_cumulative_DSTB$Baseline_HIV_neg)
Brazil_DSTB_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DSTB$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_DSTB$Baseline_HIV_neg)
Brazil_DSTB_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DSTB$Baseline_HIV_neg)
Brazil_DSTB_6h_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DSTB$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_DSTB$Expanded.6H_neg)
Brazil_DSTB_6h_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DSTB$Expanded.6H_neg)
Brazil_DSTB_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DSTB$Minimal.regimen_HIV_neg)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (RR-TB)
Brazil_DRTB_baseline_vs_6h_HIV_neg <- data.frame(Brazil_epi_cumulative_DRTB$Expanded.6H_neg-Brazil_epi_cumulative_DRTB$Baseline_HIV_neg)
Brazil_DRTB_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DRTB$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_DRTB$Baseline_HIV_neg)
Brazil_DRTB_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DRTB$Baseline_HIV_neg)
Brazil_DRTB_6h_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DRTB$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_DRTB$Expanded.6H_neg)
Brazil_DRTB_6h_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DRTB$Expanded.6H_neg)
Brazil_DRTB_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DRTB$Minimal.regimen_HIV_neg)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (TB cases (DS-TB & RR-TB))
Brazil_TBall_baseline_vs_6h_HIV_neg <- data.frame(Brazil_epi_cumulative_TBall$Expanded.6H_neg-Brazil_epi_cumulative_TBall$Baseline_HIV_neg)
Brazil_TBall_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBall$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_TBall$Baseline_HIV_neg)
Brazil_TBall_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_TBall$Baseline_HIV_neg)
Brazil_TBall_6h_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBall$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_TBall$Expanded.6H_neg)
Brazil_TBall_6h_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_TBall$Expanded.6H_neg)
Brazil_TBall_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_TBall$Minimal.regimen_HIV_neg)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (TB deaths)
Brazil_TBdeaths_baseline_vs_6h_HIV_neg <- data.frame(Brazil_epi_cumulative_TBdeaths$Expanded.6H_neg-Brazil_epi_cumulative_TBdeaths$Baseline_HIV_neg)
Brazil_TBdeaths_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBdeaths$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_TBdeaths$Baseline_HIV_neg)
Brazil_TBdeaths_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_TBdeaths$Baseline_HIV_neg)
Brazil_TBdeaths_6h_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBdeaths$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_TBdeaths$Expanded.6H_neg)
Brazil_TBdeaths_6h_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_TBdeaths$Expanded.6H_neg)
Brazil_TBdeaths_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_TBdeaths$Minimal.regimen_HIV_neg)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (DALYs)
Brazil_DALYs_baseline_vs_6h_HIV_neg <- data.frame(Brazil_epi_cumulative_DALYs$Expanded.6H_neg-Brazil_epi_cumulative_DALYs$Baseline_HIV_neg)
Brazil_DALYs_baseline_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DALYs$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_DALYs$Baseline_HIV_neg)
Brazil_DALYs_baseline_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DALYs$Baseline_HIV_neg)
Brazil_DALYs_6h_vs_minimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DALYs$Minimal.regimen_HIV_neg-Brazil_epi_cumulative_DALYs$Expanded.6H_neg)
Brazil_DALYs_6h_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DALYs$Expanded.6H_neg)
Brazil_DALYs_minimal_vs_optimal_HIV_neg <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen_HIV_neg-Brazil_epi_cumulative_DALYs$Minimal.regimen_HIV_neg)



#####################################################################################################################
#incremental effectiveness - PLHIV
#####################################################################################################################

#Incremental effectiveness - HIV negative (PT)
Brazil_PT_baseline_vs_6h_PLHIV <- data.frame(Brazil_epi_cumulative_PT$Expanded.6H_PLHIV-Brazil_epi_cumulative_PT$Baseline_PLHIV)
Brazil_PT_baseline_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_PT$Minimal.regimen_PLHIV-Brazil_epi_cumulative_PT$Baseline_PLHIV)
Brazil_PT_baseline_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen_PLHIV-Brazil_epi_cumulative_PT$Baseline_PLHIV)
Brazil_PT_6h_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_PT$Minimal.regimen_PLHIV-Brazil_epi_cumulative_PT$Expanded.6H_PLHIV)
Brazil_PT_6h_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen_PLHIV-Brazil_epi_cumulative_PT$Expanded.6H_PLHIV)
Brazil_PT_minimal_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_PT$Optimal.regimen_PLHIV-Brazil_epi_cumulative_PT$Minimal.regimen_PLHIV)
#####################################################################################################################################

#Incremental effectiveness - HIV negative (DS-TB)
Brazil_DSTB_baseline_vs_6h_PLHIV <- data.frame(Brazil_epi_cumulative_DSTB$Expanded.6H_PLHIV-Brazil_epi_cumulative_DSTB$Baseline_PLHIV)
Brazil_DSTB_baseline_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_DSTB$Minimal.regimen_PLHIV-Brazil_epi_cumulative_DSTB$Baseline_PLHIV)
Brazil_DSTB_baseline_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DSTB$Baseline_PLHIV)
Brazil_DSTB_6h_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_DSTB$Minimal.regimen_PLHIV-Brazil_epi_cumulative_DSTB$Expanded.6H_PLHIV)
Brazil_DSTB_6h_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DSTB$Expanded.6H_PLHIV)
Brazil_DSTB_minimal_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DSTB$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DSTB$Minimal.regimen_PLHIV)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (RR-TB)
Brazil_DRTB_baseline_vs_6h_PLHIV <- data.frame(Brazil_epi_cumulative_DRTB$Expanded.6H_PLHIV-Brazil_epi_cumulative_DRTB$Baseline_PLHIV)
Brazil_DRTB_baseline_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_DRTB$Minimal.regimen_PLHIV-Brazil_epi_cumulative_DRTB$Baseline_PLHIV)
Brazil_DRTB_baseline_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DRTB$Baseline_PLHIV)
Brazil_DRTB_6h_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_DRTB$Minimal.regimen_PLHIV-Brazil_epi_cumulative_DRTB$Expanded.6H_PLHIV)
Brazil_DRTB_6h_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DRTB$Expanded.6H_PLHIV)
Brazil_DRTB_minimal_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DRTB$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DRTB$Minimal.regimen_PLHIV)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (TB cases (DS-TB & RR-TB))
Brazil_TBall_baseline_vs_6h_PLHIV <- data.frame(Brazil_epi_cumulative_TBall$Expanded.6H_PLHIV-Brazil_epi_cumulative_TBall$Baseline_PLHIV)
Brazil_TBall_baseline_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBall$Minimal.regimen_PLHIV-Brazil_epi_cumulative_TBall$Baseline_PLHIV)
Brazil_TBall_baseline_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen_PLHIV-Brazil_epi_cumulative_TBall$Baseline_PLHIV)
Brazil_TBall_6h_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBall$Minimal.regimen_PLHIV-Brazil_epi_cumulative_TBall$Expanded.6H_PLHIV)
Brazil_TBall_6h_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen_PLHIV-Brazil_epi_cumulative_TBall$Expanded.6H_PLHIV)
Brazil_TBall_minimal_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBall$Optimal.regimen_PLHIV-Brazil_epi_cumulative_TBall$Minimal.regimen_PLHIV)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (TB deaths)
Brazil_TBdeaths_baseline_vs_6h_PLHIV <- data.frame(Brazil_epi_cumulative_TBdeaths$Expanded.6H_PLHIV-Brazil_epi_cumulative_TBdeaths$Baseline_PLHIV)
Brazil_TBdeaths_baseline_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBdeaths$Minimal.regimen_PLHIV-Brazil_epi_cumulative_TBdeaths$Baseline_PLHIV)
Brazil_TBdeaths_baseline_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen_PLHIV-Brazil_epi_cumulative_TBdeaths$Baseline_PLHIV)
Brazil_TBdeaths_6h_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBdeaths$Minimal.regimen_PLHIV-Brazil_epi_cumulative_TBdeaths$Expanded.6H_PLHIV)
Brazil_TBdeaths_6h_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen_PLHIV-Brazil_epi_cumulative_TBdeaths$Expanded.6H_PLHIV)
Brazil_TBdeaths_minimal_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_TBdeaths$Optimal.regimen_PLHIV-Brazil_epi_cumulative_TBdeaths$Minimal.regimen_PLHIV)

#####################################################################################################################################
#Incremental effectiveness - HIV negative (DALYs)
Brazil_DALYs_baseline_vs_6h_PLHIV <- data.frame(Brazil_epi_cumulative_DALYs$Expanded.6H_PLHIV-Brazil_epi_cumulative_DALYs$Baseline_PLHIV)
Brazil_DALYs_baseline_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_DALYs$Minimal.regimen_PLHIV-Brazil_epi_cumulative_DALYs$Baseline_PLHIV)
Brazil_DALYs_baseline_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DALYs$Baseline_PLHIV)
Brazil_DALYs_6h_vs_minimal_PLHIV <- data.frame(Brazil_epi_cumulative_DALYs$Minimal.regimen_PLHIV-Brazil_epi_cumulative_DALYs$Expanded.6H_PLHIV)
Brazil_DALYs_6h_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DALYs$Expanded.6H_PLHIV)
Brazil_DALYs_minimal_vs_optimal_PLHIV <- data.frame(Brazil_epi_cumulative_DALYs$Optimal.regimen_PLHIV-Brazil_epi_cumulative_DALYs$Minimal.regimen_PLHIV)



#####################################################################################################################################

#####################################################################################################################################
#####################################################################################################################################

#combine columns
Brazil_incr_effectiveness<-data.frame(Brazil_PT_baseline_vs_6h_overall,Brazil_PT_baseline_vs_minimal_overall,Brazil_PT_baseline_vs_optimal_overall,Brazil_PT_6h_vs_minimal_overall,Brazil_PT_6h_vs_optimal_overall,Brazil_PT_minimal_vs_optimal_overall,
                                      Brazil_PT_baseline_vs_6h_HIV_neg,Brazil_PT_baseline_vs_minimal_HIV_neg,Brazil_PT_baseline_vs_optimal_HIV_neg,Brazil_PT_6h_vs_minimal_HIV_neg,Brazil_PT_6h_vs_optimal_HIV_neg,Brazil_PT_minimal_vs_optimal_HIV_neg,
                                      Brazil_PT_baseline_vs_6h_PLHIV,
                                      Brazil_PT_baseline_vs_minimal_PLHIV,
                                      Brazil_PT_baseline_vs_optimal_PLHIV,
                                      Brazil_PT_6h_vs_minimal_PLHIV,
                                      Brazil_PT_6h_vs_optimal_PLHIV,
                                      Brazil_PT_minimal_vs_optimal_PLHIV,
                                      Brazil_DSTB_baseline_vs_6h_overall,
                                      Brazil_DSTB_baseline_vs_minimal_overall,
                                      Brazil_DSTB_baseline_vs_optimal_overall,
                                      Brazil_DSTB_6h_vs_minimal_overall,
                                      Brazil_DSTB_6h_vs_optimal_overall,
                                      Brazil_DSTB_minimal_vs_optimal_overall,
                                      Brazil_DSTB_baseline_vs_6h_HIV_neg,
                                      Brazil_DSTB_baseline_vs_minimal_HIV_neg,
                                      Brazil_DSTB_baseline_vs_optimal_HIV_neg,
                                      Brazil_DSTB_6h_vs_minimal_HIV_neg,
                                      Brazil_DSTB_6h_vs_optimal_HIV_neg,
                                      Brazil_DSTB_minimal_vs_optimal_HIV_neg,
                                      Brazil_DSTB_baseline_vs_6h_PLHIV,
                                      Brazil_DSTB_baseline_vs_minimal_PLHIV,
                                      Brazil_DSTB_baseline_vs_optimal_PLHIV,
                                      Brazil_DSTB_6h_vs_minimal_PLHIV,
                                      Brazil_DSTB_6h_vs_optimal_PLHIV,
                                      Brazil_DSTB_minimal_vs_optimal_PLHIV,
                                      Brazil_DRTB_baseline_vs_6h_overall,
                                      Brazil_DRTB_baseline_vs_minimal_overall,
                                      Brazil_DRTB_baseline_vs_optimal_overall,
                                      Brazil_DRTB_6h_vs_minimal_overall,
                                      Brazil_DRTB_6h_vs_optimal_overall,
                                      Brazil_DRTB_minimal_vs_optimal_overall,
                                      Brazil_DRTB_baseline_vs_6h_HIV_neg,
                                      Brazil_DRTB_baseline_vs_minimal_HIV_neg,
                                      Brazil_DRTB_baseline_vs_optimal_HIV_neg,
                                      Brazil_DRTB_6h_vs_minimal_HIV_neg,
                                      Brazil_DRTB_6h_vs_optimal_HIV_neg,
                                      Brazil_DRTB_minimal_vs_optimal_HIV_neg,
                                      Brazil_DRTB_baseline_vs_6h_PLHIV,
                                      Brazil_DRTB_baseline_vs_minimal_PLHIV,
                                      Brazil_DRTB_baseline_vs_optimal_PLHIV,
                                      Brazil_DRTB_6h_vs_minimal_PLHIV,
                                      Brazil_DRTB_6h_vs_optimal_PLHIV,
                                      Brazil_DRTB_minimal_vs_optimal_PLHIV,
                                      Brazil_TBall_baseline_vs_6h_overall,
                                      Brazil_TBall_baseline_vs_minimal_overall,
                                      Brazil_TBall_baseline_vs_optimal_overall,
                                      Brazil_TBall_6h_vs_minimal_overall,
                                      Brazil_TBall_6h_vs_optimal_overall,
                                      Brazil_TBall_minimal_vs_optimal_overall,
                                      Brazil_TBall_baseline_vs_6h_HIV_neg,
                                      Brazil_TBall_baseline_vs_minimal_HIV_neg,
                                      Brazil_TBall_baseline_vs_optimal_HIV_neg,
                                      Brazil_TBall_6h_vs_minimal_HIV_neg,
                                      Brazil_TBall_6h_vs_optimal_HIV_neg,
                                      Brazil_TBall_minimal_vs_optimal_HIV_neg,
                                      Brazil_TBall_baseline_vs_6h_PLHIV,
                                      Brazil_TBall_baseline_vs_minimal_PLHIV,
                                      Brazil_TBall_baseline_vs_optimal_PLHIV,
                                      Brazil_TBall_6h_vs_minimal_PLHIV,
                                      Brazil_TBall_6h_vs_optimal_PLHIV,
                                      Brazil_TBall_minimal_vs_optimal_PLHIV,
                                      Brazil_TBdeaths_baseline_vs_6h_overall,
                                      Brazil_TBdeaths_baseline_vs_minimal_overall,
                                      Brazil_TBdeaths_baseline_vs_optimal_overall,
                                      Brazil_TBdeaths_6h_vs_minimal_overall,
                                      Brazil_TBdeaths_6h_vs_optimal_overall,
                                      Brazil_TBdeaths_minimal_vs_optimal_overall,
                                      Brazil_TBdeaths_baseline_vs_6h_HIV_neg,
                                      Brazil_TBdeaths_baseline_vs_minimal_HIV_neg,
                                      Brazil_TBdeaths_baseline_vs_optimal_HIV_neg,
                                      Brazil_TBdeaths_6h_vs_minimal_HIV_neg,
                                      Brazil_TBdeaths_6h_vs_optimal_HIV_neg,
                                      Brazil_TBdeaths_minimal_vs_optimal_HIV_neg,
                                      Brazil_TBdeaths_baseline_vs_6h_PLHIV,
                                      Brazil_TBdeaths_baseline_vs_minimal_PLHIV,
                                      Brazil_TBdeaths_baseline_vs_optimal_PLHIV,
                                      Brazil_TBdeaths_6h_vs_minimal_PLHIV,
                                      Brazil_TBdeaths_6h_vs_optimal_PLHIV,
                                      Brazil_TBdeaths_minimal_vs_optimal_PLHIV,
                                      Brazil_DALYs_baseline_vs_6h_overall,
                                      Brazil_DALYs_baseline_vs_minimal_overall,
                                      Brazil_DALYs_baseline_vs_optimal_overall,
                                      Brazil_DALYs_6h_vs_minimal_overall,
                                      Brazil_DALYs_6h_vs_optimal_overall,
                                      Brazil_DALYs_minimal_vs_optimal_overall,
                                      Brazil_DALYs_baseline_vs_6h_HIV_neg,
                                      Brazil_DALYs_baseline_vs_minimal_HIV_neg,
                                      Brazil_DALYs_baseline_vs_optimal_HIV_neg,
                                      Brazil_DALYs_6h_vs_minimal_HIV_neg,
                                      Brazil_DALYs_6h_vs_optimal_HIV_neg,
                                      Brazil_DALYs_minimal_vs_optimal_HIV_neg,
                                      Brazil_DALYs_baseline_vs_6h_PLHIV,
                                      Brazil_DALYs_baseline_vs_minimal_PLHIV,
                                      Brazil_DALYs_baseline_vs_optimal_PLHIV,
                                      Brazil_DALYs_6h_vs_minimal_PLHIV,
                                      Brazil_DALYs_6h_vs_optimal_PLHIV,
                                      Brazil_DALYs_minimal_vs_optimal_PLHIV
)
colnames(Brazil_incr_effectiveness) <- c("Runs","Brazil_PT_baseline_vs_6h_overall",
                                         "Brazil_PT_baseline_vs_minimal_overall",
                                         "Brazil_PT_baseline_vs_optimal_overall",
                                         "Brazil_PT_6h_vs_minimal_overall",
                                         "Brazil_PT_6h_vs_optimal_overall",
                                         "Brazil_PT_minimal_vs_optimal_overall",
                                         "Brazil_PT_baseline_vs_6h_HIV_neg",
                                         "Brazil_PT_baseline_vs_minimal_HIV_neg",
                                         "Brazil_PT_baseline_vs_optimal_HIV_neg",
                                         "Brazil_PT_6h_vs_minimal_HIV_neg",
                                         "Brazil_PT_6h_vs_optimal_HIV_neg",
                                         "Brazil_PT_minimal_vs_optimal_HIV_neg",
                                         "Brazil_PT_baseline_vs_6h_PLHIV",
                                         "Brazil_PT_baseline_vs_minimal_PLHIV",
                                         "Brazil_PT_baseline_vs_optimal_PLHIV",
                                         "Brazil_PT_6h_vs_minimal_PLHIV",
                                         "Brazil_PT_6h_vs_optimal_PLHIV",
                                         "Brazil_PT_minimal_vs_optimal_PLHIV",
                                         "Brazil_DSTB_baseline_vs_6h_overall",
                                         "Brazil_DSTB_baseline_vs_minimal_overall",
                                         "Brazil_DSTB_baseline_vs_optimal_overall",
                                         "Brazil_DSTB_6h_vs_minimal_overall",
                                         "Brazil_DSTB_6h_vs_optimal_overall",
                                         "Brazil_DSTB_minimal_vs_optimal_overall",
                                         "Brazil_DSTB_baseline_vs_6h_HIV_neg",
                                         "Brazil_DSTB_baseline_vs_minimal_HIV_neg",
                                         "Brazil_DSTB_baseline_vs_optimal_HIV_neg",
                                         "Brazil_DSTB_6h_vs_minimal_HIV_neg",
                                         "Brazil_DSTB_6h_vs_optimal_HIV_neg",
                                         "Brazil_DSTB_minimal_vs_optimal_HIV_neg",
                                         "Brazil_DSTB_baseline_vs_6h_PLHIV",
                                         "Brazil_DSTB_baseline_vs_minimal_PLHIV",
                                         "Brazil_DSTB_baseline_vs_optimal_PLHIV",
                                         "Brazil_DSTB_6h_vs_minimal_PLHIV",
                                         "Brazil_DSTB_6h_vs_optimal_PLHIV",
                                         "Brazil_DSTB_minimal_vs_optimal_PLHIV",
                                         "Brazil_DRTB_baseline_vs_6h_overall",
                                         "Brazil_DRTB_baseline_vs_minimal_overall",
                                         "Brazil_DRTB_baseline_vs_optimal_overall",
                                         "Brazil_DRTB_6h_vs_minimal_overall",
                                         "Brazil_DRTB_6h_vs_optimal_overall",
                                         "Brazil_DRTB_minimal_vs_optimal_overall",
                                         "Brazil_DRTB_baseline_vs_6h_HIV_neg",
                                         "Brazil_DRTB_baseline_vs_minimal_HIV_neg",
                                         "Brazil_DRTB_baseline_vs_optimal_HIV_neg",
                                         "Brazil_DRTB_6h_vs_minimal_HIV_neg",
                                         "Brazil_DRTB_6h_vs_optimal_HIV_neg",
                                         "Brazil_DRTB_minimal_vs_optimal_HIV_neg",
                                         "Brazil_DRTB_baseline_vs_6h_PLHIV",
                                         "Brazil_DRTB_baseline_vs_minimal_PLHIV",
                                         "Brazil_DRTB_baseline_vs_optimal_PLHIV",
                                         "Brazil_DRTB_6h_vs_minimal_PLHIV",
                                         "Brazil_DRTB_6h_vs_optimal_PLHIV",
                                         "Brazil_DRTB_minimal_vs_optimal_PLHIV",
                                         "Brazil_TBall_baseline_vs_6h_overall",
                                         "Brazil_TBall_baseline_vs_minimal_overall",
                                         "Brazil_TBall_baseline_vs_optimal_overall",
                                         "Brazil_TBall_6h_vs_minimal_overall",
                                         "Brazil_TBall_6h_vs_optimal_overall",
                                         "Brazil_TBall_minimal_vs_optimal_overall",
                                         "Brazil_TBall_baseline_vs_6h_HIV_neg",
                                         "Brazil_TBall_baseline_vs_minimal_HIV_neg",
                                         "Brazil_TBall_baseline_vs_optimal_HIV_neg",
                                         "Brazil_TBall_6h_vs_minimal_HIV_neg",
                                         "Brazil_TBall_6h_vs_optimal_HIV_neg",
                                         "Brazil_TBall_minimal_vs_optimal_HIV_neg",
                                         "Brazil_TBall_baseline_vs_6h_PLHIV",
                                         "Brazil_TBall_baseline_vs_minimal_PLHIV",
                                         "Brazil_TBall_baseline_vs_optimal_PLHIV",
                                         "Brazil_TBall_6h_vs_minimal_PLHIV",
                                         "Brazil_TBall_6h_vs_optimal_PLHIV",
                                         "Brazil_TBall_minimal_vs_optimal_PLHIV",
                                         "Brazil_TBdeaths_baseline_vs_6h_overall",
                                         "Brazil_TBdeaths_baseline_vs_minimal_overall",
                                         "Brazil_TBdeaths_baseline_vs_optimal_overall",
                                         "Brazil_TBdeaths_6h_vs_minimal_overall",
                                         "Brazil_TBdeaths_6h_vs_optimal_overall",
                                         "Brazil_TBdeaths_minimal_vs_optimal_overall",
                                         "Brazil_TBdeaths_baseline_vs_6h_HIV_neg",
                                         "Brazil_TBdeaths_baseline_vs_minimal_HIV_neg",
                                         "Brazil_TBdeaths_baseline_vs_optimal_HIV_neg",
                                         "Brazil_TBdeaths_6h_vs_minimal_HIV_neg",
                                         "Brazil_TBdeaths_6h_vs_optimal_HIV_neg",
                                         "Brazil_TBdeaths_minimal_vs_optimal_HIV_neg",
                                         "Brazil_TBdeaths_baseline_vs_6h_PLHIV",
                                         "Brazil_TBdeaths_baseline_vs_minimal_PLHIV",
                                         "Brazil_TBdeaths_baseline_vs_optimal_PLHIV",
                                         "Brazil_TBdeaths_6h_vs_minimal_PLHIV",
                                         "Brazil_TBdeaths_6h_vs_optimal_PLHIV",
                                         "Brazil_TBdeaths_minimal_vs_optimal_PLHIV",
                                         "Brazil_DALYs_baseline_vs_6h_overall",
                                         "Brazil_DALYs_baseline_vs_minimal_overall",
                                         "Brazil_DALYs_baseline_vs_optimal_overall",
                                         "Brazil_DALYs_6h_vs_minimal_overall",
                                         "Brazil_DALYs_6h_vs_optimal_overall",
                                         "Brazil_DALYs_minimal_vs_optimal_overall",
                                         "Brazil_DALYs_baseline_vs_6h_HIV_neg",
                                         "Brazil_DALYs_baseline_vs_minimal_HIV_neg",
                                         "Brazil_DALYs_baseline_vs_optimal_HIV_neg",
                                         "Brazil_DALYs_6h_vs_minimal_HIV_neg",
                                         "Brazil_DALYs_6h_vs_optimal_HIV_neg",
                                         "Brazil_DALYs_minimal_vs_optimal_HIV_neg",
                                         "Brazil_DALYs_baseline_vs_6h_PLHIV",
                                         "Brazil_DALYs_baseline_vs_minimal_PLHIV",
                                         "Brazil_DALYs_baseline_vs_optimal_PLHIV",
                                         "Brazil_DALYs_6h_vs_minimal_PLHIV",
                                         "Brazil_DALYs_6h_vs_optimal_PLHIV",
                                         "Brazil_DALYs_minimal_vs_optimal_PLHIV")

#####################################################################################################################################
#####################################################################################################################################
#colnames(Brazil_incr_cost) <- c("Runs","All_TBPT_baseline vs 6H","All_TBPT_baseline vs minimal","All_TBPT_baseline vs optimal","All_TBPT_6H vs minimal", "All_TBPT_6H vs optimal", "All_TBPT_minimal vs optimal",
#                                    "All_PT_baseline vs 6H","All_PT_baseline vs minimal","All_PT_baseline vs optimal","All_PT_6H vs minimal", "All_PT_6H vs optimal", "All_PT_minimal vs optimal",
#                                   "All_TB_baseline vs 6H","All_TB_baseline vs minimal","All_TB_baseline vs optimal","All_TB_6H vs minimal", "All_TB_6H vs optimal", "All_TB_minimal vs optimal",
#                                    "HHC_TBPT_baseline vs 6H","HHC_TBPT_baseline vs minimal","HHC_TBPT_baseline vs optimal","HHC_TBPT_6H vs minimal", "HHC_TBPT_6H vs optimal", "HHC_TBPT_minimal vs optimal",
#                                   "HHC_PT_baseline vs 6H","HHC_PT_baseline vs minimal","HHC_PT_baseline vs optimal","HHC_PT_6H vs minimal", "HHC_PT_6H vs optimal", "HHC_PT_minimal vs optimal",
#                                   "HHC_TB_baseline vs 6H","HHC_TB_baseline vs minimal","HHC_TB_baseline vs optimal","HHC_TB_6H vs minimal", "HHC_TB_6H vs optimal", "HHC_TB_minimal vs optimal",
#                                   "HIV_TBPT_baseline vs 6H","HIV_TBPT_baseline vs minimal","HIV_TBPT_baseline vs optimal","HIV_TBPT_6H vs minimal", "HIV_TBPT_6H vs optimal", "HIV_TBPT_minimal vs optimal",
#                                   "HIV_PT_baseline vs 6H","HIV_PT_baseline vs minimal","HIV_PT_baseline vs optimal","HIV_PT_6H vs minimal", "HIV_PT_6H vs optimal", "HIV_PT_minimal vs optimal",
#                                   "HIV_TB_baseline vs 6H","HIV_TB_baseline vs minimal","HIV_TB_baseline vs optimal","HIV_TB_6H vs minimal", "HIV_TB_6H vs optimal", "HIV_TB_minimal vs optimal")


Brazil_incr_effectiveness_graph<-melt(Brazil_incr_effectiveness, id.vars =c("Runs"), variable.name = "effectiveness_pop")
Brazil_incr_effectiveness_graph$cum_effectiveness<-ave(Brazil_incr_effectiveness_graph$value, Brazil_incr_effectiveness_graph$effectiveness_pop, FUN=cumsum)
Brazil_incr_effectiveness_graph$comparison = ifelse(grepl("baseline_vs_6h", Brazil_incr_effectiveness_graph$effectiveness_pop), "1a. 6H vs baseline",
                                                    ifelse(grepl("baseline_vs_minimal", Brazil_incr_effectiveness_graph$effectiveness_pop),"1b. Minimal vs baseline",
                                                           ifelse(grepl("baseline_vs_optimal", Brazil_incr_effectiveness_graph$effectiveness_pop),"1c. Optimal vs baseline",
                                                                  ifelse(grepl("6h_vs_minimal", Brazil_incr_effectiveness_graph$effectiveness_pop), "2a. Minimal vs 6H",
                                                                         ifelse(grepl("6h_vs_optimal", Brazil_incr_effectiveness_graph$effectiveness_pop),"2b. Optimal vs 6H","3. Optimal vs minimal")))))
Brazil_incr_effectiveness_graph$outcome = ifelse(grepl("PT", Brazil_incr_effectiveness_graph$effectiveness_pop), "1. PT",
                                                 ifelse(grepl("DSTB", Brazil_incr_effectiveness_graph$effectiveness_pop),"2. DS-TB cases",
                                                        ifelse(grepl("DRTB", Brazil_incr_effectiveness_graph$effectiveness_pop),"3. RR-TB cases",
                                                               ifelse(grepl("TBall", Brazil_incr_effectiveness_graph$effectiveness_pop),"4. All TB cases (DS-TB & RR-TB)",
                                                                      ifelse(grepl("TBdeaths", Brazil_incr_effectiveness_graph$effectiveness_pop),"5. TB deaths",
                                                                             "6. DALYs")))))
Brazil_incr_effectiveness_graph$population = ifelse(grepl("overall", Brazil_incr_effectiveness_graph$effectiveness_pop), "All treatment candidates",
                                                    ifelse(grepl("PLHIV", Brazil_incr_effectiveness_graph$effectiveness_pop),"PLHIV","HHC (HIV negatives)"))
Brazil_incr_effectiveness_graph$effectiveness_pop = chartr("."," ",Brazil_incr_effectiveness_graph$effectiveness_pop)



Brazil_incr_effectiveness<-aggregate(x=Brazil_incr_effectiveness_graph$value, by=list(Runs=Brazil_incr_effectiveness_graph$Runs, comparison=Brazil_incr_effectiveness_graph$comparison,outcome=Brazil_incr_effectiveness_graph$outcome,population=Brazil_incr_effectiveness_graph$population), FUN=sum)

#####################################################################################################################
#Incremental effectiveness table with ranges  
#####################################################################################################################
Brazil_incr_effectiveness_table<-Brazil_incr_effectiveness %>% 
  group_by(population,outcome, comparison) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_incr_effectiveness_table, file = "6. Brazil incremental effectiveness output tables.csv")


#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

Brazil_incr_effectiveness<-aggregate(x=Brazil_incr_effectiveness_graph$value, by=list(Runs=Brazil_incr_effectiveness_graph$Runs, comparison=Brazil_incr_effectiveness_graph$comparison,outcome=Brazil_incr_effectiveness_graph$outcome,population=Brazil_incr_effectiveness_graph$population), FUN=sum)
#Incremental cost graph
Brazil_incr_effectiveness_graph <- ggplot(data=Brazil_incr_effectiveness, aes(comparison,x,color=comparison)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(labels = unit_format( unit = "K", scale = 1e-3))+
  facet_wrap(~population + outcome, dir = "h", scales = "free") +
  labs(x = "population group",y = expression(paste("Incremental effectiveness")),colour = "",subtitle="",caption="")+
  guides(fill = "none")+
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
ggsave("6. Brazil_incr_effectiveness_graph.jpeg", 
       width = 25, height = 16, units = "cm")


#incremental graph option 2 - OVERALL

Brazil_incr_effectiveness_table_overall<-Brazil_incr_effectiveness_table %>%
  filter(population =="All treatment candidates")
#    filter(data_type !="Epi data" | state!="TPT")

Brazil_incr_effectiveness_table_overall$fill <- ifelse(Brazil_incr_effectiveness_table_overall$median > 0, "#0408e0", "#c43b00")

Brazil_incremental_effectiveness_overall_plot <- ggplot(Brazil_incr_effectiveness_table_overall, aes(x = comparison, y = median, fill = fill)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=paste("",round(median/1000,0),"K")), position=position_dodge(width=0.9), vjust=-0.25, size=3, fontface="bold")+
  facet_wrap(~outcome, scales="fixed") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_fill_discrete(name="Incremental effectiveness",
                      breaks=c("#0408e0", "#c43b00"),
                      labels=c("Increases","Averted"))+
  #scale_color_hue(labels = c("Savings","Additional cost"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none",)+
  xlab("Comparison") +
  ylab("Incremental effectiveness") +
  labs(title="",
       subtitle="",
       caption="")+
  
  
  ggtitle("Incremental effectiveness of TPT strategies for Brazil (All treatment candidates 2020-2035)")
ggsave("6a. Brazil_incremental effectiveness (All treatment candidates).png", 
       width = 30, height = 20, units = "cm")


Brazil_incremental_effectiveness_overall_plot



#incremental graph option 2 - HHC (HIV negatives)

Brazil_incr_effectiveness_table_HIVneg<-Brazil_incr_effectiveness_table %>%
  filter(population =="HHC (HIV negatives)")

Brazil_incr_effectiveness_table_HIVneg$fill <- ifelse(Brazil_incr_effectiveness_table_HIVneg$median > 0, "#0408e0", "#c43b00")

Brazil_incremental_effectiveness_HIVneg_plot <- ggplot(Brazil_incr_effectiveness_table_HIVneg, aes(x = comparison, y = median, fill = fill)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=paste("",round(median/1000,0),"K")), position=position_dodge(width=0.9), vjust=-0.25, size=3, fontface="bold")+
  facet_wrap(~outcome, scales="fixed") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_fill_discrete(name="Incremental effectiveness",
                      breaks=c("#0408e0", "#c43b00"),
                      labels=c("Increases","Averted"))+
  #scale_color_hue(labels = c("Savings","Additional cost"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none",)+
  xlab("Comparison") +
  ylab("Incremental effectiveness") +
  labs(title="",
       subtitle="",
       caption="")+
  
  
  ggtitle("Incremental effectiveness of TPT strategies for Brazil (HHC (HIV negatives) 2020-2035)")
ggsave("6b. Brazil_incremental effectiveness (HHC (HIV negatives)).png", 
       width = 30, height = 20, units = "cm")


Brazil_incremental_effectiveness_HIVneg_plot




#incremental graph option 2 - PLHIV

Brazil_incr_effectiveness_table_PLHIV<-Brazil_incr_effectiveness_table %>%
  filter(population =="PLHIV")

Brazil_incr_effectiveness_table_PLHIV$fill <- ifelse(Brazil_incr_effectiveness_table_PLHIV$median > 0, "#0408e0", "#c43b00")

Brazil_incremental_effectiveness_PLHIV_plot <- ggplot(Brazil_incr_effectiveness_table_PLHIV, aes(x = comparison, y = median, fill = fill)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=paste("",round(median/1000,0),"K")), position=position_dodge(width=0.9), vjust=-0.25, size=3, fontface="bold")+
  facet_wrap(~outcome, scales="fixed") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_fill_discrete(name="Incremental effectiveness",
                      breaks=c("#0408e0", "#c43b00"),
                      labels=c("Increases","Averted"))+
  #scale_color_hue(labels = c("Savings","Additional cost"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1),legend.position="none",)+
  xlab("Comparison") +
  ylab("Incremental effectiveness") +
  labs(title="",
       subtitle="",
       caption="")+
  
  
  ggtitle("Incremental effectiveness of TPT strategies for Brazil (PLHIV 2020-2035)")
ggsave("6c. Brazil_incremental effectiveness (PLHIV).png", 
       width = 30, height = 20, units = "cm")


Brazil_incremental_effectiveness_PLHIV_plot







#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### INCREMENT COST EFFECTIVENESS RATIO #######
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

Brazil_ICER_calcul<-data.frame(Runs=Brazil_incr_cost$Runs,comparison=Brazil_incr_effectiveness_graph$comparison,outcome=Brazil_incr_effectiveness_graph$outcome,population=Brazil_incr_effectiveness_graph$population,(Brazil_incr_cost$x/Brazil_incr_effectiveness))

Brazil_ICER_allTB<-Brazil_incr_effectiveness %>%
  filter(outcome =="4. All TB cases (DS-TB & RR-TB)" & population == "All treatment candidates")
Brazil_ICER_allcost<-Brazil_incr_cost %>%
  filter(outcome =="3.PT & TB" & population == "All treatment candidates")
Brazil_ICER_allTBdeaths<-Brazil_incr_effectiveness %>%
  filter(outcome =="5. TB deaths" & population == "All treatment candidates")
Brazil_ICER_allDALYs<-Brazil_incr_effectiveness %>%
  filter(outcome =="6. DALYs" & population == "All treatment candidates")

Brazil_ICER_TB<-data.frame(Runs=Brazil_ICER_allcost$Runs, comparison=Brazil_ICER_allcost$comparison,TB=(Brazil_ICER_allcost$x/-Brazil_ICER_allTB$x))
Brazil_ICER_TBdeaths<-data.frame(TBdeaths=(Brazil_ICER_allcost$x/-Brazil_ICER_allTBdeaths$x))
Brazil_ICER_DALYs<-data.frame(DALYs=(Brazil_ICER_allcost$x/-Brazil_ICER_allDALYs$x))
#combine
Brazil_ICER<-data.frame(Brazil_ICER_TB,
                        Brazil_ICER_TBdeaths,
                        Brazil_ICER_DALYs)

# quadrant plots for iCERs
Brazil_quadrant_TB_cost<-data.frame(Brazil_ICER_allTB,
                                Brazil_ICER_allcost,
                                Brazil_ICER_allTBdeaths,
                                Brazil_ICER_allDALYs)
Brazil_quadrant_TB_cost<-filter(Brazil_quadrant_TB_cost)
                            # !comparison %in% c('1c. Optimal vs baseline','2b. Optimal vs 6H'))


Brazil_quadrant_TB_cost$neg<--1                                                 

Brazil_quadrant_TB_cost$incremental_cost<-Brazil_quadrant_TB_cost$x.1
Brazil_quadrant_TB_cost$TB_case_averted<-Brazil_quadrant_TB_cost$x*Brazil_quadrant_TB_cost$neg
Brazil_quadrant_TB_cost$TB_deaths_averted<-Brazil_quadrant_TB_cost$x.2*Brazil_quadrant_TB_cost$neg
Brazil_quadrant_TB_cost$DALYs_averted<-Brazil_quadrant_TB_cost$x.3*Brazil_quadrant_TB_cost$neg


write.csv(Brazil_quadrant_TB_cost, file = "10. Brazil Incremental cost and effectiveness.csv")

Brazil_quadrant<-Brazil_quadrant_TB_cost 
#%>%
#  filter(comparison=="1a. 6H vs baseline" | comparison=="1b. Minimal vs baseline")

#.quadrants TB-Cost 
Brazil_CE_TB<-Brazil_quadrant %>% 
  mutate(quadrant = case_when(TB_case_averted > 0 & incremental_cost > 0   ~ "More expensive and more effective",
                              TB_case_averted <= 0 & incremental_cost > 0  ~ "More expensive and less effective",
                              TB_case_averted <= 0 & incremental_cost <= 0 ~ "Less costly and less effective",
                              TRUE
                              ~ "Less costly and more effective")) %>% 
  ggplot(aes(x = TB_case_averted,comparison, y = incremental_cost, color = 
               ifelse(quadrant=="More expensive and more effective","deepskyblue",
                      ifelse(quadrant=="More expensive and less effective","orange",
                             ifelse(quadrant=="Less costly and less effective","red","darkgreen"))))) +
  scale_colour_manual(values = c("orange"="orange", "red" = "red","deepskyblue" = "deepskyblue","darkgreen" = "darkgreen"))+
  geom_vline(xintercept = 0, linetype="dotted") + # plot vertical line
  geom_hline(yintercept = 0, linetype="dotted") + # plot horizontal line
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6), limits=c(-200000000,200000000))+
  scale_x_continuous(labels = unit_format( unit = "", scale = 1e-3), limits=c(-20000,200000))+
  facet_wrap(~comparison, dir="h", scales="fixed", ncol = 4) +
  theme_classic()+
  geom_abline(slope=6040, intercept=0, col='red') +
  geom_abline(slope=18120, intercept=0, col='purple') +
  annotate("text", x = 10000, y = 100000000, label = "WTP - GDP", size=2, col="red")+
  annotate("text", x = 10000, y = 200000000, label = "WTP - GDP*3", size=2, col="purple")+
  xlab("TB cases averted (In Thousands)") +
  ylab("") +
  labs(color='')  +
  theme(legend.position="")+
  geom_point()+
  ggsave("10a. Brazil_quadrant_TB_cost.jpeg", 
         width = 25, height = 8, units = "cm")


#Quadrants TB deaths -Cost 
Brazil_CE_TBdeath<-Brazil_quadrant %>% 
  mutate(quadrant = case_when(TB_deaths_averted > 0 & incremental_cost > 0   ~ "More expensive and more effective",
                              TB_deaths_averted <= 0 & incremental_cost > 0  ~ "More expensive and less effective",
                              TB_deaths_averted <= 0 & incremental_cost <= 0 ~ "Less costly and less effective",
                              TRUE
                              ~ "Less costly and more effective")) %>% 
  ggplot(aes(x = TB_deaths_averted,comparison, y = incremental_cost, color = 
               ifelse(quadrant=="More expensive and more effective","deepskyblue",
                      ifelse(quadrant=="More expensive and less effective","orange",
                             ifelse(quadrant=="Less costly and less effective","red","darkgreen"))))) +
  scale_colour_manual(values = c("orange"="orange", "red" = "red","deepskyblue" = "deepskyblue","darkgreen" = "darkgreen"))+
  geom_vline(xintercept = 0, linetype="dotted") + # plot vertical line
  geom_hline(yintercept = 0, linetype="dotted") + # plot horizontal line
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6), limits=c(-200000000,200000000))+
  scale_x_continuous(labels = unit_format( unit = "", scale = 1e-3), limits=c(-2000,25000))+
  facet_wrap(~comparison, dir="h", scales="fixed", ncol = 4) +
  theme_classic()+
  geom_abline(slope=6040, intercept=0, col='red') +
  geom_abline(slope=18120, intercept=0, col='purple') +
  annotate("text", x = 10000, y = 100000000, label = "WTP - GDP", size=2, col="red")+
  annotate("text", x = 20000, y = 200000000, label = "WTP - GDP*3", size=2, col="purple")+
  xlab("TB deaths averted  (In Thousands)") +
  ylab("Incremental cost (In millions US$)") +
  labs(color='')  +
  theme(legend.position="")+
  geom_point()+
  ggsave("10b. Brazil_quadrant_TB_deaths_cost.jpeg", 
         width = 25, height = 8, units = "cm")






#Quadrants DALYs -Cost 
Brazil_CE_DALYs<-Brazil_quadrant %>% 
  mutate(quadrant = case_when(DALYs_averted > 0 & incremental_cost > 0   ~ "More expensive and more effective",
                              DALYs_averted <= 0 & incremental_cost > 0  ~ "QMore expensive and less effective",
                              DALYs_averted <= 0 & incremental_cost <= 0 ~ "Less costly and less effective",
                              TRUE
                              ~ "Less costly and more effective")) %>% 
  ggplot(aes(x = DALYs_averted,comparison, y = incremental_cost,  color = 
               ifelse(quadrant=="More expensive and more effective","deepskyblue",
                      ifelse(quadrant=="More expensive and less effective","orange",
                             ifelse(quadrant=="Less costly and less effective","red","darkgreen"))))) +
  scale_colour_manual(values = c("orange"="orange", "red" = "red","deepskyblue" = "deepskyblue","darkgreen" = "darkgreen"))+
  geom_vline(xintercept = 0, linetype="dotted") + # plot vertical line
  geom_hline(yintercept = 0, linetype="dotted") + # plot horizontal line
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6), limits=c(-200000000,200000000))+
  scale_x_continuous(labels = unit_format( unit = "", scale = 1e-3), limits=c(-60000,600000))+
  facet_wrap(~comparison, dir="h", scales="fixed", ncol = 4) +
  theme_classic()+
  geom_abline(slope=6040, intercept=0, col='red') +
  geom_abline(slope=18120, intercept=0, col='purple') +
  annotate("text", x = 200000, y = 100000000, label = "WTP - GDP", size=2, col="red")+
  annotate("text", x = 200000, y = 200000000, label = "WTP - GDP*3", size=2, col="purple")+
  xlab("DALYs averted (In Thousands)") +
  ylab("") +
  labs(color='')  +
  theme(legend.position="bottom")+
  geom_point()+
  ggsave("10c. Brazil_quadrant_DALYs_cost.jpeg", 
         width = 25, height = 8, units = "cm")





# combine ICER cost plots multiple
Brazil_CE_combine<-ggarrange(Brazil_CE_TB, Brazil_CE_TBdeath, Brazil_CE_DALYs, labels = c("TB", "Deaths", "DALYs"),nrow=3,
                         common.legend = TRUE, legend = "bottom")
Brazil_CE_combine
ggsave("10. Brazil_combined_graphs_CE.png", 
       width = 30, height = 20, units = "cm")




#####################################################################################################################
#Incremental effectiveness table with ranges  
#####################################################################################################################
#ICER TB
Brazil_ICER_table_TB<-Brazil_ICER %>% 
  group_by(comparison) %>% 
  summarise_at(vars(TB),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_ICER_table_TB, file = "8a. Brazil ICERs TB tables.csv")

#ICER - TB deaths
Brazil_ICER_table_TBdeath<-Brazil_ICER %>% 
  group_by(comparison) %>% 
  summarise_at(vars(TBdeaths),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_ICER_table_TBdeath, file = "8b. Brazil ICERs TB deaths tables.csv")

#ICER - DALYs
Brazil_ICER_table_DALYs<-Brazil_ICER %>% 
  group_by(comparison) %>% 
  summarise_at(vars(DALYs),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_ICER_table_DALYs, file = "8c. Brazil ICERs DALYs tables.csv")











#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### INCREMENT COST EFFECTIVENESS RATIO #######
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################



#####################################################################################################################################
#ICER -  Overall (TB cases (DS-TB & RR-TB))
Brazil_ICER_TB_baseline_vs_6h_overall <- data.frame(Runs=Brazil_baseline$Run,(Brazil_strategy_cost$Expanded.6H-Brazil_strategy_cost$Baseline)/(Brazil_epi_cumulative_TBall$Baseline-Brazil_epi_cumulative_TBall$Expanded.6H))
Brazil_ICER_TB_baseline_vs_minimal_overall <- data.frame((Brazil_strategy_cost$Minimal.regimen-Brazil_strategy_cost$Baseline)/(Brazil_epi_cumulative_TBall$Baseline-Brazil_epi_cumulative_TBall$Minimal.regimen))

#####################################################################################################################################
#ICER -  Overall (TB deaths)
Brazil_ICER_TBdeaths_baseline_vs_6h_overall <- data.frame((Brazil_strategy_cost$Expanded.6H-Brazil_strategy_cost$Baseline)/(Brazil_epi_cumulative_TBdeaths$Baseline-Brazil_epi_cumulative_TBdeaths$Expanded.6H))
Brazil_ICER_TBdeaths_baseline_vs_minimal_overall <- data.frame((Brazil_strategy_cost$Minimal.regimen-Brazil_strategy_cost$Baseline)/(Brazil_epi_cumulative_TBdeaths$Baseline-Brazil_epi_cumulative_TBdeaths$Minimal.regimen))

#####################################################################################################################################
#ICER -  Overall (DALYs)
Brazil_ICER_DALYs_baseline_vs_6h_overall <- data.frame((Brazil_strategy_cost$Expanded.6H-Brazil_strategy_cost$Baseline)/(Brazil_epi_cumulative_DALYs$Baseline-Brazil_epi_cumulative_DALYs$Expanded.6H))
Brazil_ICER_DALYs_baseline_vs_minimal_overall <- data.frame((Brazil_strategy_cost$Minimal.regimen-Brazil_strategy_cost$Baseline)/(Brazil_epi_cumulative_DALYs$Baseline-Brazil_epi_cumulative_DALYs$Minimal.regimen))




#combine columns
Brazil_ICER<-data.frame(Brazil_ICER_TB_baseline_vs_6h_overall,
                        Brazil_ICER_TB_baseline_vs_minimal_overall,
                        Brazil_ICER_TBdeaths_baseline_vs_6h_overall,
                        Brazil_ICER_TBdeaths_baseline_vs_minimal_overall,
                        Brazil_ICER_DALYs_baseline_vs_6h_overall,
                        Brazil_ICER_DALYs_baseline_vs_minimal_overall)
colnames(Brazil_ICER) <- c("Runs","Brazil_ICER_TB_baseline_vs_6h_overall",
                           "Brazil_ICER_TB_baseline_vs_minimal_overall",
                           "Brazil_ICER_TBdeaths_baseline_vs_6h_overall",
                           "Brazil_ICER_TBdeaths_baseline_vs_minimal_overall",
                           "Brazil_ICER_DALYs_baseline_vs_6h_overall",
                           "Brazil_ICER_DALYs_baseline_vs_minimal_overall")
Brazil_ICER_graph<-melt(Brazil_ICER, id.vars =c("Runs"), variable.name = "ICER_pop")
Brazil_ICER_graph$cum_ICER<-ave(Brazil_ICER_graph$value, Brazil_ICER_graph$ICER_pop, FUN=cumsum)
Brazil_ICER_graph$comparison = ifelse(grepl("baseline_vs_6h", Brazil_ICER_graph$ICER_pop), "1a. 6H vs baseline",
                                      "1b. Minimal vs baseline")
Brazil_ICER_graph$outcome = ifelse(grepl("DALYs", Brazil_ICER_graph$ICER_pop),"3. incr. cost per DALY averted",
                                   ifelse(grepl("TBdeaths", Brazil_ICER_graph$ICER_pop),"2. incr. cost per TB death averted",
                                          "1. incr. cost per TB case averted"))
Brazil_ICER_graph$ICER_pop = chartr("."," ",Brazil_ICER_graph$ICER_pop)


Brazil_ICER<-aggregate(x=Brazil_ICER_graph$value, by=list(Runs=Brazil_ICER_graph$Runs, comparison=Brazil_ICER_graph$comparison,outcome=Brazil_ICER_graph$outcome), FUN=sum)

#####################################################################################################################
#Incremental effectiveness table with ranges  
#####################################################################################################################
Brazil_ICER_table<-Brazil_ICER %>% 
  group_by(outcome, comparison) %>% 
  summarise_at(vars(x),
               list(median=median,
                    mean=mean,
                    quantile_lower = ~quantile(., probs = 0.025),
                    quantile_upper = ~quantile(., probs = 0.975)))
write.csv(Brazil_ICER_table, file = "8. Brazil ICERs output tables.csv")


