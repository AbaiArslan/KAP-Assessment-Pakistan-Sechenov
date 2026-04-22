# ==============================================================================
# MPH THESIS ANALYSIS SCRIPT
# Author: Arslan Arshad
# Description: Cross-Sectional Assessment of KAP on Antibiotic Use & Resistance
# Output: Publication-ready tables (.docx) and NPG-styled figures (.png)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP, LIBRARIES, AND GLOBAL THEMES
# ------------------------------------------------------------------------------
# install.packages(c("readxl", "dplyr", "tidyverse", "flextable", "officer", "ggplot2", "ggsci", "patchwork", "tidyr"))

library(readxl)
library(dplyr)
library(tidyverse)
library(flextable)
library(officer)
library(ggplot2)
library(ggsci)
library(patchwork)
library(tidyr)

# Flextable styling borders
thick <- fp_border(color = "black", width = 1.5)
thin  <- fp_border(color = "black", width = 0.75)

# Nature Publishing Group (NPG) Theme for ggplot2
nature_theme <- theme_classic(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.subtitle      = element_text(size = 9, colour = "grey40", hjust = 0.5),
    axis.title         = element_text(face = "bold", size = 10),
    axis.text          = element_text(size = 9, colour = "black"),
    axis.line          = element_line(colour = "black", linewidth = 0.4),
    axis.ticks         = element_line(colour = "black", linewidth = 0.4),
    legend.title       = element_blank(),
    legend.text        = element_text(size = 9),
    legend.position    = "top",
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    plot.margin        = margin(10, 15, 10, 10)
  )

# Global Color Palettes
group_cols  <- c("Biomedical" = "#E64B35", "Non-Biomedical" = "#4DBBD5")
gender_cols <- c("Female" = "#E64B35", "Male" = "#4DBBD5")
likert_cols_palette <- c("Always"="#E64B35", "Often"="#F39B7F", "Rarely"="#91D1C2", "Never"="#00A087")

# Global Item Labels
k_items <- paste0("K", 1:10)
k_labels_simple <- c("Effectiveness of antibiotics against bacterial infections only", "Correct conditions treatable with antibiotics (UTI and RTI only)", "Stop antibiotics only after completing the full prescribed course", "Beneficial bacteria exist in the human body", "Frequent use of antibiotics decreases their effectiveness", "Penicillin is an antibiotic", "Flagyl is an antibiotic", "Ibuprofen is a painkiller", "Amoxicillin is an antibiotic", "Paracetamol is a painkiller")

a_items <- paste0("A", 1:6)
a_labels_simple <- c("Antibiotics are overused in the community", "Bacterial antibiotic resistance is a problem in this country", "Antibiotic overuse is the main cause of bacterial resistance", "Antibiotic resistance can affect personal and family health", "More information about antibiotics is necessary", "Increased societal antibiotic use raises the risk of resistance")

p_items <- paste0("P", 1:6)
p_labels_simple <- c("Rarely/Never uses antibiotics for Fever", "Rarely/Never uses antibiotics for Common Cold/Flu", "Completes full antibiotic course", "Never self-medicates with antibiotics", "Rarely/Never asks doctor to prescribe antibiotics for common cold", "Pharmacist always/often explains antibiotic use")

likert_cols <- c("How often do you use antibiotics for the following [Fever]", "How often do you use antibiotics for the following [Common Cold / Flu]", "How often do you use antibiotics for the following [Acute Bronchitis]", "How often do you use antibiotics for the following [Pneumonia]", "Do you stop taking antibiotics as soon as your symptoms improve?", "Have you ever used antibiotics without a doctor's instruction?", "Do you ask the doctor to prescribe antibiotics when you have a common cold?", "Have doctors prescribed you antibiotics for a cold?", "Do pharmacists explain how to use antibiotics when dispensing them?", "Does increased use of antibiotics in society raise the risk of resistance?")
likert_labels <- c("Antibiotic use for Fever", "Antibiotic use for Common Cold/Flu", "Antibiotic use for Acute Bronchitis", "Antibiotic use for Pneumonia", "Stops antibiotics on symptom improvement", "Self-medication without doctor instruction", "Requests antibiotics from doctor for cold", "Doctor has prescribed antibiotics for cold", "Pharmacist explains antibiotic use", "Societal antibiotic use raises resistance risk")

# ------------------------------------------------------------------------------
# 2. DATA IMPORT & CLEANING
# ------------------------------------------------------------------------------
df <- read_excel("mph_thesis.xlsx")

# Split Data into Main Cohorts
df_Pak <- df %>% filter(Nationality == "Pakistan")
df_sec <- df %>% filter(`Current City` == "Moscow")


# ------------------------------------------------------------------------------
# 3. DATA PROCESSING & SCORING
# ------------------------------------------------------------------------------

# Function to score datasets uniformly
score_kap_data <- function(data) {
  data %>%
    mutate(Age_Group = case_when(
      Age < 25             ~ "18-24",
      Age >= 25 & Age < 35 ~ "25-34",
      Age >= 35 & Age < 45 ~ "35-44",
      Age >= 45            ~ "45 and above",
      TRUE                 ~ NA_character_
    )) %>%
    mutate(
      # Knowledge
      K1  = if_else(EFFECTIVENESS_OF_ANTIBIOTICS_AGAINST == "Bacteria only", 1L, 0L),
      K2  = if_else(str_detect(CONDITIONS_CAN_BE_TREATED_WITH_ANTIBIOTICS, "Urinary Tract Infection") &
                      str_detect(CONDITIONS_CAN_BE_TREATED_WITH_ANTIBIOTICS, "Respiratory Tract Infection") &
                      !str_detect(CONDITIONS_CAN_BE_TREATED_WITH_ANTIBIOTICS, "Malaria") &
                      !str_detect(CONDITIONS_CAN_BE_TREATED_WITH_ANTIBIOTICS, "Headache") &
                      !str_detect(CONDITIONS_CAN_BE_TREATED_WITH_ANTIBIOTICS, "Fever"), 1L, 0L),
      K3  = if_else(str_detect(str_to_lower(WHEN_TO_STOP_TAKING_ANTIBIOTICS), "full prescribed course"), 1L, 0L),
      K4  = if_else(BENEFICIAL_BACTERIA_IN_HUMAN_BODY == "Yes", 1L, 0L),
      K5  = if_else(ANTIBIOTIC_EFFECTIVENESS_DECREASE_WITH_FREQUENT_USE == "Yes", 1L, 0L),
      K6  = if_else(IDENTIFY_ANTIBIOTIC_OR_PAINKILLER_PENICILLIN  == "Antibiotic", 1L, 0L),
      K7  = if_else(IDENTIFY_ANTIBIOTIC_OR_PAINKILLER_FLAGYL       == "Antibiotic", 1L, 0L),
      K8  = if_else(IDENTIFY_ANTIBIOTIC_OR_PAINKILLER_IBUPROFEN    == "Painkiller",  1L, 0L),
      K9  = if_else(IDENTIFY_ANTIBIOTIC_OR_PAINKILLER_AMOXICILLIN  == "Antibiotic", 1L, 0L),
      K10 = if_else(IDENTIFY_ANTIBIOTIC_OR_PAINKILLER_PARACETAMOL  == "Painkiller",  1L, 0L),
      Knowledge_Score    = K1+K2+K3+K4+K5+K6+K7+K8+K9+K10,
      Knowledge_Adequate = if_else(Knowledge_Score >= 6, "Adequate", "Inadequate"),
      
      # Attitude
      A1 = if_else(ANTIBIOTICS_OVERUSED_IN_COMMUNITY == "Yes", 1L, 0L),
      A2 = if_else(`Is bacterial antibiotic resistance a problem in your country?` == "Yes", 1L, 0L),
      A3 = if_else(`Is antibiotic overuse the main cause of bacterial resistance?` == "Yes", 1L, 0L),
      A4 = if_else(`Can antibiotic resistance affect you or your family's health?` == "Yes", 1L, 0L),
      A5 = if_else(`Do you think it is necessary to get more information about antibiotics?` == "Yes", 1L, 0L),
      A6 = if_else(`Does increased use of antibiotics in society raise the risk of resistance?` %in% c("Always", "Often"), 1L, 0L),
      Attitude_Score    = A1+A2+A3+A4+A5+A6,
      Attitude_Adequate = if_else(Attitude_Score >= 4, "Positive", "Negative"),
      
      # Practice
      P1 = if_else(`How often do you use antibiotics for the following [Fever]` %in% c("Rarely", "Never"), 1L, 0L),
      P2 = if_else(`How often do you use antibiotics for the following [Common Cold / Flu]` %in% c("Rarely", "Never"), 1L, 0L),
      P3 = if_else(`Do you stop taking antibiotics as soon as your symptoms improve?` %in% c("Rarely", "Never"), 1L, 0L),
      P4 = if_else(`Have you ever used antibiotics without a doctor's instruction?` == "Never", 1L, 0L),
      P5 = if_else(`Do you ask the doctor to prescribe antibiotics when you have a common cold?` %in% c("Rarely", "Never"), 1L, 0L),
      P6 = if_else(`Do pharmacists explain how to use antibiotics when dispensing them?` %in% c("Always", "Often"), 1L, 0L),
      Practice_Score    = P1+P2+P3+P4+P5+P6,
      Practice_Adequate = if_else(Practice_Score >= 4, "Appropriate", "Inappropriate")
    )
}

# Apply scoring to cohorts
df_Pak <- score_kap_data(df_Pak)
df_sec <- score_kap_data(df_sec)

# Create Sub-cohorts for specific analysis
bio <- df_Pak %>% filter(FORMAL_EDUCATION_BIOMEDICAL_SCIENCE == "Yes")
non <- df_Pak %>% filter(FORMAL_EDUCATION_BIOMEDICAL_SCIENCE == "No")
female_sec <- df_sec %>% filter(Gender == "Female")
male_sec   <- df_sec %>% filter(Gender == "Male")

n_total <- nrow(df_Pak)
n_biomed <- nrow(bio)
n_nonbiomed <- nrow(non)
n_total_sec <- nrow(df_sec)


# ==============================================================================
# PART A: PAKISTANI GENERAL POPULATION ANALYSIS
# ==============================================================================

# --- TABLE 1 & FIGURE 1: Demographics ---
demo_table <- data.frame(
  Characteristic = c("Age", "   18-24", "   25-34", "   35-44", "   45 and above",
                     "Gender", "   Male", "   Female", "   Other",
                     "University-Level Education", "   Yes", "   No",
                     "Biomedical / Health-related Education", "   Yes", "   No"),
  Number = c("", sum(df_Pak$Age_Group == "18-24", na.rm=T), sum(df_Pak$Age_Group == "25-34", na.rm=T), sum(df_Pak$Age_Group == "35-44", na.rm=T), sum(df_Pak$Age_Group == "45 and above", na.rm=T),
             "", sum(df_Pak$Gender == "Male", na.rm=T), sum(df_Pak$Gender == "Female", na.rm=T), sum(df_Pak$Gender == "Other", na.rm=T),
             "", sum(df_Pak$FORMAL_UNIVERSITY_EDUCATION == "Yes", na.rm=T), sum(df_Pak$FORMAL_UNIVERSITY_EDUCATION == "No", na.rm=T),
             "", sum(df_Pak$FORMAL_EDUCATION_BIOMEDICAL_SCIENCE == "Yes", na.rm=T), sum(df_Pak$FORMAL_EDUCATION_BIOMEDICAL_SCIENCE == "No", na.rm=T)),
  Frequency = c("", paste0(round(mean(df_Pak$Age_Group == "18-24", na.rm=T)*100, 2), "%"), paste0(round(mean(df_Pak$Age_Group == "25-34", na.rm=T)*100, 2), "%"), paste0(round(mean(df_Pak$Age_Group == "35-44", na.rm=T)*100, 2), "%"), paste0(round(mean(df_Pak$Age_Group == "45 and above", na.rm=T)*100, 2), "%"),
                "", paste0(round(mean(df_Pak$Gender == "Male", na.rm=T)*100, 2), "%"), paste0(round(mean(df_Pak$Gender == "Female", na.rm=T)*100, 2), "%"), paste0(round(mean(df_Pak$Gender == "Other", na.rm=T)*100, 2), "%"),
                "", paste0(round(mean(df_Pak$FORMAL_UNIVERSITY_EDUCATION == "Yes", na.rm=T)*100, 2), "%"), paste0(round(mean(df_Pak$FORMAL_UNIVERSITY_EDUCATION == "No", na.rm=T)*100, 2), "%"),
                "", paste0(round(mean(df_Pak$FORMAL_EDUCATION_BIOMEDICAL_SCIENCE == "Yes", na.rm=T)*100, 2), "%"), paste0(round(mean(df_Pak$FORMAL_EDUCATION_BIOMEDICAL_SCIENCE == "No", na.rm=T)*100, 2), "%")),
  stringsAsFactors = FALSE
)

ft_demo <- flextable(demo_table) %>% set_header_labels(Characteristic = "Socio-demographic Characteristics", Number = "Number", Frequency = "Frequency (%)") %>% border_remove() %>% hline_top(border = thick, part = "header") %>% hline_bottom(border = thin, part = "header") %>% hline_bottom(border = thick, part = "body") %>% italic(i = c(1, 6, 10, 13), j = 1) %>% bold(part = "header") %>% font(fontname = "Times New Roman", part = "all") %>% fontsize(size = 10, part = "all") %>% align(j = 1, align = "left", part = "all") %>% align(j = 2:3, align = "center", part = "all") %>% width(j = 1, width = 3.5) %>% width(j = 2:3, width = 1.2) %>% padding(padding = 2, part = "all")
read_docx() %>% body_add_par(paste0("Table 1. Socio-demographic characteristics of Pakistani Population (n = ", n_total, ")"), style = "Normal") %>% body_add_flextable(ft_demo) %>% print(target = "Table1_Demographics_Pakistan.docx")

g1 <- ggplot(df_Pak %>% filter(!is.na(Age_Group)) %>% count(Age_Group) %>% mutate(Pct = round(n/sum(n)*100,1)), aes(x = factor(Age_Group, levels=c("18-24","25-34","35-44","45 and above")), y = Pct)) + geom_col(fill = "#E64B35", width = 0.55) + geom_text(aes(label = paste0(Pct, "%")), vjust = -0.5, fontface = "bold") + labs(title="Age Distribution", x="Age Group (years)", y="Percentage (%)") + nature_theme
g2 <- ggplot(df_Pak %>% filter(!is.na(Gender), Gender!="Other") %>% count(Gender) %>% mutate(Pct = round(n/sum(n)*100,1)), aes(x = Gender, y = Pct, fill=Gender)) + geom_col(width = 0.45) + geom_text(aes(label = paste0(Pct, "%")), vjust = -0.5, fontface = "bold") + scale_fill_manual(values = c("Male"="#4DBBD5", "Female"="#E64B35")) + labs(title="Gender Distribution", x=NULL, y="Percentage (%)") + nature_theme + theme(legend.position="none")
g3 <- ggplot(df_Pak %>% filter(!is.na(FORMAL_UNIVERSITY_EDUCATION)) %>% count(FORMAL_UNIVERSITY_EDUCATION) %>% mutate(Pct = round(n/sum(n)*100,1), Lbl=ifelse(FORMAL_UNIVERSITY_EDUCATION=="Yes","University Educated","No University Education")), aes(x = Lbl, y = Pct)) + geom_col(fill = "#3C5488", width = 0.45) + geom_text(aes(label = paste0(Pct, "%")), vjust = -0.5, fontface = "bold") + labs(title="University-Level Education", x=NULL, y="Percentage (%)") + nature_theme
g4 <- ggplot(df_Pak %>% filter(!is.na(FORMAL_EDUCATION_BIOMEDICAL_SCIENCE)) %>% count(FORMAL_EDUCATION_BIOMEDICAL_SCIENCE) %>% mutate(Pct = round(n/sum(n)*100,1), Lbl=ifelse(FORMAL_EDUCATION_BIOMEDICAL_SCIENCE=="Yes","Biomedical Education","Non-Biomedical Education")), aes(x = Lbl, y = Pct)) + geom_col(fill = "#00A087", width = 0.45) + geom_text(aes(label = paste0(Pct, "%")), vjust = -0.5, fontface = "bold") + labs(title="Biomedical / Health-related Education", x=NULL, y="Percentage (%)") + nature_theme
fig1 <- (g1 + g2) / (g3 + g4) + plot_annotation(title = "Figure 1. Socio-demographic Characteristics of Pakistani General Population", theme = theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5)))
ggsave("Figure1_Demographics_Pakistan.png", fig1, width = 10, height = 8, dpi = 300, bg = "white")


# --- Helper Function for Tables 3, 5, 7 ---
get_comparison_row <- function(i, item_list, lbls, df_main, df_bio, df_non) {
  item <- item_list[i]
  tbl <- table(df_main$FORMAL_EDUCATION_BIOMEDICAL_SCIENCE, df_main[[item]])
  pval <- tryCatch({ t <- chisq.test(tbl, correct=F); if(any(t$expected<5)) t<-fisher.test(tbl); if(t$p.value<0.001) "< 0.001" else as.character(round(t$p.value,3))}, error=function(e)"N/A")
  data.frame(Question=lbls[i], Total=paste0(sum(df_main[[item]],na.rm=T), " (", round(mean(df_main[[item]],na.rm=T)*100,1), "%)"), Biomedical=paste0(sum(df_bio[[item]],na.rm=T), " (", round(mean(df_bio[[item]],na.rm=T)*100,1), "%)"), NonBiomedical=paste0(sum(df_non[[item]],na.rm=T), " (", round(mean(df_non[[item]],na.rm=T)*100,1), "%)"), P_value=pval)
}

# --- TABLE 3 & FIGURE 2: Knowledge (Pakistan) ---
k_table <- do.call(rbind, lapply(seq_along(k_items), get_comparison_row, item_list=k_items, lbls=k_labels_simple, df_main=df_Pak, df_bio=bio, df_non=non))
t_test_p <- tryCatch({ p<-t.test(Knowledge_Score~FORMAL_EDUCATION_BIOMEDICAL_SCIENCE, data=df_Pak)$p.value; if(p<0.001) "< 0.001" else as.character(round(p,3))}, error=function(e)"N/A")
k_table <- rbind(k_table, data.frame(Question="Mean Knowledge Score (SD)", Total=paste0(round(mean(df_Pak$Knowledge_Score,na.rm=T),2)," (",round(sd(df_Pak$Knowledge_Score,na.rm=T),2),")"), Biomedical=paste0(round(mean(bio$Knowledge_Score,na.rm=T),2)," (",round(sd(bio$Knowledge_Score,na.rm=T),2),")"), NonBiomedical=paste0(round(mean(non$Knowledge_Score,na.rm=T),2)," (",round(sd(non$Knowledge_Score,na.rm=T),2),")"), P_value=t_test_p))

ft_k <- flextable(k_table) %>% set_header_labels(Question="Knowledge Question", Total=paste0("Total\n(n = ",n_total,")"), Biomedical=paste0("Biomedical\n(n = ",n_biomed,")"), NonBiomedical=paste0("Non-Biomedical\n(n = ",n_nonbiomed,")"), P_value="p-value") %>% add_header_row(values=c("Number (%) of Respondents Giving a Correct Answer","","","",""), colwidths=c(1,1,1,1,1)) %>% border_remove() %>% hline_top(border=thick, part="header") %>% hline_bottom(border=thin, part="header") %>% hline_bottom(border=thick, part="body") %>% hline(i=10, border=thin) %>% bold(i=11, j=1) %>% bold(part="header") %>% font(fontname="Times New Roman", part="all") %>% fontsize(size=10, part="all") %>% align(j=1, align="left", part="all") %>% align(j=2:5, align="center", part="all") %>% width(j=1, width=3.0) %>% width(j=2:4, width=1.1) %>% width(j=5, width=0.8)
read_docx() %>% body_add_par(paste0("Table 3. Number and percentage of correctly answered knowledge questions: Biomedical vs Non-Biomedical education (n = ", n_total, ")"), style="Normal") %>% body_add_flextable(ft_k) %>% print(target="Table3_Knowledge_Biomed_vs_NonBiomed.docx")

k_plot_data <- data.frame(Item = factor(rep(rev(k_labels_simple), each=2), levels=rev(k_labels_simple)), Group = rep(c("Biomedical", "Non-Biomedical"), 10), Pct = as.vector(rbind(round(colMeans(bio[,k_items], na.rm=T)*100,1), round(colMeans(non[,k_items], na.rm=T)*100,1))))
k_seg <- k_plot_data %>% pivot_wider(names_from=Group, values_from=Pct)
g2 <- ggplot() + geom_vline(xintercept=60, linetype="dashed", colour="grey50") + geom_segment(data=k_seg, aes(x=`Non-Biomedical`, xend=Biomedical, y=Item, yend=Item), colour="grey75", linewidth=1.2) + geom_point(data=k_plot_data, aes(x=Pct, y=Item, colour=Group), size=4) + geom_text(data=k_plot_data, aes(x=Pct, y=Item, label=paste0(Pct,"%"), colour=Group), nudge_y=0.35, size=2.8, show.legend=F) + scale_colour_manual(values=group_cols) + scale_x_continuous(limits=c(0,115)) + labs(title="Figure 2. Knowledge Item Correct Response Rates – General Population", subtitle="Biomedical vs Non-Biomedical Education", x="Percentage of Correct Responses (%)", y=NULL) + nature_theme
ggsave("Figure2_Knowledge_Pak.png", g2, width=10, height=7, dpi=300, bg="white")


# --- TABLE 5 & FIGURE 5: Attitude (Pakistan) ---
a_table <- do.call(rbind, lapply(seq_along(a_items), get_comparison_row, item_list=a_items, lbls=a_labels_simple, df_main=df_Pak, df_bio=bio, df_non=non))
t_test_att <- tryCatch({ p<-t.test(Attitude_Score~FORMAL_EDUCATION_BIOMEDICAL_SCIENCE, data=df_Pak)$p.value; if(p<0.001) "< 0.001" else as.character(round(p,3))}, error=function(e)"N/A")
a_table <- rbind(a_table, data.frame(Question="Mean Attitude Score (SD)", Total=paste0(round(mean(df_Pak$Attitude_Score,na.rm=T),2)," (",round(sd(df_Pak$Attitude_Score,na.rm=T),2),")"), Biomedical=paste0(round(mean(bio$Attitude_Score,na.rm=T),2)," (",round(sd(bio$Attitude_Score,na.rm=T),2),")"), NonBiomedical=paste0(round(mean(non$Attitude_Score,na.rm=T),2)," (",round(sd(non$Attitude_Score,na.rm=T),2),")"), P_value=t_test_att))

ft_a <- flextable(a_table) %>% set_header_labels(Question="Attitude Question", Total="Total", Biomedical="Biomedical", NonBiomedical="Non-Biomedical", P_value="p-value") %>% add_header_row(values=c("Number (%) of Respondents Giving a Correct Answer","","","",""), colwidths=c(1,1,1,1,1)) %>% border_remove() %>% hline_top(border=thick, part="header") %>% hline_bottom(border=thin, part="header") %>% hline_bottom(border=thick, part="body") %>% hline(i=6, border=thin) %>% bold(i=7, j=1) %>% bold(part="header") %>% font(fontname="Times New Roman", part="all") %>% fontsize(size=10, part="all") %>% align(j=1, align="left", part="all") %>% align(j=2:5, align="center", part="all") %>% width(j=1, width=3.0) %>% width(j=2:4, width=1.1) %>% width(j=5, width=0.8)
read_docx() %>% body_add_par(paste0("Table 5. Number and percentage of correct attitude responses: Biomedical vs Non-Biomedical education (n = ", n_total, ")"), style="Normal") %>% body_add_flextable(ft_a) %>% print(target="Table5_Attitude_Pak.docx")

a_plot_data <- data.frame(Item = factor(rep(rev(a_labels_simple), each=2), levels=rev(a_labels_simple)), Group = rep(c("Biomedical", "Non-Biomedical"), 6), Pct = as.vector(rbind(round(colMeans(bio[,a_items], na.rm=T)*100,1), round(colMeans(non[,a_items], na.rm=T)*100,1))))
g3 <- ggplot(a_plot_data, aes(x=Pct, y=Item, fill=Group)) + geom_col(position="dodge", width=0.6) + geom_text(aes(label=paste0(Pct,"%")), position=position_dodge(width=0.6), hjust=-0.15, size=3) + scale_fill_manual(values=group_cols) + scale_x_continuous(limits=c(0,118)) + labs(title="Figure 5. Positive Attitude Responses by Education Group - General Population", subtitle="Biomedical vs Non-Biomedical Education", x="Percentage of Positive Responses (%)", y=NULL) + nature_theme
ggsave("Figure5_Attitude_Pak.png", g3, width=10, height=6, dpi=300, bg="white")


# --- TABLE 7 & FIGURE 7: Practice (Pakistan) ---
p_table <- do.call(rbind, lapply(seq_along(p_items), get_comparison_row, item_list=p_items, lbls=p_labels_simple, df_main=df_Pak, df_bio=bio, df_non=non))
t_test_prac <- tryCatch({ p<-t.test(Practice_Score~FORMAL_EDUCATION_BIOMEDICAL_SCIENCE, data=df_Pak)$p.value; if(p<0.001) "< 0.001" else as.character(round(p,3))}, error=function(e)"N/A")
p_table <- rbind(p_table, data.frame(Question="Mean Practice Score (SD)", Total=paste0(round(mean(df_Pak$Practice_Score,na.rm=T),2)," (",round(sd(df_Pak$Practice_Score,na.rm=T),2),")"), Biomedical=paste0(round(mean(bio$Practice_Score,na.rm=T),2)," (",round(sd(bio$Practice_Score,na.rm=T),2),")"), NonBiomedical=paste0(round(mean(non$Practice_Score,na.rm=T),2)," (",round(sd(non$Practice_Score,na.rm=T),2),")"), P_value=t_test_prac))

ft_p <- flextable(p_table) %>% set_header_labels(Question="Practice Question", Total="Total", Biomedical="Biomedical", NonBiomedical="Non-Biomedical", P_value="p-value") %>% add_header_row(values=c("Number (%) of Respondents with Appropriate Practice","","","",""), colwidths=c(1,1,1,1,1)) %>% border_remove() %>% hline_top(border=thick, part="header") %>% hline_bottom(border=thin, part="header") %>% hline_bottom(border=thick, part="body") %>% hline(i=6, border=thin) %>% bold(i=7, j=1) %>% bold(part="header") %>% font(fontname="Times New Roman", part="all") %>% fontsize(size=10, part="all") %>% align(j=1, align="left", part="all") %>% align(j=2:5, align="center", part="all") %>% width(j=1, width=3.0) %>% width(j=2:4, width=1.1) %>% width(j=5, width=0.8)
read_docx() %>% body_add_par(paste0("Table 7. Number and percentage of appropriate practice responses: Biomedical vs Non-Biomedical education (n = ", n_total, ")"), style="Normal") %>% body_add_flextable(ft_p) %>% print(target="Table7_Practice_Pak.docx")

p_plot_data <- data.frame(Item = factor(rep(p_labels_simple, each=2), levels=p_labels_simple), Group = rep(c("Biomedical", "Non-Biomedical"), 6), Pct = as.vector(rbind(round(colMeans(bio[,p_items], na.rm=T)*100,1), round(colMeans(non[,p_items], na.rm=T)*100,1))))
g4 <- ggplot(p_plot_data, aes(x=Item, y=Pct, fill=Group)) + geom_col(position="dodge", width=0.6) + geom_text(aes(label=paste0(Pct,"%")), position=position_dodge(width=0.6), vjust=-0.5, size=3) + scale_fill_manual(values=group_cols) + scale_y_continuous(limits=c(0,105)) + labs(title="Figure 7. Appropriate Practice Responses by Education Group - General Population", subtitle="Biomedical vs Non-Biomedical Education", x=NULL, y="Percentage of Appropriate Responses (%)") + nature_theme + theme(axis.text.x = element_text(size=8, angle=15, hjust=1))
ggsave("Figure7_Practice_Pak.png", g4, width=10, height=6, dpi=300, bg="white")


# --- FIGURE 8: Likert Stacked Bar (Pakistan) ---
l_data <- do.call(rbind, lapply(seq_along(likert_cols), function(i) {
  df_Pak %>% filter(!is.na(.data[[likert_cols[i]]])) %>% count(Response = factor(trimws(.data[[likert_cols[i]]]), levels=c("Always","Often","Rarely","Never"))) %>% mutate(Question=likert_labels[i], Pct=round(n/sum(n)*100,1))
})) %>% mutate(Question = factor(Question, levels=rev(likert_labels)))

g_likert <- ggplot(l_data, aes(x=Pct, y=Question, fill=Response)) + geom_col(position="stack", width=0.65) + geom_text(aes(label=ifelse(Pct>=8,paste0(Pct,"%"),"")), position=position_stack(vjust=0.5), size=2.8, colour="white", fontface="bold") + scale_fill_manual(values=likert_cols_palette) + labs(title="Figure 8. Frequency of Antibiotic-Related Behaviours and Practices - General Population", x="Percentage (%)", y=NULL) + nature_theme + theme(legend.position="top")
ggsave("Figure8_Likert_Pak.png", g_likert, width=11, height=7, dpi=300, bg="white")


# --- TABLE 9 & FIGURE 13: Causes Overuse (Pakistan) ---
q22 <- df_Pak %>% filter(ANTIBIOTICS_OVERUSED_IN_COMMUNITY=="Yes")
c_data <- q22 %>% select(`What are the main causes of antibiotic overuse?`) %>% drop_na() %>% pull() %>% paste(collapse=", ") %>% strsplit(", ") %>% unlist() %>% trimws()
c_data <- c_data[c_data != ""]
c_data[str_detect(c_data, "Economic interests")] <- "Economic interests (manufacturers, hospitals, etc.)"
c_freq <- as.data.frame(table(c_data)) %>% arrange(desc(Freq)) %>% filter(!c_data %in% c("etc.)","hospitals")) %>% mutate(Pct=round(Freq/nrow(q22)*100,1))

ft_c <- flextable(c_freq) %>% set_header_labels(c_data="Cause of Antibiotic Overuse", Freq="Number", Pct="Percentage (%)") %>% border_remove() %>% hline_top(border=thick, part="header") %>% hline_bottom(border=thin, part="header") %>% hline_bottom(border=thick, part="body") %>% font(fontname="Times New Roman", part="all") %>% fontsize(size=10, part="all") %>% align(j=1, align="left", part="all") %>% align(j=2:3, align="center", part="all") %>% width(j=1, width=3.5) %>% width(j=2, width=1.0) %>% width(j=3, width=1.2) %>% padding(padding=2, part="all")
read_docx() %>% body_add_par(paste0("Table 9. Main Causes of Antibiotic Overuse – General Population (n = ", nrow(q22), ")"), style="Normal") %>% body_add_flextable(ft_c) %>% print(target="Table9_Causes_Pak.docx")

g_c <- ggplot(c_freq, aes(x=Pct, y=reorder(c_data, Pct))) + geom_col(fill="#E64B35", width=0.55) + geom_text(aes(label=paste0(Freq, " (", Pct, "%)")), hjust=-0.1) + labs(title="Figure 13. Perceived Main Causes of Antibiotic Overuse – General Population", x="Percentage of Respondents (%)", y=NULL) + scale_x_continuous(limits=c(0,100)) + nature_theme
ggsave("Figure13_Causes_Pak.png", g_c, width=9, height=5, dpi=300, bg="white")


# --- FIGURE 11: Mean KAP Scores (Pakistan) ---
kap_summary <- data.frame(Domain=rep(c("Knowledge Score (out of 10)","Attitude Score (out of 6)","Practice Score (out of 6)"),2), Group=c(rep("Biomedical",3), rep("Non-Biomedical",3)), Mean=c(round(mean(bio$Knowledge_Score,na.rm=T),2), round(mean(bio$Attitude_Score,na.rm=T),2), round(mean(bio$Practice_Score,na.rm=T),2), round(mean(non$Knowledge_Score,na.rm=T),2), round(mean(non$Attitude_Score,na.rm=T),2), round(mean(non$Practice_Score,na.rm=T),2)), SD=c(round(sd(bio$Knowledge_Score,na.rm=T),2), round(sd(bio$Attitude_Score,na.rm=T),2), round(sd(bio$Practice_Score,na.rm=T),2), round(sd(non$Knowledge_Score,na.rm=T),2), round(sd(non$Attitude_Score,na.rm=T),2), round(sd(non$Practice_Score,na.rm=T),2))) %>% mutate(Domain=factor(Domain, levels=c("Knowledge Score (out of 10)","Attitude Score (out of 6)","Practice Score (out of 6)")))
g5 <- ggplot(kap_summary, aes(x=Domain, y=Mean, fill=Group)) + geom_col(position="dodge", width=0.5) + geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), position=position_dodge(width=0.5), width=0.2) + geom_text(aes(label=Mean), position=position_dodge(width=0.5), vjust=-1.2, fontface="bold") + scale_fill_manual(values=group_cols) + labs(title="Figure 11. Mean Knowledge, Attitude and Practice Scores by Education Group", subtitle="Error bars represent ± 1 Standard Deviation", x=NULL, y="Mean Score") + nature_theme
ggsave("Figure11_Mean_KAP_Pak.png", g5, width=8, height=6, dpi=300, bg="white")


# --- TABLE 11 & FIGURE 15: Sources (Pakistan) ---
s_data <- df_Pak %>% select(`Where do you usually get antibiotics from?`) %>% drop_na() %>% pull() %>% paste(collapse=", ") %>% strsplit(", ") %>% unlist() %>% trimws()
s_freq <- as.data.frame(table(s_data)) %>% arrange(desc(Freq)) %>% mutate(Pct=round(Freq/nrow(df_Pak)*100,1))

ft_s <- flextable(s_freq) %>% set_header_labels(s_data="Source of Antibiotic Procurement", Freq="Number", Pct="Percentage (%)") %>% border_remove() %>% hline_top(border=thick, part="header") %>% hline_bottom(border=thin, part="header") %>% hline_bottom(border=thick, part="body") %>% font(fontname="Times New Roman", part="all") %>% fontsize(size=10, part="all") %>% align(j=1, align="left", part="all") %>% align(j=2:3, align="center", part="all") %>% width(j=1, width=3.5) %>% width(j=2, width=1.0) %>% width(j=3, width=1.2) %>% padding(padding=2, part="all")
read_docx() %>% body_add_par(paste0("Table 11. Sources from which respondents usually obtain antibiotics (n = ", nrow(df_Pak), ", multiple responses permitted)"), style="Normal") %>% body_add_flextable(ft_s) %>% print(target="Table11_Sources_Pak.docx")

g_s <- ggplot(s_freq, aes(x=Pct, y=reorder(s_data, Pct))) + geom_col(fill="#3C5488", width=0.55) + geom_text(aes(label=paste0(Freq, " (", Pct, "%)")), hjust=-0.1) + labs(title="Figure 15. Sources of Antibiotic Procurement – Pakistan General Population", x="Percentage of Respondents (%)", y=NULL) + scale_x_continuous(limits=c(0,100)) + nature_theme
ggsave("Figure15_Sources_Pak.png", g_s, width=9, height=5, dpi=300, bg="white")


# --- TABLE 12 & FIGURE 16: Basis Selection (Pakistan) ---
b_data <- df_Pak %>% select(`What do you rely on when choosing antibiotics?`) %>% drop_na() %>% pull() %>% paste(collapse=", ") %>% strsplit(", ") %>% unlist() %>% trimws()
std_opts <- c("Doctor’s prescription", "Personal experience", "Pharmacist’s advice", "Friends’ recommendation")
b_clean <- ifelse(b_data %in% std_opts | str_detect(b_data, "prescription|experience|advice|recommendation"), b_data, "Others")
b_clean[str_detect(b_clean, "prescription")] <- "Doctor’s prescription"
b_clean[str_detect(b_clean, "experience")] <- "Personal experience"
b_clean[str_detect(b_clean, "advice")] <- "Pharmacist’s advice"
b_clean[str_detect(b_clean, "recommendation")] <- "Friends’ recommendation"
b_freq <- as.data.frame(table(b_clean)) %>% arrange(desc(Freq)) %>% mutate(Pct=round(Freq/nrow(df_Pak)*100,1))

ft_b <- flextable(b_freq) %>% set_header_labels(b_clean="Basis for Antibiotic Selection", Freq="Number", Pct="Percentage (%)") %>% border_remove() %>% hline_top(border=thick, part="header") %>% hline_bottom(border=thin, part="header") %>% hline_bottom(border=thick, part="body") %>% font(fontname="Times New Roman", part="all") %>% fontsize(size=10, part="all") %>% align(j=1, align="left", part="all") %>% align(j=2:3, align="center", part="all") %>% width(j=1, width=3.5) %>% width(j=2, width=1.0) %>% width(j=3, width=1.2) %>% padding(padding=2, part="all")
read_docx() %>% body_add_par(paste0("Table 12. Factors respondents rely on when choosing antibiotics (n = ", nrow(df_Pak), ")"), style="Normal") %>% body_add_flextable(ft_b) %>% print(target="Table12_Basis_Pak.docx")

g_b <- ggplot(b_freq, aes(x=Pct, y=reorder(b_clean, Pct))) + geom_col(fill="#00A087", width=0.55) + geom_text(aes(label=paste0(Freq, " (", Pct, "%)")), hjust=-0.1) + labs(title="Figure 16. Basis for Antibiotic Selection – Pakistan General Population", x="Percentage of Respondents (%)", y=NULL) + scale_x_continuous(limits=c(0,100)) + nature_theme
ggsave("Figure16_Basis_Pak.png", g_b, width=9, height=5, dpi=300, bg="white")


# --- TABLE 14 & FIGURE 19: Doctor Rating (Pakistan) ---
r_col <- "How do you rate a doctor who does NOT prescribe antibiotics when you think they are needed?"
r_freq <- df_Pak %>% filter(!is.na(!!sym(r_col))) %>% count(Rating = factor(!!sym(r_col), levels=1:5)) %>% mutate(Pct=round(n/sum(n)*100,1))

ft_r <- flextable(r_freq) %>% set_header_labels(Rating="Satisfaction Rating", n="Number", Pct="Percentage (%)") %>% border_remove() %>% hline_top(border=thick, part="header") %>% hline_bottom(border=thin, part="header") %>% hline_bottom(border=thick, part="body") %>% font(fontname="Times New Roman", part="all") %>% fontsize(size=10, part="all") %>% align(j=1, align="left", part="all") %>% align(j=2:3, align="center", part="all") %>% width(j=1, width=3.5) %>% width(j=2, width=1.0) %>% width(j=3, width=1.2) %>% padding(padding=2, part="all")
read_docx() %>% body_add_par(paste0("Table 14. Satisfaction rating of a doctor who withholds antibiotics when patient believes they are needed (n = ", nrow(df_Pak), ")"), style="Normal") %>% body_add_flextable(ft_r) %>% print(target="Table14_Rating_Pak.docx")

g_r <- ggplot(r_freq, aes(x=Rating, y=Pct)) + geom_col(fill="#4DBBD5", width=0.55) + geom_text(aes(label=paste0(n, " (", Pct, "%)")), vjust=-0.5) + labs(title="Figure 19. Satisfaction Rating of a Doctor Who Withholds Antibiotics – General Pakistani Population", x="Satisfaction Rating (1 = Least satisfied, 5 = Most satisfied)", y="Percentage (%)") + scale_y_continuous(limits=c(0, 45)) + nature_theme
ggsave("Figure19_Rating_Pak.png", g_r, width=8, height=5.5, dpi=300, bg="white")



# ==============================================================================
# PART B: SECHENOV UNIVERSITY STUDENTS ANALYSIS
# ==============================================================================

# --- TABLE 2: Demographics (Sechenov) ---
demo_sec_table <- data.frame(
  Characteristic = c("Age", "   18-24", "   25-34", "   35-44",
                     "Gender", "   Male", "   Female", "   Other"),
  Number = c("", sum(df_sec$Age_Group == "18-24", na.rm=T), sum(df_sec$Age_Group == "25-34", na.rm=T), sum(df_sec$Age_Group == "35-44", na.rm=T),
             "", sum(df_sec$Gender == "Male", na.rm=T), sum(df_sec$Gender == "Female", na.rm=T), sum(df_sec$Gender == "Other", na.rm=T)),
  Frequency = c("", paste0(round(mean(df_sec$Age_Group == "18-24", na.rm=T)*100, 2), "%"), paste0(round(mean(df_sec$Age_Group == "25-34", na.rm=T)*100, 2), "%"), paste0(round(mean(df_sec$Age_Group == "35-44", na.rm=T)*100, 2), "%"),
                "", paste0(round(mean(df_sec$Gender == "Male", na.rm=T)*100, 2), "%"), paste0(round(mean(df_sec$Gender == "Female", na.rm=T)*100, 2), "%"), paste0(round(mean(df_sec$Gender == "Other", na.rm=T)*100, 2), "%")),
  stringsAsFactors = FALSE
)

ft_demo_sec <- flextable(demo_sec_table) %>% set_header_labels(Characteristic = "Socio-demographic Characteristics", Number = "Number", Frequency = "Frequency (%)") %>% border_remove() %>% hline_top(border = thick, part = "header") %>% hline_bottom(border = thin, part = "header") %>% hline_bottom(border = thick, part = "body") %>% italic(i = c(1, 5), j = 1) %>% bold(part = "header") %>% font(fontname = "Times New Roman", part = "all") %>% fontsize(size = 10, part = "all") %>% align(j = 1, align = "left", part = "all") %>% align(j = 2:3, align = "center", part = "all") %>% width(j = 1, width = 3.5) %>% width(j = 2:3, width = 1.2) %>% padding(padding = 2, part = "all")
read_docx() %>% body_add_par(paste0("Table 2. Socio-demographic characteristics of Sechenov University Students (n = ", nrow(df_sec), ")"), style = "Normal") %>% body_add_flextable(ft_demo_sec) %>% print(target = "Table2_Demographics_Sec.docx")


# --- FIGURE 3: Knowledge Overall (Sechenov) ---
k_plot_sec <- data.frame(Item = factor(rev(k_labels_simple), levels = rev(k_labels_simple)), Pct = round(colMeans(df_sec[,k_items], na.rm = TRUE) * 100, 1))
g3_sec <- ggplot(k_plot_sec, aes(x = Pct, y = Item)) + geom_col(fill = "#4DBBD5", width = 0.6) + geom_text(aes(label = paste0(Pct, "%")), hjust = -0.15, size = 3.2) + scale_x_continuous(limits = c(0, 115), expand = c(0, 0)) + labs(title = "Figure 3. Knowledge Item Correct Response Rates – Sechenov Students", x = "Percentage of Correct Responses (%)", y = NULL) + nature_theme + theme(axis.text.y = element_text(size = 8.5, lineheight = 1.1))
ggsave("Figure3_Knowledge_Sec.png", g3_sec, width = 10, height = 7, dpi = 300, bg = "white")


# --- Helper Function for Gender Comparison Tables (Sechenov) ---
get_sec_gender_row <- function(items, lbls, df_full, df_f, df_m) {
  do.call(rbind, lapply(seq_along(items), function(i) {
    tbl <- table(df_full$Gender, df_full[[items[i]]])
    pval <- tryCatch({ t<-chisq.test(tbl, correct=F); if(any(t$expected<5)) t<-fisher.test(tbl); if(t$p.value<0.001) "< 0.001" else as.character(round(t$p.value,3))}, error=function(e)"N/A")
    data.frame(Question=lbls[i], Total=paste0(sum(df_full[[items[i]]],na.rm=T)," (",round(mean(df_full[[items[i]]],na.rm=T)*100,1),"%)"), Female=paste0(sum(df_f[[items[i]]],na.rm=T)," (",round(mean(df_f[[items[i]]],na.rm=T)*100,1),"%)"), Male=paste0(sum(df_m[[items[i]]],na.rm=T)," (",round(mean(df_m[[items[i]]],na.rm=T)*100,1),"%)"), P_value=pval)
  }))
}

# --- TABLE 4a & FIGURE 4: Knowledge Gender (Sechenov) ---
k_tab_sec <- get_sec_gender_row(k_items, k_labels_simple, df_sec, female_sec, male_sec)
tp <- tryCatch(round(t.test(Knowledge_Score~Gender, data=df_sec)$p.value,3), error=function(e)"N/A")
k_tab_sec <- rbind(k_tab_sec, data.frame(Question="Mean Score", Total=round(mean(df_sec$Knowledge_Score,na.rm=T),2), Female=round(mean(female_sec$Knowledge_Score,na.rm=T),2), Male=round(mean(male_sec$Knowledge_Score,na.rm=T),2), P_value=tp))
ft_ks <- flextable(k_tab_sec) %>% set_header_labels(Question="Question", Total="Total", Female="Female", Male="Male") %>% border_remove() %>% hline_top(border=thick) %>% hline_bottom(border=thick) %>% font(fontname="Times New Roman", part="all") %>% fontsize(size=10, part="all") %>% align(j=1, align="left", part="all") %>% align(j=2:5, align="center", part="all") %>% width(j=1, width=3.0) %>% width(j=2:4, width=1.1) %>% width(j=5, width=0.8)
read_docx() %>% body_add_par("Table 4a. Number and percentage of correctly answered knowledge questions - Sechenov University Students", style="Normal") %>% body_add_flextable(ft_ks) %>% print(target="Sec_Table4a_Knowledge_Gender.docx")

ks_plot <- data.frame(Item = factor(rep(rev(k_labels_simple), each=2), levels=rev(k_labels_simple)), Group = rep(c("Female", "Male"), 10), Pct = as.vector(rbind(round(colMeans(female_sec[,k_items], na.rm=T)*100,1), round(colMeans(male_sec[,k_items], na.rm=T)*100,1))))
g_ks <- ggplot(ks_plot, aes(x=Pct, y=Item, fill=Group)) + geom_col(position="dodge", width=0.6) + scale_fill_manual(values=gender_cols) + labs(title="Figure 4. Knowledge Item Correct Response Rate by Genders – Sechenov Students", x="Percentage (%)", y=NULL) + nature_theme
ggsave("Figure4_Knowledge_Gender_Sec.png", g_ks, width=10, height=7, dpi=300, bg="white")


# --- TABLE 6a & FIGURE 6: Attitude Gender (Sechenov) ---
a_tab_sec <- get_sec_gender_row(a_items, a_labels_simple, df_sec, female_sec, male_sec)
ft_as <- flextable(a_tab_sec) %>% border_remove() %>% hline_top(border=thick) %>% hline_bottom(border=thick) %>% font(fontname="Times New Roman", part="all") %>% fontsize(size=10, part="all") %>% align(j=1, align="left", part="all") %>% align(j=2:5, align="center", part="all") %>% width(j=1, width=3.0) %>% width(j=2:4, width=1.1) %>% width(j=5, width=0.8)
read_docx() %>% body_add_par("Table 6a. Number and percentage of correct attitude responses — Sechenov University Students", style="Normal") %>% body_add_flextable(ft_as) %>% print(target="Sec_Table6a_Attitude_Gender.docx")

as_plot <- data.frame(Item = factor(rep(rev(a_labels_simple), each=2), levels=rev(a_labels_simple)), Group = rep(c("Female", "Male"), 6), Pct = as.vector(rbind(round(colMeans(female_sec[,a_items], na.rm=T)*100,1), round(colMeans(male_sec[,a_items], na.rm=T)*100,1))))
g_as <- ggplot(as_plot, aes(x=Pct, y=Item, fill=Group)) + geom_col(position="dodge", width=0.6) + scale_fill_manual(values=gender_cols) + labs(title="Figure 6. Positive attitude responses — Sechenov University Students", x="Percentage (%)", y=NULL) + nature_theme
ggsave("Figure6_Attitude_Gender_Sec.png", g_as, width=10, height=6, dpi=300, bg="white")


# --- TABLE 8a & FIGURE 9: Practice Gender (Sechenov) ---
p_tab_sec <- get_sec_gender_row(p_items, p_labels_simple, df_sec, female_sec, male_sec)
ft_ps <- flextable(p_tab_sec) %>% border_remove() %>% hline_top(border=thick) %>% hline_bottom(border=thick) %>% font(fontname="Times New Roman", part="all") %>% fontsize(size=10, part="all") %>% align(j=1, align="left", part="all") %>% align(j=2:5, align="center", part="all") %>% width(j=1, width=3.0) %>% width(j=2:4, width=1.1) %>% width(j=5, width=0.8)
read_docx() %>% body_add_par("Table 8a. Practice questions by gender — Sechenov University Students", style="Normal") %>% body_add_flextable(ft_ps) %>% print(target="Sec_Table8a_Practice_Gender.docx")

ps_plot <- data.frame(Item = factor(rep(p_labels_simple, each=2), levels=p_labels_simple), Group = rep(c("Female", "Male"), 6), Pct = as.vector(rbind(round(colMeans(female_sec[,p_items], na.rm=T)*100,1), round(colMeans(male_sec[,p_items], na.rm=T)*100,1))))
g_ps <- ggplot(ps_plot, aes(x=Item, y=Pct, fill=Group)) + geom_col(position="dodge", width=0.6) + scale_fill_manual(values=gender_cols) + labs(title="Figure 9. Appropriate Practice Response – Sechenov Students", x=NULL, y="Percentage (%)") + nature_theme + theme(axis.text.x = element_text(size=8, angle=15, hjust=1))
ggsave("Figure9_Practice_Gender_Sec.png", g_ps, width=10, height=6, dpi=300, bg="white")


# --- FIGURE 10: Likert Stacked Bar (Sechenov) ---
l_data_sec <- do.call(rbind, lapply(seq_along(likert_cols), function(i) {
  df_sec %>% filter(!is.na(.data[[likert_cols[i]]])) %>% count(Response = factor(trimws(.data[[likert_cols[i]]]), levels=c("Always","Often","Rarely","Never"))) %>% mutate(Question=likert_labels[i], Pct=round(n/sum(n)*100,1))
})) %>% mutate(Question = factor(Question, levels=rev(likert_labels)))

g_likert_sec <- ggplot(l_data_sec, aes(x=Pct, y=Question, fill=Response)) + geom_col(position="stack", width=0.65) + geom_text(aes(label=ifelse(Pct>=8,paste0(Pct,"%"),"")), position=position_stack(vjust=0.5), size=2.8, colour="white", fontface="bold") + scale_fill_manual(values=likert_cols_palette) + labs(title="Figure 10. Frequency Of Antibiotic-Related Behaviours and Practices", subtitle=paste0("Sechenov University Students (n = ", nrow(df_sec), ")"), x="Percentage (%)", y=NULL, caption = "Red = Always (most frequent/inappropriate); Green = Never (least frequent/most appropriate)") + nature_theme + theme(legend.position="top", axis.text.y = element_text(size = 8.5), plot.caption = element_text(size=7.5, colour="grey40", hjust=0, margin=margin(t=8)))
ggsave("Figure10_Likert_Sec.png", g_likert_sec, width=11, height=7, dpi=300, bg="white")


# --- FIGURE 12: Mean KAP Scores (Sechenov) ---
kap_sec_sum <- data.frame(Domain=c("Knowledge Score (out of 10)","Attitude Score (out of 6)","Practice Score (out of 6)"), Mean=c(round(mean(df_sec$Knowledge_Score,na.rm=T),2), round(mean(df_sec$Attitude_Score,na.rm=T),2), round(mean(df_sec$Practice_Score,na.rm=T),2)), SD=c(round(sd(df_sec$Knowledge_Score,na.rm=T),2), round(sd(df_sec$Attitude_Score,na.rm=T),2), round(sd(df_sec$Practice_Score,na.rm=T),2))) %>% mutate(Domain=factor(Domain, levels=c("Knowledge Score (out of 10)","Attitude Score (out of 6)","Practice Score (out of 6)")))
g_kaps <- ggplot(kap_sec_sum, aes(x=Domain, y=Mean)) + geom_col(fill="#4DBBD5", width=0.5) + geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=0.2) + geom_text(aes(label=Mean), vjust=-1.2, fontface="bold") + labs(title="Figure 12. Mean Knowledge, Attitude and Practice Scores", subtitle="Sechenov University Students", x=NULL, y="Mean Score") + nature_theme
ggsave("Figure12_Mean_KAP_Sec.png", g_kaps, width=8, height=6, dpi=300, bg="white")


# --- TABLE 10 & FIGURE 14: Causes Overuse (Sechenov) ---
q22_sec <- df_sec %>% filter(ANTIBIOTICS_OVERUSED_IN_COMMUNITY=="Yes")
c_data_sec <- q22_sec %>% select(`What are the main causes of antibiotic overuse?`) %>% drop_na() %>% pull() %>% paste(collapse=", ") %>% strsplit(", ") %>% unlist() %>% trimws()
c_data_sec <- c_data_sec[c_data_sec != ""]
c_data_sec[str_detect(c_data_sec, "Economic interests")] <- "Economic interests (manufacturers, hospitals, etc.)"
c_freq_sec <- as.data.frame(table(c_data_sec)) %>% arrange(desc(Freq)) %>% filter(!c_data_sec %in% c("etc.)","hospitals")) %>% mutate(Pct=round(Freq/nrow(q22_sec)*100,1))

ft_c_sec <- flextable(c_freq_sec) %>% set_header_labels(c_data_sec="Cause of Antibiotic Overuse", Freq="Number", Pct="Percentage (%)") %>% border_remove() %>% hline_top(border=thick, part="header") %>% hline_bottom(border=thin, part="header") %>% hline_bottom(border=thick, part="body") %>% font(fontname = "Times New Roman", part = "all") %>% fontsize(size = 10, part = "all") %>% align(j = 1, align = "left", part = "all") %>% align(j = 2:3, align = "center", part = "all") %>% width(j = 1, width = 3.5) %>% width(j = 2, width = 1.0) %>% width(j = 3, width = 1.2) %>% padding(padding = 2, part = "all")
read_docx() %>% body_add_par(paste0("Table 10. Main Causes of Antibiotic Overuse - Sechenov University Students (n = ", nrow(q22_sec), ")"), style="Normal") %>% body_add_flextable(ft_c_sec) %>% print(target="Table10_Causes_Sec.docx")

g_c_sec <- ggplot(c_freq_sec, aes(x=Pct, y=reorder(c_data_sec, Pct))) + geom_col(fill="#4DBBD5", width=0.55) + geom_text(aes(label=paste0(Freq, " (", Pct, "%)")), hjust=-0.1) + labs(title="Figure 14. Perceived Main Causes of Antibiotic Overuse – Sechenov Students", subtitle=paste0("Among respondents who perceived community overuse (n = ", nrow(q22_sec), ")"), x="Percentage of Respondents (%)", y=NULL) + scale_x_continuous(limits=c(0,105)) + nature_theme + theme(axis.text.y = element_text(size=9))
ggsave("Figure14_Causes_Sec.png", g_c_sec, width=9, height=5, dpi=300, bg="white")


# --- FIGURE 17: Sources of Procurement (Sechenov) ---
s_data_sec <- df_sec %>% select(`Where do you usually get antibiotics from?`) %>% drop_na() %>% pull() %>% paste(collapse=", ") %>% strsplit(", ") %>% unlist() %>% trimws()
s_freq_sec <- as.data.frame(table(s_data_sec)) %>% arrange(desc(Freq)) %>% mutate(Pct=round(Freq/nrow(df_sec)*100,1))

g_s_sec <- ggplot(s_freq_sec, aes(x=Pct, y=reorder(s_data_sec, Pct))) + geom_col(fill="#3C5488", width=0.55) + geom_text(aes(label=paste0(Freq, " (", Pct, "%)")), hjust=-0.1, size=3.2) + labs(title="Figure 17. Sources of Antibiotic Procurement – Sechenov Students", x="Percentage of Respondents (%)", y=NULL) + scale_x_continuous(limits=c(0,105), expand = c(0, 0)) + nature_theme + theme(axis.text.y = element_text(size=9))
ggsave("Figure17_Sources_Sec.png", g_s_sec, width=9, height=5, dpi=300, bg="white")


# --- TABLE 13 & FIGURE 18: Basis for Antibiotic Selection (Sechenov) ---
b_data_sec <- df_sec %>% select(`What do you rely on when choosing antibiotics?`) %>% drop_na() %>% pull() %>% paste(collapse=", ") %>% strsplit(", ") %>% unlist() %>% trimws()
std_opts <- c("Doctor’s prescription", "Personal experience", "Pharmacist’s advice", "Friends’ recommendation")
b_clean_sec <- ifelse(b_data_sec %in% std_opts | str_detect(b_data_sec, "prescription|experience|advice|recommendation"), b_data_sec, "Others")
b_clean_sec[str_detect(b_clean_sec, "prescription")] <- "Doctor’s prescription"
b_clean_sec[str_detect(b_clean_sec, "experience")] <- "Personal experience"
b_clean_sec[str_detect(b_clean_sec, "advice")] <- "Pharmacist’s advice"
b_clean_sec[str_detect(b_clean_sec, "recommendation")] <- "Friends’ recommendation"
b_freq_sec <- as.data.frame(table(b_clean_sec)) %>% arrange(desc(Freq)) %>% mutate(Pct=round(Freq/nrow(df_sec)*100,1))

b_table_sec <- data.frame(Basis = as.character(b_freq_sec$b_clean_sec), n = b_freq_sec$Freq, Percentage = paste0(b_freq_sec$Pct, "%"), stringsAsFactors = FALSE)
b_table_sec <- rbind(b_table_sec, data.frame(Basis = "* Others includes responses not fitting standard categories. Grouped intentionally to simplify the analysis.", n = NA, Percentage = NA, stringsAsFactors = FALSE))

ft_b_sec <- flextable(b_table_sec) %>% set_header_labels(Basis="Basis for Antibiotic Selection", n="Number", Percentage="Percentage (%)") %>% border_remove() %>% hline_top(border=thick, part="header") %>% hline_bottom(border=thin, part="header") %>% hline_bottom(border=thick, part="body") %>% italic(i = nrow(b_table_sec), j = 1) %>% fontsize(i = nrow(b_table_sec), size = 8, part = "body") %>% bold(part="header") %>% font(fontname="Times New Roman", part="all") %>% fontsize(size=10, part="all") %>% align(j=1, align="left", part="all") %>% align(j=2:3, align="center", part="all") %>% width(j=1, width=3.5) %>% width(j=2, width=1.0) %>% width(j=3, width=1.2) %>% padding(padding = 2, part = "all")
read_docx() %>% body_add_par(paste0("Table 13. Factors Sechenov University Students rely on when choosing antibiotics (n = ", nrow(df_sec), ", multiple responses permitted)"), style="Normal") %>% body_add_flextable(ft_b_sec) %>% print(target="Table13_Basis_Sec.docx")

g_b_sec <- ggplot(b_freq_sec, aes(x=Pct, y=reorder(b_clean_sec, Pct))) + geom_col(fill="#00A087", width=0.55) + geom_text(aes(label=paste0(Freq, " (", Pct, "%)")), hjust=-0.1, size=3.2) + labs(title="Figure 18. Basis for Antibiotic Selection – Sechenov Students", x="Percentage of Respondents (%)", y=NULL, caption = "* Others includes responses not fitting standard categories.\n  Grouped intentionally to simplify the analysis.") + scale_x_continuous(limits=c(0,105), expand = c(0, 0)) + nature_theme + theme(axis.text.y = element_text(size=9), plot.caption = element_text(size = 7.5, colour = "grey40", hjust = 0, margin = margin(t = 8)))
ggsave("Figure18_Basis_Sec.png", g_b_sec, width=9, height=5.5, dpi=300, bg="white")


# --- TABLE 15 & FIGURE 20: Doctor Satisfaction Rating (Sechenov) ---
r_col_sec <- "How do you rate a doctor who does NOT prescribe antibiotics when you think they are needed?"
r_freq_sec <- df_sec %>% filter(!is.na(!!sym(r_col_sec))) %>% count(Rating = factor(!!sym(r_col_sec), levels=1:5)) %>% mutate(Pct=round(n/sum(n)*100,1))

rating_table_sec <- data.frame(Rating = paste0("Rating ", r_freq_sec$Rating, ifelse(r_freq_sec$Rating == 1, " (Least satisfied)", ifelse(r_freq_sec$Rating == 5, " (Most satisfied)", ""))), n = r_freq_sec$n, Percentage = paste0(r_freq_sec$Pct, "%"), stringsAsFactors = FALSE)

ft_r_sec <- flextable(rating_table_sec) %>% set_header_labels(Rating="Satisfaction Rating", n="Number", Percentage="Percentage (%)") %>% border_remove() %>% hline_top(border=thick, part="header") %>% hline_bottom(border=thin, part="header") %>% hline_bottom(border=thick, part="body") %>% bold(part="header") %>% font(fontname="Times New Roman", part="all") %>% fontsize(size=10, part="all") %>% align(j=1, align="left", part="all") %>% align(j=2:3, align="center", part="all") %>% width(j=1, width=3.5) %>% width(j=2, width=1.0) %>% width(j=3, width=1.2) %>% padding(padding = 2, part = "all")
read_docx() %>% body_add_par(paste0("Table 15. Satisfaction rating of a doctor who withholds antibiotics — Sechenov University Students (n = ", nrow(df_sec), ")"), style="Normal") %>% body_add_flextable(ft_r_sec) %>% print(target="Table15_Rating_Sec.docx")

g_r_sec <- ggplot(r_freq_sec, aes(x=Rating, y=Pct)) + geom_col(fill="#4DBBD5", width=0.55) + geom_text(aes(label=paste0(n, " (", Pct, "%)")), vjust=-0.5, size=3.2) + scale_x_discrete(labels = c("1" = "1\n(Least satisfied)", "2" = "2", "3" = "3", "4" = "4", "5" = "5\n(Most satisfied)")) + scale_y_continuous(limits=c(0, 45), labels = function(x) paste0(x, "%"), expand = c(0, 0)) + labs(title="Figure 20. Satisfaction Rating of a Doctor Who Withholds Antibiotics – Sechenov University", x="Satisfaction Rating (1 = Least satisfied, 5 = Most satisfied)", y="Percentage (%)") + nature_theme + theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3))
ggsave("Figure20_Rating_Sec.png", g_r_sec, width=8, height=5.5, dpi=300, bg="white")

cat("\n=== ALL ANALYSIS COMPLETE AND EXPORTED ===\n")
