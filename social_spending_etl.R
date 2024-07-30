# Packages -----------------------------------------------------------
if(require(countrycode) == F) install.packages('countrycode'); require(countrycode)
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(gridExtra) == F) install.packages('gridExtra'); require(gridExtra)
if(require(haven) == F) install.packages('haven'); require(haven)
if(require(Hmisc) == F) install.packages('Hmisc'); require(Hmisc)
if(require(priceR) == F) install.packages('priceR'); require(readxl)
if(require(readxl) == F) install.packages('readxl'); require(readxl)
if(require(rvest) == F) install.packages('rvest'); require(rvest)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(tidyr) == F) install.packages('tidyr'); require(tidyr)
if(require(wbstats) == F) install.packages('wbstats'); require(wbstats)

# Functions -------------------------------------------------------------
## I designed this function to collect World Bank Data more efficiently far below
wb_etl <- function(y, w, z){
  wb_data(country = "countries_only", indicator = as.character(y), start_date = w, end_date = z) %>% 
    filter(!(country %in% c("Channel Islands", "Kosovo"))) %>% 
    rename(year = date) %>% 
    mutate(country = countryname(country),
           iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
           year = as.double(year))
}

# 1. DEPENDENT VARIABLE ------------------------------------------------
## 1.1 Social Spending per capita (Constant Prices) --------------------
# Economic Commission for Latin America and the Caribbean # Statistics > Demographic and Social
social_spending <- read_xlsx("raw_data/cepal_social_spending_pcp_constant.xlsx", sheet = 1, col_types = c(
  "text", "text", "text", "text", "numeric", "numeric", "text", "text", "numeric")
) %>%
  select(c(-1, -2, -7, -8, -9)) %>%
  pivot_wider(names_from = 2,
              values_from = value,
              values_fill = NA) %>%
  rename(country = 1, year = 2, cult_pcp = 3, edu_pcp = 4, all_spending = 5, protection_pcp = 6, health_pcp = 7, house_pcp = 8, envir_pcp = 9) %>%
  rowwise() %>% 
  mutate(soc_pcp = sum(edu_pcp, protection_pcp, health_pcp, na.rm = T)) %>% 
  dplyr::filter(country != "Caribbean")

# Warnings are referred to coercion and should be ignored

social_spending$country[social_spending$country == "Bolivia (Plurinational State of)"] <- 'Bolivia'

social_spending %>% 
  count(country, year) %>% 
  filter(n > 1)

## 1.2 Public Spending by Function -----------------------------------
# Public spending by function
public_spending <- read_xlsx("raw_data/cepal_public_spending.xlsx") %>%
  select(c(-1, -2, -7, -8, -9)) %>%
  pivot_wider(names_from = 2,
              values_from = value,
              values_fill = NA) %>%
  rename(country = 1, year = 2, total = 3, def = 4, 
         health = 5, edu = 6, sick = 7, old = 8, survivor = 9, family = 10,
         unemp_benefit = 11, housing = 12, exclusion = 13, rd = 14, 
         protecion_nec = 15) %>%
  rowwise() %>% 
  mutate(def_p = def * 100 / total,
         welfare_p = (sum(health, edu, sick, old, survivor, family,
                          unemp_benefit, housing, exclusion, protecion_nec, 
                          na.rm = T)) 
         * 100 / total) %>%
  arrange(country, year) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name",
                             destination = "iso3c"),
         year = as.double(year)) %>%
  relocate(13, 1, 2, 3:12)

# The only data available to Peru is General Government

public_spending %>% 
  count(country, year) %>% 
  filter(n > 1)

# 2. NON-CONTRIBUTORY POLICIES -------------------------------------------
# Economic Commission for Latin America and the Caribbean
# Non-contributory Social Protection Programmes Database

## 2.1 Conditional Cash Transfer --------------------------------------
ncp_cct <- "https://dds.cepal.org/bpsnc/cct" %>% 
  xml2::read_html() %>% 
  rvest::html_table() %>%
  bind_rows() %>%
  select(-3) %>%
  rename(programme = 1, country = 2) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         ptype = "cct")

ncp_cct$year <- substr(x = gsub(pattern = '\\D', "", ncp_cct$programme), 1, 4) # '\\D' removes non-numerical characters

ncp_cct$year[ncp_cct$programme == "Aid Brazil Programme"] <- 2021

ncp_cct$year[ncp_cct$programme == "Bono Vida Mejor (ex Bono 10.000 Education, health and nutrition) (2010-)"] <- 2010

ncp_cct <- ncp_cct %>% 
  mutate(year = as.numeric(year)) %>% 
  relocate(2, 3, 5, 4, 1)

## 2.2 Non-contributory pensions -------------------------------------
ncp_sp <- "https://dds.cepal.org/bpsnc/sp" %>% 
  xml2::read_html() %>% 
  rvest::html_table() %>%
  bind_rows() %>%
  select(-3) %>%
  rename(programme = 1, country = 2) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         ptype = "sp")

ncp_sp$year <- substr(x = gsub(pattern = '\\D', "", ncp_sp$programme), 1, 4)

ncp_sp$year[ncp_sp$programme == "Pension for Older People (Pensión para Adultos Mayores) (ex \"70 and over\" programme)"] <- 2007

ncp_sp$year[ncp_sp$programme == "Programme of Food Support for Adults over 68 years old living in Mexico City (2001-)"] <- 2001

ncp_sp$year[ncp_sp$programme == "120 a los 65: Programa Especial de Transferencia Económica a los Adultos Mayores (2009-) (120 to 65: Special programme of economic assistance for the elderly) (2009-)"] <- 2009

ncp_sp$year[ncp_sp$programme == "National Solidarity Assistance Programme “Pension 65“ (2011-)"] <- 2011

ncp_sp$year[ncp_sp$programme == "Programme of Food Support for Adults over 68 years old living in Mexico City (2001-)"] <- 2001

ncp_sp$year[ncp_sp$programme == "Pension for Older People (Pensión para Adultos Mayores) (ex \"70 and over\" programme)"] <- 2007

ncp_sp <- ncp_sp %>% 
  mutate(year = as.numeric(year)) %>% 
  relocate(2, 3, 5, 4, 1)

## 2.3 Labour inclusion programmes ----------------------------------
ncp_lpi <- "https://dds.cepal.org/bpsnc/lpi" %>% 
  xml2::read_html() %>% 
  rvest::html_table() %>%
  bind_rows() %>%
  select(-3) %>%
  rename(programme = 1, country = 2) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         ptype = "lpi")

ncp_lpi$year <- substr(x = gsub(pattern = '\\D', "", ncp_lpi$programme), 1, 4)

ncp_lpi$year[ncp_lpi$programme == "Programme 4 to 7 (2011 - )"] <- 2011

# One does not know IMAS Training's launch year

ncp_lpi <- ncp_lpi %>%
  mutate(year = as.numeric(year)) %>% 
  relocate(2, 3, 5, 4, 1)

ncp_all <- bind_rows(ncp_cct, ncp_lpi, ncp_sp)

## 2.4 Programme Names Backup ---------------------------------------
write_excel_csv2(ncp_all %>% 
                   filter(year %in% c(1900:2024)),
                 "final_data/ncp_programmes.csv", na = '')

# 3. INSTITUTIONAL COVARIATES-----------------------------------------
## 3.1 Database of Political Institutions ----------------------------
dpi <- read.csv2("raw_data/dpi.csv", sep = ",") %>%
  rename(country = countryname) %>%
  mutate(year = as.double(year),
         maj = as.numeric(maj)) %>%
  # There are no iso3c for the following countries
  dplyr::filter(!(country %in% c("GDR", "Yemen People\'s Republic",
                                 "Yugoslavia"))) %>%
  mutate(country = ifelse(country == 'Cent. Af. Rep.', 'Central African Republic', country),
         country = ifelse(country == 'Dom. Rep.', 'Dominican Republic', country),
         country = ifelse(country == 'PRC', "People's Republic of China", country),
         country = ifelse(country == 'PRK', "Korea (the Democratic People's Republic of)", country),
         country = ifelse(country == 'ROK', 'Republic of Korea', 
                          country),
         country = ifelse(country == 'S. Africa', 'South Africa', country),
  )%>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

# setdiff(unique(social_spending$country), unique(dpi$country))

## 3.2 Leaders Global  ------------------------------------------------
# Dupont, Nils; Doring, Holger; Bederke, Paul. (2021). "Leaders Global: Party affiliations of leaders (HoS/HoG) in 183 countries, 1880–2020"
leadglob <- read.csv2("raw_data/leadglob.csv", sep = ",") %>%
  mutate(year = as.double(year)) %>%
  # There are no iso3c for the following countries below
  dplyr::filter(!(country %in% c('German Democratic Republic', 'Kosovo',
                                 'Somaliland',
                                 'German Democratic Republic',
                                 "South Yemen",
                                 "Zanzibar",
                                 "Vietnam, South",
                                 "Czechoslovakia"))) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

leadglob %>% 
  count(country, year) %>% 
  filter(n > 1) # Panama is duplicated. The authors have wrongly inserted two values for Panama's "vdem_code", which is one of the dataset's primary keys. Thus, Panama correctly assumes the value 92, but also incorrectly assumes the value 73, which is actually Costa Rica's identifier.

leadglob <- leadglob %>% 
  filter()

leadglob <- leadglob[!duplicated(leadglob[c("country","year")]),] # Removing duplicates

# setdiff(unique(social_spending$country), unique(leadglob$country))

## 3.3 V-Party ------------------------------------------------------------
# Staffan I. Lindberg et al. (2022) “Codebook Varieties of Party Identity and Organization (V–Party) V2”
vparty <- readRDS("raw_data/v_party.rds") %>%
  rename(country = country_name) %>%
  dplyr::filter(country %in% unique(social_spending$country)) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

## 3.4 Government Effectiveness --------------------------------
gov_eff <- wb_etl(y = 'GE.EST', w = 1980, z = 2024) %>% 
  rename(gov_eff = 5)

## 3.5 Rule of Law ---------------------------------------------
rulelaw <- wb_etl(y = 'RL.EST', w = 1980, z = 2024) %>% 
  rename(rulelaw = 5)

## 3.6 State Capacity ------------------------------------------
## Source: https://public.websites.umich.edu/~jkhanson/state_capacity.html
state_cap <- read_dta('raw_data/state_capacity_v1.dta') %>% 
  filter(!(country %in% c("Serbia-Montenegro", "Vietnam, South",
                          "Czechoslovakia", "Kosovo"))) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
  rename(state_cap = Capacity, trade_tax = tax_trade_tax) %>% 
  select(country, iso3c, year, state_cap, trade_tax) %>% 
  filter(!(country %in% c("German Democratic Republic", "Yemen People\'s Republic", "Yugoslavia")))

# 4. POLITICAL ECONOMY -------------------------------------------------
## 4.1 Terms of trade --------------------------------------------------
### 4.1.1 Gruss & Kebhaj (2019) ----------------------------------------
terms_of_trade <- read.csv2("raw_data/terms_of_trade_net_exports.csv", sep = ",", dec = ".") %>%
  select(1, 3, 5, 10) %>% # Commodity Net Export Price Index, Individual Commodities Weighted by Ratio of Net Exports to GDP (xm_gdp)
  dplyr::filter(Type.Name == "Historical, Rolling Weights, Index") %>%
  rename(country = 1, year = 3, commtot = 4) %>%
  select(-Type.Name) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

# setdiff(unique(social_spending$country), unique(terms_of_trade$country))

### 4.1.2 UNCTAD (2024) -----------------------------------------------
## Deflator
euv_idx <- read.csv2("raw_data/worldbank_export_value_index.csv", 
                     sep = ',', na.strings = "..", dec = ".") %>% 
  slice(1:798)

names(euv_idx) <- substr(names(euv_idx), 2, 5)

euv_idx <- euv_idx %>% 
  rename(variable = 1,
         varcode =2,
         country = 3,
         iso3c = 4) %>% 
  pivot_longer(cols = 5:67,
               names_to = 'year', 
               values_to = 'value') %>% 
  pivot_wider(id_cols = c("country", "iso3c", "year"), names_from = 1,
              values_from = 'value') %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         year = as.numeric(year)) %>% 
  rename(unitvalue_idx = 4, volume_idx = 5, value_idx = 6)

## Commodity Trade Revenue
cmd_trade <- read.csv2("raw_data/unctad_commodities_and_stones.csv", sep = ",") %>% 
  rename(primary_commodities = 3) %>% 
  select(-4) %>% 
  left_join(read.csv2
            ("raw_data/unctad_other_ores_and_metals.csv", sep = ",") %>% 
              rename(ores_metals = 3) %>% select(-4)) %>% 
  left_join(read.csv2
            ("raw_data/unctad_iron_steel.csv", sep = ",") %>% 
              rename(iron_steel = 3) %>% select(-4)) %>% 
  left_join(read.csv2
            ("raw_data/unctad_commodities_and_stones_per.csv", sep = ",") %>% 
              rename(primary_commodities_per = 3) %>% 
              mutate(primary_commodities_per = 
                       as.numeric(primary_commodities_per)) %>% 
              select(-4)) %>% 
  left_join(read.csv2
            ("raw_data/unctad_other_ores_and_metals_per.csv", sep = ",") %>% 
              rename(ores_metals_per = 3) %>% 
              mutate(ores_metals_per = 
                       as.numeric(ores_metals_per)) %>% 
              select(-4)) %>% 
  left_join(read.csv2
            ("raw_data/unctad_iron_steel_per.csv", sep = ",") %>% 
              rename(iron_steel_per = 3) %>% 
              mutate(iron_steel_per = 
                       as.numeric(iron_steel_per)) %>% 
              select(-4)) %>% 
  left_join(read.csv2
            ("raw_data/unctad_population.csv", sep = ",") %>% 
              rename(population = 3) %>% 
              mutate(population = population)) %>% 
  mutate(all_commodities = 
           (primary_commodities + ores_metals + iron_steel)/population,
         all_commodities_per = primary_commodities_per + 
           ores_metals_per + iron_steel_per,
         Economy_Label = countryname(Economy_Label),
         iso3c = countrycode(Economy_Label, origin = "country.name", destination = "iso3c"),
         Year = as.numeric(Year))

cmd_trade2 <- cmd_trade %>% 
  drop_na(iso3c) %>% 
  left_join(euv_idx %>% 
              select(iso3c, year, unitvalue_idx), 
            join_by(Year == year, iso3c)) %>% 
  mutate(all_commodities_true = all_commodities/unitvalue_idx)


cmd_trade2 %>% 
  filter(Economy_Label == "Brazil") %>% 
  ggplot(aes(x=Year, y=all_commodities_true)) + 
  xlab('Ano') + ylab('Exportações') +
  theme_classic() +
  geom_line(linetype = 'dashed') +
  geom_point()

cmd_trade2 %>% 
  filter(Economy_Label == "Venezuela") %>% 
  ggplot(aes(x=Year, y=all_commodities_per)) + 
  xlab('Ano') + ylab('Exportações') +
  theme_classic() +
  geom_line(linetype = 'dashed') +
  geom_point()


## 4.2 World Economic Outlook ---------------------------------------------
weo <- read.csv2("raw_data/weo_data.csv", na = c("n/a", "", "--"), dec = ".") %>% 
  slice(1:2170)

names(weo) <- substr(names(weo), 2, 5)

weo <- weo %>%  
  pivot_longer(cols = c(6:48))

weo$value <- gsub(",", "", weo$value)

weo <- weo %>%
  rename(country = 2, iso3c = 1, year = "name") %>% 
  mutate(value = as.numeric(value),
         year = as.numeric(year)) %>% 
  unite(variable, 3:4, sep = "_", remove = TRUE, na.rm = FALSE) %>%
  pivot_wider(id_cols = c("country", "iso3c", "year"), names_from = "variable",
              values_from = 'value') %>% 
  rename(gdp_nc = 4, # GDP, Constant Prices, National Currency
         gdp_g = 5, # GDP, Constant prices, Percent Change
         gdp_pcp_nc = 6, # GDP Per Capita, Constant prices, National Currency
         gdp_pcp_ppp = 7, # GDP Per Capita, PPP, International Dollar
         inf_avg_idx = 8, # Inflation, Average Prices Index
         inf_avg_g = 9, # Inflation, Average Prices Percent Change
         inf_eop_idx = 10, # Inflation, End of Period, Index
         inf_eop_g = 11, # Inflation, End of Period, Percent Change
         unemp = 12, # Unemployment Rate % Total Labor Force
         population = 13,
         gov_rev_gdp = 14, # General Government Revenue % GDP
         gov_x = 15, # General Government Total Expenditure % GDP
         ggov_debt = 16, # General Government Net Deby % GDP
         acc_balance = 17 # Current Account Balance % GDP
  )

# IMF WEO Dataset differs from UNU Dataset on government revenue, this is
# possibly due to the inclusion of social grants by the latter

# It must be noted that General Government Debt and Central Government
# Debt are different indicators.

## 4.3 Government Revenue Dataset ----------------------------------------
gov_revenue <- read_dta('raw_data/unu_gov_revenue.dta') %>%
  filter(country != 'Kosovo', iso!= 'LIE') %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),       
         year = as.double(year)) %>% 
  select(country, year, iso3c, tax_inc_sc, nrtax_inc_sc)
         
## 4.4 Central Government Debt -------------------------------------------
cgov_debt <- read_xls("raw_data/imf_central_debt.xls", sheet = 1, na = 'no data') %>%
  slice(-c(1, 176:177)) %>%
  pivot_longer(cols = c(2:73)) %>%
  rename(country = 1, year = 2, cgov_debt = 3) %>%
  dplyr::filter(country != "Kosovo") %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         year = as.double(year)) %>%
  relocate(1, 4, 2, 3)

## 4.6 Economic Openness and Globalization ---------------------------
### 4.6.1 Economic Openness ------------------------------------------
ecopen <- wb_etl(y = 'NE.TRD.GNFS.ZS', w = 1980, z = 2024) %>% 
  rename(ecopen = 5)

### 4.6.2 Globalization ----------------------------------------------
globalization <- read_dta('raw_data/kof_globalization.dta') %>% 
  filter(!country %in% c("East Asia and Pacific", "Europe and Central Asia", "High income", "Latin America and Caribbean", "Low income", "Lower middle income", "Middle East and North Africa", "North America", "South Asia", "Sub-Saharan Africa", "Upper middle income", "World")) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
  rename(kof_trade_df = KOFTrGIdf, # Trade globalization idx de facto
         kof_trade_dj = KOFTrGIdj, # Trade globalization idx de jure
         kof_finance_df = KOFFiGIdf, # Financial globaliztion idx de facto
         kof_finance_dj = KOFFiGIdj, # Financial globaliztion idx de jure
         kof_personal_df = KOFIpGIdf, # Interpersonal glob. idx de facto
         kof_personal_dj = KOFIpGIdj, # Interpersonal glob. idx de jure
         kof_info_df = KOFInGIdf, # Informational globalization idx de facto
         kof_info_dj = KOFInGIdj, # Informational globalization idx de jure
         kof_pol_df = KOFPoGIdf, # Political globalization idx de facto
         kof_pol_dj = KOFPoGIdj, # Political globalization idx de jure
         kof_idx = KOFGI, # Globalization Index
         kof_idx_df = KOFGIdf, # Globalization Index de facto
         kof_idx_dj = KOFGIdj) %>% # Globalization Index de jure
  select(-c(KOFCuGIdf, KOFCuGIdj, KOFEcGI, KOFEcGIdf, KOFEcGIdj,
            KOFTrGI, KOFFiGI, KOFSoGI, KOFSoGIdf, KOFSoGIdj,
            KOFIpGI, KOFCuGI, KOFPoGI,code))
         
## 4.7 Freedom Index and State Ownership --------------------------------
fraser_freedom <- read_xlsx("raw_data/fraser_freedom.xlsx",
                            sheet = 1, skip = 4, 
                            .name_repair = "unique_quiet") %>% 
  rename(year = Year, freedom_idx = 5, state_own = 10) %>% 
  mutate(country = countryname(Countries),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
  select(c(country, year, freedom_idx, state_own, iso3c))

## 4.10 Urban population ------------------------------------------------
urban_pop <- wb_etl(y = 'SP.URB.TOTL.IN.ZS', w = 1980, z = 2024) %>% 
  rename(urban_pop = 5)

## 4.11 Natural Resources Depletion -------------------------------------
res_depletion <- wb_etl(y = 'NY.ADJ.DRES.GN.ZS', w = 1980, z = 2024) %>% 
  rename(res_depletion = 5)

# 5. SOCIETAL DYNAMICS -------------------------------------------------
## 5.1 Net Migration ---------------------------------------------------
net_migration <- wb_etl(y = 'SM.POP.NETM', w = 1980, z = 2024) %>% 
  rename(net_migration = 5)

## 5.2 Conflict -------------------------------------------------------
warfare <- read_xlsx("raw_data/csp_political_violence.xlsx") %>% 
  filter(!(country %in% c("Czechoslovakia", "Kosovo", "Yugoslavia",
                          "Serbia and Montenegro"))) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
    filter(!(country %in% c("Yemen Arab Republic", "Yemen People's Republic",
                            "German Democratic Republic", 
                            "Republic of Vietnam")))

## 5.3 Settler Mortality  ------------------------------------------------
settler <- read.csv2("raw_data/qogdata_settler_mortality.csv") %>% 
  mutate(country = countryname(cname),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))
  
## 5.4 Dependency Ratio ---------------------------------------------------
  dp_ratio <- wb_etl(y = 'SP.POP.DPND', w = 1980, z = 2024) %>% 
    rename(dp_ratio = 5)

# 6. DATA ENRICHMENT ---------------------------------------------------
## 6.1 Template --------------------------------------------------------
# We create a template data frame with all Latin American Countries ranging from 1980 to 2021 and without any additional variable. 
# This is important since every dataset covers a different set of countries and years.
# In the data enrichment phase, all variables will be attached to this object
tmp_a <- data.frame(year=rep(seq(1980,
                                 max(social_spending$year)),
                             each=length(unique(social_spending$country))),
                    country=unique(social_spending$country))

tmp_b <- data.frame(year=rep(seq(1980,
                                 max(social_spending$year)),
                             each=3),
                    country= c('Venezuela', 'Puerto Rico', 'Peru'))

final_tmp <- merge(tmp_a, tmp_b, by=c('year','country'),all=T) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
  relocate(2, 3, 1)

final_tmp <- final_tmp[order(final_tmp$country,
                final_tmp$year),]

rm(tmp_a, tmp_b)

## 6.2 Easy bind -----------------------------------------------------
db_socialx_pcp <- final_tmp %>%
# Dependent Variables
  left_join(social_spending) %>% 
  left_join(public_spending %>% select(country, year, iso3c, 
                                       def_p, welfare_p)) %>% 

# Institutional Covariates
  left_join(dpi %>% select(country, iso3c, year, system, maj)) %>% 
  left_join(gov_eff %>% select(country, year, iso3c, 
                               gov_eff)) %>% 
  left_join(rulelaw %>% select(country, year, iso3c, 
                               rulelaw)) %>% 
  left_join(state_cap %>% select(country, year, iso3c, 
                                 state_cap, trade_tax)) %>% 

# Political Economy
  left_join(terms_of_trade) %>% 
  left_join(cmd_index) %>% 
  left_join(weo %>% select(country, year, iso3c,
                           gdp_g, gdp_pcp_nc, gdp_pcp_ppp, inf_eop_g,
                           unemp, population, acc_balance)) %>%
  left_join(gov_revenue) %>% 
  left_join(cgov_debt) %>% 
  left_join(ecopen) %>% 
  left_join(globalization) %>% 
  left_join(fraser_freedom) %>% 
  left_join(res_depletion %>% select(country, year, iso3c, 
                                     res_depletion)) %>%
  left_join(urban_pop %>% select(country, year, iso3c, urban_pop)) %>% 

# Societal Dynamics
  left_join(net_migration %>% select(country, year, iso3c, 
                                     net_migration)) %>% 
  left_join(settler %>% select(country, year, iso3c, 
                               ajr_settmort)) %>% 
  left_join(dp_ratio %>% select(country, year, iso3c, 
                               dp_ratio)) %>% 
  left_join(warfare %>% select(country, year, iso3c, 
                               civviol, # Civil violence
                               civwar, # Civil war
                               ethviol, # Ethnic violence
                               ethwar, # Ethnic war
                               civtot))

## 6.3 Hard Bind -----------------------------------------------------------
### 6.3.1 Non-Contributory Policies ----------------------------------------
db_socialx_pcp <- db_socialx_pcp %>% 
  left_join(ncp_all %>% group_by(country, year, ptype) %>%
              dplyr::summarise(ncp_count=n()
              ) %>% 
              ungroup() %>% 
              pivot_wider(names_from = ptype, values_from = ncp_count)) %>% 
  rename(n_cct = cct, n_sp = sp, n_lpi = lpi, n_ncp = ncp) %>% 
  mutate(n_cct = coalesce(cct, 0),
         n_sp = coalesce(sp, 0),
         n_lpi = coalesce(lpi, 0),
         n_ncp = cct + sp + lpi)

### 6.3.2 Elections --------------------------------------------------------
# Government party
db_socialx_pcp <- leadglob %>%
  mutate(year = as.double(year)) %>%
  select(iso3c, country, year, HoS_name, HoS_party_short, HoS_party_english, HoS_party_id, HoG_name, HoG_party_short, HoG_party_english, HoG_party_id) %>%
  right_join(db_socialx_pcp) %>%
  arrange(country, iso3c, year)

db_socialx_pcp <- db_socialx_pcp %>%
  mutate(leader = ifelse(system == 'Presidential', HoS_name, NA),
         leader = ifelse(system == 'Assembly-Elected President', HoS_name, leader),
         leader = ifelse(system == 'Parliamentary', HoG_name, leader),
         party_name = ifelse(system == 'Presidential', HoS_party_english, NA),
         party_name = ifelse(system == 'Assembly-Elected President', HoS_party_english, party_name),
         party_name = ifelse(system == 'Parliamentary', HoG_party_english, party_name),
         pf_party_id = ifelse(system == 'Presidential', HoS_party_id, NA),
         pf_party_id = ifelse(system == 'Assembly-Elected President', HoS_party_id, pf_party_id),
         pf_party_id = ifelse(system == 'Parliamentary', HoG_party_id, pf_party_id),
         party_short = ifelse(system == 'Presidential', HoS_party_short, NA),
         party_short = ifelse(system == 'Assembly-Elected President', HoS_party_short, party_short),
         party_short = ifelse(system == 'Parliamentary', HoG_party_short, party_short)) 
  
db_socialx_pcp <- db_socialx_pcp %>% 
  select(-HoS_name, -HoS_party_short, -HoS_party_english, -HoS_party_id, -HoG_name, -HoG_party_short, -HoG_party_english, -HoG_party_id)

### 6.3.3 Party Ideology -----------------------------------------------------
db_socialx_pcp <- db_socialx_pcp %>%
   mutate(year1 = year)

vparty_sel <- vparty %>%
  select(iso3c, country, year, pf_party_id, v2pariglef_ord, v2pawelf_ord, v2paclient_ord, v2pagroup_2, v2pagroup_3, v2palocoff_ord, v2paactcom_ord, v2pasoctie_ord, v2paind_ord) %>%
   mutate(year2 = year)
 
db_socialx_pcp <- db_socialx_pcp %>%
   left_join(vparty_sel,
           by = join_by(country, pf_party_id, closest(year1 >= year2))) %>%
  mutate(region = countrycode(country, origin = "country.name", destination = "region23"),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

db_socialx_pcp$region[db_socialx_pcp$country == "Mexico"] <- 'North America'

db_socialx_pcp <- db_socialx_pcp %>%
  select(-c(iso3c.x, iso3c.y, year.x, iso3c.y, year.y, year2)) %>%
  rename(year = year1)

db_socialx_pcp <- db_socialx_pcp %>%
  relocate(region, iso3c, country, year, system,
           leader, party_name, pf_party_id, party_short,
           soc_pcp, cult_pcp, edu_pcp, sprot_pcp, health_pcp, house_pcp,
           envir_pcp, def_p, welfare_p,
           commtot,  sp, cct, lpi, ncp, maj, v2pariglef_ord, v2pawelf_ord,
           v2paclient_ord, v2pagroup_2, v2pagroup_3, v2paind_ord,
           v2palocoff_ord, v2paactcom_ord, v2pasoctie_ord, v2paind_ord) %>%
  arrange(region, country, year)

# 7. DATASET EXPORT ------------------------------------------------------
write_excel_csv2(db_socialx_pcp, "final_data/db_socialx_pcp.csv", na = '')
saveRDS(db_socialx_pcp, "final_data/db_socialx_pcp.RDS")
write_dta(db_socialx_pcp, "final_data/db_socialx_pcp.dta")

dataset <-readRDS("final_data/db_socialx_pcp.RDS")

# CODEBOOK ----------------------------------------------------------------
# db_socialx_pcp <- readRDS('final_data/db_socialx_pcp.RDS')
# latin <- c("Cuba", "Dominican Republic", "Haiti", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Mexico", "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")
# 
# db_socialx_pcp <- db_socialx_pcp %>%
#   mutate(macroregion = case_when(country %in% latin ~ 'latin',
#                                  country %in% setdiff(unique(db_socialx_pcp$country), 
#                                                       latin) ~ 'non_latin'))
# 
# x <- db_socialx_pcp %>%
#   dplyr::filter(year %in% 1990:2019,
#                 macroregion == 'latin')
# 
# colSums(!is.na(db_socialx_pcp)) %>%
#   View()
# 
# colSums(!is.na(x)) %>%
#   View()

# LEFTOVERS ---------------------------------------------------------------
## The codes below are preserved for future reference.
## Select a desired chunk and press "Ctrl + Shift + C" to undo its hashtags.

## 1.3 Current Social Spending ----------------------------
# socialx_current <- read_xlsx("raw_data/cepal_social_spending_current.xlsx") %>%
#   filter(.[[2]] %in% c('Central government', 'General government'),
#          .[[3]] %in% c('Venezuela (Bolivarian Republic of)', 'Peru'),
#          .[[4]] == 'Social expenditure') %>%
#   select(3:6) %>%
#   pivot_wider(names_from = 2,
#               values_from = value,
#               values_fill = NA) %>%
#   rename(country = 1, year = 2, soc_nom = 3) %>%
#   arrange(country, year) %>%
#   mutate(country = countryname(country),
#          iso3c = countrycode(country, origin = "country.name",
#                              destination = "iso3c"))