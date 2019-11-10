library(dplyr)
library(readr)

path <- file.path("C:","Users","Josh","Dropbox","Projects","costreports","misc")
setwd(path)

taxonomy <- rbind(data.frame(class=1:879, type="Short-term (General and Specialty) Hospitals"),
                  data.frame(class=880:899, type="Hospitals participating in ORD demonstration project"),
                  data.frame(class=900:999, type="Multiple Hospital Component in a Medical Complex"),
                  data.frame(class=1000:1199, type="Federally Qualified Health Centers"),
                  data.frame(class=1200:1224, type="Alcohol/Drug Hospitals"),
                  data.frame(class=1225:1299, type="Medical Assistance Facilities"),
                  data.frame(class=1300:1399, type="Critical Access Hospitals"),
                  data.frame(class=1400:1499, type="Community Mental Health Centers"),
                  data.frame(class=4600:4799, type="Community Mental Health Centers"),
                  data.frame(class=4900:4999, type="Community Mental Health Centers"),
                  data.frame(class=1500:1799, type="Hospices"),
                  data.frame(class=1800:1989, type="Federally Qualified Health Centers"),
                  data.frame(class=1990:1999, type="Religious Non-medical Health Care Institutions-formerly Christian Science Sanatoria"),
                  data.frame(class=2000:2299, type="Long-Term Hospitals (Excluded from PPS)"),
                  data.frame(class=2300:2499, type="Hospital Based Renal Dialysis Facilities"),
                  data.frame(class=2500:2899, type="Independent Renal Dialysis Facilities"),
                  data.frame(class=2900:2999, type="Independent Special Purpose Renal Dialysis Facility"),
                  data.frame(class=3000:3024, type="Formerly Tuberculosis Hospitals"),
                  data.frame(class=3025:3099, type="Rehabilitation Hospitals (Excluded from PPS)"),
                  data.frame(class=3100:3199, type="Home Health Agencies"),
                  data.frame(class=9000:9799, type="Home Health Agencies"),
                  data.frame(class=7000:8499, type="Home Health Agencies"),
                  data.frame(class=3200:3299, type="Comprehensive Outpatient Rehabilitation Facilities"),
                  data.frame(class=4800:4899, type="Comprehensive Outpatient Rehabilitation Facilities"),
                  data.frame(class=4500:4599, type="Comprehensive Outpatient Rehabilitation Facilities"),
                  data.frame(class=3300:3399, type="Children's Hospitals (Excluded from PPS)"),
                  data.frame(class=3400:3499, type="Rural Health Clinics"),
                  data.frame(class=3975:3999, type="Rural Health Clinics"),
                  data.frame(class=8500:8899, type="Rural Health Clinics"),
                  data.frame(class=3500:3699, type="Hospital Based Satellite Renal Dialysis Facilities"),
                  data.frame(class=3700:3799, type="Hospital Based Special Purpose Renal Dialysis Facility"),
                  data.frame(class=3800:3974, type="Rural Health Clinics (Free-Standing)"),
                  data.frame(class=8900:8999, type="Rural Health Clinics (Free-Standing)"),
                  data.frame(class=4000:4499, type="Psychiatric Hospitals (Excluded from PPS)"),
                  data.frame(class=5000:6499, type="Skilled Nursing Facilities (See S1060.D.)"),
                  data.frame(class=6500:6989, type="Outpatient Physical Therapy Services"),
                  data.frame(class=6990:6999, type="Numbers Reserved (formerly Christian Science Sanatoria (Skilled Nursing Services)"),
                  data.frame(class=9800:9899, type="Transplant Centers"))

facility_type <- tribble(
  ~ type,
  ~ class,
  "Short Term Acute Care",
  1:899,
  "Childrens",
  3300:3399,
  "Critical Access",
  1300:1399,
  "Long Term",
  2000:2299,
  "Psychiatric",
  4000:4499,
  "Rehabilitation",
  3025:3099
) %>%
  unnest_longer(class) %>% 
  mutate(class = str_pad(as.character(class), width = 4, pad = "0", side = "left"))

write_csv(taxonomy, "hospital-taxonomy.csv")
