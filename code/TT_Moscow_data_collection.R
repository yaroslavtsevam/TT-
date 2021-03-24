source("code/TTcalc_site_7.R")
library(ggpmisc)
library(extrafont)
library(Rmisc)

#loadfonts(device = "win", quiet = TRUE)


installation_start = 1556845000


#OLDTIM = TTcalc_site(c("http://naturetalkers.altervista.org/C1880020/ttcloud.txt"),
#                     1535637102,
#                     import_folder_name=NULL,
#                     first_imported_dates_reconstructed = F,
#                     "timold_desc.csv")
#export_site_to_excel(OLDTIM,sitename="TIMACAD_old",
#                     insert_file="TIMOLD_descr.xlsx")
  

RUDNdata=TTcalc_site(c("http://naturetalkers.altervista.org/C18A0031/ttcloud.txt",
                       "http://naturetalkers.altervista.org/C18A0025/ttcloud.txt"),
                     installation_start,
                     import_folder_name = "data/backup/RUDN",
                     first_imported_dates_reconstructed = F,
                     "data/TT_desc_20.csv",
                     "RUDN",
                     verbose = "file")

TIMdata=TTcalc_site(c("http://naturetalkers.altervista.org/C18A0029/ttcloud.txt",
                      "http://naturetalkers.altervista.org/C18A0030/ttcloud.txt"),
                    installation_start,
                    import_folder_name = "data/backup/TIMR",
                    first_imported_dates_reconstructed = T,
                    "data/TT_desc_20.csv",
                    "TIMIRYAZEV",
                    verbose = "file")


BLTNdata=TTcalc_site("http://naturetalkers.altervista.org/C18A0024/ttcloud.txt",
                     installation_start,
                     import_folder_name = NULL,
                     first_imported_dates_reconstructed = F,
                     "data/TT_desc_20.csv",
                     "BOLOTNAYA",
                     verbose="file")

SCHLdata=TTcalc_site("http://naturetalkers.altervista.org/C18A0023/ttcloud.txt",
                     installation_start,
                     import_folder_name = NULL,
                     first_imported_dates_reconstructed = F,
                     "data/TT_desc_20.csv",
                     "SCHOOL1234",
                     verbose = "file")

SHERdata=TTcalc_site("http://naturetalkers.altervista.org/C1870015/ttcloud.txt",
                     installation_start,
                     import_folder_name = NULL,
                     first_imported_dates_reconstructed = F,
                     "data/TT_desc_20.csv",
                     "SCHERBINKA",
                     verbose = "file")

GRDNdata  = TTcalc_site("http://naturetalkers.altervista.org/C18A0026/ttcloud.txt",
                        installation_start,
                        import_folder_name = NULL,
                        first_imported_dates_reconstructed = F,
                        "data/TT_desc_20.csv",
                        "GARDEN",
                        verbose = "file")


TRSKdata = TTcalc_site("http://naturetalkers.altervista.org/C18A0027/ttcloud.txt",
                       installation_start,
                       import_folder_name = NULL,
                       first_imported_dates_reconstructed = F,
                       "data/TT_desc_20.csv",
                       "TROITSK",
                       verbose = "file")

GBSSdata = TTcalc_site("http://naturetalkers.altervista.org/C18A0028/ttcloud.txt",
                       installation_start,
                       import_folder_name = NULL,
                       first_imported_dates_reconstructed = F,
                       "data/TT_desc_20.csv",
                       "GBS_STAB",
                       verbose = "con")

GBSMdata = TTcalc_site("http://naturetalkers.altervista.org/C18A0229/ttcloud.txt",
                       installation_start,
                       import_folder_name = NULL,
                       first_imported_dates_reconstructed = F,
                       "data/TT_desc_20.csv",
                       "GBS_MOIST",
                       verbose = "file")

ELTSdata = TTcalc_site(c("http://naturetalkers.altervista.org/C18A0230/ttcloud.txt",
                       "http://naturetalkers.altervista.org/C1850003/ttcloud.txt"),
                       installation_start,
                       import_folder_name = NULL,
                       first_imported_dates_reconstructed = F,
                       "data/TT_desc_20.csv",
                       "ELETS",
                       verbose = "file")
GPdata    = TTcalc_site("http://naturetalkers.altervista.org/C1880221/ttcloud.txt",
                        installation_start,
                        import_folder_name = NULL,
                        first_imported_dates_reconstructed = F,
                        "data/TT_desc_20.csv",
                        "GORKYPARK",
                        verbose = "file")






AllData = rbind(
  RUDNdata[[2]],
  TIMdata[[2]],
  BLTNdata[[2]],
  GRDNdata[[2]],
  TRSKdata[[2]],
  SCHLdata[[2]],
  SHERdata[[2]]
)
