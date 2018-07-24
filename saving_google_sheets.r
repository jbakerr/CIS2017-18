gs_auth(new_user = TRUE)


services_sheet <- gs_title("services")

studentlist_sheet <- gs_title("studentlist")

tier1_sheet <- gs_title("tier1")

site_coordination_sheet <- gs_title("site_coordination")





services_sheet <- services_sheet %>% gs_ws_rename(from = 'services', to = 'services_old')

gs_new("studenthjh", ws_title = "students", input = stlist, trim = TRUE, verbose = FALSE)

services_sheet <- services_sheet %>% gs_ws_delete("services_old")


id <- '1Q2c-M8jHu7u5Hwyf-_3BDZ-BO5llfxuC'

drive_download(file = as_id(id), type = 'csv')

drive_download(file, type = "csv")

file <- write.csv(stlist, "studentlist.csv")

drive_upload(stlist, 'studentlist_test', type = 'spreadsheet')




