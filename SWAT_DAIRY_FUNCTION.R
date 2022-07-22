SWAT_Dairy=function(user_defined_fertilizer){
  if (user_defined_fertilizer=="TRUE"){
    download.file("https://raw.githubusercontent.com/Rojakaveh/SWAT_DAIRY/main/user_SWAT_dairy.R","user_SWAT_dairy.R")
    file.edit("user_SWAT_dairy.R")
  } else if (user_defined_fertilizer=="FALSE"){
    download.file("https://raw.githubusercontent.com/Rojakaveh/SWAT_DAIRY/main/Auto_SWAT_dairy.R","Auto_SWAT_dairy.R")
    file.edit("Auto_SWAT_dairy.R")
  }
}
