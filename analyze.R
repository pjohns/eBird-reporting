getData = function(){
  mydata = read.csv("ebird_data.csv", header = TRUE)
  mydata$Date <- as.Date(mydata$Date, format="%m-%d-%Y")
  # & symbol is frequently used in location titles, causes problems printing later on
  mydata$Location = as.character(gsub("\\&", "and",mydata$Location))
  return(mydata)
}

getLocation = function(df, location_name){
  result = df[df$Location==as.character(location_name), ]
  return(result)
}

getDates = function(df, start_date, end_date){
  result = df[df$Date >= start_date & df$Date <= end_date,]
  return(result)
}

convertToDate = function(date_string){
  date = as.Date(date_string, format="%m-%d-%Y")
  return(date)
}

getDatesInRange = function(start_date, end_date){
  all_dates = seq(convertToDate(start_date), convertToDate(end_date), by="days")
  return(all_dates)
}

getUniqueSpecies = function(df){
  result = sort(unique(df$Common.Name))
  return(result)
}

observations = function(location, start_date, end_date, file_name, report_type, name_type, date_type){
  
  mydata = getData()
  
  if (name_type == "generic"){
    mydata = convertNames(mydata)
  }
  
  #get data just from location
  loc = getLocation(mydata, location)

  DATE1 <- convertToDate(start_date)
  DATE2 <- convertToDate(end_date)
  
  all_dates = getDatesInRange(DATE1, DATE2)
  
  loc_date = getDates(loc, DATE1, DATE2)
  
  species_list = getUniqueSpecies(loc_date)

  report = data.frame(as.character(species_list))
  
  if (report_type == "simple"){
    colnames(report) = c("Species")
    return(report)
  }
  
  counts = vector(mode="numeric", length=length(species_list))
  day_counts = vector(mode="numeric", length=length(all_dates))
  
  for (d in 1:length(all_dates)){
    on_date = loc_date[which(as.Date(loc_date$Date, format="%m-%d-%Y")==all_dates[d]),]
    species_ondate = getUniqueSpecies(on_date)

    x = vector(mode="character", length=length(species_list))
    day_count = 0
    
    for (i in 1:length(species_list)){
      if(species_list[i] %in% species_ondate){
        x[i] = "x"
        counts[i] = counts[i]+1
        day_count = day_count+1
      }
    }
    day_counts[d] = day_count
    report[d+1] = x
  }
  
  remove = c(0)
  
  for_avg = day_counts
  
  for_avg = for_avg[! for_avg %in% remove]
  
  avg_species_seen = as.character(round(mean(for_avg),2))
  
  day_counts = as.character(day_counts)
  
  day_counts = replace(day_counts, day_counts=="0", "ND")
  
  days = strptime(as.character(all_dates), format="%Y-%m-%d")
  
  if (date_type == "short"){
    days = format(days, "%d")
  }
  
  cols = c("Species", as.character(days))
  
  colnames(report) = cols
  
  report$frequency = round(counts/(sum(day_counts != "ND")),2)
  
  total_species = length(species_list)
  
  report$Species=as.character(report$Species)
  
  summary = c("Number of Species", day_counts, avg_species_seen)
  
  report = rbind(report, summary)
  
  
  no_data = which(summary == "ND")
  
  removed_dates = report[-no_data] 
  
  if (file_name != "NA"){
    writeCSV(report, file_name)
  }
  
  return(removed_dates)
  
}

writeCSV = function(df, file_name){
  file_path = paste("export_path.csv", file_name,".csv", sep="")
  write.csv(file=file_path, x=df)
}

convertNames = function(df){
  
  df$Common.Name <- as.character(df$Common.Name)
  df$Common.Name[df$Common.Name == "American Crow"] <- "Crow, American"
  df$Common.Name[df$Common.Name == "American Kestrel"] <- "Kestrel, American"
  df$Common.Name[df$Common.Name == "American Robin"] <- "Robin, American"
  df$Common.Name[df$Common.Name == "Black-billed Magpie"] <- "Magpie, Black-billed"
  df$Common.Name[df$Common.Name == "Black-capped Chickadee"] <- "Chickadee, Black-capped"
  df$Common.Name[df$Common.Name == "Broad-tailed Hummingbird"] <- "Hummingbird, Broad-tailed"
  df$Common.Name[df$Common.Name == "Chipping Sparrow"] <- "Sparrow, Chipping"
  df$Common.Name[df$Common.Name == "Common Grackle"] <- "Grackle, Common"
  df$Common.Name[df$Common.Name == "Cooper's Hawk"] <- "Hawk, Coopers"
  df$Common.Name[df$Common.Name == "Dark-eyed Junco"] <- "Junco, Dark-eyed"
  df$Common.Name[df$Common.Name == "Downy Woodpecker"] <- "Woodpecker, Downy"
  df$Common.Name[df$Common.Name == "Eurasian Collared-Dove"] <- "Dove, Collared"
  df$Common.Name[df$Common.Name == "European Starling"] <- "Starling, European"
  df$Common.Name[df$Common.Name == "Great Blue Heron"] <- "Heron, Great Blue"
  df$Common.Name[df$Common.Name == "House Finch"] <- "Finch, House"
  df$Common.Name[df$Common.Name == "House Sparrow"] <- "Sparrow, House"
  df$Common.Name[df$Common.Name == "House Wren"] <- "Wren, House"
  df$Common.Name[df$Common.Name == "Lesser Goldfinch"] <- "Goldfinch, Lesser"
  df$Common.Name[df$Common.Name == "Mountain Chickadee"] <- "Chickadee, Mountain"
  df$Common.Name[df$Common.Name == "Mourning Dove"] <- "Dove, Mourning"
  df$Common.Name[df$Common.Name == "Northern Flicker"] <- "Flicker, Northern"
  df$Common.Name[df$Common.Name == "Rock Pigeon (Feral Pigeon)"] <- "Pigeon, Rock"
  df$Common.Name[df$Common.Name == "Spotted Towhee"] <- "Towhee, Spotted"
  df$Common.Name[df$Common.Name == "Woodhouse's Scrub-Jay"] <- "Jay, Scrub"
  df$Common.Name[df$Common.Name == "Blue Jay"] <- "Jay, Blue"
  df$Common.Name[df$Common.Name == "Yellow-rumped Warbler"] <- "Warbler, Yellow-rumped"
  df$Common.Name[df$Common.Name == "Red-tailed Hawk"] <- "Hawk, Red-tailed"
  df$Common.Name[df$Common.Name == "American Goldfinch"] <- "Goldfinch, American"
  df$Common.Name[df$Common.Name == "Great Horned Owl"] <- "Owl, Great Horned"
  df$Common.Name[df$Common.Name == "White-crowned Sparrow"] <- "Sparrow, White-crowned"
  df$Common.Name[df$Common.Name == "Black-headed Grosbeak"] <- "Grosbeak, Black-headed"
  df$Common.Name[df$Common.Name == "Barn Swallow"] <- "Swallow, Barn"
  df$Common.Name[df$Common.Name == "Tree Swallow"] <- "Swallow, Tree"
  df$Common.Name[df$Common.Name == "Canada Goose"] <- "Goose, Canada"
  df$Common.Name[df$Common.Name == "Western Tanager"] <- "Tanager, Western"
  df$Common.Name[df$Common.Name == "Lazuli Bunting"] <- "Bunting, Lazuli"
  df$Common.Name[df$Common.Name == "Brewer's Blackbird"] <- "Blackbird, Brewer's"
  df$Common.Name[df$Common.Name == "Red-winged Blackbird"] <- "Blackbird, Red-winged"
  df$Common.Name[df$Common.Name == "Brown-headed Cowbird"] <- "Cowbird, Brown-headed"
  df$Common.Name[df$Common.Name == "Yellow-headed Blackbird"] <- "Blackbird, Yellow-headed"
  df$Common.Name[df$Common.Name == "Bullock's Oriole"] <- "Oriole, Bullock's"
  df$Common.Name[df$Common.Name == "Northern Harrier"] <- "Harrier, Northern"
  df$Common.Name[df$Common.Name == "Western Wood-Pewee"] <- "Pewee, Western-Wood-"
  df$Common.Name[df$Common.Name == "White-breasted Nuthatch"] <- "Nuthatch, White-breasted"
  
  df$Common.Name[df$Common.Name == "Goose, Canada"] <- "Goose, Canada*"
  df$Common.Name[df$Common.Name == "gull sp."] <- "gull sp.*"
  df$Common.Name[df$Common.Name == "Hawk, Red-tailed"] <- "Hawk, Red-tailed*"
  df$Common.Name[df$Common.Name == "Heron, Great Blue"] <- "Heron, Great Blue*"
  df$Common.Name[df$Common.Name == "Mallard"] <- "Mallard*"
  df$Common.Name[df$Common.Name == "Swallow, Barn"] <- "Swallow, Barn*"
  df$Common.Name[df$Common.Name == "Swallow, Tree"] <- "Swallow, Tree*"
  
  return(df)
  
}

RollingLog = function(){
  start_date = seq(Sys.Date(), length=1, by="days")-21
  end_date = seq(Sys.Date(), length=1, by="days")+7
  
  output = observations("Backyard", start_date, end_date, "NA", "full", "generic", "short")
  
  return(output)
}

PastMonth = function(){
  start_date = seq(Sys.Date(), length=1, by="days")-28
  end_date = Sys.Date()
  
  output = observations("Backyard", start_date, end_date, "NA", "full", "generic", "short")
  
  return(output)
}

BackyardMonth = function(month){
  
  date.end.month <- seq(as.Date("2017-02-01"),length=12,by="months")-1
  date.start.month = seq(as.Date("2017-01-01"),length=12,by="months")
  months = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  
  month_index = match(month,months)
  
  start = as.character(date.start.month[month_index], format="%m-%d-%Y")
  end = as.character(date.end.month[month_index], format="%m-%d-%Y")
  
  output = observations("Backyard", start, end, month, "full", "generic", "short")
}

WhereDidIGo = function(range, start_date, end_date){
  
  mydata = getData()
  
  if (range == "all"){
    locations = data.frame(sort(unique(mydata$Location)))
    colnames(locations) = c("Locations")
    return(locations)
  }
  
  DATE1 <- as.Date(start_date, format="%m-%d-%Y")
  DATE2 <- as.Date(end_date, format="%m-%d-%Y")
  
  in_range = mydata[mydata$Date >= DATE1 & mydata$Date <= DATE2,]
  
  locations = data.frame(sort(unique(in_range$Location)))
  colnames(locations) = c("Locations")
  return(locations)
  
}

WhenDidIGoTo = function(location){
  mydata = getData()
  location = mydata[mydata$Location==as.character(location), ]
  dates = data.frame(sort(unique(location$Date)))
  colnames(dates) = c("Dates")
  return(dates)
}

OnDate = function(date){
  on_date = data.frame(mydata[mydata$Date==as.Date(date, format="%m-%d-%Y"), c("Common.Name", "Location")])
  rownames(on_date) = 1:nrow(on_date)
  View(on_date)
  return(on_date)
  
}

WhereDidISee = function(bird){
  locations = data.frame(sort(unique(mydata[mydata$Common.Name==bird, "Location"])))
  colnames(locations) = "Locations"
  return(locations)
}

WhenDidISee = function(bird){
  dates = data.frame(sort(unique(mydata[mydata$Common.Name==bird, "Date"])))
  colnames(dates) = "Dates"
  View(dates)
  return(dates)
}

WhenandWhere = function(bird){
  sightings = data.frame(mydata[mydata$Common.Name==bird, c("Date", "Location", "State.Province", "County")])
  sightings = data.frame(sightings[order(sightings$Date), ])
  rownames(sightings) = 1:nrow(sightings)
  View(sightings)
  return(sightings)
}

LifeList = function(location){
  mydata = getData()
  mydata = convertNames(mydata)
  if(missing(location)) {
    lifelist = data.frame(sort(unique(mydata$Common.Name)))
  } else {
    my_loc = mydata[mydata$Location==location,]
    lifelist = data.frame(sort(unique(my_loc$Common.Name)))
  }
  
  colnames(lifelist) = "Common Name"
  return(lifelist)
}

createLocationSummary = function(location){
  start_date = "01-01-2016"
  end_date = Sys.Date()
  full_report = observations(location, start_date, end_date, "NA", "full", "common", "full")
  
  #day_counts = full_report[nrow(full_report),]
  #no_data = which(day_counts %in% "ND")
  #short_report = subset(full_report, select = -no_data)
  
  return(full_report)
}

mydata = getData()