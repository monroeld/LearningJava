
# Read in CharacterCSV, WeaponCSV, ArmorCSV, ShieldCSV
characterCSV <- read.csv("CharacterStats.csv", sep = ",", stringsAsFactors = FALSE)
weaponCSV <- read.csv("WeaponStats.csv", sep = ",", stringsAsFactors = FALSE)
armorCSV <- read.csv("ArmorStats.csv", sep = ",", stringsAsFactors = FALSE) # Waqaya stats unknown, so averaged all other FO
shieldCSV <- read.csv("ShieldStats.csv", sep = ",", stringsAsFactors = FALSE) # Toggle shields treated as both.  Note for ironthorn


charLevel <- as.numeric(characterCSV[1])

getWebsiteData <- function(monsterName) {
  # Read encyclopedia homepage's data
  encyclopedia <-readLines("http://forums2.battleon.com/f/tm.asp?m=20966452")
  
  # This is the post in which it shows up
  monsterLine <- grep(monsterName, encyclopedia)
  
  # Split the post into lines by url and get the index
  monsterPost <- c(unlist(strsplit(encyclopedia[monsterLine], "href")))
  monsterEntry <- monsterPost[grep(monsterName, monsterPost)]
  
  # Split that index's line recursively by http and the end tag
  urlList <- c(unlist(strsplit(c(unlist(strsplit(monsterEntry, "http"))), ">")))
  
  # Add the final touch and prune the ending "
  finalURL <- paste0("http", urlList[2])
  finalURL <- strtrim(finalURL, (nchar(finalURL)-1))
  
  # Pull from the retrieved URL
  monsterPage <- c(unlist(read.csv2(finalURL, stringsAsFactors = FALSE)))
  names(monsterPage) <- NULL
  
  # Prune to relevant subset
  website <- monsterPage[c((grep("COMBAT DEFENCE", monsterPage)-5):(grep("COMBAT DEFENCE", monsterPage)+20))]
  websiteData <- website[grep(monsterName, website)[1]:length(website)]
  return(websiteData)
}


getResists <- function(monsterDF) {
  websiteData <- monsterDF
  # Calculate expected monster level based on character level
  expLevel <- charLevel
  monsterLevel <- strsplit(websiteData[grep("Level", websiteData)], "Level")[[1]][2]
  monsterLevel <- c(as.numeric(unlist(strsplit(monsterLevel, "\t"))))
  monsterLevel <- monsterLevel[!is.na(monsterLevel)]
  expLevel <- monsterLevel[(monsterLevel)<charLevel]
  expLevel <- expLevel[length(expLevel)]
  levelIndex <- grep(expLevel, monsterLevel)
  
  monsterElement <- strsplit(strsplit(websiteData[1], "Element")[[1]][1], ">")[[1]]
  monsterElement <- trimws(monsterElement[length(monsterElement)])
  
  # Define resistances
  resists <- c("Fire", "Water", "Wind", "Ice", "Earth", "Energy", "Light", "Dark")
  resistEnd <- ifelse(monsterElement != "Dark", grep("Dark", websiteData), grep("Fire", websiteData)+7)
  resistStart <- ifelse(monsterElement != "Fire", grep("Fire", websiteData), grep("Dark", websiteData)-7)
  resistDF <- websiteData[resistStart:resistEnd]
  
  # Pull out a table of values
  resistDF[length(resistDF)] <- strsplit(resistDF[length(resistDF)], "<")[[1]][1]
  monsterResists <- do.call(cbind, lapply(1:length(resistDF), function(x)
    as.numeric(c(unlist(strsplit(resistDF[x], "\t"))))
  ))
  
  # Remove first row, which is NAs
  monsterResists <- monsterResists[2:nrow(monsterResists), ]
  
  # If it's a matrix, select the correct row.  If not, leave it be
  if (is.matrix(monsterResists) == TRUE) {monsterResists <- monsterResists[levelIndex, ]}
  names(monsterResists) <- resists
  
  return(monsterResists)
}

getBlock <- function(monsterDF) {
  websiteData <- monsterDF
  
  # Calculate expected monster level based on character level
  expLevel <- charLevel
  monsterLevel <- strsplit(websiteData[grep("Level", websiteData)], "Level")[[1]][2]
  monsterLevel <- c(as.numeric(unlist(strsplit(monsterLevel, "\t"))))
  monsterLevel <- monsterLevel[!is.na(monsterLevel)]
  expLevel <- monsterLevel[(monsterLevel)<charLevel]
  expLevel <- expLevel[length(expLevel)]
  levelIndex <- grep(expLevel, monsterLevel)
  
  # Define blocking stats
  blockStats <- c("DEX","LUK")
  statDF <- websiteData[c(grep("DEX", websiteData[1:20]), grep("LUK", websiteData[1:20]))]
  statDF[length(statDF)] <- strsplit(statDF[length(statDF)], "<")[[1]][1]
  monsterStats <- do.call(cbind, lapply(1:length(statDF), function(x)
    as.numeric(c(unlist(strsplit(statDF[x], "\t"))))
  ))
  monsterStats <- statDF[2:nrow(monsterStats), ]
  colnames(monsterStats) <- blockStats
  monsterStats <- monsterStats[levelIndex, ]
  
  # Define defenses
  defenses <- c("Melee", "Ranged")
  defenseDF <- websiteData[c(grep("Melee", websiteData[1:10]),
                             grep("Ranged", websiteData[1:20]))]
  defenseDF[length(defenseDF)] <- strsplit(defenseDF[length(defenseDF)], "<")[[1]][1]
  monsterDefenses <- do.call(cbind, lapply(1:length(defenseDF), function(x)
    as.numeric(c(unlist(strsplit(defenseDF[x], "\t"))))
  ))
  monsterDefenses <- monsterDefenses[2:nrow(monsterDefenses), ]
  colnames(monsterDefenses) <- defenses
  monsterDefenses <- monsterDefenses[levelIndex, ]
 
  # Dex/8 + Luk/40
  adjDefenses <- monsterDefenses + monsterStats[1]/8+monsterStats[2]/40
  
  return(adjDefenses)
}

dwDF <- getWebsiteData("Death Worms")
infDF <- getWebsiteData("Inferninon")

monsterElement <- strsplit(strsplit(websiteData[1], "Element")[[1]][1], ">")[[1]]
monsterElement <- trimws(monsterElement[length(monsterElement)])

# Calculate expected monster resists based on character level
dwResists <- getResists(dwDF)
infResists <- getResists(infDF)

# Calculate defenses based on character level
dwBlock <- getBlock(dwDF)
infResists <- getResists(infDF)







# Calculate best player resistance to monster element.  Keep ironthorn separate
armorResists <- armorCSV[colnames(armorCSV) == paste0("R", tolower(monsterElement))]
shieldResists <- shieldCSV[colnames(shieldCSV) == paste0("R", tolower(monsterElement))]
bestResist <- min(armorResists) - max(shieldResists)
ironthornResist <- min(armorResists) - shieldResists[match("Ironthorn", shieldCSV$Name), 1]
ironthornResFactor <- ironthornResist/bestResist

# Calculate expected weapon and armor damage
#   Does not account for resistances, returns a list of BTH and damage value
attackCalc <- function(weapon, armor) {
  weaponRow <- weaponCSV[grep(weapon, weaponCSV$Name), ]
  armorRow <- armorCSV[grep(armor, armorCSV$Name), ]
  
  charSTR <- characterCSV$STR
  charDEX <- characterCSV$DEX
  charLUK <- characterCSV$LUK
  statName <- ifelse(weaponRow$Type == "Melee", "STR", "DEX")
  statDMG <- ifelse(weaponRow$Type == "Melee", charSTR/8 + charLUK*3/80,
                                              charSTR/10 + charDEX/40 + charLUK*3/80)
  statBTH <- ifelse(weaponRow$Type == "Melee", charSTR*3/40 + charDEX*3/40 + charLUK/40,
                                              charDEX*3/20 + charLUK/40)
  
  totalBTH <- c(armorRow$BTH + weaponRow$BTH, armorRow$BTH + weaponRow$Bspecial)

  noSpecial <- (1-weaponRow$Rspecial) * weaponRow$Dmult * (weaponRow$Base * armorRow$Base.Mult + #Base damage
                                         (weaponRow$Random/2) * armorRow$Rand.Mult + #Random damage
                                          statDMG * armorRow$Stat.Mult + #Stat damage
                                           0.1*charLUK/2) #Lucky strike 10% chance, R damage
  onlySpecial <- weaponRow$Rspecial * (weaponRow$Dspecial * (weaponRow$Base + weaponRow$Random/2) + #B/R
                                      weaponRow$Lspecial * 0.1 * charLUK) # I think it's like a lucky strike?
  return(list(totalBTH, noSpecial, onlySpecial))
}

# Create matrix of attack damages
attackDF <- t(data.frame(do.call(cbind, lapply(weaponCSV$Name, function(x)
                            c(sapply(armorCSV$Name, function(y)
                                    round(unlist(attackCalc(x, y)[2]),1)))
                            ))))
attackDF <- data.frame(attackDF)
colnames(attackDF) <- armorCSV$Name
attackDF$Weapon <- weaponCSV$Name
rownames(attackDF) <- weaponCSV$Name
attackDF$Type <- weaponCSV$Type
attackDF$Element <- weaponCSV$Element

# Create matrix of bth possibilities
bthDF <- t(data.frame(do.call(cbind, lapply(weaponCSV$Name, function(x)
                            c(sapply(armorCSV$Name, function(y)
                                    round(unlist(attackCalc(x, y)[1]),1)))
                            ))))
bthDF <- data.frame(bthDF)
colnames(bthDF) <- armorCSV$Name
bthDF$Weapon <- weaponCSV$Name
rownames(bthDF) <- weaponCSV$Name

expectedDF <- t(data.frame(do.call(cbind, lapply(1:nrow(attackDF), function(x) {
                            c(sapply(1:10, function(y) {   # Currently 10 armors
                                    weaponName <- rownames(attackDF)[x]
                                    weaponType <- weaponCSV$Type[match(weaponName, weaponCSV$Name)]
                                    factorMRM <- ifelse(weaponType == "Melee",
                                                (100-monsterDefenses[1]+bthDF[x, y])/100,
                                                (100-monsterDefenses[2]+bthDF[x, y])/100)
                                    names(factorMRM) <- NULL
                                    factorMRM <- ifelse(factorMRM > 1, factorMRM == 1, factorMRM)
                                    factorMRM <- ifelse(factorMRM < 0, factorMRM == 0, factorMRM)
                                    weaponElement <- ifelse(colnames(attackDF)[y] == "Algern C",
                                                            "Dark",
                                                            weaponCSV$Element[match(weaponName, weaponCSV$Name)])
                                    factorResist <- monsterResists[match(weaponElement, names(monsterResists))]
                                    expectedDamage <- attackDF[x, y] * factorMRM * factorResist/100
                                    return(expectedDamage)
                            }))
  }))))
expectedDF <- data.frame(expectedDF)
colnames(expectedDF) <- armorCSV$Name
rownames(expectedDF) <- weaponCSV$Name
