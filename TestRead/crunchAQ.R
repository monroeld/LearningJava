
# Read in CharacterCSV, WeaponCSV, ArmorCSV, ShieldCSV
characterCSV <- read.csv("CharacterStats.csv", sep = ",", stringsAsFactors = FALSE)
weaponCSV <- read.csv("WeaponStats.csv", sep = ",", stringsAsFactors = FALSE)
armorCSV <- read.csv("ArmorStats.csv", sep = ",", stringsAsFactors = FALSE) 
shieldCSV <- read.csv("ShieldStats.csv", sep = ",", stringsAsFactors = FALSE) # Toggle shields treated as both.  Note for ironthorn
armorCSV <- subset(armorCSV, Name != "Steel Plate") # Will never be the best

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

getMonsterResists <- function(monsterDF) {
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

getMonsterBlock <- function(monsterDF) {
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
  monsterStats <- as.data.frame(do.call(cbind, lapply(1:length(statDF), function(x)
    as.numeric(c(unlist(strsplit(statDF[x], "\t"))))
  )))
  monsterStats <- monsterStats[2:nrow(monsterStats), ]
  colnames(monsterStats) <- blockStats
  monsterStats <- monsterStats[levelIndex, ]
  
  # Define defenses
  defenses <- c("Melee", "Ranged")
  defenseDF <- websiteData[c(grep("Melee", websiteData[1:10]),
                             grep("Ranged", websiteData[1:20]))]
  defenseDF[length(defenseDF)] <- strsplit(defenseDF[length(defenseDF)], "<")[[1]][1]
  monsterDefenses <- as.data.frame(do.call(cbind, lapply(1:length(defenseDF), function(x)
    as.numeric(c(unlist(strsplit(defenseDF[x], "\t"))))
  )))
  monsterDefenses <- monsterDefenses[2:nrow(monsterDefenses), ]
  colnames(monsterDefenses) <- defenses
  monsterDefenses <- monsterDefenses[levelIndex, ]
  
  # Dex/8 + Luk/40
  adjDefenses <- as.numeric(monsterDefenses) + as.numeric(monsterStats[1]/8) + as.numeric(monsterStats[2]/40)
  names(adjDefenses) <- c("Melee", "Ranged")
  return(adjDefenses)
}

getMonsterInfo <- function(monsterName) {
  monsterDF <- getWebsiteData(monsterName)
  monsterResists <- data.frame(getMonsterResists(monsterDF))
  monsterBlock <- data.frame(getMonsterBlock(monsterDF))
  
  monsterElement <- strsplit(strsplit(monsterDF[1], "Element")[[1]][1], ">")[[1]]
  monsterElement <- data.frame(trimws(monsterElement[length(monsterElement)]))
  colnames(monsterElement) <- "monsterElement"
  
  total <- cbind(t(monsterResists), t(monsterBlock), monsterElement)
  rownames(total) <- NULL
  
  return(total)
}

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
  
  attackBTH <- armorRow$BTH + weaponRow$BTH
  
  noSpecial <- (1-weaponRow$Rspecial) * weaponRow$Dmult * (weaponRow$Base * armorRow$Base.Mult + #Base damage
                                                             (weaponRow$Random/2) * armorRow$Rand.Mult + #Random damage
                                                             statDMG * armorRow$Stat.Mult + #Stat damage
                                                             0.1*charLUK/2) #Lucky strike 10% chance, R damage
  
  return(list(attackBTH, noSpecial))
}

specialCalc <- function(weapon, armor) {
  weaponRow <- weaponCSV[grep(weapon, weaponCSV$Name), ]
  armorRow <- armorCSV[grep(armor, armorCSV$Name), ]
  
  charSTR <- characterCSV$STR
  charDEX <- characterCSV$DEX
  charLUK <- characterCSV$LUK
  statName <- ifelse(weaponRow$Type == "Melee", "STR", "DEX")
  statBTH <- ifelse(weaponRow$Type == "Melee", charSTR*3/40 + charDEX*3/40 + charLUK/40,
                    charDEX*3/20 + charLUK/40)
  onlySpecial <- weaponRow$Rspecial * (weaponRow$Dspecial * (weaponRow$Base + weaponRow$Random/2) + #B/R
                                         weaponRow$Lspecial * 0.1 * charLUK) # I think it's like a lucky strike?
  specialBTH <- armorRow$BTH + weaponRow$Bspecial
  return(list(specialBTH, onlySpecial))
}

damageCalc <- function(monsterInfo, bthMod, dmgMult) {
  attackDF <- t(data.frame(do.call(cbind, lapply(weaponCSV$Name, function(x)
    c(sapply(armorCSV$Name, function(y)
      round(unlist(attackCalc(x, y)[2]),1)*dmgMult))
  ))))
  attackDF <- data.frame(attackDF)
  colnames(attackDF) <- armorCSV$Name
  attackDF$Weapon <- weaponCSV$Name
  rownames(attackDF) <- weaponCSV$Name
  attackDF$Type <- weaponCSV$Type
  attackDF$Element <- weaponCSV$Element
  
  bthDF <- t(data.frame(do.call(cbind, lapply(weaponCSV$Name, function(x)
    c(sapply(armorCSV$Name, function(y)
      round(unlist(attackCalc(x, y)[1]),1)+bthMod))
  ))))
  bthDF <- data.frame(bthDF)
  colnames(bthDF) <- armorCSV$Name
  bthDF$Weapon <- weaponCSV$Name
  rownames(bthDF) <- weaponCSV$Name
  
  specialDF <- t(data.frame(do.call(cbind, lapply(weaponCSV$Name, function(x)
    c(sapply(armorCSV$Name, function(y)
      round(unlist(specialCalc(x, y)[2]),1)*dmgMult))
  ))))
  specialDF <- data.frame(specialDF)
  colnames(specialDF) <- armorCSV$Name
  specialDF$Weapon <- weaponCSV$Name
  rownames(specialDF) <- weaponCSV$Name
  specialDF$Type <- weaponCSV$Type
  specialDF$Element <- weaponCSV$Element
  
  specialBTH <- t(data.frame(do.call(cbind, lapply(weaponCSV$Name, function(x)
    c(sapply(armorCSV$Name, function(y)
      round(unlist(specialCalc(x, y)[1]),1)+bthMod))
  ))))
  specialBTH <- data.frame(specialBTH)
  colnames(specialBTH) <- armorCSV$Name
  specialBTH$Weapon <- weaponCSV$Name
  rownames(specialBTH) <- weaponCSV$Name
  
  expectedDF <- as.data.frame(t(data.frame(do.call(cbind, lapply(1:nrow(attackDF), function(x) {
    c(sapply(1:nrow(armorCSV), function(y) {
      weaponName <- rownames(attackDF)[x]
      weaponType <- weaponCSV$Type[match(weaponName, weaponCSV$Name)]
      weaponElement <- weaponCSV$Element[match(weaponName, weaponCSV$Name)]
      
      # Calculate special DF.  Not subject to element locking, so it goes first
      specialMRM <- ifelse(weaponType == "Melee",
                           (100 - monsterInfo$Melee + specialBTH[x, y])/100,
                           (100 - monsterInfo$Ranged + specialBTH[x, y])/100)
      names(specialMRM) <- NULL
      specialMRM <- ifelse(specialMRM > 1, 1, specialMRM)
      specialMRM <- ifelse(specialMRM < 0, 0, specialMRM)
      factorResist <- monsterInfo[match(weaponElement, names(monsterInfo))]
      expectedSpecial <- round(specialDF[x, y] * specialMRM * factorResist/100, 1)
      
      
      # Calculate basic attack DF
      attackMRM <- ifelse(weaponType == "Melee",
                          (100 - monsterInfo$Melee + bthDF[x, y])/100,
                          (100 - monsterInfo$Ranged + bthDF[x, y])/100)
      names(attackMRM) <- NULL
      attackMRM <- ifelse(attackMRM > 1, 1, attackMRM)
      attackMRM <- ifelse(attackMRM < 0, 0, attackMRM)
      weaponElement <- ifelse(armorCSV$Element[y] != "Any",
                              armorCSV$Element[y],
                              weaponCSV$Element[match(weaponName, weaponCSV$Name)])
      factorResist <- monsterInfo[match(weaponElement, names(monsterInfo))]
      expectedAttack <- round(attackDF[x, y] * attackMRM * factorResist/100, 1)
      
      return(as.numeric(expectedSpecial) + as.numeric(expectedAttack))
    }))
  })))))
  
  colnames(expectedDF) <- armorCSV$Name
  rownames(expectedDF) <- weaponCSV$Name
  
  return(expectedDF)
}

findBest <- function(monsterInfo) {
  # Calculate best player resistance to monster element.  Keep ironthorn separate
  
  # currently just saves the best value.  How do I want to relate resistances to damage?  Dmg / res?
  armorResists <- armorCSV[colnames(armorCSV) == paste0("R", tolower(monsterInfo$monsterElement))]
  armorResists$Name <- armorCSV$Name
  shieldResists <- shieldCSV[colnames(shieldCSV) == paste0("R", tolower(monsterInfo$monsterElement))]
  shieldResists$Name <- shieldCSV$Name
  
  charResists <- data.frame(t(do.call(rbind, lapply(1:nrow(armorResists), function(x) {
    sapply(1:nrow(shieldResists), function(y)
      armorResists[x, 1] - shieldResists[y, 1] )}))), row.names = shieldCSV$Name)
  colnames(charResists) <- armorCSV$Name
  
  bestResist <- c(as.numeric(min(charResists)),
                  armorResists$Name[match(min(armorResists[, 1]), armorResists[, 1])],
                  shieldResists$Name[match(max(shieldResists[, 1]), shieldResists[, 1])])
  
  ironthornResist <- c(min(armorResists[, 1]) - shieldResists[match("Ironthorn", shieldCSV$Name), 1],
                       armorResists$Name[match(min(armorResists[, 1]), armorResists[, 1])])
  ironthornResFactor <- as.numeric(ironthornResist[1])/as.numeric(bestResist[1])
  
  anyShield <- damageCalc(monsterInfo, 0, 1)
  ironThorn <- damageCalc(monsterInfo, -10, 1.5)

  ifelse(max(anyShield) > max(ironThorn),
        mostDMG <- c(max(anyShield), weaponCSV$Name[which(max(anyShield)==anyShield, arr.ind = TRUE)[1]],
                      armorCSV$Name[which(max(anyShield)==anyShield, arr.ind = TRUE)[2]], "No Ironthorn"),
        mostDMG <- c(max(ironThorn), weaponCSV$Name[which(max(ironThorn)==ironThorn, arr.ind = TRUE)[1]],
                      armorCSV$Name[which(max(ironThorn)==ironThorn, arr.ind = TRUE)[2]], "Ironthorn"))
  names(mostDMG) <- c("Expected Damage", "Weapon", "Armor", "Shield")

  anyshieldRatio <- data.frame(sapply(1:nrow(anyShield), function(x) anyShield[x, ]/(armorResists[, 1] -
    max(shieldCSV[, match(paste0("R", tolower(monsterInfo$monsterElement)), colnames(shieldCSV))]))))
  colnames(anyshieldRatio) <- rownames(anyShield)
  anyshieldRatio <- c(max(unlist(anyshieldRatio)),
                          unlist(strsplit(names(which(unlist(anyshieldRatio) == max(unlist(t(anyshieldRatio))), arr.ind = TRUE)), "[.]")[[1]]))
  
  # Not right yet
  ironthornRatio <- data.frame(sapply(1:nrow(ironThorn), function(x) ironThorn[x, ]/(armorResists[, 1] -
                      shieldCSV[match("Ironthorn", shieldCSV$Name),
                               match(paste0("R", tolower(monsterInfo$monsterElement)), colnames(shieldCSV))])))
  colnames(ironthornRatio) <- rownames(ironThorn)
  ironthornRatio <- c(max(unlist(ironthornRatio)),
                      unlist(strsplit(names(which(unlist(ironthornRatio) == max(unlist(t(ironthornRatio))), arr.ind = TRUE)), "[.]")[[1]]))
  
  bestRatios <- rbind(ironthornRatio, anyshieldRatio)
  colnames(bestRatios) <- c("DMG / Resists", "Weapon", "Armor")
  
  
  
  return(list(mostDMG, bestRatios))
}

findBest(getMonsterInfo("BURP"))

# infInfo <- getMonsterInfo("BURP")


library(rPython)
python.exec("print 'hello world' ")
https://cran.r-project.org/web/packages/rPython/rPython.pdf




# ironthornDmgFactor <- round(max(ironThorn[, ironthornResist[2]]) /
#                         max(anyShield[, match(bestResist[2], colnames(anyShield))]), 2)
# 
# ironthornDmg <- cbind(data.frame(ironThorn[, ironthornResist[2]], row.names = rownames(ironThorn)),
#                       data.frame(anyShield[, match(bestResist[2], colnames(anyShield))]))
# colnames(ironthornDmg) <- c("ironthorn", "other")

