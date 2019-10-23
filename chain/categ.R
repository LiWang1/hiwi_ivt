
categ <- function(target){
  # add another col named purpose_cat 
  purpose_cat = integer(nrow(target))
  target = cbind(target, purpose_cat)
  
  
  # categories 
  leisure = c(4, 10, 12, 13, 14, 19, 20,31, 33, 34, 35, 36, 37, 39)  # 36 can be at home 
  errands = c(26, 29, 30, 32)
  work = c(9, 11, 15, 16, 25)
  education = c(17, 18)
  home = c(1, 2, 3, 5, 6, 8)  # 1 can be outside 
  shopping = c(27, 28)
  other = c(7, 38, 99)  #7 has to be merged #part of 38 has to be merged. 
  notsure = c(22)
  
  ## 36 39 1 has to be further processed 
  
  
  # change to the bigger categories
  index_leisure = which(target$purpose %in% leisure)
  target$purpose_cat[index_leisure] = "Leisure"
  
  index_errands = which(target$purpose %in% errands)
  target$purpose_cat[index_errands] = "Errands"
  
  index_work = which(target$purpose %in% work)
  target$purpose_cat[index_work] = "Work"
  
  index_home = which(target$purpose %in% home)
  target$purpose_cat[index_home] = "Home"
  
  index_education = which(target$purpose %in% education)
  target$purpose_cat[index_education] = "Education"
  
  index_shopping = which(target$purpose %in% shopping)
  target$purpose_cat[index_shopping] = "Shopping"
  
  index_pd = which(target$purpose %in% notsure)
  target$purpose_cat[index_pd] = "PD"
  
  index_other = which(target$purpose %in% other)
  target$purpose_cat[index_other] = "Other"

  
  index_entertainment = which(target$purpose == 36)
  index_enterhome1 = index_entertainment[which(target$purpose_cat[index_entertainment-1] == "home")]
  index_enterhome2 = index_entertainment[which(target$tract_id[index_entertainment-1] == target$tract_id[index_entertainment] )]
  index_enterhome12 = intersect(index_enterhome1, index_enterhome2)
  index_enterhome3 = index_entertainment[which(is.na(target$tcfperwgt[index_entertainment+1]))]
  index_enterhome = union(index_enterhome12, index_enterhome3)
  target$purpose_cat[index_enterhome] = "Home"
  return(target)
}
