library(tidyverse)
setwd("D:/CMML/")

explengthsample <- function(expN,expVar){
  out = rnorm(1,expN,expVar)
  while (out <= 0)
    out = rnorm(1,expN,expVar)
  end
  out
}

log_inve <- function (x,a,h=0){
  if ((1/x)-1 > 0){
    out = ((log((1/x)-1)/-a) + h)
  }else if ((1/x)-1 < 0){
    out = ((-log(-((1/x)-1))/-a) + h)
  }else if ((1/x)-1 == 0){
    out = ((0/-a) + h)
  }
  out
}

Bonus <- function (Con){
  if (Con >= 0.75){
    out = 3 * Con
  }else if(Con >= 0.6){
    out = 2 * Con
  }else{
    out = Con
  }
  out
}

computeAC_performance <- function(risk, Schema_res, Item_EI, Outputs_cho, ThisPhase, schemainfo){
  if (risk == 'high'){
    if (length(unique(Schema_res)) == 1){
      AC = 1
      performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]]) * 3
    }else if(length(unique(Schema_res)) == 2 & sum(Schema_res == unique(Schema_res)[1]) != 2){
      AC = 0.5
      performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]])
    }else{
      AC = 0
      performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]])
    }
  }else{
    if (length(unique(Schema_res)) == 1){
      AC = 1
      performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]]) * 3
    }else if(length(unique(Schema_res)) == 2 & sum(Schema_res == unique(Schema_res)[1]) != 2){
      AC = 0.5
      right_schema =  as.data.frame(sort(table(Schema_res), decreasing=TRUE))[1,1]
      performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]]) + schemainfo$payoff[schemainfo$schemaID==right_schema]*3
    }else{
      AC = 0
      performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]])
    }
  }
  
  mean.payoff <- sum(Item_EI$payoff[Item_EI$ID %in% 
                                      Outputs_cho[ThisPhase,
                                                  c("Cho_1","Cho_2","Cho_3","Cho_4")]]) / 4
  
  return(list(AC, performance, mean.payoff))
}


simulation <- function(Param.df, type, exp_type, save=F, savepath="",
                       sim.mode=c("before", "after", "whole")[3], 
                       before.path=NA,
                       save.confi=T,
                       after.read.best.only=F,
                       model.version=c(1,2,3)[1],
                       scale.confi.init){
  

  subjectnum = nrow(Param.df)
  simupath = file.path(savepath, type)
  expschema = read.csv(paste0(exp_type, "_schemainfo2.csv"))
  
  if(scale.confi.init){
    for(row in 1:nrow(expschema)){
      curr.payoff <- expschema$payoff[row]
      if(curr.payoff == 2){
        expschema$familirarity[row] <- 0.9
      }else if(curr.payoff == 3){
        expschema$familirarity[row] <- 0.6
      }else if(curr.payoff == 4){
        expschema$familirarity[row] <- 0.4
      }else if(curr.payoff == 5){
        expschema$familirarity[row] <- 0.3
      }else if(curr.payoff == 6){
        expschema$familirarity[row] <- 0.2
      }
    }
  }
  print(expschema)
  
  max_Round = 200
  ALL.df = data.frame()
  confidence.df <- data.frame()
  gconfidence.df <- data.frame()
  dwelltime.df <- data.frame()
  
  for (Subject in 1:subjectnum) {
    error = 0
    start.time <- Sys.time()
    
    a_schema = Param.df$a_schema[Subject]
    h_schema = Param.df$h_schema[Subject]
    Beta_N = Param.df$Beta_N[Subject]
    Beta_Var = Param.df$Beta_Var[Subject]
    a_generic = Param.df$a_generic[Subject]
    h_generic = Param.df$h_generic[Subject]
    Beta_gN = Param.df$Beta_gN[Subject]
    Beta_gVar = Param.df$Beta_gVar[Subject]
    w = Param.df$w[Subject]
    Phi = Param.df$Phi[Subject]
    decay_speed = Param.df$decay_speed[Subject]
    decay_speed_thres = Param.df$decay_speed_thres[Subject]
    thres_item_inter = Param.df$thres_item_inter[Subject]
    thres_item_final = Param.df$thres_item_final[Subject]
    theta_shift = Param.df$theta_shift[Subject]
    timevar = Param.df$timevar[Subject]
    modeltimestep = Param.df$modeltimestep[Subject]
    success_schema <- c()
    # Creating a list to store previously successful schemas

    Outputs_cho = data.frame(Subject = c(rep(Subject, max_Round*2)), Round = c(rep(1:(max_Round*2),each=2)),Phase = c(1:(max_Round*2)),
                             Schema = c(rep(0,max_Round*2)),Schema_RT = c(rep(0,max_Round*2)),
                             Schema_OB = c(rep(0,max_Round*2)),Schema_AS = c(rep(0,max_Round*2)),
                             Cho_1 = c(rep(0,max_Round*2)),Cho_2 = c(rep(0,max_Round*2)), Cho_3 = c(rep(0,max_Round*2)), 
                             Cho_4 = c(rep(0,max_Round*2)),RT_1 = c(rep(0,max_Round*2)), RT_2 = c(rep(0,max_Round*2)), 
                             RT_3 = c(rep(0,max_Round*2)), RT_4 = c(rep(0,max_Round*2)),OB_1 = c(rep(0,max_Round*2)), 
                             OB_2 = c(rep(0,max_Round*2)), OB_3 = c(rep(0,max_Round*2)), OB_4 = c(rep(0,max_Round*2)), 
                             AS_1 = c(rep(0,max_Round*2)), AS_2 = c(rep(0,max_Round*2)), AS_3 = c(rep(0,max_Round*2)), 
                             AS_4 = c(rep(0,max_Round*2)),
                             schema_payoff = c(rep(0,max_Round*2)), AC = c(rep(0,max_Round*2)), 
                             performance = c(rep(0,max_Round*2)),afterbreak = c(rep(0,max_Round*2)))
    Outputs_learn = data.frame()
    Outputs_glearn = data.frame()
    Outputs_dwell = data.frame() 

    schemainfo <- expschema  %>% filter(new==0) %>% select(-c(author,new))
    colnames(schemainfo)[c(3,5,6)] = c('conN','expN','expVar')

    schemainfo = arrange(schemainfo, desc(payoff))
    
    if(!scale.confi.init){
      schemainfo$conN = 0.2+0.8*schemainfo$conN
    }
    
    schemainfo$conVar = c(rep(var(schemainfo$conN), 10))


    
    Decitem = data.frame(ItemID = c(1:120))
    Decitem$SchemaID = rep(schemainfo$schemaID,each=12)
    Decitem$payoff = rep(schemainfo$payoff,each=12)
    Gcon = data.frame(conN = mean(schemainfo$conN), conVar = mean(schemainfo$conVar))

    ThisRound = 1
    clock = 0
    break_happens = 0
    explorationtime = 600
    
    if (type %in% c("L","Lc","LH")){
      risk = "low"
    } else {
      risk = "high"
    }

    not.updated <- T

    while (clock < 4500){
      print(paste("Subject:", Subject,"Round:", ThisRound,"Time:", clock))
      print(paste("break_happens:",break_happens,"risk:",risk,"type:", type))
      
      if(sim.mode == "after" & not.updated){
        clock <- 1500
        not.updated <- F
      }
      
      if (break_happens == 0 & clock >= 1500){
        
        if(sim.mode=="after"){
          ref.schemainfo <- read.csv(file.path(before.path, "confidence.csv"))
          ref.gconfidence.df <- read.csv(file.path(before.path, "gconfidence.csv"))
          
          if(after.read.best.only){
            target.schemainfo <- ref.schemainfo
            target.Gcon <- ref.gconfidence.df
            
          }else{
            random.subj <- Subject
            if(!(random.subj  %in% ref.schemainfo$Subject)){
              print('before part of estimation nonexistent, continued')
              error <- 1
              break
            }
            
          target.schemainfo <- ref.schemainfo %>%
              filter(Subject == random.subj)
            
            target.Gcon <- ref.gconfidence.df %>%
              filter(Subject == random.subj) %>%
              select(-Subject)
          }
          
          schemainfo <- target.schemainfo %>%
            filter(Time == max(target.schemainfo$Time)) %>%
            select(-c(Time, Subject))
          Gcon <- target.Gcon[nrow(target.Gcon),]
          
        }
        
        if (type %in% c("L","H")){
          schemainfo2 <- expschema %>% filter(type=='new') %>% select(-c(author,new))
          colnames(schemainfo2)[c(3,5,6)] = c('conN','expN','expVar')
          
          schemainfo2 = arrange(schemainfo2,desc(payoff))
          schemainfo2$conN = 0.2+0.8*schemainfo2$conN
          schemainfo2$conVar = c(rep(var(schemainfo2$conN),5))
          
          if("X" %in% colnames(schemainfo)){
            schemainfo <- schemainfo %>% filter(type=='same') %>% select(-c("X"))
          }else{
            schemainfo <- schemainfo %>% filter(type=='same')
          }
          schemainfo <- rbind(schemainfo,schemainfo2)

          schemainfo <- arrange(schemainfo,desc(payoff))
          
          Decitem$SchemaID = rep(schemainfo$schemaID,each=12)
          Decitem$payoff = rep(schemainfo$payoff,each=12)
        }
        
        if (type %in% c("L","Lc","HL")){## i.e. low risk after break
          risk = "low"
        } else {
          risk = "high"
        }
        
        print(paste0("break at ", clock))
        break_happens = 1
        breakR = ThisRound
        explorationtime = 300

        if(sim.mode == "before"){
          break
        }
      }
      
      Con_PriorExp = mapply(rnorm, 1, mean = schemainfo$conN, sd = schemainfo$conVar)
      
      Exptime = mapply(explengthsample, expN = schemainfo$expN, expVar = schemainfo$expVar)
      
      Exptime = Exptime  * explorationtime/sum(Exptime) * 0.6
      clock = clock + explorationtime
      explorationtime = 180
      
      Outputs_learn <- bind_rows(Outputs_learn, schemainfo)
      Outputs_glearn = rbind(Outputs_glearn,Gcon)

      learning = mapply(log_inve, Con_PriorExp, a_schema
                        #, h_schema
      )
      learning = learning + Exptime
      Con_afterExp = 1/(1+exp(-a_schema*(learning
                                         #-h_schema
      )))
      schemainfo$conN = schemainfo$conN + Beta_N*(Con_afterExp - Con_PriorExp)
      schemainfo$conVar = schemainfo$conVar + Beta_Var*(abs(Con_afterExp - Con_PriorExp) - schemainfo$conVar)
      
      Con_PriorExp = rnorm(1, mean = Gcon$conN, sd = Gcon$conVar)
      
      learning = mapply(log_inve, Con_PriorExp, a_generic
                        #, h_generic
      )
      learning = learning + mean(Exptime)
      Con_afterExp = 1/(1+exp(-a_generic*(learning
                                          #-h_generic
      )))
      Gcon$conN = Gcon$conN + Beta_gN*(Con_afterExp - Con_PriorExp)
      Gcon$conVar = Gcon$conVar + Beta_gVar*(abs(Con_afterExp - Con_PriorExp) - Gcon$conVar)
      
      Thischosen = schemainfo %>% 
        group_by(payoff) %>% 
        summarise(chosen = sample(schemaID, size = 1, prob = NULL)) 
      
      decisiontime = 1
      while (decisiontime <= 2){
        if (clock >= 5500 | Sys.time() - start.time > 240){
          print('Beyond time in decisiontime <= 2!')
          error = 1
          break
        }
        
        ThisPhase = ifelse(decisiontime == 1, (ThisRound*2)-1, ThisRound*2)
        
        Schema_EI = data.frame(schemaID = schemainfo$schemaID,
                               payoff = schemainfo$payoff)
        Schema_EI$Scon = mapply(rnorm, 1, mean = schemainfo$conN, sd = schemainfo$conVar)
        Schema_EI$Gcon = rnorm(1, mean = Gcon$conN, sd = Gcon$conVar)
        
        Schema_EI$weightCon = w*Schema_EI$Scon + (1-w)*Schema_EI$Gcon
        Schema_EI$DM = mapply(Bonus,Schema_EI$weightCon) * Schema_EI$payoff
        
        if (decisiontime == 1){
          ChoSchema = Schema_EI[Schema_EI$schemaID %in% Thischosen$chosen,]
        }else if(decisiontime == 2){
          ChoSchema = Schema_EI[!Schema_EI$schemaID %in% Thischosen$chosen,]
        }
        
        ChoSchema$evidence = 0
        ChoItem = Decitem[Decitem$SchemaID %in% ChoSchema$schemaID,] %>% 
          group_by(SchemaID) %>%
          summarise(chosen = sample(ItemID, size = 4, prob = NULL))
        
        Item_EI = data.frame(ID = Decitem[Decitem$ItemID %in% ChoItem$chosen,1], 
                             Schema = Decitem[Decitem$ItemID %in% ChoItem$chosen,2], 
                             payoff = Decitem[Decitem$ItemID %in% ChoItem$chosen,3])
        Item_EI = Item_EI %>% 
          group_by(Schema) %>% 
          mutate(N = schemainfo$conN[schemainfo$schemaID == Schema], 
                 Var = schemainfo$conVar[schemainfo$schemaID == Schema],
                 DM = ChoSchema$DM[Schema == ChoSchema$schemaID])
        
        Item_EI$evidence = 0
        Item_EI$recovery = 0
        Item_EI$timevar = 1
        Item_EI$decision = 0
        Item_EI$OB = 0
        Item_EI$AS = 0
        Item_EI$status = 0 
        Item_EI$thres = thres_item_inter
        
        # First initialise schema_decision
        schema_decision <- c()
        # Set a Boolean parameter to determine whether or not to use success_schema
        use_success <- F
        # If there is a success_schema in ChoSchema, take it out as schema_decision
        if(length(success_schema) > 0) {
          if(any(success_schema %in% ChoSchema$schemaID)){
            sel_index <- which(success_schema %in% ChoSchema$schemaID)
            schema_decision <- success_schema[sel_index]
            success_schema <- success_schema[-sel_index]
            use_success <- T
          }
        }
        
        
        Finish = 0
        dwell_choice_flag = 0 
        attention = 0
        shift = 0
        CTime = 0
        dwelltime = 0
        thres_schema = Param.df$thres_schema[Subject] 
        
        while (Finish <= 4){
          if (clock >= 5500){
            print('Beyond time in Finish <= 4!')
            error = 1
            break
          }
          if (attention == 0) {
            Item_EI$timevar = 1-(1/(exp(timevar*Item_EI$recovery)))
            if (sum(Item_EI$decision == 0) == 1){ 
              attention = Item_EI$ID[Item_EI$decision == 0]
            }else{
              if (length(schema_decision) == 0){
                if(mean(Item_EI$decision) == 1){
                  print("schema not chosen after all items decided")
                  error = 1
                  break
                }
                p.list = (exp(Phi*Item_EI$evidence[Item_EI$decision == 0] * Item_EI$DM[Item_EI$decision == 0] * Item_EI$timevar[Item_EI$decision == 0]))/
                  sum((exp(Phi*Item_EI$evidence[Item_EI$decision == 0] *Item_EI$DM[Item_EI$decision == 0] * Item_EI$timevar[Item_EI$decision == 0])))
                
                p.list[is.na(p.list)] <- 1
                attention = sample(Item_EI$ID[Item_EI$decision == 0], 1,
                                   prob = p.list)
              } else{
                p.list <- (exp(Phi*Item_EI$evidence[Item_EI$decision == 0]* Item_EI$timevar[Item_EI$decision == 0]))/
                  sum((exp(Phi*Item_EI$evidence[Item_EI$decision == 0]* Item_EI$timevar[Item_EI$decision == 0])))
                p.list[is.na(p.list)] <- 1
                attention = sample(Item_EI$ID[Item_EI$decision == 0],1,
                                   prob = p.list)
              }
            }
            Item_EI$OB[Item_EI$ID == attention] = 1
            Item_EI$AS[Item_EI$ID == attention] = Item_EI$AS[Item_EI$ID == attention] + 1

            shift = 0
          }
          
          while (shift == 0) {
            if (clock >= 5500){
              print('Beyond time in shift == 0!')
              error = 1
              break
            }
            

            CTime = CTime + modeltimestep
            clock = clock + modeltimestep
            dwelltime = dwelltime + modeltimestep
            
            Item_EI$evidence[Item_EI$ID == attention] = Item_EI$evidence[Item_EI$ID == attention] +
              rnorm(1, Item_EI$N[Item_EI$ID == attention], Item_EI$Var[Item_EI$ID == attention])
            
            Item_EI$recovery[Item_EI$ID != attention] = Item_EI$recovery[Item_EI$ID != attention] + 1

            Item_EI$evidence[Item_EI$ID != attention & Item_EI$decision == 0] = 
              Item_EI$evidence[Item_EI$ID != attention & Item_EI$decision == 0] * decay_speed
            
            Item_EI$thres = Item_EI$thres * decay_speed_thres
            thres_schema = thres_schema * decay_speed_thres
            Item_EI$evidence[Item_EI$evidence > Item_EI$thres] = Item_EI$thres

            if (Item_EI$evidence[Item_EI$decision == 0 & 
                                 Item_EI$ID == attention ] >= 
                Item_EI$thres[Item_EI$decision == 0 & Item_EI$ID == attention]){
              shift = 1
              
              if (length(schema_decision) != 0 & 
                  any(Item_EI$evidence[Item_EI$decision == 0 & 
                                       Item_EI$ID == attention & 
                                       Item_EI$status == 1] >= 
                      Item_EI$thres[Item_EI$decision == 0 & 
                                    Item_EI$ID == attention & 
                                    Item_EI$status == 1])){ 
                if(Finish == 0 ){
                  Finish = 1
                  Outputs_cho[ThisPhase, paste0("Cho_", Finish)] = Item_EI$ID[Item_EI$evidence >= Item_EI$thres &
                                                                                Item_EI$decision == 0 & 
                                                                                Item_EI$ID == attention & 
                                                                                Item_EI$status == 1]
                  Outputs_cho[ThisPhase, paste0("RT_", Finish)] = CTime
                  
                  Outputs_cho[ThisPhase, paste0("OB_", Finish)] = sum(Item_EI$OB == 1)
                  
                  Outputs_cho[ThisPhase, paste0("AS_", Finish)] = sum(Item_EI$AS)
                }else{
                  Outputs_cho[ThisPhase, paste0("Cho_", Finish)] = Item_EI$ID[Item_EI$evidence >= Item_EI$thres &
                                                                                Item_EI$decision == 0 & 
                                                                                Item_EI$ID == attention & 
                                                                                Item_EI$status == 1]
                  Outputs_cho[ThisPhase, paste0("RT_", Finish)] = CTime
                  Outputs_cho[ThisPhase, paste0("OB_", Finish)] = sum(Item_EI$OB == 1)
                  Outputs_cho[ThisPhase, paste0("AS_", Finish)] = sum(Item_EI$AS)
                  Finish = Finish + 1
                }

                Item_EI$OB = 0
                Item_EI$AS = 0
                CTime = 0
                dwell_choice_flag = 1
              }
              
              Item_EI$decision[Item_EI$evidence >= Item_EI$thres & Item_EI$decision == 0 & Item_EI$status == 1] = 1
              Item_EI$thres[Item_EI$evidence >= Item_EI$thres & Item_EI$decision == 0 & Item_EI$status == 0] = thres_item_final
              Item_EI$status[Item_EI$thres == thres_item_final & Item_EI$decision == 0 & Item_EI$status == 0] = 1
              
            }else{
              shift = sample(c(1, rep(0, theta_shift)),1, prob = NULL)
            }
            
            # An empty schema_decision or more than one will require the 
            # schema's evidence to be calculated and updated to a single value, 
            # since the schema can only select one
            if (length(schema_decision) != 1 || (length(schema_decision) == 1 && use_success)) {
              ChoSchema = ChoSchema %>% group_by(schemaID) %>% mutate(evidence = sum(Item_EI$evidence[Item_EI$Schema == schemaID]))
              
              # If schema_decision length is 1 but use_success is T and a schema reaches the threshold. 
              # Just clear the evidences of the other schema's items, which is a privilege of success_schema. 
              # The reason for setting use_success is so that this privilege can only be used once, 
              # otherwise an unlimited privilege would make the simulation meaningless (unlimited privilege equals 100% correct).
              if (any(ChoSchema$evidence >= thres_schema)){
                if(use_success & length(schema_decision) == 1){
                  schema_decision = schema_decision
                  use_success = F
                }else{
                  if (length(ChoSchema$schemaID[ChoSchema$evidence >= thres_schema]) != 1){
                    schema_decision = ChoSchema$schemaID[ChoSchema$evidence == max(ChoSchema$evidence)][1]
                  }else{
                    schema_decision = ChoSchema$schemaID[ChoSchema$evidence >= thres_schema]
                  }
                }

                if(model.version == 1){
                  Item_EI$evidence[Item_EI$Schema != schema_decision] = 0
                  Item_EI$N[Item_EI$Schema != schema_decision] = 1 - Item_EI$N[Item_EI$Schema != schema_decision]
                }else if(model.version == 2){
                  Item_EI$evidence[Item_EI$Schema != schema_decision] = - Item_EI$evidence[Item_EI$Schema != schema_decision] 
                }else if(model.version == 3){
                  Item_EI$N[Item_EI$Schema != schema_decision] = - Item_EI$N[Item_EI$Schema != schema_decision]
                }
                
                
                Outputs_cho$Schema[ThisPhase] = schema_decision[1]
                Outputs_cho$Schema_RT[ThisPhase] = CTime
                Outputs_cho$Schema_OB[ThisPhase] = sum(Item_EI$OB == 1)
                Outputs_cho$Schema_AS[ThisPhase] = sum(Item_EI$AS)
                Outputs_cho$schema_payoff[ThisPhase] = ChoSchema$payoff[ChoSchema$schemaID == schema_decision[1]]
                Item_EI$OB = 0
                Item_EI$AS = 0
                CTime = 0
                shift = 1
                Item_EI$decision = 0
                Finish = 1
                dwell_choice_flag = 1
                Item_EI$thres[Item_EI$status == 0] = thres_item_final
                Item_EI$status = 1
              }
            }
            
            
            if (shift == 1){
              onerecord = data.frame(Subject=Subject,Round=ThisRound,Phase=ThisPhase,
                                     item_ID=attention,item_Schema=Item_EI$Schema[Item_EI$ID==attention],
                                     which_choice=Finish - dwell_choice_flag, 
                                     dwelltime=dwelltime,afterbreak=break_happens) 
              Outputs_dwell = rbind(Outputs_dwell,onerecord) 
              dwelltime = 0
              dwell_choice_flag = 0 
              Item_EI$recovery[Item_EI$ID == attention] = 0
              Item_EI$timevar[Item_EI$ID == attention] = 0
              attention = 0
            }
          }
        }
        
        
        Schema_res = Item_EI$Schema[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]]
        feedback = computeAC_performance(risk, Schema_res, Item_EI, Outputs_cho, ThisPhase, schemainfo)
        Outputs_cho$AC[ThisPhase] = feedback[[1]]
        Outputs_cho$performance[ThisPhase] = feedback[[2]]
        Outputs_cho$payoff[ThisPhase] = feedback[[3]]

        # If AC is not 0, it is considered a win and the schema_decision is stored in success_schema
        if(feedback[[1]] != 0) {
          success_schema <- c(schema_decision,success_schema)
        }
        
        # Export success_schema
        if(length(success_schema) == 0){
          Outputs_cho$success_schema[ThisPhase] = "0"
        }else{
          Outputs_cho$success_schema[ThisPhase] = paste(success_schema, collapse = ",")
        }
        
        Outputs_cho$afterbreak[ThisPhase] = break_happens
        
        Outputs_learn = rbind(Outputs_learn,schemainfo)
        Outputs_glearn = rbind(Outputs_glearn,Gcon)

        schemainfo$conN[schemainfo$schemaID %in% unique(Schema_res)] = 
          schemainfo$conN[schemainfo$schemaID %in% unique(Schema_res)] + 
          Beta_N*(Outputs_cho$AC[ThisPhase] - Schema_EI$Scon[Schema_EI$schemaID %in% unique(Schema_res)])
        
        schemainfo$conVar[schemainfo$schemaID %in% unique(Schema_res)] = 
          schemainfo$conVar[schemainfo$schemaID %in% unique(Schema_res)] + 
          Beta_Var*(abs(Schema_EI$Scon[Schema_EI$schemaID %in% unique(Schema_res)] - Outputs_cho$AC[ThisPhase]) - 
                      schemainfo$conVar[schemainfo$schemaID %in% unique(Schema_res)])
        
        Gcon$conN = Gcon$conN + Beta_gN*(Outputs_cho$AC[ThisPhase] - unique(Schema_EI$Gcon))
        Gcon$conVar = Gcon$conVar + Beta_gVar*(abs(Outputs_cho$AC[ThisPhase] - unique(Schema_EI$Gcon)) - Gcon$conVar)
        decisiontime = decisiontime + 1
      }
      ThisRound = ThisRound + 1

    }
    
    
    if (error != 1){
      Outputs_cho$breakR = breakR
      Outputs_cho = Outputs_cho[Outputs_cho$RT_4!=0, ]
      Outputs_dwell = Outputs_dwell[Outputs_dwell$Phase<=max(Outputs_cho$Phase),]
      ALL.df <- rbind(ALL.df, Outputs_cho)
      dwelltime.df <- rbind(dwelltime.df, Outputs_dwell)

      Time = rep(1:((ThisRound-1)*3),each=10)
      Outputs_learn <- cbind(Outputs_learn, Time)
      Outputs_learn$Subject = Subject
      Outputs_glearn$Subject = Subject 
      if(
        #sim.mode=="before" | 
        save.confi == T){
        confidence.df <- rbind(confidence.df, Outputs_learn)
      }
      gconfidence.df <- rbind(gconfidence.df, Outputs_glearn)
    }
    
    gc()
    
  }
  
  
  ALL.df$type = type
  dwell_mean <- dwelltime.df %>%
    mutate(choice = replace(which_choice, which_choice == 0, 1)) %>%
    group_by(Subject, Round, Phase, choice, afterbreak) %>%
    summarise(dwell_mean=mean(dwelltime)) %>%
    pivot_wider(names_from = choice,
                names_glue = 'dwellmean_{choice}',
                values_from = dwell_mean)
  allresult <- merge(ALL.df,dwell_mean,by=c("Subject",'Round',"Phase","afterbreak"))
  allresult <- allresult %>% arrange(Subject,Phase)
  allresult <- allresult %>%
    mutate(RT_1 = Schema_RT + RT_1,
           OB_1 = Schema_OB + OB_1,
           AS_1 = Schema_AS + AS_1)
  allresult$OB_1[allresult$OB_1 > 20] <- 20
  
  if(save){
    if (!dir.exists(simupath)){
      dir.create(simupath, recursive = T)
    }
    write_csv(allresult,file.path(simupath,'allresult_processed.csv'))
    write_csv(Param.df, file.path(simupath,'Paras.csv'))
    
    if(
      #sim.mode == "before" | 
      save.confi == T){
      write_csv(confidence.df, file.path(simupath,'confidence.csv'))
      write_csv(gconfidence.df, file.path(simupath,'gconfidence.csv'))
    }
    
    # write_csv(dwelltime.df, file.path(simupath,'dwelltime.csv'))
  }
  
  return(list(param=Param.df,
              allresult_processed=allresult))
}


typelist = c("H","Hc","HL","L","Lc","LH")
exp_typelist = c("painting", "quote")

subjectnum = 100
Param.df <- data.frame(Subject = 1:subjectnum,
                       a_schema = rep(0.2,subjectnum),
                       h_schema = rep(1000,subjectnum),
                       Beta_N=rep(0.2,subjectnum),
                       Beta_Var= rep(0.3,subjectnum),
                       a_generic  = rep(0.1,subjectnum),
                       h_generic = rep(1500,subjectnum),
                       Beta_gN = rep(0.1,subjectnum),
                       Beta_gVar = rep(0.2,subjectnum),
                       w = rep(0.3,subjectnum),
                       Phi = rep(50,subjectnum), 
                       decay_speed = rep(0.999,subjectnum),
                       decay_speed_thres = rep(0.999,subjectnum), 
                       thres_item_inter  = rep(6,subjectnum),
                       thres_item_final = rep(13.75,subjectnum),
                       thres_schema = rep(50,subjectnum),
                       theta_shift = rep(3, subjectnum),
                       timevar = rep(0.0001,subjectnum),
                       modeltimestep = rep(0.061 ,subjectnum))

res <- simulation(Param.df, "L", "data/painting", save = T,
                  savepath = "res/wsls_before", sim.mode="before",
                  before.path = file.path("res/wsls_before", "L"),
                  scale.confi.init = T)

res <- simulation(Param.df, "L", "data/painting", save = T,
                  savepath = "res/wsls_after", sim.mode="after",
                  before.path = file.path("res/wsls_before", "L"),
                  scale.confi.init = T)

params <- res$param
allresult <- res$allresult_processed


init.range.fixed.0804 <- data.frame(
  Beta_N = c(0.001, 0.7),
  Beta_gN = c(0.001, 1),
  w = c(0.1, 0.9),
  Phi = c(1, 50),
  decay_speed = c(0.99, 0.9995),
  decay_speed_thres = c(0.99, 0.9995),
  thres_item_final = c(7.8, 50),
  theta_shift = c(1, 10),
  thres_schema = c(7.8, 50),
  
  a_schema = c(0.01, 1),
  h_schema = c(10, 3000),
  Beta_Var = c(0.001, 0.4),
  a_generic = c(0.01, 1),
  h_generic = c(10, 3000),
  Beta_gVar = c(0.001, 1),
  thres_item_inter = c(0.01, 7.75),
  timevar = c(0.0000001, 0.002),
  modeltimestep = c(0.001, 0.1)
)

