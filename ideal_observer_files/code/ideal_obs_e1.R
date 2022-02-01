#####################
#Ideal observer model
######################

library(dplyr)
library(tidyr)
library(ggplot2)
library(js)
library(V8)
library(doMC)
library(doParallel)
library(foreach)
library(rjson)


rm(list=ls())


load('../data/exp1_data.rdata') #Load the full data
load('../data/world_key.rdata') #Load the full world key
key<-key %>% filter(target_fAB!=0, target_heavier!='same') #Remove the variants that are not really in the space anymore

ppt_files<-list.files('../data/true_trajectories')#List the individual participant trial files

p<-b<-t<-1 #For debugging set all indices to 1
rm(clips)

for (p in 1:length(ppt_files))
{

  load(paste0('../data/true_trajectories/', ppt_files[p]))#Load this participant's data
  
  for (b in 1:2)
  {
    for (t in 1:4)
    {
      clip_data<-this_ppt[[b]][[t]]
      # upi<-clip_data$condition_details$upi
      # ppt_number<-clip_data$ppt_number#as.numeric(strsplit(strsplit(ppt_files[ppt_ix], 'T')[[1]][1], 'p')[[1]][2])
      twix<-which(tw.setup$upi==clip_data$condition_details$upi & tw.setup$block==b & tw.setup$trial==t)
      
      orig_res<-data.frame(frame = clip_data$frame,
                      idco = clip_data$idco,
                      mouseX = clip_data$mouseX,
                      mouseY = clip_data$mouseY,
                      oo1.x=clip_data$o1.x,
                      oo2.x=clip_data$o2.x,
                      oo3.x=clip_data$o3.x,
                      oo4.x=clip_data$o4.x,
                      oo1.y=clip_data$o1.y,
                      oo2.y=clip_data$o2.y,
                      oo3.y=clip_data$o3.y,
                      oo4.y=clip_data$o4.y,
                      oo1.vx=clip_data$o1.vx,
                      oo2.vx=clip_data$o2.vx,
                      oo3.vx=clip_data$o3.vx,
                      oo4.vx=clip_data$o4.vx,
                      oo1.vy=clip_data$o1.vy,
                      oo2.vy=clip_data$o2.vy,
                      oo3.vy=clip_data$o3.vy,
                      oo4.vy=clip_data$o4.vy)
      
      subset<-seq(10, 2700, 10)
      res<-orig_res[subset,]
      
      df <-res %>%
        gather(object, x, oo1.x:oo4.x) %>%
        select(frame, object, x) %>%
        mutate(object  = as.numeric(factor(object, levels=c('oo1.x','oo2.x','oo3.x','oo4.x'), labels=1:4)))
      tmp <-res %>%
        gather(object, y, oo1.y:oo4.y)
      df$y<-tmp$y
      tmp2 <-res %>%
        gather(object, vx, oo1.vx:oo4.vx)
      tmp3 <-res %>%
        gather(object, vy, oo1.vy:oo4.vy)
      df$vx<-tmp2$vx
      df$vy<-tmp3$vy
      
      
      true_cond<-c(clip_data$condition_details$local_forces1, clip_data$condition_details$local_forces2, clip_data$condition_details$local_forces3,
            clip_data$condition_details$local_forces4, clip_data$condition_details$local_forces5, clip_data$condition_details$local_forces6)
      true_masses<-c(clip_data$condition_details$massA, clip_data$condition_details$massB,1,1)
      if (true_masses[1]==2 & true_masses[2]==1)
      {
        true_cond[7]<-'A'
      } else if (true_masses[1]==1 & true_masses[2]==2)
      {
        true_cond[7]<-'B'
      }
      
      world_ix<-which(apply(sweep(key, 2, true_cond, '=='), 1, all))
      
      #TODO CHECK FOR REFRESH CODE!
      # tmp<-clip_data$refreshes 
      # clip_data$refreshes<-c(-1,-1,-1)
      # 
      # if (!is.null(tmp))
      # {
      #   for (i in 1:length(tmp))
      #   {
      #     clip_data$refreshes[i]<-tmp[i]
      #   }
      # }
      
      cat('\n Next problem: ppt_ix', p, ' ppt_number:', clip_data$condition_details$ppt, clip_data$condition_details$upi,
          '-- world id:', b, t, 'trial type: ', clip_data$condition_details$trialtype, ' key:', world_ix, '\n')#, 'refreshes: ', tmp)
      
      ct <- new_context()
      
      ct$source("./js/Box2dWeb-2.1.a.3.js")
      ct$assign("original_paths", clip_data)
      ct$assign("snap_at", 10)
      
      # MAIN LOOP
      ############
      for (w in 1:nrow(key))
      {
        masses<-c(1,1,1,1)
        if (key$target_heavier[w]=='A') {  masses[1]<-2 } else if (key$target_heavier[w]=='B') { masses[2]<-2 }
        
        cond <-list(key[w,1],
                    unlist(key[w,2:6]),
                    masses)
        names(cond[[2]])<-NULL
        
        ct$assign("cond", cond)
        ct$source("./js/simulate_passive_io.js")
        dataout<-ct$get("data")
        res<-data.frame(sapply(data.frame(dataout$timeline[,1:17], stringsAsFactors = F), as.numeric))
        
        names(res)<-c('frame','o1.x','o1.y','o1.vx','o1.vy','o2.x','o2.y','o2.vx','o2.vy','o3.x','o3.y','o3.vx','o3.vy','o4.x','o4.y','o4.vx','o4.vy')
        res<-res[,c(1, 2,6,10,14, 3,7,11,15, 4,8,12,16, 5,9,13,17)]
        
        if (w==world_ix)
        {
          sim_res<-res
        }
        
        tmp1 <-res[subset,] %>%
          gather(object, x, o1.x:o4.x)
        tmp2 <-res[subset,] %>%
          gather(object, y, o1.y:o4.y)
        tmp3 <-res[subset,] %>%
          gather(object, vx, o1.vx:o4.vx)
        tmp4 <-res[subset,] %>%
          gather(object, vy, o1.vy:o4.vy)
        df[paste('w', w, 'x', sep='.')]<-tmp1$x
        df[paste('w', w, 'y', sep='.')]<-tmp2$y
        df[paste('w', w, 'vx', sep='.')]<-tmp3$vx
        df[paste('w', w, 'vy', sep='.')]<-tmp4$vy
        
        cat(w, '\n')
      }#w
      # save(file=paste('../data/simulated_trajectories/p',
      #                 tw$ppt[twix], '_', clip_data$condition_details$upi, '_t', t, '_b', b,'.rdata', sep=''), df, key, world_ix, true_cond)
      

  }#trial
}#block
}#ppt


