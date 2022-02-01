#Extract measures
library(dplyr)
library(tidyr)
library(acl)

rm(list=ls())
load('../data/exp1_data.rdata')
load('../data/empirical_noise_parameters_e1.rdata')



beta<-1/50#
eta<-10 #

filenames<- list.files('../data/io/')

twm<-mutate(tw,
            post_ent.xy = NA,
            post_ent_rel.xy = NA,
            post_ent_mass.xy = NA,
            
            post_ent.rtheta = NA,
            post_ent_rel.rtheta = NA,
            post_ent_mass.rtheta = NA,
            
            post_ent_rel.r = NA,
            post_ent_mass.r = NA,
            post_ent_rel.theta = NA,
            post_ent_mass.theta = NA,
            
            post_att.xy = NA,
            post_rep.xy = NA,

            post_A.xy = NA,
            post_B.xy = NA,
            
            post_att.rtheta = NA,
            post_rep.rtheta = NA,

            post_A.rtheta = NA,
            post_B.rtheta = NA,
            
            pd.rel.xy = NA,
            pd.mass.xy = NA,
            pd.rel.r = NA,
            pd.mass.r = NA,
            pd.rel.theta = NA,
            pd.mass.theta = NA,
            pd.rel.rtheta = NA,
            pd.mass.rtheta = NA,
            
            pd.all_single_dim.xy = NA,
            pd.all_single_dim.r = NA,
            pd.all_single_dim.theta = NA,
            pd.all_single_dim.rtheta = NA,
            
            pd.uncontrol.rel.rtheta = NA,
            pd.uncontrol.mass.rtheta = NA,
            pd.uncontrol.all_single_dim.rtheta = NA,
            pd.control.rel.rtheta = NA,
            pd.control.mass.rtheta = NA,
            pd.control.all_single_dim.rtheta = NA)

start<-proc.time()[1]

# pw<-expand.grid(c(3,0,-3), c(3,0,-3), c(3,0,-3), c(3,0,-3), c(3,0,-3), c(3,0,-3), 0:2)
# names(pw)<- c("target_fAB","fAC","fAD","fBC","fBD","fCD","target_heavier")
# pw<-pw %>% filter(target_fAB!=0, target_heavier!='same')

file<-filenames[1]
for (file in filenames)
{
  load(file=paste('./data/io/', file, sep=''), verbose = T)
  
  upi<-strsplit(file, '_')[[1]][3]
  tt<-as.numeric(substr(strsplit(file, '_')[[1]][4], 2,2))
  bb<-as.numeric(substr(strsplit(file, '_')[[1]][5], 2,2))
  # cv<-as.numeric(strsplit(strsplit(file, 'tt')[[1]][1], 'cv')[[1]][2])
  #as.numeric(strsplit(strsplit(file, 'tt')[[1]][2], split='.r')[[1]][1])
  ix<-which(twm$upi==upi & twm$trial==tt & twm$block==bb)
  swix<-which(sw$upi==upi)
  control<-as.numeric(sapply(clips[[swix]][[bb]][[tt]], '[[', 3)[seq(10, 2700, 10)] !='none')
  cat(file, ix, '\n')
  
  pri<-rep(1/nrow(key), nrow(key)) #Uniform prior
  
  ######################
  #Motion error:
  ######################
  
  #Calculate velocity magnitudes and angles
  df$v.r <- sqrt(df$vx^2 + df$vy^2)
  
 
  df$v.theta = atan2(df$vy, df$vx)
  df$v.theta[df$v.r==0]<-runif(sum(df$v.r==0), -pi, pi)
  
  allw.v.r<-sqrt(select(df, contains('.vx'))^2 + select(df, contains('.vy'))^2)
  
  tmpvy<-select(df, contains('.vy'))
  tmpvx<-select(df, contains('.vx'))
  allw.v.theta<-matrix(atan2(unlist(tmpvy), unlist(tmpvx)), nrow=nrow(tmpvy))
  
  #If the velocity magnitude is zero, the angle is undefined
  allw.v.theta[allw.v.r==0]<-runif(sum(allw.v.r==0), -pi, pi)
  
  allw.vx.tw<-(tmpvx - df$vx) #all squared x distances from true world
  allw.vy.tw<-(tmpvy - df$vy) #all squared y distances from true world
  
  #Absolute velocity magnitude differences between each world and the true world
  allw.r.tw<-sweep(allw.v.r, 1, df$v.r, '-')
  #Log-Ratio velocity magnitude differences between each world and the true world
  
  #Angle differences between each world and true world
  allw.theta.tw<-sweep(allw.v.theta, 1, df$v.theta, '-')
  allw.theta.tw[allw.theta.tw>pi]<-2*pi-allw.theta.tw[allw.theta.tw>pi]
  allw.theta.tw[allw.theta.tw< (-pi)]<- (-2*pi)-allw.theta.tw[allw.theta.tw< (-pi)]
  
  
  #Skip rare cases where the velocity is zero as the angle and magnitude ratio differences are not meaningful
  cat('moments at rest', sum(allw.v.r==0), 'proportion', mean(allw.v.r==0), '\n')
  
  ##########################
  #Euclidean distance error:
  ##########################
  
  allw.x.tw<-(select(df, contains('.x')) - df$x)#x distances from true world
  allw.y.tw<-(select(df, contains('.y')) - df$y)#y distances from true world
  
  
  ###########################
  #All consequent densities:
  ###########################
  density.xy<- exp(-beta*allw.x.tw^2 / (2*noise_params$x^2)) * exp(-beta*allw.y.tw^2 / (2*noise_params$y^2))
  density.r<- exp(-beta*allw.r.tw^2 / (2*noise_params$r^2))
  density.theta<- exp(-beta*allw.theta.tw^2 / (2*noise_params$theta^2))
  density.rtheta<-density.r * density.theta
  
  #####################
  #Posterior stuff----
  #####################
  li.xy<-apply(density.xy, 2, prod)
  li.rtheta<-apply(density.rtheta, 2, prod)

  post.un<-(li.xy * pri)
  post.xy<-post.un/sum(post.un)
  post.un<-(li.rtheta * pri)
  post.rtheta<-post.un/sum(post.un)

  
  #More separate components
  li.r<-apply(density.r, 2, prod)
  li.theta<-apply(density.theta, 2, prod)
  post.un<-(li.r * pri)
  post.r<-post.un/sum(post.un)
  post.un<-(li.theta * pri)
  post.theta<-post.un/sum(post.un)
  
  post_rel.xy = c(sum(post.xy[key[,1]==3]), sum(post.xy[key[,1]==-3]))
  post_mass.xy = c(sum(post.xy[key[,7]=='A']), sum(post.xy[key[,7]=='B']))
  post_rel.rtheta = c(sum(post.rtheta[key[,1]==3]), sum(post.rtheta[key[,1]==-3]))
  post_mass.rtheta = c(sum(post.rtheta[key[,7]=='A']), sum(post.rtheta[key[,7]=='B']))

  post_rel.r = c(sum(post.r[key[,1]==3]), sum(post.r[key[,1]==-3]))
  post_mass.r = c(sum(post.r[key[,7]=='A']), sum(post.r[key[,7]=='B']))
  post_rel.theta = c(sum(post.theta[key[,1]==3]), sum(post.theta[key[,1]==-3]))
  post_mass.theta = c(sum(post.theta[key[,7]=='A']), sum(post.theta[key[,7]=='B']))
  
  twm$post_att.xy[ix]<-post_rel.xy[1]
  twm$post_rep.xy[ix]<-post_rel.xy[2]
  twm$post_A.xy[ix]<-post_mass.xy[1]
  twm$post_B.xy[ix]<-post_mass.xy[2]
  twm$post_att.rtheta[ix]<-post_rel.rtheta[1]
  twm$post_rep.rtheta[ix]<-post_rel.rtheta[2]
  twm$post_A.rtheta[ix]<-post_mass.rtheta[1]
  twm$post_B.rtheta[ix]<-post_mass.rtheta[2]
  
  twm$post_ent.xy[ix]<-shannon_entropy(post.xy, log_type='bits')#TODO do for both
  twm$post_ent_rel.xy[ix]<-shannon_entropy(post_rel.xy, log_type='bits')
  twm$post_ent_mass.xy[ix]<-shannon_entropy(post_mass.xy, log_type='bits')
  twm$post_ent.rtheta[ix]<-shannon_entropy(post.rtheta, log_type='bits')#TODO do for both
  
  twm$post_ent_rel.rtheta[ix]<-shannon_entropy(post_rel.rtheta, log_type='bits')
  twm$post_ent_mass.rtheta[ix]<-shannon_entropy(post_mass.rtheta, log_type='bits')
  
  #Separate likelihood dimension entropies (for fishing)
  twm$post_ent_rel.r[ix]<-shannon_entropy(post_rel.r, log_type='bits')
  twm$post_ent_mass.r[ix]<-shannon_entropy(post_mass.r, log_type='bits')
  twm$post_ent_rel.theta[ix]<-shannon_entropy(post_rel.theta, log_type='bits')
  twm$post_ent_mass.theta[ix]<-shannon_entropy(post_mass.theta, log_type='bits')
  
  
  ##################################################
  #Continuous measures, Predictive Divergence etc
  ##################################################
  
  
  ################
  #Big data frame:
  ################
  #one row per object and frame, one column per distance metric
  df.ext<-data.frame(frame = df$frame, object = df$object, control = control)
  #df.ext[1:10,]
  
  
  # #Euclidean 'likelihood'
  # # ma.gaus<- exp(- beta * (ma.dx + ma.dy)) #all gaussian likelihoods
  # 
  # #The overall mean likelihood truth on every frame for every object (NOT THAT MEANINGFUL)
  # df.ext$pd.truth=rowMeans(density.xy)#exp(- eta * (ma.dx + ma.dy))[,-tw]) #the average gaussian likelihood
  # 
  # #Calculate the distances between the worlds in each combination set for PD---
  # 
  # #Relationship discriminability with row for each frame and object----
  df.att.x<-df[,names(df) %in% paste('w.', which(key[,1]==3), '.x', sep='')]
  # df.none.x<-df[,names(df) %in% paste('w.', which(key[,1]==0), '.x', sep='')]
  df.rep.x<-df[,names(df) %in% paste('w.', which(key[,1]==-3), '.x', sep='')]
  df.att.y<-df[,names(df) %in% paste('w.', which(key[,1]==3), '.y', sep='')]
  # df.none.y<-df[,names(df) %in% paste('w.', which(key[,1]==0), '.y', sep='')]
  df.rep.y<-df[,names(df) %in% paste('w.', which(key[,1]==-3), '.y', sep='')]
  
  df.att.vx<-df[,names(df) %in% paste('w.', which(key[,1]==3), '.vx', sep='')]
  # df.none.vx<-df[,names(df) %in% paste('w.', which(key[,1]==0), '.vx', sep='')]
  df.rep.vx<-df[,names(df) %in% paste('w.', which(key[,1]==-3), '.vx', sep='')]
  df.att.vy<-df[,names(df) %in% paste('w.', which(key[,1]==3), '.vy', sep='')]
  # df.none.vy<-df[,names(df) %in% paste('w.', which(key[,1]==0), '.vy', sep='')]
  df.rep.vy<-df[,names(df) %in% paste('w.', which(key[,1]==-3), '.vy', sep='')]
  df.att.r<-sqrt(df.att.vx^2 + df.att.vy^2)
  # df.none.r<-sqrt(df.none.vx^2 + df.none.vy^2)
  df.rep.r<-sqrt(df.rep.vx^2 + df.rep.vy^2)

  
  df.att.theta<- matrix(atan2(unlist(df.att.vy), unlist(df.att.vx)), nrow=nrow(df.att.vx))
  # df.none.theta<- matrix(atan2(unlist(df.none.vy), unlist(df.none.vx)), nrow=nrow(df.att.vx))
  df.rep.theta<- matrix(atan2(unlist(df.rep.vy), unlist(df.rep.vx)), nrow=nrow(df.att.vx))
  df.att.theta[df.att.r==0]<-runif(sum(df.att.r==0), -pi, pi)
  # df.none.theta[df.none.r==0]<-runif(sum(df.none.r==0), -pi, pi)
  df.rep.theta[df.rep.r==0]<-runif(sum(df.rep.r==0), -pi, pi)
  
  # #Take all the pairwise divergences between the sets of worlds that differ in terms of two levels of the target relationship
  # #Take the mean of all these pairwise divergences
  
  df.ext$pd.rel.xy<-1-apply(exp(-eta*(df.att.x - df.rep.x)^2 / (2*noise_params$x^2)) * 
                               exp(-eta*(df.att.y - df.rep.y)^2 / (2*noise_params$y^2)), 1, mean)
  


  df.ext$pd.rel.r<-1-apply(exp(-eta*(df.att.r - df.rep.r)^2 / (2*noise_params$r^2)), 1, mean)
  
  # apply(cbind(exp(-eta*(df.att.r - df.none.r)^2 / (2*noise_params$r^2)),
  #             exp(-eta*(df.att.r - df.rep.r)^2 / (2*noise_params$r^2)),
  #             exp(-eta*(df.none.r - df.rep.r)^2 / (2*noise_params$r^2))), 1, mean)
  
  #Take shortest route around circle
  # an.th<-df.att.theta - df.none.theta
  # an.th[an.th>pi]   <-   2*pi - an.th[an.th>pi]
  # an.th[an.th<(-pi)]<-(-2*pi) - an.th[an.th<(-pi)]
  ar.th<-df.att.theta - df.rep.theta
  ar.th[ar.th>pi]   <-   2*pi - ar.th[ar.th>pi]
  ar.th[ar.th<(-pi)]<-(-2*pi) - ar.th[ar.th<(-pi)]
  # nr.th<-df.none.theta - df.rep.theta
  # nr.th[nr.th>pi]   <-   2*pi - nr.th[nr.th>pi]
  # nr.th[nr.th<(-pi)]<-(-2*pi) - nr.th[nr.th<(-pi)]
  
  df.ext$pd.rel.theta<-1-apply(exp(-eta*ar.th^2 / (2*noise_params$theta^2)), 1, mean)
    # apply(cbind(exp(-eta*an.th^2 / (2*noise_params$theta^2)),
    #             exp(-eta*ar.th^2 / (2*noise_params$theta^2)),
    #             exp(-eta*nr.th^2 / (2*noise_params$theta^2))), 1, mean)
  
  
  df.ext$pd.rel.rtheta<-1-apply(exp(-eta*ar.th^2 / (2*noise_params$theta^2)) * 
                                  exp(-eta*(df.att.r - df.rep.r)^2 / (2*noise_params$r^2)), 1, mean)
    
    # apply(cbind(exp(-eta*an.th^2 / (2*noise_params$theta^2)) * 
    #               exp(-eta*(df.att.r - df.none.r)^2 / (2*noise_params$r^2)),
    #             exp(-eta*ar.th^2 / (2*noise_params$theta^2)) *
    #               exp(-eta*(df.att.r - df.rep.r)^2 / (2*noise_params$r^2)),
    #             exp(-eta*nr.th^2 / (2*noise_params$theta^2)) *
    #               exp(-eta*(df.none.r - df.rep.r)^2 / (2*noise_params$r^2))), 1, mean)
  

  
  for (i in 2:6)
  {
    df.att.x<-df[,names(df) %in% paste('w.', which(key[,i]==3), '.x', sep='')]
    df.none.x<-df[,names(df) %in% paste('w.', which(key[,i]==0), '.x', sep='')]
    df.rep.x<-df[,names(df) %in% paste('w.', which(key[,i]==-3), '.x', sep='')]
    df.att.y<-df[,names(df) %in% paste('w.', which(key[,i]==3), '.y', sep='')]
    df.none.y<-df[,names(df) %in% paste('w.', which(key[,i]==0), '.y', sep='')]
    df.rep.y<-df[,names(df) %in% paste('w.', which(key[,i]==-3), '.y', sep='')]
    
    df.att.vx<-df[,names(df) %in% paste('w.', which(key[,i]==3), '.vx', sep='')]
    df.none.vx<-df[,names(df) %in% paste('w.', which(key[,i]==0), '.vx', sep='')]
    df.rep.vx<-df[,names(df) %in% paste('w.', which(key[,i]==-3), '.vx', sep='')]
    df.att.vy<-df[,names(df) %in% paste('w.', which(key[,i]==3), '.vy', sep='')]
    df.none.vy<-df[,names(df) %in% paste('w.', which(key[,i]==0), '.vy', sep='')]
    df.rep.vy<-df[,names(df) %in% paste('w.', which(key[,i]==-3), '.vy', sep='')]
    df.att.r<-sqrt(df.att.vx^2 + df.att.vy^2)
    df.none.r<-sqrt(df.none.vx^2 + df.none.vy^2)
    df.rep.r<-sqrt(df.rep.vx^2 + df.rep.vy^2)
    
    df.att.theta<- matrix(atan2(unlist(df.att.vy), unlist(df.att.vx)), nrow=nrow(df.att.vx))
    df.none.theta<- matrix(atan2(unlist(df.none.vy), unlist(df.none.vx)), nrow=nrow(df.att.vx))
    df.rep.theta<- matrix(atan2(unlist(df.rep.vy), unlist(df.rep.vx)), nrow=nrow(df.att.vx))
    df.att.theta[df.att.r==0]<-runif(sum(df.att.r==0), -pi, pi)
    df.none.theta[df.none.r==0]<-runif(sum(df.none.r==0), -pi, pi)
    df.rep.theta[df.rep.r==0]<-runif(sum(df.rep.r==0), -pi, pi)
    
    # #Take all the pairwise divergences between the sets of worlds that differ in terms of two levels of the target relationship
    # #Take the mean of all these pairwise divergences
    
    df.ext[[paste0('pd.d', i, '.xy')]]<-1-apply(cbind(exp(-eta*(df.att.x - df.none.x)^2 / (2*noise_params$x^2)) * exp(-eta*(df.att.y - df.none.y)^2 / (2*noise_params$y^2)),
                                                      exp(-eta*(df.att.x - df.rep.x)^2 / (2*noise_params$x^2)) * exp(-eta*(df.att.y - df.rep.y)^2 / (2*noise_params$y^2)),
                                                      exp(-eta*(df.none.x - df.rep.x)^2 / (2*noise_params$x^2)) * exp(-eta*(df.none.y - df.rep.y)^2 / (2*noise_params$y^2))), 1, mean)

    df.ext[[paste0('pd.d', i, '.r')]]<-1-apply(cbind(exp(-eta*(df.att.r - df.none.r)^2 / (2*noise_params$r^2)),
                                                     exp(-eta*(df.att.r - df.rep.r)^2 / (2*noise_params$r^2)),
                                                     exp(-eta*(df.none.r - df.rep.r)^2 / (2*noise_params$r^2))), 1, mean)

    #Take shortest route around circle
    an.th<-df.att.theta - df.none.theta
    an.th[an.th>pi]   <-   2*pi - an.th[an.th>pi]
    an.th[an.th<(-pi)]<-(-2*pi) - an.th[an.th<(-pi)]
    ar.th<-df.att.theta- df.rep.theta
    ar.th[ar.th>pi]   <-   2*pi - ar.th[ar.th>pi]
    ar.th[ar.th<(-pi)]<-(-2*pi) - ar.th[ar.th<(-pi)]
    nr.th<-df.none.theta - df.rep.theta
    nr.th[nr.th>pi]   <-   2*pi - nr.th[nr.th>pi]
    nr.th[nr.th<(-pi)]<-(-2*pi) - nr.th[nr.th<(-pi)]
    
    df.ext[[paste0('pd.d', i, '.theta')]]<-1-apply(cbind(exp(-eta*an.th^2 / (2*noise_params$theta^2)),
                  exp(-eta*ar.th^2 / (2*noise_params$theta^2)),
                  exp(-eta*nr.th^2 / (2*noise_params$theta^2))), 1, mean)
    
    
    df.ext[[paste0('pd.d', i, '.rtheta')]]<-1-apply(cbind(exp(-eta*an.th^2 / (2*noise_params$theta^2)) *
                    exp(-eta*(df.att.r - df.none.r)^2 / (2*noise_params$r^2)),
                  exp(-eta*ar.th^2 / (2*noise_params$theta^2)) *
                    exp(-eta*(df.att.r - df.rep.r)^2 / (2*noise_params$r^2)),
                  exp(-eta*nr.th^2 / (2*noise_params$theta^2)) *
                    exp(-eta*(df.none.r - df.rep.r)^2 / (2*noise_params$r^2))), 1, mean)
  }
  
  # #Mass
  # df.same.x<-df[,names(df) %in% paste('w.', which(key[,7]==0), '.x', sep='')]
  df.A.x<-df[,names(df) %in% paste('w.', which(key[,7]=='A'), '.x', sep='')]
  df.B.x<-df[,names(df) %in% paste('w.', which(key[,7]=='B'), '.x', sep='')]
  
  # df.same.y<-df[,names(df) %in% paste('w.', which(key[,7]==0), '.y', sep='')]
  df.A.y<-df[,names(df) %in% paste('w.', which(key[,7]=='A'), '.y', sep='')]
  df.B.y<-df[,names(df) %in% paste('w.', which(key[,7]=='B'), '.y', sep='')]
  
  
  # df.same.vx<-df[,names(df) %in% paste('w.', which(key[,7]==0), '.vx', sep='')]
  df.A.vx<-df[,names(df) %in% paste('w.', which(key[,7]=='A'), '.vx', sep='')]
  df.B.vx<-df[,names(df) %in% paste('w.', which(key[,7]=='B'), '.vx', sep='')]
  # df.same.vy<-df[,names(df) %in% paste('w.', which(key[,7]==0), '.vy', sep='')]
  df.A.vy<-df[,names(df) %in% paste('w.', which(key[,7]=='A'), '.vy', sep='')]
  df.B.vy<-df[,names(df) %in% paste('w.', which(key[,7]=='B'), '.vy', sep='')]
  # df.same.r<-sqrt(df.same.vx^2 + df.same.vy^2)
  df.A.r<-sqrt(df.A.vx^2 + df.A.vy^2)
  df.B.r<-sqrt(df.B.vx^2 + df.B.vy^2)
  
  # df.same.theta<- matrix(atan2(unlist(df.same.vy), unlist(df.same.vx)), nrow=nrow(df.same.vx))
  df.A.theta<- matrix(atan2(unlist(df.A.vy), unlist(df.A.vx)), nrow=nrow(df.A.vx))
  df.B.theta<- matrix(atan2(unlist(df.B.vy), unlist(df.B.vx)), nrow=nrow(df.B.vx))
  # df.same.theta[df.same.r==0]<-runif(sum(df.same.r==0), -pi, pi)
  df.A.theta[df.A.r==0]<-runif(sum(df.A.r==0), -pi, pi)
  df.B.theta[df.B.r==0]<-runif(sum(df.B.r==0), -pi, pi)
  
  df.ext$pd.mass.xy<-1-apply(exp(-(df.A.x - df.B.x)^2 / (2*noise_params$x^2)) * exp(-(df.A.y - df.B.y)^2 / (2*noise_params$y^2)), 1, mean)
    # apply(cbind(exp(-(df.same.x - df.A.x)^2 / (2*noise_params$x^2)) * exp(-(df.same.y - df.A.y)^2 / (2*noise_params$y^2)),
    #             exp(-(df.same.x - df.B.x)^2 / (2*noise_params$x^2)) * exp(-(df.same.y - df.B.y)^2 / (2*noise_params$y^2)),
    #             exp(-(df.A.x - df.B.x)^2 / (2*noise_params$x^2)) * exp(-(df.A.y - df.B.y)^2 / (2*noise_params$y^2))), 1, mean)
    
  df.ext$pd.mass.r<-1-apply(exp(-eta*(df.A.r - df.B.r)^2 / (2*noise_params$r^2)), 1, mean)
    # apply(cbind(exp(-eta*(df.same.r - df.A.r)^2 / (2*noise_params$r^2)),
    #             exp(-eta*(df.same.r - df.B.r)^2 / (2*noise_params$r^2)),
    #             exp(-eta*(df.A.r - df.B.r)^2 / (2*noise_params$r^2))), 1, mean)
  

  #Take shortest route around circle
  # sa.th<-df.same.theta - df.A.theta
  # sa.th[sa.th>pi]   <-   2*pi - sa.th[sa.th>pi]
  # sa.th[sa.th<(-pi)]<-(-2*pi) - sa.th[sa.th<(-pi)]
  # sb.th<-df.same.theta- df.B.theta
  # sb.th[sb.th>pi]   <-   2*pi - sb.th[sb.th>pi]
  # sb.th[sb.th<(-pi)]<-(-2*pi) - sb.th[sb.th<(-pi)]
  ab.th<-df.A.theta - df.B.theta
  ab.th[ab.th>pi]   <-   2*pi - ab.th[ab.th>pi]
  ab.th[ab.th<(-pi)]<-(-2*pi) - ab.th[ab.th<(-pi)]
  
  df.ext$pd.mass.theta<-1-apply(exp(-eta*ab.th^2 / (2*noise_params$theta^2)), 1, mean)
    # apply(cbind(exp(-eta*sa.th^2 / (2*noise_params$theta^2)),
    #             exp(-eta*sb.th^2 / (2*noise_params$theta^2)),
    #             exp(-eta*ab.th^2 / (2*noise_params$theta^2))), 1, mean)
  
  
  df.ext$pd.mass.rtheta<-1- apply(exp(-eta*ar.th^2 / (2*noise_params$theta^2)) *
    exp(-eta*(df.A.r - df.B.r)^2 / (2*noise_params$r^2)), 1, mean)
    # apply(cbind(exp(-eta*an.th^2 / (2*noise_params$theta^2)) *
    #               exp(-eta*(df.same.r - df.A.r)^2 / (2*noise_params$r^2)),
    #             exp(-eta*ar.th^2 / (2*noise_params$theta^2)) *
    #               exp(-eta*(df.same.r - df.B.r)^2 / (2*noise_params$r^2)),
    #             exp(-eta*nr.th^2 / (2*noise_params$theta^2)) *
    #               exp(-eta*(df.A.r - df.B.r)^2 / (2*noise_params$r^2))), 1, mean)
  
  df.ext$upi<-upi
  df.ext$trial<-tt
  df.ext$block<-bb
  df.ext$block_type<-twm$blocktype[ix]
  df.ext$trial_type<-twm$trialtype[ix]
  
  save(file=paste0('./data/pd_summaries/', file), df.ext)
  
  twm$pd.rel.xy[ix]<-mean(df.ext$pd.rel.xy)
  twm$pd.mass.xy[ix]<-mean(df.ext$pd.mass.xy)
  twm$pd.rel.r[ix]<-mean(df.ext$pd.rel.r)
  twm$pd.mass.r[ix]<-mean(df.ext$pd.mass.r)
  twm$pd.rel.theta[ix]<-mean(df.ext$pd.rel.theta)
  twm$pd.mass.theta[ix]<-mean(df.ext$pd.mass.theta)
  twm$pd.rel.rtheta[ix]<-mean(df.ext$pd.rel.rtheta)
  twm$pd.mass.rtheta[ix]<-mean(df.ext$pd.mass.rtheta)
  
  
  twm$pd.all_single_dim.xy[ix]<-mean(as.matrix( select(df.ext, contains('.xy'), -contains('theta'))))
  twm$pd.all_single_dim.r[ix]<-mean(as.matrix( select(df.ext, contains('.r'), -contains('theta'), -contains('xy'))))
  twm$pd.all_single_dim.theta[ix]<-mean(as.matrix( select(df.ext, contains('.theta'))))
  twm$pd.all_single_dim.rtheta[ix]<-mean(as.matrix( select(df.ext, contains('.rtheta'), -contains('xy'))))
  
  
  twm$pd.uncontrol.rel.rtheta[ix]<-mean(df.ext$pd.rel.rtheta[df.ext$control==0])
  twm$pd.uncontrol.mass.rtheta[ix]<-mean(df.ext$pd.mass.rtheta[df.ext$control==0])
  twm$pd.uncontrol.all_single_dim.rtheta[ix]<-mean(as.matrix( select(df.ext, contains('.rtheta'), -contains('xy'))[df.ext$control==0,]))
  twm$pd.control.rel.rtheta[ix]<-mean(df.ext$pd.rel.rtheta[df.ext$control==1])
  twm$pd.control.mass.rtheta[ix]<-mean(df.ext$pd.mass.rtheta[df.ext$control==1])
  twm$pd.control.all_single_dim.rtheta[ix]<-mean(as.matrix( select(df.ext, contains('.rtheta'), -contains('xy'))[df.ext$control==1,]))
  
  cat(file, upi, bb, tt, 'ix:', ix, '\n')
  
}
save(file='../data/temp_vel_beta1_50_eta10.rdata', twm)



load('../data/exp1_data.rdata')
load('../data/temp_vel_beta1_50_eta10.rdata')

swm<-sw %>% select( -contains('mdm.'), -contains('mmd.'), -contains('post'), -contains('pd.')) %>% mutate(
              post_ent.xy = NA,
              post_ent_rel.xy = NA,
              post_ent_mass.xy = NA,
              
              post_ent.rtheta = NA,
              post_ent_rel.rtheta = NA,
              post_ent_mass.rtheta = NA,
            
              post_ent_rel.r = NA,
              post_ent_mass.r = NA,
              post_ent_rel.theta = NA,
              post_ent_mass.theta = NA,
              
              post_att.xy = NA,
              post_rep.xy = NA,
              post_A.xy = NA,
              post_B.xy = NA,
              post_att.rtheta = NA,
              post_rep.rtheta = NA,
              post_A.rtheta = NA,
              post_B.rtheta = NA,
              
              pd.rel.xy = NA,
              pd.mass.xy = NA,
              pd.rel.r = NA,
              pd.mass.r = NA,
              pd.rel.theta = NA,
              pd.mass.theta = NA,
              pd.rel.rtheta = NA,
              pd.mass.rtheta = NA,
              
              pd.all_single_dim.xy = NA,
              pd.all_single_dim.r = NA,
              pd.all_single_dim.theta = NA,
              pd.all_single_dim.rtheta = NA,
              
              pd.uncontrol.rel.rtheta = NA,
              pd.uncontrol.mass.rtheta = NA,
              pd.uncontrol.all_single_dim.rtheta = NA,
              
              pd.control.rel.rtheta = NA,
              pd.control.mass.rtheta = NA,
              pd.control.all_single_dim.rtheta = NA)

#Add measures to shorter df, 1 entry per participant
for (i in 1:nrow(swm))
{
  ix<-which(twm$upi==swm$upi[i])
   
  cat('i', i, 'ix', ix,  '\n')
  column_ix<-which(names(swm)=='post_ent.xy'):length(names(swm))
    for (j in column_ix)
    {
      cur_var<-names(swm)[j]
      swm[[cur_var]][i]<-mean(twm[[cur_var]][ix], na.rm=T)
    }
}

head(swm %>% filter(trialdata_complete==T))
tail(swm)

save(file='../data/e1_passive_io.rdata', twm, swm)


rm(list=ls())

load('../data/e1_passive_io.rdata')
twm<-twm %>% select(names(twm)[1:24], contains('rtheta'), -contains('log'), -contains('xy'))
swm<-swm %>% select(names(swm)[1:24], contains('rtheta'), -contains('log'), -contains('xy'))

head(swm %>% filter(trialdata_complete))
head(twm %>% filter(trialdata_complete))
save(file='../data/e1_passive_io_rtheta.rdata', swm, twm)

#There were three exclusions that I forgot about in first pass analysis. this is removing them from these dfs.
rm(list=ls())
load('../data/e1_passive_io_rtheta.rdata')
head(swm)

lauras_exclusions1<-swm$upi%in%c('ppxjanedjj','yuafrcikvt','uuzmrjuzba')
swm<-swm[!lauras_exclusions1,]

lauras_exclusions2<-twm$upi%in%c('ppxjanedjj','yuafrcikvt','uuzmrjuzba')
twm<-twm[!lauras_exclusions2,]
save(file='../data/e1_passive_io_rtheta.rdata', swm, twm)
