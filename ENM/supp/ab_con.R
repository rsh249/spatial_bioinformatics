# Distribution modeling of Abies fraseri for model comparison
library(ENMeval)
setup =1;
outdir = '120_contest_rad';
nclus = 2;
ntest=2;
#devtools::install_github("bmaitner/RBIEN/BIEN")
library(BIEN)
library(raster)
#devtools::install_git('https://github.com/rsh249/vegdistmod.git')
library(vegdistmod)
#install.packages('fuzzySim', repos="http://R-Forge.R-project.org")
#library(fuzzySim)
#cite Barbosa (2015) for fuzzySim

options(java.parameters = "-Xmx16g" )



#envirem = get_envirem()
#env.topo = get_envirem_elev()
#env.clim = get_envirem_clim()
#envirem=stack(env.clim, env.topo)
#envirem = brick(envirem)
#names(envirem) = gsub('current_2.5arcmin_', '', names(envirem))
#e.safe = envirem
#ext = extent(-85, -78, 34, 40) #southern apps extent
#phyto <- stack('~/nas2/phytonew.gri')
#phyto <- stack('~/nas2/bio.gri');
#phyto <- stack('~/array1/clients/r_files/phytonew.gri')
#phyto <- crop(phyto, extent(envirem))

#homebrew predictor stack:
##These are a combination of variables from ENVIREM and phytoclim
#master = stack(phyto[[c('wet_sum', 'dry_sum', 'dry_mo')]],
#               envirem[[c('climaticMoistureIndex', 'continentality', 'embergerQ',
  #                        'maxTempColdest', 'minTempWarmest', 'monthCountByTemp10',
   #                       'topoWet', 'tri')]])
#master <- mask(master, envirem[[1]]) #remove the bit of africa and Iceland that stick out past greenland
bio = stack('~/nas2/climgrids/bio.gri');
envirem = stack('~/nas2/climgrids/envirem.gri');

h.ccsm4.env = stack('~/nas2/climgrids/ccsm4/holo_envirem.gri');
h.ccsm4.bio = stack('~/nas2/climgrids/ccsm4/holo_wc.gri');
names(h.ccsm4.bio) = names(bio);

h.miroc.env = stack('~/nas2/climgrids/miroc/holo_envirem.gri');
h.miroc.bio = stack('~/nas2/climgrids/miroc/holo_wc.gri');
names(h.miroc.bio) = names(bio);

h.mpi.env = stack('~/nas2/climgrids/mpi/holo_envirem.gri');
h.mpi.bio = stack('~/nas2/climgrids/mpi/holo_wc.gri');
names(h.mpi.bio) = names(bio);

l.ccsm4.env = stack('~/nas2/climgrids/ccsm4/lgm_envirem.gri');
l.ccsm4.bio = stack('~/nas2/climgrids/ccsm4/lgm_wc.gri');
names(l.ccsm4.bio) = names(bio);

l.miroc.env = stack('~/nas2/climgrids/miroc/lgm_envirem.gri');
l.miroc.bio = stack('~/nas2/climgrids/miroc/lgm_wc.gri');
names(l.miroc.bio) = names(bio);

l.mpi.env = stack('~/nas2/climgrids/mpi/lgm_envirem.gri');
l.mpi.bio = stack('~/nas2/climgrids/mpi/lgm_wc.gri');
names(l.mpi.bio) = names(bio);

#bio <- stack('~/array1/clients/r_files/bio.gri')
#master = stack(bio, phyto)

#ext = extent(c(-85, -78, 34, 42))
#master = bio;
#bio = crop(bio, ext)
#envirem = crop(envirem, ext);

#master <- mask(master, envirem[[1]]) #remove the bit of africa and Iceland that stick out past greenland
bio = stack('~/nas2/climgrids/bio.gri');
envirem = stack('~/nas2/climgrids/envirem.gri');

h.ccsm4.env = stack('~/nas2/climgrids/ccsm4/holo_envirem.gri');
h.ccsm4.bio = stack('~/nas2/climgrids/ccsm4/holo_wc.gri');
names(h.ccsm4.bio) = names(bio);

h.miroc.env = stack('~/nas2/climgrids/miroc/holo_envirem.gri');
h.miroc.bio = stack('~/nas2/climgrids/miroc/holo_wc.gri');
names(h.miroc.bio) = names(bio);

h.mpi.env = stack('~/nas2/climgrids/mpi/holo_envirem.gri');
h.mpi.bio = stack('~/nas2/climgrids/mpi/holo_wc.gri');
names(h.mpi.bio) = names(bio);

l.ccsm4.env = stack('~/nas2/climgrids/ccsm4/lgm_envirem.gri');
l.ccsm4.bio = stack('~/nas2/climgrids/ccsm4/lgm_wc.gri');
names(l.ccsm4.bio) = names(bio);

l.miroc.env = stack('~/nas2/climgrids/miroc/lgm_envirem.gri');
l.miroc.bio = stack('~/nas2/climgrids/miroc/lgm_wc.gri');
names(l.miroc.bio) = names(bio);

l.mpi.env = stack('~/nas2/climgrids/mpi/lgm_envirem.gri');
l.mpi.bio = stack('~/nas2/climgrids/mpi/lgm_wc.gri');
names(l.mpi.bio) = names(bio);

#bio <- stack('~/array1/clients/r_files/bio.gri')
#master = stack(bio, phyto)

#ext = extent(c(-85, -78, 34, 42))
ext = extent(c(-125, -90, 30, 50))
#master = bio;
bio = crop(bio, ext)
envirem = crop(envirem, ext);


h.ccsm4.env = crop(h.ccsm4.env, ext);
h.ccsm4.bio = crop(h.ccsm4.bio, ext);
h.miroc.env = crop(h.miroc.env, ext);
h.miroc.bio = crop(h.miroc.bio, ext);
h.mpi.env = crop(h.mpi.env, ext);
h.mpi.bio = crop(h.mpi.bio, ext);

l.ccsm4.env = crop(l.ccsm4.env, ext);
l.ccsm4.bio = crop(l.ccsm4.bio, ext);
l.miroc.env = crop(l.miroc.env, ext);
l.miroc.bio = crop(l.miroc.bio, ext);
l.mpi.env = crop(l.mpi.env, ext);
l.mpi.bio = crop(l.mpi.bio, ext);





master = stack(envirem, bio);
h.ccsm4.master = stack(h.ccsm4.env, h.ccsm4.bio);
h.miroc.master = stack(h.miroc.env, h.miroc.bio);
h.mpi.master = stack(h.mpi.env, h.mpi.bio);


l.ccsm4.master = stack(l.ccsm4.env, l.ccsm4.bio);
l.miroc.master = stack(l.miroc.env, l.miroc.bio);
l.mpi.master = stack(l.mpi.env, l.mpi.bio);


pred = crop(master, ext)
library(usdm)
v = vifstep(pred, th=10, maxobservations = 1000000)
pred = pred[[unlist(as.character(v@results$Variables))]]
cor<-layerStats(pred,'pearson', na.rm=T)
print(cor)

l.ccsm4.pred = l.ccsm4.master[[unlist(as.character(v@results$Variables))]]
l.miroc.pred = l.miroc.master[[unlist(as.character(v@results$Variables))]]
l.mpi.pred = l.mpi.master[[unlist(as.character(v@results$Variables))]]

h.ccsm4.pred = h.ccsm4.master[[unlist(as.character(v@results$Variables))]]
h.miroc.pred = h.miroc.master[[unlist(as.character(v@results$Variables))]]
h.mpi.pred = h.mpi.master[[unlist(as.character(v@results$Variables))]]


#data = get_dist_all('Abies fraseri', maxrec=10000, local = FALSE)
data = read.table('data2.tab')
data.ex <- 
  extraction(
    data,
    pred,
    schema = 'flat', factor=8,
    rm.outlier = TRUE,
    alpha = 0.01
  )

data.ex = (data.ex[!duplicated(data.ex$cells),])


#bg = dismo::randomPoints(pred[[1]], n = ncell(pred[[1]]))
bg = rad_bg(data.ex[,4:3], pred[[1]], radius = 300, n = 100)
#bg = cbind(rep(1111, nrow(bg)), rep('bg', nrow(bg)), bg)
#bg = cbind(bg[,1], bg[,2], bg[,4], bg[,3])
#bg=as.data.frame(bg)
#colnames(bg) <- c('ind_id', 'tax', 'lat', 'lon')
#bg$lon = as.numeric(as.character(bg$lon))
#bg$lat = as.numeric(as.character(bg$lat))
#bg = extraction(bg, pred, schema='raw')

##Random partition 75:25 train:test sets
#pick = as.numeric(sample(data.ex[, 1], 0.75 * length(data.ex[, 1]), replace =F))
#train = data.ex[which(data.ex[, 1] %in% pick), ]
#test = data.ex[-which(data.ex[, 1] %in% pick), ]



do_sdm_test <- function(extr, 
                        clim, 
                        bg.ext, 
                        whichbest = 'AICc', 
                        nclus = 4,
                        method ='block',
                        parallel=FALSE){
  require('ENMeval')
  bio = clim;
  set.ex = extr
  #bg.ext = rad_bg(set.ex[,4:3], bio, radius=200, n = 20) # Try larger radius
  #set.eval = ENMevaluate(set.ex[,4:3], bio, n.bg=10000, rasterPreds=TRUE, parallel=TRUE, numCores = nclus, method = 'block')
  in.fc = c("L", "Q", "H", "P", "T")
  fc = vector();
  for (i in 1:length(in.fc)){
    comb = combn(in.fc, i)
    fc1 = do.call(paste, as.data.frame(t(comb), stringsAsFactors=FALSE));
    fc = c(fc, fc1)
  }

  fc = gsub(" ", "", fc)
  fc = fc[grep("[LQ]", fc)]
  set.eval = ENMevaluate(set.ex[,4:3], 
                         bio, 
                         bg.coords=bg.ext[,4:3], 
                         rasterPreds=TRUE, 
                         parallel=parallel, 
 		                    # fc = fc,
					fc = fc[1:3],
                         numCores = nclus, 
                         method = method, 
			
                         clamp =TRUE, 
                        # RMvalues=c(0.5,1,1.5,2,2.5,3,3.5,4)
			RMvalues = c(0.5, 2)
                        );	
  set.eval@results
  #return(set.eval@results)
  if(whichbest == 'CV.AUC'){
    cv.AUC = set.eval@results[,'Var.AUC']/set.eval@results[,"Mean.AUC"]
    best=which(cv.AUC == min(cv.AUC));
    
  }
  if(whichbest == 'Mean.AUC'){
    best = which(set.eval@results[,whichbest]==max(na.omit(set.eval@results[,whichbest])))
    
  } 
  if(whichbest == 'AICc' | whichbest=='Mean.AUC.DIFF') {
    best = which(set.eval@results[,whichbest]==min(na.omit(set.eval@results[,whichbest])))
  }
  if(whichbest == 'delta.AICc') {
    best = which(set.eval@results[,whichbest]==min(na.omit(set.eval@results[,whichbest])))
  }
  if(whichbest == 'om.rt') {
    print("Optimizing ommission rate and predictive power...")
    setsort = set.eval@results[order(set.eval@results[,'Mean.ORmin']),]
    setsort2 = setsort[order(setsort[,'Mean.AUC'], decreasing=TRUE),]
    top = setsort2[1,]
    best = which(as.character(set.eval@results[,1]) == as.character(setsort2[1,1]))
  }
 #return(list(best, setsort2, set.eval))
  if(whichbest == 'binaryAUC'){
    print("USING binaryAUC as optimality criteria")
    keep = vector()
    for(i in 1:nlayers(set.eval@predictions)){
      pr = set.eval@predictions[[i]]
      occ <- na.omit(extract(pr, set.ex[,4:3]));
      bg <- na.omit(extract(pr, bg.ext[,4:3]));
      ev <- evaluate(as.numeric(occ>=min(occ)), as.numeric(bg>=min(occ)))
      th = threshold(ev)
      p = pr >= th$no_omission
      ex = extract(p, set.ex[,4:3])
      ex.bg = extract(p, bg.ext[,4:3])
      ev2 <- evaluate(ex, ex.bg)
      keep[[i]] = slot(ev2, 'auc');
    }
    best = which(keep == max(keep));
  }
  #Pick best (Lowest AICc) from results
  pr.set <- predict(set.eval@models[[best]], bio)
  #plot(pr.set)
  ev.set <- evaluate(set.ex[,4:3], set.eval@bg.pts, set.eval@models[[best]], bio)
  thr.set <- threshold(ev.set)

  return(list(pr.set, ev.set, thr.set, set.eval, best));
  
  
}



##Parallel foreach loop:
library(foreach)
library(parallel)
#for(i in 1:20){
cl <- parallel::makeCluster(nclus, type = "SOCK")
# doParallel::registerDoParallel(cl)
doSNOW::registerDoSNOW(cl)
#doMPI::registerDoMPI(cl)
search <-
  foreach::foreach(i = 1:ntest,
                   .combine = 'c',
			.errorhandling = "remove",
                   .packages = c('ENMeval', 'dismo')) %dopar% {
                     
  pick = as.numeric(sample(data.ex[, 1], 0.75 * length(data.ex[, 1]), replace =
                             F))
  train = data.ex[which(data.ex[, 1] %in% pick), ]
  test = data.ex[-which(data.ex[, 1] %in% pick), ]
	options(java.parameters = "-Xmx16g" )

ENMeval.maxent <- tryCatch({
  sdm.all = do_sdm_test(train,
                        pred,
                        bg,
                      #  whichbest = 'AICc',
                      whichbest = 'om.rt',
                       # whichbest = 'binaryAUC',
                        nclus = 25,
                       parallel = TRUE,
#                       method = 'checkerboard2');
                       method = 'block')
  # For picking model parameters on the complete set
  best_param = sdm.all[[4]]@results[sdm.all[[5]],1]
  best_arr = strsplit(as.character(best_param), "_");
  rm = best_arr[[1]][length(best_arr[[1]])];
  fc = best_arr[[1]][1:(length(best_arr[[1]])-1)]
  fc = sapply(strsplit(fc, ""), tolower)
  n =1;
  args = vector();
  if("l" %in% fc){print("linear=true"); args[n]="linear=true"; n=n+1;}
  if("q" %in% fc){print("quadratic=true"); args[n]="quadratic=true"; n=n+1;}
  if("h" %in% fc){print("hinge=true"); args[n]="hinge=true"; n=n+1;}
  if("p" %in% fc){print("product=true"); args[n]="product=true"; n=n+1;}
  if("t" %in% fc){print("threshold=true"); args[n]="threshold=true"; n=n+1;}

	best_mod = maxent(pred, data.ex[,4:3],
                    nbg=10000000,
                    args=c(
                      args,
                      'doclamp=TRUE',
                      paste('betamultiplier=', rm, sep ='')
                    )
                  )

  m = predict(pred, best_mod)
  e.sub = evaluate(test[, 4:3],
                   bg[, 4:3],
                   best_mod,
                   pred)
  th.sub = threshold(e.sub)
  m.ex = extract(m ,
                 data.ex[,4:3])
  omit.10 = quantile(m.ex, 0.1);
# plot(m >= omit.10, col = c('black', 'blue'))
  stack = m>=th.sub$no_omission;

  w.vec = sdm.all[[5]];

  #plot(pr.set>thr.set$no_omission)
        #predict holocene range
                #ccsm4
                h.ccsm4 = predict(h.ccsm4.pred, best_mod)>=th.sub$no_omission;
                #miroc
                h.miroc = predict(h.miroc.pred, best_mod)>=th.sub$no_omission;
                #mpi
                h.mpi = predict( h.mpi.pred, best_mod)>=th.sub$no_omission;
        #predict LGM range
                #ccsm4
                l.ccsm4 = predict(l.ccsm4.pred, best_mod)>=th.sub$no_omission;
                #miroc
                l.miroc = predict(l.miroc.pred, best_mod)>=th.sub$no_omission;
                #mpi
                l.mpi = predict(l.mpi.pred, best_mod)>=th.sub$no_omission;


},
warning = function(w) {
        message(w);
 #       return(NA);
},
error = function(cond) {
  message(cond)
#  return(NA)
})

  return(c(stack, w.vec, best_arr, h.ccsm4, h.miroc, h.mpi, l.ccsm4, l.miroc, l.mpi))
}

stopCluster(cl)

save.image('enmeval_test1.RData')
#q('no');

#odd <- function(x) x%%2 != 0 

#even <- function(x) x%%2 == 0

#realstack = stack(search[odd(seq(1:length(search)))])
#mod.parames = unlist(search[even(seq(1:length(search)))])



ln = ntest*9;
realstack = stack(search[c(seq(1,ln,9))])
mod.parames = unlist(search[c(seq(2,ln,9))])
mod.list = unlist(search[c(seq(3,ln,9))])
h.ccsm4.stack = stack(search[c(seq(4,ln,9))])
h.miroc.stack = stack(search[c(seq(5,ln,9))])
h.mpi.stack = stack(search[c(seq(6,ln,9))])

l.ccsm4.stack = stack(search[c(seq(7,ln,9))])
l.miroc.stack = stack(search[c(seq(8,ln,9))])
l.mpi.stack = stack(search[c(seq(9,ln,9))])

print(summary(mod.list))

usa1 = getData('GADM', country='US', level=1);

png(paste("~/nas2/enmeval", outdir, "maxent_stack.png", sep = '/'))
plot(sum(realstack), col = c("black", rev(heat.colors(888))))
#points(data.ex[, 4:3], cex = 0.2, col = 'green')
plot(usa1, add=TRUE, border = 'white');
dev.off();
#png('~/nas2/enmeval/maxent_all.png')
#plot(realstack, col = c("black", rev(heat.colors(999))))
#dev.off()

##plot holo.ccsm4 figure
png(paste("~/nas2/enmeval", outdir, "maxent_holo_ccsm4_stack.png", sep = '/'))
plot(sum(h.ccsm4.stack), col = c("black", rev(heat.colors(888))))
#points(data.ex[, 4:3], cex = 0.2, col = 'green')
plot(usa1, add=TRUE, border = 'white');
dev.off();

##plot holo.ccsm4 figure
png(paste("~/nas2/enmeval", outdir, "maxent_holo_miroc_stack.png", sep = '/'))
plot(sum(h.miroc.stack), col = c("black", rev(heat.colors(888))))
#points(data.ex[, 4:3], cex = 0.2, col = 'green')
plot(usa1, add=TRUE, border = 'white');
dev.off();

##plot holo.ccsm4 figure
png(paste("~/nas2/enmeval", outdir, "maxent_holo_mpi_stack.png", sep = '/'))
plot(sum(h.mpi.stack), col = c("black", rev(heat.colors(888))))
#points(data.ex[, 4:3], cex = 0.2, col = 'green')
plot(usa1, add=TRUE, border = 'white');
dev.off();

##plot holo.ccsm4 figure
png(paste("~/nas2/enmeval", outdir, "maxent_lgm_ccsm4_stack.png", sep = '/'))
plot(sum(l.ccsm4.stack), col = c("black", rev(heat.colors(888))))
#points(data.ex[, 4:3], cex = 0.2, col = 'green')
plot(usa1, add=TRUE, border = 'white');
dev.off();

##plot holo.ccsm4 figure
png(paste("~/nas2/enmeval", outdir, "maxent_lgm_miroc_stack.png", sep = '/'))
plot(sum(l.miroc.stack), col = c("black", rev(heat.colors(888))))
#points(data.ex[, 4:3], cex = 0.2, col = 'green')
plot(usa1, add=TRUE, border = 'white');
dev.off();

##plot holo.ccsm4 figure
png(paste("~/nas2/enmeval", outdir, "maxent_lgm_mpi_stack.png", sep = '/'))
plot(sum(l.mpi.stack), col = c("black", rev(heat.colors(888))))
#points(data.ex[, 4:3], cex = 0.2, col = 'green')
plot(usa1, add=TRUE, border = 'white');
dev.off();



#png('~/nas2/enmeval/maxent_all.png')
#plot(realstack, col = c("black", rev(heat.colors(999))))
#dev.off()

# ml.dens = tryCatch({
#   d = densform(train, pred, manip = 'condi', bg.n = 200)
#   h = heat_up(pred, 
#               d, 
#               parallel = TRUE, 
#               nclus = 3, 
#               type = '.kde')
#   h = sum(h)
#   ex = extract(h, test[,4:3])
#   ex.bg = extract(h, bg[,4:3])
#   ev = dismo::evaluate(ex, ex.bg);
#   th = threshold(ev)
#   plot(h >= th$spec_sens)
#   points(test[, 4:3], pch = 20, cex = 0.1)
# },
# error = function(cond) {
#   message(cond)
#   return(NA)
# })
# 
# 
# # 
ev.vec = vector()
ml.stack = pred

for(i in 1:ntest){
  print(i)
  pick = as.numeric(sample(data.ex[, 1], 0.75 * length(data.ex[, 1]), replace =
                             F))
  train = data.ex[which(data.ex[, 1] %in% pick), ]
  test = data.ex[-which(data.ex[, 1] %in% pick), ]

ml.dens = tryCatch({
  d = densform(train, pred, manip = 'condi', bg.n = 200)
  h = heat_up(pred,
              d,
              parallel = FALSE,
              nclus = 1,
              type = '.kde')
  h = sum(h)
  ex = extract(h, test[,4:3])
  ex.bg = extract(h, bg[,4:3])
  ev = dismo::evaluate(ex, ex.bg);
  ev.vec[i] = ev@auc;
  th = threshold(ev)
  #plot(h >= th$spec_sens)
  #points(test[, 4:3], pch = 20, cex = 0.1)
  ml.stack[[i]] = h>=th$spec_sens;
},
error = function(cond) {
  message(cond)
  return(NA)
})

}


png(paste("~/nas2/enmeval", outdir, "density_stack.png", sep = '/'))
plot(sum(ml.stack), col = c("black", rev(heat.colors(888))))
#points(data.ex[, 4:3], cex = 0.2, col = 'green')
plot(usa1, add=TRUE, border = 'white');
dev.off();


save.image(paste("~/nas2/enmeval", outdir, "enmeval_test.RData", sep = '/'))
q('no')
