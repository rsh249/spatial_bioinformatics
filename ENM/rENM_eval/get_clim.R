###
#script to download paleoclimate data from worldclim (bioclim) and envirem
library(vegdistmod);

#Set tmp dir in nas3
rasterOptions(tmpdir='/home/rharbert/nas3/climgrids/temp/')


##need models

##Current clim
wc = get_worldclim(period='cur', model='', varset='bio', res=2.5)
wc = wc[[c(1, 12:19, 2:11)]]
writeRaster(wc, file = 'bio');
env.topo = get_envirem_elev(period='cur');
env.clim = get_envirem_clim(period='cur');

envirem = stack(env.clim, env.topo);
writeRaster(envirem, file = 'envirem');




#ccsm4
holo.ccsm4.wc = vegdistmod::get_worldclim(period="midholo", model='ccsm4', varset='bio', res=2.5)
lgm.ccsm4.wc = vegdistmod::get_worldclim(period="lgm", model='ccsm4', varset='bio', res=2.5)
holo.ccsm4.wc = holo.ccsm4.wc[[c(1, 12:19, 2:11)]]
lgm.ccsm4.wc = lgm.ccsm4.wc[[c(1, 12:19, 2:11)]]
writeRaster(holo.ccsm4.wc, file = 'ccsm4/holo_wc');
writeRaster(lgm.ccsm4.wc, file = 'ccsm4/lgm_wc');



holo.ccsm4.env.cl = get_envirem_clim(period='midholo', model='ccsm4')
  holo.env.topo = get_envirem_elev(period='midholo')
    holot.ccsm4.env = stack(holo.ccsm4.env.cl, holo.env.topo);
writeRaster(holot.ccsm4.env, file = 'ccsm4/holo_envirem');

lgm.ccsm4.env.cl = get_envirem_clim(period='lgm', model='ccsm4')
  lgm.env.topo = get_envirem_elev(period='lgm')
    lgm.ccsm4.env = stack(lgm.ccsm4.env.cl, lgm.env.topo);
writeRaster(lgm.ccsm4.env, file = 'ccsm4/lgm_envirem');


#miroc-esm
    holo.miroc.wc = get_worldclim(period="midholo", model='MIROC-ESM', varset='bio', res=2.5)
    holo.miroc.wc = holo.miroc.wc[[c(1, 12:19, 2:11)]]
    lgm.miroc.wc = get_worldclim(period="lgm", model='MIROC-ESM', varset='bio', res=2.5)
    lgm.miroc.wc = lgm.miroc.wc[[c(1, 12:19, 2:11)]]
	writeRaster(holo.miroc.wc, file = 'miroc/holo_wc');
	writeRaster(lgm.miroc.wc, file = 'miroc/lgm_wc');
    

    holo.miroc.env.cl = get_envirem_clim(period='midholo', model='miroc-esm')
   # holo.env.topo = get_envirem_elev(period='midholo')
    holot.miroc.env = stack(holo.miroc.env.cl, holo.env.topo);
	writeRaster(holot.miroc.env, file = 'miroc/holo_envirem');
	
    lgm.miroc.env.cl = get_envirem_clim(period='lgm', model='miroc-esm')
    lgm.miroc.env = stack(lgm.miroc.env.cl, lgm.env.topo);
        writeRaster(lgm.miroc.env, file = 'miroc/lgm_envirem');
    

#mpi-esm

    holo.mpi.wc = get_worldclim(period="midholo", model='MPI-ESM-P', varset='bio', res=2.5)
    holo.mpi.wc = holo.mpi.wc[[c(1, 12:19, 2:11)]]
    lgm.mpi.wc = get_worldclim(period="lgm", model='MPI-ESM-P', varset='bio', res=2.5)
    lgm.mpi.wc = lgm.mpi.wc[[c(1, 12:19, 2:11)]]
       writeRaster(holo.mpi.wc, file = 'mpi/holo_wc');
        writeRaster(lgm.mpi.wc, file = 'mpi/lgm_wc');

    
    holo.mpi.env.cl = get_envirem_clim(period='midholo', model='mpi-esm')
    holot.mpi.env = stack(holo.mpi.env.cl, holo.env.topo);
        writeRaster(holot.mpi.env, file = 'mpi/holo_envirem');

    lgm.mpi.env.cl = get_envirem_clim(period='lgm', model='mpi-esm')
    lgm.mpi.env = stack(lgm.mpi.env.cl, lgm.env.topo);
        writeRaster(lgm.mpi.env, file = 'mpi/lgm_envirem');



    
