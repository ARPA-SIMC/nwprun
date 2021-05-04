#!/usr/bin/env python3
# encoding: utf-8

import os, sys, pygrib, numpy as np
import glob
import pandas as pd
import cartopy, cartopy.crs as ccrs, cartopy.feature as cfeature
import matplotlib, matplotlib.pyplot as plt

from matplotlib import colors
from matplotlib.colors import from_levels_and_colors
from math import *
from osgeo import gdal
from argparse import ArgumentParser
from datetime import datetime,timedelta

# ------------------------------------------------------------------------------
# PLOT GRIB CON PYTHON 
# ------------------------------------------------------------------------------
# DESCRIZIONE
# Lo script plotta i campi in output a COSMO-2I-EPS

# ------------------------------------------------------------------------------
# FUNZIONI
# ------------------------------------------------------------------------------
def get_var_grib(grib_data):
#-------------------------------------------------------------------------------
# Ricavo alcune variabili dal grib (tutte le keys sono in grb.key())
#-------------------------------------------------------------------------------
    medi=None
    perc=None
    thresh=None
    variabile = grib_data.shortName
    data      = "%08d"%grib_data.dataDate
    ora       = "%04d"%grib_data.dataTime
    Ni        = int(grib_data['Ni'])
    Nj        = int(grib_data['Nj'])
    startstep = "%02d"%grib_data.startStep
    endstep   = "%02d"%grib_data.endStep
    itr       = int(grib_data.indicatorOfUnitForTimeRange)
    ltr       = int(grib_data.lengthOfTimeRange)
    if itr==0:
        cumstep=ltr/60
    if grib_data.has_key('derivedForecast'): medi = grib_data.derivedForecast
    if grib_data.has_key('percentileValue'): perc = grib_data.percentileValue
    if grib_data.has_key('scaleFactorOfLowerLimit') and grib_data.has_key('scaledValueOfLowerLimit'):
        sf        = grib_data.scaleFactorOfLowerLimit
        sv        = grib_data.scaledValueOfLowerLimit
        if sf>0: 
            thresh=sv/(int('1'.ljust(abs(sf),'0'))*10)
        elif sf==0:
            thresh=sv
        else:
            thresh=sv*int('1'.ljust(abs(sf),'0'))*10
#    print("\nVariabile: ", variabile,   "\nData:      ", data,
#          "\nOra:       ", ora,         "\nScadenza: +", endstep,
#          "\nSoglia:    ", thresh,      "\nMedia:     ", medi,
#          "\nPercentile  ", perc, sep="")

    return variabile,data,ora,Ni,Nj,startstep,endstep,thresh,medi,perc,cumstep


def rotated_grid_transform(lon_arr, lat_arr, option, SP_coor):
#-------------------------------------------------------------------------------
# Trasformazione di coordinate da "regular latlon" a "rotated latlon"
# (option=1) e viceversa (option=0)
#-------------------------------------------------------------------------------
    # Conversione da gradi a radianti
    lon = (lon_arr*pi)/180
    lat = (lat_arr*pi)/180

    # Salvo coordinate del Southern Pole
    SP_lon, SP_lat = SP_coor[0], SP_coor[1];

    # Definisco l'angolo (in radianti) di rotazione attorno all'asse y (theta)
    # e z (phi)
    theta = (90+SP_lat)*pi/180
    phi   = (SP_lon)*pi/180

    # Converto da coordinate sferiche a cartesiane
    x = np.multiply(np.cos(lon), np.cos(lat)) 
    y = np.multiply(np.sin(lon), np.cos(lat))
    z = np.sin(lat);

    if option == 1: # Regular -> Rotated 
        cosphi_x = np.multiply(np.cos(phi), x)
        senphi_y = np.multiply(np.sin(phi), y)
        x_new =  np.multiply(cosphi_x, np.cos(theta)) + \
                 np.multiply(senphi_y, np.cos(theta)) + np.multiply(np.sin(theta), z)
        y_new = -np.multiply(np.sin(phi), x) + np.multiply(np.cos(phi), y)
        z_new = -np.multiply(cosphi_x, np.sin(theta)) - \
                 np.multiply(senphi_y, np.sin(theta)) + np.multiply(np.cos(theta), z)

    else:  # Rotated -> Regular
        phi = -phi;
        theta = -theta;

        costheta_x = np.multiply(np.cos(theta), x)
        sentheta_z = np.multiply(np.sin(theta), z)

        x_new =  np.multiply(costheta_x, np.cos(phi)) + np.multiply(np.sin(phi), y) + \
                 np.multiply(sentheta_z, np.cos(phi))
        y_new = -np.multiply(costheta_x, np.sin(phi)) + np.multiply(np.cos(phi), y) - \
                 np.multiply(sentheta_z, np.sin(phi))
        z_new = -np.multiply(np.sin(theta), x)        + np.multiply(np.cos(theta), z)

    # Ri-converto da coordinate cartesiane a sferiche e trasformo in radianti
    lon_new = (np.arctan2(y_new,x_new))*180/pi
    lat_new = (np.arcsin(z_new))*180/pi

    return lon_new,lat_new

# ------------------------------------------------------------------------------
# LETTURA DEI FILE PRESENTI NELLA DIRECTORY DI OUTPUT
# ------------------------------------------------------------------------------
if len(sys.argv) > 1:
    fname_reg=sys.argv[1]
else:
    fname_reg='Regioni_latlon.tif'
if len(sys.argv) > 2:
    fname_prov=sys.argv[2]
else:
    fname_prov='ProvinceER_latlon.tif'
if len(sys.argv) > 3:
    path_in=sys.argv[3]
else:
    path_in='/autofs/scratch-rad/vpoli/FCST_PROB/fxtr/data/tmp'
if len(sys.argv) > 4:
    fold_out=sys.argv[4]
else:
    fold_out='tmp'

# Creo la directory "fold_out" se non esiste
if not os.path.exists("%s"%fold_out):
    os.makedirs("%s"%fold_out)

# Apertura e lettura dei tif contenenti i confini amministrativi
#fname_reg='Regioni_latlon.tif'
ds_reg=gdal.Open(fname_reg)
datashp_reg=ds_reg.ReadAsArray()
gt_reg=ds_reg.GetGeoTransform()
shpext_reg=(gt_reg[0],gt_reg[0]+ds_reg.RasterXSize*gt_reg[1],
            gt_reg[3]+ds_reg.RasterYSize*gt_reg[5],gt_reg[3])

#fname_prov='ProvinceER_latlon.tif'
ds_prov=gdal.Open(fname_prov)
datashp_prov=ds_prov.ReadAsArray()
gt_prov=ds_prov.GetGeoTransform()
shpext_prov=(gt_prov[0],gt_prov[0]+ds_prov.RasterXSize*gt_prov[1],
             gt_prov[3]+ds_prov.RasterYSize*gt_prov[5],gt_prov[3])


# ------------------------------------------------------------------------------
# Ciclo sui file presenti nella directory
for fname in glob.glob(path_in):
    base = os.path.basename(fname)
    if os.path.isfile(fname) and base.endswith('.grib') and (base.startswith('tpp') or base.startswith('vmax')):

        print("Lettura file:", fname)
        grbs=pygrib.open(fname)
        grbs=grbs.select()

        # Definisco le variabili necessarie alla grafica 
        var=[None]*len(grbs)
        media=[None]*len(grbs)
        percentile=[None]*len(grbs)
        soglia=[None]*len(grbs)
        campi=[None]*len(grbs)

        n=0
        # Lettura di tutti i campi del grib 
        for grb in grbs:
            variabile,data,ora,Ni,Nj,startstep,endstep,thresh,medi,perc,cumstep=get_var_grib(grb)
            var[n]=variabile
            media[n]=medi
            percentile[n]=perc
            soglia[n]=thresh
            campi[n]=grb.values
            #print(n, campi[n].min(),campi[n].max())
            #    print(n, campi[n].shape)

            # Al primo record calcolo il grigliato
            if n==0:
                # -----------------------------------
                # CREAZIONE GRIGLIA E RITAGLIO
                # dal programma di Thomas
                # -----------------------------------
                # Se la griglia non è "rotated_ll" o "regular_ll",
                # non so gestirla quindi esco 
                if not (grb.gridType == "rotated_ll"  or  grb.gridType == "regular_ll"):
                    sys.exit("\nERRORE! La griglia %s non è gestita\n"%grb.gridType)
                    
                # Ricavo latitudine e longirudine del primo e dell'ultimo punto
                # della griglia e controllo la consistenza dei valori di
                # longitudine
                lat_first_point=float(grb['latitudeOfFirstGridPointInDegrees'])
                lon_first_point=float(grb['longitudeOfFirstGridPointInDegrees'])
                lat_last_point=float(grb['latitudeOfLastGridPointInDegrees'])
                lon_last_point=float(grb['longitudeOfLastGridPointInDegrees'])
                if lon_first_point>lon_last_point:
                    lon_first_point=lon_first_point-360
                    
                # Ricavo longitudini e latitudini
                lons=np.linspace(lon_first_point,lon_last_point,Ni)
                lats=np.linspace(lat_first_point,lat_last_point,Nj)
                grid_lon,grid_lat=np.meshgrid(lons,lats)

                # Se la griglia è "rotated_ll", la ruoto
                if grb.gridType=="rotated_ll":
                    SP=[float(grb['longitudeOfSouthernPoleInDegrees']),
                        float(grb['latitudeOfSouthernPoleInDegrees'])]
                    grid_lon,grid_lat=rotated_grid_transform(grid_lon,grid_lat,0,SP)

                # Controllo che la griglia sia a posto
                if ((np.shape(campi[n]) != np.shape(grid_lon)) or \
                    (np.shape(campi[n]) != np.shape(grid_lat))):
                    sys.exit("ERRORE! La griglia spaziale non coincide con quella della variabile!")

            n=n+1

# ----------------------------------------------------------------------
# PLOT
# ----------------------------------------------------------------------
# Do per scontato che la variabile sia uguale per tutti i campi del grib
# In alternativa si può inserire un controllo del tipo:
# if all(x==var[0] for x in var):

#----------------------------------------------------------------------------
# PRIMA IMMAGINE - ensmean, max, min, 90esimo percentile
        if var[0]=='tprate':

            # Set-up mappa
            npanel=4
            fig,axs=plt.subplots(2,2,figsize=(12,11),subplot_kw={'projection': ccrs.PlateCarree()})
            fig.subplots_adjust(hspace=0.07, wspace=0.05)
            font={'size': 8}
            plt.rc('font',**font)

            livelli=[0,1,5,10,20,30,50,70,100,150,200,300,500]
            units='[mm]'
            colori=['#f0f0f0','#d0d1e6','#a6bddb',
                    '#74a9cf','#2b8cbe','#045a8d',
                    '#fecc5c','#fd8d3c','#e31a1c',
                    '#dd3497','#ae017e','#7a0177']
            cmap,norm=from_levels_and_colors(livelli,colori,extend='neither')

            # Seleziono i campi che mi interessano:
            mappe=[None]*npanel
            var_tit=[None]*npanel
            # 1) Ensemble mean
            ind=[x for x in range(len(grbs)) if (soglia[x]==None) & (media[x]==0) & (percentile[x]==None)]
            mappe[0]=campi[ind[0]]
            mappe[0][mappe[0]<0]=0.
            var_tit[0]="Mean precipitation"
            # 2) Max
            ind=[x for x in range(len(grbs)) if (soglia[x]==None) & (media[x]==None) & (percentile[x]==100)]
            mappe[1]=campi[ind[0]]
            mappe[1][mappe[1]<0]=0.
            var_tit[1]="Maximum precipitation"
            # 3) Min
            ind=[x for x in range(len(grbs)) if (soglia[x]==None) & (media[x]==None) & (percentile[x]==0)]
            mappe[2]=campi[ind[0]]
            mappe[2][mappe[2]<0]=0.
            var_tit[2]="Minimum precipitation"
            # 4) 90esimo percentile
            ind=[x for x in range(len(grbs)) if (soglia[x]==None) & (media[x]==None) & (percentile[x]==90)]
            mappe[3]=campi[ind[0]]
            mappe[3][mappe[3]<0]=0.
            var_tit[3]="90th percentile precipitation"

            for col in range(2):
                for row in range(2):
                    ax=axs[col,row]

                    pcm=ax.pcolormesh(grid_lon,grid_lat,mappe[col*2+row],cmap=cmap,norm=norm)
                    # Geografia (grande scala)
                    ax.coastlines(resolution='10m')
                    ax.add_feature(cfeature.NaturalEarthFeature(category='cultural', 
                                                               name='admin_0_boundary_lines_land', scale='10m'),
                                   edgecolor='k', facecolor='none')
                    ax.add_feature(cartopy.feature.LAKES.with_scale('10m'), edgecolor='k', facecolor='none')
                    # Regioni
                    limiti=ax.get_extent()                    
                    ax.imshow(datashp_reg[:,:,:].transpose((1,2,0)),extent=shpext_reg,origin='upper',zorder=3)
                    ax.set_extent(limiti)

                    # Plot griglia con labels di latitudine e longitudine a sinistra
                    # e in basso
                    gl=ax.gridlines(linestyle='--')
                    #gl=ax.gridlines(draw_labels=True,linestyle='--')
                    #gl.top_labels=False
                    #gl.right_labels=False

                    # Titolo
                    sep='\n'
                    inizio=datetime.strptime(data+ora,'%Y%m%d%H%M')
                    data_in=datetime.strptime(data+ora,'%Y%m%d%H%M')+timedelta(hours=int(startstep))
                    data_fin=datetime.strptime(data+ora,'%Y%m%d%H%M')+timedelta(hours=int(endstep))
                    # Completamento con data e ora
                    title="%s %s%s - %s"%(var_tit[col*2+row],sep,data_in.strftime('%Y/%m/%d %H:%M'),
                                          data_fin.strftime('%Y/%m/%d %H:%M'))
                    ax.set_title(title,loc='left')

            # Aggiungo un'unica colorbar        
            fig.subplots_adjust(right=0.9)
            cax=fig.add_axes([0.93,0.15,0.02,0.7],title=units)
            cax.tick_params(labelsize=10)
            clb=fig.colorbar(pcm,cax=cax,extend='neither',ticks=livelli)

            # Nome file output
            fileout="%s/MAP_FC_LENS_PR_0_TPPC_GRND_NULL_NULL_NULL_NULL_%s_%s_%s_%s_meanmax_italia.png"%(fold_out,inizio.strftime('%Y%m%d%H'),data_fin.strftime('%Y%m%d%H'),f"{int(cumstep):03d}",f"{int(endstep):03d}")      
            plt.savefig(fileout,bbox_inches='tight')

            #---------------------------------------------------
            # Subarea Emilia-Romagna
            #---------------------------------------------------            
            latmin=43
            latmax=46
            lonmin=9
            lonmax=13
            for col in range(2):
                for row in range(2):
                    ax=axs[col,row]
                    ax.set_extent([lonmin,lonmax,latmin,latmax],ccrs.PlateCarree())
                    
                    ax.imshow(datashp_prov[:,:,:].transpose((1,2,0)),extent=shpext_prov,origin='upper',zorder=3)
                    ax.set_extent([lonmin,lonmax,latmin,latmax])
                    
                    gl=ax.gridlines(linestyle='--')
                    #gl=ax.gridlines(draw_labels=True,linestyle='--')            
                    #gl.top_labels=False
                    #gl.right_labels=False

            # Nome file output        
            fileout="%s/MAP_FC_LENS_PR_0_TPPC_GRND_NULL_NULL_NULL_NULL_%s_%s_%s_%s_meanmax_emro.png"%(fold_out,inizio.strftime('%Y%m%d%H'),data_fin.strftime('%Y%m%d%H'),f"{int(cumstep):03d}",f"{int(endstep):03d}")      
            plt.savefig(fileout,bbox_inches='tight')
            plt.close()
            
        elif var[0]=='vmax_10m':
            
            # Set-up mappa
            npanel=3
            fig,axs=plt.subplots(1,3,figsize=(24,6),subplot_kw={'projection': ccrs.PlateCarree()})
            fig.subplots_adjust(hspace=0.07,wspace=0.02)
            font={'size': 8}
            plt.rc('font',**font)
#            cmap=colors.ListedColormap(['#74a9cf','#2b8cbe','#045a8d',
            colori=['#74a9cf','#045a8d','#fecc5c','#fd8d3c','#e31a1c',
                    '#dd3497','#ae017e'] #,'#7a0177']
            livelli=[10.8,13.9,17.2,20.8,24.5,28.5,32.7]
            units='[m/s]'
            cmap,norm=from_levels_and_colors(livelli,colori,extend='max')

            # Seleziono i campi che mi interessano:
            mappe=[None]*npanel
            var_tit=[None]*npanel
            # 1) Ensemble mean
            ind=[x for x in range(len(grbs)) if (soglia[x]==None) & (media[x]==0) & (percentile[x]==None)]
            mappe[0]=campi[ind[0]]
            var_tit[0]="Mean 10m vmax"
            # 2) Max
            ind=[x for x in range(len(grbs)) if (soglia[x]==None) & (media[x]==None) & (percentile[x]==100)]
            mappe[1]=campi[ind[0]]
            var_tit[1]="Maximum 10m vmax"
            # 3) Min
            ind=[x for x in range(len(grbs)) if (soglia[x]==None) & (media[x]==None) & (percentile[x]==0)]
            mappe[2]=campi[ind[0]]
            var_tit[2]="Minimum 10m vmax"

            for col in range(npanel):
                ax=axs[col]
                pcm=ax.pcolormesh(grid_lon,grid_lat,mappe[col],cmap=cmap,norm=norm)

                # Geografia (grande scala)
                ax.coastlines(resolution='10m')
                ax.add_feature(cfeature.NaturalEarthFeature(category='cultural', 
                                                            name='admin_0_boundary_lines_land', scale='10m'),
                               edgecolor='k', facecolor='none')
                ax.add_feature(cartopy.feature.LAKES.with_scale('10m'), edgecolor='k', facecolor='none')
                # Regioni
                limiti=ax.get_extent()                    
                ax.imshow(datashp_reg[:,:,:].transpose((1,2,0)),extent=shpext_reg,origin='upper',zorder=3)
                ax.set_extent(limiti)
                
                gl=ax.gridlines(linestyle='--')

                # Titolo
                sep='\n'
                inizio=datetime.strptime(data+ora,'%Y%m%d%H%M')
                data_in=datetime.strptime(data+ora,'%Y%m%d%H%M')+timedelta(hours=int(startstep))
                data_fin=datetime.strptime(data+ora,'%Y%m%d%H%M')+timedelta(hours=int(endstep))
                # Completamento con data e ora
                title="%s %s%s - %s"%(var_tit[col],sep,data_in.strftime('%Y/%m/%d %H:%M'),
                                      data_fin.strftime('%Y/%m/%d %H:%M'))
                ax.set_title(title,loc='left')
                
            # Aggiungo un'unica colorbar sopra i pannelli
            p0=axs[0].get_position().get_points().flatten()
            p2=axs[2].get_position().get_points().flatten()
            cax=fig.add_axes([p0[0],1.,p2[2]-p0[0],0.05],title=units)
            cax.tick_params(labelsize=10)
            clb=fig.colorbar(pcm,cax=cax,extend='neither',ticks=livelli,orientation='horizontal')

            # Nome file output
            fileout="%s/MAP_FC_LENS_PR_0_VMAX_GRND_NULL_NULL_NULL_NULL_%s_%s_%s_%s_meanmax_italia.png"%(fold_out,inizio.strftime('%Y%m%d%H'),data_fin.strftime('%Y%m%d%H'),f"{int(cumstep):03d}",f"{int(endstep):03d}")      
            plt.savefig(fileout,bbox_inches='tight')

            #---------------------------------------------------
            # Subarea Emilia-Romagna
            #---------------------------------------------------            
            latmin=43
            latmax=46
            lonmin=9
            lonmax=13
            for col in range(npanel):
                    ax=axs[col]
                    ax.set_extent([lonmin,lonmax,latmin,latmax],ccrs.PlateCarree())
                    ax.imshow(datashp_prov[:,:,:].transpose((1,2,0)),extent=shpext_prov,origin='upper',zorder=3)
                    ax.set_extent([lonmin,lonmax,latmin,latmax])
                    
                    gl=ax.gridlines(linestyle='--')            
                    #gl.top_labels=False
                    #gl.right_labels=False

            # Nome file output        
            fileout="%s/MAP_FC_LENS_PR_0_VMAX_GRND_NULL_NULL_NULL_NULL_%s_%s_%s_%s_meanmax_emro.png"%(fold_out,inizio.strftime('%Y%m%d%H'),data_fin.strftime('%Y%m%d%H'),f"{int(cumstep):03d}",f"{int(endstep):03d}")      
            plt.savefig(fileout,bbox_inches='tight')
            plt.close()
            
        else:
            print("Variabile non ancora gestita")

#----------------------------------------------------------------------------
# SECONDA IMMAGINE - probabilità
        # Set-up mappa
        fig,axs=plt.subplots(2,2,figsize=(12,11),subplot_kw={'projection': ccrs.PlateCarree()})
        fig.subplots_adjust(hspace=0.07, wspace=0.05)
        font={'size': 8}
        plt.rc('font',**font)
        
        # Probabilità
        livelli=[0,10,25,50,75,90,100]
        units='[%]'
        cmap=colors.ListedColormap(['#cccccc','#edf8fb','#b3cde3',
                                    '#8c96c6','#8856a7','#810f7c'])
        norm=colors.BoundaryNorm(livelli,cmap.N)

        npanel=4
        # Seleziono i campi che mi interessano:
        mappe=[None]*npanel
        var_tit=[None]*npanel
        # Definisco i valori delle soglie per le diverse scadenze (ne vanno tenuti 4/5)
        if var[0]=='tprate':
            var_out='TPPC'
            if int(cumstep)==1:
                vals=[1,5,10,30]
            elif int(cumstep)==3:
                vals=[10,20,30,50]
            else:
                vals=[30,50,100,150]
        elif var[0]=='vmax_10m':
            var_out='VMAX'
            vals=[13.9,17.2,20.8,24.5]
        else:
            print("Variabile non ancora gestita")
            
        for i in range(npanel):
            # Probabilità per i-esima soglia
            ind=[x for x in range(len(grbs)) if (soglia[x]==vals[i]) & (media[x]==None) & (percentile[x]==None)]
            mappe[i]=campi[ind[0]]
            if var[0]=='tprate':
                var_tit[i]="Probability of total precipitation > %s mm"%(vals[i])
            elif var[0]=='vmax_10m':
                var_tit[i]="Probability of 10m wind > %s m/s"%(vals[i])
            else:
                print("Variabile non ancora gestita")
                
        for col in range(2):
            for row in range(2):
                ax=axs[col,row]
                pcm=ax.pcolormesh(grid_lon,grid_lat,mappe[col*2+row],cmap=cmap,norm=norm)
                # Geografia (grande scala)
                ax.coastlines(resolution='10m')
                ax.add_feature(cfeature.NaturalEarthFeature(category='cultural', 
                                                            name='admin_0_boundary_lines_land', scale='10m'),
                               edgecolor='k', facecolor='none')
                ax.add_feature(cartopy.feature.LAKES.with_scale('10m'), edgecolor='k', facecolor='none')
                # Regioni
                limiti=ax.get_extent()                    
                ax.imshow(datashp_reg[:,:,:].transpose((1,2,0)),extent=shpext_reg,origin='upper',zorder=3)
                ax.set_extent(limiti)
                
                gl=ax.gridlines(linestyle='--')

                # Titolo
                sep='\n'
                inizio=datetime.strptime(data+ora,'%Y%m%d%H%M')
                data_in=datetime.strptime(data+ora,'%Y%m%d%H%M')+timedelta(hours=int(startstep))
                data_fin=datetime.strptime(data+ora,'%Y%m%d%H%M')+timedelta(hours=int(endstep))
                # Completamento con data e ora
                title="%s %s%s - %s"%(var_tit[col*2+row],sep,data_in.strftime('%Y/%m/%d %H:%M'),
                                      data_fin.strftime('%Y/%m/%d %H:%M'))
                ax.set_title(title,loc='left')

        # Aggiungo un'unica colorbar        
        fig.subplots_adjust(right=0.9)
        cax=fig.add_axes([0.93,0.15,0.02,0.7],title=units)
        cax.tick_params(labelsize=10)
        clb=fig.colorbar(pcm,cax=cax,extend='neither',ticks=livelli)
        
        # Nome file output        
        fileout="%s/MAP_FC_LENS_PR_0_%s_GRND_NULL_NULL_NULL_NULL_%s_%s_%s_%s_prob_italia.png"%(fold_out,var_out,inizio.strftime('%Y%m%d%H'),data_fin.strftime('%Y%m%d%H'),f"{int(cumstep):03d}",f"{int(endstep):03d}")
        plt.savefig(fileout,bbox_inches='tight')

        #---------------------------------------------------
        # Subarea Emilia-Romagna
        #---------------------------------------------------            
        latmin=43
        latmax=46
        lonmin=9
        lonmax=13
        for col in range(2):
            for row in range(2):
                ax=axs[col,row]
                ax.set_extent([lonmin,lonmax,latmin,latmax],ccrs.PlateCarree())
                ax.imshow(datashp_prov[:,:,:].transpose((1,2,0)),extent=shpext_prov,origin='upper',zorder=3)
                ax.set_extent([lonmin,lonmax,latmin,latmax])
                    
                gl=ax.gridlines(linestyle='--')            

        # Nome file output        
        fileout="%s/MAP_FC_LENS_PR_0_%s_GRND_NULL_NULL_NULL_NULL_%s_%s_%s_%s_prob_emro.png"%(fold_out,var_out,inizio.strftime('%Y%m%d%H'),data_fin.strftime('%Y%m%d%H'),f"{int(cumstep):03d}",f"{int(endstep):03d}")      
        plt.savefig(fileout,bbox_inches='tight')
        plt.close()

        
quit()
    

