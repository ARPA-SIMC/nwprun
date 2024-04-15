#!/usr/bin/env python3
# encoding: utf-8

import os, sys, numpy as np, subprocess, glob, pygrib
import pandas as pd

#from osgeo import gdal
import cartopy, cartopy.crs as ccrs, cartopy.feature as cfeature
from cartopy.feature import ShapelyFeature
from cartopy.io.shapereader import Reader

import matplotlib, matplotlib.pyplot as plt
from matplotlib import colors
from matplotlib.colors import from_levels_and_colors

from datetime import datetime,timedelta

from eps_utils import get_args_maps, get_var_grib, rotated_grid_transform 

#------------------------------------------------------------------------
# Definizione delle variabili per cui può venire eseguita la grafica
#------------------------------------------------------------------------
class tprate:
    varsn   = "tp"
    varln   = "Accumulated precipitation"
    units   = "mm"
    livelli = [ 0, 1, 5, 10, 20, 30, 50, 70, 100, 150, 200, 300, 500 ]
    #orig
    #colori = [ "#f0f0f0", "#d0d1e6", "#a6bddb", "#74a9cf",
    #           "#2b8cbe", "#045a8d", "#fecc5c", "#fd8d3c",
    #           "#e31a1c", "#dd3497", "#ae017e", "#7a0177" ]
    colori  = [ "#f0f0f0", "#00ffff", "#00cdcd", "#1e90ff",
                "#0000cd", "#00008b", "#fecc5c", "#fd8d3c",
                "#e31a1c", "#dd3497", "#ae017e", "#7a0177" ]
    extend  = "neither"
    fout    = "TPPC"
    
class vmax_10m:
    varsn   = "vmax_10m"
    varln   = "10m vmax"
    units   = "m/s"
    livelli = [ 10.8, 13.9, 17.2, 20.8, 24.5, 28.5, 32.7 ]
    colori  = [ "#74a9cf", "#045a8d", "#fecc5c", "#fd8d3c",
                "#e31a1c", "#dd3497", "#ae017e" ] #,"#7a0177"]
    extend  = "max"
    fout    = "VMAX"

class lpi:
    varsn   = "lpi"
    varln   = "Lightning potential index"
    units   = "J/kg"
    fout    = "LPI"

#------------------------------------------------------------------------
# Lo script plotta i campi dell'ensemble post-processati da fieldextra
#------------------------------------------------------------------------
if __name__ == '__main__':

    args = get_args_maps()
    
    fname = args.file_in 
        
    fold_out = args.fold_out # default = tmp
    # Creo la directory di output, "fold_out", se non esiste
    if not os.path.exists( "{}".format(fold_out) ):
        os.makedirs( "{}".format(fold_out) )

    # Shapefiles per grafica
    shp_reg = args.aree_er     
    shp_it  = args.aree_it
    shape_feature = ShapelyFeature(Reader(shp_it).geometries(),
                                   ccrs.PlateCarree(), edgecolor='black')

    # ------------------------------------------------------------------------------
    base = os.path.basename(fname)
    
    try:
        print("Lettura file:", fname)
        grbs = pygrib.open(fname)
        grbs = grbs.select()

        # Definisco le variabili necessarie alla grafica 
        var = [None]*len(grbs)
        media = [None]*len(grbs)
        percentile = [None]*len(grbs)
        soglia = [None]*len(grbs)
        campi = [None]*len(grbs)
        
        n = 0
        # Lettura di tutti i campi del grib 
        for grb in grbs:
        
            variabile, data, ora, Ni, Nj, startstep, endstep, thresh, medi, perc, cumstep = get_var_grib(grb)
            var[n] = variabile
            media[n] = medi
            percentile[n] = perc
            soglia[n] = thresh
            campi[n] = grb.values
        
            # Al primo record calcolo il grigliato
            if n == 0:
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

        print("VARIABILE DA PLOTTARE:", var[0])
        
        try:
            if ( var[0] == "tprate" or var[0] == "vmax_10m" or var[0] == "10fg" ):
                if var[0] == "tprate":
                    variabile = tprate
                    if int(cumstep) == 1:
                        vals = [ 1, 5, 10, 30 ]
                    elif int(cumstep) == 3:
                        vals = [ 10, 20, 30, 50 ]
                    else:
                        vals = [ 30, 50, 100, 150 ]
                elif var[0] == "vmax_10m" or var[0] == "10fg":
                    variabile = vmax_10m
                    vals = [ 13.9, 17.2, 20.8, 24.5 ]

                #print(variabile)
                #print(vals)

#----------------------------------------------------------------------------------------
# PRIMA IMMAGINE - ensmean, max, min, 90esimo percentile
            
                # Set-up mappa 
                npanel = 3
                fig, axs = plt.subplots( 1, 3, figsize=(16, 4), # layout="constrained",
                                         subplot_kw={'projection': ccrs.PlateCarree()} )
                font = {'size': 8}
                plt.rc( 'font', **font )

                cmap, norm = from_levels_and_colors( variabile.livelli, variabile.colori, extend=variabile.extend )
            
                #---------------------------------------------------    
                # Seleziono i campi che mi interessano:
                #---------------------------------------------------
                mappe = [None] * npanel
                var_tit = [None] * npanel
                
                if any(v is not None for v in media) or any(v is not None for v in percentile):
                
                    # 1) Ensemble mean
                    ind = [ x for x in range(len(grbs)) if (soglia[x]==None) & (media[x]==0) & (percentile[x]==None) ]
                    mappe[0] = campi[ ind[0] ]
                    mappe[0][ mappe[0] < 0 ] = 0.
                    var_tit[0] = "Mean {}".format( variabile.varln )
                
                    # 2) Max
                    ind = [ x for x in range(len(grbs)) if (soglia[x]==None) & (media[x]==None) & (percentile[x]==100) ]
                    mappe[1] = campi[ ind[0] ]
                    mappe[1][ mappe[1] < 0 ] = 0.
                    var_tit[1] = "Maximum {}".format( variabile.varln )
            
                    if var[0] == "tprate":
                        # 3) 90esimo percentile
                        ind = [ x for x in range(len(grbs)) if (soglia[x]==None) & (media[x]==None) & (percentile[x]==90) ]
                        mappe[2] = campi[ ind[0] ]
                        mappe[2][ mappe[2] < 0 ] = 0.
                        var_tit[2] = "90th percentile {}".format( variabile.varln )
                    elif var[0] == "vmax_10m" or var[0] == "10fg":
                        # 4) Min
                        ind = [ x for x in range(len(grbs)) if (soglia[x]==None) & (media[x]==None) & (percentile[x]==0) ]
                        mappe[2] = campi[ ind[0] ]
                        var_tit[2] = "Minimum {}".format( variabile.varln )

                    # Grafica dei campi su pannelli contigui
                    for col in range(npanel):
                        ax = axs[col]
                        pcm = ax.pcolormesh( grid_lon, grid_lat, mappe[col], cmap=cmap, norm=norm )
                    
                        # Geografia
                        ax.coastlines(resolution='10m')
                        ax.add_feature(cfeature.NaturalEarthFeature(category='cultural', 
                                                                    name='admin_0_boundary_lines_land',
                                                                    scale='10m'),
                                       edgecolor='k', facecolor='none', zorder=1)
                        ax.add_feature( cartopy.feature.LAKES.with_scale('10m'), edgecolor='k',
                                        facecolor='none', zorder=1 )
                        # Shapefile regioni
                        ax.add_feature( shape_feature, edgecolor='black', facecolor="None", linewidth=0.8,
                                        alpha=0.4, zorder=3)
                
                        #gl = ax.gridlines(linestyle='--')
                
                        # Titolo
                        sep = '\n'
                        inizio   = datetime.strptime( data+ora, "%Y%m%d%H%M" )
                        data_in  = datetime.strptime( data+ora, "%Y%m%d%H%M" ) + timedelta( hours=int(startstep) )
                        data_fin = datetime.strptime( data+ora, "%Y%m%d%H%M" ) + timedelta( hours=int(endstep) )
                        title="{} {}{} - {}".format( var_tit[col], sep,
                                                     data_in.strftime("%Y/%m/%d %H:%M"),
                                                     data_fin.strftime("%Y/%m/%d %H:%M") )
                        ax.set_title( title, loc='left' )
                
                    # Aggiungo un'unica colorbar sopra i pannelli
                    p0 = axs[0].get_position().get_points().flatten()
                    p2 = axs[2].get_position().get_points().flatten()
                    cax = fig.add_axes( [ p0[0], 1.05, p2[2]-p0[0], 0.05 ], title="[{}]".format( variabile.units ) )
                    cax.tick_params( labelsize=10 )
                    clb = fig.colorbar( pcm, cax=cax, extend=variabile.extend, ticks=variabile.livelli,
                                        orientation='horizontal' )
                    
                    # Salvataggio output - ITALIA
                    fileout = "{}/MAP_FC_LENS_PR_0_{}_GRND_NULL_NULL_NULL_NULL_{}_{}_{}_{}_meanmax_italia.png".format( fold_out, variabile.fout, inizio.strftime('%Y%m%d%H'), data_fin.strftime('%Y%m%d%H'), f"{int(cumstep):03d}", f"{int(endstep):03d}" )
                    plt.savefig( fileout, bbox_inches='tight' )
            
                    #---------------------------------------------------
                    # Definizione e ciclo sui sottodomini
                    #---------------------------------------------------
                    zoom = {
                        "nord" : [ 6., 15., 43., 48.5 ],
                        "centro" : [ 7.5, 15., 38.5, 44.7 ],
                        "sud" : [ 11.5, 19.5, 35., 42.5 ],
                    }
            
                    for key in zoom: 
                        for col in range(npanel):
                            ax = axs[col]
                            ax.set_extent( zoom[key], ccrs.PlateCarree() )
                           
                        # Salvataggio output - SOTTODOMINI
                        fileout = "{}/MAP_FC_LENS_PR_0_{}_GRND_NULL_NULL_NULL_NULL_{}_{}_{}_{}_meanmax_{}.png".format( fold_out, variabile.fout, inizio.strftime('%Y%m%d%H'), data_fin.strftime('%Y%m%d%H'), f"{int(cumstep):03d}", f"{int(endstep):03d}", key )      
                        plt.savefig( fileout, bbox_inches='tight' )
                        
                    #---------------------------------------------------
                    # Subarea Emilia-Romagna
                    #---------------------------------------------------            
                    er = [ 9., 13., 43., 46. ]
                    shape_er = ShapelyFeature(Reader(shp_reg).geometries(),
                                              ccrs.PlateCarree(), edgecolor='black')

                    for col in range(npanel):
                        ax = axs[col]
                        ax.set_extent( er, ccrs.PlateCarree() )
                        ax.add_feature( shape_er, edgecolor='black', facecolor="None", linewidth=0.4,
                                        alpha=0.5, zorder=2 )
                        
                    # Nome file output        
                    fileout = "{}/MAP_FC_LENS_PR_0_{}_GRND_NULL_NULL_NULL_NULL_{}_{}_{}_{}_meanmax_emro.png".format( fold_out, variabile.fout, inizio.strftime('%Y%m%d%H'), data_fin.strftime('%Y%m%d%H'), f"{int(cumstep):03d}", f"{int(endstep):03d}" )
                    plt.savefig(fileout, bbox_inches='tight')

                    plt.close()

#----------------------------------------------------------------------------------------
# SECONDA IMMAGINE - probabilità

                # Set-up mappa 
                npanel = 4
                fig, axs = plt.subplots( 2, 2, figsize=(10, 9),
                                         subplot_kw={'projection': ccrs.PlateCarree()} )
                font = {'size': 8}
                plt.rc( 'font', **font )
            
                # Probabilità
                livelli = [ 0, 5, 10, 25, 50, 75, 90, 100 ]
                units = '[%]'
                cmap = colors.ListedColormap( [ '#cccccc', '#fecc5c', '#41ab5d', '#006837',
                                                '#1e90ff', '#00008b', '#dd3497' ] )   
                norm = colors.BoundaryNorm( livelli, cmap.N )

                #---------------------------------------------------    
                # Seleziono i campi che mi interessano:
                #---------------------------------------------------
                mappe = [None] * npanel
                var_tit = [None] * npanel
                
                for i in range(npanel):
                    # Probabilità per i-esima soglia
                    ind = [x for x in range(len(grbs)) if (soglia[x]==vals[i]) & (media[x]==None) & (percentile[x]==None)]
                    mappe[i] = campi[ ind[0] ]
                    var_tit[i] = "Probability of {} > {} {}".format( variabile.varln, vals[i], variabile.units)
                    
                for col in range(2):
                    for row in range(2):
                        ax = axs[ col, row ]
                        pcm = ax.pcolormesh( grid_lon, grid_lat, mappe[col*2 + row], cmap=cmap, norm=norm )

                        # Geografia
                        ax.coastlines(resolution='10m')
                        ax.add_feature(cfeature.NaturalEarthFeature(category='cultural', 
                                                                    name='admin_0_boundary_lines_land',
                                                                    scale='10m'),
                                       edgecolor='k', facecolor='none', zorder=1)
                        ax.add_feature( cartopy.feature.LAKES.with_scale('10m'), edgecolor='k',
                                        facecolor='none', zorder=1 )
                        # Shapefile regioni
                        ax.add_feature( shape_feature, edgecolor='black', facecolor="None", linewidth=0.8,
                                        alpha=0.4, zorder=3)
                        
                        #gl = ax.gridlines(linestyle='--')
                    
                        # Titolo
                        sep = '\n'
                        inizio   = datetime.strptime( data+ora, "%Y%m%d%H%M" )
                        data_in  = datetime.strptime( data+ora, "%Y%m%d%H%M" ) + timedelta( hours=int(startstep) )
                        data_fin = datetime.strptime( data+ora, "%Y%m%d%H%M" ) + timedelta( hours=int(endstep) )
                        title="{} {}{} - {}".format( var_tit[col*2 + row], sep,
                                                     data_in.strftime("%Y/%m/%d %H:%M"),
                                                     data_fin.strftime("%Y/%m/%d %H:%M") )
                        ax.set_title( title, loc='left' )

                        # Aggiungo un'unica colorbar a destra        
                        fig.subplots_adjust( right=0.9 )
                        cax = fig.add_axes( [ 0.93, 0.15, 0.02, 0.7 ], title=units )
                        cax.tick_params( labelsize=10 )
                        clb = fig.colorbar( pcm, cax=cax, extend='neither', ticks=livelli )
                 
                # Salvataggio output - ITALIA
                if all(v is None for v in media) and all(v is None for v in percentile):
                    strp = "probup"
                else:
                    strp = "prob"

                fileout = "{}/MAP_FC_LENS_PR_0_{}_GRND_NULL_NULL_NULL_NULL_{}_{}_{}_{}_{}_italia.png".format( fold_out, variabile.fout, inizio.strftime('%Y%m%d%H'), data_fin.strftime('%Y%m%d%H'), f"{int(cumstep):03d}", f"{int(endstep):03d}", strp )
                plt.savefig( fileout, bbox_inches='tight' )
            
                #---------------------------------------------------
                # Definizione e ciclo sui sottodomini
                #---------------------------------------------------
                zoom = {
                    "nord" : [ 6., 15., 43., 48.5 ],
                    "centro" : [ 7.5, 15., 38.5, 44.7 ],
                    "sud" : [ 11.5, 19.5, 35., 42.5 ],
                }
                
                for key in zoom:
                    for col in range(2):
                        for row in range(2):
                            ax = axs[ col, row ]
                            ax.set_extent( zoom[key], ccrs.PlateCarree() )
                           
                    # Salvataggio output - SOTTODOMINI
                    fileout = "{}/MAP_FC_LENS_PR_0_{}_GRND_NULL_NULL_NULL_NULL_{}_{}_{}_{}_{}_{}.png".format( fold_out, variabile.fout, inizio.strftime('%Y%m%d%H'), data_fin.strftime('%Y%m%d%H'), f"{int(cumstep):03d}", f"{int(endstep):03d}", strp, key )
                    plt.savefig( fileout, bbox_inches='tight' )

                #---------------------------------------------------
                # Subarea Emilia-Romagna
                #---------------------------------------------------            
                er = [ 9., 13., 43., 46. ]
                shape_er = ShapelyFeature(Reader(shp_reg).geometries(),
                                          ccrs.PlateCarree(), edgecolor='black')
            
                for col in range(2):
                    for row in range(2):
                        ax = axs[ col, row ]
                        ax.set_extent( er, ccrs.PlateCarree() )
                        ax.add_feature( shape_er, edgecolor='black', facecolor="None", linewidth=0.4,
                                        alpha=0.5, zorder=2 )
                           
                # Nome file output
                fileout = "{}/MAP_FC_LENS_PR_0_{}_GRND_NULL_NULL_NULL_NULL_{}_{}_{}_{}_{}_emro.png".format( fold_out, variabile.fout, inizio.strftime('%Y%m%d%H'), data_fin.strftime('%Y%m%d%H'), f"{int(cumstep):03d}", f"{int(endstep):03d}", strp )
                plt.savefig(fileout,bbox_inches='tight')
                
                plt.close()
                
            elif var[0] == "lpi":
                variabile = lpi
                # Soglie per cui fare le mappe 
                vals = [0.1, 5, 10, 20]
                # Set-up mappa 
                npanel = 4
                fig, axs = plt.subplots( 2, 2, figsize=(10, 9),
                                         subplot_kw={'projection': ccrs.PlateCarree()} )
                font = {'size': 8}
                plt.rc( 'font', **font )
            
                # Probabilità
                livelli = [ 0, 5, 10, 25, 50, 75, 90, 100 ]
                units = '[%]'
                cmap = colors.ListedColormap( [ '#cccccc', '#fecc5c', '#41ab5d', '#006837',
                                                '#1e90ff', '#00008b', '#dd3497' ] )   
                norm = colors.BoundaryNorm( livelli, cmap.N )

                #---------------------------------------------------    
                # Seleziono i campi che mi interessano:
                #---------------------------------------------------
                mappe = [None] * npanel
                var_tit = [None] * npanel
                
                for i in range(npanel):
                    # Probabilità per i-esima soglia
                    ind = [x for x in range(len(grbs)) if (soglia[x]==vals[i])]
                    mappe[i] = campi[ ind[0] ]
                    var_tit[i] = "Probability of {} > {} {}".format( variabile.varln, vals[i], variabile.units)
                    
                for col in range(2):
                    for row in range(2):
                        ax = axs[ col, row ]
                        pcm = ax.pcolormesh( grid_lon, grid_lat, mappe[col*2 + row], cmap=cmap, norm=norm )

                        # Geografia
                        ax.coastlines(resolution='10m')
                        ax.add_feature(cfeature.NaturalEarthFeature(category='cultural', 
                                                                    name='admin_0_boundary_lines_land',
                                                                    scale='10m'),
                                       edgecolor='k', facecolor='none', zorder=1)
                        ax.add_feature( cartopy.feature.LAKES.with_scale('10m'), edgecolor='k',
                                        facecolor='none', zorder=1 )
                        # Shapefile regioni
                        ax.add_feature( shape_feature, edgecolor='black', facecolor="None", linewidth=0.8,
                                        alpha=0.4, zorder=3)

                        # Titolo
                        sep = '\n'
                        inizio   = datetime.strptime( data+ora, "%Y%m%d%H%M" )
                        data_in  = datetime.strptime( data+ora, "%Y%m%d%H%M" ) + timedelta( hours=int(startstep) )
                        data_fin = datetime.strptime( data+ora, "%Y%m%d%H%M" ) + timedelta( hours=int(endstep) )
                        title="{} {}{} - {}".format( var_tit[col*2 + row], sep,
                                                     data_in.strftime("%Y/%m/%d %H:%M"),
                                                     data_fin.strftime("%Y/%m/%d %H:%M") )
                        ax.set_title( title, loc='left' )

                        # Aggiungo un'unica colorbar a destra        
                        fig.subplots_adjust( right=0.9 )
                        cax = fig.add_axes( [ 0.93, 0.15, 0.02, 0.7 ], title=units )
                        cax.tick_params( labelsize=10 )
                        clb = fig.colorbar( pcm, cax=cax, extend='neither', ticks=livelli )

                # Salvataggio output - ITALIA
                fileout = "{}/MAP_FC_LENS_PR_0_{}_GRND_NULL_NULL_NULL_NULL_{}_{}_{}_{}_prob_italia.png".format( fold_out, variabile.fout, inizio.strftime('%Y%m%d%H'), data_fin.strftime('%Y%m%d%H'), f"{int(cumstep):03d}", f"{int(endstep):03d}" )
                plt.savefig( fileout, bbox_inches='tight' )

                #---------------------------------------------------
                # Definizione e ciclo sui sottodomini
                #---------------------------------------------------
                zoom = {
                    "nord" : [ 6., 15., 43., 48.5 ],
                    "centro" : [ 7.5, 15., 38.5, 44.7 ],
                    "sud" : [ 11.5, 19.5, 35., 42.5 ],
                }
                
                for key in zoom:
                    for col in range(2):
                        for row in range(2):
                            ax = axs[ col, row ]
                            ax.set_extent( zoom[key], ccrs.PlateCarree() )
                           
                    # Salvataggio output - SOTTODOMINI
                    fileout = "{}/MAP_FC_LENS_PR_0_{}_GRND_NULL_NULL_NULL_NULL_{}_{}_{}_{}_prob_{}.png".format( fold_out, variabile.fout, inizio.strftime('%Y%m%d%H'), data_fin.strftime('%Y%m%d%H'), f"{int(cumstep):03d}", f"{int(endstep):03d}", key )      
                    plt.savefig( fileout, bbox_inches='tight' )

                #---------------------------------------------------
                # Subarea Emilia-Romagna
                #---------------------------------------------------            
                er = [ 9., 13., 43., 46. ]
                shape_er = ShapelyFeature(Reader(shp_reg).geometries(),
                                          ccrs.PlateCarree(), edgecolor='black')
            
                for col in range(2):
                    for row in range(2):
                        ax = axs[ col, row ]
                        ax.set_extent( er, ccrs.PlateCarree() )
                        ax.add_feature( shape_er, edgecolor='black', facecolor="None", linewidth=0.4,
                                        alpha=0.5, zorder=2 )
                           
                # Nome file output        
                fileout = "{}/MAP_FC_LENS_PR_0_{}_GRND_NULL_NULL_NULL_NULL_{}_{}_{}_{}_prob_emro.png".format( fold_out, variabile.fout, inizio.strftime('%Y%m%d%H'), data_fin.strftime('%Y%m%d%H'), f"{int(cumstep):03d}", f"{int(endstep):03d}" )
                plt.savefig(fileout,bbox_inches='tight')
                
                plt.close()
                
        except Exception as e:
            raise exception("ERRORE! Variabile non ancora gestita")

    except Exception as exc:
        sys.exit("ERRORE! Non riesco a leggere il file")
                
quit()
    

