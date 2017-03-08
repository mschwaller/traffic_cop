This branch contains all of the code and input files needed to run the Landsat-7 SLC-off 
retrievals on Pleiades 

ENVI+IDL Code: 
g_write_latlon_multi_2016a.pro (main)
g_calc_hdf_map_projection_2016a.pro
g_check_for_dupe_scenes_2016a.pro
g_check_for_fmask_scenes_2016a.pro
g_checkdupes_multi_2016a.pro
g_mine_lndcal_subsets_multi_2016a.pro

ENVI input files:
adelie_lat_lon_for_config.txt
B3+B1+B2+B5+B4+B6_nudge_array_Dec13h12m53s06y2015.txt
B3+B1+B2+B5+B4+B6_t_matrix_Dec13h12m53s06y2015.txt

Python "traffic cop" code to kick off the ENVI sessions:
run_exec_L7_SLC_off.py

Input file for .py code:
L7_SLC_off_config_file_NEX_master_v1.txt