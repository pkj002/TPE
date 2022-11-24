# TPE
TARGET POPULATION OF ENVIRONMENTS FOR BEAN BREEDING IN AFRICA				
main directory is 'SPATANALYST01.CGIARAD.ORG\\catalogue\BaseLineDataCluster01\temp\dssat_outputs\'				
Descriptions of the files are as follows				
S.N.	Script name	Purpose	Location	where are input data?
1	Scripts to simulate			
1.1	simu_ug	for uganda	main + uganda	functions to run the script are in 'E:/Prakash/funs_ugan'. Input data are in '//catalogue/BaseLineDataCluster01/temp/data_for_dssat_eaf/dssat_input/Uganda/' + 2030 +2050 + obs
1.2	simu_ethi	for Ethiopia	main + ethiopia	functions to run the script are in 'E:/Prakash/funs_ethi'. Input data are in '//catalogue/BaseLineDataCluster01/temp/data_for_dssat_eaf/dssat_input/Ethiopia/'+ 2030 +2050 + obs
1.3	simu_tanz	for Tanzania	main + tanzania	functions to run the script are in 'E:/Prakash/funs_mod'. Input data are in '//catalogue/BaseLineDataCluster01/temp/data_for_dssat_eaf/dssat_input/Tanzania/' + 2030 +2050 + obs
				
2	Outputs from simulations are in dir = '\\catalogue\BaseLineDataCluster01\temp\dssat_outputs\'			
	outputs of Uganda are in 			dir + uganda
	outputs of Ethiopia are in 			dir + ethiopia
	outputs of Tanzania are in 			dir + tanzania
3	data extraction from output files			
3.1.1	Extract stress from output files			
	ug_mod_wspd_wkly_main.R	for Uganda		dir + uganda + others
	ug_wspd_chunk.R	for Uganda		dir + uganda + others + funs_wspd_wkly
	main_ethi_plantGro_save.R	for Ethiopia		dir + ethiopia + others
	fun_ethi_plantGro_save.R	for Ethiopia		dir + uganda + others 
	main_stress_plantgro_rep_veg.R	for Tanzania		dir + tanzania + others
	function_stress_plantgro_rep_veg1.R	for Tanzania		dir + tanzania + others
				
3.1.2	Extract yield from output files			
	main_save_mod_yld_ug.R	for Uganda		dir + uganda + others
	fun_save_mod_yld_ug.R	for Uganda		dir + uganda + others
	main_save_yld_tz_season.R	for Tanzania		dir + tanzania + others
	fun_save_yld_tz_season.R	for Tanzania		dir + tanzania + others
	main_yld_analysis_with_season.R	for Ethiopia		dir + ethiopia + others
	fun_yld_analysis_with_season.R	for Ethiopia		dir + ethiopia + others
				
4	clustering			
	code_stress_cluster2_main.R	for Uganda		dir + uganda + others
	code_stress_cluster2.R	for Uganda		dir + uganda + others
	main_code_stress_cluster5.R	for Tanzania		dir + tanzania + others
	function_code_stress_cluster5.R	for Tanzania		dir + tanzania + others
	mod_clustering_main.R	for Ethiopia		dir + ethiopia + others
	ethi_clust4.R	for Ethiopia		dir + ethiopia + others + funs_cluster
				
5	visualization			
5.1	line plot of drought stress pattern			
5.1.1	line_plot_all_cntry_stress_2050.R	all 3 countries		 '\\catalogue\BaseLineDataCluster01\temp\scripts'
				
5.2	bar plot of corridor-specific-cluster			
	plot2_ethi_corridor_cluster_bar_space_time_av.R		bar plot of Ethiopia	same as 5.1.1
	plot2_tz_corridor_cluster_bar_space_time.R		bar plot of Tanzania	same as 5.1.1
	plot_ug_corridor_cluster_bar_afterSteve.R		bar plot of Uganda	same as 5.1.1
				
5.3	change in yield 			
	all_cntry_combined_yld_map.R			same as 5.1.1
	plot_hist_all_cntry_combined_yield.R			
				
5.4	climate change all var all countries			
	all_cntry_combined_climate_ch_all_var_final.R	all countries		same as 5.1.1
				same as 5.1.1
6	Extract corridor-specific-total_harvested-area under various environmental groups 			
	area_stress_corridor_ug.R	Uganda		same as 5.1.1
	area_stress_corridor_tz.R	Tanzania		same as 5.1.1
	area_stress_corridor_ethi.R	Ethiopia		same as 5.1.1
	Tmin_harvest_area_6cat.R	all countries		same as 5.1.1
				
7	Create input files (tibble of weather, soil and planting dates)			
	create_list_of_Uganda1.R	model Uganda		
	create_list_of_obs_Uganda.R	obs Uganda		same as 5.1.1
	create_list_of_Tanzania_new_sow_date.R	model Tanzania		same as 5.1.1
	create_list_of_obs_Tanzania.R	obs Tanzania		same as 5.1.1
	create_list_of_obs_Ethiopia_lat_all_new.R	obs Ethiopia		same as 5.1.1
	create_list_of_Ethiopia_lat_all_new.R	model Ethiopia		same as 5.1.1

CAN TOLERANT GENOTYPES AVERT DROUGHT STRESS ?			
main directory is 'SPATANALYST01.CGIARAD.ORG\E:\Prakash\dssat_out_culti'			
Descriptions of the files are as follows			
S.N.	Script name	Purpose	where are input data?
1	simulation scripts		
1.1	main_script_cul_ethi.R	simulate	functions to run the script are in 'E:/Prakash/funs_ethi'. Input data are in '//catalogue/BaseLineDataCluster01/temp/data_for_dssat_eaf/dssat_input/Ethiopia/obs'
1.2	main_script_cul_tanz.R	simulate	functions to run the script are in 'E:/Prakash/funs_mod'. Input data are in '//catalogue/BaseLineDataCluster01/temp/data_for_dssat_eaf/dssat_input/Tanzania/obs'
1.3	main_script_cul_ug.R	simulate	functions to run the script are in 'E:/Prakash/funs_ugan'. Input data are in '//catalogue/BaseLineDataCluster01/temp/data_for_dssat_eaf/dssat_input/Uganda/obs'
			
2	Outputs from simulations		
	outputs of Uganda are in 		main + uganda + obs_cul1, obs_cul2, obs_cul3, obs_cul4, obs_cul5
	outputs of Ethiopia are in 		main + ethiopia + obs_cul, obs_cul2
	outpputs of Tanzania are in 		main + tanzania + obs_cul_tz, obs_cul2
3	data analysis and visualization		
3.1	Extract stress data from Plantgro output files		
	plantGro_save_com_ug_final	for Uganda	main + uganda
	plantGro_save_comb_ethi.R	for Ethiopia	main + ethiopia
	plantGro_save_comb_ethi_6_10	for Ethiopia	main + ethiopia
	plantGro_save_comb_tanz_1_5	for Tanzania	main + tanzania
	plantGro_save_comb_tanz_6_10	for Tanzania	main + tanzania
3.2	clustering		
	cluster_ug_all_genotypes.R	for Uganda	main + uganda
	cluster_ethi_all_genotypes.R	for Ethiopia	main + ethiopia
	cluster_tanz_all_genotypes	for Tanzania	main + tanzania
3.3	map corridor-specific drought stress for all genotypes		
	map_combined_cluster_all_geno_ug.R	for Uganda	main + uganda
	map_combined_cluster_of_all_geno_ethi.R	for Ethiopia	main + ethiopia
	map_cluster_all_geno_combined_tz.R	for Tanzania	main + tanzania 
3.4	compute area under different stress types with different probability for each corridor and genotype		
	area_freq_stress_yld.R	for Uganda	main + uganda
	area_freq_stress_yld.R	for Ethiopia	main + ethiopia
	area_sterss_yld_tz	for Tanzania	main +  tanzania

			
IMPACT OF SOIL COMPACTION ON COMMON BEAN			
Files are in 'SPATANALYST01.CGIARAD.ORG\E:\Prakash\dssat_outputs\uganda'			
Descriptions of the files are as follows			
S.N.	Script name	Purpose	Input data
1	Simulations scripts		
1.1	ug_bd_simu_main_1lay.R	Main script for simulation of impact of compaction. Compaction was limited to first layer only for all grid points 	1. functions to run the script are in 'E:/Prakash/funs_ugan'. Input data are in '//catalogue/BaseLineDataCluster01/temp/data_for_dssat_eaf/dssat_input/Uganda/obs'
1.2	ug_bd_simu_main.R	same as 1.1 but compactions in more than one layer and for random 15 grids	same as 1.1
2	Outputs from simulations		
2.1	bd_random_lay1	Outputs from simulations of all grids with compaction in the first layer	
2.2	bd_random_N	Outputs from simulations of 15 points under N limited 	
2.3	bd_random	Outputs from simulations of 15 points compaction in upper 3 layers	
			
3	data analysis and visualization		
	All grids		
3.1	map_impact_of_comp_all_grids.R	Map impact of compaction (average value) for all grids	'//catalogue/BaseLineDataCluster01/temp/compac_all_grids/plts_comb/all_grids/'
3.2	map_ug_sd_imp_compaction.R	Same as 1 but sd	Same as 1.1
3.3	ug_bd_analysis_all_grids.R	Extract results (mean) from simluations	'E:/Prakash/dssat_outputs/uganda/bd_random_lay1/'
3.4	ug_dif_sd_anal_all_grids.R	Extract results (sd) from simluations	same as 1.3
	15 random points		
3.5	ug_bd_analysis_random_pts.R	Same as 3 but for just 15 points	E:/Prakash/dssat_outputs/uganda/'
3.6	ug_plot_data_av_culti	Data extraction and analysis	same as 3.5
3.7	ug_yld_diff_compact_drought.R	Compute difference in yield between compaction and not compacted.	same as 3.5
3.8	significance_test_model_out_ug1.R	Compute the significance of the difference	same as 3.5
3.9	ug_plot_data_all_cult	Extract data and average for all cultivars	same as 3.5
4	ug_plot_data.R	Extract data for each cultivar	same as 3.5
4.1	maps_sample_pts_ug.R	Map of 15 sample points	same as 3.5
4.2	ug_significance_test_soil.R	Test whether there is a significant difference between soil properties of compacted and not compacted soil	same as 3.5
4.3	significance_test_model_out_ug.R	Test whether there is a significant difference between model outputs of compacted and not compacted soil	'E:/Prakash/dssat_outputs/uganda/bd_random/'
4.4	ug_soil_texture_random_pts.R	Mapping soil properties under different drought stress categories	same as 3.3
			
Files in 'SPATANALYST01.CGIARAD.ORG\E:\Prakash\dssat_outputs\ethiopia'			
	Descriptions are same as Uganda as described above		
			
Files in 'SPATANALYST01.CGIARAD.ORG\E:\Prakash\dssat_outputs\tanzania'			
	Descriptions are same as Uganda as described above		


