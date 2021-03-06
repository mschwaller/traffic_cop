# -*- coding: utf-8 -*-
"""
Created on Thu Feb  2 08:52:26 2017

@author: mschwall
"""

def main():
    
    import datetime
    import os
    import math
    #import errno


    print "***STARTING***"    

    # this is the traffic cop for running write_latlon on Pleiades

    ########################configurable parameters###############################
    config_dir = "/Users/mschwall/~Essentials/penguin_stuff/L7_SLC_off_tests"
    config_file_name = config_dir + "/SLC_off_exec_config_file_v1.txt"
    expected_config_parms = 7 # total number of variable to be read from the config file
    version_name = "run_exec_v2.py"
    ##############################################################################
    
    ########################placeholder parameters###############################
    # maybe these need to be added to the main config file?
    # directory path to the ENVI code main program
    retrieval_code_directory_path = "/u/mschwall/IDLWorkspace81/Default/general_utilities/NEX_seabird_retrievals" 
    # filename of the ENVI code main program
    main_retrieval_filename  = "NEX_seab_nn_write_latlon_multi_2016_v7c.pro"
    #############################################################################


    result = os.path.exists(config_file_name)
    if (result != True):
        print "Did not find the config file:"
        print config_file_name
        print "EXITING!"
        print " "
        return

    print "***working on this config file:"
    print config_file_name
    print " "
    
    config_dict = {} # set up a dictionary to hold the configurable parameters
    num_config_parms = 0
    # read the config file and parse out the configurable parameters into config_dict
    with open(config_file_name, "r") as cache:
        # read the file into a list of config_lines
        config_lines = cache.readlines()
        for line in config_lines:
            if line.find("<scenes_per_node>") > 0:
                start = line.find(">") # find 1st occurance from the beginning
                end = line.rfind("<")  # find 1st occurance from the end
                config_dict["scenes_per_node"] = line[start+1:end]
                num_config_parms += 1 # increment the counter
            if line.find("<exec_dir>") > 0:
                start = line.find(">") # find 1st occurance from the beginning
                end = line.rfind("<")  # find 1st occurance from the end
                config_dict["exec_dir"] = line[start+1:end]
                num_config_parms += 1 # increment the counter
            if line.find("<etm_multi_scene_dir>") > 0:
                start = line.find(">") # find 1st occurance from the beginning
                end = line.rfind("<")  # find 1st occurance from the end
                config_dict["etm_multi_scene_dir"] = line[start+1:end]
                num_config_parms += 1 # increment the counter
            if line.find("<T_file_name>") > 0:
                start = line.find(">") # find 1st occurance from the beginning
                end = line.rfind("<")  # find 1st occurance from the end
                config_dict["T_file_name"] = line[start+1:end]
                num_config_parms += 1 # increment the counter
            if line.find("<write_lat_lon_out_dir>") > 0:
                start = line.find(">") # find 1st occurance from the beginning
                end = line.rfind("<")  # find 1st occurance from the end
                config_dict["write_lat_lon_out_dir"] = line[start+1:end]
                num_config_parms += 1 # increment the counter
            if line.find("<write_lat_lon_out_log>") > 0:
                start = line.find(">") # find 1st occurance from the beginning
                end = line.rfind("<")  # find 1st occurance from the end
                config_dict["write_lat_lon_out_log"] = line[start+1:end]
                num_config_parms += 1 # increment the counter
            if line.find("<adelie_latlon_filename>") > 0:
                start = line.find(">") # find 1st occurance from the beginning
                end = line.rfind("<")  # find 1st occurance from the end
                config_dict["adelie_latlon_filename"] = line[start+1:end]
                num_config_parms += 1 # increment the counter

    print " "
    print "***read these configuration parameters"
    for ikey in config_dict:
        print ikey                         # print the key
        print "  " + config_dict[ikey]  # print its value
        
    print " "    
    
    if num_config_parms != expected_config_parms:
        print "expected " + str(expected_config_parms) + " but found " + str(num_config_parms)
        print "***EXITING!***"
        return

    # make these assignments becasue they are a bit easier to write out
    L7_exec_dir = config_dict["exec_dir"]  
    L7_dir = config_dict["etm_multi_scene_dir"]

    # create a time-stamped job name
    str_time_date = str(datetime.datetime.now())  # result is a date type
    str_time_date = str_time_date.replace(" ","_") # replace the balnk with a _
    job_name = "L7SLC" + "_" + str_time_date
    this_job_dir = L7_exec_dir + job_name
    print " "
    print "***job directory and subdirectory names:"
    print ">>master direcotry name for this job"
    print this_job_dir

    # create a master directory for this job under the L7_exec_dir
    if not os.path.exists(this_job_dir):
        os.makedirs(this_job_dir)
        
    # create subdirectories under this_job_dir
    # for config files, bat files and adelie lat-lon files
    input_files_dir = this_job_dir + "/L7_input_files"
    print ">>input_files_dir " 
    print input_files_dir
    if not os.path.exists(input_files_dir):
        os.makedirs(input_files_dir)
    # for log files
    log_files_dir = this_job_dir + "/L7_log_files"
    print ">>log_files_dir " 
    print log_files_dir
    if not os.path.exists(log_files_dir):
        os.makedirs(log_files_dir)    

    # open a runtime results file for writing under this_job_dir
    print ">>job results file name"
    print this_job_dir + "/" + job_name + "_results"
    run_results_file = open(this_job_dir + "/" + job_name + "_results.txt", "w")
    run_results_file.write(version_name)
    run_results_file.close
    
    # now read all of the L7 scenes and store the scene directory names in L7_scenes
    print " "
    print "***reading Landsat-7 scenes from:"
    print L7_dir
    
    L7_scenes = []  # initialize the list
    for ix in os.listdir(L7_dir):   # make a list of scene directories
        if ix.find('LC8') >=0 or ix.find('LCO') >=0:
            L7_scenes.append(ix)
    
    num_L7_scenes = len(L7_scenes)
    num_chunks = math.ceil(len(L7_scenes)/float(config_dict["scenes_per_node"]))
    print "***number of L7 scenes: " + str(len(L7_scenes))
    print "***number of chunks: " + str(num_chunks)
    print " "
    print "***scene IDs:"
    for iz in L7_scenes: print iz
        
    # now revise the config file by adding the scenes each job/config 
    #   will process (and save it to the job directory)
    # create a bat file (and save it to that directory too)
    # create a PBS files (and save it in the log file)
    chunk_ctr = 0
    scene_ctr = 0
    for ir in range(1, int(num_chunks + 1)): # loop over all chunks
    
        # create 1 new config file for each chunk (job)
        chunk_ctr = chunk_ctr + 1
        new_config_path = input_files_dir + "/config_file_" + str(chunk_ctr) + ".txt"
        jobc_file = open(new_config_path, "w") # open the file with chunk-number appended to the name
        jobc_file.writelines("%s" % item for item in config_lines)
        jobc_file.write("%s\n" % "<job_scenes>") # write a delimiter/identifier in the file
        s_start = scene_ctr
        if chunk_ctr < num_chunks: 
            s_end = scene_ctr + 10
        if chunk_ctr >= num_chunks:
            s_end = num_L7_scenes
        # print "start " + str(s_start) + "     end " + str(s_end)
        for it in range(s_start, s_end): # write 10 scenes per chunk (or remainder up to s_end)
            jobc_file.write("%s\n" % L7_scenes[it])
            scene_ctr = scene_ctr + 1
        jobc_file.close()
        print "***opened and closed this config file:"
        print "   " + new_config_path
        
        # create 1 new bat file for each chunk (job)
        new_batfile_path = input_files_dir + "/bat_file_" + str(chunk_ctr) + ".txt"
        jobb_file = open(new_batfile_path, "w") # open the file with chunk-number appended to the name
        jobb_file.write("CD, " + retrieval_code_directory_path + "\n")     # 1st line
        jobb_file.write(".compile " + main_retrieval_filename + "\n") # and so on...
        jobb_file.write("config_path_name = " + new_config_path + "\n") 
        name_no_pro = main_retrieval_filename[0:main_retrieval_filename.find(".pro")] # strip out .pro
        jobb_file.write(name_no_pro + ", config_path_name") 
        jobb_file.close()
        print "***opened and closed this file:"
        print "   " + new_config_path

            
    
    
    
    
    
    return
    
main()