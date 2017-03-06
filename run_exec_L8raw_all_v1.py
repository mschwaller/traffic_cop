# -*- coding: utf-8 -*-
"""
Created on Thu Feb  2 08:52:26 2017

@author: mschwall
"""

def index_containing_substring(the_list, substring):
    for i, s in enumerate(the_list):
        if substring in s:
              return i
    return -1

def main():
    
    import datetime
    import os
    import math
    #import errno
    import subprocess


    print "***STARTING***"    

    # this is the traffic cop for running write_latlon on Pleiades

    ########################configurable parameters###############################
    config_dir = "/u/mschwall/seabird_input_files_directory"
    # config_dir = "/Users/mschwall/~Essentials/penguin_stuff/L7_SLC_off_tests"
    # config_file_name = config_dir + "/SLC_off_exec_config_file_v1.txt"
    # config_file_name = config_dir + "/L7SLC_config_file_v1.txt"
    # config_file_name = config_dir + "/SLC_off_exec_config_file_v2.txt"
    config_file_name = config_dir + "/L8raw_all_config_file_v1.txt"
    expected_config_parms = 13 # total number of variable to be read from the config file
    version_name = "run_exec_L8raw_all_v1.py"
    ##############################################################################
    


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
    cache = open(config_file_name, "r")
    config_lines = cache.readlines()
    cache.close() 
    
    for line in config_lines:
        if line.find("<scenes_per_node>") >= 0:
            start = line.find(">") # find 1st occurance from the beginning
            end = line.rfind("<")  # find 1st occurance from the end
            config_dict["scenes_per_node"] = line[start+1:end]
            num_config_parms += 1 # increment the counter
        if line.find("<exec_dir>") >= 0:
            start = line.find(">") # find 1st occurance from the beginning
            end = line.rfind("<")  # find 1st occurance from the end
            config_dict["exec_dir"] = line[start+1:end]
            num_config_parms += 1 # increment the counter
        if line.find("<etm_multi_scene_dir>") >= 0:
            start = line.find(">") # find 1st occurance from the beginning
            end = line.rfind("<")  # find 1st occurance from the end
            config_dict["etm_multi_scene_dir"] = line[start+1:end]
            num_config_parms += 1 # increment the counter
        if line.find("<T_file_name>") >= 0:
            start = line.find(">") # find 1st occurance from the beginning
            end = line.rfind("<")  # find 1st occurance from the end
            config_dict["T_file_name"] = line[start+1:end]
            num_config_parms += 1 # increment the counter
        if line.find("<write_lat_lon_out_dir>") >= 0:
            start = line.find(">") # find 1st occurance from the beginning
            end = line.rfind("<")  # find 1st occurance from the end
            config_dict["write_lat_lon_out_dir"] = line[start+1:end]
            num_config_parms += 1 # increment the counter
        if line.find("<write_lat_lon_out_log>") >= 0:
            start = line.find(">") # find 1st occurance from the beginning
            end = line.rfind("<")  # find 1st occurance from the end
            config_dict["write_lat_lon_out_log"] = line[start+1:end]
            num_config_parms += 1 # increment the counter
        if line.find("<adelie_latlon_filename>") >= 0:
            start = line.find(">") # find 1st occurance from the beginning
            end = line.rfind("<")  # find 1st occurance from the end
            config_dict["adelie_latlon_filename"] = line[start+1:end]
            num_config_parms += 1 # increment the counter
        if line.find("<queue_name>") >= 0:
            start = line.find(">") # find 1st occurance from the beginning
            end = line.rfind("<")  # find 1st occurance from the end
            queue_name = line[start+1:end]
            num_config_parms += 1 # increment the counter
        if line.find("<retrieval_code_path>") >= 0:
            start = line.find(">") # find 1st occurance from the beginning
            end = line.rfind("<")  # find 1st occurance from the end
            retrieval_code_directory_path = line[start+1:end]
            num_config_parms += 1 # increment the counter
        if line.find("<retrieval_code_filename>") >= 0:
            start = line.find(">") # find 1st occurance from the beginning
            end = line.rfind("<")  # find 1st occurance from the end
            retrieval_code_filename = line[start+1:end]
            num_config_parms += 1 # increment the counter
        if line.find("<PBS_job_name>") >= 0:
            start = line.find(">") # find 1st occurance from the beginning
            end = line.rfind("<")  # find 1st occurance from the end
            PBS_job_name = line[start+1:end]
            num_config_parms += 1 # increment the counter
        if line.find("<PBS_processor_spec>") >= 0:
            start = line.find(">") # find 1st occurance from the beginning
            end = line.rfind("<")  # find 1st occurance from the end
            PBS_processor_spec = line[start+1:end]
            num_config_parms += 1 # increment the counter
        if line.find("<PBS_walltime>") >= 0:
            start = line.find(">") # find 1st occurance from the beginning
            end = line.rfind("<")  # find 1st occurance from the end
            PBS_walltime = line[start+1:end]
            num_config_parms += 1 # increment the counter
            
    print ""
    
    # parse the bat string
    bat_start = index_containing_substring(config_lines, "start_bat_contents")
    bat_end   = index_containing_substring(config_lines, "end_bat_contents")
    bat_template = ""
    for i in range(bat_start+1,bat_end): 
        bat_template = bat_template + config_lines[i]
    
    # parse the PBS string
    PBS_start = index_containing_substring(config_lines, "start_PBS_script")
    PBS_end   = index_containing_substring(config_lines, "end_PBS_script")
    PBS_template = ""
    for i in range(PBS_start+1,PBS_end): 
        PBS_template = PBS_template + config_lines[i]
                

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
    print ">>master directory name for this job"
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
    print "***reading Landsat scenes from:"
    print L7_dir
    
    L7_scenes = []  # initialize the list
    for ix in os.listdir(L7_dir):   # make a list of scene directories
        if (ix.find('LC8') >=0) or (ix.find('LCO') >= 0) or (ix.find('LE7') >=0):
            L7_scenes.append(ix)
        
    num_L7_scenes = len(L7_scenes)
    if num_L7_scenes <=0:  # bail out if there aren't any scenes found
        print "***did not find any Landsat scenes in this directory;"
        print L7_dir
        return
    num_chunks = math.ceil(len(L7_scenes)/float(config_dict["scenes_per_node"]))
    print "***number of Landsat scenes: " + str(len(L7_scenes))
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
        sub_scene_ctr = 0 # count the number of scenes per chunk
        # create 1 new config file for each chunk (job)
        chunk_ctr = chunk_ctr + 1
        new_config_path = input_files_dir + "/config_file_" + str(chunk_ctr) + ".txt"
        jobc_file = open(new_config_path, "w") # open the file with chunk-number appended to the name
        jobc_file.writelines("%s" % item for item in config_lines)
        jobc_file.write("%s\n" % "<job_scenes>") # write a delimiter/identifier in the file
        s_start = scene_ctr
        if chunk_ctr < num_chunks: 
            s_end = scene_ctr + int(float(config_dict["scenes_per_node"]))
        if chunk_ctr >= num_chunks:
            s_end = num_L7_scenes
        envi_log_file = log_files_dir + "/envi_log_file" + str(chunk_ctr)
        # print "start " + str(s_start) + "     end " + str(s_end)
        for it in range(s_start, s_end): # write "scenes_per_node" number of scenes per chunk (or remainder up to s_end)
            jobc_file.write("%s\n" % L7_scenes[it])
            scene_ctr = scene_ctr + 1
            sub_scene_ctr = sub_scene_ctr + 1
        print ">>>>>>>>>chunk number " + str(chunk_ctr)
        print "<<<<<<<<<scene counter " + str(sub_scene_ctr)
        jobc_file.write("<this_chunk_number>%s</this_chunk_number>\n" % chunk_ctr)
        jobc_file.write("<number_of_scenes>%s</number_of_scenes>\n" % sub_scene_ctr)
        jobc_file.write("<job_name>%s</job_name>\n" % job_name)                
	jobc_file.close()
        print "***opened and closed this config file:"
        print "   " + new_config_path
        print ""
        
        # create 1 new bat file for each chunk (job)
        new_batfile_path = input_files_dir + "/bat_file_" + str(chunk_ctr) + ".bat"
        name_no_pro = retrieval_code_filename[0:retrieval_code_filename.find(".pro")] # strip out .pro

        bat_string = bat_template % (retrieval_code_directory_path,retrieval_code_filename,new_config_path,name_no_pro)
         
        jobb_file = open(new_batfile_path, "w") # open the file with chunk-number appended to the name
        jobb_file.write(bat_string)             # write to the file
        jobb_file.close()
        print "***opened and closed the bat file:"
        print "   " + new_batfile_path
        print "with these contents:"
        print ""
        print bat_string


        # create a PBS script text file for each chunk
        pbs_file_path = input_files_dir + "/pbs_file_" + str(chunk_ctr) + ".txt"
        pbs_file = open(pbs_file_path, "w") # open the pbs file with chunk-number
        job_name_pbs = PBS_job_name % chunk_ctr # PBS_job_name is specified in the config file

        PBS_string = PBS_template % (job_name_pbs,PBS_processor_spec,queue_name,PBS_walltime,new_batfile_path,envi_log_file)
        
        pbs_file.write(PBS_string)
        pbs_file.close()
        command  = "qsub -q %s " % (queue_name) + pbs_file_path 
        print ""
        print ">>>PBS command"
        print command
        p = subprocess.Popen(command, shell=True)
        (output,err) = p.communicate()
        p_status = p.wait()
        print p_status
        print output
        print err

        
        print ""
        print ">>>PBS script for chunk #" + str(chunk_ctr)
        print PBS_string


    return
    
main()
