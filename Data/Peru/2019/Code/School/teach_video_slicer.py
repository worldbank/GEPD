#!/usr/bin/env python
# coding: utf-8

# # TEACH Video Clip Creator
#
# This program will create 15 minute chunks of videos for the TEACH graders to use to evaluate teacher pedagogical skills.
#
# The program will use Python to clip the longer classroom videos into 15 minute chunks.

# In[1]:


# Import everything needed to edit video clips
from moviepy.editor import *

#import tool for dealing with files and paths
from pathlib import Path, PureWindowsPath

#Allow showing video in python notebook
from IPython.display import Video

#inport os.path
import os.path

#specify file path
save_folder=Path("C:/Users/wb469649/WBG/Sergio Venegas Marin - Videos_Peru/")

network_folder=Path("//wbgfscifs01/GEDEDU/datalib-edu/projects/GEPD-Confidential/CNT/PER/PER_2019_GEPD/PER_2019_GEPD_v01_RAW/Data/videos")
#data_folder=Path("D:/Módulo 4 Observación Aula")
data_folder=Path("C:/Users/wb469649/WBG/Sergio Venegas Marin - Videos_Peru/")

# # Test Case
#
# Now I will open a test clip to see how things are working.  This will help us understand how well the program is functioning.

# In[2]:


#specify file to open
file_to_open = data_folder / 'Ruta 1' / 'Video' / '256305-(1) IE18113 Amazonas .mp4'

print(file_to_open)


# Now we will create a subclip between 10 and 10:30 minutes of the video.

# In[3]:


#Calculate duration of video
vid=VideoFileClip(str(file_to_open))

vid.reader.close()
vid.audio.reader.close_proc()

vid_duration=vid.duration
print(vid_duration)


# In[4]:


# Load test clip and select the subclip 00:10:00 - 00:10:30
clip = vid.subclip('00:10:00','00:10:30')



clip.ipython_display(width=280)



# Now we will save the clip to the folder.

# In[5]:




#path to saved file
file_to_write= save_folder / 'Ruta 1' / 'Video_clips' / '256305-(1) IE18113 Amazonas - Test Clip 1 .mp4'

#write the clip
clip.write_videofile(str(file_to_write), preset='veryfast', threads=2)


# ## Loop through all videos and create clips
#
# Now that we have completed a test case, we will loop through all the videos and then create two clips for each video.  One clip will be between 10 and 25 minutes.  The other clip will span from 30 minutes to 45 minutes.  These clips should be checked by hand to report any mistakes or problems. This python code does a nice job of automating the process of creating the clips, but human eyes should verify the quality.
#
# Because of the large file sizes and disk space constraints, I have set up the code to run in chunks, based on the enumerator routes (Ruta 1, Ruta 2, Ruta 3, ...)

# In[6]:


# first define the route "chunk"
chunk = 'Ruta 6'

chunks=['Ruta 1','Ruta 2','Ruta 3','Ruta 4','Ruta 5','Ruta 6','Ruta 7','Ruta 8','Ruta 9','Ruta 10','Ruta 11','Ruta 12','Ruta 13','Ruta 14','Ruta 15','Ruta 16',
        'Ruta 17', 'Ruta 18', 'Ruta 19', 'Ruta 20']

for chunk in chunks:
    path_to_open = data_folder / chunk / 'Video'
    
    if os.path.isdir(str(path_to_open)):
        print("Is Path")
    else: 
        print("need new path")
        path_to_open = data_folder / chunk / 'Videos'
    
    path_to_write = save_folder / chunk / 'Video_clips'
    
    Path(path_to_write).mkdir(parents=True, exist_ok=True)
    print(path_to_write)
    
    
    # In[7]:
    
    
    #get list of all files
    file_list =[f for f in path_to_open.glob('**/*') if f.is_file() ]
    
    for f in file_list:
        print(f)
        print(f.parts[7])
        file_name=str(f.parts[7]) #this is the 4th part of the file route, with just the file name
        file_name_base=file_name[:-4]
        file_name_new=file_name_base + "Clip 1 " + ".MP4"
        print(file_name_new)
    
    
    # In[ ]:
    
    
    for f in file_list:
        #come up a new file name called ".. Clip1.MP4" and ".. Clip2.MP4"
        file_name=str(f.parts[7]) #this is the 4th part of the file route, with just the file name
        file_name=str(f.parts[7]) #this is the 4th part of the file route, with just the file name
        file_name_base=file_name[:-4]
        file_name_suffix=file_name[-4:]
        file_name_new1=file_name_base + "Clip 1" + ".MP4"
        file_name_new2=file_name_base + "Clip 2" + ".MP4"
    
        print(file_name_new1)
        print(file_name_new2)
    
        #Calculate duration of video
        vid=VideoFileClip(str(f))
        
        vid.reader.close()
        vid.audio.reader.close_proc()
        
        vid_duration=vid.duration
    
        #do this if video duration longer than 41 min
        if vid_duration>= 2340:
            print("Video is of sufficient length for two clips")
            #Now cut the clips
            clip1 = vid.subclip('00:08:00','00:23:00')
            file_to_write1= save_folder / chunk / 'Video_clips' / file_name_new1
    
            clip2 = vid.subclip('00:24:00','00:39:00')
            file_to_write2= save_folder / chunk / 'Video_clips' / file_name_new2
            if os.path.isfile(str(file_to_write1)):
                print ("File exist")
            else:
                print ("File not exist")
                #write the clip
                clip1.write_videofile(str(file_to_write1),  threads=200, codec='libx264')
            
            if os.path.isfile(str(file_to_write2)):
                print ("File exist")
    
            else:
                print ("File not exist")
                #write the clip
                clip2.write_videofile(str(file_to_write2),  threads=200, codec='libx264',  logger=None)
    
        #do this if video duration longer than 25 min but less than 41
        elif vid_duration>= 1800 and vid_duration< 2340 :
            print("Video less than 41 minutes but larger than 25 min")
            #Now cut the clips
            clip1 = vid.subclip('00:08:00','00:23:00')
            file_to_write1= save_folder / chunk / 'Video_clips' / file_name_new1
    
            if os.path.isfile(str(file_to_write1)):
                print ("File exist")
            else:
                print ("File not exist")
                #write the clip
                clip1.write_videofile(str(file_to_write1),  threads=200, codec='libx264')
    
        else:
            print("Video of insufficient length")
