@echo off

@echo +--------------------------------------
@echo Update sample...
@echo +--------------------------------------
@echo +
@echo +-- Delete old version \sample\basic\files 
rmdir /s /q .\sample\basic\files
@echo +-- Delete old version \sample\basic\lib 
rmdir /s /q .\sample\basic\lib

@echo +-- Delete old version \sample\basic.ssl\files 
rmdir /s /q .\sample\basic.ssl\files
@echo +-- Delete old version \sample\basic.ssl\lib 
rmdir /s /q .\sample\basic.ssl\lib

@echo +
@echo +-- Update new version \sample\basic
md sample\basic\files
xcopy files\  sample\basic\files\ /E/Y/Q
md sample\basic\lib
xcopy lib\  sample\basic\lib\ /E/Y/Q

@echo +
@echo +-- Update new version \sample\basic.ssl
md sample\basic.ssl\files
xcopy files\  sample\basic.ssl\files\ /E/Y/Q
md sample\basic.ssl\lib
xcopy lib\  sample\basic.ssl\lib\ /E/Y/Q

@echo +
@echo +-- UT sample folder was update ! 
@echo .

pause



