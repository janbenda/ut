@echo off

@echo +--------------------------------------
@echo Update example...
@echo +--------------------------------------
@echo +
@echo +-- Delete old version \example\basic\files 
rmdir /s /q .\example\basic\files
@echo +-- Delete old version \example\basic\lib 
rmdir /s /q .\example\basic\lib

@echo +-- Delete old version \example\basic.ssl\files 
rmdir /s /q .\example\basic.ssl\files
@echo +-- Delete old version \example\basic.ssl\lib 
rmdir /s /q .\example\basic.ssl\lib

@echo +
@echo +-- Update new version \example\basic
md example\basic\files
xcopy files\  example\basic\files\ /E/Y/Q
md example\basic\lib
xcopy lib\  example\basic\lib\ /E/Y/Q

@echo +
@echo +-- Update new version \example\basic.ssl
md example\basic.ssl\files
xcopy files\  example\basic.ssl\files\ /E/Y/Q
md example\basic.ssl\lib
xcopy lib\  example\basic.ssl\lib\ /E/Y/Q

@echo +
@echo +-- UT example folder was update ! 
@echo .

pause



