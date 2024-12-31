@echo off

if exist c:\ut.samples (
	goto :copy
) else (
	goto :error
)

:copy 

@echo +--------------------------------------
@echo UT update last version to c:\ut.samples 
@echo This process will update:
@echo    c:\ut.samples\basic
@echo    c:\ut.samples\basic.ssl
@echo    c:\ut.samples\full
@echo +--------------------------------------
@echo +
pause
@echo +-- Delete old version \files folder
rmdir /s /q c:\ut.samples\basic\files\tweb
rmdir /s /q c:\ut.samples\basic\files\uhttpd2

rmdir /s /q c:\ut.samples\basic.ssl\files\tweb
rmdir /s /q c:\ut.samples\basic.ssl\files\uhttpd2

rmdir /s /q c:\ut.samples\full\files\tweb
rmdir /s /q c:\ut.samples\full\files\uhttpd2

@echo +
@echo +-- Update new version \files folder
@echo +

xcopy files\  c:\ut.samples\basic\files\ /E/Y/Q
xcopy files\  c:\ut.samples\basic.ssl\files\ /E/Y/Q
xcopy files\  c:\ut.samples\full\files\ /E/Y/Q

@echo +
@echo +-- Update new version \lib folder
@echo +
xcopy lib\    c:\ut.samples\basic\lib\ /E/Y/Q
xcopy lib\    c:\ut.samples\basic.ssl\lib\ /E/Y/Q
xcopy lib\    c:\ut.samples\full\lib\ /E/Y/Q

@echo +
@echo +-- UT update was done ! 
@echo .

goto :end 

:error

@echo +--------------------------------------------------------------------
@echo Folder c:\ut.samples not exist !
@echo You can download it from https://github.com/carles9000/ut.samples.git 
@echo +--------------------------------------------------------------------


:end 

pause



