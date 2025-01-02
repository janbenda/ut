@echo off

if exist c:\ut.samples (
	goto :copy
) else (
	goto :error
)

:copy 

@echo +-------------------------------------------
@echo UT update last version to c:\ut.samples\full
@echo +-------------------------------------------

rmdir /s /q c:\ut.samples\full\files\tweb
rmdir /s /q c:\ut.samples\full\files\uhttpd2

@echo +
@echo +-- Update new version \files folder
@echo +
xcopy files\  c:\ut.samples\full\files\ /E/Y/Q

@echo +
@echo +-- Update new version \lib folder
@echo +
xcopy lib\    c:\ut.samples\full\lib\ /E/Y/Q

@echo +
@echo +-- UT update was done ! 
@echo .

cd\ut.samples\full\
go_msvc64.bat

goto :end 

:error


:end 

pause



