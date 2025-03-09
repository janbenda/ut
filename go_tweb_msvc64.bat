@echo off
cls

call "%ProgramFiles%\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat" amd64

set hbdir=c:\harbour
set include=%include%;%hbdir%\include
set lib=%lib%;%hbdir%\lib\win\msvc64
set path=%path%;path c:\windows\system32;c:\windows;%hbdir%;%hbdir%\bin

hbmk2 tweb.hbp -comp=msvc64

if errorlevel 1 goto compileerror

goto exit

:compileerror

echo *** Error ***

:exit

pause