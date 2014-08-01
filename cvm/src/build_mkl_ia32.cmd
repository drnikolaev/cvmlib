@@echo off
SET BUILDCMD=Rebuild
IF [%1] NEQ [] SET BUILDCMD=%1

pushd ..\ftn
call build_ftn_ia32.cmd %BUILDCMD%
popd

devenv src_mkl_2013.vcxproj /%BUILDCMD% "Debug|Win32"
	IF ERRORLEVEL 1 GOTO ErrMsg
devenv src_mkl_2013.vcxproj /%BUILDCMD% "Release|Win32"
	IF ERRORLEVEL 1 GOTO ErrMsg


GOTO Exit
:ErrMsg
echo ***ERROR DETECTED***
popd
:Exit
