@@echo off
SET ZIPPER=winrar
SET ZIPPERCMD=a -r -afzip
SET LIBDIR=..\..\lib\
SET OUTDIR=..\
SET FILESET=LICENSE_1_0.txt cvm_ia32.dll cvm_ia32.lib cvm_ia32_debug.dll cvm_ia32_debug.lib cvm_ia32_debug.pdb regtest_cvm_ia32.exe regtest_cvm_ia32_debug.exe regtest_cvm_ia32_debug.pdb

SET CVMVER=8.2
SET INTELREDIST="C:\Program Files (x86)\IntelSWTools\parallel_studio_xe_2016.1.051\compilers_and_libraries_2016\windows\redist\ia32\compiler"
SET MSREDIST="C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\redist\x86\Microsoft.VC140.CRT"


SET BUILDCMD=Rebuild
IF [%1] NEQ [] SET BUILDCMD=%1
SET ZIPMODE=Pack
IF [%2] NEQ [] SET ZIPMODE=%2

SET PATHORIG=%PATH%

rem ----------------------------------------------------------------------------------------------

pushd ..\ftn
call build_ftn_ia32.cmd %BUILDCMD%
popd

pushd %LIBDIR%
%ZIPPER% %ZIPPERCMD% cvm_ftn_ifort_lib.zip ftn_*.lib
MOVE /Y cvm_ftn_ifort_lib.zip ..
popd

rem pushd %LIBDIR%winifort
rem copy *.lib ..\
rem copy *.pdb ..\
rem popd

rem ----------------------------------------------------------------------------------------------

pushd %LIBDIR%
SET DEST=intel32
rmdir /S /Q %DEST%
mkdir %DEST%

copy %INTELREDIST%\libmmd.dll %DEST%
copy %INTELREDIST%\libiomp5md.dll %DEST%
copy %INTELREDIST%\svml_dispmd.dll %DEST%
copy %MSREDIST%\* %DEST%
popd

rem ----------------------------------------------------------------------------------------------


devenv ..\cvmlib_2015.sln /%BUILDCMD% Debug /Project src_lapack_2015.vcxproj /ProjectConfig "Debug|Win32"
	IF ERRORLEVEL 1 GOTO ErrMsg
devenv ..\cvmlib_2015.sln /%BUILDCMD% Release /Project src_lapack_2015.vcxproj /ProjectConfig "Release|Win32"
	IF ERRORLEVEL 1 GOTO ErrMsg
pushd ..\test
call build_test_ia32.cmd %BUILDCMD%
	IF ERRORLEVEL 1 GOTO ErrMsg
popd
pushd %LIBDIR%
SET PATH=.;%DEST%
regtest_cvm_ia32 -t3 -r3
IF ERRORLEVEL 1 GOTO ErrMsg
SET PATH=%PATHORIG%
popd
IF %ZIPMODE% == Pack (
pushd %LIBDIR%
del /f /s %OUTDIR%cvmlib.%CVMVER%.lapack.ia32.zip 
%ZIPPER% %ZIPPERCMD% %OUTDIR%cvmlib.%CVMVER%.lapack.ia32.zip %FILESET% %DEST%\*.*
	IF ERRORLEVEL 1 GOTO ErrMsg
popd
)

rem ----------------------------------------------------------------------------------------------

GOTO Exit
:ErrMsg
echo ***ERROR DETECTED***
popd
:Exit

SET PATHORIG=%PATH%
