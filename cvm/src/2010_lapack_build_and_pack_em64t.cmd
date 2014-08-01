@@echo off
SET ZIPPER=winrar
SET ZIPPERCMD=a -r -afzip
SET LIB64DIR=..\..\lib64\
SET OUTDIR=..\
SET FILESET=LICENSE_1_0.txt cvm_em64t.dll cvm_em64t.lib cvm_em64t_debug.dll cvm_em64t_debug.lib cvm_em64t_debug.pdb regtest_cvm_em64t.exe regtest_cvm_em64t_debug.exe regtest_cvm_em64t_debug.pdb
SET FILESETILP64=LICENSE_1_0.txt cvm_em64t_ilp64.dll cvm_em64t_ilp64.lib cvm_em64t_ilp64_debug.dll cvm_em64t_ilp64_debug.lib cvm_em64t_ilp64_debug.pdb regtest_cvm_em64t_ilp64.exe regtest_cvm_em64t_ilp64_debug.exe regtest_cvm_em64t_ilp64_debug.pdb

SET CVMVER=7.0.2010
SET INTELREDIST="C:\Program Files (x86)\Intel\Composer XE 2013\redist\intel64\compiler"
SET MSREDIST="C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\redist\x64\Microsoft.VC100.CRT"


SET BUILDCMD=Rebuild
IF [%1] NEQ [] SET BUILDCMD=%1
SET ZIPMODE=Pack
IF [%2] NEQ [] SET ZIPMODE=%2


SET PATHORIG=%PATH%

rem ----------------------------------------------------------------------------------------------

pushd ..\ftn
call 2010_build_ftn_em64t.cmd %BUILDCMD%
popd

pushd %LIB64DIR%winifort
copy *.lib ..\
copy *.pdb ..\
popd

rem ----------------------------------------------------------------------------------------------

pushd %LIB64DIR%
SET DEST=intel64
rmdir /S /Q %DEST%
mkdir %DEST%

copy %INTELREDIST%\libmmd.dll %DEST%
copy %INTELREDIST%\svml_dispmd.dll %DEST%

copy %MSREDIST%\msvcp100.dll %DEST%
copy %MSREDIST%\msvcr100.dll %DEST%
popd

rem ----------------------------------------------------------------------------------------------


devenv src_lapack_2010.vcxproj /%BUILDCMD% "Debug|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
devenv src_lapack_2010.vcxproj /%BUILDCMD% "Release|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
pushd ..\test
call 2010_build_test_em64t.cmd %BUILDCMD%
	IF ERRORLEVEL 1 GOTO ErrMsg
popd
pushd %LIB64DIR%
SET PATH=.;%DEST%
regtest_cvm_em64t -t3 -r3
IF ERRORLEVEL 1 GOTO ErrMsg
SET PATH=%PATHORIG%
popd
IF %ZIPMODE% == Pack (
pushd %LIB64DIR%
del /f %OUTDIR%cvmlib.%CVMVER%.lapack.em64t.zip 
%ZIPPER% %ZIPPERCMD% %OUTDIR%cvmlib.%CVMVER%.lapack.em64t.zip %FILESET% %DEST%\*.*
	IF ERRORLEVEL 1 GOTO ErrMsg
popd
)

rem ----------------------------------------------------------------------------------------------

devenv src_lapack_ilp64_2010.vcxproj /%BUILDCMD% "Debug|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
devenv src_lapack_ilp64_2010.vcxproj /%BUILDCMD% "Release|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
pushd ..\test
call 2010_build_test_em64t_ilp64.cmd %BUILDCMD%
	IF ERRORLEVEL 1 GOTO ErrMsg
popd
pushd %LIB64DIR%
SET PATH=.;%DEST%
regtest_cvm_em64t_ilp64 -t3 -r3
IF ERRORLEVEL 1 GOTO ErrMsg
SET PATH=%PATHORIG%
popd
IF %ZIPMODE% == Pack (
pushd %LIB64DIR%
del /f %OUTDIR%cvmlib.%CVMVER%.lapack.em64t.ilp64.zip 
%ZIPPER% %ZIPPERCMD% %OUTDIR%cvmlib.%CVMVER%.lapack.em64t.ilp64.zip %FILESETILP64% %DEST%\*.*
	IF ERRORLEVEL 1 GOTO ErrMsg
popd
) 

rem ----------------------------------------------------------------------------------------------

GOTO Exit
:ErrMsg
echo ***ERROR DETECTED***
popd
:Exit

SET PATH=%PATHORIG%
