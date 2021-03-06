@@echo off
SET ZIPPER=winrar
SET ZIPPERCMD=a -r -afzip
SET LIBDIR=..\..\lib\
SET OUTDIR=..\
SET FILESET=LICENSE_1_0.txt cvm_ia32.dll cvm_ia32.lib cvm_ia32_debug.dll cvm_ia32_debug.lib cvm_ia32_debug.pdb regtest_cvm_ia32.exe regtest_cvm_ia32_debug.exe regtest_cvm_ia32_debug.pdb

SET CVMVER=8.1
SET INTELREDIST="C:\Program Files (x86)\Intel\Composer XE 2013 SP1\redist\ia32\compiler"
SET MKLREDIST="C:\Program Files (x86)\Intel\Composer XE 2013 SP1\redist\ia32\mkl"
SET MSREDIST="C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\redist\x86\Microsoft.VC120.CRT"

SET BUILDCMD=Rebuild
IF [%1] NEQ [] SET BUILDCMD=%1
SET ZIPMODE=Pack
IF [%2] NEQ [] SET ZIPMODE=%2

SET PATHORIG=%PATH%

rem ----------------------------------------------------------------------------------------------
pushd ..\ftn
call build_ftn_ia32.cmd %BUILDCMD%
popd
rem ----------------------------------------------------------------------------------------------

pushd %LIBDIR%
SET DEST=intel32
rmdir /S /Q %DEST%
mkdir %DEST%
copy %INTELREDIST%\libicaf.dll %DEST%
copy %INTELREDIST%\libifcoremd.dll %DEST%
copy %INTELREDIST%\libifportmd.dll %DEST%
copy %INTELREDIST%\libiomp5md.dll %DEST%
copy %INTELREDIST%\libiompstubs5md.dll %DEST%
copy %INTELREDIST%\libmmd.dll %DEST%
copy %INTELREDIST%\svml_dispmd.dll %DEST%
copy %MKLREDIST%\libimalloc.dll %DEST%
rem copy %MKLREDIST%\mkl_cdft_core.dll %DEST%
copy %MKLREDIST%\mkl_core.dll %DEST%
copy %MKLREDIST%\mkl_intel_thread.dll %DEST%
copy %MKLREDIST%\mkl_p4.dll %DEST%
copy %MKLREDIST%\mkl_p4m.dll %DEST%
copy %MKLREDIST%\mkl_p4m3.dll %DEST%
copy %MKLREDIST%\mkl_p4p.dll %DEST%
copy %MKLREDIST%\mkl_rt.dll %DEST%
copy %MKLREDIST%\mkl_sequential.dll %DEST%
copy %MSREDIST%\msvcp120.dll %DEST%
copy %MSREDIST%\msvcr120.dll %DEST%
popd

rem ----------------------------------------------------------------------------------------------

devenv src_mkl_2013.vcxproj /%BUILDCMD% "Debug|Win32"
	IF ERRORLEVEL 1 GOTO ErrMsg
devenv src_mkl_2013.vcxproj /%BUILDCMD% "Release|Win32"
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
del /f /s %OUTDIR%cvmlib.%CVMVER%.mkl.ia32.zip 
%ZIPPER% %ZIPPERCMD% %OUTDIR%cvmlib.%CVMVER%.mkl.ia32.zip %FILESET% %DEST%\*.*
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
