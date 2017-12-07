# This script is invoked from my Appveyor commands
# It bootstraps to install stack and run the tests
$ErrorActionPreference = "Stop"

# If there is an error, we stop (thanks to the preference above). However, when running commands:
#
# * Writing to stderr is an error, despite the fact stack puts its progress messages there.
#   We fix that by running inside 'cmd' and redirecting stderr to stdout.
#
# * Giving a non-zero exit code is NOT an error. We fix that by testing LASTERRORCODE after each command.

$HLINT_ARGUMENTS = $env:HLINT_ARGUMENTS
if ("$HLINT_ARGUMENTS" -eq '') {
    $HLINT_ARGUMENTS = '.'
}
$Script = Invoke-WebRequest 'https://raw.githubusercontent.com/ndmitchell/hlint/master/misc/appveyor.ps1'
Invoke-Command ([Scriptblock]::Create($Script.Content)) -ArgumentList $HLINT_ARGUMENTS

$env:PATH += ";$PWD" # Make sure stack.exe is on PATH, even if we change directory
$env:STACK_ROOT = 'c:\\sr'
Invoke-WebRequest 'http://www.stackage.org/stack/windows-x86_64' -OutFile 'stack.zip'
7z x -y stack.zip stack.exe
if ($LASTEXITCODE -ne 0) {exit 1}

# If powershell ever sees anything on stderr it decides to fail
# Therefore we use cmd to redirect stderr to stdout before powershell sees it
cmd /c '.\stack init --ignore-subdirs --force 2>&1'
if ($LASTEXITCODE -ne 0) {exit 1}

cmd /c '.\stack setup 1>&2 2>&1 > nul'
if ($LASTEXITCODE -ne 0) {exit 1}

cmd /c 'echo | .\stack --no-terminal build --test --bench --ghc-options=-rtsopts 2>&1'
if ($LASTEXITCODE -ne 0) {exit 1}

$Script = Invoke-WebRequest 'https://raw.githubusercontent.com/ndmitchell/weeder/master/misc/appveyor.ps1'
Invoke-Command ([Scriptblock]::Create($Script.Content))
