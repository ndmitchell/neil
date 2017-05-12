# This script is invoked from my Appveyor commands
# It bootstraps to install stack and run the tests
$ErrorActionPreference = "Stop"

Invoke-WebRequest 'http://www.stackage.org/stack/windows-i386' -OutFile 'stack.zip'
7z x stack.zip stack.exe

Invoke-Expression (Invoke-WebRequest 'https://raw.githubusercontent.com/ndmitchell/hlint/master/misc/appveyor.ps1')

Set-Variable STACK_ROOT 'c:\\sr'
stack init
stack setup > nul
Write-Output "" | stack --no-terminal build --test --bench
if ($LASTEXITCODE -ne 0){
    exit 1
}

Invoke-Expression (Invoke-WebRequest 'https://raw.githubusercontent.com/ndmitchell/weeder/master/misc/appveyor.ps1')
