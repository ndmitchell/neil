# This script is invoked from my Appveyor commands
# It bootstraps to install stack and run the tests
# $ErrorActionPreference = "Stop"
Set-PSDebug -Trace 0

$HLINT_ARGUMENTS=$env:HLINT_ARGUMENTS
if ("$HLINT_ARGUMENTS" -eq '') {
    $HLINT_ARGUMENTS = '.'
}
$Script = Invoke-WebRequest 'https://raw.githubusercontent.com/ndmitchell/hlint/master/misc/appveyor.ps1'
Invoke-Command ([Scriptblock]::Create($Script.Content)) -ArgumentList $HLINT_ARGUMENTS

Set-Variable STACK_ROOT 'c:\\sr'
Invoke-WebRequest 'http://www.stackage.org/stack/windows-i386' -OutFile 'stack.zip'
7z x -y stack.zip stack.exe
.\stack init 2>&1
.\stack setup 2>&1 | Out-Null
Write-Output "" | .\stack --no-terminal build --test --bench
if ($LASTEXITCODE -ne 0){
    exit 1
}

$Script = Invoke-WebRequest 'https://raw.githubusercontent.com/ndmitchell/weeder/master/misc/appveyor.ps1'
Invoke-Command ([Scriptblock]::Create($Script.Content))
