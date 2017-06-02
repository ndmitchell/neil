# This script is invoked from my Appveyor commands
# It bootstraps to grab the a binary release and run it
$ErrorActionPreference = "Stop"

if ($args.length -eq 0) {
    Write-Output "No arguments provided, please pass the project name as the first argument"
    exit 1
}
$PACKAGE=$args[0]
if ($args.length -eq 1) {
    $args = @()
} else {
    $args =$args[1 .. ($args.Length-1)]
}

Write-Output "Downloading and running $PACKAGE..."
# Don't go for the API since it hits the Appveyor GitHub API limit and fails
$RELEASES=Invoke-WebRequest https://github.com/ndmitchell/$PACKAGE/releases
$FOUND = $RELEASES.Content -match '\"([^"]+-x86_64-windows.zip)\"'
if (-Not $FOUND){
    Write-Output "Failed to find a suitable URL"
    exit 1
}
$URL = "https://github.com/" + $matches[1]
$VERSION = $URL -replace ".*-([\.0-9]+)-x86_64-windows\.zip",'$1'

$TEMP=New-TemporaryFile
Remove-Item $TEMP
$TEMP=New-Item $TEMP -type Directory
$ZIP=Join-Path "$TEMP" "$PACKAGE.zip"
Invoke-WebRequest $URL -OutFile $ZIP
Add-Type -AssemblyName System.IO.Compression.FileSystem
[System.IO.Compression.ZipFile]::ExtractToDirectory($ZIP, $TEMP)
$EXE=Join-Path "$TEMP" "$PACKAGE-$VERSION\$PACKAGE.exe"
cmd /c "$EXE $args 2>&1"
Remove-Item $TEMP -Recurse -ErrorAction SilentlyContinue
if ($LASTEXITCODE -ne 0) {
    Write-Output "Exit code $LASTEXITCODE"
    exit 1
}
