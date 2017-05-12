# This script is invoked from my Appveyor commands
# It bootstraps to grab the a binary release and run it
$PACKAGE=$args[0]
if ($args.length -eq 0) {
    Write-Output "No arguments provided, please pass the project name as the first argument"
    exit 1
}

Write-Output "Downloading and running $PACKAGE..."
$RELEASES=Invoke-WebRequest https://api.github.com/repos/ndmitchell/$PACKAGE/releases
$JSON = $RELEASES.Content | ConvertFrom-Json
$URL = ""
:top foreach ($x in $JSON) {
    foreach ($x in $x.assets) {
        if ($x.browser_download_url -match '.*-x86_64-windows\.zip') {
            $URL = $x.browser_download_url
            break top
        }
    }
}
if ($URL -eq "") {
    Write-Output "Failed to find a suitable URL"
    exit 1
}
$VERSION = $URL -replace ".*-([\.0-9]+)-x86_64-windows\.zip",'$1'

$TEMP=New-TemporaryFile
Remove-Item $TEMP
$TEMP=New-Item $TEMP -type Directory
$ZIP=Join-Path "$TEMP" "$PACKAGE.zip"
Invoke-WebRequest $URL -OutFile $ZIP
Add-Type -AssemblyName System.IO.Compression.FileSystem
[System.IO.Compression.ZipFile]::ExtractToDirectory($ZIP, $TEMP)
$EXE=Join-Path "$TEMP" "$PACKAGE-$VERSION\$PACKAGE.exe"
& $EXE $args[1 .. ($args.length-1)]
Remove-Item $TEMP -Recurse
