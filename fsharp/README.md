# F# implementation


Just `dotnet run -c release`. 

The following options can be specified:

* `-m height`
* `-n width`
* `-f file.ppm`
* `-s <rgbbox|irreg>`
* `-r number of warmup runs`

Requires the [.NET Core 3.1 SDK](https://dotnet.microsoft.com/download).

### Native Build
Run `./linux-native.sh` this builds for the correct RID (runtime identifier) and calls the binary with the passed in options. 

Note: Restore may seem to be hanging, it has to download 60+mb and that specific blob feed is quite slow for some reason. 
