#!/usr/bin/env bash

RID=linux-x64

dotnet publish -r $RID -c Release trace.fsproj &&
bin/Release/netcoreapp2.1/$RID/publish/trace $@
