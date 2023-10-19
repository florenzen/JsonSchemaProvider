dotnet tool restore
dotnet run --verbosity quiet --project ./build/build.fsproj -- -t %*
