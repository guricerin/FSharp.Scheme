# FSharp.Scheme
Scheme Interpreter Implementation with F#

## Reference

* [48時間でSchemeを書こう](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
* [ぐるぐる～ - FParsecでJSONパーサーを書いてみる話](http://bleis-tift.hatenablog.com/entry/json-parser-using-fparsec)
* [Arantium Maestum - OCamlで48 Hour Schemeをやってみる](http://zehnpaard.hatenablog.com/entry/2019/06/15/223016)

## Thanks

https://github.com/wraikny/FsTemplate

## Restoring after Clone
```shell
$ dotnet tool restore
$ dotnet paket restore
```

## Build
```shell
$ dotnet fake build
```

## Run
```shell
$ dotnet run -p src/FSharp.Scheme.Repl [-c {Debug|Release}]
```

## Tests
```
$ dotnet run --project tests/SampleTest
```

## [Paket](https://fsprojects.github.io/Paket/index.html)  
Each project needs: [paket.references](/src/SampleApp/paket.references) file.

After updating [paket.dependencies](/paket.dependencies):
```shell
$ dotnet paket install
```

## [FAKE](https://fake.build/)  
Scripting at [build.fsx](/build.fsx).  

```shell
$ dotnet fake build -t Clean # Run "Clean" Target
$ dotnet fake build # Run Default Taret
```

## Tool Update
```shell
$ dotnet tool update fake-cli
$ dotnet tool update paket
```
and then, commit [.config/dotnet-tools.json](/.config/dotnet-tools.json).
