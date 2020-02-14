# FSharp.Scheme
Scheme Interpreter Implementation with F#

## Reference

* [48時間でSchemeを書こう](https://ja.wikibooks.org/wiki/48%E6%99%82%E9%96%93%E3%81%A7Scheme%E3%82%92%E6%9B%B8%E3%81%93%E3%81%86)
* [FParsecでJSONパーサーを書いてみる話](http://bleis-tift.hatenablog.com/entry/json-parser-using-fparsec)
* https://github.com/relentless/Flint

## Thanks

https://github.com/wraikny/FsTemplate

<!-- ## CI Status
|||
:---|:---
|Github Actions|[![](https://github.com/guricerin/FSharp.Scheme/workflows/CI/badge.svg)](https://github.com/guricerin/FSharp.Scheme/actions?workflow=CI)|
|Travis CI|[![](https://travis-ci.org/guricerin/FSharp.Scheme.svg?branch=master)](https://travis-ci.org/guricerin/FSharp.Scheme)|
|AppVeyor|[![](https://ci.appveyor.com/api/projects/status/5vtyb8v9twdpteb6?svg=true)](https://ci.appveyor.com/project/guricerin/FSharp.Scheme)| -->

<!---
comment out in Markdown.
--->

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
```shell
$ dotnet fake build -t Test
```
OR
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

## Create Project
```shell
$ dotnet new console -lang=f# -o src/SampleApp # Application
$ echo 'FSharp.Core' > src/SampleApp/paket.references

$ dotnet new classlib -lang=f# -o src/SampleLib # Library
$ echo 'FSharp.Core' > src/SampleLib/paket.references

$ paket install # Add reference of Paket to .fsproj file
```

## Create Test Project
```shell
$ dotnet new console -lang=f# -o tests/SampleTest
$ echo -e 'FSharp.Core\nExpecto\nExpecto.FsCheck' > tests/SampleTest/paket.references

$ paket install # Add reference of Paket to .fsproj file
```
and then, Add **Project Name** to [build.fsx](/build.fsx).

## Tool Update
```shell
$ dotnet tool update fake-cli
$ dotnet tool update paket
```
and then, commit [.config/dotnet-tools.json](/.config/dotnet-tools.json).
