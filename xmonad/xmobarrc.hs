Config { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
        , borderColor = "black"
        , border = TopB
        , bgColor = "black"
        , fgColor = "grey"
        , position = TopW L 100
        , commands = [ Run Weather "EHAM" ["-t","<tempC>C","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 36000
                        , Run Network "enp0s31f6" ["-L","0","-H","1024000", "--high","green", "-t", "Net: <rx>K/<tx>K"] 10
                        , Run Cpu ["-p", "3", "-L","0","-H","50","--normal","green","--high","red","-t", "Cpu: <user>%u <system>%s <iowait>%i"] 10
                        , Run Memory ["-t","Mem: <used>M/<total>M"] 10
                        , Run Com "uname" ["-s","-r"] "" 36000
                        , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                        , Run StdinReader
                        ]
        , sepChar = "%"
        , alignSep = "}{"
        , template = "%StdinReader% }{ %cpu% | %memory% | %enp0s31f6% | <fc=#ee9a00>%date%</fc> | %EHAM%"
        }
