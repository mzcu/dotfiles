Config { font = "xft:Ubuntu Mono:style=bold:pixelsize=14:antialias=true"
        , borderColor = "black"
        , border = TopB
        , bgColor = "black"
        , fgColor = "grey"
        , position = TopW L 100
        , commands = [
                          Run Network "enp0s31f6" ["-L","0","-H","1024000", "--high","green", "-t", "NET: <rx>K/<tx>K"] 10
                        , Run Cpu ["-p", "3", "-L","0","-H","50","--normal","green","--high","red","-t", "CPU: <user>%u <system>%s <iowait>%i"] 10
                        , Run Memory ["-t","MEM: <used>M/<total>M"] 10
                        , Run Com "uname" ["-s","-r"] "" 36000
                        , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                        , Run StdinReader
		--	, Run Weather "EHAM" ["-t","<tempC>C","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 36000
                        ]
        , sepChar = "%"
        , alignSep = "}{"
        , template = "%StdinReader% }{ %cpu% | %memory% | %enp0s31f6% | <fc=#ee9a00>%date%</fc> "
        }
