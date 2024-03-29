Config { font = "xft:Arimo for Powerline:style=normal:pixelsize=16:antialias=true"
        , borderColor = "black"
        , border = TopB
        , bgColor = "#292929"
        , fgColor = "grey"
        , position = TopW L 100
        , commands = [
                          Run DynNetwork ["--Low","0","--High","1024000", "--high","green", "--template", "<dev>: <rx>KBs/<tx>KBs"] 10
                        , Run Cpu ["-p", "1", "-L","0","-H","50","--normal","green","--high","red","-t", "<user>%u <system>%s <iowait>%i"] 10
                        , Run Memory ["-t","<used>M/<total>M"] 10
                        , Run Com ".config/xmonad/bin/volume" [] "vol" 10
                        , Run Date "%T" "date" 10
                        , Run StdinReader
			, Run Weather "EHAM" ["-t","<tempC>C <skyCondition> (<windCardinal> <windMs> m/s)","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 36000
                        ]
        , sepChar = "%"
        , alignSep = "}{"
        , template = "%StdinReader% }{ %EHAM% | VOL: %vol% | %cpu% | %memory% | %dynnetwork% | <fc=#ee9a00>%date%</fc>                             "
        }
