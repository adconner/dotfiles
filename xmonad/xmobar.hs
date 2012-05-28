Config { font = "-misc-fixed-bold-r-bold--18-120-100-100-c-90-iso10646-1"
       , borderColor = "#242424"
       , border = TopB
       , bgColor = "#242424"
       , fgColor = "#e3e0d7"
       , position = Top
       , lowerOnStart = True
       , commands = [ Run Weather "KBNA" ["-t","<tempF> F <skyCondition>","-L","65","-H","80","--normal","#95e454","--high","#ff2026","--low","lightblue"] 36000
                    , Run Wireless "wlan0" ["-t", "<essid> <quality>%"] 50
                    , Run MultiCpu ["-t", "cpu: <total0>% <total1>%" ] 10
                    , Run Memory ["-t","mem: <usedratio>%"] 10
                    , Run Swap ["-t", "swap: <usedratio>%"] 10
                    , Run Date "%a %b %_d %I:%M:%S" "date" 10
                    --, Run MPD ["-t", "<artist> - <title> <statei>"] 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       -- , template = "%multicpu% | %memory% %swap% | %wlan0wi% } %StdinReader% { %NWS% | %mpd% | <fc=#ee9a00>%date%</fc>"
       , template = "%multicpu% | %memory% %swap% | %wlan0wi% } %StdinReader% { %KBNA% | <fc=#eadead>%date%</fc>"
       }