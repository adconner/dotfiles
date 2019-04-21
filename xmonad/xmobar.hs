Config { 
         font = "xft:Bitstream Vera Sans Mono:size=12:antialias=true"
       , borderColor = "#242424"
       , border = TopB
       , bgColor = "#242424"
       , fgColor = "#e3e0d7"
       , position = Top
       , lowerOnStart = True
       , allDesktops = True
       , commands = [ Run Weather "KBNA" 
                                   ["-t","<tempF> F <skyCondition>",
                                    "-L","65","-H","80","--normal","#95e454",
                                    "--high","#ff2026","--low","lightblue"] 36000
                    , Run Wireless "wlp0s20f3" ["-t", "<essid> <quality>%"] 300
                    , Run MultiCpu ["-t", "cpu: <total0>% <total1>% <total2>% <total3>% <total4>% <total5>% <total6>% <total7>%" ] 100
                    , Run Memory ["-t","mem: <usedratio>%"] 100
                    -- , Run Swap ["-t","swap: <usedratio>%"] 100
                    , Run BatteryP ["CMB0"] ["-t", "<acstatus><timeleft> (<left>%)",
                                    "-L", "10", "-H", "80", "--",
                                    "-O", "<fc=#95e454>AC</fc> - ",
                                    "-o", "",
                                    "-f", "ADP1/online"] 600 
                    , Run Date "%a %b %_d %l:%M" "date" 600
                    -- , Run Date "%a %b %_d %l:%M:%S" "date" 10
                    , Run MPD ["-t", "<artist> - <title> <statei>"] 300
                    -- , Volume "default" "Master"  [] 10
                    --                ["-t", "<status><volume>%", "--",
                    --                 "-O", "", "-O", "<fc=#ff2026>mute</fc> - "] 10

                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%battery% | %multicpu% | %memory% | %wlp0s20f3wi% } %StdinReader% { %mpd% | %KBNA% | <fc=#eadead>%date%</fc>"
       -- , template = "%battery% | %multicpu% | %memory% %swap% | %wlan0wi% } %StdinReader% { %mpd% | %KBNA% | <fc=#eadead>%date%</fc>"
       -- , template = "%battery% | %multicpu% | %memory% | %wlan0wi% } %StdinReader% { %mpd% | %default:Master% | %KBNA% | <fc=#eadead>%date%</fc>"
       -- , template = "%battery% | %multicpu% | %memory% | %wlan0wi% } %StdinReader% { %KBNA% | <fc=#eadead>%date%</fc>"
       }
