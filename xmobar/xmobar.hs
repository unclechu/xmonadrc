-- .xmobarrc

Config { sepChar  = "%"
       , alignSep = "}{"
       , template = "%UnsafeStdinReader% }{ %cmd% <fc=#666>/</fc>\
                    \ <action= xte 'keydown Shift_L' && \
                    \          xte 'keydown Shift_R' && \
                    \          xte 'keyup Shift_L' && \
                    \          xte 'keyup Shift_R' \
                    \        >%kbd%</action>\
                    \ <fc=#666>/</fc>\
                    \ <action=gnome-calendar><fc=#999>%date%</fc></action>"

       {-^ font ^-}
       , font = "-*-terminus-bold-r-*-*-12-*-*-*-*-*-*-*"
       {-$ font $-}

       {-^ bgColor ^-}
       , bgColor = "#222"
       {-$ bgColor $-}

       {-^ fgColor ^-}
       , fgColor = "lightgray"
       {-$ fgColor $-}

       {-^ position ^-}
       -- , position = Static { xpos = 0, ypos = 0, width = 1919, height = 10 }
       , position = Top
       {-$ position $-}

       {-^ textOffset ^-}
       {-$ textOffset $-}

       , commands = [ Run Date "%A %d %B %H:%M" "date" 10
                    , Run Kbd [ ("us", "<fc=red>US</fc>")
                              , ("ru", "<fc=green>RU</fc>")
                              ]
                    , Run UnsafeStdinReader
                    , Run CommandReader "unclechu-xmobar-indicators-cmd" "cmd"
                    ]
       }
