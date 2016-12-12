-- .xmobarrc

Config {
  font = "-*-terminus-bold-r-*-*-12-*-*-*-*-*-*-*",
  bgColor = "#222",
  fgColor = "lightgray",
  position = Static { xpos = 0 , ypos = 0, width = 1919, height = 10 },
  commands = [ Run Date "%A %d %B %H:%M" "date" 10
             , Run Kbd [ ("us", "<fc=red>US</fc>")
                       , ("ru", "<fc=green>RU</fc>")
                       , ("mine(us)", "<fc=red>US</fc><fc=#900000>*</fc>")
                       , ("mine(ru)", "<fc=green>RU</fc><fc=#009000>*</fc>")
                       ]
             , Run UnsafeStdinReader
             , Run CommandReader "~/.xmonad/xmobar-cmd.sh" "cmd"
             ],
  sepChar = "%",
  alignSep = "}{",
  template = "%UnsafeStdinReader% }{ %cmd% <fc=#666>/</fc>\
             \ <action=simulate-keys LShift RShift>%kbd%</action>\
             \ <fc=#666>/</fc>\
             \ <action=gnome-calendar><fc=#999>%date%</fc></action>"
}
