-- .xmobarrc

Config {
  font = "-*-terminus-bold-r-*-*-12-*-*-*-*-*-*-*",
  bgColor = "#222",
  fgColor = "lightgray",
  position = Static { xpos = 0 , ypos = 0, width = 1919, height = 10 },
  commands = [
    Run Date "%A %d %B %H:%M" "date" 10,
    Run Kbd [ ("us", "<fc=red>US</fc>")
            , ("usmy", "<fc=red>US</fc><fc=#900000>*</fc>")
            , ("ru", "<fc=green>RU</fc>")
            , ("rumy", "<fc=green>RU</fc><fc=#009000>*</fc>")
            ],
    Run UnsafeStdinReader
  ],
  sepChar = "%",
  alignSep = "}{",
  template = "%UnsafeStdinReader% }{ %kbd% <fc=#666>/</fc> <fc=#999>%date%</fc>"
}
