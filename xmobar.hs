-- .xmobarrc

Config {
  font = "-*-terminus-bold-r-*-*-12-*-*-*-*-*-*-*",
  bgColor = "#222",
  fgColor = "lightgray",
  position = Static { xpos = 0 , ypos = -2, width = 1920, height = 14 },
  commands = [
    Run Date "%A %d %B %H:%M" "date" 10,
    Run Kbd [("us", "<fc=red>US</fc>"), ("ru", "<fc=green>RU</fc>")],
    Run StdinReader
  ],
  sepChar = "%",
  alignSep = "}{",
  template = "%StdinReader% }{ %kbd% <fc=#666>/</fc> <fc=#999>%date%</fc>"
}

-- vim: set et ts=2 sts=2 sw=2 :
