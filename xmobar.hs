-- .xmobarrc

Config {
	font = "-*-terminus-bold-r-*-*-12-*-*-*-*-*-*-*",
	bgColor = "black",
	fgColor = "lightgray",
	position = TopW L 100,
	commands = [
		Run Cpu [
			"--template", "<total>",
			"-L", "10", -- %
			"-H", "50", -- %
			"--normal", "green",
			"--high", "red"
		] 10,
		Run MultiCpu [
			"--template", "<autototal>",
			"--Low",      "10", -- units: %
			"--High",     "50", -- units: %
			"--normal",   "green",
			"--high",     "red"
		] 10,
		Run DynNetwork [
			"--template", "<rx>/<tx>",
			"--Low",      "1000", -- units: kB/s
			"--High",     "5000", -- units: kB/s
			"--low",      "#CEFFAC",
			"--normal",   "#FFFFCC",
			"--high",     "#FFB6B0"
		] 10,
		Run Memory [
			"-t", "<usedratio>%"
		] 10,
		Run Swap [
			"-t", "<usedratio>%"
		] 10,
		Run Date "%a %b %_d %l:%M" "date" 10,
		Run StdinReader
	],
	sepChar = "%",
	alignSep = "}{",
	template = "%StdinReader% }{ <fc=#FFB6B0>]</fc>   %cpu% / %multicpu% <fc=gray>|</fc> <fc=skyblue>%memory%</fc>/<fc=skyblue>%swap%</fc> <fc=gray>|</fc> %dynnetwork% <fc=gray>|</fc> <fc=#ee9a00>%date%</fc>"
}

-- vim: ft=haskell :
