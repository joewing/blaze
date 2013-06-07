
FILES= blaze.vhdl tb.vhdl

all:
	ghdl -a $(FILES)
	ghdl -e tb
	ghdl -r tb --stop-time=1ms --ieee-asserts=disable --wave=dump.ghw

