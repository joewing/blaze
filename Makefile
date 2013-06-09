
FILES= hdl/blaze_multiplier.vhdl \
       hdl/blaze_divider.vhdl \
       hdl/blaze_alu.vhdl \
       hdl/blaze.vhdl \
       hdl/tb.vhdl

all:
	ghdl -a $(FILES)
	ghdl -e tb
	ghdl -r tb --stop-time=1ms --ieee-asserts=disable --wave=dump.ghw

clean:
	rm -f dump.ghw work-obj93.cf

