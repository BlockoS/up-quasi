CC   = gcc
CXX  = g++
RASM = rasm
ECHO = echo

CCFLAGS = -W -Wall
RASMFLAGS =

ALL = bin2m12 cge2bin trajectory \
      gfx data \
      up-quasi.bin up-quasi.m12 \
      up-quasi_emu.bin up-quasi_emu.m12

all: $(ALL)

bin2m12: tools/bin2m12.c
	@$(ECHO) "CC	$@"
	@$(CC) $(CCFLAGS) -o $@ $^

cge2bin: tools/cge2bin.c
	@$(ECHO) "CC	$@"
	@$(CC) $(CCFLAGS) -o $@ $^ -lm

trajectory: tools/trajectory.c
	@$(ECHO) "CC	$@"
	@$(CXX) $(CCFLAGS) -o $@ $^ -lm

gfx:
	@$(ECHO) "GEN	GFX"
	@./cge2bin -x 0 -y 0 -w 40 -h 25 ./data/quasi.txt ./data/quasi.bin
	@./cge2bin -x 0 -y 0 -w 40 -h 25 ./data/groups.txt ./data/groups.bin
	@./cge2bin -x 0 -y 0 -w 8 -h 25 ./data/gfx2.txt ./data/gfx00.bin
	@./cge2bin -x 8 -y 0 -w 8 -h 25 ./data/gfx2.txt ./data/gfx01.bin
	@./cge2bin -x 16 -y 0 -w 8 -h 25 ./data/gfx2.txt ./data/gfx02.bin
	@./cge2bin -x 24 -y 0 -w 8 -h 25 ./data/gfx2.txt ./data/gfx03.bin
	@./cge2bin -x 32 -y 0 -w 8 -h 25 ./data/gfx2.txt ./data/gfx04.bin
	@./cge2bin -x 0 -y 0 -w 8 -h 25 ./data/gfx3.txt ./data/gfx05.bin
	@./cge2bin -x 8 -y 0 -w 8 -h 25 ./data/gfx3.txt ./data/gfx06.bin
	@./cge2bin -x 16 -y 0 -w 8 -h 25 ./data/gfx3.txt ./data/gfx07.bin
	@./cge2bin -x 24 -y 0 -w 8 -h 25 ./data/gfx3.txt ./data/gfx08.bin
	@./cge2bin -x 32 -y 0 -w 8 -h 25 ./data/gfx3.txt ./data/gfx09.bin

data/trajectory.inc:
	@$(ECHO) "GEN	DATA"
	@./trajectory > $@

up-quasi.bin: data/trajectory.inc gfx
	@$(ECHO) "RASM	$@"
	@$(RASM) $(RASMFLAGS) up-quasi.asm -o $(basename $@)

%.m12: %.bin bin2m12
	@$(ECHO) "M12	$@"
	@./bin2m12 $< $@ UP-QUASI

up-quasi_emu.bin: up-quasi.bin
	@$(ECHO) "RASM	$@"
	@$(RASM) -DEMU=1 $(RASMFLAGS) up-quasi.asm -o $(basename $@)

clean:
	@$(ECHO) "CLEANING UP..."
	@rm -f bin2m12 cge2bin trajectory trajectory.inc up-quasi.bin up-quasi_emu.bin up-quasi.m12 up-quasi_emu.m12
	@find $(BUILD_DIR) -name "*.o" -exec rm -f {} \;
