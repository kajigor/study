MMC = mmc
FLAG = make
INPUT = fib
BUILD_DIR = Mercury

all: clean build 

build: 
	$(MMC) --$(FLAG) $(INPUT)

clean: 
	rm -f $(INPUT).err $(INPUT).mh $(INPUT)
	rm -rf $(BUILD_DIR)

