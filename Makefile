all:	normal

clean:
	cd hhl ; make clean
	cd hhd ; make clean

normal:	version
	cd hhl  ; make
	cd hhd ; make

version:
	./mkversion
