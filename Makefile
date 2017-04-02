all: bridge

bridge:
	cc -O3 -dynamiclib -lsoundio -o libbridge.so chez-soundio/bridge.c
