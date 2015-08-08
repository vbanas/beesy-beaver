all:
	sbcl --load build.lisp "$@"
clean:
	rm -f play_icfp2015
