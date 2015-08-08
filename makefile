all:
	sbcl --load build.lisp "$@"
clean:
	rm -f play_icfp2015 result.txt

tests: all
	time ./play_icfp2015 -f problems/problem_0.json -f problems/problem_1.json -f problems/problem_2.json -f problems/problem_3.json -f problems/problem_4.json -f problems/problem_5.json -f problems/problem_6.json -f problems/problem_7.json -f problems/problem_8.json -f problems/problem_9.json -f problems/problem_10.json -f problems/problem_11.json -f problems/problem_12.json -f problems/problem_13.json -f problems/problem_14.json -f problems/problem_15.json -f problems/problem_16.json -f problems/problem_17.json -f problems/problem_18.json -f problems/problem_19.json -f problems/problem_20.json -f problems/problem_21.json -f problems/problem_22.json -f problems/problem_23.json > result.txt

send:
	curl --user :3vzUl6MC7IhRnYvNa9/kUXsY1SO5vHhB3KCA66M6fWM= -X POST -H "Content-Type: application/json" -d @result.txt https://davar.icfpcontest.org/teams/241/solutions
