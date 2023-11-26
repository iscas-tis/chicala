compile:
	sbt compile

publishLocal:
	sbt publishLocal

test:
# Use "clean" make sure that recompile every time.
# Would be better if only publishLocal on code change.
	sbt publishLocal testcase/clean testcase/compile

test-sim:
	sbt publishLocal "project testcase" clean 'set scalacOptions+="-P:chicala:simulation:true"' compile

test-clean:
	rm -rf test_run_dir

