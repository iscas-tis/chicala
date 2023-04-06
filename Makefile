compile:
	sbt compile

publishLocal:
	sbt publishLocal

test:
# Use "clean" make sure that recompile every time.
# Would be better if only publishLocal on code change.
	sbt publishLocal testcase/clean testcase/compile

test-clean:
	rm -rf test_run_dir

