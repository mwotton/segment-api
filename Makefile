testwatch:
	HSPEC_FAILURES_FILE=`pwd`/testwatch_canary stack build --test --test-arguments '--rerun' --file-watch --fast 2>&1

