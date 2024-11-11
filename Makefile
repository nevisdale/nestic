LOCAL_BIN=$(CURDIR)/bin
TEST_COVER_OUT=cover.out

TESTDATA=$(CURDIR)/.testdata
SINGLE_STEP_TESTS_DIR=$(TESTDATA)/65x02
SINGLE_STEP_TESTS=$(SINGLE_STEP_TESTS_DIR)/nes6502/v1

.PHONY: .bindeps
.bindeps:
	GOBIN=$(LOCAL_BIN) go install github.com/golangci/golangci-lint/cmd/golangci-lint@v1.60.3

.PHONY: lint
lint: .bindeps
	$(LOCAL_BIN)/golangci-lint run --fix

.PHONY: .testdata
.testdata:
	mkdir -p $(TESTDATA)

	# SingleStepTests
	[ -d $(SINGLE_STEP_TESTS) ] || git clone https://github.com/SingleStepTests/65x02.git $(SINGLE_STEP_TESTS_DIR)

.PHONY: test
test: .testdata
	NESTEST_BIN=$(NESTEST_BIN) NESTEST_LOG=$(NESTEST_LOG) \
	SINGLE_STEP_TEST_DIR=$(SINGLE_STEP_TESTS) \
	go test -v -timeout 5m -cover -coverprofile $(TEST_COVER_OUT) ./...

.PHONY: test-cover
test-cover: test
	go tool cover -html $(TEST_COVER_OUT)

.PHONY: clean
clean:
	rm -rf bin
	rm -rf $(TESTDATA)
	rm -rf $(TEST_COVER_OUT)
