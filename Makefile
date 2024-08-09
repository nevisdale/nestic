LOCAL_BIN=$(CURDIR)/bin
OUT_NAME=nestic
TEST_COVER_OUT=cover.out

TESTDATA=$(CURDIR)/.testdata
NESTEST_LOG=$(TESTDATA)/nestest.log
NESTEST_BIN=$(TESTDATA)/nestest.nes

.PHONY: .bindeps
.bindeps:
	GOBIN=$(LOCAL_BIN) go install github.com/golangci/golangci-lint/cmd/golangci-lint@v1.59.1

.PHONY: build
build:
	mkdir -p $(LOCAL_BIN)
	go build -o $(LOCAL_BIN)/$(OUT_NAME) ./cmd

.PHONY: lint
lint: .bindeps
	$(LOCAL_BIN)/golangci-lint run --fix

.PHONY: .testdata
.testdata:
	mkdir -p $(TESTDATA)
	[ -f $(NESTEST_LOG) ] || curl -sL https://www.qmtpro.com/\~nes/misc/nestest.log --output $(NESTEST_LOG)
	[ -f $(NESTEST_BIN) ] || curl -sL https://www.qmtpro.com/\~nes/misc/nestest.nes --output $(NESTEST_BIN)

.PHONY: test
test: .testdata
	NESTEST_BIN=$(NESTEST_BIN) NESTEST_LOG=$(NESTEST_LOG) \
	go test -v -cover -coverprofile $(TEST_COVER_OUT) ./...

.PHONY: test-cover
test-cover: test
	go tool cover -html $(TEST_COVER_OUT)

.PHONY: run
run: build
	$(LOCAL_BIN)/$(OUT_NAME)

.PHONY: clean
clean: 
	rm -rf bin
	rm -rf $(TESTDATA)
	rm -rf $(TEST_COVER_OUT)
