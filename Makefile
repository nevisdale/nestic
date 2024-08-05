LOCAL_BIN=$(CURDIR)/bin
OUT_NAME=nestic
TEST_COVER_OUT=cover.out


.PHONY: .bindeps
.bindeps:
	GOBIN=$(LOCAL_BIN) go install github.com/golangci/golangci-lint/cmd/golangci-lint@v1.59.1

.PHONY: build
build:
	mkdir -p $(LOCAL_BIN)
	go build -o $(LOCAL_BIN)/$(OUT_NAME) ./cmd

.PHONY: clean
clean:
	rm -rf bin

.PHONY: lint
lint: .bindeps
	$(LOCAL_BIN)/golangci-lint run --fix

.PHONY: test
test:
	go test -v -cover -coverprofile $(TEST_COVER_OUT) ./...

.PHONY: test-cover
test-cover: test
	go tool cover -html $(TEST_COVER_OUT)

.PHONY: run
run: build
	$(LOCAL_BIN)/$(OUT_NAME)
