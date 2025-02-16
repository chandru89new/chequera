tests to run

```sh
# test combinations
chequera test --path tests

# test ignore
CQ_IGNORE=aws_ chequera test --path /Users/chandrashekharv/Documents/turbot/steampipe-plugin-aws/docs

# test empty ignore
CQ_IGNORE= chequera test --path /Users/chandrashekharv/Documents/turbot/steampipe-plugin-aws/docs

# bad path
chequera test --path fasdfasdfasdf # bad path

# exit code 0 when all good
chequera test --path tests/good.md # to check exit code 0

# timeout
CQ_TIMEOUT=1 chequera test --path tests

# timeout 0s - will error out even before things start
CQ_TIMEOUT=0 chequera test --path tests
```
