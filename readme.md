# chequera

`chequera` is a CLI app for testing queries in [Steampipe](https://steampipe.io/) plugin documentation.

## Usage

```sh
$ chequera test --path <file/directory>
```

## Installation

- Grab the latest release binary for your machine from [releases](https://github.com/chandru89new/chequera/releases/latest)
- Make sure you have [`steampipe`](https://steampipe.io/downloads) installed.
- Install the plugin for which you are about to test docs. (eg. `steampipe plugin install aws`)
- Use chequera to run the test on the docs.

## Examples

1. Test all queries in the `steampipe-plugin-aws` plugin docs directory:

```sh
$ chequera test --path ./steampipe-plugin-aws/docs
```

2. Test queries in a single file:

```sh
$ chequera test --path ./steampipe-plugin-aws/docs/tables/aws_s3_bucket.md
```

3. Ignore certain files / files with names that contain certain strings: (will ignore files/paths whose names contain `aws_s3_bucket` or `aws_sqs_queue`)

```sh
$ CQ_IGNORE=aws_s3_bucket,aws_sqs_queue chequera test --path ./steampipe-plugin-aws/docs
```

4. Change timeout for each query to 60 seconds (default is 30s):

```sh
$ CQ_TIMEOUT=60 chequera test --path ./steampipe-plugin-aws/docs
```

## Using the test script

In `scripts/` you'll find a bash file named `test`. You can use this to test all example queries for a Steampipe plugin.

Example:

I want to test the `github` plugin docs:

```sh
> curl https://raw.githubusercontent.com/chandru89new/chequera/refs/heads/main/scripts/test.sh --output test.sh
> chmod +x test.sh
> PLUGIN=github PLUGIN_REPO=github.com/turbot/steampipe-plugin-github ./test.sh
```

This will download / update the `github` plugin, clone the plugin repo (for docs) and then run `chequera` in the docs folder.

You will need to have these two variables set:

- `PLUGIN` (eg `PLUGIN=github`, or `PLUGIN=orgname/someplugin`)
- `PLUGING_REPO` (eg `PLUGIN_REPO=github.com/turbot/steampipe-plugin-github` or `PLUGIN_REPO=github.com/orgname/someplugin`)

You can also set `chequera`-specific environment variables before running the comand to pass those to `chequera`:

```sh
> PLUGIN=github PLUGIN_REPO=github.com/turbot/steampipe-plugin-github CQ_TIMEOUT=120 ./test.sh
```

TODO:

- [x] release binaries for MacOS and Linux platforms
- [x] update readme to add installation instructions
- [ ] add support for testing SQLite code blocks
- [ ] add support for Powerpipe dashboard files that contain queries
