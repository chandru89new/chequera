# chequera

`chequera` is a CLI app for testing queries in [Steampipe](https://steampipe.io/) plugin documentation.

## Usage

```sh
$ chequera test --path <file/directory>
```

## Examples

1. Test all queries in the `steampipe-plugin-aws` plugin docs directory:

```sh
$ chequera test --path ./steampipe-plugin-aws/docs
```

2. Test queries in a single file:

```sh
$ chequera test --path ./steampipe-plugin-aws/docs/tables/aws_s3_bucket.md
```

3. Ignore certain files / files with names that contain certain strings:

```sh
$ CQ_IGNORE=aws_s3_bucket,aws_sqs_queue chequera test --path ./steampipe-plugin-aws/docs
```

(will ignore files whose names contain `aws_s3_bucket` or `aws_sqs_queue`)

4. Change timeout for each query to 60 seconds (default is 30s):

```sh
$ CQ_TIMEOUT=60 chequera test --path ./steampipe-plugin-aws/docs
```

TODO:

- [x] release binaries for MacOS and Linux platforms
- [ ] update readme to add installation instructions
- [ ] add support for Powerpipe dashboard files that contain queries
