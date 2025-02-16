# Changelog — Chequera

## 0.6.0

- More standardized logging.

## 0.5.0

- Another revamp to use better error collection logic.

## 0.4.2

- Improvements to logging.

## 0.4.0

- Refactor.

## 0.3.0

- Only start Steampipe if there are files to be checked in the path, also accounting for ignored file patterns.
- Refactor app to be even more safe.

## 0.2.10

- Improve query extraction by checking for more error cases.

## 0.2.9

- Remove success count from logs and improve error handling.

## 0.2.8

- Show errors when function to stop/start Steampipe service throws.

## 0.2.7

- Better error-handling so the app won't crash if steampipe service start/stop commands fail.
- Removed the "total queries checked" log message because the count is wrong.

## 0.2.6

- Logging improved: show total queries checked.

## 0.2.5

- Log ignored files instead of just patterns.

## 0.2.4

- Proper exit codes when the app exits.

## 0.2.3

- Catch and handle an uncaught error in files-finder function.
