# Petshop

A CLI tool for synchronizing data between Datadog and files on a local
system.

This tools is designed to allow monitoring (alerts, graphs, etc) to be
tracked as code. A common usage pattern would be to place a directory (or
directories) targeted by this program under version control and to use
that repository as a reviewable, auditable source of truth for monitoring.

Currently, Datadog monitors are supported.

## Building

**WARNING:** The current release of the `datadog` on Hackage is not up-to-date
and will not allow this tool to be built. Please use a build of the library
from git using
[commit `cf4d2da`](https://github.com/iand675/datadog/commit/cf4d2da8f6917bcd5009061208acd1fc68267f91)
or later.

`datadog-petshop` can be built and installed like any other cabal package.

## Usage

To communicate with datadog, the environment variables `DATADOG_API_KEY` and
`DATADOG_APP_KEY` must exist. These can be obtained from the Datadog account
settings page.

```
Usage: datadog-petshop [-d|--dry-run] COMMAND
  Synchronize between Datadog and the local filesystem

Available options:
  -h,--help                Show this help text
  -d,--dry-run             Do not perform any changes

Available commands:
  save                     Sync Datadog configurations to the local filesystem
  load                     Sync local filesystem configurations to Datadog
```

### Save

```
Usage: datadog-petshop save DEST URL...
  Sync Datadog configurations to the local filesystem

Available options:
  -h,--help                Show this help text
  DEST                     File/Directory in which to store configuration
  URL...                   Endpoint from which to pull configuration
  ```

### Load

```
Usage: datadog-petshop load SOURCE...
  Sync local filesystem configurations to Datadog

Available options:
  -h,--help                Show this help text
  SOURCE...                File/Directory from which to read configuration
```
