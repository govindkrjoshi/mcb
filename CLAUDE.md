# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Issue Tracking

This project uses **bd** (beads) for issue tracking. Issues are stored in `.beads/` and synced with git.

```bash
bd ready                              # Find available work
bd show <id>                          # View issue details
bd update <id> --status in_progress   # Claim work
bd close <id>                         # Complete work
bd sync                               # Sync with git
```

## Session Completion

When ending a work session, complete ALL steps:

1. File issues for remaining work with `bd create`
2. Run quality gates (tests, linters, builds) if code changed
3. Update issue status - close finished work
4. Push to remote:
   ```bash
   git pull --rebase
   bd sync
   git push
   ```
5. Verify `git status` shows "up to date with origin"

Work is NOT complete until `git push` succeeds.
