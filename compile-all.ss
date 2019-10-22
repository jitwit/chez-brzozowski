#! /usr/bin/env scheme-script

(import (chezscheme))

(for-each compile-library
          '("brzozowski.sls"))
