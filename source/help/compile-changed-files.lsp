;;; Compile any files that we've changed since this image was built

(in-package :oplan)

(compile-system 'everything :ignore-depends t)
