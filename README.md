# text-histogram

*Generate a plain-text histogram in Common Lisp.*

---

Development is, post November 2023, at: https://codeberg.org/thomp/dxh


*text-histogram* generates a histogram in plain text. The intent is to provide roughly the same functionality as https://github.com/basnijholt/text_histogram3 , a Python package for generating text histograms.

# Example

    (th::histogram (loop for x from 0 to 100
                         collect (* 10 (alexandria:gaussian-random))))


# License

The repository at https://github.com/basnijholt/text_histogram3
advertises itself as provided under the Apache License, Version 2.0.
This Common Lisp code was derived from that code.
