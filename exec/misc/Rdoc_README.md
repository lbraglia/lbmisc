# Rdoc

Since sometimes I want to access R documentation from Linux command line,
here is a small Rscript to do that:

```
Usage:

   Rdoc strings [-t type]    - Use help (type can be text and/or pdf)
   Rdoc -v [strings]         - Use vignette

Examples:

   Rdoc read.csv car::recode -t text pdf    # no html doc
   Rdoc -v grid data.table::datatable-faq   # two pdf vignette
   Rdoc -v DT                               # html vignette ok too
   
```

