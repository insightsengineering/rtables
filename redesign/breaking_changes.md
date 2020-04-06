# NECESSARY (according to gabe)
## rtabulate

### rtabulating data.frames (NECESSARY)

Order of thhe formal arguments is always the same (rtabulate isn't even a gneric anymore). This means `row_by` is always present but /after/ the `...` now, so it must be specified by argument name, calls of the form

```
rtabulate(df, factor(LETTERS[1:3]), factor(LETTERS[1:3]))
```

Will no longer work. They did work in the3e previous framework.

## rtables compatability
### header as a collection of rows
Tthe 'header' of a TableTree (or similar) object is no longer a collection of rows in the rtables sense. This means that multiple (hopefully rarely used) accessors for manipulating the header as they were no longer work, including:

 - `header_indent` throws a deprecation warning and always returns 0s
 - `header_indent<-` throws a deprecation warning and returns x unmodified
 - `header_row.naames` throws a deprecation warning and returns `NULL`
 - `header_row.naames<-` throws a deprecation warning and x unmodified
 
# FOR DISCUSSION
## row_by
Specifying `row_by` uses the layout framework which will generate label rows for each level AND data rows corrersponding to them.

In the previous way it just created individual rows that contained the computed data named for the levels.


# CONFIRMED
## rtabulate
### rtabulating factors (CONFIRMED)
```
rtabulate(factor(LETTERS[1:3], LETTERS[1:3], length)
```

Now only generatets 1 row, where  it generated 3 before (because it was an implicit self row-by when rtabulating a facttor.

Default behahior when FUN is not specified still generaets 3 rows, but that is befcause it calls the function returnd by  `lstwrapx(table)` now

