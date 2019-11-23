# TODO

# High Priority
## Design Decisions
### Label Rows
- We need to figure out how we want to model "Label rows". Currently
  they are entirely implicit based on whether the split at a node has
  any characters in its label or not.

Related:

- See subsetting, in relation to label and content rows

### Subsetting

I have subsetting by absolute numerical position working for both rows and columns. That said:

- What should happen when, e.g., only two rows are selected and they
  are from completely unrelated parts of hte nesting structure? Or
  generally, when the selection would create an "invalid" table.

- Should label rows be included in the counting of row position? Yes
  matches what the user is seeing more closely, but deviates farther
  from how things are currently modelled internally.

- What should happen when a content row is removed? Should a label row
  be inserted in its place?

Doing so kind of feels like not doing what the user asked us to do,
but not doing that gives them a table that is impossible to
understand since they can't tell what subset the data rows under it
correspond to.




# Medium Priority

## High Complexity

- Fix data.frame round-trip logic. Not sure how hard a requirement
  this is now that we're subsetting the tables directly

# Low Priority