# Data structure

For each effect, we have three types of information:

* Effects - i.e. name, description, category and status
* References - i.e. doi/link and bibliographic details, number of citations and use (original, critique, replication, meta-analysis)
* Effect sizes - i.e. type (original, replication, meta-analysis), original metric and value, N, converted metric and value

These effectively form a _relational_ database. Effects and effect sizes are in a 1:n relationship - i.e. effect sizes belong to one effect, but there will be multiple per effect. Simularly, references and effect sizes are related in a 1:n relationship - each effect size comes from exactly one reference. Publications and effects, however, are related in a n:m relationship - each effect is related to multiple publications (i.e. original and replication), but each publication can provide input on multiple effects (and play different roles in them).

Thus setting it up as a four table structure best captures the data:

* effects
* references
* effects_references_link
* effect_sizes

This removes duplication that would otherwise be inevitable and records objects (e.g., references) as such, which makes automatic updating (e.g., of citation counts), linking and searching easier. However, it means that manual entry into the final format spreadsheet is very much not desirable, apart from minor changes.
