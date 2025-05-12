(in-package :fdb)

(defsection @fdb (:title "General organisation of the database.")
  (@organisation section)
  (@functional-addons section))

(defsection @organisation (:title "Introduction")
  """
### What is Foundationdb.
It is an ACID complaint, distributed, transactional database management system. A KV store that stores lexigraphically ordered keys. It is well tested, very performant, and free. It has limitations like transaction duration and size limits, but these are ones we can work around. 
 
### Why, you ask, why foundationdb?
1. Simple, I like to fine tune things, it's easier to finetune say full text search that I built myself in foundationdb than say in postgres.
2. Also, I can easily make the features I like in foundationdb and lisp, so it is truly extensible.
3. It is rock solid, fully ACID complaint and distributed from a single machine to across data centers. It truly is a foundation for building great things. It is also extremely well crafted, like all technology I like, SBCL, BEAM, it is there. That's why.
4. The main thing I am giving up is storage space, there will be some level of data duplication.

I considered CitusDB, but it required a lot of tuning and work compared to foundationdb.

### Layers
We're mostly going to use the tuple layer to create a heirachy of data and also ease of range querying.
So the data is arranged with prefixes and if neccessary, in inverse indexes to speed search.

### Features we implement
1. Full text search - using BM25 to search over data and ngrams for perfect search. This is very compute intensive and will be a part of ongoing optimisation.
2. Geo spatial search - saving of geospatial, and radius searching, we are going to be needing this, like a lot.

These features will be implemented in the _fdb.lisp_ file so we can always just copy that file to a new application. Because of numerical speed of SBCL, we are right home.
""")

(defsection @functional-addons (:title "Extended functionality")
  (@fts section))
