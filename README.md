# IMDb analysis in Scala

A Scala coursework project that parses four IMDb non-commercial datasets and implements:

1. genre frequency analysis;
2. recent-film contributor counts;
3. vote-weighted director ratings; and
4. top-rated genres by decade.

The implementation is in `src/main/scala/imdb/`; small synthetic unit fixtures are in `src/test/scala/imdb/`.

## Requirements

- JDK 8 or 11
- sbt
- Scala is resolved from `build.sbt`

Run the unit tests (the full IMDb files are not required):

```bash
sbt test
```

## Obtain the data

IMDb distributes these files for personal and non-commercial use, subject to its dataset terms:

- `title.basics.tsv.gz`
- `title.ratings.tsv.gz`
- `title.crew.tsv.gz`
- `name.basics.tsv.gz`

Download them from <https://datasets.imdbws.com/>, review <https://developer.imdb.com/non-commercial-datasets/>, decompress them, and place the resulting TSV files in:

```text
src/main/resources/imdb/
├── title.basics.tsv
├── title.ratings.tsv
├── title.crew.tsv
└── name.basics.tsv
```

Then run:

```bash
sbt run
```

The datasets are intentionally not committed; they are large, updated independently, and carry separate usage terms.

## Publication notes

This is coursework code and the commit history includes collaborative test development. Confirm course-publication policy and permission from every contributor before changing visibility. No project-wide licence has been selected, so review does not grant reuse rights.
