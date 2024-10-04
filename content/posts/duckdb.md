---
date: 2024-10-04T19:17:59+08:00
title: DuckDB
author: "Carlo Hamalainen"
url: /2024/10/04/duckdb
---

Have you ever typed

```shell
$ sudo chmod -R 400 *
```

and then wondered which directory you were in? It happens to the best of us.

I figured this would be a good opportunity to try [DuckDB](https://duckdb.org/)

{{< figure src="/duckdb.webp" width=70% caption="DALL·E rendering of the situation" >}}

First, ask [Claude 3.5 Sonnet](https://www.anthropic.com/news/claude-3-5-sonnet) to write a Go program
to traverse a path, taking note of each file or directory's permissions in octal format, and write
to Parquet files in chunks of 10,000. Claude managed this with "one shot", no adjustments needed.


```go
package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"

	"github.com/apache/arrow/go/v15/arrow"
	"github.com/apache/arrow/go/v15/arrow/array"
	"github.com/apache/arrow/go/v15/arrow/memory"
	"github.com/apache/arrow/go/v15/parquet"
	"github.com/apache/arrow/go/v15/parquet/compress"
	"github.com/apache/arrow/go/v15/parquet/pqarrow"
)

type FileRecord struct {
	Root            string
	RelativePath    string
	OctalPermission string
	Type            string
}

func main() {
	if len(os.Args) != 3 {
		fmt.Println("Usage: program <root_directory> <output_parquet_file_prefix>")
		os.Exit(1)
	}
	rootDir := os.Args[1]
	outputPrefix := os.Args[2]

	records := make([]FileRecord, 0, 10000)
	chunkSize := 10000
	fileCounter := 0

	err := filepath.WalkDir(rootDir, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}

		fi, err := d.Info()
		if err != nil {
			return err
		}

		mode := fi.Mode()
		perm := mode.Perm()
		octPerm := fmt.Sprintf("%03o", perm)

		relPath, err := filepath.Rel(rootDir, path)
		if err != nil {
			return err
		}

		var fileType string
		if fi.IsDir() {
			fileType = "directory"
		} else if fi.Mode().IsRegular() {
			fileType = "file"
		} else {
			fileType = "other"
		}

		record := FileRecord{
			Root:            rootDir,
			RelativePath:    relPath,
			OctalPermission: octPerm,
			Type:            fileType,
		}
		records = append(records, record)

		if len(records) >= chunkSize {
			err := writeParquetFile(outputPrefix, records, fileCounter)
			if err != nil {
				return err
			}
			records = records[:0]
			fileCounter++
		}

		return nil
	})

	if err != nil {
		log.Fatalf("Error walking the path %q: %v\n", rootDir, err)
	}

	// Write any remaining records
	if len(records) > 0 {
		err := writeParquetFile(outputPrefix, records, fileCounter)
		if err != nil {
			log.Fatalf("Error writing parquet file: %v\n", err)
		}
	}
}

func writeParquetFile(outputPrefix string, records []FileRecord, counter int) error {
	fileName := fmt.Sprintf("%s_%d.parquet", outputPrefix, counter)

	f, err := os.Create(fileName)
	if err != nil {
		return err
	}
	defer f.Close()

	schema := arrow.NewSchema(
		[]arrow.Field{
			{Name: "root", Type: arrow.BinaryTypes.String},
			{Name: "relative_path", Type: arrow.BinaryTypes.String},
			{Name: "octal_permission", Type: arrow.BinaryTypes.String},
			{Name: "type", Type: arrow.BinaryTypes.String},
		},
		nil,
	)

	mem := memory.NewGoAllocator()
	rb := array.NewRecordBuilder(mem, schema)
	defer rb.Release()

	root := rb.Field(0).(*array.StringBuilder)
	relativePath := rb.Field(1).(*array.StringBuilder)
	octalPermission := rb.Field(2).(*array.StringBuilder)
	fileType := rb.Field(3).(*array.StringBuilder)

	for _, r := range records {
		root.Append(r.Root)
		relativePath.Append(r.RelativePath)
		octalPermission.Append(r.OctalPermission)
		fileType.Append(r.Type)
	}

	arrowRec := rb.NewRecord()
	defer arrowRec.Release()

	writerProps := parquet.NewWriterProperties(
		parquet.WithCompression(compress.Codecs.Snappy),
	)
	arrowProps := pqarrow.NewArrowWriterProperties(pqarrow.WithStoreSchema())

	writer, err := pqarrow.NewFileWriter(schema, f, writerProps, arrowProps)
	if err != nil {
		return err
	}

	if err := writer.Write(arrowRec); err != nil {
		return err
	}

	return writer.Close()
}
```

I ran the Go program in two rsnapshot backups to find out which files had been mangled. This produced a few hundred Parquet files:

```shell
$ ls laptop-level0*parquet | wc -l
262

$ ls laptop-level1*parquet | wc -l
218
```

Next, a quick DuckDB script to read all the files, join on filename, strip a path prefix, and write to an output Parquet file:

```sql
create or replace table dir1 as select * from parquet_scan('laptop-level0*parquet');
create or replace table dir2 as select * from parquet_scan('laptop-level1*parquet');

COPY (
  SELECT d1.octal_permission, SUBSTR(d1.relative_path, LENGTH('laptop/home/carlo/') + 1) as filepath
  FROM dir1 d1
  JOIN dir2 d2 ON d1.relative_path = d2.relative_path
  WHERE d1.octal_permission != d2.octal_permission
    AND d1.relative_path LIKE 'laptop/home/carlo/%'
) TO 'todo.parquet';
```

This is _surprisingly_ quick. Using Parquet files avoids issues of escaping filenames, dealing with spaces. CSV, never again.

We find over 42,000 files to be changed:

```
D SELECT COUNT(*) FROM 'todo.parquet';
┌──────────────┐
│ count_star() │
│    int64     │
├──────────────┤
│        42196 │
└──────────────┘
```

And it's so quick to poke around:

```
D FROM 'todo.parquet' WHERE filepath LIKE '%.vscode/extensions%';
┌──────────────────┬─────────────────────────────────────────────────────────────────────────────────┐
│ octal_permission │                                    filepath                                     │
│     varchar      │                                     varchar                                     │
├──────────────────┼─────────────────────────────────────────────────────────────────────────────────┤
│ 755              │ .vscode/extensions/valentjn.vscode-ltex-13.1.0/tmp-4sPF5t                       │
│ 755              │ .vscode/extensions                                                              │
│ 755              │ .vscode/extensions/acharluk.easy-cpp-projects-2.0.0                             │
│ 755              │ .vscode/extensions/acharluk.easy-cpp-projects-2.0.0/node_modules                │
│ 755              │ .vscode/extensions/acharluk.easy-cpp-projects-2.0.0/node_modules/node-fetch     │
│ 755              │ .vscode/extensions/acharluk.easy-cpp-projects-2.0.0/node_modules/node-fetch/lib │
│ 755              │ .vscode/extensions/acharluk.easy-cpp-projects-2.0.0/out                         │
│ 755              │ .vscode/extensions/easycpp_custom_templates                                     │
└──────────────────┴─────────────────────────────────────────────────────────────────────────────────┘
```

A one-shot session with Claude provided the simple Go program to fix all the permissions using ``todo.parquet`` as input. Then to tidy up I also ran:

```shell
$ find . -type f -exec file {} \; | grep -E 'shell script|executable' | cut -d ':' -f 1 | xargs chmod +x
```

to be conservative and make sure all shell scripts and binaries were executable.

I heard about DuckDB on [this episode of Developer Voices](https://podcasts.apple.com/us/podcast/practical-applications-for-duckdb-with-simon-aubury/id1687271887?i=1000663932507).

The guests wrote this book; I'm enjoying it:

{{< figure link="https://www.packtpub.com/en-us/product/getting-started-with-duckdb-9781803241005" src="/getting_started_with_duckdb.avif" width=70% caption="click for the book's website" >}}
