bibtex-format
-------------

[![Build Status](https://travis-ci.org/Bilalh/bibtex-format.svg?branch=master)](https://travis-ci.org/Bilalh/bibtex-format)

Formats a BibTeX file or a BibTeX entry and inlines cross-references.
Gets rid of junk fields that the websites add to the entries.

Usage:

For a BibTeX file:

	cat $file | bibtex-format

From the clipboard:

	pbpaste | bibtex-format

Use -t <field> to get rid of addition fields

`align_equals.rb`  aligns the BibTeX nicely.


###Examples

What I commonly use as a bash function:

```
function bib_w(){
	pbpaste \
	| bibtex-format -t language -t document_type -t art_number        \
		-t source -t coden -t art_number -t note -t acmid -t address  \
		-t school -t language -t document_type -t source -t coden     \
		-t langid -t shortjournal -t urldate -t note -t archiveprefix \
		-t eprint -t mendeley-tags "$@"                               \
	| bibtex-format -t publisher -i book -i inbook                    \
	| align_equals.rb
}
```

To process all .bib files in a directory using GNU parallel.

```
parallel --tag 'cat {} | bibtex-format | align_equals.rb  > {}.temp && mv {}.temp {} ' :::: <(find . -name '*.bib')
```


bibtex-new-keys
---------------

Add bibtex-new-keys to filter to create new cite keys e.g

For a BibTeX file:

	cat $file | bibtex-new-keys

From the clipboard:

	pbpaste | bibtex-new-keys


Scripts
-------

When using in Texmate:

	scripts/BibTex Format.tmCommand
	scripts/BiBTeX New keys.tmCommand


Licence
-------

Copyright [2015] Bilal Syed Hussain

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.


Authors
-------
* Bilal Syed Hussain

