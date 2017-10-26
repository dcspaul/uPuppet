# uPuppet

A compiler for a subset of the Puppet configuration language, based on a formal semantics.
See the [Paper](https://arxiv.org/abs/1608.04999) _μPuppet: A Declarative Subset of the Puppet Configuration Language_
by Paul Anderson, James Cheney, Weili Fu, Roly Perera

## Running

The Bin directory contains ready-to-run binaries for various platforms.
Eg. `uPuppet-Darwin-i386`

To compile foo.pp to a catalog on the stdout:
`./Bin/uPuppet-<YOUR-PLATFORM> -o - foo.pp`

For help with command-line arguments:
`./Bin/uPuppet-<YOUR-PLATFORM>`

Where `<YOUR-PLATFORM>` is the appropriate platform. If there is no appropriate binary in the Bin directory, you will need to build one ....

## Building

The source is in Src. To compile this, you probably need:
- libpcre-ocaml-dev
	- Necessary for regular expressions (`dev` required by `regex-pcre`)
- ghc 7.10.*
- `cabal install MissingH`
- `cabal install Aeson`
- `cabal install process`
- `cabal install filepath`
- `cabal install directory`
- `cabal install regex-pcre`

Then:
- Type `make`
- Output goes in `Bin/uPuppet-<YOUR-PLATFORM>`
- This is linked to `Bin/uPuppet` for convenience
- Intermediate files go in `Tmp` (you can delete these)

| Source File |   |
| ----------- | - |
| uPuppet.hs | The main program |
| Options.hs | Command line option handling |
| CState.hs | Common state structure (for options etc) |
| Errors.hs | Error message handling |
| UPuppet/AST.hs | AST type definition |
| UPuppet/ShowAST.hs | Render AST as string for output (simple show for now) |
| UPuppet/Catalog.hs | Catalog type definition |
| UPuppet/ShowCatalog.hs | Render Catalog as string for output |
| UPuppet/ShowJSON.hs | Render Catalog as JSON for output |
| UPuppet/Parser.hs | Parse Source -> AST |
| UPuppet/Eval.hs | Evaluate AST -> Catalog |

## Vagrant

The Vagrant directory contains subdirectories for various OS targets
(currently just Ubuntu) to build a Vagrant VM with Puppet and uPuppet installed:

- `make create-ubuntu` to create an Ubuntu Vagrant machine with Puppet/uPuppet installed
- `make provision-ubuntu` to re-provision with latest version of uPuppet etc.

This can be exported as a virtual appliance by using `File -> Export Appliance` from the VirtualBox menu. To log directly into the virtual machine (not using `vagrant ssh`) the user/password is `ubuntu`/`upuppet`. 

## Testing

The script `Tools/run-tests.pl` will compile the test sources using both uPuppet and
"real" Puppet & compare the output.

- you must have the Ubuntu Vagrant machine up and running (but, see below ..)
- to run all of the tests in the Test directory, do: `make test-ubuntu`
- to run one of the tests only, do: `make ARGS=<SOME-FILE>.pp test-ubuntu`
  (`<SOME-FILE>.pp` must be a path relative to the Test directory)
- to get full details: `make 'ARGS=-v <SOME-FILE>.pp' test-ubuntu` (note quotes)

By default, the results of the (real) Puppet compilations are cached in the Cache directory. uPuppet results are not cached, because compilations are fast, and the compiler is probably changing as well. Use the -r option to force a recompile of cached files.

The -n option can be used to compile with uPuppet only and not attempt a comparison.
This allows the compilations to be checked without a running VM, even if the cache is out of date. If you want to run the tests from the VM itself, use the `-L` option to execute Puppet, as well as uPuppet, locally.

The following annotations can appear in any comment line (starting with #) in the Puppet source:

| Annotation |   |
| ---------- | - |
| `@EXPECT_FAIL` | Expect both uPuppet & Puppet to fail |
| `@UNSUPPORTED` | Expect uPuppet to fail & Puppet to compile |
| `@UNORDERED` | Ignore order differences when comparing (top-level) catalog entries |
| `@OPTIONS <OPTIONS>` | Pass the specified `<OPTIONS>` to the (real) Puppet compiler |
| `@FEATURES <KEYWORD1>, <KEYWORD2>, ...` | Features tested by this test (see below) |

If you are using the Sublime editor, then you can put the following menu entry
in your sublime project file. Selecting this menu entry will compile whichever
test file is in the current window:

````
{
	"cmd":
	[
		"/bin/bash",
		"-lc",
		"make -C $project_path \"ARGS=-v -S $file\" test-ubuntu"
	],
	"name": "Run One Test"
}
````

## Feature Summaries

The test files can be annotated with keywords representing the tested features:
Eg: `@FEATURES classes, inheritance`
Files with no explicit features are automatically tagged `unclassified, misc`

The -f and -F options to run-tests.pl can be used to generate a summary of the test
coverage by feature. -f provides a short summary, -F provides a detailed summary.

The option `-u <SOME-FILE>.tex` can be used used automatically update a latex table with the
current feature statistics. Lines with the following form will be edited to contain the corresponding
values ...

_SOME-DESCRIPTION_ & _TOTAL-COUNT_ & _PASS-COUNT_ & _UNSUPPORTED-COUNT_ \\ % @FEATURE _KEYWORD_

Eg: `~~ Variables & 4 & 3 & 1 \\% @FEATURE variables`

Lines not matching this pattern are copied unchanged.
The original version of the file is renamed to `<SOME-FILE>.tex~`

## Latex Output

The -l option to run-tests.pl can be used to generate latex files with the (formatted) source and corresponding output (from Puppet, not uPuppet) for the tests. The latex output files are generated in the Cache/Latex directory, but they are re-generated every time run-tests.pl is run with the -l option (because the formatting options may have changed). The option `-t <FORMAT-STRING>` controls the format of the latex output (default `2fme`). This may contain the following characters:

| Format |   |
| ------ | - |
| 1 | single column style |
| 2 | double column style |
| f | enclose in figure |
| m | enclose in minipage |
| b | retain blank lines (normally removed) |
| a | retain comments containing annotations (normally removed) |
| e | expect some compilation failures |








