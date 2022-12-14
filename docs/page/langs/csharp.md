# C# #

You can customize variable `ts-docstr-csharp-style` for the following value.

* `microsoft` - Microsoft standard (default)
* `doxygen` - Doxygen Style
* `nil` - Respect to user's customization

C# currently uses mix of [Javadoc](https://en.wikipedia.org/wiki/Javadoc) and
[XML documentation](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/xmldoc/)
. The style and prefix will be auto-detect inside the code.

## Triggerations

Generally, you will need something around to parse to make document string work
correctly.

* `/*[*]*/` - hit return between `/*` and `*/` (Doxygen)
* `///` - the third slashes (XML documentation)

## References

* [Documentation comments - C# langauge specification](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/documentation-comments)
