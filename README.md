HCL2
====
[![builds.sr.ht status](https://builds.sr.ht/~jlindsey/hcl2.svg)](https://builds.sr.ht/~jlindsey/hcl2?)

Rust [HCL2][1] parsing and serialization lib.

[1]: https://github.com/hashicorp/hcl/tree/hcl2

TODOs
-----
(For a minimum 1.0 version, in no particular order)

* [ ] Finish basic syntax elements
    * [ ] Objects
    * [x] Function calls
    * [x] Number scientific notation
    * [ ] Operations
    * [ ] Expressions
    * [x] Heredocs
* [ ] Template sub-language
* [ ] Context-based Evaluation
* [ ] Unicode compliance pass
* [ ] Serde serialize / deserialze

Links
-----
* [Spec][2]
* [Syntax-agnostic model][3]
* [JSON representation][4]

[2]: https://github.com/hashicorp/hcl/blob/hcl2/hclsyntax/spec.md
[3]: https://github.com/hashicorp/hcl/blob/hcl2/spec.md
[4]: https://github.com/hashicorp/hcl/blob/hcl2/json/spec.md

License
-------
Licensed under the ISC License ([LICENSE](LICENSE) or [https://opensource.org/licenses/ISC][5]).

[5]: https://opensource.org/licenses/ISC
