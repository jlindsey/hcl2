* [ ] AST parser
    * [x] Identifiers
    * [x] Comments
    * [x] Structual Elements
        * [x] Top-level ConfigFile
        * [x] Body
            * [x] Attributes
            * [x] Blocks
                * [x] Multi-line
                * [x] One-line
    * [ ] Expressions
        * [ ] Terms
            * [x] Literals
                * [x] Booleans
                * [x] Numbers
                * [x] Nulls
            * [x] Collections
                * [x] Lists
                * [x] Objects
            * [ ] Templates
                * [x] String literals
                * [x] Heredocs
                * [ ] Template sub-language
            * [x] Function calls
            * [ ] For loops (list/object comprehensions)
            * [ ] Index postfix operator
            * [ ] Attribute access postfix operator
            * [ ] Splat operator
        * [ ] Operations
            * [x] Unary
                * [x] Negative sign (`-`)
                * [x] Boolean negation (`!`)
            * [x] Binary
                * [x] Comparisons
                    * [x] Equality (`==`)
                    * [x] Inequality (`!=`)
                    * [x] Less than (`<`)
                    * [x] Greater than (`>`)
                    * [x] Less than or equal (`<=`)
                    * [x] Greater than or equal (`>=`)
                * [x] Arithmetic
                    * [x] Sum (`+`)
                    * [x] Difference (`-`)
                    * [x] Product (`*`)
                    * [x] Quotient (`/`)
                    * [x] Modulus (`%`)
                * [x] Logical
                    * [x] And (`&&`)
                    * [x] Or (`||`)
        * [x] Conditional (ternary operator)
* [ ] Context-based Evaluation
* [ ] Unicode compliance pass
* [ ] Serde serialize / deserialze
