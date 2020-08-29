# About

A small clojure library to generate adx files. It is not very efficient, but it works and doesn't have many lines of code (only around 400 lines if you exclude definitions of orderings).

# Usage

Mainly, this is a tool originally made by one developer for their own use, so you'd need to look at the source to figure out how to use it. But here are a few tips for how to use it:

The function new-adx-write-to makes a new write-to, which is mutable for efficiency reasons.

Use the function adx-write-with-container to write the data into the write-to. It takes 4 arguments.

The first argument is the ordering, which describes the layout of the data. Normally you'll want to pass the layout of an entire file, for example facture-file-ordering-optimized, or paie-file-ordering. For efficiency reasons, you'll usually want to run this through 'core-ns-optimize-file-ordering, which pre-generates as much of the file as it can. This has already been done for facture-file-ordering-optimized.

The second argument is the "namespace", a hash-map giving functions to write the types inside the ordering. Normally this will be core-namespace.

The third argument is the write-to, usually this should be created using new-adx-write-to.

The final argument is the actual data to use in the file. What this should be will depend on the ordering you used. For example, facture-file-ordering takes a hash-map with 4 keys, :SIDE, :ajuste-inventaire, :factures, and :paiements. :SIDE must contain a hash-map with data for the SIDE-ordering. :ajuste-inventaire must contain a sequence of hash-maps with data for ITRA-ordering.

If you need ordering values for kinds of adx layouts which aren't defined here, or if you want to customize a field in an ordering which was hardcoded into it instead of being set to custom, you'll need to define a new ordering.

Finally, use the function adx-write-finish to get a string representation of the adx file in the write-to.
