I think the general organization of Struct is as a series of articles, in
`notes/`, to which correspond actual bits of HTML and CSS. As we progress in
those articles, we get more and more CSS to include in our examples.

The notes are meant to be linear, like a book. We want a layered approach that
first culminates with a design system supporting mostly textual content. We
need for instance to style the text itself, and the spacing between pieces of
text.

Then the design system needs to accomodate richer content, with e.g. menus or
forms. For that, we continue the layered approach until we can offer layouting
primitives. Before we work on actual components, we want to have placeholders
that we can layout, and visual debugging tools (such as wireframing).

Now that we have a solid basis (text, layout, but also variables that guide
spacing, gaps, sizes, ...), we can proceed to create all the necessary
components to create modern web pages and application.

Note that we concern ourselves with only HTML and CSS. JavaScript or similar
will happen much later. Technically, we use Slab to author and organize our
HTML, and SCSS for doing the same for CSS.
