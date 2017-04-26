# Public

`.rkt` files in this directory `provide` things a user may `require`
in their:

- `blog.rkt`
- `xxx-template.html`
- `.scrbl` format sources

Everything else should go under the `private` directory.

When in dobut, err on the side of making things private. Once public,
we need to support forever.
