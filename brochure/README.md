# LaTeX brochure

- `example-full.tex` demonstrates the full capabilities of `HyperedReport.cls`.
- `example-large-table.tex` is a single page.

# Embedding font

Instructions from
https://tex.stackexchange.com/questions/24002/turning-off-font-subsetting-in-pdftex.

After building example-font.tex, example-font.log contains

```
...
{/nix/store/j786imwf5wk6cgkpvx42z033j9yzcgij-texlive-combined-2021/share/texm
f/fonts/enc/dvips/plex/plx_x4zmfh.enc}</nix/store/j786imwf5wk6cgkpvx42z033j9yzc
gij-texlive-combined-2021/share/texmf/fonts/type1/ibm/plex/IBMPlexMono-Medium.p
fb></nix/store/j786imwf5wk6cgkpvx42z033j9yzcgij-texlive-combined-2021/share/tex
mf/fonts/type1/ibm/plex/IBMPlexMono.pfb>
...
```

Notice both the `{... .enc}` and `<... .pfb>`.

`kpsewhich` is brought into scopte by entering the same Nix shell used to do
the build. `pdftex.map` is something understood by `kpsewhich`, not a file in
the current directory.

```
$ grep IBMPlexMono-Medium.pfb $(kpsewhich pdftex.map)
IBMPlexMono-Medium-sup-ly1--base IBMPlexMono-Medium " AutoEnc_xl2q4zy5qznrjxd5c7q73k2whc ReEncodeFont " <[plx_xl2q4z.enc <IBMPlexMono-Medium.pfb
IBMPlexMono-Medium-sup-ot1 IBMPlexMono-Medium " AutoEnc_6tuc4cys37diwbhgj2ggyh3zyc ReEncodeFont " <[plx_6tuc4c.enc <IBMPlexMono-Medium.pfb
IBMPlexMono-Medium-sup-t1--base IBMPlexMono-Medium " AutoEnc_tibbibkahftyyvjxu5qdaupeua ReEncodeFont " <[plx_tibbib.enc <IBMPlexMono-Medium.pfb
IBMPlexMono-Medium-tlf-ly1--base IBMPlexMono-Medium " AutoEnc_wot2a6nakugfidcnezr7h27hic ReEncodeFont " <[plx_wot2a6.enc <IBMPlexMono-Medium.pfb
IBMPlexMono-Medium-tlf-ot1 IBMPlexMono-Medium " AutoEnc_lxmhqhn4noowsq3ysicmnkmtrh ReEncodeFont " <[plx_lxmhqh.enc <IBMPlexMono-Medium.pfb
IBMPlexMono-Medium-tlf-t1--base IBMPlexMono-Medium " AutoEnc_x4zmfhlt6bquu23gcxqqh2trbf ReEncodeFont " <[plx_x4zmfh.enc <IBMPlexMono-Medium.pfb
IBMPlexMono-Medium-tlf-ts1--base IBMPlexMono-Medium " AutoEnc_qdoxriqjlnc2e4wcp5lhhqafcb ReEncodeFont " <[plx_qdoxri.enc <IBMPlexMono-Medium.pfb
```

Notice that `plx_x4zmfh.enc` appears in there, so that is the releavant line:

```
IBMPlexMono-Medium-tlf-t1--base IBMPlexMono-Medium " AutoEnc_x4zmfhlt6bquu23gcxqqh2trbf ReEncodeFont " <[plx_x4zmfh.enc <IBMPlexMono-Medium.pfb
```

Modify it to replace the last `<` to `<<`. See `embed-ibm-plex-mono-medium.map`
for the result.

Finally, we have to add this line before the `\begin{document}`:

```
\pdfmapfile{=embed-ibm-plex-mono-medium.map}
```
