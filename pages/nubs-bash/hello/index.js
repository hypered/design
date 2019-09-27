import React from "react";
import styled from "styled-components";

import { Layout, H1, P, Code } from "../../../components";

function Hello(props) {
  return (
    <Layout>
      <article>
        <H1 className="f1 lh-title mv0">hello</H1>

        <Code>{`100644 blob ce013625030ba8dba906f756967f9e9ca394464a       6	README.md
100644 blob af30b8907eb547584db9acde06330c0bd9499ade     159	default.nix
100644 blob 3d1b4caeea857ba5904176a3747bce4f21308903     111	example-2.json
100644 blob b97348f9a83a7b8524092e5023645d7d5cfffc98      88	example-3.json
100644 blob 607e191c3e33779a370948a5f17ee5d57b625efb     568	git.json
h1100644 blob a68af143f8ca45ddeaf13f45968abb4dfdcf6040   513	layla.json
100644 blob 5596e6e27e686c21ae2f258ec79fd32a0527e988     321	spawn.sh
100644 blob bb7f9d263a7691eb76690f8ab4912c1a95892bdc     264	texts.json
`}</Code>

        <P>
          Waveguide builds and/or runs a Nix expression on a freshly provisioned
          machine.
        </P>

        <form className="nl3 nr3">
          <div className="flex flex-wrap items-center bt bb justify-between pv2 mh3">
            <div>show fetchgit:README.md</div>
            <div>
              <input
                type="submit"
                value="Save"
                className="button-reset bg-black ph3 pv2 white br2 bn"
              />
              <input type="hidden" name="repository" value="hello" />
              <input type="hidden" name="filename" value="README.md" />
              <input type="hidden" name="content" />
            </div>
          </div>

          <Code editable={true}>{`[
 {"id":1,"x":127,"y":99,"text":"Git","size":32},
 {"id":2,"x":127,"y":151,"text":"shape-2","size":16},
 {"decoration":"bullet","id":3,"x":127,"y":206,"text":"shape-3","size":16},
 {"decoration":"rect","id":4,"x":-1,"y":255,"text":"work dir.","size":16},
 {"decoration":"rect","id":5,"x":212,"y":255,"text":"index","size":16},
 {"decoration":"rect","id":6,"x":439,"y":255,"text":".git","size":16},
 {"id":7,"x":124,"y":594,"text":"Note: the index is actually part of .git","size":16},
  {"decoration":"rect","id":8,"x":664,"y":255,"text":"github","size":16}
]
`}</Code>
        </form>
      </article>
    </Layout>
  );
}

export default Hello;
