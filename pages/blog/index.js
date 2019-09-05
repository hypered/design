import React from "react";

import { Layout, H1, H2, P } from "../../components";

const posts = [
  {
    title: "Starting with NixOps (and thus Nix and NixOS), part 2",
    summary:
      "In part 1, I showed how to write a very basic Nix expression to describe a machine to be deployed on Digital Ocean using NixOps, and the few commands necessary to deploy and destroy it.",
    date: "2017-08-15",
  },
  {
    title: "Starting with NixOps (and thus Nix and NixOS), part 1",
    summary:
      "While learning the Nix ecosystem and trying to use it, I found it a bit more harder than I thought to achieve what I wanted. In this post and the next one, I’m documenting what I learned, partly for myself, partly to share with other people that would like to follow the same path.",
    date: "2017-08-04",
  },
  {
    title: "Exposing a local server through HAProxy using a reverse SSH tunnel",
    summary:
      "In part 1, I showed how to write a very basic Nix expression to describe a machine to be deployed on Digital Ocean using NixOps, and the few commands necessary to deploy and destroy it.",
    date: "2017-08-03",
  },
];

function Blog(props) {
  return (
    <Layout>
      <section>
        <H1>Blog</H1>
        <ul className="list pa0">
          {posts.map(post => {
            return (
              <li className="mb4">
                <H2>{post.title}</H2>
                <div>{post.date}</div>
                <P>{post.summary}</P>
              </li>
            );
          })}
        </ul>
      </section>
    </Layout>
  );
}

export default Blog;
