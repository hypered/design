// This file should match the the `document` function in `Hypered/Html.hs`.
// That function can be called from the command-line as:
//
//   $ nix-shell --run "runghc bin/hypered-design.hs wrapper"

import React from "react";
import App from "next/app";
import Head from "next/head";
import { createGlobalStyle } from "styled-components";

const GlobalStyle = createGlobalStyle`
  * {
    box-sizing: border-box;
  }

  html {
    font-family: 'IBM Plex', sans-serif;
  }

  @supports (font-variation-settings: normal) {
    html {
      font-family: 'IBM Plex', sans-serif;
    }
  }

  body {
    background: #ffffff;
    color: #000000;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
  }
`;

export default class MyApp extends App {
  render() {
    const { Component, pageProps } = this.props;
    return (
      <>
        <Head>
          <title>Hypered Design System</title>
          <link
            rel="icon"
            type="image/png"
            href={`${process.env.BACKEND_URL}/static/img/favicon.png`}
          />
          <link
            rel="stylesheet"
            href={`${process.env.BACKEND_URL}/static/css/ibm-plex.css`}
          />
          <link
            rel="stylesheet"
            href={`${process.env.BACKEND_URL}/static/css/tachyons.min.v4.11.1.css`}
          />
          <link
            rel="stylesheet"
            href={`${process.env.BACKEND_URL}/static/css/style.css`}
          />
          <link
            rel="stylesheet"
            href={`${process.env.BACKEND_URL}/static/css/styles.css`}
          />
        </Head>

        <GlobalStyle />
        <Component {...pageProps} />
      </>
    );
  }
}
