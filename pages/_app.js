import React from "react";
import App from "next/app";
import { createGlobalStyle } from "styled-components";
import Head from "next/head";
import { normalize } from "polished";

const GlobalStyle = createGlobalStyle`
  * {
    box-sizing: border-box;
  }

  @import url('https://rsms.me/inter/inter.css');

  html {
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
  }

  @supports (font-variation-settings: normal) {
    html {
      font-family: 'Inter var', -apple-system, BlinkMacSystemFont, sans-serif;
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
          <link rel="stylesheet" href="https://rsms.me/inter/inter.css" />
          <link
            rel="stylesheet"
            href="https://unpkg.com/tachyons@4.10.0/css/tachyons.min.css"
          />
        </Head>

        <GlobalStyle />
        <Component {...pageProps} />
      </>
    );
  }
}
